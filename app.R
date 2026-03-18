library(shiny)
library(dplyr)
library(DT)

# ── Demo dataset ──────────────────────────────────────────────────────────────
set.seed(42)
n <- 500

demo_data <- data.frame(
  Date       = seq(as.Date("2023-01-01"), as.Date("2024-12-31"), length.out = n),
  Region     = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  Category   = sample(c("Electronics", "Clothing", "Food", "Home", "Sports"), n, replace = TRUE),
  Salesperson= sample(paste0("Rep_", LETTERS[1:8]), n, replace = TRUE),
  Revenue    = round(runif(n, 100, 10000), 2),
  Units      = sample(1:200, n, replace = TRUE),
  Profit     = round(runif(n, 10, 3000), 2),
  stringsAsFactors = FALSE
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Segoe UI', sans-serif; background: #f5f7fa; }
    .sidebar { background: #ffffff; border-radius: 10px; padding: 20px;
               box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
    .main-panel { background: #ffffff; border-radius: 10px; padding: 20px;
                  box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
    h2 { color: #2c3e50; font-weight: 700; margin-bottom: 4px; }
    h4 { color: #34495e; font-weight: 600; }
    .section-title { font-size: 13px; font-weight: 700; text-transform: uppercase;
                     letter-spacing: 1px; color: #7f8c8d; margin-top: 18px;
                     margin-bottom: 8px; border-bottom: 1px solid #ecf0f1;
                     padding-bottom: 4px; }
    .stat-box { background: #f8f9fa; border-radius: 8px; padding: 12px 16px;
                margin-bottom: 10px; border-left: 4px solid #3498db; }
    .stat-box .value { font-size: 22px; font-weight: 700; color: #2c3e50; }
    .stat-box .label { font-size: 12px; color: #7f8c8d; margin-top: 2px; }
    .btn-primary { background: #3498db; border: none; border-radius: 6px; font-weight: 600; }
    .btn-success { background: #27ae60; border: none; border-radius: 6px; font-weight: 600; }
    .btn-warning { background: #e67e22; border: none; border-radius: 6px; font-weight: 600; color: white; }
    .badge-count { background: #3498db; color: white; border-radius: 12px;
                   padding: 2px 10px; font-size: 13px; font-weight: 600; }
  "))),
  
  titlePanel(
    div(
      h2("📊 Sales Data Explorer"),
      tags$p("Filter, aggregate, and extract your data", style = "color:#7f8c8d; margin:0; font-size:14px;")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "sidebar",
          
          # ── FILTERS ──────────────────────────────────────────────────────────
          div(class = "section-title", "🔍 Filters"),
          
          dateRangeInput("date_range", "Date Range",
                         start = min(demo_data$Date),
                         end   = max(demo_data$Date)),
          
          checkboxGroupInput("region", "Region",
                             choices  = sort(unique(demo_data$Region)),
                             selected = sort(unique(demo_data$Region))),
          
          checkboxGroupInput("category", "Category",
                             choices  = sort(unique(demo_data$Category)),
                             selected = sort(unique(demo_data$Category))),
          
          selectInput("salesperson", "Salesperson",
                      choices  = c("All", sort(unique(demo_data$Salesperson))),
                      selected = "All"),
          
          sliderInput("revenue_range", "Revenue Range ($)",
                      min   = 0,
                      max   = ceiling(max(demo_data$Revenue) / 1000) * 1000,
                      value = c(0, ceiling(max(demo_data$Revenue) / 1000) * 1000),
                      step  = 100,
                      pre   = "$"),
          
          # ── AGGREGATION ──────────────────────────────────────────────────────
          div(class = "section-title", "⚙️ Aggregation"),
          
          selectInput("group_by", "Group By",
                      choices = c("None" = "none", "Region", "Category",
                                  "Salesperson", "Month" = "Month")),
          
          checkboxGroupInput("agg_metrics", "Metrics to Include",
                             choices  = c("Total Revenue"  = "Revenue",
                                          "Total Units"    = "Units",
                                          "Total Profit"   = "Profit",
                                          "Avg Revenue"    = "Avg_Revenue",
                                          "Row Count"      = "Count"),
                             selected = c("Revenue", "Units", "Count")),
          
          # ── EXPORT ───────────────────────────────────────────────────────────
          div(class = "section-title", "💾 Extract Data"),
          
          radioButtons("export_choice", "Export:",
                       choices  = c("Filtered (raw)" = "filtered",
                                    "Aggregated"     = "aggregated"),
                       selected = "filtered",
                       inline   = TRUE),
          
          fluidRow(
            column(6, downloadButton("dl_csv",  "CSV",  class = "btn-success btn-block")),
            column(6, downloadButton("dl_tsv",  "TSV",  class = "btn-warning btn-block"))
          ),
          br(),
          actionButton("reset_filters", "↺ Reset Filters",
                       class = "btn-primary btn-block")
      )
    ),
    
    mainPanel(
      width = 9,
      div(class = "main-panel",
          
          # ── SUMMARY STATS ────────────────────────────────────────────────────
          fluidRow(
            column(3, div(class = "stat-box",
                          div(class = "value", textOutput("stat_rows")),
                          div(class = "label", "Rows selected"))),
            column(3, div(class = "stat-box",
                          div(class = "value", textOutput("stat_revenue")),
                          div(class = "label", "Total Revenue"))),
            column(3, div(class = "stat-box",
                          div(class = "value", textOutput("stat_units")),
                          div(class = "label", "Total Units"))),
            column(3, div(class = "stat-box",
                          div(class = "value", textOutput("stat_profit")),
                          div(class = "label", "Total Profit")))
          ),
          
          br(),
          
          # ── TABS ─────────────────────────────────────────────────────────────
          tabsetPanel(
            tabPanel("📄 Filtered Data",
                     br(),
                     DTOutput("filtered_table")
            ),
            tabPanel("📊 Aggregated View",
                     br(),
                     uiOutput("agg_note"),
                     DTOutput("agg_table")
            )
          )
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Filtered reactive
  filtered <- reactive({
    df <- demo_data
    
    df <- df %>%
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2],
             Region   %in% input$region,
             Category %in% input$category,
             Revenue  >= input$revenue_range[1],
             Revenue  <= input$revenue_range[2])
    
    if (input$salesperson != "All")
      df <- df %>% filter(Salesperson == input$salesperson)
    
    df
  })
  
  # Aggregated reactive
  aggregated <- reactive({
    df <- filtered()
    grp <- input$group_by
    metrics <- input$agg_metrics
    
    if (grp == "none" || length(metrics) == 0) return(NULL)
    
    if (grp == "Month") df <- df %>% mutate(Month = format(Date, "%Y-%m"))
    
    group_var <- if (grp == "Month") "Month" else grp
    
    df_grp <- df %>% group_by(.data[[group_var]])
    
    result <- df_grp %>% summarise(
      .groups = "drop",
      Revenue     = if ("Revenue"     %in% metrics) sum(Revenue)     else NA_real_,
      Units       = if ("Units"       %in% metrics) sum(Units)       else NA_real_,
      Profit      = if ("Profit"      %in% metrics) sum(Profit)      else NA_real_,
      Avg_Revenue = if ("Avg_Revenue" %in% metrics) round(mean(Revenue), 2) else NA_real_,
      Count       = if ("Count"       %in% metrics) n()              else NA_integer_
    )
    
    # Drop columns not requested
    keep_cols <- c(group_var,
                   intersect(c("Revenue","Units","Profit","Avg_Revenue","Count"), metrics))
    result %>% select(all_of(keep_cols))
  })
  
  # ── Summary stats ──────────────────────────────────────────────────────────
  output$stat_rows    <- renderText({ format(nrow(filtered()), big.mark = ",") })
  output$stat_revenue <- renderText({ paste0("$", format(round(sum(filtered()$Revenue)), big.mark = ",")) })
  output$stat_units   <- renderText({ format(sum(filtered()$Units), big.mark = ",") })
  output$stat_profit  <- renderText({ paste0("$", format(round(sum(filtered()$Profit)), big.mark = ",")) })
  
  # ── Tables ─────────────────────────────────────────────────────────────────
  output$filtered_table <- renderDT({
    datatable(filtered(),
              options = list(pageLength = 10, scrollX = TRUE,
                             dom = 'lfrtip'),
              rownames = FALSE) %>%
      formatCurrency(c("Revenue","Profit"), "$") %>%
      formatStyle("Revenue",
                  background = styleColorBar(range(filtered()$Revenue), "#d6eaf8"),
                  backgroundSize = "100% 90%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  output$agg_note <- renderUI({
    if (input$group_by == "none")
      return(div(style = "color:#e67e22; padding:12px;",
                 "⚠️  Select a Group By field in the sidebar to see aggregated results."))
    if (length(input$agg_metrics) == 0)
      return(div(style = "color:#e67e22; padding:12px;",
                 "⚠️  Select at least one metric to aggregate."))
    NULL
  })
  
  output$agg_table <- renderDT({
    req(aggregated())
    datatable(aggregated(),
              options = list(pageLength = 15, scrollX = TRUE, dom = 'lfrtip'),
              rownames = FALSE) %>%
      formatCurrency(intersect(c("Revenue","Profit","Avg_Revenue"),
                               names(aggregated())), "$")
  })
  
  # ── Downloads ──────────────────────────────────────────────────────────────
  export_data <- reactive({
    if (input$export_choice == "aggregated" && !is.null(aggregated()))
      aggregated()
    else
      filtered()
  })
  
  output$dl_csv <- downloadHandler(
    filename = function() paste0("export_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(export_data(), file, row.names = FALSE)
  )
  
  output$dl_tsv <- downloadHandler(
    filename = function() paste0("export_", Sys.Date(), ".tsv"),
    content  = function(file) write.table(export_data(), file, sep = "\t",
                                          row.names = FALSE, quote = FALSE)
  )
  
  # ── Reset filters ──────────────────────────────────────────────────────────
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range",
                         start = min(demo_data$Date), end = max(demo_data$Date))
    updateCheckboxGroupInput(session, "region",
                             selected = sort(unique(demo_data$Region)))
    updateCheckboxGroupInput(session, "category",
                             selected = sort(unique(demo_data$Category)))
    updateSelectInput(session, "salesperson", selected = "All")
    updateSliderInput(session, "revenue_range",
                      value = c(0, ceiling(max(demo_data$Revenue)/1000)*1000))
    updateSelectInput(session, "group_by", selected = "none")
    updateCheckboxGroupInput(session, "agg_metrics",
                             selected = c("Revenue","Units","Count"))
  })
}

shinyApp(ui, server)