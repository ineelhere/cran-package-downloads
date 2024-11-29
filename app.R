library(shiny)
library(shinydashboard)
library(cranlogs)
library(ggplot2)
library(plotly)
library(dplyr)
library(reactable)
library(DT)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(
    title = "CRAN Package Downloads",
    titleWidth = 500
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      column(4,
             wellPanel(
               h4("Filters"),
               selectizeInput(
                 inputId = "packages",
                 label = "R Package(s)",
                 choices = NULL,
                 multiple = TRUE,
                 options = list(
                   placeholder = "Select packages",
                   maxItems = 100
                 )
               ),
               dateRangeInput(
                 inputId = "date_range",
                 label = "Date Range",
                 start = Sys.Date() - 90,
                 end = Sys.Date(),
                 min = Sys.Date() - 365,
                 max = Sys.Date()
               ),
               selectInput(
                 inputId = "aggregation",
                 label = "Aggregate By",
                 choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month", "Yearly" = "year"),
                 selected = "month"
               ),
               radioButtons(
                 inputId = "view_type",
                 label = "View Type",
                 choices = c("Exact" = "exact", "Cumulative" = "cumulative"),
                 selected = "exact"
               ),
               actionButton("helpBtn", "Show Tips", icon = icon("info-circle"))
             )
      ),
      column(8,
             conditionalPanel(
               condition = "input.packages.length > 0",
               reactableOutput("package_metrics_table")
             ),
             conditionalPanel(
               condition = "input.packages.length == 0",
               div(class = "alert alert-info", "Please select at least one package")
             )
      )
    ),
    fluidRow(
      column(8,
             plotlyOutput("download_plot", height = "500px")
      ),
      column(4,
             DTOutput("all_data_table")
      )
    ),
    tags$footer(
      HTML('<div style="text-align:center; padding:10px; background-color:#f1f1f1; border-top:1px solid #ccc;"> 
             <p>&copy; 2024 <a href="https://www.linkedin.com/in/indraneelchakraborty/">Indraneel Chakraborty</a></p>
             <p><a href="https://github.com/ineelhere/cran-package-downloads">Code available on Github
             </a></p>
            </div>')
    )
  )
)

server <- function(input, output, session) {
  observe({
    tryCatch({
      cran_packages <- rownames(available.packages())
      updateSelectizeInput(session, "packages", choices = cran_packages, server = TRUE)
    }, error = function(e) {
      showNotification(
        "Error fetching package list. Please check your internet connection.",
        type = "error"
      )
    })
  })
  
  download_data <- reactive({
    req(input$packages)
    
    if (length(input$packages) > 10) {
      showNotification(
        "Please select 10 or fewer packages",
        type = "warning"
      )
      return(NULL)
    }
    
    tryCatch({
      cranlogs::cran_downloads(
        packages = input$packages,
        from = input$date_range[1],
        to = input$date_range[2]
      )
    }, error = function(e) {
      showNotification(
        paste("Error fetching download data:", e$message),
        type = "error"
      )
      return(NULL)
    })
  })
  
  output$package_metrics_table <- renderReactable({
    data <- download_data()
    req(data)
    
    package_stats <- data %>%
      group_by(package) %>%
      summarise(
        total_downloads = sum(count),
        avg_daily_downloads = round(mean(count), 2),
        peak_downloads = max(count),
        first_download_date = min(date),
        last_download_date = max(date)
      )
    
    reactable(
      package_stats,
      columns = list(
        total_downloads = colDef(name = "Total Downloads", format = colFormat(separators = TRUE)),
        avg_daily_downloads = colDef(name = "Avg Daily Downloads"),
        peak_downloads = colDef(name = "Peak Daily Downloads", format = colFormat(separators = TRUE)),
        first_download_date = colDef(name = "From"),
        last_download_date = colDef(name = "To")
      ),
      defaultPageSize = 8,
      striped = TRUE,
      highlight = TRUE,
      searchable = TRUE
    )
  })
  
  aggregated_data <- reactive({
    data <- download_data()
    req(nrow(data) > 0)
    
    data <- data %>%
      mutate(
        period = case_when(
          input$aggregation == "week" ~ as.Date(cut(date, "week")),
          input$aggregation == "month" ~ as.Date(cut(date, "month")),
          input$aggregation == "year" ~ as.Date(cut(date, "year")),
          TRUE ~ date
        )
      ) %>%
      group_by(period, package) %>%
      summarise(total_count = sum(count), .groups = "drop")
    
    data
  })
  
  cumulative_data <- reactive({
    data <- aggregated_data()
    req(nrow(data) > 0)
    
    if (input$view_type == "cumulative") {
      data <- data %>%
        group_by(package) %>%
        mutate(cumulative_count = cumsum(total_count)) %>%
        ungroup()
      data$total_count <- data$cumulative_count
    }
    data
  })
  
  output$download_plot <- renderPlotly({
    data <- cumulative_data()
    req(nrow(data) > 0)
    
    p <- ggplot(data, aes(x = period, y = total_count, color = package)) +
      geom_line(linewidth = 1) +
      labs(
        title = paste("CRAN Package Downloads (Aggregated by", input$aggregation, ")"),
        x = "Period", 
        y = paste(input$view_type, "Downloads")
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    if (input$aggregation %in% c("month", "year")) {
      peak_data <- data %>%
        group_by(package) %>%
        slice_max(total_count, n=1)
      
      p <- p + 
        geom_text(
          data=peak_data, 
          aes(label=round(total_count), x=period, y=total_count), 
          nudge_y=0.05, 
          size=3
        )
    }
    
    ggplotly(p) %>% 
      layout(legend=list(orientation="h", x=0.5, y=-0.2))
  })
  
  output$all_data_table <- renderDT({
    data <- download_data()
    req(nrow(data) > 0)
    
    datatable(
      data, 
      extensions='Buttons', 
      options=list(
        pageLength=10, 
        scrollX=TRUE,
        dom='Bfrtip',
        buttons=c('csv', 'excel')
      )
    )
  })
  
  observeEvent(input$helpBtn, {
    showModal(modalDialog(
      title = "How to Use",
      HTML("<p><strong>1. Use the filters on the left</strong> to select the packages, date range, and aggregation options.</p>
       <p><strong>2. The package metrics</strong> will be displayed in a table on the right.</p>
       <p><strong>3. The plot below</strong> shows the package downloads, and you can switch between exact and cumulative views.</p>
       <p><strong>4. You can export the data</strong> by clicking the export buttons in the table.</p>"),
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)
