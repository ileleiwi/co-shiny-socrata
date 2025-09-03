library(shiny)
library(yaml)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)

source(file.path("R","socrata_v3.R"))
cfg <- yaml::read_yaml("config.yml")

ui <- fluidPage(
  titlePanel("Colorado Open Data — Descriptive Stats (Socrata v3)"),
  sidebarLayout(
    sidebarPanel(
      textInput("view_id", "Socrata View ID", value = cfg$view_id),
      textAreaInput("soql", "SoQL query", value = "SELECT *", rows = 3,
                    placeholder = "e.g. SELECT * WHERE county = 'Denver' ORDER BY date DESC"),
      actionButton("refresh", "Fetch"),
      hr(),
      uiOutput("colpickers"),
      hr(),
      helpText("Using POST to /api/v3/views/<view_id>/query.json. Set env ",
               strong(cfg$app_token_env), " to a token for higher rate limits.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h4("Time series"),
                 plotOutput("ts_plot"),
                 h4("Grouped summary"),
                 DTOutput("tbl_summary")),
        tabPanel("Raw data", DTOutput("tbl_raw"))
      )
    )
  )
)

server <- function(input, output, session) {
  data_r <- eventReactive(input$refresh, {
    withProgress(message = "Querying Socrata v3", value = 0.1, {
      df <- socrata_v3_fetch(view_id = input$view_id,
                             domain = cfg$socrata_domain,
                             soql = input$soql,
                             token_env = cfg$app_token_env)
      incProgress(0.9)
      df
    })
  }, ignoreInit = TRUE)

  current <- reactiveVal(NULL)
  observeEvent(data_r(), {
    df <- data_r()

    # Try to coerce likely date/timestamp columns
    dt_guess <- names(df)[grepl("date|time|timestamp|reported|episode|visit", names(df), ignore.case = TRUE)]
    for (nm in dt_guess) {
      # try both date and datetime
      suppressWarnings({
        if (is.character(df[[nm]]) && all(nchar(df[[nm]]) >= 10)) {
          dt_val <- suppressWarnings(lubridate::ymd_hms(df[[nm]], quiet = TRUE))
          if (sum(!is.na(dt_val)) > 0) df[[nm]] <- dt_val
          else {
            d_val <- suppressWarnings(lubridate::ymd(df[[nm]], quiet = TRUE))
            if (sum(!is.na(d_val)) > 0) df[[nm]] <- d_val
          }
        }
      })
    }
    current(df)
  })

  output$colpickers <- renderUI({
    req(current())
    df <- current()
    num_cols <- names(df)[sapply(df, is.numeric)]
    chr_cols <- names(df)[sapply(df, \(x) is.character(x) || is.factor(x))]
    date_cols <- names(df)[sapply(df, \(x) inherits(x, "Date") || inherits(x, "POSIXct"))]

    tagList(
      selectInput("date_col", "Date column", choices = c("", date_cols), selected = if (length(date_cols)) date_cols[1] else ""),
      selectInput("group_col", "Group column", choices = c("", chr_cols), selected = if (length(chr_cols)) chr_cols[1] else ""),
      selectInput("value_col", "Numeric value column", choices = c("", num_cols), selected = if (length(num_cols)) num_cols[1] else "")
    )
  })

  filtered <- reactive({
    req(current())
    current()
  })

  output$ts_plot <- renderPlot({
    df <- filtered()
    req(input$date_col, input$value_col, input$date_col != "", input$value_col != "")
    df %>%
      filter(!is.na(.data[[input$date_col]])) %>%
      group_by(.data[[input$date_col]]) %>%
      summarise(value = sum(.data[[input$value_col]], na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = .data[[input$date_col]], y = value)) +
      geom_line() +
      labs(x = input$date_col, y = input$value_col, title = "Time series (sum per date)") +
      theme_minimal()
  })

  output$tbl_summary <- renderDT({
    df <- filtered()
    req(input$group_col, input$value_col, input$group_col != "", input$value_col != "")
    df %>%
      group_by(.data[[input$group_col]]) %>%
      summarise(n = n(),
                total = sum(.data[[input$value_col]], na.rm = TRUE),
                mean = mean(.data[[input$value_col]], na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      DT::datatable(rownames = FALSE, options = list(pageLength = 10))
  })

  output$tbl_raw <- renderDT({
    req(filtered())
    DT::datatable(filtered(), options = list(scrollX = TRUE, pageLength = 25))
  })
}

shinyApp(ui, server)
