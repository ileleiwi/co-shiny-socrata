# app.R
library(shiny)
library(bslib)
library(yaml)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)
library(rlang)
library(tibble)

# Avoid name masking (jsonlite::validate vs shiny::validate)
validate <- shiny::validate
need     <- shiny::need

source(file.path("R","socrata_v2.R"))
cfg <- yaml::read_yaml("config.yml")

theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

ui <- fluidPage(
  theme = theme,
  tags$head(tags$style(HTML("
    .token-badge { font-weight:600; }
    .ok { color:#198754; } .no { color:#dc3545; }
    .mt-8 { margin-top: 0.5rem; } .mt-16 { margin-top: 1rem; }
  "))),
  titlePanel("Colorado Open Data — Descriptive Stats (SODA v2)"),
  sidebarLayout(
    sidebarPanel(
      {
        token_found <- nzchar(Sys.getenv(cfg$app_token_env))
        tagList(
          div(class="token-badge",
              if (token_found) span("Token detected ✓", class="ok")
              else span("Token missing ✗", class="no"))
        )
      },
      textInput("view_id", "Socrata View ID", value = cfg$view_id),
      textAreaInput(
        "soql", "SoQL query",
        value = "SELECT * LIMIT 100",
        rows = 3,
        placeholder = "e.g. SELECT * WHERE county='Denver' ORDER BY date DESC LIMIT 1000"
      ),
      div(class="mt-8",
          actionButton("refresh", "Fetch"),
          actionButton("reset", "Reset query"),
          actionButton("health", "Health check")),
      hr(),
      uiOutput("colpickers"),
      uiOutput("filters_help"),
      hr(),
      helpText(
        "Using GET to /resource/<view_id>.json with SoQL ($query). Set env ",
        strong(cfg$app_token_env), " to a token for higher rate limits."
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
          h4("Time series"),
          plotOutput("ts_plot"),
          h4("Grouped stats", class = "mt-16"),
          fluidRow(
            column(
              6,
              selectInput(
                "stat_kind", "Statistic",
                c("Sum", "Mean", "Median", "Count non-NA"), selected = "Sum"
              )
            ),
            column(
              3,
              sliderInput("topn", "Show top N groups", min = 3, max = 50, value = 15, step = 1)
            ),
            column(
              3,
              radioButtons("orient", "Orientation", c("Horizontal", "Vertical"), inline = TRUE)
            )
          ),
          plotOutput("grp_plot"),
          h4("Grouped summary", class = "mt-16"),
          downloadButton("dl_summary", "Download summary (CSV)"),
          DTOutput("tbl_summary")
        ),
        tabPanel("Raw data",
          downloadButton("dl_raw", "Download raw (CSV)"),
          DTOutput("tbl_raw")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # default SoQL on load
  observeEvent(TRUE, {
    updateTextAreaInput(session, "soql", value = "SELECT * LIMIT 100")
  }, once = TRUE)

  # Reset button
  observeEvent(input$reset, {
    updateTextAreaInput(session, "soql", value = "SELECT * LIMIT 100")
  })

  # Health check (quick OK/fail)
  observeEvent(input$health, {
    tryCatch({
      df <- socrata_v2_fetch(
        view_id   = input$view_id,
        domain    = cfg$socrata_domain,
        soql      = "SELECT * LIMIT 10",
        token_env = cfg$app_token_env,
        page_size = 10,
        max_pages = 1L
      )
      showModal(modalDialog(
        title = "Health check OK",
        paste("Rows:", nrow(df), "| Cols:", ncol(df)),
        easyClose = TRUE, footer = modalButton("Close")
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Health check failed",
        paste(conditionMessage(e)),
        easyClose = TRUE, footer = modalButton("Close")
      ))
    })
  })

  current <- reactiveVal(NULL)

  data_r <- eventReactive(input$refresh, {
    withProgress(message = "Querying SODA v2 …", value = 0.1, {
      tryCatch({
        df <- socrata_v2_fetch(
          view_id   = input$view_id,
          domain    = cfg$socrata_domain,
          soql      = input$soql,
          token_env = cfg$app_token_env,
          page_size = 10000,
          max_pages = 5L
        )
        incProgress(0.9)

        # Try to coerce likely date/timestamp columns
        dt_guess <- names(df)[grepl("date|time|timestamp|reported|episode|visit",
                                    names(df), ignore.case = TRUE)]
        for (nm in dt_guess) {
          if (is.character(df[[nm]]) && any(nchar(df[[nm]]) >= 10, na.rm = TRUE)) {
            dt_val <- suppressWarnings(lubridate::ymd_hms(df[[nm]], quiet = TRUE))
            if (sum(!is.na(dt_val)) > 0) {
              df[[nm]] <- dt_val
            } else {
              d_val <- suppressWarnings(lubridate::ymd(df[[nm]], quiet = TRUE))
              if (sum(!is.na(d_val)) > 0) df[[nm]] <- d_val
            }
          }
        }

        validate(need(nrow(df) > 0, "No rows returned. Try a different SoQL query or view id."))
        df
      }, error = function(e) {
        showModal(modalDialog(
          title = "Fetch failed",
          paste("Socrata API error:", conditionMessage(e)),
          easyClose = TRUE, footer = modalButton("Close")
        ))
        NULL
      })
    })
  }, ignoreInit = TRUE)

  observeEvent(data_r(), {
    req(!is.null(data_r()))
    current(data_r())
  })

  # Column pickers + filters
  output$colpickers <- renderUI({
    df <- current()
    if (is.null(df)) return(NULL)

    num_cols  <- names(df)[sapply(df, is.numeric)]
    chr_cols  <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    date_cols <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))]

    tagList(
      selectInput("date_col",  "Date column",   choices = c("", date_cols), selected = if (length(date_cols)) date_cols[1] else ""),
      dateRangeInput("daterange", "Date range (optional)", start = NA, end = NA),
      selectInput("group_col", "Group column",  choices = c("", chr_cols),   selected = if (length(chr_cols)) chr_cols[1] else ""),
      selectInput("value_col", "Numeric value", choices = c("", num_cols),   selected = if (length(num_cols)) num_cols[1] else ""),
      selectizeInput("group_filter", "Filter groups (optional)", choices = NULL, multiple = TRUE)
    )
  })

  # Update date range & group filter options after fetch or picker change
  observe({
    df <- current()
    req(df)
    if (!is.null(input$date_col) && nzchar(input$date_col) && !is.null(df[[input$date_col]])) {
      rng <- range(df[[input$date_col]], na.rm = TRUE)
      if (all(is.finite(rng))) {
        updateDateRangeInput(session, "daterange", start = rng[1], end = rng[2], min = rng[1], max = rng[2])
      }
    }
    if (!is.null(input$group_col) && nzchar(input$group_col) && !is.null(df[[input$group_col]])) {
      choices <- sort(unique(df[[input$group_col]]))
      updateSelectizeInput(session, "group_filter", choices = choices, server = TRUE)
    }
  })

  # Small helper under pickers
  output$filters_help <- renderUI({
    if (is.null(current())) return(NULL)
    helpText("Pick a date, group, and numeric column. Use the group filter to limit categories; otherwise all groups are included.")
  })

  # Filtered data
  filtered <- reactive({
    df <- current()
    req(df)

    if (!is.null(input$date_col) && nzchar(input$date_col) && !all(is.na(input$daterange))) {
      if (inherits(df[[input$date_col]], "Date") || inherits(df[[input$date_col]], "POSIXct")) {
        df <- df %>% filter(.data[[input$date_col]] >= input$daterange[1],
                            .data[[input$date_col]] <= input$daterange[2])
      }
    }
    if (!is.null(input$group_col) && nzchar(input$group_col) && length(input$group_filter) > 0) {
      df <- df %>% filter(.data[[input$group_col]] %in% input$group_filter)
    }
    df
  })

  # Time series (sum over groups)
  output$ts_plot <- renderPlot({
    df <- filtered()
    req(!is.null(input$date_col), !is.null(input$value_col), nzchar(input$date_col), nzchar(input$value_col))
    validate(need(is.numeric(df[[input$value_col]]), "Selected numeric column is not numeric."))
    df %>%
      filter(!is.na(.data[[input$date_col]])) %>%
      group_by(.data[[input$date_col]]) %>%
      summarise(value = sum(.data[[input$value_col]], na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = .data[[input$date_col]], y = value)) +
      geom_line() +
      labs(x = input$date_col, y = input$value_col, title = "Time series (sum per date)") +
      theme_minimal()
  })

  # Grouped summary (shared for table + plot)
  summary_tbl <- reactive({
    df <- filtered()
    req(!is.null(input$group_col), nzchar(input$group_col))
    kind <- input$stat_kind %||% "Sum"

    if (kind %in% c("Sum","Mean","Median")) {
      req(!is.null(input$value_col), nzchar(input$value_col))
      validate(need(is.numeric(df[[input$value_col]]), "Selected numeric column is not numeric."))

      df %>%
        group_by(.data[[input$group_col]]) %>%
        summarise(
          n = n(),
          stat = dplyr::case_when(
            kind == "Sum"    ~ sum(.data[[input$value_col]], na.rm = TRUE),
            kind == "Mean"   ~ mean(.data[[input$value_col]], na.rm = TRUE),
            kind == "Median" ~ median(.data[[input$value_col]], na.rm = TRUE),
            TRUE ~ NA_real_
          ),
          .groups = "drop"
        ) %>%
        arrange(desc(stat))
    } else {
      # Count non-NA of the chosen numeric (or if none chosen, just counts per group)
      if (!is.null(input$value_col) && nzchar(input$value_col) && input$value_col %in% names(df)) {
        df %>%
          group_by(.data[[input$group_col]]) %>%
          summarise(
            n = n(),
            stat = sum(!is.na(.data[[input$value_col]])),
            .groups = "drop"
          ) %>%
          arrange(desc(stat))
      } else {
        df %>%
          group_by(.data[[input$group_col]]) %>%
          summarise(n = n(), stat = n, .groups = "drop") %>%
          arrange(desc(stat))
      }
    }
  })

  # Grouped stats plot (above table)
  output$grp_plot <- renderPlot({
    tbl <- summary_tbl()
    req(nrow(tbl) > 0)
    show_n <- min(input$topn %||% 15, nrow(tbl))
    tbl <- tbl %>% slice_head(n = show_n)

    if ((input$orient %||% "Horizontal") == "Horizontal") {
      ggplot(tbl, aes(x = reorder(.data[[input$group_col]], stat), y = stat)) +
        geom_col() +
        coord_flip() +
        labs(x = input$group_col, y = input$stat_kind, title = paste(input$stat_kind, "by group")) +
        theme_minimal()
    } else {
      ggplot(tbl, aes(x = reorder(.data[[input$group_col]], -stat), y = stat)) +
        geom_col() +
        labs(x = input$group_col, y = input$stat_kind, title = paste(input$stat_kind, "by group")) +
        theme_minimal()
    }
  })

  # Grouped summary table
  output$tbl_summary <- renderDT({
    tbl <- summary_tbl()
    DT::datatable(tbl, rownames = FALSE, options = list(pageLength = 10))
  })

  # Raw data table
  output$tbl_raw <- renderDT({
    df <- filtered()
    DT::datatable(df, options = list(scrollX = TRUE, pageLength = 25))
  })

  # Downloads
  output$dl_summary <- downloadHandler(
    filename = function() sprintf("summary_%s.csv", Sys.Date()),
    content = function(file) {
      readr::write_csv(summary_tbl(), file)
    }
  )
  output$dl_raw <- downloadHandler(
    filename = function() sprintf("raw_%s.csv", Sys.Date()),
    content = function(file) {
      readr::write_csv(filtered(), file)
    }
  )
}

shinyApp(ui, server)
