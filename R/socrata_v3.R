# R/socrata_v3.R
socrata_v3_fetch <- function(view_id,
                             domain = "data.colorado.gov",
                             soql = "SELECT *",
                             page_size = 50000,
                             max_pages = 5L,
                             include_system = FALSE,
                             token_env = "SOCRATA_APP_TOKEN",
                             fallback_v2_on_error = TRUE) {
  stopifnot(nchar(view_id) > 0)
  library(httr); library(jsonlite); library(dplyr); library(purrr); library(glue); library(tibble)

  base <- glue("https://{domain}/api/v3/views/{view_id}/query.json")
  app_token <- Sys.getenv(token_env, unset = NA_character_)
  headers <- c("Content-Type" = "application/json", "Accept" = "application/json")
  if (!is.na(app_token) && nchar(app_token) > 0) headers <- c(headers, "X-App-Token" = app_token)

  page_num <- 1L
  out <- list()

  parse_rows <- function(payload) {
    # Handle common v3 shapes:
    #  A) payload$data$rows (array-of-arrays) + col names in payload$data$columns$name or $fieldName
    #  B) payload$data (array-of-objects)
    #  C) payload$rows at top level
    if (is.list(payload$data) && !is.null(payload$data$rows)) {
      rows <- payload$data$rows
      cols <- payload$data$columns
      cn <- NULL
      if (!is.null(cols)) {
        if (!is.null(cols$name)) cn <- unlist(cols$name, use.names = FALSE)
        if (is.null(cn) && !is.null(cols$fieldName)) cn <- unlist(cols$fieldName, use.names = FALSE)
      }
      df <- as_tibble(do.call(rbind, lapply(rows, \(r) unlist(r, recursive = FALSE))))
      if (!is.null(cn) && length(cn) == ncol(df)) names(df) <- cn
      return(df)
    }

    if (is.list(payload$data) && length(payload$data) > 0 && is.null(payload$data$rows)) {
      # array-of-objects
      return(map_dfr(payload$data, \(x) as_tibble(x)))
    }

    if (!is.null(payload$rows)) {
      df <- as_tibble(do.call(rbind, lapply(payload$rows, \(r) unlist(r, recursive = FALSE))))
      return(df)
    }

    # Unknown shape
    stop("Unexpected SODA v3 response shape; keys: ", paste(names(payload), collapse = ", "))
  }

  repeat {
    body <- list(
      query = soql,
      page  = list(pageNumber = page_num, pageSize = page_size),
      includeSynthetic = isTRUE(include_system)
    )
    resp <- try(POST(base,
                     add_headers(.headers = headers),
                     body = toJSON(body, auto_unbox = TRUE, null = "null"),
                     encode = "raw",
                     timeout(60)),
                silent = TRUE)

    if (inherits(resp, "try-error") || http_error(resp)) {
      msg <- if (inherits(resp, "try-error")) as.character(resp) else paste(status_code(resp), content(resp, "text"))
      if (isTRUE(fallback_v2_on_error)) {
        message("v3 query failed (", msg, "). Falling back to v2 /resource …")
        # v2 fallback: GET /resource/<id>.json?$limit=...
        v2_url <- glue("https://{domain}/resource/{view_id}.json?$limit={page_size}&$offset={(page_num-1L)*page_size}")
        if (!is.na(app_token) && nchar(app_token) > 0) {
          resp2 <- GET(v2_url, add_headers("X-App-Token" = app_token), timeout(60))
        } else {
          resp2 <- GET(v2_url, timeout(60))
        }
        stop_for_status(resp2)
        dat <- fromJSON(content(resp2, "text", encoding = "UTF-8"), simplifyVector = TRUE)
        df <- as_tibble(dat)
      } else {
        stop("SODA v3 request failed: ", msg)
      }
    } else {
      payload <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
      if (!is.null(payload$errorCode) || !is.null(payload$error)) {
        stop("SODA v3 error: ", paste(c(payload$errorCode, payload$message, payload$error), collapse = " | "))
      }
      df <- parse_rows(payload)
    }

    out[[length(out) + 1L]] <- df
    if (nrow(df) < page_size || page_num >= max_pages) break
    page_num <- page_num + 1L
  }

  if (!length(out)) return(tibble())
  bind_rows(out)
}
