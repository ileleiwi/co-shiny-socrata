socrata_v3_fetch <- function(view_id,
                             domain = "data.colorado.gov",
                             soql = "SELECT *",
                             page_size = 50000,
                             max_pages = 50L,
                             include_system = FALSE,
                             token_env = "SOCRATA_APP_TOKEN") {
  stopifnot(nchar(view_id) > 0)
  library(httr); library(jsonlite); library(dplyr); library(purrr); library(glue)

  url <- glue("https://{domain}/api/v3/views/{view_id}/query.json")
  app_token <- Sys.getenv(token_env, unset = NA_character_)
  headers <- c("Content-Type" = "application/json")
  if (!is.na(app_token) && nchar(app_token) > 0) {
    headers <- c(headers, "X-App-Token" = app_token)
  }

  page_num <- 1L
  results <- list()

  repeat {
    body <- list(
      query = soql,
      page  = list(pageNumber = page_num, pageSize = page_size),
      includeSynthetic = isTRUE(include_system)
    )
    resp <- httr::POST(url, httr::add_headers(.headers = headers), body = jsonlite::toJSON(body, auto_unbox = TRUE))
    httr::stop_for_status(resp)
    payload <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))

    # v3 wraps results under "data" -> "rows" or similar; normalize to data.frame
    if (!is.list(payload) || is.null(payload$data)) {
      warning("Unexpected response shape; returning raw payload")
      return(payload)
    }
    rows <- payload$data
    if (length(rows) == 0) break

    # Flatten into a tibble; v3 columns may come back as list-columns; simplify where possible
    df <- tibble::as_tibble(rows)
    results[[length(results) + 1L]] <- df

    if (nrow(df) < page_size || page_num >= max_pages) break
    page_num <- page_num + 1L
  }

  if (length(results) == 0) return(tibble::tibble())
  dplyr::bind_rows(results)
}
