# R/socrata_v2.R
socrata_v2_fetch <- function(view_id,
                             domain = "data.colorado.gov",
                             soql = "SELECT *",
                             page_size = 50000,
                             max_pages = 5L,
                             token_env = "SOCRATA_APP_TOKEN") {
  stopifnot(nchar(view_id) > 0)
  library(httr); library(jsonlite); library(glue); library(tibble); library(dplyr)

  base <- glue("https://{domain}/resource/{view_id}.json")
  tok  <- Sys.getenv(token_env, "")

  fetch_text <- function(resp) {
    # Prefer text; if not usable, fall back to raw->char
    txt <- try(httr::content(resp, as = "text", encoding = "UTF-8"), silent = TRUE)
    if (!inherits(txt, "try-error") && is.character(txt) && length(txt) == 1 && !is.na(txt)) return(txt)
    raw <- try(httr::content(resp, as = "raw"), silent = TRUE)
    if (!inherits(raw, "try-error") && length(raw) > 0) {
      return(tryCatch(rawToChar(raw), error = function(e) ""))
    }
    ""
  }

  out <- list()
  offset <- 0L

  has_limit  <- grepl("\\bLIMIT\\b",  soql, ignore.case = TRUE)
  has_offset <- grepl("\\bOFFSET\\b", soql, ignore.case = TRUE)

  for (i in seq_len(max_pages)) {
    # Put paging INSIDE SoQL; do NOT also send $limit/$offset when using $query.
    soql_page <- soql
    if (!has_limit)  soql_page <- paste(soql_page, sprintf("LIMIT %d", page_size))
    if (!has_offset && offset > 0L) soql_page <- paste(soql_page, sprintf("OFFSET %d", offset))

    url <- httr::modify_url(base, query = list(`$query` = soql_page))
    resp <- if (nzchar(tok)) {
      httr::GET(url, httr::add_headers("X-App-Token" = tok), httr::timeout(60))
    } else {
      httr::GET(url, httr::timeout(60))
    }

    status <- httr::status_code(resp)
    txt <- fetch_text(resp)

    if (httr::http_error(resp)) {
      stop(sprintf("SODA v2 HTTP %s. Body (first 300 chars): %s", status, substr(txt, 1, 300)))
    }
    if (!nzchar(txt)) stop("Empty v2 response")

    ok <- try(jsonlite::validate(txt), silent = TRUE)
    if (inherits(ok, "try-error") || isFALSE(ok)) {
      stop(sprintf("Non-JSON v2 body (status %s). First 300 chars: %s", status, substr(txt, 1, 300)))
    }

    dat <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
    df  <- tibble::as_tibble(dat)

    # Coerce number-like character columns
    if (nrow(df)) {
      numish <- vapply(df, function(x) is.character(x) && all(grepl("^[-+]?[0-9]*\\.?[0-9]+$", x[!is.na(x)])), logical(1))
      df[numish] <- lapply(df[numish], function(x) suppressWarnings(as.numeric(x)))
    }

    out[[length(out) + 1L]] <- df
    if (nrow(df) < page_size || has_limit) break
    offset <- offset + nrow(df)
  }

  if (!length(out)) return(tibble())
  dplyr::bind_rows(out)
}
