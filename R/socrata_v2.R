# R/socrata_v2.R
socrata_v2_fetch <- function(view_id,
                             domain    = "data.colorado.gov",
                             soql      = "SELECT *",
                             page_size = 50000,
                             max_pages = 5L,
                             token_env = "SOCRATA_APP_TOKEN") {

  stopifnot(nchar(view_id) > 0)
  library(httr); library(jsonlite); library(glue); library(tibble); library(dplyr)

  base <- glue("https://{domain}/resource/{view_id}.json")
  tok  <- Sys.getenv(token_env, "")

  read_text <- function(resp) {
    raw <- httr::content(resp, as = "raw")
    if (length(raw) == 0) return("")
    txt <- rawToChar(raw)
    Encoding(txt) <- "UTF-8"
    txt
  }

  out <- list()
  offset <- 0L

  has_limit  <- grepl("\\bLIMIT\\b",  soql, ignore.case = TRUE)
  has_offset <- grepl("\\bOFFSET\\b", soql, ignore.case = TRUE)

  for (i in seq_len(max_pages)) {
    soql_page <- soql
    if (!has_limit)                 soql_page <- paste(soql_page, sprintf("LIMIT %d", page_size))
    if (!has_offset && offset > 0L) soql_page <- paste(soql_page, sprintf("OFFSET %d", offset))

    url  <- httr::modify_url(base, query = list(`$query` = soql_page))
    hdrs <- if (nzchar(tok)) httr::add_headers("X-App-Token" = tok) else NULL
    resp <- httr::GET(url, hdrs, httr::timeout(60))

    status <- httr::status_code(resp)
    txt <- read_text(resp)

    if (httr::http_error(resp)) {
      stop(sprintf("SODA v2 HTTP %s. Body[0:300]: %s", status, substr(txt, 1, 300)))
    }
    if (!nzchar(txt)) stop(sprintf("Empty SODA v2 body (HTTP %s).", status))

    dat <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE),
                    error = function(e) stop(sprintf(
                      "JSON parse failed (HTTP %s). Body[0:300]: %s",
                      status, substr(txt, 1, 300))))

    df <- tibble::as_tibble(dat)

    # Coerce number-like character columns to numeric (best-effort)
    if (nrow(df)) {
      numish <- vapply(df, function(x) is.character(x) && any(nzchar(x)),
                       logical(1), USE.NAMES = FALSE)
      if (any(numish)) {
        for (j in which(numish)) {
          y <- suppressWarnings(as.numeric(df[[j]]))
          if (sum(!is.na(y)) >= 0.6 * length(y)) df[[j]] <- y
        }
      }
    }

    out[[length(out) + 1L]] <- df
    if (nrow(df) < page_size || has_limit) break
    offset <- offset + nrow(df)
  }

  if (!length(out)) return(tibble())
  dplyr::bind_rows(out)
}
