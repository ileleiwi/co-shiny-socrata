# R/socrata_v2.R
socrata_v2_fetch <- function(view_id,
                             domain = "data.colorado.gov",
                             soql = "SELECT *",
                             page_size = 50000,
                             max_pages = 5L,
                             token_env = "SOCRATA_APP_TOKEN") {
  stopifnot(nchar(view_id) > 0)
  library(httr); library(jsonlite); library(glue); library(tibble); library(dplyr); library(stringr)

  base <- glue("https://{domain}/resource/{view_id}.json")
  tok  <- Sys.getenv(token_env, "")
  hdrs <- if (nzchar(tok)) add_headers("X-App-Token" = tok) else NULL

  out <- list()
  offset <- 0L

  has_limit  <- grepl("\\bLIMIT\\b",  soql, ignore.case = TRUE)
  has_offset <- grepl("\\bOFFSET\\b", soql, ignore.case = TRUE)

  for (i in seq_len(max_pages)) {
    # Build paging INTO the SoQL. Do NOT send $limit/$offset params when using $query.
    soql_page <- soql
    if (!has_limit)  soql_page <- paste(soql_page, sprintf("LIMIT %d", page_size))
    if (!has_offset && offset > 0L) soql_page <- paste(soql_page, sprintf("OFFSET %d", offset))

    url <- modify_url(base, query = list(`$query` = soql_page))
    resp <- GET(url, hdrs, timeout(60))

    if (http_error(resp)) {
      stop(sprintf("SODA v2 error: HTTP %s\nFirst 300 chars: %s",
                   status_code(resp),
                   substr(content(resp, as = "text", encoding = "UTF-8"), 1, 300)))
    }

    txt <- content(resp, as = "text", encoding = "UTF-8")
    if (!nzchar(txt) || !jsonlite::validate(txt)) stop("Non-JSON or empty v2 response")

    dat <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
    df  <- tibble::as_tibble(dat)

    # Best-effort numeric coercion (Socrata often returns numbers as strings)
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
