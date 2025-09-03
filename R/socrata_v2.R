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
  hdrs <- if (nzchar(tok)) add_headers("X-App-Token" = tok) else NULL

  out <- list()
  offset <- 0L

  for (i in seq_len(max_pages)) {
    # You can use either $query or split params ($select, $where, …).
    # $query is easiest for “pasteable” SoQL.
    url <- modify_url(base, query = list(`$query` = soql,
                                         `$limit` = page_size,
                                         `$offset` = offset))
    resp <- GET(url, hdrs, timeout(60))
    stop_for_status(resp)

    txt <- content(resp, as = "text", encoding = "UTF-8")
    if (!nzchar(txt) || !jsonlite::validate(txt)) stop("Non-JSON or empty v2 response")
    dat <- fromJSON(txt, simplifyVector = TRUE)
    df  <- as_tibble(dat)

    out[[length(out) + 1L]] <- df
    if (nrow(df) < page_size) break
    offset <- offset + nrow(df)
  }

  if (!length(out)) return(tibble())
  bind_rows(out)
}

