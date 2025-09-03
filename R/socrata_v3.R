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
  headers <- c("Accept" = "application/json")
  if (!is.na(app_token) && nchar(app_token) > 0) headers <- c(headers, "X-App-Token" = app_token)

  fetch_text <- function(resp) {
    txt <- try(httr::content(resp, as = "text", encoding = "UTF-8"), silent = TRUE)
    if (inherits(txt, "try-error") || !is.character(txt) || length(txt) != 1 || is.na(txt)) {
      raw <- try(httr::content(resp, as = "raw"), silent = TRUE)
      if (!inherits(raw, "try-error") && length(raw) > 0) {
        txt <- tryCatch(rawToChar(raw), error = function(e) "")
      } else txt <- ""
    }
    txt
  }

  parse_rows <- function(payload) {
    # A) payload$data$rows + payload$data$columns
    if (is.list(payload) && !is.null(payload$data) && !is.null(payload$data$rows)) {
      rows <- payload$data$rows
      cols <- payload$data$columns
      cn <- NULL
      if (!is.null(cols)) {
        if (!is.null(cols$name))      cn <- unlist(cols$name,      use.names = FALSE)
        if (is.null(cn) && !is.null(cols$fieldName))
          cn <- unlist(cols$fieldName, use.names = FALSE)
      }
      df <- as_tibble(do.call(rbind, lapply(rows, function(r) unlist(r, recursive = FALSE))))
      if (!is.null(cn) && length(cn) == ncol(df)) names(df) <- cn
      return(df)
    }

    # B) payload$data is array-of-objects
    if (is.list(payload) && !is.null(payload$data) && is.list(payload$data) && is.null(payload$data$rows)) {
      return(map_dfr(payload$data, function(x) as_tibble(x)))
    }

    # C) payload$rows at top level
    if (is.list(payload) && !is.null(payload$rows)) {
      df <- as_tibble(do.call(rbind, lapply(payload$rows, function(r) unlist(r, recursive = FALSE))))
      return(df)
    }

    # D) **Top-level array-of-objects** (no named keys at all)
    if (is.list(payload) && is.null(names(payload)) && length(payload) > 0 && is.list(payload[[1]])) {
      return(map_dfr(payload, function(x) as_tibble(x)))
    }

    stop("Unexpected SODA v3 response shape; keys: ",
         if (is.null(names(payload))) "(none)" else paste(names(payload), collapse = ", "))
  }

  v2_fetch <- function(page_num) {
    v2_url <- glue("https://{domain}/resource/{view_id}.json?$limit={page_size}&$offset={(page_num-1L)*page_size}")
    if (!is.na(app_token) && nchar(app_token) > 0) {
      resp2 <- GET(v2_url, add_headers("X-App-Token" = app_token), timeout(60))
    } else {
      resp2 <- GET(v2_url, timeout(60))
    }
    stop_for_status(resp2)
    txt2 <- fetch_text(resp2)
    if (!nzchar(txt2)) stop("Empty body from Socrata v2 fallback.")
    dat  <- fromJSON(txt2, simplifyVector = TRUE)
    as_tibble(dat)
  }

  page_num <- 1L
  out <- list()

  repeat {
    body <- list(
      query = soql,
      page  = list(pageNumber = page_num, pageSize = page_size),
      includeSynthetic = isTRUE(include_system)
    )

    resp <- try(POST(base,
                     add_headers(.headers = headers),
                     body   = body,
                     encode = "json",
                     timeout(60)),
                silent = TRUE)

    if (inherits(resp, "try-error") || http_error(resp)) {
      msg <- if (inherits(resp, "try-error")) as.character(resp) else paste(status_code(resp), fetch_text(resp))
      if (isTRUE(fallback_v2_on_error)) {
        message("v3 request failed (", msg, "). Falling back to v2 /resource …")
        df <- v2_fetch(page_num)
      } else {
        stop("SODA v3 request failed: ", msg)
      }
    } else {
      txt <- fetch_text(resp)
      if (!nzchar(txt)) {
        if (isTRUE(fallback_v2_on_error)) {
          message("Empty v3 body. Falling back to v2 /resource …")
          df <- v2_fetch(page_num)
        } else {
          stop("Empty body from Socrata v3.")
        }
      } else if (!isTRUE(jsonlite::validate(txt))) {
        if (isTRUE(fallback_v2_on_error)) {
          message("Non-JSON v3 body. Falling back to v2 /resource …")
          df <- v2_fetch(page_num)
        } else {
          stop("Non-JSON body from v3: ", substr(txt, 1, 300), " …")
        }
      } else {
        payload <- fromJSON(txt, simplifyVector = FALSE)
        # Try to parse; if it fails, fallback to v2
        df <- tryCatch(parse_rows(payload), error = function(e) {
          if (isTRUE(fallback_v2_on_error)) {
            message("v3 parse failed (", conditionMessage(e), "). Falling back to v2 /resource …")
            return(v2_fetch(page_num))
          }
          stop(e)
        })
      }
    }

    out[[length(out) + 1L]] <- df
    if (nrow(df) < page_size || page_num >= max_pages) break
    page_num <- page_num + 1L
  }

  if (!length(out)) return(tibble())
  bind_rows(out)
}
