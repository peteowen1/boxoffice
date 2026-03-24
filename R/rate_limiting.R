#' Create a rate-limited scraping session
#'
#' Creates a session object for polite web scraping with built-in
#' rate limiting and user agent configuration.
#'
#' @param base_url Base URL for the session
#' @param delay_seconds Minimum delay between requests (default: 2)
#' @param user_agent User agent string (uses package default if NULL)
#' @return Session object (list with configuration)
#' @export
#' @examples
#' \dontrun{
#' session <- create_scrape_session("https://www.boxofficemojo.com")
#' }
create_scrape_session <- function(
    base_url,
    delay_seconds = 1,
    user_agent = NULL
) {
  if (is.null(user_agent)) {
    user_agent <- .boxoffice_env$user_agent
  }

  session <- list(
    base_url = base_url,
    delay_seconds = delay_seconds,
    user_agent = user_agent,
    last_request = NULL,
    request_count = 0
  )

  class(session) <- c("boxoffice_session", "list")
  session
}

#' Sleep with jitter between requests
#'
#' Adds random variation to delays to appear more human-like.
#'
#' @param base_delay Base delay in seconds
#' @param jitter_factor Fraction of base_delay for random jitter (default: 0.5)
#' @return Invisible NULL
#' @keywords internal
delay_with_jitter <- function(base_delay = 1, jitter_factor = 0.3) {
  jitter <- stats::runif(1, -jitter_factor, jitter_factor) * base_delay
  actual_delay <- max(0.3, base_delay + jitter)  # Minimum 0.3 seconds
  Sys.sleep(actual_delay)
  invisible(NULL)
}

#' Polite fetch with rate limiting
#'
#' Fetches a URL with rate limiting, retries, and caching.
#'
#' @param url Full URL to fetch
#' @param session Optional session object for rate limiting
#' @param use_cache Logical: use caching (default: TRUE)
#' @param cache_source Character: "bom" or "numbers"
#' @param cache_type Character: "yearly", "daily", or "metadata"
#' @param cache_key Character: cache key for this request
#' @param max_retries Maximum retry attempts (default: 3)
#' @return rvest html_document or NULL on failure
#' @keywords internal
polite_fetch <- function(
    url,
    session = NULL,
    use_cache = TRUE,
    cache_source = NULL,
    cache_type = NULL,
    cache_key = NULL,
    max_retries = 3
) {
  # Check cache first
  if (use_cache && !is.null(cache_source) && !is.null(cache_type) && !is.null(cache_key)) {
    cached <- read_cache(cache_source, cache_type, cache_key)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  # Get delay settings
  delay <- if (!is.null(session)) session$delay_seconds else 1
  user_agent <- if (!is.null(session)) session$user_agent else .boxoffice_env$user_agent

  # Rate limit: ensure delay since last request
  if (!is.null(session) && !is.null(session$last_request)) {
    elapsed <- as.numeric(difftime(Sys.time(), session$last_request, units = "secs"))
    if (elapsed < delay) {
      Sys.sleep(delay - elapsed)
    }
  }

  # Add jitter
  delay_with_jitter(delay * 0.25, 0.5)

  # Attempt fetch with retries
  result <- NULL
  last_error <- NULL

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch({
      response <- httr2::request(url) |>
        httr2::req_user_agent(user_agent) |>
        httr2::req_timeout(30) |>
        httr2::req_retry(max_tries = 1) |>
        httr2::req_perform()

      # Check status
      status <- httr2::resp_status(response)

      if (status == 200) {
        html <- httr2::resp_body_html(response)
        html
      } else if (status == 404) {
        cli::cli_alert_warning("Page not found: {url}")
        NULL
      } else if (status == 429) {
        # Rate limited - exponential backoff
        wait_time <- 2^attempt * 5
        cli::cli_alert_warning("Rate limited, waiting {wait_time}s...")
        Sys.sleep(wait_time)
        "RETRY"
      } else {
        cli::cli_alert_warning("HTTP {status} for: {url}")
        NULL
      }
    }, error = function(e) {
      last_error <<- e
      if (grepl("timeout|timed out", e$message, ignore.case = TRUE)) {
        cli::cli_alert_warning("Timeout, attempt {attempt}/{max_retries}: {url}")
        Sys.sleep(2^attempt)
        "RETRY"
      } else if (grepl("connection", e$message, ignore.case = TRUE)) {
        cli::cli_alert_warning("Connection error, attempt {attempt}/{max_retries}")
        Sys.sleep(2^attempt)
        "RETRY"
      } else {
        cli::cli_alert_danger("Error fetching {url}: {e$message}")
        NULL
      }
    })

    if (!identical(result, "RETRY")) {
      break
    }
  }

  if (identical(result, "RETRY")) {
    cli::cli_alert_danger("All retries exhausted for: {url}")
    result <- NULL
  }

  # Update session
  if (!is.null(session)) {
    session$last_request <- Sys.time()
    session$request_count <- session$request_count + 1
  }

  # Cache successful result
  if (!is.null(result) && use_cache && !is.null(cache_source)) {
    write_cache(cache_source, cache_type, cache_key, result)
  }

  result
}

#' Fetch URL and parse as HTML
#'
#' Simple wrapper for fetching and parsing HTML with rate limiting.
#'
#' @param url URL to fetch
#' @param delay Delay in seconds before fetch
#' @return rvest html_document or NULL
#' @keywords internal
fetch_html <- function(url, delay = 1) {
  delay_with_jitter(delay, 0.3)

  tryCatch({
    httr2::request(url) |>
      httr2::req_user_agent(.boxoffice_env$user_agent) |>
      httr2::req_timeout(30) |>
      httr2::req_perform() |>
      httr2::resp_body_html()
  }, error = function(e) {
    cli::cli_alert_danger("Failed to fetch {url}: {e$message}")
    NULL
  })
}

#' Safe scrape wrapper with error handling
#'
#' Wraps a scraping function with error handling and logging.
#'
#' @param scrape_fn Function to call
#' @param ... Arguments to pass to scrape_fn
#' @param .label Label for progress messages
#' @return Result of scrape_fn or NULL on error
#' @keywords internal
safe_scrape <- function(scrape_fn, ..., .label = "item") {
  tryCatch({
    scrape_fn(...)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to scrape {.label}: {e$message}")
    NULL
  })
}

#' Batch scrape with progress
#'
#' Applies a scraping function to multiple items with progress bar.
#'
#' @param items Vector of items to scrape
#' @param scrape_fn Function to apply to each item
#' @param ... Additional arguments to pass to scrape_fn
#' @param .progress Logical: show progress bar
#' @param .label Label for progress messages
#' @return List of results (named by items)
#' @keywords internal
batch_scrape <- function(items, scrape_fn, ..., .progress = TRUE, .label = "items") {
  n <- length(items)

  if (.progress && n > 1) {
    cli::cli_progress_bar(
      format = paste0("Scraping {.label} [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent} ETA: {cli::pb_eta}"),
      total = n
    )
  }

  results <- vector("list", n)
  names(results) <- items

  for (i in seq_along(items)) {
    results[[i]] <- safe_scrape(scrape_fn, items[i], ..., .label = items[i])

    if (.progress && n > 1) {
      cli::cli_progress_update()
    }
  }

  if (.progress && n > 1) {
    cli::cli_progress_done()
  }

  results
}
