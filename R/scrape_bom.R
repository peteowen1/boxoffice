#' Scrape yearly film index from Box Office Mojo
#'
#' Retrieves the list of films for a given year from Box Office Mojo,
#' including basic metadata like title, gross, and release date.
#'
#' @param year Integer year (1980-current)
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @param save_to_data Logical: save to data/ folder (default: FALSE)
#' @return Data frame with columns:
#'   - release_id: BOM release identifier
#'   - title: Film title
#'   - year: Release year
#'   - distributor: Distribution company
#'   - total_gross: Domestic total gross
#'   - opening_gross: Opening weekend gross
#'   - opening_theaters: Opening theater count
#'   - release_date: Release date
#' @export
#' @examples
#' \dontrun{
#' films_2023 <- scrape_bom_yearly_index(2023)
#' }
scrape_bom_yearly_index <- function(year, use_cache = TRUE, save_to_data = FALSE) {
  year <- as.integer(year)

  # Validate year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (year < 1980 || year > current_year + 1) {
    cli::cli_abort("Year must be between 1980 and {current_year + 1}")
  }


  # Check cache
  cache_key <- as.character(year)
  expires <- if (year >= current_year) 7 else NULL

  if (use_cache && is_cached("bom", "yearly", cache_key, expires_days = expires)) {
    cached_data <- read_cache("bom", "yearly", cache_key)
    if (!is.null(cached_data) && is.data.frame(cached_data)) {
      cli::cli_alert_info("Using cached data for BOM {year}")
      return(cached_data)
    }
  }

  # Build URL
  url <- paste0(.boxoffice_env$bom_base_url, "/year/", year, "/")
  cli::cli_alert_info("Fetching BOM yearly index for {year}...")

  # Fetch page
  html <- fetch_html(url, delay = 1)
  if (is.null(html)) {
    cli::cli_abort("Failed to fetch yearly index for {year}")
  }

  # Parse table
  # The main table has class "a-bordered" or similar
  tables <- rvest::html_elements(html, "table")

  if (length(tables) == 0) {
    cli::cli_abort("No tables found on page for {year}")
  }

  # Find the main data table (usually the largest one)
  main_table <- NULL
  max_rows <- 0

  for (tbl in tables) {
    rows <- rvest::html_elements(tbl, "tr")
    if (length(rows) > max_rows) {
      max_rows <- length(rows)
      main_table <- tbl
    }
  }

  if (is.null(main_table) || max_rows < 2) {
    cli::cli_abort("Could not find data table for {year}")
  }

  # Extract rows
  rows <- rvest::html_elements(main_table, "tr")

  # Parse header — use names to find columns instead of positional indexing
  header_row <- rows[1]
  headers <- rvest::html_text2(rvest::html_elements(header_row, "th"))
  headers <- tolower(stringr::str_trim(headers))

  # Build a column index map using fuzzy header matching
  find_col <- function(patterns) {
    for (p in patterns) {
      idx <- grep(p, headers)
      if (length(idx) > 0) return(idx[1])
    }
    NA_integer_
  }

  col_title <- find_col(c("^release$", "title", "movie"))
  col_gross <- find_col(c("total gross", "lifetime gross", "^gross$"))
  col_theaters <- find_col(c("theater", "theatre"))
  col_distributor <- find_col(c("distributor", "studio"))

  # Parse data rows
  data_rows <- rows[-1]

  films <- purrr::map_dfr(data_rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    if (length(cells) < 5) return(NULL)

    # Extract release link for ID
    link <- rvest::html_element(row, "a[href*='/release/']")
    release_id <- NA_character_
    if (!is.null(link) && length(link) > 0) {
      href <- rvest::html_attr(link, "href")
      release_id <- extract_bom_release_id(href)
    }

    cell_text <- rvest::html_text2(cells)
    cell_text <- stringr::str_trim(cell_text)
    n_cols <- length(cell_text)

    tryCatch({
      # Use header-mapped indices when available, fall back to heuristics
      title_val <- if (!is.na(col_title) && col_title <= n_cols) {
        cell_text[col_title]
      } else {
        cell_text[min(2, n_cols)]
      }

      gross_val <- if (!is.na(col_gross) && col_gross <= n_cols) {
        parse_currency(cell_text[col_gross])
      } else {
        # Fallback: find last $-prefixed value
        dollar_idx <- grep("^\\$", cell_text)
        if (length(dollar_idx) > 0) parse_currency(cell_text[dollar_idx[length(dollar_idx)]]) else NA_real_
      }

      theaters_val <- if (!is.na(col_theaters) && col_theaters <= n_cols) {
        parse_currency(cell_text[col_theaters])
      } else {
        NA_real_
      }

      dist_val <- if (!is.na(col_distributor) && col_distributor <= n_cols) {
        cell_text[col_distributor]
      } else {
        NA_character_
      }

      data.frame(
        release_id = release_id,
        title = title_val,
        total_gross = gross_val,
        theaters_max = theaters_val,
        distributor = dist_val,
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  })

  if (nrow(films) == 0) {
    cli::cli_abort("No films parsed for {year}")
  }

  # Add year
  films$year <- year

  # Remove rows with no release_id
  films <- films[!is.na(films$release_id), ]

  # Cache result
  write_cache("bom", "yearly", cache_key, films)

  # Save to data folder (for use from data-raw scripts)
  if (save_to_data) {
    data_dir <- file.path("data")
    if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
    data_file <- file.path(data_dir, paste0("bom_", year, ".rds"))
    saveRDS(films, data_file)
    cli::cli_alert_success("Saved to {data_file}")
  }

  cli::cli_alert_success("Found {nrow(films)} films for {year}")
  films
}

#' Scrape daily box office data for a single film from Box Office Mojo
#'
#' Retrieves the complete daily box office breakdown for a film.
#'
#' @param release_id Box Office Mojo release ID (e.g., "rl1077904129")
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @return Data frame with daily breakdown:
#'   - date: Date
#'   - day_of_week: Day name (Friday, Saturday, etc.)
#'   - day_number: Sequential day number (1 = opening day)
#'   - rank: Daily box office rank
#'   - daily_gross: Daily gross revenue
#'   - pct_change_yesterday: Percent change from previous day
#'   - pct_change_last_week: Percent change from same day last week
#'   - theaters: Number of theaters
#'   - per_theater_avg: Per-theater average
#'   - cumulative_gross: Running total gross
#' @export
#' @examples
#' \dontrun{
#' # Barbie (2023)
#' barbie_daily <- scrape_bom_daily("rl1077904129")
#' }
scrape_bom_daily <- function(release_id, use_cache = TRUE) {
  if (is.na(release_id) || !grepl("^rl\\d+$", release_id)) {
    cli::cli_abort("Invalid release_id: {release_id}")
  }


  # Check cache (daily data never expires - it's historical)
  if (use_cache && is_cached("bom", "daily", release_id, expires_days = NULL)) {
    cached_data <- read_cache("bom", "daily", release_id)
    if (!is.null(cached_data) && is.data.frame(cached_data) && nrow(cached_data) > 0) {
      return(cached_data)
    }
  }

  # Skip if a recent failure is cached (retry after 1 day)
  if (use_cache && is_cached_failure("bom", "daily", release_id)) {
    return(NULL)
  }

  # Build URL
  url <- paste0(.boxoffice_env$bom_base_url, "/release/", release_id, "/")

  # Fetch page
  html <- fetch_html(url, delay = 1)
  if (is.null(html)) {
    cli::cli_warn("Failed to fetch daily data for {release_id}")
    cache_failure("bom", "daily", release_id)
    return(NULL)
  }

  # Find daily box office table
  # Look for table with "Daily" header or containing date columns
  tables <- rvest::html_elements(html, "table")

  daily_table <- NULL
  for (tbl in tables) {
    # Check if this table has date-like content
    text <- rvest::html_text2(tbl)
    if (stringr::str_detect(text, "(?i)(monday|tuesday|wednesday|thursday|friday|saturday|sunday)") &&
        stringr::str_detect(text, "(?i)(gross|daily)")) {
      daily_table <- tbl
      break
    }
  }

  if (is.null(daily_table)) {
    cli::cli_warn("No daily table found for {release_id}")
    cache_failure("bom", "daily", release_id)
    return(NULL)
  }

  # Parse table rows
  rows <- rvest::html_elements(daily_table, "tr")

  if (length(rows) < 2) {
    cli::cli_warn("Empty daily table for {release_id}")
    cache_failure("bom", "daily", release_id)
    return(NULL)
  }

  # Skip header row
  data_rows <- rows[-1]

  # Parse each row
  daily_data <- purrr::map_dfr(data_rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    if (length(cells) < 8) return(NULL)

    cell_text <- rvest::html_text2(cells)
    cell_text <- stringr::str_trim(cell_text)

    tryCatch({
      # Column order: Date, DOW, Rank, Daily, %YD, %LW, Theaters, Avg, To Date, Day, Est
      data.frame(
        date_str = cell_text[1],
        day_of_week = cell_text[2],
        rank = as.integer(gsub("[^0-9]", "", cell_text[3])),
        daily_gross = parse_currency(cell_text[4]),
        pct_change_yesterday = parse_percentage(cell_text[5]),
        pct_change_last_week = parse_percentage(cell_text[6]),
        theaters = parse_currency(cell_text[7]),
        per_theater_avg = parse_currency(cell_text[8]),
        cumulative_gross = parse_currency(cell_text[9]),
        day_number = as.integer(gsub("[^0-9]", "", cell_text[10])),
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  })

  if (nrow(daily_data) == 0) {
    cli::cli_warn("No daily data parsed for {release_id}")
    cache_failure("bom", "daily", release_id)
    return(NULL)
  }

  # Parse dates - try full date parsing first, then infer year from data
  daily_data$date <- parse_date_flexible(daily_data$date_str, year = NULL)

  # If dates didn't parse (short format like "Jun 14"), try extracting year
  # from the date strings themselves first, then from the page title
  if (all(is.na(daily_data$date))) {
    year <- extract_year(daily_data$date_str[1])
    if (is.na(year)) {
      # Try to find a year anywhere in the date strings
      all_years <- stringr::str_extract(daily_data$date_str, "\\b(19|20)\\d{2}\\b")
      all_years <- as.integer(all_years[!is.na(all_years)])
      if (length(all_years) > 0) {
        year <- all_years[1]
      } else {
        # Last resort: extract from page title text if available
        year <- as.integer(format(Sys.Date(), "%Y"))
      }
    }
    daily_data$date <- parse_date_flexible(daily_data$date_str, year)

    # If dates look wrong (future), try previous year
    if (any(daily_data$date > Sys.Date() + 365, na.rm = TRUE)) {
      daily_data$date <- parse_date_flexible(daily_data$date_str, year - 1)
    }
  }

  # Remove temporary column
  daily_data$date_str <- NULL

  # Add release_id
  daily_data$release_id <- release_id

  # Sort by day number
  daily_data <- daily_data[order(daily_data$day_number), ]

  # Cache result
  write_cache("bom", "daily", release_id, daily_data)

  daily_data
}

#' Scrape film metadata from Box Office Mojo
#'
#' Retrieves metadata for a film including title, distributor, rating, etc.
#'
#' @param release_id Box Office Mojo release ID
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @return List with metadata fields:
#'   - title: Film title
#'   - release_date: Release date
#'   - mpaa_rating: MPAA rating (G, PG, PG-13, R, etc.)
#'   - runtime: Runtime in minutes
#'   - genres: Character vector of genres
#'   - distributor: Distribution company
#'   - opening_gross: Opening weekend gross
#'   - opening_theaters: Opening theater count
#'   - widest_release: Maximum theater count
#'   - domestic_gross: Total domestic gross
#'   - international_gross: Total international gross
#'   - worldwide_gross: Total worldwide gross
#' @export
#' @examples
#' \dontrun{
#' meta <- scrape_bom_metadata("rl1077904129")
#' }
scrape_bom_metadata <- function(release_id, use_cache = TRUE) {
  if (is.na(release_id) || !grepl("^rl\\d+$", release_id)) {
    cli::cli_abort("Invalid release_id: {release_id}")
  }

  # Check cache
  if (use_cache && is_cached("bom", "metadata", release_id, expires_days = 30)) {
    cached_data <- read_cache("bom", "metadata", release_id)
    if (!is.null(cached_data) && is.list(cached_data)) {
      return(cached_data)
    }
  }

  # Build URL
  url <- paste0(.boxoffice_env$bom_base_url, "/release/", release_id, "/")

  # Fetch page
  html <- fetch_html(url, delay = 1)
  if (is.null(html)) {
    cli::cli_warn("Failed to fetch metadata for {release_id}")
    return(NULL)
  }

  # Initialize metadata list
  metadata <- list(
    release_id = release_id,
    title = NA_character_,
    release_date = as.Date(NA),
    mpaa_rating = NA_character_,
    runtime = NA_integer_,
    genres = character(0),
    distributor = NA_character_,
    opening_gross = NA_real_,
    opening_theaters = NA_integer_,
    widest_release = NA_integer_,
    domestic_gross = NA_real_,
    international_gross = NA_real_,
    worldwide_gross = NA_real_
  )

  # Extract title from h1
  title_elem <- rvest::html_element(html, "h1")
  if (!is.null(title_elem) && length(title_elem) > 0) {
    metadata$title <- stringr::str_trim(rvest::html_text2(title_elem))
  }

  # Look for summary elements - these contain key metrics
  # BOM uses spans with specific class names or data attributes
  summary_spans <- rvest::html_elements(html, "span.a-size-medium, span.money, div.a-section span")

  # Extract from summary section
  page_text <- rvest::html_text2(html)

  # Parse key values using regex on page text
  # Distributor
  dist_match <- stringr::str_match(page_text, "(?i)distributor[:\\s]+([^\\n]+)")
  if (!is.na(dist_match[1, 2])) {
    metadata$distributor <- stringr::str_trim(dist_match[1, 2])
  }

  # MPAA Rating
  rating_match <- stringr::str_match(page_text, "(?i)MPAA[:\\s]+([A-Z0-9-]+)")
  if (!is.na(rating_match[1, 2])) {
    metadata$mpaa_rating <- stringr::str_trim(rating_match[1, 2])
  }

  # Runtime
  runtime_match <- stringr::str_match(page_text, "(\\d+)\\s*hr\\s*(\\d+)?\\s*min")
  if (!is.na(runtime_match[1, 1])) {
    hrs <- as.integer(runtime_match[1, 2])
    mins <- as.integer(ifelse(is.na(runtime_match[1, 3]), 0, runtime_match[1, 3]))
    metadata$runtime <- hrs * 60 + mins
  }

  # Opening
  opening_match <- stringr::str_match(page_text, "(?i)opening[:\\s]+\\$([0-9,]+)")
  if (!is.na(opening_match[1, 2])) {
    metadata$opening_gross <- parse_currency(opening_match[1, 2])
  }

  # Domestic total
  domestic_match <- stringr::str_match(page_text, "(?i)domestic[:\\s]+\\$([0-9,]+)")
  if (!is.na(domestic_match[1, 2])) {
    metadata$domestic_gross <- parse_currency(domestic_match[1, 2])
  }

  # Worldwide
  worldwide_match <- stringr::str_match(page_text, "(?i)worldwide[:\\s]+\\$([0-9,]+)")
  if (!is.na(worldwide_match[1, 2])) {
    metadata$worldwide_gross <- parse_currency(worldwide_match[1, 2])
  }

  # Genres
  genre_match <- stringr::str_match(page_text, "(?i)genres?[:\\s]+([^\\n]+)")
  if (!is.na(genre_match[1, 2])) {
    genres <- stringr::str_split(genre_match[1, 2], "[,/]")[[1]]
    metadata$genres <- stringr::str_trim(genres)
  }

  # Cache result
  write_cache("bom", "metadata", release_id, metadata)

  metadata
}

#' Batch scrape daily data for multiple films from Box Office Mojo
#'
#' @param release_ids Character vector of release IDs
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @param save_every Integer: save progress to data/ every N films (default: 500, 0 to disable)
#' @param checkpoint_file Optional file path for checkpoint saves (NULL to disable)
#' @param progress Logical: show progress bar (default: TRUE)
#' @return List of data frames (named by release_id)
#' @export
#' @examples
#' \dontrun{
#' films <- c("rl1077904129", "rl3638199041")
#' daily_data <- scrape_bom_daily_batch(films)
#' }
scrape_bom_daily_batch <- function(release_ids, use_cache = TRUE, save_every = 500,
                                   checkpoint_file = NULL, progress = TRUE) {
  batch_scrape_daily(
    ids = release_ids,
    scrape_fn = scrape_bom_daily,
    use_cache = use_cache,
    checkpoint_file = checkpoint_file,
    save_every = save_every,
    progress = progress
  )
}
