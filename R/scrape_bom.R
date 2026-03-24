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


  # Check for saved data file first
  data_file <- file.path("data", paste0("bom_", year, ".rds"))
  if (file.exists(data_file)) {
    cli::cli_alert_info("Using saved data for BOM {year}")
    return(readRDS(data_file))
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

  # Parse header
  header_row <- rows[1]
  headers <- rvest::html_text2(rvest::html_elements(header_row, "th"))
  headers <- tolower(stringr::str_trim(headers))

  # Parse data rows
  data_rows <- rows[-1]

  films <- purrr::map_dfr(data_rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    if (length(cells) < 5) return(NULL)

    # Extract release link for ID
    title_cell <- cells[2]  # Usually second column
    link <- rvest::html_element(title_cell, "a")

    if (is.null(link) || length(link) == 0) {
      # Try to find link in any cell
      link <- rvest::html_element(row, "a[href*='/release/']")
    }

    release_id <- NA_character_
    if (!is.null(link) && length(link) > 0) {
      href <- rvest::html_attr(link, "href")
      release_id <- extract_bom_release_id(href)
    }

    # Extract text from cells
    cell_text <- rvest::html_text2(cells)
    cell_text <- stringr::str_trim(cell_text)

    # Build row - columns vary by year/page format
    # Current BOM layout (as of 2024):
    # 1:Rank, 2:Release, 3:Genre, 4:Budget, 5:Runtime, 6:Gross,
    # 7:Theaters, 8:Total Gross, 9:Release Date, 10:Distributor, 11:Estimated
    tryCatch({
      n_cols <- length(cell_text)
      # Handle both old (no Estimated column) and new layouts
      has_estimated <- n_cols >= 11 && tolower(cell_text[n_cols]) %in% c("true", "false")

      if (has_estimated) {
        # New layout with Estimated column at end
        data.frame(
          release_id = release_id,
          title = cell_text[2],
          total_gross = parse_currency(cell_text[n_cols - 3]),
          theaters_max = parse_currency(cell_text[n_cols - 4]),
          distributor = cell_text[n_cols - 1],
          stringsAsFactors = FALSE
        )
      } else {
        # Old layout without Estimated column
        data.frame(
          release_id = release_id,
          title = cell_text[2],
          total_gross = parse_currency(cell_text[n_cols - 2]),
          theaters_max = parse_currency(cell_text[n_cols - 1]),
          distributor = cell_text[n_cols],
          stringsAsFactors = FALSE
        )
      }
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

  # Save to data folder
  if (save_to_data) {
    if (!dir.exists("data")) dir.create("data", recursive = TRUE)
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
    if (!is.null(cached_data)) {
      # Return NULL for cached failures (empty data frames with 0 rows)
      if (is.data.frame(cached_data) && nrow(cached_data) == 0) {
        return(NULL)
      }
      if (is.data.frame(cached_data)) {
        return(cached_data)
      }
    }
  }

  # Build URL
  url <- paste0(.boxoffice_env$bom_base_url, "/release/", release_id, "/")

  # Fetch page
  html <- fetch_html(url, delay = 1)
  if (is.null(html)) {
    cli::cli_warn("Failed to fetch daily data for {release_id}")
    # Cache failure as empty data frame so we don't retry
    write_cache("bom", "daily", release_id, data.frame())
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
    # Cache failure as empty data frame so we don't retry
    write_cache("bom", "daily", release_id, data.frame())
    return(NULL)
  }

  # Parse table rows
  rows <- rvest::html_elements(daily_table, "tr")

  if (length(rows) < 2) {
    cli::cli_warn("Empty daily table for {release_id}")
    # Cache failure as empty data frame so we don't retry
    write_cache("bom", "daily", release_id, data.frame())
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
    # Cache failure as empty data frame so we don't retry
    write_cache("bom", "daily", release_id, data.frame())
    return(NULL)
  }

  # Parse dates - need to infer year from the data
  # Try to extract year from cumulative pattern or metadata
  year <- extract_year(url)
  if (is.na(year)) {
    # Use current year as fallback
    year <- as.integer(format(Sys.Date(), "%Y"))
  }

  daily_data$date <- parse_date_flexible(daily_data$date_str, year)

  # If dates look wrong (future), try previous year
  if (any(daily_data$date > Sys.Date() + 365, na.rm = TRUE)) {
    daily_data$date <- parse_date_flexible(daily_data$date_str, year - 1)
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
#' @param progress Logical: show progress bar (default: TRUE)
#' @return List of data frames (named by release_id)
#' @export
#' @examples
#' \dontrun{
#' films <- c("rl1077904129", "rl3638199041")
#' daily_data <- scrape_bom_daily_batch(films)
#' }
scrape_bom_daily_batch <- function(release_ids, use_cache = TRUE, save_every = 500, progress = TRUE) {
  n <- length(release_ids)

  if (n == 0) {
    return(list())
  }

  # Load existing progress from data/
  data_file <- "data/bom_daily.rds"
  if (file.exists(data_file)) {
    existing <- readRDS(data_file)
    cli::cli_alert_info("Loaded {length(existing)} existing films from {data_file}")
  } else {
    existing <- list()
  }

  # Skip already-scraped films
  already_done <- names(existing)
  to_scrape <- setdiff(release_ids, already_done)

  if (length(to_scrape) == 0) {
    cli::cli_alert_success("All {n} films already scraped")
    return(existing[release_ids[release_ids %in% names(existing)]])
  }

  cli::cli_alert_info("Scraping {length(to_scrape)} new films ({length(already_done)} already done)")

  if (progress) {
    cli::cli_progress_bar(
      format = "Scraping daily data [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent}",
      total = length(to_scrape)
    )
  }

  results <- existing
  new_count <- 0

  for (i in seq_along(to_scrape)) {
    id <- to_scrape[i]
    results[[id]] <- tryCatch(
      scrape_bom_daily(id, use_cache = use_cache),
      error = function(e) {
        cli::cli_alert_danger("Error scraping {id}: {e$message}")
        NULL
      }
    )

    new_count <- new_count + 1

    # Save progress periodically
    if (save_every > 0 && new_count %% save_every == 0) {
      if (!dir.exists("data")) dir.create("data", recursive = TRUE)
      clean_results <- results[!vapply(results, is.null, logical(1))]
      saveRDS(clean_results, data_file)
      cli::cli_alert_success("Checkpoint: saved {length(clean_results)} films to {data_file}")
    }

    if (progress) {
      cli::cli_progress_update()
    }
  }

  if (progress) {
    cli::cli_progress_done()
  }

  # Remove NULL entries and final save
  results <- results[!vapply(results, is.null, logical(1))]

  if (save_every > 0) {
    if (!dir.exists("data")) dir.create("data", recursive = TRUE)
    saveRDS(results, data_file)
    cli::cli_alert_success("Saved {length(results)} films to {data_file}")
  }

  cli::cli_alert_success("Successfully scraped {length(results)}/{n} films")
  results
}
