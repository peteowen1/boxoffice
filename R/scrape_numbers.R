#' Scrape yearly film index from The Numbers
#'
#' Retrieves the list of films for a given year from The Numbers,
#' including budget information when available.
#'
#' @param year Integer year (1980-current)
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @param save_to_data Logical: save to data/ folder (default: FALSE)
#' @return Data frame with columns:
#'   - movie_slug: The Numbers movie identifier (URL slug)
#'   - title: Film title
#'   - year: Release year
#'   - distributor: Distribution company
#'   - domestic_gross: Domestic total gross
#'   - opening_gross: Opening weekend gross
#'   - opening_theaters: Opening theater count
#'   - budget: Production budget (when available)
#' @export
#' @examples
#' \dontrun{
#' films_2023 <- scrape_numbers_yearly_index(2023)
#' }
scrape_numbers_yearly_index <- function(year, use_cache = TRUE, save_to_data = FALSE) {
  year <- as.integer(year)

  # Validate year
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (year < 1980 || year > current_year + 1) {
    cli::cli_abort("Year must be between 1980 and {current_year + 1}")
  }

  # Check for saved data file first
  data_file <- file.path("data", paste0("numbers_", year, ".rds"))
  if (file.exists(data_file)) {
    cli::cli_alert_info("Using saved data for Numbers {year}")
    return(readRDS(data_file))
  }

  # Check cache
  cache_key <- as.character(year)
  expires <- if (year >= current_year) 7 else NULL

  if (use_cache && is_cached("numbers", "yearly", cache_key, expires_days = expires)) {
    cached_data <- read_cache("numbers", "yearly", cache_key)
    if (!is.null(cached_data) && is.data.frame(cached_data)) {
      cli::cli_alert_info("Using cached data for Numbers {year}")
      return(cached_data)
    }
  }

  cli::cli_alert_info("Fetching The Numbers yearly index for {year}...")

  # The Numbers uses pagination (100 per page)
  base_url <- paste0(
    .boxoffice_env$numbers_base_url,
    "/box-office-records/domestic/all-movies/cumulative/released-in-",
    year
  )

  all_films <- data.frame()
  page_start <- 1

  repeat {
    # Build page URL
    if (page_start == 1) {
      url <- base_url
    } else {
      url <- paste0(base_url, "/", page_start)
    }

    # Fetch page
    html <- fetch_html(url, delay = 1)
    if (is.null(html)) {
      if (page_start == 1) {
        cli::cli_abort("Failed to fetch yearly index for {year}")
      }
      break
    }

    # Find the main data table - look for table with many movie links
    tables <- rvest::html_elements(html, "table")
    if (length(tables) == 0) {
      if (page_start == 1) {
        cli::cli_abort("No tables found on page for {year}")
      }
      break
    }

    # Find the table that contains movie links
    main_table <- NULL
    for (tbl in tables) {
      links <- rvest::html_elements(tbl, "a[href*='/movie/']")
      if (length(links) > 5) {
        main_table <- tbl
        break
      }
    }

    if (is.null(main_table)) {
      if (page_start == 1) {
        cli::cli_abort("Could not find movie table for {year}")
      }
      break
    }

    rows <- rvest::html_elements(main_table, "tr")

    if (length(rows) < 2) {
      break
    }

    # Parse data rows (skip header)
    data_rows <- rows[-1]

    page_films <- purrr::map_dfr(data_rows, function(row) {
      cells <- rvest::html_elements(row, "td")
      if (length(cells) < 3) return(NULL)

      # Look for movie link
      link <- rvest::html_element(row, "a[href*='/movie/']")
      if (length(link) == 0) return(NULL)

      href <- rvest::html_attr(link, "href")
      if (is.na(href)) return(NULL)

      title <- rvest::html_text2(link)

      # Extract movie slug from URL
      # Pattern: /movie/Movie-Title-(Year) or /movie/Movie-Title#tab=box-office
      slug_match <- stringr::str_match(href, "/movie/([^/#?]+)")
      movie_slug <- if (!is.na(slug_match[1, 2])) slug_match[1, 2] else NA_character_

      if (is.na(movie_slug)) return(NULL)

      # Extract other data from cells
      cell_text <- rvest::html_text2(cells)
      cell_text <- stringr::str_trim(cell_text)

      # Find the gross column (has $ sign)
      gross_val <- NA_real_
      for (ct in cell_text) {
        if (grepl("^\\$", ct)) {
          gross_val <- parse_currency(ct)
          break
        }
      }

      tryCatch({
        data.frame(
          movie_slug = movie_slug,
          title = title,
          domestic_gross = gross_val,
          stringsAsFactors = FALSE
        )
      }, error = function(e) NULL)
    })

    if (is.null(page_films) || nrow(page_films) == 0) {
      break
    }

    all_films <- rbind(all_films, page_films)

    # Check if there are more pages
    # Look for next page link or check if we got fewer than 100 results
    if (nrow(page_films) < 100) {
      break
    }

    page_start <- page_start + 100
    cli::cli_alert_info("Fetching page {page_start/100 + 1}...")
  }

  if (nrow(all_films) == 0) {
    cli::cli_abort("No films found for {year}")
  }

  # Add year and remove duplicates
  all_films$year <- year
  all_films <- all_films[!duplicated(all_films$movie_slug), ]
  all_films <- all_films[!is.na(all_films$movie_slug), ]

  # Cache result
  write_cache("numbers", "yearly", cache_key, all_films)

  # Save to data folder
  if (save_to_data) {
    if (!dir.exists("data")) dir.create("data", recursive = TRUE)
    saveRDS(all_films, data_file)
    cli::cli_alert_success("Saved to {data_file}")
  }

  cli::cli_alert_success("Found {nrow(all_films)} films for {year}")
  all_films
}

#' Scrape daily box office data for a single film from The Numbers
#'
#' Retrieves the complete daily box office breakdown for a film.
#'
#' @param movie_slug The Numbers movie slug (e.g., "Barbie-(2023)")
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @return Data frame with daily breakdown:
#'   - date: Date
#'   - day_number: Sequential day number
#'   - rank: Daily box office rank
#'   - daily_gross: Daily gross revenue
#'   - pct_change: Percent change from previous day
#'   - theaters: Number of theaters
#'   - per_theater_avg: Per-theater average
#'   - cumulative_gross: Running total gross
#' @export
#' @examples
#' \dontrun{
#' barbie_daily <- scrape_numbers_daily("Barbie-(2023)")
#' }
scrape_numbers_daily <- function(movie_slug, use_cache = TRUE) {
  if (is.na(movie_slug) || movie_slug == "") {
    cli::cli_abort("Invalid movie_slug")
  }

  # Check cache (daily data never expires)
  if (use_cache && is_cached("numbers", "daily", movie_slug, expires_days = NULL)) {
    cached_data <- read_cache("numbers", "daily", movie_slug)
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
  url <- paste0(.boxoffice_env$numbers_base_url, "/movie/", movie_slug, "#tab=box-office")

  # Fetch page
  html <- fetch_html(url, delay = 1)
  if (is.null(html)) {
    cli::cli_warn("Failed to fetch daily data for {movie_slug}")
    # Cache failure as empty data frame so we don't retry
    write_cache("numbers", "daily", movie_slug, data.frame())
    return(NULL)
  }

  # Find daily box office table
  # The Numbers has multiple tables; look for one with dates
  tables <- rvest::html_elements(html, "table")

  daily_table <- NULL
  for (tbl in tables) {
    text <- rvest::html_text2(tbl)
    # Look for date patterns and gross columns
    if (stringr::str_detect(text, "\\d{4}-\\d{2}-\\d{2}|\\w+\\s+\\d+,\\s+\\d{4}") &&
        stringr::str_detect(text, "(?i)gross|revenue")) {
      rows <- rvest::html_elements(tbl, "tr")
      if (length(rows) > 10) {  # Daily table should have many rows
        daily_table <- tbl
        break
      }
    }
  }

  if (is.null(daily_table)) {
    cli::cli_warn("No daily table found for {movie_slug}")
    # Cache failure as empty data frame so we don't retry
    write_cache("numbers", "daily", movie_slug, data.frame())
    return(NULL)
  }

  # Parse table rows
  rows <- rvest::html_elements(daily_table, "tr")

  if (length(rows) < 2) {
    cli::cli_warn("Empty daily table for {movie_slug}")
    # Cache failure as empty data frame so we don't retry
    write_cache("numbers", "daily", movie_slug, data.frame())
    return(NULL)
  }

  # Skip header row(s)
  data_rows <- rows[-1]

  # Parse each row
  daily_data <- purrr::map_dfr(data_rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    if (length(cells) < 5) return(NULL)

    cell_text <- rvest::html_text2(cells)
    cell_text <- stringr::str_trim(cell_text)

    tryCatch({
      # Try to parse date from first column
      date_val <- lubridate::ymd(cell_text[1], quiet = TRUE)
      if (is.na(date_val)) {
        date_val <- lubridate::mdy(cell_text[1], quiet = TRUE)
      }

      data.frame(
        date = date_val,
        rank = as.integer(gsub("[^0-9]", "", cell_text[2])),
        daily_gross = parse_currency(cell_text[3]),
        pct_change = parse_percentage(cell_text[4]),
        theaters = parse_currency(cell_text[5]),
        per_theater_avg = if (length(cell_text) >= 6) parse_currency(cell_text[6]) else NA_real_,
        cumulative_gross = if (length(cell_text) >= 7) parse_currency(cell_text[7]) else NA_real_,
        stringsAsFactors = FALSE
      )
    }, error = function(e) NULL)
  })

  if (nrow(daily_data) == 0) {
    cli::cli_warn("No daily data parsed for {movie_slug}")
    # Cache failure as empty data frame so we don't retry
    write_cache("numbers", "daily", movie_slug, data.frame())
    return(NULL)
  }

  # Sort by date and add day number
  daily_data <- daily_data[order(daily_data$date), ]
  daily_data$day_number <- seq_len(nrow(daily_data))
  daily_data$movie_slug <- movie_slug

  # Add day of week
  daily_data$day_of_week <- weekdays(daily_data$date)

  # Cache result
  write_cache("numbers", "daily", movie_slug, daily_data)

  daily_data
}

#' Scrape film metadata from The Numbers
#'
#' Retrieves metadata for a film including budget, genres, and more.
#'
#' @param movie_slug The Numbers movie slug
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @return List with metadata fields:
#'   - title: Film title
#'   - release_date: Release date
#'   - budget: Production budget
#'   - mpaa_rating: MPAA rating
#'   - runtime: Runtime in minutes
#'   - genres: Character vector of genres
#'   - distributor: Distribution company
#'   - domestic_gross: Total domestic gross
#'   - international_gross: Total international gross
#'   - worldwide_gross: Total worldwide gross
#'   - opening_weekend: Opening weekend gross
#'   - opening_theaters: Opening theater count
#' @export
#' @examples
#' \dontrun{
#' meta <- scrape_numbers_metadata("Barbie-(2023)")
#' }
scrape_numbers_metadata <- function(movie_slug, use_cache = TRUE) {
  if (is.na(movie_slug) || movie_slug == "") {
    cli::cli_abort("Invalid movie_slug")
  }

  # Check cache
  if (use_cache && is_cached("numbers", "metadata", movie_slug, expires_days = 30)) {
    cached_data <- read_cache("numbers", "metadata", movie_slug)
    if (!is.null(cached_data) && is.list(cached_data)) {
      return(cached_data)
    }
  }

  # Build URL
  url <- paste0(.boxoffice_env$numbers_base_url, "/movie/", movie_slug)

  # Fetch page
  html <- fetch_html(url, delay = 1)
  if (is.null(html)) {
    cli::cli_warn("Failed to fetch metadata for {movie_slug}")
    return(NULL)
  }

  # Initialize metadata
  metadata <- list(
    movie_slug = movie_slug,
    title = NA_character_,
    release_date = as.Date(NA),
    budget = NA_real_,
    mpaa_rating = NA_character_,
    runtime = NA_integer_,
    genres = character(0),
    distributor = NA_character_,
    domestic_gross = NA_real_,
    international_gross = NA_real_,
    worldwide_gross = NA_real_,
    opening_weekend = NA_real_,
    opening_theaters = NA_integer_
  )

  # Extract title from h1
  title_elem <- rvest::html_element(html, "h1")
  if (!is.null(title_elem) && length(title_elem) > 0) {
    metadata$title <- stringr::str_trim(rvest::html_text2(title_elem))
  }

  # The Numbers displays data in a summary table/div
  page_text <- rvest::html_text2(html)

  # Budget (The Numbers is known for having good budget data)
  budget_match <- stringr::str_match(page_text, "(?i)production\\s*budget[:\\s]*\\$([0-9,]+)")
  if (!is.na(budget_match[1, 2])) {
    metadata$budget <- parse_currency(budget_match[1, 2])
  }

  # Domestic gross
  domestic_match <- stringr::str_match(page_text, "(?i)domestic\\s*box\\s*office[:\\s]*\\$([0-9,]+)")
  if (!is.na(domestic_match[1, 2])) {
    metadata$domestic_gross <- parse_currency(domestic_match[1, 2])
  }

  # International
  intl_match <- stringr::str_match(page_text, "(?i)international\\s*box\\s*office[:\\s]*\\$([0-9,]+)")
  if (!is.na(intl_match[1, 2])) {
    metadata$international_gross <- parse_currency(intl_match[1, 2])
  }

  # Worldwide
  worldwide_match <- stringr::str_match(page_text, "(?i)worldwide\\s*box\\s*office[:\\s]*\\$([0-9,]+)")
  if (!is.na(worldwide_match[1, 2])) {
    metadata$worldwide_gross <- parse_currency(worldwide_match[1, 2])
  }

  # Opening weekend
  opening_match <- stringr::str_match(page_text, "(?i)opening\\s*weekend[:\\s]*\\$([0-9,]+)")
  if (!is.na(opening_match[1, 2])) {
    metadata$opening_weekend <- parse_currency(opening_match[1, 2])
  }

  # MPAA Rating
  rating_match <- stringr::str_match(page_text, "(?i)MPAA\\s*rating[:\\s]*([A-Z0-9-]+)")
  if (!is.na(rating_match[1, 2])) {
    metadata$mpaa_rating <- stringr::str_trim(rating_match[1, 2])
  }

  # Runtime
  runtime_match <- stringr::str_match(page_text, "(?i)running\\s*time[:\\s]*(\\d+)\\s*min")
  if (!is.na(runtime_match[1, 2])) {
    metadata$runtime <- as.integer(runtime_match[1, 2])
  }

  # Genres - look for genre links
  genre_links <- rvest::html_elements(html, "a[href*='/market/genre/']")
  if (length(genre_links) > 0) {
    metadata$genres <- rvest::html_text2(genre_links)
  }

  # Release date
  date_match <- stringr::str_match(page_text, "(?i)release\\s*date[:\\s]*(\\w+\\s+\\d+,\\s+\\d{4})")
  if (!is.na(date_match[1, 2])) {
    metadata$release_date <- lubridate::mdy(date_match[1, 2], quiet = TRUE)
  }

  # Distributor
  dist_match <- stringr::str_match(page_text, "(?i)domestic\\s*distributor[:\\s]*([^\\n]+)")
  if (!is.na(dist_match[1, 2])) {
    metadata$distributor <- stringr::str_trim(dist_match[1, 2])
  }

  # Cache result
  write_cache("numbers", "metadata", movie_slug, metadata)

  metadata
}

#' Batch scrape daily data for multiple films from The Numbers
#'
#' @param movie_slugs Character vector of movie slugs
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @param save_every Integer: save progress to data/ every N films (default: 500, 0 to disable)
#' @param progress Logical: show progress bar (default: TRUE)
#' @return List of data frames (named by movie_slug)
#' @export
#' @examples
#' \dontrun{
#' films <- c("Barbie-(2023)", "Oppenheimer-(2023)")
#' daily_data <- scrape_numbers_daily_batch(films)
#' }
scrape_numbers_daily_batch <- function(movie_slugs, use_cache = TRUE, save_every = 500, progress = TRUE) {
  n <- length(movie_slugs)

  if (n == 0) {
    return(list())
  }

  # Load existing progress from data/
  data_file <- "data/numbers_daily.rds"
  if (file.exists(data_file)) {
    existing <- readRDS(data_file)
    cli::cli_alert_info("Loaded {length(existing)} existing films from {data_file}")
  } else {
    existing <- list()
  }

  # Skip already-scraped films
  already_done <- names(existing)
  to_scrape <- setdiff(movie_slugs, already_done)

  if (length(to_scrape) == 0) {
    cli::cli_alert_success("All {n} films already scraped")
    return(existing[movie_slugs[movie_slugs %in% names(existing)]])
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
    slug <- to_scrape[i]
    results[[slug]] <- tryCatch(
      scrape_numbers_daily(slug, use_cache = use_cache),
      error = function(e) {
        cli::cli_alert_danger("Error scraping {slug}: {e$message}")
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
