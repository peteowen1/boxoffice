#' boxoffice: Scrape Box Office Data and Project Theatrical Runs
#'
#' The boxoffice package provides tools for scraping daily box office data
#' from Box Office Mojo and The Numbers, and building predictive models
#' to project full theatrical runs based on opening performance.
#'
#' @section Main Functions:
#'
#' **Data Retrieval:**
#' * [get_film_data()] - Get box office data for a single film
#' * [get_yearly_data()] - Get box office data for all films in a year
#' * [build_historical_dataset()] - Build complete historical dataset
#'
#' **Projection:**
#' * [project_theatrical_run()] - Project full theatrical run for a film
#' * [train_projection_model()] - Train custom projection model
#'
#' **Discovery:**
#' * [discover_films()] - Discover films across years
#' * [find_film()] - Search for a specific film
#'
#' @keywords internal
"_PACKAGE"

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

#' Get box office data for a single film
#'
#' Retrieves comprehensive box office data for a film, including daily
#' breakdown and metadata from both Box Office Mojo and The Numbers.
#'
#' @param title Film title
#' @param year Release year
#' @param sources Character vector: "bom", "numbers", or both
#' @param include_daily Logical: include daily breakdown (default: TRUE)
#' @param use_cache Logical: use cached data (default: TRUE)
#' @return List with:
#'   - metadata: Film metadata (title, budget, genres, etc.)
#'   - daily: Daily box office breakdown
#'   - source_info: Which sources were used
#' @export
#' @examples
#' \dontrun{
#' barbie <- get_film_data("Barbie", 2023)
#' head(barbie$daily)
#' }
get_film_data <- function(
    title,
    year,
    sources = c("bom", "numbers"),
    include_daily = TRUE,
    use_cache = TRUE
) {
  sources <- match.arg(sources, c("bom", "numbers"), several.ok = TRUE)

  # Find film identifiers
  ids <- get_film_ids(title, year)

  bom_daily <- NULL
  bom_meta <- NULL
  numbers_daily <- NULL
  numbers_meta <- NULL

  # Scrape Box Office Mojo
  if ("bom" %in% sources && !is.na(ids$release_id)) {
    cli::cli_alert_info("Fetching from Box Office Mojo...")

    if (include_daily) {
      bom_daily <- tryCatch(
        scrape_bom_daily(ids$release_id, use_cache = use_cache),
        error = function(e) {
          cli::cli_alert_warning("BOM daily scrape failed: {e$message}")
          NULL
        }
      )
    }

    bom_meta <- tryCatch(
      scrape_bom_metadata(ids$release_id, use_cache = use_cache),
      error = function(e) {
        cli::cli_alert_warning("BOM metadata scrape failed: {e$message}")
        NULL
      }
    )
  }

  # Scrape The Numbers
  if ("numbers" %in% sources && !is.na(ids$movie_slug)) {
    cli::cli_alert_info("Fetching from The Numbers...")

    if (include_daily) {
      numbers_daily <- tryCatch(
        scrape_numbers_daily(ids$movie_slug, use_cache = use_cache),
        error = function(e) {
          cli::cli_alert_warning("Numbers daily scrape failed: {e$message}")
          NULL
        }
      )
    }

    numbers_meta <- tryCatch(
      scrape_numbers_metadata(ids$movie_slug, use_cache = use_cache),
      error = function(e) {
        cli::cli_alert_warning("Numbers metadata scrape failed: {e$message}")
        NULL
      }
    )
  }

  # Merge data
  daily <- merge_daily_data(bom_daily, numbers_daily)
  metadata <- merge_metadata(bom_meta, numbers_meta)

  # Add identifiers
  metadata$bom_id <- ids$release_id
  metadata$numbers_slug <- ids$movie_slug

  result <- list(
    metadata = metadata,
    daily = daily,
    source_info = list(
      bom_available = !is.null(bom_daily) || !is.null(bom_meta),
      numbers_available = !is.null(numbers_daily) || !is.null(numbers_meta)
    )
  )

  cli::cli_alert_success("Retrieved data for: {metadata$title %||% title}")
  result
}

#' Get box office data for all films in a year
#'
#' Retrieves box office data for all films released in a given year.
#'
#' @param year Integer year
#' @param sources Character vector: "bom", "numbers", or both
#' @param include_daily Logical: include daily breakdown (default: FALSE for performance)
#' @param use_cache Logical: use cached data (default: TRUE)
#' @param progress Logical: show progress bar (default: TRUE)
#' @return List with:
#'   - films: Data frame with film index
#'   - daily_data: List of daily data frames (if include_daily = TRUE)
#' @export
#' @examples
#' \dontrun{
#' data_2023 <- get_yearly_data(2023)
#' nrow(data_2023$films)
#' }
get_yearly_data <- function(
    year,
    sources = c("bom", "numbers"),
    include_daily = FALSE,
    use_cache = TRUE,
    progress = TRUE
) {
  sources <- match.arg(sources, c("bom", "numbers"), several.ok = TRUE)

  # Get film index
  films <- discover_films(
    start_year = year,
    end_year = year,
    sources = sources,
    use_cache = use_cache,
    progress = progress
  )

  result <- list(films = films, daily_data = NULL)

  if (include_daily && nrow(films) > 0) {
    cli::cli_alert_info("Fetching daily data for {nrow(films)} films...")

    # Prioritize BOM IDs
    if ("bom" %in% sources && "release_id" %in% names(films)) {
      bom_ids <- films$release_id[!is.na(films$release_id)]
      if (length(bom_ids) > 0) {
        result$daily_data <- scrape_bom_daily_batch(bom_ids, use_cache = use_cache, progress = progress)
      }
    }

    # Supplement with Numbers
    if ("numbers" %in% sources && "numbers_slug" %in% names(films)) {
      numbers_slugs <- films$numbers_slug[!is.na(films$numbers_slug)]
      # Only scrape films not already fetched from BOM
      if (!is.null(result$daily_data)) {
        already_fetched <- names(result$daily_data)
        # Match by title/year since IDs differ
        films_with_bom <- films$title[films$release_id %in% already_fetched]
        numbers_slugs <- films$numbers_slug[!films$title %in% films_with_bom &
                                              !is.na(films$numbers_slug)]
      }

      if (length(numbers_slugs) > 0) {
        numbers_data <- scrape_numbers_daily_batch(numbers_slugs, use_cache = use_cache, progress = progress)
        result$daily_data <- c(result$daily_data, numbers_data)
      }
    }
  }

  result
}

#' Build historical dataset
#'
#' Creates a comprehensive dataset of films across multiple years,
#' suitable for model training.
#'
#' @param start_year Starting year (default: 1980)
#' @param end_year Ending year (default: current year)
#' @param sources Character vector: "bom", "numbers", or both
#' @param include_daily Logical: include daily breakdown (default: TRUE)
#' @param use_cache Logical: use cached data (default: TRUE)
#' @param progress Logical: show progress bar (default: TRUE)
#' @return List with:
#'   - films: Data frame with all films
#'   - daily_data: Named list of daily data frames
#' @export
#' @examples
#' \dontrun{
#' # Build dataset for recent years
#' dataset <- build_historical_dataset(2020, 2024)
#' }
build_historical_dataset <- function(
    start_year = 1980,
    end_year = as.integer(format(Sys.Date(), "%Y")),
    sources = c("bom", "numbers"),
    include_daily = TRUE,
    use_cache = TRUE,
    progress = TRUE
) {
  years <- seq(start_year, end_year)
  n_years <- length(years)

  cli::cli_alert_info("Building dataset for {n_years} years ({start_year}-{end_year})")

  all_films <- data.frame()
  all_daily <- list()

  for (i in seq_along(years)) {
    yr <- years[i]
    cli::cli_h3("Year {yr} ({i}/{n_years})")

    year_data <- tryCatch({
      get_yearly_data(
        year = yr,
        sources = sources,
        include_daily = include_daily,
        use_cache = use_cache,
        progress = progress
      )
    }, error = function(e) {
      cli::cli_alert_danger("Failed to get data for {yr}: {e$message}")
      NULL
    })

    if (!is.null(year_data)) {
      if (nrow(year_data$films) > 0) {
        all_films <- rbind(all_films, year_data$films)
      }
      if (!is.null(year_data$daily_data)) {
        all_daily <- c(all_daily, year_data$daily_data)
      }
    }
  }

  cli::cli_alert_success("Built dataset with {nrow(all_films)} films")

  list(
    films = all_films,
    daily_data = all_daily
  )
}

#' Project theatrical run for a film
#'
#' Projects the full theatrical run for a film based on its opening
#' performance.
#'
#' @param title Film title
#' @param year Release year
#' @param days_available Number of days of data available (NULL = all available)
#' @param model Trained model from train_projection_model (NULL = use trajectory only)
#' @param sources Data sources to use
#' @param use_cache Use cached data
#' @return Projection report object
#' @export
#' @examples
#' \dontrun{
#' projection <- project_theatrical_run("Barbie", 2023)
#' print(projection)
#' }
project_theatrical_run <- function(
    title,
    year,
    days_available = NULL,
    model = NULL,
    sources = c("bom", "numbers"),
    use_cache = TRUE
) {
  # Get film data
  film_data <- get_film_data(
    title = title,
    year = year,
    sources = sources,
    include_daily = TRUE,
    use_cache = use_cache
  )

  if (is.null(film_data$daily) || nrow(film_data$daily) == 0) {
    cli::cli_abort("No daily data available for {title} ({year})")
  }

  daily <- film_data$daily

  # Limit to specified days if requested
  if (!is.null(days_available)) {
    daily <- daily[daily$day_number <= days_available, ]
    cli::cli_alert_info("Using first {days_available} days for projection")
  }

  # Generate report
  report <- generate_projection_report(
    daily_data = daily,
    metadata = film_data$metadata,
    model = model,
    historical_features = NULL
  )

  report
}

#' Quick summary of package capabilities
#'
#' @export
boxoffice_info <- function() {
  cli::cli_h1("boxoffice Package")
  cli::cli_text("Scrape box office data and project theatrical runs")
  cli::cli_text("")

  cli::cli_h2("Data Sources")
  cli::cli_bullets(c(
    "*" = "Box Office Mojo (boxofficemojo.com)",
    "*" = "The Numbers (the-numbers.com)"
  ))

  cli::cli_h2("Main Functions")
  cli::cli_bullets(c(
    "*" = "get_film_data(title, year) - Get data for a single film",
    "*" = "get_yearly_data(year) - Get all films for a year",
    "*" = "project_theatrical_run(title, year) - Project a film's run",
    "*" = "train_projection_model(features) - Train custom model"
  ))

  cli::cli_h2("Examples")
  cli::cli_code('
# Get Barbie (2023) data
barbie <- get_film_data("Barbie", 2023)

# View daily breakdown
head(barbie$daily)

# Project theatrical run
projection <- project_theatrical_run("Barbie", 2023)
print(projection)
')

  invisible(NULL)
}
