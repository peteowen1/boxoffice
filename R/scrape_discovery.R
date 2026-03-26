#' Discover all films for a year range
#'
#' Builds a comprehensive index of films from both Box Office Mojo
#' and The Numbers for a range of years.
#'
#' @param start_year Starting year (default: 1980)
#' @param end_year Ending year (default: current year)
#' @param sources Character vector: "bom", "numbers", or both
#' @param use_cache Logical: use cached data if available (default: TRUE)
#' @param save_to_data Logical: save each year to data/ folder (default: FALSE)
#' @param progress Logical: show progress bar (default: TRUE)
#' @return Data frame with film identifiers from both sources
#' @export
#' @examples
#' \dontrun{
#' # Discover films from 2020-2023
#' films <- discover_films(2020, 2023)
#' }
discover_films <- function(
    start_year = 1980,
    end_year = as.integer(format(Sys.Date(), "%Y")),
    sources = c("bom", "numbers"),
    use_cache = TRUE,
    save_to_data = FALSE,
    progress = TRUE
) {
  sources <- match.arg(sources, c("bom", "numbers"), several.ok = TRUE)

  years <- seq(start_year, end_year)
  n_years <- length(years)

  if (progress) {
    cli::cli_alert_info("Discovering films from {start_year} to {end_year} ({n_years} years)")
  }

  all_films <- list()

  # Scrape Box Office Mojo
  if ("bom" %in% sources) {
    bom_results <- vector("list", n_years)

    if (progress) {
      cli::cli_progress_bar("BOM yearly indexes", total = n_years)
    }

    for (i in seq_along(years)) {
      yr <- years[i]
      bom_results[[i]] <- tryCatch(
        scrape_bom_yearly_index(yr, use_cache = use_cache, save_to_data = save_to_data),
        error = function(e) {
          cli::cli_alert_warning("Failed to get BOM data for {yr}: {e$message}")
          NULL
        }
      )
      if (progress) cli::cli_progress_update()
    }

    if (progress) cli::cli_progress_done()

    bom_films <- dplyr::bind_rows(bom_results)

    if (!is.null(bom_films) && nrow(bom_films) > 0) {
      bom_films$source <- "bom"
      all_films$bom <- bom_films
      if (progress) cli::cli_alert_success("BOM: {nrow(bom_films)} films")
    }
  }

  # Scrape The Numbers
  if ("numbers" %in% sources) {
    numbers_results <- vector("list", n_years)

    if (progress) {
      cli::cli_progress_bar("Numbers yearly indexes", total = n_years)
    }

    for (i in seq_along(years)) {
      yr <- years[i]
      numbers_results[[i]] <- tryCatch(
        scrape_numbers_yearly_index(yr, use_cache = use_cache, save_to_data = save_to_data),
        error = function(e) {
          cli::cli_alert_warning("Failed to get Numbers data for {yr}: {e$message}")
          NULL
        }
      )
      if (progress) cli::cli_progress_update()
    }

    if (progress) cli::cli_progress_done()

    numbers_films <- dplyr::bind_rows(numbers_results)

    if (!is.null(numbers_films) && nrow(numbers_films) > 0) {
      numbers_films$source <- "numbers"
      all_films$numbers <- numbers_films
      if (progress) cli::cli_alert_success("Numbers: {nrow(numbers_films)} films")
    }
  }

  # Combine and return based on sources
  if (length(all_films) == 0) {
    cli::cli_abort("No films discovered from any source")
  }

  if (length(all_films) == 1) {
    result <- all_films[[1]]
  } else {
    # Match films across sources
    result <- match_films_across_sources(all_films$bom, all_films$numbers)
  }

  cli::cli_alert_success("Discovered {nrow(result)} films total")
  result
}

#' Match films across Box Office Mojo and The Numbers
#'
#' Attempts to match films from both sources based on title and year
#' using fuzzy matching.
#'
#' @param bom_films Data frame from BOM discovery
#' @param numbers_films Data frame from The Numbers discovery
#' @return Data frame with matched films containing identifiers for both sources
#' @export
#' @examples
#' \dontrun{
#' bom <- scrape_bom_yearly_index(2023)
#' numbers <- scrape_numbers_yearly_index(2023)
#' matched <- match_films_across_sources(bom, numbers)
#' }
match_films_across_sources <- function(bom_films, numbers_films) {
  if (is.null(bom_films) || nrow(bom_films) == 0) {
    if (is.null(numbers_films) || nrow(numbers_films) == 0) {
      cli::cli_abort("Both data frames are empty")
    }
    numbers_films$release_id <- NA_character_
    numbers_films$numbers_slug <- numbers_films$movie_slug
    return(numbers_films)
  }

  if (is.null(numbers_films) || nrow(numbers_films) == 0) {
    bom_films$numbers_slug <- NA_character_
    return(bom_films)
  }

  cli::cli_alert_info("Matching {nrow(bom_films)} BOM films with {nrow(numbers_films)} Numbers films...")

  # Normalize titles for matching
  normalize_title <- function(x) {
    x <- tolower(x)
    x <- stringr::str_remove_all(x, "[^a-z0-9\\s]")
    x <- stringr::str_squish(x)
    x
  }

  bom_films$title_norm <- normalize_title(bom_films$title)
  numbers_films$title_norm <- normalize_title(numbers_films$title)

  # Create matching key with year
  bom_films$match_key <- paste(bom_films$title_norm, bom_films$year)
  numbers_films$match_key <- paste(numbers_films$title_norm, numbers_films$year)

  # Match BOM films with Numbers data
  matched <- dplyr::left_join(
    bom_films,
    numbers_films[, c("match_key", "movie_slug", "domestic_gross")],
    by = "match_key",
    suffix = c("", "_numbers")
  )

  # Rename for clarity
  names(matched)[names(matched) == "movie_slug"] <- "numbers_slug"
  if ("domestic_gross_numbers" %in% names(matched)) {
    names(matched)[names(matched) == "domestic_gross_numbers"] <- "numbers_gross"
  }

  # Find unmatched Numbers films
  unmatched_numbers <- numbers_films[!numbers_films$match_key %in% bom_films$match_key, ]

  if (nrow(unmatched_numbers) > 0) {
    # Build a simple data frame for unmatched Numbers films
    unmatched_df <- data.frame(
      release_id = NA_character_,
      title = unmatched_numbers$title,
      year = unmatched_numbers$year,
      numbers_slug = unmatched_numbers$movie_slug,
      domestic_gross = unmatched_numbers$domestic_gross,
      stringsAsFactors = FALSE
    )

    # Use bind_rows which handles missing columns gracefully
    matched <- dplyr::bind_rows(matched, unmatched_df)
  }

  # Clean up temporary columns
  matched$title_norm <- NULL
  matched$match_key <- NULL
  matched$source <- NULL

  # Mark match quality
  has_bom <- !is.na(matched$release_id)
  has_numbers <- !is.na(matched$numbers_slug)
  matched$matched <- has_bom & has_numbers

  n_matched <- sum(matched$matched, na.rm = TRUE)
  n_bom_only <- sum(has_bom & !has_numbers, na.rm = TRUE)
  n_numbers_only <- sum(!has_bom & has_numbers, na.rm = TRUE)

  cli::cli_alert_success("Matched {n_matched} films, {n_bom_only} BOM-only, {n_numbers_only} Numbers-only")

  matched
}

#' Build comprehensive film index
#'
#' Creates or updates a comprehensive index of all films across years.
#'
#' @param years Integer vector of years to index
#' @param sources Character vector: "bom", "numbers", or both
#' @param refresh Logical: re-scrape even if cached (default: FALSE)
#' @param progress Logical: show progress bar (default: TRUE)
#' @return Data frame with all film identifiers and basic metadata
#' @export
#' @examples
#' \dontrun{
#' index <- build_film_index(2020:2024)
#' }
build_film_index <- function(
    years,
    sources = c("bom", "numbers"),
    refresh = FALSE,
    progress = TRUE
) {
  if (refresh) {
    # Clear cache for specified years
    for (yr in years) {
      for (src in sources) {
        cp <- cache_path(src, "yearly", as.character(yr))
        if (file.exists(cp)) {
          file.remove(cp)
        }
      }
    }
  }

  discover_films(
    start_year = min(years),
    end_year = max(years),
    sources = sources,
    use_cache = !refresh,
    progress = progress
  )
}

#' Search for a film in the index
#'
#' Searches for a film by title and/or year.
#'
#' @param title Character: film title (partial match)
#' @param year Integer: release year (optional)
#' @param index Data frame from build_film_index (optional, will create if NULL)
#' @return Data frame with matching films
#' @export
#' @examples
#' \dontrun{
#' find_film("Barbie", 2023)
#' }
find_film <- function(title, year = NULL, index = NULL) {
  if (is.null(index)) {
    if (is.null(year)) {
      cli::cli_abort("Must provide either year or index")
    }
    index <- build_film_index(year)
  }

  # Search by title (use fixed = TRUE to avoid regex injection from special chars in titles)
  title_pattern <- tolower(title)
  matches <- index[grepl(title_pattern, tolower(index$title), fixed = TRUE), ]

  # Filter by year if provided
  if (!is.null(year) && nrow(matches) > 0) {
    matches <- matches[matches$year == year, ]
  }

  if (nrow(matches) == 0) {
    cli::cli_alert_warning("No films found matching '{title}'")
    return(data.frame())
  }

  cli::cli_alert_success("Found {nrow(matches)} matching film(s)")
  matches
}

#' Get identifiers for a film
#'
#' Retrieves both BOM and Numbers identifiers for a film.
#'
#' @param title Character: film title
#' @param year Integer: release year
#' @return List with release_id (BOM) and movie_slug (Numbers)
#' @export
#' @examples
#' \dontrun{
#' ids <- get_film_ids("Barbie", 2023)
#' }
get_film_ids <- function(title, year) {
  matches <- find_film(title, year)

  if (nrow(matches) == 0) {
    return(list(release_id = NA_character_, movie_slug = NA_character_))
  }

  if (nrow(matches) > 1) {
    cli::cli_alert_warning("Multiple matches found, using first result")
    matches <- matches[1, ]
  }

  list(
    release_id = matches$release_id[1],
    movie_slug = if ("numbers_slug" %in% names(matches)) matches$numbers_slug[1] else NA_character_
  )
}
