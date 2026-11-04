#' Parse currency strings to numeric
#'
#' Converts currency strings like "$1,234,567" to numeric values.
#'
#' @param x Character vector with currency formatting
#' @return Numeric vector
#' @keywords internal
parse_currency <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- stringr::str_remove_all(x, "[$,]")
  x <- stringr::str_trim(x)
  x <- ifelse(x == "" | x == "-" | is.na(x), NA_character_, x)
  as.numeric(x)
}

#' Parse percentage strings to numeric
#'
#' Converts percentage strings like "-15.2%" to decimal values.
#'
#' @param x Character vector with percentage formatting
#' @return Numeric vector (as decimal, e.g., -0.152)
#' @keywords internal
parse_percentage <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- stringr::str_remove_all(x, "[%,]")
  x <- stringr::str_trim(x)
  x <- ifelse(x == "" | x == "-" | is.na(x), NA_character_, x)
  as.numeric(x) / 100
}

#' Parse runtime strings to minutes
#'
#' Converts runtime strings like "2 hr 15 min" to integer minutes.
#'
#' @param x Character vector with runtime formatting
#' @return Integer vector (minutes)
#' @keywords internal
parse_runtime <- function(x) {
  if (is.numeric(x)) return(as.integer(x))
  x <- as.character(x)

  hours <- stringr::str_extract(x, "\\d+(?=\\s*hr)")
  hours <- ifelse(is.na(hours), 0, as.integer(hours))

  minutes <- stringr::str_extract(x, "\\d+(?=\\s*min)")
  minutes <- ifelse(is.na(minutes), 0, as.integer(minutes))

  as.integer(hours * 60 + minutes)
}

#' Parse flexible date formats
#'
#' Handles various date formats found on box office sites.
#'
#' @param x Character vector with dates
#' @param year Optional year to use if not present in string
#' @return Date vector
#' @keywords internal
parse_date_flexible <- function(x, year = NULL) {
  x <- as.character(x)
  x <- stringr::str_trim(x)

  # Try common formats
  parsed <- lubridate::mdy(x, quiet = TRUE)

  # If that fails, try other formats
  if (all(is.na(parsed))) {
    parsed <- lubridate::ymd(x, quiet = TRUE)
  }

  # Handle short dates like "Jun 14" by adding year

  if (all(is.na(parsed)) && !is.null(year)) {
    x_with_year <- paste(x, year)
    parsed <- lubridate::mdy(x_with_year, quiet = TRUE)
  }

  parsed
}

#' Extract year from URL or title
#'
#' @param x Character vector
#' @return Integer year
#' @keywords internal
extract_year <- function(x) {
 year_match <- stringr::str_extract(x, "\\b(19|20)\\d{2}\\b")
  as.integer(year_match)
}

#' Sanitize film title for URL construction
#'
#' Converts a film title to a URL-safe slug for The Numbers.
#'
#' @param title Film title
#' @param year Release year
#' @return URL-safe string
#' @keywords internal
sanitize_title_for_url <- function(title, year = NULL) {
  slug <- title

  # Replace common characters
  slug <- stringr::str_replace_all(slug, "&", "and")
  slug <- stringr::str_replace_all(slug, "'", "")
  slug <- stringr::str_replace_all(slug, ":", "")
  slug <- stringr::str_replace_all(slug, ",", "")
  slug <- stringr::str_replace_all(slug, "\\.", "")
  slug <- stringr::str_replace_all(slug, "!", "")
  slug <- stringr::str_replace_all(slug, "\\?", "")

  # Replace spaces with hyphens
  slug <- stringr::str_replace_all(slug, "\\s+", "-")

  # Remove any double hyphens
  slug <- stringr::str_replace_all(slug, "-+", "-")

  # Trim hyphens from ends
  slug <- stringr::str_remove(slug, "^-+")
  slug <- stringr::str_remove(slug, "-+$")

  # Add year if provided
  if (!is.null(year)) {
    slug <- paste0(slug, "-(", year, ")")
  }

  slug
}

#' Extract release ID from Box Office Mojo URL
#'
#' @param url Character URL or href string
#' @return Character release ID (e.g., "rl1234567890")
#' @keywords internal
extract_bom_release_id <- function(url) {
  stringr::str_extract(url, "rl\\d+")
}

#' Standardize genre names
#'
#' Maps various genre spellings to standard names.
#'
#' @param genre Character vector of genre names
#' @return Character vector with standardized genre names
#' @keywords internal
standardize_genre <- function(genre) {
  genre <- stringr::str_trim(genre)
  genre <- stringr::str_to_title(genre)

  # Common mappings
  genre <- dplyr::case_when(
    genre %in% c("Sci-Fi", "Science Fiction", "Scifi") ~ "Sci-Fi",
    genre %in% c("Romantic Comedy", "Rom-Com") ~ "Romance",
    genre %in% c("Action/Adventure", "Action-Adventure") ~ "Action",
    genre %in% c("Animated", "Cgi Animation") ~ "Animation",
    TRUE ~ genre
  )

  genre
}

#' Standardize distributor names
#'
#' Maps various distributor name variations to standard names.
#'
#' @param distributor Character vector of distributor names
#' @return Character vector with standardized names
#' @keywords internal
standardize_distributor <- function(distributor) {
  distributor <- stringr::str_trim(distributor)

  # Common mappings for major studios
  distributor <- dplyr::case_when(
    stringr::str_detect(distributor, "(?i)disney|buena vista|marvel|lucasfilm|pixar|20th century") ~ "Disney",
    stringr::str_detect(distributor, "(?i)warner|wb|new line|dc") ~ "Warner Bros.",
    stringr::str_detect(distributor, "(?i)universal|focus features|illumination") ~ "Universal",
    stringr::str_detect(distributor, "(?i)paramount") ~ "Paramount",
    stringr::str_detect(distributor, "(?i)sony|columbia|screen gems|tristar") ~ "Sony",
    stringr::str_detect(distributor, "(?i)lionsgate|summit") ~ "Lionsgate",
    stringr::str_detect(distributor, "(?i)a24") ~ "A24",
    stringr::str_detect(distributor, "(?i)mgm|metro-goldwyn|amazon|orion") ~ "MGM/Amazon",
    stringr::str_detect(distributor, "(?i)stx") ~ "STX",
    stringr::str_detect(distributor, "(?i)neon") ~ "Neon",
    stringr::str_detect(distributor, "(?i)searchlight") ~ "Searchlight",
    TRUE ~ distributor
  )

  distributor
}

#' Determine distributor tier
#'
#' Classifies distributors into major, mini-major, or indie tiers.
#'
#' @param distributor Character vector of standardized distributor names
#' @return Character vector with tier classifications
#' @keywords internal
get_distributor_tier <- function(distributor) {
  dplyr::case_when(
    distributor %in% c("Disney", "Warner Bros.", "Universal", "Paramount", "Sony") ~ "major",
    distributor %in% c("Lionsgate", "MGM/Amazon", "STX") ~ "mini_major",
    TRUE ~ "indie"
  )
}

#' Check if a date is a holiday period
#'
#' Identifies major US holiday periods that affect box office.
#'
#' @param date Date vector
#' @return Logical vector
#' @keywords internal
is_holiday_period <- function(date) {
  month <- lubridate::month(date)
  day <- lubridate::day(date)

  # Thanksgiving week (late November)
  thanksgiving <- month == 11 & day >= 20

  # Christmas/New Year (Dec 15 - Jan 5)
  christmas <- (month == 12 & day >= 15) | (month == 1 & day <= 5)

  # Memorial Day weekend (late May)
  memorial <- month == 5 & day >= 24

  # July 4th period
  july4 <- month == 7 & day >= 1 & day <= 7

  # Labor Day (early September)
  labor <- month == 9 & day <= 7

  thanksgiving | christmas | memorial | july4 | labor
}

#' Check if a date is in summer movie season
#'
#' @param date Date vector
#' @return Logical vector
#' @keywords internal
is_summer_season <- function(date) {
  month <- lubridate::month(date)
  month >= 5 & month <= 8
}

#' Find the opening weekend rows in daily data
#'
#' Returns the row indices corresponding to the first Fri-Sat-Sun window.
#' If `day_of_week` is unavailable, falls back to days 1-3.
#'
#' @param daily_data Data frame with daily box office (must have `day_number`,
#'   optionally `day_of_week`)
#' @return Integer vector of row indices for the opening weekend
#' @keywords internal
opening_weekend_rows <- function(daily_data) {
  if (!"day_of_week" %in% names(daily_data) || all(is.na(daily_data$day_of_week))) {
    # Fallback: assume days 1-3
    return(which(daily_data$day_number <= 3))
  }

  # Find first Friday
  fri_idx <- which(daily_data$day_of_week == "Friday")
  if (length(fri_idx) == 0) {
    # No Friday in data — use days 1-3
    return(which(daily_data$day_number <= 3))
  }

  first_fri <- fri_idx[1]
  first_fri_day <- daily_data$day_number[first_fri]

  # Opening weekend = that Friday + next 2 days (Sat, Sun)
  weekend_days <- first_fri_day:(first_fri_day + 2)
  which(daily_data$day_number %in% weekend_days)
}

#' Validate CPI data integrity
#'
#' Checks that the CPI table is roughly monotonically increasing (post-1940)
#' and has no implausible jumps. Run once per session on first use.
#'
#' @return Invisible TRUE if valid; warns on issues
#' @keywords internal
validate_cpi_data <- function() {
  # Only validate once per session
  if (isTRUE(.boxoffice_env$cpi_validated)) return(invisible(TRUE))

  cpi <- cpi_annual
  post_1940 <- cpi[cpi$year >= 1940, ]

  # Check for year-over-year decreases > 5% (extremely rare, only in

  # severe deflation). A hand-typed digit swap would show as a big drop.
  pct_change <- diff(post_1940$cpi) / head(post_1940$cpi, -1)
  big_drops <- which(pct_change < -0.05)

  if (length(big_drops) > 0) {
    bad_years <- post_1940$year[big_drops + 1]
    cli::cli_warn(c(
      "CPI data may have errors: unexpected drop(s) in {paste(bad_years, collapse = ', ')}.",
      "i" = "Check {.file data-raw/cpi.R} for typos in the CPI values."
    ))
  }

  # Check for implausibly large jumps (> 20% in one year)
  big_jumps <- which(pct_change > 0.20)
  if (length(big_jumps) > 0) {
    bad_years <- post_1940$year[big_jumps + 1]
    cli::cli_warn(c(
      "CPI data may have errors: implausibly large jump(s) in {paste(bad_years, collapse = ', ')}.",
      "i" = "Check {.file data-raw/cpi.R} for typos in the CPI values."
    ))
  }

  .boxoffice_env$cpi_validated <- TRUE
  invisible(TRUE)
}

#' Adjust revenue for inflation using CPI-U
#'
#' Converts nominal dollars to constant dollars for a target year using the
#' bundled [cpi_annual] dataset. When a release date is provided, interpolates
#' between adjacent years' CPI values based on the exact date position within
#' the year, since annual CPI represents the mid-year average.
#'
#' @param revenue Numeric vector of revenue values in nominal dollars
#' @param from_year Integer vector of the year each revenue was earned
#' @param to_year Integer target year for constant dollars (default: most
#'   recent year in the CPI table)
#' @param release_date Optional Date vector for proportional within-year
#'   interpolation. When NULL, uses the flat annual CPI for `from_year`.
#' @return Numeric vector of inflation-adjusted revenue
#' @export
#' @examples
#' # What is $100M in 1990 dollars worth in 2025 dollars?
#' adjust_inflation(1e8, 1990, 2025)
#'
#' # With date-based interpolation (Jan release vs Dec release)
#' adjust_inflation(1e8, 1990, 2025, as.Date("1990-01-15"))
#' adjust_inflation(1e8, 1990, 2025, as.Date("1990-12-15"))
adjust_inflation <- function(revenue, from_year, to_year = max(cpi_annual$year),
                             release_date = NULL) {
  validate_cpi_data()
  cpi_lookup <- stats::setNames(cpi_annual$cpi, cpi_annual$year)
  min_year <- min(cpi_annual$year)
  max_year <- max(cpi_annual$year)

  # Validate target year
  if (!as.character(to_year) %in% names(cpi_lookup)) {
    cli::cli_abort(c(
      "No CPI data for target year {to_year}.",
      "i" = "CPI table covers {min_year}-{max_year}.",
      "i" = "Update {.file data-raw/cpi.R} to add newer years."
    ))
  }

  to_cpi <- cpi_lookup[as.character(to_year)]

  if (!is.null(release_date)) {
    # Interpolate CPI based on exact date position within the year.
    # Annual CPI is centered ~July 1 (mid-year average), so a date's
    # fractional position tells us how to weight adjacent years.
    release_date <- as.Date(release_date)
    day_of_year <- as.integer(format(release_date, "%j"))
    days_in_year <- ifelse(
      lubridate::leap_year(from_year), 366, 365
    )
    # fraction: 0.0 = Jan 1, 0.5 ~= July 1 (CPI midpoint), 1.0 = Dec 31
    frac <- (day_of_year - 1) / (days_in_year - 1)

    # Offset from CPI midpoint: -0.5 (Jan 1) to +0.5 (Dec 31)
    offset <- frac - 0.5

    cpi_this <- cpi_lookup[as.character(from_year)]
    # For interpolation, use adjacent year in the direction of the offset
    adj_year <- ifelse(offset < 0, from_year - 1L, from_year + 1L)
    cpi_adj <- cpi_lookup[as.character(adj_year)]

    # Interpolate: at mid-year (offset=0) use cpi_this exactly,
    # at edges blend halfway toward the adjacent year
    from_cpi <- ifelse(
      is.na(cpi_adj),
      cpi_this,
      cpi_this + abs(offset) * (cpi_adj - cpi_this)
    )
  } else {
    from_cpi <- cpi_lookup[as.character(from_year)]
  }

  # For from_years outside the table, clamp to the nearest available year
  missing_from <- is.na(from_cpi) & !is.na(from_year)
  if (any(missing_from)) {
    bad_years <- paste(unique(from_year[missing_from]), collapse = ", ")
    cli::cli_warn(
      "No CPI for {bad_years}; adjustment uses {max_year} CPI (as at {Sys.Date()})."
    )
    from_cpi[missing_from] <- cpi_lookup[as.character(max_year)]
  }

  adjusted <- revenue * (to_cpi / from_cpi)
  ifelse(is.na(adjusted), revenue, adjusted)
}

#' Format large numbers for display
#'
#' @param x Numeric vector
#' @param prefix Character prefix (e.g., "$")
#' @return Character vector with formatted numbers
#' @keywords internal
format_gross <- function(x, prefix = "$") {
  sign <- ifelse(!is.na(x) & x < 0, "-", "")
  ax <- abs(x)
  dplyr::case_when(
    is.na(ax) ~ NA_character_,
    ax >= 1e9 ~ paste0(sign, prefix, round(ax / 1e9, 2), "B"),
    ax >= 1e6 ~ paste0(sign, prefix, round(ax / 1e6, 1), "M"),
    ax >= 1e3 ~ paste0(sign, prefix, round(ax / 1e3, 1), "K"),
    TRUE ~ paste0(sign, prefix, round(ax, 0))
  )
}
