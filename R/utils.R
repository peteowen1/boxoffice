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

#' Format large numbers for display
#'
#' @param x Numeric vector
#' @param prefix Character prefix (e.g., "$")
#' @return Character vector with formatted numbers
#' @keywords internal
format_gross <- function(x, prefix = "$") {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 1e9 ~ paste0(prefix, round(x / 1e9, 2), "B"),
    abs(x) >= 1e6 ~ paste0(prefix, round(x / 1e6, 1), "M"),
    abs(x) >= 1e3 ~ paste0(prefix, round(x / 1e3, 1), "K"),
    TRUE ~ paste0(prefix, round(x, 0))
  )
}
