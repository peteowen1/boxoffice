#' Merge daily data from both sources for a film
#'
#' Combines daily box office data from Box Office Mojo and The Numbers,
#' preferring BOM data where available but filling gaps from Numbers.
#'
#' @param bom_daily Data frame from scrape_bom_daily
#' @param numbers_daily Data frame from scrape_numbers_daily
#' @param prefer_source Which source to prefer for conflicts ("bom" or "numbers")
#' @return Merged data frame with source indicator for each day
#' @export
merge_daily_data <- function(bom_daily, numbers_daily, prefer_source = "bom") {
  # Handle NULL/empty inputs
  if (is.null(bom_daily) || nrow(bom_daily) == 0) {
    if (is.null(numbers_daily) || nrow(numbers_daily) == 0) {
      return(NULL)
    }
    numbers_daily$source <- "numbers"
    return(numbers_daily)
  }

  if (is.null(numbers_daily) || nrow(numbers_daily) == 0) {
    bom_daily$source <- "bom"
    return(bom_daily)
  }

  # Standardize column names
  bom_cols <- c("date", "day_number", "day_of_week", "rank", "daily_gross",
                "theaters", "per_theater_avg", "cumulative_gross")
  numbers_cols <- c("date", "day_number", "day_of_week", "rank", "daily_gross",
                    "theaters", "per_theater_avg", "cumulative_gross")

  # Select common columns that exist
  bom_daily <- bom_daily[, intersect(names(bom_daily), bom_cols), drop = FALSE]
  numbers_daily <- numbers_daily[, intersect(names(numbers_daily), numbers_cols), drop = FALSE]

  # Add source indicator
  bom_daily$source <- "bom"
  numbers_daily$source <- "numbers"

  if (prefer_source == "bom") {
    primary <- bom_daily
    secondary <- numbers_daily
  } else {
    primary <- numbers_daily
    secondary <- bom_daily
  }

  # Merge by date or day_number
  if ("date" %in% names(primary) && "date" %in% names(secondary)) {
    # Fill missing days from secondary source
    missing_dates <- setdiff(secondary$date, primary$date)
    if (length(missing_dates) > 0) {
      fill_data <- secondary[secondary$date %in% missing_dates, ]
      primary <- rbind(primary, fill_data)
    }
    primary <- primary[order(primary$date), ]
  } else if ("day_number" %in% names(primary) && "day_number" %in% names(secondary)) {
    missing_days <- setdiff(secondary$day_number, primary$day_number)
    if (length(missing_days) > 0) {
      fill_data <- secondary[secondary$day_number %in% missing_days, ]
      primary <- rbind(primary, fill_data)
    }
    primary <- primary[order(primary$day_number), ]
  }

  primary
}

#' Merge metadata from both sources
#'
#' Combines film metadata from Box Office Mojo and The Numbers,
#' preferring The Numbers for budget data and BOM for other fields.
#'
#' @param bom_metadata List from scrape_bom_metadata
#' @param numbers_metadata List from scrape_numbers_metadata
#' @return List with merged metadata
#' @export
merge_metadata <- function(bom_metadata, numbers_metadata) {
  # Start with BOM as base
  if (is.null(bom_metadata)) {
    bom_metadata <- list()
  }
  if (is.null(numbers_metadata)) {
    numbers_metadata <- list()
  }

  merged <- list(
    title = bom_metadata$title %||% numbers_metadata$title,
    release_date = bom_metadata$release_date %||% numbers_metadata$release_date,
    mpaa_rating = bom_metadata$mpaa_rating %||% numbers_metadata$mpaa_rating,
    runtime = bom_metadata$runtime %||% numbers_metadata$runtime,
    genres = if (length(bom_metadata$genres) > 0) bom_metadata$genres else numbers_metadata$genres,
    distributor = bom_metadata$distributor %||% numbers_metadata$distributor,

    # Prefer Numbers for budget (more reliable)
    budget = numbers_metadata$budget %||% bom_metadata$budget,

    # Financial data - prefer BOM for consistency
    opening_gross = bom_metadata$opening_gross %||% numbers_metadata$opening_weekend,
    opening_theaters = bom_metadata$opening_theaters %||% numbers_metadata$opening_theaters,
    domestic_gross = bom_metadata$domestic_gross %||% numbers_metadata$domestic_gross,
    international_gross = bom_metadata$international_gross %||% numbers_metadata$international_gross,
    worldwide_gross = bom_metadata$worldwide_gross %||% numbers_metadata$worldwide_gross,

    # Source identifiers
    bom_id = bom_metadata$release_id,
    numbers_slug = numbers_metadata$movie_slug
  )

  merged
}

#' Validate daily box office data
#'
#' Checks daily data for common issues and inconsistencies.
#'
#' @param daily_data Data frame with daily box office
#' @return List with: is_valid, issues (character vector), cleaned_data
#' @export
validate_daily_data <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(list(is_valid = FALSE, issues = "Empty data", cleaned_data = NULL))
  }

  issues <- character(0)
  cleaned <- daily_data

  # Check for required columns
  required_cols <- c("day_number", "daily_gross")
  missing_cols <- setdiff(required_cols, names(cleaned))
  if (length(missing_cols) > 0) {
    issues <- c(issues, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check for negative values
  if ("daily_gross" %in% names(cleaned)) {
    neg_gross <- sum(cleaned$daily_gross < 0, na.rm = TRUE)
    if (neg_gross > 0) {
      issues <- c(issues, paste(neg_gross, "negative daily gross values"))
      cleaned$daily_gross[cleaned$daily_gross < 0] <- NA
    }
  }

  if ("theaters" %in% names(cleaned)) {
    neg_theaters <- sum(cleaned$theaters < 0, na.rm = TRUE)
    if (neg_theaters > 0) {
      issues <- c(issues, paste(neg_theaters, "negative theater counts"))
      cleaned$theaters[cleaned$theaters < 0] <- NA
    }
  }

  # Check cumulative is monotonically increasing
  if ("cumulative_gross" %in% names(cleaned)) {
    cum_diff <- diff(cleaned$cumulative_gross)
    decreasing <- sum(cum_diff < 0, na.rm = TRUE)
    if (decreasing > 0) {
      issues <- c(issues, paste(decreasing, "days with decreasing cumulative"))
    }
  }

  # Check for gaps in day_number
  if ("day_number" %in% names(cleaned)) {
    expected_days <- seq(min(cleaned$day_number, na.rm = TRUE),
                         max(cleaned$day_number, na.rm = TRUE))
    missing_days <- setdiff(expected_days, cleaned$day_number)
    if (length(missing_days) > 0) {
      issues <- c(issues, paste(length(missing_days), "missing days"))
    }
  }

  list(
    is_valid = length(issues) == 0,
    issues = issues,
    cleaned_data = cleaned
  )
}

#' Fill missing days in daily data
#'
#' Interpolates or fills missing days in daily box office data.
#'
#' @param daily_data Data frame with daily box office
#' @param method Character: "interpolate", "zero", or "previous"
#' @return Data frame with complete daily series
#' @export
fill_missing_days <- function(daily_data, method = "interpolate") {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(daily_data)
  }

  if (!"day_number" %in% names(daily_data)) {
    return(daily_data)
  }

  min_day <- min(daily_data$day_number, na.rm = TRUE)
  max_day <- max(daily_data$day_number, na.rm = TRUE)
  all_days <- seq(min_day, max_day)
  missing_days <- setdiff(all_days, daily_data$day_number)

  if (length(missing_days) == 0) {
    return(daily_data)
  }

  # Create rows for missing days
  missing_df <- data.frame(day_number = missing_days)

  # Add other columns with NA
  for (col in setdiff(names(daily_data), "day_number")) {
    missing_df[[col]] <- NA
  }

  # Combine and sort
  combined <- rbind(daily_data, missing_df)
  combined <- combined[order(combined$day_number), ]

  # Fill values based on method
  if (method == "interpolate") {
    # Linear interpolation for numeric columns
    numeric_cols <- c("daily_gross", "theaters", "per_theater_avg", "cumulative_gross")
    for (col in intersect(numeric_cols, names(combined))) {
      if (sum(!is.na(combined[[col]])) >= 2) {
        combined[[col]] <- stats::approx(
          x = combined$day_number,
          y = combined[[col]],
          xout = combined$day_number,
          rule = 2
        )$y
      }
    }
  } else if (method == "zero") {
    combined$daily_gross[is.na(combined$daily_gross)] <- 0
  } else if (method == "previous") {
    # Forward fill
    for (col in names(combined)) {
      if (is.numeric(combined[[col]])) {
        combined[[col]] <- zoo::na.locf(combined[[col]], na.rm = FALSE)
      }
    }
  }

  # Fill day_of_week if date is available
  if ("date" %in% names(combined) && "day_of_week" %in% names(combined)) {
    combined$day_of_week <- weekdays(combined$date)
  }

  combined
}

#' Calculate derived metrics for daily data
#'
#' Adds calculated fields like week number, weekend flags, and rolling averages.
#'
#' @param daily_data Data frame with daily box office
#' @return Data frame with additional columns
#' @export
calculate_derived_metrics <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(daily_data)
  }

  # Week number
  if ("day_number" %in% names(daily_data)) {
    daily_data$week_number <- ceiling(daily_data$day_number / 7)
  }

  # Weekend flag
  if ("day_of_week" %in% names(daily_data)) {
    weekend_days <- c("Friday", "Saturday", "Sunday")
    daily_data$is_weekend <- daily_data$day_of_week %in% weekend_days
  }

  # Calculate percent of opening day
  if ("daily_gross" %in% names(daily_data)) {
    opening_gross <- daily_data$daily_gross[daily_data$day_number == 1]
    if (length(opening_gross) == 1 && !is.na(opening_gross) && opening_gross > 0) {
      daily_data$pct_of_opening <- daily_data$daily_gross / opening_gross
    }
  }

  # Day-over-day change
  if ("daily_gross" %in% names(daily_data)) {
    daily_data <- daily_data[order(daily_data$day_number), ]
    daily_data$daily_change <- c(NA, diff(daily_data$daily_gross))
    daily_data$daily_pct_change <- daily_data$daily_change / dplyr::lag(daily_data$daily_gross)
  }

  # 7-day rolling average
  if ("daily_gross" %in% names(daily_data) && nrow(daily_data) >= 7) {
    daily_data$rolling_7d_avg <- stats::filter(
      daily_data$daily_gross,
      rep(1/7, 7),
      sides = 1
    )
  }

  daily_data
}

#' Calculate opening period metrics
#'
#' Computes opening day, weekend, and week metrics.
#'
#' @param daily_data Data frame with daily box office
#' @return List with opening metrics
#' @export
calculate_opening_metrics <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(list())
  }

  # Sort by day number
  daily_data <- daily_data[order(daily_data$day_number), ]

  metrics <- list()

  # Opening day (day 1)
  day1 <- daily_data[daily_data$day_number == 1, ]
  if (nrow(day1) == 1) {
    metrics$opening_day_gross <- day1$daily_gross[1]
    metrics$opening_day_theaters <- day1$theaters[1]
    metrics$opening_day_avg <- day1$per_theater_avg[1]
    metrics$opening_day_of_week <- day1$day_of_week[1]
  }

  # Opening weekend (days 1-3, assuming Friday open)
  weekend <- daily_data[daily_data$day_number <= 3, ]
  if (nrow(weekend) >= 1) {
    metrics$opening_weekend_gross <- sum(weekend$daily_gross, na.rm = TRUE)
    metrics$opening_weekend_days <- nrow(weekend)

    # Friday-to-Sunday ratio if we have all 3 days
    if (nrow(weekend) == 3) {
      metrics$friday_saturday_ratio <- weekend$daily_gross[2] / weekend$daily_gross[1]
      metrics$saturday_sunday_ratio <- weekend$daily_gross[3] / weekend$daily_gross[2]
    }
  }

  # Opening week (days 1-7)
  week1 <- daily_data[daily_data$day_number <= 7, ]
  if (nrow(week1) >= 1) {
    metrics$opening_week_gross <- sum(week1$daily_gross, na.rm = TRUE)
  }

  # Weekend-to-total ratio (for first week)
  if (!is.null(metrics$opening_weekend_gross) && !is.null(metrics$opening_week_gross)) {
    metrics$weekend_to_week_ratio <- metrics$opening_weekend_gross / metrics$opening_week_gross
  }

  # Second weekend drop
  weekend2 <- daily_data[daily_data$day_number >= 8 & daily_data$day_number <= 10, ]
  if (nrow(weekend2) >= 1 && !is.null(metrics$opening_weekend_gross)) {
    second_weekend_gross <- sum(weekend2$daily_gross, na.rm = TRUE)
    metrics$second_weekend_gross <- second_weekend_gross
    metrics$second_weekend_drop <- (metrics$opening_weekend_gross - second_weekend_gross) /
      metrics$opening_weekend_gross
  }

  metrics
}

#' Calculate theater metrics
#'
#' Computes theater-related metrics like expansion rate and decay.
#'
#' @param daily_data Data frame with daily box office
#' @return List with theater metrics
#' @export
calculate_theater_metrics <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0 || !"theaters" %in% names(daily_data)) {
    return(list())
  }

  daily_data <- daily_data[order(daily_data$day_number), ]
  theaters <- daily_data$theaters[!is.na(daily_data$theaters)]

  if (length(theaters) == 0) {
    return(list())
  }

  metrics <- list(
    opening_theaters = theaters[1],
    peak_theaters = max(theaters, na.rm = TRUE),
    final_theaters = theaters[length(theaters)],
    min_theaters = min(theaters, na.rm = TRUE)
  )

  # Peak timing
  peak_idx <- which.max(theaters)
  metrics$days_to_peak <- daily_data$day_number[peak_idx]

  # Theater trajectory
  if (length(theaters) >= 2) {
    # Expansion percentage (from opening to peak)
    if (metrics$peak_theaters > metrics$opening_theaters) {
      metrics$expansion_pct <- (metrics$peak_theaters - metrics$opening_theaters) /
        metrics$opening_theaters
    } else {
      metrics$expansion_pct <- 0
    }

    # Contraction from peak to final
    if (metrics$peak_theaters > 0) {
      metrics$contraction_pct <- (metrics$peak_theaters - metrics$final_theaters) /
        metrics$peak_theaters
    }
  }

  # Release type
  metrics$release_type <- dplyr::case_when(
    metrics$opening_theaters >= 3000 ~ "wide",
    metrics$opening_theaters >= 600 ~ "moderate",
    metrics$opening_theaters >= 100 ~ "limited",
    TRUE ~ "platform"
  )

  metrics
}

#' Calculate final run multiplier
#'
#' Computes the ratio of total gross to opening weekend.
#'
#' @param daily_data Data frame with daily box office
#' @return Numeric multiplier
#' @export
calculate_multiplier <- function(daily_data) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(NA_real_)
  }

  daily_data <- daily_data[order(daily_data$day_number), ]

  # Opening weekend (first 3 days)
  opening <- sum(daily_data$daily_gross[daily_data$day_number <= 3], na.rm = TRUE)

  # Total gross
  total <- if ("cumulative_gross" %in% names(daily_data)) {
    max(daily_data$cumulative_gross, na.rm = TRUE)
  } else {
    sum(daily_data$daily_gross, na.rm = TRUE)
  }

  if (is.na(opening) || opening == 0) {
    return(NA_real_)
  }

  total / opening
}
