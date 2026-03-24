#' Extract all features for prediction
#'
#' Combines opening, temporal, trajectory, and categorical features
#' into a single feature vector for a film.
#'
#' @param daily_data Data frame with daily box office
#' @param metadata List with film metadata
#' @param n_days Number of opening days to use as features (default: 10)
#' @return Data frame with one row containing all features
#' @export
extract_all_features <- function(daily_data, metadata = NULL, n_days = 10) {
  features <- data.frame(row.names = 1)

  # Opening features
  opening <- extract_opening_features(daily_data, n_days)
  features <- cbind(features, opening)

  # Trajectory features
  trajectory <- extract_trajectory_features(daily_data, n_days)
  features <- cbind(features, trajectory)

  # Theater features
  theater <- extract_theater_features(daily_data)
  features <- cbind(features, theater)

  # Temporal features
  if (!is.null(metadata) && !is.null(metadata$release_date)) {
    temporal <- extract_temporal_features(metadata$release_date)
    features <- cbind(features, temporal)
  }

  # Categorical features
  if (!is.null(metadata)) {
    categorical <- extract_categorical_features(metadata)
    features <- cbind(features, categorical)
  }

  features
}

#' Extract opening period features
#'
#' @param daily_data Data frame with daily box office
#' @param n_days Number of days to consider (default: 10)
#' @return Data frame with opening features
#' @export
extract_opening_features <- function(daily_data, n_days = 10) {
  if (is.null(daily_data) || nrow(daily_data) == 0) {
    return(data.frame(row.names = 1))
  }

  daily_data <- daily_data[order(daily_data$day_number), ]

  # Limit to first n_days
  early_data <- daily_data[daily_data$day_number <= n_days, ]

  features <- data.frame(row.names = 1)

  # Day 1 metrics
  if (any(early_data$day_number == 1)) {
    day1 <- early_data[early_data$day_number == 1, ]
    features$day1_gross <- day1$daily_gross[1]
    features$day1_theaters <- day1$theaters[1]
    features$day1_per_theater <- if (!is.null(day1$per_theater_avg[1])) day1$per_theater_avg[1] else
      day1$daily_gross[1] / day1$theaters[1]
  }

  # Opening weekend (days 1-3)
  weekend1 <- early_data[early_data$day_number <= 3, ]
  if (nrow(weekend1) > 0) {
    features$weekend1_gross <- sum(weekend1$daily_gross, na.rm = TRUE)
    features$weekend1_days <- nrow(weekend1)
  }

  # Opening week (days 1-7)
  week1 <- early_data[early_data$day_number <= 7, ]
  if (nrow(week1) > 0) {
    features$week1_gross <- sum(week1$daily_gross, na.rm = TRUE)
    features$week1_avg_daily <- mean(week1$daily_gross, na.rm = TRUE)
  }

  # Cumulative at n_days
  if ("cumulative_gross" %in% names(early_data)) {
    last_day <- early_data[early_data$day_number == max(early_data$day_number), ]
    features$cumulative_n_days <- last_day$cumulative_gross[1]
    features$n_days_used <- max(early_data$day_number)
  }

  # Weekend to weekday patterns
  if ("day_of_week" %in% names(early_data) && nrow(early_data) >= 7) {
    weekend_days <- c("Friday", "Saturday", "Sunday")
    weekend_gross <- sum(early_data$daily_gross[early_data$day_of_week %in% weekend_days], na.rm = TRUE)
    weekday_gross <- sum(early_data$daily_gross[!early_data$day_of_week %in% weekend_days], na.rm = TRUE)

    if (weekday_gross > 0) {
      features$weekend_weekday_ratio <- weekend_gross / weekday_gross
    }
  }

  # Second weekend (days 8-10) if available
  if (n_days >= 10) {
    weekend2 <- early_data[early_data$day_number >= 8 & early_data$day_number <= 10, ]
    if (nrow(weekend2) > 0 && !is.null(features$weekend1_gross)) {
      features$weekend2_gross <- sum(weekend2$daily_gross, na.rm = TRUE)
      features$weekend2_drop <- (features$weekend1_gross - features$weekend2_gross) / features$weekend1_gross
    }
  }

  features
}

#' Extract trajectory and decay features
#'
#' @param daily_data Data frame with daily box office
#' @param n_days Number of days to analyze
#' @return Data frame with trajectory features
#' @export
extract_trajectory_features <- function(daily_data, n_days = 10) {
  if (is.null(daily_data) || nrow(daily_data) < 3) {
    return(data.frame(row.names = 1))
  }

  daily_data <- daily_data[order(daily_data$day_number), ]
  early_data <- daily_data[daily_data$day_number <= n_days, ]

  features <- data.frame(row.names = 1)

  # Daily decay rate (average day-over-day change)
  if (nrow(early_data) >= 2) {
    daily_changes <- diff(log(early_data$daily_gross + 1))
    features$avg_daily_decay <- mean(daily_changes, na.rm = TRUE)
    features$decay_volatility <- stats::sd(daily_changes, na.rm = TRUE)
  }

  # Fit exponential decay curve
  # log(gross) = a - b * day
  if (nrow(early_data) >= 5) {
    tryCatch({
      log_gross <- log(early_data$daily_gross + 1)
      fit <- stats::lm(log_gross ~ early_data$day_number)
      features$decay_slope <- stats::coef(fit)[2]
      features$decay_r_squared <- summary(fit)$r.squared
    }, error = function(e) NULL)
  }

  # Weekend hold patterns
  if ("day_of_week" %in% names(early_data)) {
    saturdays <- early_data$daily_gross[early_data$day_of_week == "Saturday"]
    sundays <- early_data$daily_gross[early_data$day_of_week == "Sunday"]

    if (length(saturdays) > 0 && length(sundays) > 0) {
      features$sat_sun_ratio <- mean(sundays, na.rm = TRUE) / mean(saturdays, na.rm = TRUE)
    }
  }

  # Momentum: is the film accelerating or decelerating?
  if (nrow(early_data) >= 7) {
    first_half <- early_data[early_data$day_number <= ceiling(n_days/2), ]
    second_half <- early_data[early_data$day_number > ceiling(n_days/2), ]

    if (nrow(first_half) > 0 && nrow(second_half) > 0) {
      first_avg <- mean(first_half$daily_gross, na.rm = TRUE)
      second_avg <- mean(second_half$daily_gross, na.rm = TRUE)
      features$momentum_score <- second_avg / first_avg
    }
  }

  features
}

#' Extract theater-related features
#'
#' @param daily_data Data frame with daily box office
#' @return Data frame with theater features
#' @export
extract_theater_features <- function(daily_data) {

  if (is.null(daily_data) || nrow(daily_data) == 0 || !"theaters" %in% names(daily_data)) {
    return(data.frame(row.names = 1))
  }

  daily_data <- daily_data[order(daily_data$day_number), ]
  theaters <- daily_data$theaters[!is.na(daily_data$theaters)]

  if (length(theaters) == 0) {
    return(data.frame(row.names = 1))
  }

  features <- data.frame(row.names = 1)

  features$opening_theaters <- theaters[1]
  features$max_theaters <- max(theaters, na.rm = TRUE)

  # Log scale for modeling
  features$log_opening_theaters <- log(theaters[1] + 1)

  # Release type indicator
  features$is_wide_release <- as.integer(theaters[1] >= 3000)
  features$is_limited_release <- as.integer(theaters[1] < 600)

  # Theater trajectory
  if (length(theaters) >= 2) {
    # Did theaters expand?
    features$theaters_expanded <- as.integer(max(theaters) > theaters[1])

    # Theater change by day 7 (if available)
    week1_theaters <- daily_data$theaters[daily_data$day_number <= 7]
    if (length(week1_theaters) >= 2) {
      features$theater_change_week1 <- (week1_theaters[length(week1_theaters)] - week1_theaters[1]) /
        week1_theaters[1]
    }
  }

  # Per-theater average trend
  if ("per_theater_avg" %in% names(daily_data)) {
    pta <- daily_data$per_theater_avg[!is.na(daily_data$per_theater_avg)]
    if (length(pta) >= 1) {
      features$opening_per_theater <- pta[1]
      features$log_opening_pta <- log(pta[1] + 1)
    }
  }

  features
}

#' Extract temporal/seasonality features
#'
#' @param release_date Date of film release
#' @return Data frame with temporal features
#' @export
extract_temporal_features <- function(release_date) {
  if (is.null(release_date) || is.na(release_date)) {
    return(data.frame(row.names = 1))
  }

  features <- data.frame(row.names = 1)

  # Basic date components
  features$release_year <- lubridate::year(release_date)
  features$release_month <- lubridate::month(release_date)
  features$release_day <- lubridate::day(release_date)
  features$release_dow <- lubridate::wday(release_date, label = FALSE)
  features$release_week <- lubridate::week(release_date)

  # Season indicators
  month <- features$release_month
  features$is_summer <- as.integer(month >= 5 & month <= 8)
  features$is_holiday <- as.integer(month %in% c(11, 12) | (month == 1 & features$release_day <= 7))
  features$is_q1 <- as.integer(month <= 3)
  features$is_q2 <- as.integer(month >= 4 & month <= 6)
  features$is_q3 <- as.integer(month >= 7 & month <= 9)
  features$is_q4 <- as.integer(month >= 10)

  # Day of week (Friday is typical premiere)
  features$is_friday_release <- as.integer(features$release_dow == 6)
  features$is_wednesday_release <- as.integer(features$release_dow == 4)

  features
}

#' Extract categorical features
#'
#' @param metadata List with film metadata
#' @return Data frame with encoded categorical features
#' @export
extract_categorical_features <- function(metadata) {
  if (is.null(metadata)) {
    return(data.frame(row.names = 1))
  }

  features <- data.frame(row.names = 1)

  # Genre encoding (binary indicators for common genres)
  if (!is.null(metadata$genres) && length(metadata$genres) > 0) {
    genres <- standardize_genre(metadata$genres)

    common_genres <- c("Action", "Comedy", "Drama", "Horror", "Sci-Fi",
                       "Animation", "Romance", "Thriller", "Adventure", "Fantasy")

    for (g in common_genres) {
      features[[paste0("genre_", tolower(g))]] <- as.integer(g %in% genres)
    }

    features$genre_count <- length(genres)
  }

  # MPAA rating
  if (!is.null(metadata$mpaa_rating) && !is.na(metadata$mpaa_rating)) {
    rating <- toupper(metadata$mpaa_rating)
    features$is_rated_g <- as.integer(rating == "G")
    features$is_rated_pg <- as.integer(rating == "PG")
    features$is_rated_pg13 <- as.integer(rating == "PG-13")
    features$is_rated_r <- as.integer(rating == "R")
  }

  # Distributor tier
  if (!is.null(metadata$distributor) && !is.na(metadata$distributor)) {
    dist <- standardize_distributor(metadata$distributor)
    tier <- get_distributor_tier(dist)
    features$is_major_studio <- as.integer(tier == "major")
    features$is_indie <- as.integer(tier == "indie")
  }

  # Budget (if available)
  if (!is.null(metadata$budget) && !is.na(metadata$budget)) {
    features$budget <- metadata$budget
    features$log_budget <- log(metadata$budget + 1)

    # Budget tier
    features$is_blockbuster_budget <- as.integer(metadata$budget >= 150e6)
    features$is_mid_budget <- as.integer(metadata$budget >= 30e6 & metadata$budget < 150e6)
    features$is_low_budget <- as.integer(metadata$budget < 30e6)
  }

  # Runtime
  if (!is.null(metadata$runtime) && !is.na(metadata$runtime)) {
    features$runtime <- metadata$runtime
    features$is_long_runtime <- as.integer(metadata$runtime >= 150)
    features$is_short_runtime <- as.integer(metadata$runtime < 90)
  }

  features
}

#' Build feature matrix for multiple films
#'
#' @param films_data Data frame with film metadata
#' @param daily_data_list Named list of daily data frames
#' @param n_days Number of opening days to use
#' @param progress Show progress bar
#' @return Data frame with features for all films
#' @export
build_feature_matrix <- function(films_data, daily_data_list, n_days = 10, progress = TRUE) {
  n <- nrow(films_data)

  if (n == 0) {
    return(data.frame())
  }

  if (progress) {
    cli::cli_progress_bar("Building features", total = n)
  }

  results <- vector("list", n)

  for (i in seq_len(n)) {
    film <- films_data[i, ]
    film_id <- film$release_id[1]

    # Get daily data
    daily <- daily_data_list[[film_id]]

    # Get metadata as list
    meta <- as.list(film)

    # Extract features
    features <- extract_all_features(daily, meta, n_days)

    # Add film identifier
    features$release_id <- film_id
    features$title <- film$title[1]
    features$year <- film$year[1]

    results[[i]] <- features

    if (progress) {
      cli::cli_progress_update()
    }
  }

  if (progress) {
    cli::cli_progress_done()
  }

  # Combine all features
  dplyr::bind_rows(results)
}
