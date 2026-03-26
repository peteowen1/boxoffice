#' Train theatrical run projection model
#'
#' Trains an XGBoost model to predict total domestic gross based on
#' opening performance and film characteristics.
#'
#' @param feature_matrix Data frame from build_feature_matrix
#' @param target Character: "total_gross" or "multiplier"
#' @param n_days Number of opening days used in features
#' @param cv_folds Number of cross-validation folds (default: 5)
#' @param verbose Show training progress
#' @return Trained model object with metadata
#' @export
#' @examples
#' \dontrun{
#' model <- train_projection_model(features, target = "total_gross")
#' }
train_projection_model <- function(
    feature_matrix,
    target = "total_gross",
    n_days = 10,
    cv_folds = 5,
    verbose = TRUE
) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package 'xgboost' is required for model training. Install with: install.packages('xgboost')")
  }

  # Prepare target variable
  if (target == "total_gross") {
    if (!"total_gross" %in% names(feature_matrix)) {
      cli::cli_abort("total_gross column not found in feature_matrix")
    }
    y <- log(feature_matrix$total_gross + 1)
    target_transform <- "log"
  } else if (target == "multiplier") {
    if (!"multiplier" %in% names(feature_matrix)) {
      # Calculate multiplier
      if ("total_gross" %in% names(feature_matrix) && "weekend1_gross" %in% names(feature_matrix)) {
        feature_matrix$multiplier <- feature_matrix$total_gross / feature_matrix$weekend1_gross
      } else {
        cli::cli_abort("Cannot calculate multiplier - missing total_gross or weekend1_gross")
      }
    }
    y <- feature_matrix$multiplier
    target_transform <- "none"
  } else {
    cli::cli_abort("Unknown target: {target}")
  }

  # Remove rows with missing target
  valid_rows <- !is.na(y) & is.finite(y)
  feature_matrix <- feature_matrix[valid_rows, ]
  y <- y[valid_rows]

  if (length(y) < 100) {
    cli::cli_warn("Training with only {length(y)} samples - model may not be reliable")
  }

  # Select feature columns (exclude identifiers and target)
  exclude_cols <- c("release_id", "title", "year", "total_gross", "multiplier",
                    "domestic_gross", "worldwide_gross", "numbers_slug", "bom_id")
  feature_cols <- setdiff(names(feature_matrix), exclude_cols)

  # Keep only numeric columns
  feature_cols <- feature_cols[vapply(feature_matrix[feature_cols], is.numeric, logical(1))]

  X <- as.matrix(feature_matrix[, feature_cols])

  # Handle any remaining NAs
  X[is.na(X)] <- 0

  if (verbose) {
    cli::cli_alert_info("Training with {nrow(X)} films and {ncol(X)} features")
  }

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  # XGBoost parameters
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  )

  # Cross-validation to find optimal rounds
  if (verbose) {
    cli::cli_alert_info("Running {cv_folds}-fold cross-validation...")
  }

  cv_result <- xgboost::xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 500,
    nfold = cv_folds,
    early_stopping_rounds = 20,
    verbose = 0
  )

  best_nrounds <- cv_result$best_iteration

  if (verbose) {
    cli::cli_alert_success("Best iteration: {best_nrounds}")
  }

  # Train final model
  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )

  # Get feature importance
  importance <- xgboost::xgb.importance(model = model)

  # Package model with metadata
  result <- list(
    model = model,
    feature_cols = feature_cols,
    target = target,
    target_transform = target_transform,
    n_days = n_days,
    n_samples = nrow(X),
    best_iteration = best_nrounds,
    cv_rmse = min(cv_result$evaluation_log$test_rmse_mean),
    importance = importance,
    trained_at = Sys.time()
  )

  class(result) <- c("boxoffice_model", "list")

  if (verbose) {
    cli::cli_alert_success("Model trained successfully")
    cli::cli_alert_info("CV RMSE: {round(result$cv_rmse, 4)}")
  }

  result
}

#' Project total domestic gross
#'
#' Uses a trained model to predict total gross for a film based on
#' its opening performance.
#'
#' @param model Trained model from train_projection_model
#' @param features Data frame with film features (single row)
#' @param confidence_level Confidence level for intervals (default: 0.95)
#' @return List with prediction and confidence interval
#' @export
project_total_gross <- function(model, features, confidence_level = 0.95) {
  if (!inherits(model, "boxoffice_model")) {
    cli::cli_abort("model must be a boxoffice_model object from train_projection_model()")
  }

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package 'xgboost' is required")
  }

  # Prepare features
  X <- as.matrix(features[, model$feature_cols, drop = FALSE])
  X[is.na(X)] <- 0

  dtest <- xgboost::xgb.DMatrix(data = X)

  # Get prediction
  pred <- predict(model$model, dtest)

  # Transform back if needed
  if (model$target_transform == "log") {
    pred_gross <- exp(pred) - 1
  } else if (model$target == "multiplier" && "weekend1_gross" %in% names(features)) {
    # Convert multiplier to gross
    pred_gross <- pred * features$weekend1_gross
  } else {
    pred_gross <- pred
  }

  # Estimate confidence interval using CV RMSE
  # Approximate: prediction +/- z * rmse
  z <- stats::qnorm((1 + confidence_level) / 2)
  rmse <- model$cv_rmse

  if (model$target_transform == "log") {
    # Transform intervals from log-space
    lower <- exp(pred - z * rmse) - 1
    upper <- exp(pred + z * rmse) - 1
  } else {
    # RMSE is in the same units as the target — use additive intervals
    lower <- pred_gross - z * rmse
    upper <- pred_gross + z * rmse
  }

  list(
    predicted_total = pred_gross[1],
    lower_bound = max(0, lower[1]),
    upper_bound = upper[1],
    confidence_level = confidence_level,
    n_days_used = model$n_days,
    model_target = model$target
  )
}

#' Project daily trajectory
#'
#' Projects the remaining daily gross trajectory using decay curve fitting.
#'
#' @param daily_data Data frame with available daily box office
#' @param days_ahead Number of days to project forward
#' @param method Projection method: "exponential", "power_law", or "weekday_adjusted"
#' @return Data frame with projected daily values
#' @export
project_daily_trajectory <- function(
    daily_data,
    days_ahead = 120,
    method = "weekday_adjusted"
) {
  if (is.null(daily_data) || nrow(daily_data) < 7) {
    cli::cli_abort("Need at least 7 days of data for trajectory projection")
  }

  daily_data <- daily_data[order(daily_data$day_number), ]
  last_day <- max(daily_data$day_number)

  # Fit decay model on log scale
  log_gross <- log(daily_data$daily_gross + 1)

  if (method == "exponential") {
    # log(y) = a - b*day
    fit <- stats::lm(log_gross ~ daily_data$day_number)
    a <- stats::coef(fit)[1]
    b <- stats::coef(fit)[2]

    # Project forward
    future_days <- seq(last_day + 1, last_day + days_ahead)
    projected_log <- a + b * future_days
    projected_gross <- exp(projected_log) - 1

  } else if (method == "power_law") {
    # log(y) = a - b*log(day)
    log_day <- log(daily_data$day_number)
    fit <- stats::lm(log_gross ~ log_day)
    a <- stats::coef(fit)[1]
    b <- stats::coef(fit)[2]

    future_days <- seq(last_day + 1, last_day + days_ahead)
    projected_log <- a + b * log(future_days)
    projected_gross <- exp(projected_log) - 1

  } else if (method == "weekday_adjusted") {
    # Exponential decay with weekday adjustment
    fit <- stats::lm(log_gross ~ daily_data$day_number)
    a <- stats::coef(fit)[1]
    b <- stats::coef(fit)[2]

    # Calculate weekday factors
    if ("day_of_week" %in% names(daily_data)) {
      daily_data$residual <- log_gross - (a + b * daily_data$day_number)
      weekday_factors <- stats::aggregate(residual ~ day_of_week, daily_data, mean)
    } else {
      weekday_factors <- data.frame(day_of_week = character(0), residual = numeric(0))
    }

    # Project forward
    future_days <- seq(last_day + 1, last_day + days_ahead)
    projected_gross <- numeric(length(future_days))

    # Infer last date for weekday calculation
    if ("date" %in% names(daily_data)) {
      last_date <- max(daily_data$date, na.rm = TRUE)
      future_dates <- last_date + seq_along(future_days)
      future_dow <- weekdays(future_dates)
    } else {
      # Assume Friday start
      future_dow <- rep(c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday"),
                        length.out = length(future_days))
    }

    base_pred <- a + b * future_days
    adj <- weekday_factors$residual[match(future_dow, weekday_factors$day_of_week)]
    adj[is.na(adj)] <- 0
    projected_gross <- exp(base_pred + adj) - 1
  }

  # Ensure non-negative
  projected_gross <- pmax(0, projected_gross)

  # Stop when daily gross is very small
  min_threshold <- 1000  # $1,000
  below_threshold <- which(projected_gross < min_threshold)
  if (length(below_threshold) > 0 && below_threshold[1] < length(projected_gross)) {
    cutoff <- below_threshold[1]
    future_days <- future_days[1:cutoff]
    projected_gross <- projected_gross[1:cutoff]
  }

  # Build projection data frame
  # Safely get prior cumulative — fall back to sum of daily if cumulative is missing
  prior_cumulative <- if ("cumulative_gross" %in% names(daily_data) &&
                          any(!is.na(daily_data$cumulative_gross))) {
    max(daily_data$cumulative_gross, na.rm = TRUE)
  } else {
    sum(daily_data$daily_gross, na.rm = TRUE)
  }

  projection <- data.frame(
    day_number = future_days,
    daily_gross = projected_gross,
    cumulative_gross = cumsum(projected_gross) + prior_cumulative,
    is_projected = TRUE
  )

  # Add dates if available
  if ("date" %in% names(daily_data)) {
    last_date <- max(daily_data$date, na.rm = TRUE)
    projection$date <- last_date + (projection$day_number - last_day)
    projection$day_of_week <- weekdays(projection$date)
  }

  projection
}

#' Find comparable films
#'
#' Finds historically similar films based on opening performance
#' and characteristics.
#'
#' @param features Data frame with target film features
#' @param historical_features Data frame with historical film features
#' @param n Number of comparable films to return
#' @param same_genre Require same genre
#' @param same_rating Require same MPAA rating
#' @return Data frame with comparable films
#' @export
find_comparable_films <- function(
    features,
    historical_features,
    n = 5,
    same_genre = FALSE,
    same_rating = FALSE
) {
  if (nrow(historical_features) == 0) {
    return(data.frame())
  }

  # Filter by genre if requested
  if (same_genre && "genres" %in% names(features)) {
    target_genres <- features$genres[[1]]
    # Look for any matching genre columns
    genre_cols <- grep("^genre_", names(historical_features), value = TRUE)
    if (length(genre_cols) > 0 && length(target_genres) > 0) {
      target_genre_col <- paste0("genre_", tolower(target_genres[1]))
      if (target_genre_col %in% genre_cols) {
        historical_features <- historical_features[historical_features[[target_genre_col]] == 1, ]
      }
    }
  }

  # Filter by rating if requested
  if (same_rating && "mpaa_rating" %in% names(features)) {
    rating <- features$mpaa_rating[1]
    rating_col <- paste0("is_rated_", tolower(gsub("-", "", rating)))
    if (rating_col %in% names(historical_features)) {
      historical_features <- historical_features[historical_features[[rating_col]] == 1, ]
    }
  }

  if (nrow(historical_features) == 0) {
    cli::cli_warn("No comparable films found with specified filters")
    return(data.frame())
  }

  # Calculate similarity based on opening metrics
  compare_cols <- c("log_opening_theaters", "day1_gross", "weekend1_gross")
  compare_cols <- intersect(compare_cols, names(features))
  compare_cols <- intersect(compare_cols, names(historical_features))

  if (length(compare_cols) == 0) {
    # Fall back to any numeric columns
    compare_cols <- intersect(names(features), names(historical_features))
    compare_cols <- compare_cols[vapply(features[compare_cols], is.numeric, logical(1))]
    compare_cols <- compare_cols[1:min(5, length(compare_cols))]
  }

  # Normalize features to z-scores so all dimensions contribute equally
  compare_matrix <- as.matrix(historical_features[, compare_cols, drop = FALSE])
  target_vals <- as.numeric(features[1, compare_cols])

  col_means <- colMeans(compare_matrix, na.rm = TRUE)
  col_sds <- apply(compare_matrix, 2, stats::sd, na.rm = TRUE)
  col_sds[col_sds == 0 | is.na(col_sds)] <- 1  # avoid division by zero

  norm_matrix <- sweep(sweep(compare_matrix, 2, col_means), 2, col_sds, "/")
  norm_target <- (target_vals - col_means) / col_sds

  # Calculate Euclidean distance on normalized features
  distances <- apply(norm_matrix, 1, function(row) {
    sqrt(sum((as.numeric(row) - norm_target)^2, na.rm = TRUE))
  })

  # Get top N most similar
  top_idx <- order(distances)[1:min(n, length(distances))]

  comparables <- historical_features[top_idx, ]
  comparables$similarity_distance <- distances[top_idx]
  comparables$similarity_rank <- seq_along(top_idx)

  comparables
}

#' Generate projection report
#'
#' Creates a comprehensive projection report for a film.
#'
#' @param daily_data Data frame with available daily box office
#' @param metadata List with film metadata
#' @param model Trained projection model (optional)
#' @param historical_features Data frame for finding comparables (optional)
#' @param adjust_for_inflation Logical: include inflation-adjusted figures
#'   (default: FALSE)
#' @param inflation_year Integer target year for constant dollars (default:
#'   most recent year in the CPI table)
#' @return List with projections and analysis
#' @export
generate_projection_report <- function(
    daily_data,
    metadata = NULL,
    model = NULL,
    historical_features = NULL,
    adjust_for_inflation = FALSE,
    inflation_year = max(cpi_annual$year)
) {
  report <- list(
    generated_at = Sys.time(),
    title = metadata$title %||% "Unknown Film",
    days_of_data = max(daily_data$day_number, na.rm = TRUE)
  )

  # Current performance — safely handle missing cumulative_gross
  total_gross <- if ("cumulative_gross" %in% names(daily_data) &&
                     any(!is.na(daily_data$cumulative_gross))) {
    max(daily_data$cumulative_gross, na.rm = TRUE)
  } else {
    sum(daily_data$daily_gross, na.rm = TRUE)
  }

  report$current <- list(
    total_gross = total_gross,
    daily_average = mean(daily_data$daily_gross, na.rm = TRUE),
    opening_weekend = sum(daily_data$daily_gross[opening_weekend_rows(daily_data)], na.rm = TRUE)
  )

  # Opening metrics
  report$opening_metrics <- calculate_opening_metrics(daily_data)

  # Theater metrics
  report$theater_metrics <- calculate_theater_metrics(daily_data)

  # Daily trajectory projection
  tryCatch({
    report$trajectory <- project_daily_trajectory(daily_data, days_ahead = 120)
    cum <- report$trajectory$cumulative_gross
    report$projected_final_gross <- if (any(!is.na(cum))) max(cum, na.rm = TRUE) else NA_real_
  }, error = function(e) {
    cli::cli_alert_warning("Could not project trajectory: {e$message}")
  })

  # Model-based projection (if model provided)
  if (!is.null(model)) {
    features <- extract_all_features(daily_data, metadata, model$n_days)
    tryCatch({
      report$model_projection <- project_total_gross(model, features)
    }, error = function(e) {
      cli::cli_alert_warning("Model projection failed: {e$message}")
    })
  }

  # Comparable films (if historical data provided)
  if (!is.null(historical_features)) {
    features <- extract_all_features(daily_data, metadata)
    tryCatch({
      report$comparables <- find_comparable_films(features, historical_features, n = 5)
    }, error = function(e) {
      cli::cli_alert_warning("Could not find comparables: {e$message}")
    })
  }

  # Calculated multiplier
  if (!is.null(report$projected_final_gross) && report$current$opening_weekend > 0) {
    report$projected_multiplier <- report$projected_final_gross / report$current$opening_weekend
  }

  # Inflation-adjusted figures
  if (adjust_for_inflation) {
    release_year <- if (!is.null(metadata$release_date)) {
      lubridate::year(as.Date(metadata$release_date))
    } else if (!is.null(metadata$year)) {
      metadata$year
    } else {
      NULL
    }

    if (!is.null(release_year)) {
      release_date_val <- if (!is.null(metadata$release_date)) as.Date(metadata$release_date) else NULL

      adj <- function(x) {
        adjust_inflation(x, release_year, inflation_year, release_date_val)
      }

      report$inflation_adjusted <- list(
        target_year = inflation_year,
        total_to_date = adj(report$current$total_gross),
        opening_weekend = adj(report$current$opening_weekend),
        projected_final = if (!is.null(report$projected_final_gross)) adj(report$projected_final_gross) else NULL
      )

      if (!is.null(report$model_projection)) {
        report$inflation_adjusted$model_predicted <- adj(report$model_projection$predicted_total)
        report$inflation_adjusted$model_lower <- adj(report$model_projection$lower_bound)
        report$inflation_adjusted$model_upper <- adj(report$model_projection$upper_bound)
      }
    }
  }

  class(report) <- c("boxoffice_projection", "list")
  report
}

#' Print method for projection report
#'
#' @param x Projection report object
#' @param ... Additional arguments (ignored)
#' @export
print.boxoffice_projection <- function(x, ...) {
  cli::cli_h1("{x$title} - Box Office Projection")
  cli::cli_alert_info("Based on {x$days_of_data} days of data")
  cli::cli_text("")

  cli::cli_h2("Current Performance")
  cli::cli_bullets(c(
    "*" = "Total to date: {format_gross(x$current$total_gross)}",
    "*" = "Opening weekend: {format_gross(x$current$opening_weekend)}",
    "*" = "Daily average: {format_gross(x$current$daily_average)}"
  ))

  if (!is.null(x$projected_final_gross)) {
    cli::cli_h2("Projection")
    cli::cli_bullets(c(
      "*" = "Projected final: {format_gross(x$projected_final_gross)}",
      "*" = "Projected multiplier: {round(x$projected_multiplier, 2)}x opening weekend"
    ))
  }

  if (!is.null(x$model_projection)) {
    cli::cli_h2("Model Projection")
    cli::cli_bullets(c(
      "*" = "Predicted total: {format_gross(x$model_projection$predicted_total)}",
      "*" = "Range: {format_gross(x$model_projection$lower_bound)} - {format_gross(x$model_projection$upper_bound)}"
    ))
  }

  if (!is.null(x$inflation_adjusted)) {
    adj <- x$inflation_adjusted
    cli::cli_h2("Inflation-Adjusted ({adj$target_year} dollars)")
    bullets <- c(
      "*" = "Total to date: {format_gross(adj$total_to_date)}",
      "*" = "Opening weekend: {format_gross(adj$opening_weekend)}"
    )
    if (!is.null(adj$projected_final)) {
      bullets <- c(bullets, "*" = "Projected final: {format_gross(adj$projected_final)}")
    }
    if (!is.null(adj$model_predicted)) {
      bullets <- c(bullets,
        "*" = "Model predicted: {format_gross(adj$model_predicted)}",
        "*" = "Model range: {format_gross(adj$model_lower)} - {format_gross(adj$model_upper)}"
      )
    }
    cli::cli_bullets(bullets)
  }

  invisible(x)
}
