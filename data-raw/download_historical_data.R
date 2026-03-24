# Download Historical Box Office Data
# Run this script to build the complete historical dataset
#
# Usage:
#   source("data-raw/download_historical_data.R")
#
# This will take a while due to rate limiting (~1s between requests)
# Progress is saved incrementally, so you can stop and resume.

devtools::load_all()

# Configuration
START_YEAR <- 1980
END_YEAR <- as.integer(format(Sys.Date(), "%Y"))
SOURCES <- c("bom", "numbers")

cat("=== Box Office Historical Data Download ===\n")
cat(sprintf("Years: %d to %d\n", START_YEAR, END_YEAR))
cat(sprintf("Sources: %s\n", paste(SOURCES, collapse = ", ")))
cat("\n")

# Step 1: Build film index (relatively fast)
cat("Step 1: Building film index...\n")
film_index <- discover_films(
  start_year = START_YEAR,
  end_year = END_YEAR,
  sources = SOURCES,
  use_cache = TRUE,
  save_to_data = TRUE,
  progress = TRUE
)

cat(sprintf("Found %d films\n\n", nrow(film_index)))

# Save film index
boxoffice_films <- film_index
usethis::use_data(boxoffice_films, overwrite = TRUE)

# Step 2: Download daily data for all films (slow - hours)
cat("Step 2: Downloading daily box office data...\n")
cat("This will take several hours. Progress is cached.\n\n")

# Get BOM daily data
if ("release_id" %in% names(film_index)) {
  bom_ids <- film_index$release_id[!is.na(film_index$release_id)]
  cat(sprintf("Fetching daily data for %d BOM films...\n", length(bom_ids)))

  bom_daily_data <- scrape_bom_daily_batch(
    bom_ids,
    use_cache = TRUE,
    progress = TRUE
  )
}

# Get Numbers daily data for films not in BOM
if ("numbers_slug" %in% names(film_index)) {
  # Find films only in Numbers
  numbers_only <- film_index$numbers_slug[
    is.na(film_index$release_id) & !is.na(film_index$numbers_slug)
  ]

  if (length(numbers_only) > 0) {
    cat(sprintf("Fetching daily data for %d Numbers-only films...\n", length(numbers_only)))

    numbers_daily_data <- scrape_numbers_daily_batch(
      numbers_only,
      use_cache = TRUE,
      progress = TRUE
    )

    # Combine
    bom_daily_data <- c(bom_daily_data, numbers_daily_data)
  }
}

boxoffice_daily <- bom_daily_data
usethis::use_data(boxoffice_daily, overwrite = TRUE)

# Step 3: Build feature matrix
cat("\nStep 3: Building feature matrix...\n")

boxoffice_features <- build_feature_matrix(
  film_index,
  bom_daily_data,
  n_days = 10,
  progress = TRUE
)

# Add total gross as target variable
if ("total_gross" %in% names(film_index)) {
  boxoffice_features <- merge(
    boxoffice_features,
    film_index[, c("release_id", "total_gross")],
    by = "release_id",
    all.x = TRUE
  )
} else if ("domestic_gross" %in% names(film_index)) {
  boxoffice_features <- merge(
    boxoffice_features,
    film_index[, c("release_id", "domestic_gross")],
    by = "release_id",
    all.x = TRUE
  )
  names(boxoffice_features)[names(boxoffice_features) == "domestic_gross"] <- "total_gross"
}

usethis::use_data(boxoffice_features, overwrite = TRUE)

# Step 4: Train default model
cat("\nStep 4: Training projection model...\n")

# Filter to films with valid data
train_data <- boxoffice_features[
 !is.na(boxoffice_features$total_gross) &
    !is.na(boxoffice_features$weekend1_gross) &
    boxoffice_features$total_gross > 0,
]

if (nrow(train_data) >= 100) {
  boxoffice_model <- train_projection_model(
    train_data,
    target = "total_gross",
    n_days = 10,
    cv_folds = 5,
    verbose = TRUE
  )

  usethis::use_data(boxoffice_model, overwrite = TRUE)
  cat(sprintf("Model trained on %d films\n", nrow(train_data)))
} else {
  cat("Not enough data to train model (need 100+ films with complete data)\n")
}

# Summary
cat("\n=== Download Complete ===\n")
cat(sprintf("Films indexed: %d\n", nrow(film_index)))
cat(sprintf("Daily data downloaded: %d films\n", length(bom_daily_data)))
cat(sprintf("Features built: %d films\n", nrow(boxoffice_features)))
cat("\nData saved to:\n")
cat("  data/boxoffice_films.rda\n")
cat("  data/boxoffice_daily.rda\n")
cat("  data/boxoffice_features.rda\n")
cat("  data/boxoffice_model.rda\n")
