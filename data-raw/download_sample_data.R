# Download Sample Box Office Data (Quick Test)
# Downloads just 2023-2024 data for quick testing
#
# Usage:
#   source("data-raw/download_sample_data.R")
#
# Takes ~30-60 minutes depending on connection

library(boxoffice)

cat("=== Box Office Sample Data Download ===\n")
cat("Downloading 2023-2024 data for testing\n\n")

# Step 1: Get film index for recent years
cat("Step 1: Building film index for 2023-2024...\n")
sample_films <- discover_films(
  start_year = 2023,
  end_year = 2024,
  sources = c("bom", "numbers"),
  use_cache = TRUE,
  progress = TRUE
)

cat(sprintf("Found %d films\n\n", nrow(sample_films)))

# Step 2: Download daily data
cat("Step 2: Downloading daily data...\n")

bom_ids <- sample_films$release_id[!is.na(sample_films$release_id)]
sample_daily <- scrape_bom_daily_batch(
  bom_ids,
  use_cache = TRUE,
  progress = TRUE
)

# Step 3: Build features
cat("\nStep 3: Building features...\n")
sample_features <- build_feature_matrix(
  sample_films,
  sample_daily,
  n_days = 10,
  progress = TRUE
)

# Save as sample data
boxoffice_sample_films <- sample_films
boxoffice_sample_daily <- sample_daily
boxoffice_sample_features <- sample_features

usethis::use_data(boxoffice_sample_films, overwrite = TRUE)
usethis::use_data(boxoffice_sample_daily, overwrite = TRUE)
usethis::use_data(boxoffice_sample_features, overwrite = TRUE)

cat("\n=== Sample Download Complete ===\n")
cat(sprintf("Films: %d\n", nrow(sample_films)))
cat(sprintf("Daily data: %d films\n", length(sample_daily)))
cat("\nSaved to data/boxoffice_sample_*.rda\n")
