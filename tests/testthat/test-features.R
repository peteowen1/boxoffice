# Tests for feature extraction (pure logic, no network)

# Helper: create minimal daily data for testing
make_daily <- function(n_days = 14, opening_gross = 50e6, decay = 0.85,
                       opening_day = "Friday") {
  days_of_week <- c("Friday", "Saturday", "Sunday", "Monday", "Tuesday",
                     "Wednesday", "Thursday")
  start_idx <- match(opening_day, days_of_week)
  if (is.na(start_idx)) start_idx <- 1

  daily_gross <- numeric(n_days)
  daily_gross[1] <- opening_gross
  for (i in 2:n_days) {
    # Weekend bump on Sat/Sun
    dow_idx <- ((start_idx - 1 + i - 1) %% 7) + 1
    bump <- if (days_of_week[dow_idx] %in% c("Saturday", "Sunday")) 1.3 else 1.0
    daily_gross[i] <- daily_gross[i - 1] * decay * bump
  }

  dow_sequence <- days_of_week[((start_idx - 1 + seq_len(n_days) - 1) %% 7) + 1]

  data.frame(
    day_number = seq_len(n_days),
    daily_gross = daily_gross,
    cumulative_gross = cumsum(daily_gross),
    theaters = rep(3500L, n_days),
    per_theater_avg = daily_gross / 3500,
    day_of_week = dow_sequence,
    stringsAsFactors = FALSE
  )
}

test_that("opening_weekend_rows finds first Fri-Sat-Sun", {
  daily <- make_daily(opening_day = "Friday")
  rows <- opening_weekend_rows(daily)
  expect_equal(daily$day_of_week[rows], c("Friday", "Saturday", "Sunday"))
})

test_that("opening_weekend_rows handles Wednesday release", {
  daily <- make_daily(opening_day = "Wednesday")
  rows <- opening_weekend_rows(daily)
  # Should find Friday (day 3), Saturday (day 4), Sunday (day 5)
  expect_equal(daily$day_of_week[rows], c("Friday", "Saturday", "Sunday"))
  expect_equal(daily$day_number[rows], c(3, 4, 5))
})

test_that("opening_weekend_rows falls back to days 1-3 without day_of_week", {
  daily <- make_daily()
  daily$day_of_week <- NULL
  rows <- opening_weekend_rows(daily)
  expect_equal(daily$day_number[rows], c(1, 2, 3))
})

test_that("extract_opening_features returns expected columns", {
  daily <- make_daily()
  features <- extract_opening_features(daily, n_days = 10)
  expect_true("day1_gross" %in% names(features))
  expect_true("weekend1_gross" %in% names(features))
  expect_true("week1_gross" %in% names(features))
  expect_equal(nrow(features), 1)
})

test_that("extract_trajectory_features computes decay", {
  daily <- make_daily(n_days = 10)
  features <- extract_trajectory_features(daily, n_days = 10)
  # With consistent decay, slope should be negative
  expect_true("decay_slope" %in% names(features))
  expect_lt(features$decay_slope, 0)
})

test_that("extract_theater_features identifies wide release", {
  daily <- make_daily()  # 3500 theaters
  features <- extract_theater_features(daily)
  expect_equal(features$is_wide_release, 1L)
  expect_equal(features$is_limited_release, 0L)
})

test_that("extract_temporal_features classifies seasons", {
  summer <- extract_temporal_features(as.Date("2023-06-15"))
  expect_equal(summer$is_summer, 1L)

  holiday <- extract_temporal_features(as.Date("2023-12-20"))
  expect_equal(holiday$is_holiday, 1L)
})

test_that("calculate_multiplier handles all-NA cumulative_gross", {
  daily <- make_daily()
  daily$cumulative_gross <- NA_real_
  result <- calculate_multiplier(daily)
  # Should fall back to sum of daily_gross
  expect_false(is.na(result))
  expect_gt(result, 0)
})

test_that("extract_all_features produces complete feature set", {
  daily <- make_daily()
  metadata <- list(
    release_date = as.Date("2023-06-15"),
    genres = c("Action", "Comedy"),
    mpaa_rating = "PG-13",
    distributor = "Universal",
    budget = 150e6,
    runtime = 120
  )
  features <- extract_all_features(daily, metadata, n_days = 10)
  expect_equal(nrow(features), 1)
  expect_true("day1_gross" %in% names(features))
  expect_true("is_summer" %in% names(features))
  expect_true("genre_action" %in% names(features))
})
