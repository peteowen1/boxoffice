# Tests for data processing functions

test_that("merge_daily_data handles NULL inputs", {
  expect_null(merge_daily_data(NULL, NULL))

  df <- data.frame(day_number = 1:3, daily_gross = c(100, 200, 300))
  result <- merge_daily_data(df, NULL)
  expect_equal(nrow(result), 3)
  expect_equal(result$source, rep("bom", 3))

  result2 <- merge_daily_data(NULL, df)
  expect_equal(result2$source, rep("numbers", 3))
})

test_that("merge_daily_data fills gaps from secondary source", {
  bom <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    day_number = 1:3,
    daily_gross = c(100, 200, 300)
  )
  numbers <- data.frame(
    date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-04")),
    day_number = c(1, 2, 4),
    daily_gross = c(110, 210, 400)
  )

  result <- merge_daily_data(bom, numbers)
  # Should have 4 rows: 3 from BOM + day 4 from Numbers
  expect_equal(nrow(result), 4)
  # Day 4 should come from numbers
  expect_equal(result$source[result$day_number == 4], "numbers")
})

test_that("merge_metadata coalesces with correct priority", {
  bom <- list(title = "Test Film", budget = NA, distributor = "Disney")
  numbers <- list(title = "Test Film", budget = 100e6, distributor = "Buena Vista")

  result <- merge_metadata(bom, numbers)
  # Title from BOM (preferred)
  expect_equal(result$title, "Test Film")
  # Budget from Numbers (preferred for budget)
  expect_equal(result$budget, 100e6)
  # Distributor from BOM (preferred)
  expect_equal(result$distributor, "Disney")
})

test_that("validate_daily_data catches negative gross", {
  df <- data.frame(day_number = 1:3, daily_gross = c(100, -50, 300))
  result <- validate_daily_data(df)
  expect_false(result$is_valid)
  expect_true(any(grepl("negative", result$issues)))
  # Cleaned data should have NA where negative was
  expect_true(is.na(result$cleaned_data$daily_gross[2]))
})

test_that("validate_daily_data catches missing days", {
  df <- data.frame(day_number = c(1, 2, 5), daily_gross = c(100, 200, 300))
  result <- validate_daily_data(df)
  expect_false(result$is_valid)
  expect_true(any(grepl("missing days", result$issues)))
})

test_that("fill_missing_days fills gaps with interpolation", {
  df <- data.frame(
    day_number = c(1, 2, 5),
    daily_gross = c(100, 80, 50)
  )
  result <- fill_missing_days(df, method = "interpolate")
  expect_equal(nrow(result), 5)
  # Interpolated values should be between neighbors
  expect_true(result$daily_gross[3] > 50 && result$daily_gross[3] < 80)
})

test_that("calculate_derived_metrics adds expected columns", {
  df <- data.frame(
    day_number = 1:7,
    daily_gross = c(50e6, 60e6, 40e6, 20e6, 15e6, 12e6, 10e6),
    day_of_week = c("Friday", "Saturday", "Sunday", "Monday",
                     "Tuesday", "Wednesday", "Thursday")
  )
  result <- calculate_derived_metrics(df)
  expect_true("week_number" %in% names(result))
  expect_true("is_weekend" %in% names(result))
  expect_true("daily_change" %in% names(result))
  expect_true("daily_pct_change" %in% names(result))
  # First day should have NA change
  expect_true(is.na(result$daily_change[1]))
  expect_true(is.na(result$daily_pct_change[1]))
})
