# Tests for inflation adjustment logic

test_that("adjust_inflation returns same value for same year", {
  result <- adjust_inflation(1e8, 2020, 2020)
  expect_equal(unname(result), 1e8)
})

test_that("adjust_inflation increases for older years", {
  # $100M in 1990 should be worth more in 2025 dollars
  result <- adjust_inflation(1e8, 1990, 2025)
  expect_gt(result, 1e8)
})

test_that("adjust_inflation decreases for future target", {

  # $100M in 2025 should be worth less in 1990 dollars
  result <- adjust_inflation(1e8, 2025, 1990)
  expect_lt(result, 1e8)
})

test_that("adjust_inflation is vectorized", {
  revenues <- c(1e8, 2e8, 3e8)
  years <- c(1990, 2000, 2010)
  result <- adjust_inflation(revenues, years, 2025)
  expect_length(result, 3)
  # All should be higher than nominal (older dollars worth more)
  expect_true(all(result > revenues))
})

test_that("adjust_inflation handles NA revenue gracefully", {
  result <- adjust_inflation(NA, 2020, 2025)
  expect_true(is.na(result))
})

test_that("adjust_inflation with release_date interpolates", {
  # Same year, different dates should give slightly different results
  jan_result <- adjust_inflation(1e8, 1990, 2025, as.Date("1990-01-15"))
  dec_result <- adjust_inflation(1e8, 1990, 2025, as.Date("1990-12-15"))
  no_date_result <- adjust_inflation(1e8, 1990, 2025)
  # Jan release should adjust slightly more (earlier = more inflation)
  expect_gt(jan_result, dec_result)
  # Both should be close to the no-date result
  expect_equal(jan_result, no_date_result, tolerance = 0.05 * no_date_result)
})

test_that("adjust_inflation errors on out-of-range target year", {
  expect_error(adjust_inflation(1e8, 2020, 1800), "No CPI data")
})

test_that("validate_cpi_data passes on real CPI table", {
  # Reset validation flag so it actually runs
  .boxoffice_env$cpi_validated <- FALSE
  expect_no_warning(validate_cpi_data())
})

test_that("CPI data is monotonically increasing post-1940 (sanity check)", {
  # Direct check on the actual data — catches typos in data-raw/cpi.R
  post_1940 <- cpi_annual[cpi_annual$year >= 1940, ]
  pct_change <- diff(post_1940$cpi) / head(post_1940$cpi, -1)

  # No drops > 5%
  big_drops <- which(pct_change < -0.05)
  expect_length(big_drops, 0)

  # No jumps > 20%
  big_jumps <- which(pct_change > 0.20)
  expect_length(big_jumps, 0)
})
