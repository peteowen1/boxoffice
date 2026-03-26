# Tests for utility/parser functions (no network access needed)

test_that("parse_currency handles standard formats", {
  expect_equal(parse_currency("$1,234,567"), 1234567)
  expect_equal(parse_currency("$100"), 100)
  expect_equal(parse_currency("1234"), 1234)
  expect_equal(parse_currency("$0"), 0)
})

test_that("parse_currency handles edge cases", {
  expect_true(is.na(parse_currency("")))
  expect_true(is.na(parse_currency("-")))
  expect_true(is.na(parse_currency(NA)))
  expect_equal(parse_currency(42), 42)  # numeric passthrough
})

test_that("parse_percentage converts to decimal", {
  expect_equal(parse_percentage("50%"), 0.50)
  expect_equal(parse_percentage("-15.2%"), -0.152)
  expect_true(is.na(parse_percentage("")))
  expect_true(is.na(parse_percentage("-")))
  expect_equal(parse_percentage(0.5), 0.5)  # numeric passthrough
})

test_that("parse_runtime converts hr/min to minutes", {
  expect_equal(parse_runtime("2 hr 15 min"), 135L)
  expect_equal(parse_runtime("1 hr"), 60L)
  expect_equal(parse_runtime("90 min"), 90L)
  expect_equal(parse_runtime(120), 120L)  # numeric passthrough
})

test_that("format_gross formats correctly", {
  expect_equal(format_gross(1.5e9), "$1.5B")
  expect_equal(format_gross(250e6), "$250M")
  expect_equal(format_gross(50e3), "$50K")
  expect_equal(format_gross(500), "$500")
  expect_true(is.na(format_gross(NA)))
})

test_that("format_gross handles negative values", {
  expect_equal(format_gross(-5e6), "-$5M")
  expect_equal(format_gross(-1.2e9), "-$1.2B")
})

test_that("extract_year finds 4-digit years", {
  expect_equal(extract_year("Barbie-(2023)"), 2023L)
  expect_equal(extract_year("Released in 1997"), 1997L)
  expect_true(is.na(extract_year("no year here")))
})

test_that("sanitize_title_for_url produces valid slugs", {
  expect_equal(sanitize_title_for_url("Barbie", 2023), "Barbie-(2023)")
  expect_equal(sanitize_title_for_url("Spider-Man: No Way Home"), "Spider-Man-No-Way-Home")
  expect_equal(sanitize_title_for_url("Puss & Boots"), "Puss-and-Boots")
})

test_that("standardize_genre maps known variations", {
  expect_equal(standardize_genre("Science Fiction"), "Sci-Fi")
  expect_equal(standardize_genre("Animated"), "Animation")
  expect_equal(standardize_genre("Drama"), "Drama")  # no change
})

test_that("standardize_distributor groups studio families", {
  expect_equal(standardize_distributor("Walt Disney Studios"), "Disney")
  expect_equal(standardize_distributor("Marvel Studios"), "Disney")
  expect_equal(standardize_distributor("Warner Bros. Pictures"), "Warner Bros.")
  expect_equal(standardize_distributor("Some Indie Co"), "Some Indie Co")
})

test_that("get_distributor_tier classifies correctly", {
  expect_equal(get_distributor_tier("Disney"), "major")
  expect_equal(get_distributor_tier("Lionsgate"), "mini_major")
  expect_equal(get_distributor_tier("A24"), "indie")
})

test_that("coalesce_value returns first non-NULL, non-NA", {
  expect_equal(coalesce_value("hello", "world"), "hello")
  expect_equal(coalesce_value(NULL, "fallback"), "fallback")
  expect_equal(coalesce_value(NA, "fallback"), "fallback")
  expect_equal(coalesce_value(0, "fallback"), 0)  # 0 is not NA
})

test_that("%||% returns first non-NULL", {
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_true(is.na(NA %||% "default"))  # NA is not NULL
})
