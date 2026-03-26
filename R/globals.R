# Global variable declarations to suppress R CMD check NOTEs from
# unquoted column names in dplyr pipelines (mostly in tmdb.R).
#
# Prefer .data$column_name in new code — it provides typo detection
# and doesn't require maintaining this list. These declarations exist
# only for the existing tmdb.R pipelines.

utils::globalVariables(c(
  # tmdb.R pipeline columns
  "tmdb_id", "release_date", "title", "year", "revenue",
  "billing_order", "film_label", "actor_name", "tmdb_person_id",
  "n_films", "total_revenue", "avg_revenue", "avg_billing", "films",
  "character", "adjusted_revenue",
  # cpi_annual is a lazy-loaded package dataset used in adjust_inflation()
  "cpi_annual"
))

# Package-level constants
.boxoffice_env <- new.env(parent = emptyenv())

# Default user agent for scraping
.boxoffice_env$user_agent <- "boxoffice R package (https://github.com/user/boxoffice)"

# Default delays between requests (seconds)
.boxoffice_env$bom_delay <- 2
.boxoffice_env$numbers_delay <- 2

# Base URLs
.boxoffice_env$bom_base_url <- "https://www.boxofficemojo.com"
.boxoffice_env$numbers_base_url <- "https://www.the-numbers.com"

# TMDB API
.boxoffice_env$tmdb_base_url <- "https://api.themoviedb.org/3"
.boxoffice_env$tmdb_delay <- 0.05
