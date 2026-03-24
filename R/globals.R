# Global variable declarations to avoid R CMD check NOTEs
# These are column names used in dplyr pipelines

utils::globalVariables(c(

  # Common column names
  ".",
  "date",
  "day_number",

  "day_of_week",
  "daily_gross",
  "cumulative_gross",
  "theaters",
  "per_theater_avg",
  "rank",
  "pct_change_yesterday",
  "pct_change_last_week",
  "estimated",


  # Film metadata

  "title",
  "year",
  "release_id",
  "movie_slug",
  "release_date",
  "distributor",
  "genre",
  "genres",
  "mpaa_rating",
  "runtime",
  "budget",
  "opening_weekend",
  "opening_theaters",
  "widest_release",
  "domestic_gross",
  "international_gross",
  "worldwide_gross",

  # Derived metrics
  "week_number",
  "is_weekend",
  "weekend_gross",
  "weekday_gross",
  "opening_day_gross",
  "opening_weekend_gross",
  "opening_week_gross",
  "total_gross",
  "multiplier",
  "decay_rate",

  # Feature columns
  "source",
  "bom_id",
  "numbers_slug",
  "matched",
  "confidence",

  # TMDB / actor columns
  "tmdb_id",
  "revenue",
  "film_label",
  "actor_name",
  "tmdb_person_id",
  "character",
  "billing_order",
  "n_films",
  "total_revenue",
  "avg_revenue",
  "avg_billing",
  "films"
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
