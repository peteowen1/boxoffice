#' Historical Box Office Film Index
#'
#' A dataset containing basic information about films released from 1980 to present,
#' scraped from Box Office Mojo and The Numbers.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{release_id}{Box Office Mojo release identifier}
#'   \item{numbers_slug}{The Numbers movie slug}
#'   \item{title}{Film title}
#'   \item{year}{Release year}
#'   \item{distributor}{Distribution company}
#'   \item{total_gross}{Total domestic gross}
#'   \item{opening_gross}{Opening weekend gross}
#'   \item{theaters_max}{Maximum theater count}
#' }
#' @source \url{https://www.boxofficemojo.com} and \url{https://www.the-numbers.com}
"boxoffice_films"

#' Historical Daily Box Office Data
#'
#' A named list of data frames containing daily box office breakdown for each film.
#' Each data frame contains day-by-day performance data.
#'
#' @format A named list where each element is a data frame with columns:
#' \describe{
#'   \item{date}{Date}
#'   \item{day_number}{Day number in release (1 = opening day)}
#'   \item{day_of_week}{Day of week name}
#'   \item{daily_gross}{Daily gross revenue}
#'   \item{theaters}{Number of theaters}
#'   \item{per_theater_avg}{Per-theater average}
#'   \item{cumulative_gross}{Running total gross}
#' }
#' @source \url{https://www.boxofficemojo.com}
"boxoffice_daily"

#' Pre-computed Feature Matrix
#'
#' A dataset containing extracted features for all historical films,
#' ready for model training.
#'
#' @format A data frame with features including:
#' \describe{
#'   \item{release_id}{Film identifier}
#'   \item{title}{Film title}
#'   \item{year}{Release year}
#'   \item{day1_gross}{Opening day gross}
#'   \item{weekend1_gross}{Opening weekend gross}
#'   \item{week1_gross}{Opening week gross}
#'   \item{opening_theaters}{Opening theater count}
#'   \item{decay_slope}{Daily decay rate}
#'   \item{weekend2_drop}{Second weekend drop percentage}
#'   \item{total_gross}{Final domestic gross (target variable)}
#' }
"boxoffice_features"

# TODO: Uncomment when boxoffice_model.rda is created in data/
# #' Pre-trained Projection Model
# #'
# #' An XGBoost model trained to predict total domestic gross
# #' based on opening performance and film characteristics.
# #'
# #' @format A boxoffice_model object containing:
# #' \describe{
# #'   \item{model}{Trained XGBoost model}
# #'   \item{feature_cols}{Feature column names used}
# #'   \item{target}{Target variable ("total_gross")}
# #'   \item{n_days}{Number of opening days used (10)}
# #'   \item{cv_rmse}{Cross-validation RMSE}
# #'   \item{importance}{Feature importance rankings}
# #' }
# "boxoffice_model"

# TODO: Uncomment when sample datasets are created in data/
# #' Sample Film Index (2023-2024)
# #'
# #' A smaller sample dataset for testing and examples.
# #'
# #' @format Same structure as [boxoffice_films]
# #' @seealso [boxoffice_films]
# "boxoffice_sample_films"
#
# #' Sample Daily Data (2023-2024)
# #'
# #' A smaller sample dataset for testing and examples.
# #'
# #' @format Same structure as [boxoffice_daily]
# #' @seealso [boxoffice_daily]
# "boxoffice_sample_daily"
#
# #' Sample Feature Matrix (2023-2024)
# #'
# #' A smaller sample dataset for testing and examples.
# #'
# #' @format Same structure as [boxoffice_features]
# #' @seealso [boxoffice_features]
# "boxoffice_sample_features"
