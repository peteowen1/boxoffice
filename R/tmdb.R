# ---- API key ----

#' Get TMDB API key
#'
#' @return Character API key
#' @keywords internal
tmdb_api_key <- function() {
  key <- Sys.getenv("TMDB_API_KEY", unset = "")
  if (key == "") key <- getOption("boxoffice.tmdb_api_key", default = "")
  if (key == "") {
    cli::cli_abort(c(
      "TMDB API key not found.",
      "i" = "Get a free key at {.url https://www.themoviedb.org/settings/api}",
      "i" = "Set {.envvar TMDB_API_KEY} in your {.file ~/.Renviron}",
      "i" = "Or call {.code options(boxoffice.tmdb_api_key = 'your-key')}"
    ))
  }
  key
}

# ---- Low-level API helper ----

#' Make a GET request to the TMDB API
#'
#' @param endpoint API endpoint path (e.g., "discover/movie")
#' @param ... Additional query parameters
#' @param api_key TMDB API key
#' @return Parsed JSON as list
#' @keywords internal
tmdb_get <- function(endpoint, ..., api_key = tmdb_api_key()) {
  url <- paste0(.boxoffice_env$tmdb_base_url, "/", endpoint)

  resp <- httr2::request(url) |>
    httr2::req_url_query(...) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_user_agent(.boxoffice_env$user_agent) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3, is_transient = \(resp) httr2::resp_status(resp) == 429) |>
    httr2::req_perform()

  Sys.sleep(.boxoffice_env$tmdb_delay)

  httr2::resp_body_json(resp)
}

# ---- Film discovery ----

#' Discover films above a revenue threshold via TMDB
#'
#' Uses the TMDB `/discover/movie` endpoint sorted by revenue descending to get
#' a sorted list of film IDs, then fetches `/movie/{id}` for each to get the
#' actual revenue (which isn't included in discover results). Uses
#' `append_to_response=credits` to also fetch cast in the same call.
#'
#' @param min_revenue Numeric minimum worldwide revenue in USD (default 1e8 = $100M)
#' @param start_year Integer start year filter (inclusive), or NULL for all time
#' @param end_year Integer end year filter (inclusive), or NULL for all time
#' @param max_pages Maximum discover pages to fetch (default 500, TMDB cap)
#' @param use_cache Logical whether to use cached results
#' @return Tibble with columns: tmdb_id, title, release_date, year, revenue
#' @export
tmdb_discover_films <- function(min_revenue = 1e8,
                                start_year = NULL,
                                end_year = NULL,
                                max_pages = 500,
                                use_cache = TRUE) {
  # Check cache for final results
  cache_key <- make_cache_key(
    "discover", min_revenue,
    ifelse(is.null(start_year), "all", start_year),
    ifelse(is.null(end_year), "all", end_year)
  )

  if (use_cache && is_cached("tmdb", "discover", cache_key, expires_days = 30)) {
    cli::cli_alert_info("Using cached TMDB discover results")
    return(read_cache("tmdb", "discover", cache_key))
  }

  # Build date filters
  date_params <- list()
  if (!is.null(start_year)) {
    date_params[["primary_release_date.gte"]] <- paste0(start_year, "-01-01")
  }
  if (!is.null(end_year)) {
    date_params[["primary_release_date.lte"]] <- paste0(end_year, "-12-31")
  }

  # Paginate discover and fetch movie details page-by-page.
  # Discover returns films sorted by revenue desc but without the revenue field,
  # so we fetch /movie/{id}?append_to_response=credits for each to get both
  # revenue and cast in one call. We stop when an entire discover page yields
  # no qualifying films (revenue is monotonically decreasing across pages).
  all_films <- list()
  page <- 1
  n_fetched <- 0

  cli::cli_alert_info("Discovering and fetching films sorted by revenue...")

  repeat {
    # Get next page of discover results
    params <- c(
      list(sort_by = "revenue.desc", page = page),
      date_params
    )
    result <- do.call(tmdb_get, c(list(endpoint = "discover/movie"), params))

    if (length(result$results) == 0) break

    page_ids <- vapply(result$results, function(m) m$id, integer(1))
    page_qualifying <- 0

    # Fetch details for each film on this page
    for (id in page_ids) {
      n_fetched <- n_fetched + 1
      if (n_fetched %% 100 == 0) {
        cli::cli_alert_info(
          "Progress: {n_fetched} films checked, {length(all_films)} qualifying so far"
        )
      }

      movie_cached <- use_cache && is_cached("tmdb", "movie", as.character(id))
      credits_cached <- use_cache && is_cached("tmdb", "credits", as.character(id))

      if (movie_cached) {
        movie <- read_cache("tmdb", "movie", as.character(id))
      } else {
        raw <- tryCatch(
          tmdb_get(paste0("movie/", id), append_to_response = "credits"),
          error = function(e) {
            cli::cli_alert_warning("Failed to fetch TMDB ID {id}: {e$message}")
            return(NULL)
          }
        )
        if (is.null(raw)) next

        movie <- list(
          tmdb_id = raw$id,
          title = raw$title %||% NA_character_,
          release_date = raw$release_date %||% NA_character_,
          revenue = raw$revenue %||% 0
        )
        if (use_cache) write_cache("tmdb", "movie", as.character(id), movie)

        # Cache credits from the same response
        if (!credits_cached && !is.null(raw$credits) && length(raw$credits$cast) > 0) {
          credits_df <- purrr::imap(raw$credits$cast, function(person, idx) {
            dplyr::tibble(
              actor_name = person$name %||% NA_character_,
              tmdb_person_id = person$id %||% NA_integer_,
              character = person$character %||% NA_character_,
              billing_order = as.integer(idx)
            )
          }) |>
            dplyr::bind_rows()
          write_cache("tmdb", "credits", as.character(id), credits_df)
        }
      }

      if (movie$revenue >= min_revenue) {
        all_films <- c(all_films, list(movie))
        page_qualifying <- page_qualifying + 1
      }
    }

    # If no film on this entire page qualified, we've passed the threshold boundary
    if (page_qualifying == 0) {
      cli::cli_alert_info("Stopping: no qualifying films on page {page}")
      break
    }

    if (page >= min(max_pages, result$total_pages)) break
    page <- page + 1
  }

  if (length(all_films) == 0) {
    films <- dplyr::tibble(
      tmdb_id = integer(), title = character(),
      release_date = as.Date(character()), year = integer(),
      revenue = numeric()
    )
  } else {
    films <- dplyr::bind_rows(purrr::map(all_films, dplyr::as_tibble)) |>
      dplyr::mutate(
        release_date = as.Date(release_date),
        year = as.integer(format(release_date, "%Y"))
      ) |>
      dplyr::distinct(tmdb_id, .keep_all = TRUE)
  }

  cli::cli_alert_success("Found {nrow(films)} films with revenue >= ${format(min_revenue, big.mark = ',')}")

  if (use_cache && nrow(films) > 0) write_cache("tmdb", "discover", cache_key, films)

  films
}

# ---- Credits ----

#' Get cast credits for a single film
#'
#' @param tmdb_id Integer TMDB movie ID
#' @param use_cache Logical
#' @return Tibble with columns: actor_name, tmdb_person_id, character, billing_order
#' @keywords internal
tmdb_get_credits <- function(tmdb_id, use_cache = TRUE) {
  cache_key <- as.character(tmdb_id)

  if (use_cache && is_cached("tmdb", "credits", cache_key)) {
    return(read_cache("tmdb", "credits", cache_key))
  }

  result <- tryCatch(
    tmdb_get(paste0("movie/", tmdb_id, "/credits")),
    error = function(e) {
      cli::cli_alert_warning("Failed to get credits for TMDB ID {tmdb_id}: {e$message}")
      return(NULL)
    }
  )

  if (is.null(result) || is.null(result$cast) || length(result$cast) == 0) {
    empty <- dplyr::tibble(
      actor_name = character(),
      tmdb_person_id = integer(),
      character = character(),
      billing_order = integer()
    )
    if (use_cache) write_cache("tmdb", "credits", cache_key, empty)
    return(empty)
  }

  credits <- purrr::imap(result$cast, function(person, idx) {
    dplyr::tibble(
      actor_name = person$name %||% NA_character_,
      tmdb_person_id = person$id %||% NA_integer_,
      character = person$character %||% NA_character_,
      billing_order = as.integer(idx)
    )
  }) |>
    dplyr::bind_rows()

  if (use_cache) write_cache("tmdb", "credits", cache_key, credits)

  credits
}

#' Get credits for multiple films
#'
#' @param tmdb_ids Integer vector of TMDB movie IDs
#' @param use_cache Logical
#' @return Named list of tibbles (keyed by tmdb_id)
#' @keywords internal
tmdb_get_credits_batch <- function(tmdb_ids, use_cache = TRUE) {
  results <- list()

  cli::cli_progress_bar(
    "Fetching credits",
    total = length(tmdb_ids),
    type = "iterator"
  )

  for (id in tmdb_ids) {
    results[[as.character(id)]] <- tmdb_get_credits(id, use_cache = use_cache)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  results
}

# ---- Main analysis function ----

#' Find top actors by number of box office hits
#'
#' Discovers all films above a worldwide revenue threshold using TMDB, fetches
#' cast credits for each, and ranks actors by number of qualifying films.
#'
#' @param min_revenue Numeric minimum worldwide revenue in USD (default 1e8 = $100M).
#'   This is **worldwide theatrical revenue** as reported by TMDB. When
#'   `adjust_inflation = TRUE`, the threshold is applied in constant
#'   (`inflation_year`) dollars — the discover query automatically uses a
#'   deflated nominal floor so older hits are not missed.
#' @param start_year Integer start year (inclusive), or NULL for all time
#' @param end_year Integer end year (inclusive), or NULL for all time
#' @param max_billing Integer maximum billing position to include, or NULL for
#'   all credited cast. E.g., `max_billing = 5` counts only top-5-billed actors.
#' @param min_films Integer minimum number of qualifying films to include an
#'   actor in results (default 2)
#' @param adjust_inflation Logical whether to adjust revenue for inflation
#'   using CPI-U (default FALSE). When TRUE, revenue columns reflect constant
#'   dollars for `inflation_year`.
#' @param inflation_year Integer target year for inflation adjustment (default:
#'   most recent year in CPI table, currently 2025).
#' @param use_cache Logical whether to use cached API results (default TRUE)
#' @return Tibble with columns: actor_name, tmdb_person_id, n_films,
#'   total_revenue, avg_revenue, avg_billing, films (list-column)
#'
#' @details
#' Requires a TMDB API key. Get a free key at
#' \url{https://www.themoviedb.org/settings/api} and set
#' `TMDB_API_KEY` in your `.Renviron` or call
#' `options(boxoffice.tmdb_api_key = "your-key")`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Top actors in $500M+ films (quick test)
#' top_actors_by_boxoffice(min_revenue = 5e8, max_billing = 5)
#'
#' # All actors in $100M+ films since 2000
#' top_actors_by_boxoffice(start_year = 2000)
#'
#' # Only leads (top 3 billed) in $200M+ films
#' top_actors_by_boxoffice(min_revenue = 2e8, max_billing = 3)
#' }
top_actors_by_boxoffice <- function(min_revenue = 1e8,
                                    start_year = NULL,
                                    end_year = NULL,
                                    max_billing = NULL,
                                    min_films = 2,
                                    adjust_inflation = FALSE,
                                    inflation_year = max(cpi_annual$year),
                                    use_cache = TRUE) {
  # Step 1: Discover qualifying films (also caches credits via append_to_response)
  #
  # When adjusting for inflation, deflate min_revenue to a nominal floor so we

  # don't miss older films that were massive hits in real terms. E.g. $100M in
  # 2025 dollars ≈ $6M in 1970 nominal — without this, a 1975 film earning $90M
  # nominal ($500M+ adjusted) would be excluded.
  if (adjust_inflation) {
    earliest_year <- start_year %||% min(cpi_annual$year)
    cpi_lookup <- stats::setNames(cpi_annual$cpi, cpi_annual$year)
    target_cpi <- cpi_lookup[as.character(inflation_year)]
    earliest_cpi <- cpi_lookup[as.character(earliest_year)]
    if (!is.na(earliest_cpi) && !is.na(target_cpi) && target_cpi > 0) {
      discover_min <- min_revenue * (earliest_cpi / target_cpi)
    } else {
      discover_min <- min_revenue
    }
  } else {
    discover_min <- min_revenue
  }

  films <- tmdb_discover_films(
    min_revenue = discover_min,
    start_year = start_year,
    end_year = end_year,
    use_cache = use_cache
  )

  if (nrow(films) == 0) {
    cli::cli_alert_warning("No films found above ${format(min_revenue, big.mark = ',')}")
    return(dplyr::tibble(
      actor_name = character(), tmdb_person_id = integer(),
      n_films = integer(), total_revenue = numeric(),
      avg_revenue = numeric(), avg_billing = numeric(),
      films = list()
    ))
  }

  # Step 2: Build flat (film, actor) table — cached as a single file to avoid
  # reading thousands of individual credit files on repeat calls.
  cast_cache_key <- make_cache_key(
    "cast_long", discover_min,
    ifelse(is.null(start_year), "all", start_year),
    ifelse(is.null(end_year), "all", end_year)
  )

  cast_long <- NULL
  if (use_cache && is_cached("tmdb", "cast_long", cast_cache_key, expires_days = 30)) {
    cli::cli_alert_info("Using cached cast data")
    cast_long <- read_cache("tmdb", "cast_long", cast_cache_key)
  }

  if (is.null(cast_long)) {
    credits_list <- tmdb_get_credits_batch(films$tmdb_id, use_cache = use_cache)

    cast_long <- purrr::imap(credits_list, function(credits_df, id_str) {
      film_row <- dplyr::filter(films, tmdb_id == as.integer(id_str))
      if (nrow(film_row) == 0 || nrow(credits_df) == 0) return(NULL)

      credits_df |>
        dplyr::mutate(
          tmdb_id = film_row$tmdb_id[1],
          title = film_row$title[1],
          year = film_row$year[1],
          release_date = film_row$release_date[1],
          revenue = film_row$revenue[1]
        )
    }) |>
      purrr::compact() |>
      dplyr::bind_rows()

    if (use_cache && nrow(cast_long) > 0) {
      write_cache("tmdb", "cast_long", cast_cache_key, cast_long)
    }
  }

  if (nrow(cast_long) == 0) {
    cli::cli_alert_warning("No cast data found for qualifying films")
    return(dplyr::tibble(
      actor_name = character(), tmdb_person_id = integer(),
      n_films = integer(), total_revenue = numeric(),
      avg_revenue = numeric(), avg_billing = numeric(),
      films = list()
    ))
  }

  # Step 3: Filter by billing position if requested
  if (!is.null(max_billing)) {
    cast_long <- dplyr::filter(cast_long, billing_order <= max_billing)
    cli::cli_alert_info("Filtered to top {max_billing} billed cast per film")
  }

  # Step 4: Inflation adjustment (before aggregation so per-film revenue is correct)
  if (adjust_inflation) {
    # release_date may be absent in older cached cast_long tables
    rd <- if ("release_date" %in% names(cast_long)) cast_long$release_date else NULL
    cast_long$revenue <- adjust_inflation(
      cast_long$revenue, cast_long$year, inflation_year,
      release_date = rd
    )
    cli::cli_alert_info("Revenue adjusted to {inflation_year} dollars")

    # Now filter at the user's threshold in constant dollars (we discovered
    # with a lower nominal floor to catch older hits)
    n_before <- length(unique(cast_long$tmdb_id))
    cast_long <- dplyr::filter(cast_long, revenue >= min_revenue)
    n_after <- length(unique(cast_long$tmdb_id))
    if (n_before != n_after) {
      cli::cli_alert_info(
        "Filtered to {n_after} films with adjusted revenue >= {format_gross(min_revenue)}"
      )
    }
  }

  # Step 5: Aggregate by actor
  result <- cast_long |>
    dplyr::mutate(film_label = paste0(title, " (", year, ")")) |>
    dplyr::group_by(actor_name, tmdb_person_id) |>
    dplyr::summarise(
      n_films = dplyr::n(),
      total_revenue = sum(revenue, na.rm = TRUE),
      avg_revenue = mean(revenue, na.rm = TRUE),
      avg_billing = mean(billing_order, na.rm = TRUE),
      films = list(sort(unique(film_label))),
      .groups = "drop"
    ) |>
    dplyr::filter(n_films >= min_films) |>
    dplyr::arrange(dplyr::desc(n_films), dplyr::desc(total_revenue))

  cli::cli_alert_success(
    "Found {nrow(result)} actors with {min_films}+ films above ${format(min_revenue, big.mark = ',')}"
  )

  result
}

# ---- Actor filmography ----

#' Search for a person on TMDB by name
#'
#' @param name Character name to search for
#' @return List with `id`, `name`, and `popularity` for the best match
#' @keywords internal
tmdb_search_person <- function(name) {
  result <- tmdb_get("search/person", query = name)

  if (length(result$results) == 0) {
    cli::cli_abort("No person found matching {.val {name}} on TMDB.")
  }

  # Results come sorted by popularity — take the top match
  best <- result$results[[1]]

  if (length(result$results) > 1) {
    others <- vapply(
      result$results[2:min(5, length(result$results))],
      function(p) paste0(p$name, " (ID: ", p$id, ")"),
      character(1)
    )
    cli::cli_alert_info(
      "Matched {.val {best$name}} (ID: {best$id}). Other matches: {paste(others, collapse = ', ')}"
    )
  }

  list(
    id = best$id,
    name = best$name,
    popularity = best$popularity %||% NA_real_
  )
}

#' Get an actor's complete filmography from TMDB
#'
#' Looks up an actor by name (or TMDB person ID) and returns all their film
#' credits with revenue, billing order, and character played.
#'
#' @param name Character actor name to search for (ignored if `tmdb_person_id`
#'   is provided)
#' @param tmdb_person_id Integer TMDB person ID. If provided, skips the name
#'   search.
#' @param min_revenue Numeric minimum worldwide revenue to include (default 0,
#'   i.e. all films). Set higher to focus on hits. Applied after inflation
#'   adjustment if `adjust_inflation = TRUE`.
#' @param adjust_inflation Logical whether to adjust revenue for inflation
#'   using CPI-U (default FALSE). When TRUE, an `adjusted_revenue` column is
#'   added and used for filtering/sorting.
#' @param inflation_year Integer target year for inflation adjustment (default:
#'   most recent year in CPI table, currently 2025).
#' @param use_cache Logical whether to use cached results (default TRUE)
#' @return Tibble with columns: title, year, release_date, revenue, character,
#'   billing_order, tmdb_id. If `adjust_inflation = TRUE`, also includes
#'   `adjusted_revenue`.
#'
#' @details
#' Requires a TMDB API key. See [top_actors_by_boxoffice()] for setup
#' instructions.
#'
#' @export
#' @examples
#' \dontrun{
#' # By name
#' get_actor_filmography("Julia Roberts")
#'
#' # By TMDB person ID (faster, no search needed)
#' get_actor_filmography(tmdb_person_id = 1204)
#'
#' # Only $100M+ films
#' get_actor_filmography("Julia Roberts", min_revenue = 1e8)
#'
#' # Inflation-adjusted to 2025 dollars
#' get_actor_filmography("Julia Roberts", adjust_inflation = TRUE)
#' }
get_actor_filmography <- function(name = NULL,
                                  tmdb_person_id = NULL,
                                  min_revenue = 0,
                                  adjust_inflation = FALSE,
                                  inflation_year = max(cpi_annual$year),
                                  use_cache = TRUE) {
  if (is.null(name) && is.null(tmdb_person_id)) {
    cli::cli_abort("Provide either {.arg name} or {.arg tmdb_person_id}.")
  }

  # Resolve person ID from name if needed
  if (is.null(tmdb_person_id)) {
    person <- tmdb_search_person(name)
    tmdb_person_id <- person$id
    cli::cli_alert_success("Found: {person$name} (TMDB ID: {person$id})")
  }

  # Check cache

  cache_key <- paste0("filmography_", tmdb_person_id)
  if (use_cache && is_cached("tmdb", "person", cache_key, expires_days = 30)) {
    cli::cli_alert_info("Using cached filmography")
    filmography <- read_cache("tmdb", "person", cache_key)
    if (min_revenue > 0) {
      filmography <- dplyr::filter(filmography, revenue >= min_revenue)
    }
    return(filmography)
  }

  # Fetch movie credits
  result <- tryCatch(
    tmdb_get(paste0("person/", tmdb_person_id, "/movie_credits")),
    error = function(e) {
      cli::cli_abort("Failed to fetch filmography for TMDB person {tmdb_person_id}: {e$message}")
    }
  )

  if (is.null(result$cast) || length(result$cast) == 0) {
    cli::cli_alert_warning("No film credits found.")
    return(dplyr::tibble(
      title = character(), year = integer(), release_date = as.Date(character()),
      revenue = numeric(), character = character(),
      billing_order = integer(), tmdb_id = integer()
    ))
  }

  # Build tibble — need to fetch each movie for revenue since credits

  # endpoint doesn't include it. But we can get release_date from the
  # credits response itself.
  filmography <- purrr::map(result$cast, function(credit) {
    dplyr::tibble(
      title = credit$title %||% NA_character_,
      release_date = credit$release_date %||% NA_character_,
      revenue = NA_real_,
      character = credit$character %||% NA_character_,
      billing_order = (credit$order %||% NA_integer_) + 1L,
      tmdb_id = credit$id %||% NA_integer_
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      release_date = as.Date(release_date),
      year = as.integer(format(release_date, "%Y"))
    ) |>
    dplyr::filter(!is.na(release_date))

  # Fetch revenue for each film (uses per-movie cache from discover)
  cli::cli_alert_info("Fetching revenue for {nrow(filmography)} films...")

  n_films <- nrow(filmography)
  revenues <- numeric(n_films)
  for (i in seq_len(n_films)) {
    id <- filmography$tmdb_id[i]
    if (i %% 20 == 0) cli::cli_alert_info("Progress: {i}/{n_films} films")
    movie_key <- as.character(id)
    if (use_cache && is_cached("tmdb", "movie", movie_key)) {
      movie <- read_cache("tmdb", "movie", movie_key)
      revenues[i] <- movie$revenue %||% 0
      next
    }
    raw <- tryCatch(tmdb_get(paste0("movie/", id)), error = function(e) NULL)
    if (is.null(raw)) { revenues[i] <- 0; next }
    movie <- list(
      tmdb_id = raw$id, title = raw$title %||% NA_character_,
      release_date = raw$release_date %||% NA_character_,
      revenue = raw$revenue %||% 0
    )
    if (use_cache) write_cache("tmdb", "movie", movie_key, movie)
    revenues[i] <- movie$revenue
  }

  filmography$revenue <- revenues

  filmography <- dplyr::arrange(filmography, dplyr::desc(revenue))

  if (use_cache) write_cache("tmdb", "person", cache_key, filmography)

  # Inflation adjustment
  if (adjust_inflation) {
    rd <- if ("release_date" %in% names(filmography)) filmography$release_date else NULL
    filmography$adjusted_revenue <- adjust_inflation(
      filmography$revenue, filmography$year, inflation_year,
      release_date = rd
    )
    sort_col <- "adjusted_revenue"
    cli::cli_alert_info("Revenue adjusted to {inflation_year} dollars")
  } else {
    sort_col <- "revenue"
  }

  if (min_revenue > 0) {
    filmography <- dplyr::filter(filmography, .data[[sort_col]] >= min_revenue)
  }

  filmography <- dplyr::arrange(filmography, dplyr::desc(.data[[sort_col]]))

  cli::cli_alert_success(
    "Found {nrow(filmography)} films{if (min_revenue > 0) paste0(' with revenue >= $', format(min_revenue, big.mark = ',')) else ''}"
  )

  filmography
}
