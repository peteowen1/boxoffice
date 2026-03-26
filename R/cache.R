#' Get default cache directory path
#'
#' Returns the path to the boxoffice package cache directory.
#' Uses project-local .cache folder by default.
#' Creates the directory if it doesn't exist.
#'
#' @return Character path to cache directory
#' @export
#' @examples
#' boxoffice_cache_dir()
boxoffice_cache_dir <- function() {
  cache_dir <- getOption("boxoffice.cache_dir", NULL)

 if (is.null(cache_dir)) {
    # Use a local (non-synced) directory to avoid OneDrive overhead
    cache_dir <- file.path(tools::R_user_dir("boxoffice", which = "cache"))
  }

  # Ensure directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  cache_dir
}

#' Set cache directory
#'
#' @param path Character path to cache directory
#' @export
#' @examples
#' set_cache_dir(tempdir())
set_cache_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  options(boxoffice.cache_dir = path)
  invisible(path)
}

#' Get path to specific cache subdirectory
#'
#' @param source Character: "bom" or "numbers"
#' @param type Character: "yearly", "daily", or "metadata"
#' @return Character path
#' @keywords internal
cache_subdir <- function(source, type) {
  path <- file.path(boxoffice_cache_dir(), source, type)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

#' Generate cache key from parameters
#'
#' @param ... Parameters to include in key
#' @return Character cache key
#' @keywords internal
make_cache_key <- function(...) {
  parts <- list(...)
  parts <- vapply(parts, as.character, character(1))
  paste(parts, collapse = "_")
}

#' Get cache file path for a key
#'
#' @param source Character: "bom" or "numbers"
#' @param type Character: "yearly", "daily", or "metadata"
#' @param key Character key identifier
#' @return Character file path
#' @keywords internal
cache_path <- function(source, type, key) {
  file.path(cache_subdir(source, type), paste0(key, ".rds"))
}

#' Check if data is cached and not expired
#'
#' @param source Character: "bom" or "numbers"
#' @param type Character: "yearly", "daily", or "metadata"
#' @param key Character key identifier
#' @param expires_days Number of days until expiry (NULL = never expires)
#' @return Logical
#' @keywords internal
is_cached <- function(source, type, key, expires_days = NULL) {
  path <- cache_path(source, type, key)

  if (!file.exists(path)) {
    return(FALSE)
  }

  # Check expiration
  if (!is.null(expires_days)) {
    file_time <- file.info(path)$mtime
    age_days <- as.numeric(difftime(Sys.time(), file_time, units = "days"))
    if (age_days > expires_days) {
      return(FALSE)
    }
  }

  TRUE
}

#' Read data from cache
#'
#' @param source Character: "bom" or "numbers"
#' @param type Character: "yearly", "daily", or "metadata"
#' @param key Character key identifier
#' @return Cached data or NULL if not found
#' @keywords internal
read_cache <- function(source, type, key) {
  path <- cache_path(source, type, key)

  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(
    readRDS(path),
    error = function(e) {
      warning("Failed to read cache: ", e$message)
      NULL
    }
  )
}

#' Write data to cache
#'
#' @param source Character: "bom" or "numbers"
#' @param type Character: "yearly", "daily", or "metadata"
#' @param key Character key identifier
#' @param data Data to cache
#' @return Invisible path to cache file
#' @keywords internal
write_cache <- function(source, type, key, data) {
  path <- cache_path(source, type, key)

  tryCatch({
    saveRDS(data, path)
    invisible(path)
  }, error = function(e) {
    warning("Failed to write cache: ", e$message)
    invisible(NULL)
  })
}

#' Cache a scrape failure with short expiry
#'
#' Caches a failure marker that expires after 1 day, so transient
#' errors don't permanently poison the cache.
#'
#' @param source Character: "bom" or "numbers"
#' @param type Character: "yearly", "daily", or "metadata"
#' @param key Character key identifier
#' @return Invisible NULL
#' @keywords internal
cache_failure <- function(source, type, key) {
  failure_type <- paste0(type, "_failures")
  write_cache(source, failure_type, key, data.frame())
  invisible(NULL)
}

#' Check if a previous scrape attempt failed recently
#'
#' @param source Character: "bom" or "numbers"
#' @param type Character: "yearly", "daily", or "metadata"
#' @param key Character key identifier
#' @return Logical: TRUE if a recent failure is cached (within 1 day)
#' @keywords internal
is_cached_failure <- function(source, type, key) {
  failure_type <- paste0(type, "_failures")
  is_cached(source, failure_type, key, expires_days = 1)
}

#' Clear cached data
#'
#' @param type Character: "all", "bom", "numbers", or specific type like "bom/daily"
#' @export
#' @examples
#' \dontrun{
#' clear_cache("all")
#' clear_cache("bom/daily")
#' }
clear_cache <- function(type = "all") {
  cache_dir <- boxoffice_cache_dir()

  if (type == "all") {
    unlink(cache_dir, recursive = TRUE)
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    cli::cli_alert_success("Cleared all cached data")
  } else {
    target_dir <- file.path(cache_dir, type)
    if (dir.exists(target_dir)) {
      unlink(target_dir, recursive = TRUE)
      cli::cli_alert_success("Cleared cache: {type}")
    } else {
      cli::cli_alert_warning("Cache directory not found: {type}")
    }
  }

  invisible(TRUE)
}

#' Get cache statistics
#'
#' @return List with cache statistics
#' @export
#' @examples
#' cache_stats()
cache_stats <- function() {
  cache_dir <- boxoffice_cache_dir()

  if (!dir.exists(cache_dir)) {
    return(list(
      total_files = 0,
      total_size_mb = 0,
      sources = list()
    ))
  }

  files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)

  if (length(files) == 0) {
    return(list(
      total_files = 0,
      total_size_mb = 0,
      sources = list()
    ))
  }

  file_info <- file.info(files)
  total_size <- sum(file_info$size, na.rm = TRUE) / 1024 / 1024

  # Count by source and type — normalize separators for reliable stripping
  norm_cache <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  norm_files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  rel_paths <- sub(paste0(norm_cache, "/"), "", norm_files, fixed = TRUE)

  parts <- strsplit(rel_paths, "[/\\\\]")
  sources <- vapply(parts, function(x) if (length(x) >= 1) x[1] else "unknown", character(1))
  types <- vapply(parts, function(x) if (length(x) >= 2) x[2] else "unknown", character(1))

  source_counts <- table(sources)
  type_counts <- table(paste(sources, types, sep = "/"))

  list(
    total_files = length(files),
    total_size_mb = round(total_size, 2),
    by_source = as.list(source_counts),
    by_type = as.list(type_counts),
    oldest = min(file_info$mtime, na.rm = TRUE),
    newest = max(file_info$mtime, na.rm = TRUE)
  )
}

#' Check cache for a film's daily data
#'
#' Convenience function to check if daily data is cached.
#'
#' @param source Character: "bom" or "numbers"
#' @param id Character film identifier (release_id for BOM, movie_slug for Numbers)
#' @return Logical
#' @export
has_cached_daily <- function(source, id) {
  is_cached(source, "daily", id, expires_days = NULL)
}

#' Check cache for yearly index
#'
#' @param source Character: "bom" or "numbers"
#' @param year Integer year
#' @param expires_days Days until expiry (default: 7 for current year, NULL for past)
#' @return Logical
#' @export
has_cached_yearly <- function(source, year, expires_days = NULL) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  # Current year expires after 7 days, past years never expire
  if (is.null(expires_days)) {
    expires_days <- if (year >= current_year) 7 else NULL
  }

  is_cached(source, "yearly", as.character(year), expires_days = expires_days)
}
