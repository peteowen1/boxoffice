# CLAUDE.md — boxoffice

R package for scraping daily box office data from Box Office Mojo (BOM) and The Numbers, and building XGBoost/regression models to project full theatrical runs from opening performance. Includes TMDB integration for film discovery and metadata enrichment.

## Development Commands

Standard `devtools::load_all()` / `document()` / `check()` workflow (NAMESPACE is auto-generated — never edit manually).

No `tests/` directory — verify changes by running `data-raw/download_sample_data.R` (or the other `data-raw/` scripts) against the live BOM/Numbers sites.

## Pipeline

```
discover_films(year)            → film index across BOM/Numbers/TMDB
  → scrape_bom_daily() / scrape_numbers_daily()
                                → daily gross + theater counts
  → build_historical_dataset()  → merge sources, fill missing days, dedupe
  → extract_*_features()        → opening / trajectory / theater / temporal / categorical
  → train_projection_model()    → XGBoost on historical opening → total gross
  → project_theatrical_run()    → per-film projection (total + day-by-day trajectory)
```

47 exports across 13 R files. The pipeline is designed for **batch historical builds** (`build_historical_dataset()` covers 1980→present) and **single-film projection** (`project_theatrical_run()` for new releases).

## Module Map

| File | Purpose |
|------|---------|
| `boxoffice.R` | Package-level docs and the top-level wrappers |
| `scrape_bom.R` | Box Office Mojo: daily, yearly index, metadata. `scrape_bom_daily_batch()` for bulk |
| `scrape_numbers.R` | The Numbers: parallel scrapers for cross-source validation |
| `scrape_discovery.R` | `discover_films()`, `match_films_across_sources()` |
| `tmdb.R` | TMDB API for film metadata + actor lookups (`top_actors_by_boxoffice()`) |
| `features.R` | Five extractor families: opening, trajectory, theater, temporal, categorical |
| `prediction.R` | `train_projection_model()`, `project_total_gross()`, `project_daily_trajectory()` |
| `data_processing.R` | `merge_daily_data()`, `merge_metadata()`, `fill_missing_days()`, `calculate_multiplier()`, `calculate_opening_metrics()`, `validate_daily_data()` |
| `cache.R` | Disk cache via `boxoffice_cache_dir()` — defaults to `tools::R_user_dir("boxoffice", "cache")` |
| `rate_limiting.R` | `create_scrape_session()` + per-request throttling |
| `utils.R` | Parsing/standardising helpers: `parse_currency()`, `parse_date_flexible()`, `standardize_distributor()`, etc. |
| `data.R` | Roxygen for the `top_actors` packaged dataset |
| `globals.R` | NSE declarations for `R CMD check` |

## Caching

`boxoffice_cache_dir()` returns `tools::R_user_dir("boxoffice", which = "cache")` by default (the per-user cache under `AppData/Local`, which OneDrive does not sync). Override via `options(boxoffice.cache_dir = "path")`.

- `has_cached_daily(source, id)` / `has_cached_yearly(source, year, expires_days)` — check before scraping
- `cache_stats()` / `clear_cache()` — inspect and prune
- `set_cache_dir(path)` — relocate

## Key Conventions

- **Two sources, two ID systems** — BOM uses its own film IDs (alphanumeric), Numbers uses URL slugs. `match_films_across_sources()` joins by normalised title + year + lead actor. Always pass `source = "bom"` or `"numbers"` explicitly when calling per-source scrapers.
- **Daily data may have gaps** — wide weekends, holiday closures, missing weekdays for limited releases. `fill_missing_days()` interpolates with `validate_daily_data()` flagging suspicious zero-runs.
- **Per-day rate limiting** — `create_scrape_session()` wraps an httr session with throttle config; never call scrapers in parallel without sharing the session, or BOM will rate-limit.
- **NAMESPACE auto-generated** — never edit; use `@export` + `devtools::document()`.

## Data Locations

- `data-raw/data/` — downloaded historicals (gitignored, large)
- `data-raw/top_actors.parquet` — cached TMDB actor enrichment

## Output Object

`project_theatrical_run()` returns an S3 `boxoffice_projection` with a `print()` method. Access components: `$total_gross`, `$daily_trajectory`, `$opening_metrics`, `$comparables` (from `find_comparable_films()`).
