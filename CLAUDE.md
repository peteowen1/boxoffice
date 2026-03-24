# CLAUDE.md

`boxoffice` — R package for scraping box office data and projecting theatrical runs.

## Core Commands

```r
devtools::load_all()    # Load package
devtools::check()       # Full check
devtools::document()    # Regenerate docs (never edit NAMESPACE manually)
```

## Key Modules

- `scrape_bom.R`, `scrape_numbers.R`, `scrape_discovery.R` — Data scrapers
- `features.R` — Feature engineering for prediction
- `prediction.R` — Theatrical run projection models
- `cache.R` — Caching layer
- `rate_limiting.R` — Rate limiting for scrapers

## Data

- `data-raw/` — Raw data processing scripts
- `data/` — Package datasets
