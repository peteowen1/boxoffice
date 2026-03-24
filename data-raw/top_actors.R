# Top actors by box office hits
# Requires TMDB_API_KEY in .Renviron

devtools::load_all()

# All actors in $100M+ worldwide films, all time
actors <- top_actors_by_boxoffice(
  min_revenue = 1e8,
  min_films = 3
)

# Preview top 30
print(
  dplyr::select(actors, actor_name, n_films, total_revenue, avg_billing) |>
    dplyr::slice_head(n = 30),
  n = 30
)

# Save full results
arrow::write_parquet(actors, "data-raw/top_actors.parquet")
cli::cli_alert_success("Saved {nrow(actors)} actors to data-raw/top_actors.parquet")
