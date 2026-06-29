filter_spThin <- function(df, crs = 25830, min_dist, strictly = TRUE) {

  # Index.
  df$id <- 1:nrow(df)

  # From data.frame to sf.
  sf_object <- df |>
    sf::st_as_sf(coords = c("x", "y"), crs = crs) |>
    sf::st_transform(sf_utm, crs = 4326)

  # Back into a data.frame. A fake "species" column is added.
  sfcoord <- sf::st_coordinates(sf_object)
  df_new <- data.frame(
    id = sf_object$id,
    species = rep("A", nrow(df)),
    long = sfcoord[,1],
    lat = sfcoord[,2]
  )

  # Spatial thinning.
  df_thinned <- spThin::thin(
    loc.data = df_new,
    lat.col = "lat", long.col = "long",
    spec.col = "species",
    thin.par = min_dist, # 20 km threshold
    reps = 1, locs.thinned.list.return = TRUE,
    write.files = FALSE, write.log.file = FALSE,
    verbose = FALSE
  )[[1]]

  # Select rows.
  df <- df[as.numeric(rownames(df_thinned)), ]
  df$id <- NULL

  return(df)
}
