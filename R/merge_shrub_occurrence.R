merge_shrub_occurrence <- function(x, crs = "EPSG:4326") {

  # Check inputs.
  if (!is.list(l)) cli::cli_abort("Input object 'l' must be a list")
  if (!is_valid_epsg(crs)) cli::cli_abort("Input 'crs' code is not valid")


  # Create SpatVector objects.
  x <- lapply(x, function(obj) terra::vect(obj, geom = c("coordx", "coordy"), crs = df_crs))


  # Project to input crs.
  x <- lapply(x, function(obj) terra::project(obj, crs))


  # Merge into a single SpatVector object.
  x <- dplyr::bind_rows(x)




}
