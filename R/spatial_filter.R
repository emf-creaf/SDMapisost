#' Title
#'
#' @param p
#' @param min_dist \code{numeric} value, minimum distance in Km.
#' @param crs
#' @param num_simu
#'
#' @returns
#' @export
#'
#' @examples
spatial_filter <- function(p, min_dist = 20, num_simu = 10) {

  # Checks.
  if (!is(p, "SpatVector")) cli::cli_abort("Input 'p' must be a SpatVect object")


  # Reproject if needed.
  if (!terra::same.crs(p, "epsg:4326")) p <- terra::project(p, "EPSG:4326")


  # Transform to data.frame. A fake "species" column is added.
  p_coord <- terra::geom(p)
  df <- data.frame(
    id = p_coord[, "geom"],
    species = rep("A", nrow(p)),
    long = p_coord[, "x"],
    lat = p_coord[, "y"]
  )

  # Spatial thinning.
  df_thinned <- spThin::thin(
    loc.data = df,
    lat.col = "lat", long.col = "long",
    spec.col = "species",
    thin.par = min_dist,
    reps = 1, locs.thinned.list.return = TRUE,
    write.files = FALSE, write.log.file = FALSE,
    verbose = FALSE
  )[[1]]


  # Select rows and transform, if needed.
  p <- p[as.numeric(rownames(df_thinned)), ]
  if (!terra::same.crs(p, "epsg:25830")) p <- terra::project(p, "epsg:25830")


  return(p)
}
