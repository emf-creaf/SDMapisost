get_mdt_spain <- function(epsg = NULL, mainland = TRUE, z = 10, verbose = TRUE) {

  # Checks.
  if (is.null(epsg)) epsg = "4326"


  # Get polygon of mainland Spain.
  if (verbose) cli::cli_alert_info("Retrieving polygon for Spain")
  spain <- get_polygon_spain(epsg = epsg, mainland = mainland)


  # Download higher resolution raster (z = 10 is ~100m,
  # check https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution)
  dem_raw <- elevatr::get_elev_raster(locations = spain, z = z, clip = "bbox", verbose = verbose)


  # Older versions of elevatr package returned a RasterLayer object.
  # Convert into SpatRaster if needed.
  if (inherits(dem_raw, "RasterLayer")) dem_raw <- terra::rast(dem_raw)


  # Crop dem.
  dem <- terra::crop(dem_raw, spain, mask = TRUE)


  return(dem)

}

