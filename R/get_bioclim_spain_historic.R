#' Title
#'
#' @param resolution
#' @param dest_folder
#'
#' @returns
#' @export
#'
#' @examples
get_bioclim_spain_historic <- function(file_to_download = NULL,
                              destination_folder = NULL,
                              create_folder = TRUE,
                              resolution = "30s",
                              epsg = NULL,
                              mainland = TRUE,
                              verbose = TRUE) {


  # Checks.
  if (!is.character(resolution) || length(resolution) > 1) {
    cli::cli_abort("Parameter 'resolution' must be a single-element character vector")
  }

  if (!(resolution %in% c("30s", "2.5m", "5m", "10m"))) {
    cli::cli_abort("Wrong 'resolution' value")
  }

  if (!is.null(epsg)) {
    if (!is_valid_epsg(epsg)) {
      cli::cli_abort("Wrong 'epsg' code")
    }
  }

  # Building URL for download.
  if (is.null(file_to_download)) {
    file_to_download <- paste0("https://geodata.ucdavis.edu/climate/worldclim/2_1/base/wc2.1_", resolution, "_bio.zip")
  }
  if (!is_valid_url(file_to_download)) {
    cli::cli_abort("Wrong 'file_to_download' parameter")
  }


  # Checking that remote file exists before commiting to download.
  files_exist <- sapply(file_to_download, is_valid_url)
  if (any(!files_exist)) {
    cli::cli_abort("Invalid remote files")
  }


  # Fetch Spain polygon.
  if (verbose) cli::cli_alert_info("Retrieving Spain geographic limits")
  mask <- get_polygon_spain(mainland = mainland)


  # Downloading.
  out <- download_bioclim_files(file_to_download = file_to_download,
                                destination_folder = destination_folder,
                                create_folder = create_folder,
                                verbose = verbose)
browser()

  # # Downloading file onto temporary folder.
  # if (verbose) cli::cli_alert_info("Downloading to a temporary file")
  # temp_dir <- tempdir()
  # temp_name <- file.path(temp_dir, basename(bioclim_url))
  # r <- curl::multi_download(url = bioclim_url,
  #                           destfile = temp_name,
  #                           progress = verbose)
  #
  # # Unzipping file.
  # if (verbose) cli::cli_alert_info("Unzipping bioclim file")
  # folder_name <- file.path(dest_folder, tools::file_path_sans_ext(basename(bioclim_url)))
  # out <- unzip(temp_name, exdir = folder_name, list = TRUE)
  # file.remove(temp_name)


  # If no EPSG is provided it extracts it from the Spain polygon.
  # If it is, the Spain polygon is transformed.
  if (is.null(epsg)) {
    epsg <- extract_epsg(mask)
  } else {
    if (extract_epsg(mask) != epsg) {
      mask <- sf::st_transform(mask, epsg)
    }
  }


  # Loop to crop bioclim files to Spain limits.
  if (verbose) cli::cli_alert_info("Clipping bioclim rasters")

  for (i in out$Name) {
browser()
    # Read file and get EPSG code.
    if (verbose) cli::cli_alert_info(paste0("Reading file from disk"))
    r <- terra::rast(file.path(destination_folder, i))
    epsg_r <- extract_epsg(r)


    # A coordinate transformation may be required.
    if (epsg_r != epsg) {
      if (verbose) cli::cli_alert_info("Reprojecting raster to EPSG:", epsg)
      r <- terra::project(r, paste0("epsg:", epsg))
    }


    # Crop and mask.
    if (verbose) cli::cli_alert_info(" Cropping and masking raster")
    r <- r |>
      terra::crop(mask, snap = "out") |>
      terra::mask(mask)


    # Save back on disk with the same name.
    if (verbose) cli::cli_alert_info(paste0("  Writing raster to disk"))
    terra::writeRaster(r, file.path(destination_folder, i), overwrite = TRUE)
  }

}
