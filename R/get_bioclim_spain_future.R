#' Title
#'
#' @param bioclim_url
#' @param resolution
#' @param epsg
#' @param dest_folder
#' @param mainland
#' @param ssp
#' @param gcm
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
get_bioclim_spain_future <- function(base_url_to_download = NULL,
                                     gcm = "EC-Earth3-Veg",
                                     ssp = 126,
                                     resolution = "30s",
                                     destination_folder = NULL,
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

  if (is.null(destination_folder) || !file.exists(destination_folder)) {
    cli::cli_abort("Parameter 'destination_folder' must correspond to a valid folder")
  }

  if (!is_valid_ssp(ssp)) {
    cli::cli_abort("Invalid 'ssp' value")
  }

  if (!is_valid_gcm(gcm)) {
    cli::cli_abort("Invalid 'gcm' value")
  }


  # Time intervals.
  time_intervals <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")


  # Full URL for files.
  if (is.null(base_url_to_download)) {
    base_url_to_download <- "https://geodata.ucdavis.edu/cmip6"
  }
  base_url_to_download <- paste0(file.path(base_url_to_download,
                                           resolution,
                                           gcm,
                                           paste0("ssp", ssp),
                                           "wc2.1_"),
                                 resolution,
                                 "_bioc_",
                                 gcm,
                                 "_ssp", ssp, "_")
  file_to_download <- paste0(base_url_to_download, time_intervals, ".tif")


  # Checking that remote files exist before commiting to download.
  files_exist <- sapply(file_to_download, is_valid_url)
  if (any(!files_exist)) {
    cli::cli_abort("Invalid remote files")
  }


  # # Checking that remote files exist before commiting to download.
  # file_to_download <- paste0(base_url, time_intervals, ".tif")
  # files_exist <- sapply(files_to_download, is_valid_url)
  # if (any(!files_exist)) {
  #   cli::cli_abort("Couldn't find some remote files")
  # }


  # Fetch Spain polygon.
  if (verbose) cli::cli_alert_info("Retrieving Spain geographic limits")
  mask <- get_polygon_spain(mainland = mainland)


  # If no EPSG is provided it extracts it from the Spain polygon.
  # If it is, the Spain polygon is transformed.
  if (is.null(epsg)) {
    epsg <- extract_epsg(mask)
  } else {
    if (extract_epsg(mask) != epsg) {
      mask <- sf::st_transform(mask, epsg)
    }
  }


  # Downloading.
  out <- download_bioclim_files(file_to_download = file_to_download,
                                destination_folder = destination_folder,
                                create_folder = create_folder,
                                verbose = verbose)



  # # Time intervals.
  # time_intervals <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")
  #
  #
  # # The basic URL for downloading files.
  # base_url <- paste0("https://geodata.ucdavis.edu/cmip6/", resolution, "/")
  # gcm_url <- paste0(base_url, gcm, "/")
  # gcm_ssp_url <- paste0(gcm_url, "ssp", ssp, "/")
  # file_url <- paste0(gcm_ssp_url, "wc2.1_", resolution, "_bioc_", gcm, "_ssp", ssp, "_")
  #
  #
  # # Checking that remote files exist before commiting to download.
  # files_to_download <- paste0(paste0(file_url, time_intervals, ".tif"))
  # files_exist <- sapply(files_to_download, is_valid_url)
  # if (any(!files_exist)) {
  #   cli::cli_abort("Couldn't find some remote files")
  # }


  # Download files and reproject files if needed.
  if (verbose) cli::cli_alert_info("Clipping bioclim rasters")
  destfile <- setNames(file.path(dest_folder, basename(files_to_download)), files_to_download)

browser()
  # Multiple download.
  out <- curl::multi_download(url = files_to_download, destfile = destfile, progress = verbose)


  # Reproject (if needed), clip and mask.
  for (x in out$destfile) {

    # Read file and get EPSG code.
    if (verbose) cli::cli_alert_info(paste0("Reading file from disk"))
    r <- terra::rast(x)
    epsg_r <- extract_epsg(r)

    # A coordinate transformation may be required.
    if (epsg_r != epsg) {
      if (verbose) cli::cli_alert_info("Reprojecting raster to EPSG:", epsg)
      r <- terra::project(r, paste0("epsg:", epsg))
    }

    # Crop and mask.
    if (verbose) cli::cli_alert_info("Cropping and masking raster")
    r <- r |>
      terra::crop(mask, snap = "out") |>
      terra::mask(mask)

    # Save back on disk with the same name.
    if (verbose) cli::cli_alert_info(paste0("Writing raster to disk"))
    terra::writeRaster(r, destfile[i], overwrite = TRUE)

  }

  return(destfile)

}
