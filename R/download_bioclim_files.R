#' Title
#'
#' @param file_to_download
#' @param destination_folder
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
download_bioclim_files <- function(file_to_download = NULL,
                                   destination_folder = NULL,
                                   create_folder = TRUE,
                                   verbose = TRUE) {


  # Checks.
  if (is.null(file_to_download) || is.null(destination_folder)) {
    cli::cli_abort("Input parameters can't be empty")
  }

  if (!is.character(file_to_download) || !is.vector(file_to_download)) {
    cli::cli_abort("Input 'file_to_download' must be a character vector")
  }

  # If length = 1, name in file_to_download must have a 'zip' extension, corresponding to historic data.
  # If length > 1, names in file_to_download must have a 'tif' extensions, corresponding to future data.
  ext_file <- tolower(tools::file_ext(file_to_download))
  if (length(ext_file) == 1) {
    if (ext_file != "zip") {
      cli::cli_abort("Single file extension must be 'zip'")
    }
  } else {
    if (!all(ext_file == "tif")) cli::cli_abort("File extensions must be 'tif")
  }


  # Check that destination folder exists.
  if (!dir.exists(destination_folder)) {
    if (create_folder) {
      if (verbose) cli::cli_alert_info("'destination folder' does not exist. Creating it")
      dir.create(destination_folder)
    } else {
      cli::cli_abort("'destination_folder' does not exist. Set 'create_folder' to TRUE to create it")
    }
  }


  # Download file(s).
  if  (length(ext_file) == 1) {

    # Downloading zip file to temporary folder with a temporary file name.
    if (verbose) cli::cli_alert_info("Downloading 'zip' file")
    temp_name <- paste0(tempfile(pattern = "dummy"), ".zip")
    r <- curl::multi_download(url = file_to_download,
                              destfile = temp_name,
                              progress = verbose)

    # Unzipping.
    if (verbose) cli::cli_alert_info("Unzipping into destination folder")
    out <- unzip(temp_name, exdir = destination_folder, list = FALSE, overwrite = TRUE)
    file.remove(temp_name)

  } else {

    # Download all files into destination folder.
    if (verbose) cli::cli_alert_info("Downloading 'tif' files")
    out <- curl::multi_download(url = file_to_download,
                                destfile = file.path(destination_folder, basename(file_to_download)),
                                progress = verbose)

  }

  return(out)

}
