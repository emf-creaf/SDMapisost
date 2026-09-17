#' Title
#'
#' @description
#' \code{extract_occurrence_list} extracts occurrence data from several IFN data files
#' and for a single species. That occurrence data can be for a tree, shrub, herb and
#' regenerate species.
#'
#' @param path \code{character} string indicating the path to the file with the data..
#' @param species \code{character} string with the full name of the species.
#' @param target \code{character} string specifying what to extract. It can be
#' "tree", "shrub", "herbs" or "regen".
#' @param merge \code{logical} if set to TRUE
#' @param crs \code{character} string specifying a valid EPSG code.
#' @param verbose \code{logical} variable, if set to TRUE (default) progress information
#' is shown on screen.
#'
#' @returns
#' @export
#'
#' @returns
#' An object, or list of objects, containing a logical column labelled "occurrence".
#' All other columns are the same that are found in the original ".rds" file.
#'
#' There are three options for the output:
#' 1. merge = FALSE and crs = NULL: a list of data.frames as obtained from 'readRDS'
#' 2. merge = FALSE and crs = valid crs: a list of SpatVect objects projected to the input 'crs'.
#' 3. merge = TRUE and crs = valid crs; a single SpatVect object containing all files and projected to 'crs'.
#'
#' @examples
#' # IFN data.
#' path <- file.path("C:/imidra/peninsula/ifn", paste0("ifn4_", formatC(1:50, width = 2, flag = "0"), ".rds"))
#' species <- "Erica arborea"
#' path <- select_files_exist(path)
#' x <- extract_occurrence_list(path, species, "shrub", merge = TRUE, crs = "EPSG:4326")
extract_occurrence_list <- function(path, species, target = "tree", merge = TRUE, crs = NULL, verbose = TRUE) {

  # Checks.
  if (merge) {
    if (!is_valid_epsg(crs)) {
      cli::cli_abort("If 'merge' is TRUE then a valid 'crs' must be provided")
    }
  }


  # Reading data files.
  idx <- if (verbose) {
    cli::cli_progress_along(path,
                            name = "Reading IFN files and determining species occurrence",
                            clear = FALSE)
  } else {
    seq_along(path)
  }
  dat <- lapply(idx, function(i) {
    x <- path[i]
    extract_occurrence(x, species = species, target = target, verbose = FALSE)
  })
  if (verbose) cli::cli_progress_done()


  # If crs is valid we assume that a spatial transformation of each element
  # in list 'dat' must be performed.
  if (!is.null(crs)) {
    if (is_valid_epsg(crs)) {

      # Use cli_progress_along if verbose, otherwise standard index sequence
      idx <- if (verbose) {
        cli::cli_progress_along(seq_along(dat),
                                name = paste0("Projecting coordinates to ", crs),
                                clear = FALSE)
      } else {
        seq_along(dat)
      }

      dat <- lapply(idx, function(i) {
        sub_dat <- dat[[i]]

        # Split sub_dat into groups sharing the same CRS
        crs_groups <- split(sub_dat, sub_dat$crs)

        # Vectorize creation and projection per CRS group
        v_list <- lapply(crs_groups, function(group) {
          epsg_str <- paste0("EPSG:", group$crs[1])
          x <- terra::vect(group, geom = c("coordx", "coordy"), crs = epsg_str)
          terra::project(x, crs)
        })

        # Combine grouped SpatVectors back into a single object
        terra::vect(v_list)

      })
      if (verbose) cli::cli_progress_done()
    }
  }
  names(dat) <- path


  # If merge = TRUE the input 'crs' has already been proved above to be valid.
  if (merge) {
    if (verbose) cli::cli_alert_info(paste0("Merging into a single SpatVector object"))

    # First we make sure that all datasets have the same columns, in any order.
    # If this is ok, we also make sure that the order is the same.
    if (length(dat) > 1) {
      column_names <- sort(colnames(dat[[1]]))
      all_match <- all(sapply(dat, function(df) identical(sort(colnames(df)), column_names)))

      if (!all_match) {
        stop("There are data frames with different column names.")
      }

      column_names <- colnames(dat[[1]])
      all_match <- all(sapply(dat, function(df) identical(colnames(df), column_names)))

      if (!all_match) {
        stop("All data frames have matching column names, but column order differ")
      }
    }

    # Then, all datasets are merged into a big single data.frame.
    dat <- terra::vect(dat)

  }

  return(dat)

}
