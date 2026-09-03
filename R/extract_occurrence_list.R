#' Title
#'
#' @param path
#' @param species
#' @param target
#' @param merge
#' @param crs
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
#' # IFN data.
#' path <- paste0("C:/imidra/peninsula/ifn/", c("ifn4_01.rds", "ifn4_28.rds"))
#' species <- "Erica arborea"
#' x <- extract_occurrence_list(path, species, "shrub", merge = TRUE, crs = "EPSG:4326")
extract_occurrence_list <- function(path, species, target = "tree", merge = TRUE, crs = NULL, verbose = TRUE) {

  # Checks.
  if (merge) {
    if (!is_valid_epsg(crs)) {
      cli::cli_abort("If 'merge' is TRUE the a valid 'crs' must be provided")
    }
  }


  # Read data files.
  dat <- lapply(path, extract_occurrence, species = species, target = target, verbose = FALSE)
  num_elem <- length(dat)


  # If crs is valid we assume that a spatial transformation is sought.
  if (!is.null(crs)) {
    if (is_valid_epsg(crs)) {

      # First we make sure that all datasets have the same columns, in any order.
      # If this is ok, we also make sure that the order is the same.
      if (num_elem > 1) {
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

      # Each row may have a different crs. We transform each row to SpatVector and project to crs.
      if (verbose) cli::cli_alert_info(paste0("Projecting to crs = ", crs, " and merging"))
      dat <- lapply(dat, function(sub_dat) {
        v <- lapply(1:nrow(sub_dat), function(i) {
          x <- terra::vect(sub_dat[i, ], geom = c("coordx", "coordy"), crs = paste0("EPSG:", sub_dat[i, "crs"]))
          terra::project(x, crs)
        })
        v <- terra::vect(v)
      })


      # Finally, all datasets are merged into a big single data.frame.
      dat <- terra::vect(dat)

    }
  }

  return(dat)

}
