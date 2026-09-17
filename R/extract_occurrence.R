#' EXtract single occurrence file
#'
#' @description
#' \code{extract_occurrence} extracts occurrence data from a single IFN data file
#' and for a single species. That occurrence data can be for a tree, shrub, herb and
#' regenerate species.
#'
#' @param path \code{character} string indicating the path to the file with the data..
#' @param species \code{character} string with the full name of the species.
#' @param target \code{character} string specifying what to extract. It can be
#' "tree", "shrub", "herbs" or "regen".
#' @param verbose \code{logical} variable, if set to TRUE (default) progress information
#' is shown on screen.
#'
#' @returns description
#' A \code{data.frame} containing a logical column labelled "occurrence".
#' All other columns are the same that are found in the original ".rds" file.
#'
#' @details
#' Straightforward.
#'
#' @export
#'
#' @examples
#' # IFN data.
#' path <- "C:/imidra/peninsula/ifn/ifn4_28.rds"
#' species <- "Erica arborea"
#' x <- extract_occurrence(path, species, "shrub")
#'
extract_occurrence <- function(path, species, target = "tree", verbose = TRUE) {

  # Check inputs.
  if (length(path) > 1) {
    cli::cli_abort("Only one path is allowed")
  }
  if (!file.exists(path)) {
    cli::cli_abort(paste0("Wrong input path ", path))
  }

  if (!(is.character(species) & length(species) == 1)) {
    cli::cli_abort("Input 'species' must be a single species name")
  }

  column_names <- c("tree", "shrub", "herbs", "regen")
  target <- tolower(target)
  if (!any(target %in% column_names)) cli::cli_abort("Wrong value for 'target' input")


  # Species to lower case.
  species <- tolower(trimws(species))


  # Reads all files into a list.
  if (verbose) cli::cli_alert_info("Reading data file")
  dat <- readRDS(path)


  # Extract shrub occurrence data for species.
  if (verbose) cli::cli_alert_info(paste0("Retreving ", target, " occurrence data for ", species))

  # Select element.
  dat <- dat |>
    tidyr::unnest("understory") |>
    dplyr::select(-all_of(column_names[-which(column_names == target)]))

  new_col <- paste0(colnames(dat), collapse = "")
  colnames(dat)[colnames(dat) == target] <- new_col


  dat$occurrence <- vapply(dat[[new_col]], function(sub_df) {

    # Check if sub_df exists and is a valid data frame with rows
    if (is.null(sub_df) || !is.data.frame(sub_df) || nrow(sub_df) == 0) {
      return(FALSE)
    }

    # Check if "sp_name" column exists
    if (!"sp_name" %in% names(sub_df)) {
      return(FALSE)
    }

    # Check for the presence of the species (ignoring NAs)
    any(!is.na(sub_df$sp_name) & tolower(sub_df$sp_name) == species)
  }, FUN.VALUE = logical(1))
  dat[[new_col]] <- NULL
  dat[["sp_name"]]<- species


  return(dat)

}
