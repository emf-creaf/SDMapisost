#' Title
#'
#' @param gcm
#'
#' @returns
#' @export
#'
#' @examples
is_valid_gcm <- function(gcm) {

  # Checks.
  if (!is.character(gcm)) cli::cli_abort()

  valid_gcm <- c("ACCESS-CM2", "BCC-CSM2-MR", "CMCC-ESM2", "EC-Earth3-Veg",
                 "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "HadGEM3-GC31-LL",
                 "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR",
                 "MRI-ESM2-0", "UKESM1-0-LL")

  if (gcm %in% valid_gcm) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
