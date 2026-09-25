#' Title
#'
#' @param ssp
#'
#' @returns
#' @export
#'
#' @examples
is_valid_ssp <- function(ssp) {

  if (is.character(ssp)) ssp <- as.numeric(ssp)

  valid_ssp <- c(126, 245, 370, 585)
  if (ssp %in% valid_ssp) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}
