#' Title
#'
#' @param x
#'
#' @returns
#' @export
#'
#' @details
#' Algorithm from GEMINI AI.
#'
#' @examples
all_named <- function(x) {
  # 1. Check if the names vector even exists
  # 2. Check that no name is an empty string ("")
  # 3. Check that no name is missing (NA)
  !is.null(names(x)) && all(names(x) != "" & !is.na(names(x)))
}
