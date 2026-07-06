#' Title
#'
#' @param x
#' @param names_to_keep
#'
#' @returns
#' @export
#'
#' @examples
filter_raster_list <- function(x, names_to_keep) {

  filtered_list <- lapply(x, function(a) a[names(a) %in% names_to_keep])


  return(filtered_list)

}
