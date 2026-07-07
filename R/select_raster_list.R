#' Title
#'
#' @param x
#' @param names_to_keep
#'
#' @returns
#' @export
#'
#' @examples
select_raster_list <- function(raster_list, names_to_keep) {

  selected_list <- lapply(raster_list, function(a) a[names(a) %in% names_to_keep])


  return(selected_list)

}
