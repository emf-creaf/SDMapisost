#' Title
#'
#' @param epsg
#' @param mainland
#'
#' @returns
#' @export
#'
#' @details
#' Simple use of package 'mapSpain' to retrieve polygons.
#'
#'
#' @examples
get_polygon_spain <- function(epsg = NULL, mainland = TRUE) {


  # Checks.
  if (is.null(epsg)) epsg = "4326"


  # Extract map of Spain with best resolution available.
  spain <- mapSpain::esp_get_country(moveCAN = FALSE,
                                     epsg = epsg,
                                     resolution = "01")


  # Convert to "POLYGON" and select polygon with largest area.
  spain_polygons <- spain |>
    sf::st_geometry() |>
    sf::st_cast("POLYGON")


  # Select mainland Spain if required.
  if (mainland) spain_polygons <- spain_polygons[which.max(sf::st_area(spain_polygons)), ]
  spain_polygons <- spain_polygons |>
    sf::st_geometry() |>
    sf::st_sf()


  return(spain_polygons)
}
