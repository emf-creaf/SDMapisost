#' Title
#'
#' @param carpeta Cadena de caracteres indicando la carpeta en la que encontrar los archivos
#' de datos ".rds" de los IFN.
#' @param nombre_especie Cadena de caracteres con el nombre en latín de la especie.
#' @param ifn Valor numérico indicando qué IFN hay que leer. Debe ser 2, 3 o 4.
#'
#' @returns
#' Un objecto "sf" con el contenido de los datos de presencia de la especie.
#' @export
#'
#' @examples
datos_especie <- function(carpeta, nombre_especie, ifn = 2) {

  # Checks.
  nombre_especie <- trimws(nombre_especie)
  if (!is.numeric(ifn)) cli::cli_abort("Parámetro de entrada 'ifn' debe ser numérico")
  if (length(ifn) != 1) cli::cli_abort("Parámetro de entrada 'ifn' debe tener longitud = 1")
  if (!(ifn %in% c(2, 3, 4))) cli::cli_abort("Parámetro de entrada 'ifn' debe ser igual a 2, 3 o 4")


  # Leemos archivo para Madrid (código 28).
  df <- switch(as.character(ifn),
               "2" = readRDS(file.path(carpeta, "ifn2_28.rds")),
               "3" = readRDS(file.path(carpeta, "ifn3_28.rds")),
               "4" = readRDS(file.path(carpeta, "ifn4_28.rds"))
  )


  # Obtenemos datos de presencia para la especie.
  x <- df[, c("id_unique_code", "plot", "coordx", "coordy")]
  x$y <- sapply(df$understory, function(z) {
    zz <- z$shrub[[1]]
    if (is.data.frame(zz) && nrow(zz) > 0) {
      nombre_especie %in% trimws(zz$sp_name)
      } else {
        FALSE
      }
  })


  # Convertir a un objeto "sf".
  x <- sf::st_as_sf(x,
                    coords = c("coordx", "coordy"),
                    crs = sf::st_crs(paste0("EPSG:", df$crs[1])))


  return(x)

}
