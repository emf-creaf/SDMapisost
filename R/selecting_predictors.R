#' Title
#'
#' @returns
#' @export
#'
#' @examples
selecting_predictors <- function(save = TRUE) {

  # Presence location.
  folder <- "C:/imidra/ifn/ifn4_28.rds"
  species <- c("Cistus ladanifer", "Erica arborea", "Genista florida",
               "Quercus coccifera", "Retama sphaerocarpa")

  # Absolute paths to files.
  path <- list(terrain = file.path("C:/imidra/mdt/mdt_madrid.tif"),
               climate = file.path("C:/imidra/bioclim", paste0("historico/wc2.1_10m_bio/wc2.1_10m_bio_", 1:19, ".tif")),
               categorical = list(corine = file.path("C:/imidra/corine/corine_2018/CORINE Madrid nivel 1.tif")),
               distances = list(hydro = file.path("C:/imidra/hidro/distancia_hidro.tif")))
  names(path$climate) <- paste0("bioclim_", 1:19)


  #' Read predictor data.
  x <- get_predictors(path)

  cutoff <- setNames(rep(0.95, length(species)), species)

  out <- list()

  for (sp in species) {
    p <- get_presence(folder, sp, "EPSG:25830")

    #' Extract predictors at 'p' locations.
    y <- extract_predictors(p, x)

    out[[sp]] <- select_variables(y, cutoff = cutoff[sp])

    # Save.
    if (save) {
      cli::cli_alert_info(paste0("Saving results for species ", sp))
      base::saveRDS(out, file = "final_predictors.rds")
    }

  }

  return(out)

}
