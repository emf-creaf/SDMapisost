get_example_data <- function(what = NULL, species = NULL) {

  data_labels <- c("presence", "mask", "predictors")

  # Checks.
  if (any(is.null(what), is.null(species))) cli::cli_abort("Wrong inputs")
  if (!(what %in% data_labels)) cli::cli_abort("Wrong 'what' input")

  # For "predictors" we also need the presence data.
  if (what %in% c("presence", "predictors")) {
    data(presence_data)
    if (!is.null(species)) presence_data <- presence_data[[species]]
    if (what == "presence") return(presence_data)
  }

  if (what == "mask") {
    data(corine_mask)
    corine_mask <- terra::unwrap(corine_mask)
    return(corine_mask)
  } else if (what == "predictors") {
    data(raster_list_all)
    for (x in names(raster_list_all)) {
      for (y in names(raster_list_all[[x]])) {
        raster_list_all[[x]][[y]] <- terra::unwrap(raster_list_all[[x]][[y]])
      }
    }
    raster_list_selected <- select_raster_list(raster_list_all, names(presence_data))
    return(raster_list_selected)
  }

}
