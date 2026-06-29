compare_spThin_filter <- function() {


  library(spThin)
  library(sf)
  library(ggplot2)

  # 1. Create mock UTM data (e.g., UTM Zone 18N, EPSG: 32618)
  set.seed(42)
  n_points <- 150
  nsimu <- 100

  utm_x <- c(runif(n_points - 10, 300000, 400000), runif(10, 340000, 350000))
  utm_y <- c(runif(n_points - 10, 4200000, 4300000), runif(10, 4240000, 4250000))

  utm_df <- data.frame(
    id = 1:n_points,
    species = "Species_A",
    Easting = utm_x,
    Northing = utm_y
  )

  # 2. Convert UTM to Lat/Long using the sf package
  # Replace '32618' with your specific UTM zone EPSG code
  sf_utm <- st_as_sf(utm_df, coords = c("Easting", "Northing"), crs = 32618)
  sf_latlon <- st_transform(sf_utm, crs = 4326)

  # Extract the converted coordinates back into a dataframe
  thin_input <- data.frame(
    id = sf_latlon$id,
    species = sf_latlon$species,
    long = st_coordinates(sf_latlon)[,1],
    lat = st_coordinates(sf_latlon)[,2]
  )

  # 3. Run spThin (thin.par is in kilometers)
  thin_results <- thin(
    loc.data = thin_input,
    lat.col = "lat", long.col = "long", spec.col = "species",
    thin.par = 20, # 20 km threshold
    reps = nsimu, locs.thinned.list.return = TRUE,
    write.files = FALSE, write.log.file = FALSE
  )

  # 4. Extract and map back to your original UTM dataframe
  thinned_points <- thin_results[[1]]
  retained_rows <- as.numeric(rownames(thinned_points))

  # Mark status in the original UTM data frame
  utm_df$status <- "Discarded"
  utm_df$status[thin_input$id[retained_rows]] <- "Retained"


  filter_results <- lapply(1:nsimu, function(i) distance_filter(utm_df, min_dist = 20000,
                                                   columns = c("Easting", "Northing"), verbose = FALSE))

  nrow_spThin <- sapply(thin_results, nrow)
  nrow_filter <- sapply(filter_results, nrow)

  print(list('Num. points spThin' = table(nrow_spThin),
          'Num. points filter_distance' = table(nrow_filter)))


}
