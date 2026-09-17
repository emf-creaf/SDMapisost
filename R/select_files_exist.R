select_files_exist <- function(path, filter = TRUE) {

  # Checks.
  if (!is.character(path) || length(path) == 0) {
    cli::cli_abort("Input 'path' must be a non-empty character vector")
  }


  # Simple command.
  x <- sapply(path, file.exists)


  #
  if (filter) {
    return(path[x])
  } else {
    return(x)
  }


  return(x)

}
