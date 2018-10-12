p3_create_dir <- function(copy = FALSE, path = NA) {

  dir.create("phi/processed", recursive = TRUE)
  dir.create("phi/temp", recursive = TRUE)
  dir.create("phi/unprocessed", recursive = TRUE)

  if (copy == TRUE) {

    file.copy(from = path, to = getwd(), recursive = TRUE)

  }

}
