
#' Extract files from the .tar archive and move them to the results directory
#' @param tar.file .tar archive containing files of interest 
#' @param extract.file files from the .tar archive to extract
#' @param new.file file to move extracted data files into
#' @param resultsdir where the new.file will be saved
#' @return untarred data file moved to resultsdir/new.file
#' @export
extract.file <- function(tar.file, extract.file, new.file, resultsdir) {
  # get file path to extracted file
  x.file <-
    grep(extract.file,
      untar(tar.file, list = T),
      value = T
    )
    
  # extract the tar file
  cat("Extracting", tar.file, "to", new.file, "\n")
  untar(tar.file, exdir=resultsdir, extras="--no-same-owner")
  x.file = file.path(resultsdir,x.file)

  # move the data to named output
  file.copy(x.file, new.file)

  # remove untared directory
  unlink(dirname(x.file), recursive = TRUE)
}

#' Create a table and save it to a directory
#' @param x dataframe to save 
#' @param filename path and filename to save the dataframe to
#' @return a saved dataframe
#' @export
my.write.table <- function(x, filename) {
  cat("saving", basename(filename), "...\n")
  write.table(x, file = filename, row.names = T, col.names = T, sep = "\t")
}

#' Extract participant ID from TCGA barcode
#' @param id TCGA barcode
#' @return participant ID
#' @export
extract.participant <- function(id) {
  sub("TCGA-[^-]+-([^-]+)-.*", "\\1", id)
}

#' Extract tissue type from TCGA barcode
#' @param id TCGA barcode
#' @return Tissue type
#' @export
extract.tissue <- function(id) {
  sub("TCGA-[^-]+-[^-]+-([0-9]+)[^-]+-.*", "\\1", id)
}

