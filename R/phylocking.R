#' Select data, create trees, and find relationships between variables
#'
#' This function selects data based on the given species and variables. The function will
#' create a plain phylogenetic tree, two painted trees, and will provide corrlelation or model
#' output for the given variables.
#'
#' @param dat A list of species given in scientific name format, common name, or 4 letter
#' bander abbreviation.
#' @param variables Two variables from dataset.
#' @param UTFRREC The dataset.
#' @return Two painted phylogenetic trees.
#' @examples
#' dat <- c("CACH", "TUTI", "BLJA", "AMCR")
#' variables <- c("MSF", "Weight_oz")
#' phylocking(dat, variables, UTFRREC)
#' phylocking(c("CACH", "TUTI", "BLJA", "AMCR"), c("MSF", "Weight_oz"), UTFRREC)
#' #export
phylocking <- function (dat, variables, UTFRREC){
  limited.dat <- birds(dat, UTFRREC)
  plain.tree <- tree(dat, UTFRREC)
  paint.tree <- paint(dat, variables, UTFRREC)
  correl.dat <- relationship(dat, variables, UTFRREC)
  output <- print(correl.dat)
  return(output)
}

