#' Create a phylogenetic tree based on given species
#'
#' This makes a tree for the given species with the Open Tree of Life simulated tree. There are
#' also estimated branch lengths for the tree.
#'
#' @param dat A list of species given in scientific name format, common name, or 4 letter
#' bander abbreviation.
#' @param UTFRREC The dataset.
#' @return A plot with a phylogenetic tree.
#' @examples
#' dat <- c("CACH", "TUTI", "BLJA", "AMCR")
#' tree(dat, UTFRREC)
#' tree(c("CACH", "TUTI", "BLJA", "AMCR"), UTFRREC)
#' #export
tree <- function(dat, UTFRREC){
  species <- birds(dat, UTFRREC)$Species
  match.tree <- rotl::tnrs_match_names(species)$ott_id
  make.tree <- rotl::tol_induced_subtree(ott_id=match.tree)
  make.tree$tip.label <- rotl::strip_ott_ids(make.tree$tip.label, remove_underscores = TRUE)
  est.tree <- ape::compute.brlen(make.tree)
  ape::plot.phylo(est.tree, cex=0.8)
  return(est.tree)
}
