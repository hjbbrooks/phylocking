#' Visualize the variables given with painted trees
#'
#' This selects data based on the given species and variables. The function will result in two
#' painted phylogenetic trees that correspond with the variables given. Discrete variables
#' require placing the legend.
#'
#' @param dat A list of species given in scientific name format, common name, or 4 letter
#' bander abbreviation.
#' @param variables Two variables from dataset.
#' @param UTFRREC The dataset.
#' @return Two painted phylogenetic trees.
#' @examples
#' dat <- c("CACH", "TUTI", "BLJA", "AMCR")
#' variables <- c("MSF", "Weight_oz")
#' paint(dat, variables, UTFRREC)
#' paint(c("CACH", "TUTI", "BLJA", "AMCR"), c("MSF", "Weight_oz"), UTFRREC)
#' #export
paint <- function (dat, variables, UTFRREC){
  plot.dat <- as.data.frame(birds(dat, UTFRREC, variables))
  new.traits <- as.data.frame(plot.dat[c(4:5)])
  rownames(new.traits) <- plot.dat[,1]
  paint.tree <- tree(dat, UTFRREC)
  for(i in colnames(new.traits)) {
    if(class(new.traits[,i]) == "character"){
      trait <- new.traits[,i]
      names(trait) <- plot.dat[,1]
      print(trait)
      painted.tree <- phytools::plotSimmap(phytools::make.simmap(paint.tree, trait), pts=FALSE, fsize=1)
      coloring <- stats::setNames(palette()[1:length(unique(trait))],sort(unique(trait)))
      painted.tree <- phytools::add.simmap.legend(colors = coloring)
    } else {
      trait <- new.traits[,i]
      names(trait) <- plot.dat[,1]
      cont.tree <- phytools::contMap(paint.tree, trait, res=100, fsize=0.8, legend=TRUE)
      painted.tree <- plot(cont.tree, legend = TRUE)
    }
  }
  return(painted.tree)
}
