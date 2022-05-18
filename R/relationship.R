#' Find the relationship between given variables
#'
#' This selects data based on the given species and variables. The function chooses a model
#' to run depending on the chosen variables. If both variables are discrete, a correlation
#' matrix will be created with corHMM. If both variables are continuous, a correlation matrix
#' will be created with ape. If there is one continuous variable and one discrete variable,
#' a mixed model will be created with phylolm.
#'
#' @param dat A list of species given in scientific name format, common name, or 4 letter
#' bander abbreviation.
#' @param variables Two variables from dataset.
#' @param UTFRREC The dataset.
#' @return Two painted phylogenetic trees.
#' @examples
#' dat <- c("CACH", "TUTI", "BLJA", "AMCR")
#' variables <- c("MSF", "Weight_oz")
#' relationship(dat, variables, UTFRREC)
#' relationship(c("CACH", "TUTI", "BLJA", "AMCR"), c("MSF", "Weight_oz"), UTFRREC)
#' #export
relationship <- function (dat, variables, UTFRREC) {
  corr.tree <- tree(dat, UTFRREC)
  ship.dat <- as.data.frame(birds(dat, UTFRREC, variables))
  ship.traits <- as.data.frame(ship.dat[c(4:5)])
  rownames(ship.traits) <- ship.dat[,1]
  if (class(ship.traits[,1]) == "character" && class(ship.traits[,2]) == "character"){
    make.traits <- corHMM:::rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=2, nstates=(length(unique(new.traits[,2])) + length(unique(ship.traits[,3]))), model="ARD")
    corr.traits <- corHMM::corHMM(corr.tree, ship.traits, rate.cat = 1, rate.mat = make.traits, node.states = "marginal")
  } else if (class(ship.traits[,1]) == "numeric" && class(ship.traits[,2]) == "character"){
    factored <- ship.traits
    factored[,2] <- as.factor(factored[,2])
    corr.mod <- phylolm::phylolm(factored[,1] ~ factored[,2], factored, corr.tree, model =("BM"), boot = 100, full.matrix=TRUE)
    corr.traits <- summary(corr.mod)
  } else if (class(ship.traits[,1]) == "character" && class(ship.traits[,2]) == "numeric"){
    factored <- ship.traits
    factored[,1] <- as.factor(factored[,1])
    corr.mod <- phylolm::phylolm(factored[,2] ~ factored[,1], factored, corr.tree, model =("BM"), boot = 100, full.matrix=TRUE)
    corr.traits <- summary(corr.mod)
  } else if (class(ship.traits[,1]) == "numeric" && class(ship.traits[,2]) == "numeric"){
    corr.traits <- ape::corphylo(ship.traits, phy = corr.tree, method = "Nelder-Mead")
  } else {
    NULL
  }
  return(corr.traits)
}
