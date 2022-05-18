#Workspace

# A function to choose variables only
traits <- function(variables, UTFRREC) {
  new_list <- list(
    UTFRREC %>%
      dplyr::select("Species","Common","Bander_abbr", all_of(variables))
  )
  return(as.data.frame(new_list))
}

# A function to subset from the bigger data set
birds <- function(dat, UTFRREC, variables = NULL) {
  if(is.null(variables)){
    return_list <- list(
      UTFRREC[UTFRREC$Bander_abbr %in% c(dat),],
      UTFRREC[UTFRREC$Species %in% c(dat),],
      UTFRREC[UTFRREC$Common %in% c(dat),]
    )
    return(as.data.frame(return_list[[which.max(sapply(return_list, nrow))]]))
  }
  else {
    df <- as.data.frame(traits(variables, UTFRREC))
    return_list <- list(
      df[df$Bander_abbr %in% c(dat),],
      df[df$Species %in% c(dat),],
      df[df$Common %in% c(dat),]
    )
    return(as.data.frame(return_list[[which.max(sapply(return_list, nrow))]]))
  }
}


# A function to create a tree
tree <- function(dat, UTFRREC){
  species <- birds(dat, UTFRREC)$Species
  match.tree <- rotl::tnrs_match_names(species)$ott_id
  make.tree <- rotl::tol_induced_subtree(ott_id=match.tree)
  make.tree$tip.label <- rotl::strip_ott_ids(make.tree$tip.label, remove_underscores = TRUE)
  est.tree <- ape::compute.brlen(make.tree)
  ape::plot.phylo(est.tree, cex=0.8)
  return(est.tree)
}

# A function to paint the tree - should make legend, but can wait if necessary
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
      cont.tree <- phytools::contMap(corr.tree, trait, res=100, fsize=0.8, legend=TRUE)
      painted.tree <- plot(cont.tree, legend = TRUE)
    }
  }
  return(painted.tree)
}

# A function to correlate the variables
relationship <- function (dat, variables, UTFRREC) {
  corr.tree <- tree(dat, UTFRREC)
  ship.dat <- as.data.frame(birds(dat, UTFRREC, variables))
  ship.traits <- as.data.frame(ship.dat[c(4:5)])
  rownames(ship.traits) <- ship.dat[,1]
  if (class(ship.traits[,1]) == "character" && class(ship.traits[,2]) == "character"){
    make.traits <- corHMM:::rate.mat.maker(rate.cat=1, hrm=FALSE, ntraits=2, nstates = (length(unique(ship.traits[,1])) + length(unique(ship.traits[,2]))), model="ARD")
    corr.traits <- corHMM::corHMM(corr.tree, ship.traits[,c(1,2)], rate.cat = 1, rate.mat = make.traits, node.states = "marginal")
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

# A function that does all the things
phylocking <- function (dat, variables, UTFRREC){
  limited.dat <- birds(dat, UTFRREC)
  plain.tree <- tree(dat, UTFRREC)
  paint.tree <- paint(dat, variables, UTFRREC)
  correl.dat <- relationship(dat, variables, UTFRREC)
  return(correl.dat)
}

dat <- c("CACH", "WBNU", "TUTI", "GCKI", "YBSA", "DOWO", "HAWO", "PIWO", "AMCR", "CARW", "BLJA", "SSHA", "TUVU", "NOCA", "RBWO", "WITU")
variables <- c("MSF", "Habitat")
phylocking(dat, variables, UTFRREC)

#based on this output, it looks like these states (mixed species flocking and habitat) are not very correlated. The painted trees are interesting, they are only simulated so should be taken lightly, but they predict much of the ancestral states included mixed-species flocking. The habitat painted tree is much patchier and the simulation seems to think that the habitats have changed often. It is not likely real, but biologically it could make sense that the species could fluctuate between denser forest and more open woodlands.
