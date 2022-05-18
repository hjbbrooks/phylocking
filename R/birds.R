#' Limit the dataset by given species and variables
#'
#' This loads the UTFRREC dataset and then selects
#' the rows based on given species, and can also limit columns based on given variables.
#'
#' @param dat A list of species given in scientific name format, common name, or 4 letter
#' bander abbreviation.
#' @param variables Two variables from dataset.
#' @param UTFRREC The dataset.
#' @return A dataframe with the new dataset.
#' @examples
#' dat <- c("CACH", "TUTI", "BLJA", "AMCR")
#' variables <- c("MSF", "Weight_oz")
#' birds(dat, UTFRREC, variables)
#' birds(c("CACH", "TUTI", "BLJA", "AMCR"), UTFRREC)
#' #export
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
