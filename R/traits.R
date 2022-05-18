#' Limit the dataset by variables
#'
#' This loads the UTFRREC dataset and then selects
#' the columns based on the given variables.
#'
#' @param variables Two variables from dataset.
#' @param UTFRREC The dataset.
#' @return A dataframe with the new dataset.
#' @examples
#' variables <- c("MSF", "Weight_oz")
#' traits(variables, UTFRREC)
#' traits(c("MSF", "Weight_oz"), UTFRREC)
#' #export
traits <- function(variables, UTFRREC) {
  new_list <- list(
    UTFRREC %>%
      dplyr::select("Species","Common","Bander_abbr", all_of(variables))
  )
  return(as.data.frame(new_list))
}
