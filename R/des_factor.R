#' @title des_factor
#'
#' @description Exploratory data analysis of a Factor variable
#'
#' @details
#' This function will take a dataframe, provide summary statistics graphs
#'
#' @param fvar dataframe object
#'
#' @return Object with generical information, statistics, ggplot2 plots
#'
#' @author Kevin W. McConeghy
#'
#' @note if no filepath specified, will default to working directory with timestamp.
#'
#' @examples
#'
#' @export
#'
#'
des_factor <- function(fvar) {
  df <- data.frame(fvar)
  n <- length(fvar)
  empty <- sum(which(fvar==''))

  #nmiss <- sum(fvar)==""
  var_nonmiss <- fvar[!is.na(fvar)]

  #Levels
  vlevels <- levels(fvar)

  #summary
  vsummary <- summary(fvar)
  freq <- data.frame(summary(fvar))
  most <- which(vsummary==max(vsummary))
  least <- which(vsummary==min(vsummary))

  freqplot <- ggplot(df, aes(x = fvar)) + geom_bar()

  var.result <- list(n,empty,most,least,vlevels,freq,freqplot)
  return(var.result)
}
