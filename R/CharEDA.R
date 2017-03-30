#' @title CharEDA
#'
#' @description Exploratory data analysis of a string character var
#'
#' @details
#' This function will take a dataframe variable, provide summary and graphs
#'
#' @param fvar string variable
#'
#' @return Object with generic information, statistics
#'
#' @author Kevin W. McConeghy
#'
#' @note
#'
#' @examples
#' data("acq")
#' @export
#'

require(tm)
require(wordcloud)

CharEDA <-
function (fvar)
{
  df <- data.frame(fvar)
  n <- length(fvar)
  empty <- sum(which(fvar == ""))

  #Frequency Function
  freqfunc <- function(x, n) {
    tail(sort(table(unlist(strsplit(as.character(x), ", ")))),n)
  }

  vsummary <- freqfunc(df$fvar, 100)
  most <- which(vsummary == max(vsummary))
  least <- which(vsummary == min(vsummary))

  #Truncate Strings
  truncString <- function(x, maxlen = 10, justify = "left") {
    toolong <- nzchar(x) > maxlen
    maxwidth <- ifelse(toolong, maxlen - 3, maxlen)
    chopx <- substr(x, 1, maxwidth)
    lenx <- length(x)
    for (i in 1:length(x)) if (toolong[i])
      chopx[i] <- paste(chopx[i], "...", sep = "")
    return(formatC(chopx, width = maxlen, flag = ifelse(justify ==
                                                          "left", "-", " ")))
  }

  df.words <- data.frame(word = truncString(names(vsummary)),
                         freq = as.integer(vsummary))
  pal <- brewer.pal(6, "Dark2")
  pal <- pal[-(1)]
  wordPlot <- wordcloud(df.words$word, df.words$freq,
                        FALSE, TRUE, 0, colors = brewer.pal(8, "RdYlGn"))
  var.result <- list(n, empty, most, least, wordPlot)
  return(var.result)
}
