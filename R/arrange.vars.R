#' @title arrange.vars
#'
#' @description Rearranges columns in a dataframe 
#' 
#' @param data Dataframe class object
#' @param vars character vector that identifies position of each variable
#'
#' @return Dataframe class object
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#' data(iris)
#' #Put Species as first column
#' head(arrange.vars(iris, c('Species'=1)))
#' 
#' @export
#'
arrange.vars <- function(data, vars) {
  
  stopifnot(is.data.frame(data))
  
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  
  stopifnot( !any(duplicated(var.nms)),
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms),
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos >0),
             all(var.pos <= var.nr) )
  
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[!(data.nms %in% var.nms)]
  stopifnot( length(out.vec)==var.nr)
  
  data <- data[, out.vec]
  return(data)
}