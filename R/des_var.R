#' @title des_var
#'
#' @description Generate an object with html table of var statistics
#'
#' @details
#' This function will take a variable, provide summary statistics, distributions checks, graphs
#'
#' @param x object
#'
#' @return List with HtmlTable, var statistics
#'
#' @author Kevin W. McConeghy
#'
#' @note Excepts var class integer, numeric
#'
#' @examples
#'
#' @export
#'

des_var <- function(x) {

  #x <- cars$speed
  var.class <- class(x)
  var.unique <- length(unique(x))

  #String
    if (var.class == "character" & var.unique >10) { result <- stringEDA(x)}
    if (var.class == "factor" & var.unique >10) { result <- stringEDA(x)}

  #Categorical
    if (var.unique <=10 & var.unique>2) { result <- catEDA(x)}
    if (var.unique <=10 & var.unique>2) { result <- catEDA(x)}

  #Binary
    if (var.unique ==2) { result <- binEDA(x)}

  #Numeric >10 unique values
    if (var.class == "integer" || var.class == "numeric") {
      if (var.unique>10) { result <- NumEDA(x)}
    }

  #Pro
  return(result)

}

