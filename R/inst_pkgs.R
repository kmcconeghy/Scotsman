#' @title inst_pkgs
#' 
#' @description Install a standard list of packages
#'
#' @usage inst_pkgs()
#' 
#' @export
#' @examples
#' inst_pkgs()
#'
inst_pkgs <- function() {
  
  pkgs <- c('tidyverse', 
            'data.table', 
            'haven',
            'rcpp',
            'DT',
            'gee')
  
  inst_pkg <- function(package) {
    if(!require(package, character.only = T)) {
      paste0(package," is not installed! installing....")
      install.packages(package)
      }
  }
  
  sapply(pkgs, inst_pkg)
}