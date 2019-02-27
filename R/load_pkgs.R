#' @title load_pkgs
#' @description Check for package and load if found
#'
#' @usage load_pkgs(packages, quietly=F)
#' 
#' @param packages vector list of packages to install
#'
#' @export
#' @examples
#' load_pkgs(c("dplyr", "ggplot2", "tm"))
#'
load_pkgs <- function(packages, quietly=F) {

  CheckPackage <- function(package) {
    if(!require(package, character.only = T, warn.conflicts = F, quietly=quietly)) {
      paste0(package," is not installed!")}
  }

  invisible(sapply(packages, CheckPackage))
  if (quietly==F) (.packages())
}


