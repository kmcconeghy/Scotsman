#' Check for package and load if found
#'
#' @param packages vector list of packages to install
#'
#' @export
#' @examples
#' load.packages("plyr")
#'


load.packages <- function(packages, quietly=F) {

  CheckPackage <- function(package) {
    if(!require(package, character.only = T)) {
      paste0(package," is not installed!")}
  }

  invisible(sapply(packages, CheckPackage))
  if (quietly=F) (.packages())
}


