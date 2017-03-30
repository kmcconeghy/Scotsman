#' @title load.project
#'
#' @description Initiate Project Commands
#'
#' @param FilePath Root folder where insheet etc. located
#' @param INSHEET logic indicator for which files to run
#' @param MUNGE logic indicator for which files to run
#' @param ANALYZE logic indicator for which files to run
#' @param REPORT logic indicator for which files to run
#'
#' @export
#' @examples
#'
#' FilePath <- 'mydir'
#' load.project(FilePath,INSHEET=TRUE) #Run Insheet only
#'
#'

load.project <- function (FilePath, INSHEET = FALSE, MUNGE = FALSE, ANALYZE = FALSE,
          REPORT = FALSE)
{
  if (INSHEET == TRUE) {
    source(paste0(FilePath, "_01_Insheet.R"),echo=T, print.eval=T)
  }
  if (MUNGE == TRUE) {
    source(paste0(FilePath, "_02_Munge.R"),echo=T, print.eval=T)
  }
  if (ANALYZE == TRUE) {
    source(paste0(FilePath, "_03_Analyze.R"),echo=T, print.eval=T)
  }
  if (REPORT == TRUE) {
    source(paste0(FilePath, "_04_Report.R"),echo=T, print.eval=T)
  }
}
