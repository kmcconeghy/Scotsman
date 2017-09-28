#' @title load.project
#'
#' @description Initiate project commands, this program will search the SrcPath directory 
#' for files which conform to the nomenclature, A[0-9][0-9], B[0-9][0-9], C[0-9][0-9], D[0-9]
#'
#' @param FilePath Root folder where insheet etc. located
#' @param INSHEET logic indicator for which files to run
#' @param MUNGE logic indicator for which files to run
#' @param ANALYZE logic indicator for which files to run
#' @param REPORT logic indicator for which files to run
#' @param SrcPath Path whether code files exist
#' @param ReportPath Path to place log files  
#' 
#' @export
#' @examples
#'
#' FilePath <- 'mydir'
#' CodePath <- 'CodeDir'
#' load.project(FilePath,INSHEET=TRUE) #Run Insheet only
#'
#'

load.project <- function (FilePath, 
                          INSHEET = F, MUNGE = F, ANALYZE = F, REPORT = F,
                          SrcPath, ReportPath) {
  #Generate list of code files
    AllSrcFiles <- list.files(SrcPath, pattern='.Rmd', full.names = T)
  
  if (INSHEET == T) {
    SrcFiles <- AllSrcFiles[str_detect(AllSrcFiles, 'A[0-9][0-9]')]
    HtmlPath <- paste0(ReportPath, 'Insheet\\')
    render2(i, HtmlPath)
  }
  if (MUNGE == T) {
    SrcFiles <- AllSrcFiles[str_detect(AllSrcFiles, 'B[0-9][0-9]')]
    HtmlPath <- paste0(ReportPath, 'Munge\\')
    render2(i, HtmlPath)  
    }
  if (ANALYZE == T) {
    SrcFiles <- AllSrcFiles[str_detect(AllSrcFiles, 'C[0-9][0-9]')]
    HtmlPath <- paste0(ReportPath, 'Analyze\\')
    render2(i, HtmlPath)  
    }
  if (REPORT == T) {
    SrcFiles <- AllSrcFiles[str_detect(AllSrcFiles, 'D[0-9][0-9]')]
    HtmlPath <- paste0(ReportPath, 'Report\\')
    render2(i, HtmlPath)  
    }
}
