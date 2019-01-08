#' @title load_prj
#'
#' @description Initiate project commands in Scotty canon project, this program will search the SrcPath 
#' directory for files which conform to the nomenclature, 'A[0-9][0-9]', 'B[0-9][0-9]', 
#' 'C[0-9][0-9]', 'D[0-9][0-9]'
#'
#' @param INSHEET logical for which files to run
#' @param MUNGE logical for which files to run
#' @param REPORT logical for which files to run
#' @param SrcPath Path whether markdown files exist
#' @param OutPath Path to place rendered files  
#' 
#' @export
#' @examples
#'
#' FilePath <- 'mydir'
#' CodePath <- 'CodeDir'
#' load_prj(INSHEET=TRUE, SrcPath=CodePath, ReportPath = FilePath) #Run Insheet only
#'
#'

load_prj <- function (INSHEET = F, MUNGE = F, REPORT = F,
                      SrcPath, ReportPath) {
  #Generate list of code files
    AllSrcFiles <- list.files(SrcPath, pattern='.Rmd', full.names = T)
  
  if (INSHEET == T) {
    SrcFiles <- AllSrcFiles[str_detect(AllSrcFiles, 'A[0-9][0-9]')]
    render2(i, ReportPath)
  }
  if (MUNGE == T) {
    SrcFiles <- AllSrcFiles[str_detect(AllSrcFiles, 'B[0-9][0-9]')]
    render2(i, ReportPath)
    }
  if (REPORT == T) {
    SrcFiles <- AllSrcFiles[str_detect(AllSrcFiles, 'C[0-9][0-9]')]
    render2(i, ReportPath)
    }
}
