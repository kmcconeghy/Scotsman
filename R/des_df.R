#' @title des_df: Simple description of dataframe
#'
#' @description Reports on dataset size, row counts, unique variables. Requires dplyr.
#'
#' @param df dataframe class object
#' @param dfName character vector of length 1, title of dataset
#' @param VarList character vector of variable names which will be checked for unique values
#' @param CheckMiss default=True, will report number of NA values
#'
#' @return Prints report
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#' data(iris)
#' des_df(iris, 'Flower dataset', c('Petal.Length'))
#' 
#' @export
#'
des_df <- function(df, dfName='', VarList=NA, CheckMiss=T) {

  #Sanity Checks
  stopifnot(is.data.frame(df))

  #Stats
  DfMemSize <- format(object.size(df), units = 'Mb', digits=0)
  DfStamp <- ""
  DfStamp <- paste0(DfStamp, 'Dataframe: ', dfName, sep='\n')
  DfStamp <- paste0(DfStamp, 'Memory Size: ', prettyNum(DfMemSize, big.mark = ','), sep='  ')
  DfStamp <- paste0(DfStamp, 'Rows: ', prettyNum(nrow(df), big.mark = ','), sep='  ')
  DfStamp <- paste0(DfStamp, 'Columns: ', prettyNum(ncol(df), big.mark = ','), sep='\n')
  
  if (!is.na(VarList[1])) {
    for (i in VarList) {
      if (CheckMiss==F) {
        DfStamp <- paste0(DfStamp, i ,': ', prettyNum(n_distinct(df[,i]), big.mark = ','), sep='\n')
      } else {
        DfStamp <- paste0(DfStamp, i ,': ', prettyNum(n_distinct(df[,i]), big.mark = ','), 
                          ' ', 'Missing: ', sum(is.na(df[,i])), sep='\n')
        }
      }
  }
  return(cat(DfStamp))
}