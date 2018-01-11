#' @title xlsx.AddLineBreak
#'
#' @description Several commands to be used with xlsx package
#'
#' @details
#'
#' @param sheet A sheet class object created by xlsx package (see example)
#' @param numberOfLine Number of linebreaks
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#'
#' @export
#'

xlsx.AddLineBreak<-function(sheet, numberOfLine=1)
{
  library("xlsx")

  nrows<-length(getRows(sheet)) #list of row object
  startRow=nrows
  for(i in 1:numberOfLine){
    #Append row to sheet
    startRow=startRow+1
    # Create the Sheet title and subtitle
    rows <- createRow(sheet,rowIndex=startRow)
    sheetLineBreak <- createCell(rows, colIndex=1)
    setCellValue(sheetLineBreak[[1,1]], "  ")
  }
}
