#' @title xlsx_break
#'
#' @description Several commands to be used with xlsx package
#'
#' @details Adds a row between input commands to an excel object from xlsx package
#'
#' @param sheet A sheet class object created by xlsx package (see example)
#' @param numberOfLine Number of linebreaks
#'
#' @author Kevin W. McConeghy
#'
#' @export

xlsx_break<-function(sheet, numberOfLine=1) {
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
