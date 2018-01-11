#' @title xlsx.AddHeader
#'
#' @description Several commands to be used with xlsx package
#'
#' @details
#' Package contains xlsx.AddHeader,
#'
#' @param wb Workbook object
#' @param sheet A sheet class object created by xlsx package (see example)
#' @param value Input to be placed into cell
#' @param StartRow excel file starting row number
#' @param StartCol excel file starting col number
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#'
#' @export
#'

xlsx.AddHeader<-function(wb, sheet, value="Header",StartRow=NULL, StartCol=1)
{
  library("xlsx")

  # Define some cell styles within that workbook
  H_STYLE <- CellStyle(wb) + Font(wb, heightInPoints=16, color="white", isItalic=FALSE, isBold=TRUE, underline=0)

  #Append row to sheet
  if(is.null(StartRow)){
    rows<- getRows(sheet) #list of row object
    StartRow=length(rows)+1
  }

  # Create the Sheet title and subtitle
  rows <- createRow(sheet,rowIndex=StartRow)
  sheetTitle <- createCell(rows, colIndex=StartCol)
  setCellValue(sheetTitle[[1,1]], value)
  xlsx::setCellStyle(sheetTitle[[1,1]], H_STYLE)
}
