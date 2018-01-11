#' @title xlsx_paragph
#'
#' @description Several commands to be used with xlsx package
#'
#' @details
#' Package contains xlsx_paragph,
#'
#' @param wb Workbook object
#' @param sheet A sheet class object created by xlsx package (see example)
#' @param value Input to be placed into cell
#' @param fontColor Color font
#' @param fontSize Excel font size
#' @param backGroundColor Excel background color
#' @param isBold logical indicating if font is bold or not
#' @param isItalic logical indicating italic font
#' @param startRow integer indicating excel row to insert
#' @param startCol integer indicating excel column to insert
#' @param colSpan Number of columns to paste across
#' @param rowSpan Number of rows to paste down
#'
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#'
#' @export
#'
#'
xlsx_paragph<-function(wb,sheet, value, fontColor="#FFFFFF", fontSize=12, backGroundColor="#FFFFFF",
                            isBold=FALSE, isItalic=FALSE,
                            startRow=NULL, startCol=1, colSpan=10, rowSpan=6)
{
  library("xlsx")
  #Append table to sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1
  }
  rows <- createRow(sheet,rowIndex=startRow)
  sheetParagraph <- createCell(rows, colIndex=startCol)
  setCellValue(sheetParagraph[[1,1]], value)
  #style
  PARAGRAPH_STYLE <- CellStyle(wb)+
    Font(wb,  heightInPoints=fontSize,color=fontColor, isBold=isBold, isItalic=isItalic)+
    Alignment(wrapText=TRUE, horizontal="ALIGN_JUSTIFY",
              vertical="VERTICAL_CENTER")
  #background fill
  if(!backGroundColor %in% c("white", "#FFFFFF"))
    PARAGRAPH_STYLE+Fill(backgroundColor=backGroundColor, foregroundColor=backGroundColor)
  xlsx::setCellStyle(sheetParagraph[[1,1]], PARAGRAPH_STYLE)
  #Spanning region : -1, because we start to count from zero.
  #if not, an additionnal row or column are added to merged region
  addMergedRegion(sheet, startRow, endRow=startRow+rowSpan-1, startCol, endColumn=startCol+colSpan-1)
  xlsx.AddLineBreak(sheet, rowSpan)
}
