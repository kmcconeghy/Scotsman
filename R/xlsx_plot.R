#' @title xlsx_plot
#'
#' @description Several commands to be used with xlsx package
#'
#' @details Function for adding plot to a excel sheet using xlsx package
#'
#' @param wb Workbook class object
#' @param sheet Sheet class object
#' @param plotFunction Plot function to execute with return value plot
#' @param startRow integer for row to insert at
#' @param startCol integer for column to insert at
#' @param width Figure width
#' @param height Figure height
#' @param ... other arguments to pass on to png()
#'
#' @author Kevin W. McConeghy
#'
#' @export
xlsx_plot<-function( wb, sheet, plotFunction, startRow=NULL,startCol=1,
                        width=480, height=480,... ) {
  png(filename = "plot.png", width = width, height = height,...)
  plotFunction()
  dev.off()
  #Append plot to the sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1
  }
  # Add the file created previously
  addPicture("plot.png", sheet=sheet,  startRow = startRow, startColumn = startCol)
  xlsx_break(sheet, round(width/20)+1)
  res<-file.remove("plot.png")
}
