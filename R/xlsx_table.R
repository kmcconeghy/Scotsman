#' @title xlsx_table
#'
#' @description Several commands to be used with xlsx package
#'
#' @details
#' Package contains xlsx_table,
#'
#' @param wb Workbook object
#' @param sheet A sheet class object created by xlsx package (see example)
#' @param data Dataframe class object
#' @param startRow excel file starting row number (top left of table)
#' @param startCol excel file starting col number (top left of table)
#' @param col.names logical for using column names
#' @param row.names logical for using row names
#' @param columnWidth Width of column cells
#' @param fontColor Excel font color
#' @param fontSize Excel font size
#' @param rownamesFill color for font background
#' @param colnamesFill color for font background
#' @param rowFill color for cell background in rows
#'
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#'
#' @export
#'
#'
xlsx_table<-function(wb, sheet, data, startRow=NULL,startCol=1,
                        col.names=TRUE, row.names=TRUE, columnWidth=14,
                        fontColor="#FFFFFF", fontSize=12,
                        rownamesFill="white", colnamesFill="white",
                        rowFill=c("white", "white")){

  #++++++++++++++++++++++++++++++++++++++
  #Define table style
  #++++++++++++++++++++++++++++++++++++++
  #***Border position and pen value*****
  #Border(color="black", position="BOTTOM", pen="BORDER_THIN"
  #position :  Valid values are "BOTTOM", "LEFT", "TOP", "RIGHT"
  # pen : valid values are BORDER_DASH_DOT,BORDER_DASH_DOT_DOT,BORDER_DASHED,BORDER_DOTTED,BORDER_DOUBLE,BORDER_HAIR,BORDER_MEDIUM,BORDER_MEDIUM_DASH_DOT,BORDER_MEDIUM_DASH_DOT_DOT,BORDER_MEDIUM_DASHED,BORDER_NONE,BORDER_SLANTED_DASH_DOT,BORDER_THICK,BORDER_THIN
  #***Alignement value*****
  #Alignment(horizontal=NULL, vertical=NULL, wrapText=FALSE, rotation=0, indent=0)
  #HALIGN_STYLES_: "ALIGN_CENTER, ALIGN_JUSTIFY, ALIGN_LEFT, ALIGN_RIGHT"
  #VALIGN_STYLES_: "VERTICAL_BOTTOM, VERTICAL_CENTER, VERTICAL_JUSTIFY, VERTICAL_TOP"
  #Alignement :
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE, color=fontColor,
                                               heightInPoints=fontSize)
  #rownames fill
  if(rownamesFill!="white") {
    TABLE_ROWNAMES_STYLE <-TABLE_ROWNAMES_STYLE+
      Fill(foregroundColor = rownamesFill,
           backgroundColor=rownamesFill)
  }


  TABLE_COLNAMES_STYLE <- CellStyle(wb) +
    Font(wb, isBold=TRUE, color=fontColor, heightInPoints=fontSize) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK"))
  #colnames fill
  if(colnamesFill!="white") {
    TABLE_COLNAMES_STYLE <-TABLE_COLNAMES_STYLE+
      Fill(foregroundColor = colnamesFill,
           backgroundColor=colnamesFill)
  }

  #Append table to sheet
  #get current active row of sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1
  }

  #font color
  col.n=ncol(data)
  column.style=NULL
  for(i in 1:col.n){
    column.style[[i]]=CellStyle(wb, font=Font(wb, color=fontColor, heightInPoints=fontSize))
  }
  names(column.style)<-as.character(1:ncol(data))

  # Add the table  to the sheet
  addDataFrame(data, sheet, startRow=startRow, startColumn=startCol,
               col.names=col.names, row.names=row.names,
               colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle=TABLE_ROWNAMES_STYLE,
               colStyle=column.style)
  #Column width
  #+++++++++++++++++++++++++++++++++++++++
  colIndex=1:(ncol(data)+startCol)
  xlsx::setColumnWidth(sheet, colIndex=colIndex, colWidth=columnWidth)

  #Table styling
  #+++++++++++++++++++++++++++++++++++++++
  if(!all(rowFill==c("white", "white"))){
    col.n =ncol(data)
    row.n=nrow(data)
    if(col.names==TRUE) col.n<-col.n+1
    if(row.names==TRUE) row.n<-row.n+1
    cb<-CellBlock(sheet, startRow=startRow, startColumn=startCol,
                  noRows=row.n, noColumns=col.n, create=FALSE )
    #color pair row for styling
    for(i in 1: nrow(data)){
      if(i%%2==0) CB.setFill( cb, fill=Fill(foregroundColor = rowFill[2], backgroundColor=rowFill[2]),
                              rowIndex=i, colIndex=1:col.n)
      else CB.setFill( cb, fill=Fill(foregroundColor = rowFill[1], backgroundColor=rowFill[1]),
                       rowIndex=i, colIndex=1:col.n)
    }

  }
}
