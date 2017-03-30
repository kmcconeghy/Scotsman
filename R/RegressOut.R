#' @title RegressOut
#'
#' @description Place Output from lm into excel file
#'
#' @details
#' This function will take the lm R (basic OLS model) class object.
#' It will store/format and analyze results and output these to an excel file.
#' Also will optionally create graphs (diagnostic plots).
#'
#' @param lmObject lm class object
#' @param lmFilePath The filepath and name of file (no extension) to output (Program will add timestamp to end of name)
#' @param exp If a particular variable is of interest, specify here
#' @param plots Specify whether diagnostics are desired
#'
#' @return Excel file with following worksheets:
#' \enumerate{
#'  \item{OLS}{ :Stores regression steps and statistics.}
#'  \item{Param}{ :Stores parameters with SE, p-value, 95 percent CI}
#'  \item{Graphs}{ :Stores regression diagnostics.}
#' }
#'
#' @author Kevin W. McConeghy
#'
#' @note if no filepath specified, will default to working directory with timestamp.
#' If file exists, without replace option R will return error.
#'
#' @examples
#' Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#' Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' weight <- c(ctl, trt)
#' lm.ref <- lm(weight ~ group)
#'
#' RegressOut(lm.ref, Plots, Replace)
#'
#' @export
#'

RegressOut <- function(lmObject, lmFilePath=NULL, exp=NULL, plots=TRUE) {

#Set Time
StartTime <-lubridate::ymd_hms(Sys.time())
TimeStamp <- paste0(".",
                    lubridate::year(StartTime),
                    ".",
                    lubridate::month(StartTime),
                    ".",
                    lubridate::day(StartTime),
                    ".",
                    paste0(lubridate::hour(StartTime),lubridate::minute(StartTime),lubridate::second(StartTime))
                    )

#Quality Checks
  #Test class linear model
  if (is(lmObject,"lm")==FALSE) {paste0("lmObject is not a lm class object!")}
  lm.Results <- summary(lmObject)

#Load necessary packages
  Packages <- c("graphics","xlsx","lubridate","lattice","ggplot2")
  for (i in Packages) {CheckPackage(i)}

#Set-up Excel Workbook Object
  wb <- createWorkbook(type="xlsx")
  #Create worksheets
  OLS <- createSheet(wb,sheetName="OLS")
  Param <- createSheet(wb,sheetName="Param")
  Graphs <- createSheet(wb,sheetName="Graphs")

#Output regression worksheet
  sys <- Sys.info()
  #Output Description
  RegressOut.Title <- "Linear Regression Output"
  RegressOut.Desc=paste("Programmer : Kevin W McConeghy. @:kevinmcconeghy@gmail.com. \n",
             "Date Run: ", StartTime,"\n",
             "System user: ", sys[6],", R version:", getRversion(),"\n",
             "CMD: ", lmObject$call[1]," ",lmObject$call[2], "\n",sep="")

  #Add Desc
  xlsx.AddHeader(wb, OLS, value=RegressOut.Title, StartRow=1, StartCol=1)
  xlsx.AddLineBreak(OLS, 1)
  xlsx.AddParagraph(wb, OLS,value=RegressOut.Desc, isItalic=FALSE, colSpan=5,
                    rowSpan=8, fontSize=14)
  xlsx.AddLineBreak(OLS, 2)


  #Model ANOVA and Results
  model.terms <- paste0(lm.Results$call[2])
  model.resid <- paste0("Residual standard error ", round(lm.Results$sigma,3)," on ",lm.Results$df[2]," degress of freedom")
  model.r_sq <- paste0("Multiple R-squared: ",round(lm.Results$r.squared,3),", ","Adjusted R-squared: ", round(lm.Results$adj.r.squared,3))
  pval <- 1-pf(lm.Results$fstatistic[1],lm.Results$fstatistic[2],lm.Results$fstatistic[3])
  if(pval<=0) {pval <- "<0.001"} else {pval <- round(pval,3)}
  model.fstat <- paste0("F-statistic: ",
                  round(lm.Results$fstatistic[1],3),
                  " on ",lm.Results$fstatistic[2],
                  " and ",lm.Results$fstatistic[3]," DF, ",
                  "p-value: ",pval)

  #Input into excel
  if(length(getRows(OLS))==0) {ResultStartRow=1} else{ResultStartRow=length(getRows(OLS))} #Start of Results
  xlsx.AddHeader(wb, OLS, value="Model Results", StartRow=ResultStartRow, StartCol=1)
  xlsx.AddValue(wb, OLS, value=model.terms, StartRow=ResultStartRow+1, StartCol=1)
  xlsx.AddValue(wb, OLS, value=model.resid, StartRow=ResultStartRow+2, StartCol=1)
  xlsx.AddValue(wb, OLS, value=model.r_sq, StartRow=ResultStartRow+3, StartCol=1)
  xlsx.AddValue(wb, OLS, value=model.fstat, StartRow=ResultStartRow+4, StartCol=1)



#Output parameter worksheet
  lm.Coef <- round(
              cbind(
              data.frame(lm.Results$coefficients),
              data.frame(confint(lm.test)))
              ,3)
  names(lm.Coef)[names(lm.Coef) == 'Std..Error'] <- 'SE'
  names(lm.Coef)[names(lm.Coef) == 'Pr...t..'] <- 'Pr(T)'
  names(lm.Coef)[names(lm.Coef) == 'X2.5..'] <- '95% Lower CI'
  names(lm.Coef)[names(lm.Coef) == 'X97.5..'] <- '95% Upper CI'

  refcols <- c("Estimate", "SE", "95% Lower CI", "95% Upper CI")
  lm.Coef <- lm.Coef[, c(refcols, setdiff(names(lm.Coef), refcols))]

  if(length(getRows(Param))==0) {ResultStartRow=1} else{ResultStartRow=length(getRows(Param))} #Start of Results
  xlsx.AddHeader(wb, Param,value="Estimated Model Parameters", StartRow=ResultStartRow, StartCol=1)
  xlsx.AddTable(wb, Param, data=lm.Coef, startRow=ResultStartRow+1, startCol=1)

#Output graph worksheet
  if(length(getRows(Graphs))==0) {ResultStartRow=1} else{ResultStartRow=length(getRows(Graphs))} #Start of Results

  if(plots==T) {

     #lm.plot
    for (i in 1:6) {
     lm.plot.i <- function() {
        g.i <- plot(lmObject, which=c(i), ask=F)
        }
       xlsx.AddPlot(wb, Graphs, lm.plot.i)
     } #4 standard plots for linear models


     #Add plots to Excel


  }
  else {xlsx.AddHeader(wb, Graphs, value="Not requested", StartRow=ResultStartRow, StartCol=1)}

#Determine file path and/or default
  #Specified filepath
  if(is.null(lmFilePath)==F) {
    lmFilePath2 <- paste0(lmFilePath,TimeStamp,".xlsx")
    saveWorkbook(wb, lmFilePath2)
    shell.exec(lmFilePath2)
  }

  else { #Default filepath
    lmFilePath2 <- paste0(getwd(),"/RegressOut",TimeStamp,".xlsx")
    saveWorkbook(wb, lmFilePath2)
    shell.exec(lmFilePath2)
  } #end of default filepath

}

