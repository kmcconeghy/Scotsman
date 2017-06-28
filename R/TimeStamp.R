#' @title TimeStamp
#'
#' @description Returns a length=1 character vector
#'
#' @export
#' @examples
#' TimeStamp()
#' 

TimeStamp <- function () {
  TimeStamp <- paste0(sprintf("%04d", lubridate::year(Sys.Date())), ".", 
                      sprintf("%02d", lubridate::month(Sys.Date())), ".", 
                      sprintf("%02d", lubridate::day(Sys.Date())), ".", 
                      sprintf("%02d", lubridate::hour(Sys.time())), 
                      sprintf("%02d", lubridate::minute(Sys.time()))
                      )
  return(TimeStamp)
}