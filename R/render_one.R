#' @title render_one: Render a single .Rmd file, and output to location
#'
#' @param f_prefix Passes the file prefix to the markdown Scotty canon name, e.g. 'A01'
#' @param rmd_path filepath where '.Rmd' files exist, if none specified, assumed working directory
#' @param rend_path filepath to render to
#' @param timestamp default=True, will fix date-time to file name.  
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#' Scotty::render_one('A01', 'CodeFilesPath', paste0('ReportFilesPath'))
#' 
#' @export   
#' @import str_detect  
#' 
render_one <- function(f_prefix, rmd_path, rend_path=NULL, stamp=T) {
  
  if (!exists('rmd_path', inherits=F)) rmd_path <- getwd() 
  
  #get full .Rmd filename to source and knit
  lst_files <- list.files(rmd_path, pattern = ".Rmd", full.names = T)
  f_path <- lst_files[str_detect(lst_files, f_prefix)]
  if (length(f_path)!=1L) stop('More than one file with prefix found')
  
  #get root name only for saving
  f_name <- list.files(rmd_path, pattern = ".Rmd")[stringr::str_detect(list.files(rmd_path, pattern = ".Rmd"), f_prefix)]
  
  #if timestamp wanted, modify filename
  if (stamp) { 
    rend_f_name <- paste0(f_name, '_', str_replace_all(Scotty::timestamp(), '\\.', '')) 
  } else {
    rend_f_name <- paste0(f_name) 
  } 
  rend_f_name <- gsub('.Rmd', '', rend_f_name)
  
  #if html path given, add to filename
  if (!is.null(rend_path)) rend_f_name <- paste0(rend_path, '\\', rend_f_name) 
  
  cat(rend_f_name)
  
  #render with arguments  
  rmarkdown::render(input=f_path,
                    output_file=rend_f_name,
                    params = list(f_prefix = f_prefix),
                    envir=new.env())
}