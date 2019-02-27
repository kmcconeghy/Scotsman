#' @title render_one: Render a single .Rmd file, and output to location
#'
#' @param CodeFileId file prefix with Scotty canon name, e.g. 'A01'
#' @param SrcPath filepath where '.Rmd' files exist
#' @param HtmPath filepath to render to
#' @param stamp default=True, will fix date-time to file name.  
#'
#' @author Kevin W. McConeghy
#'
#' @examples
#' Scotty::render_one('A01', 'CodeFilesPath', paste0('ReportFilesPath'))
#' 
#' @export
render_one <- function(CodeFileId, SrcPath, HtmlPath, stamp=T) {
  
  #Pull CodeFileId
  lst_files <- list.files(SrcPath, pattern = ".Rmd", full.names = T)
  f_path <- lst_files[str_detect(lst_files, CodeFileId)]
  
  f_name <- list.files(SrcPath, pattern = ".Rmd")[str_detect(list.files(SrcPath, pattern = ".Rmd"), CodeFileId)]
  
  if (stamp) { 
    HtmlPath <- paste0(HtmlPath, f_name, '.', Scotty::timestamp(), '.html') 
  } else {
    HtmlPath <- paste0(HtmlPath, f_name, '.html') 
  } 
  
  rmarkdown::render(input=f_path,
                    output_file=HtmlPath,
                    params = list(NamedId = CodeFileId),
                    envir=new.env())
}