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
  RenderOne.CodeFiles <- list.files(SrcPath, pattern = ".Rmd", full.names = T)
  RmdPath <- RenderOne.CodeFiles[str_detect(RenderOne.CodeFiles, CodeFileId)]
  
  if (stamp) HtmlPath <- paste0(HtmlPath, '.', timestamp())
  
  render(input=RmdPath,
         output_dir=HtmlPath,
         params = list(NamedId = CodeFileId),
         envir=new.env())
}