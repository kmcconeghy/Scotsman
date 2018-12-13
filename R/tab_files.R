#' @title tab_files
#'
#' @description Helper function, takes Scotty canon list of files and
#' outputs an HTML DT table, for use with rmarkdown.  
#'
#' @export
#' @examples
#' tab_files()
#' 
tab_files <- function(files) {
  t(files) %>%
  as_tibble(.) %>%
  dplyr::filter(!is.na(1) | !is.na(2)) %>%
  DT::datatable(., options=list(pageLength=10))
}