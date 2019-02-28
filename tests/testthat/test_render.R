context('Test render_one()')


library(Scotty)
library(rmarkdown)

dir <- getwd()

test_that('render_one() executes', {
  #library
  render_one('A01', dir)
  
  lst_files <- list.files(test_fpath, pattern = ".html", full.names = T)
  file.remove(lst_files)

})