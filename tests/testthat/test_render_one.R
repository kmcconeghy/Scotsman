context('Test render_one()')

test_that('render_one() creates file', {
  library(tidyverse)
  library(here)
  
  #library
  render_one(f_prefix='A01', rmd_path = here::here('tests', 'testthat'))
  
  ls_files <- list.files(here::here('tests', 'testthat'))
  
  rm_file <- ls_files[str_detect(ls_files, 'A01_render_[0-9]')]

  file.remove(here::here('tests', 'testthat', rm_file))
})