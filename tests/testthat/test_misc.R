context('Test timestamp()')

test_that('timestamp() executes', {
  library('Scotty')
  #library
  test <- Scotty::timestamp()
  testthat::expect_type(test, 'character')
  testthat::expect_equal(length(test), 1L)
  
  
})