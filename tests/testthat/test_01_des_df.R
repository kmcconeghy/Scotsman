context('Test dataframe descriptor')

test_that('des_df executes', {
  
  expect_output(des_df(mtcars, 'Test file'))
  
})