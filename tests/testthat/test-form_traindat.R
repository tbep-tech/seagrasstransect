test_that("Checking form_traindat", {
  
  result <- form_traindat(trndat)
  
  expect_is(result, 'tbl_df')
  
})
