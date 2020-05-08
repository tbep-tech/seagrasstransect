test_that("Checking form_trndat", {
  
  result <- form_trndat(trndat)
  
  expect_is(result, 'tbl_df')
  
})
