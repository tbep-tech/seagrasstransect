test_that("Checking form_trnjsn", {
  
  trnjsn <- read_trnjsn()
  result <- form_trnjsn(trnjsn)
  
  expect_is(result, 'tbl_df')
  
})
