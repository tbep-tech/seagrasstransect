test_that("Checking read_trndat class", {
  
  pth <- 'Class_Data_20190627.xlsx'
  result <- read_trndat(pth)
  
  expect_is(result, 'tbl_df')
  
})
