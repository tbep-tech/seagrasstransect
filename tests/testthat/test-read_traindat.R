test_that("Checking read_traindat class", {
  
  pth <- 'Class_Data_20190627.xlsx'
  result <- read_traindat(pth)
  
  expect_is(result, 'tbl_df')
  
})
