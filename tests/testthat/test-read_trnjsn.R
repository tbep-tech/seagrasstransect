test_that("Checking read_trnjsn class", {
  
  result <- read_trnjsn()
  
  expect_is(result, 'data.frame')
  
})
