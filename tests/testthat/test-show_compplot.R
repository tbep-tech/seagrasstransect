test_that("Checking show_compplot", {
  
  trnjsn <- read_trnjsn()
  dat<- form_trnjsn(trnjsn)
  
  result <- show_compplot(dat, site = '1')
  
  expect_is(result, 'ggplot')
  
  result <- show_compplot(dat, site = '1', varplo = 'Blade Length')
  
  expect_is(result, 'ggplot')
  
})
