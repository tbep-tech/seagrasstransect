test_that("Checking show_compplot", {
  
  dat <- form_trndat(trndat)
  result <- show_compplot(dat, site = '1')
  
  expect_is(result, 'ggplot')
  
  result <- show_compplot(dat, site = '1', varplo = 'Blade Length')
  
  expect_is(result, 'ggplot')
  
})
