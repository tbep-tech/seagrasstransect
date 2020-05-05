test_that("Checking show_compplot", {
  
  dat <- form_traindat(trndat)
  result <- show_compplot(dat)
  
  expect_is(result, 'ggplot')
  
  result <- show_compplot(dat, varplo = 'Blade Length')
  
  expect_is(result, 'ggplot')
  
})
