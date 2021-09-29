data(leaflife)
leaf.low.soilp <-leaflife %>% dplyr::filter(soilp == 'low')
V2 = matrix(c(0.01,0,0.0,0.01),2,2)

#Most simple fit
default_sma <- sma(longev~lma, data=leaf.low.soilp)
measure_error_sma <- sma(longev~lma, data=leaf.low.soilp, V = V2)

test_that("Functions accounts for measurement error", {
  expect_silent(sma(longev~lma, data=leaf.low.soilp, V = V2))
  expect_true(measure_error_sma$coef[[1]][1,1] != default_sma$coef[[1]][1,1])
  expect_true(measure_error_sma$coef[[1]][2,1] != default_sma$coef[[1]][2,1])
  expect_vector(all.equal(measure_error_sma$coef, default_sma$coef))
}) 


