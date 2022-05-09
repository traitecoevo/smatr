
data(leaflife)
leaf.low.soilp <-leaflife[leaflife$soilp == 'low',]

# log-transform
leaf.low.soilp$logLMA = log10(leaf.low.soilp$lma)
leaf.low.soilp$logLongev = log10(leaf.low.soilp$longev)

#Create a measurement error matrix
# By making measurement error in lma only, we would expect slope correcting for measurement error to be flatter 


V2 = matrix(c(0.01,0,0,0),2,2)
# V2 = matrix(c(0,
#               0,
#               0,
#               0.1),2,2) # This matrix fails?

V2group = array(c(V2,V2),c(2,2,2)) # in array form 

# var(leaf.low.soilp[,c("logLMA","logLongev")]) #True variances

#No group fits
default_sma <- sma(logLongev~logLMA, data=leaf.low.soilp)
default_sma$coef 

me_sma <- sma(logLongev~logLMA, data=leaf.low.soilp, V = V2)
me_sma$coef

test_that("Single estimations", {
  expect_silent(sma(longev~lma, data=leaf.low.soilp, V = V2))
  expect_true(abs(me_sma$coef[[1]][2,1]) < abs(default_sma$coef[[1]][2,1]))
}) 

# Group fits common elevation
default_gp_sma <- sma(logLongev~logLMA+rain, data=leaf.low.soilp)
default_gp_sma$coef 

me_gp_sma <- sma(logLongev~logLMA+rain, data=leaf.low.soilp, V = V2)
me_gp_sma$coef

test_that("Common elevation group estimations", {
  expect_silent(sma(longev~lma+rain, data=leaf.low.soilp, V = V2))
  expect_true(me_gp_sma$coef[[1]][2,1] != default_gp_sma$coef[[1]][2,1])
  expect_true(me_gp_sma$coef[[2]][2,1] != default_gp_sma$coef[[2]][2,1])
}) 

# Group fits common slope
default_gp_inter_sma <- sma(logLongev~logLMA*rain, data=leaf.low.soilp)
default_gp_inter_sma$coef 

me_gp_inter_sma <- sma(logLongev~logLMA*rain, data=leaf.low.soilp, V = V2)
me_gp_inter_sma$coef

test_that("Common slope group estimations", {
  expect_silent(sma(longev~lma*rain, data=leaf.low.soilp, V = V2))
  expect_true(me_gp_inter_sma$coef[[1]][2,1] != default_gp_inter_sma$coef[[1]][2,1])
  expect_true(me_gp_inter_sma$coef[[2]][2,1] != default_gp_inter_sma$coef[[2]][2,1])
}) 

# Array V matrices
arr_default_sma <- sma(logLongev~logLMA+rain, data=leaf.low.soilp)
arr_default_sma$coef #works

arr_me_sma <- sma(logLongev~logLMA+rain, data=leaf.low.soilp, V = V2group)
arr_me_sma$coef #works

test_that("Array group estimations", {
  expect_silent(sma(logLongev~logLMA+rain, data=leaf.low.soilp, V = V2group))
  expect_length(V2group, length(V2group))
  expect_true(arr_me_sma$coef[[1]][2,1] != arr_default_sma$coef[[1]][2,1])
  expect_true(arr_me_sma$coef[[2]][2,1] != arr_default_sma$coef[[2]][2,1])
}) 

# Cross checking with other functions
# Create high rainfall, low soil ph dataset
highrain.lowsoilp <- leaf.low.soilp[leaf.low.soilp$rain == "high",]

test_that("Values match other functions", {
  expect_equal(me_sma$coef[[1]], 
               line.cis(leaf.low.soilp$logLongev, leaf.low.soilp$logLMA, V = V2))
  expect_equal(me_gp_inter_sma$coef$high,
               line.cis(highrain.lowsoilp$logLongev, highrain.lowsoilp$logLMA, V = V2))
}) 







