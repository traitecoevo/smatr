
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

# Cross checking with other functions #Isaac Towers inspired
# Create high rainfall, low soil ph dataset
highrain.lowsoilp <- leaf.low.soilp[leaf.low.soilp$rain == "high",]

test_that("Values match other functions", {
  expect_equal(me_sma$coef[[1]], 
               line.cis(leaf.low.soilp$logLongev, leaf.low.soilp$logLMA, V = V2))
  expect_equal(me_gp_inter_sma$coef$high,
               line.cis(highrain.lowsoilp$logLongev, highrain.lowsoilp$logLMA, V = V2))
}) 


# Including elevation test with ME #Robert Colwell inspired

rb_default <- sma(logLongev~logLMA, type = "elevation", data=leaf.low.soilp)
rb_me <- sma(logLongev~logLMA, type = "elevation", data=leaf.low.soilp, V=V2)

test_that("Values are different", {
  expect_true(rb_me$coef[[1]][2,1] != rb_default$coef[[1]][2,1])
  expect_true(me_gp_inter_sma$coef[[1]][2,1] == coef.sma(me_gp_inter_sma)[1,2])
}) 

leaflife$logLMA=log10(leaflife$lma) 
leaflife$logLongev=log10(leaflife$longev) 

#Adding error to the y variable (logLongev)
ftMeasY=sma(logLongev~logLMA*site,data=leaflife,V=matrix(c(0.01,0,0,0),ncol=2)) 
ftZero=sma(logLongev~logLMA*site,data=leaflife,V=matrix(c(0,0,0,0),ncol=2)) 
ftMeasY$coef 
ftZero$coef 
#ftMEasY should have flatter slopes than all of ftZero 

expect_lt(ftMeasY$coef$`1`[2,1], ftZero$coef$`1`[2,1])
expect_lt(ftMeasY$coef$`2`[2,1], ftZero$coef$`2`[2,1])
expect_lt(ftMeasY$coef$`3`[2,1], ftZero$coef$`3`[2,1])
expect_lt(ftMeasY$coef$`4`[2,1], ftZero$coef$`4`[2,1])

#Adding error to the x variable (logLMA)
ftMeasX=sma(logLongev~logLMA*site,data=leaflife,V=matrix(c(0,0,0,0.002),ncol=2)) 
ftZero=sma(logLongev~logLMA*site,data=leaflife,V=matrix(c(0,0,0,0),ncol=2)) 
ftMeasX$coef 
ftZero$coef 
#ftMEasX should be smaller/less negative

expect_lt(ftMeasX$coef$`1`[1,1], ftZero$coef$`1`[1,1])
expect_lt(ftMeasX$coef$`2`[1,1], ftZero$coef$`2`[1,1])
expect_lt(ftMeasX$coef$`3`[1,1], ftZero$coef$`3`[1,1])
expect_lt(ftMeasX$coef$`4`[1,1], ftZero$coef$`4`[1,1])


ft=sma(logLongev~logLMA*site,data=leaflife) 
ftZero=sma(logLongev~logLMA*site,data=leaflife,V=matrix(c(0,0,0,0),ncol=2)) 
# ft and ftZero should be identical 
ft$coef 
ftZero$coef 

expect_identical(ft$coef$`1`[1,1], ftZero$coef$`1`[1,1])
expect_identical(ft$coef$`2`[1,1], ftZero$coef$`2`[1,1])
expect_identical(ft$coef$`3`[1,1], ftZero$coef$`3`[1,1])
expect_identical(ft$coef$`4`[1,1], ftZero$coef$`4`[1,1])
expect_identical(ft$coef$`1`[2,1], ftZero$coef$`1`[2,1])
expect_identical(ft$coef$`2`[2,1], ftZero$coef$`2`[2,1])
expect_identical(ft$coef$`3`[2,1], ftZero$coef$`3`[2,1])
expect_identical(ft$coef$`4`[2,1], ftZero$coef$`4`[2,1])

# Fitting a common slope

ftZero=sma(logLongev~logLMA+site,data=leaflife,V=matrix(c(0,0,0,0),ncol=2)) 
coef(ftZero) 
ft=sma(logLongev~logLMA+site,data=leaflife) 
coef(ft)
ftMeasY=sma(logLongev~logLMA+site,data=leaflife,V=matrix(c(0.001,0,0,0),ncol=2)) 
coef(ftMeasY) 
ftMeasX=sma(logLongev~logLMA+site,data=leaflife,V=matrix(c(0,0,0,0.002),ncol=2)) 
coef(ftMeasX) 

# common slope flatter for ftMeasY, elevs higher, less negative
expect_lt(coef(ftMeasY)[1,2], coef(ftZero)[1,2])
expect_gt(coef(ftMeasY)[1,1], coef(ftZero)[1,1])
expect_gt(coef(ftMeasY)[2,1], coef(ftZero)[2,1])
expect_gt(coef(ftMeasY)[3,1], coef(ftZero)[3,1])
expect_gt(coef(ftMeasY)[4,1], coef(ftZero)[4,1])

# common slope steeper for ftMeasX, elevs lower, more negative 
expect_gt(coef(ftMeasX)[1,2], coef(ftZero)[1,2])
expect_lt(coef(ftMeasX)[1,1], coef(ftZero)[1,1])
expect_lt(coef(ftMeasX)[2,1], coef(ftZero)[2,1])
expect_lt(coef(ftMeasX)[3,1], coef(ftZero)[3,1])
expect_lt(coef(ftMeasX)[4,1], coef(ftZero)[4,1])
