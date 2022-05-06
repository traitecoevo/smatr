
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

#Most simple fits
default_sma <- sma(logLongev~logLMA, data=leaf.low.soilp)
default_sma$coef 

me_sma <- sma(logLongev~logLMA, data=leaf.low.soilp, V = V2)
me_sma$coef

test_that("Functions accounts for measurement error", {
  expect_silent(sma(longev~lma, data=leaf.low.soilp, V = V2))
  expect_true(abs(me_sma$coef[[1]][1,1]) < abs(default_sma$coef[[1]][1,1]))
  expect_true(me_sma$coef[[1]][2,1] < default_sma$coef[[1]][2,1])
}) 













arr_me_sma <- sma(logLongev~logLMA+rain, data=leaf.low.soilp, V = V2group)
arr_me_sma$coef #works
