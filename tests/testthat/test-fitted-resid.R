# Extract only low-nutrient, low-rainfall data from leaflife dataset:
leaf.low <- subset(leaflife, soilp == 'low' & rain == 'low')

# Fit a single MA for log(leaf longevity) vs log(leaf mass per area):
ma_obj <- ma(longev ~ lma, log='xy', slope.test=1, data=leaf.low) 

# Single anaylses
test_that("Functions run", {
  expect_visible(fitted.sma(ma_obj, type = "fitted", centered = FALSE))
  expect_silent(fitted.sma(ma_obj))
  expect_length(fitted.sma(ma_obj), nrow(leaf.low)) 
  expect_equal(fitted.sma(ma_obj, type = "residuals"),
               residuals.sma(ma_obj))
  expect_length(residuals.sma(ma_obj), nrow(leaf.low)) 
})

test_that("Calculations are correct", {
  expect_equal(fitted.sma(ma_obj, type = "fitted", centered = FALSE) ,
               (fitted <- (summary(ma_obj)$Slope*log10(leaf.low$longev)) + log10(leaf.low$lma)) # Following equation in Table 4 for MA 
               )
  expect_equal(fitted.sma(ma_obj, type = "fitted", centered = TRUE) , 
               (fitted <- (summary(ma_obj)$Slope*log10(leaf.low$longev)) + log10(leaf.low$lma) - mean(fitted)) 
  )
  expect_equal(fitted.sma(ma_obj, type = "residuals"),
               resid <- log10(leaf.low$longev) - (summary(ma_obj)$Int + summary(ma_obj)$Slope*log10(leaf.low$lma)))
  expect_equal(fitted.sma(ma_obj, type = "residuals", centered = TRUE),
               fitted.sma(ma_obj, type = "residuals", centered = FALSE)
  )
})

# Group analyses
# Extract only low-nutrient, low-rainfall data from leaflife dataset:
soil.low <- subset(leaflife, soilp == 'low')

sma_elev_obj <- sma(longev ~ lma+rain, log='xy', data=soil.low) 
sma_slop_obj <- sma(longev ~ lma*rain, log='xy', data=soil.low) 

test_that("Functions run", {
  expect_silent(fitted.sma(sma_elev_obj))
  expect_silent(fitted.sma(sma_slop_obj))
  
  expect_visible(fitted.sma(sma_elev_obj, type = "fitted", centered = FALSE))
  expect_visible(fitted.sma(sma_slop_obj, type = "fitted", centered = FALSE))
  
  expect_length(fitted.sma(sma_slop_obj), nrow(soil.low)) 
})

test_that("Calculations are correct", {
  high <- subset(soil.low, rain == "high")
  low <- subset(soil.low, rain == "low")
  
  ( fitted_high <- log10(high$longev) + (summary(sma_slop_obj)$Slope[1] * log10(high$lma) ) )
  ( fitted_high - mean(fitted_high) )
  
  ( fitted_low <- log10(low$longev) + (summary(sma_slop_obj)$Slope[2] * log10(low$lma) ))
  ( fitted_low - mean(fitted_low) )
  
  expect_length(fitted.sma(sma_slop_obj), length(fitted_high) + length(fitted_low))
  expect_equal(fitted.sma(sma_slop_obj, centered = FALSE), c(fitted_high, fitted_low))
  expect_equal(fitted.sma(sma_slop_obj, centered = TRUE), c(fitted_high, fitted_low) - mean( c(fitted_high, fitted_low)))
})
