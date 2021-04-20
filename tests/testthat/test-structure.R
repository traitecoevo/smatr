
## One sample analyses
# Extract only low-nutrient, low-rainfall data:
leaf.low <- subset(leaflife, soilp == 'low' & rain == 'low')

# Fit a MA for log(leaf longevity) vs log(leaf mass per area):
fit <- sma(longev ~ lma, log='xy', data=leaflife)
fit2 <- ma(longev ~ lma, log='xy', data=leaflife)

expect_type(fit, "list")
expect_identical(class(fit), "sma")
expect_identical(class(fit2), class(fit))

expected <- c('coef', 'nullcoef', 'commoncoef', 'commonslopetestval', 'alpha', 'method', 'intercept', 'call', 'data', 'log', 'variables', 'origvariables', 'formula', 'groups', 'groupvarname', 'gt', 'gtr', 'slopetest', 'elevtest', 'slopetestdone', 'elevtestdone', 'n', 'r2', 'pval', 'from', 'to', 'groupsummary', 'multcompresult', 'multcompdone', 'multcompmethod')

expect_named(fit, expected)

expect_equal(nrow(fit$groupsummary), 1)
expect_equal(ncol(fit$groupsummary), 14)


fit.sma_multi <- sma(longev ~ lma*site, log='xy', data=leaflife)
expected <- c('coef', 'nullcoef', 'commoncoef', 'commonslopetestval', 'alpha', 'method', 'intercept', 'call', 'data', 'log', 'variables', 'origvariables', 'formula', 'groups', 'groupvarname', 'gt', 'gtr', 'slopetest', 'elevtest', 'slopetestdone', 'elevtestdone', 'n', 'r2', 'pval', 'from', 'to', 'groupsummary', 'multcompresult', 'multcompdone', 'multcompmethod')

expect_named(fit.sma_multi , expected)

expect_equal(nrow(fit.sma_multi$groupsummary), 4)
expect_equal(ncol(fit.sma_multi$groupsummary), 14)

expect_named(fit.sma_multi$groupsummary, c('group', 'n', 'r2', 'pval', 'Slope', 'Slope_lowCI', 'Slope_highCI', 'Int', 'Int_lowCI', 'Int_highCI', 'Slope_test', 'Slope_test_p', 'Elev_test', 'Elev_test_p'))

