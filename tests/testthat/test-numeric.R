
## One sample analyses

# Fit a MA for log(leaf longevity) vs log(leaf mass per area):
fit.ma <- ma(longev ~ lma, log='xy', data=leaflife)
# Fit a SMA for log(leaf longevity) vs log(leaf mass per area):
fit.sma <- sma(longev ~ lma, log='xy', data=leaflife)

TOL = 1e-5

expect_gt(fit.sma$coef[[1]][1,1], fit.ma$coef[[1]][1,1])

expect_equal(fit.ma$coef[[1]][,1], c(-3.085214, 1.492616), tolerance = TOL)
expect_equal(fit.ma$coef[[1]][,2], c(-3.968020, 1.146777), tolerance = TOL)
expect_equal(fit.ma$coef[[1]][,3], c(-2.202407, 2.001084), tolerance = TOL)

expect_equal(fit.sma$coef[[1]][,1], c(-2.698800, 1.315031), tolerance = TOL)
expect_equal(fit.sma$coef[[1]][,2], c(-3.224305, 1.096261), tolerance = TOL)
expect_equal(fit.sma$coef[[1]][,3], c(-2.173295, 1.577457), tolerance = TOL)

expect_equal(nrow(fit.sma$groupsummary), 1)


## Multi site tests

fit.sma_multi <- sma(longev ~ lma*site, log='xy', data=leaflife)
expect_equal(nrow(fit.sma$groupsummary), 1)

expected <- c('coef', 'nullcoef', 'commoncoef', 'commonslopetestval', 'alpha', 'method', 'intercept', 'call', 'data', 'log', 'variables', 'origvariables', 'formula', 'groups', 'groupvarname', 'gt', 'gtr', 'slopetest', 'elevtest', 'slopetestdone', 'elevtestdone', 'n', 'r2', 'pval', 'from', 'to', 'groupsummary', 'multcompresult', 'multcompdone', 'multcompmethod')

expect_named(fit.sma_multi , expected)
