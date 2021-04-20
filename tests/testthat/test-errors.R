
# Should run smoothly
expect_silent(sma(longev ~ lma, log='xy', data=leaflife))
expect_silent(sma(longev ~ lma, data=leaflife))
expect_silent(sma(longev ~ lma*rain, data=leaflife))
expect_silent(ma(longev ~ lma*rain, data=leaflife))
expect_silent(sma(longev~lma*rain, data=leaflife, method="OLS"))

# Should fail
expect_error(sma(longev ~ lma, log='xy', data=NULL))
expect_error(sma(longev ~ lma2, log='xy', data=leaflife))
expect_error(sma(longev ~ lma*group, log='xy', data=leaflife))
expect_error(sma(longev ~ lma*group, log='xyz', data=leaflife))
