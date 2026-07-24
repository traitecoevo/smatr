
test_that("Should run smoothly", {
expect_silent(sma(longev ~ lma, log='xy', data=leaflife))
expect_silent(sma(longev ~ lma, data=leaflife))
expect_silent(sma(longev ~ lma*rain, data=leaflife))
expect_silent(ma(longev ~ lma*rain, data=leaflife))
expect_silent(sma(longev~lma*rain, data=leaflife, method="OLS"))
})

test_that("Should fail", {
expect_error(sma(longev ~ lma, log='xy', data=NULL))
expect_error(sma(longev ~ lma2, log='xy', data=leaflife))
expect_error(sma(longev ~ lma*group, log='xy', data=leaflife))
expect_error(sma(longev ~ lma*group, log='xyz', data=leaflife))
})

test_that("Empty grouping factor levels do not crash slope.com (issues #30, #34)", {
  # A grouping factor with a level that is retained but has no observations
  # (e.g. after subsetting) previously caused 'object xi not found' because
  # group.names came from levels() rather than the observed groups.
  d <- leaflife[leaflife$rain == "high", ]
  # Force an empty level that sorts first, mimicking a subset() leftover.
  d$site <- factor(d$site, levels = c("__empty__", sort(unique(d$site))))
  expect_silent(res <- slope.com(log10(d$longev), log10(d$lma), d$site,
                                 group.names = levels(d$site)))
  # Result matches dropping the empty level by hand.
  ref <- slope.com(log10(d$longev), log10(d$lma), droplevels(d$site))
  expect_equal(res$b, ref$b)
})