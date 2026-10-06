# Regression tests for the package's exported API.
#
# Regenerating NAMESPACE with roxygen2 silently dropped nine functions that
# smatr 3.4-8 exported (makeLogMinor, seqLog, defineAxis, ...), because their
# source files lacked @export tags, so users of 3.5-1 could no longer call
# them. See https://github.com/traitecoevo/smatr/issues/40

test_that("functions exported in smatr 3.4-8 are still exported (#40)", {
  exports_3.4_8 <- c(
    "alpha.fun", "b.com.est", "com.ci", "defineAxis", "elev.com",
    "elev.test", "huber.M", "line.cis", "lr.b.com", "ma", "makeLogMinor",
    "meas.est", "multcompmatrix", "nicePlot", "seqLog", "shift.com",
    "slope.com", "slope.test", "sma"
  )
  # setdiff() names any missing function in the failure message
  expect_equal(setdiff(exports_3.4_8, getNamespaceExports("smatr")), character())
})

test_that("log-axis helpers return the expected ticks", {
  expect_equal(seqLog(1, 1000), c(1, 10, 100, 1000))
  expect_equal(seqLog(2, 16, base = 2), c(2, 4, 8, 16))
  expect_equal(makeLogMinor(c(1, 10)), c(1:9, 10))
  ax <- defineAxis(major.ticks = c(1, 10), minor.ticks = makeLogMinor(c(1, 10)))
  expect_equal(ax$minor.ticks, 2:9)
})
