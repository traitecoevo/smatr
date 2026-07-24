# Regression tests for coef() dispatch on 'sma'/'ma' objects.
#
# coef.sma() existed but was never tagged with @export, so no
# `S3method(coef, sma)` line was written to NAMESPACE. As a result the
# generic coef() failed to dispatch for users of the *installed* package
# and returned NULL, even though coef.sma() worked when called directly.
# (The package's own test suite did not catch this because tests run
# inside the package namespace, where UseMethod finds coef.sma directly.)
# See https://github.com/traitecoevo/smatr/issues/26

single <- sma(longev ~ lma, log = "xy", data = leaflife)
grouped <- sma(longev ~ lma * rain, log = "xy", data = leaflife)

test_that("coef.sma is registered as an S3 method for the coef generic", {
  # Guards against the @export tag being dropped again. Under R CMD check
  # (installed package) a missing S3method(coef, sma) registration makes
  # getS3method() return NULL; under devtools::test() the method is found by
  # naming convention regardless, so this passes harmlessly in dev.
  expect_false(is.null(getS3method("coef", "sma", optional = TRUE)))
})

test_that("coef() returns elevation and slope for a single fit", {
  cf <- coef(single)
  expect_false(is.null(cf))
  expect_named(cf, c("elevation", "slope"))
  expect_identical(coef(single), coef.sma(single))
})

test_that("coef() returns a data.frame of coefficients per group", {
  cf <- coef(grouped)
  expect_false(is.null(cf))
  expect_s3_class(cf, "data.frame")
  expect_named(cf, c("elevation", "slope"))
  expect_identical(rownames(cf), grouped$groups)
  expect_identical(coef(grouped), coef.sma(grouped))
})

test_that("coef() dispatches for ma() fits too", {
  ma_obj <- ma(longev ~ lma, log = "xy", data = leaflife)
  cf <- coef(ma_obj)
  expect_false(is.null(cf))
  expect_named(cf, c("elevation", "slope"))
})
