# Route any graphics produced while running the test suite (e.g. plots drawn
# by the package examples exercised via test_examples(), or base-graphics
# plot.sma() calls) to a null device. Plotting code still executes in full,
# so its behaviour is tested, but no graphics windows pop up during
# devtools::test() / R CMD check.
grDevices::pdf(NULL)
withr::defer(grDevices::dev.off(), teardown_env())
