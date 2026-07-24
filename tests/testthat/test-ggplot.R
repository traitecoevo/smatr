## ggplot2 method for sma objects
skip_if_not_installed("ggplot2")

leaf <- subset(leaflife, soilp == "low")

test_that("ggplot.sma returns a ggplot object for every log case", {
  for (lg in c("", "x", "y", "xy")) {
    fit <- sma(longev ~ lma, data = leaf, log = lg)
    p <- ggplot2::ggplot(fit)
    expect_s3_class(p, "ggplot")
    # builds without error and yields two layers (points + line)
    b <- ggplot2::ggplot_build(p)
    expect_length(b$plot$layers, 2)
  }
})

test_that("ggplot.sma works with a group structure and colours by group", {
  fit <- sma(longev ~ lma * rain, data = leaf, log = "xy")
  p <- ggplot2::ggplot(fit)
  expect_s3_class(p, "ggplot")
  expect_true("colour" %in% names(p$mapping))

  pd <- make_plot_data(fit)
  expect_true(pd$grouped)
  expect_setequal(unique(pd$points$group), as.character(fit$groups))
  expect_setequal(unique(pd$lines$group), as.character(fit$groups))
})

test_that("plot data are finite on the natural scale for all log cases", {
  for (lg in c("", "x", "y", "xy")) {
    fit <- sma(longev ~ lma, data = leaf, log = lg)
    pd <- make_plot_data(fit)
    expect_true(all(is.finite(pd$points$x)) && all(is.finite(pd$points$y)))
    expect_true(all(is.finite(pd$lines$x)) && all(is.finite(pd$lines$y)))
  }
})

test_that("fitted line matches the plot.sma curve formula", {
  for (lg in c("", "x", "y", "xy")) {
    fit <- sma(longev ~ lma, data = leaf, log = lg)
    a <- fit$groupsummary$Int[1]
    B <- fit$groupsummary$Slope[1]
    xs <- seq(fit$from[[1]], fit$to[[1]], length.out = 20)
    ref <- if (lg == "") {
      a + B * xs
    } else {
      switch(lg,
        x  = a + B * log10(xs),
        y  = 10^(a + B * xs),
        xy = 10^a * xs^B
      )
    }
    pd <- make_plot_data(fit)
    got <- stats::approx(pd$lines$x, pd$lines$y, xout = xs)$y
    expect_equal(got, ref, tolerance = 1e-3)
  }
})

test_that("get_coef returns the group's intercept and slope", {
  fit <- sma(longev ~ lma * rain, data = leaf, log = "xy")
  g <- as.character(fit$groups[1])
  expect_equal(get_coef(fit, g, "a"), fit$groupsummary$Int[fit$groupsummary$group == g])
  expect_equal(get_coef(fit, g, "B"), fit$groupsummary$Slope[fit$groupsummary$group == g])
  expect_error(get_coef(fit, g, "z"))
})

test_that("ggplot.sma rejects non-sma input", {
  expect_error(ggplot.sma(list(a = 1)), "class 'sma'")
})
