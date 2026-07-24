#' Plot a (S)MA fit using ggplot2
#'
#' @description Produce a \code{ggplot2} version of the default \code{x}-\code{y}
#' plot for an \code{sma} fit: the data points plus the fitted (standardised)
#' major axis line(s), with separate colours for each group where present. This
#' is a ggplot2-based alternative to the base-graphics \code{\link{plot.sma}}.
#'
#' @details Data and fitted lines are drawn on the natural (back-transformed)
#' scale of the original variables. Where the fit used a log transformation
#' (\code{log} was one of \code{"x"}, \code{"y"} or \code{"xy"}), the
#' corresponding axis is drawn on a log10 scale via
#' \code{\link[ggplot2]{scale_x_log10}} / \code{\link[ggplot2]{scale_y_log10}}.
#' The fitted line geometry mirrors the curve logic used by
#' \code{\link{plot.sma}}, so both plotting methods agree.
#'
#' The returned object is a standard \code{ggplot} object and can be further
#' customised with additional layers, themes, scales and facets in the usual
#' way.
#'
#' @param data,mapping Present for compatibility with the \code{ggplot2::ggplot}
#' generic; not used. Pass the \code{sma} object as the first argument.
#' @param obj An object of class \code{sma}, as returned by \code{\link{sma}} or
#' \code{\link{ma}}.
#' @param ... Further arguments passed to \code{\link[ggplot2]{geom_line}} for
#' the fitted lines (for example \code{linewidth} or \code{linetype}).
#'
#' @return A \code{ggplot} object.
#' @author F. Kar, D. Falster
#' @seealso \code{\link{plot.sma}}
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme_bw scale_x_log10 scale_y_log10
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' data(leaflife)
#' leaf.low.soilp <- subset(leaflife, soilp == "low")
#'
#' # Single fit
#' ft1 <- sma(longev ~ lma, data = leaf.low.soilp, log = "xy")
#' ggplot2::ggplot(ft1)
#'
#' # Grouped fit with common-slope test
#' ft2 <- sma(longev ~ lma * rain, data = leaf.low.soilp, log = "xy")
#' ggplot2::ggplot(ft2)
#'
#' # ggplot object can be extended as usual
#' ggplot2::ggplot(ft2) + ggplot2::theme_minimal()
#' }
ggplot.sma <- function(obj, ..., data = NULL, mapping = NULL) {
  if (!inherits(obj, "sma")) {
    stop("`obj` must be an object of class 'sma'.", call. = FALSE)
  }

  pd <- make_plot_data(obj)
  grouped <- pd$grouped

  # Base plot: colour by group only when there is a group structure
  if (grouped) {
    p <- ggplot(mapping = aes(x = .data$x, y = .data$y, colour = .data$group))
  } else {
    p <- ggplot(mapping = aes(x = .data$x, y = .data$y))
  }

  p <- p +
    geom_point(data = pd$points, shape = 21, size = 2) +
    geom_line(data = pd$lines, ...) +
    labs(x = obj$variables[2], y = obj$variables[1]) +
    theme_bw()

  # Log-scale the axes that were log-transformed in the fit
  if (obj$log %in% c("x", "xy")) p <- p + scale_x_log10()
  if (obj$log %in% c("y", "xy")) p <- p + scale_y_log10()

  p
}

#' Build point and fitted-line data for plotting an sma fit
#'
#' @description Internal helper that assembles the data used by
#' \code{\link{ggplot.sma}}: observed points and fitted (S)MA lines, both on the
#' natural (back-transformed) scale of the original variables.
#'
#' @param obj An object of class \code{sma}.
#' @param n Number of points used to draw each fitted line.
#'
#' @return A list with elements \code{points} (a data frame of \code{x},
#' \code{y} and \code{group}), \code{lines} (a data frame of the same columns
#' describing the fitted lines) and \code{grouped} (a logical flag indicating
#' whether the fit has a group structure).
#' @keywords internal
make_plot_data <- function(obj, n = 100) {
  grouped <- length(obj$groups) > 1
  logx <- obj$log %in% c("x", "xy")
  logy <- obj$log %in% c("y", "xy")

  # Observed data are stored on the fitting scale (log10 where transformed);
  # back-transform to the natural scale for plotting.
  X <- obj$data[, 2]
  Y <- obj$data[, 1]

  points <- data.frame(
    x = if (logx) 10^X else X,
    y = if (logy) 10^Y else Y,
    group = if (grouped) as.character(obj$data[, 3]) else "all",
    stringsAsFactors = FALSE
  )

  # One fitted line per group, evaluated across each group's x-range.
  # sma() stores from/to on the natural (back-transformed) scale of x, so the
  # line is built directly in raw x and the fitted value uses the same curve
  # formula as plot.sma() for each log case.
  line_list <- lapply(seq_along(obj$groups), function(i) {
    g <- obj$groups[i]
    a <- get_coef(obj, g, "a")
    B <- get_coef(obj, g, "B")

    xseq <- seq(as.numeric(obj$from[i]), as.numeric(obj$to[i]), length.out = n)

    yfit <- if (obj$log == "") {
      a + B * xseq
    } else {
      switch(obj$log,
        "x"  = a + B * log10(xseq),
        "y"  = 10^(a + B * xseq),
        "xy" = 10^a * xseq^B
      )
    }

    data.frame(
      x = xseq,
      y = yfit,
      group = as.character(g),
      stringsAsFactors = FALSE
    )
  })
  lines <- do.call(rbind, line_list)

  list(points = points, lines = lines, grouped = grouped)
}

#' Extract a fitted coefficient for one level of the grouping variable
#'
#' @param obj An object of class \code{sma}.
#' @param group_level Character string naming the group level.
#' @param coef_type \code{"a"} for the intercept (elevation) or \code{"B"} for
#' the slope.
#'
#' @return A single numeric coefficient.
#' @keywords internal
get_coef <- function(obj, group_level, coef_type) {
  gs <- obj$groupsummary
  row <- gs[as.character(gs$group) == as.character(group_level), , drop = FALSE]

  switch(coef_type,
    a = as.numeric(row$Int),
    B = as.numeric(row$Slope),
    stop("`coef_type` must be 'a' (intercept) or 'B' (slope).", call. = FALSE)
  )
}
