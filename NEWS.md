# smatr 3.5-1

* Fixed `coef()` returning `NULL` for `sma`/`ma` fits: `coef.sma()` was never
  registered as an S3 method, so the generic did not dispatch to it for users
  of the installed package (#26).
* Removed the ad hoc `test/` directory of user bug reproductions; the checks
  that remain relevant are now covered by the `testthat` suite (#26).

# smatr 3.5-0

* Added `ggplot.sma()`, a `ggplot2` method for `sma` objects. Call
  `ggplot()` on a fit to draw the data and fitted (S)MA line(s), coloured by
  group where present, with log-scaled axes for any log-transformed variables
  (#33). Revives earlier work by Fonti Kar.

# smatr 3.4-9

* Fixed `object 'xi' not found` error in `slope.com()` (and therefore `sma()`
  with a grouping variable, e.g. `sma(y ~ x * group)`) that occurred when the
  grouping factor contained a level with no observations — for example, an
  unused level retained after `subset()`. This also affected
  `sma(..., multcomp = TRUE)`. Empty groups are now dropped before fitting
  (which also removes a latent bug where a non-first empty group silently
  reused the previous group's variance matrix), and a common-slope test with
  fewer than two usable groups gives a clear error instead of crashing
  (#30, #34).

# smatr 3.4-8

* Added a `NEWS.md` file to track changes to the package.
