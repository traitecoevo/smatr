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
