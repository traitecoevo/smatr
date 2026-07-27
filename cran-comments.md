## Resubmission

This is a resubmission. In response to the previous review:

* Removed "+ file LICENSE" from the License field and deleted the LICENSE
  file. The package is licensed under GPL (>= 3) with no additional
  restrictions, so the License field is now simply `GPL (>= 3)`.

## Submission notes

This is a maintenance release of smatr, updating from the archived/previous
CRAN version 3.4-8 to 3.5-1.

* The package maintainer has changed from Remko Duursma to Daniel Falster
  (daniel.falster@unsw.edu.au). Remko Duursma is retained as an author.
* This release fixes several bugs and adds a `ggplot2` method for `sma`
  objects. See NEWS.md for details.

## Test environments

* local macOS (aarch64), R 4.6.0
* GitHub Actions: ubuntu-latest (R-devel, R-release, R-oldrel-1),
  macOS-latest (R-release), windows-latest (R-release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies that are broken by this update.
