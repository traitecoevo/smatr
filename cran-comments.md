## Submission notes

This is a patch release of smatr, updating from 3.5-1 to 3.5-2.

* 3.5-1 accidentally stopped exporting nine functions that 3.4-8 exported (including `makeLogMinor()`, reported by a user). This release exports them again and restores their documentation. See NEWS.md for details.

## Test environments

* local macOS (aarch64), R 4.6.1
* GitHub Actions: ubuntu-latest (R-devel, R-release, R-oldrel-1), macOS-latest (R-release), windows-latest (R-release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

This release only adds exports; no existing function or export changes. None of the 8 reverse dependencies imports the whole smatr namespace or uses any of the restored function names, so none is affected.
