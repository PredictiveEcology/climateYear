# climateYear (development version)

- The placeholder `projectedClimateRasters` made when none are supplied failed for any run longer than one year (`terra::rast(nlyrs = <vector>)`); it now has one layer per simulation year. The calendar-year check calls `climateData::checkCalendarYears()`, so `climateData` is in `reqdPkgs` (needs >= 2.2.3.9009).
- `init` now stops, when climate rasters are supplied and `samplingRange` is not set, if the run's start is not a calendar year from 1901 to 2100 (for example the model years 0-10 that `setupProject()` gives without `times`). Before, a random climate year was drawn each year without notice. The message says to set `times`, e.g. `times = list(start = 2020, end = 2120)`, or `samplingRange` to sample on purpose.
- `reproducible` is now in `reqdPkgs` (`Filenames()`, `asPath()` are used unqualified).

# climateYear 0.1.0

climateYear now produces each year's climate layers as their own object, instead of changing the historical or projected climate layers it was given. When sampling, it now draws from the historical and projected years together. A simulation year outside the sampling range no longer stops the run.

Checks for missing values no longer fail when they look at more than one value. The module lists every package it needs, and it now has automatic tests.

* `reqdPkgs` now lists `data.table` and `terra`, which the module's code uses.

# climateYear 0.0.1 (07 January 2026)

- initial module version
