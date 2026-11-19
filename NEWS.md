# climateYear (development version)

- Each climate year is written once, to `year<year>_<digest>.tif` beside the climate layers (e.g. `year2001_3f9c2a1.tif`; the digest is 7 characters, from the source files and layer names), and every process that draws that year from the same climate layers uses that file, in later runs too. It was `year<year>_<.runName>_<process ID>.tif`, so every R process wrote its own copy of every year it drew and none was removed (169 files, 1.8 GB, in one project). Parallel runs that draw the same year wait on `Cache()`'s file lock while the first writes it, then use that file.
- `reproducible` is now in `reqdPkgs` (`Filenames()`, `asPath()` are used unqualified).

# climateYear 0.1.0

climateYear now produces each year's climate layers as their own object, instead of changing the historical or projected climate layers it was given. When sampling, it now draws from the historical and projected years together. A simulation year outside the sampling range no longer stops the run.

Checks for missing values no longer fail when they look at more than one value. The module lists every package it needs, and it now has automatic tests.

* `reqdPkgs` now lists `data.table` and `terra`, which the module's code uses.

# climateYear 0.0.1 (07 January 2026)

- initial module version
