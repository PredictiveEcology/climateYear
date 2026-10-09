# climateYear 0.1.0

* `getClimate` no longer stops when `projectedClimateRasters` is an empty list, as in an NRV run that asks for no projected years (`availableClimateYears()`).

climateYear now produces each year's climate layers as their own object, instead of changing the historical or projected climate layers it was given. When sampling, it now draws from the historical and projected years together. A simulation year outside the sampling range no longer stops the run.

Checks for missing values no longer fail when they look at more than one value. The module lists every package it needs, and it now has automatic tests.

* `reqdPkgs` now lists `data.table` and `terra`, which the module's code uses.

# climateYear 0.0.1 (07 January 2026)

- initial module version
