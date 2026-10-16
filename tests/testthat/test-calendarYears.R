## The rule (climateData::checkCalendarYears()) and its span are tested in climateData.
## climateYear matches simulation years to the years of the climate rasters it is given (historical
## from 1901, projections to 2100). With model-year times (setupProject() without `times` gives 0 to
## 10) and no `samplingRange`, no climate year matches, so it would draw random years without saying
## so. A `samplingRange` is a deliberate request to sample, so any times are then accepted.

climateRasters <- function() {
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3, vals = 1)
  names(r) <- paste0("year", 2011:2013)
  list(fooVar = r)
}

initSim <- function(times, samplingRange = NA_real_) {
  sim <- suppressMessages(SpaDES.core::simInit(
    times = times, modules = moduleName, paths = testPaths,
    params = list(climateYear = list(samplingRange = samplingRange)),
    objects = list(projectedClimateRasters = climateRasters())))
  suppressMessages(SpaDES.core::spades(sim, debug = FALSE, events = list(climateYear = "init")))
}

test_that("calendar-year times are accepted", {
  expect_no_error(initSim(list(start = 2020, end = 2120)))
})

test_that("model-year times with climate rasters fail with a message that says what to supply", {
  expect_error(initSim(list(start = 0, end = 10)),
               "climateYear: times are model years 0-10.*calendar years.*times = list\\(start = 2020.*samplingRange")
})

test_that("a samplingRange accepts model-year times", {
  expect_no_error(initSim(list(start = 0, end = 10), samplingRange = 2011:2013))
})

test_that("the placeholder rasters of a multi-year run, with none supplied, have a layer per year", {
  sim <- suppressMessages(SpaDES.core::simInit(
    times = list(start = 0, end = 10), modules = moduleName, paths = testPaths))
  expect_identical(names(sim$projectedClimateRasters[[1]]), paste0("year", 0:10))
  expect_no_error(suppressMessages(SpaDES.core::spades(sim, debug = FALSE, events = list(climateYear = "init"))))
})
