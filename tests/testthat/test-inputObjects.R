## With no climate layers supplied, `.inputObjects` makes an empty projected stack with one
## layer per simulation year. It passed the years themselves to `terra::rast(nlyrs = )`,
## which errored with "the condition has length > 1".

test_that("the module runs on its default projected climate layers", {
  sim <- SpaDES.core::simInit(times = list(start = 2001, end = 2003), modules = moduleName,
                              paths = testPaths)
  expect_identical(names(sim$projectedClimateRasters[[1]]), paste0("year", 2001:2003))
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  expect_equal(sim$climateYearRecord$climateYear, 2001:2003)
})
