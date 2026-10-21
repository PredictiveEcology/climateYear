test_that("an empty projected list contributes no years (NRV runs ask for none)", {
  ## FireSense 2026-10-09: every NRV job stopped at `sim$projectedClimateRasters[[1]]`
  hist <- list(CMD = terra::rast(nlyrs = 2, names = c("year1990", "year1991")))
  expect_identical(availableClimateYears(list(), hist), c("year1990", "year1991"))
  expect_identical(availableClimateYears(NULL, hist), c("year1990", "year1991"))
})

test_that("projected and historical years are combined, sorted and unique", {
  proj <- list(CMD = terra::rast(nlyrs = 2, names = c("year2025", "year2024")))
  hist <- list(CMD = terra::rast(nlyrs = 2, names = c("year2023", "year2024")))
  expect_identical(availableClimateYears(proj, hist), c("year2023", "year2024", "year2025"))
  expect_identical(availableClimateYears(proj, list()), c("year2025", "year2024"))
  expect_null(availableClimateYears(list(), list()))
})
