test_that("yearSequence: same yearSeed gives the same years, whatever else used random numbers", {
  yearSequence <- modFun("yearSequence")
  set.seed(1); runif(5)
  a <- yearSequence(2001:2020, 1990:1999, seed = 42L)
  set.seed(99); runif(100)
  b <- yearSequence(2001:2020, 1990:1999, seed = 42L)
  expect_identical(a, b)
  expect_identical(names(a), as.character(2001:2020))
  expect_true(all(a %in% 1990:1999))
  ## a different seed gives a different sequence
  expect_false(identical(a, yearSequence(2001:2020, 1990:1999, seed = 43L)))
  ## simulation years inside the range use themselves
  d <- yearSequence(1995:2004, 1990:1999, seed = 42L)
  expect_identical(unname(d[as.character(1995:1999)]), 1995:1999)
})

test_that("yearSequence leaves the caller's random numbers and RNGkind alone", {
  yearSequence <- modFun("yearSequence")
  old <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(7)
  seedBefore <- .Random.seed
  yearSequence(2001:2010, 1990:1999, seed = 42L)
  expect_identical(.Random.seed, seedBefore)
  expect_identical(RNGkind()[1:3], c("L'Ecuyer-CMRG", "Inversion", "Rejection"))

  ## no .Random.seed before: none is left behind
  rm(.Random.seed, envir = globalenv())
  yearSequence(2001:2010, 1990:1999, seed = 42L)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("checkYearSeedRange names the samplingRange years missing from the climate data", {
  checkYearSeedRange <- modFun("checkYearSeedRange")
  avail <- paste0("year", 1990:1999)
  expect_silent(checkYearSeedRange(1990:1999, avail))
  expect_error(checkYearSeedRange(1990:2001, avail), "2000, 2001")
  expect_error(checkYearSeedRange(NA, avail), "samplingRange")
})

test_that("yearSeed in getClimate: same climate years whatever years each run has available", {
  skip_if_not_installed("SpaDES.core")
  skip_if_not_installed("terra")
  library(SpaDES.core)
  library(data.table)

  modDir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  mp <- withr::local_tempdir()
  file.copy(modDir, mp, recursive = TRUE)
  file.rename(file.path(mp, basename(modDir)), file.path(mp, "climateYear"))

  mkProj <- function(yrs) {
    r <- terra::rast(nrows = 4, ncols = 4, nlyrs = length(yrs), xmin = 0, xmax = 4, ymin = 0, ymax = 4,
                     crs = "EPSG:3857")
    terra::values(r) <- seq_len(terra::ncell(r) * length(yrs))
    names(r) <- paste0("year", yrs)
    list(CMD = r)
  }
  run <- function(proj, params, extraRng = TRUE) {
    if (extraRng) runif(sample(1:50, 1)) ## the session's random numbers differ between runs
    unlink(file.path(mp, "out"), recursive = TRUE)
    sim <- simInit(
      times = list(start = 2001, end = 2012), modules = "climateYear",
      params = list(climateYear = c(params, list(.useCache = FALSE))),
      objects = list(projectedClimateRasters = proj),
      paths = list(modulePath = mp, inputPath = file.path(mp, "in"), outputPath = file.path(mp, "out"))
    )
    spades(sim, debug = FALSE)$climateYearRecord
  }
  rng <- 1995:1999
  p <- list(samplingRange = rng, yearSeed = 7L)
  ## two study areas of one replicate: one has an extra year available that is not sampled
  a <- run(mkProj(1995:2000), p)
  b <- run(mkProj(1995:1999), p)
  expect_identical(a, b)
  expect_true(all(a$climateYear %in% rng))
  ## a different seed gives different years
  expect_false(identical(a, run(mkProj(1995:2000), modifyList(p, list(yearSeed = 8L)))))
  ## a range year missing from the data stops the run, naming it
  expect_error(run(mkProj(1995:1998), p), "1999")
})
