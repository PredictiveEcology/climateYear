## Each climate year is written to its own file beside the climate stacks. The file was named
## with the R process ID, so every process wrote its own copy of every year it drew and none
## was ever removed (169 files, 1.8 GB, in one project). It is now named by the year and a
## digest of its sources, shared by every process that draws that year.

climateStacks <- function(dir) {
  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 5, ymin = 0, ymax = 5,
                   crs = "EPSG:3005", nlyrs = 3, vals = seq_len(75))
  names(r) <- paste0("year", 2001:2003)
  list(CMD = terra::writeRaster(r, file.path(dir, "CMD_historical.tif")),
       MDC = terra::writeRaster(r * 2, file.path(dir, "MDC_historical.tif")))
}

runClimateYear <- function(climDir, outDir) {
  hist <- lapply(c(CMD = "CMD_historical.tif", MDC = "MDC_historical.tif"),
                 function(f) terra::rast(file.path(climDir, f)))
  paths <- testPaths
  paths$outputPath <- outDir
  sim <- SpaDES.core::simInit(times = list(start = 2001, end = 2003), modules = moduleName,
                              paths = paths, objects = list(historicalClimateRasters = hist,
                                                             projectedClimateRasters = hist))
  sim <- SpaDES.core::spades(sim, debug = FALSE)
  list(pid = Sys.getpid(), source = terra::sources(sim$currentClimateRasters),
       names = names(sim$currentClimateRasters), values = terra::values(sim$currentClimateRasters))
}

test_that("processes drawing the same climate year share one file, not one per process ID", {
  skip_on_os("windows")                          # forks two R processes
  climDir <- withr::local_tempdir()
  climateStacks(climDir)

  runs <- parallel::mclapply(1:2, function(i) {
    runClimateYear(climDir, file.path(testPaths$outputPath, i))
  }, mc.cores = 2)
  expect_false(identical(runs[[1]]$pid, runs[[2]]$pid))

  ## every simulation year is a historical year here, so the years drawn are 2001 to 2003
  yearFiles <- list.files(climDir, "^year")
  expect_length(yearFiles, 3L)
  expect_match(yearFiles, "^year200[1-3]_[0-9a-f]{7}\\.tif$")
  expect_identical(runs[[1]]$source, runs[[2]]$source)
  expect_identical(basename(runs[[1]]$source), grep("^year2003_", yearFiles, value = TRUE))

  ## the file holds that year's layer of each variable
  expect_identical(runs[[1]]$names, c("CMD", "MDC"))
  expect_equal(runs[[1]]$values[, "CMD"], 51:75)
  expect_equal(runs[[1]]$values[, "MDC"], 2 * (51:75))

  ## a later run reuses the files without writing them again
  before <- file.mtime(file.path(climDir, yearFiles))
  Sys.sleep(1.1)
  runClimateYear(climDir, file.path(testPaths$outputPath, 3))
  expect_identical(list.files(climDir, "^year"), yearFiles)
  expect_identical(file.mtime(file.path(climDir, yearFiles)), before)
})
