## the module code uses its reqdPkgs unqualified, as it does when SpaDES.core runs it
for (pkg in c("reproducible", "terra", "data.table")) suppressPackageStartupMessages(library(pkg, character.only = TRUE))

## a module function, without running defineModule()
modFun <- function(name) {
  exprs <- parse(file.path(testthat::test_path(), "..", "..", "climateYear.R"))
  isFn <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                   identical(e[[2]], as.name(name)), logical(1))
  env <- new.env(parent = globalenv())
  eval(exprs[isFn][[1]], env)
  get(name, env)
}
