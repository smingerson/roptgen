roptgen_test_dir <- function() {
  testvar <- getOption("roptgen.test")
  if (is.character(testvar)) {
    if (testvar == 'tinytest')
      'inst/tinytest'
  } else {
    "tests/testthat"
  }
}

