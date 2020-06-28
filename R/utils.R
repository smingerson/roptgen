not_in <- function(value, x) {
  !value %in% x
}

wrong_prototype <- function(value, x) {
  !(typeof(value) == typeof(x) && length(x) == length(value))
}

opt_stop_invalid_vec <- function(option, x) {
  stop(str_opt_invalid(option), " It must be a length-", length(x), " ", typeof(x), " vector. ",
       "Alternatively, do not set to use the default.",
       call. = FALSE
  )
}
opt_stop_invalid_choice <- function(option, choices) {
  stop(str_opt_invalid(option), " It must be ", paste_or(choices),
       ".  Alternatively, do not set to use the default.",
       call. = FALSE
  )
}
str_opt_invalid <- function(x) {
  paste("Value of option", dQuote(x), " is not valid.")
}


paste_and <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  paste(paste(x[-length(x)], collapse = ", "), x[length(x)], sep = " and ")
}
paste_or <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  paste(paste(x[-length(x)], collapse = ", "), x[length(x)], sep = " or ")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
roptgen_test_dir <- function() {
  testvar <- getOption("roptgen.test")
  if (is.character(testvar)) {
    if (testvar == 'tinytest')
      'inst/tinytest'
  } else {
      "tests/testthat"
    }
}

