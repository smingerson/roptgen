ropt_get <- function(name, value, type, default, check, args, partial) {
  return_value_if_present_and_valid(name, value, type, default, check, args, partial)
  return_option_if_present_and_valid(name, value, type, default, check, args, partial)
  call_if_fun(default, args)
}
call_if_fun <- function(fn, args) {
  if (is.function(fn)) {
    return(do.call(fn, args))
  }
  fn
}
#' @include utils.R
#' @include option-type-handler.R
return_option_if_present_and_valid <- function(name, value, type, default, check, args, partial) {
  opt <- getOption(name)
  opt_envvar <- "option"
  if (is.null(opt)) {
    opt <- Sys.getenv(toupper(gsub(".", "_", name)),unset = "<NA>")
    if (identical(opt, "<NA>")) {
      return()
    }
    opt_envvar <- "environment variable"
  }
  opt <- roptgen_validate(type, name, partial)(opt)
  if (!is.null(check)) {
    opt <- call_if_fun(opt, args)
    pass <- check(opt)
    if (!pass) {
      warning("Value of ", opt_envvar, " ",
              dQuote(name),
              " did not pass check function. ",
              "Using the default instead.",
              call. = FALSE
      )
      return()
    }
  }
  do.call(return, list(opt), envir = sys.frame(-1))
}

return_value_if_present_and_valid <-
  function(name, value, type, default, check, args, partial) {
    if (is.null(value)) {
      return()
    }

    value <- roptgen_validate(type, name, partial)(value)
    if (!is.null(check)) {
      value <- call_if_fun(value, args)
      pass <- check(value)
      if (!pass) {
        warning(
          dQuote(value),
          " did not pass check function. ",
          "Using value of option, ",
          dQuote(name),
          " instead, or the default if unset.",
          call. = FALSE
        )
        return()
      }
    }
    do.call(return, list(value), envir = sys.frame(-1))
  }
