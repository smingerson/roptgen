stop_invalid_vec <- function(option, x) {
  stop(str_opt_invalid(option), " It must be a length-", length(x), " ", typeof(x), " vector. ",
       "Alternatively, do not set to use the default.",
       call. = FALSE
  )
}
stop_invalid_choice <- function(option, choices) {
  stop(str_opt_invalid(option), " It must be ", paste_or(choices),
       ".  Alternatively, do not set to use the default.",
       call. = FALSE
  )
}
str_opt_invalid <- function(x) {
  paste("Value of option", dQuote(x), " is not valid.")
}


is_wrong_prototype <- function(value, x) {
  !(typeof(value) == typeof(x) && length(x) == length(value))
}
