#' @include utils.R
scratch_config_type_handler <- function(valid, option, ...) {
  UseMethod("scratch_config_type_handler")
}

scratch_config_type_handler_base <- function(valid, option, ...) {
  if (all(valid == 0)) {
    return(function(value) {
      if (wrong_prototype(value, valid)) {
        opt_stop_invalid_vec(option, valid)
      }
    })
  }
  function(value) {
    if (not_in(value, valid)) {
      opt_stop_invalid_choice(option, valid)
    }
    value
  }
}
scratch_config_type_handler.default <- function(valid, option, ...) {
  if (is.null(valid)) {
    return(option)
  }
  stop("A validation handler does not exist for this type/class.")
}
scratch_config_type_handler.double <- scratch_config_type_handler_base
scratch_config_type_handler.logical <- scratch_config_type_handler_base
scratch_config_type_handler.integer <- scratch_config_type_handler_base
scratch_config_type_handler.character <- function(valid, option, partial, ...) {
  if (all(valid == "")) {
    return(function(value) {
      if (wrong_prototype(value, valid)) {
        opt_stop_invalid_vec(option, valid)
      }
    })
  }
  function(value) {
    # If we are provided a vector of choices, presumption is we only pick 1
    if (length(value) > 1) {
      opt_stop_invalid_choice(option, dQuote(valid))
    }
    if (partial) {
      has_match <- pmatch(value, valid)
      if (!is.na(has_match)) {
        value <- valid[has_match]
      }
    }
    if (not_in(value, valid)) {
      opt_stop_invalid_choice(option, dQuote(valid))
    }
    value
  }
}

scratch_config_type_handler.list <- function(valid, option, ...) {
  # return a function which checks each condition, as long as one is true we are good
  type_handlers <- vector(mode = "list", length(valid))
  for (k in seq_along(type_handlers)) {
    type_handlers[[k]] <- scratch_config_type_handler(valid[[k]], option, ...)
  }
  function(value) {
    passes <- vector(mode = "list", length = length(type_handlers))
    for (k in seq_along(type_handlers)) {
      passes[[k]] <- tryCatch(type_handlers[[k]](value),
        error = function(e) e
      )
    }
    errors <- vapply(passes, function(x) inherits(x, "error"), TRUE)
    if (all(errors)) {
      messages <- lapply(passes, function(x) x$message)
      messages <- regmatches(
        unlist(messages),
        regexpr(
          "It must be .*\\. Alternatively, do not set to use the default.",
          unlist(messages)
        )
      )
      messages <- gsub("must be", "may be", messages)
      messages <- gsub("Alternatively, do not set to use the default.", ".", messages)

      stop("Value of option ", dQuote(option), " is not valid. ",
        "Unset it to use the default, or ",
        "use any of the following:\n",
        paste("*", messages, "\n"),
        call. = FALSE
      )
    }
    return(value)
  }
}
scratch_config_type_handler.function <- function(valid, option) {
  force(option)
  f.n <- valid
  function(value) {
    if (!is.function(value) || !(formals(value) %in% formals(f.n))) {
      stop(
        str_opt_invalid(option), " It must be a function, ",
        "taking ", paste_and(formals(f.n)), "for arguments."
      )
    }
    value
  }
}

# scratch_config_type_handler(c('a'), "my.option")('c')
# scratch_config_type_handler(c('a'), "my.option")('a')
#
# scratch_config_type_handler.character(c('a'), "my.option", FALSE)('c')
# scratch_config_type_handler.list(list('a', 1, TRUE), 'weird1', FALSE)('meh')
# scratch_config_type_handler.list(list(list('a', 'meh'), 1, TRUE), 'weird1')(FALSE)
# scratch_config_type_handler.list(list(NA_character_, rep(NA_integer_, 4), TRUE), 'weird1')('meh')

# # Each option should have a list of methods/slots
# value
# validate
# choices
#
