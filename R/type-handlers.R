#' @include utils.R
roptgen_validate <- function(valid, option, ...) {
  UseMethod("roptgen_validate")
}

roptgen_validate_base <- function(valid, option, ...) {
  if (all(valid == 0)) {
    return(function(value) {
      if (is_wrong_prototype(value, valid)) {
        stop_invalid_vec(option, valid)
      }
    })
  }
  function(value) {
    if (not_in(value, valid)) {
      stop_invalid_choice(option, valid)
    }
    value
  }
}
roptgen_validate.default <- function(valid, option, ...) {
  if (is.null(valid)) {
    return(option)
  }
  stop("A validation handler does not exist for this type/class.")
}
roptgen_validate.double <- roptgen_validate_base
roptgen_validate.logical <- roptgen_validate_base
roptgen_validate.integer <- roptgen_validate_base
roptgen_validate.character <- function(valid, option, partial, ...) {
  if (all(valid == "")) {
    return(function(value) {
      if (is_wrong_prototype(value, valid)) {
        stop_invalid_vec(option, valid)
      }
    })
  }
  function(value) {
    # If we are provided a vector of choices, presumption is we only pick 1
    if (length(value) > 1) {
      stop_invalid_choice(option, dQuote(valid))
    }
    if (partial) {
      has_match <- pmatch(value, valid)
      if (!is.na(has_match)) {
        value <- valid[has_match]
      }
    }
    if (not_in(value, valid)) {
      stop_invalid_choice(option, dQuote(valid))
    }
    value
  }
}

roptgen_validate.list <- function(valid, option, ...) {
  # return a function which checks each condition, as long as one is true we are good
  type_handlers <- vector(mode = "list", length(valid))
  for (k in seq_along(type_handlers)) {
    type_handlers[[k]] <- roptgen_validate(valid[[k]], option, ...)
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
roptgen_validate.function <- function(valid, option) {
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

# roptgen_validate(c('a'), "my.option")('c')
# roptgen_validate(c('a'), "my.option")('a')
#
# roptgen_validate.character(c('a'), "my.option", FALSE)('c')
# roptgen_validate.list(list('a', 1, TRUE), 'weird1', FALSE)('meh')
# roptgen_validate.list(list(list('a', 'meh'), 1, TRUE), 'weird1')(FALSE)
# roptgen_validate.list(list(NA_character_, rep(NA_integer_, 4), TRUE), 'weird1')('meh')

# # Each option should have a list of methods/slots
# value
# validate
# choices
#
