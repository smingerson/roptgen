not_in <- function(value, x) {
  !value %in% x
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
