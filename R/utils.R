#' Make a data.table from input: coerce it or read it (if it is a file path) and ensure column names
#' @keywords internal
#' @importFrom data.table as.data.table fread
#' @import R.utils
.as.data.table <- function(input, colmap = NULL) {
  rval <- NULL
  if (is.character(input)) {
    if (!startsWith(input, "http") && !file.exists(input)) {
      stop("Data file does not exist: ", input)
    }
    rval <- data.table::fread(input)
  } else {
    rval <- data.table::as.data.table(input)
  }
  if (!is.null(rval) && !is.null(colmap)) {
    for (col in names(colmap)) {
      colnames(rval)[colnames(rval) == colmap[[col]]] <- col
    }
  }
  rval
}

#' Verbose option
#'
#' @param verbose Logical to set for having verbose messages
#' @return (Invisible) the return value of \link[base]{options}
#' @export
with_verbose <- function(verbose = TRUE) {
  options(recharge.verbose = isTRUE(verbose))
}

#' Progress option
#'
#' @param progress Logical to set for having a progress bar
#' @importFrom progressr handlers
#' @return (Invisible) the return value of \link[progressr]{handlers}
#' @export
with_progress <- function(progress = TRUE) {
  options(recharge.progress = isTRUE(progress))
  if (isTRUE(progress)) {
    handlers(global = TRUE)
    handlers("progress")
  } else {
    handlers(global = FALSE)
  }
}

#' Make a data.table from input: coerce it or read it (if it is a file path) and ensure column names
#' @keywords internal
.is.verbose <- function() {
  getOption("recharge.verbose", TRUE)
}
