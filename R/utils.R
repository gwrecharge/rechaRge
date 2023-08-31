#' Make a data.table from input: coerce it or read it (if it is a file path)
#' @keywords internal
#' @importFrom data.table as.data.table fread
#' @import R.utils
.as.data.table <- function(input) {
  if (is.character(input)) {
    if (!file.exists(input)) {
      stop("Data file does not exist: ", input)
    }
    data.table::fread(input)
  } else {
    data.table::as.data.table(input)
  }
}

#' Make the computation cluster
#' @keywords internal
.make_cluster <- function(nb_core) {
  cluster <- makeCluster(nb_core) # if on a computer with windows
  registerDoParallel(cluster) # if on a computer with windows
  # registerDoMC(nb_core)        #if on a server - DoParallel does not work, use that instead (doMC)
  cluster
}

#' Stop the computation cluster
#' @keywords internal
.stop_cluster <- function(cluster) {
  stopCluster(cluster) # to be muted if doMC is used instead of doParallel
}

#' Create a new progress instance with default settings.
#' @import progress
#' @keywords internal
.newProgress <- function(format = "  :what [:bar] :current/:total | :elapsed", clear = getOption("recharge.progress.clear", !interactive()), total, width = 100) {
  pb <- progress::progress_bar$new(format = format, clear = clear, total = total, width = width, show_after = 0)
  pb$tick(0, tokens = list(what = ""))
  pb
}

#' Update and increment the progress status if option "recharge.progress" is TRUE.
#' @keywords internal
.tickProgress <- function(progress, tokens = list()) {
  if (getOption("recharge.progress", interactive())) progress$tick(tokens = tokens)
}

#' Update the progress status if option "recharge.progress" is TRUE.
#' @keywords internal
.updateProgress <- function(progress, step, total, tokens = list()) {
  if (getOption("recharge.progress", interactive())) progress$update(ratio = step / total, tokens = tokens)
}
