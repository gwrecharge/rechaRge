#' River flow processing
#'
#' Processing of the river flow to compute the baseflow with Lyne and Hollick and resample them into monthly observations by
#' (1) select the available river flow observations for the simulation period and fill the gaps (up to 5 missing days),
#' (2) extract the list of the available gauging stations for the simulation period (list of the names),
#' (3) compute the Lyne and Hollick baseflow and resample river flow and baseflow with a monthly time step.
#'
#' @param observed_flow The flow rates in mm/day.
#' @param alpha_lyne_hollick The Lyne and Hollick filter.
#'
#' @return A list of observed_flow_month and gauging
#' @export
#'
#' @importFrom hydrostats baseflows
#'
#' @examples
process_river_flow <- function(observed_flow, alpha_lyne_hollick) {
  # 1-Observations data processing ####
  # 1.1-Select the observed flow for the simulation period and interpolate the gaps ####
  observed_flow_no_na <- observed_flow[, which(unlist(lapply(observed_flow, function(x) !all(is.na(x))))), with = F]

  if (ncol(observed_flow_no_na) < 4) {
    stop("error - no observed data on the simulation period")
  }

  for (c in 4:ncol(observed_flow_no_na)) {
    observed_flow_no_na[[c]][2:(nrow(observed_flow_no_na) - 1)] <- zoo::na.approx(observed_flow_no_na[[c]][2:(nrow(observed_flow_no_na) - 1)],
      maxgap = 5, na.rm = FALSE
    )
  } # fill up the gap in the observed flow up to 5 days
  observed_flow_no_na$date <- as.POSIXct(paste(observed_flow_no_na$year, observed_flow_no_na$month, observed_flow_no_na$day, sep = "-"),
    format = "%Y-%m-%d", tz = "UTC"
  )
  observed_flow_no_na <- observed_flow_no_na[, c(ncol(observed_flow_no_na), 1:3, 4:(ncol(observed_flow_no_na) - 1)), with = FALSE]
  observed_flow_month <- data.table::data.table(
    year = c(rep(unique(observed_flow_no_na$year), each = 12)),
    month = c(rep(c(1:12), length(unique(observed_flow_no_na$year))))
  )

  # 1.2-List of the available gauging station for the simulation period ####
  if (ncol(observed_flow_no_na) > 4) {
    gauging <- as.numeric(colnames(observed_flow_no_na)[5:ncol(observed_flow_no_na)])
  }

  # 1.3-compute baseflow with Lyne and Hollick (alpha calibrated independently) ####
  if (ncol(observed_flow_no_na) < 5) {
    stop("error - no observed river flow on the simulation period - baseflow computation impossible")
  }

  for (c in 5:ncol(observed_flow_no_na)) {
    # c<-5
    bf <- observed_flow_no_na[, c(1:3, c), with = FALSE]
    bf <- na.contiguous(bf) # select the longest period without NA
    colnames(bf) <- c("Date", "year", "month", "Q")
    bf$bf_lh <- baseflows(bf[, c(1, 4), with = FALSE],
      alpha_lyne_hollick$alpha[which(alpha_lyne_hollick$station == colnames(observed_flow_no_na)[c])],
      n.reflected = 30, ts = "daily"
    )[, 3]
    q_month <- bf[, .(qmonth = sum(Q, na.rm = TRUE), bf_lh_month = sum(bf_lh, na.rm = TRUE)), .(year, month)]
    colnames(q_month)[3:4] <- c(colnames(observed_flow_no_na)[c], paste(colnames(observed_flow_no_na)[c], "_bf", sep = ""))
    observed_flow_month <- merge(observed_flow_month, q_month, by = c("year", "month"), all.x = TRUE)
  }

  # returned value
  list(
    observed_flow_month = observed_flow_month,
    gauging = gauging
  )
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
  pb$tick(0, tokens = list(what = ''))
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
  if (getOption("recharge.progress", interactive())) progress$update(ratio = step/total, tokens = tokens)
}
