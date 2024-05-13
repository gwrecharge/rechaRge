#' Simulation quality assessment
#'
#' Evaluates the simulated water budget with the average KGE.
#'
#' The columns of the water budget data set input are:
#' * **year**
#' * **month**
#' * **vi**
#' * **t_mean**
#' * **runoff**
#' * **pet**
#' * **aet**
#' * **gwr**
#' * **runoff_2**
#' * **delta_reservoir**
#' * **rcn_id**
#'
#' The columns of the observed flow data set input are:
#' * **year**
#' * **month**
#' * **day**
#' * **one column per station** (named by the station ID), the flow rates in mm/day
#'
#' The columns of the RCN gauging stations data set input are:
#' * **rcn_id**, the cell ID
#' * **station_id**, the station ID
#'
#' The columns of the Lyne and Hollick filter data set input are:
#' * **station_id**, the station ID
#' * **alpha**
#'
#' @param obj The HydroBudget object with calibration parameters and column names mappings.
#' @param water_budget The computed water budget. Input can be a data.frame/data.table or a path to a data file.
#' @param rcn_gauging The table with the list of RCN cells located in each gauging station watershed. Input can be a data.frame/data.table or a path to a data file.
#' @param observed_flow The flow rates in mm/day. Input can be a data.frame/data.table or a path to a data file.
#' @param alpha_lyne_hollick The Lyne and Hollick filter. Input can be a data.frame/data.table or a path to a data file.
#' @param period The start and end years. If not provided, the start/end years will be extracted from the water budget data.
#' @return The HydroBudget quality assessment.
#'
#' @importFrom data.table setkey
#' @rdname evaluate_simulation_quality
#' @method evaluate_simulation_quality hydrobudget
#' @export
#'
#' @examples
#' \dontrun{
#' # Use input example files provided by the package
#' base_url <- "https://github.com/gwrecharge/rechaRge-book/raw/main/examples/input/"
#' input_rcn_gauging <- paste0(base_url, "rcn_gauging.csv.gz")
#' input_observed_flow <- paste0(base_url, "observed_flow.csv.gz")
#' input_alpha_lyne_hollick <- paste0(base_url, "alpha_lyne_hollick.csv.gz")
#'
#' # Calibration parameters
#' HB <- rechaRge::new_hydrobudget(
#'   T_m = 2.1, # melting temperature (°C)
#'   C_m = 6.2, # melting coefficient (mm/°C/d)
#'   TT_F = -17.6, # Threshold temperature for soil frost (°C)
#'   F_T = 16.4, # Freezing time (d)
#'   t_API = 3.9, # Antecedent precipitation index time (d)
#'   f_runoff = 0.63, # Runoff factor (-)
#'   sw_m = 431, # Maximum soil water content (mm)
#'   f_inf = 0.07 # infiltration factor (-)
#' )
#'
#' # ... compute the water budget ...
#'
#' result <- evaluate_simulation_quality(
#'   HB,
#'   water_budget = water_budget,
#'   rcn_gauging = input_rcn_gauging,
#'   observed_flow = input_observed_flow,
#'   alpha_lyne_hollick = input_alpha_lyne_hollick,
#'   period = simul_period
#' )
#' }
evaluate_simulation_quality.hydrobudget <- function(obj, water_budget, rcn_gauging, observed_flow, alpha_lyne_hollick, period = NULL, ...) {
  water_budget_data <- .as.data.table(water_budget)
  rcn_gauging_data <- .as.data.table(rcn_gauging, obj$rcn_gauging_columns)
  year_range <- period

  # Simulation period
  if (is.null(year_range)) {
    year_range <- c(min(water_budget_data$year), max(water_budget_data$year))
  } else if (length(year_range) == 1) {
    year_range <- append(year_range, max(water_budget_data$year))
  }
  year_start <- as.numeric(year_range[1])
  year_end <- as.numeric(year_range[2])
  if (year_end < year_start) {
    stop("Wrong simulation period, start year must be before end year")
  }

  # Load and filter observed flow
  observed_flow_ <- .as.data.table(observed_flow, obj$observed_flow_columns)
  list_year <- seq(year_start, year_end, 1)
  observed_flow_ <- observed_flow_[year %in% list_year]
  flow <- process_river_flow(obj, observed_flow_, alpha_lyne_hollick)
  observed_flow_month <- flow$observed_flow_month
  gauging <- flow$gauging

  # Calibration parameters
  # Snow model
  calibration_ <- obj$calibration
  T_snow <- calibration_$Tsnow
  T_m <- calibration_$T_m
  C_m <- calibration_$C_m
  # Soil frost
  TT_F <- calibration_$TT_F
  F_T <- calibration_$F_T
  # Runoff
  t_API <- calibration_$t_API
  f_runoff <- calibration_$f_runoff
  # Soil parameters
  sw_m <- calibration_$sw_m
  f_inf <- calibration_$f_inf

  output <- list(
    gauging = c()
  )

  for (st in 1:length(gauging)) {
    # Load the modeled data per gauging station
    setkey(water_budget_data, cols = "rcn_id")
    rcn_ids <- rcn_gauging_data$rcn_id[which(rcn_gauging_data$station_id == gauging[st])]
    if (length(rcn_ids) > 0) {
      budget_month <- water_budget_data[.(rcn_ids)]
      budget_month <- na.omit(budget_month, invert = FALSE)
      budget_month <- budget_month[, .(
        vi = mean(get("vi")),
        t_mean = mean(get("t_mean")),
        runoff = mean(get("runoff")),
        pet = mean(get("pet")),
        aet = mean(get("aet")),
        gwr = mean(get("gwr")),
        runoff_2 = mean(get("runoff_2")),
        delta_reservoir = mean(get("delta_reservoir"))
      ), .(year, month)]
      budget_month$station_id <- gauging[st]

      # Create the comparison data frame
      cols <- which(colnames(observed_flow_month) %in% c(
        "year", "month", as.character(unique(budget_month$station_id)),
        paste(as.character(unique(budget_month$station_id)), "_bf", sep = "")
      ))
      comparison_month <- merge(observed_flow_month[, cols, with = FALSE],
        budget_month[, 1:(ncol(budget_month) - 1), with = FALSE],
        by = c("year", "month"), all.x = TRUE
      )
      rm(cols)
      colnames(comparison_month)[1:4] <- c("year", "month", "q", "qbase")

      # Save the simulation results by gauging station
      output$gauging <- append(output$gauging, list(list(gauging = gauging[st], comparison_month = comparison_month)))

      # Objective functions
      # combining the error indicators in a data table
      error_ind <- matrix(ncol = 2, nrow = 2)
      colnames(error_ind) <- c("month_cal", "month_val")
      rownames(error_ind) <- c("KGE_qtot", "KGE_baseflow")
      # defining the periods of data for the observed river flow and base flow per gauging station and separate it in
      # first 2/3 = calibration period // last 1/3 = validation period
      flow_beg <- min(na.contiguous(comparison_month[, 1:3])[, 1])
      flow_end <- max(na.contiguous(comparison_month[, 1:3])[, 1])
      ifelse(flow_beg < year_start, flow_beg <- year_start, flow_beg <- as.numeric(flow_beg))
      ifelse(flow_end > year_end, flow_end <- year_end, flow_end <- as.numeric(flow_end))
      calibration_start <- flow_beg
      calibration_end <- flow_beg + round((flow_end - flow_beg) * 2 / 3)
      validation_start <- calibration_end + 1
      validation_end <- flow_end
      # defining the modeled and observed on each period for flow and baseflow
      m_q_month_cal <- (comparison_month$runoff[which(comparison_month$year %in% c(calibration_start:calibration_end))] +
        comparison_month$runoff_2[which(comparison_month$year %in% c(calibration_start:calibration_end))] +
        comparison_month$gwr[which(comparison_month$year %in% c(calibration_start:calibration_end))])
      o_q_month_cal <- comparison_month[[3]][which(comparison_month$year %in% c(calibration_start:calibration_end))]
      m_q_month_val <- (comparison_month$runoff[which(comparison_month$year %in% c(validation_start:validation_end))] +
        comparison_month$runoff_2[which(comparison_month$year %in% c(validation_start:validation_end))] +
        comparison_month$gwr[which(comparison_month$year %in% c(validation_start:validation_end))])
      o_q_month_val <- comparison_month[[3]][which(comparison_month$year %in% c(validation_start:validation_end))]

      m_baseflow_month_cal <- comparison_month$gwr[which(comparison_month$year %in% c(calibration_start:calibration_end))]
      o_baseflow_month_cal <- comparison_month[[4]][which(comparison_month$year %in% c(calibration_start:calibration_end))]
      m_baseflow_month_val <- comparison_month$gwr[which(comparison_month$year %in% c(validation_start:validation_end))]
      o_baseflow_month_val <- comparison_month[[4]][which(comparison_month$year %in% c(validation_start:validation_end))]
      # KGE
      error_ind[1, 1] <- KGE(m_q_month_cal[which(!is.na(o_q_month_cal))],
        o_q_month_cal[which(!is.na(o_q_month_cal))])
      error_ind[1, 2] <- KGE(m_q_month_val[which(!is.na(o_q_month_val))],
        o_q_month_val[which(!is.na(o_q_month_val))])

      error_ind[2, 1] <- KGE(m_baseflow_month_cal[which(!is.na(o_baseflow_month_cal))],
        o_baseflow_month_cal[which(!is.na(o_baseflow_month_cal))])
      error_ind[2, 2] <- KGE(m_baseflow_month_val[which(!is.na(o_baseflow_month_val))],
        o_baseflow_month_val[which(!is.na(o_baseflow_month_val))])
      # Clean
      rm(
        m_q_month_cal, o_q_month_cal, m_q_month_val, o_q_month_val, m_baseflow_month_cal, o_baseflow_month_cal,
        m_baseflow_month_val, o_baseflow_month_val
      )

      # Write the simulation metadata in a datatable and save it
      if (st == 1) {
        simulation_metadata <- data.table(
          station_id = unique(budget_month$station_id),
          cal_beg = calibration_start,
          cal_end = calibration_end,
          val_beg = validation_start,
          val_end = validation_end,
          T_snow = T_snow,
          T_m = T_m,
          C_m = C_m,
          TT_F = TT_F,
          F_T = F_T,
          t_API = t_API,
          f_runoff = f_runoff,
          sw_m = sw_m,
          f_inf = f_inf,
          KGE_qtot_cal = error_ind[1, 1], KGE_qbase_cal = error_ind[2, 1],
          KGE_qtot_val = error_ind[1, 2], KGE_qbase_val = error_ind[2, 2],
          qtot_sim = mean(budget_month[
            , .(qtot = sum(get("runoff") + get("runoff_2") + get("gwr"), na.rm = TRUE)),
            .(year)
          ][[2]]),
          aet_sim = mean(budget_month[
            , .(aet = sum(get("aet"), na.rm = TRUE)),
            .(year)
          ][[2]]),
          gwr_sim = mean(budget_month[
            , .(gwr = sum(get("gwr"), na.rm = TRUE)),
            .(year)
          ][[2]]),
          time = format(Sys.time(), "%Y_%m_%d-%H_%M"),
          KGE_mean_cal = NA_real_,
          KGE_mean_val = NA_real_
        )
      } else {
        simulation_metadata <- rbind(
          simulation_metadata,
          data.table(
            station_id = unique(budget_month$station_id),
            cal_beg = calibration_start,
            cal_end = calibration_end,
            val_beg = validation_start,
            val_end = validation_end,
            T_snow = T_snow,
            T_m = T_m,
            C_m = C_m,
            TT_F = TT_F,
            F_T = F_T,
            t_API = t_API,
            f_runoff = f_runoff,
            sw_m = sw_m,
            f_inf = f_inf,
            KGE_qtot_cal = error_ind[1, 1],
            KGE_qbase_cal = error_ind[2, 1],
            KGE_qtot_val = error_ind[1, 2],
            KGE_qbase_val = error_ind[2, 2],
            qtot_sim = mean(budget_month[
              , .(qtot = sum(get("runoff") + get("runoff_2") + get("gwr"), na.rm = TRUE)),
              .(year)
            ][[2]]),
            aet_sim = mean(budget_month[
              , .(aet = sum(get("aet"), na.rm = TRUE)),
              .(year)
            ][[2]]),
            gwr_sim = mean(budget_month[
              , .(gwr = sum(get("gwr"), na.rm = TRUE)),
              .(year)
            ][[2]]),
            time = format(Sys.time(), "%Y_%m_%d-%H_%M"),
            KGE_mean_cal = NA_real_,
            KGE_mean_val = NA_real_
          )
        )
      }
      if (st == length(gauging)) {
        simulation_metadata[nrow(simulation_metadata), c("KGE_mean_cal", "KGE_mean_val") := as.list(c(mean((simulation_metadata$KGE_qtot_cal + simulation_metadata$KGE_qbase_cal) / 2),
          mean((simulation_metadata$KGE_qtot_val + simulation_metadata$KGE_qbase_val) / 2),
          na.rm = TRUE
        ))[1:2]]
        output$simulation_metadata <- simulation_metadata
      }
      rm(calibration_start, calibration_end, validation_start, validation_end, flow_beg, flow_end)
    }
  }
  rm(st, comparison_month)

  output
}

#' River flow processing
#'
#' Processing of the river flow to compute the baseflow with Lyne and Hollick and resample them into monthly observations by:
#' 1. select the available river flow observations for the simulation period and fill the gaps (up to 5 missing days),
#' 2. extract the list of the available gauging stations for the simulation period (list of the names),
#' 3. compute the Lyne and Hollick baseflow and resample river flow and baseflow with a monthly time step.
#'
#' @param obj The HydroBudget object with calibration parameters and column names mappings.
#' @param observed_flow The flow rates in mm/day. Input can be a data.frame/data.table or a path to a data file.
#' @param alpha_lyne_hollick The Lyne and Hollick filter. Input can be a data.frame/data.table or a path to a data file.
#'
#' @return A list of observed_flow_month and gauging
#' @keywords internal
#'
#' @importFrom hydrostats baseflows
process_river_flow <- function(obj, observed_flow, alpha_lyne_hollick) {
  # Read or coerce input data
  observed_flow_ <- .as.data.table(observed_flow, obj$observed_flow_columns)
  alpha_lyne_hollick_ <- .as.data.table(alpha_lyne_hollick, obj$alpha_lyne_hollick_columns)

  # Observations data processing
  # Select the observed flow for the simulation period and interpolate the gaps
  observed_flow_no_na <- observed_flow_[, which(unlist(lapply(observed_flow_, function(x) !all(is.na(x))))), with = F]

  if (ncol(observed_flow_no_na) < 4) {
    stop("error - no observed data on the simulation period")
  }

  # Station IDs are the unknown columns
  get_station_ids <- function(flw) {
    station_ids <- colnames(observed_flow_no_na)
    station_ids <- station_ids[!(station_ids %in% c("year", "month", "day", "date"))]
    # station IDs must have an alpha lyne hollick
    station_ids_alpha <- unique(as.character(alpha_lyne_hollick_$station))
    station_ids[station_ids %in% station_ids_alpha]
  }
  station_ids <- get_station_ids(observed_flow_no_na)

  row_count <- nrow(observed_flow_no_na) - 1
  for (station_id in station_ids) {
    observed_flow_no_na[[station_id]][2:row_count] <- zoo::na.approx(observed_flow_no_na[[station_id]][2:row_count],
      maxgap = 5, na.rm = FALSE
    )
  } # Fill up the gap in the observed flow up to 5 days

  # Add date column and move it to 1st position
  observed_flow_no_na$date <- as.POSIXct(paste(observed_flow_no_na$year, observed_flow_no_na$month, observed_flow_no_na$day, sep = "-"),
    format = "%Y-%m-%d", tz = "UTC"
  )
  observed_flow_no_na <- observed_flow_no_na[, c(ncol(observed_flow_no_na), 1:(ncol(observed_flow_no_na) - 1)), with = FALSE]

  observed_flow_month <- data.table::data.table(
    year = c(rep(unique(observed_flow_no_na$year), each = 12)),
    month = c(rep(c(1:12), length(unique(observed_flow_no_na$year))))
  )

  # List of the available gauging station for the simulation period
  if (ncol(observed_flow_no_na) > 4) {
    station_ids <- get_station_ids(observed_flow_no_na)
    # FIXME why numeric ID?
    gauging <- as.numeric(station_ids)
  }

  # Compute baseflow with Lyne and Hollick (alpha calibrated independently)
  if (ncol(observed_flow_no_na) < 5) {
    stop("error - no observed river flow on the simulation period - baseflow computation impossible")
  }

  for (station_id in station_ids) {
    bf <- observed_flow_no_na[, c("date", "year", "month", station_id), with = FALSE]
    bf <- na.contiguous(bf) # select the longest period without NA
    colnames(bf) <- c("Date", "year", "month", "Q")
    bf$bf_lh <- baseflows(bf[, c(1, 4), with = FALSE],
      alpha_lyne_hollick_$alpha[which(alpha_lyne_hollick_$station == station_id)],
      n.reflected = 30, ts = "daily"
    )[, 3]
    q_month <- bf[, .(qmonth = sum(get("Q"), na.rm = TRUE),
                      bf_lh_month = sum(get("bf_lh"), na.rm = TRUE)), .(year, month)]
    colnames(q_month)[3:4] <- c(station_id, paste(station_id, "_bf", sep = ""))
    observed_flow_month <- merge(observed_flow_month, q_month, by = c("year", "month"), all.x = TRUE)
  }

  # Returned value
  list(
    observed_flow_month = observed_flow_month,
    gauging = gauging
  )
}
