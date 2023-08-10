#' Simulation quality assessment
#' 
#' Evaluates the simulated water budget with the average KGE.
#' 
#' The columns of the water budget data set input are:
#' * **year**
#' * **month**
#' * **VI**
#' * **t_mean**
#' * **runoff**
#' * **pet**
#' * **aet**
#' * **gwr**
#' * **runoff_2**
#' * **delta_reservoir**
#' * **rcn_cell**
#' 
#' The columns of the observed flow data set input are:
#' * **year**
#' * **month**
#' * **day**
#' * **one column per station** (named by the station ID), the flow rates in mm/day
#' 
#' The columns of the RCN gauging stations data set input are:
#' * **cell_ID**, the cell ID
#' * **gauging_stat**, the station ID
#' 
#' The columns of the Lyne and Hollick filter data set input are:
#' * **station**, the station ID
#' * **alpha**
#' 
#' @param calibration The calibration parameters.
#' @param water_budget The computed water budget. Input can be a data.frame/data.table or a path to a data file.
#' @param rcn_gauging The table with the list of RCN cells located in each gauging station watershed. Input can be a data.frame/data.table or a path to a data file.
#' @param observed_flow The flow rates in mm/day. Input can be a data.frame/data.table or a path to a data file.
#' @param alpha_lyne_hollick The Lyne and Hollick filter. Input can be a data.frame/data.table or a path to a data file.
#' @param period The start and end years. If not provided, the start/end years will be extracted from the water budget data.
#'
#' @importFrom hydroGOF KGE
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' # Use input example files provided by the package
#' examples_dir <- system.file("examples", package = "rechaRge")
#' observed_flow <- file.path(examples_dir, "input", "observed_flow.csv.gz") # flow rates in mm/d
#' alpha_lyne_hollick <- file.path(examples_dir, "input", "alpha_lyne_hollick.csv.gz")
#' 
#' # ... compute the water budget ...
#'
#' result <- compute_simulation_quality_assessment(
#'   calibration = param, 
#'   water_budget = water_budget,
#'   rcn_gauging = input_rcn_gauging, 
#'   observed_flow = observed_flow,
#'   alpha_lyne_hollick = alpha_lyne_hollick, 
#'   period = simul_period)
#' }
compute_simulation_quality_assessment <- function(calibration, water_budget, rcn_gauging, observed_flow, alpha_lyne_hollick, period = NULL) {
  param <- calibration
  water_budget_data <- .as.data.table(water_budget)
  rcn_gauging_data <- .as.data.table(rcn_gauging)
  year_range <- period
  
  
  # 1.3-Simulation period ####
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
  
  # load and filter observed flow
  observed_flow_ <- .as.data.table(observed_flow)
  list_year <- seq(year_start, year_end, 1)
  observed_flow_ <- observed_flow_[year %in% list_year]
  flow <- process_river_flow(observed_flow_, alpha_lyne_hollick)
  observed_flow_month <- flow$observed_flow_month
  gauging <- flow$gauging
  
  # 1.4-Calibration parameters ####
  # 1.4.1-Snow model
  calibration_ <- make_calibration_parameters(calibration)
  T_snow <- calibration_$Tsnow
  T_m <- calibration_$T_m
  C_m <- calibration_$C_m
  # 1.4.2-soil frost
  TT_F <- calibration_$TT_F
  F_T <- calibration_$F_T
  # 1.4.3-runoff
  t_API <- calibration_$t_API
  f_runoff <- calibration_$f_runoff
  # 1.4.4-soil parameters
  sw_m <- calibration_$sw_m
  f_inf <- calibration_$f_inf
  
  output <- list(
    gauging = c()
  )
  
  for (st in 1:length(gauging)) {
    # 1.7.1-load the modeled data per gauging station ####
    setkey(water_budget_data, rcn_cell)
    budget_month <- water_budget_data[.(rcn_gauging_data$cell_ID[which(rcn_gauging_data$gauging_stat == gauging[st])])]
    budget_month <- na.omit(budget_month, invert = FALSE)
    budget_month <- budget_month[, .(
      VI = mean(VI),
      t_mean = mean(t_mean),
      runoff = mean(runoff),
      pet = mean(pet),
      aet = mean(aet),
      gwr = mean(gwr),
      runoff_2 = mean(runoff_2),
      delta_reservoir = mean(delta_reservoir)
    ), .(year, month)]
    budget_month$gauging_stat <- gauging[st]
    
    # 1.7.2-Create the comparison data frame ####
    cols <- which(colnames(observed_flow_month) %in% c(
      "year", "month", as.character(unique(budget_month$gauging_stat)),
      paste(as.character(unique(budget_month$gauging_stat)), "_bf", sep = "")
    ))
    comparison_month <- merge(observed_flow_month[, ..cols],
                              budget_month[, 1:(ncol(budget_month) - 1), with = FALSE],
                              by = c("year", "month"), all.x = TRUE
    )
    rm(cols)
    colnames(comparison_month)[1:4] <- c("year", "month", "q", "qbase")
    
    # 1.7.3-save the simulation results by gauging station ####
    output$gauging <- append(output$gauging, list(list(gauging = gauging[st], comparison_month = comparison_month)))
    
    # 1.7.4-Objective functions ####
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
                           o_q_month_cal[which(!is.na(o_q_month_cal))],
                           na.rm = TRUE
    )
    error_ind[1, 2] <- KGE(m_q_month_val[which(!is.na(o_q_month_val))],
                           o_q_month_val[which(!is.na(o_q_month_val))],
                           na.rm = TRUE
    )
    
    error_ind[2, 1] <- KGE(m_baseflow_month_cal[which(!is.na(o_baseflow_month_cal))],
                           o_baseflow_month_cal[which(!is.na(o_baseflow_month_cal))],
                           na.rm = TRUE
    )
    error_ind[2, 2] <- KGE(m_baseflow_month_val[which(!is.na(o_baseflow_month_val))],
                           o_baseflow_month_val[which(!is.na(o_baseflow_month_val))],
                           na.rm = TRUE
    )
    # Clean
    rm(
      m_q_month_cal, o_q_month_cal, m_q_month_val, o_q_month_val, m_baseflow_month_cal, o_baseflow_month_cal,
      m_baseflow_month_val, o_baseflow_month_val
    )
    
    # 1.7.5-write the simulation metadata in a datatable and save it ####
    if (st == 1) {
      simulation_metadata <- data.table(
        gauging_stat = unique(budget_month$gauging_stat),
        cal_beg = calibration_start, cal_end = calibration_end, val_beg = validation_start, val_end = validation_end,
        T_snow = T_snow, T_m = T_m, C_m = C_m, TT_F = TT_F, F_T = F_T, t_API = t_API,
        f_runoff = f_runoff, sw_m = sw_m, f_inf = f_inf,
        KGE_qtot_cal = error_ind[1, 1], KGE_qbase_cal = error_ind[2, 1],
        KGE_qtot_val = error_ind[1, 2], KGE_qbase_val = error_ind[2, 2],
        qtot_sim = mean(budget_month[
          , .(qtot = sum(runoff + runoff_2 + gwr, na.rm = TRUE)),
          .(year)
        ][[2]]),
        aet_sim = mean(budget_month[
          , .(aet = sum(aet, na.rm = TRUE)),
          .(year)
        ][[2]]),
        gwr_sim = mean(budget_month[
          , .(gwr = sum(gwr, na.rm = TRUE)),
          .(year)
        ][[2]]),
        time = format(Sys.time(), "%Y_%m_%d-%H_%M"),
        KGE_mean_cal = NA_real_, KGE_mean_val = NA_real_
      )
    } else {
      simulation_metadata <- rbind(
        simulation_metadata,
        data.table(
          gauging_stat = unique(budget_month$gauging_stat),
          cal_beg = calibration_start, cal_end = calibration_end, val_beg = validation_start, val_end = validation_end,
          T_snow = T_snow, T_m = T_m, C_m = C_m, TT_F = TT_F, F_T = F_T, t_API = t_API,
          f_runoff = f_runoff, sw_m = sw_m, f_inf = f_inf,
          KGE_qtot_cal = error_ind[1, 1], KGE_qbase_cal = error_ind[2, 1],
          KGE_qtot_val = error_ind[1, 2], KGE_qbase_val = error_ind[2, 2],
          qtot_sim = mean(budget_month[
            , .(qtot = sum(runoff + runoff_2 + gwr, na.rm = TRUE)),
            .(year)
          ][[2]]),
          aet_sim = mean(budget_month[
            , .(aet = sum(aet, na.rm = TRUE)),
            .(year)
          ][[2]]),
          gwr_sim = mean(budget_month[
            , .(gwr = sum(gwr, na.rm = TRUE)),
            .(year)
          ][[2]]),
          time = format(Sys.time(), "%Y_%m_%d-%H_%M"),
          KGE_mean_cal = NA_real_, KGE_mean_val = NA_real_
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
#' @param observed_flow The flow rates in mm/day. Input can be a data.frame/data.table or a path to a data file.
#' @param alpha_lyne_hollick The Lyne and Hollick filter. Input can be a data.frame/data.table or a path to a data file.
#'
#' @return A list of observed_flow_month and gauging
#' @keywords internal
#'
#' @importFrom hydrostats baseflows
process_river_flow <- function(observed_flow, alpha_lyne_hollick) {
  # 0-Read or coerce input data
  observed_flow_ <- .as.data.table(observed_flow)
  alpha_lyne_hollick_ <- .as.data.table(alpha_lyne_hollick)
  # TODO check data structures
  
  # 1-Observations data processing ####
  # 1.1-Select the observed flow for the simulation period and interpolate the gaps ####
  observed_flow_no_na <- observed_flow_[, which(unlist(lapply(observed_flow_, function(x) !all(is.na(x))))), with = F]
  
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
                          alpha_lyne_hollick_$alpha[which(alpha_lyne_hollick_$station == colnames(observed_flow_no_na)[c])],
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

