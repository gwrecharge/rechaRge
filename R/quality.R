#' Simulation quality assessment
#' 
#' @param calibration The calibration parameters.
#' @param water_budget The computed water budget.
#' @param input_rcn_gauging The table with the list of RCN cells located in each gauging station watershed. Input can be a data.frame/data.table or a path to a data file.
#' @param observed_flow The flow rates in mm/day. Input can be a data.frame/data.table or a path to a data file.
#' @param alpha_lyne_hollick The Lyne and Hollick filter. Input can be a data.frame/data.table or a path to a data file.
#' @param simul_period The start and end years.
#'
#' @export
compute_simulation_quality_assessment <- function(calibration, water_budget, input_rcn_gauging, observed_flow, alpha_lyne_hollick, simul_period) {
  param <- calibration
  rcn_gauging <- .as.data.table(input_rcn_gauging)
  
  # 1.3-Simulation period ####
  year_start <- as.numeric(simul_period[1])
  year_end <- as.numeric(simul_period[2])
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
  T_snow <- 0 # threshold temperature rain/snow (°C)
  T_m <- param$T_m
  C_m <- param$C_m
  # 1.4.2-soil frost
  TT_F <- param$TT_F
  F_T <- param$F_T
  # 1.4.3-runoff
  t_API <- param$t_API
  f_runoff <- param$f_runoff
  # 1.4.4-soil parameters
  sw_m <- param$sw_m
  f_inf <- param$f_inf
  
  output <- list(
    gauging = c()
  )
  
  for (st in 1:length(gauging)) {
    # 1.7.1-load the modeled data per gauging station ####
    setkey(water_budget, rcn_cell)
    budget_month <- water_budget[.(rcn_gauging$cell_ID[which(rcn_gauging$gauging_stat == gauging[st])])]
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
