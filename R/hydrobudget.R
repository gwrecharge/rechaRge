# HydroBudget function


#' Simulation using HydroBudget model
#'
#' HydroBudget is a spatially distributed GWR model that computes a superficial water budget on
#' grid cells of regional-scale watersheds. Runoff, actual evapotranspiration (AET), and potential
#' GWR are simulated for each grid cell, with a monthly time step, and fluxes do not
#' transfer from a cell to another (no water routing). The model inputs are distributed daily
#' precipitation and temperature as well as distributed data of pedology, land cover, and slope.
#'
#' @param param The calibration parameters.
#' @param input_rcn The RCN values.
#' @param input_rcn_gauging The table with the list of RCN cells located in each gauging station watershed.
#' @param input_climate The daily total precipitation (mm/d) and average daily temperature (°C).
#' @param simul_period The start and end years.
#' @param observed_flow_month The observed river flow (mm/month).
#' @param gauging The gauging.
#' @param nb_core The number of cores to use in the parallel computations. If not provided, all cores minus one will be used.
#' @param output_dir The output directory where result files will be written. Default is current working directory.
#'
#' @return The output directory path.
#' @export
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom lubridate yday year month
#' @importFrom data.table data.table fwrite setkey :=
#' @importFrom foreach foreach %dopar%
#' @importFrom hydroGOF KGE
#' @importFrom raster rasterFromXYZ setMinMax writeRaster
#' @importFrom sp coordinates
#' @importFrom zoo rollmean rollsum
#' @importFrom stats na.contiguous na.omit
#' @importFrom airGR PE_Oudin
#' @importFrom plyr .
#'
#' @examples
compute_hydrobudget <- function(param, input_rcn, input_rcn_gauging, input_climate, simul_period, observed_flow_month, gauging, nb_core = NULL, output_dir = getwd()) {
  gc()
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1.1-Load the input data ####
  param <- param
  input_rcn <- input_rcn
  input_rcn_gauging <- input_rcn_gauging
  climate_data <- input_climate
  simul_period <- simul_period
  nb_core <- nb_core
  observed_flow_month <- observed_flow_month
  gauging <- gauging

  # 1.2-cluster parameters ####
  ifelse(is.null(nb_core),
         nb_core <- detectCores() - 1,
         nb_core <- as.numeric(nb_core)
  )

  # 1.3-Simulation period ####
  year_start <- simul_period[1]
  year_end <- simul_period[2]

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
  sw_init <- 50

  # 1.5-execute the snow model and compute Oudin PET ####
  # 1.5.1-Parallel loop ####
  cluster <- makeCluster(nb_core) # if on a computer with windows
  registerDoParallel(cluster) # if on a computer with windows
  # registerDoMC(nb_core)        #if on a server - DoParallel does not work, use that instead (doMC)

  climate_data_VI <- foreach(k = 1:(length(unique(climate_data$climate_cell))), .combine = rbind, .inorder = FALSE) %dopar% {
    # 1.5.1.1-load the packages ####
    requireNamespace("data.table")
    requireNamespace("airGR")

    # 1.5.1.2-loop for the snowpack ####
    cellgrid <- as.character(unique(climate_data$climate_cell)[k])
    input_dd <- climate_data[climate_cell == cellgrid]
    input_dd$snow <- ifelse(input_dd$t_mean < T_snow, input_dd$p_tot, 0)
    input_dd$rain <- ifelse(input_dd$t_mean > T_snow, input_dd$p_tot, 0)
    input_dd$dd <- ifelse(input_dd$t_mean - T_m > 0, input_dd$t_mean - T_m, 0)
    input_dd$melt <- NA
    input_dd$storage <- NA
    for (y in 1:nrow(input_dd)) {
      # y<-1
      input_dd$melt[y] <- ifelse(y == 1, ifelse(input_dd$dd[y] * C_m < 0, input_dd$dd[y] * C_m, 0),
        ifelse(input_dd$dd[y] * C_m < input_dd$storage[y - 1],
          input_dd$dd[y] * C_m,
          input_dd$storage[y - 1]
        )
      )
      input_dd$storage[y] <- ifelse(y == 1,
        ifelse(0 + input_dd$snow[y] - input_dd$melt[y] > 0,
          0 + input_dd$snow[y] - input_dd$melt[y], 0
        ),
        ifelse(input_dd$storage[y - 1] + input_dd$snow[y] - input_dd$melt[y] > 0,
          input_dd$storage[y - 1] + input_dd$snow[y] - input_dd$melt[y], 0
        )
      )
    }
    # 1.5.1.3-compute Oudin PET ####
    input_dd$PET <- round(PE_Oudin(
      JD = yday(as.POSIXct(paste(input_dd$year, input_dd$month, input_dd$day, sep = "-"), format = "%Y-%m-%d")),
      Temp = input_dd$t_mean,
      Lat = unique(input_dd$lat),
      # the name of the cells is long-lat binded, so the last 3 numbers are the latitude with 1 digit
      LatUnit = "deg", TimeStepIn = "daily", TimeStepOut = "daily"
    ), 2)
    rm(y)
    input_dd
  }
  stopCluster(cluster) # to be muted if doMC is used instead of doParallel

  # 1.5.2-Compute vertical inflow (VI) ####
  climate_data_VI$VI <- climate_data_VI$rain + climate_data_VI$melt
  climate_data <- climate_data_VI[, c(1:6, 14, 13)]

  # 1.5.3-Clean ####
  rm(climate_data_VI, cluster)
  gc()

  # 1.6-run HB water budget partitioning ####
  # 1.6.1-Model loop by grid cell in parallel #####
  unique_rcn_cell <- unique(input_rcn$cell_ID)

  cluster <- makeCluster(nb_core)
  registerDoParallel(cluster)
  # registerDoMC(nb_core)          #if on a server - DoParallel does not work, use that instead (doMC)

  water_budget <- foreach(j = 1:(length(unique_rcn_cell)), .combine = rbind, .inorder = FALSE) %dopar% {
    requireNamespace("data.table")
    requireNamespace("zoo")
    # 1.6.1.1-subsets ####
    rcn_subset <- input_rcn[cell_ID == unique_rcn_cell[j]]
    rcn_climate <- merge(rcn_subset, climate_data, by = "climate_cell", all.x = TRUE)
    rcn_climate <- na.omit(rcn_climate[order(rcn_climate$climate_cell, rcn_climate$cell_ID, rcn_climate$year, rcn_climate$month, rcn_climate$day), ])

    # 1.6.1.2-Mean temperature function of F_T ####
    roll_mean_freez <- as.numeric(rollmean(as.numeric(rcn_climate$t_mean), F_T, fill = NA, na.pad = T, align = "right"))
    rcn_climate$temp_freez <- ifelse(is.na(roll_mean_freez), rcn_climate$t_mean, roll_mean_freez)
    rm(roll_mean_freez)

    # 1.6.1.3-API computation ####
    roll_sum_api <- as.numeric(rollsum(rcn_climate$VI, t_API, fill = NA, na.pad = T, align = "right"))
    rcn_climate$api <- ifelse(is.na(roll_sum_api), rcn_climate$VI, roll_sum_api)
    rm(roll_sum_api)

    # 1.6.1.4-RCN variations based on API ####
    rcn_api <- data.table(
      "year" = as.numeric(rcn_climate$year),
      "month" = as.numeric(rcn_climate$month),
      "day" = as.numeric(rcn_climate$day),
      "julian_day" = yday(as.POSIXct(paste(rcn_climate$year, rcn_climate$month, rcn_climate$day, sep = "-"), format = "%Y-%m-%d")),
      "temperature_moy" = as.numeric(rcn_climate$t_mean),
      "temp_freez" = as.numeric(rcn_climate$temp_freez),
      "rcn_II" = as.numeric(rcn_climate$RCNII * f_runoff),
      "api" = as.numeric(rcn_climate$api)
    )
    rcn_api$rcn3 <- ((-0.00563) * rcn_api$rcn_II^2) + (1.45535 * rcn_api$rcn_II) + 10.82878 # function from Monfet (1979)
    rcn_api$rcn1 <- (0.00865 * rcn_api$rcn_II^2) + (0.0148 * rcn_api$rcn_II) + 7.39846 # function from Monfet (1979)
    rcn_api$season <- ifelse(rcn_api$julian_day < 121 | rcn_api$julian_day > 283, 1, # for winter (value of 1)
      ifelse((rcn_api$julian_day > 120 & rcn_api$julian_day < 181) | (rcn_api$julian_day > 243 & rcn_api$julian_day < 284), 2, # for spring and fall (value of 2)
        0
      )
    ) # for summer (value of 0)
    rcn_api$rcn_wint <- ifelse(rcn_api$season == 1, ifelse(rcn_api$api > 22, rcn_api$rcn3, ifelse(rcn_api$api < 11, rcn_api$rcn1, rcn_api$rcn_II)), 0)
    rcn_api$rcn_spr_fall <- ifelse(rcn_api$season == 2, ifelse(rcn_api$api > 37, rcn_api$rcn3, ifelse(rcn_api$api < 18.5, rcn_api$rcn1, rcn_api$rcn_II)), 0)
    rcn_api$rcn_summ <- ifelse(rcn_api$season == 0, ifelse(rcn_api$api < 50, rcn_api$rcn1, ifelse(rcn_api$api > 80, rcn_api$rcn3, rcn_api$rcn_II)), 0)

    if (rcn_subset$RCNII == 100) { # is the RCNII value = 100 (water, wetlands)
      rcn_api$rcn_api <- ifelse(rcn_climate$temp_freez < TT_F, # is the soil frozen ?
        100, # if yes, RCN = 100 % (just runoff)
        10
      ) # if no, consider RCN = 10
    } else {
      rcn_api$rcn_api <- ifelse(rcn_climate$temp_freez < TT_F, # is the soil frozen ?
        100, # if yes, RCN = 100 % (just runoff)
        rcn_api$rcn_summ + rcn_api$rcn_spr_fall + rcn_api$rcn_wint
      )
    }
    rcn_climate$rcn_api <- rcn_api$rcn_api
    rm(rcn_api)

    # 1.6.1.5-Runoff computation ####
    sat <- (1000 / rcn_climate$rcn_api) - 10 # function from Monfet (1979)
    rcn_climate$runoff <- (rcn_climate$VI - (0.2 * sat))^2 / (rcn_climate$VI + (0.8 * sat)) # function from Monfet (1979)
    rcn_climate$runoff <- ifelse(rcn_climate$VI > (0.2 * sat), rcn_climate$runoff, 0) # function from Monfet (1979)

    # 1.6.1.6-Available water that reaches the soil reservoir ####
    rcn_climate$available_water <- ifelse((rcn_climate$VI - rcn_climate$runoff) < 0, 0, rcn_climate$VI - rcn_climate$runoff)

    # 1.6.1.7- AET and GWR computation ####
    budget1 <- vector()
    gwr <- vector()
    budget2 <- vector()
    pet_list <- rcn_climate$PET
    available_water_list <- rcn_climate$available_water
    aet <- vector()
    delta_reservoir <- vector()
    runoff_2 <- vector()
    for (ii in 1:nrow(rcn_climate)) {
      if (ii == 1) {
        budget1[ii] <- 0
        gwr[ii] <- 0
        budget2[ii] <- sw_init
        aet[ii] <- 0
        runoff_2 <- 0
      } else {
        budget1[ii] <- ifelse((budget2[ii - 1] + available_water_list[ii]) > sw_m, sw_m, budget2[ii - 1] + available_water_list[ii])
        aet[ii] <- min(budget1[ii], pet_list[ii])
        gwr[ii] <- ifelse(rcn_subset$RCNII == 100, # is the cell water or wetland?
          0, # if yes, no gwr
          (budget1[ii] - aet[ii]) * ((budget1[ii] / sw_m) * f_inf)
        ) # if not, normal gwr computation
        budget2[ii] <- budget1[ii] - aet[ii] - gwr[ii]
        runoff_2[ii] <- ifelse((available_water_list[ii] + budget1[ii]) > sw_m, (available_water_list[ii] + budget1[ii]) - sw_m, 0)
      }
    }
    rm(ii)
    delta_reservoir <- as.numeric(c(0, diff(budget2)))

    # 1.6.1.8- compute results ####
    w_b <- data.table(
      "climate_cell" = as.integer(rcn_climate$climate_cell),
      "rcn_cell" = as.integer(rcn_climate$ID),
      "year" = as.integer(rcn_climate$year),
      "month" = as.integer(rcn_climate$month),
      "day" = as.integer(rcn_climate$day),
      "VI" = as.numeric(rcn_climate$VI),
      "t_mean" = as.numeric(rcn_climate$t_mean),
      "runoff" = as.numeric(rcn_climate$runoff),
      # "available_water" = as.numeric(available_water$available_water),
      "pet" = as.numeric(rcn_climate$PET),
      # "vadose" =  as.numeric(budget2),
      "delta_reservoir" = as.numeric(delta_reservoir),
      "aet" = as.numeric(aet),
      "gwr" = as.numeric(gwr),
      "runoff_2" = as.numeric(runoff_2)
    )
    w_b <- w_b[, .(
      VI = sum(VI),
      t_mean = mean(t_mean),
      runoff = sum(runoff),
      # available_water = sum(available_water),
      pet = sum(pet),
      aet = sum(aet),
      gwr = sum(gwr),
      runoff_2 = sum(runoff_2),
      delta_reservoir = sum(delta_reservoir)
    ), .(year, month)]
    w_b[, (names(w_b)[3:ncol(w_b)]) := round(.SD, 1), .SDcols = names(w_b)[3:ncol(w_b)]]
    w_b[, rcn_cell := unique_rcn_cell[j]]
    w_b
  }
  stopCluster(cluster) # to be muted if doMC is used instead of doParallel

  # 1.6.2-write results ####
  fwrite(water_budget, file.path(output_dir, "01_bilan_spat_month.csv"))
  budget_unspat <- water_budget[, .(
    VI = mean(VI),
    t_mean = mean(t_mean),
    runoff = mean(runoff),
    pet = mean(pet),
    aet = mean(aet),
    gwr = mean(gwr),
    runoff_2 = mean(runoff_2),
    delta_reservoir = mean(delta_reservoir)
  ), .(year, month)]
  budget_unspat[, (names(budget_unspat)[3:ncol(budget_unspat)]) := round(.SD, 1), .SDcols = names(budget_unspat)[3:ncol(budget_unspat)]]
  fwrite(budget_unspat, file.path(output_dir, "02_bilan_unspat_month.csv"))

  # 1.6.3-Clean ####
  rm(unique_rcn_cell, cluster, input_climate)
  gc()

  # 1.7-result by gauging station, error computation ####
  for (st in 1:length(gauging)) {
    # 1.7.1-load the modeled data per gauging station ####
    setkey(water_budget, rcn_cell)
    budget_month <- water_budget[.(input_rcn_gauging$cell_ID[which(input_rcn_gauging$gauging_stat == gauging[st])])]
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
    fwrite(comparison_month, file.path(output_dir, paste0("03_bilan_unspat_month_", gauging[st], ".csv")))

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
      fwrite(simulation_metadata, file.path(output_dir, "04_simulation_metadata.csv"))
      rm(simulation_metadata)
    }
    rm(calibration_start, calibration_end, validation_start, validation_end, flow_beg, flow_end)
  }
  # 1.8-export raster for interannual runoff, aet and GWR ####
  budget_month_spat <- water_budget[
    , .(runoff = sum(runoff + runoff_2, na.rm = TRUE), aet = sum(aet, na.rm = TRUE), gwr = sum(gwr, na.rm = TRUE)),
    .(rcn_cell, year)
  ]
  budget_month_spat <- budget_month_spat[, .(runoff = mean(runoff), aet = mean(aet), gwr = mean(gwr)), .(rcn_cell)]
  input_rcn <- input_rcn[which(!duplicated(input_rcn$cell_ID)), ]
  x_interannual <- merge(budget_month_spat, input_rcn[, c(2, 4, 5)], by.x = "rcn_cell", by.y = "cell_ID")
  runoff <- x_interannual[, .(x = X_L93, y = Y_L93, z = runoff)]
  aet <- x_interannual[, .(x = X_L93, y = Y_L93, z = aet)]
  gwr <- x_interannual[, .(x = X_L93, y = Y_L93, z = gwr)]

  sp::coordinates(runoff) <- ~ x + y
  runoff <- rasterFromXYZ(runoff, crs = "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  runoff <- setMinMax(runoff)
  writeRaster(runoff, filename = file.path(output_dir, "05_interannual_runoff_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)

  sp::coordinates(aet) <- ~ x + y
  aet <- rasterFromXYZ(aet, crs = "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  aet <- setMinMax(aet)
  writeRaster(aet, filename = file.path(output_dir, "06_interannual_aet_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)

  sp::coordinates(gwr) <- ~ x + y
  gwr <- rasterFromXYZ(gwr, crs = "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  gwr <- setMinMax(gwr)
  writeRaster(gwr, filename = file.path(output_dir, "07_interannual_gwr_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)

  # 1.9-Clean ####
  rm(
    st, comparison_month, budget_unspat, observed_flow_month, input_rcn_gauging, water_budget, input_rcn, climate_data,
    budget_month_spat, x_interannual, runoff, aet, gwr
  )
  gc()
  invisible(output_dir)
}
