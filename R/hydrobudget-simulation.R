# HydroBudget model

#' HydroBudget object
#'
#' Make a new HydroBudget object, by providing the calibration parameters for the
#' model computation.
#'
#' @param T_m The melting temperature (°C)
#' @param C_m The melting coefficient (mm/°C/d)
#' @param TT_F The Threshold temperature for soil frost (°C)
#' @param F_T The freezing time (d)
#' @param t_API The antecedent precipitation index time (d)
#' @param f_runoff The runoff factor (-)
#' @param sw_m The maximum soil water content (mm)
#' @param f_inf The infiltration factor (-)
#'
#' @return
#' @export
new_hydrobugdet <- function(T_m, C_m, TT_F, F_T, t_API, f_runoff, sw_m, f_inf) {
  # TODO some sanity checks with the calibration values
  structure(list(
    calibration = list(
      # threshold temperature rain/snow (°C)
      T_snow = 0,
      T_m = T_m,
      C_m = C_m,
      # soil frost
      TT_F = TT_F,
      F_T = F_T,
      # runoff
      t_API = t_API,
      f_runoff = f_runoff,
      # soil parameters
      sw_m = sw_m,
      f_inf = f_inf,
      sw_init = 50
    )
  ), class = "hydrobudget")
}

#' Simulation using HydroBudget model
#'
#' HydroBudget is a spatially distributed GWR model that computes a superficial water budget on
#' grid cells of regional-scale watersheds. Runoff, actual evapotranspiration (AET), and potential
#' GWR are simulated for each grid cell, with a monthly time step, and fluxes do not
#' transfer from a cell to another (no water routing). The model inputs are distributed daily
#' precipitation and temperature as well as distributed data of pedology, land cover, and slope.
#'
#' @rdname compute_recharge
#' @method compute_recharge hydrobudget
#' @export
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom lubridate yday year month
#' @importFrom data.table data.table setkey :=
#' @importFrom foreach foreach %dopar%
#' @importFrom zoo rollmean rollsum
#' @importFrom stats na.contiguous na.omit
#' @importFrom airGR PE_Oudin
#' @importFrom plyr .
#'
#' @examples
#' \dontrun{
#' # Use input example files provided by the package
#' examples_dir <- system.file("examples", package = "rechaRge")
#' input_rcn <- file.path(examples_dir, "input", "input_rcn.csv.gz")
#' input_climate <- file.path(
#'   examples_dir, "input",
#'   "input_climate.csv.gz"
#' ) # precipitation total in mm/d
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
#' # Simulation period
#' simul_period <- c(2010, 2017)
#'
#' # Parallel computing option
#' # nb_core <- 6 # if nothing is set, by default it will be all the computer core - 1
#'
#' # Simulation with the HydroBudget model
#' water_budget <- rechaRge::compute_recharge(
#'   HB,
#'   rcn = input_rcn,
#'   climate = input_climate,
#'   period = simul_period
#'   # nb_core = nb_core
#' )
#' head(water_budget)
#' }
compute_recharge.hydrobudget <- function(obj, rcn, climate, period = NULL, nb_core = NULL, ...) {
  gc()
  pb <- .newProgress(total = 4)

  # 1.1-Load the input data ####
  .updateProgress(pb, step = 1, total = 4, tokens = list(what = "Checking input data..."))
  # TODO sanity checks
  rcn_data <- .as.data.table(rcn)
  climate_data <- .as.data.table(climate)
  year_range <- period
  nb_core <- nb_core

  # 1.2-Simulation period ####
  if (is.null(year_range)) {
    year_range <- c(min(climate_data$year), max(climate_data$year))
  } else if (length(year_range) == 1) {
    year_range <- append(year_range, max(climate_data$year))
  }
  year_start <- as.numeric(year_range[1])
  year_end <- as.numeric(year_range[2])
  if (year_end < year_start) {
    stop("Wrong simulation period, start year must be before end year")
  }
  # filter climate data for the period
  list_year <- seq(year_start, year_end, 1)
  climate_data <- climate_data[year %in% list_year]

  # 1.3-cluster parameters ####
  ifelse(is.null(nb_core),
    nb_core <- detectCores() - 1,
    nb_core <- as.numeric(nb_core)
  )

  # 1.4-Calibration parameters ####
  calibration_ <- obj$calibration

  # 1.5-execute the snow model and compute Oudin PET ####
  .updateProgress(pb, step = 2, total = 4, tokens = list(what = "Computing vertical inflow..."))
  climate_data <- compute_vertical_inflow(calibration_, climate_data, nb_core)

  # 1.6-run HB water budget partitioning ####
  # 1.6.1-Model loop by grid cell in parallel #####
  .updateProgress(pb, step = 3, total = 4, tokens = list(what = "Computing water budget..."))
  water_budget <- compute_water_budget(calibration_, rcn_data, climate_data, nb_core)

  # 1.9-Clean ####
  rm(climate_data, rcn_data)
  gc()
  .updateProgress(pb, step = 4, total = 4, tokens = list(what = "Completed"))

  water_budget
}

#' Determine if precipitation is rain or snow and simulate the snowpack (accumulation
#' and melt) to compute the vertical inflow (VI), the liquid water available per day (rainfall + melt water).
#'
#' @param calibration The calibration parameters.
#' @param climate_data The daily total precipitation (mm/d) and average daily temperature (°C).
#' @param nb_core The number of cores to use in the parallel computations.
#'
#' @keywords internal
compute_vertical_inflow <- function(calibration, climate_data, nb_core) {
  # 1.5-execute the snow model and compute Oudin PET ####
  # 1.5.1-Parallel loop ####
  cluster <- .make_cluster(nb_core)
  climate_data_VI <- foreach(k = 1:(length(unique(climate_data$climate_cell))), .combine = rbind, .inorder = FALSE) %dopar% {
    # NSE
    climate_cell <- NULL
    cellgrid <- as.character(unique(climate_data$climate_cell)[get("k")])
    input_dd <- climate_data[climate_cell == cellgrid]
    compute_vertical_inflow_cell(calibration, input_dd)
  }
  .stop_cluster(cluster)
  rm(cluster)

  # 1.5.2-Compute vertical inflow (VI) ####
  climate_data_VI$VI <- climate_data_VI$rain + climate_data_VI$melt
  climate_data_VI[, c(1:6, 14, 13)]
}

#' Compute the vertical inflow and the potential evapotranspiration (PET) for a single cell
#'
#' @param calibration The calibration parameters.
#' @param input_dd Spatially distributed daily precipitation and mean temperature time series in a single cell.
#'
#' @keywords internal
compute_vertical_inflow_cell <- function(calibration, input_dd) {
  # 1.5.1.1-load the packages ####
  requireNamespace("data.table")
  requireNamespace("airGR")

  # calibration parameters
  T_m <- calibration$T_m
  C_m <- calibration$C_m
  T_snow <- calibration$T_snow

  # 1.5.1.2-loop for the snowpack ####
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
  input_dd$PET <- compute_potential_evapotranspiration_cell(input_dd)

  input_dd
}

#' Compute PET based on the Oudin formula
#'
#' @param input_dd Spatially distributed daily precipitation and mean temperature time series in a single cell.
#' @return Spatially distributed daily PET
#'
#' @keywords internal
compute_potential_evapotranspiration_cell <- function(input_dd) {
  round(PE_Oudin(
    JD = yday(as.POSIXct(paste(input_dd$year, input_dd$month, input_dd$day, sep = "-"), format = "%Y-%m-%d")),
    Temp = input_dd$t_mean,
    Lat = unique(input_dd$lat),
    # the name of the cells is long-lat binded, so the last 3 numbers are the latitude with 1 digit
    LatUnit = "deg", TimeStepIn = "daily", TimeStepOut = "daily"
  ), 2)
}

#' Compute the water budget
#'
#' @param calibration The calibration parameters.
#' @param rcn_data The RCN values.
#' @param climate_data The daily total precipitation (mm/d) and average daily temperature (°C).
#' @param nb_core The number of cores to use in the parallel computations.
#'
#' @keywords internal
compute_water_budget <- function(calibration, rcn_data, climate_data, nb_core) {
  cluster <- .make_cluster(nb_core)
  unique_rcn_cell <- unique(rcn_data$cell_ID)
  j <- 1
  water_budget <- foreach(j = 1:(length(unique_rcn_cell)), .combine = rbind, .inorder = FALSE) %dopar% {
    # 1.6.1.1-subsets ####
    cid <- unique_rcn_cell[j]
    # NSE
    cell_ID <- NULL
    rcn_subset <- rcn_data[cell_ID == cid]
    rcn_climate <- merge(rcn_subset, climate_data, by = "climate_cell", all.x = TRUE)
    rcn_climate <- na.omit(rcn_climate[order(rcn_climate$climate_cell, rcn_climate$cell_ID, rcn_climate$year, rcn_climate$month, rcn_climate$day), ])
    compute_water_budget_cell(calibration, rcn_climate)
  }
  .stop_cluster(cluster)
  rm(unique_rcn_cell, cluster)

  water_budget
}

#' Compute the water budget for a single cell
#'
#' @param calibration The calibration parameters.
#' @param rcn_climate The RCN values with the climate data (daily total precipitation (mm/d) and average daily temperature (°C)) for a single cell.
#'
#' @keywords internal
compute_water_budget_cell <- function(calibration, rcn_climate) {
  requireNamespace("data.table")
  requireNamespace("zoo")

  # calibration parameters
  F_T <- calibration$F_T
  t_API <- calibration$t_API
  f_runoff <- calibration$f_runoff
  TT_F <- calibration$TT_F
  sw_m <- calibration$sw_m
  f_inf <- calibration$f_inf
  sw_init <- calibration$sw_init

  # 1.6.1.2-Mean temperature function of F_T ####
  roll_mean_freez <- as.numeric(rollmean(as.numeric(rcn_climate$t_mean), F_T, fill = NA, na.pad = T, align = "right"))
  rcn_climate$temp_freez <- ifelse(is.na(roll_mean_freez), rcn_climate$t_mean, roll_mean_freez)
  rm(roll_mean_freez)

  # 1.6.1.3-API computation ####
  roll_sum_api <- as.numeric(rollsum(rcn_climate$VI, t_API, fill = NA, na.pad = T, align = "right"))
  rcn_climate$api <- ifelse(is.na(roll_sum_api), rcn_climate$VI, roll_sum_api)
  rm(roll_sum_api)

  # 1.6.1.4-RCN variations based on API ####
  RCNII <- unique(rcn_climate$RCNII)
  rcn_api <- data.table(
    "year" = as.numeric(rcn_climate$year),
    "month" = as.numeric(rcn_climate$month),
    "day" = as.numeric(rcn_climate$day),
    "julian_day" = yday(as.POSIXct(paste(rcn_climate$year, rcn_climate$month, rcn_climate$day, sep = "-"), format = "%Y-%m-%d")),
    "temperature_moy" = as.numeric(rcn_climate$t_mean),
    "temp_freez" = as.numeric(rcn_climate$temp_freez),
    "rcn_II" = as.numeric(RCNII * f_runoff),
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

  # If land use = water or wetland - if frozen rcn = 100 otherwise artificially decrease the RCN value to 10 to allow for maximum water storage on the cell
  if (RCNII == 100) { # is the RCNII value = 100 (water, wetlands)
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
      # If land use = water or wetland => artificially bloc GWR
      gwr[ii] <- ifelse(RCNII == 100, # is the cell water or wetland?
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
  # NSE
  VI <- t_mean <- runoff <- pet <- rcn_cell <- NULL
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
  w_b[, (names(w_b)[3:ncol(w_b)]) := round(get(".SD"), 1), .SDcols = names(w_b)[3:ncol(w_b)]]
  w_b[, rcn_cell := rcn_climate$cell_ID[1]]
  w_b
}
