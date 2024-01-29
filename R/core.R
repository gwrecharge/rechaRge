
#' Simulation using a recharge model
#' 
#' Performs a simulation of water recharge using a specific model. 
#'
#' The expected columns for the RCN data set input are:
#' * **climate_cell**
#' * **cell_ID**, the cell ID
#' * **RCNII**
#' * **X_L93**
#' * **Y_L93**
#'
#' The expected columns for the climate data set input are:
#' * **climate_cell**
#' * **day**
#' * **month**
#' * **year**
#' * **t_mean**
#' * **p_tot**
#' * **lat**
#' 
#' The columns of the water budget data set output are:
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
#' @param obj The recharge object.
#' @param rcn The RCN values. Input can be a data.frame/data.table or a path to a data file.
#' @param climate The daily total precipitation (mm/d) and average daily temperature (°C). Input can be a data.frame/data.table or a path to a data file.
#' @param period The start and end years. If not provided, the start/end years will be extracted from the climate data.
#' @param nb_core The number of cores to use in the parallel computations. If not provided, all cores minus one will be used.
#' @param ... Other arguments passed to methods
#' 
#' @return The water budget
#' @rdname compute_recharge
#' @export
compute_recharge <- function(obj, rcn, climate, period = NULL, nb_core = NULL, ...) {
  UseMethod("compute_recharge")
}

#' @rdname compute_recharge
#' @method compute_recharge default
#' @export
compute_recharge.default <- function(obj, rcn, climate, period = NULL, nb_core = NULL, ...) {
  cat("The compute recharge function has no default model\n")
}

#' Write result as data files
#'
#' Export water budget.
#'
#' @param water_budget The computed water budget.
#' @param output_dir The output directory where result files will be written. Default is current working directory.
#'
#' @importFrom data.table fwrite
#'
#' @export
write_results <- function(water_budget, output_dir = getwd()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  fwrite(water_budget, file.path(output_dir, "01_bilan_spat_month.csv"))
  budget_unspat <- water_budget[, .(
    VI = mean(get("VI")),
    t_mean = mean(get("t_mean")),
    runoff = mean(get("runoff")),
    pet = mean(get("pet")),
    aet = mean(get("aet")),
    gwr = mean(get("gwr")),
    runoff_2 = mean(get("runoff_2")),
    delta_reservoir = mean(get("delta_reservoir"))
  ), .(year, month)]
  budget_unspat[, (names(budget_unspat)[3:ncol(budget_unspat)]) := round(get(".SD"), 1), .SDcols = names(budget_unspat)[3:ncol(budget_unspat)]]
  fwrite(budget_unspat, file.path(output_dir, "02_bilan_unspat_month.csv"))
  rm(budget_unspat)
}

#' Write result as raster files
#'
#' Export raster for interannual runoff, aet and GWR.
#'
#' @param water_budget The computed water budget. Input can be a data.frame/data.table or a path to a data file.
#' @param input_rcn The RCN values. Input can be a data.frame/data.table or a path to a data file.
#' @param crs The coordinate reference systems.
#' @param output_dir The output directory where result files will be written. Default is current working directory.
#'
#' @importFrom data.table fwrite
#' @importFrom raster rasterFromXYZ setMinMax writeRaster
#' @importFrom sp coordinates
#'
#' @export
write_rasters <- function(water_budget, input_rcn, crs, output_dir = getwd()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  wb <- .as.data.table(water_budget)
  
  rcn <- .as.data.table(input_rcn)
  
  budget_month_spat <- wb[
    , .(runoff = sum(get("runoff") + get("runoff_2"), na.rm = TRUE),
        aet = sum(get("aet"), na.rm = TRUE),
        gwr = sum(get("gwr"), na.rm = TRUE)),
    .(rcn_cell = get("rcn_cell"), year)
  ]
  budget_month_spat <- budget_month_spat[, .(runoff = mean(get("runoff")),
                                             aet = mean(get("aet")),
                                             gwr = mean(get("gwr"))), .(rcn_cell = get("rcn_cell"))]
  rcn <- rcn[which(!duplicated(rcn$cell_ID)), ]
  x_interannual <- merge(budget_month_spat, rcn[, c(2, 4, 5)], by.x = "rcn_cell", by.y = "cell_ID")
  runoff <- x_interannual[, .(x = get("X_L93"), y = get("Y_L93"), z = runoff)]
  aet <- x_interannual[, .(x = get("X_L93"), y = get("Y_L93"), z = aet)]
  gwr <- x_interannual[, .(x = get("X_L93"), y = get("Y_L93"), z = gwr)]
  
  sp::coordinates(runoff) <- ~ x + y
  runoff <- rasterFromXYZ(runoff, crs = crs)
  runoff <- setMinMax(runoff)
  writeRaster(runoff, filename = file.path(output_dir, "05_interannual_runoff_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  sp::coordinates(aet) <- ~ x + y
  aet <- rasterFromXYZ(aet, crs = crs)
  aet <- setMinMax(aet)
  writeRaster(aet, filename = file.path(output_dir, "06_interannual_aet_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  sp::coordinates(gwr) <- ~ x + y
  gwr <- rasterFromXYZ(gwr, crs = crs)
  gwr <- setMinMax(gwr)
  writeRaster(gwr, filename = file.path(output_dir, "07_interannual_gwr_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  rm(aet, budget_month_spat, gwr, x_interannual)
}