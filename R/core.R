
#' Simulation using a recharge model
#'
#' Performs a simulation of water recharge using a specific model.
#'
#' The expected columns for the RCN data set input are:
#' * **rcn_id**, the RCN cell ID
#' * **RCNII**
#' * **lon**
#' * **lat**
#'
#' The expected columns for the climate data set input are:
#' * **climate_id** the climate cell ID
#' * **day**
#' * **month**
#' * **year**
#' * **t_mean**
#' * **p_tot**
#' * **lat**
#'
#' The expected columns for the RCN-climate data set input are:
#' * **climate_id** the climate cell ID
#' * **rcn_id**, the RCN cell ID
#'
#' The columns of the water budget data set output are:
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
#' @param obj The recharge object.
#' @param rcn The RCN values. Input can be a data.frame/data.table or a path to a data file.
#' @param climate The daily total precipitation (mm/d) and average daily temperature (°C). Input can be a data.frame/data.table or a path to a data file.
#' @param rcn_climate The relation between the RCN and climate cells. Input can be a data.frame/data.table or a path to a data file.
#' @param period The start and end years. If not provided, the start/end years will be extracted from the climate data.
#' @param workers The number of workers to use in the parallel computations. If NULL, an optimal number of cores will be used. This optimal number is also the maximum value. Default value is 1 (no parallelization).
#' @param ... Other arguments passed to methods
#'
#' @return The water budget
#' @rdname compute_recharge
#' @export
compute_recharge <- function(obj, rcn, climate, rcn_climate, period = NULL, workers = 1, ...) {
  UseMethod("compute_recharge")
}

#' @rdname compute_recharge
#' @method compute_recharge default
#' @export
compute_recharge.default <- function(obj, rcn, climate, rcn_climate, period = NULL, workers = 1, ...) {
  stop("The compute recharge function has no default model")
}

#' Write result as data files
#'
#' Export water budget.
#'
#' @param obj The recharge object.
#' @param water_budget The computed water budget.
#' @param output_dir The output directory where result files will be written. Default is a temporary directory.
#' @param ... Other arguments passed to methods
#' @return (Invisible) the output directory.
#' @rdname write_recharge_results
#' @export
write_recharge_results <- function(obj, water_budget, output_dir = tempdir(), ...) {
  UseMethod("write_recharge_results")
}

#' @rdname write_recharge_results
#' @method write_recharge_results default
#' @export
write_recharge_results.default <- function(obj, water_budget, output_dir = tempdir(), ...) {
  stop("The write recharge results function has no default model")
}

#' Write result as raster files
#'
#' Export raster for interannual runoff, aet and GWR.
#'
#' @param obj The recharge object.
#' @param water_budget The computed water budget. Input can be a data.frame/data.table or a path to a data file.
#' @param input_rcn The RCN values. Input can be a data.frame/data.table or a path to a data file.
#' @param crs The coordinate reference systems.
#' @param output_dir The output directory where result files will be written. Default is a temporary directory.
#' @param ... Other arguments passed to methods
#' @return (Invisible) the output directory.
#' @rdname write_recharge_rasters
#' @export
write_recharge_rasters <- function(obj, water_budget, input_rcn, crs, output_dir = tempdir(), ...) {
  UseMethod("write_recharge_rasters")
}

#' @rdname write_recharge_rasters
#' @method write_recharge_rasters default
#' @export
write_recharge_rasters.default <- function(obj, water_budget, input_rcn, crs, output_dir = tempdir(), ...) {
  stop("The write recharge rasters function has no default model")
}

#' Evaluate the quality of the simulation result
#'
#' From a simulation result, evaluate the quality by comparing with observations. The quality
#' measurement can be used for model calibration (e.g. caRamel package) or sensitivity evaluation
#' (e.g. sensitivity package).
#'
#' @param obj The recharge object.
#' @param water_budget The computed water budget. Input can be a data.frame/data.table or a path to a data file.
#' @param ... Other arguments passed to methods
#' @return The model-specific quality assessment.
#' @rdname evaluate_simulation_quality
#' @export
evaluate_simulation_quality <- function(obj, water_budget, ...) {
  UseMethod("evaluate_simulation_quality")
}

#' @rdname evaluate_simulation_quality
#' @method evaluate_simulation_quality default
#' @export
evaluate_simulation_quality.default <- function(obj, water_budget, ...) {
  stop("The Evaluate simulation quality function has no default model")
}
