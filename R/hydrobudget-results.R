#' Write HydroBudget model results as data files
#'
#' @rdname write_recharge_results
#' @method write_recharge_results hydrobudget
#' @importFrom data.table fwrite
#' @export
write_recharge_results.hydrobudget <- function(obj, water_budget, output_dir = getwd(), ...) {
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

#' Write HydroBudget model results as raster files
#'
#' @rdname write_recharge_rasters
#' @method write_recharge_rasters hydrobudget
#' @importFrom data.table fwrite
#' @importFrom raster rasterFromXYZ setMinMax writeRaster
#' @importFrom sp coordinates
#' @export
write_recharge_rasters.hydrobudget <- function(obj, water_budget, input_rcn, crs, output_dir = getwd(), ...) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  wb <- .as.data.table(water_budget)
  rcn <- .as.data.table(input_rcn, obj$rcn_columns)
  
  budget_month_spat <- wb[
    , .(runoff = sum(get("runoff") + get("runoff_2"), na.rm = TRUE),
        aet = sum(get("aet"), na.rm = TRUE),
        gwr = sum(get("gwr"), na.rm = TRUE)),
    .(rcn_id = get("rcn_id"), year)
  ]
  budget_month_spat <- budget_month_spat[, .(runoff = mean(get("runoff")),
                                             aet = mean(get("aet")),
                                             gwr = mean(get("gwr"))), .(rcn_id = get("rcn_id"))]
  rcn <- rcn[which(!duplicated(rcn$rcn_id)), ]
  x_interannual <- merge(budget_month_spat, rcn[, c("rcn_id", "lon", "lat")], by.x = "rcn_id", by.y = "rcn_id")
  runoff <- x_interannual[, .(x = get("lon"), y = get("lat"), z = runoff)]
  aet <- x_interannual[, .(x = get("lon"), y = get("lat"), z = aet)]
  gwr <- x_interannual[, .(x = get("lon"), y = get("lat"), z = gwr)]
  
  sp::coordinates(runoff) <- ~ x + y
  runoff <- rasterFromXYZ(runoff, crs = crs)
  runoff <- setMinMax(runoff)
  writeRaster(runoff, filename = file.path(output_dir, "03_interannual_runoff_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  sp::coordinates(aet) <- ~ x + y
  aet <- rasterFromXYZ(aet, crs = crs)
  aet <- setMinMax(aet)
  writeRaster(aet, filename = file.path(output_dir, "04_interannual_aet_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  sp::coordinates(gwr) <- ~ x + y
  gwr <- rasterFromXYZ(gwr, crs = crs)
  gwr <- setMinMax(gwr)
  writeRaster(gwr, filename = file.path(output_dir, "05_interannual_gwr_NAD83.tif"), Format = "GTiff", bylayer = TRUE, overwrite = TRUE)
  
  rm(aet, budget_month_spat, gwr, x_interannual)
}