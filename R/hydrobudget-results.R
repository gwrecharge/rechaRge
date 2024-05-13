#' Write HydroBudget model results as data files
#'
#' Supported formats are "csv" (default) or "nc" (NetCDF).
#'
#' @param format The file output format. Use "nc" for NetCDF format. Default is "csv".
#' @param input_rcn The RCN values. Input can be a data.frame/data.table or a path to a data file.
#' @param names The long names and units of the NetCDF dimensions.
#' @rdname write_recharge_results
#' @method write_recharge_results hydrobudget
#' @importFrom data.table fwrite dcast
#' @importFrom ncdf4 nc_close nc_create ncatt_put ncdim_def ncvar_def ncvar_put
#' @export
write_recharge_results.hydrobudget <-
  function(obj,
           water_budget,
           output_dir = tempdir(),
           format = "csv",
           input_rcn = NULL,
           names = list(
             "lon" = list(
               longname = "Longitude",
               unit = "deg"
               ),
             "lat" = list(
               longname = "Lattitude",
               unit = "deg"
               ),
             "time" = list(
               longname = "Month since start of the water budget",
               unit = "month"
               )
           ),
           ...) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    if (format == "nc") {
      # Spatial reference
      if (is.null(input_rcn)) {
        stop("RCN dataset is required for exporting to NetCDF format")
      }
      # Load the input data and ensure expected column names
      lat_long_hb <- .as.data.table(input_rcn, obj$rcn_columns)[, c("rcn_id", "lon", "lat")]
      colnames(lat_long_hb)<-c("id", "x", "y")
      unique_id <- unique(lat_long_hb$rcn_id)
      lon <- as.array(unique(lat_long_hb$x))
      nlon <- length(lon)
      lat <- as.array(unique(lat_long_hb$y))
      nlat <- length(lat)

      # Time reference ####
      seq_months <-
        rep(c(1:12), length(c(
          min(water_budget$year):max(water_budget$year)
        )))
      seq_months <- seq(1, length(seq_months), 1)
      time <- as.integer(length(seq_months))
      time_bind <-
        data.table(
          year = rep(c(
            min(water_budget$year):max(water_budget$year)
          ), each = 12),
          month = rep(c(1:12), length(c(
            min(water_budget$year):max(water_budget$year)
          ))),
          time = seq_months
        )

      # Variables, and fill value
      variable_names <- names(water_budget)
      variable_names <-
        variable_names[!(variable_names %in% c("year", "month", "delta_reservoir", "rcn_id"))]
      fillvalue <- -99

      # Utils
      get_longname <- function(key, default) {
        if (is.null(names[[key]]) || !inherits(names[[key]], "list") || is.null(names[[key]]$longname)) {
          default
        } else {
          names[[key]]$longname
        }
      }

      get_unit <- function(key, default) {
        if (is.null(names[[key]]) || !inherits(names[[key]], "list") || is.null(names[[key]]$unit)) {
          default
        } else {
          names[[key]]$unit
        }
      }

      # Dimensions
      longdim <- ncdim_def("lon",
                           get_unit("lon", "deg"),
                           as.integer(lon),
                           longname = get_longname("lon", "Longitude"))
      latdim <- ncdim_def("lat",
                          get_unit("lat", "deg"),
                          as.integer(lat),
                          longname = get_longname("lat", "Lattitude"))
      timedim <- ncdim_def("time",
                           get_unit("time", "month"),
                           as.integer(seq_months),
                           longname = get_longname("time", "Month since start of the water budget"))

      # Variables
      variables <- list(
        "vi" = ncvar_def("vi", "mm", list(longdim, latdim, timedim), fillvalue, "Vertical inflow", prec = "single"),
        "t_mean" = ncvar_def("t_mean", "deg_C", list(longdim, latdim, timedim), fillvalue, "Mean temperature", prec="single"),
        "runoff" = ncvar_def("runoff", "mm", list(longdim, latdim, timedim), fillvalue, "Runoff", prec = "single"),
        "pet" = ncvar_def("pet", "mm", list(longdim, latdim, timedim), fillvalue, "Potential evapotranspiration", prec = "single"),
        "aet" = ncvar_def("aet", "mm", list(longdim, latdim, timedim), fillvalue, "Actual evapotranspiration", prec = "single"),
        "gwr" = ncvar_def("gwr", "mm",  list(longdim, latdim, timedim),fillvalue, "Groundwater recharge", prec = "single"),
        "runoff_2" = ncvar_def("runoff_2", "mm",  list(longdim, latdim, timedim), fillvalue, "Excess runoff", prec = "single")
      )

      # Create the NetCDF file
      hb_sim <- nc_create(file.path(output_dir, "water_budget.nc"),
                          variables,
                          force_v4 = TRUE)


      # Additional attributes into dimension and data variables
      ncatt_put(hb_sim,"lon","axis","Longitude")
      ncatt_put(hb_sim,"lat","axis","Lattitude")
      ncatt_put(hb_sim,"time","axis","Time")

      # Add global attributes
      ncatt_put(hb_sim,0,"title","Water budget simulated with the HydroBudget model from the rechaRge package")
      ncatt_put(hb_sim,0,"institution","EPFL")
      ncatt_put(hb_sim,0,"references", "Dubois et al. (2021) HESS")
      ncatt_put(hb_sim,0,"source", "https://doi.org/10.5194/hess-25-6567-2021")

      # Loop per variable
      for(v in variable_names){
        # Add coordinates to water budget
        in_var <- merge(water_budget[,c("year", "month", v, "rcn_id"), with = FALSE], time_bind, all.x = TRUE)
        in_var[,`:=`(year = NULL, month = NULL)]
        # NSE
        rcn_id <- NULL
        in_var <- dcast(in_var, rcn_id~time, value.var = names(in_var)[1])
        in_var <- merge(in_var, lat_long_hb, by.x = "rcn_id", by.y = "id", all.x = TRUE)
        in_var[,rcn_id := NULL]

        # Create the empty array
        var_array <- array(fillvalue, dim=c(as.integer(nlon), as.integer(nlat), as.integer(time)))

        # Fill up the array
        nobs <- dim(in_var)[1]
        l <- rep(1:nrow(time_bind),each=nobs)
        j <- sapply(in_var$x, function(x) which.min(abs(lon-x)))
        k <- sapply(in_var$y, function(x) which.min(abs(lat-x)))
        var_array[cbind(j,k,l)] <- as.matrix(in_var[1:nobs, 1:nrow(time_bind)])
        rm(j, k, in_var)

        # Write the data in the NetCDF file
        ncvar_put(hb_sim, variables[[v]], var_array)
      }

      # Close the file to write it on the disk
      nc_close(hb_sim)

    } else {
      fwrite(water_budget,
             file.path(output_dir, "bilan_spat_month.csv"))
      budget_unspat <- water_budget[, .(
        vi = mean(get("vi")),
        t_mean = mean(get("t_mean")),
        runoff = mean(get("runoff")),
        pet = mean(get("pet")),
        aet = mean(get("aet")),
        gwr = mean(get("gwr")),
        runoff_2 = mean(get("runoff_2")),
        delta_reservoir = mean(get("delta_reservoir"))
      ), .(year, month)]
      budget_unspat[, (names(budget_unspat)[3:ncol(budget_unspat)]) := round(get(".SD"), 1), .SDcols = names(budget_unspat)[3:ncol(budget_unspat)]]
      fwrite(budget_unspat,
             file.path(output_dir, "bilan_unspat_month.csv"))
      rm(budget_unspat)
    }

    invisible(output_dir)
  }

#' Write HydroBudget model results as raster files
#'
#' @rdname write_recharge_rasters
#' @method write_recharge_rasters hydrobudget
#' @importFrom data.table fwrite
#' @importFrom raster rasterFromXYZ setMinMax writeRaster
#' @importFrom sp coordinates
#' @export
write_recharge_rasters.hydrobudget <-
  function(obj,
           water_budget,
           input_rcn,
           crs,
           output_dir = tempdir(),
           ...) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    wb <- .as.data.table(water_budget)
    rcn <- .as.data.table(input_rcn, obj$rcn_columns)

    budget_month_spat <- wb[, .(
      runoff = sum(get("runoff") + get("runoff_2"), na.rm = TRUE),
      aet = sum(get("aet"), na.rm = TRUE),
      gwr = sum(get("gwr"), na.rm = TRUE)
    ),
    .(rcn_id = get("rcn_id"), year)]
    budget_month_spat <-
      budget_month_spat[, .(runoff = mean(get("runoff")),
                            aet = mean(get("aet")),
                            gwr = mean(get("gwr"))), .(rcn_id = get("rcn_id"))]
    rcn <- rcn[which(!duplicated(rcn$rcn_id)),]
    x_interannual <-
      merge(budget_month_spat, rcn[, c("rcn_id", "lon", "lat")], by.x = "rcn_id", by.y = "rcn_id")
    runoff <-
      x_interannual[, .(x = get("lon"),
                        y = get("lat"),
                        z = runoff)]
    aet <- x_interannual[, .(x = get("lon"),
                             y = get("lat"),
                             z = aet)]
    gwr <- x_interannual[, .(x = get("lon"),
                             y = get("lat"),
                             z = gwr)]

    sp::coordinates(runoff) <- ~ x + y
    runoff <- rasterFromXYZ(runoff, crs = crs)
    runoff <- setMinMax(runoff)
    writeRaster(
      runoff,
      filename = file.path(output_dir, "interannual_runoff_NAD83.tif"),
      Format = "GTiff",
      bylayer = TRUE,
      overwrite = TRUE
    )

    sp::coordinates(aet) <- ~ x + y
    aet <- rasterFromXYZ(aet, crs = crs)
    aet <- setMinMax(aet)
    writeRaster(
      aet,
      filename = file.path(output_dir, "interannual_aet_NAD83.tif"),
      Format = "GTiff",
      bylayer = TRUE,
      overwrite = TRUE
    )

    sp::coordinates(gwr) <- ~ x + y
    gwr <- rasterFromXYZ(gwr, crs = crs)
    gwr <- setMinMax(gwr)
    writeRaster(
      gwr,
      filename = file.path(output_dir, "interannual_gwr_NAD83.tif"),
      Format = "GTiff",
      bylayer = TRUE,
      overwrite = TRUE
    )

    rm(aet, budget_month_spat, gwr, x_interannual)

    invisible(output_dir)
  }
