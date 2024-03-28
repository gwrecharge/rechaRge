load_example_remote_datasets <- function() {
  base_url <- "https://github.com/gwrecharge/rechaRge-book/raw/main/examples/input/"
  list(
    rcn = paste0(base_url, "rcn.csv.gz"),
    climate = paste0(base_url, "climate.csv.gz"),
    rcn_climate = paste0(base_url, "rcn_climate.csv.gz"),
    rcn_gauging = paste0(base_url, "rcn_gauging.csv.gz"),
    observed_flow = paste0(base_url, "observed_flow.csv.gz"),
    alpha_lyne_hollick = paste0(base_url, "alpha_lyne_hollick.csv.gz")
  )
}

load_example_local_datasets <- function() {
  list(
    rcn = file.path("data", "rcn_79402.csv.gz"),
    climate = file.path("data", "climate_1.csv.gz"),
    rcn_climate = file.path("data", "rcn_79402_climate_1.csv.gz"),
    rcn_gauging = file.path("data", "rcn_79402_gauging.csv.gz"),
    observed_flow = file.path("data", "observed_flow.csv.gz"),
    alpha_lyne_hollick = file.path("data", "alpha_lyne_hollick.csv.gz")
  )
}


load_hydrobudget <- function() {
  HB <- new_hydrobudget(
    T_m = 2.1, # melting temperature (°C)
    C_m = 6.2, # melting coefficient (mm/°C/d)
    TT_F = -17.6, # Threshold temperature for soil frost (°C)
    F_T = 16.4, # Freezing time (d)
    t_API = 3.9, # Antecedant precipitation index time (d)
    f_runoff = 0.63, # Runoff factor (-)
    sw_m = 431, # Maximum soil water content (mm)
    f_inf = 0.07 # infiltration factor (-)
  )
  HB$rcn_columns <- list(
    climate_id = "climate_cell",
    rcn_id = "cell_ID",
    RCNII = "RCNII",
    lon = "X_L93",
    lat = "Y_L93"
  )
  HB$climate_columns$climate_id <- "climate_cell"
  HB$rcn_climate_columns <- list(
    climate_id = "climate_cell",
    rcn_id = "cell_ID"
  )
  HB$rcn_gauging_columns <- list(
    rcn_id = "cell_ID",
    station_id = "gauging_stat"
  )
  HB$alpha_lyne_hollick_columns$station_id <- "station"
  HB
}
