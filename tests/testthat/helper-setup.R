load_example_datasets <- function() {
  examples_dir <- system.file("examples", package="rechaRge")
  input_rcn <- file.path(examples_dir, "input", "input_rcn.csv.gz")
  input_climate <- file.path(examples_dir, "input", "input_climate.csv.gz") # precipitation total in mm/d
  input_rcn_gauging <- file.path(examples_dir, "input", "input_rcn_gauging.csv.gz")
  list(
    input_rcn = data.table::fread(input_rcn),
    input_rcn_gauging = data.table::fread(input_rcn_gauging),
    input_climate = data.table::fread(input_climate)
  )
}

load_parameters <- function() {
  list(
    T_m = 2.1, # melting temperature (°C)
    C_m = 6.2, # melting coefficient (mm/°C/d)
    TT_F = -17.6, # Threshold temperature for soil frost (°C)
    F_T = 16.4, # Freezing time (d)
    t_API = 3.9, # Antecedant precipitation index time (d)
    f_runoff = 0.63, # Runoff factor (-)
    sw_m = 431, # Maximum soil water content (mm)
    f_inf = 0.07 # infiltration factor (-)
  )
}