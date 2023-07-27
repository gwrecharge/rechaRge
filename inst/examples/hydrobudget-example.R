# 1-Install the package ####
remotes::install_github("gwrecharge/rechaRge", ref = "dev")

# 2-Load the input data for the simulation and enter the parameters values ####
## 2.1-Create the folder to save the simulation results ####
sim_dir <- file.path(getwd(), paste0("simulation_HydroBudget_", format(Sys.time(), "%Y%m%dT%H:%M")))

## 2.2-Input data ####
# use input example files provided by the package 
examples_dir <- file.path(path.package("rechaRge"), "examples")
input_rcn <- data.table::fread(file.path(examples_dir, "input", "input_rcn.csv.gz"))
input_climate <- data.table::fread(file.path(examples_dir, "input", "input_climate.csv.gz")) # precipitation total in mm/d
input_rcn_gauging <- data.table::fread(file.path(examples_dir, "input", "input_rcn_gauging.csv.gz"))
observed_flow <- data.table::fread(file.path(examples_dir, "input", "observed_flow.csv.gz")) # flow rates in mm/d
alpha_lyne_hollick <- data.table::fread(file.path(examples_dir, "input", "alpha_lyne_hollick.csv.gz"))

## 2.3-Calibration parameters ####
param <- list(
  T_m = 2.1, # melting temperature (°C)
  C_m = 6.2, # melting coefficient (mm/°C/d)
  TT_F = -17.6, # Threshold temperature for soil frost (°C)
  F_T = 16.4, # Freezing time (d)
  t_API = 3.9, # Antecedant precipitation index time (d)
  f_runoff = 0.63, # Runoff factor (-)
  sw_m = 431, # Maximum soil water content (mm)
  f_inf = 0.07
) # infiltration factor (-)

## 2.4-Simulation period ####
simul_period <- c(2010, 2017)
list_year <- seq(simul_period[1], simul_period[2], 1)
input_climate <- input_climate[year %in% list_year]
observed_flow <- observed_flow[year %in% list_year]

## 2.5-Parallel computing option ####
#nb_core <- 6 # if nothing is set, by default it will be all the computer core - 1

# 3-Process the river flow observations ####
flow <- rechaRge::process_river_flow(observed_flow, alpha_lyne_hollick)

# 4-Simulation with the HydroBudget model ####
rechaRge::compute_hydrobudget(
  param = param,
  input_rcn = input_rcn,
  input_rcn_gauging = input_rcn_gauging,
  input_climate = input_climate,
  simul_period = simul_period,
  observed_flow_month = flow$observed_flow_month,
  gauging = flow$gauging,
  #nb_core = nb_core,
  output_dir = sim_dir
)
# list simulation output files
list.files(sim_dir)

data.table::fread(file.path(sim_dir, "01_bilan_spat_month.csv"))
data.table::fread(file.path(sim_dir, "02_bilan_unspat_month.csv"))
data.table::fread(file.path(sim_dir, "03_bilan_unspat_month_23702.csv"))
data.table::fread(file.path(sim_dir, "04_simulation_metadata.csv"))

# data viz
library(tidyterra)
library(terra)
library(ggplot2)
library(cowplot)
subtitle <- paste0("From ", simul_period[1], " to ", simul_period[2])
runoff <- terra::rast(file.path(sim_dir, "05_interannual_runoff_NAD83.tif"))
runoffplot <- ggplot() +
  geom_spatraster(data = runoff) +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    fill = "",
    title = "Runoff",
    subtitle = subtitle
  )
aet <- terra::rast(file.path(sim_dir, "06_interannual_aet_NAD83.tif"))
aetplot <- ggplot() +
  geom_spatraster(data = aet) +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    fill = "",
    title = "Actual Evapotranspiration",
    subtitle = subtitle
  )
gwr <- terra::rast(file.path(sim_dir, "07_interannual_gwr_NAD83.tif"))
gwrplot <- ggplot() +
  geom_spatraster(data = gwr) +
  scale_fill_viridis_c(option = "inferno") +
  labs(
    fill = "",
    title = "Ground Water Recharge",
    subtitle = subtitle
  )
cowplot::plot_grid(runoffplot, aetplot, gwrplot)

# 5-Clean simulated data ####
unlink(sim_dir, recursive = TRUE)
