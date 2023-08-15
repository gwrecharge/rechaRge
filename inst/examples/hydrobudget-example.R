# 1-Install the package ####

# Using remotes
remotes::install_github("gwrecharge/rechaRge", ref = "dev")

# OR using pak
pak::pkg_install("gwrecharge/rechaRge@dev")

# 2-Load the input data for the simulation and enter the parameters values ####

## 2.2-Input data ####
# use input example files provided by the package
examples_dir <- system.file("examples", package = "rechaRge")
input_rcn <- file.path(examples_dir, "input", "input_rcn.csv.gz")
input_climate <- file.path(examples_dir, "input", "input_climate.csv.gz") # precipitation total in mm/d
input_rcn_gauging <- file.path(examples_dir, "input", "input_rcn_gauging.csv.gz")

## 2.3-Calibration parameters ####
HB <- rechaRge::new_hydrobugdet(
  T_m = 2.1, # melting temperature (°C)
  C_m = 6.2, # melting coefficient (mm/°C/d)
  TT_F = -17.6, # Threshold temperature for soil frost (°C)
  F_T = 16.4, # Freezing time (d)
  t_API = 3.9, # Antecedent precipitation index time (d)
  f_runoff = 0.63, # Runoff factor (-)
  sw_m = 431, # Maximum soil water content (mm)
  f_inf = 0.07 # infiltration factor (-)
)

## 2.4-Simulation period ####
simul_period <- c(2010, 2017)

## 2.5-Parallel computing option ####
# nb_core <- 6 # if nothing is set, by default it will be all the computer core - 1

# 3-Simulation with the HydroBudget model ####
water_budget <- rechaRge::compute_recharge(
  HB,
  rcn = input_rcn,
  climate = input_climate,
  period = simul_period
  # nb_core = nb_core
)
head(water_budget)

# 4-Process the river flow observations and assess simulation quality ####
observed_flow <- file.path(examples_dir, "input", "observed_flow.csv.gz") # flow rates in mm/d
alpha_lyne_hollick <- file.path(examples_dir, "input", "alpha_lyne_hollick.csv.gz")

result <- rechaRge::compute_simulation_quality_assessment(
  HB,
  water_budget = water_budget,
  rcn_gauging = input_rcn_gauging,
  observed_flow = observed_flow,
  alpha_lyne_hollick = alpha_lyne_hollick,
  period = simul_period
)

result$gauging[[1]]$gauging
head(result$gauging[[1]]$comparison_month)
result$simulation_metadata

## 5-Save the simulation results ####
sim_dir <- file.path(getwd(), paste0("simulation_HydroBudget_", format(Sys.time(), "%Y%m%dT%H:%M")))

# 5.1-write output files
rechaRge::write_results(water_budget, output_dir = sim_dir)
rechaRge::write_rasters(
  water_budget = water_budget,
  input_rcn = input_rcn,
  crs = "+proj=lcc +lat_1=60 +lat_2=46 +lat_0=44 +lon_0=-68.5 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
  output_dir = sim_dir
)

# 5.2-list simulation output files
list.files(sim_dir)

data.table::fread(file.path(sim_dir, "01_bilan_spat_month.csv"))
data.table::fread(file.path(sim_dir, "02_bilan_unspat_month.csv"))

# data viz
library(tidyterra)
library(terra)
library(ggplot2)
library(cowplot)
subtitle <- ifelse(simul_period[1] == simul_period[2],
  paste0("In ", simul_period[1]),
  paste0("From ", simul_period[1], " to ", simul_period[2])
)
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
