# rechaRge [![CRAN status](https://www.r-pkg.org/badges/version/rechaRge)](https://CRAN.R-project.org/package=rechaRge) [![R-CMD-check](https://github.com/gwrecharge/rechaRge/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gwrecharge/rechaRge/actions/workflows/R-CMD-check.yaml)

> The results of the survey used to assess the community expectations regarding this package are presented here: https://github.com/gwrecharge/rechaRge-book/blob/main/community_survey/survey_main_results.md 

## Installation

Using `remotes`

```r
remotes::install_github("gwrecharge/rechaRge", ref = "main")
```

OR using `pak`

```r
pak::pkg_install("gwrecharge/rechaRge@main")
```

## Example

See [hydrobudget-example-simulation.R](https://github.com/gwrecharge/rechaRge-book/blob/main/examples/hydrobudget-example-simulation.R) and [hydrobudget-example-calibration.R](https://github.com/gwrecharge/rechaRge-book/blob/main/examples/hydrobudget-example-calibration.R).

## Publication

Original material from **HydroBudget – Groundwater recharge model in R** [https://doi.org/10.5683/SP3/EUDV3H](https://doi.org/10.5683/SP3/EUDV3H).

--------------------------------------------------

#### GENERAL INFORMATION

Title: HydroBudget – Groundwater recharge model in R

Authors:

    Emmanuel Dubois
    UQAM
    
    Marie Larocque
    UQAM

    Sylvain Gagne
    UQAM

    Guillaume Meyzonnat
    UQAM



Description:
HydroBudget (HB) is a spatially distributed groundwater recharge (GWR) model that computes a superficial water budget on grid cells with outputs aggregated into monthly time steps. It was developed as an accessible and computationally affordable model to simulate GWR over large areas (thousands of km2, regional-scale watersheds) and for long time periods (decades), in cold and humid climates.
The model is coded in R and was developed at UQAM by the team of Pr Marie Larocque’s research Chair (Water and land conservation) as part of a project funded by the Quebec Ministry of the Environment (Ministère de l’Environnement et de la Lutte contre les changements climatiques - MELCC). Results of GWR simulation over southern Quebec with HB are presented in Dubois et al. (2021).
Le model script is furnished with an application example for the Petite du Chene River in southern Quebec and a user guide.

Date of publication: 2021-11-05
Location of the dataset: Montréal, Quebec, Canada

DOI of this dataset: 10.5683/SP3/EUDV3H

Reference of this dataset: Dubois, E., Larocque, M., Gagné, S., and Meyzonnat, G. (2021a). HydroBudget – Groundwater recharge model in R. Scholars Portal Dataverse. doi: 10.5683/SP3/EUDV3H

The script was tested within the research team of Pr Larocque before making it publicly available.

--------------------------------------------------

#### ACCESS INFORMATION

Data licenses / restrictions, or usage limitations:

The HydroBudget code, developed by the research team of Pr Larocque, is open-licensed and therefore assigned to a CC BY licence.

For the application example of the Petite du Chene River, the climate data and river flow observations are provided for training purpose only. They are under a governmental licence (http://www.droitauteur.gouv.qc.ca/copyright.php):

- The climate data was extracted and post-processed from the interpolated climate data provided by the MELCC (Bergeron, 2016) for the extend of the watershed of the Petite du Chene River. These data are available for training purpose only.
- River flow observations are provided by the CEHQ (Centre de l’Expertise Hydrique du Quebec) and the raw data are available here:
    https://www.cehq.gouv.qc.ca/hydrometrie/historique_donnees/fiche_station.asp?NoStation=023701
    https://www.cehq.gouv.qc.ca/hydrometrie/historique_donnees/fiche_station.asp?NoStation=023702

References for these datasets are, respectively:

- Bergeron, O. (2016). Guide d’utilisation 2016 - Grilles climatiques quotidiennes du Programme de surveillance du climat du Québec, version 2 (User guide 2016 – Daily climate grids from the Quebec Climate monitoring program, version 2). Québec City (Canada) : ministère du Développement durable, de l’Environnement et de la Lutte contre les changements climatiques, Direction du suivi de l’état de l’environnement.
- CEHQ (Centre d’expertise hydrique du Québec). (2021). cehq.gouv.qc.ca website accessed in November 2021.

As well, these GIS files of the application example have licence restrictions:

- L_watercourse_NRCAN_petite_riv_du_chene_NAD83.shp open government licence – Canada (https://open.canada.ca/en/open-government-licence-canada)
- S_gauging_station_watersheds_NAD83.shp government licence – Quebec (http://www.droitauteur.gouv.qc.ca/copyright.php)


Associated publication:

> Dubois, E., Larocque, M., Gagné, S. and Meyzonnat, G. (2021). Simulation of long-term spatiotemporal variations in regional-scale groundwater recharge: contributions of a water budget approach in cold and humid climates. Hydrology and Earth System Sciences, 25(12), 6567‑6589. https://doi.org/10.5194/hess-25-6567-2021

--------------------------------------------------

#### DATA & FILES OVERVIEW

The dataset is composed of 3 components:

- the current READ_ME.txt file
- the HydroBudget user-guide
- the HydroBudget folder (17 files)

The HydroBudget folder contains different folders:

**folder 01-input contains the input data**

- alpha_lyne_hollick.csv: statistically calibrated α following Ladson et al. (2013) procedure for the Lyne and Hollick (1979) baseflow computation for the two gauging stations (the baseflow computation itself is included in HB script). The file contains two attributes:
    - station: name of the gauging station
    - alpha: value of the calibrated alpha parameter

- input_climate.csv: daily total precipitation (mm/d) and average daily temperature (°C) of the Quebec climate interpolated grid (Bergeron, 2016) from 1961-01-01 to 2017-12-31. The file contains six attributes:
    - climate_cell: ID of the 10 km x 10 km climate cell
    - day: day of the date
    - month: month of the date
    - year: year of the date
    - t_mean: average temperature of the day (°C)
    - p_tot: total precipitation of the day (mm/d)
    - lat: latitude of the climate cell center (°)

- input_rcn.csv: RCN values on a 500 m x 500 m grid. A RCN value is given for each grid cell of the watershed with the corresponding climate cell and the coordinates of the center of each RCN cell in NAD83 Quebec Lambert (EPSG: 32198):
    - climate_cell: ID of the 10 km x 10 km climate cell
    - cell_ID: ID of the 500 m x 500 m RCN cell
    - RCNII: value of the RCN computed for the RCN cell
    - X_L93: x coordinate of the center of the RCN cell
    - Y_L93: y coordinate of the center of the RCN cell

- input_rcn_gauging.csv: table with the list of RCN cells located in each gauging station watershed. The table is composed of two attributes:
    - cell_ID: ID of the 500 m x 500 m RCN cell
    - gauging_stat: gauging station associated to that cell
      Note: Since the watersheds of the two gauging stations can be overlaying, RCN cells can be associated with the two gauging stations; in that case, the RCN cell ID appears twice in the table, once with each gauging station

- observed_flow.csv: measured river flow (mm/d) of the 2 gauging stations for the entire time period covered by the climate data with:
    - year: year of the date
    - month: month of the date
    - day: day of the date
    - 23701: measured river flow at the 23701 station (mm/d)
    - 23702: measured river flow at the 23702 station (mm/d)
      Note: unavailable data (including if flow measurements do not exist for a given period) are marked with a “NA” (Not Available).

Note: all the csv files are “data.table” formatted for R. The extension of the files can change depending on the chosen downloading option.

Note: on the Dataverse website, the file extension of the input data is .tab


**folder 02-scripts_HB contains R scripts**

- 01-river_flow_data_processing.R: R script used to process the observed river flow rates. It will automatically fill the gaps in the daily time series (up to 5 missing days) and select a subset of the longest period of river flow observations within the simulation period for each station; extract the list of the gauging stations with observations during the simulation period; compute the Lyne and Hollick baseflow using the calibrated α for the gauging stations; and resample river flows and baseflows with a monthly time step.

- 02-HB_function.R: R script containing the HydroBudget function (HB). The function will run HB in parallel on all the grid cells of the watershed following Figure 1 processes. After defining all the variables in the function (sections 1.1 to 1.4), a first parallel loop runs the degree-day model and compute Oudin PET on each climate cell for the chosen simulation period (section 1.5 of the script). A second parallel loop runs HB water partitioning on each RCN cell (section 1.6 of the script). To start and finish the parallel loops, the code needs to be changed if the model is run on a computer without a Windows environment (change package “doParallel” for Windows to package “doMC”). The options for the non-Windows environment are muted by default. The monthly spatialized and averaged water budget is saved in the working directory. As well, the function automatically analyzes the results for each available gauging station based on the comparison of the simulated monthtly total flow (runoff + excess runoff + potential GWR) and potential GWR to the monthly river flow and baseflow (section 1.7). For each station the code considers a calibration period (first 2/3 of the observation period) and a validation period (last 1/3 of the observation period). The last section (1.8) exports rasters of the interannual simulated runoff, AET, and potential GWR with the resolution of the RCN grid and in NAD83 Quebec Lambert coordinates (EPSG: 32198) into the working directory. In case the RCN method needs to be adapted to another version of the method, changes need to be done in the subsections 3.6.1.3 to 3.6.1.5 of the parallel loop of the model.

**folder 03-GIS_petite_du_chene contains GIS files for the application example**

Note: all coordinates are in NAD83 Quebec Lambert (EPSG: 32198)

Note: all the files associated to a shapefile are in a compressed ZIP file

- L_watercourse_NRCAN_petite_riv_du_chene_NAD83.zip (lines): Petite du Chene River watercourse extracted and simplified (one attribute with the name of the river) from https://www.nrcan.gc.ca/science-and-data/science-and-research/earth-sciences/geography/topographic-information/geobase-surface-water-program-geeau/national-hydrographic-network/21361

- P_gauging_stations_petite_riv_du_chene_NAD83.zip (points): location of the gauging station with 3 attributes:
    - station_id: name of the gauging station
    - x_NAD83: x coordinate of the station
    - y_NAD83: y coordinate of the station

- R_RCN_NAD83.tif (raster): raster of the RCN values for the RCN cells

- S_climate_grid_petite_riv_du_chene_NAD83.zip (polygons): 10 km x 10 km climate cells for the Petite du Chene River watershed (one attribute with the climate cell ID)

- S_gauging_station_watersheds_NAD83.zip (polygons): watersheds of the gauging stations (one attribute with the name of the gauging station) from https://www.cehq.gouv.qc.ca/hydrometrie/index.htm

- S_grid_gauging_petite_riv_du_chene_NAD83.zip (polygons): grid for the gauging stations with 3 attributes:
    - cell_id: ID of the 500 m x 500 m RCN cell
    - gauging_st: gauging station
    - clim_cell: ID of the 10 km x 10 km climate cell
      Note: Since the watersheds of the two gauging stations can be overlaying, RCN cells can be associated with each of them; in that case, the cell_id appears twice in the attribute table, once with each gauging station

- S_grid_petite_riv_du_chene_NAD83.zip (polygons): grid for the river watershed with 2 attributes:
    - cell_id: ID of the 500 m x 500 m RCN cell
    - clim_cell: ID of the 10 km x 10 km climate cell

- S_watershed_petite_riv_du_chene_simul_NAD83.zip (polygon): watershed of the Petite du Chene River (one attribute corresponding to the ID given to the watershed in Dubois et al. (2021))


**00-demonstration_HB.Rproj: R project**

It opens the R scripts 01-HydroBudget.R (if it does not, manually load the script in the R project)

**01-HydroBudget.R: R script that runs HB on the Petite du Chene River**

This script will run HB on the Petite du Chene River with a single set of parameters. It contains the references to the inputs data located in the folder 01-input previously detailed and the references to the scripts located in 02-scripts_HB. It creates a folder for the results, simulates the water budget on the watershed, saves the results for the entire watershed (spatially distributed, averaged on the watershed, and averaged for each gauging station), computes the objective functions for the 2 gauging stations per period ((KGEqtot)station, (KGEqbase)station, and KGEmean for both calibration and validation periods), and saves a summary of the simulation in a separate csv file;

The HydroBudget user guide provides details for the HydroBudget model as well as for the application example on the Petite du Chene River. It should be cited as:

> Dubois, E., Larocque, M., Gagné, S. et Meyzonnat, G. (2021). HydroBudget User Guide: Version 1.1. Université du Québec à Montréal. Département des sciences de la Terre et de l’atmosphere. https://archipel.uqam.ca/14075/

--------------------------------------------------

#### INFORMATION ON METHODOLOGY

The technical details, equations ruling the model, sensitivity analysis, and calibration procedure are detailed in the HydroBudget user guide and in the publication from Dubois et al. (2021). Users are invited to refer to these documents for more information.

The packages used for the simulation of groundwater recharge with the Hydrobuget model are:

* data.table: (version 1.13.6) used to optimize computation time
* plyr: (version 1.8.6) spilt-apply-combine tools
* lubridate: (version 1.8.0) to work with date
* zoo: (version 1.8.8 ) for time gaps interpolation
* hydrostats: (version 0.2.7) to compute baseflows with the Lyne and Hollick filter
* stats: (version 4.0.3) to find the longest period without NA
* hydroGOF: (version 0.4.0) to compute KGE
* stringr: (version 1.4.0) for string operations
* sp: (version 1.4.5) to work with CRS (for raster projections)
* raster: (version 3.4.5) to export rasters
* airGR: (version 1.4.3.65) to compute Oudin PET
* doParallel: (version 1.0.16) for parallel computing on a Windows environment
* doMC: (version 1.3.7) for parallel computing on a non-Windows environment /!\ need to mute the stopCluster(cluster) after the parallel loops

