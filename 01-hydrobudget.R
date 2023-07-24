
#1-Load the packages ####
  library("data.table")     #everything is in data.table
  library("plyr")           #spilt-apply-combine tools
  library("lubridate")      #to work with date
  library("zoo")            #for time gaps interpolation
  library("hydrostats")     #to compute baseflows with the Lyne and Hollick filter
  library("stats")          #to find the longest period without NA
  library("hydroGOF")       #to compute KGE
  library("stringr")        #for string operations
  library("sp")             #to work with CRS (for raster projections)
  library("raster")         #to export rasters
  library("airGR")          #to compute Oudin PET
  library("doParallel")     #for parallel computing on a Windows environment
  #library("doMC")          #for parallel computing on a non-Windows environment /!\ need to mute the stopCluster(cluster) after the parallel loop

  installpackage<-"no" #select "yes" only for the first use, to be sure that all the requested packages are installed
  if(installpackage == "yes"){
    install.packages("data.table")
    install.packages("plyr")
    install.packages("lubridate")
    install.packages("zoo")
    install.packages("hydrostats")
    install.packages("stats")
    install.packages("hydroGOF")
    install.packages("stringr")
    install.packages("doParallel")
    install.packages("sp")
    install.packages("raster")
    install.packages("airGR")
    #install.packages("doMC")
  }
    rm(installpackage)
  
#2-Load the input data for the simulation and enter the parameters values ####
  #2.1-Define main directory and create the folder to save the simulation results####
    main_dir<-"X:/XXX/XXX/XXX/HydroBudget/"
      # /!\ pay attention to keep a "/" at the end of the directory /!\
        #enter here the directory of the folder where the input data are located and where the simulation results will be saved
      
    run_directory <- paste(format(Sys.time(), "%Y_%m_%d-%H_%M"),"_simulation_HydroBudget" , sep = "")
    dir.create(paste(main_dir, run_directory, sep = ""))
    setwd(paste(main_dir, run_directory, sep = ""))
    rm(run_directory)
    getwd()
    
  #2.2-input data ####
    input_rcn<-fread(paste(main_dir, "01-input/input_rcn.csv", sep = ""))
    input_climate<-fread(paste(main_dir, "01-input/input_climate.csv", sep = ""))             #precipitation total in mm/d
    input_rcn_gauging<-fread(paste(main_dir, "01-input/input_rcn_gauging.csv", sep = ""))
    observed_flow<-fread(paste(main_dir, "01-input/observed_flow.csv", sep = ""))             #flow rates in mm/d
    alpha_lyne_hollick<-fread(paste(main_dir, "01-input/alpha_lyne_hollick.csv", sep = ""))
    
  #2.3-calibration parameters ####
    param<-data.table(T_m = 2.1,        #melting temperature (°C)
                      C_m = 6.2,        #melting coefficient (mm/°C/d)
                      TT_F = -17.6,     #Threshold temperature for soil frost (°C)
                      F_T = 16.4,       #Freezing time (d)
                      t_API = 3.9,      #Antecedant precipitation index time (d)
                      f_runoff = 0.63,  #Runoff factor (-)
                      sw_m = 431,       #Maximum soil water content (mm)
                      f_inf = 0.07)     #infiltration factor (-)
    
  #2.4-simulation period and spatial resolution ####
    simul_period<-c(2010, 2017)
    cell_size<-500               #considering that the RCN cells are square (m) - For your information only
    
    list_year <- seq(simul_period[1], simul_period[2],1)
    input_climate<-input_climate[year %in% list_year]
    observed_flow<-observed_flow[year %in% list_year]
    
  #2.5-Parallel computing option ####
    nb_core<-"6" #if nothing is set, by default it will be all the computer core - 1
    
#3-Process the river flow observations and load the HB function ####
    
  source(paste0(main_dir, "02-scripts_HB/01-river_flow_data_processing.R"))
    #this script 1/ select the available river flow observations for the simulation period and fill the gaps (up to 5 missing days)
                #2/ extract the list of the available gauging stations for the simulation period (list of the names)
                #3/ compute the Lyne and Hollick baseflow and resample river flow and baseflow with a monthly time step
    
  source(paste0(main_dir, "02-scripts_HB/02-HB_function.R"))
    #this script load the HydroBudget function (HB) into the local R environment
    
#4-Simulation with HB ####
  HB(param = param,
     input_rcn = input_rcn,
     input_rcn_gauging = input_rcn_gauging,
     input_climate = input_climate,
     simul_period = simul_period,
     nb_core = nb_core,
     observed_flow_month = observed_flow_month,
     gauging = gauging)

