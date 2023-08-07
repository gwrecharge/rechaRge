#automatic calibration of HB with the package caRamel
#if needed, install the packages ####
  #install.packages("Rcpp")
  #library("Rcpp")
  #install.packages("geometry")
  #library("geometry")
  #install.packages("hydrostats")
  #install.packages("plyr")
  #install.packages("lubridate")
  #install.packages("reshape2")
  #install.packages("zoo")
  #install.packages("data.table")
  #install.packages("FlowScreen")
  #install.packages("sp")
  #install.packages("stats")
  #install.packages("hydroGOF")
  #install.packages("Kendall")
  #install.packages("stringr")
  #install.packages("foreach")
  #install.packages("doMC")
  #install.packages("caRamel")
  

#1-Load the packages ####
  library("hydrostats")
  library("plyr")
  library("lubridate")
  library("reshape2")
  library("zoo")
  library("data.table")
  library("FlowScreen")
  library("sp")
  library("stats")
  library("hydroGOF")
  library("Kendall")
  library("stringr")
  #library("doParallel")
  #library("doSNOW")
  library("doMC")
  #install.packages("caRamel")
  library("caRamel")

#2-HB function ####
  #2.1-create a folder where to write all the simulations synthesis ####
    setwd("/Data2/E_Dubois/03-automatic_calibration/")
    nom_station <- 16
    run_directory <- paste(format(Sys.time(), "%Y_%m_%d-%H_%M"),"_ws", nom_station,"_rcn_correct", sep = "")
    dir.create(run_directory)
    setwd(run_directory)
    
  #2.2-preload the data set ####
    #intrants_rcn_pre_old<-fread("/Data2/E_Dubois/03-automatic_calibration/01-input_data/rcn_cells_river3.csv")
    intrants_rcn_pre<-fread("/Data2/E_Dubois/03-automatic_calibration/01-input_data/rcn_cells_river5.csv")
    intrants_rcn_pre<-intrants_rcn_pre[group_ws %in% nom_station]
    intrants_rcn_pre$taille_cellules_m2<-250000
    intrants_rcn_pre<-intrants_rcn_pre[,c(2, 1, 7, 5:6, 8)]
    colnames(intrants_rcn_pre)<-c("Maille", "ID", "RCNII", "X", "Y", "taille_cellules_m2 river_ws")
    
    
    list_gauging_selec<-fread("/Data2/E_Dubois/03-automatic_calibration/01-input_data/list_retained_gauging_v2.csv")
    ifelse(nom_station %in% list_gauging_selec$subwatersh == FALSE,
           list_gauging_selec<-NA_character_,
           list_gauging_selec<-as.character(list_gauging_selec[subwatersh == nom_station][[1]]))
    gauging<-c(as.numeric(list_gauging_selec))
    #rcn_gauging_pre_old<-fread("/Data2/E_Dubois/03-automatic_calibration/01-input_data/rcn_cells.csv")
    #rcn_gauging_pre_old[,c(2,7)]
    rcn_gauging_pre<-fread("/Data2/E_Dubois/03-automatic_calibration/01-input_data/rcn_cells_gauging5.csv")
    rcn_gauging_pre<-rcn_gauging_pre[station3 %in% gauging, c(1,3)]
    colnames(rcn_gauging_pre)<-c("ID", "gauging_stat")
        rm(gauging, list_gauging_selec)
        
    #to reduce the watershed to the RCN cell only used for the calibration measurement <=> the RCN 
        #from the gauging stations
    intrants_rcn_pre<-intrants_rcn_pre[ID %in% (unique(rcn_gauging_pre$ID))]
    
    intrants_meteo_pre <- fread("/Data2/E_Dubois/03-automatic_calibration/01-input_data/climatics3.csv.csv")
    intrants_meteo_pre <- intrants_meteo_pre[cell %in% unique(intrants_rcn_pre$Maille)]
    
  #2.3-Function ####
    HB<-function(i){
      gc()
      nom_station <- nom_station
      intrants_rcn<-intrants_rcn_pre
      intrants_meteo<-intrants_meteo_pre
      rcn_gauging<-rcn_gauging_pre
      
      #x<-c(0, 4.4, 10, -10, 3, 0.6, 0.03, 300)
      #x<-matrix(data = c(0, 4.4, 10, -10, 3, 0.6, 0.03, 300), ncol = 8, nrow = 1)
      #x<-matrix(data = c(0, 4.4, -10, 3, 0.6, 0.03, 300), ncol = 7, nrow = 1) #if only 7 parameters
      #i<-1
      #x1<-0
      #x2<-4.4
      #x3<-10
      #x4<--10
      #x5<-3
      #x6<-0.60
      #x7<-0.03
      #x8<-300
      
      
      #0-Load the packages ####
        library("hydrostats")
        library("plyr")
        library("lubridate")
        library("reshape2")
        library("zoo")
        library("data.table")
        library("FlowScreen")
        library("sp")
        library("stats")
        library("hydroGOF")
        library("Kendall")
        library("stringr")
        library("doMC")
        
      
      
      #1-directories ####
        #calib_directory<-"D:/OneDrive - UQAM/1-UQAM-these/03-data/12-HYDROBILAN/20-automatic_calibration/"
        calib_directory<-"/home/dubois/Data/03-automatic_calibration/"
        #setwd(calib_directory)
        #getwd() 
         
      #2-cluster parameters ####
        nb_core<-"15"
        ifelse(nb_core == "", 
               nb_core<-detectCores()-1,
               nb_core<-as.numeric(nb_core))
        watershed<-"river"
        
      #3-Choose the watershed ####
        #Choose the ID of the station or the river /!\ should be the same as in the RCN cell grid data
        #nom_station <- 16
        
        #/!\group of ungauged watersheds made by geographical location for # > 48
        if (nom_station > 48){
          groupe_ws<-nom_station
          list_group_ws<-fread(paste(calib_directory, "01-input_data/list_river_ws_grp.csv", sep = ""))
          nom_station<-list_group_ws$ID[which(list_group_ws$group == groupe_ws)]
        }
        
      #4-List of the gauging stations of the watershed ####
        #list of gauging station per watershed => list_gauging_selec
        list_gauging_selec<-fread(paste(calib_directory, "01-input_data/list_retained_gauging_v2.csv", sep = ""))
        ifelse(nom_station %in% list_gauging_selec$subwatersh == FALSE,
               list_gauging_selec<-NA_character_,
               list_gauging_selec<-as.character(list_gauging_selec[subwatersh == nom_station][[1]]))
        gauging<-c(as.numeric(list_gauging_selec))
      
      #5-Simulated period ####
        begin_modeling<-"1961"
        end_modeling<-"2017"  
        
        # define the modeling eras depending on the period chosen
        #define the period of calibration and validation based on the modeling period
        #define just before or on the longest period of available flow rates measurements
        debut_simulation<-as.numeric(begin_modeling)
        fin_val<- as.numeric(end_modeling)
        fin_simulation <- debut_simulation+round((fin_val-debut_simulation)*2/3)
        debut_val<- fin_simulation+1
        liste_year <- seq(debut_simulation, fin_val,1)
      
      #6-RCN and area####
        maille_area <- 500*500
        study_area <- length(unique(intrants_rcn$ID))*maille_area
        
      #7-Calibration parameters ####
        #7.1-Snow model
          T_snow<-0
          T_m<-x[i,1]
          C_m<-x[i,2]
      
        #7.2-soil frost
          #temps_gel_sol <- x[i,3] #time to freeze the soil removed - need to do +1 on each column to reput it
          temps_gel_sol <- 16.4
          temp_gel <- x[i,3] #4 if temps_gel_sol is in
          
        #7.3-runoff
          temps_api <- x[i,4]
          facteur_rcn_2 <- x[i,5]
          
        #7.4-soil parameters
          pourc_infiltration <- x[i,6]
          volume_max_vadose<- x[i,7]
          vadose_depart <- 50
      
      #8-Load the climate data ######
        #intrants_meteo <- fread(paste(calib_directory, "01-input_data/climatics3.csv.csv", sep = ""))
        #intrants_meteo <- intrants_meteo[cell %in% unique(intrants_rcn$Maille)]
        #intrants_meteo <- fread(paste(calib_directory, "01-input_data/ws19_climatics3.csv", sep = "")) #only for ws19
        #fwrite(intrants_meteo, paste(calib_directory, "01-input_data/ws24_climatics3.csv", sep = "")) #only for ws24
        
      #9-execute the snow model ####
        #9.1-convert the input data ####
          climate_data<-intrants_meteo
          colnames(climate_data)<-c("climate_cell", "day", "month", "year", "julian_day", "tmean", "total_precip", "PET")
          
        #9.2-Make cluster ####
          #cluster<-makeCluster(nb_core)
          #registerDoParallel(cluster)
          #registerDoSNOW(cluster)
          registerDoMC(nb_core)
          
        #9.3-Start the parallel loop ####
          #pb <- txtProgressBar(min = 0, max =length(unique(climate_data$climate_cell)), style = 3)
          #progress <- function(n) setTxtProgressBar(pb,n)
          #opts<- list(progress = progress)
          #system.time(
            #climate_data_VI<-foreach(k = 1 : (length(unique(climate_data$climate_cell))), .combine = rbind, .inorder = FALSE, .options.snow = opts) %dopar% {
            climate_data_VI<-foreach(k = 1 : (length(unique(climate_data$climate_cell))), .combine = rbind, .inorder = FALSE) %dopar% {
              #k<-1
              library("data.table")
              cellgrid<-as.character(unique(climate_data$climate_cell)[k])
              input_dd<-climate_data[climate_cell == cellgrid]
              #colnames(input_dd)<-c("cell", "time", "tmean", "total_precip")
              input_dd$snow<-ifelse(input_dd$tmean<T_snow,input_dd$total_precip,0)
              input_dd$rain<-ifelse(input_dd$tmean>T_snow,input_dd$total_precip,0)
              input_dd$dd<-ifelse(input_dd$tmean-T_m>0,input_dd$tmean-T_m,0)
              input_dd$melt<-NA
              input_dd$storage<-NA
              #loop for the snowpack ############
              for(y in 1:nrow(input_dd)){
                #y<-1
                input_dd$melt[y] <- ifelse(y==1,ifelse(input_dd$dd[y]*C_m<0,input_dd$dd[y]*C_m,0),
                                           ifelse(input_dd$dd[y]*C_m<input_dd$storage[y-1],
                                                  input_dd$dd[y]*C_m,
                                                  input_dd$storage[y-1]))
                input_dd$storage[y] <- ifelse(y==1,
                                              ifelse(0+input_dd$snow[y]-input_dd$melt[y]>0,
                                                     0+input_dd$snow[y]-input_dd$melt[y],0),
                                              ifelse(input_dd$storage[y-1]+input_dd$snow[y]-input_dd$melt[y]>0,
                                                     input_dd$storage[y-1]+input_dd$snow[y]-input_dd$melt[y],0))
              }
              rm(y)
              input_dd
            }
          #)
          #stopCluster(cluster)
          #stopImplicitCluster()
          
        #9.4-VI and instrants_meteo ####
          climate_data_VI$VI<-climate_data_VI$rain+climate_data_VI$melt
          intrants_meteo<-climate_data_VI[,c(1:6,14, 8)]
          colnames(intrants_meteo)<-c("Maille", "Jour", "Mois", "Annee", "JourJulien", "Tmoy", "AV", "PET")
          
        #9.5-Clean ####
          #rm(climate_data_VI, climate_data, cluster, opts, pb, progress)
          #rm(climate_data_VI, climate_data, cluster)
          rm(climate_data_VI, climate_data)
          gc()
         
      #10-Exe HB ####
        #10.1-Model loop by grid cell #####
          #10.1.1-Load data ####
            unique_rcn_maille <- unique(intrants_rcn$ID)
            
          #10.1.2-Make cluster ####
            #cluster<-makeCluster(nb_core)
            #registerDoParallel(cluster)
            #registerDoSNOW(cluster)
            registerDoMC(nb_core)
                 
          #10.1.3-Start the parallel loop ####
            water_budget<-foreach(j = 1 : (length(unique_rcn_maille)), .combine = rbind, .inorder = FALSE) %dopar% {
                #j<-1
                library(data.table)
                library(zoo)
                #10.1.3.1-subsets ####  
                  intrants_rcn_subset <- intrants_rcn[ID == unique_rcn_maille[j]]
                  
                  rcn_meteo <- merge(intrants_rcn_subset, intrants_meteo, by = "Maille", all.x = TRUE)
                  
                  rcn_meteo <- na.omit(rcn_meteo[order(rcn_meteo$Maille,rcn_meteo$ID, rcn_meteo$Annee, rcn_meteo$Mois, rcn_meteo$Jour),])
                  
                #10.1.3.2-Boucle qui extrait la temp?rature moyenne selon le Temps_gel_sol ####
                  temperature_gel <- data.table(
                    "year" = as.numeric(rcn_meteo$Annee),
                    "month" = as.numeric(rcn_meteo$Mois),
                    "day" = as.numeric(rcn_meteo$Jour),
                    "jour_julien" = as.numeric(rcn_meteo$JourJulien),
                    "temperature" = as.numeric(rcn_meteo$Tmoy))
                  
                  roll_mean_gel <- as.numeric(rollmean(temperature_gel$temperature, temps_gel_sol, fill=NA, na.pad = T, align = "right"))
                  temperature_gel$temperature_gel <- ifelse(is.na(roll_mean_gel), temperature_gel$temperature, roll_mean_gel)
                  rm(roll_mean_gel)
                  
                #10.1.3.3-Boucle pour le calcul de l'API ####
                  api <- data.table(
                    "year" = as.numeric(rcn_meteo$Annee),
                    "month" = as.numeric(rcn_meteo$Mois),
                    "day" = as.numeric(rcn_meteo$Jour),
                    "jour_julien" = as.numeric(rcn_meteo$JourJulien),
                    "av" = as.numeric(rcn_meteo$AV))
                  
                  
                  roll_sum_api <- as.numeric(rollsum(rcn_meteo$AV, temps_api, fill=NA, na.pad = T, align = "right"))
                  api$api <- ifelse(is.na(roll_sum_api), api$av, roll_sum_api)
                  rm(roll_sum_api)
                  
                #10.1.3.4-Boucle pour le calcul des RCN en fonction des conditions ant?c?dantes (API) ####
                  fonction_rcn_1 <- min(100,(0.00865*rcn_meteo$RCNII^2)+(0.0148*rcn_meteo$RCNII)+7.39846)
                  fonction_rcn_3 <- min(100,(-0.00563*rcn_meteo$RCNII^2)+(1.45535*rcn_meteo$RCNII)+10.82878)
                  
                  rcn_api <- data.table(
                    "year" = as.numeric(rcn_meteo$Annee),
                    "month" = as.numeric(rcn_meteo$Mois),
                    "day" = as.numeric(rcn_meteo$Jour),
                    "jour_julien" = as.numeric(rcn_meteo$JourJulien),
                    "temperature_moy" =as.numeric(rcn_meteo$Tmoy),
                    "temperature_gel" =  as.numeric(temperature_gel$temperature_gel),
                    "rcn_II" = as.numeric(rcn_meteo$RCNII*facteur_rcn_2),
                    "api" = as.numeric(api$api))
                  
                  #api_1 correspond aux jours julien de l'hiver (valeur de 1)
                  #api_2 correspond aux jours juliens du printemps et de l'automne(valeur de 2)
                  #L'?t? correspond ? la valeur de 0
                  
                  rcn_api$rcn3<-((-0.00563)*rcn_api$rcn_II^2)+(1.45535*rcn_api$rcn_II)+10.82878
                  rcn_api$rcn1 <- (0.00865*rcn_api$rcn_II^2)+(0.0148*rcn_api$rcn_II)+7.39846
                  
                  api_1<-ifelse(rcn_meteo$JourJulien < 121 | rcn_meteo$JourJulien > 283,1,0)
                  api_2<-ifelse((rcn_meteo$JourJulien >120 & rcn_meteo$JourJulien <181) | (rcn_meteo$JourJulien >243 & rcn_meteo$JourJulien <284),2,0)
                  rcn_api$saison <- api_1+api_2
                  
                  
                  rcn_api$rcn_hiver <- ifelse(rcn_api$saison == 1, ifelse(rcn_api$api>22, rcn_api$rcn3, ifelse(rcn_api$api<11, rcn_api$rcn1,rcn_api$rcn_II)),0)
                  rcn_api$rcn_printemps_automne <- ifelse(rcn_api$saison == 2, ifelse(rcn_api$api>37, rcn_api$rcn3, ifelse(rcn_api$api<18.5, rcn_api$rcn1,rcn_api$rcn_II)),0)
                  rcn_api$rcn_ete <- ifelse(rcn_api$saison == 0, ifelse(rcn_api$api<50, rcn_api$rcn1, ifelse(rcn_api$api>80, rcn_api$rcn3,rcn_api$rcn_II)),0)
                  
                  #comptabilisation des rcn par saison en un vecteur et correction pour le gel
                  
                  if(intrants_rcn_subset$RCNII == 100){                                                               #is the RCNII value = 100 (water, wetlands)
                    rcn_api$rcn_api<-ifelse(temperature_gel$temperature_gel< temp_gel,                                #is the soil frozen ?
                                            100,                                                                      #if yes, RCN = 100 % (just runoff)
                                            10)                                                                       #if no, consider RCN = 10
                  }else{
                    rcn_api$rcn_api <- ifelse(temperature_gel$temperature_gel< temp_gel,                                #is the soil frozen ?
                                              100,                                                                      #if yes, RCN = 100 % (just runoff)
                                              rcn_api$rcn_ete+rcn_api$rcn_printemps_automne+rcn_api$rcn_hiver) 
                  }
                  
                #10.1.3.5-Calcul du ruissellement ######
                  sat<-(1000/rcn_api$rcn_api)-10
                  #Calcul du ruissellement
                  runoff <- data.table(
                    "maille_av" = as.numeric(rcn_meteo$Maille),
                    "maille_rcn"= as.numeric(rcn_meteo$ID),
                    "year" = as.numeric(rcn_meteo$Annee),
                    "month" = as.numeric(rcn_meteo$Mois),
                    "day" = as.numeric(rcn_meteo$Jour),
                    "jour_julien" = as.numeric(rcn_meteo$JourJulien),
                    "rcn" = as.numeric(rcn_meteo$RCNII),
                    "rcn_corrected" = rcn_api$rcn_api,
                    "AV" =  as.numeric(rcn_meteo$AV)
                  )
                  
                  runoff$runoff <-   (runoff$AV-(0.2*sat))^2/(runoff$AV+(0.8*sat))
                  runoff$runoff <-  ifelse(runoff$AV>(0.2*sat), runoff$runoff,0)
                  
                #10.1.3.6-calcul de l'eau disponible pour infiltration et ETP ####
                  eau_disponible <- data.table(
                    "year" = as.numeric(rcn_meteo$Annee),
                    "month" = as.numeric(rcn_meteo$Mois),
                    "day" = as.numeric(rcn_meteo$Jour),
                    "jour_julien" = as.numeric(rcn_meteo$JourJulien),
                    "AV" =  as.numeric(rcn_meteo$AV),
                    "runoff" = as.numeric(runoff$runoff)
                  )
                  
                  eau_disponible$eau_disponible <- ifelse((eau_disponible$AV-eau_disponible$runoff)<0,0,eau_disponible$AV-eau_disponible$runoff)
                  
                #10.1.3.7- Calcul du bilan hydrique####
                  #Le bilan 1 calcul l'eau de la journ?e pr?c?dente, l'eau de la journ?e et enl?ve l'etr
                  
                  bilan_1<-vector()
                  infiltration <- vector()
                  bilan_2 <- vector()
                  #etp_list<- etp_parametres$etp
                  etp_list<- rcn_meteo$PET
                  eau_disponible_list <-eau_disponible$eau_disponible
                  etr<-vector()
                  delta_reservoir<- vector()
                  runoff_2<-vector()
                  
                  for (ii in 1:nrow(rcn_meteo)){
                    #pos_jour_precedent <- ii-1
                    if(ii == 1){
                      bilan_1[ii]<- 0
                      infiltration[ii] <-0
                      bilan_2[ii] <- vadose_depart
                      etr[ii] <- 0
                      runoff_2 <-0
                    }else{
                      bilan_1[ii] <-ifelse((bilan_2[ii-1]+eau_disponible_list[ii])> volume_max_vadose, volume_max_vadose, bilan_2[ii-1]+eau_disponible_list[ii])
                      etr[ii]<- min(bilan_1[ii],etp_list[ii])
                      infiltration[ii]<-ifelse(intrants_rcn_subset$RCNII == 100,                                            #is the cell water or wetland?
                                               0,                                                                           #if yes, no infiltration
                                               (bilan_1[ii]-etr[ii])*((bilan_1[ii]/volume_max_vadose)*pourc_infiltration))  #if not, normal infiltration computation
                      bilan_2[ii] <- bilan_1[ii]-etr[ii]-infiltration[ii]
                      runoff_2[ii] <- ifelse((eau_disponible_list[ii] + bilan_1[ii])> volume_max_vadose, (eau_disponible_list[ii] + bilan_1[ii])-volume_max_vadose,0)
                    }}
                      rm(ii)
                  
                  delta_reservoir <- as.numeric(c(0,diff(bilan_2)))
                  
                #10.1.3.8-Calcul du bilan final #####
                  bilan_final_dt <- data.table(
                    "maille_av" = as.integer(rcn_meteo$Maille),
                    "maille_rcn" = as.integer(rcn_meteo$ID),
                    "x" = rcn_meteo$X,
                    "y" = rcn_meteo$Y,
                    "year" = as.integer(rcn_meteo$Annee),
                    "month" = as.integer(rcn_meteo$Mois),
                    "day" = as.integer(rcn_meteo$Jour),
                    "jour_julien" = as.integer(rcn_meteo$JourJulien),
                    "AV" =  as.numeric(rcn_meteo$AV),
                    "tmean"=as.numeric(temperature_gel$temperature),
                    "runoff" = as.numeric(runoff$runoff),
                    "eau_disponible" = as.numeric(eau_disponible$eau_disponible),
                    "etp" = as.numeric(rcn_meteo$PET),
                    "infiltration" =  as.numeric(infiltration),
                    "vadose" =  as.numeric(bilan_2),
                    "delta_reservoir" = as.numeric(delta_reservoir),
                    "etr" = as.numeric(etr),
                    "runoff_2" = as.numeric(runoff_2))
                  
                  bilan_final <-  bilan_final_dt[,. (AV = sum(AV),
                                                     tmean = mean(tmean),
                                                     runoff = sum(runoff),
                                                     #eau_disponible = sum(eau_diponible),
                                                     etp = sum(etp),
                                                     etr = sum(etr),
                                                     infiltration = sum(infiltration),
                                                     runoff_2 = sum(runoff_2),
                                                     delta_reservoir = sum(delta_reservoir)),. (year, month)]
                  bilan_final[,(names(bilan_final)[3:ncol(bilan_final)]) := round(.SD,1), .SDcols=names(bilan_final)[3:ncol(bilan_final)]]
                  bilan_final[, maille_rcn :=unique_rcn_maille[j]]
                  bilan_final
              }
            #)
            #stopCluster(cluster)
            #stopImplicitCluster()
            
        #10.2-Clean ####
          #rm(unique_rcn_maille, cluster, opts, pb, progress)
          rm(unique_rcn_maille)
          gc()
          
      #11-Error computation####
        #11.1-Error by gauging station ####
          #11.1.1-import the rcn cells for the selected gauging stations ####
            #rcn_gauging<-fread(paste(calib_directory, "01-input_data/rcn_cells.csv", sep = ""))
            #rcn_gauging<-rcn_gauging[gauging_stat %in% gauging, c(2,7)]
            #rcn_gauging<-fread(paste(calib_directory, "01-input_data/ws19_rcn_cells.csv", sep = ""))#only for ws 19
            #fwrite(rcn_gauging, paste(calib_directory, "01-input_data/ws24_rcn_cells.csv", sep = ""))#only for ws 24
            
          #11.1.2-Import the river flow rates ####
            debit_observed<-fread(paste(calib_directory, "01-input_data/flow_mm_day_1961_2017.csv", sep = ""))
            debit_observed<-debit_observed[,c(1:3,which(colnames(debit_observed)%in% gauging)), with = FALSE]
            for (c in 4:ncol(debit_observed)){
              debit_observed[[c]][2:(nrow(debit_observed)-1)]<-na.approx(debit_observed[[c]][2:(nrow(debit_observed)-1)],
                                                                         maxgap = 5,na.rm = FALSE)
            }
            rm(c)
            debit_observed$date<-as.POSIXct(paste(debit_observed$year, debit_observed$month, debit_observed$day, sep = "-"),
                                            format = "%Y-%m-%d", tz = "UTC")
            debit_observed<-debit_observed[,c(ncol(debit_observed), 1:3, 4:(ncol(debit_observed)-1)), with = FALSE]
            
            debit_observed_annee_mois <- data.table(year = c(rep(unique(debit_observed$year),each = 12)), 
                                                    month = c(rep(c(1:12),length(unique(debit_observed$year)))))
            
          #11.1.3-Compute baseflow ####
            mround <- function(x,base){base*round(x/base)}
            alpha_lyne_hollick_calibrated<-fread(paste(calib_directory, "01-input_data/alpha_lyne_hollick_calibrated.csv", sep = ""))
            #loop to add the baseflow to the observated river flow
              for(c in 5:ncol(debit_observed)){
                #c<-6
                bf<-debit_observed[,c(1:3,c), with = FALSE]
                bf<-na.contiguous(bf)
                colnames(bf)<-c("Date", "year", "month", "Q")
                bf$bf_lh<-baseflows(bf[,c(1,4), with = FALSE],
                                    alpha_lyne_hollick_calibrated$alpha[which(alpha_lyne_hollick_calibrated$station==
                                                                                colnames(debit_observed)[c])], 
                                    n.reflected = 30, ts="daily")[,3]
                q_month<-bf[,.(qmonth = sum(Q, na.rm = TRUE), bf_lh_month = sum(bf_lh, na.rm = TRUE)),.(year, month)]
                colnames(q_month)[3:4]<-c(colnames(debit_observed)[c],paste(colnames(debit_observed)[c],"_bf", sep = ""))
                ifelse(ncol(bf) == 6,
                       colnames(bf)<-c("date", "year", "month", colnames(debit_observed)[c],paste(colnames(debit_observed)[c],
                                                                                                  "_bf", sep = ""),paste(colnames(debit_observed)[c],"_bf_ek", sep = "")),
                       colnames(bf)<-c("date", "year", "month",colnames(debit_observed)[c],paste(colnames(debit_observed)[c],
                                                                                                 "_bf", sep = "")))
                debit_observed<- merge(debit_observed, bf[, c(1,5:ncol(bf)), with = FALSE], by = "date", all.x = TRUE)
                debit_observed_annee_mois<- merge(debit_observed_annee_mois, q_month, by = c("year", "month"), all.x = TRUE)
              }
            rm(alpha_lyne_hollick_calibrated, c, bf, q_month)
            
          #11.1.4-Model error on qtot, qbase, and ETR####
            for (st in 1:length(gauging)){
              #st<-6
              #11.1.4.1-load the modeled data per gauging station ####
                study_area2<-length(unique(rcn_gauging$ID[which(rcn_gauging$gauging_stat==gauging[st])]))*maille_area
                setkey(water_budget, maille_rcn)
                bilan_month<- water_budget[.(rcn_gauging$ID[which(rcn_gauging$gauging_stat == gauging[st])])]
                bilan_month<- na.omit(bilan_month, invert = FALSE)
                bilan_month<- bilan_month[,.(AV = mean(AV),
                                             tmean = mean(tmean),
                                             runoff = mean(runoff),
                                             etp = mean(etp),
                                             etr = mean(etr),
                                             infiltration = mean(infiltration),
                                             runoff_2 = mean(runoff_2),
                                             delta_reservoir = mean(delta_reservoir)),.(year,month)]
                bilan_month$gauging_stat<-gauging[st]
                
              #11.1.4.2-Create the comparison data frame ####
                #comparaison_debit_mois<- merge(debit_observed_annee_mois[,c(1:2,which(grepl(as.character(unique(bilan_month$gauging_stat)),
                #                                                                            colnames(debit_observed_annee_mois), fixed=TRUE))), with = FALSE], 
                #                               bilan_month[,1:(ncol(bilan_month)-1), with = FALSE], 
                #                               by = c("year", "month"), all.x = TRUE)
                cols<-which(colnames(debit_observed_annee_mois) %in% c("year", "month", as.character(unique(bilan_month$gauging_stat)), 
                                                                 paste(as.character(unique(bilan_month$gauging_stat)), "_bf", sep = "")))
                comparaison_debit_mois<- merge(debit_observed_annee_mois[, ..cols], 
                                               bilan_month[,1:(ncol(bilan_month)-1), with = FALSE], 
                                               by = c("year", "month"), all.x = TRUE)
                rm(cols)
                colnames(comparaison_debit_mois)[1:4]<-c("year", "month", "q", "qbase")
                
              #11.1.4.3-Calcul indicateurs erreurs/performances####
                #combining the error indicators in a data table
                  error_ind<-matrix(ncol = 2, nrow = 9)
                  colnames(error_ind)<-c("month_cal", "month_val")
                  rownames(error_ind)<-c("RMSE_qtot", "RMSE_baseflow", "RMSE_etr", "N_RMSE_qtot", "N_RMSE_baseflow", "N_RMSE_etr",
                                         "KGE_qtot", "KGE_baseflow", "KGE_et")
                #defining the periods of data for the observed river flow and base flow per gauging station
                  flow_beg<-min(na.contiguous(comparaison_debit_mois[,1:3])[,1])
                  flow_end<-max(na.contiguous(comparaison_debit_mois[,1:3])[,1])
                  ifelse(flow_beg < debut_simulation, flow_beg<-debut_simulation, flow_beg<-as.numeric(flow_beg))
                  ifelse(flow_end > fin_val, flow_end<-fin_val, flow_end<-as.numeric(flow_end))
                  debut_simulation2<-flow_beg
                  fin_simulation2 <- flow_beg+round((flow_end-flow_beg)*2/3)
                  debut_val2<- fin_simulation2+1
                  fin_val2<- flow_end
                #defining the modeled and observed on each period for flow and baseflow
                  m_debit_month_cal<-(comparaison_debit_mois$runoff[which(comparaison_debit_mois$year %in% c(debut_simulation2:fin_simulation2))]+
                                        comparaison_debit_mois$runoff_2[which(comparaison_debit_mois$year %in% c(debut_simulation2:fin_simulation2))]+
                                        comparaison_debit_mois$infiltration[which(comparaison_debit_mois$year %in% c(debut_simulation2:fin_simulation2))])
                  o_debit_month_cal<-comparaison_debit_mois[[3]][which(comparaison_debit_mois$year %in% c(debut_simulation2:fin_simulation2))]
                  m_debit_month_val<-(comparaison_debit_mois$runoff[which(comparaison_debit_mois$year %in% c(debut_val2:fin_val2))]+
                                        comparaison_debit_mois$runoff_2[which(comparaison_debit_mois$year %in% c(debut_val2:fin_val2))]+
                                        comparaison_debit_mois$infiltration[which(comparaison_debit_mois$year %in% c(debut_val2:fin_val2))])
                  o_debit_month_val<-comparaison_debit_mois[[3]][which(comparaison_debit_mois$year %in% c(debut_val2:fin_val2))]
                  
                  m_baseflow_month_cal<-comparaison_debit_mois$infiltration[which(comparaison_debit_mois$year %in% c(debut_simulation2:fin_simulation2))]
                  o_baseflow_month_cal<-comparaison_debit_mois[[4]][which(comparaison_debit_mois$year %in% c(debut_simulation2:fin_simulation2))]
                  m_baseflow_month_val<-comparaison_debit_mois$infiltration[which(comparaison_debit_mois$year %in% c(debut_val2:fin_val2))]
                  o_baseflow_month_val<-comparaison_debit_mois[[4]][which(comparaison_debit_mois$year %in% c(debut_val2:fin_val2))]
                  
                  p_q<-comparaison_debit_mois[year %in% c(debut_simulation2 : fin_val2), c(1:3,5,9)]
                  colnames(p_q)<-c("year", "month", "q", "av", "etr")
                  p_q$hydroyear<-ifelse(p_q$month %in% c(11, 12), p_q$year +1, p_q$year)
                  p_q<-p_q[,.(q = sum(q, na.rm = FALSE), av = sum(av), etr = sum(etr)),.(hydroyear)]
                  p_q<-na.omit(p_q, cols="q")
                  
                #RMSE - scaled RMSE, e.g., dividing by average of observed values
                  error_ind[1,1]<-rmse(sim = m_debit_month_cal, obs =  o_debit_month_cal)
                  error_ind[1,2]<-rmse(sim = m_debit_month_val, obs = o_debit_month_val)
                  
                  error_ind[2,1]<-rmse(sim = m_baseflow_month_cal, obs = o_baseflow_month_cal)
                  error_ind[2,2]<-rmse(sim = m_baseflow_month_val, obs = o_baseflow_month_val)
                  
                  error_ind[3,1]<-rmse(sim = p_q$etr, obs = (p_q$av-p_q$q))
                  
                  error_ind[4,1]<-error_ind[1,1]/mean(o_debit_month_cal, na.rm = TRUE)
                  error_ind[4,2]<-error_ind[1,2]/mean(o_debit_month_val, na.rm = TRUE)
                  
                  error_ind[5,1]<-error_ind[2,1]/mean(o_baseflow_month_cal, na.rm = TRUE)
                  error_ind[5,2]<-error_ind[2,2]/mean(o_baseflow_month_val, na.rm = TRUE)
                  
                  error_ind[6,1]<- error_ind[3,1]/ mean(p_q$av-p_q$q)
                  
                #KGE
                  error_ind[7,1]<-KGE(m_debit_month_cal[which(!is.na(o_debit_month_cal))],
                                      o_debit_month_cal[which(!is.na(o_debit_month_cal))], na.rm = TRUE)
                  error_ind[7,2]<-KGE(m_debit_month_val[which(!is.na(o_debit_month_val))],
                                      o_debit_month_val[which(!is.na(o_debit_month_val))], na.rm = TRUE)
                  
                  error_ind[8,1]<-KGE(m_baseflow_month_cal[which(!is.na(o_baseflow_month_cal))],
                                      o_baseflow_month_cal[which(!is.na(o_baseflow_month_cal))], na.rm = TRUE)
                  error_ind[8,2]<-KGE(m_baseflow_month_val[which(!is.na(o_baseflow_month_val))],
                                      o_baseflow_month_val[which(!is.na(o_baseflow_month_val))], na.rm = TRUE)
                  
                  error_ind[9,1]<-KGE(p_q$etr, (p_q$av-p_q$q), na.rm = TRUE)
                  
                #Clean 
                  rm(m_debit_month_cal, o_debit_month_cal, m_debit_month_val, o_debit_month_val, m_baseflow_month_cal, o_baseflow_month_cal,
                     m_baseflow_month_val, o_baseflow_month_val, p_q)
                
              #11.1.4.4-write the simulation metadata in the datatable and save it ####
                #simulation_metadata<-fread(paste(calib_directory, "simulation_metadata.csv", sep = ""))
                if(st == 1){
                  simulation_metadata<-data.table(watershed = as.character(nom_station),gauging_stat = unique(bilan_month$gauging_stat),
                                                        cal_beg = debut_simulation2, cal_end = fin_simulation2, val_beg = debut_val2, val_end = fin_val2,
                                                        T_snow= T_snow, T_m= T_m, C_m = C_m, tps_gel_sol = temps_gel_sol, tr_gel = temp_gel, tps_api = temps_api,
                                                        f_runoff = facteur_rcn_2, f_inf = pourc_infiltration, vol_max_vadose = volume_max_vadose,
                                                        RMSE_qtot_cal = error_ind[1,1], RMSE_qtot_val = error_ind[1,2], RMSE_qtot_mean = mean(error_ind[1,]),
                                                        N_RMSE_qtot_cal = error_ind[4,1], N_RMSE_qtot_val = error_ind[4,2], N_RMSE_qtot_mean = mean(error_ind[4,]),
                                                        RMSE_qbase_cal = error_ind[2,1], RMSE_qbase_val = error_ind[2,2], RMSE_qbase_mean = mean(error_ind[2,]),
                                                        N_RMSE_qbase_cal = error_ind[5,1], N_RMSE_qbase_val = error_ind[5,2], N_RMSE_qbase_mean = mean(error_ind[5,]),
                                                        RMSE_etr = error_ind[3,1], N_RMSE_etr = error_ind[6,1],
                                                        KGE_qtot_cal = error_ind[7,1], KGE_qtot_val = error_ind[7,2], KGE_qtot_mean = mean(error_ind[7,]),
                                                        KGE_qbase_cal = error_ind[8,1], KGE_qbase_val = error_ind[8,2], KGE_qbase_mean = mean(error_ind[8,]),
                                                        KGE_etr = error_ind[9,1],
                                                        qtot = mean(bilan_month[,.(qtot = sum(runoff+runoff_2 +infiltration, na.rm = TRUE)),
                                                                                .(year)][[2]]),
                                                        etr = mean(bilan_month[,.(etr = sum(etr, na.rm = TRUE)),
                                                                               .(year)][[2]]),
                                                        gwr = mean(bilan_month[,.(infiltration = sum(infiltration, na.rm = TRUE)),
                                                                               .(year)][[2]]),
                                                        time = format(Sys.time(), "%Y_%m_%d-%H_%M"),
                                                        obj_fun1 = NA_real_, obj_fun2 = NA_real_, obj_fun3 = NA_real_)
                }else {
                  simulation_metadata<-rbind(simulation_metadata, 
                                             data.table(watershed = as.character(nom_station),gauging_stat = unique(bilan_month$gauging_stat),
                                                        cal_beg = debut_simulation2, cal_end = fin_simulation2, val_beg = debut_val2, val_end = fin_val2,
                                                        T_snow= T_snow, T_m= T_m, C_m = C_m, tps_gel_sol = temps_gel_sol, tr_gel = temp_gel, tps_api = temps_api,
                                                        f_runoff = facteur_rcn_2, f_inf = pourc_infiltration, vol_max_vadose = volume_max_vadose,
                                                        RMSE_qtot_cal = error_ind[1,1], RMSE_qtot_val = error_ind[1,2], RMSE_qtot_mean = mean(error_ind[1,]),
                                                        N_RMSE_qtot_cal = error_ind[4,1], N_RMSE_qtot_val = error_ind[4,2], N_RMSE_qtot_mean = mean(error_ind[4,]),
                                                        RMSE_qbase_cal = error_ind[2,1], RMSE_qbase_val = error_ind[2,2], RMSE_qbase_mean = mean(error_ind[2,]),
                                                        N_RMSE_qbase_cal = error_ind[5,1], N_RMSE_qbase_val = error_ind[5,2], N_RMSE_qbase_mean = mean(error_ind[5,]),
                                                        RMSE_etr = error_ind[3,1], N_RMSE_etr = error_ind[6,1],
                                                        KGE_qtot_cal = error_ind[7,1], KGE_qtot_val = error_ind[7,2], KGE_qtot_mean = mean(error_ind[7,]),
                                                        KGE_qbase_cal = error_ind[8,1], KGE_qbase_val = error_ind[8,2], KGE_qbase_mean = mean(error_ind[8,]),
                                                        KGE_etr = error_ind[9,1],
                                                        qtot = mean(bilan_month[,.(qtot = sum(runoff+runoff_2 +infiltration, na.rm = TRUE)),
                                                                                .(year)][[2]]),
                                                        etr = mean(bilan_month[,.(etr = sum(etr, na.rm = TRUE)),
                                                                               .(year)][[2]]),
                                                        gwr = mean(bilan_month[,.(infiltration = sum(infiltration, na.rm = TRUE)),
                                                                               .(year)][[2]]),
                                                        time = format(Sys.time(), "%Y_%m_%d-%H_%M"),
                                                        obj_fun1 = NA_real_, obj_fun2 = NA_real_, obj_fun3 = NA_real_))
                }
                #fwrite(simulation_metadata,paste(calib_directory, "simulation_metadata.csv", sep = ""))
                #rm(simulation_metadata) 
                
              #11.1.4.5-Mean stat of the simulation####
                if (st == 1){
                  #mean_error<-data.table(watershed = as.character(nom_station), gauging_stat = unique(bilan_month$gauging_stat),
                  #                       RMSE_qtot_mean = mean(error_ind[1,]),
                  #                       N_RMSE_qtot_mean = mean(error_ind[4,]),
                  #                       RMSE_qbase_mean = mean(error_ind[2,]),
                  #                       N_RMSE_qbase_mean = mean(error_ind[5,]),
                  #                       RMSE_etr = error_ind[3,1], N_RMSE_etr = error_ind[6,1],
                  #                       KGE_qtot_mean = mean(error_ind[7,]),
                  #                       KGE_qbase_mean = mean(error_ind[8,]),
                  #                       KGE_etr = error_ind[9,1])
                  
                  #to use only the calibration period
                  mean_error<-data.table(watershed = as.character(nom_station), gauging_stat = unique(bilan_month$gauging_stat),
                                         RMSE_qtot_mean = mean(error_ind[1,]),
                                         N_RMSE_qtot_mean = mean(error_ind[4,]),
                                         RMSE_qbase_mean = mean(error_ind[2,]),
                                         N_RMSE_qbase_mean = mean(error_ind[5,]),
                                         RMSE_etr = error_ind[3,1], N_RMSE_etr = error_ind[6,1],
                                         KGE_qtot_mean = error_ind[7,1],
                                         KGE_qbase_mean = error_ind[8,1],
                                         KGE_etr = error_ind[9,1])
                } else {
                  #mean_error<-rbind(mean_error, data.table(watershed = as.character(nom_station), gauging_stat = unique(bilan_month$gauging_stat),
                  #                                         RMSE_qtot_mean = mean(error_ind[1,]),
                  #                                         N_RMSE_qtot_mean = mean(error_ind[4,]),
                  #                                         RMSE_qbase_mean = mean(error_ind[2,]),
                  #                                         N_RMSE_qbase_mean = mean(error_ind[5,]),
                  #                                         RMSE_etr = error_ind[3,1], N_RMSE_etr = error_ind[6,1],
                  #                                         KGE_qtot_mean = mean(error_ind[7,]),
                  #                                         KGE_qbase_mean = mean(error_ind[8,]),
                  #                                         KGE_etr = error_ind[9,1]))
                  
                  #to use only the calibration period
                  mean_error<-rbind(mean_error, data.table(watershed = as.character(nom_station), gauging_stat = unique(bilan_month$gauging_stat),
                                                                                                    RMSE_qtot_mean = mean(error_ind[1,]),
                                                                                                    N_RMSE_qtot_mean = mean(error_ind[4,]),
                                                                                                    RMSE_qbase_mean = mean(error_ind[2,]),
                                                                                                    N_RMSE_qbase_mean = mean(error_ind[5,]),
                                                                                                    RMSE_etr = error_ind[3,1], N_RMSE_etr = error_ind[6,1],
                                                                                                     KGE_qtot_mean = error_ind[7,1],
                                                                                                     KGE_qbase_mean = error_ind[8,1],
                                                                                                    KGE_etr = error_ind[9,1]))
                }
                if(st == length(gauging)){
                  #simulation_metadata<-fread(paste(calib_directory, "simulation_metadata.csv", sep = ""))
                  #simulation_metadata[nrow(simulation_metadata), c("obj_fun1", "obj_fun2", "obj_fun3") := as.list(c(mean(mean_error[[4]]), 
                  #                                                                        mean(mean_error[[6]]), mean(mean_error[[8]])))]    #for N_RMSE
                  simulation_metadata[nrow(simulation_metadata), c("obj_fun1", "obj_fun2", "obj_fun3") := as.list(c(mean(mean_error[[9]]), 
                                                                                                                    mean(mean_error[[10]]),
                                                                                                                    mean(mean_error[[11]], na.rm = TRUE)))]    #for KGE
                  fwrite(simulation_metadata,paste("simulation_metadata_ws", nom_station, "_",
                        ifelse(length(list.files(getwd()))<10, paste("000", length(list.files(getwd())), sep = ""),
                               ifelse(length(list.files(getwd()))<100, paste("00", length(list.files(getwd())), sep = ""),
                                      ifelse(length(list.files(getwd()))<1000, paste("0", length(list.files(getwd())), sep = ""),
                                             length(list.files(getwd()))))),
                                                   ".csv", sep = ""))
                  rm(simulation_metadata)
                }
            }
            
      #12-Clean ####
            rm(st, comparaison_debit_mois, error_ind, bilan_month, flow_beg, flow_end, debut_simulation2, fin_simulation2, debut_val2, fin_val2,
                 debit_observed, debit_observed_annee_mois, rcn_gauging, mround, water_budget, intrants_rcn, intrants_meteo)
            gc()
            
      #13-objective function of the model ####
            #return(c(mean(mean_error[[4]]), mean(mean_error[[6]]), mean(mean_error[[8]])))   #for N_RMSE
            #return(c(mean(mean_error[[9]]), mean(mean_error[[10]]), mean(mean_error[[11]])))  #for KGE
            return(c(mean(mean_error[[9]]), mean(mean_error[[10]])))  #for KGE without P-Q
    }

#3-calibration ####
  # Number of objectives
    #nobj <- 3 #for rmse qtot, qbase, p-q
    nobj <- 2 #for rmse qtot, qbase
  #number of variables
    #nvar <- 8 #8 calibration parameters
    nvar <- 7 #if time to freez the soil is taken out of the optimization
  #all the objectives are to be minimized
    #minmax <- c(FALSE, FALSE, FALSE) # to minimize the rmse
    #minmax <- c(TRUE, TRUE, TRUE) # to maximize the KGE
    minmax <- c(TRUE, TRUE) # to maximize the KGE without the P-Q
  #range of the parameters
    bounds <- matrix(nrow = nvar, ncol = 2)
    #bounds[, 1] <- c(-2, 3, 5, -20, 1, 0.5, 0.01, 50)
    #bounds[, 1] <- c(-1, 3, -20, 3, 0.5, 0.01, 200)    #for watershed 19
    #bounds[, 1] <- c(-1, 3, -17, 2.5, 0.5, 0.01, 95)    #for watershed 25
    #bounds[, 1] <- c(-1, 3, -20, 3, 0.53, 0.05, 200)    #for watershed 24
    #bounds[, 1] <- c(0, 3.5, -7, 3.5, 0.5, 0.03, 400)    #for watershed 0
    #bounds[, 1] <- c(0.5, 3.5, -20, 2.4, 0.52, 0.08, 250)    #for watershed 13
    bounds[, 1] <- c(1, 4, -20, 3.05, 0.5, 0.01, 160)    #for watershed 47
    #bounds[, 1] <- c(-1, 3, -16, 3.2, 0.52, 0.05, 400)    #for watershed 01
    #bounds[, 1] <- c(1.3, 4.5, -20, 2, 0.52, 0.02, 350)    #for watershed 16
    
    #bounds[, 2] <- c(2, 6.5, 30, 0, 5, 1, 0.25, 900)
    #bounds[, 2] <- c(1, 4.5, -10, 4, 0.6, 0.09, 450)     #for watershed 19
    #bounds[, 2] <- c(1.5, 4.5, -4, 4.5, 0.7, 0.05, 650)     #for watershed 25
    #bounds[, 2] <- c(2, 5.5, -12, 4.5, 0.66, 0.2, 900)     #for watershed 24
    #bounds[, 2] <- c(1.5, 5, -2, 5, 0.55, 0.07, 700)     #for watershed 0
    #bounds[, 2] <- c(2, 5, -9, 5, 0.65, 0.25, 900)     #for watershed 13
    bounds[, 2] <- c(2.5, 6.5, -12, 4.8, 0.6, 0.05, 720)     #for watershed 47
    #bounds[, 2] <- c(1.5, 5, -3, 5, 0.7, 0.2, 850)     #for watershed 01
    #bounds[, 2] <- c(2.1, 6.3, -12, 4.5, 0.63, 0.09, 900)     #for watershed 16
    
    #T_snow<-0
    #T_m<-x1
    #C_m<-x2
    #temps_gel_sol <- x3
    #temp_gel <- x4
    #temps_api <- x5
    #facteur_rcn_2 <- x6
    #pourc_infiltration <- x7
    #volume_max_vadose<- x8
    
  #calibration
    results <-caRamel(nobj = nobj, nvar = nvar, minmax =  minmax,
                      bounds = bounds,func = HB, 
                      popsize = 50,
                      archsize = 100,
                      maxrun = 500,
                      prec = matrix(0.01, nrow = 1, ncol = nobj),
                      #write_gen = 1,
                      #listsave = list("pmt",  "obj",  "evol", "totalpop"),
                      carallel = FALSE)
                      #carallel = TRUE,
                      #numcores = 2)
    
    
#4-save results ####
    #nom_station <- 25
    list_meta<-list.files(getwd())
    for(i in 1:length(list_meta)){
      #i<-2
      if (i == 1){
        sim_meta<-fread(list_meta[i])
        sim_meta$it<-i
      }else{
        zzz<-fread(list_meta[i])
        zzz$it<-i
        sim_meta<-rbind(sim_meta, zzz)
      }
    }
    rm(i, zzz, list_meta)
    fwrite(sim_meta, paste("ws", nom_station, "_01-simulation_metadata_calib_500it_rcn_correct.csv", sep = ""))
    
    param<-data.table(results$parameters)
    fwrite(param, paste("ws", nom_station,  "_02_param_optim_rcn_correct.csv", sep = ""))
    object<-data.table(results$objectives)
    fwrite(object, paste("ws", nom_station,  "_03_object_optim_rcn_correct.csv", sep = ""))
    save_crit<-data.table(results$save_crit)
    fwrite(save_crit, paste("ws", nom_station,  "_04_save_crit_rcn_correct.csv", sep = ""))
    tot_pop<-data.table(results$total_pop)
    fwrite(tot_pop, paste("ws", nom_station,  "_05_tot_pop_rcn_correct.csv", sep = ""))
    #save the plot
      png(filename = paste("ws", nom_station, "_06_plot_calib_rcn_correct.png", sep = ""),
          height = 20, width = 20, units = 'cm',res = 1200, bg = "transparent")
      plot.new()
      plot_caramel(results)
      dev.off()
  
    
  
  
  
  
  