library(data.table)
remotes::install_github("mccreigh/rwrfhydro")
library(rwrfhydro) #contain a KGE function

#KGE = KGE function of the rechaRge package

test<-data.table(ite = c(1:10000), kge_test = NA_real_, kge_bm = NA_real_)

for (ii in c(1:10000)){
  #ii<-1
  sim = runif(n = 1000, min = 0, max = 10^6)
  obs = runif(n = 1000, min = 0, max = 10^6)
  
  test$kge_test[ii]<-KGE(sim, obs)
  test$kge_bm[ii]<-Kge(m = sim, o = obs)
   
}

test[,verif := ifelse(kge_test == kge_bm, 1, 0)]
length(which(test$verif == 0)) # if 0 line with a value 0 => results are identical