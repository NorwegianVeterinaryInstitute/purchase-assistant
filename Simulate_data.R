################################################################################
##                                                                            ##
## Script for simulating data fro Norwegian Purchase assistant                ##
##                                                                            ##
## Author: Lars Erik Gangsei                                                  ##

# Run "data.R" to get input
source(system.file("R/data.R", package = "purchaseAssistant"))
library(tidyverse)

# Fake herds
n_herds <- 2000
fake_herds <- data.frame(herdID = 2000+(1:n_herds),
                         animalBreed = sample(all_animal_breeds(),size = n_herds,
                         replace = TRUE,prob = c(1,1,1,1,0.5,5,2,1,1,1,1,0.5,0.5)),
                         county = sample(counties$county,size = n_herds,
                                         replace = TRUE,
                                         prob = c(0.1,5,1,1,2,2,2,3,2,2,2,2,4,2,1,0,0)))

# Diagnosis status
Diagnosis_df <- dplyr::left_join(data.frame(id = rep(fake_herds$herdID,each = 4),
                                    date_bcov_brsv = as.Date(as.numeric(as.Date(
                                      c('2024-04-01','2024-07-01','2024-10-01','2025-02-01')
                                      )+rep(sample(0:89,size = n_herds,replace = TRUE),each = 4))),
                                    date_jurstatus = as.Date(as.numeric(as.Date(
                                      c('2024-04-01','2024-07-01','2024-10-01','2025-02-01')
                                    )+rep(sample(0:89,size = n_herds,replace = TRUE),each = 4))),
                                    date_klauvstatus = as.Date(as.numeric(as.Date(
                                      c('2024-04-01','2024-07-01','2024-10-01','2025-02-01')
                                    )+rep(sample(0:89,size = n_herds,replace = TRUE),each = 4))),
                                    BCoVRes =  sample(c(0,1),size = 4*n_herds,
                                                      replace = TRUE,prob = c(0.95,0.05)),
                                    BRSVRes = sample(c(0,1),size = 4*n_herds,
                                                    replace = TRUE,prob = c(0.9,0.1)),
                                    KlauvstatusRes = sample(c(0,1),size = 4*n_herds,
                                                      replace = TRUE,prob = c(0.975,0.025)),
                                    JurstatusRes = sample(c(0,1),size = 4*n_herds,
                                                          replace = TRUE,prob = c(0.975,0.025))),
                                 dplyr::rename(fake_herds,id = herdID),by = 'id')
                                 
idx_bov_brsv <-sample(n_herds, size = 3*n_herds/4)
idx_klauv <- sample(n_herds, size = n_herds/4)
idx_jur  <- sample(n_herds,size = n_herds)

write.csv(dplyr::select(Diagnosis_df[idx_bov_brsv,],id,county,date_bcov_brsv,BCoVRes)%>%
            dplyr::rename(date = date_bcov_brsv)%>%
            dplyr::mutate(date = as.character(date))%>%
            dplyr::rename(result = BCoVRes),
          file = file.path(system.file("datasets", package = "purchaseAssistant"),
                           "dt_BCoV2App.csv"),sep = ',',row.names = FALSE)

write.csv(dplyr::select(Diagnosis_df[idx_bov_brsv,],id,county,date_bcov_brsv,BRSVRes)%>%
            dplyr::rename(date = date_bcov_brsv)%>%
            dplyr::mutate(date = as.character(date))%>%
            dplyr::rename(result = BRSVRes),
          file = file.path(system.file("datasets", package = "purchaseAssistant"),
                           "dt_BRSV2App.csv"),
          sep = ',',row.names = FALSE)

write.csv(dplyr::select(Diagnosis_df[idx_klauv,],id,county,date_klauvstatus,KlauvstatusRes)%>%
            dplyr::rename(date = date_klauvstatus)%>%
            dplyr::mutate(date = as.character(date))%>%
            dplyr::rename(result = KlauvstatusRes),
          file = file.path(system.file("datasets", package = "purchaseAssistant"),
                           "dt_Klauvstatus2App.csv"),
          sep = ',',row.names = FALSE)

write.csv(dplyr::select(Diagnosis_df[idx_jur,],id,county,date_jurstatus,JurstatusRes)%>%
            dplyr::rename(date = date_jurstatus)%>%
            dplyr::mutate(date = as.character(date))%>%
            dplyr::rename(result = JurstatusRes),
          file = file.path(system.file("datasets", package = "purchaseAssistant"),
                           "dt_Jurstatus2App.csv"),
          sep = ',',row.names = FALSE)

# Fake sellers
n_sellers <- 2000

herd_idx <- sample(1:n_herds,size = n_sellers,replace = TRUE) 

fake_sellers <- dplyr::left_join(data.frame(herdID = fake_herds$herdID[herd_idx],
                           county = fake_herds$county[herd_idx],
                           animalType = sample(all_animal_types(),size = n_sellers,
                                               replace = TRUE,
                                               prob = c(1,1,2,2,2,2,2,2,1,1,3,3,1,1,3,1)),
                           animalBreed = fake_herds$animalBreed[herd_idx],  
                           animalNumber = sample(1:10,size = n_sellers,replace = TRUE,
                                                 prob = 1/(1:10))),
                              dplyr::group_by(Diagnosis_df,id)%>%
                             dplyr::summarise(BCoVRes = as.numeric(sum(BCoVRes)>0),
                                              BRSVRes = as.numeric(sum(BRSVRes)>0),
                                              KlauvstatusRes = as.numeric(sum(KlauvstatusRes)>0),
                                              JurstatusRes = as.numeric(sum(JurstatusRes)>0))%>%
                             dplyr::rename(herdID = id),
                                  by = 'herdID')%>%
                            dplyr::relocate(BCoVRes,BRSVRes,KlauvstatusRes,JurstatusRes,
                                            .before = animalType)


write.csv(fake_sellers,file = file.path(system.file("datasets", 
                                  package = "purchaseAssistant"),
                                  "dt_fake_sellers2App.csv"),
                                  sep = ',',row.names = FALSE)