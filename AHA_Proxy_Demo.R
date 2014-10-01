source('C:/Datasets/3. Asset Health Analytics pipeline/AHA_RDStoGPS.R')

load("C:/Datasets/3. Asset Health Analytics pipeline/1. Ruwe datasets/6. NOR/masterset_backupELCVERBINDINGSKNOOPPUNTEN_1012.Rda")
masterset[,c("lon","lat")]=AHA_RDCtoGPS(masterset[,c(3,4)]);