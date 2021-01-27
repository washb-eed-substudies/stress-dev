


rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))

#Load stress-growth data
d<-readRDS("data/stress_growth_data.RDS")

#Load development data
dev <- readRDS("data/bangladesh-development.RDS")

## Merge datasets
dfull <- left_join(d, dev, by="childid")

#Save new dataset
saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Audrie/stress-dev.RDS"))
