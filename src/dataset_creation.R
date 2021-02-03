


rm(list=ls())

source(here::here("0-config.R"))
source(here::here("src/0-gam-functions.R"))

#Load stress-growth data
d<-readRDS("data/stress_growth_data.RDS")

d <- d %>%
  mutate(
    t2_iso.pca=iso.pca
    
  )

#Load development data
dev <- readRDS("data/bangladesh-development.RDS")

dev <- dev %>%
  mutate(
    sum_who_t2_t3 = sum_who,
    z_comm_easq_t3 = z_comm_easq,
    z_motor_easq_t3 = z_motor_easq,
    z_personal_easq_t3 = z_personal_easq,
    z_combined_easq_t3 = z_combined_easq
  )
## Merge datasets
dfull <- left_join(d, dev, by="childid")

#Save new dataset
saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Audrie/stress-dev.RDS"))
