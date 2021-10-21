


rm(list=ls())
vignette("boxr")
source(here::here("0-config.R"))

zbdbox <- box_read(147779347962)
#Load stress-growth data
d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Andrew/stress_growth_data.RDS"))

d <- d %>%
  mutate(
    t2_f2_iso.pca= t2_iso_pca
  )

#Load development data
dev <-readRDS(paste0(dropboxDir, "Data/Cleaned/Andrew/bangladesh-development.RDS"))

dev <- dev %>%
  mutate(
    sum_who_t2_t3 = sum_who,
    z_comm_easq_t3 = z_comm_easq,
    z_motor_easq_t3 = z_motor_easq,
    z_personal_easq_t3 = z_personal_easq,
    z_combined_easq_t3 = z_combined_easq
  )
## Merge datasets
dim(d)
dim(dev)
dfull <- left_join(d, dev, by="childid")
dim(dfull)

#Save new dataset
saveRDS(dfull, paste0(dropboxDir,"Data/Cleaned/Audrie/stress-dev.RDS"))
