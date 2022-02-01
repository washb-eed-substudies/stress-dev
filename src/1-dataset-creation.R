rm(list=ls())

source(here::here("0-config.R"))

#Read in master dataset
d <- box_read("871638120165") %>% filter(.data[["stress_dev"]]==1)
head(d)


#Rename variables as needed for analysis workflow 

d <- d %>%
  mutate(
    t2_f2_iso.pca= t2_iso_pca
  )


d <- d %>%
  mutate(
    sum_who_t2_t3 = sum_who,
    z_comm_easq_t3 = z_comm_easq,
    z_motor_easq_t3 = z_motor_easq,
    z_personal_easq_t3 = z_personal_easq,
    z_combined_easq_t3 = z_combined_easq
  )

#Save new dataset


box_write(d, "stress_dev.RDS", dir_id = 148798406168)
