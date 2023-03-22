#Public data creation

#read in data
d <- readRDS("data/stress_dev.RDS")

#create dataid column
d <- d %>%
     mutate( dataid = floor(childid/10))

#read in public IDs
id <- read.csv("public-data/public-ids.csv")
#merge
dmerge<- left_join(d, id, all.x=TRUE, by="dataid")
#dmerge<- inner_join(id, d, by="dataid_r")

#extract relevant columns



dpub <- dmerge %>%
  select(block_r, clusterid_r, dataid_r, "sex", "birthord", "momage","momheight","momedu", 
                                                  "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
                         
                          "cesd_sum_t2", "diar7d_t2", "tr", "life_viol_any_t3",
         
  
"ageday_at3", "month_at3", "diar7d_t3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "life_viol_any_t3",  
         
"ageday_ut2", "month_ut2",

"laz_t2", "waz_t2", "ageday_t3_vital", "month_vt3", "ageday_t3_salimetrics", "month_lt3",
 "ageday_t3_oragene", "month_ot3", 

"t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca",    


"t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02",

"t3_map", "t3_hr_mean",

"t3_gcr_mean", "t3_gcr_cpg12",

"sum_who_t2_t3", 
           "z_cdi_say_t2", "z_cdi_und_t2", 

"z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
"z_cdi_say_t3", "z_cdi_und_t3")

#remove _r suffixes
dpub <- dpub %>%
  rename(block = block_r,
         clusterid = clusterid_r, 
         dataid = dataid_r)
         
  

write.csv(dpub, "public-data/wbb_stress-dev_public.csv")
