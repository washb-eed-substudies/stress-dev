#EMM Tables

#subgroup_tbl <- function(name, expo_var, out_var, sub_var, exposure, outcome, subgroup, results)

#V.set.t2 <- c("fci_t2")
#V.set.t3 <- c("fci_t2", "fci_t3")

rm(list=ls())

library('flextable')
library('officer')
source(here::here("0-config.R")) 

source(here::here("tables/table-functions.R"))
here::here()
d <- box_read("880476682582")



#Adjusted
H1a_adj_emm <- readRDS(here('results/bh-corrected/H1a_adj_emm_res_BH.RDS'))  %>% mutate(int.Pval = int.p) 
H1b_adj_emm <- readRDS(here('results/bh-corrected/H1b_adj_emm_res_BH.RDS')) %>% mutate(int.Pval = int.p) 
H2_adj_emm <- readRDS(here('results/bh-corrected/H2_adj_emm_res_BH.RDS')) %>% mutate(int.Pval = int.p) 
H3_adj_emm <- readRDS(here('results/bh-corrected/H3_adj_emm_res_BH.RDS')) %>% mutate(int.Pval = int.p) 
H4_adj_emm <- readRDS(here('results/bh-corrected/H4_adj_emm_res_BH.RDS')) %>% mutate(int.Pval = int.p) 

#WHO Hazard Ratios
H1a_who_adj_emm <- readRDS(here('results/bh-corrected/H1a_who_adj_emm_res_BH.RDS'))

#### Functions for growth tables ####
source(here::here("tables/table-functions.R"))


#### Table S1 ####

sub_var <- c("Family Care Inventory at Year 1")
subgroup <- c("fci_t2")
exposure <- c("t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")
outcome <- c("sum_who_t2_t3", "z_cdi_say_t2","z_cdi_und_t2")
expo_var <- c("IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative stress biomarker score")
out_var <- c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI expressive language Z-score","CDI comprehension Z-score")

tbl_s1 <- subgroup_tbl("Effect Measure Modification of Urinary isoprostanes and child development by Family Care Inventory at Year 1", expo_var, out_var, sub_var, exposure, outcome, subgroup, H1a_adj_emm)




######
#Supp table 2: Urinary isoprostanes at Year 1 and child development at Year 2



sub_var <- c("Family Care Inventory at Year 1", "Family Care Inventory at Year 2")
subgroup <- c("fci_t2", "fci_t3")
exposure <- c("t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3","z_personal_easq_t3","z_combined_easq_t3", 
             "z_cdi_say_t3","z_cdi_und_t3")
expo_var <- c("IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative stress biomarker score")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ Score",
             "CDI expressive language Z-score","CDI comprehension Z-score")

tbl_s2 <- subgroup_tbl("Urinary isoprostanes at Year 1 and child development at Year 2", expo_var, out_var, sub_var, exposure, outcome, subgroup, H1b_adj_emm)




#### Table 3 ####
#Hypothesis 2#

sub_var <- c("Family Care Inventory at Year 1", "Family Care Inventory at Year 2")
subgroup <- c("fci_t2", "fci_t3")

exposure <- c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02" )
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 


expo_var <- c("Cortisol Reactivity (ug/dl/min)", "Pre-stressor Cortisol (ug/dl)", "Post-stressor Cortisol (ug/dl)", "sAA Reactivity (U/ml/min)", "Pre-stressor sAA (U/ml)", "Post-stressor sAA (U/ml)")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")


tbl_s3 <- subgroup_tbl("Salivary Stress Biomarkers and Child Development at Year 2", expo_var, out_var, sub_var, exposure, outcome, subgroup, H2_adj_emm)




#### Table 4 ####
#Hypothesis 3#

sub_var <- c("Family Care Inventory at Year 1", "Family Care Inventory at Year 2")
subgroup <- c("fci_t2", "fci_t3")

exposure <- c("t3_map", "t3_hr_mean")  
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean arterial pressure (mmHg)", "Mean Resting Heart Rate (bpm)")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")

tbl_s4 <- subgroup_tbl("Mean arterial pressure and heart rate at year 2 v. development at year 2", expo_var, out_var, sub_var, exposure, outcome, subgroup, H3_adj_emm)


#### Table 5 ####
#Hypothesis 4#

sub_var <- c("Family Care Inventory at Year 1", "Family Care Inventory at Year 2")
subgroup <- c("fci_t2", "fci_t3")

exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")   
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean Overall Percentage Glucocorticoid Receptor Methylation", "Percentage methylation at NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")

tbl_s5 <- subgroup_tbl("Glucocortoic receptor methylation and child development at Year 2", expo_var, out_var, sub_var, exposure, outcome, subgroup, H4_adj_emm)


#SAVE TABLES
#write.csv(tbl_s1, here('tables/supp-tables/stress-dev-table-S1.csv'))
#write.csv(tbl_s2, here('tables/supp-tables/stress-dev-table-S2.csv'))
#write.csv(tbl_s3, here('tables/supp-tables/stress-dev-table-S3.csv'))
#write.csv(tbl_s4, here('tables/supp-tables/stress-dev-table-S4.csv'))
#write.csv(tbl_s5, here('tables/supp-tables/stress-dev-table-S5.csv'))


save_as_docx("Table S1" = tbl_s1, "Table S2" = tbl_s2, "Table S3" = tbl_s3, "Table S4" = tbl_s4, "Table S5" = tbl_s5,  
             pr_section = sect_properties, path='tables/supp-tables/stress-dev-EMM-tables.docx')
