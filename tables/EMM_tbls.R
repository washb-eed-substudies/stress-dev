#EMM Tables

#subgroup_tbl <- function(name, expo_var, out_var, sub_var, exposure, outcome, subgroup, results)

#V.set.t2 <- c("fci_t2")
#V.set.t3 <- c("fci_t2", "fci_t3")

#### Table S1 ####

sub_var <- c("Family Care Inventory at Year 1")
subgroup <- c("fci_t2")
exposure <- c("t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")
outcome <- c("sum_who_t2_t3", "z_cdi_say_t2","z_cdi_und_t2")
expo_var <- c("IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative stress biomarker score")
out_var <- c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI expressive language Z-score","CDI comprehension Z-score")

tbl_s1 <- subgroup_tbl("Effect Measure Modification of Urinary isoprostanes and child development by Family Care Inventory at Year 1", expo_var, out_var, sub_var, exposure, outcome, subgroup, H1a_adj_emm)

print(tbl_s1, preview = "html")

#tbls1flex <- subgroup_tbl_flex("Effect Measure Modification of Urinary isoprostanes and child development by Family Care Inventory at Year 1", expo_var, out_var, sub_var, exposure, outcome, subgroup, H1a_adj_emm)

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
print(tbl_s2, preview = "docx") 
#tbl3flex <- growth_tbl_flex("Urinary isoprostanes at Year 1 and child development at Year 2", expo_var, out_var, exposure, outcome, H1b, H1b_adj)


#### Table 4 ####
#Hypothesis 2#
exposure <- c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02" )
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 


expo_var <- c("Cortisol Reactivity (ug/dl/min)", "Pre-stressor Cortisol (ug/dl)", "Post-stressor Cortisol (ug/dl)", "sAA Reactivity (U/ml/min)", "Pre-stressor sAA (U/ml)", "Post-stressor sAA (U/ml)")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")


tbl4 <- growth_tbl("Salivary Stress Biomarkers and Child Development at Year 2", expo_var, out_var, exposure, outcome, H2, H2_adj)
tbl4flex <- growth_tbl_flex("Salivary Stress Biomarkers and Development at Year 2", expo_var, out_var, exposure, outcome, H2, H2_adj)


#### Table 5 ####
#Hypothesis 3#
exposure <- c("t3_map", "t3_hr_mean")  
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean arterial pressure (mmHg)", "Mean Resting Heart Rate (bpm)")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")

tbl5 <- growth_tbl("Mean arterial pressure and heart rate at year 2 v. development at year 2", expo_var, out_var, exposure, outcome, H3, H3_adj)
tbl5flex <- growth_tbl_flex("Mean arterial pressure and heart rate at year 2 v. development at year 2", expo_var, out_var, exposure, outcome, H3, H3_adj)

#### Table 6 ####
#Hypothesis 4#

exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")   
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean Overall Percentage Glucocorticoid Receptor Methylation", "Percentage methylation at NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")

tbl6 <- growth_tbl("Glucocortoic receptor methylation and child development at Year 2", expo_var, out_var, exposure, outcome, H4, H4_adj)
tbl6flex <- growth_tbl_flex("Glucocortoic receptor methylation and child development at Year 2", expo_var, out_var, exposure, outcome, H4, H4_adj)
