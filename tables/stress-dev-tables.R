rm(list=ls())

library('flextable')
library('here')
library('officer')
library('data.table')
source(here::here("0-config.R")) 
  #ERROR: "there is no package called washb"
  # go to file and comment out the line for library washb
source(here::here("tables/table-functions.R"))
here::here()
d <- readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/stress-dev.RDS"))
#test
#library('officer')
#source(here::here("0-config.R"))
#library('here')
#library('data.table')
# install.packages("gdtools", type = "source")
# library('flextable')
# install.packages("flextable", type = "binary")
# /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# load enrollment characteristics and results
# d <- read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/bangladesh-dm-ee-stress-growth-covariates-stresslab-anthro.csv"))

#Unadjusted
H1a <- readRDS(here('results/unadjusted/H1a_res.RDS'))
H1b <- readRDS(here('results/unadjusted/H1b_res.RDS'))
H2 <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3 <- readRDS(here('results/unadjusted/H3_res.RDS'))
H4 <- readRDS(here('results/unadjusted/H4_res.RDS'))

#Adjusted
H1a_adj <- readRDS(here('results/adjusted/H1a_adj_res.RDS'))
H1a_adj_emm <- readRDS(here('results/adjusted/H1a_adj_emm_res.RDS'))
H1b_adj <- readRDS(here('results/adjusted/H1b_adj_res.RDS'))
H1b_adj_emm <- readRDS(here('results/adjusted/H1b_adj_emm_res.RDS'))
H2_adj <- readRDS(here('results/adjusted/H2_adj_res.RDS'))
H2_adj_emm <- readRDS(here('results/adjusted/H2_adj_emm_res.RDS'))
H3_adj <- readRDS(here('results/adjusted/H3_adj_res.RDS'))
H3_adj_emm <- readRDS(here('results/adjusted/H3_adj_emm_res.RDS'))
H4_adj <- readRDS(here('results/adjusted/H4_adj_res.RDS'))
H4_adj_emm <- readRDS(here('results/adjusted/H4_adj_emm_res.RDS'))

#WHO Hazard Ratios

H1a_who <- readRDS(here('results/unadjusted/H1_who_res.RDS'))
H1a_who_adj <- readRDS(here('results/adjusted/H1a_who_adj_res.RDS'))

#### Functions for growth tables ####
source(here::here("tables/table-functions.R"))


#COMMENTED FOLLOWING LINES AS THEY LED TO AN ERROR - ADDITIONAL TROUBLESHOOTING MAY BE NECEESSARY

# # format for export
# flextbl<-flextable(tbl, col_keys=names(tbl))
# flextbl <- set_header_labels(flextbl,
#                              values = list("V1" = " ", "V2" = " ", "V3" = " ", "V4" = " ", "V5" = " ",
#                                            "V6" = "Predicted Outcome at 25th Percentile", "V7" = "Predicted Outcome at 75th Percentile", "V8" = "Coefficient (95% CI)", "V9" = "P-value", "V10" = "FDR Corrected P-value",
#                                            "V11" = "Predicted Outcome at 25th Percentile", "V12" = "Predicted Outcome at 75th Percentile", "V13" = "Coefficient (95% CI)", "V14" = "P-value", "V15" = "FDR Corrected P-value"))
# flextbl <- add_header_row(flextbl, values = c("","","","","", "Unadjusted", "Fully adjusted"), colwidths=c(1,1,1,1,1,5,5))
# # flextbl <- hline_top(flextbl, part="header", border=fp_border(color="black"))
# flextbl <- add_header_row(flextbl, values = c(name, "Outcome","N","25th Percentile","75th Percentile", "Outcome, 75th Percentile v. 25th Percentile"), colwidths=c(1,1,1,1,1,10))
# # flextbl <- hline_top(flextbl, part="header", border=fp_border(color="black"))
# flextbl <- hline(flextbl, part="header", border=fp_border(color="black"))
# flextbl <- hline_bottom(flextbl, part="body", border=fp_border(color="black"))
#>>>>>>> ec8753a515e35971430abe99d2494499e5fd1a6b
# flextbl <- hline_top(flextbl, part="header", border=fp_border(color="black"))
# flextbl <- align(flextbl, align = "center", part = "all")
# flextbl <- align(flextbl, j = c(1, 2), align = "left", part="all")
# flextbl <- autofit(flextbl, part = "all")
# flextbl <- fit_to_width(flextbl, max_width=8)
# flextbl



#### MAIN TABLES ####
#### Table 1 ####
# Characteristics of participants
nperc <- function(vector){
  n <- sum(vector==1, na.rm=T)
  perc <- round(n/sum(!is.na(vector))*100)
  paste(n, " (", perc, "%)", sep="")
}

mediqr <- function(vector){
  quantiles <- round(quantile(vector, na.rm=T), 2)
  paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
}

n_med_col <- c(nperc(d$sex), mediqr(d$t2_f2_8ip), mediqr(d$t2_f2_23d), mediqr(d$t2_f2_VI), mediqr(d$t2_f2_12i),
               mediqr(d$t3_cort_slope), mediqr(d$t3_residual_cort), mediqr(d$t3_saa_slope), mediqr(d$t3_residual_saa),
               mediqr(d$t3_map), mediqr(d$t3_hr_mean), mediqr(d$t3_gcr_mean), mediqr(d$t3_gcr_cpg12),
               mediqr(d$laz_t2), mediqr(d$waz_t2), mediqr(d$whz_t2), mediqr(d$hcz_t2),
               mediqr(d$laz_t3), mediqr(d$waz_t3), mediqr(d$whz_t3), mediqr(d$hcz_t3),
               nperc(d$diar7d_t2), nperc(d$diar7d_t3), mediqr(d$momage), mediqr(d$momheight), 
               mediqr(d$momeduy), mediqr(d$cesd_sum_t2), mediqr(d$cesd_sum_ee_t3), mediqr(d$pss_sum_mom_t3), 
               nperc(d$life_viol_any_t3))

tbl1 <- data.table("C1" = c("Child","","","","","","","","","","","","","","","","","","","","","","","Mother","","","","","",""),
                   "C2" = c("", "Urinary F2-isoprostanes (Year 1)","","","", "Salivary cortisol reactivity (Year 2)","", "sAA reactivity (Year 2)","",
                            "SAM biomarkers (Year 2)","", "Glucocorticoid receptor","", "Anthropometry (14 months, Year 1)","","","",
                            "Anthropometry (28 months, Year 2)","","","", "Diarrhea (14 months, Year 1)", "Diarrhea (28 months, Year 2)","",
                            "Anthropometry at enrollment", "Education", "Depression at Year 1", "Depression at Year 2", "Perceived stress at Year 2", 
                            "Intimate partner violence"),
                   "C3" = c("Female", "iPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a-VI", "8,12-iso-iPF(2a)-VI", 
                            "Change between pre- and post-stressor cortisol", "Cortisol residualized gain score", 
                            "Change between pre- and post-stressor sAA", "sAA residualized gain score",
                            "Mean arterial pressure", "Resting heart rate", "NR3C1 exon 1F promoter methylation", "NGFI-A transcription factor binding site methylation",
                            "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                            "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                            "Caregiver-reported 7-day recall", "Caregiver-reported 7-day recall", "Age (years)", "Height (cm)", "Schooling completed (years)",
                            "CES-D score", "CES-D score", "Perceived Stress Scale score", "Any lifetime exposure"),
                   "C4" = n_med_col)

tbl1flex <- flextable(tbl1, col_keys=names(tbl1))
tbl1flex <- set_header_labels(tbl1flex,
                              values = list("C1" = "", "C2" = "", "C3" = "", "C4" = "n (%) or median (IQR)"))
tbl1flex <- hline_top(tbl1flex, part="header", border=fp_border(color="black", width = 1))
tbl1flex <- hline_bottom(tbl1flex, part="all", border=fp_border(color="black", width = 1))
tbl1flex <- autofit(tbl1flex, part = "all")
tbl1flex <- align(tbl1flex, j = c(1, 2, 3), align = "left", part="all")
tbl1flex <- align(tbl1flex, j = 4, align = "center", part="all")
tbl1flex <- fit_to_width(tbl1flex, max_width=8)
names(tbl1)<- c("","","","n (%) or median (IQR)")


#FOR GATES MILESTONES
#### Table 2 ####

#previously, there were two t2_f2_8ip, i edited it so tghat there is now "t2_f2_8ip", "t2_f2_23d" based on my ipv, stress, dep - child stress tables 
exposure <- c("t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")
outcome <- c("sum_who", "z_cdi_say_t2","z_cdi_und_t2","z_comm_easq", "z_motor_easq","z_personal_easq","z_combined_easq", 
             "z_cdi_say_t3","z_cdi_und_t3")
expo_var <- c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined urinary oxidative stress biomarkers")
out_var <- c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI expressive language Z-score Year 1","CDI comprehension Z-score Year 1",
             "EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score Year 2","CDI comprehension Z-score Year 2")

tbl2 <- growth_tbl("Urinary isoprostanes at Year 1 and child development at Years 1 and 2", expo_var, out_var, exposure, outcome, H1a, H1a_adj)
tbl2flex <- growth_tbl_flex("Urinary isoprostanes at Year 1 and child development at Years 1 and 2", expo_var, out_var, exposure, outcome, H1a, H1a_adj)


#### Table 3 ####
#Hypothesis 2#
exposure <- c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02" )
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 


expo_var <- c("Cortisol Reactivity", "Pre-stressor Cortisol", "Post-stressor Cortisol", "sAA Reactivity", "Pre-stressor sAA", "Post-stressor sAA")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")


tbl3 <- growth_tbl("Salivary Stress Biomarkers and Child Development at Year 2", expo_var, out_var, exposure, outcome, H2, H2_adj)
tbl3flex <- growth_tbl_flex("Salivary Stress Biomarkers and Development at Year 2", expo_var, out_var, exposure, outcome, H2, H2_adj)


#### Table 4 ####
#Hypothesis 3#
exposure <- c("t3_map", "t3_hr_mean")  
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean arterial pressure", "Mean Resting Heart Rate")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")

tbl4 <- growth_tbl("Mean arterial pressure and heart rate at year 2 v. development at year 2", expo_var, out_var, exposure, outcome, H3, H3_adj)
tbl4flex <- growth_tbl_flex("Mean arterial pressure and heart rate at year 2 v. development at year 2", expo_var, out_var, exposure, outcome, H3, H3_adj)

#### Table 5 ####
#Hypothesis 4#

exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")   
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean Overall Percentage Glucocorticoid Receptor Methylation", "Percentage methylation at NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ",
             "CDI expressive language Z-score","CDI comprehension Z-score")

tbl5 <- growth_tbl("Glucocortoic receptor methylation and child development at Year 2", expo_var, out_var, exposure, outcome, H4, H4_adj)
tbl5flex <- growth_tbl_flex("Glucocortoic receptor methylation and child development at Year 2", expo_var, out_var, exposure, outcome, H4, H4_adj)


#### SAVE TABLES ####
write.csv(tbl1, here('tables/stress-dev-table1.csv'))
write.csv(tbl2, here('tables/stress-dev-table2.csv'))
write.csv(tbl3, here('tables/stress-dev-table3.csv'))
write.csv(tbl4, here('tables/stress-dev-table4.csv'))
write.csv(tbl5, here('tables/stress-dev-table5.csv'))

save_as_docx("Table 1" = tbl1flex, "Table 2" = tbl2flex, "Table 3" = tbl3flex, "Table 4" = tbl4flex, "Table 5" = tbl5flex,  path='tables/stress-dev-tables.docx')

