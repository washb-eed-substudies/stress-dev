rm(list=ls())

library('flextable')
library('officer')
source(here::here("0-config.R")) 

source(here::here("tables/table-functions.R"))
here::here()
d <- box_read("880476682582")

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

H1a_who <- readRDS(here('results/unadjusted/H1a_who_res.RDS'))
H1a_who_adj <- readRDS(here('results/adjusted/H1a_who_adj_res.RDS'))
H1a_who_adj_emm <- readRDS(here('results/adjusted/H1a_who_adj_emm_res.RDS'))

#### Functions for growth tables ####
source(here::here("tables/table-functions.R"))


#### MAIN TABLES ####
#### Table 1 ####

#Relevel sex (1=female, 0= male) for nperc function

levels(d$sex) <- c("1", "0")
      
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
               
               mediqr(d$sum_who_t2_t3), mediqr(d$z_cdi_say_t2), mediqr(d$z_cdi_und_t2),
               mediqr(d$z_comm_easq_t3), mediqr(d$z_motor_easq_t3), mediqr(d$z_personal_easq_t3), mediqr(d$z_combined_easq_t3), mediqr(d$z_cdi_say_t3), mediqr(d$z_cdi_und_t3),
               
               mediqr(d$laz_t2), mediqr(d$waz_t2), mediqr(d$whz_t2), mediqr(d$hcz_t2),
               mediqr(d$laz_t3), mediqr(d$waz_t3), mediqr(d$whz_t3), mediqr(d$hcz_t3),
               nperc(d$diar7d_t2), nperc(d$diar7d_t3), mediqr(d$momage), mediqr(d$momheight), 
               mediqr(d$momeduy), mediqr(d$cesd_sum_t2), mediqr(d$cesd_sum_ee_t3), mediqr(d$pss_sum_mom_t3), 
               nperc(d$life_viol_any_t3))

tbl1 <- data.table("C1" = c("Child","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","Mother","","","","","",""),
                   "C2" = c("", "Urinary F2-isoprostanes (ng/mg creatinine; Year 1)","","","", "Salivary cortisol reactivity (ug/dl; Year 2)","", "Salivary alpha-amylase reactivity (U/ml; Year 2)","",
                            "Sympathetic-adreno-medullar biomarkers (Year 2)","", "Glucocorticoid receptor percent methylation","", "Child development (Year 1)", "", "", "Child development (Year 2)", "", "", "", "", "", "Anthropometry (14 months, Year 1)","","","",
                            "Anthropometry (28 months, Year 2)","","","", "Diarrhea (14 months, Year 1)", "Diarrhea (28 months, Year 2)","",
                            "Anthropometry at enrollment", "Education", "Depression at Year 1", "Depression at Year 2", "Perceived stress at Year 2", 
                            "Intimate partner violence"),
                   "C3" = c("Female", "iPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a-VI", "8,12-iso-iPF(2a)-VI", 
                            "Cortisol reactivity", "Cortisol residualized gain score", 
                            "Salivary alpha-amylase reactivity", "Salivary alpha-amylase residualized gain score",
                            "Mean arterial pressure (mmHg)", "Resting heart rate (bpm)", "NR3C1 exon 1F promoter", "NGFI-A transcription factor binding site",
                            "WHO gross motor milestone sum score", "CDI expressive language z-score", "CDI language understanding z-score", 
                            "EASQ communication z-score", "EASQ motor development z-score", "EASQ personal-social development z-score", "EASQ combined z-score", "CDI expressive language z-score", "CDI language understanding z-score",
                            "Length-for-age z-score", "Weight-for-age z-score", "Weight-for-length z-score", "Head circumference-for-age z-score",
                            "Length-for-age z-score", "Weight-for-age z-score", "Weight-for-length z-score", "Head circumference-for-age z-score",
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
#Hypothesis 2#
exposure <- c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02" )
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 


expo_var <- c("Cortisol reactivity (ug/dl/min)", "Pre-stressor cortisol (ug/dl)", "Post-stressor cortisol (ug/dl)", "Salivary alpha-amylase reactivity (U/ml/min)", "Pre-stressor salivary alpha-amylase (U/ml)", "Post-stressor salivary alpha-amylase (U/ml)")
out_var <- c("EASQ communication score", "EASQ gross motor score", "EASQ personal social score", "Combined EASQ score",
             "CDI expressive language score","CDI comprehension score")


tbl2 <- growth_tbl("Exposure", expo_var, out_var, exposure, outcome, H2, H2_adj, adj_only = T)
tbl2flex <- growth_tbl_flex("Exposure", expo_var, out_var, exposure, outcome, H2, H2_adj, adj_only = T)

#### Table 3 ####
#Hypothesis 4#

exposure <- c("t3_gcr_mean", "t3_gcr_cpg12")   
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean overall percentage glucocorticoid receptor methylation", "Percentage methylation at NGFI-A transcription factor binding site (CpG site #12)")
out_var <- c("EASQ communication score", "EASQ gross motor score", "EASQ personal social score", "Combined EASQ score",
             "CDI expressive language score","CDI comprehension score")

tbl3 <- growth_tbl("Exposure", expo_var, out_var, exposure, outcome, H4, H4_adj, adj_only = T)
tbl3flex <- growth_tbl_flex("Exposure", expo_var, out_var, exposure, outcome, H4, H4_adj, adj_only = T)

#### Table 4 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")
outcome <- c("sum_who_t2_t3", "z_cdi_say_t2","z_cdi_und_t2")
expo_var <- c("IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative status score")
out_var <- c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI expressive language Z-score","CDI comprehension Z-score")

tbl4 <- growth_tbl("Exposure", expo_var, out_var, exposure, outcome, H1a, H1a_adj, adj_only = T)
tbl4flex <- growth_tbl_flex("Exposure", expo_var, out_var, exposure, outcome, H1a, H1a_adj, adj_only = T)

######

#### Table 5 ####

exposure <- c("t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3","z_personal_easq_t3","z_combined_easq_t3", 
             "z_cdi_say_t3","z_cdi_und_t3")
expo_var <- c("IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative status score")
out_var <- c("EASQ communication score", "EASQ gross motor score", "EASQ personal social score", "Combined EASQ score",
             "CDI expressive language score","CDI comprehension score")

tbl5 <- growth_tbl("Exposure", expo_var, out_var, exposure, outcome, H1b, H1b_adj, adj_only = T)
tbl5flex <- growth_tbl_flex("Exposure", expo_var, out_var, exposure, outcome, H1b, H1b_adj, adj_only = T)

#Table 6. WHO Hazard Ratios

exposure <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")    

outcome <- c("who_sit", "who_crawl", "who_stand_supp",
             "who_walk_supp", "who_stand_nosupp", "who_walk_nosup" )

expo_var <- c("IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative status score")
out_var <- c("Time to sitting unsupported", "Time to crawling","Time to standing with support",
             "Time to walking with support", "Time to standing unsupported", "Time to walking unsupported")

tbl6 <- hr_tbl("Exposure", expo_var, out_var, exposure, outcome, H1a_who, H1a_who_adj, adj_only = T)
tbl6flex <- hr_tbl_flex("Exposure", expo_var, out_var, exposure, outcome, H1a_who, H1a_who_adj, adj_only = T)


#### Table 7 ####
#Hypothesis 3#
exposure <- c("t3_map", "t3_hr_mean")  
outcome <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
             "z_cdi_say_t3", "z_cdi_und_t3") 
expo_var <- c("Mean arterial pressure (mmHg)", "Mean resting heart rate (bpm)")
out_var <- c("EASQ communication score", "EASQ gross motor score", "EASQ personal social score", "Combined EASQ score",
             "CDI expressive language score","CDI comprehension score")

tbl7 <- growth_tbl("Exposure", expo_var, out_var, exposure, outcome, H3, H3_adj, adj_only = T)
tbl7flex <- growth_tbl_flex("Exposure", expo_var, out_var, exposure, outcome, H3, H3_adj, adj_only = T)

#Change font and size
tbl1flex <- tbl1flex %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(i = NULL, j = NULL, size = 8, part = "all") %>%
  set_table_properties(layout = "autofit")

tbl2flex <- tbl2flex %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(i = NULL, j = NULL, size = 8, part = "all") %>%
  set_table_properties(layout = "autofit")

tbl3flex <- tbl3flex %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(i = NULL, j = NULL, size = 8, part = "all") %>%
  set_table_properties(layout = "autofit")

tbl4flex <- tbl4flex %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(i = NULL, j = NULL, size = 8, part = "all") %>%
  set_table_properties(layout = "autofit")

tbl5flex <- tbl5flex %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(i = NULL, j = NULL, size = 8, part = "all") %>%
  set_table_properties(layout = "autofit")

tbl6flex <- tbl6flex %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(i = NULL, j = NULL, size = 8, part = "all") %>%
  set_table_properties(layout = "autofit")

tbl7flex <- tbl7flex %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(i = NULL, j = NULL, size = 8, part = "all") %>%
  set_table_properties(layout = "autofit")

#### SAVE TABLES ####
write.csv(tbl1, here('tables/stress-dev-table1.csv'))
write.csv(tbl2, here('tables/stress-dev-table2.csv'))
write.csv(tbl3, here('tables/stress-dev-table3.csv'))
write.csv(tbl4, here('tables/stress-dev-table4.csv'))
write.csv(tbl5, here('tables/stress-dev-table5.csv'))
write.csv(tbl6, here('tables/stress-dev-table6.csv'))
write.csv(tbl7, here('tables/stress-dev-table6.csv'))

save_as_docx("Table 1. Descriptive statistics of sample population" = tbl1flex, "Table 3. Salivary stress biomarkers and child development at Year 2" = tbl2flex, "Table 4. Glucocorticoid receptor methylation and child development at Year 2" = tbl3flex, "Table 5. Urinary isoprostanes and child development at Year 1" = tbl4flex, "Table 6. Urinary isoprostanes at Year 1 and child development at Year 2" = tbl5flex, "Table 7. Urinary isoprostanes and time to WHO motor milestone at Year 1" = tbl6flex, "Table 8. Mean arterial pressure and heart rate and child development at Year 2" = tbl7flex, 
            pr_section = sect_properties, path='tables/stress-dev-tables.docx')

