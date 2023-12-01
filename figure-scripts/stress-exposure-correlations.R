#Correlation matrix for stress biomarkers

rm(list=ls())
library(here)
library(tidyverse)
library(viridis)
library(rstatix)
df<- readRDS(here("data/stress_dev.RDS"))


cors <- df %>% 
  cor_test(
    vars = c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_gcr_mean", "t3_gcr_cpg12", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02", "t3_map", "t3_hr_mean", "t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")
  )

#Add labels
cors$var1 <- factor(cors$var1,
                   levels = c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_gcr_mean", "t3_gcr_cpg12", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02", "t3_map", "t3_hr_mean", "t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca"),
  labels = c("Cortisol Reactivity (ug/dl/min)", "Pre-stressor Cortisol (ug/dl)", "Post-stressor Cortisol (ug/dl)", "Mean Overall Percentage Glucocorticoid Receptor Methylation", "Percentage methylation at NGFI-A transcription factor binding site",
             "sAA Reactivity (U/ml/min)", "Pre-stressor sAA (U/ml)", "Post-stressor sAA (U/ml)", "Mean arterial pressure (mmHg)", "Mean Resting Heart Rate (bpm)",
  "IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative stress biomarker score"))

cors$var2 <- factor(cors$var2,
                    levels = c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_gcr_mean", "t3_gcr_cpg12", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02", "t3_map", "t3_hr_mean", "t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca"),
                    labels = c("Cortisol Reactivity (ug/dl/min)", "Pre-stressor Cortisol (ug/dl)", "Post-stressor Cortisol (ug/dl)", "Mean Overall Percentage Glucocorticoid Receptor Methylation", "Percentage methylation at NGFI-A transcription factor binding site",
                               "sAA Reactivity (U/ml/min)", "Pre-stressor sAA (U/ml)", "Post-stressor sAA (U/ml)", "Mean arterial pressure (mmHg)", "Mean Resting Heart Rate (bpm)",
                               "IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative stress biomarker score"))

?ggplot()
#
p <- ggplot(cors, aes(x = var1, y = var2, fill = p)) + 
  geom_tile(#colour = "grey80", size = 0.25
  ) + 
  scale_fill_viridis() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_discrete(expand = c(0, 0)) +
  theme_minimal(base_size = 8) + 
  theme(axis.text.x = element_text(angle = 90)) +
  theme(text = element_text(family = "Times New Roman"))
#+ ggtitle("TITLE")


#save plots
ggsave(p, file = here::here("figures/plot-stress-cors.png"), height=5, width=7)


