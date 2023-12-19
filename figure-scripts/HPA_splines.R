#HPA Splines

#Figure scripts
rm(list=ls())
source(here::here("0-config.R"))
library(ggpubr)
library(cowplot)
library(patchwork)
theme_set(theme_ki())

#load spline data
H1a_spline <- readRDS(here("results/figure-data/H1a_adj_spline_data.RDS"))
H1b_spline <- readRDS(here("results/figure-data/H1b_adj_spline_data.RDS"))
H2_spline <- readRDS(here("results/figure-data/H2_adj_adj_spline_data.RDS"))
H3_spline <- readRDS(here("results/figure-data/H3_adj_spline_data.RDS"))
H4_spline <- readRDS(here("results/figure-data/H4_adj_spline_data.RDS"))

#load results for quartiles
H1a_quartiles <- readRDS(here("results/adjusted/H1a_adj_res.RDS"))
H1b_quartiles <- readRDS(here("results/adjusted/H1b_adj_res.RDS"))
H2_quartiles <- readRDS(here("results/adjusted/H2_adj_res.RDS"))
H3_quartiles <- readRDS(here("results/adjusted/H3_adj_res.RDS"))
H4_quartiles <- readRDS(here("results/adjusted/H4_adj_res.RDS"))
#

#### FUNCTIONS

d_for_plot <- function(x_name, y_name, x_var, y_var, spline, quart){
  d <- NULL
  for (i in 1: length(x_var)) {
    for (j in 1:length(y_var)){
      exists <- (quart%>%filter(X==x_var[i], Y==y_var[j]) %>% nrow()) != 0
      if (exists){
        new <- data.frame(x=x_name[i], y=y_name[j], spline%>%filter(Xvar==x_var[i], Yvar==y_var[j]), quart%>%filter(X==x_var[i], Y==y_var[j])%>%select(q1, q3))
        d <- rbind(d, new)
      }
    }
  }
  d
}

color_levels <- c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI expressive language Z-score","CDI comprehension Z-score")

plot_facet <- function(d, theme, title, x_label, type) {
  if (type=="solid") {
    p <- d %>% ggplot(aes(x=X))+
      geom_smooth(aes(y = fit, color=y), se = F) 
  } else {
    p <- d %>% ggplot(aes(x=X))+
      geom_smooth(aes(y = fit, color=y), se = F)
  }
  p <- p +
    geom_vline(aes(xintercept=q1), size=.5, color="grey30", linetype="dashed") +
    geom_vline(aes(xintercept=q3), size=.5, color="grey30", linetype="dashed") +
    #geom_point(aes(y=Y), alpha=0.5) +
    scale_colour_manual(values=tableau10[c(1:7)], limits=color_levels) + 
    scale_fill_manual(values=tableau10[c(1:7)], limits=color_levels) + 
    #scale_linetype_manual("Outcome measurement",values=c("ng/mg creatinine"=1,"Combined score via PCA"=6)) +
    geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) + 
    xlab(" ") + ylab(element_blank()) + 
    facet_grid(rows = vars(y), scales = "free_y") 
  
  
  if(!theme) {p <- p + theme(strip.text.y = element_blank())}
  if(!is.null(title)) {p <- p + ylab(title)}
  if(x_label) {p <- p + xlab(d$x[1])}
  
  p + theme(legend.position = "right") + guides(color="none", fill="none", linetype=guide_legend(override.aes = aes(color="black")))
}
#Rebind H2 for cort
#Cortisol from H2


#Bind with GCR methylation from H4
H_hpa_spline <- rbind(H2_spline, H4_spline)

#Bind quartiles

H_hpa_quartiles <- rbind(H2_quartiles, H4_quartiles)
#HPA Axis Spline
#first run main-figure.R script
color_levels <- c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ", "CDI expressive language Z-score","CDI comprehension Z-score")



d <- d_for_plot(c("Mean Overall Percentage Glucocorticoid Receptor Methylation", "Percentage methylation at NGFI-A transcription factor binding site", "Cortisol Reactivity (ug/dl/min)", "Pre-stressor Cortisol (ug/dl)", "Post-stressor Cortisol (ug/dl)"),
                 c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ", "CDI expressive language Z-score","CDI comprehension Z-score"), 
                 c("t3_gcr_mean", "t3_gcr_cpg12", "t3_cort_slope", "t3_cort_z01", "t3_cort_z03"), 
                 c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
                   "z_cdi_say_t3", "z_cdi_und_t3"),
                 H_hpa_spline, H_hpa_quartiles)

d$x <- factor(d$x, levels=c("Mean Overall Percentage Glucocorticoid Receptor Methylation", "Percentage methylation at NGFI-A transcription factor binding site", "Cortisol Reactivity (ug/dl/min)", "Pre-stressor Cortisol (ug/dl)", "Post-stressor Cortisol (ug/dl)"))
d$y <- factor(d$y, levels=c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ", "CDI expressive language Z-score","CDI comprehension Z-score"))

p <- plot_facet(d %>% filter(grepl("z_comm_easq_t3", Yvar) & Xvar=="t3_gcr_mean"), F, "EASQ Communication Score", F,  type = "solid") 
p1 <- plot_facet(d %>% filter(grepl("z_comm_easq_t3", Yvar) & Xvar=="t3_gcr_cpg12"), F, " ", F, type = "solid")
p2 <- plot_facet(d %>% filter(grepl("z_comm_easq_t3", Yvar) & Xvar=="t3_cort_slope"), F, " ", F, type = "solid")
p3 <- plot_facet(d %>% filter(grepl("z_comm_easq_t3", Yvar) & Xvar=="t3_cort_z01"), F, " ", F, type = "solid")
p4 <- plot_facet(d %>% filter(grepl("z_comm_easq_t3", Yvar) & Xvar=="t3_cort_z03"), F, " ", F, type = "solid")

pa <- plot_facet(d %>% filter(grepl("z_motor_easq_t3", Yvar) & Xvar=="t3_gcr_mean"), F, "EASQ Motor Development Score", F,  type = "solid") 
pa1 <- plot_facet(d %>% filter(grepl("z_motor_easq_t3", Yvar) & Xvar=="t3_gcr_cpg12"), F, " ", F, type = "solid")
pa2 <- plot_facet(d %>% filter(grepl("z_motor_easq_t3", Yvar) & Xvar=="t3_cort_slope"), F, " ", F, type = "solid")
pa3 <- plot_facet(d %>% filter(grepl("z_motor_easq_t3", Yvar) & Xvar=="t3_cort_z01"), F, " ", F, type = "solid")
pa4 <- plot_facet(d %>% filter(grepl("z_motor_easq_t3", Yvar) & Xvar=="t3_cort_z03"), F, " ", F, type = "solid")

pb <- plot_facet(d %>% filter(grepl("z_personal_easq_t3", Yvar) & Xvar=="t3_gcr_mean"), F, "EASQ Personal-Social Development Score", F,  type = "solid") 
pb1 <- plot_facet(d %>% filter(grepl("z_personal_easq_t3", Yvar) & Xvar=="t3_gcr_cpg12"), F, " ", F, type = "solid")
pb2 <- plot_facet(d %>% filter(grepl("z_personal_easq_t3", Yvar) & Xvar=="t3_cort_slope"), F, " ", F, type = "solid")
pb3 <- plot_facet(d %>% filter(grepl("z_personal_easq_t3", Yvar) & Xvar=="t3_cort_z01"), F, " ", F, type = "solid")
pb4 <- plot_facet(d %>% filter(grepl("z_personal_easq_t3", Yvar) & Xvar=="t3_cort_z03"), F, " ", F, type = "solid")

pc <- plot_facet(d %>% filter(grepl("z_combined_easq_t3", Yvar) & Xvar=="t3_gcr_mean"), F, "EASQ Combined Score", F,  type = "solid") 
pc1 <- plot_facet(d %>% filter(grepl("z_combined_easq_t3", Yvar) & Xvar=="t3_gcr_cpg12"), F, " ", F, type = "solid")
pc2 <- plot_facet(d %>% filter(grepl("z_combined_easq_t3", Yvar) & Xvar=="t3_cort_slope"), F, " ", F, type = "solid")
pc3 <- plot_facet(d %>% filter(grepl("z_combined_easq_t3", Yvar) & Xvar=="t3_cort_z01"), F, " ", F, type = "solid")
pc4 <- plot_facet(d %>% filter(grepl("z_combined_easq_t3", Yvar) & Xvar=="t3_cort_z03"), F, " ", F, type = "solid")

pd <- plot_facet(d %>% filter(grepl("z_cdi_say_t3", Yvar) & Xvar=="t3_gcr_mean"), F, "CDI Expressive Language Score", F,  type = "solid") 
pd1 <- plot_facet(d %>% filter(grepl("z_cdi_say_t3", Yvar) & Xvar=="t3_gcr_cpg12"), F, " ", F, type = "solid")
pd2 <- plot_facet(d %>% filter(grepl("z_cdi_say_t3", Yvar) & Xvar=="t3_cort_slope"), F, " ", F, type = "solid")
pd3 <- plot_facet(d %>% filter(grepl("z_cdi_say_t3", Yvar) & Xvar=="t3_cort_z01"), F, " ", F, type = "solid")
pd4 <- plot_facet(d %>% filter(grepl("z_cdi_say_t3", Yvar) & Xvar=="t3_cort_z03"), F, " ", F, type = "solid")

pe <- plot_facet(d %>% filter(grepl("z_cdi_und_t3", Yvar) & Xvar=="t3_gcr_mean"), F, "CDI Receptive Language Score", T,  type = "solid") 
pe1 <- plot_facet(d %>% filter(grepl("z_cdi_und_t3", Yvar) & Xvar=="t3_gcr_cpg12"), F, " ", T, type = "solid")
pe2 <- plot_facet(d %>% filter(grepl("z_cdi_und_t3", Yvar) & Xvar=="t3_cort_slope"), F, " ", T, type = "solid")
pe3 <- plot_facet(d %>% filter(grepl("z_cdi_und_t3", Yvar) & Xvar=="t3_cort_z01"), F, " ", T, type = "solid")
pe4 <- plot_facet(d %>% filter(grepl("z_cdi_und_t3", Yvar) & Xvar=="t3_cort_z03"), F, " ", T, type = "solid")
# Load the cowplot package
library(cowplot)

# Arrange the plots using plot_grid
arranged_plot <- plot_grid(p, p1, p2, p3, p4, pa, pa1, pa2, pa3, pa4, pb, pb1, pb2, pb3, pb4, pc, pc1, pc2, pc3, pc4, pd, pd1, pd2, pd3, pd4, pe, pe1, pe2, pe3, pe4,
                           align = 'v', 
                           ncol = 5, 
                           nrow = 6)
                      

# Save the arranged plot
ggsave(filename = here("figures/HPA_splines.jpg"), plot = arranged_plot, width = 30, height = 25)

arranged_plot


