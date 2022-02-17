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

color_levels <- c("Sum of WHO motor milestones", "Comprehension", "Expressive language",
                  "Communication", "Gross motor", "Personal social", "Combined")

#### SPLINE FIGURES


#Figure 2 - H1B: Urinary isoprostanes at Year 1 and child development at Year 2


d2 <- d_for_plot(c("IPF(2a)-III (ng/mg creatinine)", "2,3-dinor-iPF(2a)-III (ng/mg creatinine)", "iPF(2a)-VI (ng/mg creatinine)", "8,12-iso-iPF(2a)-VI (ng/mg creatinine)", "Combined urinary oxidative stress biomarker score"),
                 c("EASQ Communication Score", "EASQ Gross Motor Score", "EASQ Personal Social Score", "Combined EASQ Score", "CDI expressive language Z-score","CDI comprehension Z-score"), 
                 c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca"), 
                 c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", "z_cdi_say_t3", "z_cdi_und_t3"),
                 H1b_spline, H1b_quartiles)

d2$x <- d2$x %>% as.factor()
d2$y <- factor(d2$y, levels=c("Sum of WHO motor milestones", 'Comprehension', "Expressive language",
                              "Communication", "Gross motor", "Personal social", "Combined"))
#p <- plot_facet(d2 %>% filter(grepl("sum_who", Yvar)), F, "Sum of WHO motor\nmilestones", F, type = "solid")
#p1 <- plot_facet(d2 %>% filter(grepl("z_cdi_.{3}_t2", Yvar)), F, "CDI Z-scores", T, type = "solid")
p <- plot_facet(d2 %>% filter(grepl("z_cdi_.{3}_t3", Yvar) & Xvar=="t2_f2_8ip"), F, "CDI Z-scores", F, "longdash")
p1 <- plot_facet(d2 %>% filter(grepl("z_cdi_.{3}_t3", Yvar) & Xvar=="t2_f2_23d"), T, NULL, F, "longdash") 
p2 <- plot_facet(d2 %>% filter(grepl("z_cdi_.{3}_t3", Yvar) & Xvar=="t2_f2_VI"), F, NULL, F, "longdash")
p3 <- plot_facet(d2 %>% filter(grepl("z_cdi_.{3}_t3", Yvar) & Xvar=="t2_f2_12i"), T, NULL, F, "longdash")
p4 <- plot_facet(d2 %>% filter(grepl("z_cdi_.{3}_t3", Yvar) & Xvar=="t2_f2_iso.pca"), T, NULL, F, "longdash")
p5 <- ggplot() + theme_nothing()
p6 <- plot_facet(d2 %>% filter(grepl("easq", Yvar) & Xvar=="t2_f2_8ip"), F, "EASQ Z-scores", F, "longdash")
p7 <- plot_facet(d2 %>% filter(grepl("easq", Yvar) & Xvar=="t2_f2_23d"), T, NULL, F, "longdash") 
p8 <- plot_facet(d2 %>% filter(grepl("easq", Yvar) & Xvar=="t2_f2_VI"), F, NULL, F, "longdash")
p9 <- plot_facet(d2 %>% filter(grepl("easq", Yvar) & Xvar=="t2_f2_12i"), T, NULL, F, "longdash")
p10 <- plot_facet(d2 %>% filter(grepl("easq", Yvar) & Xvar=="t2_f2_iso.pca"), T, NULL, F, "longdash")

ggpubr::ggarrange(p,ggplot()+theme_nothing(),ggplot()+theme_nothing(),
                  p, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, common.legend = T,
                  ncol = 2, nrow = 4,
                  heights = c(1, 1.7, 3.5), widths=c(1, 1, 1)) %>% ggsave(filename = here("figures/splines_H1b.jpg"), width = 10, height=15)
