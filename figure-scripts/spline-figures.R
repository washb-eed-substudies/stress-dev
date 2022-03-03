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
      geom_smooth(aes(y = fit, color=y, linetype="ng/mg creatinine"), se = F) 
  } else {
    p <- d %>% ggplot(aes(x=X))+
      geom_smooth(aes(y = fit, color=y, linetype="Combined score via PCA"), se = F)
  }
  p <- p +
    geom_vline(aes(xintercept=q1), size=.5, color="grey30", linetype="dashed") +
    geom_vline(aes(xintercept=q3), size=.5, color="grey30", linetype="dashed") +
    #geom_point(aes(y=Y), alpha=0.5) +
    scale_colour_manual(values=tableau10[c(1:7)], limits=color_levels) + 
    scale_fill_manual(values=tableau10[c(1:7)], limits=color_levels) + 
    scale_linetype_manual("Outcome measurement",values=c("ng/mg creatinine"=1,"Combined score via PCA"=6)) +
    geom_ribbon(aes(ymin=lwrS, ymax=uprS, fill=y, color=y), alpha=0.5) + 
    xlab(" ") + ylab(element_blank()) + 
    facet_grid(rows = vars(y), scales = "free_y") 
  
  
  if(!theme) {p <- p + theme(strip.text.y = element_blank())}
  if(!is.null(title)) {p <- p + ylab(title)}
  if(x_label) {p <- p + xlab(d$x[1])}
  
  p + theme(legend.position = "right") + guides(color="none", fill="none", linetype=guide_legend(override.aes = aes(color="black")))
}

#### SPLINE FIGURES 


#Figure 12 - H1A: Urinary isoprostanes at Year 1 and child development at Year 1



d2 <- d_for_plot(c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined score"),
                 c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI expressive language Z-score","CDI comprehension Z-score"), 
                 c("t2_f2_8ip", "t2_f2_23d","t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca"), 
                 c("sum_who_t2_t3", "z_cdi_say_t2","z_cdi_und_t2"),
                 H1a_spline, H1a_quartiles)

d2$x <- factor(d2$x, levels=c("IPF(2a)-III", "2,3-dinor-iPF(2a)-III", "iPF(2a)-VI", "8,12-iso-iPF(2a)-VI", "Combined score"))
d2$y <- factor(d2$y, levels=c("Sum of 2nd, 4th, 5th, and 6th WHO motor milestones", "CDI expressive language Z-score","CDI comprehension Z-score"))

p <- plot_facet(d2 %>% filter(grepl("sum_who", Yvar) & Xvar=="t2_f2_8ip"), F, "Sum of WHO motor\nmilestones", F,  type = "solid") 
p1 <- plot_facet(d2 %>% filter(grepl("sum_who", Yvar) & Xvar=="t2_f2_23d"), F, " ", F, type = "solid")
p2 <- plot_facet(d2 %>% filter(grepl("sum_who", Yvar) & Xvar=="t2_f2_VI"), F, " ", F, type = "solid")
p3 <- plot_facet(d2 %>% filter(grepl("sum_who", Yvar) & Xvar=="t2_f2_12i"), F, " ", F, type = "solid")
p4 <- plot_facet(d2 %>% filter(grepl("sum_who", Yvar) & Xvar=="t2_f2_iso.pca"), F, " ", F, type = "long-dash")

pa <- plot_facet(d2 %>% filter(grepl("z_cdi_say", Yvar) & Xvar=="t2_f2_8ip"), F, "CDI Expression", F, type = "solid")
pa1 <- plot_facet(d2 %>% filter(grepl("z_cdi_say", Yvar) & Xvar=="t2_f2_23d"), F, " ", F, type = "solid")
pa2 <- plot_facet(d2 %>% filter(grepl("z_cdi_say", Yvar) & Xvar=="t2_f2_VI"), F, " ", F, type = "solid")
pa3 <- plot_facet(d2 %>% filter(grepl("z_cdi_say", Yvar) & Xvar=="t2_f2_12i"), F, " ", F, type = "solid")
pa4 <- plot_facet(d2 %>% filter(grepl("z_cdi_say", Yvar) & Xvar=="t2_f2_iso.pca"), F, " ", F, type = "long-dash")

pb <- plot_facet(d2 %>% filter(grepl("z_cdi_und_t2", Yvar)& Xvar=="t2_f2_8ip"), F, "CDI Comprehension", T, type = "solid")
pb1 <- plot_facet(d2 %>% filter(grepl("z_cdi_und_t2", Yvar) & Xvar=="t2_f2_23d"), F, " ", T, type = "solid") 
pb2 <- plot_facet(d2 %>% filter(grepl("z_cdi_und_t2", Yvar) & Xvar=="t2_f2_VI"), F, " ", T, type = "solid") 
pb3 <- plot_facet(d2 %>% filter(grepl("z_cdi_und_t2", Yvar) & Xvar=="t2_f2_12i"), F, " ", T, type = "solid")

pb4 <- plot_facet(d2 %>% filter(grepl("z_cdi_und_t2", Yvar) & Xvar=="t2_f2_iso.pca"), F, " ", T, type = "long-dash")



ggpubr::ggarrange(p, p1, p2, p3, p4, pa, pa1, pa2, pa3, pa4, pb, pb1, pb2, pb3, pb4,
                  common.legend = T,
                  show.legend = F,
                  ncol = 5, nrow = 4,
                  heights = c(1, 1, 1, 1.2), widths=c(1.2, 1, 1, 1)) %>% 
  annotate_figure(top = text_grob("Spline curves of the relationships between concurrent urinary F2 isoprostanes and child development at Year 1", face = "bold", size = 12)) %>%
  ggsave(filename = here("figures/splines_H1a.jpg"), width = 10, height=15)

############################################