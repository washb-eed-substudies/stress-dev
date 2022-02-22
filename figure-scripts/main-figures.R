###MAIN FIGURES####

rm(list=ls())
source(here::here("0-config.R"))


#GAM FIGURES
#start with plot_gam_diff() and plot_sig_heatmap()
#https://github.com/washb-eed-substudies/washbgam/tree/main/R

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

####################Plot_sig_heatmap#################
##Function
plot_sig_heatmap <- function(d,
                             pval_var="Pval", title="",
                             Outcome="Outcome", Exposure="Exposure",
                             print.est=T, print.ci=F,
                             null=0){
  
  require(RColorBrewer)
  
  colnames(d)[colnames(d)==pval_var] <- "pval"
  
  dfull <- expand_grid(unique(d$Y), unique(d$X))
  colnames(dfull) <- c("Y","X")
  d <- left_join(dfull, d, by=c("Y","X"))
  d <- distinct(d)
  
  #Get direction of estimate
  if(null==0){
    d$sign <- sign(d$point.diff)
  }else{
    d$sign <- ifelse(d$point.diff>1,1,-1)
  }
  
  #Get significance category
  d$pval_cat <- cut(d$pval, breaks = c(-1,0.01, 0.05, 0.2, 0.5, 2), labels = c("<0.01","<0.05","0.05-0.2","0.2-0.5","0.5-1"))
  d$pval_cat <- ifelse(d$sign== 1, paste0(d$pval_cat, " increase"), paste0(d$pval_cat, " decrease"))
  d$pval_cat[d$pval_cat %in% c("0.5-1 decrease", "0.5-1 increase")] <- "0.5-1"
  table(d$pval_cat)
  d$pval_cat <- factor(d$pval_cat, levels = c("<0.01 decrease",
                                              "<0.05 decrease", "0.05-0.2 decrease", "0.2-0.5 decrease",
                                              "0.5-1", "0.05-0.2 increase", "0.2-0.5 increase",
                                              "<0.05 increase", "<0.01 increase"))
  
  d$pval_cat <- addNA(d$pval_cat)
  levels(d$pval_cat) = c(levels(d$pval_cat), "Not estimated")
  d$pval_cat[is.na(d$pval_cat)] <- "Not estimated"
  
  table(d$pval_cat)
  table(is.na(d$pval_cat))
  
  d$est=""
  if(print.est){
    d$est=round(d$point.diff, 2)
    if(print.ci){
      d$est= paste0(
        round(d$est, 2), " (",
        round(d$lb.diff, 2), ", ",
        round(d$ub.diff, 2), ")"
      )
    }
  }
  d$est=gsub("NA \\(NA, NA\\)","",d$est)
  
  
  textcol = "grey20"
  cols = rev(brewer.pal(n = 9, name = "Spectral"))
  
  colours <- c("<0.01 decrease" = cols[1],
               "<0.05 decrease" = cols[2],
               "0.05-0.2 decrease"  = cols[3],
               "0.2-0.5 decrease"  = cols[4],
               "0.5-1" = cols[5],
               "0.2-0.5 increase" = cols[6],
               "0.05-0.2 increase" = cols[7],
               "<0.05 increase" = cols[8],
               "<0.01 increase" = cols[9],
               "Not estimated"="gray80")
  d <- droplevels(d)
  
  hm <- ggplot(d, aes(x=X, y=Y, fill=pval_cat)) +
    geom_tile(colour="grey80",size=0.25) +
    scale_x_discrete(expand=c(0,0), limits = rev(levels(d$X)))+
    scale_y_discrete(expand=c(0,0))+
    theme_minimal(base_size=10) +
    scale_fill_manual(#labels = levels(d$pval_cat),
      values = colours, drop = FALSE) +
    geom_text(aes(label=est)) +
    theme(
      aspect.ratio = 1,
      legend.title=element_text(color=textcol,size=8),
      legend.margin = margin(grid::unit(0.1,"cm")),
      legend.text=element_text(colour=textcol,size=7,face="bold"),
      legend.key.height=grid::unit(0.2,"cm"),
      legend.key.width=grid::unit(1,"cm"),
      legend.position = "right",
      #axis.text.x=element_text(size=8,colour=textcol,angle=45,hjust=1),
      axis.text.x=element_text(size=8,colour=textcol),
      axis.text.y=element_text(size=8,vjust = 0.2,colour=textcol),
      axis.ticks=element_line(size=0.4),
      plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
      strip.text.x = element_text(size=10),
      strip.text.y = element_text(angle=0,size=10),
      plot.background=element_blank(),
      panel.border=element_blank(),
      strip.background = element_blank(),
      panel.background=element_rect(fill="grey80", colour="grey80"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank()
    ) +
    guides(fill = guide_legend("P-value strength", ncol=1)) +
    labs(x=Exposure,y=Outcome,title=title)
  hm
  
  return(hm)
}

###H1A
heat1a <- plot_sig_heatmap(H1a_adj,
                 pval_var="Pval", title="",
                 Outcome="Outcome", Exposure="Exposure",
                 print.est=T, print.ci=F,
                 null=0)

###H1B
heat1b <- plot_sig_heatmap(H1b_adj,
                 pval_var="Pval", title="",
                 Outcome="Outcome", Exposure="Exposure",
                 print.est=T, print.ci=F,
                 null=0)

###H2
heat2 <- plot_sig_heatmap(H2_adj,
                 pval_var="Pval", title="",
                 Outcome="Outcome", Exposure="Exposure",
                 print.est=T, print.ci=F,
                 null=0)

###H3
heat3 <- plot_sig_heatmap(H3_adj,
                 pval_var="Pval", title="",
                 Outcome="Outcome", Exposure="Exposure",
                 print.est=T, print.ci=F,
                 null=0)

###H4
heat4 <- plot_sig_heatmap(H4_adj,
                 pval_var="Pval", title="",
                 Outcome="Outcome", Exposure="Exposure",
                 print.est=T, print.ci=F,
                 null=0)

ggsave(heat1a, file = here::here("figures/H1a_heatmap.png"), height=10, width=8)
ggsave(heat1b, file = here::here("figures/H1b_heatmap.png"), height=10, width=8)
ggsave(heat2, file = here::here("figures/H2_heatmap.png"), height=10, width=8)
ggsave(heat3, file = here::here("figures/H3_heatmap.png"), height=10, width=8)
ggsave(heat4, file = here::here("figures/H4_heatmap.png"), height=10, width=8)


######################FOREST PLOTS ####################################

###Adapted from WASH-Stress
library(ggtext)

#H1A
p1a <- ggplot(H1a_adj, (aes(x=X, y=point.diff))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff),
                width = 0.75, size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Y, ncol=1, scales="free") +
  coord_flip() +
  labs(y = "Mean difference", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(vjust=1),
        axis.text.y=ggtext::element_markdown(),
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(1, "lines")) 
p1a
#H1B
p1b <- ggplot(H1b_adj, (aes(x=X, y=point.diff))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff),
                width = 0.75, size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Y, ncol=1, scales="free") +
  coord_flip() +
  labs(y = "Mean difference", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(vjust=1),
        axis.text.y=ggtext::element_markdown(),
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(1, "lines")) 
p1b
#H2
p2 <- ggplot(H2_adj, (aes(x=X, y=point.diff))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff),
                width = 0.75, size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Y, ncol=1, scales="free") +
  coord_flip() +
  labs(y = "Mean difference", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(vjust=1),
        axis.text.y=ggtext::element_markdown(),
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(1, "lines")) 
p2

#H3
p3 <- ggplot(H3_adj, (aes(x=X, y=point.diff))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff),
                width = 0.75, size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Y, ncol=1, scales="free") +
  coord_flip() +
  labs(y = "Mean difference", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(vjust=1),
        axis.text.y=ggtext::element_markdown(),
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(1, "lines")) 
p3

#H4
p4 <- ggplot(H4_adj, (aes(x=X, y=point.diff))) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lb.diff, ymax=ub.diff),
                width = 0.75, size = 1) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Y, ncol=1, scales="free") +
  coord_flip() +
  labs(y = "Mean difference", x = "Biomarker") +
  theme(axis.ticks.x=element_blank(),
        legend.position = "bottom",
        strip.text = element_text(vjust=1),
        axis.text.y=ggtext::element_markdown(),
        #axis.text.x=ggtext::element_markdown(),
        plot.title = element_text(hjust = 0.5, face = "plain", size=9),
        panel.spacing = unit(1, "lines")) 
p4
#SAVE PLOTS
ggsave(p1a, file = here::here("figures/H1a_forest_diff_adj.png"), height=10, width=8)
ggsave(p1b, file = here::here("figures/H1b_forest_diff_adj.png"), height=10, width=8)
ggsave(p2, file = here::here("figures/H2_forest_diff_adj.png"), height=10, width=8)
ggsave(p3, file = here::here("figures/H3_forest_diff_adj.png"), height=10, width=8)
ggsave(p4, file = here::here("figures/H4_forest_diff_adj.png"), height=10, width=8)

