


rm(list=ls())

source(here::here("0-config.R"))


H1a_res <- readRDS(paste0(here::here(),"/results/adjusted/H1a_adj_emm_res.RDS"))
H1b_res <- readRDS(paste0(here::here(),"/results/adjusted/H1b_adj_emm_res.RDS"))
H2_res <- readRDS(paste0(here::here(),"/results/adjusted/H2_adj_emm_res.RDS"))
H3_res <- readRDS(paste0(here::here(),"/results/adjusted/H3_adj_emm_res.RDS"))
H4_res <- readRDS(paste0(here::here(),"/results/adjusted/H4_adj_emm_res.RDS"))


#Cleaner function from stress-growth

# clean_H1 <- function(res){
#   res$Y <- gsub("_t2_t3","",res$Y)
#   res$Y <- gsub("delta","Change in",res$Y)
#   res$Y <- gsub("_"," ",res$Y)
#   
#   res$Y <- gsub("laz","LAZ",res$Y)
#   res$Y <- gsub("waz","WAZ",res$Y)
#   res$Y <- gsub("whz","WLZ",res$Y)
#   res$Y <- gsub("hcz","HCZ",res$Y)
#   
#   res$Y <- gsub("t2","-Year 1",res$Y)
#   res$Y <- gsub("t3","-Year 2",res$Y)
#   
#   res$Y <- gsub("len","Length",res$Y)
#   res$Y <- gsub("wei","Weight",res$Y)
#   res$Y <- gsub("hc","Head Circumference",res$Y)
#   
#   unique(res$Y)
#   res$Y <- factor(res$Y, levels = rev(c("LAZ -Year 1","WAZ -Year 1","WLZ -Year 1","HCZ -Year 1",               
#                                         "LAZ -Year 2",   "WAZ -Year 2","WLZ -Year 2","HCZ -Year 2",                
#                                         "Change in LAZ","Change in WAZ","Change in WLZ","Change in HCZ",
#                                         "Length velocity","Weight velocity","Head Circumference velocity")))
#   return(res)
# }


#format for plots
H1a_res_f <- H1a_res %>% group_by(X,Y,V) %>% summarize(point.diff=last(point.diff)-first(point.diff), Pval=int.p[1])
H1b_res_f <- H1b_res %>% group_by(X,Y,V) %>% summarize(point.diff=last(point.diff)-first(point.diff), Pval=int.p[1])
H2_res_f <- H2_res %>% group_by(X,Y,V) %>% summarize(point.diff=last(point.diff)-first(point.diff), Pval=int.p[1])
H3_res_f <- H3_res %>% group_by(X,Y,V) %>% summarize(point.diff=last(point.diff)-first(point.diff), Pval=int.p[1])
H4_res_f <- H4_res %>% group_by(X,Y,V) %>% summarize(point.diff=last(point.diff)-first(point.diff), Pval=int.p[1])



plot_sig_heatmap

hm1a<- washbgam::plot_sig_heatmap(H1a_res_f)
hm1b_t2<- washbgam::plot_sig_heatmap(H1b_res_f %>% filter(V=="fci_t2"))
hm1b_t3<- washbgam::plot_sig_heatmap(H1b_res_f %>% filter(V=="fci_t3"))

hm2_t2 <- washbgam::plot_sig_heatmap(H2_res_f %>% filter(V=="fci_t2"))
hm2_t3 <- washbgam::plot_sig_heatmap(H2_res_f %>% filter(V=="fci_t3"))

hm3_t2 <- washbgam::plot_sig_heatmap(H3_res_f %>% filter(V=="fci_t2"))
hm3_t3 <- washbgam::plot_sig_heatmap(H3_res_f %>% filter(V=="fci_t3"))

hm4_t2 <- washbgam::plot_sig_heatmap(H4_res_f %>% filter(V=="fci_t2"))
hm4_t3 <- washbgam::plot_sig_heatmap(H4_res_f %>% filter(V=="fci_t3"))


plot_emm_heatmap <- function(d, pval_var = "Pval", title = "", Outcome = "Outcome", 
          Exposure = "Exposure", print.est = T, print.ci = F, 
          null = 0){
  require(RColorBrewer)
  colnames(d)[colnames(d) == pval_var] <- "pval"
  dfull <- expand_grid(unique(d$Y), unique(d$X))
  colnames(dfull) <- c("Y", "X")
  d <- left_join(dfull, d, by = c("Y", "X"))
  d <- distinct(d)
  if (null == 0) {
    d$sign <- sign(d$point.diff)
  }
  else {
    d$sign <- ifelse(d$point.diff > 1, 1, -1)
  }
  d$pval_cat <- cut(d$pval, breaks = c(-1, 0.01, 0.05, 0.2, 
                                       0.5, 2), labels = c("<0.01", "<0.05", "0.05-0.2", 
                                                           "0.2-0.5", "0.5-1"))
  d$pval_cat <- ifelse(d$sign == 1, paste0(d$pval_cat, " increase"), 
                       paste0(d$pval_cat, " decrease"))
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
  d$est = ""
  if (print.est) {
    d$est = round(d$point.diff, 2)
    if (print.ci) {
      d$est = paste0(round(d$est, 2), " (", round(d$lb.diff, 
                                                  2), ", ", round(d$ub.diff, 2), ")")
    }
  }
  d$est = gsub("NA \\(NA, NA\\)", "", d$est)
  textcol = "grey20"
  cols = rev(brewer.pal(n = 9, name = "Spectral"))
  colours <- c(`<0.01 decrease` = cols[1], `<0.05 decrease` = cols[2], 
               `0.05-0.2 decrease` = cols[3], `0.2-0.5 decrease` = cols[4], 
               `0.5-1` = cols[5], `0.2-0.5 increase` = cols[6], 
               `0.05-0.2 increase` = cols[7], `<0.05 increase` = cols[8], 
               `<0.01 increase` = cols[9], `Not estimated` = "gray80")
  d <- d %>% filter(!is.na(V)) %>% droplevels()
  hm <- ggplot(d, aes(x = X, y = Y, fill = pval_cat)) + geom_tile(colour = "grey80", 
                                                                  size = 0.25) + scale_x_discrete(expand = c(0, 0), limits = rev(levels(d$X))) + 
    facet_wrap(~V) +
    scale_y_discrete(expand = c(0, 0)) + theme_minimal(base_size = 10) + 
    scale_fill_manual(values = colours, drop = FALSE) + geom_text(aes(label = est)) + 
    theme(aspect.ratio = 1, legend.title = element_text(color = textcol, 
                                                        size = 8), legend.margin = margin(grid::unit(0.1, 
                                                                                                     "cm")), legend.text = element_text(colour = textcol, 
                                                                                                                                        size = 7, face = "bold"), legend.key.height = grid::unit(0.2, 
                                                                                                                                                                                                 "cm"), legend.key.width = grid::unit(1, "cm"), 
          legend.position = "right", axis.text.x = element_text(size = 8, 
                                                                colour = textcol), axis.text.y = element_text(size = 8, 
                                                                                                              vjust = 0.2, colour = textcol), axis.ticks = element_line(size = 0.4), 
          plot.title = element_text(colour = textcol, hjust = 0, 
                                    size = 12, face = "bold"), strip.text.x = element_text(size = 10), 
          strip.text.y = element_text(angle = 0, size = 10), 
          plot.background = element_blank(), panel.border = element_blank(), 
          strip.background = element_blank(), panel.background = element_rect(fill = "grey80", 
                                                                              colour = "grey80"), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + guides(fill = guide_legend("P-value strength", 
                                                                           ncol = 1)) + labs(x = Exposure, y = Outcome, title = title)
  hm
  return(hm)
}


plotdf <- bind_rows(H1a_res_f,  H1b_res_f,  H2_res_f,  H3_res_f, H4_res_f) %>% droplevels()
plotdf[is.na(plotdf$V),]


plot_emm_heatmap(plotdf)

#save(list=ls(pattern="hm"), file=paste0(here::here(),"/figures/supplementary/emm_heatmaps.Rdata"))