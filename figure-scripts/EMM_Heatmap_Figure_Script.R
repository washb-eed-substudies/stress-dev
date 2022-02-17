# EMM Heatmap figures 
#Code from Caitlin

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

save <- NULL
for(i in 1:nrow(igf.models)){
  preds <- predict_gam_emm(fit=igf.models$fit[i][[1]], d=igf.models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=igf.models$X[i], Yvar=igf.models$Y[i])
  gamm_diff_res <- data.frame(V=igf.models$V[i], preds$res) %>% mutate(int.Pval = c(NA, igf.models$int.p[[i]]))
  save <-  bind_rows(save, gamm_diff_res)
}

save$igf <- rep(c("25th","75th"), nrow(save)/2)
save$intp <- cut(save$int.Pval, breaks = c(-0.1, 0.05, 0.1, 0.2, 1), labels = c("***", "**", "*", ""))
save.long <- save %>%
  select(igf, X, Y, V, pred.q1, pred.q3, intp) %>%
  fill(intp, .direction = "up") %>%
  melt(id.vars = c("igf", "X", "Y", "V", "intp")) %>%
  mutate(X.quart = ifelse(variable == "pred.q1", "25th", "75th"),
         outcome = ifelse(Y %in% c(grep("hcz", adj_res$Y, value = T), "hc_velocity_t2_t3"), 
                          "Head\ncircumference", 
                          ifelse(Y %in% c(grep("laz", adj_res$Y, value = T), "len_velocity_t2_t3"), "Length",
                                 ifelse(Y %in% c(grep("waz", adj_res$Y, value = T), "wei_velocity_t2_t3"), "Weight",
                                        ifelse(Y %in% grep("whz", adj_res$Y, value = T), "Weight for length", NA)))),
         #X = factor(X, levels = c("t2_ln_crp", "t3_ln_crp", "t2_ln_agp", "t3_ln_agp"),
         #          labels = c("CRP at 14mo", "CRP at 28mo", "AGP at 14mo", "AGP at 28mo")),
         marker = ifelse(X %in% grep("crp", X, value = T), "CRP", "AGP"),
         time = ifelse(X %in% grep("t2", X, value = T), "14mo", "28mo"),
         label = paste(round(value,2), intp, sep = ""),
         cross= ifelse(X %in% grep("t2", X, value = T) & Y %in% grep("z_t3", adj_res$Y, value = T), "Subsequent", "Concurrent"),
         time = ifelse(cross == "Subsequent", "", time))