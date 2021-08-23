#### Adjust all pvalues with BH procedure ####
rm(list=ls())

source(here::here("0-config.R"))
  
# load all results
H1a_res <- readRDS(here('results/unadjusted/H1a_res.RDS'))
H1b_res <- readRDS(here('results/unadjusted/H1b_res.RDS'))
H2_res <- readRDS(here('results/unadjusted/H2_res.RDS'))
H3_res <- readRDS(here('results/unadjusted/H3_res.RDS'))
H4_res <- readRDS(here('results/unadjusted/H4_res.RDS'))

H1a_adj_res <- readRDS(here('results/adjusted/H1a_adj_res.RDS'))
H1b_adj_res <- readRDS(here('results/adjusted/H1b_adj_res.RDS'))
H2_adj_res <- readRDS(here('results/adjusted/H2_adj_res.RDS'))
H3_adj_res <- readRDS(here('results/adjusted/H3_adj_res.RDS'))
H4_adj_res <- readRDS(here('results/adjusted/H4_adj_res.RDS'))

H1a_adj_emm_res <- readRDS(here('results/adjusted/H1a_adj_emm_res.RDS'))
H1b_adj_emm_res <- readRDS(here('results/adjusted/H1b_adj_emm_res.RDS'))
H2_adj_emm_res <- readRDS(here('results/adjusted/H2_adj_emm_res.RDS'))
H3_adj_emm_res <- readRDS(here('results/adjusted/H3_adj_emm_res.RDS'))
H4_adj_emm_res <- readRDS(here('results/adjusted/H4_adj_emm_res.RDS'))

#WHO Hazard Ratios

H1a_who <- readRDS(here('results/unadjusted/H1_who_res.RDS'))
H1a_who_adj <- readRDS(here('results/adjusted/H1a_who_adj_res.RDS'))
H1a_who_adj_emm <- readRDS(here('results/adjusted/H1a_who_adj_emm_res.RDS'))

H1a_res$H = 1
H1a_res$subH = "a"
H1b_res$H = 1
H1b_res$subH = "b"
H1a_who$H = 1
H1a_who$subH = "c"
H2_res$H = 2
H3_res$H = 3
H4_res$H = 4


H1a_adj_res$H = 1
H1a_adj_res$subH = "a"
H1b_adj_res$H = 1
H1b_adj_res$subH = "b"
H1a_who_adj$H = 1
H1a_who$subH = "c"
H2_adj_res$H = 2
H3_adj_res$H = 3
H4_adj_res$H = 4

H1a_adj_emm_res$H = 1
H1a_adj_emm_res$subH = "a"
H1b_adj_emm_res$H = 1
H1b_adj_emm_res$subH = "b"
H1a_who_adj_emm$H = 1
H1a_who_adj_emm$subH = "c"
H2_adj_emm_res$H = 2
H3_adj_emm_res$H = 3
H4_adj_emm_res$H = 4

full_res <- bind_rows(H1a_res, H1b_res, H2_res, H3_res, H4_res)

full_adj_res <- bind_rows(H1a_adj_res, H1b_adj_res, H2_adj_res, H3_adj_res, H4_adj_res)

full_adj_emm_res <- bind_rows(H1a_adj_emm_res, H1b_adj_emm_res, H2_adj_emm_res, H3_adj_emm_res, H4_adj_emm_res)

full_res <- full_res %>% group_by(H) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()


full_adj_res <- full_adj_res %>% group_by(H) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

full_adj_emm_res <- full_adj_emm_res %>% group_by(H) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

saveRDS(full_res %>% filter(H==1, subH=="a") %>% select(-c(H, subH)), here("results/unadjusted/H1a_res.RDS"))
saveRDS(full_res %>% filter(H==1, subH=="b") %>% select(-c(H,subH)), here("results/unadjusted/H1b_res.RDS"))
saveRDS(full_res %>% filter(H==2) %>% select(-H), here("results/unadjusted/H2_res.RDS"))
saveRDS(full_res %>% filter(H==3) %>% select(-H), here("results/unadjusted/H3_res.RDS"))
saveRDS(full_res %>% filter(H==4) %>% select(-H), here("results/unadjusted/H4_res.RDS"))

saveRDS(full_adj_res %>% filter(H==1, subH=="a") %>% select(-c(H, subH)), here("results/adjusted/H1a_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==1, subH=="b") %>% select(-c(H,subH)), here("results/adjusted/H1b_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==2) %>% select(-H), here("results/adjusted/H2_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==3) %>% select(-H), here("results/adjusted/H3_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==4) %>% select(-H), here("results/adjusted/H4_adj_res.RDS"))

saveRDS(full_adj_res %>% filter(H==1, subH=="a") %>% select(-c(H, subH)), here("results/adjusted/H1a_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==1, subH=="b") %>% select(-c(H,subH)), here("results/adjusted/H1b_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==2) %>% select(-H), here("results/adjusted/H2_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==3) %>% select(-H), here("results/adjusted/H3_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==4) %>% select(-H), here("results/adjusted/H4_adj_emm_res.RDS"))
