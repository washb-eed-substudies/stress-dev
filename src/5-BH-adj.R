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

H1a_res$H = 1
H1b_res$H = 2
H2_res$H = 3
H3_res$H = 4
H4_res$H = 5

H1a_adj_res$H = 1
H1b_adj_res$H = 2
H2_adj_res$H = 3
H3_adj_res$H = 4
H4_adj_res$H = 5

H1a_adj_emm_res$H = 1
H1b_adj_emm_res$H = 2
H2_adj_emm_res$H = 3
H3_adj_emm_res$H = 4
H4_adj_emm_res$H = 5

full_res <- rbind(H1a_res, H1b_res, H2_res, H3_res, H4_res)

full_adj_res <- rbind(H1a_adj_res, H1b_adj_res, H2_adj_res, H3_adj_res, H4_adj_res)
full_adj_emm_res <- rbind(H1a_adj_emm_res, H1b_adj_emm_res, H2_adj_emm_res, H3_adj_emm_res, H4_adj_emm_res)

full_res <- full_res %>% group_by(Y) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()


full_adj_res <- full_adj_res %>% group_by(Y) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

full_adj_emm_res <- full_adj_emm_res %>% group_by(Y) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

saveRDS(full_res %>% filter(H==1) %>% select(-H), here("results/unadjusted/H1a_res.RDS"))
saveRDS(full_res %>% filter(H==1) %>% select(-H), here("results/unadjusted/H1b_res.RDS"))
saveRDS(full_res %>% filter(H==2) %>% select(-H), here("results/unadjusted/H2_res.RDS"))
saveRDS(full_res %>% filter(H==3) %>% select(-H), here("results/unadjusted/H3_res.RDS"))
saveRDS(full_res %>% filter(H==4) %>% select(-H), here("results/unadjusted/H4_res.RDS"))

saveRDS(full_adj_res %>% filter(H==1) %>% select(-H), here("results/adjusted/H1a_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==1) %>% select(-H), here("results/adjusted/H1b_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==2) %>% select(-H), here("results/adjusted/H2_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==3) %>% select(-H), here("results/adjusted/H3_adj_res.RDS"))
saveRDS(full_adj_res %>% filter(H==4) %>% select(-H), here("results/adjusted/H4_adj_res.RDS"))

saveRDS(full_adj_res %>% filter(H==1) %>% select(-H), here("results/adjusted/H1a_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==1) %>% select(-H), here("results/adjusted/H1b_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==2) %>% select(-H), here("results/adjusted/H2_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==3) %>% select(-H), here("results/adjusted/H3_adj_emm_res.RDS"))
saveRDS(full_adj_res %>% filter(H==4) %>% select(-H), here("results/adjusted/H4_adj_emm_res.RDS"))
