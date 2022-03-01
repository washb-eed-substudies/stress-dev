#Sensitivity Analysis Exploration

rm(list=ls())

source(here::here("0-config.R"))

d <- box_read("880476682582")

test_fci_adj <- lm(z_cdi_und_t3 ~ t3_cort_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3 + fci_t2, data = d)
test_nofci_adj <- lm(z_cdi_und_t3 ~ t3_cort_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3,  data = d)
test_fci_int_adj <- lm(z_cdi_und_t3 ~ t3_cort_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3 + fci_t2 + fci_t2*t3_cort_slope, data = d)
summary(test_nofci_adj)
summary(test_fci_adj)
summary(test_fci_int_adj)

# time varying covariates "laz_t2", "waz_t2", "ageday_t3_salimetrics", "month_lt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3"
