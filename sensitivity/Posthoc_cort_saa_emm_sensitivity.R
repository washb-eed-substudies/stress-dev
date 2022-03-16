#Posthoc exploratory sensitivity analysis: cortisol*saa interaction

#Covariates and initialization

rm(list=ls())

source(here::here("0-config.R"))

d <- box_read("880476682582")

#Simple linear model

test_fci_int_adj <- lm(z_cdi_und_t3 ~ t3_cort_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3 + fci_t2 + fci_t2*t3_cort_slope, data = d)

#GAM Model

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "cesd_sum_t2", "diar7d_t2", "tr", "life_viol_any_t3")

Wvars[!(Wvars %in% colnames(d))]



#Add in time varying covariates:

Wvars2_anthro<-c("ageday_at2", "month_at2")
Wvars3_anthro<-c("ageday_at3", "month_at3", "diar7d_t3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "life_viol_any_t3")  

Wvars2_F2<-c("ageday_ut2", "month_ut2") 
Wvars3_vital<-c("laz_t2", "waz_t2", "ageday_t3_vital", "month_vt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_salimetrics<-c("laz_t2", "waz_t2", "ageday_t3_salimetrics", "month_lt3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 
Wvars3_oragene<-c("laz_t2", "waz_t2", "ageday_t3_oragene", "month_ot3", "cesd_sum_ee_t3", "pss_sum_mom_t3", "diar7d_t3", "life_viol_any_t3") 



W2_F2.W2_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro) %>% unique(.)
W2_F2.W3_anthro <- c(Wvars, Wvars2_F2, Wvars3_anthro, 
                     "laz_t2", "waz_t2") %>% unique(.)
W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro) %>% unique(.)


W3_vital.W3_anthro <- c(Wvars, Wvars3_vital, Wvars3_anthro) %>% unique(.)
W3_salimetrics.W3_anthro <- c(Wvars, Wvars3_salimetrics, Wvars3_anthro) %>% unique(.)
W3_oragene.W3_anthro <- c(Wvars, Wvars3_oragene, Wvars3_anthro) %>% unique(.)


#pick covariates function

pick_covariates <- function(i, j){
  # i is exposure as string
  # j is outcome as string
  # choose correct/build correct adjustment set based on exposure and outcome
  if(grepl("t2_f2", i)){
    if(grepl("_t2_t3", j)){Wset = W2_F2.W23_anthro}
    else if(grepl("_t2", j)){Wset = W2_F2.W2_anthro}
    else if(grepl("_who", j)){Wset = W2_F2.W2_anthro}
    else if(grepl("_t3", j)){Wset = W2_F2.W3_anthro}
    else if(grepl("_t3", j)){Wset = W2_F2.W3_anthro}}
  else if(grepl("slope", i)){Wset = c(W3_salimetrics.W3_anthro, "t3_col_time_z01_cont")}
  else if(grepl("residual", i)){Wset = W3_salimetrics.W3_anthro}
  else if(i %in% c("t3_map", "t3_hr_mean")){Wset = W3_vital.W3_anthro}
  else{Wset = W3_oragene.W3_anthro}
  
  if(j=="hcz_t3"){Wset=c(Wset, "hcz_t2")}
  return(Wset)
}

#Forced in Covariates

#Make vectors of adjustment variable names
Wvars_forced<-c("sex")

Wvars_forced[!(Wvars_forced %in% colnames(d))]



#Add in time varying covariates:

Wvars2_anthro_forced<-c("ageday_at2")
Wvars3_anthro_forced<-c("ageday_at3")  

Wvars2_F2_forced<-c("ageday_ut2") 
Wvars3_vital_forced<-c("ageday_t3_vital") 
Wvars3_salimetrics_forced<-c("ageday_t3_salimetrics") 
Wvars3_oragene_forced<-c("ageday_t3_oragene") 



W2_F2.W2_anthro_forced <- c(Wvars_forced, Wvars2_F2_forced, Wvars2_anthro_forced) %>% unique(.)
W2_F2.W3_anthro_forced <- c(Wvars_forced, Wvars2_F2_forced, Wvars3_anthro_forced) %>% unique(.)
W2_F2.W23_anthro_forced <- c(Wvars_forced, Wvars2_F2_forced, Wvars2_anthro_forced, Wvars3_anthro_forced) %>% unique(.)


W3_vital.W3_anthro_forced <- c(Wvars_forced, Wvars3_vital_forced, Wvars3_anthro_forced) %>% unique(.)
W3_salimetrics.W3_anthro_forced <- c(Wvars_forced, Wvars3_salimetrics_forced, Wvars3_anthro_forced) %>% unique(.)
W3_oragene.W3_anthro_forced <- c(Wvars_forced, Wvars3_oragene_forced, Wvars3_anthro_forced) %>% unique(.)

###

pick_covariates_forced <- function(i, j){
  # i is exposure as string
  # j is outcome as string
  # choose correct/build correct adjustment set based on exposure and outcome
  if(grepl("t2_f2", i)){
    if(grepl("_t2_t3", j)){Wset_forced = W2_F2.W23_anthro_forced}
    else if(grepl("_t2", j)){Wset_forced = W2_F2.W2_anthro_forced}
    else if(grepl("_who", j)){Wset_forced = W2_F2.W2_anthro_forced}
    else if(grepl("_t3", j)){Wset_forced = W2_F2.W3_anthro_forced}
    else if(grepl("_t3", j)){Wset_forced = W2_F2.W3_anthro_forced}}
  else if(grepl("slope", i)){Wset_forced = W3_salimetrics.W3_anthro_forced}
  else if(grepl("residual", i)){Wset_forced = W3_salimetrics.W3_anthro_forced}
  else if(i %in% c("t3_map", "t3_hr_mean")){Wset_forced = W3_vital.W3_anthro_forced}
  else{Wset_forced = W3_oragene.W3_anthro_forced}
  
  if(j=="hcz_t3"){Wset_forced=c(Wset_forced)}
  return(Wset_forced)
}

V.set.cort <- c("t3_saa_slope")
V.set.saa <- c("t3_cort_slope")


#Simple linear model

#EASQ Comm
posthoc_commeasq_lm_cort <- lm(z_comm_easq_t3 ~ t3_cort_slope + t3_saa_slope +  t3_cort_slope*t3_saa_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3, data = d)

summary(posthoc_commeasq_lm_cort)

#EASQ Motor
posthoc_z_motor_easq_t3_lm_cort <- lm(z_motor_easq_t3 ~ t3_cort_slope + t3_saa_slope +  t3_cort_slope*t3_saa_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3, data = d)

summary(posthoc_z_motor_easq_t3_lm_cort)

#EASQ Personal Social
posthoc_z_personal_easq_t3_lm_cort <- lm(z_personal_easq_t3 ~ t3_cort_slope + t3_saa_slope +  t3_cort_slope*t3_saa_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3, data = d)

summary(posthoc_z_personal_easq_t3_lm_cort)

#Combined EASQ
posthoc_z_combined_easq_t3_lm_cort <- lm(z_combined_easq_t3 ~ t3_cort_slope + t3_saa_slope +  t3_cort_slope*t3_saa_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3, data = d)

summary(posthoc_z_combined_easq_t3_lm_cort)

#CDI Expressive
posthoc_z_cdi_say_t3_lm_cort <- lm(z_cdi_say_t3 ~ t3_cort_slope + t3_saa_slope +  t3_cort_slope*t3_saa_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3, data = d)

summary(posthoc_z_cdi_say_t3_lm_cort)

#CDI Comprehensive
posthoc_z_cdi_und_t3_lm_cort <- lm(z_cdi_und_t3 ~ t3_cort_slope + t3_saa_slope +  t3_cort_slope*t3_saa_slope + sex + birthord + momage + momheight + momedu + hfiacat + Nlt18 + Ncomp + watmin + walls + floor + HHwealth_scaled + cesd_sum_t2 + diar7d_t2 + tr + life_viol_any_t3, data = d)

summary(posthoc_z_cdi_und_t3_lm_cort)

## Hypothesis 2 - sAA

Xvars <- c("t3_saa_slope" )  

Yvars <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
           "z_cdi_say_t3", "z_cdi_und_t3") 


#Fit models
H2_posthoc_saa_int_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    for (k in V.set.cort){
      print(i)
      print(j)
      print(k)
      Wset<-pick_covariates(i, j)
      Wset_forced <- pick_covariates_forced(i, j)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset, forcedW = Wset_forced,  V = k)
      res <- data.frame(X=i, Y=j, V = k, int.p = res_adj$int.p, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      H2_posthoc_saa_int_models <- bind_rows(H2_posthoc_saa_int_models, res)
    }
  }
}

#Get primary contrasts
H2_posthoc_saa_int_res <- NULL
for(i in 1:nrow(H2_posthoc_saa_int_models)){
  res <- data.frame(V=H2_posthoc_saa_int_models$V[i], X=H2_posthoc_saa_int_models$X[i], Y=H2_posthoc_saa_int_models$Y[i])
  preds <- predict_gam_emm(fit=H2_posthoc_saa_int_models$fit[i][[1]], d=H2_posthoc_saa_int_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  preds <- preds$res
  preds$V <- res$V
  preds$int.p <- H2_posthoc_saa_int_models$int.p[i]
  H2_posthoc_saa_int_res <-  bind_rows(H2_posthoc_saa_int_res , preds)
  
}

#Make list of plots
#H2_posthoc_saa_int_plot_list <- NULL
#H2_posthoc_saa_int_plot_data <- NULL
#for(i in 1:nrow(H2_posthoc_saa_int_models)){
#   print(i)
#   res <- data.frame(X=H2_posthoc_saa_int_models$X[i], Y=H2_posthoc_saa_int_models$Y[i])
#   simul_plot <- gam_simul_CI(H2_posthoc_saa_int_models$fit[i][[1]], H2_posthoc_saa_int_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
#   H2_posthoc_saa_int_plot_list[[i]] <-  simul_plot$p
#   H2_posthoc_saa_int_plot_data <-  rbind(H2_posthoc_saa_int_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
# }


#Save models
#saveRDS(H2_posthoc_saa_int_models, paste0(dropboxDir,"results/stress-dev-models/models/adj_H2_posthoc_saa_int_models.RDS"))

#Save results
#saveRDS(H2_posthoc_saa_int_res, here("results/adjusted/H2_posthoc_saa_int_res.RDS"))


#Save plots
#saveRDS(H2_posthoc_saa_int_plot_list, paste0(dropboxDir,"results/stress-dev-models/figure-objects/H2_posthoc_saa_int_splines.RDS"))

#Save plot data
#saveRDS(H2_posthoc_saa_int_plot_data, paste0(dropboxDir,"results/stress-dev-models/figure-data/H2_posthoc_saa_int_spline_data.RDS"))

## Hypothesis 2 - cortisol

Xvars <- c("t3_cort_slope")  

Yvars <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
           "z_cdi_say_t3", "z_cdi_und_t3") 


#Fit models
H2_posthoc_cort_int_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    for (k in V.set.saa){
      print(i)
      print(j)
      print(k)
      Wset<-pick_covariates(i, j)
      Wset_forced <- pick_covariates_forced(i, j)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset, forcedW = Wset_forced,  V = k)
      res <- data.frame(X=i, Y=j, V = k, int.p = res_adj$int.p, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      H2_posthoc_cort_int_models <- bind_rows(H2_posthoc_cort_int_models, res)
    }
  }
}

#Get primary contrasts
H2_posthoc_cort_int_res <- NULL
for(i in 1:nrow(H2_posthoc_cort_int_models)){
  res <- data.frame(V=H2_posthoc_cort_int_models$V[i], X=H2_posthoc_cort_int_models$X[i], Y=H2_posthoc_cort_int_models$Y[i])
  preds <- predict_gam_emm(fit=H2_posthoc_cort_int_models$fit[i][[1]], d=H2_posthoc_cort_int_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  preds <- preds$res
  preds$V <- res$V
  preds$int.p <- H2_posthoc_cort_int_models$int.p[i]
  H2_posthoc_cort_int_res <-  bind_rows(H2_posthoc_cort_int_res , preds)
  
}

#Make list of plots
#H2_posthoc_cort_int_plot_list <- NULL
#H2_posthoc_cort_int_plot_data <- NULL
#for(i in 1:nrow(H2_posthoc_cort_int_models)){
#   print(i)
#   res <- data.frame(X=H2_posthoc_cort_int_models$X[i], Y=H2_posthoc_cort_int_models$Y[i])
#   simul_plot <- gam_simul_CI(H2_posthoc_cort_int_models$fit[i][[1]], H2_posthoc_cort_int_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
#   H2_posthoc_cort_int_plot_list[[i]] <-  simul_plot$p
#   H2_posthoc_cort_int_plot_data <-  rbind(H2_posthoc_cort_int_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
# }


#Save models
#saveRDS(H2_posthoc_cort_int_models, paste0(dropboxDir,"results/stress-dev-models/models/adj_H2_posthoc_cort_int_models.RDS"))

#Save results
#saveRDS(H2_posthoc_cort_int_res, here("results/adjusted/H2_posthoc_cort_int_res.RDS"))


#Save plots
#saveRDS(H2_posthoc_cort_int_plot_list, paste0(dropboxDir,"results/stress-dev-models/figure-objects/H2_posthoc_cort_int_splines.RDS"))

#Save plot data
#saveRDS(H2_posthoc_cort_int_plot_data, paste0(dropboxDir,"results/stress-dev-models/figure-data/H2_posthoc_cort_int_spline_data.RDS"))


