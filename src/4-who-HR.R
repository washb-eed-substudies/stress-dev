# WHO Hazard Ratios

rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/stress-dev.RDS"))

#Unadjusted

#Hypothesis 1A

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")    

Yvars <- c("sit_nosupp", "crawl_nosupp", "stand_supp",
           "walk_supp", "stand_nosupp", "walk_nosupp" )

?fit_HR_GAM()
#Fit models
H1_who_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_HR_GAM(d=d, X=i, Y=j, age = 'agedays_motor', W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1_who_models <- bind_rows(H1_who_models, res)
  }
}
#Get primary contrasts
H1_who_res <- NULL
for(i in 1:nrow(H1_who_models)){
  res <- data.frame(X=H1_who_models$X[i], Y=H1_who_models$Y[i])
  preds <- predict_gam_HR(fit=H1_who_models$fit[i][[1]], d=H1_who_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_who_res <-  bind_rows(H1_who_res , preds$res)
}
#Make list of plots
H1_who_plot_list <- NULL
H1_who_plot_data <- NULL
for(i in 1:nrow(H1_who_models)){
  res <- data.frame(X=H1_who_models$X[i], Y=H1_who_models$Y[i])
  simul_plot <- gam_simul_CI(H1_who_models$fit[i][[1]], H1_who_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_who_plot_list[[i]] <-  simul_plot$p
  H1_who_plot_data <-  rbind(H1_who_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

#Save models
saveRDS(H1_who_models, here("models/H1_who_models.RDS"))
#Save results
saveRDS(H1_who_res, here("results/unadjusted/H1_who_res.RDS"))
#Save plots
#saveRDS(H1_who_plot_list, here("figure-objects/H1_who_unadj_splines.RDS"))
#Save plot data
saveRDS(H1_who_plot_data, here("figure-data/H1_who_unadj_spline_data.RDS"))

## Adjusted Models


#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth",
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
W2_F2.W23_anthro <- c(Wvars, Wvars2_F2, Wvars2_anthro, Wvars3_anthro)


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

# --------------------------------------------------------------------------
#### Hypothesis 1a ####

H1_W <- c(Wvars)

##########################

##########################

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")              

Yvars <- c('ageday_st1', 'agedays_motor', 
               'month_st1', 'month_mm')

#Fit models
H1a_who_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_HR_GAM(d=d, X=i, Y=j, age = "agedays_motor", W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1a_who_adj_models <- bind_rows(H1a_who_adj_models, res)
  }
}



#Get primary contrasts
H1a_who_adj_res <- NULL
for(i in 1:nrow(H1a_who_adj_models)){
  res <- data.frame(X=H1a_who_adj_models$X[i], Y=H1a_who_adj_models$Y[i])
  preds <- predict_gam_HR(fit=H1a_who_adj_models$fit[i][[1]], d=H1a_who_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_who_adj_res <-  bind_rows(H1a_who_adj_res , preds$res)
}

#Make list of plots
H1a_who_adj_plot_list <- NULL
H1a_who_adj_plot_data <- NULL
for(i in 1:nrow(H1a_who_adj_models)){
  res <- data.frame(X=H1a_who_adj_models$X[i], Y=H1a_who_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_who_adj_models$fit[i][[1]], H1a_who_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_who_adj_plot_list[[i]] <-  simul_plot$p
  H1a_who_adj_plot_data <-  rbind(H1a_who_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1a_who_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1a_who_adj_models.RDS"))

#Save results
saveRDS(H1a_who_adj_res, here("results/adjusted/H1a_who_adj_res.RDS"))


