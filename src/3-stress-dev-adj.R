rm(list=ls())

source(here::here("0-config.R"))

d <- readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/stress-dev.RDS"))

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



#Loop over exposure-outcome pairs

##Hypothesis 1a

#Exposure: Quartiles of F2-isoprostanes isomer score Year 1
#Primary Outcome: WHO Gross motor and CDI score at Year 1

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")            
Yvars <- c("sum_who_t2_t3", 
           "z_cdi_say_t2", "z_cdi_und_t2") 

#Fit models
H1a_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1a_adj_models <- bind_rows(H1a_adj_models, res)
  }
}



#Get primary contrasts
H1a_adj_res <- NULL
for(i in 1:nrow(H1a_adj_models)){
  res <- data.frame(X=H1a_adj_models$X[i], Y=H1a_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1a_adj_models$fit[i][[1]], d=H1a_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_adj_res <-  bind_rows(H1a_adj_res , preds$res)
}

#Make list of plots
H1a_adj_plot_list <- NULL
H1a_adj_plot_data <- NULL
for(i in 1:nrow(H1a_adj_models)){
  res <- data.frame(X=H1a_adj_models$X[i], Y=H1a_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_adj_models$fit[i][[1]], H1a_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_adj_plot_list[[i]] <-  simul_plot$p
  H1a_adj_plot_data <-  rbind(H1a_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1a_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1a_adj_models.RDS"))

#Save results
saveRDS(H1a_adj_res, here("results/adjusted/H1a_adj_res.RDS"))


#Save plots
#saveRDS(H1a_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1a_adj_splines.RDS"))

#Save plot data
saveRDS(H1a_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1a_adj_spline_data.RDS"))


######H1b
#Exposure: Quartiles of F2-isoprostanes isomer score Year 1
#Primary Outcome: EASQ and CDI score at Year 2

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")            
Yvars <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

#Fit models
H1b_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1b_adj_models <- bind_rows(H1b_adj_models, res)
  }
}



#Get primary contrasts
H1b_adj_res <- NULL
for(i in 1:nrow(H1b_adj_models)){
  res <- data.frame(X=H1b_adj_models$X[i], Y=H1b_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1b_adj_models$fit[i][[1]], d=H1b_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1b_adj_res <-  bind_rows(H1b_adj_res , preds$res)
}

#Make list of plots
H1b_adj_plot_list <- NULL
H1b_adj_plot_data <- NULL
for(i in 1:nrow(H1b_adj_models)){
  res <- data.frame(X=H1b_adj_models$X[i], Y=H1b_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1b_adj_models$fit[i][[1]], H1b_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1b_adj_plot_list[[i]] <-  simul_plot$p
  H1b_adj_plot_data <-  rbind(H1b_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1b_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1b_adj_models.RDS"))

#Save results
saveRDS(H1b_adj_res, here("results/adjusted/H1b_adj_res.RDS"))


#Save plots
#saveRDS(H1b_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1b_adj_splines.RDS"))

#Save plot data
saveRDS(H1b_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H1b_adj_spline_data.RDS"))

## Hypothesis 2

Xvars <- c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02" )  

Yvars <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
           "z_cdi_say_t3", "z_cdi_und_t3") 


#Fit models
H2_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#Get primary contrasts
H2_adj_res <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_adj_res <-  bind_rows(H2_adj_res , preds$res)
}

#Make list of plots
H2_adj_plot_list <- NULL
H2_adj_plot_data <- NULL
for(i in 1:nrow(H2_adj_models)){
  print(i)
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2_adj_models$fit[i][[1]], H2_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_adj_plot_list[[i]] <-  simul_plot$p
  H2_adj_plot_data <-  rbind(H2_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H2_adj_models.RDS"))

#Save results
saveRDS(H2_adj_res, here("results/adjusted/H2_adj_res.RDS"))


#Save plots
#saveRDS(H2_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2_adj_adj_splines.RDS"))

#Save plot data
saveRDS(H2_adj_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H2_adj_adj_spline_data.RDS"))



##Hypothesis 3
# Mean arterial pressure and heart rate at year 2 v. development at year 2

Xvars <- c("t3_map", "t3_hr_mean")            
Yvars <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
           "z_cdi_say_t3", "z_cdi_und_t3") 



#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_models <- bind_rows(H3_models, res)
  }
}

#Get primary contrasts
H3_res <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_res <-  bind_rows(H3_res , preds$res)
}

#Make list of plots
H3_plot_list <- NULL
H3_plot_data <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_plot_list[[i]] <-  simul_plot$p
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H3_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H3_models.RDS"))

#Save results
saveRDS(H3_res, here("results/adjusted/H3_adj_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H3_adj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H3_adj_spline_data.RDS"))


##Hypothesis 4
#Glucocortoic receptor methylation year 2 v. development at year 2

Xvars <- c("t3_gcr_mean", "t3_gcr_cpg12")            
Yvars <- c("z_comm_easq_t3", "z_motor_easq_t3", "z_personal_easq_t3", "z_combined_easq_t3", 
           "z_cdi_say_t3", "z_cdi_und_t3") 


#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(i, j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_models <- bind_rows(H4_models, res)
  }
}

#Get primary contrasts
H4_res <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  preds <- predict_gam_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_res <-  bind_rows(H4_res , preds$res)
}

#Make list of plots
H4_plot_list <- NULL
H4_plot_data <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  simul_plot <- gam_simul_CI(H4_models$fit[i][[1]], H4_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_plot_list[[i]] <-  simul_plot$p
  H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H4_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H4_models.RDS"))

#Save results
saveRDS(H4_res, here("results/adjusted/H4_adj_res.RDS"))


#Save plots
#saveRDS(H4_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H4_adj_splines.RDS"))

#Save plot data
saveRDS(H4_plot_data, paste0(dropboxDir,"results/stress-growth-models/figure-data/H4_adj_spline_data.RDS"))
