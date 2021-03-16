
rm(list=ls())

source(here::here("0-config.R"))
#source(here::here("src/0-gam-functions.R"))

d <- readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/stress-dev.RDS"))


#Example:

#Fit GAM model with random effects for childid
#res_unadj <- fit_RE_gam(d=d, X="t3_cort_z01", Y="laz_t3",  W=NULL)

#Get predictions of differences from the 25th percentile of exposure
#preds_unadj <- predict_gam_diff(fit=res_unadj$fit, d=res_unadj$dat, quantile_diff=c(0.25,0.75), Xvar="delta_TS", Yvar="laz_t3")


#Primary parameter we are estimating: difference between 25th and 75th percentile of the exposure
#preds_unadj$res

#Plot the difference from the 25th percentile for the full range of the exposure:
#NOTE: not making these plots anymore, just using for diagnostics
#p <- plot_gam_diff(preds_unadj$plotdf)
#print(p)

#Fit spline with simultaneous confidence intervals
#simul_plot <- gam_simul_CI(res_unadj$fit, res_unadj$dat, xlab="delta_TS", ylab="laz_t3", title="example title")
#simul_plot$p


#Loop over exposure-outcome pairs
#### Hypothesis 1a ####
# Exposure: F2 Isoprostenes
#Outcome: Child dev at year 1
Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")             
Yvars <- c("sum_who", 
           "z_cdi_say_t2", "z_cdi_und_t2") 

#Fit models
H1a_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1a_models <- bind_rows(H1a_models, res)
  }
}

#Get primary contrasts
H1a_res <- NULL
for(i in 1:nrow(H1a_models)){
  res <- data.frame(X=H1a_models$X[i], Y=H1a_models$Y[i])
  preds <- predict_gam_diff(fit=H1a_models$fit[i][[1]], d=H1a_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_res <-  bind_rows(H1a_res , preds$res)
}

#Make list of plots
H1a_plot_list <- NULL
H1a_plot_data <- NULL
for(i in 1:nrow(H1a_models)){
  res <- data.frame(X=H1a_models$X[i], Y=H1a_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_models$fit[i][[1]], H1a_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_plot_list[[i]] <-  simul_plot$p
  H1a_plot_data <-  rbind(H1a_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H1a_models, here("models/H1a_models.RDS"))

#Save results
saveRDS(H1a_res, here("results/unadjusted/H1a_res.RDS"))


#Save plots
#saveRDS(H1a_plot_list, here("figure-objects/H1a_unadj_splines.RDS"))

#Save plot data
saveRDS(H1a_plot_data, here("figure-data/H1a_unadj_spline_data.RDS"))

#### Hypothesis 1b ####
# Exposure: F2 Isoprostenes
#Outcome: Child dev at year 2
Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")           
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

#Fit models
H1b_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1b_models <- bind_rows(H1b_models, res)
  }
}

#Get primary contrasts
H1b_res <- NULL
for(i in 1:nrow(H1b_models)){
  res <- data.frame(X=H1b_models$X[i], Y=H1b_models$Y[i])
  preds <- predict_gam_diff(fit=H1b_models$fit[i][[1]], d=H1b_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1b_res <-  bind_rows(H1b_res , preds$res)
}

#Make list of plots
H1b_plot_list <- NULL
H1b_plot_data <- NULL
for(i in 1:nrow(H1b_models)){
  res <- data.frame(X=H1b_models$X[i], Y=H1b_models$Y[i])
  simul_plot <- gam_simul_CI(H1b_models$fit[i][[1]], H1b_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1b_plot_list[[i]] <-  simul_plot$p
  H1b_plot_data <-  rbind(H1b_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

summary(d$sum_who)
d$who
#Save models
saveRDS(H1b_models, here("models/H1b_models.RDS"))

#Save results
saveRDS(H1b_res, here("results/unadjusted/H1b_res.RDS"))


#Save plots
#saveRDS(H1b_plot_list, here("figure-objects/H1b_unadj_splines.RDS"))

#Save plot data
saveRDS(H1b_plot_data, here("figure-data/H1b_unadj_spline_data.RDS"))


#### Hypothesis 2 ####
# Cortisol and sAA at  Year 2 and child development at Year 2
Xvars <- c("t3_cort_slope", "t3_cort_z01", "t3_cort_z03", "t3_saa_slope", "t3_saa_z01", "t3_saa_z02" )            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

#Fit models
H2_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H2_models <- bind_rows(H2_models, res)
  }
}

#Get primary contrasts
H2_res <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  preds <- predict_gam_diff(fit=H2_models$fit[i][[1]], d=H2_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_res <-  bind_rows(H2_res , preds$res)
}

#Make list of plots
H2_plot_list <- NULL
H2_plot_data <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_plot_list[[i]] <-  simul_plot$p
  H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H2_models, here("models/H2_models.RDS"))

#Save results
saveRDS(H2_res, here("results/unadjusted/H2_res.RDS"))


#Save plots
#saveRDS(H2_plot_list, here("figure-objects/H2_unadj_splines.RDS"))

#Save plot data
saveRDS(H2_plot_data, here("figure-data/H2_unadj_spline_data.RDS"))


#### Hypothesis 3 ####
# Mean arterial pressure and heart rate at year 2 v. development at year 2
Xvars <- c("t3_map", "t3_hr_mean")            
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

d$t3_ma
#Fit models
H3_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
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
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H3_models, here("models/H3_models.RDS"))

#Save results
saveRDS(H3_res, here("results/unadjusted/H3_res.RDS"))


#Save plots
#saveRDS(H3_plot_list, here("figure-objects/H3_unadj_splines.RDS"))

#Save plot data
saveRDS(H3_plot_data, here("figure-data/H3_unadj_spline_data.RDS"))



#### Hypothesis 4 ####
#Glucocortoic receptor methylation year 2 v. development at year 2
Xvars <- c("t3_gcr_mean", "t3_gcr_cpg12")             
Yvars <- c("z_comm_easq", "z_motor_easq", "z_personal_easq", "z_combined_easq", 
           "z_cdi_say_t3", "z_cdi_und_t3") 

#Fit models
H4_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
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
  H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Save models
saveRDS(H4_models, here("models/H4_models.RDS"))

#Save results
saveRDS(H4_res, here("results/unadjusted/H4_res.RDS"))


#Save plots
#saveRDS(H4_plot_list, here("figure-objects/H4_unadj_splines.RDS"))

#Save plot data
saveRDS(H4_plot_data, here("figure-data/H4_unadj_spline_data.RDS"))

