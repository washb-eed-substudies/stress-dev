# WHO Hazard Ratios
rm(list=ls())

source(here::here("0-config.R"))
d <- box_read("880476682582")

#Unadjusted

#Hypothesis 1A

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")    

Yvars <- c("who_sit", "who_crawl", "who_stand_supp",
           "who_walk_supp", "who_stand_nosupp", "who_walk_nosup" )

#Fit models
H1a_who_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    res_unadj <- fit_HR_GAM(d=d, X=i, Y=j, age = "agedays_motor", 
                            W=NULL, 
                            forcedW = NULL,
                            V = NULL,
                            id = "childid",
                            pval = 0.2,
                            print = TRUE)
    res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
    H1a_who_models <- bind_rows(H1a_who_models, res)
  }
}

#Get primary contrasts
H1a_who_res <- NULL
for(i in 1:nrow(H1a_who_models)){
  res <- data.frame(X=H1a_who_models$X[i], Y=H1a_who_models$Y[i])
  preds <- predict_gam_HR(fit=H1a_who_models$fit[i][[1]], d=H1a_who_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_who_res <-  bind_rows(H1a_who_res , preds$res)
}
#Make list of plots
H1a_who_plot_list <- NULL
H1a_who_plot_data <- NULL
for(i in 1:nrow(H1a_who_models)){
  res <- data.frame(X=H1a_who_models$X[i], Y=H1a_who_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_who_models$fit[i][[1]], H1a_who_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_who_plot_list[[i]] <-  simul_plot$p
  H1a_who_plot_data <-  rbind(H1a_who_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

#Save models
saveRDS(H1a_who_models, here("models/H1a_who_models.RDS"))
#Save results
saveRDS(H1a_who_res, here("results/unadjusted/H1a_who_res.RDS"))
#Save plots
#saveRDS(H1a_who_plot_list, here("figure-objects/H1a_who_unadj_splines.RDS"))
#Save plot data
saveRDS(H1a_who_plot_data, here("figure-data/H1a_who_unadj_spline_data.RDS"))

## Adjusted Models


#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth",
         "cesd_sum_t2", "diar7d_t2", "tr", "life_viol_any_t3", "month_at2", "month_ut2", "ageday_ut2", "ageday_at2", "sex")

# --------------------------------------------------------------------------
#### Hypothesis 1a ####
H1a_W_forced <- c("ageday_ut2", "sex")

######Included forced-in covariates for screening as led to error in forcedW command

##########################

##########################

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")    

Yvars <- c("who_sit", "who_crawl", "who_stand_supp",
           "who_walk_supp", "who_stand_nosupp", "who_walk_nosup" )

#Fit models
H1a_who_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_HR_GAM(d=d, X=i, Y=j, 
                          age = "agedays_motor", 
                          W= Wvars, 
                          forcedW = H1a_W_forced,
                          V = NULL,
                          id = "childid",
                          pval = 0.2,
                          print = TRUE)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1a_who_adj_models <- bind_rows(H1a_who_adj_models, res)
  }
}

#Yields Warning message:In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  : Fitting terminated with step failure - check results carefully

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

#Effect Measure Modification

####Effect Measure Modifiers: family care inventory at t2 and t3

V.set.t2 <- c("fci_t2")

#Loop over exposure-outcome pairs

##Hypothesis 1a

#Exposure: Quartiles of F2-isoprostanes isomer score Year 1
#Primary Outcome: Time to WHO Gross motor milestone at Year 1

Xvars <- c("t2_f2_8ip", "t2_f2_23d", "t2_f2_VI", "t2_f2_12i", "t2_f2_iso.pca")            
Yvars <- c("who_sit", "who_crawl", "who_stand_supp",
           "who_walk_supp", "who_stand_nosupp", "who_walk_nosup" )


#Fit models
H1a_who_adj_emm_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    res_adj <- fit_HR_GAM(d=d, X=i, Y=j, 
                          age = "agedays_motor", 
                          W= Wvars, 
                          forcedW = H1a_W_forced,
                          V = V.set.t2,
                          id = "childid",
                          pval = 0.2,
                          print = TRUE)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1a_who_adj_emm_models <- bind_rows(H1a_who_adj_emm_models, res)
  }
}


#Get primary contrasts
H1a_who_adj_emm_res <- NULL
for(i in 1:nrow(H1a_who_adj_emm_models)){
  res <- data.frame(X=H1a_who_adj_emm_models$X[i], Y=H1a_who_adj_emm_models$Y[i])
  preds <- predict_gam_HR(fit=H1a_who_adj_emm_models$fit[i][[1]], d=H1a_who_adj_emm_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1a_who_adj_emm_res <-  bind_rows(H1a_who_adj_emm_res , preds$res)
}

#Make list of plots
H1a_who_adj_emm_plot_list <- NULL
H1a_who_adj_emm_plot_data <- NULL
for(i in 1:nrow(H1a_who_adj_emm_models)){
  res <- data.frame(X=H1a_who_adj_emm_models$X[i], Y=H1a_who_adj_emm_models$Y[i])
  simul_plot <- gam_simul_CI(H1a_who_adj_emm_models$fit[i][[1]], H1a_who_adj_emm_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1a_who_adj_emm_plot_list[[i]] <-  simul_plot$p
  H1a_who_adj_emm_plot_data <-  rbind(H1a_who_adj_emm_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1a_who_adj_emm_models, paste0(dropboxDir,"results/stress-growth-models/models/H1a_who_adj_emm_models.RDS"))

#Save results
saveRDS(H1a_who_adj_emm_res, here("results/adjusted/H1a_who_adj_emm_res.RDS"))



