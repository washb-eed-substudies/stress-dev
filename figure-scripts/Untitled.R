#Figure scripts

source(here::here("0-config.R"))
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

d_for_plot <- function(x_name, y_name, x_var, y_var, spline, quart){
  d <- NULL
  for (i in 1: length(x_var)) {
    for (j in 1:length(y_var)){
      exists <- (quart%>%filter(X==x_var[i], Y==y_var[j]) %>% nrow()) != 0
      if (exists){
        new <- data.frame(x=x_name[i], y=y_name[j], spline%>%filter(Xvar==x_var[i], Yvar==y_var[j]), quart%>%filter(X==x_var[i], Y==y_var[j])%>%select(q1, q3))
        d <- rbind(d, new)
      }
    }
  }
  d
}

color_levels <- c("Sum of WHO motor milestones", "Comprehension", "Expressive language",
                  "Communication", "Gross motor", "Personal social", "Combined")

