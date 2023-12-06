#Cortisol reactivity figure descriptive stats

d <- readRDS('data/stress_dev.RDS')
library(ggplot2)

# Basic histogram
corthist<- ggplot(d, aes(x=t3_cort_slope)) + geom_histogram() + labs(y = "Count", x = "Salivary cortisol reactivity (ug/dl/min)") 
#+ ggtitle("Supplemental Figure X. Distribution of child cortisol reactivity at Year 2") 
corthist

cortdens<- ggplot(d, aes(x=t3_cort_slope)) + geom_density() + labs(y = "Count", x = "Salivary cortisol reactivity (ug/dl/min)") + ggtitle("Supplemental Figure 2. Distribution of child cortisol reactivity at Year 2") 
cortdens

ggsave(cortdens, file = here::here("figures/cort-reactivity-density.png"), height=10, width=10)
