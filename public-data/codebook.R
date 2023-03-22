#Codebook creation
library(haven)
library(sjlabelled)
library(dplyr)
library(labelled)
library(tibble)

#read in public data
d<- read.csv("public-data/wbb_stress-dev_public.csv")

#check for existing labels
get_labels(d)

#add labels

#Create list of varnames

labs= c(
  "Data row",
  "Participant block",
  "Enrollment cluster ID",
  "De-identified participant ID",
  "Child sex",
  "Birth order",
  "Maternal age (years)",
  "Maternal height (cm)",
  "Maternal education",
  "Household food insecurity category",
  "Number of children in the household",
  "Number of children in the compound",
  "Minimum walking distance (minutes) to primary drinking water source",
  "Household wall material",
  "Household floor material",
  "Scaled principal component of household wealth",
  "Center for Epidemiologic Studies Depression sum score (Year 1)",
  "7-day caregiver reported recall of child diarrhea (Year 1)",
  "Treatment arm",
  "Maternal experience of any type of intimate partner violence (Year 2)",
  "Child age at anthropometry assessment (days, Year 2)",
  "Month of anthropometry assessment (Year 2)",
  "7-day caregiver reported recall of child diarrhea (Year 2)",
  "Center for Epidemiologic Studies Depression sum score (Year 2)",
  "Maternal perceived stress scale sum score (Year 2)",
  "Child age at urinalysis assessment (days, Year 1)",
  "Month of urinalysis assessment (Year 1)",
  "Child length for age Z-score (Year 1)",
  "Child weight for age Z-score (Year 1)",
  "Child age at vital signs assessment (days, Year 2)",
  "Month at vital signs assessment (Year 2)",
  "Child age at Salimetrics assessment (days, Year 2)",
  "Month of Salimetrics assessment (Year 2)",
  "Child age at Oragene assessment (days, Year 2)",
  "Month of Oragene assessment (Year 2)",
  "F2 isoprostane iPF(2a)-III (Year 1)",
  "F2 isoprostane 2,3-dinor-iPF(2a)-III (Year 1)",
  "F2 isoprostane iPF(2a)-VI (Year 1)",
  "F2 isoprostane 8,12-iso-iPF(2a)-VI (Year 1)",
  "F2 isoprostane principal component (Year 1)",
  "Salivary cortisol reactivity (slope, Year 2)",
  "Pre-stressor salivary cortisol (Year 2)",
  "Post-stressor salivary cortisol (Year 2)",
  "Salivary alpha-amylase reactivity (slope, Year 2)",
  "Pre-stressor salivary alpha-amylase (Year 2)",
  "Post-stressor salivary alpha-amylase (Year 2)",
  "Mean arterial pressure (Year 2)",
  "Mean heart rate (Year 2)",
  "Mean glucocorticoid receptor methylation (Year 2)",
  "Percentage methylation at NGFI-A transcription factor binding site (CpG site #12, Year 2)",
  "WHO motor milestones sum score (Years 1 and 2)",
  "Communication development inventories expressive communication score (Year 1)",
  "Communication development inventories receptive communication score (Year 1)",
  "Extended ages and stages questionnaire communication score (Year 2)",
  "Extended ages and stages questionnaire motor development score (Year 2)",
  "Extended ages and stages questionnaire personal development score (Year 2)",
  "Extended ages and stages questionnaire combined development score (Year 2)",
  "Communication development inventories expressive communication score (Year 2)",
  "Communication development inventories receptive communication score (Year 2)"
)

#create codebook

var_id <- colnames(d)
dlab <- cbind(var_id, labs)
colnames(dlab) <- c("Variable ID", "Variable Definition")

dlab

write.csv(dlab, "public-data/wbb_stress-dev_codebook.csv")
