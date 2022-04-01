# F2 Isoprostanes Exploration
rm(list=ls())

source(here::here("0-config.R"))
d <- box_read("880476682582")

#Tables of F2-Iso by laz categories
d %>%
  group_by(laz_t2_cat) %>%
  summarise_at(vars(t2_iso_pca), list(name = mean), na.rm = TRUE)

d %>%
  group_by(laz_t2_cat) %>%
  summarise_at(vars(t2_f2_8ip), list(name = mean), na.rm = TRUE)

d %>%
  group_by(laz_t2_cat) %>%
  summarise_at(vars(t2_f2_12i), list(name = mean), na.rm = TRUE)

d %>%
  group_by(laz_t2_cat) %>%
  summarise_at(vars(t2_f2_23d), list(name = mean), na.rm = TRUE)

d %>%
  group_by(laz_t2_cat) %>%
  summarise_at(vars(t2_f2_VI), list(name = mean), na.rm = TRUE)

###SUMMARY: STUNTED KIDS HAVE HIGHER F2 ISO

#F2 iso and sex

d %>%
  group_by(sex) %>%
  summarise_at(vars(t2_iso_pca), list(name = mean), na.rm = TRUE)

d %>%
  group_by(sex) %>%
  summarise_at(vars(t2_f2_8ip), list(name = mean), na.rm = TRUE)

d %>%
  group_by(sex) %>%
  summarise_at(vars(t2_f2_12i), list(name = mean), na.rm = TRUE)

d %>%
  group_by(sex) %>%
  summarise_at(vars(t2_f2_23d), list(name = mean), na.rm = TRUE)

d %>%
  group_by(sex) %>%
  summarise_at(vars(t2_f2_VI), list(name = mean), na.rm = TRUE)

#SUMMARY: ALL F2 ISO ARE HIGHER IN GIRLS

#F2 iso and age
#measure of age: age at Y1 urinalysis = ageday_ut2
summary(d$ageday_ut2)
d$age
#Create binary age, greater (=1) or less than (=0) median of 429 days
d$ageday_ut2_binary <- ifelse(d$ageday_ut2 > 429, 1, 0, na.rm = TRUE)

#create tables of binary age and F2 Isoprostanes

d %>%
  group_by(ageday_ut2_binary) %>%
  summarise_at(vars(t2_iso_pca), list(name = mean), na.rm = TRUE)

d %>%
  group_by(ageday_ut2_binary) %>%
  summarise_at(vars(t2_f2_8ip), list(name = mean), na.rm = TRUE)

d %>%
  group_by(ageday_ut2_binary) %>%
  summarise_at(vars(t2_f2_12i), list(name = mean), na.rm = TRUE)

d %>%
  group_by(ageday_ut2_binary) %>%
  summarise_at(vars(t2_f2_23d), list(name = mean), na.rm = TRUE)

d %>%
  group_by(ageday_ut2_binary) %>%
  summarise_at(vars(t2_f2_VI), list(name = mean), na.rm = TRUE)

#Mostly higher in low age (except for t2_f2_8ip)
