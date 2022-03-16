# F2 Isoprostanes Exploration

d <- box_read("880476682582")

#Table of F2-Iso by laz categories
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
