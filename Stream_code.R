#Load required packages
requiredPackages <- c("sf","lubridate", "tidyr", "ggplot2", "tidyverse", "dplyr", "RPostgreSQL", "terra")
lapply(requiredPackages, require, character.only = TRUE)
library(spsurvey)
library(rgeoda)
library(purrr)

# read in the input and snapped points data 
vv_data <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_forstream_snap_outputs_201123.xlsx',sheet=1)
vv_slope <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Slope_extra.xlsx',sheet=1)
vv_scores <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites_middle/points_with_vv_fixed_sj_reprojxy.shp")
vv_scores2 <- vv_scores %>% dplyr::select(Site_ID, vv_A_struc, vv_B_richn, vv_E_regen, vv_D_patch, vv_C_instr, vv_overall, vv_vv_scor, xcoord, ycoord)
vv_scores2 <- vv_scores2 %>% 
  rename("site_id" = "Site_ID")%>% 
  rename("x" = "xcoord") %>% 
  rename("y" = "ycoord")

db_mwstr <- RPostgres::dbConnect(RPostgres::Postgres(), "mwstr", host = "localhost", port = 5432, user = "readonly", password = "reachcode_42")
sites <- vv_data %>% dplyr::select(site_id, reach_v12, carea_km2, af_2022, slope_perc, meanq_mm)

cat_env_var <- DBI::dbGetQuery(db_mwstr, "SELECT reach, ei_2022, af_2022, meanq_mm FROM cat_env")
slope <- DBI::dbGetQuery(db_mwstr, "SELECT reach, slope_perc FROM subc_env")
catch_area <- DBI::dbGetQuery(db_mwstr, "SELECT reach, carea_km2 FROM subcs")

slopes <- dplyr::left_join(vv_slope, slope, by=c("reach_v12" = "reach"))
sites4 <- dplyr::left_join(sites, catch_area, by=c("reach_v12" = "reach"))
sites3 <- dplyr::left_join(sites, slope, by=c("reach_v12" = "reach"))
sites2 <- dplyr::left_join(sites, cat_env_var, by=c("reach_v12" = "reach"))
writexl::write_xlsx(sites2, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_201123.xlsx")

correlation_dat <- instream %>% dplyr::select(carea_km2, ei_2022, af_2022, slope_perc, meanq_mm)
str(correlation_dat)
nc_df2 <- correlation_dat %>% st_drop_geometry()
res <- cor(nc_df2)
round(res, 2)

# Plotting variables to look at their distribution
ggplot(data = instream) + 
  geom_point(mapping = aes(x = meanq_mm, y = site_id))
ggplot(data = instream) + 
  geom_point(mapping = aes(x = carea_km2, y = site_id))
ggplot(data = instream) + 
  geom_point(mapping = aes(x = af_2022, y = site_id))
ggplot(data = instream) + 
  geom_point(mapping = aes(x = slope_perc, y = site_id))
ggplot(data = instream) + 
  geom_point(mapping = aes(x = ei_2022, y = site_id))
ggplot(data = instream) + 
  geom_point(mapping = aes(x = GMUT1DESC, y = site_id))

instream <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_221123_finalssitesdata.shp")
str(instream)
instream2 <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_221123_finalssitesdata.shp")
str(instream2)
# Categorising variables
instream$area <- cut(instream$carea_km2,
                     breaks=c(0, 250, 1000, 4000),
                     labels=c('A', 'B', 'C'))
instream$rain <- cut(instream$meanq_mm,
                     breaks=c(0, 125, 250, 500, 1000),
                     labels=c('A', 'B', 'C', 'D'))
instream$forest <- cut(instream$af_2022,
                     breaks=c(-0.1, 0.5, 1),
                     labels=c('A', 'B'))
instream$slope <- cut(instream$slope_perc,
                       breaks=c(-0.1, 5, 15, 33),
                       labels=c('A', 'B', 'C'))
instream$ei <- cut(instream$ei_2022,
                      breaks=c(-0.1, 0.05, 0.1, 0.3, 0.6),
                      labels=c('A', 'B', 'C', 'D'))
#writexl::write_xlsx(instream, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/instream_categories.xlsx")
## Choose sites using slice_sample. Maintaining steep slopes and removing Coastal areas
cat <- instream %>% dplyr::select(site_id, ei, slope, forest, rain, area, GMUT1DESC)
cat1<- cat %>% filter(!GMUT1DESC %in% c("Coast (C)"))
cat1DF <- cat1 %>% st_drop_geometry()
group1 <- cat1DF %>% tidyr::expand(slope, rain, area, forest, ei)
group2 <- cat1DF %>% tidyr::expand(nesting(site_id, slope, rain, area, ei))
## Write groups of site combinations to excel for manual grouping using concatenate
writexl::write_xlsx(group2, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Site_ID_categories_a.xlsx")
## 33 groups of combinatations excluding atenuated forest cover
group3 <- group2 %>% 
  group_by(slope, rain, area, ei) %>% 
  count
#Read in new dataset
site_cat <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Site_ID_categories.xlsx',sheet=1)
geo <- instream %>% dplyr::select(site_id, GMUT1DESC)
geo2 <- geo %>% st_drop_geometry()
vvscores <- vv_scores2 %>% st_drop_geometry()
site_cat <- left_join(site_cat, geo2)
site_cat <- left_join(site_cat, vvscores)
#All groups with less that 3 sites and steep slopes. Coastal sites excluded and attenuated forest cover not included in groups
target_1 <- c("AABB","ABAB","ACAB","ACBD","ACCB","ADBA","ADCA","BBAC","BCAD","CAAA","CDAA","CCAA")
grp1 <- filter(site_cat, group %in% target_1)
sites_AAAA <- filter(site_cat, group == "AAAA")
grp2<- sites_AAAA %>% group_by(GMUT1DESC) %>% slice_sample(n = 3)
sites_4 <- filter(site_cat, group == "AABC")
grp3<- sites_4 %>% slice_sample(n = 4)
sites_5 <- filter(site_cat, group == "AAAD")
grp4<- sites_5 %>% slice_sample(n = 4)
sites_6 <- filter(site_cat, group == "ACAC")
grp5<- sites_6 %>% slice_sample(n = 4)
sites_7 <- filter(site_cat, group == "ABAD")
grp6<- sites_7 %>% slice_sample(n = 4)
sites_9 <- filter(site_cat, group == "AAAB")
grp7<- sites_9 %>% slice_sample(n = 4)
sites_11 <- filter(site_cat, group == "AAAC")
grp8<- sites_11 %>% slice_sample(n = 4)
sites_12 <- filter(site_cat, group == "BDAA")
grp9<- sites_12 %>% slice_sample(n = 4)
sites_15 <- filter(site_cat, group == "BCAA")
grp10<- sites_15 %>% slice_sample(n = 4)
sites_17 <- filter(site_cat, group == "ACAD")
grp11<- sites_17 %>% slice_sample(n = 4)
sites_20 <- filter(site_cat, group == "ADAA")
grp12<- sites_20 %>% slice_sample(n = 4)
sites_22 <- filter(site_cat, group == "AABA")
grp13<- sites_22 %>% slice_sample(n = 3)
sites_49 <- filter(site_cat, group == "ACAA")
grp14<- sites_49 %>% slice_sample(n = 4)
sites_95 <- filter(site_cat, group == "ABAA")
grp15<- sites_95 %>% group_by(GMUT1DESC) %>% slice_sample(n = 2)
sites_4b <- filter(site_cat, group == "ABBA")
grp16<- sites_4b %>% slice_sample(n = 4)
sites_4c <- filter(site_cat, group == "BAAA")
grp17<- sites_4c %>% slice_sample(n = 4)
sites_5b <- filter(site_cat, group == "ACBA")
grp18<- sites_5b %>% slice_sample(n = 4)
sites_9a <- filter(site_cat, group == "ABAC")
grp19<- sites_9a %>% slice_sample(n = 4)
sites_9b <- filter(site_cat, group == "BBAA")
grp20<- sites_9b %>% slice_sample(n = 4)
sites_11a <- filter(site_cat, group == "AACA")
grp21<- sites_11a %>% slice_sample(n = 4)
sites_11b <- filter(site_cat, group == "ACCA")
grp22<- sites_11b %>% slice_sample(n = 4)
grouped <- rbind(grp1,grp2,grp3,grp4,grp5,grp6,grp7,grp8,grp9,grp10,grp11,grp12,grp13,grp14,grp15,grp16,grp17,grp18,grp19,grp20,grp21,grp22)

grouped2<- grouped %>% filter(!site_id %in% c(175, 88, 318, 20, 242, 399, 493, 467, 425, 435, 118, 242))
ggplot(data = grouped2) + 
  geom_point(mapping = aes(x = slope, y = site_id))
ggplot(data = grouped2) + 
  geom_point(mapping = aes(x = rain, y = site_id))
ggplot(data = grouped2) + 
  geom_point(mapping = aes(x = area, y = site_id))
ggplot(data = grouped2) + 
  geom_point(mapping = aes(x = ei, y = site_id))
ggplot(data = grouped2) + 
  geom_point(mapping = aes(x = GMUT1DESC, y = site_id))
ggplot(data = grouped2) + 
  geom_point(mapping = aes(x = vv_vv_scor, y = site_id))
ggplot(data = grouped2) + 
  geom_point(mapping = aes(x = vv_C_instr, y = site_id))
#save shapefile
grouped2shp <- st_as_sf(x = grouped2, 
                        coords = c("x", "y"),
                        crs = 7855)
plot(grouped2shp, key.width = lcm(3))
plot(joined_shp, key.width = lcm(3))
s <- terra::vect(grouped2shp)
outfile <- "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites_middle/subset_vv_data121223.shp"
terra::writeVector(s, outfile, overwrite=TRUE)

#Site selection using GRTS (spsurvey)

#instream$slope_perc %>% dplyr::mutate(across(is.numeric, log))
#instream$slope_perc1 <- (instream$slope_perc)+1
#instream$ei_20221 <- log(1+instream$ei_2022)+1
eqprob <- grts(instream, n_base = 100)
plot(eqprob, instream, key.width = lcm(3))

ggplot(data = subset) + 
propprob <- grts(
  instream,
  n_base = 100,
  aux_var = "ei_20221"
)
plot(propprob, instream, key.width = lcm(3))
propprob <- grts(
  instream,
  n_base = strata_n,
  aux_var = "meanq_mm"
)

strata_n <- c(low = 50, high = 50)
strat_propprob <- grts(
  instream,
  n_base = strata_n,
  stratum_var = "forest",
  aux_var = "ei_20221"
)
plot(strat_propprob, instream, key.width = lcm(3))
