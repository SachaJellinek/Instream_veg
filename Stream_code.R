#Load required packages
requiredPackages <- c("sf","lubridate", "tidyr", "ggplot2", "tidyverse", "dplyr", "RPostgreSQL", "terra")
lapply(requiredPackages, require, character.only = TRUE)
library(spsurvey)
library(rgeoda)
library(purrr)

# read in the input and snapped points data 
vv_data <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_forstream_snap_outputs_201123.xlsx',sheet=1)
#vv_slope <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Slope_extra.xlsx',sheet=1)
vv_scores <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites/points_with_vv_fixed_sj_reprojxy.shp")
vv_scores2 <- vv_scores %>% dplyr::select(Site_ID, vv_A_struc, vv_B_richn, vv_E_regen, vv_D_patch, vv_C_instr, vv_overall, vv_vv_scor, xcoord, ycoord)
vv_scores2 <- vv_scores2 %>% 
  rename("site_id" = "Site_ID")%>% 
  rename("x" = "xcoord") %>% 
  rename("y" = "ycoord")

db_mwstr <- RPostgres::dbConnect(RPostgres::Postgres(), "mwstr", host = "localhost", port = 5432, user = "readonly", password = "reachcode_42")
sites <- vv_data %>% dplyr::select(site_id, reach_v12, carea_km2, af_2022, slope_perc, meanq_mm)
# Get variables from db_mwstr
cat_env_var <- DBI::dbGetQuery(db_mwstr, "SELECT reach, ei_2022, af_2022, meanq_mm FROM cat_env")
#slope <- DBI::dbGetQuery(db_mwstr, "SELECT reach, slope_perc FROM subc_env")
#catch_area <- DBI::dbGetQuery(db_mwstr, "SELECT reach, carea_km2 FROM subcs")
#slopes <- dplyr::left_join(vv_slope, slope, by=c("reach_v12" = "reach"))
#sites_catcharea <- dplyr::left_join(sites, catch_area, by=c("reach_v12" = "reach"))
#sites_slope <- dplyr::left_join(sites, slope, by=c("reach_v12" = "reach"))

vvsites_db_mwstr_var <- dplyr::left_join(sites, cat_env_var, by=c("reach_v12" = "reach"))
writexl::write_xlsx(vvsites_db_mwstr_var, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_201123.xlsx")
# vvsites_db_mwstr_var data without data gaps and including geology and xy coordinates
instream_221123 <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_221123_finalssitesdata.shp")

# Correlation analysis
correlation_dat <- instream %>% dplyr::select(carea_km2, ei_2022, af_2022, slope_perc, meanq_mm)
str(correlation_dat)
nc_df2 <- correlation_dat %>% st_drop_geometry()
res <- cor(nc_df2)
round(res, 2)

# Plotting variables to look at their distribution
ggplot(data = instream_221123) + 
  geom_point(mapping = aes(x = meanq_mm, y = site_id))
ggplot(data = instream_221123) + 
  geom_point(mapping = aes(x = carea_km2, y = site_id))
ggplot(data = instream_221123) + 
  geom_point(mapping = aes(x = af_2022, y = site_id))
ggplot(data = instream_221123) + 
  geom_point(mapping = aes(x = slope_perc, y = site_id))
ggplot(data = instream_221123) + 
  geom_point(mapping = aes(x = ei_2022, y = site_id))
ggplot(data = instream_221123) + 
  geom_point(mapping = aes(x = GMUT1DESC, y = site_id))

# Categorising variables
instream_221123$area <- cut(instream_221123$carea_km2,
                     breaks=c(0, 250, 1000, 4000),
                     labels=c('A', 'B', 'C'))
instream_221123$rain <- cut(instream_221123$meanq_mm,
                     breaks=c(0, 125, 250, 500, 1000),
                     labels=c('A', 'B', 'C', 'D'))
instream_221123$forest <- cut(instream_221123$af_2022,
                     breaks=c(-0.1, 0.5, 1),
                     labels=c('A', 'B'))
instream_221123$slope <- cut(instream_221123$slope_perc,
                       breaks=c(-0.1, 5, 15, 33),
                       labels=c('A', 'B', 'C'))
instream_221123$ei <- cut(instream_221123$ei_2022,
                      breaks=c(-0.1, 0.05, 0.1, 0.3, 0.6),
                      labels=c('A', 'B', 'C', 'D'))
#writexl::write_xlsx(instream, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/instream_categories.xlsx")
## Choose sites using slice_sample. Maintaining steep slopes and removing Coastal areas
cat <- instream_221123 %>% dplyr::select(site_id, ei, slope, forest, rain, area, GMUT1DESC)
#remove sites on the coast (n = 5)
nocoast<- cat %>% filter(!GMUT1DESC %in% c("Coast (C)"))
instream_221123_df <- nocoast %>% st_drop_geometry()
# grouping instream variables but removing attenuated forest cover - af_2022
instream_221123_grp <- instream_221123_df %>% tidyr::expand(nesting(site_id, slope, rain, area, ei))
## Write groups of site combinations to excel for manual grouping using concatenate
writexl::write_xlsx(instream_221123_grp, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Site_ID_categories_a.xlsx")
# Re-import Site_ID_categories_a.xlsx with combinations of categories
site_cat <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Site_ID_categories.xlsx',sheet=1)
## 33 groups of combinatations excluding attenuated forest cover
combinations <- instream_221123_grp %>% 
  group_by(slope, rain, area, ei) %>% 
  count

#add veg visions scores and geology to site_cat 
geo <- instream_221123 %>% dplyr::select(site_id, GMUT1DESC)
geo2 <- geo %>% st_drop_geometry()
vvscores <- vv_scores2 %>% st_drop_geometry()
site_cat <- left_join(site_cat, geo2)
site_cat <- left_join(site_cat, vvscores)
# Keeping all groups with less than 4 sites and steep slopes. Coastal sites excluded and attenuated forest cover not included in groups
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

grouped2<- grouped %>% filter(!site_id %in% c(175, 318, 20, 242, 399, 493, 467, 425, 435, 118, 242))
# Remove for romp sites
groupedRomp<- grouped2 %>% filter(!site_id %in% c(465, 128, 144, 422, 388, 60, 13, 487, 89))
# Remove for macro sites
groupedMacroRomp<- groupedRomp %>% filter(!site_id %in% c(307, 89, 156, 172, 408, 400, 381, 335, 429))
# Romp and macro sites to add
Romp <- c("466", "152", "135", "298", "409", "50", "37", "86", "10")
Macro <- c("313", "164", "114", "52", "382", "336", "278")
Rompsites <- filter(site_cat, site_id %in% Romp)
Macrosites <- filter(site_cat, site_id %in% Macro)
Romp_macro <- rbind(Rompsites, Macrosites)
finalsites <- rbind(groupedMacroRomp, Romp_macro)

ggplot(data = finalsites) + 
  geom_point(mapping = aes(x = slope, y = site_id))
ggplot(data = finalsites) + 
  geom_point(mapping = aes(x = rain, y = site_id))
ggplot(data = finalsites) + 
  geom_point(mapping = aes(x = area, y = site_id))
ggplot(data = finalsites) + 
  geom_point(mapping = aes(x = ei, y = site_id))
ggplot(data = finalsites) + 
  geom_point(mapping = aes(x = GMUT1DESC, y = site_id))
ggplot(data = finalsites) + 
  geom_point(mapping = aes(x = vv_vv_scor, y = site_id))
ggplot(data = finalsites) + 
  geom_point(mapping = aes(x = vv_C_instr, y = site_id))
#save shapefile
finalsitesshp <- st_as_sf(x = finalsites, 
                        coords = c("x", "y"),
                        crs = 7855)
plot(finalsitesshp, key.width = lcm(3))
plot(finalsitesshp, key.width = lcm(3))
s <- terra::vect(finalsitesshp)
outfile <- "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites_middle/finalsubset_vv_data131223_2.shp"
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

##select samplable sites
instream_sites300124 <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites/finalsubset_vv_data131223_2.shp")
# add stream reach to data
sites2 <- sites %>% dplyr::select(site_id, reach_v12)
instream_sites300124reach <- dplyr::left_join(instream_sites300124, sites2, by=c("site_id"))
## table with 2 columns: reach and sampleable dataframe in db_mwstr
dat <- dbGetQuery(db_mwstr, "SELECT reach, sampleable FROM streams")
#add the sampleable 0/1 information to your final_sites spatial table
samplable_sites050224 <- dplyr::left_join(instream_sites300124reach, dat, by=c("reach_v12" = "reach"))
writexl::write_xlsx(samplable_sites050224, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites/samplable_sites050224.xlsx")

#samplable_streams <- sf::st_read(db_mwstr,
      #                 query = "SELECT * FROM streams WHERE sampleable = 0;")
#streams2 <- streams %>% dplyr::select(reach, sampleable)
#streams2_ng <- streams2 %>% st_drop_geometry()
#samplable <- dplyr::left_join(final_sites, streams2_ng, by=c("reach_v12" = "reach"))
#writexl::write_xlsx(samplable, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites_middle/Not_samplable_sites.xlsx")


