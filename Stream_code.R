#Load required packages
requiredPackages <- c("sf","lubridate", "ggplot2", "tidyverse", "dplyr", "RPostgreSQL", "terra")
lapply(requiredPackages, require, character.only = TRUE)
library(spsurvey)
library(rgeoda)

# read in the input and snapped points data 
vv_data <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_forstream_snap_outputs_201123.xlsx',sheet=1)
vv_slope <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Slope_extra.xlsx',sheet=1)
vv_scores <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites_middle/points_with_vv_fixed_sj_reprojxy.shp")
vv_scores2 <- vv_scores %>% dplyr::select(Site_ID, vv_A_struc, vv_B_richn, vv_E_regen, vv_D_patch, vv_C_instr, vv_overall, vv_vv_scor)
vv_scores2 <- vv_scores2 %>% 
  rename("site_id" = "Site_ID")
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
#coast <- cat %>%filter (GMUT1DESC == "Coast (C)")
steepslope <- cat %>%filter (slope == "C")
cat2<- cat %>% filter(!GMUT1DESC %in% c("Coast (C)"))
cat3<- cat2 %>% filter(!slope %in% c("C"))
cat4<- cat3 %>% group_by(GMUT1DESC) %>% slice_sample(n = 18)
subset <- rbind(cat4, steepslope)

plot(subset, key.width = lcm(3))

#save shapefile
s <- terra::vect(subset)
outfile <- "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/Veg_Visions_2021_sites_middle/subset_vv_data.shp"
terra::writeVector(s, outfile, overwrite=TRUE)

#Site selection using GRTS (spsurvey)
instream <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_221123_finalssitesdata.shp")
str(instream)
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
