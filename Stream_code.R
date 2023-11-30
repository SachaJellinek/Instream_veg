#Load required packages
requiredPackages <- c("sf","lubridate", "ggplot2", "tidyverse", "dplyr", "RPostgreSQL", "terra")
lapply(requiredPackages, require, character.only = TRUE)
library(spsurvey)
set.seed(51)

# read in the input and snapped points data 
vv_data <- readxl::read_xlsx('~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_forstream_snap_outputs_201123.xlsx',sheet=1)
db_mwstr <- RPostgres::dbConnect(RPostgres::Postgres(), "mwstr", host = "localhost", port = 5432, user = "readonly", password = "reachcode_42")
sites <- vv_data %>% dplyr::select(site_id, reach_v12, carea_km2, af_2022, slope_perc, meanq_mm)

cat_env_var <- DBI::dbGetQuery(db_mwstr, "SELECT reach, ei_2022, af_2022, meanq_mm FROM cat_env")
slope <- DBI::dbGetQuery(db_mwstr, "SELECT reach, slope_perc FROM subc_env")
catch_area <- DBI::dbGetQuery(db_mwstr, "SELECT reach, carea_km2 FROM subcs")

sites4 <- dplyr::left_join(sites, catch_area, by=c("reach_v12" = "reach"))
sites3 <- dplyr::left_join(sites, slope, by=c("reach_v12" = "reach"))
sites2 <- dplyr::left_join(sites, cat_env_var, by=c("reach_v12" = "reach"))
writexl::write_xlsx(sites2, "~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_201123.xlsx")

"~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_221123_finalsitesdata.xlsx"

instream <- st_read("~/uomShare/wergProj/W12 - Revegetation/Instream_veg/VV_sites_envdata_221123_finalssitesdata.shp")
str(instream)
#instream$slope_perc %>% dplyr::mutate(across(is.numeric, log))
instream$slope_perc1 <- log(1+instream$slope_perc)+1
instream$ei_20221 <- log(1+instream$ei_2022)+1
eqprob <- grts(instream, n_base = 100)
plot(eqprob, instream, key.width = lcm(3))
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

instream$area <- cut(instream$carea_km2,
              breaks=c(0, 10, 100, 4000),
              labels=c('A', 'B', 'C'))
instream$forest <- cut(instream$af_2022,
                     breaks=c(-0.1, 0.3, 1),
                     labels=c('low', 'high'))
instream$slope <- cut(instream$slope_perc,
                       breaks=c(-0.1, 1, 10, 30),
                       labels=c('A', 'B', 'C'))
strata_n <- c(low = 50, high = 50)
strat_propprob <- grts(
  instream,
  n_base = strata_n,
  stratum_var = "forest",
  aux_var = "ei_20221"
)
plot(strat_propprob, instream, key.width = lcm(3))
