# In this example the gpkg files are in a subfolder of the working directory
mwstr_dir <- "mwstr_v13/"  
# Create a sqlite connection to the database and to the cats layer
db_m <- RSQLite::dbConnect(drv = RSQLite::SQLite(), 
                               paste0(mwstr_dir,"mwstr_v13.gpkg"))
db_c <- RSQLite::dbConnect(drv = RSQLite::SQLite(), 
                               paste0(mwstr_dir,"mwstr_cats_v13.gpkg"))

# Retrieve site and nextds fields for all subcs and convert to an igraph
#  objectfor rapid network calculations
subcs <- RSQLite::dbGetQuery(db_m,"SELECT site, nextds FROM subcs;")
subcs <- apply(subcs, 2, as.character)
subcs_ig <- igraph::graph_from_data_frame(subcs)

# Retrieve details for Riddells Creek
riddells_ck <- RSQLite::dbGetQuery(db_m, 
      "SELECT * FROM stream_names WHERE SUBSTR(str_nm,1,7) = 'RIDDELL';")
# Inspecting the object riddells_ck reveals strcode for Riddells Creek is 
# RID, and terminal reach is 11580

# Retrieve all reaches upstream of Riddells Creek's terminal reach
x <- subcs[igraph::subcomponent(subcs_ig, "11580", "in"),1]
rid_network <- sf::st_read(db_m, query = paste0("SELECT * FROM streams ",
                        "WHERE site IN (",paste(x, collapse = ", "), ");"))

# Retrieve Riddells Creek mainstem
rid_ms <- sf::st_read(db_m, query = 
          "SELECT * FROM streams WHERE SUBSTR(reach,1,3) = 'RID';")

# Retrieve catchment boundary for terminal reach of Riddells Creek
rid_cat <- sf::st_read(db_c,
                       query = "SELECT * FROM cats WHERE site = 11580;")

# Disconnect database connections
RSQLite::dbDisconnect(db_m); RSQLite::dbDisconnect(db_c)

# Plot the stream, its tribs and its catchment
par(mar = c(0,0,0,0))
plot(rid_network$geom, col = "cornflowerblue")
plot(rid_ms$geom, col = "blue", lwd = 2, add = TRUE)
plot(rid_cat$geom, border = "darkgreen", lwd = 2, lty = 2, add = TRUE)
