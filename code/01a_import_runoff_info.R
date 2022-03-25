#01a_import_runoff_info.R
#2022-03-25

# First step

library(data.table)

list.files('./data/raw')

runoff_stations <- fread('./data/raw/runoff_stations.csv')

head(runoff_stations)
str(runoff_stations)
runoff_stations$station

runoff_stations[, sname := factor(abbreviate(station))] #abbreviate: trim by default to 4 lenght name
runoff_stations[, id := factor(id)] #factor converts the field ID in a categorial variable
runoff_stations[, lat := round(lat, 3)]
runoff_stations[, lon := round(lon, 3)]
runoff_stations[, altitude := round(altitude, 0)]

saveRDS(runoff_stations, './data/runoff_stations_raw.rds')