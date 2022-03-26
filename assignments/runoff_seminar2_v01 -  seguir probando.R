## --- Assignment ---
## Navigator’s Tasks
##
## 1) Create a new data.table from runoff_stations, containing sname, area, altitude. Then, transform it to tidy format.
## 2) Create a geom_point plot of area (x axis) vs. altitude (y axis).
## 3) Try to reproduce the following graphs:

library(data.table)
library(ggplot2)

# Loading database and subset

runoff_stations <- readRDS('./data/runoff_stations.rds')

runoff_stations_3col <- runoff_stations[, c('sname', 'area' ,'altitude'), with = FALSE]
runoff_stations_plot <- runoff_stations[, c('sname', 'area' ,'altitude', 'size', 'lat', 'lon'), with = FALSE]
runoff_stations_years <- runoff_stations[, .(sname, start, end)]
###equivalente>>>
###runoff_stations_years <- runoff_stations[, c('sname', 'start' ,'end'), with = FALSE]

####
runoff_stations_missed <- readRDS('./data/runoff_stations_raw.rds')
runoff_statio <- readRDS('./data/runoff_day_raw.rds')
runoff_statio
raw_path <- './data/raw/runoff_day/'
fnames <- list.files(raw_path)
n_station <- length(fnames)
id_length <- 7
runoff_day_raw <- data.table() #we create an empty data table
id_sname <- runoff_stations[, .(id, sname)]

for(file_count in 1:n_station){
  temp_dt <- fread(paste0(raw_path, fnames[file_count]))
  station_id <- substr(fnames[file_count], 1, id_length)
  temp_dt <- cbind(id = factor(station_id), temp_dt)
  temp_dt <- id_sname[temp_dt, on = 'id', ]
  runoff_day_raw <- rbind(runoff_day_raw, temp_dt)
}

runoff_day_raw[, 'hh:mm' := NULL]
colnames(runoff_day_raw)[3:4] <- c('date', 'value')
runoff_day_raw[, date := as.Date(date)]


runoff_stations_missing <- runoff_stations[, .(sname, size, missing)]
missing_values <- runoff_stations_missing[size < 0, .(missing = .N), by = sname]
missing_values

#transform(runoff_stations_missing, with_value = size * missing)

runoff_day <- readRDS('./data/runoff_day.rds')
temp_dt <- fread('./data/raw/runoff_day/6335050_Q_Day.Cmd.txt')
head(temp_dt)

# Tidy

agua <- ggplot(rees_runoff_day) +
  geom_line(aes(x = date, y = value)) #, col = sname))
agua




# Plotting

p1 <- ggplot(data = runoff_stations_plot) +
  geom_point(aes(x = area, y = altitude, col = size)) +
  geom_text(aes(area, altitude, label = sname, colour = size))
p1

p2 <- ggplot(data = runoff_stations_plot) +
  geom_point(aes(x = lon, y = lat, col = altitude)) +
  geom_text(aes(lon, lat, label = sname, colour = altitude)) +
  scale_color_gradient(low = 'dark green', high = 'brown')
p2

p3 <- ggplot(data = runoff_stations_years) +
  geom_segment(aes(x = start, xend = end, y = sname, yend = sname), color="dark blue", size=3) +
  ggtitle("Segment Chart with starting and ending years of data") +
  labs(x = "Start / End", y = "Station name")
p3
