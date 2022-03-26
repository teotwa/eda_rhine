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

# Tidy
### Actually, it's already runoff_stations

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
