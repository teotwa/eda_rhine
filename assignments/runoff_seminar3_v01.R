# Assignment 2022-03-28

# https://rpubs.com/imarkonis/beds_eda_04
# 1) Transform runoff_stats to tidy format and then use it to plot mean, median, minimum and maximum for each location as a scaterplot (x axis station, y axis runoff) with different colors and point types for each statistic.
# 2) Estimate the skewness and coefficient of variation for each record (a) as a new column in runoff_stats and (b) as a new data.table.
# 3) Can you plot each boxplot (facet) of monthly runoff with different fill colour according to the runoff class?
# 4) Use boxplot to plot daily runoff per station. What do you observe regarding outliers? Why do you think this happens?
# 5) Create your own classes for area and altitude and plot them in a scater plot similar to this one.

library(data.table)
library(ggplot2)
library(moments)


### 1

runoff_stats1 <- readRDS('./data/runoff_stats.rds')
runoff_stations_tidy <- melt(runoff_stats1, id.vars = "sname")

colours_4 <- c("red", "green", "blue", "violet")
ggplot(data = runoff_stations_tidy, aes(x = sname, y = value, col = variable)) +
  geom_point() +
  geom_text(label = runoff_stations_tidy$sname) +
  scale_color_manual(values = colorRampPalette(colours_4)(4)) +
  theme_bw() 


### 2

runoff_day <- readRDS('./data/runoff_day.rds')
runoff_stats <- runoff_day[, .(mean_day = round(mean(value), 0),
                               median_day = round(median(value), 0),
                               sd_day = round(sd(value), 0),
                               min_day = round(min(value), 0),
                               max_day = round(max(value), 0),
                               skewness = round(skewness(value), 2)), by = sname]
runoff_stats[, coef_var := sd_day / mean_day, by = sname]
coef_var_skew <- runoff_stats[, .(sname, skewness, coef_var)]


### 3

runoff_month <- runoff_day[, .(value = mean(value)), by = .(month, year, sname)]
runoff_month[, date := as.Date(paste0(year, '-', month, '-1'))]

ggplot(runoff_month, aes(x = factor(month), y = value)) +
  geom_boxplot(fill = colours_4[1]) +
  facet_wrap(~sname, scales = 'free') + 
  theme_bw()


### 4


### 5

runoff_stations <- readRDS('data/runoff_stations.rds')

runoff_stations[, area_class := factor('small')]
runoff_stations[area >= 10000 & area < 150000, area_class := factor('medium')]
runoff_stations[area >= 150000, area_class := factor('large')]

runoff_stations[, alt_class := factor('low')]
runoff_stations[altitude >= 50 & altitude < 500, alt_class := factor('medium')]
runoff_stations[altitude >= 500, alt_class := factor('high')]

dt <- runoff_stations[, .(sname, area, area_class, alt_class)]
to_plot <- runoff_stats[dt, on = 'sname']
ggplot(to_plot, aes(x = mean_day, y = area, col = area_class, size = alt_class)) +
  geom_point() +
  theme_bw()
