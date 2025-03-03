#install.packages("data.table") #In order to be able to use the data.table library,
#firstly has to be run only the first time, the line 1.

runoff <- 130 #m3/s 

runoff_ts <- data.frame(time = as.Date(1:90, origin = '2020/12/31'), 
                        value = sample(c(130, 135, 140), 
                                       size = 90, replace = T))
head(runoff_ts)

library(data.table)
runoff_dt <- data.table(runoff_ts)

runoff_dt[value > 130]

runoff_dt[value > 130, mean(value)]

runoff_dt[value > 130, mean(value), by = month(time)]

runoff_dt[, mon := month(time)]

runoff_dt[, mon_mean := mean(value), by = mon]

runoff_month <- runoff_dt[, .(mon, mon_mean)] 
runoff_month
unique(runoff_month)

#saveRDS(runoff_dt, file = './data/dt_example.rds') #During seminar


# Begin of Assignment on this previous code

runoff_mon_1Q <- unique(runoff_month)

pct_change_to_next_month = function(val_1, val_2) {
  pct <- ((val_2 - val_1) / val_1)
}

numerical_to_percentage = function(val_1) {
  BASE_NUMBER <- 100
  num <- (val_1 * BASE_NUMBER)
}

mean_mon_1 <- as.numeric(as.character(runoff_mon_1Q[1, 2]))
mean_mon_2 <- as.numeric(as.character(runoff_mon_1Q[2, 2]))
mean_mon_3 <- as.numeric(as.character(runoff_mon_1Q[3, 2]))

delta_1_to_2 <- numerical_to_percentage(pct_change_to_next_month(mean_mon_1, mean_mon_2))
delta_2_to_3 <- numerical_to_percentage(pct_change_to_next_month(mean_mon_2, mean_mon_3))

NO_NEXT_MONTH_TO_COMPARE <- 99
runoff_pct_change_mon_1Q <- runoff_mon_1Q[, "%_delta_to_next_month" := c(delta_1_to_2, delta_2_to_3, NO_NEXT_MONTH_TO_COMPARE)]

saveRDS(runoff_dt, file = './assignments/dt_assignment_1_20220321.rds') #As assignment