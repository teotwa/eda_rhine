dir.create('./assignments')
#install.packages("data.table")

temperatures = c(3, 6, 10, 14)
weights = c(1, 0.8, 1.2, 1)

library(data.table)

multiply = function(x, y) {
  x*y
}

results <- multiply(temperatures,weights)
