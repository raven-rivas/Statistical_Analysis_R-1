# load libraries
install.packages('tidyverse')
library(tidyverse)
# load csv dataset
mechaCar_mpg <- read.csv('MechaCar_mpg.csv')

#---------------------Normality----------------

# quantitative Test of normality 
shapiro.test(mechaCar_mpg$mpg)
# quality test of normality
ggplot(mechaCar_mpg,aes(mpg))+geom_density()

# --------statistical test----correlation------

# collect variables of interest from dataset into a metric
matrix <- as.matrix(mechaCar_mpg[, c('mpg','vehicle.length', 'vehicle.weight','spoiler.angle','ground.clearance' )])

# produce a correlation matrixusing Pearson
cor(matrix)

#---------Multiple Linear Regression model-------
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance, data =mechaCar_mpg))