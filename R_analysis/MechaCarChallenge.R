# load libraries
install.packages('tidyverse')
library(tidyverse)
library(dplyr)

# load csv dataset
mechaCar_mpg <- read.csv('MechaCar_mpg.csv')
suspension_coil <- read.csv('Suspension_Coil.csv')
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


#-----Statistic Summary------------------------
summarize_sus_coil <- suspension_coil  %>%
          summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))

# statistic summary group by different Lot 
group_summarize_sus_coil <- suspension_coil %>% group_by(Manufacturing_Lot) %>%
  summarise(Mean_PSI = mean(PSI), Median_PSI = median(PSI),Variance_PSI = var(PSI),Stdev_PSI =sd(PSI))

#---------One sample t-test----------
t.test(suspension_coil$PSI, mu = 1500)



