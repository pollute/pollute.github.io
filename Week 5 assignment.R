#Group Homework Assignment, Week 5
library(ggplot2)
library(dplyr)

thinning = read.csv(file = "thinning.csv")
View(thinning)

#How does the rate of evapotranspiration (evap) change with leaf area index (lai)? 
LAIcarbon = ggplot(thinning, aes(x = lai, y = evap)) + geom_point(col = 'chartreuse4') + ggtitle('Evapotranspiration Rate Changes with Leaf Area Index') + labs(x = 'Leaf Area Index', y = 'Rate of Evapotranspiration')

windows()
LAIcarbon  #hold your horses, this takes a bit.... so much data....

#Add the mean evap observation for each lai point
evap.mean = thinning %>% 
  group_by(lai) %>% 
  mutate(EvapMean = mean(evap), by = c('lai'))

windows()
LAIcarbon.mean = LAIcarbon + geom_line(aes(x = thinning$lai, y = evap.mean))
##NEED TO EDIT
