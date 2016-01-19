#### 
# Air Quality Summary Statistics
# Dataset: Air pollutant concentrations 2013 - Dataset by cities (compared to EU values)
# Using SO2 tab
# data taken from http://www.eea.europa.eu/data-and-maps/data/air-pollutant-concentrations-at-station
# Author(s): Elise. (add your name if you work on this)
# Date started: 18 Jan 2016
# Informatics Winter quarter 2016
####

# Not sure how to load files here... hmmm is this directory correct...
EUSO2 <- read.csv("~EUSO2.csv")

# Summarize countries by their mean SO2 concentration across all monitoring stations
EUSO2CountryM = aggregate(ug.m3 ~ country.iso.code, data = EUSO2, FUN = mean)

# Boxplot summary of SO2 measurements taken in France, Germany and Czech Republic
# ... this boxplot has an issue with the country codes ALL showing up. Will fix later.
TriComp = subset(EUSO2, country.iso.code == "FR" | country.iso.code == "DE" | country.iso.code == "CZ")
TriComp = data.frame(TriComp)
boxplot(ug.m3 ~ country.iso.code , data=TriComp, xlab = "Countries", ylab = "Atmosphertic SO2 (ug/m3)", main="SO2 Concentrations in Selected EU Countries")
