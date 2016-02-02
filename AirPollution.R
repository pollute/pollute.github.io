#This function summarizes the number of of cities that have air pollution levels 
# in micrograms/cubic meter above a certain level.

AirPollution=function(COUNTRY,LEVEL){

  #User defines the COUNTRY of interest, "Germany for example would need the input 'DE'
  
  #User also defines the LEVEL of pollution (SO2) that must be exceeded or equal to in analysis
  
library(readr)
library(dplyr)
  
EUSO2=read_csv('data/EUSO2.csv')
  
  
OUTPUT=EUSO2 %T>% 
  select(country_iso_code, city_name, ug_m3) %>% 
  filter(ug_m3 > LEVEL) %>% 
  filter(country_iso_code == COUNTRY) %>% 
  group_by(city_name)  %>% 
  summarize(n = n());
return(OUTPUT)
}

