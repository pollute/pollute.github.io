#SO2 air pollution by EU city.
#
#This function summarizes the number of of cities that have air pollution levels 
# in micrograms/cubic meter above a certain level.

#' @param COUNTRY Country of interest, use code (2 letters) chosen from EUSO2 dataset
#' @param LEVEL Concentration of SO2 pollution (ug/m^3) as threshold level
#' @return OUTPUT Number of cities within the country exceeding threshold level
#' @examples 
#' COUNTRY = DE
#' LEVEL = 20
#' AirPollution(COUNTRY, LEVEL)
#' @author Mitchell, Steph, Elise, Dannique

AirPollution=function(COUNTRY,LEVEL){
  
  library(readr)
  library(dplyr)
  library(stringr)# loads str_replace_all function
  
  
  EUSO2=read_csv('data/EUSO2.csv')
  
  EUSO2$city_name=(str_replace_all(EUSO2$city_name, "[^[:alnum:]]", " "))#Removes non-alphanumeric characters with a space which otherwise will cause problems with kable function
  
  OUTPUT=EUSO2 %T>% 
    select(country_iso_code, city_name, ug_m3) %>% 
    filter(ug_m3 > LEVEL) %>% 
    filter(country_iso_code == COUNTRY) %>% 
    group_by(city_name)  %>% 
    summarize(n = n(), mean = mean(ug_m3)) %>%
    arrange(desc(mean));
  return(OUTPUT)
}