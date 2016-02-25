#' SO2 air pollution in EU cities (2013)
#'
#' This function summarizes the mean of multiple pollutantss for a given city 
#'
#' @author Mitchell, Steph, Elise
#' @param COUNTRY Country of interest, use code (2 letters) chosen from EUSO2 dataset
#' @return OUTPUT Number of cities within the country exceeding threshold level
#' @examples
#' COUNTRY = 'DE'
#' LvlCities(COUNTRY)
#' @export

LvlCities=function(COUNTRY='AT'){

  library(readr)
  library(dplyr)
  library(stringr)
  library(readxl)
  
  OUTPUT=vector(length=5)
  
  for(n in c(1, 2, 3, 4, 5)) {
    
    EUSO2=read_excel('data/EUSO22013.xlsx', sheet=n)
    
    OUTPUT[n]=EUSO2 %>%
      filter(country_iso_code == COUNTRY) %>%
      select(µg_m3) %>%
      summarize(mean(µg_m3));
  }
  
  names(OUTPUT) = c('SO2', 'PM10', 'PM2.5', 'NO2', 'O3')
  return(OUTPUT)
  
}

# library(testthat)