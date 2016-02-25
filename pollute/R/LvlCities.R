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
  
  levels=vector(length=5)
  
  for(n in c(1, 2, 3, 4, 5)) {
    
    EUSO2=read_excel('data/EUSO22013.xlsx', sheet=n)
    
    levels[n]=EUSO2 %>%
      filter(country_iso_code == COUNTRY) %>%
      select(µg_m3) %>%
      summarize(mean(µg_m3));
  }
  
  names(OUTPUT) = c('SO2', 'PM10', 'PM2.5', 'NO2', 'O3')
  return(OUTPUT)
  
}

# library(testthat)
# 
# #Automated Test 1: If all ug_m3 observations < LEVEL, then output should be 0.
# expect_that(str_length(NumCities('AT',50,'SO2')$n), equals(integer(0)))
# 
# #Automated Test 2: If all ug_m3 observations > LEVEL, then output should be all cities.
# expect_that(length(NumCities('AT',0,'SO2')$n), equals(6))
# 
# #Automated Test 3: Should have 6 values for AT no matter pollution type.
# expect_that(length(NumCities('AT',0,'SO2')$n), equals(6))
# expect_that(length(NumCities('AT',0,'PM10')$n), equals(6))
# expect_that(length(NumCities('AT',0,'PM2.5')$n), equals(6))
# expect_that(length(NumCities('AT',0,'NO2')$n), equals(6))
# expect_that(length(NumCities('AT',0,'O3')$n), equals(6))