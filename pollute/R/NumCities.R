#' SO2 air pollution in EU cities (2013)
#'
#' This function summarizes the number of of cities that have air pollution levels
#'  in micrograms/cubic meter above a certain level.
#'
#' @author Mitchell, Steph, Elise
#' @param COUNTRY Country of interest, use code (2 letters) chosen from EUSO2 dataset
#' @param LEVEL Concentration of SO2 pollution (ug/m^3) as threshold level
#' @param POLLUTION is the pollution type (SO2,PM10, PM2.5,NO2,O3)
#' @return OUTPUT Number of cities within the country exceeding threshold level
#' @examples
#' COUNTRY = 'DE'
#' LEVEL = 10
#' POLLUTIOn='SO2'
#' NumCities(COUNTRY, LEVEL,POLLUTION)
#' @export
NumCities=function(COUNTRY,LEVEL,POLLUTION){


  #select the desired polluiton type
  if(POLLUTION=='SO2'){SHEET=1}
  if(POLLUTION=='PM10'){SHEET=2}
  if(POLLUTION=='PM2.5'){SHEET=3}
  if(POLLUTION=='NO2'){SHEET=4}
  if(POLLUTION=='O3'){SHEET=5}


  library(readr)
  library(dplyr)
  library(stringr)# loads str_replace_all function
  library(readxl)

 EUSO2=read_excel('data/EUSO22013.xlsx',sheet=SHEET)

  OUTPUT=EUSO2 %T>%
    select(country_iso_code, city_name, µg_m3) %>%
    filter(µg_m3 > LEVEL) %>%
    filter(country_iso_code == COUNTRY) %>%
    group_by(city_name)  %>%
    summarize(n = n(), mean = mean(µg_m3)) %>%
    arrange(desc(mean));
  return(as.data.frame(OUTPUT))
}


library(testthat)

#Automated Test 1: If all ug_m3 observations < LEVEL, then output should be 0.
expect_that(str_length(NumCities('AT',50,'SO2')$n), equals(integer(0)))

#Automated Test 2: If all ug_m3 observations > LEVEL, then output should be all cities.
expect_that(length(NumCities('AT',0,'SO2')$n), equals(6))

#Automated Test 3: Should have 6 values for AT no matter pollution type
expect_that(length(NumCities('AT',0,'SO2')$n), equals(6))
expect_that(length(NumCities('AT',0,'PM10')$n), equals(6))
expect_that(length(NumCities('AT',0,'PM2.5')$n), equals(6))
expect_that(length(NumCities('AT',0,'NO2')$n), equals(6))
expect_that(length(NumCities('AT',0,'O3')$n), equals(6))
