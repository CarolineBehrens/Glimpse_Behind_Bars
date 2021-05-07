library(readr) 
library(tidyverse)
library(gtsummary) 
library(gt) 
library(broom.mixed) 
library(rstanarm)
library(shiny)
library(shinythemes)
library(viridis)
library(tidybayes)



#loading in the appropriate data sets for the project


total_mortality <- msfp0116stt12 <- read_csv("raw_data/msfp0116stt12.csv",
                                             col_types = cols(
                                               .default = col_character(),
                                               `2001` = col_double(),
                                               `2006` = col_double(),
                                               `2007` = col_double(),
                                               `2008` = col_double(),
                                               `2009` = col_double(),
                                               `2010` = col_double(),
                                               `2011` = col_double(),
                                               `2012` = col_double(),
                                               `2013` = col_double(),
                                               `2014` = col_double(),
                                               `2015` = col_double(),
                                               `2016` = col_double()
                                             ))

crime_and_incarceration_by_state <- read_csv("raw_data/crime_and_incarceration_by_state.csv",
                                             col_types = cols(
                                               jurisdiction = col_character(),
                                               includes_jails = col_logical(),
                                               year = col_double(),
                                               prisoner_count = col_double(),
                                               crime_reporting_change = col_logical(),
                                               crimes_estimated = col_logical(),
                                               state_population = col_double(),
                                               violent_crime_total = col_double(),
                                               murder_manslaughter = col_double(),
                                               rape_legacy = col_double(),
                                               rape_revised = col_double(),
                                               robbery = col_double(),
                                               agg_assault = col_double(),
                                               property_crime_total = col_double(),
                                               burglary = col_double(),
                                               larceny = col_double(),
                                               vehicle_theft = col_double()
                                             )) 
#cleaning up the first data set to show only the columns 
#that are beneficial. caveat columns are empty so they should
#be removed

total_mortality <- total_mortality %>%
  rename(jurisdiction = `State/Federal`) %>%
  filter(!(jurisdiction %in% c("Federal", "State"))) %>%
  select(-"2001 caveat", -"2006 caveat", -"2007 caveat", -"2008 caveat", -"2009 caveat",
         -"2010 caveat", -"2011 caveat", -"2012 caveat", -"2013 caveat", -"2014 caveat",
         -"2015 caveat", -"2016 caveat")

mortality_numbers <- total_mortality %>%
  pivot_longer(names_to = "Year",
               values_to = "Mortality_Number",
               cols = c(`2001`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`,
                        `2013`, `2014`,`2015`,`2016`)) %>%
  mutate(south = ifelse(jurisdiction %in% c("Alabama", "Florida", "Georgia", "Kentucky",
                                            "Louisiana", "Maryland", "Mississippi",
                                            "North Carolina", "Oklahoma", "South Carolina",
                                            "Tennesse", "Texas", "Virginia", "West Virginia"),
                        TRUE, FALSE))


#saving cleaned up data to an rds file 
#putting it in the clean data folder.
write_rds(mortality_numbers, file = "prison/clean_data/mortality_numbers_data.rds")



#taking a first look at the second data set and seeing that 
#I should change the name of the jurisdiction column 
#to "name" so that it matches with my other data set 


state_crime <- crime_and_incarceration_by_state %>%
  sample_n(1000, replace = TRUE) %>%
  mutate(name = str_to_sentence(jurisdiction)) %>%
  select(name, year, violent_crime_total,agg_assault, 
         murder_manslaughter, robbery, property_crime_total,
         larceny, vehicle_theft) %>%
  pivot_longer(names_to = "type",
               values_to = "total",
               cols = c(property_crime_total, violent_crime_total))

#saving as rds again

write_rds(state_crime, file = "prison/clean_data/state_crime_data.rds")

#First I am creating the south variable so I can show that 
#there is a difference between northern and southern states.
#Divided some columns by 1000 because the numbers were too large
#as is and this makes them easier to work with.
#Then, I created  bins for the 
#population and violent crime columns because when creating a newobs
#it will produce too many outcomes. Grouping these values allows for 
#easier data control and more manageable outcomes 

crime_w_bins <- crime_and_incarceration_by_state%>%
  mutate(south = ifelse(jurisdiction %in% c("ALABAMA", "FLORIDA", "GEORGIA", "KENTUCKY",
                                            "LOUISIANA", "MARYLAND", "MISSISSIPPI",
                                            "NORTH CAROLINA", "OKLAHOMA", "SOUTH CAROLINA",
                                            "TENNESSEE", "TEXAS", "VIRGINIA", "WEST VIRGINIA"),
                        TRUE, FALSE)) %>%
  mutate(population_in_thousands = state_population/1000) %>%
  mutate(prisoner_count_in_thousands = prisoner_count/1000) %>%
  mutate(violent_crime_in_thousands = violent_crime_total/1000) %>%
  drop_na(population_in_thousands) %>%
  drop_na(prisoner_count_in_thousands) %>%
  drop_na(violent_crime_in_thousands) %>%
  mutate(population_bins = case_when(population_in_thousands < 8000 ~ 1,
                                     population_in_thousands >= 8000 & population_in_thousands <16000 ~ 2,
                                     population_in_thousands >= 16000 & population_in_thousands <24000 ~ 3,
                                     population_in_thousands >= 24000 & population_in_thousands < 32000 ~ 4,
                                     population_in_thousands >= 32000 & population_in_thousands < 40000 ~ 5)) %>%
  mutate(violent_crime_bins = case_when(violent_crime_in_thousands < 43 ~ 1,
                                        violent_crime_in_thousands  >= 43 & violent_crime_in_thousands < 86 ~2,
                                        violent_crime_in_thousands >= 86 & violent_crime_in_thousands < 129 ~ 3,
                                        violent_crime_in_thousands >= 129 & violent_crime_in_thousands < 172 ~ 4,
                                        violent_crime_in_thousands >= 172 & violent_crime_in_thousands < 215 ~ 5))

write_rds(crime_w_bins, file = "prison/clean_data/crime_w_bins_data.rds")


fit_1 <- stan_glm(prisoner_count_in_thousands ~ population_bins + south + violent_crime_bins + south*violent_crime_bins,
                                    data = crime_w_bins, 
                                      seed = 17,
                                      refresh = 0)

#saving as rds so it runs quicker in future and 
#dont have to run the fit live
write_rds(fit_1, file = "prison/clean_data/fit_1.rds")




#lastly creating a newobs and using the unique function 
#makes it much easier. 

population_bins <- unique(crime_w_bins$population_bins)
south <- unique(crime_w_bins$south)
violent_crime_bins <- unique(crime_w_bins$violent_crime_bins)

#expand grid to obtain all possible outcomes for posterior

newobs_1 <- expand_grid(population_bins, south, violent_crime_bins)



real_p <-add_fitted_draws(newobs_1, fit_1) 

#saving real_p as an rds to make easier when 
#creating the plot associated with the data.
write_rds(real_p, file = "prison/clean_data/real_p.rds")
                  



