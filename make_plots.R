library(readr) 
library(tidyverse)
library(gtsummary) 
library(gt) 
library(broom.mixed) 


#created this r script to better visualize 
#the plots that I want to make. Here, I could 
#run each plot I created to make sure they 
#functioned correctly before moving to the app.



#loaded in data sets created in other r script.

state_crime_data <- read_rds("clean_data/state_crime_data.rds")
mortality_data <- read_rds("clean_data/mortality_numbers_data.rds")

#created a plot now to show the difference between north and south 
#mortality numbers in prisons 

mortality_plot <- qplot(Year, Mortality_Number, data = mortality_data, 
                        geom= "violin", fill = south) +
  labs(title = "Mortality Numbers", x = "Year", y = "count")



#loading in the fit through rds so it runs quicker
#and easier for the regression tbl.

fit_1<- read_rds("clean_data/fit_1.rds")




model_prisoner_count <-tbl_regression(fit_1,
                                      intercept = TRUE,
                                      estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "Prisoner Count Varies Greatly by Population") %>%
  tab_source_note(md("Source: Data.world "))

gtsave(model_prisoner_count, filename = "model_prisoner_count.png")


#saving the regression table as an rds file 

write_rds(model_prisoner_count, file = "prison/clean_data/model_prisoner_count.rds")



#real p data is the fitted draws of the 
#newobs and fit created in the gather 
#data r script. Loading it here so I 
#can then create the plot using the data.

real_p_data <- read_rds("prison/clean_data/real_p.rds")


#making a nice looking plot to show the difference between 
#the variables I selected for the newobs. Color argument 
#is nice because it allows me to show my third variable 
#which plays  great impact on my data 
model_plot <- real_p_data %>%
  ggplot(aes(x = population_bins, y = .value, colour = violent_crime_bins)) +
  geom_jitter(alpha = .5) +
  geom_smooth(formula = y ~ x,
              method = "lm") +
  #facet wrap is key to show the difference between geographical location 
  facet_wrap(~south) +
  labs(title = "Prisoner Count", subtitle = "Geographical location and population size have greatest effects",
       x = "Population Bins", y = "Prisoner Count  (Thousands)")


