library(readr) 
library(tidyverse)
library(gtsummary) 
library(gt) 
library(broom.mixed) 


state_crime_data <- read_rds("prison/clean_data/state_crime_data.rds")
mortality_data <- read_rds("prison/clean_data/mortality_numbers_data.rds")

#created a plot now to show the difference between north and south 
#mortality numbers in prisons 

mortality_plot <- qplot(Year, Mortality_Number, data = mortality_data, 
                        geom= "violin", fill = south) +
  labs(title = "Mortality Numbers", x = "Year", y = "count")

ggsave(filename = "prison/mortality_plot.png", 
       plot = mortality_plot, 
       dpi = 300)






fit_1_data<- read_rds("prison/clean_data/fit_1.rds")




model_prisoner_count <-tbl_regression(fit_1_data,
                                      intercept = TRUE,
                                      estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
  as_gt() %>%
  tab_header(title = "Prisoner Count Varies Greatly by Population") %>%
  tab_source_note(md("Source: Data.world "))

write_rds(model_prisoner_count, file = "prison/clean_data/model_prisoner_count.rds")

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



ggsave(filename = "prison/model_plot.png", 
       plot = model_plot, 
       dpi = 300)