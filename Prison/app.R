library(shiny)
library(tidyverse)
library(gtsummary)
library(shinythemes)
library(gt)
library(broom.mixed)
library(viridis)
library(viridisLite)
library(readr)


state_crime <- read_rds("clean_data/state_crime_data.rds")
crime_w_bins <- read_rds("clean_data/crime_w_bins_data.rds")
mortality_numbers <- read_rds("clean_data/mortality_numbers_data.rds")
fit_1 <- read_rds("clean_data/fit_1.rds") 
real_p_data <- read_rds("clean_data/real_p.rds")


ui <- navbarPage(
    "Behind Bars",
    tabPanel("Overview",
             fluidPage(theme = shinytheme("darkly"),
                       titlePanel("Crime and Mortality Trends from 2001-2016"),
                       sidebarLayout(
                           sidebarPanel(
                               selectInput(
                                   "plot_type",
                                   "Plot Type",
                                   c(state_crime$name)
                               )),
                           mainPanel(plotOutput("state_crime"),
                                     plotOutput("mortality_plot"))
                       ))),
    tabPanel("Model",
             titlePanel("Model Discussion"),
             h3("Preliminary Thoughts"),
             p("From my initial data exploration I was curoious about the 
               relationship between northern and southern states and how 
               the amount of prisoners each state has varies. I figured that 
               a good indicator of this would be the population of the state in
               general because more citizens creats more opportunity for crime.
               I also know that the relationship between violent crime 
               and prisoner count has to be linear because those who 
               commit violent crimes will most likely be put in prison." ),
             
             h3("Mathematical Model"),
             withMathJax(
                 '$$ PrisonerCountInThousands_i = \\beta_0 + 
                      \\beta_1 PopulationBins_i + \\beta_2 South_i + 
                      \\beta_3 ViolentCrimeInThousands_i + 
                      \\beta_4 South*ViolentCrimeInThousands_i + 
                     \\epsilon_i $$'),
             h3("Table"),
             gt_output("model_prisoner_count"),
             
             h3("Analysis"),
             p("From this table we can see that population has a great 
      effect on the prisoner count. This is expected because as 
      population increases, there are more people available to 
      commit crimes. Going along with this, we see that the amount
      of violent crime also has a great effect. A linear relationship 
      can be found between violent crime and prisoner count because 
      most punishements for violent crime involve prison time."),
             plotOutput("model_plot")),
    
    
    tabPanel("Deeper Dive into Crime Type",
             titlePanel("Crime Trends Per State"), 
             sidebarLayout(
                 sidebarPanel(
                     selectInput(
                         "state",
                         "State",
                         c(crime_w_bins$jurisdiction)
                     ),
                     selectInput(
                         "crime",
                         "Crime",
                         c("Robbery","Vehicle Theft", "Murder Manslaughter",
                           "Rape", "Aggrevated Assault","Burglary", "Larceny"
                         ))
                 ),
                 mainPanel(plotOutput("crime_per_state")))
             ),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background Motivations"),
             p("I have always been intrigued about the criminal justice 
               system, but have never before gotten to look at statistics 
               pertaining to crimes and prisoners across each state. I have 
               done research before related to programs in place at prisons, 
               but have never explored the types of people and crimes they are
               convicted of.I gathered my data from a website called data.world
               which is a sit that contains data sets for many different 
               topics. The data I use comes from the Bureau of Justice
               Statistics."),
             
             h3("About Me"),
             p("My name is Caroline Behrens and I study Economics. 
             You can reach me at Cbehrens@college.harvard.edu."),
             p(tags$a(href ="https://github.com/CarolineBehrens/Behind_Bars_Analysis")))) 



#Define server logic required to draw a histogram

server <- function(input, output){
    
    # state_crime <- read_rds("clean_data/state_crime_data.rds")
    # crime_w_bins <- read_rds("clean_data/crime_w_bins_data.rds")
    # mortality_numbers <- read_rds("clean_data/mortality_numbers_data.rds")
    # fit_1 <- read_rds("clean_data/fit_1.rds") 
    # real_p_data <- read_rds("clean_data/real_p.rds")
    # 
    output$state_crime <- renderPlot({ 
        state_crime %>%
            filter(name == input$plot_type) %>%
            ggplot(aes(x = year, y = total/1000, color = type)) +
            geom_point(size = 3) +
            labs(title = "Crime Variaton Sy State", x = "Year")
        
    })
    
    output$mortality_plot <- renderPlot({ 
         qplot(Year, Mortality_Number, data = mortality_numbers, 
                                geom= "violin", fill = south) +
            labs(title = "Mortality Numbers", x = "Year", y = "count")
        })
    
    output$model_prisoner_count <- render_gt(tbl_regression(fit_1,
                                                intercept = TRUE,
                                                 estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
                                                 as_gt() %>%
                                                 tab_header(title = "Prisoner Count Varies Greatly by Population") %>%
                                                 tab_source_note(md("Source: Data.world ")))
    
    
 

    output$model_plot <- renderPlot(real_p_data %>%
                                        ggplot(aes(x = population_bins, y = .value, colour = violent_crime_bins)) +
                                        geom_jitter(alpha = .5) +
                                        geom_smooth(formula = y ~ x,
                                                    method = "lm") +
                                        #facet wrap is key to show the difference between geographical location 
                                        facet_wrap(~south) +
                                        labs(title = "Prisoner Count", subtitle = "Geographical location and population size have great effects",
                                             x = "Population Bins", y = "Prisoner Count  (Thousands)"))
    
    #case when function is allowing for multiple drop downs for 
    #second interactive plot
    output$crime_per_state <- renderPlot({
        case_when(
            input$crime == "Robbery" ~ list(crime_w_bins %>%
                                                filter(jurisdiction == input$state) %>%
                                                ggplot(aes(y = robbery, x = year)) + 
                                                geom_point(color = 'purple') + 
                                                geom_smooth()),
            input$crime == "Vehicle Theft" ~ list(crime_w_bins %>%
                                                      filter(jurisdiction == input$state) %>%
                                                      ggplot(aes(y = vehicle_theft, x = year)) + 
                                                      geom_point(color = 'blue') + 
                                                      geom_smooth()),
            input$crime == "Murder Manslaughter" ~ list(crime_w_bins %>%
                                                            filter(jurisdiction == input$state) %>%
                                                            ggplot(aes(y = murder_manslaughter, x = year)) + 
                                                            geom_point(color = 'red') +
                                                            geom_smooth()),
            input$crime == "Rape" ~ list(crime_w_bins %>%
                                             filter(jurisdiction == input$state) %>%
                                             ggplot(aes(y = rape_legacy, x = year)) + 
                                             geom_point(color = 'green') +
                                             geom_smooth()),
            input$crime == "Aggrevated Assault" ~ list(crime_w_bins %>%
                                                           filter(jurisdiction == input$state) %>%
                                                           ggplot(aes(y = agg_assault, x = year)) + 
                                                           geom_point(color = 'blue') +
                                                           geom_smooth()),
            input$crime == "Burglary" ~ list(crime_w_bins %>%
                                                 filter(jurisdiction == input$state) %>%
                                                 ggplot(aes(y = burglary, x = year)) + 
                                                 geom_point(color = 'red') +
                                                 geom_smooth()),
            input$crime == "Larceny" ~ list(crime_w_bins %>%
                                                filter(jurisdiction == input$state) %>%
                                                ggplot(aes(y = larceny, x = year)) + 
                                                geom_point(color = 'green') +
                                                geom_smooth())
            
        )
        
        
        
        
    })
} 



# Run the application 
shinyApp(ui = ui, server = server)
