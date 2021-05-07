library(shiny)
library(tidyverse)
library(gtsummary)
library(shinythemes)
library(gt)
library(broom.mixed)
library(TMB)
library(viridis)
library(viridisLite)
library(readr)

#Loading data sets previously cleaned up in another r script. 
#In these sets I created new variables and changed column names 
#so the data could be better understood.

state_crime <- read_rds("clean_data/state_crime_data.rds")
crime_w_bins <- read_rds("clean_data/crime_w_bins_data.rds")
mortality_numbers <- read_rds("clean_data/mortality_numbers_data.rds")
fit_1 <- read_rds("clean_data/fit_1.rds") 
real_p_data <- read_rds("clean_data/real_p.rds")

#Defining the UI for this application 

ui <- navbarPage(
    "Behind Bars",
    tabPanel("Overview",
             
#adding the darkly theme to make the app more appealing

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

#Showing the model I created here which evaluates the 
#effects of different variables on prisoner count. Also 
#showed the mathematical equation so it is easier to 
#see the variables I am working with.

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
#sourced my regression table as an image here 
#because was running into issues when outputing
#it in the server
             img(src = "model_prisoner_count.png", align = "center",
                 height = "80%", width = "80%"),
             
             h3("Analysis"),
             p("From this table we can see that population has a great 
      effect on the prisoner count. This is expected because as 
      population increases, there are more people available to 
      commit crimes. We can think about big cities are likely to have 
      lots of crime. Going along with this, we see that the amount
      of violent crime also has a great effect. A linear relationship 
      can be found between violent crime and prisoner count because 
      most punishements for violent crime involve prison time. My 
      predictions from my preliminary thoughts were correct becaue 
      as seen on the table, violent crime has a positive effect on 
      prisoner count. We can also visually see the difference between
      each population bin (1 being the smallest and 5 being the biggest).
      There is a significant difference between prisoner count of states
      falling into the population bin of 1 compared to 5. It is also 
      interesting to not that each population bin for southern states
      (located under the TRUE column) has a slightly higher prisoner 
       count compared to the northern states."),
             plotOutput("model_plot")),

#Wanted to have a more in depth look at the different
#crimes and how they trended over the years for each 
#state. In the overview panel I grouped them 
#together and looked at violent crime vs. property
#crime so here I found it helpful to look at 
#each crime individually. 

    
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

#About panel discussing motivations and link 
#to my github account for the viewers. 

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
             p(tags$a(href ="https://github.com/CarolineBehrens/Glimpse_Behind_Bars")))) 



#Define server logic required to draw a histogram

server <- function(input, output){
 
#At first I tried loading the data sets here, 
#but found that it caused errors so then 
#I found it better to load them under libraries.

    output$state_crime <- renderPlot({ 
        
#Filter argument here is essential because
#it allows me to make the plot interactive. 
        
        state_crime %>%
            filter(name == input$plot_type) %>%
            
#Scaled the y axis to make easier to read
            
            ggplot(aes(x = year, y = total/1000, color = type)) +
            geom_point(size = 3) +
            labs(title = "Crime Variaton Sy State", x = "Year")
        
    })
    
    output$mortality_plot <- renderPlot({ 
         qplot(Year, Mortality_Number, data = mortality_numbers, 
               
#set geom to violin for unique plot shape 

                                geom= "violin", fill = south) +
            labs(title = "Mortality Numbers", x = "Year", y = "count")
        })
    


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
#second interactive plot. Needed to make individual 
#plots for each crime which was tedious but not difficult.
#Inside each plot I set the color argument to different
#colors to make the dots on the graph unique. 
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
