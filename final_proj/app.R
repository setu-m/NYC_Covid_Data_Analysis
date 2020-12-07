
library(shiny) 
library(tidyverse)
library(knitr)
library(shinythemes)
library(ggplot2)
library(plotly)
library(gganimate)

# Loading in my data

age_long <- readRDS("processed_data/age_long.RDS")
group_three <- readRDS("processed_data/group_three.RDS")
race_long <- readRDS("processed_data/race_long.RDS")
sex_long <- readRDS("processed_data/sex_long.RDS")
joined_data <- readRDS("processed_data/joined_data.RDS")
case_posterior <- readRDS("processed_data/case_posterior.RDS")
hosp_posterior <- readRDS("processed_data/hosp_posterior.RDS")
death_posterior <- readRDS("processed_data/death_posterior.RDS")

# Beginning the UI

ui <- navbarPage(
    theme = shinytheme("journal"),
    "Going the (Social) Distance in New York City",
    tabPanel("Abstract",
             fluidPage(
                 
                 # Adding some basic CSS styling to the page
                 
                 tags$head(tags$style(HTML("h4 {line-height: 1.6; padding-top: 5px; text-align: justify;}"))),
                 tags$head(tags$style(HTML("h2 {padding-top: 10px; padding-bottom: 0px;}"))),
                 
                 br(),
                 imageOutput("banner_image", width = "100%", height = "100%"),
                 br(),
                 fluidRow(
                     column(1), column(10,
                                       h2("Background", style = "color: darkred"),
                                       h4("When the first wave of COVID-19 struck 
                                          the US, New York City (NYC) was one of 
                                          the hardest hit regions in the country. 
                                          Thousands of lives were lives as ICUs were 
                                          overrun, doctors were overworked, and 
                                          PPE was scarce. NYC has come a long way 
                                          since then, maintaining a low infection 
                                          rate from late September through November. 
                                          However, NYC has recently seen a huge 
                                          uptick in cases and spread of COVID-19. 
                                          In the first week of December, there was 
                                          an average of 3,078 new cases per day, an
                                          increase of 70 percent from the average 
                                          two weeks earlier. Many explain this 
                                          resurgence of COVID-19 due to a combination 
                                          of “living room spread” and COVID fatigue. 
                                          As people become weary of the isolation 
                                          caused by COVID, they become less cautious, 
                                          holding small gathering at their homes, 
                                          unknowingly spreading the virus. With 
                                          the horrors of the past few months in 
                                          mind, how can NYC smartly battle this new wave of infections?"),
                                       h2("Project Objectives", style = "color: darkred"),
                                       h4("In my project, I will explore the links 
                                          between reported social distancing violations 
                                          and COVID-19 cases, deaths, and hospitalizations 
                                          in NYC. I hope these analyses will reveal 
                                          new predictors for COVID-19 surges that 
                                          can be used as a red flag to prepare 
                                          hospital systems with the right resources 
                                          to treat the incoming surge. "),
                                       h2("Findings", style = "color: darkred"),
                                       h4("My analysis revealed that there is 
                                          significant clinical and public health 
                                          relevance in creating a predictive model 
                                          for cases, hospitalizations, and deaths 
                                          due to COVID-19. However, this model 
                                          requires more information in order to 
                                          increase its predictive power. COVID-related 
                                          metrics are informed by not only social 
                                          distancing violations, but also co-morbidities 
                                          of the population, location, and age, 
                                          among other factors. ")
                     )
                 )
             )),      
    
    # Displaying the COVID-19 data by age, race and sex
    
    tabPanel("Data by Category",
             fluidPage(
                 titlePanel(h2("COVID-Factors", style = "color: darkred")),
                 br(),
                 p("This page will showcase the various COVID related statistics per borough
                   related to age, race, and sex. Explore these relationships
                   below!"),
                 strong("Use the selectors below to view different COVID-19 trends per borough"),
                 br(),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Chose Cases, Hospitalized, or Death",
                             c("Cases" = "Case", "Hospitalized" = "Hospitalization",
                               "Death" = "Death")
                                    ),
                         radioButtons(
                             inputId = "selected_variable",            
                             label = "Choose Race, Age, or Sex!",             
                             choices = c("Race", "Age", "Sex")     
                                     )
                                    ),
                     mainPanel(plotOutput("age_plots"))
                            ),
                     p("These plots demonstrate that various boroughs
                        have differing trends in regards to deaths, cases,
                        and hospitalizations. It's clear that different
                        boroughs will need to utilize different approaches
                        in treating the ways the COVID-19 surge materializes
                        in their borough. For example, the Bronx will need to 
                        address the high cases of COVID in their Latino 
                        population. Brooklyn will need to address the high hospitalizations
                        and death count in their Black population. All boroughs
                        need to protect their elderly, as they are more likely
                        to die from COVID. One interesting trend is that Queens 
                        has a relatively high rate of hospitalizations in
                        people aged 45-64, whose causes should be explored.
                        Looking at the data in this way helps us understand
                        how to target various interventions specific to each
                        trend.")
                               )
                               
                ),
    tabPanel("311 Data",
             tabsetPanel(
                 
                # 1st tab: Graph of 311 Data
                 
                 tabPanel("311 Graphs",
                          plotOutput("three"),
                          p("This graph plots the distribution
                             of locations of reported social distancing violations
                             per borough. It's clear that each borough has a different
                             distribution of hot spots for social distancing violations.
                             However, we can see here that stores seem to be a hotspot
                             for violations throughout each borough. This data can serve
                             as a partial justification for why legislation has focused on
                             shutting down non-essential businesses. However, we must keep
                             in mind the unreported social distancing violations, of which
                             violations in residential buildings are the culprit for this
                             most recent wave of COVID.")
                          ),
                 
                 # 2nd tab: Gif of Mapped Social Distancing Violations
                 
                 tabPanel("311 Map",
                          br(),
                          column(5, "This is an animated image that plots the location 
                          of social distancing violations per location type. It is 
                          clear from this image that there is a wide distribution
                          of violations throughout the city, with most of
                          the violations occuring at residential locations.
                          This trend is crucial in supporting the idea 
                          that living room spread is a dangerous spreader of COVID-19,
                          which occured in this past wave of COVID."),
                          column(5, imageOutput("NYC", height = "100%"))
             ))),
    
    # Tab describing componenets of my model
    
    tabPanel("Model",
             tabsetPanel(
             
               # This fist tab will give the most information about the output and
               # visualization of my model
               
               tabPanel("Model Choice", h2("Model Choice", style = "color: darkred"),
                 br(),
                 p("I chose to utilize a standard gaussian model to predict
                   the number of cases, hospitalizations, and deaths based 
                   upon the predictors of location, and social distancing 
                   violations. Ultimately, I wanted to see if there was a linear
                   correlation between social distancing violation counts 
                   and hospitalizations, deaths, and cases in each borough. 
                   In order to see if there is a true linear correlation, 
                   I calculated the linear correlations between each variables,
                   displayed in the table here."),
                 br(),
                 h3("Correlation Table"),
                 fluidRow(
                     includeHTML("processed_data/joined_table1.html")),
                 br(),
                 p("As I fit my model, I wanted to investigate the correlations between
                   the variables, and the strength of their linear correlation. 
                   I noticed that there are not very strong correlations between the
                   three variables and social distancing violations. However, 
                   the strongest correlations exist between social distancing
                   violations and death."),
                 h2("Model Per Covid Variable",  style = "color: darkred"), 
                 br(),
                 p("The model will be setup according to this specification: "),
                     withMathJax(),
                     tags$div(HTML("<script type='text/x-mathjax-config' >
                    MathJax.Hub.Config({
                    tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                    });
                    </script >
                    ")),
                 helpText("$y_i = \\beta_1 location_{brooklyn, i} + \\beta_2 location_{bronx, i} + \\beta_3 location_{manhattan, i} + \\beta_4 location_{queens, i} + \\beta_5 location_{staten_island, i} + \\beta_6 violations_i + \\epsilon_i$"),
                 p("where y represents the number of cases, deaths, or hospitalizations."),
                 br(),
                 h3("Hospitalizations"),
                 fluidRow(
                     column(1),
                     column(5, includeHTML("processed_data/hospe_table.html")),
                     column(5, plotOutput("hosp_posterior", width = 500, height = 500))),
                     br(),
                     p("This model analyses the predictive relationship of location and 
                       reported social distancing violations and hospitalizations per 
                       day per borough. In order to get the value of hospitalizations 
                       per day per borough, you will need to add the beta value for each
                       borough with .45 times the number of social distancing violations 
                       reported in that county. The baseline value of hospitalizations is
                       in Brooklyn, of 21 hospitalizations per day, and you would add 
                       .45 times the number of social distancing violations to get the 
                       total predicted number of hospitalizations per day in Brooklyn. 
                       This model is interesting in that interaction between social 
                       distancing violations and predicted number of hospitalizations, 
                       especially in the case of Manhattan. To get the predicted number 
                       of hospitalizations in Manhattan, you would subtract 7 from 21, 
                       and add .45 times the number of social distancing violations reported 
                       that day in Manhattan."),
                 p("The graph to the right represents the models prediction of 
                   the hospitalizations per day per borough. The model is 
                   predicting hospitalizations on 12/18/2020, where there were 
                   12 social distancing violations in Brooklyn, 10 violations in 
                   Queens, 8 in Manhattan, 4 in Staten Island, and 3 in the Bronx. 
                   These violations were chosen based off of the maximum reported 
                   violations in the respective borough from the month of November. 
                   It’s clear from this graph, especially in the case of Manhattan, 
                   that that the number of reported violations impacts the predicted 
                   value of hospitalizations, which is clear from my model equation as well. 
                   In this graph, Queens will have the highest hospitalizations with
                   this input of violations."),
                 br(),
                 h3("Deaths"),
                 fluidRow(
                    column(1),
                    column(5, includeHTML("processed_data/death_table.html")),
                    column(5, plotOutput("death_posterior", width = 500, height = 500))),
                    br(),
                    p("This model analyses the predictive relationship of location and 
                   reported social distancing violations and death per day per borough. 
                   In order to get the value of deaths per day per borough, you will 
                   need to add the beta value for each borough with .22 times the 
                   number of social distancing violations reported in that county. 
                   The baseline value of deaths is in Brooklyn, of 5.7 deaths per day, 
                   and you would add .22 times the number of social distancing violations
                   to get the total predicted number of deaths per day in Brooklyn. 
                   Once again, this model is interesting in that interaction between 
                   social distancing violations and predicted number of deaths, 
                   especially in the case of Manhattan. To get the predicted number 
                   of deaths in Manhattan, you would subtract 6.8 from 5.7, which is
                   a negative number, and add .22 times the number of social distancing 
                   violations reported that day in Manhattan, which will give you a positive
                   number of deaths per day."),
                 p("The graph to the right represents the models prediction of the 
                   deaths per day per borough. The model is predicting deaths on 
                   12/18/2020, where there were 12 social distancing violations in 
                   Brooklyn, 10 violations in Queens, 8 in Manhattan, 4 in Staten Island, 
                   and 3 in the Bronx. These violations were chosen based off of 
                   the maximum reported violations in the respective borough from 
                   the month of November. Similar to the case of hospitalizations, 
                   the number of reported violations impacts the predicted value of 
                   death. In this graph, Queens also will have the highest death 
                   with this input of violations."),
                 br(),
                 h3("Cases"),
                 fluidRow(
                     column(1),
                     column(5, includeHTML("processed_data/case_table.html")),
                     column(5, plotOutput("case_posterior", width = 500, height = 500))),
                     br(),
                     p("This model analyses the predictive relationship of location and 
                     reported social distancing violations and cases per day per borough. 
                     In order to get the value of cases per day per borough, you will 
                     need to add the beta value for each borough with 1.6 times the number 
                     of social distancing violations reported in that county. The baseline 
                     value of cases is in Brooklyn, of 128 cases per day, and you would add 
                     1.6 times the number of social distancing violations to get the total 
                     predicted number of cases per day in Brooklyn. Once again, this model 
                     is interesting in that interaction between social distancing violations 
                     and predicted number of cases, especially in the case of Manhattan. 
                     To get the predicted number of cases in Manhattan, you would subtract 
                     13 from 128, and add 1.6 times the number of social distancing violations 
                     reported that day in Manhattan."),
                 p("The graph to the right represents the models prediction of 
                   the cases per day per borough. The model is predicting cases on 
                   12/18/2020, where there were 12 social distancing violations 
                   in Brooklyn, 10 violations in Queens, 8 in Manhattan, 4 in 
                   Staten Island, and 3 in the Bronx. These violations were chosen 
                   based off of the maximum reported violations in the respective 
                   borough from the month of November. Similarly, the number of 
                   reported violations impacts the predicted value of cases. In 
                   this graph, Queens also will have the highest cases with this 
                   input of violations."),
                 ),
             tabPanel("Model Implications",
                 h2("Model Implications", style = "color: darkred"),
                 p("This model, as seen from the correlation values, does not fit 
                   the data as well as possible. This is highly likely because there 
                   are many important predictive variables missing from the model. 
                   For example, there are other factors such as comorbidities, age, 
                   race, and mask wearing habits that can impact case counts, 
                   hospitalizations, and deaths. Furthermore, the current measure 
                   of social distancing violations is a reported number and does 
                   not represent the actual number of social distancing violations 
                   that occur in each borough. However, this model does provide a 
                   valuable base upon which to base these future predictive models 
                   off of, and demonstrates the potential power of such a model 
                   to inform policies and bring resources to areas that are predicted 
                   to be severely impacted by COVID-19. For example, the posterior 
                   distributions demonstrate that if we know the reported social 
                   distancing, we can predict hospitalizations, deaths, and cases 
                   of COVID in a borough. Therefore, healthcare institutions can 
                   funnel that information into a model to better prepare their 
                   hospitals for a wave of new cases and hospitalizations and 
                   prevent future deaths – which is something so critical in our 
                   current second wave. Furthermore, the distribution of various 
                   311 reports by location can reveal new areas of improvement for 
                   rightening social distancing restrictions. Having the ability 
                   to display the data in this format makes it easier to draw conclusions. 
                   Therefore, while this model is not perfect, there is still much 
                   to be learned from these conclusions that can be improved upon 
                   with other sources of data.")
                 ))),
    tabPanel("About",
             includeHTML(rmarkdown::render("about.Rmd"))
             ))

             
# Beginning the server             

server <- function(input, output) {
   
# Generating the variable plots
    
     output$age_plots <- renderPlot({
         joined_data %>%
             filter(selected_variable == input$selected_variable,
                    type == input$plot_type) %>% 
             ggplot(aes(x = group, y = av, fill = location)) +
                     geom_col(position = "dodge") +
                     theme_bw() +
             ggtitle(paste0("Covid ", input$plot_type, "s per ", input$selected_variable)) +
                     labs(
                          x = paste0(input$selected_variable),
                          y = "Count") +
                     scale_fill_manual(name = "Borough",
                                       breaks = c("BK", "BX", "MN", "QN", "SI"),
                                       labels = c("Brooklyn", "Bronx", "Manhattan",
                                                   "Queens", "Staten Island"),
                                       values = c("#1D2F6F", "#8390fa", "#fac748",
                                           "#f9e9ec", "#f88dad"))
        
    })
   
# Generating the social distancing per location plot
     
      output$three <- renderPlot({group_three})
      
# Generating the posterior distributions of cases, hospitalizations, and deaths 

      output$hosp_posterior <- renderPlot({hosp_posterior})
      output$case_posterior <- renderPlot({case_posterior})
      output$death_posterior <- renderPlot({death_posterior})
      
# Creating banner image
      
     output$banner_image <- renderImage({
          list(src = 'hercules.png', 
               width = "80%",
               style = "display: block; margin-left: auto; margin-right: auto;")}, 
          deleteFile = FALSE)
      
# Plots the gif of New York City Violations
    
    output$NYC <- renderImage({
        
        # Return a list containing the filename
        
        list(src = "processed_data/violations.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
}

# Run the application

shinyApp(ui, server)