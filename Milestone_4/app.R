
library(shiny) 
library(tidyverse)
library(knitr)
library(shinythemes)
library(ggplot2)
library(plotly)

# Loading in my data


age_long <- readRDS("processed_data/age_long.RDS")
group_three <- readRDS("processed_data/group_three.RDS")
race_long <- readRDS("processed_data/race_long.RDS")
sex_long <- readRDS("processed_data/sex_long.RDS")
joined_data <- readRDS("processed_data/joined_data.RDS")

# Creating an equation for my model


# Beginning the UI

ui <- navbarPage(
    theme = shinytheme("journal"),
    "NYC Covid Data",
    tabPanel("Data by Category",
             fluidPage(
                 titlePanel("COVID-Factors"),
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
                         )),
                     mainPanel(plotOutput("age_plots"),
                               p("There is a relationship with the factors and the 
                               various counts. I will need to normalize my y-axis."),
                               ))
                               
             )),
    tabPanel("311 Data",
             tabsetPanel(
                 
                # 1st tab: Graph of 311 Data
                 
                 tabPanel("311 Graphs",
                          plotOutput("three")
                 ),
                 tabPanel("311 Map")
             )),
    tabPanel("Model",
             tabsetPanel(
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
             br(),
             h3("Hospitalizations"),
             fluidRow(
                 column(1),
                 column(5, includeHTML("processed_data/hospe_table.html")),
                 column(5, "This model analyses the predictive relationship of location and 
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
                   that day in Manhattan.")),
             br(),
             h3("Deaths"),
             fluidRow(
                column(1),
                column(5, includeHTML("processed_data/death_table.html")),
                column(5, "This model analyses the predictive relationship of location and 
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
               number of deaths per day.")),
             br(),
             h3("Cases"),
             fluidRow(
                 column(1),
                 column(5, includeHTML("processed_data/case_table.html")),
                 column(5, "This model analyses the predictive relationship of location and 
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
                 reported that day in Manhattan.")),
             ),
             tabPanel("Model Implications",
             h2("Model Implications", style = "color: darkred"),
             p("Making this model is timely as cases are rising in New York.
               We can use these predictive models to caution New Yorkers, 
               especially in Brooklyn, that an increase in social distancing
               violations may serve as a predictor of deaths. However, this model
               also demonstrates that social distancing is just one factor
               that impacts cases, deaths, and hospitalzations.")
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
                     scale_fill_manual(name = "County",
                                       breaks = c("BK", "BX", "MN", "QN", "SI"),
                                       labels = c("Brooklyn", "Bronx", "Manhattan",
                                                   "Queens", "Staten Island"),
                                       values = c("#1D2F6F", "#8390fa", "#fac748",
                                           "#f9e9ec", "#f88dad"))
        
    })
    output$three <- renderPlot({group_three})
            
        
}

shinyApp(ui, server)