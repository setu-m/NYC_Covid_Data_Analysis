
library(shiny) 
library(tidyverse)
library(knitr)
library(shinythemes)

# Loading in my data

# source("raw_data.R")
age_long <- readRDS("age_long.RDS")
group_three <- readRDS("group_three.RDS")
race_long <- readRDS("race_long.RDS")
sex_long <- readRDS("sex_long.RDS")
joined_data <- readRDS("joined_data.RDS")


ui <- navbarPage(
    "NYC Covid Data",
    theme = shinytheme("journal"),
    tabPanel("Age Data",
             fluidPage(
                 theme = shinytheme("cerulean"),
                 titlePanel("Age Distribution"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Cases" = "case", "Hospitalized" = "hosp",
                               "Death" = "death")
                         ),
                         radioButtons(
                             inputId = "selected_variable",            
                             label = "Choose Race, Age, or Sex!",             
                             choices = c("Race", "Age", "Sex")     
                         )),
                     mainPanel(plotOutput("age_plots"),
                               plotOutput("three")))
                               
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About",
             includeHTML("about.html")))
             
             

server <- function(input, output) {
    output$age_plots <- renderPlot({
        if(input$plot_type == "case") {age_long %>%
                filter(type == "CASE_COUNT") %>%
                ggplot(aes(x = group, y = av, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Covid Cases per age group per NYC county",
                     x = "Age Groups",
                     y = "Count") 
        } else{if(input$plot_type == "hosp") {age_long %>%
                filter(type == "HOSPITALIZED_COUNT") %>%
                ggplot(aes(x = group, y = av, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Hospitalizations Cases per
                     age group per NYC county")}
         else {age_long %>%
                filter(type == "DEATH_COUNT") %>%
                ggplot(aes(x = group, y = av, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Deaths Cases per 
                     age group per NYC county")}
            
         }
        
    })
    output$three <- renderPlot({group_three})
            
        
}

shinyApp(ui, server)