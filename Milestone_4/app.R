
library(shiny) 
library(tidyverse)


includeMarkdown("raw_data.Rmd")   

ui <- navbarPage(
    "NYC Covid Data",
    tabPanel("Age Data",
             fluidPage(
                 titlePanel("Age Distribution"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Cases" = "case", "Hospitalized" = "hosp",
                               "Death" = "death")
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
                filter(type == "CASE") %>%
                ggplot(aes(x = group, y = av, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Covid Cases per age group per NYC county")
        } else{if(input$plot_type == "hosp") {age_long %>%
                filter(type == "HOSPITALIZED") %>%
                ggplot(aes(x = group, y = av, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Hospitalizations Cases per 
                     age group per NYC county")}
         else {age_long %>%
                filter(type == "DEATH") %>%
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