
library(shiny) 
library(tidyverse)


   

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
                     mainPanel(plotOutput("age_plots")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
             p(htmlOutput("about")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Setu and I study stem cell biology. 
             You can reach me at setumehta@college.harvard.edu.")))

server <- function(input, output) {
    getPage<-function() {
        return(includeHTML("about.html"))
    }
    output$about<-renderUI({getPage()})
    output$age_plots <- renderPlot({
        if(input$plot_type == "case") {age_long %>%
                filter(type == "CASE") %>%
                ggplot(aes(x = group, y = count, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Covid Cases per age group per NYC county")
        } else{if(input$plot_type == "hosp") {age_long %>%
                filter(type == "HOSPITALIZED") %>%
                ggplot(aes(x = group, y = count, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Hospitalizations Cases per 
                     age group per NYC county")}
         else {age_long %>%
                filter(type == "DEATH") %>%
                ggplot(aes(x = group, y = count, fill = location)) +
                geom_col(position = "dodge") +
                theme_bw() +
                labs(title = "Deaths Cases per 
                     age group per NYC county")}
            
         }
        
    })
            
        
}

shinyApp(ui, server)