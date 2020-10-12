#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(PPBDS.data)
library(ggplot2)
library(tidyverse)

bob <- PPBDS.data::qscores

    

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Health Statistics",
    tabPanel("Plots",
             fluidPage(
                 titlePanel("Model Title"),
                 mainPanel(plotOutput("line_plot"),
             ))),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Setu Mehta and I study stem cell biology. 
             You can reach me at setumehta@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
    bob %>%
            filter(department == "MATH") %>%
            ggplot(aes(enrollment, rating)) +
            geom_point()
        
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
