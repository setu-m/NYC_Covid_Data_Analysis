
library(shiny) 
library(tidyverse)
library(knitr)
library(shinythemes)
library(ggplot2)

# Loading in my data


age_long <- readRDS("processed_data/age_long.RDS")
group_three <- readRDS("processed_data/group_three.RDS")
race_long <- readRDS("processed_data/race_long.RDS")
sex_long <- readRDS("processed_data/sex_long.RDS")
joined_data <- readRDS("processed_data/joined_data.RDS")


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
             h2("Model Choice",  style = "text-align: center; margin-left: 40px; 
             margin-right: 40px; line-height: 1.4;"),
             br(),
             p("I chose to utilize a standard gausian model to predict
               the number of cases, hospitalizations, and deaths based 
               upon the predictors of location, and social distancing 
               violations. Ultimately, I wanted to see if there was a linear
               correlation between social distancing violation counts 
               and hospitalizations, deaths, and cases in each borough. 
               In order to see if there is a true linear correlation, 
               I calculated the linear correlations between each variables,
               displayed in the table here."),
             br(),
             fluidRow(
                 includeHTML("processed_data/joined_table1.html")),
             br(),
             p("These correlations will put the predictions of my model in 
               context. There are not very strong correlations between the
               three variables and social distancing violations. However, 
               the strongest correlations exist between social distancing
               violations and death."),
             br(),
             h2("Model Per Covid Variable",  style = "text-align: center; margin-left: 40px; 
             margin-right: 40px; line-height: 1.4;"),
             br(),
             includeHTML("processed_data/hospe_table.html"),
             br(),
             p("Hello"),
             includeHTML("processed_data/death_table.html"),
             br(),
             includeHTML("processed_data/case_table.html"),
             h2("Model Implications",  style = "text-align: center; margin-left: 40px; 
             margin-right: 40px; line-height: 1.4;"),
             p("Making this model is timely as cases are rising in New York.
               We can use these predictive models to caution New Yorkers, 
               especially in Brooklyn, that an increase in social distancing
               violations may serve as a predictor of deaths. However, this model
               also demonstrates that social distancing is just one factor
               that impacts cases, deaths, and hospitalzations.")
             ),
    tabPanel("About",
             includeHTML(rmarkdown::render("about.Rmd"))
             ))

             
             

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