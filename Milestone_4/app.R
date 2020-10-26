
library(shiny) 
library(tidyverse)
library(knitr)
library(shinythemes)
library(ggplot2)

# Loading in my data

# source("raw_data.R")
age_long <- readRDS("age_long.RDS")
group_three <- readRDS("group_three.RDS")
race_long <- readRDS("race_long.RDS")
sex_long <- readRDS("sex_long.RDS")
joined_data <- readRDS("joined_data.RDS")


ui <- navbarPage(
    theme = shinytheme("journal"),
    "NYC Covid Data",
    tabPanel("Age Data",
             fluidPage(
                 titlePanel("Age Distribution"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Outcome Type",
                             c("Cases" = "Case", "Hospitalized" = "Hospitalization",
                               "Death" = "Death")
                         ),
                         radioButtons(
                             inputId = "selected_variable",            
                             label = "Choose Race, Age, or Sex!",             
                             choices = c("Race", "Age", "Sex")     
                         )),
                     mainPanel(plotOutput("age_plots"),
                               p("There is a relationship with age and the 
                               various counts. I will need to decide how I want
                               to display these relationships."),
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
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About",
             includeHTML(rmarkdown::render("about.Rmd"))
             ))

             
             

server <- function(input, output) {
   
# Generating the age plots
    
     output$age_plots <- renderPlot({
         joined_data %>%
             filter(selected_variable == input$selected_variable,
                    type == input$plot_type) %>% 
             ggplot(aes(x = group, y = av, fill = location)) +
                     geom_col(position = "dodge") +
                     theme_bw() +
             ggtitle(paste0("Covid ", input$plot_type, "s per ", input$selected_variable)) +
                     labs(
                          x = "Age Groups",
                          y = "Count") +
                     scale_fill_manual(name = "County",
                                       breaks = c("BK", "BX", "MN", "QN", "SI"),
                                       labels = c("Brooklyn", "Bronx", "Manhattan",
                                                   "Queens", "Staten Island"),
                                       values = c("#1D2F6F", "#8390fa", "#fac748",
                                           "#f9e9ec", "#f88dad"))
         
         
         
         
         
         
         
        # if(input$plot_type == "case") {age_long %>%
        #         filter(type == "CASE_COUNT") %>%
        #         ggplot(aes(x = group, y = av, fill = location)) +
        #         geom_col(position = "dodge") +
        #         theme_bw() +
        #         labs(title = "Covid Cases per age group per NYC county",
        #              x = "Age Groups",
        #              y = "Count") +
        #         scale_fill_manual(name = "County",
        #                           breaks = c("BK", "BX", "MN", "QN", "SI"),
        #                           labels = c("Brooklyn", "Bronx", "Manhattan",
        #                                       "Queens", "Staten Island"),
        #                           values = c("#1D2F6F", "#8390fa", "#fac748",
        #                               "#f9e9ec", "#f88dad"))
        # } else{if(input$plot_type == "hosp") {age_long %>%
        #         filter(type == "HOSPITALIZED_COUNT") %>%
        #         ggplot(aes(x = group, y = av, fill = location)) +
        #         geom_col(position = "dodge") +
        #         theme_bw() +
        #         labs(title = "Hospitalizations Cases per
        #              age group per NYC county") +
        #         scale_fill_manual(name = "County",
        #                           breaks = c("BK", "BX", "MN", "QN", "SI"),
        #                           labels = c("Brooklyn", "Bronx", "Manhattan",
        #                                      "Queens", "Staten Island"),
        #                           values = c("#1D2F6F", "#8390fa", "#fac748",
        #                                      "#f9e9ec", "#f88dad"))}
        #  else {age_long %>%
        #         filter(type == "DEATH_COUNT") %>%
        #         ggplot(aes(x = group, y = av, fill = location)) +
        #         geom_col(position = "dodge") +
        #         theme_bw() +
        #         labs(title = "Deaths Cases per 
        #              age group per NYC county") +
        #          scale_fill_manual(name = "County",
        #                            breaks = c("BK", "BX", "MN", "QN", "SI"),
        #                            labels = c("Brooklyn", "Bronx", "Manhattan",
        #                                       "Queens", "Staten Island"),
        #                            values = c("#1D2F6F", "#8390fa", "#fac748",
        #                                       "#f9e9ec", "#f88dad"))}
        #     
        #  }
        
    })
    output$three <- renderPlot({group_three})
            
        
}

shinyApp(ui, server)