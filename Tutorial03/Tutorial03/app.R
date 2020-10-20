#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(tidyverse)
library(lubridate)
library(bupaR)
library(eventdataR)
library(edeaR)
library(processmapR)
library(xesreadR)
library(DiagrammeR)

# Load the process files

process_semester<-read_xes("process.xes")
process_grades<-read_xes("process_graded.xes")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Process Analytics"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Filters",  icon = icon("dashboard")),
            sliderTextInput(
                inputId = "years",
                label = "Choose Cohorts:", 
                choices = 2000:2017,
                selected =c(2000,2017)
            ),
            selectInput("semester", "Choose Starting Semester:",
                        choices = list("Both"=0,
                                       "Fall"=2,
                                       "Spring"=1), 
                        selected = "Both"),
            sliderInput("coverage", "Percentage of Cases Covered",  min = 0, max = 100, value = 30)
        )
    ),
    dashboardBody(
        tabBox(height = "1100px", width = "1000px",
               
               tabPanel(title = tagList(icon("project-diagram", 
                                             class = "fas fa-project-diagram"),
                                        "SEMESTER COURSE"),
                        box(grVizOutput("Pr_map_semester"), 
                            status = "primary", 
                            solidHeader = TRUE,
                            title = "PROCESS MAP", 
                            width = 12, 
                            height = 612, 
                            collapsible = TRUE),                
        
                            
                     ),
               tabPanel(title = tagList(icon("graduation-cap", 
                                             class="fas fa-graduation-cap"),
                                        "GRADES"),
                        box(grVizOutput("Pr_map_grades"), 
                            status = "primary", 
                            solidHeader = TRUE,
                            title = "PROCESS MAP", 
                            width = 12, 
                            height = 612, 
                            collapsible = TRUE),                
                        
                        
               )
               
               
        )
        # Second tab content
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Pr_map_semester <- renderGrViz({
        cohort_start=input$years[1]
        cohort_end=input$years[2]
        print(cohort_start)
        print(cohort_end)
        if(input$semester!=0){
            sem_name=ifelse(input$semester==1,"Spring","Fall")
            process_semester %>% 
                filter(start_year>=cohort_start & start_year<=cohort_end)%>%
                filter(start_semester==sem_name)%>%
                filter_trace_frequency(percentage = input$coverage/100)%>%
                process_map(fixed_edge_width = FALSE, render = TRUE)
        
        }
        else
        {
            process_semester %>% 
                filter(start_year>=cohort_start & start_year<=cohort_end)%>%
                filter_trace_frequency(percentage = input$coverage/100)%>%
            process_map(fixed_edge_width = FALSE, render = TRUE)
        }
    })
    
    output$Pr_map_grades <- renderGrViz({
        
        process_grades %>% process_map(fixed_edge_width = FALSE, render = TRUE)
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
