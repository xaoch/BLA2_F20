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

classificationModel= readRDS("./classification_model.rds")
print("model")
print(classificationModel)
regressionModel= readRDS("./regression_model.rds")

ui <- dashboardPage(
    dashboardHeader(title = "Prediction Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            h3("Prediction Types"),
            menuItem("Classification", tabName = "classification", icon = icon("dashboard")),
            menuItem("Regression", tabName = "regression", icon = icon("th")),
            h3("VLE Data"),
                numericInput("total_clicks", "Total Clicks", value = 100),
                numericInput("total_elements", "Total Elements", value = 100),
                sliderInput("active_days", "Active Days",  min = 0, max = 50, value = 25),
                numericInput("average_daily_clicks", "Average Daily Clicks",  value = 25),
                numericInput("average_elements", "Average Daily Elements",  value = 25),
            h3("Assessment Data"),
                sliderInput("avgScore", "Average Score",  min = 0, max = 100, value = 50),
                numericInput("delivered", "Deliverd Assessments", value = 5),
                numericInput("sumDelays", "Delayed Days",  value = 0)
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "classification",
                    fluidRow(
                        box(title="Student Info",
                            radioButtons("gender", "Gender",
                                         choices = list("Male" = "M", "Female" = "F"),selected = "M"),
                            selectInput("region", "Region",
                                        choices = list("East Anglian Region"="East Anglian Region",
                                                       "Yorkshire Region"="Yorkshire Region",
                                                       "East Midlands Region"="East Midlands Region",
                                                       "South East Region"="South East Region",
                                                       "North Western Region"= "North Western Region",
                                                       "Scotland"="Scotland",
                                                       "South West Region"="South West Region",
                                                       "West Midlands Region"="West Midlands Region",
                                                       "Wales"="Wales",
                                                       "Ireland"="Ireland",             
                                                       "South Region"="South Region",
                                                       "London Region"="London Region",
                                                       "North Region"="North Region"),selected = "London Region"),
                            selectInput("highest_education", "Highest Level of Education",
                                        choices =list("A Level or Equivalent",
                                                      "Lower Than A Level",
                                                      "HE Qualification",
                                                      "Post Graduate Qualification",
                                                      "No Formal quals"
                                        ), selected="HE Qualification"),
                            selectInput("imd_band", "IMD Band",
                                        choices =list("0-10%", 
                                                      "20-30%",
                                                      "30-40%",
                                                      "40-50%",
                                                      "50-60%",
                                                      "60-70%",
                                                      "70-80%",
                                                      "80-90%",
                                                      "90-100%"
                                        ), selected="50-60%"),
                            selectInput("age_band", "Age Band",
                                        choices =list("0-35",
                                                      "35-55",
                                                      "55<="
                                        ), selected="0-35"),
                            numericInput("num_of_prev_attempts", "Previous Attempts", value = 0),
                            numericInput("studied_credits", "Studied Credits", value = 60),
                            radioButtons("disability", "Disability",
                                         choices = list("Yes" = "Y", "No" = "N"),selected = "N")
                        ),
                        valueBoxOutput("classificationPrediction"),

                    ),
                    
                    ),
            tabItem(tabName = "regression",
                    
                    fluidRow(
                        valueBoxOutput("regressionPrediction"),
                    )
            )
            ),
            
            # Second tab content
            
        
    )
)

server <- function(input, output) {
    
    output$classificationPrediction <- renderValueBox({
        dataset=data.frame("total_clicks"=input$total_clicks,
                           "total_elements"=input$total_elements,
                           "active_days"= input$active_days,
                           "average_daily_clicks"=input$average_daily_clicks,
                           "average_elements" = input$average_elements,
                           "avgScore" = input$avgScore,
                           "delivered" =input$delivered,
                           "sumDelays" = input$sumDelays,
                           "gender"= input$gender,
                           "region"= input$region,
                           "highest_education"= input$highest_education,
                           "imd_band"=input$imd_band,
                           "age_band"=input$age_band,
                           "num_of_prev_attempts"=input$num_of_prev_attempts,
                           "studied_credits"=input$studied_credits,
                           "disability"=input$disability,
                           "final_result"=NA
                           
        )
        print(classificationModel)
        predictedValue=predict(classificationModel,dataset)
        print(predictedValue)
        valueBox(
            ifelse(predictedValue[1]=="Pass","Pass","Fail"),"Prediction", icon = icon(ifelse(predictedValue[1]=="Pass","check","exclamation")),
            color = ifelse(predictedValue[1]=="Pass","green","red")
        )
    })
    
    output$regressionPrediction <- renderValueBox({
        datasetRegression=data.frame("total_clicks"=input$total_clicks,
                           "total_elements"=input$total_elements,
                           "active_days"= input$active_days,
                           "average_daily_clicks"=input$average_daily_clicks,
                           "average_elements" = input$average_elements,
                           "avgScore" = input$avgScore,
                           "delivered" =input$delivered,
                           "sumDelays" = input$sumDelays,
                           "score"=NA
                           
        )
        value=predict(regressionModel,datasetRegression)
        valueBox(
            format(value[1], digits=2, nsmall=2),"Final Grade", icon = icon(ifelse(value[1]>70,"check",ifelse(value[1]>50,"exclamation","times"))),
            color = ifelse(value[1]>70,"green",ifelse(value>50,"yellow","red"))
        )
    })
}

shinyApp(ui, server)