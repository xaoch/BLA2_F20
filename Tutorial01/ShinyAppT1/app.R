library(shiny)
library(shinydashboard)

# Loading student data
student_data=read.table("student-por.csv",sep=";",header=TRUE)

# Generating variables names and types dataset
variables = names(student_data)
types=c("nominal","nominal","ordinal","nominal","nominal","nominal","ordinal","ordinal","nominal","nominal","nominal","nominal","ordinal","ordinal","ordinal","nominal","nominal","nominal","nominal","nominal","nominal","nominal","nominal","ordinal","ordinal","ordinal","ordinal","ordinal","ordinal","numerical","numerical","numerical","numerical")
var_df = data.frame(variables, types) 

# User Interface
ui <- dashboardPage(
    dashboardHeader(title = "Students' Data"),
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
                    menuItem("Single Variable", 
                             tabName = "single",  
                             icon = icon("chart-pie", 
                                         lib="font-awesome")
                    ),
                    conditionalPanel(
                        condition = "input.sidebarmenu == 'single'",
                        selectInput("variable",
                                    label = "Variable to show:", 
                                    choices = var_df$variables, 
                                    selected = "school")
                    ),
                    menuItem("Comparisons", 
                             tabName = "comparisons", 
                             icon = icon("chart-line", 
                                         lib = "font-awesome")
                    ),
                    conditionalPanel(
                        condition = "input.sidebarmenu == 'comparisons'",
                        selectInput("variable1", 
                                    label = "First Variable:",
                                    choices = var_df$variables, 
                                    selected = "school"),
                        uiOutput("secondSelection")
                    )
        )
        
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "single",
                    fluidRow(
                        box(title = "Single Variable", 
                            status = "primary", 
                            solidHeader = TRUE,
                            plotOutput("plot1",
                                       height=700), 
                            width=12, 
                            height="800")
                    )
            ),
            
            tabItem(tabName = "comparisons",
                    fluidRow(
                        box(title = "Comparison", 
                            status = "primary", 
                            solidHeader = TRUE,
                            plotOutput("plot2",
                                       height=700),
                            width=12, 
                            height="800")
                    )
            )
        )
        
    )
)

# Server code
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        variableType =var_df[var_df$variable==input$variable,]$types
        
        if (variableType=="numerical"){
            hist(student_data[[input$variable]],
                 main=paste("Distribution of",input$variable),
                 xlab=input$variable,
                 ylab="Students")
        }
        if (variableType=="ordinal"){
            factors=factor(student_data[[input$variable]], order = TRUE)
            plot(factors,col = rainbow(15),
                 main=paste("Distribution of",input$variable),
                 xlab=input$variable,
                 ylab="Students")
        }
        if (variableType=="nominal"){
            nominal_data<-as.data.frame(table(student_data[[input$variable]]))
            colnames(nominal_data)<-c("Name","Freq")
            pct <- round(nominal_data$Freq/sum(nominal_data$Freq)*100)
            labels<-paste(nominal_data$Name,"-",pct,"%")
            pie(nominal_data$Freq,
                labels, 
                main=paste("Distribution of",input$variable))
        }
    })
    
    output$plot2 <-renderPlot({
        type1=var_df[var_df$variable==input$variable1,]$types
        type2=var_df[var_df$variable==input$variable2,]$types
        title=paste("Relationship between",input$variable1,"and",input$variable2)
        
        if (type1=="numerical" & type2=="numerical"){
            plot(student_data[[input$variable1]],
                 student_data[[input$variable2]],
                 main=title,
                 xlab=input$variable1,
                 ylab=input$variable2)
        }
        
        if(type1=="numerical" & (type2=="ordinal" | type2=="nominal")){
            formula=as.formula(paste(input$variable1,"~",input$variable2)) 
            boxplot(formula,
                    data=student_data,
                    main=title,
                    xlab=input$variable1,
                    ylab=input$variable2)    
        }
        
        if((type1=="ordinal" | type1=="nominal") & (type2=="ordinal" | type2=="nominal")){
            formula=as.formula(paste("~",input$variable1,"+",input$variable2))  
            xt=xtabs(formula, data = student_data)
            plot(xt, 
                 col = rainbow(15), 
                 main=title,
                 xlab=input$variable1,
                 ylab=input$variable2)
        }
        
        if((type1=="ordinal" | type1=="nominal") & type2=="numerical"){
            formula=as.formula(paste(input$variable2,"~",input$variable1)) 
            boxplot(formula,
                    data=student_data,
                    main=title,
                    xlab=input$variable1,
                    ylab=input$variable2)
        }
    })
    
    output$secondSelection <- renderUI({
        selectInput("variable2", 
                    label="Second Variable:", 
                    choices = var_df[var_df$variable!=input$variable1,]$variables,
                    selected=var_df[var_df$variable!=input$variable1,]$variables[0])
    })
    
}

# Running application

shinyApp(ui = ui, server = server)


