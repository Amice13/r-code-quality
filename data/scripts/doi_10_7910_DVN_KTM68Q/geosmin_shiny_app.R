###############################################################################
## Load Libraries

library(shiny)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(viridis)
library(dplyr)
library(openair)
library(GGally)
library(tree)
library(randomForest)
library(shinythemes)
library(ggplot2)
library(lattice)
library(caret)
library(scales)
library(readr)

###############################################################################
## Load Data


load("geosmin.rda")
all_geosmin <- geosmin$geo_all_original_times$measurements
big_wq_B <- geosmin$geo_all_original_times$water_quality
geo_and_wq_1hr_B <- geosmin$geo_and_wq_time_avg$geo_and_wq_1hr
geo_and_wq_2hr_B <- geosmin$geo_and_wq_time_avg$geo_and_wq_2hr
geo_and_wq_12hr_B <- geosmin$geo_and_wq_time_avg$geo_and_wq_12hr
geo_and_wq_24hr_B <- geosmin$geo_and_wq_time_avg$geo_and_wq_24hr
geo_and_wq_30min_B <- geosmin$geo_and_wq_time_avg$geo_and_wq_30min

col_big_names <- c("Datetime",
                   "Depth (m)",
                   "Conductivity (uS/cm)",
                   "DO (mg/L)",
                   "pH",
                   "Saturation (%)",
                   "Temperature (C)",
                   "Turbidity (NTU)")

colnames(big_wq_B) <- col_big_names


colnames(all_geosmin) <- c("Datetime",
                           "Depth (m)",
                           "GeosminEntry (ng/L)")

col_names <- c("SampleNumber",
               "Datetime",
               "Depth (m)",
               "GeosminEntry (ng/L)",
               "Conductivity (uS/cm)",
               "DO (mg/L)",
               "pH",
               "Saturation (%)",
               "Temperature (C)",
               "Turbidity (NTU)",
               "event")

colnames(geo_and_wq_30min_B) <- col_names
colnames(geo_and_wq_1hr_B) <- col_names
colnames(geo_and_wq_2hr_B) <- col_names
colnames(geo_and_wq_12hr_B) <- col_names
colnames(geo_and_wq_24hr_B) <- col_names

var_big_names <- c("Datetime" = "Datetime",
                   "Depth (m)" = "Depth (m)",
                   "Conductivity (uS/cm)" = "Conductivity (uS/cm)",
                   "DO (mg/L)" = "DO (mg/L)",
                   "pH" = "pH",
                   "Saturation (%)" = "Saturation (%)",
                   "Temperature (C)" = "Temperature (C)",
                   "Turbidity (NTU)" = "Turbidity (NTU)")

var_names <- c("SampleNumber" = "SampleNumber",
               "Datetime" ="Datetime",
               "Depth (m)" ="Depth (m)",
               "Geosmin (ng/L)"= "GeosminEntry (ng/L)",
               "Conductivity (uS/cm)"= "Conductivity (uS/cm)",
               "DO (mg/L)" = "DO (mg/L)",
               "pH" = "pH",
               "Saturation (%)" = "Saturation (%)",
               "Temperature (C)" = "Temperature (C)",
               "Turbidity (NTU)" = "Turbidity (NTU)",
               "event" = "event")


###############################################################################
## ShinyApp

###########################################
## UI
ui <- navbarPage(
  "Geosmin",
  theme = shinytheme("cerulean"),
  ##-----------------------------------------------------------------------
  ## TabPanel1
  tabPanel(
    
    "Predictions",
    
    fluidPage(
      
      
      fluidRow(
        
        column(width=4,
               h3('Prediction'),
               p("Step1: Choose a dataset to create the models."),
               p("Step2: Enter the water quality parameter that you would like to predict with."),
               p("Step3: See the result of predicted geosmin."),
               selectInput(inputId = "train", 
                           label = "Choose a time window for your data:", 
                           choices = c("30mins", "1hr", "2hr", "12hr", "24hr")),
               helpText("The above data sets are created using the mean of all water quality variables within specified time range of each geosmin event. ")
               
               
        ),
        
        column(width=4,
               
               h3("Water Quality Parameters"),
               numericInput("cond", "Conductivity (uS/cm)", 307, min = 0, step = 0.1),
               numericInput("do", "Dissolved Oxygen (mg/L)", 8.92, min = 0,step = 0.1),
               numericInput("ph", "pH", 8.19, min = 0,step = 0.1),
               numericInput("temperature", "Temperature (C)", 15.9, min = 0,step = 0.1),
               numericInput("turbidity", "Turbidity (NTU)", 1.0692, min = 0,step = 0.1)
        ),
        
        column(width=4,
               h3("Results"),
               helpText("RF Classification and Regression may yield different results. The models were trained using different response variables. Classification results will yield a more accurate result. "),
               h4("RF Regression Results"), 
               tableOutput("predictedReg"),
               h4("RF Classification Results"), 
               tableOutput("predictedClass"),
               h5("Proportion of Trees that voted for Classification Result:"),
               tableOutput("percentAccuracy")
               
               
        ),
      )
    )
  ),
  
  
  ##-----------------------------------------------------------------------
  ## TabPanel2
  tabPanel(
    
    "Variables", 
    
    fluidPage(
      
      fluidRow(
        
        column(width=4,
               h3('Water Quality'),
               
               selectInput("xvar1", "Datetime (X-axis)",
                           c("Datetime" = "Datetime"),
                           selected = "Datetime"),
               selectInput("yvar1", "Water Quality Paramaters (Y-axis)",
                           c("Depth (m)" ="Depth (m)",  
                             "Conductivity (uS/cm)"= "Conductivity (uS/cm)", 
                             "DO (mg/L)" = "DO (mg/L)", 
                             "pH" = "pH",
                             "Temperature (C)" = "Temperature (C)",
                             "Turbidity (NTU)" = "Turbidity (NTU)"), 
                           selected = "Depth (m)"),
               sliderInput("year1", "Time Range",
                           min = as.POSIXct("2010-05-21", "%Y-%m-%d", tz = "GMT"),
                           max = as.POSIXct("2020-12-07", "%Y-%m-%d", tz = "GMT"),
                           value = c(as.POSIXct("2010-05-21", tz = "GMT"), 
                                     as.POSIXct("2020-12-07", tz = "GMT")),
                           timeFormat="%Y-%m-%d",
                           step = 1)
        ),
        
        br(),
        br(),
        br(),
        column(width=5,
               tableOutput("table1"),
               helpText("Note: Depth is measured from the surface of the reservoir. 0m = surface")),
        
        
      ),
      
      fluidRow(
        
        plotOutput("plotWQ")
        
      )
      
    )
    
  ),
  
  
  ##-----------------------------------------------------------------------
  ## TabPanel2
  tabPanel(
    
    "Relationships", 
    
    fluidPage(
      
      fluidRow(
        
        column(width=3,
               h3('Relationship'),
               h4("Dataset"),
               selectInput(inputId = "dataset2", 
                           label = "Choose a dataset:", 
                           choices = c("30mins", "1hr", "2hr", "12hr", "24hr")),
               numericInput(inputId = "obs",
                            label = "Preview the dataset (num of obs)",
                            value = 5),
               helpText("Note: Depth is measured from the surface of the reservoir. 0m = surface")
        ),
        
        
        column(width = 3,
               h4("Plot"),
               selectInput("xvar2", "Variables (X-axis)",
                           c("Geosmin (ng/L)"= "GeosminEntry (ng/L)",
                             "Depth (m)" ="Depth (m)", 
                             "Conductivity (uS/cm)"= "Conductivity (uS/cm)", 
                             "DO (mg/L)" = "DO (mg/L)", 
                             "pH" = "pH",
                             "Temperature (C)" = "Temperature (C)",
                             "Turbidity (NTU)" = "Turbidity (NTU)"), 
                           selected = "Geosmin (ng/L)"),
               selectInput("yvar2", "Variables (Y-axis)",
                           c("Geosmin (ng/L)"= "GeosminEntry (ng/L)",
                             "Depth (m)" ="Depth (m)", 
                             "Conductivity (uS/cm)"= "Conductivity (uS/cm)", 
                             "DO (mg/L)" = "DO (mg/L)", 
                             "pH" = "pH",
                             "Temperature (C)" = "Temperature (C)",
                             "Turbidity (NTU)" = "Turbidity (NTU)"), 
                           selected = "Geosmin (ng/L)"),
               br(),
               h4("Time range for the plot and summary statistics"),
               sliderInput("year2", "Year", 
                           min = as.POSIXct("2010", "%Y", tz = "GMT"),
                           max = as.POSIXct("2020", "%Y", tz = "GMT"),  
                           value = c(as.POSIXct("2010", "%Y", tz = "GMT"), 
                                     as.POSIXct("2020", "%Y", tz = "GMT")),
                           timeFormat="%Y",
                           step = 1),
               sliderInput("month2", "Month",
                           min = 1, max =12,
                           value = c(1,12), step = 1)
               
        ),
        
        column(width=3,
               h4("Comparison plot"),
               h5("Left Plot"),
               selectInput("yvar21", "Variables (Y-axis)",
                           c("Geosmin (ng/L)"= "GeosminEntry (ng/L)",
                             "Depth (m)" ="Depth (m)", 
                             "Conductivity (uS/cm)"= "Conductivity (uS/cm)", 
                             "DO (mg/L)" = "DO (mg/L)", 
                             "pH" = "pH",
                             "Temperature (C)" = "Temperature (C)",
                             "Turbidity (NTU)" = "Turbidity (NTU)"), 
                           selected = "Geosmin (ng/L)"),
               sliderInput("year21", "Year", 
                           min = as.POSIXct("2010", "%Y", tz = "GMT"),
                           max = as.POSIXct("2020", "%Y", tz = "GMT"),  
                           value = c(as.POSIXct("2010", "%Y", tz = "GMT"), 
                                     as.POSIXct("2020", "%Y", tz = "GMT")),
                           timeFormat="%Y",
                           step = 1),
               sliderInput("month21", "Month",
                           min = 1, max =12,
                           value = c(1,12), step = 1)
        ),
        column(width = 3,
               br(),
               br(),
               h5("Right Plot"),
               selectInput("yvar22", "Variables (Y-axis)",
                           c("Geosmin (ng/L)"= "GeosminEntry (ng/L)",
                             "Depth (m)" ="Depth (m)", 
                             "Conductivity (uS/cm)"= "Conductivity (uS/cm)", 
                             "DO (mg/L)" = "DO (mg/L)", 
                             "pH" = "pH",
                             "Temperature (C)" = "Temperature (C)",
                             "Turbidity (NTU)" = "Turbidity (NTU)"), 
                           selected = "Geosmin (ng/L)"),
               sliderInput("year22", "Year", 
                           min = as.POSIXct("2010", "%Y", tz = "GMT"),
                           max = as.POSIXct("2020", "%Y", tz = "GMT"),  
                           value = c(as.POSIXct("2010", "%Y", tz = "GMT"), 
                                     as.POSIXct("2020", "%Y", tz = "GMT")),
                           timeFormat="%Y",
                           step = 1),
               sliderInput("month22", "Month",
                           min = 1, max =12,
                           value = c(1,12), step = 1)
               
               
        )
        
      ),
      
      fluidRow(width = 12,
               tabsetPanel(type = "tabs",
                           tabPanel("Plot", plotOutput("plotvar")),
                           tabPanel("Summary",
                                    column(width = 12, 
                                           verbatimTextOutput("summary"))),
                           tabPanel("Table", tableOutput("preview")),
                           tabPanel("Comparison", 
                                    h4("Variable change over time"),
                                    column(width=6,
                                           plotOutput("left")),
                                    column(width=6,
                                           plotOutput("right")))
               )
      )
      
    )
    
  ),
  

  
)


###########################################
## Sever
server <- function(input, output, session) {
  
  ##-----------------------------------------------------------------------
  ## TabPanel1
  selectedData1 <- reactive({
    
    big_wq_B$Datetime <- as.POSIXct(big_wq_B$Datetime, 
                                    format = "%Y/%m/%d %H:%M:%S",
                                    tz = "GMT")
    big_wq_B$Date <- format(big_wq_B$Datetime, format = "%Y-%m-%d")
    data <- big_wq_B[which(big_wq_B$Date >= input$year1[1] & 
                             big_wq_B$Date <= input$year1[2]),
                     c(input$xvar1, input$yvar1)]
    
  })
  
  output$table1 <- renderTable({
    
    data.frame("Variable" = c("Depth",
                              "Conductivity",
                              "Dissolved Oxygen", 
                              "pH",
                              "Temperature",
                              "Turbidity"),
               "Units" = c("m", "us/cm", "mg/L", "", "C", "NTU"))
    
  })
  
  
  output$plotWQ <- renderPlot({
    
    x <- input$xvar1
    y <- input$yvar1
    #y <- as.numeric(y)
    
    if (input$yvar1  == "Depth (m)") {
      plot(selectedData1(), col = alpha("black", 0.5), pch = 19, 
           ylim=rev(range(selectedData1()$'Depth (m)')))
    } 
    else {
      plot(selectedData1(), col = alpha("black", 0.5), pch = 19)
    }
    
  })
  
  ##-----------------------------------------------------------------------
  ## TabPanel2
  datasetInput2 <- reactive({
    
    switch(input$dataset2,
           "30mins" = geo_and_wq_30min_B,
           "1hr" = geo_and_wq_1hr_B,
           "2hr" = geo_and_wq_2hr_B,
           "12hr" = geo_and_wq_12hr_B,
           "24hr" = geo_and_wq_24hr_B)
    
  })
  
  selectedData2 <- reactive({
    
    dataset2 <- datasetInput2()
    dataset2$Datetime <- as.POSIXct(dataset2$Datetime, 
                                    format = "%Y/%m/%d %H:%M:%S",
                                    tz = "GMT")
    dataset2$Date <- format(dataset2$Datetime, format = "%Y-%m-%d")
    dataset2 <- dataset2[which(dataset2$Date >= input$year2[1] & 
                                 dataset2$Date <= input$year2[2]),]
    dataset2 <- dataset2[between(month(dataset2$Datetime), 
                                 min(input$month2), 
                                 max(input$month2)),]
    
    dataset2[,c(input$xvar2, input$yvar2)]
    
  })
  

  
  output$plotvar <- renderPlot({
    
    x <- input$xvar2
    y <- input$yvar2
    head(datasetInput2())
    
     if(input$yvar2 == "Depth (m)"){
      plot(selectedData2(), col = alpha("black", 0.5), pch = 19,
            ylim=rev((range(na.omit(selectedData2()$`Depth (m)`)))))
     }
     else{
      plot(selectedData2(),col = alpha("black", 0.5), pch = 19)
     }
    
  })
  
  output$summary <- renderPrint({
    
    dataset2 <- datasetInput2()
    dataset2$Datetime <- as.POSIXct(dataset2$Datetime, 
                                    format = "%Y/%m/%d %H:%M:%S",
                                    tz = "GMT")
    dataset2$Date <- format(dataset2$Datetime, format = "%Y-%m-%d")
    dataset2 <- dataset2[which(dataset2$Date >= input$year2[1] & 
                                 dataset2$Date <= input$year2[2]),]
    dataset2 <- dataset2[between(month(dataset2$Datetime), 
                                 min(input$month2), 
                                 max(input$month2)),]
    dataset2 <- dataset2[, -c(1,2,8,11)]
    summary(dataset2)
    
  })
  
  output$preview <- renderTable({
    
    dataset2 <- datasetInput2()
    dataset2$Datetime <- strftime(dataset2$Datetime, 
                                  format="%Y-%m-%d %H:%M:%S")
    dataset2$SampleNumber <- as.integer(dataset2$SampleNumber)
    head(dataset2,
         n = input$obs)
    
  })
  
  selectedData21 <- reactive({
    
    dataset2 <- datasetInput2()
    
    dataset2$Datetime <- as.POSIXct(dataset2$Datetime, 
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "GMT")
    dataset2$Date <- format(dataset2$Datetime, format = "%Y-%m-%d")
    dataset2 <- dataset2[which(dataset2$Date >= input$year21[1] & 
                                 dataset2$Date <= input$year21[2]),]
    dataset2 <- dataset2[between(month(dataset2$Datetime), 
                                 min(input$month21), 
                                 max(input$month21)),]
    
    dataset2[,c("Datetime", input$yvar21)]
    
    
  })
  
  
  output$left <- renderPlot({
    
    data21 <- selectedData21()
    x <- data21$Datetime
    y <- input$yvar21
    if(input$yvar21 == "Depth (m)"){
      plot(selectedData21(),col = alpha("black", 0.5), pch = 19,
           ylim=rev((range(na.omit(selectedData21()$`Depth (m)`)))))
    }
    else{
      plot(selectedData21(),col = alpha("black", 0.5), pch = 19)
     }
  })
  
  selectedData22 <- reactive({
    
    
    dataset2 <- datasetInput2()
    
    dataset2$Datetime <- as.POSIXct(dataset2$Datetime, 
                                    format = "%Y-%m-%d %H:%M:%S",
                                    tz = "GMT")
    dataset2$Date <- format(dataset2$Datetime, format = "%Y-%m-%d")
    dataset2 <- dataset2[which(dataset2$Date >= input$year22[1] & 
                                 dataset2$Date <= input$year22[2]),]
    dataset2 <- dataset2[between(month(dataset2$Datetime), 
                                 min(input$month22), 
                                 max(input$month22)),]
    
    dataset2[,c("Datetime", input$yvar22)]
    
  })
  
  output$right <- renderPlot({
    
    data22 <- selectedData22()
    x <- data22$Datetime
    y <- input$yvar22
    if(input$yvar22 == "Depth (m)"){
      plot(selectedData22(),col = alpha("black", 0.5), pch = 19,
           ylim=rev((range(na.omit(selectedData22()$`Depth (m)`)))))
    }
    else{
      plot(selectedData22(),col = alpha("black", 0.5), pch = 19)
    }
  })
  
  ##-----------------------------------------------------------------------
  ## TabPanel3
  
  
  datasetInput3 <- reactive({
    
    switch(input$train,
           "30mins" = geo_and_wq_30min_B,
           "1hr" = geo_and_wq_1hr_B,
           "2hr" = geo_and_wq_2hr_B,
           "12hr" = geo_and_wq_12hr_B,
           "24hr" = geo_and_wq_24hr_B)
  })
  
  
  
  
  output$predictedReg <- renderTable({
    data2train <- datasetInput3()
    data2train <- data2train[,-c(1,2,3,8)]
    data2train$`GeosminEntry (ng/L)`[is.na(data2train$GeosminEntry)] <- 0
    data2train$event[is.na(data2train$event)] <- "low"
    data2train$event <- as.factor(data2train$event)

    rfTestData <- data.frame(input$cond, input$do, input$ph, input$temperature, input$turbidity)
    colnames(rfTestData) <- colnames(data2train[-c(1,7)])
    
    rfmodel <- randomForest(
      data=model2train,
      x=data2train[,-c(1,7)],
      y=data2train$GeosminEntry,
      #xtest=rfTestData,
      ntree=5000,
      importance=TRUE,
      mtry = 6/3)
    PredictedReg <- data.frame(predict(rfmodel, newdata = rfTestData))
    #PredictedReg <- data.frame(head(rfmodel$test$predicted))
    colnames(PredictedReg)<- "Predicted Geosmin (ng/L)"
    head(PredictedReg)
  })
  runClassification<- reactive({
      data2train <- datasetInput3()
      data2train <- data2train[,-c(1,2,3,8)]
      data2train$`GeosminEntry (ng/L)`[is.na(data2train$GeosminEntry)] <- 0
      data2train$event[is.na(data2train$event)] <- "low"
      data2train$event <- as.factor(data2train$event)
      
      rfmodel <- randomForest(
        data=model2train,
        x=data2train[,2:6],
        y=data2train$event,
        ntree=5000,
        importance=TRUE,
        mtry = 6^(1/2))
      rfmodel
      
  })
  
  output$predictedClass <- renderTable({
    rfmodel <- runClassification()
    
    rfTestData <- data.frame(input$cond, input$do, input$ph, input$temperature, input$turbidity)
    
    
    colnames(rfTestData) <- c("Conductivity (uS/cm)", "DO (mg/L)", "pH","Temperature (C)","Turbidity (NTU)")
    
    PredictedClass <- data.frame(predict(rfmodel, newdata = rfTestData))
    colnames(PredictedClass) <- "High Geosmin: >5 ng/L"
    head(PredictedClass)
  })
  
  
  
  # output$lmPredict <- renderTable({
  #   data2model <- datasetInput3()
  #   data2model <- data2model[,c(4,7:9,11:12)] ##removes excess variables
  #   data2model$logGeosmin <- ifelse(data2model$GeosminEntry == 0 | is.na(data2model$GeosminEntry) ,0, log(data2model$GeosminEntry))
  #   data2model <- data2model[,-c(1)]
  #   model <- lm(logGeosmin ~ ., data = data2model)
  #   #plot(model)
  #   #summary(model)
  #   
  #   rfTestData <- data.frame(input$cond, input$do, input$ph, input$temperature, input$turbidity)
  #   colnames(rfTestData ) <- colnames(data2model)[-c(6)]
  #   
  #   predicted <- predict(model,newdata = rfTestData )
  #   predicted
  #   return(exp(predicted))
  # })
  
  output$percentAccuracy <- renderTable({
    rfmodel <- runClassification()
    
    rfTestData <- data.frame(input$cond, input$do, input$ph, input$temperature, input$turbidity)
    colnames(rfTestData) <- c("Conductivity (uS/cm)", "DO (mg/L)", "pH","Temperature (C)","Turbidity (NTU)")
    
    PredictedClass <- predict(rfmodel, newdata = rfTestData, type = "vote")
    
    PredictedClass <- data.frame(PredictedClass*100)
    #colnames(PredictedClass) <- c("Low", "High")
    
    head(PredictedClass)
    
  })
  
  
  
  
}

###########################################
## Run the app
shinyApp(ui = ui, server = server)