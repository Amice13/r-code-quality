
##--Load needed libraries
library(shiny)
library(scales)
library(lubridate)

##--Load the data
load("city_filters.rda")

##--Rename each of the NTU variables with just "NTU
for(i in 1:16) {
  colnames(city_filters[[1]][[i]])[3] <- "NTU"
}

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("City Filter Experiment"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    
      numericInput("num", label = h3("Filter Number (1-16)"), value = 1, step = 1),

      sliderInput("Obs", label = h3("Observation Range"), min = 1, 
                  max = nrow(city_filters[[1]][[1]]), value = c(1, nrow(city_filters[[1]][[1]]))),
      
      h1("Outlier Control"),
      
      checkboxInput("outliers", label = "Show Outliers", value = FALSE),
      
      radioButtons("outLvl", label = h3("Outlier Display Level"),
                   choices = list("3x Std Deviation" = 3, 
                                  "4x Std Deviation" = 4, 
                                  "5x Std Deviation" = 5), 
                   selected = 4),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput("plot1")
      
    )
  )

)

# Define server logic ----
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    den_filter <- city_filters[[1]][[input$num]]
    
    plot(den_filter$datetime[input$Obs[1]:input$Obs[2]], 
         den_filter$NTU[input$Obs[1]:input$Obs[2]], 
         xlab = "Date (2019-2020)",
         ylab = "NTU",
         main = "NTU Readings 2019-2020",
         type = "l")

    if(input$outliers == TRUE){
      den_sd <- sd(den_filter$NTU, na.rm = TRUE)
      den_mean <- mean(den_filter$NTU, na.rm = TRUE)
      
      scale    <- as.numeric(input$outLvl)
      outliers <- den_filter[which(den_filter$NTU > (scale * den_sd + den_mean)), ]
      
      points(outliers$datetime, outliers$NTU, col = alpha("red", 0.3), pch = 19)
    }      
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
