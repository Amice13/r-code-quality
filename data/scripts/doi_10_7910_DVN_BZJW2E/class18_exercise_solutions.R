library("shiny")


load("/Users/LuisdeBonoPaula/Desktop/Data_Science_Class_Formatted_2024/18_MP_GH_reviewed/city_filters.rda")


for(i in 1:16) {
  colnames(city_filters[[1]][[i]])[3] <- "ntu"
}

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("City Filters"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    
      numericInput("num", label = h3("Filter Number (1-16)"), value = 1),

      sliderInput("Obs", label = h3("Obseration Range"), min = 1, 
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
    
    city_filter <- city_filters[[1]][[input$num]]
    
    plot(city_filter$datetime[input$Obs[1]:input$Obs[2]], 
         city_filter$ntu[input$Obs[1]:input$Obs[2]], 
         xlab = "Date (2019-2020)",
         ylab = "NTU",
         main = "NTU Readings 2019-2020",
         type="l")

    if(input$outliers == TRUE){
      city_sd <- sd(city_filter$ntu, na.rm = TRUE)
      city_mean <- mean(city_filter$ntu, na.rm = TRUE)
      
      scale <- as.numeric(input$outLvl)
      outliers <- city_filter[which(city_filter$ntu > (scale * city_sd + city_mean)), ]
      
      points(outliers$datetime, outliers$ntu, col = "red")
    }      
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
