library(shiny)
library(fields)

ui <- fluidPage(
  
  titlePanel("Cubic Smoothing Spline"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "df",
                  label = "Effective DF",
                  min = 2,
                  max = 35,
                  step = 1,
                  value = 7,
                  round = -1),
      
      sliderInput(inputId = "outlier",
                  label = "Outlier y value",
                  min = 14,
                  max = 40,
                  step = 1,
                  value = 15.65,
                  round = -1),
      
      checkboxGroupInput("checkGroup", label = h3("Point Manipulation"), 
                         choices = list("Point 2" = 2, "Point 10" = 10, "Point 15" = 15, 
                                        "Point 20" = 20, "Point 25" = 25, "Point 30" = 30,
                                        "Point 38" = 38),
                         selected = 20),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
  
)


# Define server logic ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    x    <- rat.diet$t
    y    <- rat.diet$trt
    xg <- seq(0, 105, length.out = 200 )
    index <- as.numeric(input$checkGroup)
    
    y[index] <- rep(input$outlier, length(index))
    
    ghat <- splint(x, y, xgrid = xg, df = input$df)
    
    plot(x, y, pch=16,  col="grey", cex=2, ylim = c(min(y),40))
    points(x[index], y[index], col="red3",pch=16, cex=2)
    points(x,y,  cex=2)
    lines(xg, ghat, col="blue", lwd=2.5)
    
  })
  
}
# Run the app ----
shinyApp(ui = ui, server = server)
