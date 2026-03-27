library(shiny)
load("eml.rda")
eml_data <- eml
vars <- setdiff(names(eml), c('Depth', "Date.Time"))
depths_list <- seq(0,10, by =0.5)

# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Plotting EML Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    #headerPanel('Plotting EML Data'),
    sidebarPanel(
      p("User Input #1 - Pull Down"),
      selectInput(inputId = 'xAxis', label = 'X Axis', choices = vars, selected = 'pH'),
  
      p("User Input #2 - Pull Down"),
      selectInput(inputId = 'yAxis', label = 'Y Axis', choices = vars, selected = 'DOsat'), 
      
      p("User Select Depth"),
      selectInput(inputId = 'depth', label = 'Depth', choices = depths_list, selected = depths_list[1])
    ),
    
    
    mainPanel(
      h1("Main Plot"),
      plotOutput('plot1')
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    plot(eml[which(eml$Depth == input$depth), input$xAxis], eml[which(eml$Depth == input$depth), input$yAxis], 
         main = paste(input$yAxis, "vs", input$xAxis), 
         xlab = input$xAxis,
         ylab = input$yAxis, 
         col = rgb(0, 0, 1, 0.25),
         pch = 19, 
         xlim = range(eml[, input$xAxis]), 
         ylim = range(eml[, input$yAxis]))
    points(eml[which(eml$Depth == input$depth), input$xAxis], eml[which(eml$Depth == input$depth), input$yAxis], 
           col = rgb(0,0,0,0.5))
         
  })
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)