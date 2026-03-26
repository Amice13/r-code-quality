library(shiny)



######################
# Define UI ----
######################
ui <- fluidPage(
  
  # App title ----
  titlePanel("City Filters"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    
      #First widget, uncomment & fill in the arguments
      #numericInput(),

      #Second widget, uncomment & fill in the arguments
      #sliderInput(),
      
      h1("Outlier Control"),
      
      #Third widget, uncomment & fill in the arguments
      #checkboxInput(),
      
      #Fourth widget, uncomment & fill in the arguments
      #radioButtons(),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      

      
    )
  )

)

# Define server logic ----
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
  #Fill in the time series plot here
    
  
  #Uncomment and fill in the if() statement to show the outliers
  # if(){
  #    
  # } 
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
