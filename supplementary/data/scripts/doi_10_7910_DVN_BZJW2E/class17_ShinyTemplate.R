library(shiny)


# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Template Title"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)