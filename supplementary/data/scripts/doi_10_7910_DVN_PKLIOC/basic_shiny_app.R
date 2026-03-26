library(shiny)

# These examples (1-6) are from the first 3 lessons on the shiny app site. 
# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
# This is a really great resource and one you should look to use during your 
# Shiny development. 

# Define UI ----
# Shiny uses the function fluidPage() to create a display that automatically 
# adjusts to the dimensions of your user's browser window. You lay out the 
# user interface of your app by placing elements in the fluidPage function.
ui <- fluidPage(
  
  # # 1
  # # Creates a user interface that has a title panel and a sidebar layout,
  # # which includes a sidebar panel and a main panel.
  # titlePanel("title panel"),
  # 
  # sidebarLayout(
  #   sidebarPanel("sidebar panel"),
  #   mainPanel("main panel")
  # )
  
  
  # # 2
  # # Sidebar always takes two arguments (sidebarPanel and mainPanel)
  # titlePanel("title panel"),
  # 
  # sidebarLayout(position = "right",
  #               sidebarPanel("sidebar panel"),
  #               mainPanel("main panel")
  # )
  
  
  # # 3
  # # Text will appear in the corresponding panel of your web page.
  # # You can place multiple elements in the same panel if you separate them with a comma.
  # titlePanel("My Shiny App"),
  # sidebarLayout(
  #   sidebarPanel(),
  #   mainPanel(
  #     h1("First level title"),
  #     h2("Second level title"),
  #     h3("Third level title"),
  #     h4("Fourth level title"),
  #     h5("Fifth level title"),
  #     h6("Sixth level title")
  #   )
  # )
  
  
  # # 4
  # # Shiny offers many tag functions for formatting text.
  # # titlePanel("My Shiny App"),
  # sidebarLayout(
  #   sidebarPanel(),
  #   mainPanel(
  #     p("p creates a paragraph of text."),
  #     p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
  #     strong("strong() makes bold text."),
  #     em("em() creates italicized (i.e, emphasized) text."),
  #     br(),
  #     code("code displays your text similar to computer code"),
  #     div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
  #     br(),
  #     p("span does the same thing as div, but it works with",
  #       span("groups of words", style = "color:blue"),
  #       "that appear inside a paragraph.")
  #   )
  # )
  
  ## 4
  ## To insert an image, give the img function the name of your image file as the
  ## src argument (e.g., img(src = "my_image.png")). You must spell out this argument
  ## since img passes your input to an HTML tag, and src is what the tag expects.
  ## You can also include other HTML friendly parameters such as height and width.

  ## The img function looks for your image file in a specific place. Your file
  ## must be in a folder named www in the same directory as the app.R script.
   # titlePanel("My Shiny App"),
   # sidebarLayout(
   #   sidebarPanel(),
   #   mainPanel(
   #     img(src = "rstudio.png", height = 140, width = 400)
   #   )
   # )
  
  
  ## 5
  ## Lets tie it all together and make a more complex layout.
  # titlePanel("My Shiny App"),
  # sidebarLayout(
  #   sidebarPanel(
  #     h2("Installation"),
  #     p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
  #     code('install.packages("shiny")'),
  #     br(),
  #     br(),
  #     br(),
  #     br(),
  #     img(src = "rstudio.png", height = 70, width = 200),
  #     br(),
  #     "Shiny is a product of ",
  #     span("RStudio", style = "color:blue")
  #   ),
  #   mainPanel(
  #     h1("Introducing Shiny"),
  #     p("Shiny is a new package from RStudio that makes it ",
  #       em("incredibly easy "),
  #       "to build interactive web applications with R."),
  #     br(),
  #     p("For an introduction and live examples, visit the ",
  #       a("Shiny homepage.",
  #         href = "http://shiny.rstudio.com")),
  #     br(),
  #     h2("Features"),
  #     p("- Build useful web applications with only a few lines of code-no JavaScript required."),
  #     p("- Shiny applications are automatically 'live' in the same way that ",
  #       strong("spreadsheets"),
  #       " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
  #   )
  # )
  
  ## 6
  # # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

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
  
  ## 7
  output$distPlot <- renderPlot({

    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, 
         breaks = bins, 
         col = "steelblue2", 
         border = "white",
         xlab = "Waiting Time to Next Eruption (mins)",
         main = "Histogram of Waiting Times")

  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)