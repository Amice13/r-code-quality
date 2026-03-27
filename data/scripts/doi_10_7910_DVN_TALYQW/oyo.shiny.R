
# This application allows a general audience to interactively explore the history of West African slave trade by visualizing the data and models used in the paper "". 
# In our paper, we attempt to use statistical tools to capture the historical narrative of inland West African slave trade in the early 19$^{th}$ century. 
# Ultimately, we can use the two-step approach of providing simulated capture locations to a historical trade network MDP to generate and visualize the conditional probability of a slave coming from a certain spatial region given they were sold at a set of one or more points-of-sale. 
# This is a data-driven visual answer to the research question of where the slaves departing these ports originated.
# To make this research more widely available to a general audience, we created an interactive web application using the \texttt{Shiny} package in the \texttt{R} programming language. 
# The user can select a year and one or more points-of-sale, and the application generates and displays a conditional probability map showing the most likely region of capture based on our simplified model. 
# The app can also display the yearly conflict data as discrete points, a heatmap of the estimated intensity surface, or a contour plot. 
# Furthermore, the annual approximate state borders \cite{lovejoy2013redrawing} and trade network informing the MDP can be overlayed. 
# We have run our model independently for each year from 1816-1836 with the annual trade network and reward vectors changing over time, reflecting the historical narrative.
# For each year, we generate 10,000 capture locations and record the spatial coordinates, the initial location in the network, and the point-of-sale. 
# We use Kriging on these annual data using the methods in this paper to produce an annual conditional probability surface. 
# For each year, we save the conditional probability Kriging surface, the conflict point data, the KDE conflict intensity surface, the trade network, and the state border shapefiles, which are all the data sets required to host the app.
# Our web application is easy to use and freely hosted at \textttt{website.com}. 


################################################################
################################################################

################ USAGE INSTRUCTIONS
# Run the code below until the two lines of comments (as above), then run the app.

# make conflict heatmap, contours update by year (need to save krg for each year)
# and MDP network update by year (need a different aug$TR.xy for each year)
# need to save slavepaths for each year in database (will update conditional probability maps by year)

##### TO DO
# add town names, be careful of cities when they were founded ( especially Abeakuta and Ibadan)
# aug$AS from forShiny.rda may be a problem if AS switches


### need to compress oyo.shiny.R, AllOyoFunctions2.R
### col.pals.rda, forShiny.rda, shinydata/




################################################################
################################################################
source("./global.R")

# User interface ----
ui <- fluidPage(
  titlePanel(paste0("Visualising slave departures from West African cities during the Transatlantic slave trade")),
  
  sidebarLayout(
    #tags$head(tags$style(HTML('#sidebar {width: 100px;}'))),
    sidebarPanel(id = "sidebar",
                 #radioButtons("year",
                 #             h3("Choose a year"),
                 #             choices = 1817:1836, selected = 1825, inline=TRUE),
                 sliderInput("year", 
                             label = h3("Choose a year"),
                             min = 1817, max = 1836, value = 1825, sep=''),
                 checkboxGroupInput("cities", 
                                    h3("Choose one or more cities"),
                                    choiceNames = aug$AS,
                                    choiceValues = 1:length(aug$AS),
                                    selected = c(1,2,3,5,6,7), inline=TRUE),
                 sliderInput("bw", 
                             label = "Bandwidth",
                             min = 1, max = 6, value = 3),
                 checkboxInput("addconf",
                               label="Add Conflict Points",
                               value=FALSE),
                 checkboxInput("addcontour",
                               label="Add Conflict Contour",
                               value=FALSE),
                 # checkboxInput("addconfheat",
                 #               label="Add Conflict Heatmap",
                 #               value=FALSE),
                 checkboxInput("addnet",
                               label="Add MDP Network",
                               value=FALSE),
                 checkboxInput("addborders",
                               label="Add State Borders",
                               value=FALSE),
                 width=3
    ),
    
    mainPanel( 
      #textOutput(outputId = "print"),
      plotOutput(outputId = "conditional.plot") )
  )
)


# input <- list(bw=10, cities=1, year=1818, addconf=TRUE, addnet=FALSE, addcontour=FALSE, addborders=FALSE)

# Server logic ----
server <- function(input, output) {
  #output$print <- renderText(print(xlim.krg))
  output$conditional.plot <- renderPlot({
    cond.loc.plot( paths=object.list$paths, bwd = input$bw/4 ,
                   locs = object.list$aug$absstatesnew[as.numeric(input$cities)], 
                   Cities = data.list$Cities,
                   conf = conf,
                   krig = object.list$krg,
                   aug = object.list$aug,
                   borders=borders,
                   year=input$year,
                   shiptotals=data.list$shiptotals,
                   add.conf=input$addconf,
                   #add.conf.heat=input$addconfheat,
                   add.network=input$addnet,
                   add.contour=input$addcontour,
                   add.borders=input$addborders,
                   path=path)
  })
}

# Run app ----
shinyApp(ui, server)



