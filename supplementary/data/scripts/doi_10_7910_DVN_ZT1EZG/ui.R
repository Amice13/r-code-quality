shinyUI(fluidPage( theme = "bootstrap.css",
  titlePanel("Browse Denaturation Curves for all Peptides of a Protein"),
  sidebarLayout(
    sidebarPanel(
      
      textInput("uniprotID", label = h3("Search Protein"), value = "Uniprot ID..."), helpText("   ... try for example P00350"),
      
      hr(),
      
      selectInput('organism', 'Please choose an organism', c("E.coli", "S.cerevisiae", "T.thermophilus", "H.sapiens"), selectize=FALSE),
      
      
      hr(),
      
      submitButton("Search"), 
      
      hr(),
      
      textInput("kelvins2degree", label = h5("Convert Kelvin to Degree Celsius"), value = 1),
      h5(textOutput("degree"))
    ),
    
    mainPanel(
  
     h4( htmlOutput("infotext")),
     uiOutput( "images")
  
      
    )
  )
))
