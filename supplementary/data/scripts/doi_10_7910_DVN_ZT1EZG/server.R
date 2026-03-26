library(shiny, quietly = T)

wd      <- paste0(scriptDir , "/plots_" )

server  <- shinyServer(function(input, output) {

  output$degree <- renderText({
                               paste( as.character( as.numeric ( input$kelvins2degree ) - 273.15 ), "C" )
                               
                              })
  
  output$images <- renderUI({

    wdn         <-  paste( wd , input$organism , "/", "hq", "/", sep = "") 
    
    if(length(  grep(input$uniprotID, list.files(wdn))) == 0 ) {return(NULL)}
    else{
      
    proteinName   <- input$uniprotID
    pos           <- grep(proteinName , list.files(wdn) )
    peptideFiles  <- list.files( wdn )[ pos ] 
    numboffiles   <- length( peptideFiles )
    
    image_output_list <- 
      lapply(1:numboffiles,
             function(i)
             {
               imagename = paste0("image", i)
               imageOutput(imagename)
             
               })
    
    do.call(tagList, image_output_list)
    
    }
  })
  
  observe({
   
    wdn          <-  paste(wd, input$organism, "/", "hq", "/", sep = "") 
    
    if(is.null(input$uniprotID)) return(NULL)
    else{
    proteinName   <- input$uniprotID
    pos           <- grep(proteinName , list.files(wdn) )
    peptideFiles  <- list.files(wdn)[pos] 
    numboffiles   <- length(peptideFiles)
 
    for (i in 1:numboffiles)
    {
      wdn         <-  paste(wd, input$organism, "/", "hq", "/", sep = "") 

      local({
        my_i <- i
        imagename = paste0("image", my_i)
        print(imagename)
        print( paste(wd, input$organism, "/", "hq", "/", sep = "") ) 
        output[[imagename]] <- 
          renderImage({
            
            proteinName   <- input$uniprotID
            pos           <- grep(proteinName , list.files(wdn) )
            peptideFiles  <- list.files(wdn)[pos]
            image_file    <- paste(wdn, peptideFiles[my_i] ,sep="") #this might cause trouble 
            
            list(src = image_file,
                   height = 400,
                    width = 400,
                      alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
    }
  })
  
  output$infotext <- renderText({
  
    wdn           <- paste(wd, input$organism, "/", "info_", input$organism, sep = "") 
    infotable     <- read.csv(paste(wdn, ".csv", sep=""))
    proteinName   <- input$uniprotID 
    
    if(length(  which(input$uniprotID == infotable$entry)) == 0 ) 
      {
       if( input$uniprotID == "Uniprot ID..."){return(NULL)}
       else{
            return("protein not available, sorry :-(")
            }
       }
    
    else{
           paste( as.character( infotable[ which(proteinName == infotable$entry), 4] ),
           paste( "Melting Temperature:", as.character( round(infotable[ which(proteinName == infotable$entry), 1] ,2)), "C"),  
           paste( "Stability Class:", as.character( infotable[ which(proteinName == infotable$entry), 2])), 
           sep= "<br/>" )
    }
  })
  
})
