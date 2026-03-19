
#setup
    
    rm(list=ls())
    setwd("/Users/Vincent/Dropbox/Historical Legacies/replication")
    
    color.printing <- FALSE
    
    source("scripts/libraries.R")
    source("scripts/helpers.R")

    load(rev(list.files(path="input", pattern="^data", full.names=TRUE))[1])  #load most recent data file
    
    if(color.printing){
      color.set = "Set1"
    } else {
      color.set = "gs"
    }
    
    library(tidyverse)

#descriptive statistics
    
    #outcome variables
    table <- group_by(data, type) %>% summarize(n = n(), 
                                                nlight = mean(nlight), 
                                                infmor = mean(dhs_infmort_ratio_10to17, na.rm=TRUE), 
                                                school=mean(avg_school, na.rm=TRUE)) %>% 
        as.data.frame
    
    rownames(table) <- toTitleCase(as.character(table$type))
    table <- table[, !(colnames(table) == "type")]
    colnames(table) <- c("N", "Night Lights", "Infant Mortality", "School Years")
    rownames(table) <- c("Islamic", "Non-Islamic", "Stateless")
    
    table <- xtable(table, digits=c(0, 0, 2, 2, 2), align="lcccc")
    print(table, file="tables/tab_descriptives.txt")

#balance tables
    
    table <- matrix(NA, ncol=5, nrow=9) %>% as.data.frame
    colnames(table) <- c("Islamic", "Non-Islamic", "Difference", "T statistic", "P value")
    rownames(table) <- c("Population", "Land suitability", "Land inequality", "River length", "Coast distance", 
                         "Trade distance", "Elevation", "Rough terrain", "Malaria Index")
    
    table.fill <- function(x){
        c(mean(data[data$type=="islam",x]), 
          mean(data[data$type=="pagan",x]),
          mean(data[data$type=="islam",x]) -  mean(data[data$type=="pagan",x]),
          t.test(x=data[data$type=="pagan",x], 
                 y=data[data$type=="islam",x])[c("statistic", "p.value")] %>% unlist())
    }
    
    table["Population", ] <- table.fill("population")
    table["Land suitability", ] <- table.fill("landuse")
    table["Land inequality", ] <- table.fill("landineq")
    table["River length", ] <- table.fill("lriverm_100km")
    table["Coast distance", ] <- table.fill("coast_dist_100km")
    table["Trade distance", ] <- table.fill("trade_dist_100km")
    table["Elevation", ] <- table.fill("dem")
    table["Rough terrain", ] <- table.fill("dem.var")
    table["Malaria Index", ] <- table.fill("malaria")
    
    # write table
    table <- xtable(table, digits=2)
    print(table, file="tables/tab_balance.txt")



#list of kingdoms

    load("input/kingdoms.Rdata")
    kingdoms <- kingdoms@data
    kingdoms$kingdom <- toTitleCase(kingdoms$kingdom)
    kingdoms$islam_gov <- ifelse(kingdoms$islam_gov > 0, 1, 0)
    table <- kingdoms[,c("kingdom", "start_century", "end_century", "islam_gov", "tribes")]
    colnames(table) <- c("Kingdom", "Start", "End", "Islamic", "Ethnic Groups")
    
    xtable(table,label="sec:kingdomstable",caption="List of Kingdoms", 
                    align = "lp{1in}p{.25in}p{.25in}p{.25in}p{3.75in}",
                    digits = 0) %>% 
        print(tabular.environment="longtable",floating=FALSE, 
              size = "\\small",
              include.rownames = TRUE, file = "tables/tab_kingdoms.txt")