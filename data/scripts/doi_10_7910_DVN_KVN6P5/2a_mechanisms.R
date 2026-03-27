
#setup

        rm(list=ls())
        setwd("/Users/Vincent/Dropbox/Historical Legacies/replication")
        color.printing <- FALSE
        source("scripts/libraries.R")
        source("scripts/helpers.R")
        load(rev(list.files(path="input", pattern="^data", full.names=TRUE))[1])  #load most recent data file

# missions
        
        # relevel to show islam in the table
        data$type <- relevel(data$type, ref="pagan")

        #run the models
        mod1 <- lm(f.build("ctmission", c("type")),data=data) 
        mod2 <- lm(f.build("ctmission", c("type", controls)),data=data) 
        mod3 <- lm(f.build("ctmission", c("type", controls, "country_overlap")),data=data) 
        
        #set up the tables and save
        dv.lab <- c("No Controls", "Controls", "Fixed Effects")
        cov.lab <- c("Islamic Kingdom", "Stateless")
        
        star <- capture.output(stargazer(mod1, mod2,mod3,
                                         omit = c(controls, "country_overlap"),
                                         dep.var.labels= dv.lab,
                                         covariate.labels = cov.lab,
                                         title = "Christian Missions",
                                         caption = "tab:mech1_missions",
                                         type="latex"))
        
        writeLines(star, con="tables/mech_missions.txt")
        
# meyers scores
        
        #run the models
        mod1 <- lm(f.build("myers", c("type")),data=data) 
        mod2 <- lm(f.build("myers", c("type", controls)),data=data) 
        mod3 <- lm(f.build("myers", c("type", controls, "country_overlap")),data=data) 
        
        #set up the tables and save
        dv.lab <- c("No Controls", "Controls", "Fixed Effects")
        cov.lab <- c("Islamic Kingdom", "Stateless")
        
        star <- capture.output(stargazer(mod1, mod2,mod3,
                                         omit = c(controls, "country_overlap"),
                                         dep.var.labels= dv.lab,
                                         covariate.labels = cov.lab,
                                         title = "Digit Heaping",
                                         label = "tab:mech2_myers",
                                         type="latex"))
        
        star <- stampTime(star)

        writeLines(star, con="tables/mech_myers.txt")