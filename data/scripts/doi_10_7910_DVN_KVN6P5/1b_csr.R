
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
        
#cross sectional regression

        #setup common to all plots
        notes <- paste0("Controls include estimated population in 1800AD, land suitability for agriculture, ",
                        "land suitability inequality, the length of internal rivers, the length of colonial railways, ",
                        "total area, average elevation, rough terrain, malaria stability, the presence of oil wells and diamond minds, ",
                        "total slave exports, and latitude and longitude. ",
                        "Models using DHS data also include controls for the number of surveys in the ethnic group's territory. ", 
                        "The Moran's I statistic tests for the presence of spatial correlation in the model's residuals. ", 
                        "All models use heterskedasticity-consistent (HC1) standard errors clustered on the country. ", 
                        "\\textsuperscript{*}p$<0.1$,  \\textsuperscript{**}p$<0.05$, \\textsuperscript{***}p$<0.01$")
        data$type <- relevel(data$type, ref="pagan")  #set the base comparison
        
        cov.lab <- c("Islamic Kingdom", "Stateless")

        #run models
        
                #some additional setup for a few models
                data$islamDistance <- data$islam * data$coast_dist_100km
                data$islamMuslim <- data$islam * data$pctMuslim
                data$stateless <- ifelse(data$state==1, 0, 1)
                
                #conley standard errors
                
                    #setup the data and scripts
                    source("scripts/conley_burlig.R")  #load external scripts
                    data$country_overlap <- as.factor(data$country_overlap)  #make sure country is set as a factor
                    #controls <- controls[!(controls %in% c("LAT", "LON"))]  #remove LAT and LON from the controls
                    
                    myConleySE <- function(reg, lat = "LAT", lon = "LON", data, dist_cutoff = 500){
                        
                        robustse <- conley_burlig(reg = reg,
                                                  data = data, 
                                                  lat = lat, lon = lon, 
                                                  cutoff = dist_cutoff)$conleySE
                        return(list(mod = reg,
                                    origse = sqrt(diag(vcov(reg))),
                                    robustse = robustse,
                                    dummies = ifelse(any(grepl("country_overlap", names(coefficients(reg)), fixed=TRUE)), "\\checkmark", ""),
                                    controls = ifelse(length(robustse) > 4, "\\checkmark", "")))
                    }
                
                #double check that these scripts are working
                    
                    #1) replicate the canned quakes ses, should be 16.48 
                    stopifnot(round(summary(lm(depth ~ mag, data = quakes))$coefficients[2,2],2)==16.48)
                    
                    #2) duplicate the conley ses from burlig, needs to be 19.27
                    stopifnot(round(conley_burlig(reg=lm(depth ~ mag, data = quakes), data = quakes, lat = "lat", lon = "long", cutoff = 100)$conleySE[2],2)==19.27)
                    
                #night lights
                csr.nlight.basic <- lm(f.build("nlight_ln1", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.nlight.cntrl <- lm(f.build("nlight_ln1", c("type", controls)),data=data)  %>% myConleySE(reg = ., data=data)
                csr.nlight.fixed <- lm(f.build("nlight_ln1", c("type", controls, "country_overlap")),data=data)  %>% myConleySE(reg = ., data=data)

                #infant mortality
                data$dhs_n_tmp <- data$dhs_birth_n
                csr.infmor.basic <- lm(f.build("dhs_infmort_ratio", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.cntrl <- lm(f.build("dhs_infmort_ratio", c("type", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.fixed <- lm(f.build("dhs_infmort_ratio", c("type", controls, "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)

                #School Years
                data$dhs_n_tmp <- data$dhs_n
                csr.school.basic <- lm(f.build("avg_school", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.cntrl <- lm(f.build("avg_school", c("type", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.fixed <- lm(f.build("avg_school", c("type", controls, "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                
        #print  results
        
                #extract the pieces from the various models
                extract <- myExtract( list(csr.nlight.basic, 
                                           csr.nlight.cntrl,
                                           csr.nlight.fixed,
                                           csr.infmor.basic,
                                           csr.infmor.cntrl,
                                           csr.infmor.fixed,
                                           csr.school.basic,
                                           csr.school.cntrl,
                                           csr.school.fixed))
                        
                 #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$robustses,
                                                  omit=c("country"), 
                                                  dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                  title="Cross-sectional regression showing full controls", 
                                                  label = "tab:mainresults",
                                                  font.size = "tiny",
                                                  digits = 2,
                                                  digits.extra = 0,
                                                  add.lines=extract$addlines))
                
                star <- renameCovars(star)
                star <- stampTime(star)
                


                #add conventional ses to stargazer
                star2 <- capture.output( stargazer(extract$mods, 
                                                  se= extract$origses,
                                                  omit=c("country"), 
                                                  dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                  title="", 
                                                  add.lines=extract$addlines))
                star[grepl("  & (", star, fixed=TRUE)] <- paste(star[grepl("  & (", star, fixed=TRUE)], 
                                                                gsub(")", "]", gsub("(", "[", star2[grepl("  & (", 
                                                            star2, fixed=TRUE)], fixed=TRUE), fixed = TRUE), sep = "")
                

                writeLines(star, con="tables/tab_csr.txt")
                        
        
        #make a results matrix for coefficient plot

                #table setup
                results.cross <- matrix(NA, ncol=6, nrow=length(extract$mods)) %>% as.data.frame
                colnames(results.cross) <- c("var", "type", "coef", "se", "sig", "x")
                results.cross$var <- rep(c("Night Lights", "Infant Mortality", "School Years"), each=length(extract$mods)/3)
                results.cross$type <- rep(c("No Controls", "No FE", "Full Controls"), 3)
                results.cross$coef <- lapply(extract$mods, function(x) summary(x)$coefficients["typeislam","Estimate"]) %>% unlist
                results.cross$se <- lapply(extract$robustses, function(x) x["typeislam"]) %>% unlist
                results.cross$sig <- sign(results.cross$coef - 1.64*results.cross$se) == sign(results.cross$coef + 1.64*results.cross$se)  %>% as.character
                results.cross$x <- rep(c(0, 1, 2),3)

        #exporting and plotting

                #r data
                save(results.cross, file="output/results_cross.Rdata")

# #alternative cross sectional regression models

        # alternative specification, add islamic influence in decentralized states
                
                
                data <- data %>%
                    mutate(type2 = case_when(
                        as.character(type) == "stateless" & islamicinfluence == TRUE ~ "statelessIslamic",
                        as.character(type) == "stateless" & islamicinfluence == FALSE ~ "statelessNonIslamic",
                        TRUE ~ as.character(type)
                    )) %>%
                        mutate(type2 = as.factor(type2))

                data$type2 <- relevel(data$type2, ref="islam")  #set the base comparison so that we can see if the stateless islamic is different than the islamic kingdom
                
                #night lights
                csr.nlight.basic <- lm(f.build("nlight_ln1", c("type2")),data=data) %>% myConleySE(reg = ., data=data)
                csr.nlight.cntrl <- lm(f.build("nlight_ln1", c("type2", controls)),data=data)  %>% myConleySE(reg = ., data=data)

                #infant mortality
                data$dhs_n_tmp <- data$dhs_birth_n
                csr.infmor.basic <- lm(f.build("dhs_infmort_ratio", c("type2")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.cntrl <- lm(f.build("dhs_infmort_ratio", c("type2", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.fixed <- lm(f.build("dhs_infmort_ratio", c("type2", controls[c(-15, -16)], "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)

                #School Years
                data$dhs_n_tmp <- data$dhs_n
                csr.school.basic <- lm(f.build("avg_school", c("type2")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.cntrl <- lm(f.build("avg_school", c("type2", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.fixed <- lm(f.build("avg_school", c("type2", controls, "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                
                #print  results
                
                #extract the pieces from the various models
                extract <- myExtract( list(csr.nlight.basic, 
                                           csr.nlight.cntrl,
                                           csr.nlight.fixed,
                                           csr.infmor.basic,
                                           csr.infmor.cntrl,
                                           csr.infmor.fixed,
                                           csr.school.basic,
                                           csr.school.cntrl,
                                           csr.school.fixed))
                
                #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$robustses,
                                                  omit=c("country"), 
                                                  dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                  title="", 
                                                  add.lines=extract$addlines))
                
                #add conventional ses to stargazer
                star2 <- capture.output( stargazer(extract$mods, 
                                                   se= extract$origses,
                                                   omit=c("country"), 
                                                   dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                   title="Islamic Influence in Stateless areas",
                                                   label = "tab:islamicinfluencestateless",
                                                   font.size = "tiny",
                                                   digits = 2,
                                                   digits.extra = 0,
                                                    add.lines=extract$addlines))
                star[grepl("  & (", star, fixed=TRUE)] <- paste(star[grepl("  & (", star, fixed=TRUE)], 
                                                                gsub(")", "]", gsub("(", "[", star2[grepl("  & (", 
                                                                                                          star2, fixed=TRUE)], fixed=TRUE), fixed = TRUE), sep = "")
                star <- renameCovars(star)
                star <- stampTime(star)
                
                writeLines(star, con="tables/tab_csr_islamicinfluence.txt")
                
                
        # alternative specification, stateless reference category
                
                # relevel the base case
                data$type <- relevel(data$type, ref="stateless")  #set the base comparison
                
                #night lights
                csr.nlight.basic <- lm(f.build("nlight_ln1", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.nlight.cntrl <- lm(f.build("nlight_ln1", c("type", controls)),data=data)  %>% myConleySE(reg = ., data=data)
                csr.nlight.fixed <- lm(f.build("nlight_ln1", c("type", controls, "country_overlap")),data=data)  %>% myConleySE(reg = ., data=data)

                #infant mortality
                data$dhs_n_tmp <- data$dhs_birth_n
                csr.infmor.basic <- lm(f.build("dhs_infmort_ratio", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.cntrl <- lm(f.build("dhs_infmort_ratio", c("type", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.fixed <- lm(f.build("dhs_infmort_ratio", c("type", controls[c(-15, -16)], "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)

                #School Years
                data$dhs_n_tmp <- data$dhs_n
                csr.school.basic <- lm(f.build("avg_school", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.cntrl <- lm(f.build("avg_school", c("type", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.fixed <- lm(f.build("avg_school", c("type", controls, "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                
                #print  results
                
                #extract the pieces from the various models
                extract <- myExtract( list(csr.nlight.basic, 
                                           csr.nlight.cntrl,
                                           csr.nlight.fixed,
                                           csr.infmor.basic,
                                           csr.infmor.cntrl,
                                           csr.infmor.fixed,
                                           csr.school.basic,
                                           csr.school.cntrl,
                                           csr.school.fixed))
                
                #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$robustses,
                                                  omit=c("country"), 
                                                  dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                  title="Stateless Base Comparison",
                                                  label = "tab:relevel",
                                                  font.size = "tiny",
                                                  digits = 2,
                                                  digits.extra = 0,
                                                  add.lines=extract$addlines))
                
                #add conventional ses to stargazer
                star2 <- capture.output( stargazer(extract$mods, 
                                                   se= extract$origses,
                                                   omit=c("country"), 
                                                   dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                   title="", 
                                                   add.lines=extract$addlines))
                star[grepl("  & (", star, fixed=TRUE)] <- paste(star[grepl("  & (", star, fixed=TRUE)], 
                                                                gsub(")", "]", gsub("(", "[", star2[grepl("  & (", 
                                                                                                          star2, fixed=TRUE)], fixed=TRUE), fixed = TRUE), sep = "")
                star <- renameCovars(star)
                star <- stampTime(star)
                
                writeLines(star, con="tables/tab_csr_relevel.txt")
                
        # alternative specification, not including railroads
                
                controls2 <- controls[!(controls == "railroads_100km")]
                
                #night lights
                csr.nlight.basic <- lm(f.build("nlight_ln1", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.nlight.cntrl <- lm(f.build("nlight_ln1", c("type", controls2)),data=data)  %>% myConleySE(reg = ., data=data)
                csr.nlight.fixed <- lm(f.build("nlight_ln1", c("type", controls2, "country_overlap")),data=data)  %>% myConleySE(reg = ., data=data)

                #infant mortality
                data$dhs_n_tmp <- data$dhs_birth_n
                csr.infmor.basic <- lm(f.build("dhs_infmort_ratio", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.cntrl <- lm(f.build("dhs_infmort_ratio", c("type", controls2, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.fixed <- lm(f.build("dhs_infmort_ratio", c("type", controls2, "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)

                #School Years
                data$dhs_n_tmp <- data$dhs_n
                csr.school.basic <- lm(f.build("avg_school", c("type")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.cntrl <- lm(f.build("avg_school", c("type", controls2, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.fixed <- lm(f.build("avg_school", c("type", controls2, "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)

                #print  results
                
                #extract the pieces from the various models
                extract <- myExtract( list(csr.nlight.basic, 
                                           csr.nlight.cntrl,
                                           csr.nlight.fixed,
                                           csr.infmor.basic,
                                           csr.infmor.cntrl,
                                           csr.infmor.fixed,
                                           csr.school.basic,
                                           csr.school.cntrl,
                                           csr.school.fixed))
                
                #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$robustses,
                                                  omit=c("country"), 
                                                  dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                  title="Cross-sectional regression showing full controls", 
                                                  label = "tab:mainresults",
                                                  font.size = "tiny",
                                                  digits = 2,
                                                  digits.extra = 0,
                                                  add.lines=extract$addlines))
                
                star <- renameCovars(star)
                star <- stampTime(star)
                
                
                
                #add conventional ses to stargazer
                star2 <- capture.output( stargazer(extract$mods, 
                                                   se= extract$origses,
                                                   omit=c("country"), 
                                                   dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                   title="", 
                                                   add.lines=extract$addlines))
                star[grepl("  & (", star, fixed=TRUE)] <- paste(star[grepl("  & (", star, fixed=TRUE)], 
                                                                gsub(")", "]", gsub("(", "[", star2[grepl("  & (", 
                                                                                                          star2, fixed=TRUE)], fixed=TRUE), fixed = TRUE), sep = "")
                
                
                writeLines(star, con="tables/tab_csr_norailroads.txt")
                
                
        # alternative specification, states that persist until 19th century
                
                # relevel the base case
                data$type19th <-as.factor(ifelse(data$type19th=="mixed", "islam", as.character(data$type19th)))
                data$type19th <- relevel(data$type19th, ref="pagan")  #set the base comparison
                
                #night lights
                csr.nlight.basic <- lm(f.build("nlight_ln1", c("type19th")),data=data) %>% myConleySE(reg = ., data=data)
                csr.nlight.cntrl <- lm(f.build("nlight_ln1", c("type19th", controls)),data=data)  %>% myConleySE(reg = ., data=data)
                csr.nlight.fixed <- lm(f.build("nlight_ln1", c("type19th", controls, "country_overlap")),data=data)  %>% myConleySE(reg = ., data=data)

                #infant mortality
                data$dhs_n_tmp <- data$dhs_birth_n
                csr.infmor.basic <- lm(f.build("dhs_infmort_ratio", c("type19th")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.cntrl <- lm(f.build("dhs_infmort_ratio", c("type19th", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.infmor.fixed <- lm(f.build("dhs_infmort_ratio", c("type19th", controls[c(-15, -16)], "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)

                #School Years
                data$dhs_n_tmp <- data$dhs_n
                csr.school.basic <- lm(f.build("avg_school", c("type19th")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.cntrl <- lm(f.build("avg_school", c("type19th", controls, "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                csr.school.fixed <- lm(f.build("avg_school", c("type19th", controls, "country_overlap", "dhs_n_tmp")),data=data) %>% myConleySE(reg = ., data=data)
                
                #print  results
                
                #extract the pieces from the various models
                extract <- myExtract( list(csr.nlight.basic, 
                                           csr.nlight.cntrl,
                                           csr.nlight.fixed,
                                           csr.infmor.basic,
                                           csr.infmor.cntrl,
                                           csr.infmor.fixed,
                                           csr.school.basic,
                                           csr.school.cntrl,
                                           csr.school.fixed))
                
                #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$robustses,
                                                  omit=c("country"), 
                                                  dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                  title="States that Survive until 19th century",
                                                  label = "tab:nineteenthcent",
                                                  font.size = "tiny",
                                                  digits = 2,
                                                  digits.extra = 0,
                                                  add.lines=extract$addlines))
                
                #add conventional ses to stargazer
                star2 <- capture.output( stargazer(extract$mods, 
                                                   se= extract$origses,
                                                   omit=c("country"), 
                                                   dep.var.labels=c("Night Lights", "Infant Mortality", "School Years"),
                                                   title="", 
                                                   add.lines=extract$addlines))
                star[grepl("  & (", star, fixed=TRUE)] <- paste(star[grepl("  & (", star, fixed=TRUE)], 
                                                                gsub(")", "]", gsub("(", "[", star2[grepl("  & (", 
                                                                                                          star2, fixed=TRUE)], fixed=TRUE), fixed = TRUE), sep = "")
                
                star <- renameCovars(star)
                star <- stampTime(star)
                
                writeLines(star, con="tables/tab_csr_19thcent.txt")
                
                
        #disaggregated results - infant mortality brackets
                
                #DV = infant mortality brackets
                mod1 <- lm(f.build("dhs_infmort_ratio_80to89", c("type", controls, "country_overlap", "dhs_birth_n_80to89")),data=data) %>% myRobustSE()
                mod2 <- lm(f.build("dhs_infmort_ratio_90to99", c("type", controls, "country_overlap", "dhs_birth_n_90to99")),data=data) %>% myRobustSE()
                mod3 <- lm(f.build("dhs_infmort_ratio_00to09", c("type", controls, "country_overlap", "dhs_birth_n_00to09")),data=data) %>% myRobustSE()
                mod4 <- lm(f.build("dhs_infmort_ratio_10to17", c("type", controls, "country_overlap", "dhs_birth_n_10to17")),data=data) %>% myRobustSE()
                
                labs <- c("1990s", "2000s", "2010-2017")
                
                #extract the pieces from the various models
                extract <- myExtract( list(mod2, mod3, mod4))
                
                #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$ses,
                                                  omit=c("country", controls, "dhs"), 
                                                  title="Infant Mortality by Decade", 
                                                  label = "tab:infmor_decade",
                                                  dep.var.labels=labs,
                                                  add.lines=extract$addlines))
                
                star <- stampTime(star)
                
                writeLines(star, con="tables/tab_csr_infmor.txt")
                
                
        #disaggregated results - School Years cohorts
                
                mod1 <- lm(f.build("avg_school_18to25", c("type", controls, "country_overlap")), data=data) %>% myRobustSE()
                mod2 <- lm(f.build("avg_school_26to35", c("type", controls, "country_overlap")), data=data) %>% myRobustSE()
                mod3 <- lm(f.build("avg_school_36to45", c("type", controls, "country_overlap")), data=data) %>% myRobustSE()
                mod4 <- lm(f.build("avg_school_46to55", c("type", controls, "country_overlap")), data=data) %>% myRobustSE()
                mod5 <- lm(f.build("avg_school_56plus", c("type", controls, "country_overlap")), data=data) %>% myRobustSE()
                
                labs <- c("18 to 25", "26 to 35", "36 to 45", "46 to 55", "56 plus")
                
                #extract the pieces from the various models
                extract <- myExtract( list(mod1, mod2, mod3, mod4, mod5))
                
                #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$ses,
                                                  omit=c("country", controls, "dhs"), 
                                                  title="School Years by Cohort", 
                                                  label = "tab:schooling_cohort",
                                                  dep.var.labels=labs,
                                                  add.lines=extract$addlines))
                
                star <- stampTime(star)
                
                writeLines(star, con="tables/tab_csr_schooling_cohort.txt")
                
        #disaggregated results - School Years by gender
                
                mod1 <- lm(f.build("avg_school_m", c("type")),data=data) %>% myRobustSE()
                mod2 <- lm(f.build("avg_school_m", c("type", controls, "country_overlap", "dhs_n")),data=data) %>% myRobustSE()
                mod3 <- lm(f.build("avg_school_f", c("type")),data=data) %>% myRobustSE()
                mod4 <- lm(f.build("avg_school_f", c("type", controls, "country_overlap", "dhs_n")),data=data) %>% myRobustSE()
                
                labs <-  c("Men", "Men", "Women", "Women")
                
                #extract the pieces from the various models
                extract <- myExtract( list(mod1, mod2, mod3, mod4))
                
                #run stargazer and save to text file
                star <- capture.output( stargazer(extract$mods, 
                                                  se= extract$ses,
                                                  omit=c("country", controls, "dhs"), 
                                                  title="School Years by Gender", 
                                                  label = "tab:schooling_gender",
                                                  dep.var.labels=labs,
                                                  add.lines=extract$addlines))
                
                star <- stampTime(star)
                
                writeLines(star, con="tables/tab_csr_schooling_gender.txt")
                
                
        #sensitivity analysis
                
                library(treatSens)
                
                data.sens <- data[data$state==1,]
                data.sens$type <- ifelse(data.sens$type == "pagan", 0, 1)
                
                treatSens(eval(csr.nlight.cntrl$mod$call$formula), data=data.sens, trt.family=binomial(link="probit"))
                
                
                
                crs.nlight.sens <- suppressWarnings(treatSens(as.formula(paste0("nlight ~ type + ", paste0(controls, collapse="+"))), 
                                      data=data.sens, trt.family=binomial(link="probit"),
                                      verbose=TRUE))
                
                crs.infmor.sens <- suppressWarnings(treatSens(as.formula(paste0("dhs_infmort_ratio ~ type + dhs_birth_n +", paste0(controls, collapse="+"))), 
                                                              data=data.sens, trt.family=binomial(link="probit"),
                                                              verbose=TRUE))
                
                crs.school.sens <- suppressWarnings(treatSens(as.formula(paste0("avg_school ~ type + dhs_n + ", paste0(controls, collapse="+"))), 
                                                              data=data.sens, trt.family=binomial(link="probit"),
                                                              verbose=TRUE))
            
                #creating a plot with options that match black-and-white printing
                source("scripts/sensPlot_custom.R") #load custom formatting for sensPlot
                
                png("charts/fig_sensplot_nlight.png", width=3, height=4, units="in", pointsize=12, res=300)
                par(mar=c(4, 4, 1, 2))
                
                sensPlot(crs.nlight.sens,
                         data.line = FALSE, 
                         contour.levels = 100,  #this is far off the plot
                         col.zero = "black",
                         lty.zero = 2,  #dashed
                         lty.insig = 1  #solid
                )
                #title("Night Lights")
                dev.off()
                
                #creating a plot with options that match black-and-white printing
                png("charts/fig_sensplot_infmor.png", width=3, height=4, units="in", pointsize=12, res=300)
                par(mar=c(4, 4, 1, 2))
                
                sensPlot(crs.infmor.sens,
                         data.line = FALSE, 
                         contour.levels = 100,  #this is far off the plot
                         col.zero = "black",
                         lty.zero = 2,  #dashed
                         lty.insig = 1  #solid
                )
                #title("Infant Mortality")
                dev.off()
                
                #creating a plot with options that match black-and-white printing
                png("charts/fig_sensplot_school.png", width=3, height=4, units="in", pointsize=12, res=300)
                par(mar=c(4, 4, 1, 2))
                
                sensPlot(crs.school.sens,
                         data.line = FALSE, 
                         contour.levels = 100,  #this is far off the plot
                         col.zero = "black",
                         lty.zero = 2,  #dashed
                         lty.insig = 1  #solid
                )
                #title("School Years")
                dev.off()
                
                
