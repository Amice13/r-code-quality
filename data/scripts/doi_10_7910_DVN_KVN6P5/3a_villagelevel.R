
# setup the environment
        
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
        library(lme4)
        library(sjPlot)
        
        controls <- c("diamonds", "oil", "trade_dist_100km", "malaria", "landuse", "landineq",
                      "population_k", "dem_1km", "dem.sd", "lriverm_100km", "coast_dist_100km", "railroads_100km")
        
        
# data loading and validation
        
        # load village level data
        
               #load original file
                load("input/villagelevel.Rdata")
                dhs.comb <- dhs.comb[,!duplicated(colnames(dhs.comb))] #kingdom column duplicated
                
                #break stateless into stateless-country
                dhs.comb <- as.data.frame(dhs.comb)
                dhs.comb$kingdom_orig <- dhs.comb$kingdom
                dhs.comb$kingdom <- ifelse(dhs.comb$kingdom=="stateless", 
                                           paste0(dhs.comb$kingdom, "-", 
                                                  dhs.comb$country), dhs.comb$kingdom)
                        
                #drop points with wrong lats/longs
                dhs.comb <- dhs.comb[!(dhs.comb$lat_indiv == 0 & dhs.comb$lng_indiv == 0) &  #zeros for both or NAs
                                                 !is.na(dhs.comb$lat_indiv) & !is.na(dhs.comb$lng_indiv),]
                dhs.comb <- dhs.comb[!(dhs.comb$lat_indiv < 2 & dhs.comb$lat_indiv > - 2 &  #really small, 5 obs
                                                   dhs.comb$lng_indiv < 2 & dhs.comb$lng_indiv > - 2),]
        
#regression
                
        #set up the data
                
                #change the dependent variables
                dhs.comb$nlight.ln <- log(dhs.comb$light + .01)
                dhs.comb$religion_isl_bin_flip <- as.factor(ifelse(dhs.comb$religion_isl > .5, 1, 0))
                dhs.comb$type <- plyr::revalue(dhs.comb$type, c("pagan"="Non-Islamic", "islamic"="Islamic", "stateless"="Stateless"))
                dhs.comb$type <- relevel(dhs.comb$type, ref = "Islamic")
                
                #rescale the controls
                dhs.comb$lriverm_100km <- dhs.comb$rivers / 100000
                dhs.comb$trade_dist_100km <- dhs.comb$routes / 100000
                dhs.comb$population_k <- dhs.comb$population/1000
                dhs.comb$railroads_100km <- dhs.comb$railroads/100000
                dhs.comb$coast_dist_100km <- dhs.comb$coast/100000
                dhs.comb$dem_1km <- dhs.comb$dem / 1000
                dhs.comb$dem.sd <- sqrt(dhs.comb$dem.var)
                
        #specify the multi-level formula
                
                #flip the direction so that the interaction we want is the base
                dhs.comb$religion_isl_bin_flip_flip <- ifelse(dhs.comb$religion_isl_bin == 1, 0, 1)
                
                #syntax for multiple levels is on page 7
                #https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
                form <- " ~ type*religion_isl_bin_flip + (1|kingdom)"
                form <- as.formula(paste(form, " + ", paste(controls, collapse = " + ")))
                
        #run the regressions
                
                #this is worse for writing in a table but easier to check whether the interactions are SS
                mod.school <- lmer(update(form, schoolyears ~ .), data=dhs.comb)
                mod.infmor <- lmer(update(form, infmort ~ .), data=dhs.comb) 
                mod.nlight <- lmer(update(form, nlight.ln ~ .), data=dhs.comb)
        
        # save a table for the appendix
                
                star <- capture.output(stargazer(mod.nlight, mod.infmor, mod.school))
                writeLines(star, con="tables/tab_religion_village.txt")
                
                
        #plot the effects
                
                #within villages that are mostly muslim, still have an effect of islamic states
                common <- list( scale_x_continuous(name = "Pct Muslim", 
                                                   limits= c(-.25, 1.25), 
                                                   minor_breaks=FALSE, 
                                                   breaks = NULL),
                                theme_bw(),
                                labs(color = "Kingdom History"))
                     
                notmuslim <- list(scale_x_continuous(limits= c(-.25, .25), 
                                                     minor_breaks=NULL),
                                  scale_y_continuous(minor_breaks=NULL),
                                  labs(y = NULL),  
                                  theme(axis.title.x=element_blank(),
                                        axis.text.x=element_blank(),
                                        axis.ticks.x=element_blank()))
                                  
                muslim <- list(scale_x_continuous(limits= c(.75, 1.25),
                                                  minor_breaks = NULL),
                               scale_y_continuous(minor_breaks=NULL),
                               labs(y = NULL),  
                               theme(axis.title.x=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank()))
                               
                plot1 <- plot_model(mod.nlight, type = "pred", 
                                    terms = c("religion_isl_bin_flip", "type"), 
                                    colors="gs", dodge = .3) +
                    ggtitle("A. Night Lights") + common + notmuslim + labs(y = "Not Muslim")
                
                plot2 <- plot_model(mod.infmor, type = "pred", 
                                    terms = c("religion_isl_bin_flip", "type"), 
                                    colors="gs", dodge = .3) +
                    ggtitle("B. Infant Mortality") + common + notmuslim
                
                plot3 <- plot_model(mod.school, type = "pred", 
                                    terms = c("religion_isl_bin_flip", "type"), 
                                    colors="gs", dodge = .3) +
                    ggtitle("C. School Years") + common + notmuslim
                
                plot4 <- plot_model(mod.nlight, type = "pred", 
                                    terms = c("religion_isl_bin_flip", "type"), 
                                    colors="gs", dodge = .3) +
                    ggtitle("A. Night Lights") + common + muslim + labs(y = "Muslim")
                
                plot5 <- plot_model(mod.infmor, type = "pred", 
                                    terms = c("religion_isl_bin_flip", "type"), 
                                    colors="gs", dodge = .3) +
                    ggtitle("B. Infant Mortality") + common + muslim
                
                plot6 <- plot_model(mod.school, type = "pred", 
                                    terms = c("religion_isl_bin_flip", "type"),
                                    colors="gs", dodge = .3) +
                    ggtitle("C. School Years") + common + muslim
                
                plot <- grid_arrange_shared_legend(plot4, plot5, plot6, 
                                                   plot1, plot2, plot3, 
                                                   nrow=2, ncol = 3, sep=.3, bottom=.2, top = .2)
                
                ggsave(file="charts/fig_islamtest.png", plot = plot, width = 6.5, height=4.1, units="in")
                
        