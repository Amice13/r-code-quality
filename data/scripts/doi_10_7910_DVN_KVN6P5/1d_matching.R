
#setup

        rm(list=ls())
        setwd("/Users/Vincent/Dropbox/Historical Legacies/replication")
        color.printing <- FALSE
        
        source("scripts/libraries.R")
        source("scripts/helpers.R")
        load(rev(list.files(path="input", pattern="^data", full.names=TRUE))[1])  #load most recent data file

#matching

        #setup
        library(MatchIt)
        match.data <- data #I need to make changes to the data structure so save a new object
        match.data$treat <- ifelse(match.data$type=="islam", 1, ifelse(match.data$type=="pagan", 0, NA))
        match.data <- match.data[!is.na(match.data$treat),]
    
        #matchit can't deal with any NAs so drop most of the Murdock columns
        match.data <- match.data[,c(1:3, 29, (137:ncol(match.data)))]
        match.data <- match.data[,!colnames(match.data)=="type_orig2"]
        match.data <- match.data[,!colnames(match.data)=="type2"]
        match.data <- match.data[, !grepl("[0-9]to[0-9]", colnames(match.data))]
    
        #save version with only columns that have non-missing values
        match.data_nlight <- match.data[,!apply(match.data, 2, function(x) any(is.na(x)))]
        match.data_infmor <- match.data[, !grepl("v33|2015|pctMuslim|avg_school_m|avg_school_f|trust|joshua|coup|year|hristian|listn|scode|country|religion|avg_school", colnames(match.data))]
        match.data_infmor <- match.data_infmor[!is.na(match.data_infmor$dhs_infmort_ratio),]
        match.data_school <- match.data[, !grepl("v33|2015|pctMuslim|avg_school_m|avg_school_f|trust|joshua|coup|year|hristian|listn|scode|country|religion|ratio", colnames(match.data))]
        match.data_school <- match.data_school[!is.na(match.data_school$avg_school),]

        #get balance
        
                #choose variables
                covars <- c("population", "landuse", "lriverm", "area_1MsqK", "malaria", "coast_dist", "dem.var")
        
                #matchit doesn't work with f.build so add controls manually
                m.out_nlight <- matchit(treat ~ population + landuse + lriverm + area_1MsqK + 
                                            malaria + coast_dist + dem.var, 
                                        data = match.data_nlight, method = "nearest", replace = TRUE)
                
                m.out_infmor <- matchit(treat ~ population + landuse + lriverm + area_1MsqK + 
                                             malaria + coast_dist + dem.var, 
                                         data = match.data_infmor, method = "nearest", replace = TRUE)
                
                m.out_school <- matchit(treat ~ population + landuse + lriverm + area_1MsqK + 
                                            malaria + coast_dist + dem.var, 
                                        data = match.data_school, method = "nearest", replace = TRUE)
                
                plot1_nlight <- love.plot(bal.tab(m.out_nlight), stat = "ks.statistics", threshold = .1, 
                                          var.order = "unadjusted", abs = TRUE,
                                          line = TRUE, colors = c(fill1, fill2)) + 
                                    ggtitle("Night Lights") + labs(x="KS Statistic")
                                          
                
                plot2_nlight <- love.plot(bal.tab(m.out_nlight), stat = "mean.diffs", threshold = .1, 
                                          var.order = "unadjusted", abs = TRUE,
                                          line = TRUE, colors = c(fill1, fill2)) + 
                                    ggtitle("Night Lights") + labs(x="Mean Difference")
                
                plot1_infmor <- love.plot(bal.tab(m.out_infmor), stat = "ks.statistics", threshold = .1, 
                                           var.order = "unadjusted", abs = TRUE,
                                           line = TRUE, colors = c(fill1, fill2)) + 
                                    ggtitle("Infant Mortality") + labs(x="KS Statistic")
                
                plot2_infmor <- love.plot(bal.tab(m.out_infmor), stat = "mean.diffs", threshold = .1, 
                                           var.order = "unadjusted", abs = TRUE,
                                           line = TRUE, colors = c(fill1, fill2))+ 
                                    ggtitle("Infant Mortality") + labs(x="Mean Difference")
                
                plot1_school <- love.plot(bal.tab(m.out_school), stat = "ks.statistics", threshold = .1, 
                                          var.order = "unadjusted", abs = TRUE,
                                          line = TRUE, colors = c(fill1, fill2)) + 
                                    ggtitle("School Years") + labs(x="KS Statistic")
                
                plot2_school <- love.plot(bal.tab(m.out_school), stat = "mean.diffs", threshold = .1, 
                                          var.order = "unadjusted", abs = TRUE,
                                          line = TRUE, colors = c(fill1, fill2)) + 
                                    ggtitle("School Years") + labs(x="Mean Difference")
        
                plot_nlight <- grid_arrange_shared_legend(plot1_nlight, plot2_nlight)
                plot_infmor <- grid_arrange_shared_legend(plot1_infmor, plot2_infmor)
                plot_school <- grid_arrange_shared_legend(plot1_school, plot2_school)
                plot_ks <- grid_arrange_shared_legend(plot1_nlight, plot1_infmor, plot1_school)
                plot_md <- grid_arrange_shared_legend(plot2_nlight, plot2_infmor, plot2_school)
        
                ggsave(file=paste0("charts/fig_matching_balance_nlight", ifelse(color.printing, "_col", "_bw"), ".png"), plot = plot_nlight, width = 6.5, height=3.25, units="in")
                ggsave(file=paste0("charts/fig_matching_balance_infmor", ifelse(color.printing, "_col", "_bw"), ".png"), plot = plot_infmor, width = 6.5, height=3.25, units="in")
                ggsave(file=paste0("charts/fig_matching_balance_school", ifelse(color.printing, "_col", "_bw"), ".png"), plot = plot_school, width = 6.5, height=3.25, units="in")
                ggsave(file=paste0("charts/fig_matching_balance_ks", ifelse(color.printing, "_col", "_bw"), ".png"), plot = plot_ks, width = 6.5, height=3.25, units="in")
                ggsave(file=paste0("charts/fig_matching_balance_md", ifelse(color.printing, "_col", "_bw"), ".png"), plot = plot_md, width = 6.5, height=3.25, units="in")

                
        #regression estimates
                
                #need zelig
                library(Zelig)

                #nlight 
                z.out_nlight1 <- zelig(nlight ~ treat, model="ls",  data = match.data(m.out_nlight))
                z.out_nlight2 <- zelig(f.build("nlight", c("treat", covars)), model = "ls", data = match.data(m.out_nlight))

                #infant mortality 
                z.out_infmor1 <- zelig(f.build("dhs_infmort_ratio", c("treat")), model = "ls", data = match.data(m.out_infmor))
                z.out_infmor2 <- zelig(f.build("dhs_infmort_ratio", c("treat", covars)), model = "ls", data = match.data(m.out_infmor))

                #School Years 
                z.out_school1 <- zelig(f.build("avg_school", c("treat")), model = "ls", data = match.data(m.out_school))
                z.out_school2 <- zelig(f.build("avg_school", c("treat", covars)), model = "ls", data = match.data(m.out_school))

                #stargazer
                
                        #change the formatting so that the zelig results can be interpretted by stargazer
                        #https://stackoverflow.com/questions/41627048/using-stargazer-with-zelig
                        mod1 <- lm(z.out_nlight1$zelig.out$z.out[[1]])
                        mod2 <- lm(z.out_nlight2$zelig.out$z.out[[1]])
                        mod3 <- lm(z.out_infmor1$zelig.out$z.out[[1]])
                        mod4 <- lm(z.out_infmor2$zelig.out$z.out[[1]])
                        mod5 <- lm(z.out_school1$zelig.out$z.out[[1]])
                        mod6 <- lm(z.out_school2$zelig.out$z.out[[1]])
                        
                
                        #other stargazer objects
                        cov.lab.match <- c("Islamic Kingdom", "Population", "Landuse", "River Length", "Area", "Malaria", "Coast Distance", "Rough Terrain")
                        dep.lab.match <- c("Night Lights", "Infant Mortality", "Average School Years")
                        notes.match <- "All models use heterskedasticity-consistent (HC1) standard errors clustered on the country. \\textsuperscript{*}p$<0.1$,  \\textsuperscript{**}p$<0.05$, \\textsuperscript{***}p$<0.01$"

                        #stargazer
                        star <- capture.output(stargazer(mod1, mod2, mod3,mod4, mod5, mod6,
                                                         title="Matching",
                                                         label = "tab:matching",
                                                         column.labels=dep.lab.match, 
                                                         digits = 2,
                                                         digits.extra = 0,
                                                         covariate.labels=cov.lab.match))
                        
                        star <- renameCovars(star)
                        star <- stampTime(star)
                        
                        
                        writeLines(star, con="tables/tab_matching.txt")


            #creates a results matrix
                        
                        #consolidate the model output
                        models <- list(z.out_nlight1, z.out_nlight2, z.out_infmor1, z.out_infmor2, z.out_school1, z.out_school2)
                        results.match <- as.data.frame(matrix(NA, nrow = 6, ncol = 8))
                        colnames(results.match) <- c("var", "type", "x", "coef", "se", "lo", "hi", "sig")

                        #create results matrix
                        results.match[,"var"] <- rep(c("Night Lights", "Infant Mortality", "School Years"),each=2)
                        results.match[,"type"] <- rep(c("No Controls", "Full Controls"), 3)
                        results.match[,"x"] <- rep(c(0, 1),3)
                        results.match[,"coef"] <- lapply(models, function(x) coef(x)[2]) %>% unlist
                        results.match[,"se"] <- lapply(models, function(x) get_se(x)[[1]][2]) %>% unlist
                        results.match$sig <- sign(results.match$coef - 1.64*results.match$se) == sign(results.match$coef + 1.64*results.match$se)  %>% as.character

            #exporting and plotting
                        
                        #r data
                        save(results.match, file="output/results_match.Rdata")
                        