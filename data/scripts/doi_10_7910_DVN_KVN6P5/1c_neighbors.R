
#setup

        rm(list=ls())
        setwd("/Users/Vincent/Dropbox/Historical Legacies/replication")
        color.printing <- FALSE
        
        #load scripts
        source("scripts/libraries.R")
        source("scripts/helpers.R")
        load(rev(list.files(path="input", pattern="^data", full.names=TRUE))[1])  #load most recent data file
        load("input/overlap.Rdata") # list of which countries we're excluding as North Africa
        
#create neighbors
                
        #load data onto the shapefile, which has shorter column names that I don't want
        bind <- readOGR(rev(list.files(path="input", pattern="^bind", full.names=TRUE))[1], verbose=FALSE) #load most recent shape file
        projection(bind) <- unproj
        bind <- bind[!(bind@data$country_ov %in% overlap),] #subset to sub-saharan africa
        stopifnot(all(as.character(bind@data$TRIBE_NAME) == data$TRIBE_NAME))  #confirm that the data frames line up
        bind@data <- data
                
        #make the polygon ids the same as the spatial dataframe rownames
        new_IDs = rownames(bind@data)
        for (i in 1:length(slot(bind, "polygons"))){
            slot(slot(bind, "polygons")[[i]], "ID") = new_IDs[i]
        }

        #choose neighbors
        
                #based on distance
                bind <- spTransform(bind, proj)
                neighbors <- gWithinDistance(bind, byid=TRUE, dist = 100000)  #ethnic groups <100km apart
                neighbors <- apply(neighbors, 1, function(x) rownames(bind@data)[x])  #select only the neighbors
                
        #create dyads dataframe
                
                #lots of steps
                names(neighbors) <- rownames(bind@data)
                neighbors <- data.frame("left"=rep(names(neighbors), times=unlist(lapply(neighbors, length))),  "right"=unlist(neighbors)) #turn into dyads
                neighbors$left <- as.character(neighbors$left)  #format as character
                neighbors$right <- as.character(neighbors$right)  #format as character
                stopifnot(bind[1,]@data$TRIBE_NAME == bind["2",]@data$TRIBE_NAME ) #these should be the same
        
                #check that it is working correctly
                if(FALSE){
                neighbors[neighbors$left=="105",]
                    bind <- spTransform(bind, unproj)
                    bind.sub <- bind[rownames(bind@data) %in% neighbors[neighbors$left=="105","right"],]
                    plot(bind)
                    plot(bind.sub, col="blue", add=TRUE)
                   plot(bind["105",], col="red", add=TRUE)
                    bind <- spTransform(bind, proj)
                }
        
                
                #add more information, the left and right variables are rownames so they will pick out the right value
                neighbors$left_name <- bind@data[neighbors$left,"TRIBE_NAME"]
                neighbors$right_name <- bind@data[neighbors$right,"TRIBE_NAME"]
                neighbors$left_type <- bind@data[neighbors$left,"type"] %>% as.character
                neighbors$right_type <- bind@data[neighbors$right,"type"] %>% as.character
                neighbors$left_nlight <- bind@data[neighbors$left,"nlight"] 
                neighbors$right_nlight <- bind@data[neighbors$right,"nlight"]
                neighbors$left_infmor <- bind@data[neighbors$left,"dhs_infmort_ratio"] 
                neighbors$right_infmor <- bind@data[neighbors$right,"dhs_infmort_ratio"]
                neighbors$left_school <- bind@data[neighbors$left,"avg_school"] 
                neighbors$right_school <- bind@data[neighbors$right,"avg_school"]
                
                #subset to relevant dyands
                neighbors <- neighbors[neighbors$left_type=="islam" | neighbors$right_type=="islam",] #at least one of the pair Islamic
                neighbors <- neighbors[neighbors$left_type  != neighbors$right_type,]  #but not both
                
                #check which are getting included
                
                if(FALSE){
                    included <- unique(c(neighbors[,"left"], neighbors[,"right"]))
                    length(included)  #284 states
                    bind <- spTransform(bind, unproj)
                    bind.islam <- bind[rownames(bind@data) %in% included & bind@data$type == "islam",]  #115 states
                    bind.pagan <- bind[rownames(bind@data) %in% included & bind@data$type == "pagan",]  #68 states
                    bind.stateless <- bind[rownames(bind@data) %in% included & bind@data$type == "stateless",]  #101 states
                    
                    plot(bind)
                    plot(bind.pagan, col="blue", add=TRUE)
                    plot(bind.islam, col="green", add=TRUE)
                    plot(bind.stateless, col="red", add=TRUE)
                    
                    bind <- spTransform(bind, proj)
                }
                
                #rename left and right type for stateless to pagan to fit code below
                neighbors$left_type <- ifelse(neighbors$left_type == "stateless", "pagan", neighbors$left_type)
                neighbors$right_type <- ifelse(neighbors$right_type == "stateless", "pagan", neighbors$right_type)
                
                #reorganize the DV so that its based on state type and not left or right
                neighbors$islam_nlight <- ifelse(neighbors$left_type == "islam", neighbors$left_nlight, neighbors$right_nlight)
                neighbors$pagan_nlight <- ifelse(neighbors$left_type == "pagan", neighbors$left_nlight, neighbors$right_nlight)
                neighbors$islam_infmor <- ifelse(neighbors$left_type == "islam", neighbors$left_infmor, neighbors$right_infmor)
                neighbors$pagan_infmor <- ifelse(neighbors$left_type == "pagan", neighbors$left_infmor, neighbors$right_infmor)
                neighbors$islam_school <- ifelse(neighbors$left_type == "islam", neighbors$left_school, neighbors$right_school)
                neighbors$pagan_school <- ifelse(neighbors$left_type == "pagan", neighbors$left_school, neighbors$right_school)
        
                #run t-tests, paired because the observations are intentinoally grouped
                t.nlight <- t.test(x= neighbors$islam_nlight, y = neighbors$pagan_nlight, paired=TRUE) 
                t.school <- t.test(x= neighbors$islam_school, y = neighbors$pagan_school, paired=TRUE) 
                t.infmor <- t.test(x= neighbors$islam_infmor, y = neighbors$pagan_infmor, paired=TRUE)
                
                #set it up uni-directionally
                neighbors$diff_nlight <- neighbors$islam_nlight - neighbors$pagan_nlight
                neighbors$diff_infmor <- neighbors$islam_infmor - neighbors$pagan_infmor
                neighbors$diff_school <- neighbors$islam_school - neighbors$pagan_school

                #set up the cluster-robust standard errors
                source("scripts/dyad_clusterSE.R")
                neighbors$dyads <- paste0(neighbors$left, "-", neighbors$right)
                
        #night lights 

                #create index objects
                fit_nlight <- lm(diff_nlight ~ 1, data=neighbors)
                index <- unique(c(neighbors$left, neighbors$right))
                dyad.mat <- neighbors[,c("left", "right")]
                
                #run the analysis
                cl <- makeCluster(4)
                registerDoSNOW(cl)
                dyad.vcov_nlight <- dyad.robust.se(neighbors, fit_nlight, index, dyad.mat)
                stopCluster(cl)

                #replace normal standard errors with clustered version
                se <- c(vcov(fit_nlight), dyad.vcov_nlight) %>% sqrt
                stargazer(fit_nlight, fit_nlight, se=se, type="text")

        #School Years 
        
                #some of the observations are missing school data
                neighbors_school <- neighbors[!is.na(neighbors$diff_school),]
                
                #create index objects
                fit_school <- lm(diff_school ~ 1, data=neighbors_school)
                index <- unique(c(neighbors_school$left, neighbors_school$right))
                dyad.mat <- neighbors_school[,c("left", "right")]
                
                #run the analysis
                cl <- makeCluster(4)
                registerDoSNOW(cl)
                dyad.vcov_school <- dyad.robust.se(neighbors_school, fit_school, index, dyad.mat)
                stopCluster(cl)
                
                #replace normal standard errors with clustered version
                se <- c(vcov(fit_school), dyad.vcov_school) %>% sqrt
                stargazer(fit_school, fit_school, se=se, type="text")
        
        #infant mortality 
        
                #some of the observations are missing infant mortality data
                neighbors_infmor <- neighbors[!is.na(neighbors$diff_infmor),]
                
                #create index objects
                fit_infmor <- lm(diff_infmor ~ 1, data=neighbors_infmor)
                index <- unique(c(neighbors_infmor$left, neighbors_infmor$right))
                dyad.mat <- neighbors_infmor[,c("left", "right")]
                
                #run the analysis
                cl <- makeCluster(4)
                registerDoSNOW(cl)
                dyad.vcov_infmort <- dyad.robust.se(neighbors_infmor, fit_infmor, index, dyad.mat)
                stopCluster(cl)
                
                #replace normal standard errors with clustered version
                se <- c(vcov(fit_infmor), dyad.vcov_infmort) %>% sqrt
                stargazer(fit_infmor, fit_infmor, se=se, type="text")
                
                #save some files
                save(fit_nlight, fit_school, fit_infmor, 
                     dyad.vcov_nlight, 
                     dyad.vcov_school, 
                     dyad.vcov_infmort, file="output/neighbors.Rdata")
                
                
        #create a results matrix
                
                #create a matrix
                results.dyads <- as.data.frame(matrix(NA, nrow=3, ncol=0))
                results.dyads$var <- c("Night Lights", "Infant Mortality", "School Years")
                results.dyads$type <- c("Neighbors")
                results.dyads$coef <- c(fit_nlight$coefficients, fit_infmor$coefficients, fit_school$coefficients)
                results.dyads$se <- c(dyad.vcov_nlight, dyad.vcov_infmort, dyad.vcov_school) %>% sqrt
                results.dyads$sig <- sign(results.dyads$coef - 1.64*results.dyads$se) == sign(results.dyads$coef + 1.64*results.dyads$se)  %>% as.character
                results.dyads$x <- 1
                
        #exporting and plotting
        
                #r data
                save(results.dyads, file="output/results_dyads.Rdata")
        
        #comprehensive table

                #create a table
                table <- matrix(NA, ncol=4, nrow=4)
                rownames(table) <- c("Islamic", "Traditional", "Difference", "p-value")
                colnames(table) <- c("N", "Night Lights", "Infant Mortality", "School Years")
                table["Islamic",2:4] <- colMeans(neighbors[,grepl("islam", colnames(neighbors))], na.rm=TRUE)
                table["Traditional",2:4] <- colMeans(neighbors[,grepl("pagan", colnames(neighbors))], na.rm=TRUE)
                table["Difference",2:4] <- table["Islamic",2:4] - table["Traditional",2:4]
                table["Islamic","N"] <- length(unique(c(neighbors[neighbors$left_type=="islam", "left_name"], neighbors[neighbors$right_type=="islam", "right_name"])))
                table["Traditional","N"] <- length(unique(c(neighbors[neighbors$left_type=="pagan", "left_name"], neighbors[neighbors$right_type=="pagan", "right_name"])))
                print(xtable(table, digits=2, caption = "Neighbors Analysis", label = "tab:pairs"), file="tables/tab_neighbors.txt")
                results <- table

        #make a map
                
                included <- unique(c(neighbors$left, neighbors$right))
                bind.tmp <- bind  #create a new object because I'm going to mess with the data
                bind.tmp$type <- ifelse(!(rownames(bind@data) %in% included), "excluded", as.character(bind.tmp$type))
                bind.tmp@data$id <- rownames(bind.tmp@data)
                tribes.df     <- suppressMessages(fortify(bind.tmp))
                tribes.df     <- left_join(tribes.df, bind.tmp@data, by="id")

                x <- c(extent(bind[rownames(bind@data) %in% included,])@xmin, extent(bind[rownames(bind@data) %in% included,])@xmax)
                y <- c(extent(bind[rownames(bind@data) %in% included,])@ymin, extent(bind[rownames(bind@data) %in% included,])@ymax)

                plot <- 
                    ggplot(tribes.df, aes(x=long, y=lat, group=group, fill=id)) +
                    geom_polygon(data=tribes.df, aes(fill=type),  color=border, size=.1) +
                    scale_fill_manual(values=c("islam" = fill1, "excluded" = fill4, "pagan" = fill2, "stateless"= fill3), 
                                      labels= c("islam"="Islamic", "pagan"="Non-Islamic", "excluded" = "Excluded", "stateless"="Stateless"),
                                      name="Kingdom History") +
                    theme_bw() +
                    theme(
                        panel.border = element_blank(),
                        axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.title.y=element_blank(),
                        axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.spacing = unit(c(0, 0, 0, 0), "cm")) +
                    coord_fixed(xlim=x, ylim=y)

                ggsave(file=paste0("charts/fig_neighbors", ifelse(color.printing, "_col", "_bw"), ".png"), plot = plot, width = 6.5, height=3.25, units="in")
                

