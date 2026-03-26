setwd(pathData)

load("prioGridTemp.rda")
					#plot(prio.grid.temp)
						prio.sf <- st_as_sf(prio.grid.temp)
							world <- ne_countries(scale = "medium", returnclass = "sf")
				
				africa <- world[world$continent == "Africa",]
									
					p1 <- 	ggplot(data = africa) +
							geom_sf()+
    						geom_sf(data=prio.sf,aes(fill = matched),lwd=0)+
    						#scale_fill_discrete(name="Matched",labels=c("Control (no UN deployment)", "Unmatched", "Treated (UN deployment)"))+
    						scale_fill_manual(values=c("#e5925b","grey","#5b92e5"),name="Matched",labels=c("Control (no UN deployment)", "Unmatched", "Treated (UN deployment)"))+
    										theme(legend.position="bottom", legend.box = "horizontal")
	
							p2 <-		ggplot(data = africa) +
										geom_sf()+
    									geom_sf(data=prio.sf,aes(fill = un),lwd=0)+
    									scale_fill_manual(values=c("#e5925b","#5b92e5"),name="UN deployment",labels=c("No", "Yes"))+
    										theme(legend.position="bottom", legend.box = "horizontal")
setwd(pathFig)    									
    	ggsave("matchedUN.pdf",plot=p1, width = 22, height = 22, units = c("cm"))
			ggsave("justUN.pdf",plot=p2, width = 22, height = 22, units = c("cm"))