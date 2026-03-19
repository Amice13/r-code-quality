######################################################################
## Plotting the Positions                                           ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################


# ===========
# = Library =
# ===========

# define oxford colors
oxblue 			<- rgb(0,33,71,maxColorValue = 255)
oxlightblue 	<- rgb(161,196,208,maxColorValue = 255)
oxorange 		<- rgb(207,122,48,maxColorValue = 255)
oxyellow 		<- rgb(245,207,71,maxColorValue = 255)
oxdarkgreen 	<- rgb(105,145,59,maxColorValue = 255)
oxlightgreen 	<- rgb(170, 179,0,maxColorValue = 255)
oxred 			<- rgb(135,36,52,maxColorValue = 255)
oxgrey 			<- "grey65"

# make data
dat.long.all <- data.frame(dat.pres.pos.all80$year, dat.pres.pos.all80$coun, dat.pres.pos.all80$position.b.z, dat.pres.pos.all80$pres)
names(dat.long.all) <- c("year","coun","pos","pres")


# rename
dat.long.all$coun <- car::recode(dat.long.all$coun,
	"'arg'='Argentina';
	 'bra'='Brazil';
	 'chi'='Chile';
	 'col'='Colombia';
	 'cri'='Costa Rica';
	 'ecu'='Ecuador';
	 'gtm'='Guatemala';
	 'mex'='Mexico';
	 'per'='Peru';
	 'pry'='Paraguay';
	 'slv'='El Salvador';
	 'ury'='Uruguay';
	 'ven'='Venezuela'
	")
	
dat.long.all$pres <- car::recode(dat.long.all$pres, "'Kirchner_Cris'='Kirchner C'
														;'Kirchner'='Kirchner N'
														;'Rua'='Rúa'
														;'Dilma'='Rousseff'
														;'Macchi'='Gonzalez'
														;'Pinera'='Piñera'
														;'Rodriguez'='Rodríguez'
														;'Gutierrez'='Gutiérrez'
														;'Arzu'='Arzú'
														;'Pena'='Peña Nieto'
														;'Belaunde'='Belaúnde'
														;'Vazquez'='Vázquez'
														;'Perez'='Pérez'
														;'Chavez'='Chávez'
														;'FebresCordero'='LFC'
														;'DuranBallen'='Dur. Ballén'
														;'CalderonSol'='Cald. Sol'
														;'Calderon'='Calderón'
														;'PerezMolina'='Molina'
														;'Garcia'='García'")



# Reorder the internal factor coding
dat.long.all$coun <- factor(dat.long.all$coun, levels=rev(levels(dat.long.all$coun)))


## Black and White for the paper
pdf("output/figures/figure1.pdf")
print(
    ggplot(dat.long.all, aes(x = year, y = coun, fill = pos)) + 
    	geom_tile() + scale_fill_continuous(low="black", high="grey80") +
    	geom_text(aes(angle = 90, label = dat.long.all$pres), colour="white", size = 2) + 	
    	scale_x_continuous(breaks=seq(1980, 2014,1), expand = c(0,0)) + 
    	xlab(" ") + ylab(" ") +
    	labs(fill='Value') + 
    	theme(axis.text.x=element_text(color = "black", size=8, angle=90, vjust=.8, hjust=0.8)) +
    	theme(axis.text.y=element_text(color = "black", size=12, vjust=.8, hjust=0.8)) + 
    	theme(panel.grid.minor = element_blank(), 
    		panel.background = element_blank(), 
    		panel.grid.major = element_line(
    			colour="grey", 
    			size = .35, 
    			linetype = 3
    		),
    		plot.margin = unit(c(0,0,0,0),"cm"),
    		axis.ticks.x=element_blank(), axis.ticks.y=element_blank()
    	)
)
dev.off()


## Color for pres
#pdf("../figures/heatmaps/heatmap_pres.pdf")

ggplot(dat.long.all, aes(x = year, y = coun, fill = pos)) + 
#	geom_tile() + scale_fill_continuous(low=oxorange, high=oxblue) +
	geom_tile() + scale_fill_continuous(low=oxorange, high=oxblue) +
	geom_text(aes(angle = 90, label = dat.long.all$pres), colour="white", size = 2) + 	
	scale_x_continuous(breaks=seq(1980, 2014,1), expand = c(0,0)) + 
	xlab(" ") + ylab(" ") +
	labs(fill='Value') + 
	theme(axis.text.x=element_text(color = "black", size=8, angle=90, vjust=.8, hjust=0.8)) +
	theme(axis.text.y=element_text(color = "black", size=12, vjust=.8, hjust=0.8)) + 
	theme(panel.grid.minor = element_blank(), 
		panel.background = element_blank(), 
		panel.grid.major = element_line(
			colour="grey", 
			size = .5, 
			linetype = 3
		),
		plot.margin = unit(c(0,0,0,0),"cm"),
		axis.ticks.x=element_blank(), axis.ticks.y=element_blank()
	)
#dev.off()







# Whoever reads this, please make sure to claim a beer with Chris Arnold at the next conference