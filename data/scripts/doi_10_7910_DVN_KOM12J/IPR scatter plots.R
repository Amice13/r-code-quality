

library(ggplot2)
library(gridExtra)

load(file=file.choose()) ##### Load the following objects after running factor analysis
				 # reg.and.monitor.private.annual.inspections.RData
				 # reg.and.monitor.private.regular.inspections.RData
				 # public.quality.control.RData
				 # post.f1.IPR.scores.RData
				 # only.central.medical.stores.RData
				 # basic.intitutions.RData

ls()

private.market.annual <- summary(reg.and.monitor.private.annual.inspections)$statistics
rm(reg.and.monitor.private.annual.inspections)
private.market.regular <- summary(reg.and.monitor.private.regular.inspections)$statistics
rm(reg.and.monitor.private.regular.inspections)
public.distribution <- summary(public.quality.control)$statistics
rm(public.quality.control)
IPR <- summary(post.f1.IPR.scores)$statistics
rm(post.f1.IPR.scores)
public.with.medical.stores <- summary(f1.central.medical.stores)$statistics
rm(f1.central.medical.stores)
only.storage <- summary(only.central.medical.stores)$statistics
rm(only.central.medical.stores)
regulation <- summary(basic.intitutions)$statistics
rm(basic.intitutions)


#####
##### Extract the estimated phi
#####

IPR <- IPR*-1	# Flip the scores so that positive represents a highter IPR score.


rownames(private.market.annual)
private.market.annual.phi <- as.data.frame(private.market.annual[53:130,1])
colnames(private.market.annual.phi) <- c("private.annual.Phi")

rownames(private.market.regular)
private.market.regular.phi <- as.data.frame(private.market.regular[53:130,1])
colnames(private.market.regular.phi ) <- c("private.regular.Phi")

rownames(public.distribution)
public.distribution.phi <- as.data.frame(public.distribution[31:108])
colnames(public.distribution.phi) <- c("distribution.Phi")

rownames(regulation)
regulation.phi <- as.data.frame(regulation[29:106])
colnames(regulation.phi) <- c("regulation.Phi")

rownames(IPR)
IPR.phi <- as.data.frame(IPR[17:94])
colnames(IPR.phi) <- c("IPR.phi")

rownames(public.with.medical.stores)
public.with.medical.stores.phi <- as.data.frame(public.with.medical.stores[41:109])
colnames(public.with.medical.stores.phi) <- c("stores.phi")

rownames(only.storage)
only.storage.phi <- as.data.frame(only.storage[11:79])
colnames(only.storage.phi) <- c("only.storage.phi")


##### Made a matrix with only the phi scores

phi.data <- cbind(private.market.annual.phi, private.market.regular.phi, public.distribution.phi, IPR.phi, regulation.phi) 
summary(phi.data)

##### Make the matrix used to produce the scatter plot graphics

scatter.plot.data <- cbind(pharma.factors$Country, pharma.factors$Country_Code, phi.data)
colnames(scatter.plot.data)[1:2] <- c("Country", "Country_Code") 
scatter.plot.data <- merge(scatter.plot.data, gdp.data, , by.x="Country_Code", by.y="Country.Code", all.x=T)
scatter.plot.data <- as.data.frame(scatter.plot.data)


central.medical.store.data <- subset(pharma.factors, X7.02.01 == 1) 
public.procurement.phi <- cbind(central.medical.store.data$Country, central.medical.store.data$Country_Code, public.with.medical.stores.phi, only.storage.phi)
colnames(public.procurement.phi) <- c("Country", "Country_Code", "public.control.phi", "only.storage.phi")

public.procurement.scatter.data <- merge(public.procurement.phi, scatter.plot.data, by=c("Country", "Country"), all.x=T ) 
public.procurement.scatter.data <- as.data.frame(public.procurement.scatter.data)
colnames(public.procurement.scatter.data)[5] <- "Country_Code"


save(scatter.plot.data, file= file.choose())
save(public.procurement.scatter.data, file= file.choose())

rm(private.market)
rm(public.distribution)
rm(public.procurement)
rm(regulation)

ls()



#### Load the Lui and Croiz Index 2005 data
#### Load the Park 2005 Data



#####
##### Make scattter plots
#####

title.text <- NULL
y.title <- "Regulation of the Private Market"
x.title <-  "Pharmaceutical IPRs"
  
summary(lm(private.regular.Phi~IPR.phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=IPR.phi, y=private.regular.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=PR.IPR, y=PR.Phi1, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + ylim(-1.8, 1.8)
p <- p + xlim(-1.6,2.1)
p1.1 <- p + annotate("text", x = -1.25, y = -1.4, label = "R^{2}==0.12", parse=TRUE)

print(p1.1)


title.text <- NULL
y.title <- NULL
x.title <-  "Pharmaceutical IPRs (Lui and La Croix)"
  
summary(lm(private.regular.Phi~Lui.Croiz.Index2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=Lui.Croiz.Index2005, y=private.regular.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=PR.Lui.Croiz, y=PR.Phi2, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + ylim(-1.8, 1.7)
p <- p + xlim(-0.1, 3.9)
p1.2 <- p + annotate("text", x = 0.25, y = -1.4, label = "R^{2}==0.03", parse=TRUE)

print(p1.2)


Graphic1 <- grid.arrange(p1.1, p1.2, ncol = 2, nrow = 1, main="Graphic 1. Relationship between Regulation of the Private Market and Pharmaceutical IPRs")





############ distribution.Phi



title.text <- NULL
y.title <- "Regulation of the Public Market"
x.title <-  "Pharmaceutical IPRs"
  
summary(lm(distribution.Phi~IPR.phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=IPR.phi, y=distribution.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=DIS.IPR, y=DIS.Phi1, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + ylim(-2.35, 1.35)
p <- p + xlim(-1.6,2.1)
p2.1 <- p + annotate("text", x = -1.25, y = -1.4, label = "R^{2}==0.03", parse=TRUE)

print(p2.1)


title.text <- NULL
y.title <- NULL
x.title <-  "Pharmaceutical IPRs (Lui and La Croix)"
  
summary(lm(distribution.Phi~Lui.Croiz.Index2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=Lui.Croiz.Index2005, y=distribution.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=DIS.Lui.Croiz, y=DIS.Phi2, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + ylim(-2.35, 1.35)
p <- p + xlim(-0.1, 3.9)
p2.2 <- p + annotate("text", x = 0.25, y = -1.4, label = "R^{2}==0.001", parse=TRUE)

print(p2.2)


Graphic2 <- grid.arrange(p2.1, p2.2, ncol = 2, nrow = 1, main="Graphic 2. Relationship between Regulation of the Public Market and Pharmaceutical IPRs")



###########



title.text <- NULL
y.title <- "Regulation of the Private Market"
x.title <-  "International Patent Protection (Park)"
  
summary(lm(private.regular.Phi~PG.RP.2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=PG.RP.2005, y=private.regular.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=PG.RP.1, y=PR.Phi3, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + ylim(-1.5, 1.75)
p <- p + xlim(1.75, 4.6)
p3.1 <- p + annotate("text", x = 2, y = -1.4, label = "R^{2}==0.004", parse=TRUE)

print(p3.1)

y.title <- "Regulation of the Public Market"
summary(lm(distribution.Phi~PG.RP.2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=PG.RP.2005, y=distribution.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=PG.RP.2, y=DIS.Phi3, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + ylim(-1.5, 1.75)
p <- p + xlim(1.75, 4.6)
p3.2 <- p + annotate("text", x = 2, y = -1.4, label = "R^{2}==0.02", parse=TRUE)

print(p3.2)

Graphic3 <- grid.arrange(p3.1, p3.2, ncol = 2, nrow = 1, main="Graphic 3. Relationship between Regulation of Pharmaceuticals and Interational Patent Protection")




####################




title.text <- NULL
y.title <- "Regulation of the Private Market"
x.title <-  "Regulatory Infrastructure"
  
summary(lm(private.regular.Phi~regulation.Phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=regulation.Phi, y=private.regular.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=REG.Phi1, y=PR.Phi4, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + ylim(-2.35, 1.8)
p <- p + xlim(-2.15, 1.9)
p4.1 <- p + annotate("text", x = 1, y = -2, label = "R^{2}==0.49", parse=TRUE)

print(p4.1)

y.title <- "Regulation of the Public Market"
summary(lm(distribution.Phi~regulation.Phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=regulation.Phi, y=distribution.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(x=REG.Phi2, y=DIS.Phi4, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + ylim(-2.35, 1.8)
p <- p + xlim(-2.15, 1.9)
p4.2 <- p + annotate("text", x = 1, y = -2, label = "R^{2}==0.29", parse=TRUE)

print(p4.2)

Graphic4 <- grid.arrange(p4.1, p4.2, ncol = 2, nrow = 1, main="Graphic 4. Relationship between Regulation of Pharmaceuticals and Regulatory Infrastructure")









#######
#######################  Individual Graphics #########
#######



title.text <- "Graphic 1a: Relationship between Regulation of the Private Market and Pharmaceutical IPRs"
y.title <- "Regulation of the Private Market"
x.title <-  "Pharmaceutical IPRs"
  
summary(lm(private.regular.Phi~IPR.phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=IPR.phi, y=private.regular.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))
p <- p + geom_text(aes(x=PR.IPR, y=PR.Phi1, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-1.8, 1.8)
p <- p + xlim(-1.6,2.1)
p1.1 <- p + annotate("text", x = -1.25, y = -1.4, label = "R^{2}==0.12", parse=TRUE)

print(p1.1)


title.text <- "Graphic 1b: Relationship between Regulation of the Private Market and Pharmaceutical IPRs"
y.title <- "Regulation of the Private Market"
x.title <- "Pharmaceutical IPRs (Lui and La Croix)"
  
summary(lm(private.regular.Phi~Lui.Croiz.Index2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=Lui.Croiz.Index2005, y=private.regular.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))
p <- p + geom_text(aes(x=PR.Lui.Croiz, y=PR.Phi2, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-1.8, 1.7)
p <- p + xlim(-0.1, 3.9)
p1.2 <- p + annotate("text", x = 0.25, y = -1.4, label = "R^{2}==0.03", parse=TRUE)

print(p1.2)




############ distribution.Phi



title.text <- "Graphic 2a: Relationship between Regulation of the Public Market and Pharmaceutical IPRs"
y.title <- "Regulation of the Public Market"
x.title <- "Pharmaceutical IPRs"
  
summary(lm(distribution.Phi~IPR.phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=IPR.phi, y=distribution.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))
p <- p + geom_text(aes(x=DIS.IPR, y=DIS.Phi1, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-2.35, 1.35)
p <- p + xlim(-1.6,2.1)
p2.1 <- p + annotate("text", x = -1.25, y = -1.4, label = "R^{2}==0.03", parse=TRUE)

print(p2.1)


title.text <- "Graphic 2b: Relationship between Regulation of the Public Market and Pharmaceutical IPRs"
y.title <- "Regulation of the Public Market"
x.title <- "Pharmaceutical IPRs (Lui and La Croix)"
  
summary(lm(distribution.Phi~Lui.Croiz.Index2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=Lui.Croiz.Index2005, y=distribution.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))
p <- p + geom_text(aes(x=DIS.Lui.Croiz, y=DIS.Phi2, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-2.35, 1.35)
p <- p + xlim(-0.1, 3.9)
p2.2 <- p + annotate("text", x = 0.25, y = -1.4, label = "R^{2}==0.001", parse=TRUE)

print(p2.2)



###########



title.text <- "Graphic 3a: Relationship between Regulation of Pharmaceuticals and Interational Patent Protection"
y.title <- "Regulation of the Private Market"
x.title <-  "International Patent Protection (Park)"
  
summary(lm(private.regular.Phi~PG.RP.2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=PG.RP.2005, y=private.regular.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))
p <- p + geom_text(aes(x=PG.RP.1, y=PR.Phi3, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-1.5, 1.75)
p <- p + xlim(1.75, 4.6)
p3.1 <- p + annotate("text", x = 2, y = -1.4, label = "R^{2}==0.004", parse=TRUE)

print(p3.1)

title.text <- "Graphic 3b: Relationship between Regulation of Pharmaceuticals and Interational Patent Protection"
y.title <- "Regulation of the Public Market"
x.title <-  "International Patent Protection (Park)"
summary(lm(distribution.Phi~PG.RP.2005, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=PG.RP.2005, y=distribution.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))
p <- p + geom_text(aes(x=PG.RP.2, y=DIS.Phi3, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-1.5, 1.75)
p <- p + xlim(1.75, 4.6)
p3.2 <- p + annotate("text", x = 2, y = -1.4, label = "R^{2}==0.02", parse=TRUE)

print(p3.2)


####################




title.text <- "Graphic 4a: Relationship between Regulation of Pharmaceuticals and Regulatory Infrastructure"
y.title <- "Regulation of the Private Market"
x.title <-  "Regulatory Infrastructure"
  
summary(lm(private.regular.Phi~regulation.Phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=regulation.Phi, y=private.regular.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))

p <- p + geom_text(aes(x=REG.Phi1, y=PR.Phi4, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-2.35, 1.8)
p <- p + xlim(-2.15, 1.9)
p4.1 <- p + annotate("text", x = 1, y = -2, label = "R^{2}==0.49", parse=TRUE)

print(p4.1)

title.text <- "Graphic 4b: Relationship between Regulation of Pharmaceuticals and Regulatory Infrastructure"
y.title <- "Regulation of the Public Market"
x.title <-  "Regulatory Infrastructure"
summary(lm(distribution.Phi~regulation.Phi, data=scatter.plot.data))
p <- ggplot(scatter.plot.data, aes(x=regulation.Phi, y=distribution.Phi, group=Region.LatinAmerica))
p <- p + geom_point(aes(colour=Region.LatinAmerica))

p <- p + geom_text(aes(x=REG.Phi2, y=DIS.Phi4, label=NewCountry), hjust=-.1, vjust=-.1, position="jitter", size= 2.5, angle=0)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + theme(plot.title = element_text(size=12, face="bold", vjust=1, lineheight=1))
p <- p + theme(legend.position="bottom")
p <- p + theme(legend.text = element_text(size = 7))
p <- p + scale_colour_hue("Region",
		breaks = c("Caribbean Islands", "Europe","Latin America", "North Africa", "South and South East Asia", "Sub-Saharan Africa", "West and Central Asia"),
		labels = c("Caribbean", "Europe", "L. America", "N. Africa", "S & SE Asia", "SS Africa", "W. & C. Asia"))
p <- p + ylim(-2.35, 1.8)
p <- p + xlim(-2.15, 1.9)
p4.2 <- p + annotate("text", x = 1, y = -2, label = "R^{2}==0.29", parse=TRUE)

print(p4.2)




















