library(MCMCpack)
library(ggplot2)

load(file=file.choose())

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

private.market <- private.market.regular     ### Select regular inspection model


#####
##### Extract the estimated phi
#####


private.market <- private.market*-1		# Flip the scores to better represent subnative knowledge of the countries
IPR <- IPR*-1					# Flip the scores so that positive represents a highter IPR score


rownames(private.market)
private.market.phi <- as.data.frame(private.market.annual[53:130,1])
colnames(private.market.phi) <- c("private.Phi")

rownames(public.distribution)
public.distribution.phi <- as.data.frame(public.distribution[31:108])
colnames(public.distribution.phi) <- c("distribution.Phi")

rownames(public.procurement)
public.procurement.phi <- as.data.frame(public.procurement[41:118)
colnames(public.procurement.phi) <- c("procurement.Phi")


rownames(regulation)
regulation.phi <- as.data.frame(regulation[29:106])
colnames(regulation.phi) <- c("regulation.Phi")


##### Make the matrix used to produce the scatter plot graphics

scatter.plot.data <- cbind(pharma.factors$Country, pharma.factors$Country_Code, private.market.phi, public.distribution.phi, regulation.phi)
colnames(scatter.plot.data) <- c("Country", "Country_Code", "private.Phi", "distribution.Phi", "regulation.Phi") 
scatter.plot.data <- as.data.frame(scatter.plot.data)

central.medical.store.data <- subset(pharma.factors, X7.02.01 == 1) 
public.procurement.phi <- cbind(central.medical.store.data$Country, public.procurement.phi)
colnames(public.procurement.phi) <- c("Country", "procurement.Phi")

public.procurement.scatter.data <- merge(public.procurement.phi,scatter.plot.data, by=c("Country", "Country") ) 
public.procurement.scatter.data <- as.data.frame(public.procurement.scatter.data)

rm(private.market)
rm(public.distribution)
rm(public.procurement)
rm(regulation)



#####
##### Make scattter plots
#####


summary(lm(scatter.plot.data$private.Phi~scatter.plot.data$distribution.Phi))


title.text <- "Relation Between Regulation of Private Market and Monitoring of Public Distribution"
x.title <- "Private Market"
y.title <- "Public Distribution"

p <- ggplot(scatter.plot.data, aes(x=private.Phi, y=distribution.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(label=Country), hjust=-.05, vjust=-.05, position="jitter", vjust=0, size= 3)
p <- p+ labs(list(title = title.text, x = x.title, y = y.title))
p <- p + ylim(-2.5,2)
p <- p + xlim(-2.5, 2.55)
p <- p + annotate("text", x = 1.5, y = -2.25, label = "R^{2}==0.365", parse=TRUE)

print(p)



summary(lm(public.procurement.scatter.data$private.Phi~public.procurement.scatter.data$procurement.Phi))

title.text <- "Relation Between Regulation of Private Market and Quality Control of Public Medicines"
x.title <- "Private Market"
y.title <- "Quality Control of Public Medicines"

p <- ggplot(public.procurement.scatter.data, aes(x=private.Phi, y=procurement.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(label=Country), hjust=-.05, vjust=-.05, position="jitter", vjust=0, size= 3)
p <- p+ labs(list(title = title.text, x = x.title, y = y.title))
p <- p + ylim(-2.5,2)
p <- p + xlim(-2.5, 2.55)
p <- p + annotate("text", x = 1.5, y = -2.25, label = "R^{2}==0.06", parse=TRUE)
print(p)


summary(lm(public.procurement.scatter.data$distribution.Phi~public.procurement.scatter.data$procurement.Phi))

title.text <- "Relation Between Monitoring of Public Distribution and Quality Control of Public Medicines"
x.title <- "Public Distribution"
y.title <- "Quality Control of Public Medicines"

p <- ggplot(public.procurement.scatter.data, aes(x=distribution.Phi, y=procurement.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(label=Country), hjust=-.05, vjust=-.05, position="jitter", vjust=0, size= 3)
p <- p+ labs(list(title = title.text, x = x.title, y = y.title))
p <- p + ylim(-2.5,2)
p <- p + xlim(-2.5, 2.55)
p <- p + annotate("text", x = 1.5, y = -2.25, label = "R^{2}==0.26", parse=TRUE)
print(p)



summary(lm(scatter.plot.data$private.Phi~scatter.plot.data$regulation.Phi))


title.text <- "Relation Between Regulatory Infrastructure and Regulation of the Private Market"
x.title <- "Regulatory Infrastructure"
y.title <- "Regulation of the Private Market"

p <- ggplot(scatter.plot.data, aes(x=regulation.Phi, y=private.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(label=Country), hjust=-.05, vjust=-.05, position="jitter", vjust=0, size= 3)
p <- p + labs(list(title = title.text, x = x.title, y = y.title))
p <- p + ylim(-2.5,2.5)
p <- p + xlim(-2.5, 2.55)
p <- p + annotate("text", x = 1.5, y = -2.25, label = "R^{2}==0.58", parse=TRUE)
print(p)



summary(lm(scatter.plot.data$distribution.Phi~scatter.plot.data$regulation.Phi))


title.text <- "Relation Between Regulatory Infrastructure and Monitoring of Public Distribution"
x.title <- "Regulatory Infrastructure"
y.title <- "Monitoring of Public Distribution"

p <- ggplot(scatter.plot.data, aes(x=regulation.Phi, y=distribution.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(label=Country), hjust=-.05, vjust=-.05, position="jitter", vjust=0, size= 3)
p <- p+ labs(list(title = title.text, x = x.title, y = y.title))
p <- p + ylim(-3,2.5)
p <- p + xlim(-2.5, 2.55)
p <- p + annotate("text", x = 1.5, y = -2.25, label = "R^{2}==0.27", parse=TRUE)
print(p)


summary(lm(public.procurement.scatter.data$procurement.Phi~public.procurement.scatter.data$regulation.Phi))

title.text <- "Relation Between Regulatory Infrastructure and Quality Control of Public Medicines"
x.title <- "Regulatory Infrastructure"
y.title <- "Quality Control of Public Medicines"

p <- ggplot(public.procurement.scatter.data, aes(x=regulation.Phi, y=procurement.Phi))
p <- p + geom_point()
p <- p + geom_text(aes(label=Country), hjust=-.05, vjust=-.05, position="jitter", vjust=0, size= 3)
p <- p+ labs(list(title = title.text, x = x.title, y = y.title))
p <- p + ylim(-2.5,2)
p <- p + xlim(-2.5, 2.55)
p <- p + annotate("text", x = 1.5, y = -2.25, label = "R^{2}==0.09", parse=TRUE)
print(p)


######
######
gdp.per.capita.data <- read.table(file=file.choose(), sep="\t", header=T)
qog.data <- read.table(file=file.choose(), sep="\t", header=T)



gdp.per.capita.data <- subset(gdp.per.capita.data, select = c(Country.Code, X2010._YR2010))
colnames(gdp.per.capita.data) <- c("Country.Code", "GDP_percap_2010")

gdp.data <- subset(gdp.data, select = c(Country.Code,X2010._YR2010))
colnames(gdp.data) <- c("Country.Code", "GDP_2010")

trade.openness <- subset(trade.openness, select = c(Country.Code,X2010._YR2010))
colnames(trade.openness) <- c("Country.Code", "Openness_2010")


new.gdp.data <- cbind(gdp.data, gdp.per.capita.data[,2], trade.openness[,2])
colnames(new.gdp.data) <- c("Country.Code", "GDP_2010", "GDP_percap_2010", "Openness_2010") 


rm(gdp.per.capita.data)
rm(gdp.data)
rm(trade.openness)

scatter.plot.data.with.GDP <- merge(scatter.plot.data, new.gdp.data, by.x="Country_Code", by.y="Country.Code") 
scatter.plot.data.with.GDP <- merge(scatter.plot.data.with.GDP, qog.data, by.x="Country_Code", by.y="ccodealp") 

public.procurement.scatter.data.with.GDP <- merge(public.procurement.scatter.data, new.gdp.data, by.x="Country_Code", by.y="Country.Code")
public.procurement.scatter.data.with.GDP <- merge(public.procurement.scatter.data.with.GDP, qog.data, by.x="Country_Code", by.y="ccodealp")



summary(lm(scatter.plot.data.with.GDP$private.Phi~log(scatter.plot.data.with.GDP$GDP_2010)))
summary(lm(scatter.plot.data.with.GDP$private.Phi~scatter.plot.data.with.GDP$GDP_2010))
summary(lm(scatter.plot.data.with.GDP$private.Phi~log(scatter.plot.data.with.GDP$GDP_percap_2010)))
summary(lm(scatter.plot.data.with.GDP$private.Phi~scatter.plot.data.with.GDP$GDP_percap_2010))
summary(lm(scatter.plot.data.with.GDP$private.Phi~scatter.plot.data.with.GDP$Openness_2010))
summary(lm(scatter.plot.data.with.GDP$private.Phi~scatter.plot.data.with.GDP$functioning_of_government))
summary(lm(scatter.plot.data.with.GDP$private.Phi~scatter.plot.data.with.GDP$icrg_qog))
summary(lm(scatter.plot.data.with.GDP$private.Phi~scatter.plot.data.with.GDP$government_effectiveness))
summary(lm(scatter.plot.data.with.GDP$private.Phi~scatter.plot.data.with.GDP$voice_and_accountability))



summary(lm(scatter.plot.data.with.GDP$distribution.Phi~log(scatter.plot.data.with.GDP$GDP_2010)))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~scatter.plot.data.with.GDP$GDP_2010))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~log(scatter.plot.data.with.GDP$GDP_percap_2010)))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~scatter.plot.data.with.GDP$GDP_percap_2010))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~scatter.plot.data.with.GDP$Openness_2010))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~scatter.plot.data.with.GDP$functioning_of_government))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~scatter.plot.data.with.GDP$icrg_qog))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~scatter.plot.data.with.GDP$government_effectiveness))
summary(lm(scatter.plot.data.with.GDP$distribution.Phi~scatter.plot.data.with.GDP$voice_and_accountability))




summary(lm(scatter.plot.data.with.GDP$regulation.Phi~log(scatter.plot.data.with.GDP$GDP_2010)))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~scatter.plot.data.with.GDP$GDP_2010))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~log(scatter.plot.data.with.GDP$GDP_percap_2010)))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~scatter.plot.data.with.GDP$GDP_percap_2010))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~scatter.plot.data.with.GDP$Openness_2010))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~scatter.plot.data.with.GDP$functioning_of_government))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~scatter.plot.data.with.GDP$icrg_qog))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~scatter.plot.data.with.GDP$government_effectiveness))
summary(lm(scatter.plot.data.with.GDP$regulation.Phi~scatter.plot.data.with.GDP$voice_and_accountability))



summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~log(public.procurement.scatter.data.with.GDP$GDP_2010)))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~public.procurement.scatter.data.with.GDP$GDP_2010))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~log(public.procurement.scatter.data.with.GDP$GDP_percap_2010)))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~public.procurement.scatter.data.with.GDP$GDP_percap_2010))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~public.procurement.scatter.data.with.GDP$Openness_2010))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~public.procurement.scatter.data.with.GDP$functioning_of_government))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~public.procurement.scatter.data.with.GDP$icrg_qog))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~public.procurement.scatter.data.with.GDP$government_effectiveness))
summary(lm(public.procurement.scatter.data.with.GDP$procurement.Phi~public.procurement.scatter.data.with.GDP$voice_and_accountability))







