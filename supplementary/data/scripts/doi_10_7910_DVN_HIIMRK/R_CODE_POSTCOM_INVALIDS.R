#######################################################
########## INVALID VOTES IN POST-COMMUNIST EUROPE #####
#######################################################
# Author: JAKUB LYSEK, jakub.lysek@gmail.com
# ORCID: 0000-0003-3583-9917
# DATA AND MODELS FOR THE MANUSCRIPT:
# "TURNING OUT BUT NOT VOTING: Invalid Ballots in Post-Communist Parliamentary Elections"
# Input File: POSTCOM_INVALIDS.dta *
  

#BASIC GRAPHs
hist(PAR_INVALID$Invalidvotes, main="Histogram", 
     xlab="Invalid votes %", 
     ylab="Frequency",
     border="black", 
     col="grey",
     xlim=c(0, 16),
     las=1, 
     breaks=20,
     prob=FALSE)

boxplot(Invalidvotes~Country,data=PAR_INVALID, main="Invalid votes in %", 
        xlab="Country", ylab="Invalid votes in %")

#Regression OLS
MODEL_INVALID <- lm(Invalidvotes ~ IntDemTradition + FHAVScore + First_election + EarlyElections 
                    + Concurrenceelections + TheleastsquaresindexLSq + Effectivenumberofparliamentarypa + Votedifferential1th2thin 
                    + Giniindex + LogGDP_per_capita + GDPgrowthin + Unemployment + Literacyrate 
                    + Urbanpopulation + Year, data=PAR_INVALID)
summary(MODEL_INVALID) 
layout(matrix(c(1,2,3,4),2,2)) #
plot(MODEL_INVALID)

#Albanie elections 1996  outlier |(60), Serbia elections 1997 a 1993 (77,78)

#CORRELATION MATRIX
COR_DATA_IN <- PAR_INVALID[, c(7, 5, 9, 11, 12, 14, 16, 17, 19, 23, 25, 28, 30,
                               32, 33, 36, 40, 42, 45 ,46, 47, 48, 49, 81)]
COR_INVALID <-cor(COR_DATA_IN, use="complete.obs", method="pearson")
summary(COR_INVALID)
head(round(COR_INVALID,2))
library(corrplot)
corrplot(COR_INVALID, method="number", type="upper", tl.col = "black",  number.cex = .7, cl.cex = .7, tl.cex = .7)

#socio-economic variables only
KOR_SOCEK <- PAR_INVALID[, c(7, 9, 23, 25, 28, 30,
                               32, 33, 36, 45 ,46, 47, 48)]
KOR_SOCEK <-cor(KOR_SOCEK, use="complete.obs", method="pearson")

colnames(KOR_SOCEK) <- c("Invalid votes", "Tunrout", "Gini index", "Logged per capita GDP", "GDP growth in %","Unemployment (%)",
                         "Literacy rate (%)", "Urban population (%)", "Freedom House Score", "WGI index", "TCI Index", "VDEM-corruption", "Ethnic fragmentation")
                          
rownames(KOR_SOCEK) <-  c("Invalid votes", "Tunrout", "Gini index", "Logged per capita GDP", "GDP growth in %","Unemployment (%)",
                          "Literacy rate (%)", "Urban population (%)", "Freedom House Score", "WGI index", "TCI Index", "VDEM-corruption", "Ethnic fragmentation")


#Figures in the Appendix!

#Correlogram
# grey version
corrplot(KOR_SOCEK, method="number", type="upper", tl.col = "black",  number.cex = 1, cl.cex = 1, tl.cex = 1,
         col=colorRampPalette(c("black","white","#0D0E0E"))(400))
#color version
corrplot(KOR_SOCEK, method="number", type="upper", tl.col = "black",  number.cex = 1, cl.cex = 1, tl.cex = 1)

#Line graph in time
#overlay
library(ggplot)
ggplot(PAR_INVALID, aes(x = Year, y = Invalidvotes, group = Country, color = Country)) +
  geom_line(size=1) + labs(x = "Year", y = "Invalid votes") + scale_color_grey() + theme_classic()  

#by country
GRAF <- ggplot(PAR_INVALID, aes(x = Year, y = Invalidvotes)) + geom_line(size=1) + facet_wrap(~Country)
GRAF + labs(x = "Year", y = "Invalid votes")


