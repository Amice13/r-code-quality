###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
##### NEIGHBORHOOD QUALITY PROXY ANALYSIS
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################




cat("Begin NEIGHBORHOOD QUALITY PROXY ANALYSIS\n")

load("qual.proxies.Rdata")##county level data on crime, %BA and home-ownership rates
head(qual)
dim(qual)

##subset to places where reporting jurisdictions reported 100 percent of their data
qual2<-qual[qual$COVIND==100, ]
dim(qual2)


c1<-as.character(cor(qual2$vpc, qual2$pctba, use="complete.obs"))##violent crimes per cap and %BA
cat("Correlation between county-level violent crimes per capita and % of residents with BA:\n")
cat(paste(c1,"\n",sep=""))
c2<-cor(qual2$vpc, qual2$pctown, use="complete.obs")##violent crimes per cap and home-ownership rate
cat("Correlation between county-level violent crimes per capita and % of residents who own home:\n")
cat(paste(c2,"\n",sep=""))

cat("End NEIGHBORHOOD QUALITY PROXY ANALYSIS\n")
