# Script to replicate bootstrap differences tests from Bergner, Desmarais, and Hird
# Journal of Behavioral Public Administration

varNames <- c("Group","Treatment","Clicked")

type1 <- c("Activist","Control","Click")
type2 <- c("Activist","Control","NoClick")
type3 <- c("Activist","PeerReview","Click")
type4 <- c("Activist","PeerReview","NoClick")
type5 <- c("Activist","JournalName","Click")
type6 <- c("Activist","JournalName","NoClick")
type7 <- c("Expert","Control","Click")
type8 <- c("Expert","Control","NoClick")
type9 <- c("Expert","PeerReview","Click")
type10 <- c("Expert","PeerReview","NoClick")
type11 <- c("Expert","JournalName","Click")
type12 <- c("Expert","JournalName","NoClick")

rows <- c(7519,33031-7519, 7363,32895-7363,6901,29183-6901,402,2039-402,341,1945-341,301,1919-301)

rowVals <- list(type1,type2,type3,type4,type5,type6,type7,type8,type9,type10,type11,type12)

experimentData <- NULL
for(i in 1:length(rowVals)){
	dati <- matrix(rep(rowVals[[i]],rows[i]),rows[i],3,byrow=T)
	experimentData <- rbind(experimentData,dati)
}

colnames(experimentData) <- varNames

experimentData <- data.frame(experimentData,stringsAsFactors=F)

activistData <- experimentData[experimentData$Group=="Activist",]
expertData <- experimentData[experimentData$Group=="Expert",]
 
### Bootstrap differences in differences
nboot <- 1000
set.seed(5)
diffInDiff <- numeric(nboot)
for(i in 1:nboot){
	expertDati <- expertData[sample(1:nrow(expertData),nrow(expertData),rep=T),]
	activistDati <- activistData[sample(1:nrow(activistData),nrow(activistData),rep=T),]
	expertDiff <- mean((expertDati$Clicked=="Click")[which(expertDati$Treatment=="JournalName")])-mean((expertDati$Clicked=="Click")[which(expertDati$Treatment=="Control")])
	activistDiff <- mean((activistDati$Clicked=="Click")[which(activistDati$Treatment=="JournalName")])-mean((activistDati$Clicked=="Click")[which(activistDati$Treatment=="Control")])
	diffInDiff[i] <- activistDiff-expertDiff
	if(i/100==round(i/100)) print(i)
 }

pdf("bootstrap_hist.pdf",width=5,height=4,xlim=c(0,0.1))
hist(diffInDiff,col="grey65",xlab="Difference in Difference Estimate")
dev.off()
 



 