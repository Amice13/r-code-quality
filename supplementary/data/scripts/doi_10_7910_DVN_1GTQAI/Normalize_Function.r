#  Normalize_Function.r
#  Cory White
#  Systems Biology Group
#  Merck Cambridge Exploratory Science Center
#  Function to normalize and write out cytokine information.  

Normalize<-function(DataName,Outname){
	Data_0<-read.csv(DataName,stringsAsFactors=FALSE,check.names=FALSE)

	#  Subset to relevant days
	K1<-grep("-0",Data_0[,1])
	K2<-grep("control",Data_0[,1])
	Keep<-c(K1,K2)
	Data<-Data_0[Keep,]

	Samples<-Data[,1]
	Data<-Data[,-c(1,ncol(Data))]

	#  Fix non-numeric entries
	Data<-t(Data)
	DataNew<-Data
	Max<-max(as.numeric(DataNew),na.rm=TRUE)
	Min<-min(as.numeric(DataNew),na.rm=TRUE)
	DataNew[grepl("< ",DataNew)]<-0.5*Min
	DataNew[grepl("> ",DataNew)]<-2*Min

	#  Only for using mins and maxes for each marker as opposed to a global min or max.  
	colnames(DataNew)<-Samples

	class(DataNew)<-"numeric"
	Meds<-apply(DataNew,1,FUN=median,na.rm=TRUE)
	#  Get scale factors for normalization.  
	SF<-Meds/mean(Meds)

	DataOut<-sweep(DataNew,1,STATS=SF,FUN="*")
	Names<-rownames(DataOut)

	#  Get log2(x+1) of normalized data
	Datalg2<-log(DataOut+1,2)
	Datalg2<-cbind(Cytokines=Names,Datalg2)
	write.csv(Datalg2,file=paste0("lg2_Normalized_Cytokines_",Outname,".csv"),row.names=FALSE)
	Datalg2
}