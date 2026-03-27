### Script for reading in focal data ###
read.focal.data.v1<-function(file){
	input<-read.csv(file,header=F,stringsAsFactors=F)
	site<-input[1,2]
	wind.speed<-input[2,2]
	wind.gust<-input[3,2]
	temp<-input[4,2]
	time<-input[5,2]
	date<-input[6,2]
	forage<-input[7,2]
	territorial<-input[8,2]
	data<-input[10:nrow(input),1]
	
	foo<-as.numeric(3-nchar(data))
	
	for(i in 1:length(foo)){
		data[i]<-paste(paste(rep("0",foo[i]),collapse=""),data[i],sep="")
	}
	min<-sapply(strsplit(data,""),function(x) x[1])
	sec<-sapply(strsplit(data,""),function(x) paste(x[2:3],collapse=""))
	
	time<-as.numeric(min)*60+as.numeric(sec)
	
	rest<-vector()
	fly<-vector()
	
	for(i in 1:((length(time)-1)/2)){
		foo<-seq(1,length(time)-1,2)
		rest<-c(rest,time[foo[i]]-time[foo[i]+1])
		fly<-c(fly,(time[foo[i]+1]-time[foo[i]+2]))
	}
	rest<-c(rest,time[length(time)])
	
	nflight<-length(fly)
	rest<-sum(rest)
	fly<-sum(fly)
	
	if(!rest+fly==300){
		stop("total time does not equal 5 min")
	}
		return(list(site=as.numeric(site),wind.speed=as.numeric(wind.speed),wind.gust=as.numeric(wind.gust),temp=as.numeric(temp),forage=as.numeric(forage),territorial=as.numeric(territorial),time=time,date=date,nflight=nflight,rest=rest,fly=fly))
}