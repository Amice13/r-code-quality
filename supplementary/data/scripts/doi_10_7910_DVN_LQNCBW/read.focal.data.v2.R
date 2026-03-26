### Script for reading in focal data ###
read.focal.data.v2<-function(file){
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
	data<-as.numeric(data)
	
	if(data[1]==0){
		rest<-300
		nflight<-0
		fly<-0
	}else{
		fly<-sum(data)
		rest<-300-fly
		nflight<-length(data)
	}

return(list(site=as.numeric(site),wind.speed=as.numeric(wind.speed),wind.gust=as.numeric(wind.gust),temp=as.numeric(temp),forage=as.numeric(forage),territorial=as.numeric(territorial),time=time,date=date,nflight=nflight,rest=rest,fly=fly))
}