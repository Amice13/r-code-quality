### Script for reading in observational data ###
require(circular)

read.orientation.data<-function(file){
	input<-read.csv(file,header=F,stringsAsFactors=F)
	site<-input[1,2]
	wind.speed<-input[2,2]
	wind.gust<-input[3,2]
	temp<-input[4,2]
	orientation<-input[5,2]
	orientation<-conversion.circular(circular(as.numeric(orientation),type="angles",units="degrees",template="geographics",zero=0),type="angles",units="radians")
	time<-input[6,2]
	date<-input[7,2]
	data<-input[9:nrow(input),1]
	data<-circular(as.numeric(data),type="angles",units="degrees",template="geographics",zero=0)
	data<-conversion.circular(data,type="angles",units="radians")
	return(list(site=as.numeric(site),wind.speed=as.numeric(wind.speed),wind.gust=as.numeric(wind.gust),temp=as.numeric(temp),orientation=as.numeric(orientation),time=as.character(time),date=date,data=data))
}