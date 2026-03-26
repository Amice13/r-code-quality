


###################################################################
###              TIME SERIES FOR MAIN ANALYSES                  ###




#first task is to reorder the structure of output file to enable the output to be plotted

#import the data
time_data<-read.table("joint_time_series_ALL_GLS_with_interaction.txt", header=T)  #this will produce a figure based on the full model, but no "First distance from First Empire" - that is done in the section below


#create data frame with mean values for each time step and the standard deviation of these estimates
steppe<-c()
agri<-c()
yield<-c()
int<-c()
elev<-c()

steppe_sd<-c()
agri_sd<-c()
yield_sd<-c()
int_sd<-c()
elev_sd<-c()


#step through the 22 time slices
for (j in 1:22){
    
    time_points<-seq(from=j, to=220, by=22) #allows us to examine the same time slice for each of the 10 replicates
  
    #calculate mean and standard deviations of the different parameter estimates 
    st_mean<-mean(time_data[,1][time_points])
    st_sd<-sd(time_data[,1][time_points])
    steppe<-c(steppe,st_mean)
    steppe_sd<-c(steppe_sd,st_sd)
    
    yd_mean<-mean(time_data[,2][time_points])
    yd_sd<-sd(time_data[,2][time_points])
    yield<-c(yield,yd_mean)
    yield_sd<-c(yield_sd,yd_sd)
    
    ag_mean<-mean(time_data[,3][time_points])
    ag_sd<-sd(time_data[,3][time_points])
    agri<-c(agri,ag_mean)
    agri_sd<-c(agri_sd,ag_sd)
    
    el_mean<-mean(time_data[,4][time_points])
    el_sd<-sd(time_data[,4][time_points])
    elev<-c(elev,el_mean)
    elev_sd<-c(elev_sd,el_sd)
    
    in_mean<-mean(time_data[,5][time_points])
    in_sd<-sd(time_data[,5][time_points])
    int<-c(int,in_mean)
    int_sd<-c(int_sd,in_sd)
    
    
  }  


#create dataframe for creating the figure
time_step<-seq(from=1, to=22, by=1)
fig_data<-as.data.frame(cbind(time_step,steppe,agri,yield,int,elev,steppe_sd,agri_sd,yield_sd,int_sd,elev_sd))

#set values used for the X axis of the figure
Xaxis<-(-700+(fig_data$time_step*100))

#plot the data, with different lines representing different variables
plot(Xaxis,fig_data$steppe,type="n", xlim=c(-600,1500),ylim=c(0,0.6), xlab="Year", ylab="Standardized Effect Size", cex.lab=1.3)
lines(Xaxis,fig_data$steppe, type="l",col="red",lwd=5)
lines(Xaxis+20,fig_data$agri, type="l",col="blue",lwd=5)
lines(Xaxis,fig_data$yield, type="l",col="green4",lwd=5)
lines(Xaxis+40,fig_data$int, type="l",col="black",lwd=5)
lines(Xaxis+20,fig_data$elev, type="l",col="grey",lwd=5)


#add standard deviations to the lines to indicate variation in the estimates
arrows(Xaxis,fig_data$steppe, Xaxis,fig_data$steppe+fig_data$steppe_sd, length=0.05, angle=90,col="red", lwd=2)
arrows(Xaxis,fig_data$steppe, Xaxis,fig_data$steppe-fig_data$steppe_sd, length=0.05, angle=90,col="red", lwd=2)

arrows(Xaxis+20,fig_data$agri, Xaxis+20,fig_data$agri+fig_data$agri_sd, length=0.05, angle=90,col="blue", lwd=2)
arrows(Xaxis+20,fig_data$agri, Xaxis+20,fig_data$agri-fig_data$agri_sd, length=0.05, angle=90,col="blue", lwd=2)

arrows(Xaxis+40,fig_data$int, Xaxis+40,fig_data$int+fig_data$int_sd, length=0.05, angle=90,col="black", lwd=2)
arrows(Xaxis+40,fig_data$int, Xaxis+40,fig_data$int-fig_data$int_sd, length=0.05, angle=90,col="black", lwd=2)

arrows(Xaxis,fig_data$yield, Xaxis,fig_data$yield+fig_data$yield_sd, length=0.05, angle=90,col="green4", lwd=2)
arrows(Xaxis,fig_data$yield, Xaxis,fig_data$yield-fig_data$yield_sd, length=0.05, angle=90,col="green4", lwd=2)

arrows(Xaxis+20,fig_data$elev, Xaxis+20,fig_data$elev+fig_data$elev_sd, length=0.05, angle=90,col="grey", lwd=2)
arrows(Xaxis+20,fig_data$elev, Xaxis+20,fig_data$elev-fig_data$elev_sd, length=0.05, angle=90,col="grey", lwd=2)

legend("topleft", inset=.05, title="Predictor",
    legend=c("Distance from Steppe","Duration of Agriculture","Agricultural Productivity","Elevation","Steppe*Agriculture duration"), 
	fill=c("red","blue","green4","grey","black"), horiz=F)



#######################################################################################################################


####################################################################
###   TIME SERIES FOR ANALYSES WITH DISTANCE FROM FE AS CONTROL  ###

#first task is to reorder the output file  

#import the data with FE as a control
time_data<-read.table("joint_time_series_FE_ALL_GLS.txt", header=T)

#script below is basically the sames as above with the extra variable


#create data frame with mean values for each time step and the standard deviation of these estimates
steppe<-c()
agri<-c()
yield<-c()
int<-c()
elev<-c()
FEmp<-c()

steppe_sd<-c()
agri_sd<-c()
yield_sd<-c()
int_sd<-c()
elev_sd<-c()
FEmp_sd<-c()

for (j in 1:22){
  
  time_points<-seq(from=j, to=220, by=22)
  
  
  st_mean<-mean(time_data[,1][time_points])
  st_sd<-sd(time_data[,1][time_points])
  steppe<-c(steppe,st_mean)
  steppe_sd<-c(steppe_sd,st_sd)
  
  yd_mean<-mean(time_data[,2][time_points])
  yd_sd<-sd(time_data[,2][time_points])
  yield<-c(yield,yd_mean)
  yield_sd<-c(yield_sd,yd_sd)
  
  ag_mean<-mean(time_data[,3][time_points])
  ag_sd<-sd(time_data[,3][time_points])
  agri<-c(agri,ag_mean)
  agri_sd<-c(agri_sd,ag_sd)
  
  fe_mean<-mean(time_data[,4][time_points])
  fe_sd<-sd(time_data[,4][time_points])
  FEmp<-c(FEmp,fe_mean)
  FEmp_sd<-c(FEmp_sd,fe_sd)
  
  el_mean<-mean(time_data[,5][time_points])
  el_sd<-sd(time_data[,5][time_points])
  elev<-c(elev,el_mean)
  elev_sd<-c(elev_sd,el_sd)
  
  in_mean<-mean(time_data[,6][time_points])
  in_sd<-sd(time_data[,6][time_points])
  int<-c(int,in_mean)
  int_sd<-c(int_sd,in_sd)
  
  
}  


time_step<-seq(from=1, to=22, by=1)

fig_data<-as.data.frame(cbind(time_step,steppe,agri,yield,int,elev,FEmp,steppe_sd,agri_sd,yield_sd,int_sd,elev_sd,FEmp_sd))

str(fig_data)

Xaxis<-(-700+(fig_data$time_step*100))

windows()

plot(Xaxis,fig_data$steppe,type="n", xlim=c(-600,1500),ylim=c(0,0.6), xlab="Year", ylab="Standardized Effect Size", cex.lab=1.3)
lines(Xaxis,fig_data$steppe, type="l",col="red",lwd=5)
lines(Xaxis+20,fig_data$agri, type="l",col="blue",lwd=5)
lines(Xaxis,fig_data$yield, type="l",col="green4",lwd=5)
lines(Xaxis+40,fig_data$int, type="l",col="black",lwd=5)
lines(Xaxis+20,fig_data$elev, type="l",col="grey",lwd=5)
lines(Xaxis+60,fig_data$FEmp, type="l",col="orange",lwd=5)

arrows(Xaxis,fig_data$steppe, Xaxis,fig_data$steppe+fig_data$steppe_sd, length=0.05, angle=90,col="red", lwd=2)
arrows(Xaxis,fig_data$steppe, Xaxis,fig_data$steppe-fig_data$steppe_sd, length=0.05, angle=90,col="red", lwd=2)

arrows(Xaxis+20,fig_data$agri, Xaxis+20,fig_data$agri+fig_data$agri_sd, length=0.05, angle=90,col="blue", lwd=2)
arrows(Xaxis+20,fig_data$agri, Xaxis+20,fig_data$agri-fig_data$agri_sd, length=0.05, angle=90,col="blue", lwd=2)

arrows(Xaxis+40,fig_data$int, Xaxis+40,fig_data$int+fig_data$int_sd, length=0.05, angle=90,col="black", lwd=2)
arrows(Xaxis+40,fig_data$int, Xaxis+40,fig_data$int-fig_data$int_sd, length=0.05, angle=90,col="black", lwd=2)

arrows(Xaxis,fig_data$yield, Xaxis,fig_data$yield+fig_data$yield_sd, length=0.05, angle=90,col="green4", lwd=2)
arrows(Xaxis,fig_data$yield, Xaxis,fig_data$yield-fig_data$yield_sd, length=0.05, angle=90,col="green4", lwd=2)

arrows(Xaxis+20,fig_data$elev, Xaxis+20,fig_data$elev+fig_data$elev_sd, length=0.05, angle=90,col="grey", lwd=2)
arrows(Xaxis+20,fig_data$elev, Xaxis+20,fig_data$elev-fig_data$elev_sd, length=0.05, angle=90,col="grey", lwd=2)

arrows(Xaxis+60,fig_data$FEmp, Xaxis+60,fig_data$FEmp+fig_data$FEmp_sd, length=0.05, angle=90,col="orange", lwd=2)
arrows(Xaxis+60,fig_data$FEmp, Xaxis+60,fig_data$FEmp-fig_data$FEmp_sd, length=0.05, angle=90,col="orange", lwd=2)



legend("top", inset=.05, title="Predictor",
       legend=c("Distance from Steppe","Duration of Agriculture","Agricultural Productivity","Elevation","Steppe*Agriculture duration", "Distance from First Empire"), 
       fill=c("red","blue","green4","grey","black", "orange"), horiz=F)



#######################################################################################################################
