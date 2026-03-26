# ----------------------------------------------------------------------
# Fig3.R
# (Main Figures from PurpleAir CA Indoor/Outdoor Analysis)
# ----------------------------------------------------------------------

# Load in Coefficients
main.daily = read_csv("Results/Data/main_daily.csv")
main.hourly = read_csv("Results/Data/main_hourly.csv")
lags.hourly = read_csv("Results/Data/lags_hourly.csv")
temp.hourly = read_csv("Results/Data/temp_hourly.csv")

# ----------------------------------------------------------------------
# Figure 3A - Main Regression Coefficients (Daily and Hourly)
# ----------------------------------------------------------------------

main = rbind(main.hourly,main.daily)
main$size = rep(c("0.3-0.5","0.5-1.0","1.0-2.5","2.5-5.0","5.0-10.0"),times=2)
main$type = c(rep("Hourly",5),rep("Daily",5))
main$type = as.factor(main$type)
main$x = as.numeric(main$type) + c(-0.1,-0.05,0,0.05,0.1,-0.1,-0.05,0,0.05,0.1)

p.main = ggplot(data=main,aes(x=x,group=type,y=coef,col=size)) + theme_minimal(base_size=14) + ylab("Estimated Coefficient") + 
  ylim(c(0.3,0.7)) + xlab("") + geom_vline(aes(xintercept=1),col="lightgrey",lwd=0.5) + geom_vline(aes(xintercept=2),col="lightgrey",lwd=0.5) +
  geom_point(size=2) + geom_linerange(aes(ymin=ci_lower,ymax=ci_upper),size=1) + 
  scale_x_continuous(breaks=c(1,2),limits=c(0,3),labels=c("1.0"="Daily","2.0"="Hourly")) + 
  scale_color_manual(values=c("0.3-0.5"="blue","0.5-1.0"="darkgreen","1.0-2.5"="yellow3","2.5-5.0"="orange","5.0-10.0"="darkred"))
p.main

pdf(width=4,height=6,file="Results/Figures/Fig3A_Basic_Regs.pdf")
  print(p.main)
dev.off()

# ----------------------------------------------------------------------
# Figure 3B - Hourly Lags Regression Coefficients
# ----------------------------------------------------------------------

sz = 2

p.hr = ggplot(data=lags.hourly,aes(x=t-0.2)) + geom_hline(yintercept=0,col="darkgrey") + 
  geom_point(aes(y=coef_0p3),col="blue",size=sz) + geom_linerange(aes(ymin=ci_lower_0p3,ymax=ci_upper_0p3),col="blue") + 
  geom_point(aes(x=t-0.1,y=coef_0p5),col="darkgreen",size=sz) + geom_linerange(aes(x=t-0.1,ymin=ci_lower_0p5,ymax=ci_upper_0p5),col="darkgreen") + 
  geom_point(aes(x=t,y=coef_1),col="yellow3",size=sz) + geom_linerange(aes(x=t,ymin=ci_lower_1,ymax=ci_upper_1),col="yellow3") + 
  geom_point(aes(x=t+0.1,y=coef_2p5),col="orange",size=sz) + geom_linerange(aes(x=t+0.1,ymin=ci_lower_2p5,ymax=ci_upper_2p5),col="orange") + 
  geom_point(aes(x=t+0.2,y=coef_5),col="darkred",size=sz) + geom_linerange(aes(x=t+0.2,ymin=ci_lower_5,ymax=ci_upper_5),col="darkred") + 
  theme_minimal(base_size=14) + xlab("Time Lag [hours]") + theme(legend.position="none") + ylab("Coefficient") + ylim(c(-0.05,0.3))

pdf(width=6,height=3,file="Results/Figures/Fig3B_HrLags.pdf")
  print(p.hr)
dev.off()

# ----------------------------------------------------------------------
# Figures 3C,D - Temperature Bins Regression Coefficients & Observations
# ----------------------------------------------------------------------

p.temp = ggplot(data=temp.hourly,aes(x=temp-0.4)) + geom_hline(yintercept=0,col="darkgrey") +
  geom_point(aes(y=coef_0p3),col="blue",size=sz) + geom_linerange(aes(ymin=ci_lower_0p3,ymax=ci_upper_0p3),col="blue") + 
  geom_point(aes(x=temp-0.2,y=coef_0p5),col="darkgreen",size=sz) + geom_linerange(aes(x=temp-0.2,ymin=ci_lower_0p5,ymax=ci_upper_0p5),col="darkgreen") + 
  geom_point(aes(x=temp,y=coef_1),col="yellow3",size=sz) + geom_linerange(aes(x=temp,ymin=ci_lower_1,ymax=ci_upper_1),col="yellow3") + 
  geom_point(aes(x=temp+0.2,y=coef_2p5),col="orange",size=sz) + geom_linerange(aes(x=temp+0.2,ymin=ci_lower_2p5,ymax=ci_upper_2p5),col="orange") + 
  geom_point(aes(x=temp+0.4,y=coef_5),col="darkred",size=sz) + geom_linerange(aes(x=temp+0.4,ymin=ci_lower_5,ymax=ci_upper_5),col="darkred") + 
  theme_minimal(base_size=14) + xlab("Temperature [C]") + theme(legend.position="none") + ylab("Coefficient") 

pdf(width=6,height=6,file="Results/Figures/Fig3C_TempBins.pdf")
  print(p.temp)
dev.off()

p.nobs = ggplot(data=temp.hourly,aes(x=temp,y=nobs)) + geom_col(orientation="x",fill="lightgrey",col="lightgrey") + theme_minimal() + xlab("") + ylab("")


pdf(width=6,height=1.5,file="Results/Figures/Fig3D_TempBins_NOBS.pdf")
  print(p.nobs)
dev.off()


