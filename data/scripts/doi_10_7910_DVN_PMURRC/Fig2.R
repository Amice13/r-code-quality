# ----------------------------------------------------------------------
# Fig2.R
# (Main Figures from PurpleAir CA Indoor/Outdoor Analysis)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Figure 2A - Density Plots
# ----------------------------------------------------------------------
daily_all$type=as.factor(daily_all$inside)

pdens2 = ggplot(data=daily_all,aes(lty=type)) + 
  geom_density(aes(x=PC_0p3),col="blue") + 
  geom_density(aes(x=PC_0p5),col="darkgreen") + 
  geom_density(aes(x=PC_1),col="yellow3") + 
  geom_density(aes(x=PC_2p5),col="orange") + 
  geom_density(aes(x=PC_5),col="darkred") + 
  theme_minimal(base_size=14) + xlab("Particles per deciliter") + ylab("Probability Density") + theme(legend.position="none") + scale_x_log10()

pdf(width=8,height=6,file="Results/Figures/Fig2A_Raw_DensityPlots.pdf")
  print(pdens2)
dev.off()

# ---------------------------------------------------------------
# Add different time units to data
# ---------------------------------------------------------------

dat$J = as.numeric(format(as.Date(dat$date2),"%j"))
dat$mo = format(as.Date(dat$date2),"%b")
dat$hourofday = as.factor(as.numeric(substr(dat$datetime_local,start=12,stop=13)))
dat$hr = as.numeric(substr(dat$datetime_local,start=12,stop=13))

dat_daily$J = as.numeric(format(as.Date(dat_daily$date),"%j"))
dat_daily$mo = format(as.Date(dat_daily$date),"%b")
dat_daily$month = as.factor(as.numeric(format(as.Date(dat_daily$date),"%m")))
dat_daily$mo = as.numeric(format(as.Date(dat_daily$date),"%m"))

daily_rg$J = as.numeric(format(as.Date(daily_rg$date),"%j"))
daily_rg$mo = format(as.Date(daily_rg$date),"%b")
daily_rg$month = as.factor(as.numeric(format(as.Date(daily_rg$date),"%m")))

# ---------------------------------------------------------------
# Figure 2B - Day-of-Year Plots
# ---------------------------------------------------------------

pd1 = ggplot(data=dat_daily,aes(x=J)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_0p3_outdoor_500),col="blue") + geom_smooth(aes(y=PC_0p3_indoor),col="blue",lty=2) 

pd2 = ggplot(data=dat_daily,aes(x=J)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_0p5_outdoor_500),col="darkgreen") + geom_smooth(aes(y=PC_0p5_indoor),col="darkgreen",lty=2) 

pd3 = ggplot(data=dat_daily,aes(x=J)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_1_outdoor_500),col="yellow3") + geom_smooth(aes(y=PC_1_indoor),col="yellow3",lty=2) 

pd4 = ggplot(data=dat_daily,aes(x=J)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_2p5_outdoor_500),col="orange") + geom_smooth(aes(y=PC_2p5_indoor),col="orange",lty=2) 

pd5 = ggplot(data=dat_daily,aes(x=J)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_5_outdoor_500),col="darkred") + geom_smooth(aes(y=PC_5_indoor),col="darkred",lty=2)  

pdf(width=4,height=7,file="Results/Figures/Fig2B_IDW_daily_J_NEW.pdf")
  ggarrange(pd1,pd2,pd3,pd4,pd5,ncol=1)
dev.off()

# ---------------------------------------------------------------
# Figure 2C - Hour-of-Day Plots
# ---------------------------------------------------------------

ph1 = ggplot(data=dat,aes(x=hr)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_0p3_outdoor_500),col="blue") + geom_smooth(aes(y=PC_0p3_indoor),col="blue",lty=2) 

ph2 = ggplot(data=dat,aes(x=hr)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_0p5_outdoor_500),col="darkgreen") + geom_smooth(aes(y=PC_0p5_indoor),col="darkgreen",lty=2) 

ph3 = ggplot(data=dat,aes(x=hr)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_1_outdoor_500),col="yellow3") + geom_smooth(aes(y=PC_1_indoor),col="yellow3",lty=2) 

ph4 = ggplot(data=dat,aes(x=hr)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_2p5_outdoor_500),col="orange") + geom_smooth(aes(y=PC_2p5_indoor),col="orange",lty=2) 

ph5 = ggplot(data=dat,aes(x=hr)) + xlab("") + ylab("") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=PC_5_outdoor_500),col="darkred") + geom_smooth(aes(y=PC_5_indoor),col="darkred",lty=2) 

pdf(width=4,height=7,file="Results/Figures/Fig2C_IDW_hourly_J_NEW.pdf")
  ggarrange(ph1,ph2,ph3,ph4,ph5,ncol=1)
dev.off()

# ---------------------------------------------------------------
# Figure 2D - Day-of-Year Indoor:Outdoor Ratios
# ---------------------------------------------------------------
# Daily ratios
rr = dat_daily %>% group_by(J) %>% transmute(pc0p3inout=PC_0p3_indoor/PC_0p3_outdoor_500,
                                             pc0p5inout=PC_0p5_indoor/PC_0p5_outdoor_500,
                                             pc1p0inout=PC_1_indoor/PC_1_outdoor_500,
                                             pc2p5inout=PC_2p5_indoor/PC_2p5_outdoor_500,
                                             pc5p0inout=PC_5_indoor/PC_5_outdoor_500,
                                             
)
rr = rr %>% group_by(J) %>% summarize_all(list(min=min,med=median,max=max,mea=mean),na.rm=TRUE)
rr = pivot_longer(rr,cols=starts_with("pc"))
rr$stat = substr(rr$name,start=12,stop=14)
rr$name = substr(rr$name,start=1,stop=5)
rr = pivot_wider(rr,names_from="stat",values_from="value")

prr = ggplot(data=rr,aes(col=name,fill=name,x=J)) + xlab("Day of Year") + ylab("Indoor:Outdoor") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=med),se=TRUE) + 
  ylim(c(0.25,0.75)) + scale_color_manual(values=c("pc0p3"="blue","pc0p5"="darkgreen","pc1p0"="yellow3","pc2p5"="orange","pc5p0"="darkred")) +
  scale_fill_manual(values=c("pc0p3"="blue","pc0p5"="darkgreen","pc1p0"="yellow3","pc2p5"="orange","pc5p0"="darkred"))

pdf(width=4,height=3,file="Results/Figures/Fig2D_InOut_J_med.pdf")
  print(prr + theme(legend.position="NONE"))
dev.off()

# ---------------------------------------------------------------
# Figure 2E - Hour-of-Day Indoor:Outdoor Ratios
# ---------------------------------------------------------------
# Hourly ratios
ratios2 = dat %>% group_by(hourofday,hr) %>% transmute(pc0p3inout=PC_0p3_indoor/PC_0p3_outdoor_500,
                                                       pc0p5inout=PC_0p5_indoor/PC_0p5_outdoor_500,
                                                       pc1p0inout=PC_1_indoor/PC_1_outdoor_500,
                                                       pc2p5inout=PC_2p5_indoor/PC_2p5_outdoor_500,
                                                       pc5p0inout=PC_5_indoor/PC_5_outdoor_500
)
ratios2 = pivot_longer(ratios2,cols=starts_with("pc"))
ratios2$name = substr(ratios2$name,start=1,stop=5)

rr2 = ratios2 %>% group_by(hr,name) %>% summarize(min=min(value,na.rm=TRUE),med=median(value,na.rm=TRUE),max=max(value,na.rm=TRUE),mea=mean(value,na.rm=TRUE))

prr2 = ggplot(data=rr2,aes(col=name,fill=name,x=hr)) + xlab("Hour of Day") + ylab("Indoor:Outdoor") + theme_minimal(base_size=14) +
  geom_smooth(aes(y=med),se=TRUE) + theme(legend.position = "none") +
  ylim(c(0.25,0.75)) + scale_color_manual(values=c("pc0p3"="blue","pc0p5"="darkgreen","pc1p0"="yellow3","pc2p5"="orange","pc5p0"="darkred")) +
  scale_fill_manual(values=c("pc0p3"="blue","pc0p5"="darkgreen","pc1p0"="yellow3","pc2p5"="orange","pc5p0"="darkred"))

pdf(width=4,height=3,file="Results/Figures/Fig2E_InOut_hr_med.pdf")
  print(prr2)
dev.off()
