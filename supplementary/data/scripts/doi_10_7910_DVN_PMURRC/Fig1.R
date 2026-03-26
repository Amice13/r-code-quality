# ----------------------------------------------------------------------
# Fig1.R
# (Main Figures from PurpleAir CA Indoor/Outdoor Analysis)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Figure 1A - Maps
# ----------------------------------------------------------------------

png(width=4,height=6,units="in",res=300,file="Results/Figures/Fig1A_CA.png")
  plot(ca)
  points(locs[locs$inside==0,],col="navyblue",pch=16,cex=0.3)
  points(locs[locs$inside==1,],col="orange",pch=16,cex=0.3)
dev.off()

png(width=4,height=6,units="in",res=300,file="Results/Figures/Fig1A_LA.png")
  plot(ca,ylim=c(33,35),xlim=c(-119,-117))
  points(locs[locs$inside==0,],col="navyblue",pch=16,cex=0.3)
  points(locs[locs$inside==1,],col="orange",pch=16,cex=0.3)
dev.off()

png(width=4,height=6,units="in",res=300,file="Results/Figures/Fig1A_Bay.png")
  plot(ca,ylim=c(37.5,38),xlim=c(-123,-121.8))
  points(locs[locs$inside==0,],col="navyblue",pch=16,cex=0.3)
  points(locs[locs$inside==1,],col="orange",pch=16,cex=0.3)
dev.off()

# ----------------------------------------------------------------------
# Clean Data:
# Data have already (a) had zeros cut and (b) been rounded to better 
# increment to avoid aliasing. Need to deal with tails:
# Low tails - setting 0 (from new rounding) to 0.01 (min increment)
# High tails - already cut top in accordance with PA guidance.
# ----------------------------------------------------------------------

minval = 0.01

# Store summary for later
nzeros_matched = c(sum(dat$PC_0p3_outdoor_500==0,na.rm=TRUE),
                   sum(dat$PC_0p5_outdoor_500==0,na.rm=TRUE),
                   sum(dat$PC_1_outdoor_500==0,na.rm=TRUE),
                   sum(dat$PC_2p5_outdoor_500==0,na.rm=TRUE),
                   sum(dat$PC_5_outdoor_500==0,na.rm=TRUE))

nzeros_raw = c(sum(raw$PC_0p3==0,na.rm=TRUE),
               sum(raw$PC_0p5==0,na.rm=TRUE),
               sum(raw$PC_1==0,na.rm=TRUE),
               sum(raw$PC_2p5==0,na.rm=TRUE),
               sum(raw$PC_5==0,na.rm=TRUE))

# Replace
dd = dat
rr = raw

dat = dd %>% mutate(PC_0p3_outdoor_500=replace(PC_0p3_outdoor_500,PC_0p3_outdoor_500==0,minval),
                    PC_0p5_outdoor_500=replace(PC_0p5_outdoor_500,PC_0p5_outdoor_500==0,minval),
                    PC_1_outdoor_500=replace(PC_1_outdoor_500,PC_1_outdoor_500==0,minval),
                    PC_2p5_outdoor_500=replace(PC_2p5_outdoor_500,PC_2p5_outdoor_500==0,minval),
                    PC_5_outdoor_500=replace(PC_5_outdoor_500,PC_5_outdoor_500==0,minval))

raw = rr %>% mutate(PC_0p3=replace(PC_0p3,PC_0p3==0,minval),
                    PC_0p5=replace(PC_0p5,PC_0p5==0,minval),
                    PC_1=replace(PC_1,PC_1==0,minval),
                    PC_2p5=replace(PC_2p5,PC_2p5==0,minval),
                    PC_5=replace(PC_5,PC_5==0,minval))

# ----------------------------------------------------------------------
# Figure 1B - TS of PM2.5 Data, with Histograms
# ----------------------------------------------------------------------

# Get daily averages in aggregated
dat$date2 = format(as.Date(dat$datetime_local,"%m/%d/%Y"),"%Y-%m-%d")
dat_daily = dat %>% group_by(date2,id) %>% summarize_all(list(mean),na.rm=TRUE)
dat_daily$date = dat_daily$date2
dat_daily$date2 = NULL
dat_daily$date = as.Date(dat_daily$date)

# Get daily raw averages by monitor
raw$date = format(as.Date(raw$datetime_GMT,"%m/%d/%Y"),"%Y-%m-%d")
daily = raw %>% group_by(date,id) %>% summarize_all(mean,na.rm=TRUE)
daily$date = as.Date(daily$date)


# Merge to know if indoor or outdoor
daily_all = left_join(daily,locs@data,by="id")
daily_all$datetime_GMT_fn1 = daily_all$datetime_GMT = NULL

# N by day
monitors_by_date = daily_all %>% group_by(date,inside) %>% summarize(monitors=n_distinct(id))
monitors_by_date$location = "Indoor"
monitors_by_date$location[monitors_by_date$inside==0] = "Outdoor"
monitors_by_date$location = factor(monitors_by_date$location)
monitors_by_date$date = as.Date(monitors_by_date$date)

# N by day
p = ggplot(data=monitors_by_date,aes(x=date,y=monitors,fill=location,order=-location)) + geom_col() + 
  theme_minimal() + scale_fill_manual(values=c("Indoor"="orange","Outdoor"="navyblue")) + xlab("") + 
  ylab("# of Monitors") + theme(legend.position="none")

pdf(width=5,height=1.5,file="Results/Figures/Fig1B_Nmonitors.pdf")
print(p)
dev.off()


# Daily range
daily_rg = daily_all %>% group_by(date,inside) %>% summarize(median.pm2p5=median(PM_2p5,na.rm=TRUE),
                                                             high.pm2p5=quantile(PM_2p5,probs=0.95,na.rm=TRUE),
                                                             low.pm2p5=quantile(PM_2p5,probs=0.05,na.rm=TRUE),
                                                             median.pm10=median(PM_10p0,na.rm=TRUE),
                                                             high.pm10=quantile(PM_10p0,probs=0.95,na.rm=TRUE),
                                                             low.pm10=quantile(PM_10p0,probs=0.05,na.rm=TRUE),
                                                             median.pm1=median(PM_1p0,na.rm=TRUE),
                                                             high.pm1=quantile(PM_1p0,probs=0.95,na.rm=TRUE),
                                                             low.pm1=quantile(PM_1p0,probs=0.05,na.rm=TRUE),
                                                             median.c0p3=median(PC_0p3,na.rm=TRUE),
                                                             high.c0p3=quantile(PC_0p3,probs=0.95,na.rm=TRUE),
                                                             low.c0p3=quantile(PC_0p3,probs=0.05,na.rm=TRUE),
                                                             median.c0p5=median(PC_0p5,na.rm=TRUE),
                                                             high.c0p5=quantile(PC_0p5,probs=0.95,na.rm=TRUE),
                                                             low.c0p5=quantile(PC_0p5,probs=0.05,na.rm=TRUE),
                                                             median.c1=median(PC_1,na.rm=TRUE),
                                                             high.c1=quantile(PC_1,probs=0.95,na.rm=TRUE),
                                                             low.c1=quantile(PC_1,probs=0.05,na.rm=TRUE),
                                                             median.c2p5=median(PC_2p5,na.rm=TRUE),
                                                             high.c2p5=quantile(PC_2p5,probs=0.95,na.rm=TRUE),
                                                             low.c2p5=quantile(PC_2p5,probs=0.05,na.rm=TRUE),
                                                             median.c5=median(PC_5,na.rm=TRUE),
                                                             high.c5=quantile(PC_5,probs=0.95,na.rm=TRUE),
                                                             low.c5=quantile(PC_5,probs=0.05,na.rm=TRUE))                                                             
daily_rg$inside = as.factor(daily_rg$inside)
daily_rg$date = as.Date(daily_rg$date)

# Plots of PM concentrations over time series
p1 = ggplot(data=daily_rg,aes(x=date,ymax=high.pm1,ymin=low.pm1,group=inside,fill=inside)) + 
  geom_ribbon(alpha=0.3) + geom_line(aes(y=median.pm1,col=inside),lwd=0.5) +
  theme_minimal() + scale_fill_manual(values=c("1"="orange","0"="navyblue")) + 
  scale_color_manual(values=c("1"="orange","0"="navyblue")) + xlab("") + 
  ylab(expression(paste(PM[1.0]," [",mu,"g ",m^-3,"]",sep=""))) + theme(legend.position="none")

p2 = ggplot(data=daily_rg,aes(x=date,ymax=high.pm2p5,ymin=low.pm2p5,group=inside,fill=inside)) + 
  geom_ribbon(alpha=0.3) + geom_line(aes(y=median.pm2p5,col=inside),lwd=0.5) +
  theme_minimal() + scale_fill_manual(values=c("1"="orange","0"="navyblue")) + 
  scale_color_manual(values=c("1"="orange","0"="navyblue")) + xlab("") +
  ylab(expression(paste(PM[2.5]," [",mu,"g ",m^-3,"]",sep=""))) + theme(legend.position="none")

p10 = ggplot(data=daily_rg,aes(x=date,ymax=high.pm10,ymin=low.pm10,group=inside,fill=inside)) + 
  geom_ribbon(alpha=0.3) + geom_line(aes(y=median.pm10,col=inside),lwd=0.5) +
  theme_minimal() + scale_fill_manual(values=c("1"="orange","0"="navyblue")) + 
  scale_color_manual(values=c("1"="orange","0"="navyblue")) + xlab("") + 
  ylab(expression(paste(PM[10]," [",mu,"g ",m^-3,"]",sep=""))) + theme(legend.position="none")


pdf(width=5,height=1.5,file="Results/Figures/Fig1B_pm1.pdf")
print(p1)
dev.off()

pdf(width=5,height=1.5,file="Results/Figures/Fig1B_pm2p5.pdf")
print(p2)
dev.off()

pdf(width=5,height=1.5,file="Results/Figures/Fig1B_pm10.pdf")
print(p10)
dev.off()

# Time series of all particle count bins
p.all = ggplot(data=daily_rg,aes(x=date,col=inside)) + 
  geom_line(aes(y=median.c0p3,col=inside)) +
  geom_line(aes(y=median.c0p5,col=inside),lty=3) +
  geom_line(aes(y=median.c1,col=inside)) +
  geom_line(aes(y=median.c2p5,col=inside),lty=3) +
  geom_line(aes(y=median.c5,col=inside)) +
  #  geom_line(aes(y=median.c10,col=inside),lty=3) +
  theme_minimal() + xlab("") + scale_y_log10(position="right") + 
  scale_color_manual(values=c("1"="orange","0"="navyblue")) + 
  theme(legend.position="none") + ylab("") 

pdf(width=6,height=6,file="Results/Figures/Fig1C_SizeBins.pdf")
  print(p.all)
dev.off()


