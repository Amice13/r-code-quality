# ----------------------------------------------------------------------
# SuppFigs.R
# (Supplemental) Figures from PurpleAir CA Indoor/Outdoor Analysis)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Figure S1 - Mass v. Particle Count Measurement Demonstration
# ----------------------------------------------------------------------

m1 = 25        # Total (eg) measured ug/m3
m2 = 12        # Total (eg) measured ug/m3
step = 1e-9    # um
avo = 6.022e23 # avogadro (# in a mol)

x = seq(1e-9,1e-4,by=step)
y1 = dlnorm(x,mean=log(2e-6),sd=1)*m1
y2 = dlnorm(x,mean=log(1.5e-6),sd=1)*m2

# check
sum(y1)*step
sum(y2)*step

# Mass under 10um,2.5,1
pm10mass1 = round(sum(y1[x<1e-5])*step,1)
pm2.5mass1 = round(sum(y1[x<2.5e-6])*step,1)
pm1mass1 = round(sum(y1[x<1e-6])*step,1)

pm10mass2 = round(sum(y2[x<1e-5])*step,1)
pm2.5mass2 = round(sum(y2[x<2.5e-6])*step,1)
pm1mass2 = round(sum(y2[x<1e-6])*step,1)

# N under 10um,2.5,1
n1 = y1/(4/3*pi*(x/2)^3)
n2 = y2/(4/3*pi*(x/2)^3)

pm10n1 = round(sum(n1[x<1e-5])/avo/1e3,1)
pm2.5n1 = round(sum(n1[x<2.5e-6])/avo/1e3,1)
pm1n1 = round(sum(n1[x<1e-6])/avo/1e3,1)

pm10n2 = round(sum(n2[x<1e-5])/avo/1e3,1)
pm2.5n2 = round(sum(n2[x<2.5e-6])/avo/1e3,1)
pm1n2 = round(sum(n2[x<1e-6])/avo/1e3,1)

# Mass
png(width=8,height=5,units="in",res=300,file="Results/Figures/FigS1A_MassDist.png")
  par(mar=c(5,5,2,1),las=1)
  plot(x,y1,type="l",col="black",lty=1,lwd=2,xlab=expression(paste("diameter [",mu,"m]")),ylab="",log="x")
  lines(x,y2,col="black",lty=2,lwd=2)
  abline(v=c(1e-9,1e-8,1e-7,1e-6,1e-5,1e-4),h=0,col="darkgrey",lty=3)
  abline(v=10e-6,col="yellow3",lty=1)
  abline(v=2.5e-6,col="orange",lty=1)
  abline(v=1e-6,col="red",lty=1)
  text(1e-9,8e6,expr(paste(!!pm10mass2," ",mu,"g",m^-3,sep="")),col="yellow3",pos=4)
  text(1e-9,7.5e6,expr(paste(!!pm2.5mass2," ",mu,"g",m^-3,sep="")),col="orange",pos=4)
  text(1e-9,7e6,expr(paste(!!pm1mass2," ",mu,"g",m^-3,sep="")),col="red",pos=4)
  text(1e-4,8e6,expr(paste(!!pm10mass1," ",mu,"g",m^-3,sep="")),col="yellow3",pos=2)
  text(1e-4,7.5e6,expr(paste(!!pm2.5mass1," ",mu,"g",m^-3,sep="")),col="orange",pos=2)
  text(1e-4,7e6,expr(paste(!!pm1mass1," ",mu,"g",m^-3,sep="")),col="red",pos=2)
  title(main="Mass density distribution")
dev.off()
  
# Number of particles (mols)
png(width=8,height=5,units="in",res=300,file="Results/Figures/FigS1B_NDist.png")
  par(mar=c(5,5,2,1),las=1)
  plot(x,n2/avo,type="l",col="black",lty=2,lwd=2,xlab=expression(paste("diameter [",mu,"m]")),ylab="",log="x")
  lines(x,n1/avo,col="black",lty=1,lwd=2)
  abline(v=c(1e-9,1e-8,1e-7,1e-6,1e-5,1e-4),h=0,col="darkgrey",lty=3)
  abline(v=10e-6,col="yellow3",lty=1)
  abline(v=2.5e-6,col="orange",lty=1)
  abline(v=1e-6,col="red",lty=1)
  text(1e-9,8500,expr(paste(!!pm10n2," kmol",sep="")),col="yellow3",pos=4)
  text(1e-9,8000,expr(paste(!!pm2.5n2," kmol",sep="")),col="orange",pos=4)
  text(1e-9,7500,expr(paste(!!pm1n2," kmol",sep="")),col="red",pos=4)
  text(1e-4,8500,expr(paste(!!pm10n1," kmol",sep="")),col="yellow3",pos=2)
  text(1e-4,8000,expr(paste(!!pm2.5n1," kmol",sep="")),col="orange",pos=2)
  text(1e-4,7500,expr(paste(!!pm1n1," kmol",sep="")),col="red",pos=2)
  title(main="Number density distribution")
dev.off()

# ----------------------------------------------------------------------
# Figure S2 - Mass v. Particle Count Measurement Demonstration
# ----------------------------------------------------------------------

pdf(width=9,height=6,file="Results/Figures/FigS2_SizeDistributions.pdf")
  par(mar=c(5,5,1,1),las=1)
  plot(-10,-10,xlab="Share of Total Particle Count",ylab="Distribution Density",xlim=c(0,1),ylim=c(0,35),cex.lab=2,cex.axis=1.5)
  grid(col="darkgrey")
  lines(density(dat_daily$PC_0p3_indoor/(dat_daily$PC_0p3_indoor+dat_daily$PC_0p5_indoor+dat_daily$PC_1_indoor),bw=0.005),col="orange",lwd=2)
  lines(density(dat_daily$PC_0p5_indoor/(dat_daily$PC_0p3_indoor+dat_daily$PC_0p5_indoor+dat_daily$PC_1_indoor),bw=0.005),col="orange",lwd=2)
  lines(density(dat_daily$PC_1_indoor/(dat_daily$PC_0p3_indoor+dat_daily$PC_0p5_indoor+dat_daily$PC_1_indoor),bw=0.005),col="orange",lwd=2)
  lines(density(dat_daily$PC_0p3_outdoor_500/(dat_daily$PC_0p3_outdoor_500+dat_daily$PC_0p5_outdoor_500+dat_daily$PC_1_outdoor_500),bw=0.005,na.rm=TRUE),col="darkblue",lwd=2)
  lines(density(dat_daily$PC_0p5_outdoor_500/(dat_daily$PC_0p3_outdoor_500+dat_daily$PC_0p5_outdoor_500+dat_daily$PC_1_outdoor_500),bw=0.005,na.rm=TRUE),col="darkblue",lwd=2)
  lines(density(dat_daily$PC_1_outdoor_500/(dat_daily$PC_0p3_outdoor_500+dat_daily$PC_0p5_outdoor_500+dat_daily$PC_1_outdoor_500),bw=0.005,na.rm=TRUE),col="darkblue",lwd=2)
  legend("topright",inset=0.01,bty="n",col=c("orange","darkblue"),legend=c("indoor","outdoor"),lty=1,lwd=2)
  abline(h=0,col="black")
dev.off()

# Distributions of coefficients
p.dists = ggplot(data=df) + xlab("Estimated Coefficient") + ylab("Distribution Density") + theme_minimal(base_size=14) +
  geom_density(aes(x=coef_0p3),col="blue",lwd=2,n=512) + 
  geom_density(aes(x=coef_0p5),col="darkgreen",lwd=2,n=512) + 
  geom_density(aes(x=coef_1),col="yellow3",lwd=2,n=512) + 
  geom_density(aes(x=coef_2p5),col="orange",lwd=2,n=512) + 
  geom_density(aes(x=coef_5),col="darkred",lwd=2,n=512)

pdf(width=6,height=5,file="Results/Figures/FigS3_Coefficient_Distributions.pdf")
  print(p.dists)
dev.off()
