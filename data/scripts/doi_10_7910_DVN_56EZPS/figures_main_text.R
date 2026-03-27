#File Name: figures_main_text.R
#Data: rcl_ideology_estimates.dta, rcl_skills_estimates.dta, fig2_data.RData 
#Purpose: Create figures for the main text

library(readstata13)

###########################################
#Figure 1: Intra-Department variation ####
###########################################

x<-read.dta13(file="C:/Users/mdr/Dropbox/IdeologyPaper/JOP Dataverse/rcl_ideology_estimates.dta",convert.factors=F)

dos<-x[(x$dept=="Department of State"|x$agency=="Department of State")&is.na(x$dept)==F,]
dhs<-x[(x$dept=="Department of Homeland Security"|x$agency=="Department of Homeland Security")&
         is.na(x$dept)==F,]
dos<-dos[order(dos$ideo_rating),]
dhs<-dhs[order(dhs$ideo_rating),]

y<-rbind(dos,-100,dhs)
row.names(y)<-1:nrow(y)
y$agency[y$agency=="Bureau of International Narcotics and Law Enforcement Affairs"]<-"Bureau of Int. Narcotics and Law Enforcement Affairs"

cabs<-c(3,13)
ags<-c(1:2,4:9,11:12,14:18)

png(file="C:/Users/richar33/Dropbox/IdeologyPaper/Figures/ideo_ratings_cab.png",height=4,width=8.5,
    units="in",res=144)
par(cex.lab=1.2,cex.axis=1.2,mar=c(5,0.1,3,0.2))
plot(y$ideo_rating,1:nrow(y),pch=19,xlim=c(-5.6,2.3),xaxt="n",ylab="",yaxt="n",bty="]",
     xlab="Estimate of Agency Ideology")
axis(side=1,at=-2:2,labels=c("-2","-1","0","1","2"))
axis(side=3,at=-2:2,labels=c("-2","-1","0","1","2"))
segments(y$ideo_lb,1:nrow(y),y$ideo_ub,1:nrow(y),lwd=2)

text(-2.1,cabs,y$agency[cabs],pos=2,cex=.9,font=2)
text(-2.1,ags,
     y$agency[ags],pos=2,cex=.9)
mtext("Conservative",side=3,padj=0,adj=.98, line=2)
mtext("Liberal",side=3,padj=0,adj=.46, line=2)
abline(h=c(10,21))
abline(v=-2:2,lty=2)
dev.off()


###########################################
#Figure 2: Ratings Comparison ####
###########################################
rm(list=ls())

load("C:/Users/MDR/Dropbox/IdeologyPaper/JOP Dataverse/fig2_data.RData")
#Note: Each of the four data frames contains the informed ideology estiamtes merged with
#other estimates.
#xcar contains mean careersits ideology from the 2014 survey
#xcblgn contains the Clinton et al. (2012) estimates
#xcj contains the Chen and Johnson (2014) estimates
#xcl contains the Clinton and Lewis (2008) estimates

png(file="C:/Users/richar33/Dropbox/IdeologyPaper/Figures/all_comp.png",height=7,width=7,
    units="in",res=144)
par(mfrow=c(2,2),mar=c(4.5,4,1,1),cex.axis=0.9)
plot(xcl$ideo_rating,xcl$cl_mean,type="n",ylim=c(-2.5,2.5),xlim=c(-2.5,2.5),
     xlab="Our Estimate",ylab="Clinton-Lewis (2008) Estimate")
text(xcl$ideo_rating,xcl$cl_mean,xcl$acr,cex=0.7)

plot(xcblgn$ideo_rating,xcblgn$agentmean,type="n",ylim=c(-0.7,0.7),xlim=c(-2.5,2.5),
     xlab="Our Estimate",ylab="Clinton et al. (2012) Estimate")
text(xcblgn$ideo_rating,xcblgn$agentmean,xcblgn$acr,cex=0.7)

plot(xcj$ideo_rating,xcj$obama,type="n",ylim=c(-0.4,0.4),xlim=c(-2.5,2.5),
     xlab="Our Estimate",ylab="Chen-Johnson (2014) Estimate")
text(xcj$ideo_rating,xcj$obama,xcj$acr,cex=0.7)

plot(xcar$ideo_rating,xcar$mean_ideo,type="n",ylim=c(1,4),xlim=c(-2.5,2.5),
     xlab="Our Estimate",ylab="Mean Careerist Ideology 2014 Survey")
text(xcar$ideo_rating,xcar$mean_ideo,xcar$acr,cex=0.7)
dev.off()

#pairwise correlations
cor(xcl$cl_mean,xcl$ideo_rating,use="pairwise")
cor(xcblgn$agentmean,xcblgn$ideo_rating,use="pairwise")
cor(xcj$obama,xcj$ideo_rating,use="pairwise")
cor(xcar$mean_ideo,xcar$ideo_rating,use="pairwise")

###########################################
#Figure 3: Skills versus Ideology ####
###########################################
rm(list=ls())
x<-read.dta13(file="C:/Users/mdr/Dropbox/IdeologyPaper/JOP Dataverse/rcl_skills_estimates.dta",convert.factors = F)
y<-read.dta13(file="C:/Users/mdr/Dropbox/IdeologyPaper/JOP Dataverse/rcl_ideology_estimates.dta",convert.factors=F)

z<-merge(x,y,by=c("agency","dept","acr"),all=T)

#remove skills ratings with extreme imprecision - difference in upper and lower bound is greater than 6;
names<-c("National Railroad Passenger Corporation (AMTRAK)","Defense Commissary Agency",
         "Federal Home Loan Mortgage Corporation","National Credit Union Administration",
         "Federal Energy Regulatory Commission")

drop<-NA

for(i in 1:length(names)){
  drop<-c(drop,grep(names[i],z$agency,fixed=T))
}

drop<-drop[-1]

z<-z[-1*drop,]

png(file="C:/Users/richar33/Dropbox/IdeologyPaper/Figures/ideologyVskills.png",height=7,width=7,
    units="in",res=400)
par(mar=c(5,4,2,2))
plot(z$ideo_rating,z$skills_rating,xlim=c(-2,2),ylim=c(-2,2),xlab="Estimate of Agency Ideology",
     ylab="Estimate of Agency Workforce Skill",type="n")
text(z$ideo_rating,z$skills_rating,labels=z$acr,cex=0.6)
dev.off()
