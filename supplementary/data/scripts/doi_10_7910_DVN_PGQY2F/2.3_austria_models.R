#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script runs the models for Austria.
# It produces Figure 3 in the paper.
# It also produces the numbers for Table 2 in the paper and tables A3, A4, A5 in the Appendix

##########################
# Intro
library(Hmisc)#This code was built with Hmisc version 4.1-1
library(plm)#This code was built with plm version 1.6-6
library(stargazer)#This code was built with stargazer version 5.2.2
library(extrafont)#This code was built with extrafont version 0.17

# Check if all packages are the ones we used
if(packageVersion("Hmisc")=="4.1-1"&
   packageVersion("plm")=="1.6-6"&
   packageVersion("stargazer")=="5.2.2"&
   packageVersion("extrafont")=="0.17"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

set.seed(1711)
#This is for the plot to show the dates correctly:
Sys.setlocale("LC_ALL", "en_US.UTF-8")
load("generated_data/Austria_wf_out")
#####################
# Plot Wordfish over time
sum.wf.austria$label=paste(capitalize(sum.wf.austria$Name)," - ",sum.wf.austria$party_name)

# This produeces Figure 3 in the paper
graph.time.points<-ggplot(data=sum.wf.austria, mapping=aes(x=date, y=theta, ymin=lower, ymax=upper,color=label))+
  scale_colour_manual(values=c("#bf271c","#63c3d0","#87b52a","#0056a2","#e84188"), name=' Name - Party')+
  geom_point(size=4)+
  geom_smooth(method = 'loess')+  labs(x = '', y = 'Position Estimates')+
  annotate("text", x=sum.wf.austria$date[51]+1, y=1.5, label="Freedom Party",colour="#0056a2",size=6,hjust = 0,family="Verdana") +
  annotate("text", x=sum.wf.austria$date[51]+7, y=0.9, label="New People's Party",colour="#63c3d0",size=6,hjust = 0,family="Verdana")+
  annotate("text", x=sum.wf.austria$date[51]+8, y=-0.1, label="Social Democrats",colour="#bf271c",size=6,hjust = 0,family="Verdana")+
  annotate("text", x=sum.wf.austria$date[51]+1, y=-0.4, label="Green Party",colour="#87b52a",size=6,hjust = 0,family="Verdana")+
  annotate("text", x=sum.wf.austria$date[51]+3, y=-1.4, label="NEOS - The New Austria",colour="#e84188",size=6,hjust = 0,family="Verdana")+ 
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.position="none",
        legend.text=element_text(family="Verdana"))+ 
  scale_x_date(date_labels = "%b %d")
graph.time.points

pdf("graphs_paper/Figure_3.pdf", width = 14, height = 10) # Open a new pdf file
graph.time.points
dev.off()

#############
# Clustered Nonparametric Bootstraps

boot.res=data.frame(estimate.lr=rep(NA,2000),
                    estimate.migr=rep(NA,2000),
                    estimate.galtan=rep(NA,2000))

for(i in 1:nrow(boot.res)){
  #build clustered data frame
  parties=as.character(sample(unique(sum.wf.austria$party_name),size = 5,replace = T))
  boot.dat=rbind(sum.wf.austria[sum.wf.austria$party_name==parties[1],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[2],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[3],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[4],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[5],])
  #estimate model
  boot.dat.pdata <- pdata.frame(boot.dat, index = "party_name")
  if(length(unique(parties))>1){
    mod.lr.boot <- plm(theta~oppo_lr, data = boot.dat.pdata, model = "within", effect="individual")
    mod.migr.boot <- plm(theta~oppo_migr, data = boot.dat.pdata, model = "within", effect="individual")
    mod.galtan.boot <- plm(theta~oppo_galtan, data = boot.dat.pdata, model = "within", effect="individual")
    # save estimates
    boot.res[i,"estimate.lr"]=coef(mod.lr.boot)
    boot.res[i,"estimate.migr"]=coef(mod.migr.boot)
    boot.res[i,"estimate.galtan"]=coef(mod.galtan.boot)
  }
  print(i)
}


#This produces the numbers for model 1 in Table 2
round(mean(boot.res$estimate.lr,na.rm = T),2)
round(mean(boot.res$estimate.lr,na.rm = T)-1.96*sd(boot.res$estimate.lr,na.rm = T),2)
round(mean(boot.res$estimate.lr,na.rm = T)+1.96*sd(boot.res$estimate.lr,na.rm = T),2)

#This produces the numbers for model 2 in Table 2
round(mean(boot.res$estimate.migr,na.rm = T),2)
round(mean(boot.res$estimate.migr,na.rm = T)-1.96*sd(boot.res$estimate.migr,na.rm = T),2)
round(mean(boot.res$estimate.migr,na.rm = T)+1.96*sd(boot.res$estimate.migr,na.rm = T),2)

#This produces the numbers for model 3 in Table 2
round(mean(boot.res$estimate.galtan,na.rm = T),2)
round(mean(boot.res$estimate.galtan,na.rm = T)-1.96*sd(boot.res$estimate.galtan,na.rm = T),2)
round(mean(boot.res$estimate.galtan,na.rm = T)+1.96*sd(boot.res$estimate.galtan,na.rm = T),2)

#####################
# Model Influence of debate party on own position
sum.wf.austria.pdata <- pdata.frame(sum.wf.austria, index = "party_name")
sum.wf.austria.pdata$oppo_lr_med
sum.wf.austria$oppo_lr_med

mod.lr <- plm(theta~oppo_lr, data = sum.wf.austria.pdata, model = "within", effect="individual")
mod.migr <- plm(theta~oppo_migr, data = sum.wf.austria.pdata, model = "within", effect="individual")
mod.galtan <- plm(theta~oppo_galtan, data = sum.wf.austria.pdata, model = "within", effect="individual")

estimates.models=c(round(mean(boot.res$estimate.lr,na.rm = T),2),
                   round(mean(boot.res$estimate.migr,na.rm = T),2),
                   round(mean(boot.res$estimate.galtan,na.rm = T),2))
ci.models=list(Model1=matrix(c(round(mean(boot.res$estimate.lr,na.rm = T)-1.96*sd(boot.res$estimate.lr,na.rm = T),2),
                        round(mean(boot.res$estimate.lr,na.rm = T)+1.96*sd(boot.res$estimate.lr,na.rm = T),2)),ncol = 2),
               Model2=matrix(c(round(mean(boot.res$estimate.migr,na.rm = T)-1.96*sd(boot.res$estimate.migr,na.rm = T),2),
                               round(mean(boot.res$estimate.migr,na.rm = T)+1.96*sd(boot.res$estimate.migr,na.rm = T),2)),ncol = 2),
               Model3=matrix(c(round(mean(boot.res$estimate.galtan,na.rm = T)-1.96*sd(boot.res$estimate.galtan,na.rm = T),2),
                               round(mean(boot.res$estimate.galtan,na.rm = T)+1.96*sd(boot.res$estimate.galtan,na.rm = T),2)),ncol = 2))
#####
# We use this for getting the R-squared values in Table 2, the actual estimates and confidence intervals in Table 2 come from the bootstrap above.
# The results of this are reported in Table A3 in Appendix 13
stargazer(mod.lr,mod.migr,mod.galtan,
          title            = "Table 2: Explaining Campaign Positions of Party Leaders (Austria 2017)",
          dep.var.labels=c(""),
          dep.var.caption=c(""),
          covariate.labels=c("Left-Right Position of Debating Partners",
                             "Migration Policy Position of Debating Partners",
                             "GAL-TAN Position of Debating Partners"),
          column.labels = c("Model 1","Model 2", "Model 3"),
          coef=estimates.models,
          omit.stat = c("f" , "ser","adj.rsq"),
          ci = T,
          ci.custom = ci.models,
          type = "text",star.cutoffs = c(0.00000000001),
          add.lines = list(c("Fixed effects", "Candidates", "Candidates","Candidates")),
          omit.table.layout = "n",
          font.size = "scriptsize",
          out="tables/Table2.tex")

stargazer(mod.lr,mod.migr,mod.galtan,
          title            = "Table A3: Explaining Campaign Positions of Party Leaders (Austria 2017)",
          dep.var.labels=c(""),
          dep.var.caption=c(""),
          covariate.labels=c("Left-Right Position of Debating Partners",
                             "Migration Policy Position of Debating Partners",
                             "GAL-TAN Position of Debating Partners"),
          column.labels = c("Model A1","Model A2", "Model A3"),
          omit.stat = c("f" , "ser","adj.rsq"),
          ci = T,
          type = "text",star.cutoffs = c(0.00000000001),
          add.lines = list(c("Fixed effects", "Parties", "Parties","Parties")),
          omit.table.layout = "n",
          font.size = "scriptsize",
          out="tables/TableA3.tex")

#####################
# Model Influence of debate party on own position Bootstrapped with Griss, Moser, Hofer

boot.res.app=data.frame(estimate.lr=rep(NA,2000),
                        estimate.migr=rep(NA,2000),
                        estimate.galtan=rep(NA,2000))

for(i in 1:nrow(boot.res.app)){
  #build clustered data frame
  parties=as.character(sample(unique(sum.wf.austria.app$party_name),size = 5,replace = T))
  boot.dat=rbind(sum.wf.austria.app[sum.wf.austria.app$party_name==parties[1],],
                 sum.wf.austria.app[sum.wf.austria.app$party_name==parties[2],],
                 sum.wf.austria.app[sum.wf.austria.app$party_name==parties[3],],
                 sum.wf.austria.app[sum.wf.austria.app$party_name==parties[4],],
                 sum.wf.austria.app[sum.wf.austria.app$party_name==parties[5],])
  #estimate model
  boot.dat.pdata <- pdata.frame(boot.dat, index = "party_name")
  if(length(unique(parties))>1){
    mod.lr.boot <- plm(theta~oppo_lr, data = boot.dat.pdata, model = "within", effect="individual")
    mod.migr.boot <- plm(theta~oppo_migr, data = boot.dat.pdata, model = "within", effect="individual")
    mod.galtan.boot <- plm(theta~oppo_galtan, data = boot.dat.pdata, model = "within", effect="individual")
    # save estimates
    boot.res.app[i,"estimate.lr"]=coef(mod.lr.boot)
    boot.res.app[i,"estimate.migr"]=coef(mod.migr.boot)
    boot.res.app[i,"estimate.galtan"]=coef(mod.galtan.boot)
  }
  print(i)
}

#This produces the numbers for model A1 in Table A4
round(mean(boot.res.app$estimate.lr,na.rm = T),digits=2)
round(mean(boot.res.app$estimate.lr,na.rm = T)-1.96*sd(boot.res.app$estimate.lr,na.rm = T),2)
round(mean(boot.res.app$estimate.lr,na.rm = T)+1.96*sd(boot.res.app$estimate.lr,na.rm = T),2)

#This produces the numbers for model A2 in Table A4
round(mean(boot.res.app$estimate.migr,na.rm = T),digits=2)
round(mean(boot.res.app$estimate.migr,na.rm = T)-1.96*sd(boot.res.app$estimate.migr,na.rm = T),2)
round(mean(boot.res.app$estimate.migr,na.rm = T)+1.96*sd(boot.res.app$estimate.migr,na.rm = T),2)

#This produces the numbers for model A3 in Table A4
round(mean(boot.res.app$estimate.galtan,na.rm = T),2)
round(mean(boot.res.app$estimate.galtan,na.rm = T)-1.96*sd(boot.res.app$estimate.galtan,na.rm = T),2)
round(mean(boot.res.app$estimate.galtan,na.rm = T)+1.96*sd(boot.res.app$estimate.galtan,na.rm = T),2)

sum.wf.austria.pdata.app <- pdata.frame(sum.wf.austria.app, index = "party_name")

mod.lr.app <- plm(theta~oppo_lr, data = sum.wf.austria.pdata.app, model = "within", effect="individual")
mod.migr.app <- plm(theta~oppo_migr, data = sum.wf.austria.pdata.app, model = "within", effect="individual")
mod.galtan.app <- plm(theta~oppo_galtan, data = sum.wf.austria.pdata.app, model = "within", effect="individual")
stargazer(mod.lr.app,mod.migr.app,mod.galtan.app,ci = T,type = "text")

estimates.models=c(round(mean(boot.res.app$estimate.lr,na.rm = T),2),
                   round(mean(boot.res.app$estimate.migr,na.rm = T),2),
                   round(mean(boot.res.app$estimate.galtan,na.rm = T),2))
ci.models=list(Model1=matrix(c(round(mean(boot.res.app$estimate.lr,na.rm = T)-1.96*sd(boot.res.app$estimate.lr,na.rm = T),2),
                               round(mean(boot.res.app$estimate.lr,na.rm = T)+1.96*sd(boot.res.app$estimate.lr,na.rm = T),2)),ncol = 2),
               Model2=matrix(c(round(mean(boot.res.app$estimate.migr,na.rm = T)-1.96*sd(boot.res.app$estimate.migr,na.rm = T),2),
                               round(mean(boot.res.app$estimate.migr,na.rm = T)+1.96*sd(boot.res.app$estimate.migr,na.rm = T),2)),ncol = 2),
               Model3=matrix(c(round(mean(boot.res.app$estimate.galtan,na.rm = T)-1.96*sd(boot.res.app$estimate.galtan,na.rm = T),2),
                               round(mean(boot.res.app$estimate.galtan,na.rm = T)+1.96*sd(boot.res.app$estimate.galtan,na.rm = T),2)),ncol = 2))
#####
# We use this for getting the R-squared values in Table 2, the actual estimates and confidence intervals in Table 2 come from the bootstrap above.
# The results of this are reported in Table A3 in Appendix 13
stargazer(mod.lr.app,mod.migr.app,mod.galtan.app,
          title            = "Table A4: Explaining Campaign Positions of Party Leaders (Austria 2017)",
          dep.var.labels=c(""),
          dep.var.caption=c(""),
          covariate.labels=c("Left-Right Position of Debating Partners",
                             "Migration Policy Position of Debating Partners",
                             "GAL-TAN Position of Debating Partners"),
          column.labels = c("Model A1","Model A2", "Model A3"),
          coef=estimates.models,
          omit.stat = c("f" , "ser","adj.rsq"),
          ci = T,
          ci.custom = ci.models,
          type = "text",star.cutoffs = c(0.00000000001),
          add.lines = list(c("Fixed effects", "Parties", "Parties","Parties")),
          omit.table.layout = "n",
          font.size = "scriptsize",
          out="tables/TableA4.tex")


#############
# Clustered Nonparametric Bootstraps with Median Position in CHES

boot.res.med=data.frame(estimate.lr.med=rep(NA,2000),
                        estimate.migr.med=rep(NA,2000),
                        estimate.galtan.med=rep(NA,2000))

for(i in 1:nrow(boot.res.med)){
  #build clustered data frame
  parties=as.character(sample(unique(sum.wf.austria$party_name),size = 5,replace = T))
  boot.dat=rbind(sum.wf.austria[sum.wf.austria$party_name==parties[1],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[2],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[3],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[4],],
                 sum.wf.austria[sum.wf.austria$party_name==parties[5],])
  #estimate model
  boot.dat.pdata <- pdata.frame(boot.dat, index = "party_name")
  if(length(unique(parties))>1){
    mod.lr.boot <- plm(theta~oppo_lr_med, data = boot.dat.pdata, model = "within", effect="individual")
    mod.migr.boot <- plm(theta~oppo_migr_med, data = boot.dat.pdata, model = "within", effect="individual")
    mod.galtan.boot <- plm(theta~oppo_galtan_med, data = boot.dat.pdata, model = "within", effect="individual")
    # save estimates
    boot.res.med[i,"estimate.lr.med"]=coef(mod.lr.boot)
    boot.res.med[i,"estimate.migr.med"]=coef(mod.migr.boot)
    boot.res.med[i,"estimate.galtan.med"]=coef(mod.galtan.boot)
  }
  print(i)
}

#This produces the numbers for model A1 in Table A5
round(mean(boot.res.med$estimate.lr.med,na.rm = T),2)
round(mean(boot.res.med$estimate.lr.med,na.rm = T)-1.96*sd(boot.res.med$estimate.lr.med,na.rm = T),2)
round(mean(boot.res.med$estimate.lr.med,na.rm = T)+1.96*sd(boot.res.med$estimate.lr.med,na.rm = T),2)

#This produces the numbers for model A2 in Table A5
round(mean(boot.res.med$estimate.migr.med,na.rm = T),2)
round(mean(boot.res.med$estimate.migr.med,na.rm = T)-1.96*sd(boot.res.med$estimate.migr.med,na.rm = T),2)
round(mean(boot.res.med$estimate.migr.med,na.rm = T)+1.96*sd(boot.res.med$estimate.migr.med,na.rm = T),2)

#This produces the numbers for model A3 in Table A5
round(mean(boot.res.med$estimate.galtan.med,na.rm = T),2)
round(mean(boot.res.med$estimate.galtan.med,na.rm = T)-1.96*sd(boot.res.med$estimate.galtan.med,na.rm = T),2)
round(mean(boot.res.med$estimate.galtan.med,na.rm = T)+1.96*sd(boot.res.med$estimate.galtan.med,na.rm = T),2)


mod.lr.med <- plm(theta~oppo_lr_med, data = sum.wf.austria.pdata, model = "within", effect="individual")
mod.migr.med <- plm(theta~oppo_migr_med, data = sum.wf.austria.pdata, model = "within", effect="individual")
mod.galtan.med <- plm(theta~oppo_galtan_med, data = sum.wf.austria.pdata, model = "within", effect="individual")
stargazer(mod.lr.med,mod.migr.med,mod.galtan.med,ci = T,type = "text")

estimates.models=c(round(mean(boot.res.med$estimate.lr,na.rm = T),2),
                   round(mean(boot.res.med$estimate.migr,na.rm = T),2),
                   round(mean(boot.res.med$estimate.galtan,na.rm = T),2))
ci.models=list(Model1=matrix(c(round(mean(boot.res.med$estimate.lr,na.rm = T)-1.96*sd(boot.res.med$estimate.lr,na.rm = T),2),
                               round(mean(boot.res.med$estimate.lr,na.rm = T)+1.96*sd(boot.res.med$estimate.lr,na.rm = T),2)),ncol = 2),
               Model2=matrix(c(round(mean(boot.res.med$estimate.migr,na.rm = T)-1.96*sd(boot.res.med$estimate.migr,na.rm = T),2),
                               round(mean(boot.res.med$estimate.migr,na.rm = T)+1.96*sd(boot.res.med$estimate.migr,na.rm = T),2)),ncol = 2),
               Model3=matrix(c(round(mean(boot.res.med$estimate.galtan,na.rm = T)-1.96*sd(boot.res.med$estimate.galtan,na.rm = T),2),
                               round(mean(boot.res.med$estimate.galtan,na.rm = T)+1.96*sd(boot.res.med$estimate.galtan,na.rm = T),2)),ncol = 2))
#####
# We use this for getting the R-squared values in Table 2, the actual estimates and confidence intervals in Table 2 come from the bootstrap above.
# The results of this are reported in Table A3 in Appendix 13
stargazer(mod.lr.med,mod.migr.med,mod.galtan.med,
          title            = "Table A5: Explaining Campaign Positions of Party Leaders (Austria 2017)",
          dep.var.labels=c(""),
          dep.var.caption=c(""),
          covariate.labels=c("Left-Right Position of Debating Partners",
                             "Migration Policy Position of Debating Partners",
                             "GAL-TAN Position of Debating Partners"),
          column.labels = c("Model A1","Model A2", "Model A3"),
          coef=estimates.models,
          omit.stat = c("f" , "ser","adj.rsq"),
          ci = T,
          ci.custom = ci.models,
          type = "text",star.cutoffs = c(0.00000000001),
          add.lines = list(c("Fixed effects", "Parties", "Parties","Parties")),
          omit.table.layout = "n",
          font.size = "scriptsize",
          out="tables/TableA5.tex")
