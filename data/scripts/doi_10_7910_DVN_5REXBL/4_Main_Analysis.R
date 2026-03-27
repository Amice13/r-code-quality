
load("data/main_analysis.rdata")

# formatierung homogenisieren (ttp)   alle tabellen
# überschrift splits
# weite regulieren?




source("tools/toolbox.R")

me$theta<-log(me$theta)*20
me$theta_se<-me$theta_se*20


me$theta_r<-log(me$theta_r)*20
me$theta_r_se<-me$theta_r_se*20


me$winner<-factor(me$winner,labels=c("Loser","Winner"))
names(me)
me<-me[me$ttp>=-25,]
me<-me[me$ttp<=25,]
pdf("./figures/figure3.pdf",width = 12,height = 8)

plot_ts(me_ts = me,var = "theta",time = "ttp",var2 = "theta_se",ci=2)
dev.off()


me$party<-as.factor(me$party)


me$dep<-me$theta
me$dep2<-me$theta_r
me$depl<-me$dep # phony copy for stargazer layout reasons
me$dep2l<-me$dep2
me$interven<-me$ttp>0
me$loser<-relevel(me$winner,ref = "Winner")
me$share<-me$poltweets/me$n

# Democrats

checkm1_d<-lm(dep~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=me[me$party=="D",])
summary(checkm1_d)

checkm1_pd<-lm(dep~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven+poltweets,data=me[me$party=="D",])
summary(checkm1_pd)


checkm1_ds<-lm(dep~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven+share,data=me[me$party=="D",])
summary(checkm1_ds)

checkm2_d<-lm(dep2~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=me[me$party=="D",])
summary(checkm2_d)

### Loser only models, dem


checkm1_d_l<-lm(depl~ttp+interven+ttp*interven*ttp,data=me[me$party=="D" & me$loser=="Loser",])
summary(checkm1_d_l)

checkm2_d_l<-lm(dep2l~ttp+interven+ttp*interven*ttp,data=me[me$party=="D" & me$loser=="Loser",])
summary(checkm2_d_l)



# Republicans
checkm1_r<-lm(dep~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=me[me$party=="R",])
summary(checkm1_r)

checkm1_rp<-lm(dep~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven+poltweets,data=me[me$party=="R",])
summary(checkm1_rp)

checkm1_rs<-lm(dep~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven+share,data=me[me$party=="R",])
summary(checkm1_rs)

checkm2_r<-lm(dep2~ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=me[me$party=="R",])
summary(checkm2_d)
nrow(me)



### Loser only models, rep

checkm1_r_l<-lm(depl~ttp+interven+ttp*interven*ttp,data=me[me$party=="R" & me$loser=="Loser",])
summary(checkm1_r_l)
checkm2_r_l<-lm(dep2l~ttp+interven+ttp*interven*ttp,data=me[me$party=="R" & me$loser=="Loser",])
summary(checkm2_r_l)







model1<-checkm1_d
model2<-checkm1_r

interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


######## Controlling for number of political tweets


model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          Model = "Democrats")

model1Frame$Variable<-c('Intercept',"Time to Primary","After Primary","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          Model = "Republicans")

# Combine these data.frames

# Combine these data.frames
model2Frame$Variable<-c('Intercept',"Time to Primary","After Primary","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")

level_order1<-c('Intercept',"Time to Primary","After Primary","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")


allModelFrame <- data.frame(rbind(model1Frame, model2Frame))  # etc.

colscheme<-c("blue","red")

zp1<-mult_plot(allModelFrame,level_order=level_order1,sz=14,l=1.5,legend=T)
## Coefficient plot not part of the paper
#pdf('./appendix/figures/figure_x.pdf',width = 12, height = 10)
#zp1
#dev.off()
#level_order2<-c("Time to Primary","After Primary","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser",'Intercept')

level_order<-c("Time to Primary (T\\textsubscript{t})","After Primary (X\\textsubscript{t})","Loser (Z\\textsubscript{i})","TTP:After Primary (X\\textsubscript{t}T\\textsubscript{t})","After Primary: Loser (Z\\textsubscript{i}T\\textsubscript{t}) ","TTP: Loser","TTP:After Primary: Loser (Z\\textsubscript{i}X\\textsubscript{t}T\\textsubscript{t})",'Intercept')

stargazer(checkm1_d,checkm1_r,checkm1_d_l,checkm1_r_l,type="latex",out ="./tables/table1.tex",covariate.labels = level_order,label = "itsparty",title = "ITS Results: Party Level",column.labels = c("Democrats","Republicans","Democrats","Republicans"),dep.var.labels=c("All Candidates","Losers Only"),style = "apsr")


#### Plot models with correction, controlling for Policy Tweets (Table 9, Appendix)


model1<-checkm1_pd
model2<-checkm1_rp


# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          Model = "Democrats")

model1Frame$Variable<-c('Intercept',"Time to Primary","After Primary","Total Policy Tweets","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          Model = "Republicans")

# Combine these data.frames

# Combine these data.frames
model2Frame$Variable<-c('Intercept',"Time to Primary","After Primary","Total Policy Tweets","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")

level_order1<-c('Intercept',"Time to Primary","After Primary","Total Policy Tweets","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")


allModelFrame <- data.frame(rbind(model1Frame, model2Frame))  # etc.

colscheme<-c("blue","red")

zp1<-mult_plot(allModelFrame,level_order=level_order1,sz=14,l=1.5,legend=T)

#pdf('./appendix/figures/figure13.pdf',width = 12, height = 10)
#zp1
#dev.off()

# Creating Table 9, additional control for policy tweets

level_order<-c("Time to Primary (T\\textsubscript{t})","After Primary (X\\textsubscript{t})","Loser (Z\\textsubscript{i})","No. Policy Tweets","TTP:After Primary (X\\textsubscript{t}T\\textsubscript{t})","After Primary: Loser (Z\\textsubscript{i}T\\textsubscript{t}) ","TTP: Loser","TTP:After Primary: Loser (Z\\textsubscript{i}X\\textsubscript{t}T\\textsubscript{t})",'Intercept')

stargazer(checkm1_pd,checkm1_rp,type="latex",out ="./appendix/tables/table9.tex",covariate.labels = level_order,dep.var.labels = "Position",label = "itsparty_polcontrol",title = "ITS Results: Policy Tweets Control",column.labels = c("Democrats","Republicans"),style = "apsr")
#stargazer(checkm1,checkm2,type="latex",out ="./tables/party_models.tex",covariate.labels = level_order2,dep.var.labels = "Position",label = "itsparty",title = "Interrupted Time-Series Analysis Results",column.labels = c("Democrats","Republicans"),style = "apsr")





### Exporting Results only using Policy Tweets:


model1<-checkm2_d
model2<-checkm2_r


# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          Model = "Democrats")

model1Frame$Variable<-c('Intercept',"Time to Primary","After Primary","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          Model = "Republicans")


model2Frame$Variable<-c('Intercept',"Time to Primary","After Primary","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")

level_order1<-c('Intercept',"Time to Primary","After Primary","Loser","TTP:After Primary","After Primary: Loser","TTP: Loser","TTP:After Primary: Loser")


allModelFrame <- data.frame(rbind(model1Frame, model2Frame))  # etc.

colscheme<-c("blue","red")

zp1<-mult_plot(allModelFrame,level_order=level_order1,sz=14,l=1.5,legend=T)

pdf('./appendix/figures/figure13.pdf',width = 12, height = 10)
zp1
dev.off()


# Creating Table 2

level_order<-c("Time to Primary (T\\textsubscript{t})","After Primary (X\\textsubscript{t})","Loser (Z\\textsubscript{i})","TTP:After Primary (X\\textsubscript{t}T\\textsubscript{t})","After Primary: Loser (Z\\textsubscript{i}T\\textsubscript{t}) ","TTP: Loser","TTP:After Primary: Loser (Z\\textsubscript{i}X\\textsubscript{t}T\\textsubscript{t})",'Intercept')

stargazer(checkm2_d,checkm2_r,checkm2_d_l,checkm2_r_l,type="latex",out ="./tables/table2.tex",covariate.labels = level_order,dep.var.labels = c("All Candidates","Losers Only"),label = "itsparty_policy",title = "ITS Results: Policy Tweets Only",
          column.labels = c("Democrats","Republicans","Democrats","Republicans"),style = "apsr")






### lagged dv

rl<-me[me$party=="R" & me$loser=="Loser",]

dl<-me[me$party=="D" & me$loser=="Winner",]

dw<-me[me$party=="D" & me$loser=="Loser",]

rw<-me[me$party=="R" & me$loser=="Winner",]


rw<-rw[order(rw$ttp,decreasing = F),]
dw<-dw[order(dw$ttp,decreasing = F),]
rl<-rl[order(rl$ttp,decreasing = F),]
dl<-dl[order(dl$ttp,decreasing = F),]


rw$dep_lag<-lag(rw$dep,1)
rl$dep_lag<-lag(rl$dep,1)
dw$dep_lag<-lag(dw$dep,1)
dl$dep_lag<-lag(dl$dep,1)


d2<-rbind(dw,dl)
r2<-rbind(rw,rl)

checkm1_r_lag<-lm(dep~dep_lag+ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=r2)
summary(checkm1_r_lag)

checkm1_d_lag<-lm(dep~dep_lag+ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=d2)
summary(checkm1_d_lag)



rw$dep2_lag<-lag(rw$dep2,1)
rl$dep2_lag<-lag(rl$dep2,1)
dw$dep2_lag<-lag(dw$dep2,1)
dl$dep2_lag<-lag(dl$dep2,1)

d2<-rbind(dw,dl)
r2<-rbind(rw,rl)

checkm2_r_lag<-lm(dep2~dep2_lag+ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=r2)
summary(checkm2_r_lag)

checkm2_d_lag<-lm(dep2~dep2_lag+ttp+interven+ttp*interven+loser+loser*interven+loser*ttp+loser*ttp*interven,data=d2)
summary(checkm2_d_lag)

level_order<-c("Lagged Position"
               ,"Lagged Position Policy Only"
               ,"Time to Primary (T\\textsubscript{t})"
               ,"After Primary (X\\textsubscript{t})"
               ,"Loser (Z\\textsubscript{i})"
               ,"TTP:After Primary (X\\textsubscript{t}T\\textsubscript{t})"
               ,"After Primary: Loser (Z\\textsubscript{i}T\\textsubscript{t}) "
               ,"TTP: Loser"
               ,"TTP:After Primary: Loser (Z\\textsubscript{i}X\\textsubscript{t}T\\textsubscript{t})"
               ,'Intercept')

stargazer(checkm1_d_lag,checkm1_r_lag,checkm2_d_lag,checkm2_r_lag,type="latex",out ="./appendix/tables/table15.tex",covariate.labels = level_order,dep.var.labels = c("Position","Policy Only Position"),label = "lagged_models",title = "Lagged DV As Additional Control",column.labels = c("Democrats","Republicans","Democrats","Republicans"),style = "apsr")




# two-way fixed effects 


checkm2_d_ff<-lm(dep~loser*interven,data=d2)
summary(checkm2_d_ff)


checkm2_r_ff<-lm(dep~loser*interven,data=r2)
summary(checkm2_r_ff)

level_order<-c("Lagged Position","Time to Primary (T\\textsubscript{t})","After Primary (X\\textsubscript{t})","Loser (Z\\textsubscript{i})","TTP:After Primary (X\\textsubscript{t}T\\textsubscript{t})","After Primary: Loser (Z\\textsubscript{i}T\\textsubscript{t}) ","TTP: Loser","TTP:After Primary: Loser (Z\\textsubscript{i}X\\textsubscript{t}T\\textsubscript{t})",'Intercept')


stargazer(checkm2_d_ff,checkm2_r_ff,type="latex",out ="./appendix/tables/table14.tex",covariate.labels = level_order,dep.var.labels = c("Position","Policy Only Position"),label = "fixed_effects_main",title = "Two-Way-Fixed-Effect Version",column.labels = c("Democrats","Republicans"),style = "apsr")


