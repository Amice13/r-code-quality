

############################
####MAIN SPECIFICATIONS#####
############################


##
##Table 1: Reduced form
##



lm1 <- lm(tmh.dummy~china.inland.mission.1865.ln, data=dat)
lm2 <- lm(tmh.dummy~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- lm(tmh.dummy~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- lm(nationalist.post05~china.inland.mission.1865.ln, data=dat)
lm5 <- lm(nationalist.post05~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- lm(nationalist.post05~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)

rob.fit1        <- coeftest(lm1, function(x) vcovHC(x, type="HC0"))
rob.fit2        <- coeftest(lm2, function(x) vcovHC(x, type="HC0"))
rob.fit3        <- coeftest(lm3, function(x) vcovHC(x, type="HC0"))
rob.fit4        <- coeftest(lm4, function(x) vcovHC(x, type="HC0"))
rob.fit5        <- coeftest(lm5, function(x) vcovHC(x, type="HC0"))
rob.fit6        <- coeftest(lm6, function(x) vcovHC(x, type="HC0"))


stargazer(lm1, lm2, lm3, lm4, lm5, lm6, type = "latex", 
          se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"],
                    rob.fit3[,"Std. Error"], rob.fit4[,"Std. Error"],
                    rob.fit5[,"Std. Error"], rob.fit6[,"Std. Error"]), 
          omit=c("prov", "treaty", "size.ln", "coast", "railwaydist.ln", "newspapers.ln", "quota.ln", "pop.ln", "Constant" ),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Mission Size (Log)"),
          out = "Table_1.tex")




##
##Table 2: First stage
##


lm1 <- lm(missionary.cases.ln~china.inland.mission.1865.ln, data=dat)
lm2 <- lm(missionary.cases.ln~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- lm(missionary.cases.ln~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- lm(missionary.cases.dummy~china.inland.mission.1865.ln, data=dat)
lm5 <- lm(missionary.cases.dummy~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- lm(missionary.cases.dummy~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)

rob.fit1        <- coeftest(lm1, function(x) vcovHC(x, type="HC0"))
rob.fit2        <- coeftest(lm2, function(x) vcovHC(x, type="HC0"))
rob.fit3        <- coeftest(lm3, function(x) vcovHC(x, type="HC0"))
rob.fit4        <- coeftest(lm4, function(x) vcovHC(x, type="HC0"))
rob.fit5        <- coeftest(lm5, function(x) vcovHC(x, type="HC0"))
rob.fit6        <- coeftest(lm6, function(x) vcovHC(x, type="HC0"))


stargazer(lm1, lm2, lm3, lm4, lm5, lm6, type = "latex", 
          se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"],
                    rob.fit3[,"Std. Error"], rob.fit4[,"Std. Error"],
                    rob.fit5[,"Std. Error"], rob.fit6[,"Std. Error"]), 
          omit=c("prov", "treaty", "size.ln", "coast", "railwaydist.ln", "newspapers.ln", "quota.ln", "pop.ln", "Constant" ),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("Missionary Conflicts (Log)", "Violent Missionary Conflicts (Log)"),
          covariate.labels = c("Mission Size (Log)"),
          out = "Table_2.tex")



##
##Table 3: 2SLS results
##


lm1 <- ivreg(tmh.dummy~missionary.cases.ln|china.inland.mission.1865.ln, data=dat)
lm2 <- ivreg(tmh.dummy~missionary.cases.ln+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- ivreg(tmh.dummy~missionary.cases.ln+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov)|
               china.inland.mission.1865.ln+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- ivreg(nationalist.post05~missionary.cases.ln|china.inland.mission.1865.ln, data=dat)
lm5 <- ivreg(nationalist.post05~missionary.cases.ln+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- ivreg(nationalist.post05~missionary.cases.ln+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov)|
               china.inland.mission.1865.ln+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)


rob.fit1        <- coeftest(lm1, function(x) vcovHC(x, type="HC0"))
rob.fit2        <- coeftest(lm2, function(x) vcovHC(x, type="HC0"))
rob.fit3        <- coeftest(lm3, function(x) vcovHC(x, type="HC0"))
rob.fit4        <- coeftest(lm4, function(x) vcovHC(x, type="HC0"))
rob.fit5        <- coeftest(lm5, function(x) vcovHC(x, type="HC0"))
rob.fit6        <- coeftest(lm6, function(x) vcovHC(x, type="HC0"))
summ.fit1 <- summary(lm1, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit2 <- summary(lm2, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit3 <- summary(lm3,  vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit4 <- summary(lm4, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit5 <- summary(lm5, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)
summ.fit6 <- summary(lm6, vcov. = function(x) vcovHC(x, type="HC0"), diagnostics=T)


stargazer(lm1, lm2, lm3, lm4, lm5, lm6, type = "latex", 
          se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"],
                    rob.fit3[,"Std. Error"], rob.fit4[,"Std. Error"],
                    rob.fit5[,"Std. Error"], rob.fit6[,"Std. Error"]), 
          add.lines = list(c(rownames(summ.fit1$diagnostics)[1], 
                             round(summ.fit1$diagnostics[1, "statistic"], 2),
                             round(summ.fit2$diagnostics[1, "statistic"], 2), 
                             round(summ.fit2$diagnostics[1, "statistic"], 2), 
                             round(summ.fit4$diagnostics[1, "statistic"], 2), 
                             round(summ.fit5$diagnostics[1, "statistic"], 2), 
                             round(summ.fit6$diagnostics[1, "statistic"], 2))),
          omit=c("prov", "treaty", "size.ln", "coast", "railwaydist.ln", "newspapers.ln", "quota.ln", "pop.ln", "Constant" ),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Missionary conflicts (log)"),
          out = "Table_3.tex")





##
##Table 4: Mission size and press publications
##


lm1 <- lm(books.politics.ln~china.inland.mission.1865.ln, data=dat, subset = books.total<1000 )
lm2 <- lm(books.politics.ln~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat , subset = books.total<1000)
lm3 <- lm(books.politics.ln~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat, subset = books.total<1000 )
lm4 <- lm(books.foreign.ln~china.inland.mission.1865.ln, data=dat, subset = books.total<1000 )
lm5 <- lm(books.foreign.ln~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat, subset = books.total<1000 )
lm6 <- lm(books.foreign.ln~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat, subset = books.total<1000)


rob.fit1        <- coeftest(lm1, function(x) vcovHC(x, type="HC0"))
rob.fit2        <- coeftest(lm2, function(x) vcovHC(x, type="HC0"))
rob.fit3        <- coeftest(lm3, function(x) vcovHC(x, type="HC0"))
rob.fit4        <- coeftest(lm4, function(x) vcovHC(x, type="HC0"))
rob.fit5        <- coeftest(lm5, function(x) vcovHC(x, type="HC0"))
rob.fit6        <- coeftest(lm6, function(x) vcovHC(x, type="HC0"))


stargazer(lm1, lm2, lm3, lm4, lm5, lm6, type = "latex", 
          se = list(rob.fit1[,"Std. Error"], rob.fit2[,"Std. Error"],
                    rob.fit3[,"Std. Error"], rob.fit4[,"Std. Error"],
                    rob.fit5[,"Std. Error"], rob.fit6[,"Std. Error"]), 
          omit=c("prov", "pop.ln", "treaty", "size", "coast", "railwaydist.ln", "newspapers.ln", "quota.ln", "Constant"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Mission Size (Log)"),
          dep.var.labels = c("Politics Books (Log)", "Foreign Books (Log)"),
          out = "Table_4.tex")






##
##Figure 1: 2SLS placebo test
##


lm1 <- lm(scale(taxpc1820)~log(china_inland_mission_1865+1), data=dat)
lm2 <- lm(scale(rankc_p)~log(china_inland_mission_1865+1), data=dat)
lm3 <- lm(scale(log(jinshi/pop+1))~log(china_inland_mission_1865+1), data=dat)
lm4 <- lm(scale(temples.pop)~log(china_inland_mission_1865+1), data=dat)
lm5 <- lm(scale(rice)~log(china_inland_mission_1865+1), data=dat)
lm6 <- lm(scale(fragm)~log(china_inland_mission_1865+1), data=dat)



estimates <- data.frame(pe=c(summary(lm1)$coefficients[2,1], 
                             summary(lm2)$coefficients[2,1],
                             summary(lm3)$coefficients[2,1],
                             summary(lm4)$coefficients[2,1],
                             summary(lm5)$coefficients[2,1],
                             summary(lm6)$coefficients[2,1]),
                        se.high=c(summary(lm1)$coefficients[2,1]+1.96*summary(lm1)$coefficients[2,2],
                                  summary(lm2)$coefficients[2,1]+1.96*summary(lm2)$coefficients[2,2],
                                  summary(lm3)$coefficients[2,1]+1.96*summary(lm3)$coefficients[2,2],
                                  summary(lm4)$coefficients[2,1]+1.96*summary(lm4)$coefficients[2,2],
                                  summary(lm5)$coefficients[2,1]+1.96*summary(lm5)$coefficients[2,2],
                                  summary(lm6)$coefficients[2,1]+1.96*summary(lm6)$coefficients[2,2]),
                        se.low=c(summary(lm1)$coefficients[2,1]-1.96*summary(lm1)$coefficients[2,2],
                                 summary(lm2)$coefficients[2,1]-1.96*summary(lm2)$coefficients[2,2],
                                 summary(lm3)$coefficients[2,1]-1.96*summary(lm3)$coefficients[2,2],
                                 summary(lm4)$coefficients[2,1]-1.96*summary(lm4)$coefficients[2,2],
                                 summary(lm5)$coefficients[2,1]-1.96*summary(lm5)$coefficients[2,2],
                                 summary(lm6)$coefficients[2,1]-1.96*summary(lm6)$coefficients[2,2]),
                        names=c("Taxes per\ncapita (1820)", "Tax collection\ndifficult", "Imperial exam\npassers", "Confucian\ntemples", 
                                "Rice\nagriculture", "Linguistic\nfragmentation")
)
estimates$names <- factor(estimates$names, levels = c("Taxes per\ncapita (1820)", "Tax collection\ndifficult", "Imperial exam\npassers", "Confucian\ntemples", 
                                                      "Rice\nagriculture", "Linguistic\nfragmentation"))

theme_bw1 <- function(base_size = 20, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size , colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_line(colour = "grey50"),
      #   axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}


pdf("Figure_2_Placebo_Test.pdf", width=14)
p = ggplot(estimates, aes(y=pe, x=names))
p = p + geom_hline(yintercept = 0,size=1,colour="black",linetype="dotted") 
p = p + geom_pointrange(aes(ymin=se.high,ymax=se.low),position="dodge",size=1.4, col="blue")
#p = p + geom_pointrange(aes(ymin=lower,ymax=upper,width=.6),position="dodge",size=.8)
p = p + scale_y_continuous(name="Estimated Effect",  limits=c(-3, 3)) 
p = p + scale_x_discrete(name="") 
p = p  + theme_bw1()
p
dev.off()



#Clean workspace
rm(list=ls()[which(ls()!="dat")])



