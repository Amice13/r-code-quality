

##################################
####APPENDIX TABLES & FIGURES#####
##################################


##
##Descriptive Statistics
##



stargazer(dat[, c("tmh", "tmh.dummy", "tmh.ln",
                  "nationalist.post05", "nationalist.post05.ln",
                  "china_inland_mission_1865", "china.inland.mission.1865.dummy", "china.inland.mission.1865.ln",
                  "conflict_total", "missionary.cases.dummy", "missionary.cases.ln",
                  "quota", "quota.ln", 
                  "books.total", "books.total.ln",  
                  "books.politics", "books.politics.ln",
                  "books.foreign", "books.foreign.ln",
                  "newspapers", "newspapers.ln" ,
                  "size", "size.ln",
                  "railwaydist", "railwaydist.ln",
                  "pop", "pop.ln",
                  "temples",  "temples.pop",
                  "taxpc1820", "rankc_p", "coast", "rice")],
          covariate.labels=c("Tongmenghui Members", "Tongmenghui Members, Indicator", "Tongmenghui Members, Log Standard",
                             "All Nationalist Parties, Indicator", "All Nationalist Parties, Log Standard",
                             "China Inland Mission Numbers", "China Inland Missions, Indicator", "China Inland Missions, Log Standard",
                             "Anti-Missionary Conflict", "Anti-Missionary Conflict, Dummy", "Anti-Missionary Conflict, Log Standard",
                             "Imperial Exam Quota", "Imperial Exam Quota Log Standard", 
                             "Books Published, 1911 to 1920", "Books Published, 1911 to 1920, Log Standard",  
                             "Books About Politics, 1911 to 1920", "Books About Politics, 1911 to 1920, Log Standard",
                             "Books in Foreign Language, 1911 to 1920", "Books by Foreign Authors, 1911 to 1920, Log Standard",
                             "Newspapers in 1905", "Newspapers in 1905, Log Standard" ,
                             "Area of Prefecture", "Area of Prefecture, Log Standard",
                             "Distance to Railway (m)", "Distance to Railway (m), Log Standard",
                             "Population circa  1900", "Population circa  1900, Log Standard",
                             "Confucian Temples",  "Confucian Temples Per Capita",
                             "Tax Per Capita 1820", "Prefecture Difficult to Tax", "Cotal", "Rice Cultivation"),
          out = "Table_A2.tex")




##
##Sensitivity analysis
##

model <- lm(tmh.dummy~missionary.cases.ln+newspapers.ln, data=dat)


sensitivity <- sensemakr(model, treatment = "missionary.cases.ln",
                         benchmark_covariates = c( "newspapers.ln"),
                         kd = c(5, 10, 15, 20), sensitivity.of = c("t-value"))



pdf("Figure_A2_Sensitivity_Analysis.pdf")
plot(sensitivity)
title(ylab=expression(paste("Partial ", R^2, " of confounder(s) with the outcome")), line=2.2, cex.lab=1.4)
title(xlab=expression(paste("Partial ", R^2, " of confounder(s) with the treatment")), line=2.6, cex.lab=1.4)
dev.off()



####################
##Appendix results##
####################



##
##Table A3: Reduced form
##


lm1 <- lm(tmh.dummy~china.inland.mission.1865.ln, data=dat)
lm2 <- lm(tmh.dummy~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- lm(tmh.dummy~china.inland.mission.1865.ln, data=dat)
lm4 <- lm(nationalist.post05~china.inland.mission.1865.ln, data=dat)
lm5 <- lm(nationalist.post05~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- lm(nationalist.post05~china.inland.mission.1865.ln, data=dat)

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
          omit=c("prov"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Mission Size (Log)", "Population (Log)", "Treaty Port", "Area (Log)", "Coastal", "Railway Distance (Log)", "Newspapers (Log)", "Exam Quota (Log)"),
          out = "Table_A3.tex")




##
##Table A4: First stage
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
          covariate.labels = c("Mission Size (Log)", "Population (Log)", "Treaty Port", "Area (Log)", "Coastal", "Railway Distance (Log)", "Newspapers (Log)", "Exam Quota (Log)"),
          out = "Table_A4.tex")




##
##Table A5: 2SLS results
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
          omit=c("prov"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Mission Size (Log)", "Treaty Port",  "Area (Log)", "Coastal", "Population (Log)",   "Railway Distance (Log)", "Newspapers (Log)", "Exam Quota (Log)"),
          out = "Table_A5.tex")





##
##Table A6: Mission size and press publications
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
          dep.var.labels = c("Politics Books (Log)", "Foreign Books (Log)"),
          covariate.labels = c("Mission Size (Log)", "Population (Log)", "Treaty Port", "Area (Log)", "Coastal", "Railway Distance (Log)", "Newspapers (Log)", "Exam Quota (Log)"),
          out = "Table_A6.tex")





##
##Table A7: 2SLS results, alternate measure of explanatory variables
##



lm1 <- ivreg(tmh.dummy~missionary.cases.dummy|china.inland.mission.1865.dummy, data=dat)
lm2 <- ivreg(tmh.dummy~missionary.cases.dummy+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.dummy+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- ivreg(tmh.dummy~missionary.cases.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov)|
               china.inland.mission.1865.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- ivreg(nationalist.post05~missionary.cases.dummy|china.inland.mission.1865.dummy, data=dat)
lm5 <- ivreg(nationalist.post05~missionary.cases.dummy+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.dummy+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- ivreg(nationalist.post05~missionary.cases.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov)|
               china.inland.mission.1865.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)


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
          omit=c("prov"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Mission Size (Dummy)", "Treaty Port",  "Area (Log)", "Coastal", "Population (Log)",   "Railway Distance (Log)", "Newspapers (Log)", "Exam Quota (Log)"),
          out = "Table_A7.tex")




##
##Table A8: 2SLS results, alternate measure of explanatory and outcome variables
##


lm1 <- ivreg(tmh.ln~missionary.cases.dummy|china.inland.mission.1865.dummy, data=dat)
lm2 <- ivreg(tmh.ln~missionary.cases.dummy+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.dummy+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- ivreg(tmh.ln~missionary.cases.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov)|
               china.inland.mission.1865.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- ivreg(nationalist.post05.ln~missionary.cases.dummy|china.inland.mission.1865.dummy, data=dat)
lm5 <- ivreg(nationalist.post05.ln~missionary.cases.dummy+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.dummy+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- ivreg(nationalist.post05.ln~missionary.cases.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov)|
               china.inland.mission.1865.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)


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
          omit=c("prov"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Mission Size (Dummy)", "Treaty Port",  "Area (Log)", "Coastal", "Population (Log)",   "Railway Distance (Log)", "Newspapers (Log)", "Exam Quota (Log)"),
          out = "Table_A8.tex")



##
##Table A9: 2SLS results, alternate measure of outcome variables
##




lm1 <- ivreg(tmh.ln~missionary.cases.ln|china.inland.mission.1865.dummy, data=dat)
lm2 <- ivreg(tmh.ln~missionary.cases.ln+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- ivreg(tmh.ln~missionary.cases.ln+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov)|
               china.inland.mission.1865.ln+treaty+size.ln+coast+pop.ln+railwaydist.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- ivreg(nationalist.post05.ln~missionary.cases.ln|china.inland.mission.1865.dummy, data=dat)
lm5 <- ivreg(nationalist.post05.ln~missionary.cases.ln+treaty+size.ln+coast+as.factor(prov)|china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- ivreg(nationalist.post05.ln~missionary.cases.ln+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov)|
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
          omit=c("prov"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("Mission Size (Dummy)", "Treaty Port",  "Area (Log)", "Coastal", "Population (Log)",   "Railway Distance (Log)", "Newspapers (Log)", "Exam Quota (Log)"),
          out = "Table_A9.tex")





##
##Table A10: All missionaries
##



lm1 <- lm(tmh.dummy~missionary_stauffer, data=dat)
lm2 <- lm(tmh.dummy~missionary_stauffer+pop.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- lm(tmh.dummy~missionary_stauffer+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- lm(nationalist.post05~missionary_stauffer, data=dat)
lm5 <- lm(nationalist.post05~missionary_stauffer+pop.ln+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- lm(nationalist.post05~missionary_stauffer+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)

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
          omit=c("prov"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("All Missionaries (Indicator, Stauffer Yearbook)", "Population", "Treaty port", "Size", "Coastal",
                               "Distance to railway", "Newspapers", "Firm capital", "Imperial exam quota"),
          out = "Table_A10.tex")



##
##Table A11: Converted Christians and Missionary students
##



lm1 <- lm(tmh.dummy~scale(log(I(stauffer_total_christians/stauffer_pop)+1)), data=dat)
lm2 <- lm(tmh.dummy~scale(log(I(stauffer_total_christians/stauffer_pop)+1))+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm3 <- lm(tmh.dummy~scale(log(I(stauffer_congregations/stauffer_pop)+1)), data=dat)
lm4 <- lm(tmh.dummy~scale(log(I(stauffer_congregations/stauffer_pop)+1))+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm5 <- lm(tmh.dummy~scale(log(I(stauffer_total_primary_mission/(stauffer_total_primary_gov+stauffer_total_primary_mission))+1)), data=dat)
lm6 <- lm(tmh.dummy~scale(log(I(stauffer_total_primary_mission/(stauffer_total_primary_gov+stauffer_total_primary_mission))+1))+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)


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
          omit=c("prov"),
          omit.stat=c("f", "ser"),
          dep.var.labels=c("Tongmenghui founder", "Any nationalist org. founder"),
          covariate.labels = c("Christians per capita", "Population", "Treaty port", "Size", "Coastal",
                               "Distance to railway", "Newspapers", "Imperial exam quota", "Conregations per capita", "Mission primary students"),
          out = "Table_A11.tex")




##
##Table A12: Interactions

  


lm1 <- lm(tmh.dummy~missionary.cases.dummy*china.inland.mission.1865.dummy, data=dat)
lm2 <- lm(tmh.dummy~missionary.cases.dummy*china.inland.mission.1865.dummy+treaty+size.ln+coast+as.factor(prov), data=dat)
lm3 <- lm(tmh.dummy~missionary.cases.dummy*china.inland.mission.1865.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm4 <- lm(nationalist.post05~missionary.cases.dummy*china.inland.mission.1865.dummy, data=dat)
lm5 <- lm(nationalist.post05~missionary.cases.dummy*china.inland.mission.1865.dummy+treaty+size.ln+coast+as.factor(prov), data=dat)
lm6 <- lm(nationalist.post05~missionary.cases.dummy*china.inland.mission.1865.dummy+treaty+size.ln+coast+pop.ln+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)

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
          omit=c("prov", "pop_int", "treaty", "size.ln", "coast", "railwaydist.ln", "newspapers.ln", "quota"),
          omit.stat=c("f", "ser"),
          covariate.labels = c("China Inland Mission", "Anti-Missionary Conflict", "Mission X Conflict"),
          out = "Table_A12.tex")




##
##Bounding analysis
##


lm1 <- lm(tmh.dummy~china.inland.mission.1865.ln+missionary.cases.ln, data=dat)
coef_lm1_z <- coefficients(lm1)[3]

dat$lm1_y1 <- dat$tmh.dummy-0.6*coef_lm1_z*log(dat$conflict_total+1)
dat$lm1_y2 <- dat$tmh.dummy-0.8*coef_lm1_z*log(dat$conflict_total+1)
dat$lm1_y3 <- dat$tmh.dummy-0.9*coef_lm1_z*log(dat$conflict_total+1)
dat$lm1_y4 <- dat$tmh.dummy-1*coef_lm1_z*log(dat$conflict_total+1)
dat$lm1_y5 <- dat$tmh.dummy-1.1*coef_lm1_z*log(dat$conflict_total+1)
dat$lm1_y6 <- dat$tmh.dummy-1.2*coef_lm1_z*log(dat$conflict_total+1)
dat$lm1_y7 <- dat$tmh.dummy-1.4*coef_lm1_z*log(dat$conflict_total+1)

lm1_b1 <- lm(lm1_y1~china.inland.mission.1865.ln, data=dat)
lm1_b2 <- lm(lm1_y2~china.inland.mission.1865.ln, data=dat)
lm1_b3 <- lm(lm1_y3~china.inland.mission.1865.ln, data=dat)
lm1_b4 <- lm(lm1_y4~china.inland.mission.1865.ln, data=dat)
lm1_b5 <- lm(lm1_y5~china.inland.mission.1865.ln, data=dat)
lm1_b6 <- lm(lm1_y6~china.inland.mission.1865.ln, data=dat)
lm1_b7 <- lm(lm1_y7~china.inland.mission.1865.ln, data=dat)


lm1_b1        <- coeftest(lm1_b1, function(x) vcovHC(x, type="HC0"))
lm1_b2        <- coeftest(lm1_b2, function(x) vcovHC(x, type="HC0"))
lm1_b3        <- coeftest(lm1_b3, function(x) vcovHC(x, type="HC0"))
lm1_b4        <- coeftest(lm1_b4, function(x) vcovHC(x, type="HC0"))
lm1_b5        <- coeftest(lm1_b5, function(x) vcovHC(x, type="HC0"))
lm1_b6        <- coeftest(lm1_b6, function(x) vcovHC(x, type="HC0"))
lm1_b7        <- coeftest(lm1_b7, function(x) vcovHC(x, type="HC0"))


stargazer(lm1_b1, lm1_b2, lm1_b3, lm1_b4, lm1_b5, lm1_b6, lm1_b7,
          omit=c("prov", "pop_int", "treaty", "size", "coast", "lnrailwaydist", "newspapers.ln", "quota", "Constant"),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("40% below", "20% below", "10% below", "OLS estimate", "10% above", "20% above", "40% above"),
          covariate.labels = c("Mission Size (Log)"),
          out="Table_A13_1.tex")


lm2 <- lm(tmh.dummy~china.inland.mission.1865.ln+missionary.cases.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

coef_lm2_z <- coefficients(lm2)[3]

dat$lm2_y1 <- dat$tmh.dummy-0.6*coef_lm2_z*log(dat$conflict_total+1)
dat$lm2_y2 <- dat$tmh.dummy-0.8*coef_lm2_z*log(dat$conflict_total+1)
dat$lm2_y3 <- dat$tmh.dummy-0.9*coef_lm2_z*log(dat$conflict_total+1)
dat$lm2_y4 <- dat$tmh.dummy-1*coef_lm2_z*log(dat$conflict_total+1)
dat$lm2_y5 <- dat$tmh.dummy-1.1*coef_lm2_z*log(dat$conflict_total+1)
dat$lm2_y6 <- dat$tmh.dummy-1.2*coef_lm2_z*log(dat$conflict_total+1)
dat$lm2_y7 <- dat$tmh.dummy-1.4*coef_lm2_z*log(dat$conflict_total+1)

lm2_b1 <- lm(lm2_y1~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm2_b2 <- lm(lm2_y2~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm2_b3 <- lm(lm2_y3~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm2_b4 <- lm(lm2_y4~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm2_b5 <- lm(lm2_y5~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm2_b6 <- lm(lm2_y6~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm2_b7 <- lm(lm2_y7~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)




lm2_b1        <- coeftest(lm2_b1, function(x) vcovHC(x, type="HC0"))
lm2_b2        <- coeftest(lm2_b2, function(x) vcovHC(x, type="HC0"))
lm2_b3        <- coeftest(lm2_b3, function(x) vcovHC(x, type="HC0"))
lm2_b4        <- coeftest(lm2_b4, function(x) vcovHC(x, type="HC0"))
lm2_b5        <- coeftest(lm2_b5, function(x) vcovHC(x, type="HC0"))
lm2_b6        <- coeftest(lm2_b6, function(x) vcovHC(x, type="HC0"))
lm2_b7        <- coeftest(lm2_b7, function(x) vcovHC(x, type="HC0"))


stargazer(lm2_b1, lm2_b2, lm2_b3, lm2_b4, lm2_b5, lm2_b6, lm2_b7,
          omit=c("prov", "pop_int", "treaty", "size", "coast", "lnrailwaydist", "newspapers.ln", "quota", "Constant"),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("40% below", "20% below", "10% below", "OLS estimate", "10% above", "20% above", "40% above"),
          covariate.labels = c("Mission Size (Log)"),
          out="Table_A13_2.tex")




lm3 <- lm(tmh.dummy~china.inland.mission.1865.ln+missionary.cases.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota+as.factor(prov), data=dat)

coef_lm3 <- coefficients(lm3)
coef_lm3_z <- coef_lm3[3]

dat$lm3_y1 <- dat$tmh.dummy-0.6*coef_lm3_z*log(dat$conflict_total+1)
dat$lm3_y2 <- dat$tmh.dummy-0.8*coef_lm3_z*log(dat$conflict_total+1)
dat$lm3_y3 <- dat$tmh.dummy-0.9*coef_lm3_z*log(dat$conflict_total+1)
dat$lm3_y4 <- dat$tmh.dummy-1*coef_lm3_z*log(dat$conflict_total+1)
dat$lm3_y5 <- dat$tmh.dummy-1.1*coef_lm3_z*log(dat$conflict_total+1)
dat$lm3_y6 <- dat$tmh.dummy-1.2*coef_lm3_z*log(dat$conflict_total+1)
dat$lm3_y7 <- dat$tmh.dummy-1.4*coef_lm3_z*log(dat$conflict_total+1)

lm3_b1 <- lm(lm3_y1~scale(china.inland.mission.1865.ln)+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm3_b2 <- lm(lm3_y2~scale(china.inland.mission.1865.ln)+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm3_b3 <- lm(lm3_y3~scale(china.inland.mission.1865.ln)+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm3_b4 <- lm(lm3_y4~scale(china.inland.mission.1865.ln)+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm3_b5 <- lm(lm3_y5~scale(china.inland.mission.1865.ln)+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm3_b6 <- lm(lm3_y6~scale(china.inland.mission.1865.ln)+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm3_b7 <- lm(lm3_y7~scale(china.inland.mission.1865.ln)+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)


lm3_b1        <- coeftest(lm3_b1, function(x) vcovHC(x, type="HC0"))
lm3_b2        <- coeftest(lm3_b2, function(x) vcovHC(x, type="HC0"))
lm3_b3        <- coeftest(lm3_b3, function(x) vcovHC(x, type="HC0"))
lm3_b4        <- coeftest(lm3_b4, function(x) vcovHC(x, type="HC0"))
lm3_b5        <- coeftest(lm3_b5, function(x) vcovHC(x, type="HC0"))
lm3_b6        <- coeftest(lm3_b6, function(x) vcovHC(x, type="HC0"))
lm3_b7        <- coeftest(lm3_b7, function(x) vcovHC(x, type="HC0"))


stargazer(lm3_b1, lm3_b2, lm3_b3, lm3_b4, lm3_b5, lm3_b6, lm3_b7,
          omit=c("prov", "pop.ln", "treaty", "size", "coast", "railwaydist.ln", "newspapers.ln", "quota", "Constant"),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("40% below", "20% below", "10% below", "OLS estimate", "10% above", "20% above", "40% above"),
          covariate.labels = c("Missionary Size (log)"),
          out="Table_A13_3.tex")





lm4 <- lm(nationalist.post05~china.inland.mission.1865.ln+missionary.cases.ln, data=dat)

coef_lm4 <- coefficients(lm4)
coef_lm4_z <- coef_lm4[3]

dat$lm4_y1 <- dat$nationalist.post05-0.6*coef_lm4_z*log(dat$conflict_total+1)
dat$lm4_y2 <- dat$nationalist.post05-0.8*coef_lm4_z*log(dat$conflict_total+1)
dat$lm4_y3 <- dat$nationalist.post05-0.9*coef_lm4_z*log(dat$conflict_total+1)
dat$lm4_y4 <- dat$nationalist.post05-1*coef_lm4_z*log(dat$conflict_total+1)
dat$lm4_y5 <- dat$nationalist.post05-1.1*coef_lm4_z*log(dat$conflict_total+1)
dat$lm4_y6 <- dat$nationalist.post05-1.2*coef_lm4_z*log(dat$conflict_total+1)
dat$lm4_y7 <- dat$nationalist.post05-1.4*coef_lm4_z*log(dat$conflict_total+1)

lm4_b1 <- lm(lm4_y1~china.inland.mission.1865.ln, data=dat)
lm4_b2 <- lm(lm4_y2~china.inland.mission.1865.ln, data=dat)
lm4_b3 <- lm(lm4_y3~china.inland.mission.1865.ln, data=dat)
lm4_b4 <- lm(lm4_y4~china.inland.mission.1865.ln, data=dat)
lm4_b5 <- lm(lm4_y5~china.inland.mission.1865.ln, data=dat)
lm4_b6 <- lm(lm4_y6~china.inland.mission.1865.ln, data=dat)
lm4_b7 <- lm(lm4_y7~china.inland.mission.1865.ln, data=dat)


lm4_b1        <- coeftest(lm4_b1, function(x) vcovHC(x, type="HC0"))
lm4_b2        <- coeftest(lm4_b2, function(x) vcovHC(x, type="HC0"))
lm4_b3        <- coeftest(lm4_b3, function(x) vcovHC(x, type="HC0"))
lm4_b4        <- coeftest(lm4_b4, function(x) vcovHC(x, type="HC0"))
lm4_b5        <- coeftest(lm4_b5, function(x) vcovHC(x, type="HC0"))
lm4_b6        <- coeftest(lm4_b6, function(x) vcovHC(x, type="HC0"))
lm4_b7        <- coeftest(lm4_b7, function(x) vcovHC(x, type="HC0"))


stargazer(lm4_b1, lm4_b2, lm4_b3, lm4_b4, lm4_b5, lm4_b6, lm4_b7, 
          omit=c("prov", "pop.ln", "treaty", "size", "coast", "railwaydist.ln", "newspapers.ln", "quota", "Constant"),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("40% below", "20% below", "10% below", "OLS estimate", "10% above", "20% above", "40% above"),
          covariate.labels = c("Missionary Size (log)"),
          out="Table_A13_4.tex")



lm5 <- lm(nationalist.post05~china.inland.mission.1865.ln+missionary.cases.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

coef_lm5 <- coefficients(lm5)
coef_lm5_z <- coef_lm5[3]

dat$lm5_y1 <- dat$nationalist.post05-0.6*coef_lm5_z*log(dat$conflict_total+1)
dat$lm5_y2 <- dat$nationalist.post05-0.8*coef_lm5_z*log(dat$conflict_total+1)
dat$lm5_y3 <- dat$nationalist.post05-0.9*coef_lm5_z*log(dat$conflict_total+1)
dat$lm5_y4 <- dat$nationalist.post05-1*coef_lm5_z*log(dat$conflict_total+1)
dat$lm5_y5 <- dat$nationalist.post05-1.1*coef_lm5_z*log(dat$conflict_total+1)
dat$lm5_y6 <- dat$nationalist.post05-1.2*coef_lm5_z*log(dat$conflict_total+1)
dat$lm5_y7 <- dat$nationalist.post05-1.4*coef_lm5_z*log(dat$conflict_total+1)

lm5_b1 <- lm(lm5_y1~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)


lm5_b2 <- lm(lm5_y2~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm5_b3 <- lm(lm5_y3~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)


lm5_b4 <- lm(lm5_y4~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)


lm5_b5 <- lm(lm5_y5~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)

lm5_b6 <- lm(lm5_y6~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)


lm5_b7 <- lm(lm5_y7~china.inland.mission.1865.ln+treaty+size.ln+coast+as.factor(prov), data=dat)




lm5_b1        <- coeftest(lm5_b1, function(x) vcovHC(x, type="HC0"))
lm5_b2        <- coeftest(lm5_b2, function(x) vcovHC(x, type="HC0"))
lm5_b3        <- coeftest(lm5_b3, function(x) vcovHC(x, type="HC0"))
lm5_b4        <- coeftest(lm5_b4, function(x) vcovHC(x, type="HC0"))
lm5_b5        <- coeftest(lm5_b5, function(x) vcovHC(x, type="HC0"))
lm5_b6        <- coeftest(lm5_b6, function(x) vcovHC(x, type="HC0"))
lm5_b7        <- coeftest(lm5_b7, function(x) vcovHC(x, type="HC0"))


stargazer(lm5_b1, lm5_b2, lm5_b3, lm5_b4, lm5_b5, lm5_b6, lm5_b7,
          omit=c("prov", "pop_int", "treaty", "size", "coast", "lnrailwaydist", "newspapers.ln", "quota", "Constant"),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("40% below", "20% below", "10% below", "OLS estimate", "10% above", "20% above", "40% above"),
          covariate.labels = c("Missionary Size (log)"),
          out="Table_A13_5.tex")





lm6 <- lm(nationalist.post05~china.inland.mission.1865.ln+missionary.cases.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)

coef_lm6 <- coefficients(lm6)
coef_lm6_z <- coef_lm6[3]

dat$lm6_y1 <- dat$nationalist.post05-0.6*coef_lm6_z*log(dat$conflict_total+1)
dat$lm6_y2 <- dat$nationalist.post05-0.8*coef_lm6_z*log(dat$conflict_total+1)
dat$lm6_y3 <- dat$nationalist.post05-0.9*coef_lm6_z*log(dat$conflict_total+1)
dat$lm6_y4 <- dat$nationalist.post05-1*coef_lm6_z*log(dat$conflict_total+1)
dat$lm6_y5 <- dat$nationalist.post05-1.1*coef_lm6_z*log(dat$conflict_total+1)
dat$lm6_y6 <- dat$nationalist.post05-1.2*coef_lm6_z*log(dat$conflict_total+1)
dat$lm6_y7 <- dat$nationalist.post05-1.4*coef_lm6_z*log(dat$conflict_total+1)

lm6_b1 <- lm(lm6_y1~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm6_b2 <- lm(lm6_y2~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm6_b3 <- lm(lm6_y3~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm6_b4 <- lm(lm6_y4~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm6_b5 <- lm(lm6_y5~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm6_b6 <- lm(lm6_y6~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)
lm6_b7 <- lm(lm6_y7~china.inland.mission.1865.ln+pop.ln+treaty+size.ln+coast+railwaydist.ln+newspapers.ln+quota.ln+as.factor(prov), data=dat)


lm6_b1        <- coeftest(lm6_b1, function(x) vcovHC(x, type="HC0"))
lm6_b2        <- coeftest(lm6_b2, function(x) vcovHC(x, type="HC0"))
lm6_b3        <- coeftest(lm6_b3, function(x) vcovHC(x, type="HC0"))
lm6_b4        <- coeftest(lm6_b4, function(x) vcovHC(x, type="HC0"))
lm6_b5        <- coeftest(lm6_b5, function(x) vcovHC(x, type="HC0"))
lm6_b6        <- coeftest(lm6_b6, function(x) vcovHC(x, type="HC0"))
lm6_b7        <- coeftest(lm6_b7, function(x) vcovHC(x, type="HC0"))


stargazer(lm6_b1, lm6_b2, lm6_b3, lm6_b4, lm6_b5, lm6_b6, lm6_b7, 
          omit=c("prov", "pop.ln", "treaty", "size", "coast", "railwaydist.ln", "newspapers.ln", "quota", "Constant"),
          omit.stat=c("f", "ser"),
          dep.var.labels = c("40% below", "20% below", "10% below", "OLS estimate", "10% above", "20% above", "40% above"),
          covariate.labels = c("Missionary Size (log)"),
          out="Table_A13_6.tex")



#Clean workspace
rm(list=ls()[which(ls()!="dat")])




