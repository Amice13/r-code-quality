

library(plm) # Table A 5 was created using plm version 1.6-6, stargazer version 5.2 and car version 2.1-6
library(stargazer)
library(car)


# Read data from your directory
dat <- read.csv("C:/PATH/.../data.csv")

dat$concoutint <- dat$conc*dat$concmigou

dat2 <- pdata.frame(dat, index = c("cod","year"))

estd3 <- plm(diflesocsppc~socsppc+conc+concmigou+concoutint+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag+as.factor(year) , dat2, model = "pooling")

delta1 <- rbind(deltaMethod(estd3, c("conc/socsppc*(-1)"), vcov. = vcovBK(estd3, cluster = "time")),
                deltaMethod(estd3, c("concmigou/socsppc*(-1)"), vcov. = vcovBK(estd3, cluster = "time")),
                deltaMethod(estd3, c("concoutint/socsppc*(-1)"), vcov. = vcovBK(estd3, cluster = "time")),
                deltaMethod(estd3, c("concmigin/socsppc*(-1)"), vcov. = vcovBK(estd3, cluster = "time")))

delcoef <- as.matrix(estd3$coefficients)
delcoef[3:6,1] <- as.matrix(delta1[,1])
delstd <- as.matrix(sqrt(diag(vcovBK(estd3, cluster = "time"))))
delstd[3:6,1] <- as.matrix(delta1[,2])

dat2$conc0 <- ifelse(dat2$concalt ==0,1,0)
dat2$conc1 <- ifelse(dat2$concalt ==1,1,0)
dat2$conc2 <- ifelse(dat2$concalt ==2,1,0)
dat2$conc3 <- ifelse(dat2$concalt ==3,1,0)
dat2$conc4 <- ifelse(dat2$concalt ==4,1,0)

dat2$concoutint2 <- dat2$conc2*dat2$concmigou
dat2$concoutint3 <- dat2$conc3*dat2$concmigou
dat2$concoutint4 <- dat2$conc4*dat2$concmigou

estd7 <- plm(diflesocsppc~socsppc+concmigou+conc2+conc3+conc4+concoutint2+concoutint3+concoutint4+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag+as.factor(year) , dat2, model = "pooling")

delta2 <- rbind(deltaMethod(estd7, c("concmigou/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")),
                deltaMethod(estd7, c("conc2/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")),
                deltaMethod(estd7, c("conc3/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")),
                deltaMethod(estd7, c("conc4/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")),
                deltaMethod(estd7, c("concoutint2/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")),
                deltaMethod(estd7, c("concoutint3/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")),
                deltaMethod(estd7, c("concoutint4/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")),
                deltaMethod(estd7, c("concmigin/socsppc*(-1)"), vcov. = vcovBK(estd7, cluster = "time")))

delcoef2 <- as.matrix(estd7$coefficients)
delcoef2[3:10,1] <- as.matrix(delta2[,1])
delstd2 <- as.matrix(sqrt(diag(vcovBK(estd7, cluster = "time"))))
delstd2[3:10,1] <- as.matrix(delta2[,2])

est6 <- plm(lesocsppc~diflesocsppc+as.factor(year)+conc+concmigou+concoutint+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag | 
              socsppc+as.factor(year)+conc+concmigou+concoutint+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag
            , dat2, model = "pooling")
cov6 <- as.matrix(sqrt(diag(vcovBK(est6, cluster = "time"))))
rownames(cov6) <- (row.names(est6$vcov))

est9 <- plm(lesocsppc~diflesocsppc+as.factor(year)+concmigou+conc2+conc3+conc4+concoutint2+concoutint3+concoutint4+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag | 
              socsppc+as.factor(year)+concmigou+conc2+conc3+conc4+concoutint2+concoutint3+concoutint4+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag
            , dat2, model = "pooling")
cov9 <- as.matrix(sqrt(diag(vcovBK(est9, cluster = "time"))))
rownames(cov9) <- (row.names(est9$vcov))

stargazer(est6, estd3,est9,estd7,coef = list(NULL,delcoef,NULL,delcoef2),
          se = list(cov6,delstd,cov9,delstd2), type = "latex", 
          out = "C:/PATH/.../delta.tex", 
          omit = c("year","diflesocsppc","socsppc","infantmor","ind_sh","depratio","urban","lnpop",
                   "lnPCfedsub","sigperpop20_gini","numpar","left","femsuff","PR_dummy","Mandbugref","splag"), 
          font.size = "footnotesize", dep.var.labels.include = F, 
          dep.var.caption = "$\\Delta$ ln Social Expenditure per Capita",
          no.space = T, keep.stat = c("n"), 
          add.lines = list(c("Year FE","Yes","Yes","Yes","Yes"),
                           c("Canton FE","No","No","No","No"),
                           c("Controls","Yes","Yes","Yes","Yes"),
                           c("Long Term Effects","Yes","Yes","Yes","Yes"),
                           c("Cluster","25","25","25","25","25","25","25","25")),
          notes = c("Panel-Corrected Standard Errors in Parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, title = "Long run effects using IV estimation and delta method", digits = 5,
          covariate.labels = c("Concordat","Outward Migration","Conc*Outward Mig.","Conc. 1923","Conc. 1937",
                               "Conc. 1959","Conc. 1923*Outward Mig.","Conc. 1937*Outward Mig.","Conc. 1959*Outward Mig.",
                               "Inward Migration"))