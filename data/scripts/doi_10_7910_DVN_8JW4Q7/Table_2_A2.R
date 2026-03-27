
library(plm) # Table 2 & Table A 2 were created using plm version 1.6-6 and stargazer version 5.2
library(stargazer)


# Read data from your directory
dat <- read.csv("C:/PATH/.../data.csv")

dat2 <- pdata.frame(dat, index = c("cod","year"))

est1 <- plm(diflesocsppc~socsppc+as.factor(year)+conc*concmigou+conc*concmigin, dat2, model = "pooling")
cov1 <- as.matrix(sqrt(diag(vcovBK(est1, cluster = "time"))))
rownames(cov1) <- (row.names(est1$vcov))

est2 <- plm(diflesocsppc~socsppc+conc*concmigou+conc*concmigin, dat2, effect = "twoways")
cov2 <- as.matrix(sqrt(diag(vcovBK(est2, cluster = "time"))))
rownames(cov2) <- (row.names(est2$vcov))

est3 <- plm(diflesocsppc~socsppc+as.factor(year)+conc*concmigou+conc*concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag , dat2, model = "pooling")
cov3 <- as.matrix(sqrt(diag(vcovBK(est3, cluster = "time"))))
rownames(cov3) <- (row.names(est3$vcov))

est4 <- plm(diflesocsppc~socsppc+conc*concmigou+conc*concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag , dat2, effect = "twoways")
cov4 <- as.matrix(sqrt(diag(vcovBK(est4, cluster = "time"))))
rownames(cov4) <- (row.names(est4$vcov))

est6 <- plm(lesocsppc~diflesocsppc+as.factor(year)+conc*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag | 
              socsppc+as.factor(year)+conc*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag
            , dat2, model = "pooling")
cov6 <- as.matrix(sqrt(diag(vcovBK(est6, cluster = "time"))))
rownames(cov6) <- (row.names(est6$vcov))

est7 <- plm(diflesocsppc~socsppc+as.factor(year)+factor(concalt)*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag , dat2, model = "pooling")
cov7 <- as.matrix(sqrt(diag(vcovBK(est7, cluster = "time"))))
rownames(cov7) <- (row.names(est7$vcov))

est8 <- plm(diflesocsppc~socsppc+factor(concalt)*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag , dat2, effect = "twoways")
cov8 <- as.matrix(sqrt(diag(vcovBK(est8, cluster = "time"))))
rownames(cov8) <- (row.names(est8$vcov))

est9 <- plm(lesocsppc~diflesocsppc+as.factor(year)+factor(concalt)*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag | 
              socsppc+as.factor(year)+factor(concalt)*concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+sigperpop20_gini*numpar+left+femsuff+PR_dummy+Mandbugref+splag
            , dat2, model = "pooling")
cov9 <- as.matrix(sqrt(diag(vcovBK(est9, cluster = "time"))))
rownames(cov9) <- (row.names(est9$vcov))

stargazer(est1, est2,est3,est4,est6,est7,est8,est9,
          se = list(cov1,cov2,cov3,cov4,cov6,cov7,cov8,cov9), type = "latex", 
          out = "C:/PATH/.../regfin.tex", omit = c("year","diflesocsppc",
          "infantmor","ind_sh","depratio","urban","lnpop","lnPCfedsub","sigperpop20_gini",
          "numpar","left","femsuff","PR_dummy","Mandbugref","splag"), 
          font.size = "scriptsize", dep.var.labels.include = F, 
          dep.var.caption = "$\\Delta$ ln Social Expenditure per Capita",
          no.space = T, keep.stat = c("adj.rsq", "n"), 
          add.lines = list(c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Canton FE","No","Yes","No","Yes","No","No","Yes","No"),
                           c("Controls","No","No","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Long Term Effects","No","No","No","No","Yes","No","No","Yes"),
                           c("Cluster","25","25","25","25","25","25","25","25")),
          notes = c("Panel-Corrected Standard Errors in Parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, order = c(1,52:53,48,67:68), 
          covariate.labels = c("lag Social Exp.","Outward Migration","Inward Migration","Concordat",
                               "Conc*Outward Mig.","Conc*Inward Mig.","Conc. 1923","Conc. 1937",
                               "Conc. 1959","Conc. 1923*Outward Mig.","Conc. 1937*Outward Mig.",
                               "Conc. 1959*Outward Mig."), float.env = "sidewaystable", 
          title = "Concordat Accession, Outward Migration, and Social Expenditure Growth", digits = 4)



## Appendix


stargazer(est1, est2,est3,est4,est6,est7,est8,est9,
          se = list(cov1,cov2,cov3,cov4,cov6,cov7,cov8,cov9), type = "latex", 
          out = "C:/PATH/.../reg.tex", omit = c("year","fdlogPCwelfare"), 
          font.size = "tiny", dep.var.labels.include = F, 
          dep.var.caption = "$\\Delta$ ln Social Expenditure per Capita",
          no.space = T, keep.stat = c("adj.rsq", "n"), 
          add.lines = list(c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Canton FE","No","Yes","No","Yes","No","No","Yes","No"),
                           c("Controls","No","No","Yes","Yes","Yes","Yes","Yes","Yes"),
                           c("Long Term Effects","No","No","No","No","Yes","No","No","Yes"),
                           c("Cluster","25","25","25","25","25","25","25","25")),
          notes = c("Panel-Corrected Standard Errors in Parentheses. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"),
          notes.append = F, order = c(1,52:53,48,67:68,49:51,69:71,54:61,72),
          covariate.labels = c("lag Social Exp.","Outward Migration","Inward Migration","Concordat", "Conc*Outward Mig.","Conc*Inward Mig.","Conc. 1923","Conc. 1937",
                               "Conc. 1959","Conc. 1923*Outward Mig.","Conc. 1937*Outward Mig.","Conc. 1959*Outward Mig.",
                               "Infant Mort.","$\\%$ Industr. Labor Force",
                               "$\\%$ Dependency Ratio","$\\%$ Urbanization","ln Pop. Size","ln Federal Subsidies",
                               "$\\%$ Sig. req. Initiative","Number Parties in Gov.","Init.*Num. Part.",
                               "$\\%$ Left Government","Intro. Female Suffrage","Intro. PR",
                               "Mandatory Budget Referendum","Spatial Lag"),
          title = "Full Model Specification", digits = 4)