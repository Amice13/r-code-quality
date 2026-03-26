

library(stargazer) # Table 1 was created using stargazer version 5.2

# Read data from your directory
dat <- read.csv("C:/PATH/.../data.csv")

sumdat <- dat[,c("socsppc","conc","concalt","concmigou","concmigin","infantmor","depratio","ind_sh","urban",
                 "lnpop","lnPCfedsub","sigperpop20_gini","Mandbugref","femsuff","PR_dummy","left","numpar","splag")]

stargazer(sumdat, summary = T, title = "Summary Statistics", font.size = "small",
          out = "C:/PATH/.../sumst.tex",
          covariate.labels = c("ln Soc. Exp. p.c.","Concordat","Concordat Reforms","Outward Mig. Conc",
                               "Inward Mig. Conc.","Infant Mortality",
                               "$\\%$ Dependency Ratio","$\\%$ Industrial Labor Force","$\\%$ Urbanization","ln Population Size",
                               "ln Federal Subsidies","$\\%$ Sig. Initiative","Mand. Budget Ref.","Female Suffrage",
                               "PR Legislative","Left Parties","Num. Parties","Spatial Lag"))
