start_time <- Sys.time()

## Example 1: Card and Krueger study, data from Imai's book
minwage = read.csv("minwage.csv")  
head(minwage)
G       = 1 - (minwage$location == "PA")

## outcome = number of full time employees
Ybefore = minwage$fullBefore + 0.5*minwage$partBefore
Yafter  = minwage$fullAfter + 0.5*minwage$partAfter

## DID
summary(lm(I(Yafter - Ybefore) ~ G))
## LDV
summary(lm(Yafter ~ G + Ybefore))
## nonparametric
pdf("Figure1a.pdf", width = 5, height = 4)
plot(Yafter[G==0] ~ Ybefore[G==0],
     pch = 15, cex = 0.3,
     xlab = expression(Y[t]), ylab = expression(Y[t+1]),
     main = expression(G==0))
abline(lm(Yafter[G==0] ~ Ybefore[G==0]))
fit2 = lm(Yafter ~ Ybefore + I(Ybefore^2), 
          subset = (G==0))$coef
x = seq(0, 80, 0.1)
y = fit2[1] + fit2[2]*x + fit2[3]*x^2
lines(y ~ x)
dev.off()

## DID  based on quadratic regression
mean(Yafter[G==1]) -  
  mean(fit2[1] + fit2[2]*Ybefore[G==1] + fit2[3]*(Ybefore[G==1])^2)

## means of the lag outcomes
t.test(Ybefore ~ G)

ks.test(Ybefore[G == 1], Ybefore[G == 0])

pdf("Figure1b.pdf", width = 5, height = 4)
plot(ecdf(Ybefore[G == 0]), do.points = FALSE,
     xlab = expression(Y[t]),
     ylab = expression(F[Y[t]](y~"|"~G)),
     main = "")
lines(ecdf(Ybefore[G == 1]), do.points = FALSE,
      lwd = 2)
legend("bottomright", c("G=1", "G=0"),
       lty = c(1, 1), lwd = c(1, 2))
dev.off()


## bootstrap
Boot = 10^3
DID  = rep(0, Boot)
LDV  = DID
LDV2 = DID
N    = length(G)
for(boot in 1:Boot)
{
     index    = sample(1:N, replace = TRUE)
     YbeforeB = Ybefore[index]
     YafterB  = Yafter[index]
     GB       = G[index]
     
     DID[boot]  = lm(I(YafterB - YbeforeB) ~ GB)$coef[2]
     LDV[boot]  = lm(YafterB ~ GB + YbeforeB)$coef[2]
     fit2       = lm(YafterB ~ YbeforeB + I(YbeforeB^2), 
                     subset = (GB==0))$coef
     LDV2[boot] = mean(YafterB[GB==1]) -  
                   mean(fit2[1] + fit2[2]*YbeforeB[GB==1] + fit2[3]*(YbeforeB[GB==1])^2)
     
}

quantile(DID - LDV, c(0.025, 0.05, 0.5, 0.95, 0.975))
quantile(DID - LDV2, c(0.025, 0.05, 0.5, 0.95, 0.975))



#######################
## Example 2: M. Bechtel and J. Hainmueller's paper
## How Lasting is Voter Gratitude? 
## An Analysis of the Short- and Long-term Electoral Returns to Beneficial Policy
library(foreign)
d = read.dta("1998_2002.dta")
varnames = c("spd_z_vs","Flooded","PostPeriod", "wkr")
d = na.omit(d[,varnames])
wided = reshape(d, 
                v.names = c("spd_z_vs", "Flooded"),
                timevar = "PostPeriod",
                idvar = "wkr",
                direction = "wide")
wided$Yafter  = wided$spd_z_vs.1
wided$Ybefore = wided$spd_z_vs.0
wided$G       = wided$Flooded.1
wided = wided[,c("Yafter", "Ybefore", "G")]

## DID
summary(lm(I(Yafter - Ybefore) ~ G, data = wided))
## LDV
summary(lm(Yafter ~ G + Ybefore, data = wided))
## nonparametric
pdf("Figure2a.pdf", width = 5, height = 4)
plot(Yafter ~ Ybefore, data = wided, subset = (G==0),
     pch = 15, cex = 0.3,
     xlab = expression(Y[t]), ylab = expression(Y[t+1]),
     main = expression(G==0))
abline(lm(Yafter ~ Ybefore, data = wided, subset = (G==0)))
dev.off()

## pretreatment balance
t.test(Ybefore ~ G, data = wided)

ks.test(wided$Ybefore[wided$G == 1],
        wided$Ybefore[wided$G == 0])
pdf("Figure2b.pdf", width = 5, height = 4)
plot(ecdf(wided$Ybefore[wided$G == 0]),
     do.points = FALSE,
     xlab = expression(Y[t]),
     ylab = expression(F[Y[t]](y~"|"~G)),
     main = "")
lines(ecdf(wided$Ybefore[wided$G == 1]),
      do.points = FALSE, lwd = 2)
legend("bottomright", c("G=1", "G=0"),
       lty = c(1, 1), lwd = c(1, 2))
dev.off()


## bootstrap
Boot = 10^3
DID  = rep(0, Boot)
LDV  = DID
N    = length(G)
for(boot in 1:Boot)
{
  index    = sample(1:N, replace = TRUE)
  YbeforeB = wided$Ybefore[index]
  YafterB  = wided$Yafter[index]
  GB       = wided$G[index]
  
  DID[boot]  = lm(I(YafterB - YbeforeB) ~ GB)$coef[2]
  LDV[boot]  = lm(YafterB ~ GB + YbeforeB)$coef[2]
}

quantile(DID - LDV, c(0.025, 0.05, 0.5, 0.95, 0.975))



##############################
## Example 3: Traffic safety evaluation
# load the PennCrashBK.csv (full data: 1986 sites with covariates and count outcomes) 

penncrach  = read.csv("PennCrashBK.csv", header=TRUE)
head(penncrach)

penncrach$tot08t = ifelse(penncrach$tot08 < 3, penncrach$tot08, 3)
penncrach$tot12t = ifelse(penncrach$tot12 < 3, penncrach$tot12, 3)

penncrach1 = penncrach[penncrach$Treat==1, ]
penncrach0 = penncrach[penncrach$Treat==0, ]

tb1 = table(penncrach1$tot08t, penncrach1$tot12t)
tb0 = table(penncrach0$tot08t, penncrach0$tot12t)
tb1.rowsum = apply(tb1, 1, sum)
tb0.rowsum = apply(tb0, 1, sum)
tb1 = cbind(tb1, tb1.rowsum)
tb0 = cbind(tb0, tb0.rowsum)
tb1.colsum = apply(tb1, 2, sum)
tb0.colsum = apply(tb0, 2, sum)
tb1 = rbind(tb1, tb1.colsum)
tb0 = rbind(tb0, tb0.colsum)

library("xtable")
print(xtable(tb0), 
      file = "Table1a.tex")
print(xtable(tb1), 
      file = "Table1b.tex")


232/331
1102/1655
232/331 - 1102/1655


## binary outcome
Ybefore = penncrach$bintot08
Yafter  = penncrach$bintot12
G       = penncrach$Treat
## DID for mu0
mean(Ybefore[G==1]) + mean(Yafter[G==0]) - mean(Ybefore[G==0])
## LDV for mu0
mean(Yafter[G==0&Ybefore==0])*mean(1-Ybefore[G==1]) + 
  mean(Yafter[G==0&Ybefore==1])*mean(Ybefore[G==1])

## bootstrap
Boot = 10^3
DID  = rep(0, Boot)
LDV  = DID
N    = length(G)
for(boot in 1:Boot)
{
  index    = sample(1:N, replace = TRUE)
  YbeforeB = Ybefore[index]
  YafterB  = Yafter[index]
  GB       = G[index]
  
  DID[boot]  = mean(YbeforeB[GB==1]) + mean(YafterB[GB==0]) - 
                 mean(YbeforeB[GB==0])
  LDV[boot]  = mean(YafterB[GB==0&YbeforeB==0])*mean(1-YbeforeB[GB==1]) + 
                 mean(YafterB[GB==0&YbeforeB==1])*mean(YbeforeB[GB==1])
  
}

quantile(DID - LDV, c(0.025, 0.05, 0.5, 0.95, 0.975))





## count outcome
Ybefore = penncrach$tot08t
Yafter  = penncrach$tot12t
G       = penncrach$Treat
## DID for mu0
mean(Ybefore[G==1]) + mean(Yafter[G==0]) - mean(Ybefore[G==0])
## LDV for mu0
mean(Yafter[G==0&Ybefore==0])*mean(Ybefore[G==1]==0) + 
  mean(Yafter[G==0&Ybefore==1])*mean(Ybefore[G==1]==1) + 
   mean(Yafter[G==0&Ybefore==2])*mean(Ybefore[G==1]==2) +
     mean(Yafter[G==0&Ybefore==3])*mean(Ybefore[G==1]==3) 
## check stochastic dominance
ecdf(Ybefore[G==1])(0:3)
ecdf(Ybefore[G==0])(0:3)
  
## bootstrap
Boot = 10^3
DID  = rep(0, Boot)
LDV  = DID
N    = length(G)
for(boot in 1:Boot)
{
  index    = sample(1:N, replace = TRUE)
  YbeforeB = Ybefore[index]
  YafterB  = Yafter[index]
  GB       = G[index]
  
  DID[boot]  = mean(YbeforeB[GB==1]) + mean(YafterB[GB==0]) - 
               mean(YbeforeB[GB==0])
  LDV[boot]  = mean(YafterB[GB==0&YbeforeB==0])*mean(YbeforeB[GB==1]==0) + 
               mean(YafterB[GB==0&YbeforeB==1])*mean(YbeforeB[GB==1]==1) + 
               mean(YafterB[GB==0&YbeforeB==2])*mean(YbeforeB[GB==1]==2) +
               mean(YafterB[GB==0&YbeforeB==3])*mean(YbeforeB[GB==1]==3) 
  
}

quantile(DID - LDV, c(0.025, 0.05, 0.5, 0.95, 0.975))


end_time <- Sys.time()

end_time - start_time

