library(nnet)
library(data.table)
library(texreg)
library(ggplot2)
library(MASS)
library(sandwich)
library(lmtest)
make.cl.se <- function(model) {
  ct <-coeftest(model, function(x) vcovHC(x,type="HC",cluster="gwno"))
  se <- ct[, 2]
  pval <- ct[, 4]
  return(list(se, pval))
}
logitlink <- function(x) {1/(1+exp(-x))}

theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"))


load("analysis-data-bmr.Rdata")
data <- data.bmr


#############################################
################# Economic and Legal Media Freedom, without Electoral Democracy Index 
#############################################
## state and disaggregated
s1 <- glm(state.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + fh.A.Legal +
          + factor(year),
          family=binomial(link="logit"), data=data)

s2 <- glm(state.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + fh.A.Legal +
            factor(year),
          family=binomial(link="logit"), data=data)

s3 <- glm(state.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + fh.A.Legal +
            factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

u1 <- glm(unknown.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + fh.A.Legal +
            factor(year),
          family=binomial(link="logit"), data=data)

u2 <- glm(unknown.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + fh.A.Legal +
           factor(year),
          family=binomial(link="logit"), data=data)

u3 <- glm(data$unknown.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + fh.A.Legal +
             factor(year),
          family=binomial(link="logit"), data=data)

modelnames <- c("I-State", "II-State-national", "III-State-local", 
                "IV-Unconfirmed", "V-Unconf-national", "VI-Unconf-local")

coefnames <- c("Intercept", "Elected local gov", "Judicial Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
               "FH Media Freedom - Econ", "FH Media Freedom - Legal")

## save clustered SEs
model.ses <- list(make.cl.se(s1)[[1]], make.cl.se(s2)[[1]], 
                  make.cl.se(s3)[[1]], make.cl.se(u1)[[1]],
                  make.cl.se(u2)[[1]], make.cl.se(u3)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(s1)[[2]], make.cl.se(s2)[[2]], 
                    make.cl.se(s3)[[2]], make.cl.se(u1)[[2]],
                    make.cl.se(u2)[[2]], make.cl.se(u3)[[2]])

model.list.d <- list(s1,s2,s3,u1,u2,u3)


screenreg(model.list.d, custom.model.name=modelnames,
          custom.coef.names=coefnames,
          override.se = model.ses,
          override.pvalues = model.pvals,
          omit.coef = "year")


texreg(model.list.d,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings",
       custom.note="%stars. Logistic regression. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       file="logit-econ-leg-mediarestrictions.tex", 
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="logit:2")



#### media freedom squared terms

s.ec <- glm(state.d ~ v2xel_locelec + v2jucorrdc  +
              l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + I(fh.C.Economic^2)+
              factor(year),
            family=binomial(link="logit"), data=data)
s.pol <- glm(state.d ~ v2xel_locelec + v2jucorrdc  +
               l.lpop + l.lrgdp + armedconf + l.pts + fh.B.Political + I(fh.B.Political^2)+
              factor(year),
             family=binomial(link="logit"), data=data)
s.leg <- glm(state.d ~ v2xel_locelec + v2jucorrdc  +
               l.lpop + l.lrgdp + armedconf + l.pts + fh.A.Legal + I(fh.A.Legal^2)+
               factor(year),
             family=binomial(link="logit"), data=data)



u.ec <- glm(unknown.d ~ v2xel_locelec + v2jucorrdc  +
              l.lpop + l.lrgdp + armedconf + l.pts + fh.C.Economic + I(fh.C.Economic^2)+
               factor(year),
            family=binomial(link="logit"), data=data)
u.pol <- glm(unknown.d ~ v2xel_locelec + v2jucorrdc  +
              l.lpop + l.lrgdp + armedconf + l.pts + fh.B.Political + I(fh.B.Political^2)+
             factor(year),
            family=binomial(link="logit"), data=data)
u.leg <- glm(unknown.d ~ v2xel_locelec + v2jucorrdc  +
              l.lpop + l.lrgdp + armedconf + l.pts + fh.A.Legal + I(fh.A.Legal^2)+
             factor(year),
            family=binomial(link="logit"), data=data)



t.table <- list(s.ec,s.pol,s.leg, u.ec, u.pol, u.leg)
## save clustered SEs
model.ses <- list(make.cl.se(s.ec)[[1]], make.cl.se(s.pol)[[1]], make.cl.se(s.leg)[[1]], 
                  make.cl.se(u.ec)[[1]], make.cl.se(u.pol)[[1]], make.cl.se(u.leg)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(s.ec)[[2]], make.cl.se(s.pol)[[2]], make.cl.se(s.leg)[[2]], 
                    make.cl.se(u.ec)[[2]], make.cl.se(u.pol)[[2]], make.cl.se(u.leg)[[2]])


screenreg(t.table,
          override.se = model.ses,
          override.pvalues = model.pvals, 
          #custom.coef.names=coefnames,
          omit.coef = "year")

modelnames <- c("I-state", "II-state", "III-state", "IV-unconf", "V-unconf", "VI-unconf")

coefnames <- c("Intercept", "Elected loc. gov", "Judicial corruption", "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", "Econ Media Restrictions", "Econ Media Restrictions squ",  "Pol Media Restrictions", "Pol Media Restrictions squ", "Legal Media Restrictions", "Legal Media Restrictions squ")

screenreg(t.table, custom.model.name=modelnames, custom.coef.names=coefnames,
          reorder.coef=c(1,2,3,8,9,10,11,12,13,4,5,6,7),
          override.se = model.ses,
          override.pvalues = model.pvals, 
          omit.coef = "year")

texreg(t.table,
       table = FALSE,
       custom.coef.names=coefnames,
       caption="Determinants of journalist killings by state authorities",
       custom.note="%stars. Logistic regression.Country-clustered standard errors. Year fixed effects not shown",
       reorder.coef=c(1,2,3,8,9,10,11,12,13,4,5,6,7),
       omit.coef = "year",
       custom.model.name=modelnames,
       override.se = model.ses,
       override.pvalues = model.pvals,
       scriptsize=TRUE,
       file="logit-mediarestrictions.tex",
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="logit:media")


make.media.sim <- function(model, var, modelname){
  
  output <- model
  betas <- output$coefficients
  CV <- vcovHC(model,type="HC",cluster="gwno")
  S <- mvrnorm(10000, betas, CV) 
  
  media <- seq(min(var, na.rm=T), max(var, na.rm=T))
  year <- c(rep(0, length(unique(data$year))-2),1)
  evs <- array(data=NA, dim=c(length(media), 4))
  evs <- as.data.frame(evs)
  names(evs) <- c("lo", "mean", "hi", "mediarestriction")
  
  for (i in 1:length(media)) {
    X1 <- c(1, mean(data$v2xel_locelec, na.rm=T), 
            mean(data$v2jucorrdc, na.rm=T),
            mean(data$lpop, na.rm=T), mean(data$lrgdp, na.rm=T),
            median(data$armedconf), median(data$l.pts, na.rm=T),
            media[i], media[i]^2, year)
    ev1 <- logitlink(S %*% X1)
    evs[i,1:3] <- c(quantile(ev1, 0.025), mean(ev1), quantile(ev1, 0.975))
    evs[i, 4] <- media[i]
  }
  evs$model <- modelname
  return(evs)
}


econ <- make.media.sim(s.ec, data$fh.C.Economic, "Economic Media Restrictions")
pol <- make.media.sim(s.pol, data$fh.B.Political, "Political Media Restricitons")
leg <- make.media.sim(s.leg, data$fh.A.Legal, "Legal Media Restrictions")
state <- rbind(econ, pol, leg)
state$perp <- "State"

econ <- make.media.sim(u.ec, data$fh.C.Economic, "Economic Media Restrictions")
pol <- make.media.sim(u.pol, data$fh.B.Political, "Political Media Restricitons")
leg <- make.media.sim(u.leg, data$fh.A.Legal, "Legal Media Restrictions")
unconf <- rbind(econ, pol, leg)
unconf$perp <- "Unconfirmed"

plot.data <- rbind(state, unconf)

plot <-
   ggplot(data=plot.data) +
   geom_point(aes(x=mediarestriction, y=mean),position=position_dodge(width=.2)) +
   geom_errorbar(aes(x=mediarestriction, ymin=lo, ymax=hi),
                 position=position_dodge(width=.2), width=.02,  alpha=.3) +
   geom_hline(yintercept=0, linetype="dotted") +
    ylab("Probability of journalist killing") + xlab("Media Restriction")+
    facet_wrap(perp~model, scales="free")+ 
   theme.arg + theme(axis.text = element_text(size=10,angle=0, vjust=.2))
plot
ggsave(plot=plot, file="figA2-mediarestrictions-pred.pdf", width=9, height=4)

