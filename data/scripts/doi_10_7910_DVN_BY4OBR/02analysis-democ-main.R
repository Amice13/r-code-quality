library(data.table)
library(texreg)
library(plm)
library(ggplot2)
library(MASS)
library(sandwich)
library(lmtest)

#############################################
############### Functions ###############
#############################################

theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"),
                    text=element_text(size=14))


make.cl.se <- function(model) {
  ct <-coeftest(model, function(x) vcovHC(x,type="HC",cluster="gwno"))
  se <- ct[, 2]
  pval <- ct[, 4]
  return(list(se, pval))
}

logitlink <- function(x) {1/(1+exp(-x))}

load("analysis-data-bmr.Rdata")
data <- data.bmr

#############################################
################# Judicial corruption
#############################################
## state and disaggregated
s1 <- glm(state.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq
          + factor(year),
          family=binomial(link="logit"), data=data)

s2 <- glm(state.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

s3 <- glm(state.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

u1 <- glm(unknown.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u2 <- glm(unknown.nationalreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u3 <- glm(unknown.localreach.d ~ v2xel_locelec + v2jucorrdc  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

modelnames <- c("I-State", "II-State-national", "III-State-local", 
                "IV-Unconfirmed", "V-Unconf-national", "VI-Unconf-local")

coefnames <- c("Intercept", "Elected local gov", "Judicial Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
               #"FH Media Freedom - Econ", "FH Media Freedom - Legal", 
               "Electoral dem. index", "Electoral dem. index (squ)")

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
       custom.note="%stars. Country-clustered standard errors. Year fixed effects not shown",
       custom.model.name=modelnames, scriptsize=TRUE,
       override.se = model.ses,
       override.pvalues = model.pvals,
       omit.coef = "year",
       file="table1-logit-democracy-locelec-disag-jucorr-bmr.tex", 
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="logit:2")



#############################################
################# Public Sector corruption
#############################################
## state and disaggregated
s1p <- glm(state.d ~ v2xel_locelec + v2x_pubcorr  +
            l.lpop + l.lrgdp + armedconf + l.pts +  #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

s2p <- glm(state.nationalreach.d ~ v2xel_locelec + v2x_pubcorr  +
            l.lpop + l.lrgdp + armedconf + l.pts +  #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

s3p <- glm(state.localreach.d ~ v2xel_locelec + v2x_pubcorr  +
            l.lpop + l.lrgdp + armedconf + l.pts +  #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)


## unknown and disaggregated

u1p <- glm(unknown.d ~ v2xel_locelec + v2x_pubcorr  +
            l.lpop + l.lrgdp + armedconf + l.pts +  #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u2p <- glm(unknown.nationalreach.d ~ v2xel_locelec + v2x_pubcorr  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

u3p <- glm(data$unknown.localreach.d ~ v2xel_locelec + v2x_pubcorr  +
            l.lpop + l.lrgdp + armedconf + l.pts + #fh.C.Economic + fh.A.Legal +
            v2x_polyarchy + v2x_polyarchy.sq + factor(year),
          family=binomial(link="logit"), data=data)

coefnames <- c("Intercept", "Elected local gov", "Public Sector Corr",
               "log Pop (lag)", "log rGDP (lag)","Armed conflict", "PTS (lag)", 
               #"FH Media Freedom - Econ", "FH Media Freedom - Legal", 
               "Electoral dem. index", "Electoral dem. index (squ)")


## save clustered SEs
model.ses <- list(make.cl.se(s1p)[[1]], make.cl.se(s2p)[[1]], 
                  make.cl.se(s3p)[[1]], make.cl.se(u1p)[[1]],
                  make.cl.se(u2p)[[1]], make.cl.se(u3p)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(s1p)[[2]], make.cl.se(s2p)[[2]], 
                    make.cl.se(s3p)[[2]], make.cl.se(u1p)[[2]],
                    make.cl.se(u2p)[[2]], make.cl.se(u3p)[[2]])

model.list.d <- list(s1p,s2p,s3p,u1p,u2p,u3p)


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
       file="logit-democracy-locelec-disag-pubcorr-bmr.tex", 
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+", label="logit:2")



############################################################
################# Local Elections Graph
############################################################

make.locelec.sim <- function(model, modelname){
  
  output <- model
  betas <- output$coefficients
  CV <- vcovHC(model,type="HC",cluster="gwno")
  S <- mvrnorm(10000, betas, CV) 
  
  locelec <- c(min(data$v2xel_locelec, na.rm=T),max(data$v2xel_locelec, na.rm=T))
  year <- c(rep(0, length(unique(data$year))-1))
  evs <- array(data=NA, dim=c(1, 3))
  evs <- as.data.frame(evs)
  names(evs) <- c("lo", "mean", "hi")
  
  X1 <- c(1, locelec[1], 
          mean(data$v2x_pubcorr, na.rm = T),
          mean(data$lpop, na.rm=T), mean(data$lrgdp, na.rm=T),
          median(data$armedconf), median(data$l.pts, na.rm=T),
          mean(data$v2x_polyarchy, na.rm=T), 
          mean(data$v2x_polyarchy, na.rm=T)^2, year)
  ev1 <- logitlink(S %*% X1)
  
  X2 <- c(1, locelec[2], 
          mean(data$v2x_pubcorr, na.rm = T),
          mean(data$lpop, na.rm=T), mean(data$lrgdp, na.rm=T),
          median(data$armedconf), median(data$l.pts, na.rm=T),
          mean(data$v2x_polyarchy, na.rm=T), 
          mean(data$v2x_polyarchy, na.rm=T)^2, year)
  
  ev2 <- logitlink(S %*% X2)
  diff <- ev2-ev1
  evs[,1:3] <- c(quantile(diff, 0.025), mean(diff), quantile(diff, 0.975))
  
  evs$model <- modelname
  return(evs)
}


state <- rbind(make.locelec.sim(s1,  "All journalists"),
               make.locelec.sim(s2,  "Journalists w/\nnational reach"),
               make.locelec.sim(s3,  "Journalists w/\nlocal reach"))
state$perp <- "State"

unconf <- rbind(make.locelec.sim(u1,  "All journalists"),
                make.locelec.sim(u2,  "Journalists w/\nnational reach"),
                make.locelec.sim(u3,  "Journalists w/\nlocal reach"))
unconf$perp <- "Unconfirmed"

plot.data <- rbind(state, unconf)
plot.data$model <- factor(plot.data$model, levels = c("Journalists w/\nnational reach", "Journalists w/\nlocal reach","All journalists"))


plot.loc <- ggplot(data=plot.data, aes(shape=perp)) +
  geom_point(aes(x=model, y=mean),position=position_dodge(width=.3), size=3) +
  geom_errorbar(aes(x=model, ymin=lo, ymax=hi),position=position_dodge(width=.3),
                width=.03,  alpha=.3) +
  geom_hline(yintercept=0, linetype="dotted", size=.2) +
  ylab("Change in probability of journalist killings,\ngiven change in local government") + xlab("") +
  coord_flip()+
  scale_shape_manual("Perpetrator", labels=c("State","Unconfirmed"),
                     values=c(16,17))+
  theme.arg + theme(legend.position = "bottom")
ggsave(plot=plot.loc, file="fig3-change-in-prob-locelec.png", 
       width=5, height=5, dpi = 500)


############################################################
################## Corruption graph
############################################################
make.sim <- function(model, corr.var, modelname, perp){
  
  output <- model
  betas <- output$coefficients
  CV <- vcovHC(model,type="HC",cluster="gwno")
  S <- mvrnorm(10000, betas, CV) 
  year <- c(rep(0, length(unique(data$year))-2), 1)
  
  evs <- array(data=NA, dim=c(1, 3))
  evs <- as.data.frame(evs)
  names(evs) <- c("lo", "mean", "hi")
  
  X1 <- c(1, mean(data$v2xel_locelec, na.rm=T), 
          corr.var[1],
          mean(data$lpop, na.rm=T), mean(data$lrgdp, na.rm=T),
          median(data$armedconf), median(data$l.pts, na.rm=T),
          mean(data$v2x_polyarchy, na.rm=T), 
          mean(data$v2x_polyarchy, na.rm=T)^2, year)
  ev1 <- logitlink(S %*% X1)
  
  X2 <- c(1, mean(data$v2xel_locelec, na.rm=T), 
          corr.var[2],
          mean(data$lpop, na.rm=T), mean(data$lrgdp, na.rm=T),
          median(data$armedconf), median(data$l.pts, na.rm=T),
          mean(data$v2x_polyarchy, na.rm=T), 
          mean(data$v2x_polyarchy, na.rm=T)^2, year)
  
  ev2 <- logitlink(S %*% X2)
  diff <- ev2-ev1
  evs[,1:3] <- c(quantile(diff, 0.025), mean(diff), quantile(diff, 0.975))
  
  evs$model <- modelname
  evs$perp <- perp
  return(evs)
}

ju.corr <- rbind(make.sim(s1, c(min(data$v2jucorrdc), max(data$v2jucorrdc)), "All journalists", "State"),
                  make.sim(s2, c(min(data$v2jucorrdc), max(data$v2jucorrdc)), "Journalists w/\nnational reach", "State"),
                  make.sim(s3, c(min(data$v2jucorrdc), max(data$v2jucorrdc)), "Journalists w/\nlocal reach", "State"),
                  make.sim(u1, c(min(data$v2jucorrdc), max(data$v2jucorrdc)), "All journalists", "Unconfirmed"),
                  make.sim(u2, c(min(data$v2jucorrdc), max(data$v2jucorrdc)), "Journalists w/\nnational reach", "Unconfirmed"),
                  make.sim(u3, c(min(data$v2jucorrdc), max(data$v2jucorrdc)), "Journalists w/\nlocal reach", "Unconfirmed"))
ju.corr$type <- "Judicial Corruption"

pub.corr <- rbind(make.sim(s1p, c(min(data$v2x_pubcorr), max(data$v2x_pubcorr)), "All journalists", "State"),
                  make.sim(s2p, c(min(data$v2x_pubcorr), max(data$v2x_pubcorr)), "Journalists w/\nnational reach", "State"),
                  make.sim(s3p, c(min(data$v2x_pubcorr), max(data$v2x_pubcorr)), "Journalists w/\nlocal reach", "State"),
                  make.sim(u1p, c(min(data$v2x_pubcorr), max(data$v2x_pubcorr)), "All journalists", "Unconfirmed"),
                  make.sim(u2p, c(min(data$v2x_pubcorr), max(data$v2x_pubcorr)), "Journalists w/\nnational reach", "Unconfirmed"),
                  make.sim(u3p, c(min(data$v2x_pubcorr), max(data$v2x_pubcorr)), "Journalists w/\nlocal reach", "Unconfirmed"))
pub.corr$type <- "Public Sector Corruption"

plot.data <- rbind(ju.corr, pub.corr)
plot.data$type <- factor(plot.data$type, levels = c("Judicial Corruption","Public Sector Corruption"))
plot.data$perp <- factor(plot.data$perp, levels = c("State", "Unconfirmed"))
plot.data$model <- factor(plot.data$model, levels = c("Journalists w/\nnational reach", "Journalists w/\nlocal reach","All journalists"))


plot.corr <- ggplot(data=plot.data, aes(shape=perp)) +
  geom_point(aes(x=model, y=mean),position=position_dodge(width=.3), size=2) +
  geom_errorbar(aes(x=model, ymin=lo, ymax=hi),position=position_dodge(width=.3),
                width=.03,  alpha=.3) +
  geom_hline(yintercept=0, linetype="dotted", size=.2) +
  facet_grid(~type, scales = "free")+
  ylab("Change in probability of journalist killings, given increase in corruption") + xlab("") +
  scale_shape_manual("", values = c(16,17))+
  scale_linetype_manual("", values = c(16,17))+
  coord_flip()+
  theme.arg + theme(legend.position = "bottom")
ggsave(plot=plot.corr, file="fig4-change-in-prob-corr.png", 
       width=8, height=5, dpi = 500)


















