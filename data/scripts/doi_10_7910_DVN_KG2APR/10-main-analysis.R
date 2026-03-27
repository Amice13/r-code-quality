library(ggplot2)
library(data.table)
library(MASS)
library(sandwich)
library(lmtest)
library(scales)
library(texreg)

theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"))
label.size <- 12

load("analysis-data.Rdata")

data$date2w <- factor(data$date2w)

make.cl.se <- function(model) {
   ct <-coeftest(model, function(x) vcovHC(x,type="HC",cluster="gov"))
   se <- ct[, 2]
   pval <- ct[, 4]
   return(list(se, pval))
}


make.main.analysis.tables <- function(data, ivar, add.models=FALSE){
   # base model
   data$i.var <- data[, ivar]
   
   fm.1<- glm(cbind(targeted, total-targeted) ~ 
                   i.var*control, family=binomial(logit),
                data=data)
   summary(fm.1)
   
   ## control interaction
   fm.2<- glm(cbind(targeted, total-targeted) ~ 
                   i.var*control + date2w, family=binomial(logit),
                data=data)
   summary(fm.2)
   
   
   ## gov change
   fm.3<- glm(cbind(targeted, total-targeted) ~ 
                   i.var*control + total.log + date2w, family=binomial(logit),
                data=data)
   summary(fm.3)
   
   ## territorial control + gov change * internet
   
   fm.4<- glm(cbind(targeted, total-targeted) ~ 
                   i.var*control + date2w + total.log + Government.diff, family=binomial(logit),
                data=data)
   summary(fm.4)

   ## ethnic goups, interaction between Alawi and X3G

   fm.5<- glm(cbind(targeted, total-targeted) ~ 
                   i.var*Alawi + i.var*control + date2w + total.log +
                   Alawi + Druze + Kurds + Christians, family=binomial(logit),
                data=data)
   summary(fm.5)
   
   
   ## pop (2011) and unemployment (2011)
   fm.6<- glm(cbind(targeted, total-targeted) ~ 
                   i.var* control + date2w +  total.log +
                   Alawi + Druze + Kurds + Christians +
                   pop11.log  + unemp_perc_2011,
                family=binomial(logit),
                data=data)
   summary(fm.6)
   
   ## percentage of government control
   fm.7<- glm(cbind(targeted, total-targeted) ~ 
                   i.var*perc.Government + control + date2w + total.log +
                   Alawi + Druze + Kurds + Christians +
                   pop11.log + unemp_perc_2011,
                family=binomial(logit),
                data=data)
   summary(fm.7)

   
   ## save clustered SEs
   model.ses <- list(make.cl.se(fm.1)[[1]], make.cl.se(fm.2)[[1]],make.cl.se(fm.3)[[1]],make.cl.se(fm.4)[[1]],
                     make.cl.se(fm.5)[[1]], make.cl.se(fm.6)[[1]], make.cl.se(fm.7)[[1]])
   ## save clustered pvalues
   model.pvals <- list(make.cl.se(fm.1)[[2]], make.cl.se(fm.2)[[2]],make.cl.se(fm.3)[[2]], make.cl.se(fm.4)[[2]],
                       make.cl.se(fm.5)[[2]], make.cl.se(fm.6)[[2]], make.cl.se(fm.7)[[2]])
   ## save model list for table
   model.list <-list(fm.1, fm.2, fm.3, fm.4, fm.5, fm.6, fm.7)

   if (add.models==TRUE) {
      table.info <- list(model.list = model.list, model.ses = model.ses, model.pvals = model.pvals)
   }

   if (add.models==FALSE) {
      table.info <- list(model.list = model.list[1:4], model.ses = model.ses[1:4], model.pvals = model.pvals[1:4])
   }
   
   return(table.info)
}

table1.3G.d <- make.main.analysis.tables(data, "X3G.d")
table1.2G <- make.main.analysis.tables(data, "X2G.GPRS")
table1.Mobile <- make.main.analysis.tables(data, "Mobile.Phones")
model.names =c("I", "II", "III", "IV")


#######################################################################
##### Table A5 ######
#######################################################################
# 3G binary
coefnames <- c("Intercept", "Internet access (3G binary)", "Govt control",
               "IS control", "Kurd control", "Opp control", 
               "Internet (3G binary) * Govt control","Internet (3G binary) * IS control",
               "Internet (3G binary) * Kurd control", "Internet (3G binary) * Opp control",               
               "\\# Killings (log)",
               "Govt gains", "Govt losses")

texreg(table1.3G.d$model.list, file="TableA5.tex", 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names, 
       custom.coef.names = coefnames,
       omit.coef="date2w", override.se = table1.3G.d$model.ses,
       override.pvalues = table1.3G.d$model.pvals,
       digits=3, custom.note="%stars. Reference category: Contested control. Governorate-clustered SEs.",
       symbol = "+", label="3gd:base", scriptsize=TRUE)


screenreg(table1.3G.d$model.list, 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names, 
       custom.coef.names = coefnames,
       omit.coef="date2w", override.se = table1.3G.d$model.ses,
       override.pvalues = table1.3G.d$model.pvals,
       digits=3)

#######################################################################
##### Table A3 ######
#######################################################################
# 2G
coefnames <- c("Intercept", "Internet access (2G)", "Govt control",
               "IS control", "Kurd control", "Opp control", 
               "Internet (2G) * Govt control", "Internet (2G) * IS control",
               "Internet (2G) * Kurd control", "Internet (2G) * Opp control", "\\# Killings (log)",
               "Govt gains", "Govt losses")
texreg(table1.2G$model.list, file="TableA3.tex", 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names, 
       custom.coef.names = coefnames,
       omit.coef="date2w", override.se = table1.2G$model.ses,
       override.pvalues = table1.2G$model.pvals,
       digits=3, custom.note="%stars. Reference category: Contested control. Governorate-clustered SEs.",
       symbol = "+", label="2g:base", scriptsize=TRUE)


#######################################################################
##### Table A4 ######
#######################################################################
## Mobile Phones
coefnames <- c("Intercept", "Mobile Phones", "Govt control",
               "IS control", "Kurd control", "Opp control", 
               "Mobile Phones * Govt control", "Mobile Phones * IS control",
               "Mobile Phones * Kurd control", "Mobile Phones * Opp control", "\\# Killings (log)",
               "Govt gains", "Govt losses")
texreg(table1.Mobile$model.list, file="TableA4.tex", 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names, 
       custom.coef.names = coefnames,
       omit.coef="date2w", override.se = table1.Mobile$model.ses,
       override.pvalues = table1.Mobile$model.pvals,
       digits=3, custom.note="%stars. Reference category: Contested control. Governorate-clustered SEs.",
       symbol = "+", label="mobile:base", scriptsize=TRUE)


#######################################################################
##### Table 1 ######
#######################################################################
# 3G
table1.3G <- make.main.analysis.tables(data, "X3G", add.models=TRUE)
model.names =c("I", "II", "III", "IV", "V", "VI", "VII")

coefnames <- c("Intercept", "Internet access (3G)", "Govt control", "IS control",
               "Kurd control", "Opp control",
               "Internet (3G) * Govt control","Internet (3G) * IS control",
               "Internet (3G) * Kurd control", "Internet (3G) * Opp. control",
               "\\# Killings (log)", "Govt gains", "Govt losses",
               "Alawi",  "Druze", "Kurd", "Christian",
               "Internet (3G) * Alawi",
               "Pop (log)", "Unempl. (%)",
               "% Govt control", "Internet (3G) * % Govt control")

screenreg(table1.3G$model.list,
          custom.model.names=model.names,
          custom.coef.names=coefnames,
          omit.coef="date2w",
          override.se = table1.3G$model.ses,
          override.pvalues = table1.3G$model.pvals,
          digits=3, custom.note="%stars. Reference category: Contested control. Governorate-clustered SEs.",
          symbol = "+", label="3g:base", scriptsize=TRUE,
          reorder.coef=c(1,2,21,22,3,4,5,6,7,8,9,10,11,12,13,17,14,15,16,18,19,20))

texreg(table1.3G$model.list, file="Table1.tex", 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names, 
       custom.coef.names = coefnames,
       omit.coef="date2w",
       reorder.coef=c(1,2,21,22,3,4,5,6,7,8,9,10,11,12,13,17,14,15,16,18,19,20),
       override.se = table1.3G$model.ses,
       override.pvalues = table1.3G$model.pvals,
       digits=3, custom.note="%stars. Reference category: Contested control. Governorate-clustered SEs.",
       symbol = "+", label="3g:base", scriptsize=TRUE)


#######################################################################
##### Table A2 ######
#######################################################################
### lagged models, using different measures of internet access
table1.l.3G <- make.main.analysis.tables(data, "l.X3G", add.models=TRUE)
table1.l.2G <- make.main.analysis.tables(data, "l.X2G", add.models=TRUE)
table1.l.MP <- make.main.analysis.tables(data, "l.Mobile.Phones", add.models=TRUE)

model.list <-  list(table1.l.3G$model.list[[6]], table1.l.3G$model.list[[7]],
                    table1.l.2G$model.list[[6]], table1.l.2G$model.list[[7]],
                    table1.l.MP$model.list[[6]], table1.l.MP$model.list[[7]])

model.ses <- list(table1.l.3G$model.ses[[6]], table1.l.3G$model.ses[[7]],
                    table1.l.2G$model.ses[[6]], table1.l.2G$model.ses[[7]],
                    table1.l.MP$model.ses[[6]], table1.l.MP$model.ses[[7]])

model.pvals <- list(table1.l.3G$model.pvals[[6]], table1.l.3G$model.pvals[[7]],
                    table1.l.2G$model.pvals[[6]], table1.l.2G$model.pvals[[7]],
                    table1.l.MP$model.pvals[[6]], table1.l.MP$model.pvals[[7]])

model.names =c("lagged 3G-I", "lagged 3G-II", "lagged 2G-I", "lagged 2G-II", "lagged Mob. Phone-I", "lagged Mob. Phone-II")

screenreg(model.list, custom.model.names=model.names, omit.coef="date2w")

coefnames <- c("Intercept", "lag Internet/Mobile Phone access",
               "Govt control", "IS control",
               "Kurd control", "Opp control",
                 "\\# Killings (log)",
                "Alawi", "Druze", "Kurd", "Christian", 
               "Pop (log)", "Unempl. (%)",
               "lag Internet/Mobile Phone * Govt control",
               "lag Internet/Mobile Phone * IS control",
               "lag Internet/Mobile Phone * Kurd control",
               "lag Internet/Mobile Phone * Opp. control",
               "% Govt control", "Internet/Mobile Phone * % Govt control")

screenreg(model.list,
          custom.model.names=model.names,
          custom.coef.names=coefnames,
          omit.coef="date2w",
          override.se = model.ses,
          override.pvalues = model.pvals,
          reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))

texreg(model.list, file="TableA2.tex", 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names, 
       custom.coef.names = coefnames,
       omit.coef="date2w",
       reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19,17,18),
       override.se = model.ses,
       override.pvalues = model.pvals,
       digits=3, custom.note="%stars. Reference category: Contested control. Governorate-clustered SEs.",
       symbol = "+", label="lagged:base", scriptsize=TRUE)



#### Simulations for Figures
## use models 5,6,7

fm.5<- glm(cbind(targeted, total-targeted) ~ 
                   X3G*Alawi + X3G*control + date2w + total.log +
                   Alawi + Druze + Kurds + Christians, family=binomial(logit),
                data=data)
summary(fm.5)


## pop (2011) and unemployment (2011)
fm.6<- glm(cbind(targeted, total-targeted) ~ 
              X3G* control + date2w +  total.log +
              Alawi + Druze + Kurds + Christians +
              pop11.log  + unemp_perc_2011,
                family=binomial(logit),
           data=data)
summary(fm.6)

## percentage of government control
fm.7<- glm(cbind(targeted, total-targeted) ~ 
              X3G*perc.Government + control + date2w + total.log +
              Alawi + Druze + Kurds + Christians +
              pop11.log + unemp_perc_2011,
           family=binomial(logit),
           data=data)
summary(fm.7)

make.evs <- function(model, gov.control=0, sim.name, access,
                     perc.model=FALSE, gov.perc=0, alawi.model=FALSE, alawi=0) {
   num.sims <- length(access)
   betas <- na.omit(coef(model))
   X <- as.data.frame(matrix(data=NA, nrow=length(betas), ncol=num.sims+1))
   X[,num.sims+1] <- names(betas)
   X[,num.sims+1] <- gsub("control70", "control", X[,num.sims+1])
   X[,num.sims+1] <- gsub("control80", "control", X[,num.sims+1])
   X[grep("date2w", names(betas)),1:num.sims] <- 0

   names(X)[num.sims+1] <- "vars"
   X[X$vars=="(Intercept)",1:num.sims] <- 1
   X[X$vars=="total.log", 1:num.sims] <- mean(data$total.log)
   X[X$vars=="Druze",1:num.sims] <- 0
   X[X$vars=="Kurds",1:num.sims] <- 0
   X[X$vars=="Christians",1:num.sims] <- 0
   X[X$vars=="controlIslamic State", 1:num.sims] <- 0
   X[X$vars=="controlKurds",1:num.sims] <- 0
   X[X$vars=="controlOpposition",1:num.sims] <- 0


   if (perc.model == TRUE) {
      
      X[X$vars=="X3G", 1:num.sims] <- access   
      X[X$vars=="perc.Government",1:num.sims] <- gov.perc
      X[X$vars=="X3G:perc.Government",1:num.sims] <- X[X$vars=="X3G", 1:num.sims] *
         X[X$vars=="perc.Government",1:num.sims]
      X[X$vars=="controlGovernment",1:num.sims] <- gov.control
      X[X$vars=="Alawi",1:num.sims] <- alawi
      X[X$vars=="pop11.log",1:num.sims] <- mean(data$pop11.log)
      X[X$vars=="unemp_perc_2011",1:num.sims] <- mean(data$unemp_perc_2011)
   
   }

   if(alawi.model==TRUE) {

      X[X$vars=="X3G", 1:num.sims] <- access   
      X[X$vars=="Alawi",1:num.sims] <- alawi
      X[X$vars=="X3G:Alawi",1:num.sims] <- X[X$vars=="X3G", 1:num.sims] *
         X[X$vars=="Alawi",1:num.sims]
      X[X$vars=="controlGovernment",1:num.sims] <- gov.control

      X[X$vars=="X3G:controlGovernment",1:num.sims] <- X[X$vars=="X3G",1:num.sims] *
         X[X$vars=="controlGovernment", 1:num.sims]

      X[X$vars=="X3G:controlIslamic State",1:num.sims] <- X[X$vars=="X3G",1:num.sims] *
         X[X$vars=="controlIslamic State", 1:num.sims]
      
      X[X$vars=="X3G:controlKurds",1:num.sims] <- X[X$vars=="X3G",1:num.sims] *
         X[X$vars=="controlKurds", 1:num.sims]
      
      X[X$vars=="X3G:controlOpposition",1:num.sims] <-
         X[X$vars=="X3G",1:num.sims] * X[X$vars=="controlOpposition", 1:num.sims]
      

   }

   ## use VCOV / cluster standard errors by governorate
   
   S <- mvrnorm(1000, betas,vcovHC(model,type="HC",cluster="gov"))
   y.hat <- S %*% as.matrix(X[,1:num.sims])
   pred <- 1/(1 + exp(-y.hat))
   pred <- as.data.frame(pred)
   
   evs <- array(data=NA, dim=c(num.sims, 5))
   evs <- as.data.frame(evs)
   names(evs) <- c("lo95", "lo83", "mean", "up83", "up95")
   evs$access <- access
   
   evs$lo95 <- apply(pred, 2, function(x) quantile(x, 0.025))
   evs$lo83 <- apply(pred, 2, function(x) quantile(x, 0.085))
   evs$mean <- apply(pred, 2, function(x) mean(x))
   evs$up83 <- apply(pred, 2, function(x) quantile(x, 0.915))
   evs$up95 <- apply(pred, 2, function(x) quantile(x, 0.975))
   evs$sim.name <- sim.name
   
   return(evs)
}

#######################################################################
##### Figure 4 ######
#######################################################################

## model with perc. of government control
# access 
num.sims <- 100
access <- seq(1, 4, length.out = num.sims)

plot.data <- rbind(make.evs(model=fm.7, gov.perc=20, gov.control=0,
                            sim.name="20% government\ncontrol", access,
                            perc.model=TRUE),
                   make.evs(model=fm.7, gov.perc=40, gov.control=0,
                            sim.name="40% government\ncontrol", access,
                            perc.model=TRUE),
                   make.evs(model=fm.7, gov.perc=60, gov.control=1,
                            sim.name="60% government\ncontrol", access,
                            perc.model=TRUE))

plot <- ggplot(plot.data,
               aes(x=access, y =mean)) +
   geom_line()+
   geom_ribbon(aes(ymax = up95, ymin= lo95), alpha=0.1)+
   geom_ribbon(aes(ymax = up83, ymin= lo83), alpha=0.2)+
   facet_wrap(~sim.name, nrow=1)+
   labs(x = "Internet access", y = "Expected proportion (%) of targeted killings")+ 
   scale_x_continuous(breaks = c(1, 4), label = c("None", "Full")) + 
   theme.arg  +theme(legend.position="none",
                     axis.title=element_text(size=label.size),
                     strip.text.x = element_text(size = 14))
   plot
ggsave(plot=plot, file="Figure4_expected_proportion_by_govcontrol.pdf",
       width=8, height=4)

#######################################################################
##### Figure 3 ######
#######################################################################

## Alawi effect
num.sims <- 2
access <- seq(1, 4, length.out = num.sims)
plot.data.alawi <- rbind(make.evs(model=fm.5, gov.control=0, "Alawi region", access,
                                  gov.perc=0, alawi.model=TRUE, alawi=1),
                         make.evs(model=fm.5, gov.control=0, "other regions", access,
                                  gov.perc=0, alawi.model=TRUE, alawi=0))

plot <- ggplot(plot.data.alawi,
               aes(x=access, y =mean, shape=sim.name)) +
   geom_errorbar(aes(ymax = up95, ymin= lo95, linetype=sim.name),alpha=0.7, position=position_dodge(width=1), width=.1)+
   geom_point(position=position_dodge(width=1), size=3)+
   scale_shape_manual("", values=c(16, 17))+
   scale_linetype_manual("", values=c(1, 2))+
   labs(x = "Internet access", y = "Expected proportion (%) of targeted killings")+
   scale_x_continuous(breaks = c(1, 4), label = c("No Access", "Full Access")) +
   theme.arg + theme(legend.position="bottom",
                     legend.text=element_text(size = label.size),
                     axis.text = element_text(size = label.size),
                     axis.title=element_text(size = label.size))
plot
ggsave(plot=plot, file="Figure3_expected_proportion_Alawi.pdf",
       width=4, height=5)




#######################################################################
##### Figure A3 ######
#######################################################################

### simulate relative effect sizes
make.diff.effect.sizes <- function(model, sim.name, gov.control=0, opp.control=0,
                                   access=median(data$X3G,na.rm=T),
                                   kill.log=mean(data$total.log),
                                   Alawi=0,Druze=0, Kurds=0, Christians=0,
                                   pop=mean(data$pop11.log),
                                   unemp=mean(data$unemp_perc_2011)) {
   num.sims <- 2
   betas <- na.omit(coef(model))
   X <- as.data.frame(matrix(data=NA, nrow=length(betas), ncol=num.sims+1))
   X[,num.sims+1] <- names(betas)
   X[grep("date2w", names(betas)),1:num.sims] <-0
   
   X[X$V3=="(Intercept)",1:num.sims] <- 1
   X[X$V3=="total.log", 1:num.sims] <- kill.log
   X[X$V3=="X3G", 1:num.sims] <- access   
   X[X$V3=="controlGovernment",1:num.sims] <- gov.control
   X[X$V3=="controlIslamic State", 1:num.sims] <- 0
   X[X$V3=="controlKurds",1:num.sims] <- 0
   X[X$V3=="controlOpposition",1:num.sims] <- opp.control
   
   X[X$V3=="X3G:controlGovernment",1:num.sims] <- X[X$V3=="X3G",1:num.sims] *
      X[X$V3=="controlGovernment", 1:num.sims]

   X[X$V3=="X3G:controlIslamic State",1:num.sims] <- X[X$V3=="X3G",1:num.sims] *
      X[X$V3=="controlIslamic State", 1:num.sims]

   X[X$V3=="X3G:controlKurds",1:num.sims] <- X[X$V3=="X3G",1:num.sims] *
      X[X$V3=="controlKurds", 1:num.sims]
   
   X[X$V3=="X3G:controlOpposition",1:num.sims] <-
      X[X$V3=="X3G",1:num.sims] * X[X$V3=="controlOpposition", 1:num.sims]
   
   X[X$V3=="Alawi",1:num.sims] <- Alawi
   X[X$V3=="Druze",1:num.sims] <- Druze
   X[X$V3=="Kurds",1:num.sims] <- Kurds
   X[X$V3=="Christians",1:num.sims] <- Christians
   X[X$V3=="pop11.log",1:num.sims] <- pop
   X[X$V3=="unemp_perc_2011",1:num.sims] <- unemp
   
   S <- mvrnorm(1000, betas,vcovHC(model,type="HC",cluster="gov"))
   y.hat <- S %*% as.matrix(X[,1:num.sims])
   pred <- 1/(1 + exp(-y.hat))
   pred <- as.data.frame(pred)
   fd <- pred[,2]-pred[,1]
   
   evs <- array(data=NA, dim=c(1, 3))
   evs <- as.data.frame(evs)
   names(evs) <- c("low", "mean", "up")
   
   evs$low <- quantile(fd, 0.025)
   evs$mean <- mean(fd)
   evs$up <- quantile(fd, 0.975)
   evs$sim.name <- sim.name
   return(evs)
}   

internet <- make.diff.effect.sizes(fm.6, "Internet access\n(none to full)", access=c(1,4))
govt.control <- make.diff.effect.sizes(fm.6, "Government control\n(contested to gov control)",gov.control=c(0,1))
alawi <- make.diff.effect.sizes(fm.6, "Alawi region", Alawi=c(0,1))
kurds <- make.diff.effect.sizes(fm.6, "Kurdish region", Kurds=c(0,1))
pop <- make.diff.effect.sizes(fm.6, "Population\n(min to max)",
                       pop=c(min(data$pop11.log),max(data$pop11.log)))
unemp <- make.diff.effect.sizes(fm.6, "Unemployment\n(min to max)",
                       unemp=c(min(data$unemp_perc_2011),max(data$unemp_perc_2011)))
plot.data <- rbind(internet, govt.control, alawi, kurds, pop, unemp)
plot.data <- plot.data[order(plot.data$mean),]
plot.data$mean <- plot.data$mean*100
plot.data$low <- plot.data$low*100
plot.data$up <- plot.data$up*100

plot.data$sim.name <- factor(plot.data$sim.name,
                             levels=c("Population\n(min to max)",
                                      "Government control\n(contested to gov control)",
                                      "Unemployment\n(min to max)", "Kurdish region",
                                      "Alawi region",
                                      "Internet access\n(none to full)"))
plot.data$sim.col <- ifelse(plot.data$sim.name=="Internet access\n(none to full)", 1,0)

plot <- ggplot(plot.data, aes(x=sim.name, y=mean)) +
   geom_hline(yintercept=0, linetype="dotted", color="grey")+
   geom_errorbar(width=.1, aes(ymin=low, ymax=up, color=factor(sim.col))) +
   geom_point(aes(color=factor(sim.col)),size=3) +
   ylab(expression(Delta~"% targeted killings")) +xlab("")+
   scale_color_manual("", labels=c(0,1), values=c("grey", "black"))+
   scale_y_continuous(breaks=c(-20,-10,0,10,20, 30)) + coord_flip() +
   theme.arg + theme(legend.position="none",
                     axis.title = element_text(size=label.size),
                     axis.text = element_text(size=label.size))
plot
ggsave(plot=plot, file="FigureA3_rel-effect-sizes-firstdiff.pdf", width=5, height=4)


#######################################################################
##### Table A6 ######
#######################################################################
#### Control at 70%
fm.3g9.70<- glm(cbind(targeted, total-targeted) ~ 
                   X3G*control70 + date2w + total.log + Government.diff, family=binomial(logit),
                data=data)
summary(fm.3g9.70)

fm.3g6.70<- glm(cbind(targeted, total-targeted) ~ 
                X3G*control70 + date2w +  total.log +
                Alawi + Druze + Kurds + Christians +
                pop11.log,
             family=binomial(logit),
           data=data)
summary(fm.3g6.70)

## percentage of government control
fm.3g7.70<- glm(cbind(targeted, total-targeted) ~ 
                X3G*perc.Government + control70
             + date2w + total.log +
                Alawi + Druze + Kurds + Christians +
                pop11.log,
             family=binomial(logit),
           data=data)
summary(fm.3g7.70)



model.ses.70 <- list(make.cl.se(fm.3g9.70)[[1]],
                   make.cl.se(fm.3g6.70)[[1]],
                   make.cl.se(fm.3g7.70)[[1]]
                   )
model.pvals.70 <-list(make.cl.se(fm.3g9.70)[[2]],
                   make.cl.se(fm.3g6.70)[[2]],
                   make.cl.se(fm.3g7.70)[[2]]
                   )
model.list.70 <- list(fm.3g9.70, fm.3g6.70, fm.3g7.70)

screenreg(model.list.70, override.se = model.ses.70,
          override.pvalues = model.pvals.70, omit.coef="date2w")


model.names.70 =c("I", "II", "III")
coefnames.70 <- c("Intercept", "Internet access (3G)", "Govt control (70%)", "IS control (70%)",
                "Kurd control (70%)", "Opp control (70%)", "\\# Killings (log)",
                "Govt gains", "Govt losses",
               "Internet * Govt control (70%)", "Internet * Kurd control (70%)",
               "Internet * Opp. control (70%)",
                "Alawi", "Druze", "Kurd", "Christian",
               "Pop (log)",  "Internet * IS control (70%)", "% Govt control", "Internet * % Govt control")
screenreg(model.list.70, custom.model.names=model.names.70, omit.coef="date2w", custom.coef.names=coefnames.70,
          override.se = model.ses.70,  override.pvalues = model.pvals.70, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,18,11,12,13,14,15,16,17,19,20))


texreg(model.list.70, file="TableA6_3G-control-70.tex", 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names.70, 
       custom.coef.names = coefnames.70,
       omit.coef="date2w",
       override.se = model.ses.70,  override.pvalues = model.pvals.70, reorder.coef=c(1,2,3,4,5,6,7,8,9,10,18,11,12,13,14,15,16,17,19,20),
       digits=3, custom.note="%stars. Reference category: Control=Contested. Governorate-clustered SEs.",
       symbol = "+", label="3g:70", scriptsize=TRUE)

#######################################################################
##### Table A7 ######
#######################################################################
## Model that includes further explanatory variables, for both categorical control variable, and for percentage government variable
## normal
data$i.var <- data$X3G
fm.all.1 <- glm(cbind(targeted, total-targeted) ~ 
              i.var*control + date2w +  total.log +
              i.var*Alawi + Druze + Kurds + Christians +
              pop11.log  + unemp_perc_2011 + Government.diff,
           family=binomial(logit),
           data=data)

fm.all.2<- glm(cbind(targeted, total-targeted) ~ 
              i.var*perc.Government+ control + date2w +  total.log +
              i.var*Alawi + Druze + Kurds + Christians +
              pop11.log  + unemp_perc_2011 + Government.diff,
           family=binomial(logit),
           data=data)

## lags
data$i.var <- data$l.X3G
fm.all.1.lag <- glm(cbind(targeted, total-targeted) ~ 
              i.var*control + date2w +  total.log +
              i.var*Alawi + Druze + Kurds + Christians +
              pop11.log  + unemp_perc_2011 + Government.diff,
           family=binomial(logit),
           data=data)

fm.all.2.lag<- glm(cbind(targeted, total-targeted) ~ 
              i.var*perc.Government+ control + date2w +  total.log +
              i.var*Alawi + Druze + Kurds + Christians +
              pop11.log  + unemp_perc_2011 + Government.diff,
           family=binomial(logit),
           data=data)

model.ses.ks <- list(make.cl.se(fm.all.1)[[1]],
                     make.cl.se(fm.all.1.lag)[[1]],
                     make.cl.se(fm.all.2)[[1]],
                     make.cl.se(fm.all.2.lag)[[1]])
model.pvals.ks <-list(make.cl.se(fm.all.1)[[2]],
                     make.cl.se(fm.all.1.lag)[[2]],
                     make.cl.se(fm.all.2)[[2]],
                     make.cl.se(fm.all.2.lag)[[2]])


ks.list <- list(fm.all.1, fm.all.1.lag, fm.all.2, fm.all.2.lag)

coefnames.ks <- c("Intercept", "Internet", "Govt control", "IS control",
                  "Kurd control", "Opp control", "\\# Killings (log)",
                  "Alawi", "Druze", "Kurd", "Christian", "Pop (log)", "Unempl. (%)",
                  "Govt gains", "Govt losses",
                  "Internet * Govt control", "Internet * Kurd control",
                  "Internet * Opp. control",    "Internet * Alawi",  "Internet * IS control",
                  "% Govt control", "Internet * % Govt control")

model.names.ks <- c("3G" , "lagged 3G", "3G", "lagged 3G")

screenreg(ks.list,
          omit.coef="date2w",
          custom.coef.names=coefnames.ks,
          custom.model.names=model.names.ks,
          override.se = model.ses.ks,
          override.pvalues = model.pvals.ks,  reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,19,21,22))

texreg(ks.list, file="TableA7_3G-fullmodels.tex", 
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       custom.model.names=model.names.ks, 
       custom.coef.names = coefnames.ks,
       reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,19,21,22),
       omit.coef="date2w",
       override.se = model.ses.ks,  override.pvalues = model.pvals.ks,
       digits=3, custom.note="%stars. Reference category: Control=Contested. Governorate-clustered SEs.",
       symbol = "+", label="3g:ks", scriptsize=TRUE)
