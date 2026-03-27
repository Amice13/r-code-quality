library(ggplot2)
library(data.table)
library(texreg)
library(MASS)
library(sandwich)
library(lmtest)

make.cl.se <- function(model) {
   ct <-coeftest(model, function(x) vcovHC(x,type="HC",cluster="gov"))
   se <- ct[, 2]
   pval <- ct[, 4]
   return(list(se, pval))
}

theme.arg <-  theme(panel.background=element_blank(),
                    panel.border=element_rect(colour = "black", fill=NA),
                    axis.text=element_text(color="black"))
label.size <- 12

load("analysis-data.Rdata")

data$date2w <- factor(data$date2w)

fm.t.1<- glm.nb(targeted ~ X3G*control + date2w,
                data=data)

fm.t.2<- glm.nb(targeted ~ X3G*control + date2w +  X3G*Alawi, 
                data=data)

fm.t.3<- glm.nb(targeted ~ X3G*perc.Government + control + date2w + Alawi + Druze + Kurds + Christians +
                   pop11.log + unemp_perc_2011, 
                data=data)

fm.u.1<- glm.nb(nhat.untarg ~X3G*control + date2w,
                data=data)

fm.u.2<- glm.nb(nhat.untarg ~ 
                   X3G*control + date2w + X3G*Alawi, 
                data=data)

fm.u.3<- glm.nb(nhat.untarg ~ X3G*perc.Government + control + date2w + Alawi + Druze + Kurds + Christians +
                   pop11.log + unemp_perc_2011,
                data=data)

model.ses <-  list(make.cl.se(fm.t.1)[[1]], make.cl.se(fm.u.1)[[1]], make.cl.se(fm.t.2)[[1]], make.cl.se(fm.u.2)[[1]], make.cl.se(fm.t.3)[[1]], make.cl.se(fm.u.3)[[1]])

## save clustered pvalues
model.pvals <- list(make.cl.se(fm.t.1)[[2]], make.cl.se(fm.u.1)[[2]], make.cl.se(fm.t.2)[[2]], make.cl.se(fm.u.2)[[2]], make.cl.se(fm.t.3)[[2]], make.cl.se(fm.u.3)[[2]])

## save model list for table
model.list <-list(fm.t.1, fm.u.1, fm.t.2, fm.u.2, fm.t.3, fm.u.3)

table.info <- list(model.list = model.list, model.ses = model.ses, model.pvals = model.pvals)

model.names =c("Targeted \\#", "Untargeted \\#", "Targeted \\#", "Untargeted \\#", "Targeted \\#", "Untargeted \\#")

coefnames <- c("Intercept", "Internet access (3G)", "Govt control", "IS control",
               "Kurd control", "Opp control",
               "Internet * Govt control", "Internet * IS control",
                "Internet * Kurd control", "Internet * Opp. control",
                "Alawi", "Internet * Alawi", "% Govt control", "Druze", "Kurd", "Christian", 
                "Pop (log)", "Unempl. (%)",
                "Internet * % Govt control")


screenreg(table.info$model.list,
          omit.coef="date2w",
          override.se = table.info$model.ses,
          override.pvalues = table.info$model.pvals,
          custom.model.names=model.names,
          custom.coef.names=coefnames,
          reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,13,19))
  
 
texreg(table.info$model.list, file="TableA1_3G-countmodels.tex", 
       omit.coef="date2w",
       override.se = table.info$model.ses,
       override.pvalues = table.info$model.pvals,
       custom.model.names=model.names,
       custom.coef.names=coefnames,
       table = FALSE,
       stars = c(0.001, 0.01, 0.05),
       reorder.coef=c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,13,19),
       digits=3, custom.note="%stars. Reference category: Contested control. Governorate-clustered SEs.",
       symbol = "+", label="3G:count", scriptsize=TRUE)


make.evs <- function(model, gov.control, sim.name, access) {
   num.sims <- length(access)
   betas <- na.omit(coef(model))
   X <- as.data.frame(matrix(data=NA, nrow=length(betas), ncol=num.sims+1))
   X[,num.sims+1] <- names(betas)
   X[grep("date2w", names(betas)),1:num.sims] <- 0

   names(X)[num.sims+1] <- "vars"
   X[X$vars=="(Intercept)",1:num.sims] <- 1
   X[X$vars=="Druze",1:num.sims] <- 0
   X[X$vars=="Kurds",1:num.sims] <- 0
   X[X$vars=="Christians",1:num.sims] <- 0
   X[X$vars=="controlIslamic State", 1:num.sims] <- 0
   X[X$vars=="controlKurds",1:num.sims] <- 0
   X[X$vars=="controlOpposition",1:num.sims] <- 0
   X[X$vars=="controlGovernment",1:num.sims] <- gov.control
   
   X[X$vars=="X3G", 1:num.sims] <- access   
   X[X$vars=="X3G:controlGovernment",1:num.sims] <- X[X$vars=="X3G", 1:num.sims] *
      X[X$vars=="controlGovernment",1:num.sims]
   X[X$vars=="X3G:controlIslamic State", 1:num.sims] <- 0
   X[X$vars=="X3G:controlKurds",1:num.sims] <- 0
   X[X$vars=="X3G:controlOpposition",1:num.sims] <- 0

   X[X$vars=="Alawi",1:num.sims] <- 0
   X[X$vars=="pop11.log",1:num.sims] <- mean(data$pop11.log)
   X[X$vars=="unemp_perc_2011",1:num.sims] <- mean(data$unemp_perc_2011)
   
   
   ## use VCOV -> cluster standard errors by governorate
   
   S <- mvrnorm(1000, betas,vcovHC(model,type="HC",cluster="gov"))
   y.hat <- S %*% as.matrix(X[,1:num.sims])
   pred <- exp(y.hat)
   pred <- as.data.frame(pred)

   diff <- pred[ ,num.sims]- pred[ ,1]

   diff.evs <- data.frame(low=quantile(diff, 0.025), mean=mean(diff), up=quantile(diff, 0.975), sim.name=sim.name)


   
   evs <- array(data=NA, dim=c(num.sims, 3))
   evs <- as.data.frame(evs)
   names(evs) <- c("low", "mean", "up")
   evs$access <- access

   
  
   evs$lo95 <- apply(pred, 2, function(x) quantile(x, 0.025))
   evs$lo83 <- apply(pred, 2, function(x) quantile(x, 0.085))
   evs$mean <- apply(pred, 2, function(x) mean(x))
   evs$up83 <- apply(pred, 2, function(x) quantile(x, 0.915))
   evs$up95 <- apply(pred, 2, function(x) quantile(x, 0.975))

   
   evs$sim.name <- sim.name
   
   return(list(evs, diff.evs))
}


## model with perc. of government control
# access 
num.sims <- 100
access <- seq(1, 4, length.out = num.sims)

plot.data <- rbind(make.evs(model=fm.t.1, gov.control=0, sim.name="Targeted Killings", access)[[1]],
                   make.evs(model=fm.u.1, gov.control=0, sim.name="Untargeted Killings", access)[[1]])


plot <- ggplot(plot.data,
               aes(x=access, y =mean)) +
   geom_line()+
   geom_ribbon(aes(ymax = up95, ymin= lo95),alpha=0.1)+
   geom_ribbon(aes(ymax = up83, ymin= lo83),alpha=0.2)+
   facet_wrap(~sim.name, nrow=1, scales="free")+
   labs(x = "Internet access", y = "Expected number of killings")  +
   scale_x_continuous(breaks = c(1, 4), label = c("None", "Full")) +
   theme.arg + theme(legend.position="none", 
                     axis.title = element_text(size = label.size),
                     strip.text.x = element_text(size = label.size))
    
plot
ggsave(plot=plot, file="FigureA1_expected_counts.pdf",
       width=7, height=3)



plot.data <- rbind(make.evs(model=fm.t.1, gov.control=0, sim.name="Targeted Killings", access)[[2]],
                   make.evs(model=fm.u.1, gov.control=0, sim.name="Untargeted Killings", access)[[2]])

plot <- ggplot(plot.data,
               aes(x=sim.name, y =mean)) +
   geom_point(size=2)+
   geom_errorbar(aes(ymax = up, ymin= low),alpha=0.7, width=.02)+
   geom_hline(yintercept=0, linetype="dotted") +
   labs(x = "", y = "Expected change in number of killings\nNo Internet access to full access")  +
   theme.arg + theme(axis.text = element_text(size = label.size),
                     axis.title = element_text(size = label.size))
plot
ggsave(plot=plot, file="FigureA2_expected_change_counts.pdf",
       width=4, height=4)


