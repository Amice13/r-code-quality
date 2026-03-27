
library(brglm) # Table A3 & Figure A 1 were created using brglm version 0.6.1, stargazer version 5.2, multiwayvcov version 1.2.3, 
library(multiwayvcov) # arm version 1.9-3, effects version 4.0-0 and ggplot2 version 2.2.1
library(arm)
library(stargazer)
library(effects)
library(ggplot2)


# Read data from your directory
dat <- read.csv("C:/PATH/.../ehadata.csv")

### EHA

m1 <- glm(conc~concmigou+concmigin+time+time2+time3, dat, family = binomial("logit"))
clust.m1 <- as.matrix(sqrt(diag(cluster.vcov(m1, cbind(dat$cod)))),ncol =1)
rownames(clust.m1) <- (variable.names(m1))

m2 <- glm(conc~concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+numpar+left+PR_dummy+Mandbugref+time+time2+time3, dat, family = binomial("logit"))
clust.m2 <- as.matrix(sqrt(diag(cluster.vcov(m2, cbind(dat$cod)))),ncol =1)
rownames(clust.m2) <- (variable.names(m2))

m3 <- brglm(conc~concmigou+concmigin+time+time2+time3, dat, family = binomial("logit"))
clust.m3 <- as.matrix(sqrt(diag(cluster.vcov(m3, cbind(dat$cod)))),ncol =1)
rownames(clust.m3) <- (variable.names(m3))

m4 <- brglm(conc~concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+numpar+left+PR_dummy+Mandbugref+time+time2+time3, dat, family = binomial("logit"))
clust.m4 <- as.matrix(sqrt(diag(cluster.vcov(m4, cbind(dat$cod)))),ncol =1)
rownames(clust.m4) <- (variable.names(m4))

m5 <- bayesglm(conc~concmigou+concmigin+time+time2+time3, dat, family = binomial("logit"))
clust.m5 <- as.matrix(sqrt(diag(cluster.vcov(m5, cbind(dat$cod)))),ncol =1)
rownames(clust.m5) <- (variable.names(m5))

m6 <- bayesglm(conc~concmigou+concmigin+infantmor+ind_sh+depratio+urban+lnpop+lnPCfedsub+numpar+left+PR_dummy+Mandbugref+time+time2+time3, dat, family = binomial("logit"))
clust.m6 <- as.matrix(sqrt(diag(cluster.vcov(m6, cbind(dat$cod)))),ncol =1)
rownames(clust.m6) <- (variable.names(m6))

class(m5) <- c("glm","lm")
m5$call[1] <- quote(lm())

class(m6) <- c("glm","lm")
m6$call[1] <- quote(lm())

stargazer(m1, m2,m3,m4,m5,m6,
          se = list(clust.m1,clust.m2,clust.m3,clust.m4,clust.m5,clust.m6), type = "latex", 
          out = "C:/PATH/.../endo.tex", 
          font.size = "footnotesize", dep.var.labels.include = F, 
          dep.var.caption = "Concordat Accession",
          no.space = T, float.env = "sidewaystable", 
          title = "The Determinants of Concordat Accession 1915-1967",
          covariate.labels = c("Outward Migration","Inward Migration","Infant Mort.","$\\%$ Industr. Labor Force",
                               "$\\%$ Dependency Ratio","$\\%$ Urbanization","ln Pop. Size","ln Federal Subsidies",
                               "Number Parties in Gov.", "$\\%$ Left Government","Intro. PR",
                               "Mandatory Budget Referendum","Time", "Time squared", "Time cubed"),
          model.names = F, label = "tab2")

cluster.ids.h <- data.frame(i = dat$cod)
mwfunch <- function(x,cluster) {return(cluster.vcov(x, cluster= cluster.ids.h))}

plotf <- as.data.frame(effect("concmigou", m5, vcov=mwfunch,xlevels=list(concmigou=seq(min(dat$concmigou),max(dat$concmigou),0.01)),typical = mean))

plot1 <- ggplot(data=plotf, aes(x=concmigou, y=fit))  +  geom_rug(data=dat, aes(x=concmigou,y=concmigou),position = "jitter",sides = "b",inherit.aes = F) +geom_line(data=plotf)+
  geom_ribbon(data=plotf,aes(ymin=lower,ymax=upper),alpha=0.3)+ theme(plot.title = element_text(size=18),
                                                                      panel.background = element_rect(fill = 'white', colour = 'grey'),panel.grid.major = element_line(colour = "grey",linetype = "dotted"))+
  labs(title = "Predicted Probabilities for Outward Migration",x = "Outward Migration", y="Predicted Probability")


# Save figure in your directory
ggsave("C:/PATH/.../migprob.eps", height=7, width=10, units='in',device=cairo_ps)