## This file imports spatial proxy data for CRSV from existing datasets
## And merges this file with the main dataset.
## Then it conducts analyses of the relationship  between
## these spatial proxies and socio-political outcomes
## This file is associated with Appendix Section 8
## and produces all Tables A22-A28 and Figure A1
## It also exports Figure A1

## Note: this file requires the downloaded dataset Data_Spatial_acl_ged_vio.csv


# this file is designed to run after the following files
# uncomment to run previous coding files
# source("00_Packages.R")
# source("01_DataVariables.R")
# source("02_MainAnalyses_Estimation.R")
# source("03_MainAnalyses_Mechanisms.R")
# source("04_MainAnalyses_Outcomes.R")
# source("05_Appendix_Descriptives.R")

#####################
## Appendix 8 Comparison of Survey Measures with Geospatial Measures
#####################


# Load spatial data -----------------
D.space <- data.frame(read.csv("Data_Spatial_acl_ged_vio.csv"))
names(D.space)
names(D.space)[2:4] <- c("acled.all", "acled.sv", "ged.all")
names(D.space)


# Merge datasets 
D.spm <- merge(D.space, D, by.x="X_uuid")


#####################
## Appendix 8 Table A22
#####################

# Correlate spatial measures with direct measure of SV

BCOR.AA <- lm(D.spm$rape_yes ~ D.spm$acled.all) #significantly correlated
BCOR.AS <- lm(D.spm$rape_yes ~ D.spm$acled.sv) #somewhat significantly correlated
BCOR.GA <- lm(D.spm$rape_yes ~ D.spm$ged.all) #not correlated


#produce Table A22
stargazer(BCOR.AA, BCOR.AS, BCOR.GA, type="text", 
          title="Correlations between spatial measure of Violence and our direct measure of SV", 
          notes = c("Bivariate linear model regressing the direct survey","measure on spatial measures from ACLED and UCDP-GED"), 
          header=FALSE, 
          font.size = "footnotesize", 
          column.sep.width = "0pt", 
          no.space=TRUE, 
          omit.stat = c("ser", "f"), 
          single.row=TRUE, 
          align=TRUE)

#####################
## Appendix 8 Table A23
#####################

# Correlate spatial measures with listmeasure of SV

BCORLIST.AA <- ictreg(D.spm$list1_CRSV ~ D.spm$acled.all,
                      treat="treat_list_CRSV", 
                      data = D.spm, J=3, method="lm")

BCORLIST.AS <- ictreg(D.spm$list1_CRSV ~ D.spm$acled.sv,
                      treat="treat_list_CRSV", 
                      data = D.spm, J=3, method="lm")

BCORLIST.GA <-  ictreg(D.spm$list1_CRSV ~ D.spm$ged.all,
                       treat="treat_list_CRSV", 
                       data = D.spm, J=3, method="lm")
## make a table of correlations
r2 <- round(rbind(c(BCORLIST.AA$par.treat[2],BCORLIST.AA$se.treat[2]), 
                  c(BCORLIST.AS$par.treat[2],BCORLIST.AS$se.treat[2]),
                  c(BCORLIST.GA$par.treat[2],BCORLIST.GA$se.treat[2])),3)

kable(r2, caption = "Correlations between Spatial measures and List measure of SV", format = "html",col.names = c("Coefficient", "Standard Error")) %>%
  kable_styling(latex_options = c("striped", "hold_position"), font_size=10) %>%
  footnote(general = c("Bivariate linear model regressing", "spatial measure on our list experiment measure", "using ict.reg from the list package.")) 

#####################
## Appendix 8 Figure A1
#####################

# Run same analyses on outcomes replacing measure of CRSV with spatial approximation measure

## generate functions for regressions
cl   <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  not <- attr(fm$model,"na.action")
  if(!is.null(not)){
    cluster <- cluster[-not]
    dat <- dat[-not,]
  }
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }

## create a function to run a model for each socio-political outcome with 
spc.fct <- function(OUTS, SMEAS){
  #get two variables into formula form
  regterms.all <- paste0(OUTS, " ~ ", SMEAS, "+ D.spm$treat_list_CRSV + D.spm$vio_witness1 + D.spm$murder_yes + D.spm$female + D.spm$age + D.spm$edu_level + D.spm$hh_size + D.spm$assets_sum + D.spm$exchange_prev + as.factor(D.spm$territoire)")
  regterms.all <- as.formula(regterms.all)
  
  #Write linear functions
  reg.all <- lm(regterms.all)
  
  #cluster standard errors
  reg.all.c <- cl(dat=D.spm,reg.all, cluster=D.spm$VILLID)
  
  #generate to matrix to hold variables and insert
  MAT <- vector(mode="list")
  
  MAT$reg.all <- reg.all
  
  MAT$reg.all.c <- reg.all.c

  
  return(MAT)
}

##Acled All

AA.PE <- spc.fct(OUTS="D.spm$ingroup_visit_mean_norm", SMEAS="D.spm$acled.all")
AA.OL <- spc.fct(OUTS="D.spm$org_leader_d", SMEAS="D.spm$acled.all")
AA.OM <- spc.fct(OUTS="D.spm$org_member_r1_norm", SMEAS="D.spm$acled.all")
AA.EV <- spc.fct(OUTS="D.spm$event_com_mean_norm", SMEAS="D.spm$acled.all")
AA.DA <- spc.fct(OUTS="D.spm$donate_amount_norm", SMEAS="D.spm$acled.all")

##Acled SV

AS.PE <-spc.fct(OUTS="D.spm$ingroup_visit_mean_norm", SMEAS="D.spm$acled.sv")
AS.OL <- spc.fct(OUTS="D.spm$org_leader_d", SMEAS="D.spm$acled.sv")
AS.OM <- spc.fct(OUTS="D.spm$org_member_r1_norm", SMEAS="D.spm$acled.sv")
AS.EV <- spc.fct(OUTS="D.spm$event_com_mean_norm", SMEAS="D.spm$acled.sv")
AS.DA <- spc.fct(OUTS="D.spm$donate_amount_norm", SMEAS="D.spm$acled.sv")

##GED All

GA.PE <-spc.fct(OUTS="D.spm$ingroup_visit_mean_norm", SMEAS="D.spm$ged.all")
GA.OL <- spc.fct(OUTS="D.spm$org_leader_d", SMEAS="D.spm$ged.all")
GA.OM <- spc.fct(OUTS="D.spm$org_member_r1_norm", SMEAS="D.spm$ged.all")
GA.EV <- spc.fct(OUTS="D.spm$event_com_mean_norm", SMEAS="D.spm$ged.all")
GA.DA <- spc.fct(OUTS="D.spm$donate_amount_norm", SMEAS="D.spm$ged.all")


### Create data frames for figures
### For Appendix Section 8

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier


## Political Exchange Spatial
PE.spatial <- data.frame(Outcome = c("Personal Exchange",
                                     "Personal Exchange",
                                     "Personal Exchange"),
                         Coefficient = c(AA.PE$reg.all$coefficients["D.spm$acled.all"],
                                         AS.PE$reg.all$coefficients["D.spm$acled.sv"],
                                         GA.PE$reg.all$coefficients["D.spm$ged.all"]),
                         SE = c(AA.PE$reg.all.c["D.spm$acled.all","Std. Error"],
                                AS.PE$reg.all.c["D.spm$acled.sv","Std. Error"],
                                GA.PE$reg.all.c["D.spm$ged.all","Std. Error"]),
                         modelName = c("Acled All",
                         "Acled SV",
                         "UCDP-GED All"))

## Org Leader Spatial
OL.spatial <- data.frame(Outcome = c(" Org Leader          ",
                                     " Org Leader          ",
                                     " Org Leader          "),
                         Coefficient = c(AA.OL$reg.all$coefficients["D.spm$acled.all"],
                                         AS.OL$reg.all$coefficients["D.spm$acled.sv"],
                                         GA.OL$reg.all$coefficients["D.spm$ged.all"]),
                         SE = c(AA.OL$reg.all.c["D.spm$acled.all","Std. Error"],
                                AS.OL$reg.all.c["D.spm$acled.sv","Std. Error"],
                                GA.OL$reg.all.c["D.spm$ged.all","Std. Error"]),
                         modelName = c("Acled All",
                                       "Acled SV",
                                       "UCDP-GED All"))

## Org Member Spatial
OM.spatial <- data.frame(Outcome = c(" Org Member          ",
                                     " Org Member          ",
                                     " Org Member          "),
                         Coefficient = c(AA.OM$reg.all$coefficients["D.spm$acled.all"],
                                         AS.OM$reg.all$coefficients["D.spm$acled.sv"],
                                         GA.OM$reg.all$coefficients["D.spm$ged.all"]),
                         SE = c(AA.OM$reg.all.c["D.spm$acled.all","Std. Error"],
                                AS.OM$reg.all.c["D.spm$acled.sv","Std. Error"],
                                GA.OM$reg.all.c["D.spm$ged.all","Std. Error"]),
                         modelName = c("Acled All",
                                       "Acled SV",
                                       "UCDP-GED All"))

## Event Community Spatial
EV.spatial <- data.frame(Outcome = c("Event Engagements",
                                     "Event Engagements",
                                     "Event Engagements"),
                         Coefficient = c(AA.EV$reg.all$coefficients["D.spm$acled.all"],
                                         AS.EV$reg.all$coefficients["D.spm$acled.sv"],
                                         GA.EV$reg.all$coefficients["D.spm$ged.all"]),
                         SE = c(AA.EV$reg.all.c["D.spm$acled.all","Std. Error"],
                                AS.EV$reg.all.c["D.spm$acled.sv","Std. Error"],
                                GA.EV$reg.all.c["D.spm$ged.all","Std. Error"]),
                         modelName = c("Acled All",
                                       "Acled SV",
                                       "UCDP-GED All"))

## Donate Amount Spatial
DA.spatial <- data.frame(Outcome = c(" Donate Amount      ",
                                     " Donate Amount      ",
                                     " Donate Amount      "),
                         Coefficient = c(AA.DA$reg.all$coefficients["D.spm$acled.all"],
                                         AS.DA$reg.all$coefficients["D.spm$acled.sv"],
                                         GA.DA$reg.all$coefficients["D.spm$ged.all"]),
                         SE = c(AA.DA$reg.all.c["D.spm$acled.all","Std. Error"],
                                AS.DA$reg.all.c["D.spm$acled.sv","Std. Error"],
                                GA.DA$reg.all.c["D.spm$ged.all","Std. Error"]),
                         modelName = c("Acled All",
                                       "Acled SV",
                                       "UCDP-GED All"))

## Plot function for Appendix Section 8 Coefficient Plots 

spatial.plot.fct <- function(SPATIAL.OUT){ggplot(SPATIAL.OUT, aes(colour = modelName, fill = Outcome)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = Outcome, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = Outcome, y = Coefficient, ymin = Coefficient - SE*interval2,
                      ymax = Coefficient + SE*interval2),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  #geom_text(aes(x=Outcome,y=Coefficient, label=as.character(round(Coefficient,2))), colour="gray20", alpha=2, size=2, nudge_x = 0, nudge_y = 0, check_overlap = T) + 
  coord_flip() + theme_bw() + theme(legend.position="top") + scale_color_manual(values=c('#fdbb84','#2c7fb8', '#CC79A7')) + ylim(-.015,.01) +
  theme(legend.title=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))}

## apply function to create multiple figures

sp1 <- spatial.plot.fct(SPATIAL.OUT=PE.spatial)

sp2 <- spatial.plot.fct(SPATIAL.OUT=OL.spatial)

sp3 <- spatial.plot.fct(SPATIAL.OUT=OM.spatial)

sp4 <- spatial.plot.fct(SPATIAL.OUT=EV.spatial)

sp5 <- spatial.plot.fct(SPATIAL.OUT=DA.spatial)

## view figures individually
sp1
sp2
sp3
sp4
sp5

## export subfigures individually

jpeg("FigureA1_1.jpeg", units="in", width=5, height=4, res=300)
sp1
dev.off()

jpeg("FigureA1_2.jpeg", units="in", width=5, height=4, res=300)
sp2
dev.off()

jpeg("FigureA1_3.jpeg", units="in", width=5, height=4, res=300)
sp3
dev.off()

jpeg("FigureA1_4.jpeg", units="in", width=5, height=4, res=300)
sp4
dev.off()

jpeg("FigureA1_5.jpeg", units="in", width=5, height=4, res=300)
sp5
dev.off()

### Appendix Figure plot summarizing spatial measure of CRSV on socio-political outcomes

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(sp5)

lay <- rbind(c(1,1,1,2,2,2),
             c(3,3,3,4,4,4),
             c(5,5,5,6,6,6))

# produces figure
grid.arrange(sp1 + theme(legend.position="none"), 
             sp2 + theme(legend.position="none"),
             sp3 + theme(legend.position="none"),
             sp4 + theme(legend.position="none"),
             sp5 + theme(legend.position="none"),
             mylegend,
             layout_matrix = lay,
             nrow=3, ncol=6, 
             top=textGrob("", gp=gpar(fontsize=5)))


#####################
## Appendix 8 Table A24-A28
#####################

#### Table outputs for socio-political outcomes using spatial measures of CRSV

star.space <- function(A, B, C, AC, BC, CC, TI){stargazer(A, B, C, type = "text", 
                                                          title = TI, 
                                                          se = list(AC[,2], BC[,2], CC[,2]), 
                                                          t = list(AC[,3], BC[,3], CC[,3]), 
                                                          p = list(AC[,4], BC[,4], CC[,4]), 
                                                          omit.stat = c("ser", "f"),
                                                          omit = "territoire",
                                                          omit.labels="Territory Fixed Effects",
                                                          dep.var.labels.include = FALSE,
                                                          font.size = "footnotesize",
                                                          column.sep.width = "0pt",
                                                          notes = c("Linear Models", "Standard Errors clustered at the Village Level"),
                                                          no.space = TRUE,
                                                          single.row=TRUE,
                                                          align=TRUE,
                                                          intercept.bottom = TRUE,
                                                          covariate.labels = c("Acled All", "Acled SV", "UCDP-GED All","List Treatment", "Witnessed CRSV", "Homicide", "Female", "Age", "Education", "HH Size", "Assets", "Prev. Exchange", "Constant"),
                                                          header=FALSE) 
}

#produce Table A24
star.space(A=AA.PE$reg.all, B=AS.PE$reg.all, C=GA.PE$reg.all, AC=AA.PE$reg.all.c, BC=AS.PE$reg.all.c, CC=GA.PE$reg.all.c, TI="Political Exchange")
#produce Table A25
star.space(A=AA.OL$reg.all, B=AS.OL$reg.all, C=GA.OL$reg.all, AC=AA.OL$reg.all.c, BC=AS.OL$reg.all.c, CC=GA.OL$reg.all.c, TI="Org Leader")
#produce Table A26
star.space(A=AA.OM$reg.all, B=AS.OM$reg.all, C=GA.OM$reg.all, AC=AA.OM$reg.all.c, BC=AS.OM$reg.all.c, CC=GA.OM$reg.all.c, TI="Org Member")
#produce Table A27
star.space(A=AA.EV$reg.all, B=AS.EV$reg.all, C=GA.EV$reg.all, AC=AA.EV$reg.all.c, BC=AS.EV$reg.all.c, CC=GA.EV$reg.all.c, TI="Event Engagements")
#produce Table A28
star.space(A=AA.DA$reg.all, B=AS.DA$reg.all, C=GA.DA$reg.all, AC=AA.DA$reg.all.c, BC=AS.DA$reg.all.c, CC=GA.DA$reg.all.c, TI="Donate Amount")


