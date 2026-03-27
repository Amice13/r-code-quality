rm(list=ls())
packages <- c("mediation","foreign","DirectEffects","lmtest","lfe","dplyr","broom","ggplot2","stargazer",
              "sandwich","conleyreg","mapdata","Rcpp","RcppArmadillo","data.table","geosphere","DirectEffects","mediation","reshape","plyr")

package_installed <-
  sapply(packages, function(pack)
    pack %in% rownames(installed.packages()))
if (any(!package_installed)) {
  sapply(packages[!package_installed], install.packages)
}

sapply(packages, require, character.only = TRUE)
rm(packages,package_installed)


data1<-read.csv("maindata.csv")

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A1: Histograms of Headquarters Inflow (left panel) and Headquarters Outflow (right panel).
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

ggplot(data1, aes(x=cont_exo)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) +
  #geom_vline(aes(xintercept=mean(cont_exo)), color="black", linetype="dashed", size=1)
  xlab("Number of Headquarters Outflow (t and t-1)") + ylab("Proportion of Counties Affected") +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) -> p
p
ggsave(p, file="Fig_Hist_cont_exo.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

ggplot(data1, aes(x=cont_in)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) +
  #geom_vline(aes(xintercept=mean(cont_exo)), color="black", linetype="dashed", size=1)
  xlab("Number of Headquarters Inflow (t and t-1)") + ylab("Proportion of Counties Affected") +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) -> p
p
ggsave(p, file="Fig_Hist_cont_in.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A2: Histograms of Headquarters Inflow (left panel) and Headquarters Outflow (right panel) under Democratic governors (top panels) and Republican governors (bottom panels), respectively.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data_d <-subset(data1,inc_party_d==1)
data_r <-subset(data1,inc_party_r==1)

ggplot(data_d, aes(x=cont_exo)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) +
  #geom_vline(aes(xintercept=mean(cont_exo)), color="black", linetype="dashed", size=1)
  xlab("Number of Headquarters Outflow (t and t-1)") + ylab("Proportion of Counties Affected Under Democratic Governor") +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) -> p
p
ggsave(p, file="Fig_Hist_cont_exo_d.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

ggplot(data_r, aes(x=cont_exo)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) +
  #geom_vline(aes(xintercept=mean(cont_exo)), color="black", linetype="dashed", size=1)
  xlab("Number of Headquarters Outflow (t and t-1)") + ylab("Proportion of Counties Affected Under Republican Governor") +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) -> p
p
ggsave(p, file="Fig_Hist_cont_exo_r.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


ggplot(data_d, aes(x=cont_in)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) +
  #geom_vline(aes(xintercept=mean(cont_in)), color="black", linetype="dashed", size=1)
  xlab("Number of Headquarters Inflow (t and t-1)") + ylab("Proportion of Counties Affected Under Democratic Governor") +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) -> p
p
ggsave(p, file="Fig_Hist_cont_in_d.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

ggplot(data_r, aes(x=cont_in)) + geom_histogram(aes(y=..count../sum(..count..)), binwidth=1) +
  #geom_vline(aes(xintercept=mean(cont_in)), color="black", linetype="dashed", size=1)
  xlab("Number of Headquarters Inflow (t and t-1)") + ylab("Proportion of Counties Affected Under Republican Governor") +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()) -> p
p
ggsave(p, file="Fig_Hist_cont_in_r.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#Figure A3: STATE-LEVEL FLOW FIGURE
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

flow <- read.csv("flow_state.csv")

flow1 <- flow %>%
  summarise_at(vars(tohigh), funs(sum(., na.rm=TRUE),mean(., na.rm=TRUE),sd(., na.rm=TRUE))) %>% mutate(id = "To High Tax State")
flow2 <- flow %>%
  summarise_at(vars(tolow), funs(sum(., na.rm=TRUE),mean(., na.rm=TRUE),sd(., na.rm=TRUE))) %>% mutate(id = "To Low Tax State")
flow3 <- flow %>%
  summarise_at(vars(tosame), funs(sum(., na.rm=TRUE),mean(., na.rm=TRUE),sd(., na.rm=TRUE))) %>% mutate(id = "To Same Tax State") 
flow_all <- bind_rows(flow1, flow2, flow3)

flow_all <- flow_all %>% 
  mutate(meanlabel = paste0("Mean=",round(mean, 2))) %>%
  mutate(sdlabel = paste0("SD=",round(sd, 2)))

ggplot(data=flow_all, aes(x=id, y=mean, fill=id)) +
  geom_bar(stat="identity", position=position_dodge())+
  #scale_fill_manual(values=c("blue", "red"))+
  geom_text(aes(label=meanlabel), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=7)+
  geom_text(aes(label=sdlabel), vjust=1.5, color="white",
            position = position_dodge(0.9), size=6)+
  #geom_text(aes(label=party), vjust=-0.8, color="black",
  #          position = position_dodge(0.9), size=3.5, fontface="bold")+
  xlab("") + ylab("Proportion of Interstate Headquarters Relocation Cases")+ labs(fill = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=16, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=16, face="bold"),
    legend.text = element_text(size = 7, colour = "black"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.15, 0.92),
    legend.position="none",
    legend.title = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())-> p
p
ggsave(p, file="Fig_Hist_tax.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


pflow1 <- flow %>%
  summarise_at(vars(todemo), funs(sum(., na.rm=TRUE),mean(., na.rm=TRUE),sd(., na.rm=TRUE))) %>% mutate(id = "Republican State \nto Democratic State")
pflow2 <- flow %>%
  summarise_at(vars(torep), funs(sum(., na.rm=TRUE),mean(., na.rm=TRUE),sd(., na.rm=TRUE))) %>% mutate(id = "Democratic State \nto Republican State")
pflow3 <- flow %>%
  summarise_at(vars(todr), funs(sum(., na.rm=TRUE),mean(., na.rm=TRUE),sd(., na.rm=TRUE))) %>% mutate(id = "Rep(Dem).State \nto Rep(Dem).State") 
pflow_all <- bind_rows(pflow1, pflow2, pflow3)
#pflow_all <- pflow_all %>% 
#  mutate(meanlabel = paste0("Mean=",scales::percent(round(mean, 4), accuracy=0.1))) %>%
#  mutate(sdlabel = paste0("SD=",scales::percent(round(sd, 4), accuracy=0.1)))
pflow_all <- pflow_all %>% 
  mutate(meanlabel = paste0("Mean=",round(mean, 2))) %>%
  mutate(sdlabel = paste0("SD=",round(sd, 2)))

ggplot(data=pflow_all, aes(x=id, y=mean, fill=id)) +
  geom_bar(stat="identity", position=position_dodge())+
  #scale_fill_manual(values=c("blue", "red"))+
  geom_text(aes(label=meanlabel), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=7)+
  geom_text(aes(label=sdlabel), vjust=1.5, color="white",
            position = position_dodge(0.9), size=6)+
  #geom_text(aes(label=party), vjust=-0.8, color="black",
  #          position = position_dodge(0.9), size=3.5, fontface="bold")+
  xlab("") + ylab("Proportion of Interstate Headquarters Relocation Cases")+ labs(fill = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=16, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=16, face="bold"),
    legend.text = element_text(size = 7, colour = "black"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.15, 0.92),
    legend.position="none",
    legend.title = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())-> p
p


ggsave(p, file="Fig_Hist_party.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)





#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#Figure A4: HQ Relocation Cases by States, 1995-2015.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

lecon<-read.csv("localecondata.csv")

lecon_st <- lecon %>%
  group_by(state1, year) %>%
  summarise_at(vars(ccont_in, ccont_exo), funs(sum(., na.rm=TRUE),mean(., na.rm=TRUE),sd(., na.rm=TRUE)))

lecon_st <- lecon_st %>% mutate(ccont_exo_sum= -1 *ccont_exo_sum)


ggplot(data = lecon_st, aes(x = year, group = state1)) +
  geom_line(aes(y = ccont_in_sum, color = "Headquarters Inflow"), size=1) +
  geom_line(aes(y = ccont_exo_sum, color = "Headquarters Outflow"), size=1) +
  #geom_vline(aes(xintercept=election), color ="gray", linetype = "dashed") + 
  geom_hline(yintercept = 0) +
  #scale_colour_manual( 
  #  breaks = c("Net HQ Influx (Dem)","Net HQ Influx (Rep)"),
  #  values = c("Net HQ Influx (Dem)"="blue","Net HQ Influx (Rep)"="red")) +
  xlab("Year") +
  scale_x_continuous("Year", expand=c(0,0.5), breaks = c(1995, 2000, 2005, 2010, 2015)) + 
  scale_y_continuous("Cases", expand=c(0,0), limits = c(-15,25)) +
  theme_bw() +
  theme(
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    #legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position="none",
    #legend.title = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size=7))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  #coord_fixed(ratio=13) +
  facet_wrap(~  state1, ncol = 6,scales="free") ->p

p
ggsave(p, file="Fig_FlowByState.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)



#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A4: Falsification Tests for Parallel Trend Assumption.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data11<-data1[which(data1$d_cz_HQexo!=1 & data1$d_cz_HQin!=1),]
data11 <- data11 %>% mutate(leadintd_d_cz_HQexo = leadd_cz_HQexo * inc_party_d) %>% mutate(leadintd_d_cz_HQin = leadd_cz_HQin * inc_party_d)

leadmain_czfe<-felm(DemVotesMajorPercent~ leadd_cz_HQexo + leadint_d_cz_HQexo + leadd_cz_HQin + leadint_d_cz_HQin | stateyear + czone_state | 0 | czone_state, data= data11, weight= data11$pop[!is.na( data11$pop)])
leadmain_cfe<-felm(DemVotesMajorPercent~ leadd_cz_HQexo + leadint_d_cz_HQexo + leadd_cz_HQin + leadint_d_cz_HQin | stateyear + cid | 0 | czone_state, data= data11, weight= data11$pop[!is.na( data11$pop)])
leadlagmain_czfe<-felm(DemVotesMajorPercent~ leadd_cz_HQexo  + leadd_cz_HQin + d_cz_HQexo + d_cz_HQin + int_d_cz_HQexo + int_d_cz_HQin + lagd_cz_HQexo + lagd_cz_HQin + leadint_d_cz_HQexo  + leadint_d_cz_HQin  + lagint_d_cz_HQexo + lagint_d_cz_HQin | stateyear + czone_state | 0 | czone_state, data= data1, weight= data1$pop[!is.na( data1$pop)])
leadlagmain_cfe<-felm(DemVotesMajorPercent~ leadd_cz_HQexo  + leadd_cz_HQin + d_cz_HQexo + d_cz_HQin + int_d_cz_HQexo + int_d_cz_HQin + lagd_cz_HQexo + lagd_cz_HQin + leadint_d_cz_HQexo  + leadint_d_cz_HQin  + lagint_d_cz_HQexo + lagint_d_cz_HQin | stateyear + cid | 0 | czone_state, data= data1, weight= data1$pop[!is.na( data1$pop)])

stargazer(leadmain_czfe, leadmain_cfe, leadlagmain_czfe, leadlagmain_cfe,
          ##dep.var.caption = "\\emph{Dependent Variable=Democratic Party’s vote share}",
          dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          omit = ("^DemVotesMajorPercent"),
          order=c(3,5,9,
                  10,7,12,
                  1,4,8,
                  2,6,11),
          covariate.labels = c("Headquarters Inflow (t+1)", "Headquarters Inflow (t)", "Headquarters Inflow (t-1)",
                               "Headquarters Inflow X Republican Governor (t+1)", "Headquarters Inflow X Republican Governor (t)", "Headquarters Inflow X Republican Governor (t-1)",
                               "Headquarters Outflow (t+1)", "Headquarters Outflow (t)", "Headquarters Outflow (t-1)",
                               "Headquarters Outflow X Republican Governor (t+1)", "Headquarters Outflow X Republican Governor (t)", "Headquarters Outflow X Republican Governor (t-1)"
                               
          ),
          float=F,
          add.lines = list(c("State-Year fixed effects","Yes","Yes","Yes","Yes"),c("Commuting zone fixed effects","Yes","No","Yes","No"),c("County fixed effects","No","Yes","No","Yes")),
          #omit.stat = "ser",
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          column.sep.width = "10pt",
          #notes = "Standard errors clustered by CZ.",notes.align = "l",
          out = "Tab_falsification_final.tex")

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A5: Estimation Results Using HQ Relocation Indicators Based On DMA
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

dma_cfe<-felm(DemVotesMajorPercent~ d_dm_HQexo + int_d_dm_HQexo + d_dm_HQin + int_d_dm_HQin | stateyear + cid | 0 | dm_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
dma_cfer<-felm(DemVotesMajorPercent~ d_dm_HQexo + intd_d_dm_HQexo + d_dm_HQin + intd_d_dm_HQin | stateyear + cid | 0 | dm_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

#-----------------------------------------------------------------------#
#FIGURE - main coefficient plots
#-----------------------------------------------------------------------#

dma_cfe<-tidy(dma_cfe, conf.int = TRUE, se.type = "cluster")
dma_cfe<-dma_cfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "d_dm_HQexo" | term== "d_dm_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
dma_cfer<-tidy(dma_cfer, conf.int = TRUE, se.type = "cluster")
dma_cfer<-dma_cfer %>% mutate(type="Under Republican Governor") %>% filter(term== "d_dm_HQexo" | term== "d_dm_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(dma_cfe, dma_cfer)
all$term[all$term=="d_dm_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_dm_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=6)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE))-> p
p

ggsave(p, file="Fig_DMA.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A5: Estimation Results With Spatially-clustered Standard Errors (DV = Democratic Party Vote Share): The distance cutoff is set to 26 km (16 miles), the average one-way commute distance in the U.S. (as of 2005).
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

to.radians<-function(degrees){
  degrees * pi / 180
}

haversine <- function(lat1, long1, lat2, long2, unit="km"){
  radius <- 6378      # radius of Earth in kilometers
  delta.phi <- to.radians(lat2 - lat1)
  delta.lambda <- to.radians(long2 - long1)
  phi1 <- to.radians(lat1)
  phi2 <- to.radians(lat2)
  term1 <- sin(delta.phi/2) ^ 2
  term2 <- cos(phi1) * cos(phi2) * sin(delta.lambda/2) ^ 2
  the.terms <- term1 + term2
  delta.sigma <- 2 * atan2(sqrt(the.terms), sqrt(1-the.terms))
  distance <- radius * delta.sigma
  if(unit=="km") return(distance)
  if(unit=="miles") return(0.621371*distance)
}

#xcrun: error:
#xcode-select --install

sourceCpp("cpp-functions.cpp")

##Source: https://github.com/darinchristensen/conley-se

ConleySEs <- function(reg,
                      unit, time, lat, lon,
                      kernel = "bartlett", dist_fn = "Haversine",
                      dist_cutoff = 500, lag_cutoff = 5,
                      lat_scale = 111, verbose = FALSE, cores = 1, balanced_pnl = FALSE) {
  
  Fac2Num <- function(x) {as.numeric(as.character(x))}
  source("iterate-obs-function.R", local = TRUE)
  if(cores > 1) {invisible(library(parallel))}
  
  if(class(reg) == "felm") {
    Xvars <- rownames(reg$coefficients)
    dt = data.table(reg$cY, reg$cX,
                    fe1 = Fac2Num(reg$fe[[1]]),
                    fe2 = Fac2Num(reg$fe[[2]]),
                    coord1 = Fac2Num(reg$clustervar[[1]]),
                    coord2 = Fac2Num(reg$clustervar[[2]]))
    setnames(dt,
             c("fe1", "fe2", "coord1", "coord2"),
             c(names(reg$fe), names(reg$clustervar)))
    dt = dt[, e := as.numeric(reg$residuals)]
    
  } else {
    message("Model class not recognized.")
    break
  }
  
  n <- nrow(dt)
  k <- length(Xvars)
  
  # Renaming variables:
  orig_names <- c(unit, time, lat, lon)
  new_names <- c("unit", "time", "lat", "lon")
  setnames(dt, orig_names, new_names)
  
  # Empty Matrix:
  XeeX <- matrix(nrow = k, ncol = k, 0)
  
  #================================================================ 
  # Correct for spatial correlation:
  timeUnique <- unique(dt[, time])
  Ntime <- length(timeUnique)
  setkey(dt, time)
  
  if(verbose){message("Starting to loop over time periods...")}
  
  if(balanced_pnl){
    sub_dt <- dt[time == timeUnique[1]]
    lat <- sub_dt[, lat]; lon <- sub_dt[, lon]; rm(sub_dt)
    
    if(balanced_pnl & verbose){message("Computing Distance Matrix...")}
    
    d <- DistMat(cbind(lat, lon), cutoff = dist_cutoff, kernel, dist_fn)
    rm(list = c("lat", "lon"))
  }
  
  if(cores == 1) {
    XeeXhs <- lapply(timeUnique, function(t) iterateObs(sub_index = t,
                                                        type = "spatial", cutoff = dist_cutoff))
  } else {
    XeeXhs <- mclapply(timeUnique, function(t) iterateObs(sub_index = t,
                                                          type = "spatial", cutoff = dist_cutoff), mc.cores = cores)
  }
  
  if(balanced_pnl){rm(d)}
  
  # First Reduce:
  XeeX <- Reduce("+",  XeeXhs)
  
  # Generate VCE for only cross-sectional spatial correlation:
  X <- as.matrix(dt[, eval(Xvars), with = FALSE])
  invXX <- solve(t(X) %*% X) * n
  
  V_spatial <- invXX %*% (XeeX / n) %*% invXX / n
  
  V_spatial <- (V_spatial + t(V_spatial)) / 2
  
  if(verbose) {message("Computed Spatial VCOV.")}
  
  #================================================================ 
  # Correct for serial correlation:
  panelUnique <- unique(dt[, unit])
  Npanel <- length(panelUnique)
  setkey(dt, unit)
  
  if(verbose){message("Starting to loop over units...")}
  
  if(cores == 1) {
    XeeXhs <- lapply(panelUnique, function(t) iterateObs(sub_index = t,
                                                         type = "serial", cutoff = lag_cutoff))
  } else {
    XeeXhs <- mclapply(panelUnique,function(t) iterateObs(sub_index = t,
                                                          type = "serial", cutoff = lag_cutoff), mc.cores = cores)
  }
  
  XeeX_serial <- Reduce("+",  XeeXhs)
  
  XeeX <- XeeX + XeeX_serial
  
  V_spatial_HAC <- invXX %*% (XeeX / n) %*% invXX / n
  V_spatial_HAC <- (V_spatial_HAC + t(V_spatial_HAC)) / 2
  
  return_list <- list(
    "OLS" = reg$vcv,
    "Spatial" = V_spatial,
    "Spatial_HAC" = V_spatial_HAC)
  return(return_list)
}

main_czfe_noint<-felm(DemVotesMajorPercent~ d_cz_HQexo  + d_cz_HQin | stateyear + czone_state | 0 | lat+lng, data=data1, weight=data1$pop[!is.na(data1$pop)], keepCX = TRUE)
main_czfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + czone_state | 0 | lat+lng, data=data1, weight=data1$pop[!is.na(data1$pop)], keepCX = TRUE)
main_cfe_noint<-felm(DemVotesMajorPercent~ d_cz_HQexo + d_cz_HQin | stateyear + cid | 0 | lat+lng, data=data1, weight=data1$pop[!is.na(data1$pop)], keepCX = TRUE)
main_cfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | lat+lng, data=data1, weight=data1$pop[!is.na(data1$pop)], keepCX = TRUE)

#https://www.google.com/search?q=16+miles+to+km&oq=16+miles+to+km&aqs=chrome..69i57j6.2576j0j7&sourceid=chrome&ie=UTF-8
#https://www.cnbc.com/2018/02/22/study-states-with-the-longest-and-shortest-commutes.html
#In the U.S., the average, one-way commute time is 26.1 minutes, according to the U.S. Census Bureau. 

SE1 <-ConleySEs(reg = main_czfe_noint, unit = "czone_state", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 26, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 
SE2 <-ConleySEs(reg = main_czfe, unit = "czone_state", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 26, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 
SE3 <-ConleySEs(reg = main_cfe_noint, unit = "cid", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 26, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 
SE4 <-ConleySEs(reg = main_cfe, unit = "cid", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 26, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 

se1<-sqrt(sapply(SE1, diag))
se2<-sqrt(sapply(SE2, diag))
se3<-sqrt(sapply(SE3, diag))
se4<-sqrt(sapply(SE4, diag))

se1<-se1[,3]  
se2<-se2[,3]  
se3<-se3[,3]  
se4<-se4[,3]  

stargazer(main_czfe_noint, main_czfe, main_cfe_noint, main_cfe , se=list(se1,se2,se3,se4),
          ##dep.var.caption = "\\emph{Dependent Variable=Democratic Party’s vote share}",
          dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          omit = "^DemVotesMajorPercent",
          order = c("d_cz_HQin","int_d_cz_HQin","d_cz_HQexo","int_d_cz_HQexo"),
          covariate.labels = c("Headquarters Inflow", "Headquarters Inflow X Republican Governor",
                               "Headquarters Outflow", "Headquarters Outflow X Republican Governor"
          ),
          float=F,
          add.lines = list(c("State-Year fixed effects","Yes","Yes","Yes","Yes"),c("Commuting zone fixed effects","Yes","Yes","No","No"),c("County fixed effects","No","No","Yes","Yes")),
          omit.stat = "all",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          column.sep.width = "10pt",
          notes = "Conley Spatial Heteroskedasticity and Autocorrelation Consistent  standard errors are in parentheses.",notes.align = "l",
          out = "Tab_conley_final.tex")


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A6: Estimation Results With Spatially-clustered Standard Errors (DV = Democratic Party Vote Share): The distance cutoff is set to 100 km (62 miles).
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#


SE1 <-ConleySEs(reg = main_czfe_noint, unit = "czone_state", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 100, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 
SE2 <-ConleySEs(reg = main_czfe, unit = "czone_state", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 100, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 
SE3 <-ConleySEs(reg = main_cfe_noint, unit = "cid", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 100, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 
SE4 <-ConleySEs(reg = main_cfe, unit = "cid", time = "stateyear", lat = "lat", lon = "lng",dist_fn = "SH", dist_cutoff = 100, lag_cutoff = 1,cores = 1, verbose = FALSE, balanced_pnl = FALSE) 

se1<-sqrt(sapply(SE1, diag))
se2<-sqrt(sapply(SE2, diag))
se3<-sqrt(sapply(SE3, diag))
se4<-sqrt(sapply(SE4, diag))

se1<-se1[,3]  
se2<-se2[,3]  
se3<-se3[,3]  
se4<-se4[,3]  

stargazer(main_czfe_noint, main_czfe, main_cfe_noint, main_cfe , se=list(se1,se2,se3,se4),
          #dep.var.caption = "\\emph{Dependent Variable=Democratic Party’s vote share}",
          dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          omit = "^DemVotesMajorPercent",
          order = c("d_cz_HQin","int_d_cz_HQin","d_cz_HQexo","int_d_cz_HQexo"),
          covariate.labels = c("Headquarters Inflow", "Headquarters Inflow X Republican Governor",
                               "Headquarters Outflow", "Headquarters Outflow X Republican Governor"
          ),
          float=F,
          add.lines = list(c("State-Year fixed effects","Yes","Yes","Yes","Yes"),c("Commuting zone fixed effects","Yes","Yes","No","No"),c("County fixed effects","No","No","Yes","Yes")),
          omit.stat = "all",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          column.sep.width = "10pt",
          notes = "Conley Spatial Heteroskedasticity and Autocorrelation Consistent  standard errors are in parentheses.",notes.align = "l",
          out = "Tab_conley_100_final.tex")

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A6: Estimation Results Including CZ-specific Time Trend
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data1 <- data1 %>% mutate(time = year-1994)

######
t_main_cfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin + factor(czone_state)*time| stateyear  | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
t_main_cfer<-felm(DemVotesMajorPercent~ d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin + factor(czone_state)*time| stateyear  | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

t_main_cfe<-tidy(t_main_cfe, conf.int = TRUE, se.type = "cluster")
t_main_cfe<-t_main_cfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
t_main_cfer<-tidy(t_main_cfer, conf.int = TRUE, se.type = "cluster")
t_main_cfer<-t_main_cfer %>% mutate(type="Under Republican Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(t_main_cfe, t_main_cfer)
all$term[all$term=="d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=6)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE))-> p
p

ggsave(p, file="Fig_CZtrend.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A7: Estimation Results Controlling for Local Economic Indicators (DV = Democratic Party Vote Share)
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

econ_main_cfe_eg<-felm(DemVotesMajorPercent~  d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin + econgrowth| stateyear +cid| 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
econ_main_cfe_wage<-felm(DemVotesMajorPercent~  d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin +ac_log_empl_pc + ac_log_wage_pc | stateyear +cid| 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
econ_main_cfe_inc<-felm(DemVotesMajorPercent~  d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin +ac_log_empl_pc + ac_log_inc_pc| stateyear +cid| 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

stargazer( econ_main_cfe_eg, econ_main_cfe_inc,econ_main_cfe_wage,
           #dep.var.caption = "\\emph{Dependent Variable - Democratic Governor Vote Share:}",
           dep.var.labels.include = FALSE,
           #dep.var.labels = c("President", "Senate","House", "Federal Average"),
           omit = "^DemVotesMajorPercent",
           order = c("d_cz_HQin","int_d_cz_HQin","d_cz_HQexo","int_d_cz_HQexo","econgrowth","ac_log_empl_pc","ac_log_inc_pc","ac_log_wage_pc"),
           covariate.labels = c("Headquarters Inflow", "Headquarters Inflow X Republican Governor",
                                "Headquarters Outflow", "Headquarters Outflow X Republican Governor", 
                                "Economic Growth","$/Delta$ Employment per capita(logged)","$/Delta$ Income per capita(logged)","$/Delta$ Wage per capita(logged)"),
           float=F,
           add.lines = list(c("State-Year fixed effects","Yes","Yes","Yes"),c("County fixed effects","Yes","Yes","Yes")),
           keep.stat = "n",
           star.cutoffs = c(0.1,0.05,0.01),
           star.char = c("†", "*", "**"),
           omit.table.layout = "n",
           notes = "Standard errors clustered by CZ.",notes.align = "l",
           out = "Tab_econ_ctrl_final.tex")

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A7: Estimation Results Using Continuous Measures of HQ Relocation Indicators
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

cont_main_cfe<-felm(DemVotesMajorPercent~ log_cont_exo + int_log_cont_exo + log_cont_in + int_log_cont_in | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
cont_main_cfer<-felm(DemVotesMajorPercent~ log_cont_exo + intd_log_cont_exo + log_cont_in + intd_log_cont_in | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

cont_main_cfe<-tidy(cont_main_cfe, conf.int = TRUE, se.type = "cluster")
cont_main_cfe<-cont_main_cfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "log_cont_exo" | term== "log_cont_in") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
cont_main_cfer<-tidy(cont_main_cfer, conf.int = TRUE, se.type = "cluster")
cont_main_cfer<-cont_main_cfer %>% mutate(type="Under Republican Governor") %>% filter(term== "log_cont_exo" | term== "log_cont_in") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(cont_main_cfe, cont_main_cfer)
all$term[all$term=="log_cont_in"] <- "Headquarters Inflow"
all$term[all$term=="log_cont_exo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=6)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE))-> p
p

ggsave(p, file="Fig_cont.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A8: Estimation Results Using HQ Relocation Indicators Based on Election Year
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

f4_main_cfe<-felm(DemVotesMajorPercent~ f4_d_cz_HQexo + int_f4_d_cz_HQexo + f4_d_cz_HQin + int_f4_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
f4_main_cfer<-felm(DemVotesMajorPercent~ f4_d_cz_HQexo + intd_f4_d_cz_HQexo + f4_d_cz_HQin + intd_f4_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

f4_main_cfe<-tidy(f4_main_cfe, conf.int = TRUE, se.type = "cluster")
f4_main_cfe<-f4_main_cfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "f4_d_cz_HQexo" | term== "f4_d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
f4_main_cfer<-tidy(f4_main_cfer, conf.int = TRUE, se.type = "cluster")
f4_main_cfer<-f4_main_cfer %>% mutate(type="Under Republican Governor") %>% filter(term== "f4_d_cz_HQexo" | term== "f4_d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(f4_main_cfe, f4_main_cfer)
all$term[all$term=="f4_d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="f4_d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=6)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE))-> p
p

ggsave(p, file="Fig_elecyear.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A9: Estimation Results Based on LDV models
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

l1_main_cfe<-felm(DemVotesMajorPercent~ l1DemVotesMajorPercent + d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear +cid| 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
l1_main_cfer<-felm(DemVotesMajorPercent~ l1DemVotesMajorPercent + d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin | stateyear +cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

l1_main_cfe<-tidy(l1_main_cfe, conf.int = TRUE, se.type = "cluster")
l1_main_cfe<-l1_main_cfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
l1_main_cfer<-tidy(l1_main_cfer, conf.int = TRUE, se.type = "cluster")
l1_main_cfer<-l1_main_cfer %>% mutate(type="Under Republican Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(l1_main_cfe, l1_main_cfer)
all$term[all$term=="d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=6)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE))-> p
p

ggsave(p, file="Fig_ldv.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A10: Estimation Results Based on CZ
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data1_cz<-read.csv("czdata.csv")


cz_main_czfe<-felm(DemVotesMajorPercent_st~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + czone_state | 0 | czone_state, data=data1_cz, weight=data1_cz$cz_pop[!is.na(data1_cz$cz_pop)])
cz_main_czfer<-felm(DemVotesMajorPercent_st~ d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin | stateyear + czone_state | 0 | czone_state, data=data1_cz, weight=data1_cz$cz_pop[!is.na(data1_cz$cz_pop)])

cz_main_czfe<-tidy(cz_main_czfe, conf.int = TRUE, se.type = "cluster")
cz_main_czfe<-cz_main_czfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
cz_main_czfer<-tidy(cz_main_czfer, conf.int = TRUE, se.type = "cluster")
cz_main_czfer<-cz_main_czfer %>% mutate(type="Under Republican Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(cz_main_czfe, cz_main_czfer)
all$term[all$term=="d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=6)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE))-> p
p

ggsave(p, file="Fig_CZ.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)



#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A11: Estimation Results without weihts
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

main_cfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1)
main_cfer<-felm(DemVotesMajorPercent~ d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1)

main_cfe<-tidy(main_cfe, conf.int = TRUE, se.type = "cluster")
main_cfe<-main_cfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
main_cfer<-tidy(main_cfer, conf.int = TRUE, se.type = "cluster")
main_cfer<-main_cfer %>% mutate(type="Under Republican Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(main_cfe, main_cfer)
all$term[all$term=="d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=10)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=20),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE, override.aes = list(size = 2)))-> p
p


ggsave(p, file="Fig_noweight.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)



#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A12: Estimation Results Excluding the Counties that Had Never Experienced Interstate HQ Relocations.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data_only<-data1[!is.na(data1$total_d_cz_HQexo | data1$total_d_cz_HQin),]

only_main_cfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data_only, weight=data_only$pop[!is.na(data_only$pop)])
only_main_cfer<-felm(DemVotesMajorPercent~ d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data_only, weight=data_only$pop[!is.na(data_only$pop)])

main_cfe<-tidy(only_main_cfe, conf.int = TRUE, se.type = "cluster")
main_cfe<-main_cfe %>% mutate(type="Under Democratic Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)
main_cfer<-tidy(only_main_cfer, conf.int = TRUE, se.type = "cluster")
main_cfer<-main_cfer %>% mutate(type="Under Republican Governor") %>% filter(term== "d_cz_HQexo" | term== "d_cz_HQin") %>% 
  mutate(estimate= -1*estimate) %>% mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)  %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(main_cfe, main_cfer)
all$term[all$term=="d_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="d_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]

all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red", "blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=10)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-3,3)) +
  theme(
    axis.text.y = element_text(size=18, face="bold"),
    axis.text.x = element_text(size=18),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 14, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.75, 0.10),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE, override.aes = list(size = 2)))-> p
p


ggsave(p, file="Fig_onlycase.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A13: Partial Residual Plots
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

exo_resid<-felm(DemVotesMajorPercent~ int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
in_resid<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
int_exo_resid<-felm(DemVotesMajorPercent~ d_cz_HQexo+ d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
int_in_resid<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin  | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

exo_resid <- residuals(exo_resid)   
in_resid <- residuals(in_resid)   
int_exo_resid <- residuals(int_exo_resid)   
int_in_resid <- residuals(int_in_resid)   

y_exo_resid<-felm(d_cz_HQexo~ int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
y_in_resid<-felm(d_cz_HQin~ d_cz_HQexo + int_d_cz_HQexo + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
y_int_exo_resid<-felm(int_d_cz_HQexo~ d_cz_HQexo+ d_cz_HQin + int_d_cz_HQin | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
y_int_in_resid<-felm(int_d_cz_HQin~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin  | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

y_exo_resid <- residuals(y_exo_resid)   
y_in_resid <- residuals(y_in_resid)   
y_int_exo_resid <- residuals(y_int_exo_resid)   
y_int_in_resid <- residuals(y_int_in_resid)   

df<-data.frame(exo_resid, in_resid, int_exo_resid, int_in_resid, y_exo_resid, y_in_resid, y_int_exo_resid, y_int_in_resid)

ggplot(df, aes(x=exo_resid, y=y_exo_resid)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  #  (by default includes 95% confidence region)
  scale_x_continuous(name="e(Democratic Party Vote Share | X)")+
  scale_y_continuous(name="e(Headquarters Outflow | X)")+
  ggtitle("Headquarters Outflow")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color='black'),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
ggsave("Fig_exo.pdf")

ggplot(df, aes(x=in_resid, y=y_in_resid)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  #  (by default includes 95% confidence region)
  scale_x_continuous(name="e(Democratic Party Vote Share | X)")+
  scale_y_continuous(name="e(Headquarters Inflow | X)")+
  ggtitle("Headquarters Inflow")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color='black'),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
ggsave("Fig_in.pdf")


ggplot(df, aes(x=int_exo_resid, y=y_int_exo_resid)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  #  (by default includes 95% confidence region)
  scale_x_continuous(name="e(Democratic Party Vote Share | X)")+
  scale_y_continuous(name="e(Headquarters Outflow*Republican Governor | X)")+
  ggtitle("Headquarters Outflow*Republican Governor")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color='black'),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
ggsave("Fig_exo_int.pdf")

ggplot(df, aes(x=int_in_resid, y=y_int_in_resid)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  #  (by default includes 95% confidence region)
  scale_x_continuous(name="e(Democratic Party Vote Share | X)")+
  scale_y_continuous(name="e(Headquarters Inflow*Republican Governor | X)")+
  ggtitle("Headquarters Inflow*Republican Governor")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color='black'),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
ggsave("Fig_in_int.pdf")


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A8: Estimation results (DV = Democratic GOV Vote share): effects off the president's partisanship
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

presc_mainczfe_noint<-felm(DemVotesMajorPercent~ d_cz_HQexo  + d_cz_HQin + int_d_cz_HQexo_pres + int_d_cz_HQin_pres| stateyear + czone_state | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
presc_mainczfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin + int_d_cz_HQexo_pres + int_d_cz_HQin_pres | stateyear + czone_state | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
presc_mainczfe_ctrl<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin + int_d_cz_HQexo_pres + int_d_cz_HQin_pres + midterm + sameparty + inc_party_r | czone_state | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

presc_maincfe_noint<-felm(DemVotesMajorPercent~ d_cz_HQexo + d_cz_HQin + int_d_cz_HQexo_pres + int_d_cz_HQin_pres | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
presc_maincfe<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin + int_d_cz_HQexo_pres + int_d_cz_HQin_pres | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])
presc_maincfe_ctrl<-felm(DemVotesMajorPercent~ d_cz_HQexo + int_d_cz_HQexo + d_cz_HQin + int_d_cz_HQin + int_d_cz_HQexo_pres + int_d_cz_HQin_pres + midterm + sameparty + inc_party_r | cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

presc_maincfer<-felm(DemVotesMajorPercent~ d_cz_HQexo + intd_d_cz_HQexo + d_cz_HQin + intd_d_cz_HQin + int_d_cz_HQexo_pres + int_d_cz_HQin_pres | stateyear + cid | 0 | czone_state, data=data1, weight=data1$pop[!is.na(data1$pop)])

stargazer(presc_mainczfe_noint, presc_mainczfe, presc_mainczfe_ctrl, presc_maincfe_noint, presc_maincfe, presc_maincfe_ctrl,
          #dep.var.caption = "\\emph{Dependent Variable - Democratic Governor Vote Share:}",
          dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          omit = "^DemVotesMajorPercent",
          order = c("d_cz_HQin","int_d_cz_HQin", "int_d_cz_HQin_pres","d_cz_HQexo","int_d_cz_HQexo","int_d_cz_HQexo_pres","midterm","sameparty","inc_party_r"),
          covariate.labels = c("Headquarters Inflow", "Headquarters Inflow X Republican Governor", "Headquarters Inflow X Republican President",
                               "Headquarters Outflow", "Headquarters Outflow X Republican Governor", "Headquarters Outflow X Republican President",
                               "Midterm Election", "Same Party President", "Republican Governor"
          ),
          float=F,
          add.lines = list(c("State-Year fixed effects","Yes","Yes","No","Yes","Yes","No"),c("Commuting zone fixed effects","Yes","Yes","Yes","No","No","No"),c("County fixed effects","No","No","No","Yes","Yes","Yes")),
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          notes = "Standard errors clustered by CZ.",notes.align = "l",
          out = "Tab_presc_final.tex")


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A9: Estimation results: Electoral Effects of HQ Relocation on Presidential Elections.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

presdv<-read.csv("presdvdata.csv")


presdv_mainczfe_noint<-felm(DemVotesMajorPercent_pr~ d_cz_HQexo_pr  + d_cz_HQin_pr | stateyear + czone_state | 0 | czone_state, data=presdv, weight=presdv$pop[!is.na(presdv$pop)])
presdv_mainczfe<-felm(DemVotesMajorPercent_pr~ d_cz_HQexo_pr + int_d_cz_HQexo_pr + d_cz_HQin_pr + int_d_cz_HQin_pr  | stateyear + czone_state | 0 | czone_state, data=presdv, weight=presdv$pop[!is.na(presdv$pop)])
presdv_maincfe_noint<-felm(DemVotesMajorPercent_pr~ d_cz_HQexo_pr + d_cz_HQin_pr  | stateyear + cid | 0 | czone_state, data=presdv, weight=presdv$pop[!is.na(presdv$pop)])
presdv_maincfe<-felm(DemVotesMajorPercent_pr~ d_cz_HQexo_pr + int_d_cz_HQexo_pr + d_cz_HQin_pr + int_d_cz_HQin_pr  | stateyear + cid | 0 | czone_state, data=presdv, weight=presdv$pop[!is.na(presdv$pop)])

stargazer(presdv_mainczfe_noint, presdv_mainczfe, presdv_maincfe_noint, presdv_maincfe,
          #dep.var.caption = "\\emph{Dependent Variable - Democratic Party Vote Share:}",
          dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          omit = "^DemVotesMajorPercent",
          order = c("d_cz_HQin_pr","int_d_cz_HQin_pr","d_cz_HQexo_pr","int_d_cz_HQexo_pr"),
          covariate.labels = c("Headquarters Inflow", "Headquarters Inflow X Republican President",
                               "Headquarters Outflow", "Headquarters Outflow X Republican President"
          ),
          float=F,
          add.lines = list(c("State-Year fixed effects","Yes","Yes","Yes","Yes"),c("Commuting zone fixed effects","Yes","Yes","No","No"),c("County fixed effects","No","No","Yes","Yes")),
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          notes = "Standard errors clustered by CZ.",notes.align = "l",
          out = "Tab_presdv_final.tex")


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A14: Estimation Results (DV = Gubernatorial Approval Ratings) Using CCES data, 2006- 2015
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data_cces_full<-read.csv("ccesfulldata.csv")

datadd<-subset(data_cces_full, inc_party_d==1 )
datarr<-subset(data_cces_full, inc_party_r==1 )

est_d <-felm(dv_approval_gov ~ dd_cz_HQexo + dd_cz_HQin + pid7_ + female + black + hisp + asian + college + income  + employed | stateyear+fips | 0 |czone_state, weights=datadd$weight, data=datadd)
est_r <-felm(dv_approval_gov ~ dd_cz_HQexo + dd_cz_HQin + pid7_ + female + black + hisp + asian + college + income  + employed | stateyear+fips | 0 |czone_state, weights=datarr$weight, data=datarr)

estd<-tidy(est_d, conf.int = TRUE, se.type = "cluster")
estr<-tidy(est_r, conf.int = TRUE, se.type = "cluster")

estd<-estd %>% mutate(type="Under Democratic Governor") %>% filter(term== "dd_cz_HQexo" | term== "dd_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)%>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

estr<-estr %>% mutate(type="Under Republican Governor") %>% filter(term== "dd_cz_HQexo" | term== "dd_cz_HQin") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)%>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)


all<-rbind(estd, estr)
all$term[all$term=="dd_cz_HQin"] <- "Headquarters Inflow"
all$term[all$term=="dd_cz_HQexo"] <- "Headquarters Outflow"

all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))
all$ord2 <-factor(all$type, levels = c("Under Republican Governor","Under Democratic Governor"))
#all<- all[order(all$ord),]


all %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord, color=as.factor(ord2))) +
  scale_colour_manual(values=c("red","blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),position=position_dodge(width = .8),size=10)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Incumbent Party Vote Share", expand=c(0,0), limits = c(-0.15,0.15)) +
  theme(
    axis.text.y = element_text(size=20, face="bold"),
    axis.text.x = element_text(size=18),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 20, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.25, 0.9),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE, override.aes = list(size = 2)))-> p
p

ggsave(p, file="Fig_CCES_FULL.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A15: Estimation Results (DV = Approval Ratings of Democratic State Legislature Job Performance under the Republican Governorship) 
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

data_cces <- read.csv("ccespaneldata.csv")
data_cces_rd <-subset(data_cces, inc_party_r==1 & data_cces$demctrl==1 & data_cces$move==0)

rdctrl_caseid<-felm(legapp ~ cz_b_hq_exo + cz_b_hq_in + birthyr_ + pid7_ + ba  + unempl + married + faminc_ | sy+caseid | 0 |czone_state, weights=data_cces_rd$weight, data=data_cces_rd)
rdctrl_caseid<-tidy(rdctrl_caseid, conf.int = TRUE, se.type = "cluster")

rdctrl_caseid<-rdctrl_caseid %>% mutate(type="Under Republican Governor") %>% filter(term== "cz_b_hq_exo" | term== "cz_b_hq_in") %>% 
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error)%>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all_caseid<-rbind(rdctrl_caseid)
all_caseid$term[all_caseid$term=="cz_b_hq_in"] <- "Headquarters Inflow"
all_caseid$term[all_caseid$term=="cz_b_hq_exo"] <- "Headquarters Outflow"

all_caseid$ord <-factor(all_caseid$term, levels = c("Headquarters Outflow","Headquarters Inflow"))

all_caseid %>% 
  filter(estimate!=0) %>% 
  ggplot(aes(estimate, ord)) +
  #scale_colour_manual(values=c("red","blue"))+
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  geom_text(aes(label=round(estimate,2)),vjust=-1,size=10)+
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.6) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1.4) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Effect on Democratic State Legislature Approval \n(Under Republican Governor)", expand=c(0,0), limits = c(-0.8,0.2)) +
  theme(
    axis.text.y = element_text(size=20, face="bold"),
    axis.text.x = element_text(size=18),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.text = element_text(size = 25, colour = "black"),
    #legend.background = element_rect(fill = "white"),
    legend.position = c(0.2, 0.9),
    #legend.position="right",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")-> p
p
ggsave(p, file="Fig_cces_panel_caseid_rdctrl.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A16: Graphical representation of average controlled direct effects (ACDE)
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

#Source = Explaining Causal Findings Without Bias: Detecting and Assessing Direct Effects (Acharya et al 2016)
#https://www.cambridge.org/core/journals/american-political-science-review/article/abs/explaining-causal-findings-without-bias-detecting-and-assessing-direct-effects/D11BEB8666E913A0DCD7D0B9872F5D11

night <- rgb(26,31,30, max=255)
beach <- rgb(227,223,186, max=255)
red60 <- rgb(1,.4,.4)
tangyblue <- rgb(108,189,181, max=255)
purp <- rgb(181.5,145.5, 141.5, max=255)


green1 <- rgb(178,179,159, max = 255)
lightbrown <- rgb(205,140,82, max = 255)

green2 <- rgb(200,201,181, max = 255)
green3 <- rgb(222,223,197, max = 255)
orange <- rgb(61,66,60, max = 255)
aqua <- rgb(240,236,201, max = 255)

lower.95.bound <- function(x) quantile(x, prob = 0.025)
upper.95.bound <- function(x) quantile(x, prob = 0.975)
regTable <- each(mean, sd, lower.95.bound, upper.95.bound)

dt.to.week <- function(x) {
  x <- as.numeric(format(x, "%W"))
  
}

#
# Function: tslag (lagging a vector)
#
tslag <- function(x, d=1) {
  x <- as.vector(x)
  n <- length(x)
  c(rep(NA,d),x)[1:n]
}

pastmin <- function(x) {
  xt <- x
  xt[!is.na(xt)] <- cummin(na.omit(x))
  return(xt)
}
pastmax <- function(x) {
  xt <- x
  xt[!is.na(xt)] <- cummax(na.omit(x))
  return(xt)
}

pastsum <- function(x) {
  xt <- x
  xt[!is.na(xt)] <- cumsum(na.omit(x))
  return(xt)
}

pan.lag <- function(x,ind) {
  unlist(tapply(x,ind, function(x) c(NA,x[-length(x)])))
}

pan.lag <- function(x, ind, lag = 1) {
  unlist(tapply(x,ind, function(x) c(rep(NA, times = lag),x[-((length(x) - lag +1):length(x))])))
}


pan.sum <- function(x, ind) {
  unlist(tapply(x,ind, function(x) {
    xt <- x
    xt[!is.na(xt)] <- cumsum(na.omit(x))
    return(xt)
  }))
}

pan.prod <- function(x,ind) {
  unlist(tapply(x,ind, function(x) {
    xt <- x
    xt[!is.na(xt)] <- cumprod(na.omit(x))
    return(xt)
  }))
}

pan.mean <- function(x,ind) {
  unlist(tapply(x, ind, function(x) rep(mean(x, na.rm=TRUE), length(x))))
}

pan.first <- function(x,ind) {
  unlist(tapply(x, ind, function(x) rep(ifelse(any(x),which(x)[1],NA),length(x)) ))
}


pan.min <- function(x,ind) {
  unlist(tapply(x, ind, function(x) rep(min(x, na.rm=TRUE), length(x))))
}


pan.cummin <- function(x, ind) {
  unlist(tapply(x,ind, function(x) {
    xt <- x
    xt[!is.na(xt)] <- cummin(na.omit(x))
    return(xt)
  }))
}



## ones is an argument to manually set certain observations as having
## weight 1. This is needed due to common support or positivity
## problems. Basically, in certain ranges of the covariates, there are
## either random or structural zeros. For negative advertising, a
## strutural zero occurs when there are no ads. Random zeros generally
## occur in the extremes of the polls and number of ads
## covariates. Extremely uncompetitive races never see negativity and
## extreme competitive race (in the number of ads) almost always go
## negative.

iptw <- function(denominator, numerator, data, id, time, family, subset,
                 ones = NULL, bal.covs = NULL, eval.balance = TRUE) {
  
  require(mgcv)
  require(MASS)
  
  if ("family" %in% class(family)) {
    shortfam <- family$family
  } else {
    shortfam <- family
  }
  
  supported.families <- c("binomial", "gaussian", "ologit")
  if (!(shortfam %in% supported.families)) {
    stop("That family is not supported, silly.")
  }
  
  if (missing(ones)) {
    ones <- rep(FALSE, nrow(data))
  }
  
  ## via the subset.data.frame function
  if (missing(subset)) {
    r <- TRUE
  } else {
    e <- substitute(subset)
    r <- eval(e, data, parent.frame())
    if (!is.logical(r)) {
      stop("'subset' must evaluate to logical")
    }
    r <- r & !is.na(r)
    if (sum(r) == 0) {
      stop("no observations in the subset")
    }
  }
  
  odata <- data
  data <- data[r,]
  ones <- ones[r]
  idmat <- data[,c(id, time)]
  d.fit <- rep(NA, nrow(data))
  n.fit <- rep(NA, nrow(data))
  names(d.fit) <- rownames(data)
  names(n.fit) <- rownames(data)
  trvar <- all.vars(denominator)[1]
  tr <- data[, trvar]
  
  if (shortfam %in% c("binomial", "gaussian")) {
    dem <- gam(denominator, data = data[!ones,], family = family)
    nom <- gam(numerator, data = data[!ones,], family = family)
  }
  if (shortfam == "ologit") {
    dem <- polr(denominator, data = data[!ones,], Hess = TRUE)
    nom <- polr(numerator, data = data[!ones,], Hess = TRUE)
  }
  
  if (length(nom) > 1) {
    d.nms <- rownames(model.matrix(dem))
    n.nms <- rownames(model.matrix(nom))
    if (shortfam == "binomial") {
      d.fit[d.nms] <- dem$fitted.values
      n.fit[n.nms] <- nom$fitted.values
      
      d.pr.tr <- ifelse(tr == 1, d.fit, 1-d.fit)
      n.pr.tr <- ifelse(tr == 1, n.fit, 1-n.fit)
    }
    if (shortfam == "gaussian") {
      dem.dens <- dnorm(model.frame(dem)[,1], dem$fitted.values,
                        sqrt(summary(dem)$dispersion)[1])
      nom.dens <- dnorm(model.frame(nom)[,1], nom$fitted.values,
                        sqrt(summary(nom)$dispersion)[1])
      d.fit[d.nms] <- dem.dens
      n.fit[n.nms] <- nom.dens
      
      d.pr.tr <- d.fit
      n.pr.tr <- n.fit
    }
    if (shortfam == "ologit") {
      d.indic <- model.matrix( ~ dem$model[,1]-1)
      n.indic <- model.matrix( ~ nom$model[,1]-1)
      d.fit[d.nms] <- rowSums(d.indic * dem$fitted.values)
      n.fit[n.nms] <- rowSums(n.indic * nom$fitted.values)
      d.pr.tr <- d.fit
      n.pr.tr <- n.fit
    }
  } else {
    d.nms <- rownames(model.matrix(dem))
    
    d.fit[d.nms] <- dem$fitted.values
    n.fit[TRUE] <- nom
    d.pr.tr <- ifelse(tr == 1, d.fit, 1-d.fit)
    n.pr.tr <- ifelse(tr == 1, n.fit, 1-n.fit)
  }
  
  sw <- n.pr.tr/d.pr.tr
  names(sw) <- rownames(data)
  sw[ones] <- 1
  sw <- pan.prod(sw, idmat[,1])
  wtab <- data.frame(idmat[,1], idmat[,2], sw = sw, d.pscore = d.fit,
                     n.pscore = n.fit)
  colnames(wtab)[1:2] <- c(id, time)
  out <- list(sw = sw, wtab = wtab, d.mod = dem, n.mod = nom, d.pscore = d.fit,
              n.pscore = n.fit, ivar = id, tvar = time, subset = r, ones = ones)
  class(out) <- "iptw"
  if (shortfam == "binomial" && eval.balance) {
    bal.out <- balance.iptw(out, data = odata, add.covs = bal.covs)
    out$balance <- bal.out
  }
  return(out)
}

summary.iptw <- function(x) {
  print(x$balance[,c(3,6)])
}

plot.iptw <- function(x, logw = TRUE, ...) {
  if (logw) {
    x$wtab$sw <- log(x$wtab$sw)
  }
  colnames(x$wtab)[1:2] <- c("ivar", "tvar")
  tsum <- cast(na.omit(melt(x$wtab, id.variables = c("ivar", "tvar"),
                            measure=c("sw"))), tvar ~ ., summary)
  subtle.boxplot(tsum, ...)
  if (logw) {
    abline(h = 0, col = "grey")
  } else {
    abline(h = 1, col = "grey")
  }
  invisible(tsum)
}

merge.iptw <- function(x, data) {
  data$sw <- data$d.pscore <- data$n.pscore <- NULL
  data <- merge(data, x$wtab, by = c(x$ivar, x$tvar), all.x = TRUE)
  return(data)
}

sens.adj <- function(y, a, a.fits, alpha, confound.func = function(x,y) {x}) {
  prob.astar <- ifelse(a == 0, a.fits, 1 - a.fits)
  adjust <- prob.astar
  alpha.out <- confound.func(alpha, a)
  ionic <- alpha.out * prob.astar
  #  ionic <- t(matrix(alpha.out, ncol = 1) %*% prob.astar)
  
  adjust <- pastsum(ionic)
  
  y.alpha <- y - adjust
  return(y.alpha)
}



subtle.boxplot <- function(sum, xlab = "", ylab = "") {
  plot(x = NULL, y = NULL, pch = 19, axes = FALSE,
       ylim = c(min(sum$Min), max(sum$Max)), xlim =
         range(sum[,1]), xlab = xlab, ylab = ylab)
  axis(side = 1)
  axis(side = 2, las = 2)
  
  segments(x0 = sum[,1], x1 = sum[,1],
           y0 = sum$Min, y1 = sum$Max, lwd = 1,
           col = rgb(0.25, 0.25, 0.25, alpha = 0.25))
  rect(xleft = sum[,1]-0.4, xright = sum[,1]+0.4,
       ybottom = sum$X1st.Qu., ytop = sum$X3rd.Qu.,
       col = rgb(0.75, 0.75, 0.75), border = FALSE)
  points(x = sum[,1], y = sum$Min, pch = 19,
         col = rgb(0.75, 0.75, 0.75))
  points(x = sum[,1], y = sum$Max, pch = 19,
         col = rgb(0.75, 0.75, 0.75))
  segments(x0 = sum[,1]-0.4, x1 = sum[,1] + 0.4,
           y0 = sum$Mean, y1 = sum$Mean, lwd = 3)
  invisible()
}


balance.iptw <- function(obj, data, add.covs = NULL) {
  require(survey)
  
  
  d.form <- interpret.gam(obj$d.mod$formula)$fake.formula
  d.vars <- all.vars(d.form[[3]])
  tr.var <- all.vars(d.form)[1]
  n.form <- interpret.gam(obj$n.mod$formula)$fake.formula
  n.vars <- all.vars(n.form[[3]])
  n.form <- as.character(n.form)[3]
  covs <- d.vars[!(d.vars %in% n.vars)]
  covs <- unique(c(covs, add.covs))
  
  data <- data[obj$subset, c(obj$ivar, obj$tvar, tr.var, covs, n.vars)]
  #data$ones <- obj$ones
  data$sw <- obj$sw
  
  id.form <- as.formula(paste("~",obj$ivar))
  
  causal.des <- svydesign(ids = id.form, weights = ~sw,
                          data = data[!obj$ones & !is.na(obj$sw),])
  
  all.bal <- panel.balance(treat = tr.var, bal.covs = covs, n.form = n.form,
                           design = causal.des)
  return(all.bal)
  
}

panel.balance <- function(treat, bal.covs, n.form, design, subset = NULL) {
  
  if (!is.null(subset)) {
    subset <- eval(parse(text = subset))
  }
  unweighted <- matrix(NA, nrow = length(bal.covs), ncol = 2)
  weighted <- unweighted
  
  for (i in seq(along.with=bal.covs)) {
    new.form <- as.formula(paste(bal.covs[i], "~", treat, "+", n.form))
    mc <- design$call
    mc$weights <- as.formula("~1")
    unw.design <- eval(mc, parent.frame())
    
    mc[[1]] <- as.name("lm")
    mc$formula <- new.form
    mc$subset <- subset
    unw <- svyglm(new.form, subset = subset, design = unw.design)
    unweighted[i, ] <- summary(unw)$coef[treat, 1:2]
    
    wei <- svyglm(new.form, subset = subset, design = design)
    weighted[i, ] <- summary(wei)$coef[treat, 1:2]
  }
  out <- cbind(unweighted, unweighted[,1]/unweighted[,2],
               weighted, weighted[,1]/weighted[,2])
  
  rownames(out) <- bal.covs
  colnames(out) <- c("Unweighed Coef.", "Unweighted SE", "USCoef",
                     "Weighted Coef.", "Weighted SE", "WSCoef")
  return(out)
}


## confint for gee LM object
confint.geelm <- function(object, parm, level = 0.95, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                             pct))
  ses <- summary(object)$coefficients[parm,2]
  ci[] <- cf[parm] + ses %o% fac
  ci
}

stata.codebook <- function(x) {
  cb <- data.frame(attr(x, "var.labels"))
  rownames(cb) <- names(x)
  cb
}


snmm.var.single <- function(mods, blip.vars, data) {
  WW <- lapply(mods, model.matrix)
  W.tilde <- WW[-length(WW)]
  for (w in 1:length(W.tilde)) W.tilde[[w]][,-blip.vars] <- 0
  kvec <- sapply(WW, ncol)
  K <- sum(kvec)
  iii <- cumsum(kvec)
  ii <- cumsum(c(0,kvec[-length(kvec)])) + 1
  Ghat <- matrix(0, nrow = K, ncol = K)
  meat <- list()
  for (i in 1:length(kvec)) {
    Ghat[ii[i]:iii[i], ii[i]:iii[i]] <- t(WW[[i]]) %*% WW[[i]]
    meat[[i]] <- estfun(mods[[i]])
    for (j in i:length(kvec)) {
      if (i < j) {
        nms <- intersect(rownames(WW[[i]]), rownames(WW[[j]]))
        Ghat[ii[j]:iii[j], ii[i]:iii[i]] <- t(WW[[j]][nms,]) %*% W.tilde[[i]][nms,]
      }
    }
  }
  meat.big <- do.call(cbind, meat)
  NN <- nrow(WW[[1]])
  dfc <- ((NN-1)/(NN-K))
  vcv <- dfc*solve(crossprod(Ghat)) %*% t(Ghat) %*% crossprod(meat.big) %*% Ghat %*% solve(crossprod(Ghat))
  return(vcv)
}

seq.g.var <- function(mod.first, mod.direct, med.vars) {
  require(sandwich)
  mat.x <-  model.matrix(mod.direct)
  mat.first <-  model.matrix(mod.first)
  n <- nrow(mat.x)
  Fhat <- crossprod(mat.x, mat.first)/n
  Fhat[, !(colnames(mat.first) %in% med.vars)] <- 0
  Mhat.inv <- solve(crossprod(mat.first)/n)
  ghat <- t(estfun(mod.direct)) + Fhat %*% Mhat.inv %*% t(estfun(mod.first))
  meat <- crossprod(t(ghat))/n
  bread <- (t(mat.x)%*%mat.x)/n
  vv <- (n/(n-ncol(mat.x)))*(solve(bread) %*% meat %*% solve(bread))/n
  return(vv)
}


first <- lm(m_DemVotesMajorPercent ~ m_econgrowth + m_d_cz_HQexo + m_int_d_cz_HQexo + m_d_cz_HQin + m_int_d_cz_HQin , data = data1)
direct <- lm(I(m_DemVotesMajorPercent - coef(first)["m_econgrowth"]*m_econgrowth) ~ m_d_cz_HQexo + m_int_d_cz_HQexo + m_d_cz_HQin + m_int_d_cz_HQin, data = data1)
#subset = rownames(fear) %in% rownames(model.matrix(first)))

summary(direct)
direct.vcov <- seq.g.var(first, direct, "m_econgrowth")
coeftest(direct, vcov = direct.vcov)

#1. d_cz_HQexo
rho <- seq(-0.9,0.9, by = 0.05)
acde.sens <- rep(NA, times = length(rho))
acde.sens.se <- rep(NA, times = length(rho))
acde.sens_in <- rep(NA, times = length(rho))
acde.sens_in.se <- rep(NA, times = length(rho))
res.y <- residuals(lm(m_DemVotesMajorPercent ~ m_econgrowth + m_d_cz_HQexo + m_int_d_cz_HQexo + m_d_cz_HQin + m_int_d_cz_HQin , data = data1))
res.m <- residuals(lm(m_econgrowth ~ m_d_cz_HQexo + m_int_d_cz_HQexo + m_d_cz_HQin + m_int_d_cz_HQin , data = data1))
rho.tilde <- cor(res.y, res.m)
m.fixed <- coef(first)["m_econgrowth"] - rho*sd(res.y)*sqrt((1-rho.tilde^2)/(1-rho^2))/sd(res.m)
n <- nrow(model.matrix(direct))
for(i in 1:length(rho)) {
  sens.direct  <- lm(I(m_DemVotesMajorPercent - m.fixed[i]*m_econgrowth) ~ m_d_cz_HQexo + m_int_d_cz_HQexo + m_d_cz_HQin + m_int_d_cz_HQin, data = data1)
  acde.sens[i] <- coef(sens.direct)["m_d_cz_HQexo"]
  sens.vcov <- seq.g.var(first, sens.direct, "m_econgrowth")
  acde.sens.se[i] <- sqrt(sens.vcov["m_d_cz_HQexo", "m_d_cz_HQexo"])
  acde.sens_in[i] <- coef(sens.direct)["m_d_cz_HQin"]
  acde.sens_in.se[i] <- sqrt(sens.vcov["m_d_cz_HQin", "m_d_cz_HQin"])
}

ci.hi <- acde.sens + 1.96 * acde.sens.se
ci.lo <- acde.sens - 1.96 * acde.sens.se

pdf("Fig_ACDE_dem_out.pdf",width=5,height=5)
plot(rho, acde.sens, type = 'n', xlab = bquote("Correlation between mediator and outcome errors" ~~ (rho)),
     ylab = "Estimated Average Controlled Direct Effects", bty = "n", las = 1, ylim=c(-3,3), xlim=c(-1,1))
polygon(x = c(rho, rev(rho)), y = c(ci.lo, rev(ci.hi)), col = "grey70", border = NA)
lines(rho, acde.sens, lwd = 2)
abline(v=0, lty = 2)
abline(h=0, lty = 2)
dev.off()

#2. d_cz_HQin

ci.hi <- acde.sens_in + 1.96 * acde.sens_in.se
ci.lo <- acde.sens_in - 1.96 * acde.sens_in.se

pdf("Fig_ACDE_dem_in.pdf",width=5,height=5)
plot(rho, acde.sens_in, type = 'n', xlab = bquote("Correlation between mediator and outcome errors" ~~ (rho)),
     ylab = "Estimated Average Controlled Direct Effects", bty = "n", las = 1, ylim=c(-3,3), xlim=c(-1,1))
polygon(x = c(rho, rev(rho)), y = c(ci.lo, rev(ci.hi)), col = "grey70", border = NA)
lines(rho, acde.sens_in, lwd = 2)
abline(v=0, lty = 2)
abline(h=0, lty = 2)
dev.off()

#3. REP d_cz_HQexo
rho <- seq(-0.9,0.9, by = 0.05)
acde.sens <- rep(NA, times = length(rho))
acde.sens.se <- rep(NA, times = length(rho))
acde.sens_in <- rep(NA, times = length(rho))
acde.sens_in.se <- rep(NA, times = length(rho))
res.y <- residuals(lm(m_DemVotesMajorPercent ~ m_econgrowth + m_d_cz_HQexo + m_intd_d_cz_HQexo + m_d_cz_HQin + m_intd_d_cz_HQin , data = data1))
res.m <- residuals(lm(m_econgrowth ~ m_d_cz_HQexo + m_intd_d_cz_HQexo + m_d_cz_HQin + m_intd_d_cz_HQin , data = data1))
rho.tilde <- cor(res.y, res.m)
m.fixed <- coef(first)["m_econgrowth"] - rho*sd(res.y)*sqrt((1-rho.tilde^2)/(1-rho^2))/sd(res.m)
n <- nrow(model.matrix(direct))
for(i in 1:length(rho)) {
  sens.direct  <- lm(I(m_DemVotesMajorPercent - m.fixed[i]*m_econgrowth) ~ m_d_cz_HQexo + m_intd_d_cz_HQexo + m_d_cz_HQin + m_intd_d_cz_HQin, data = data1)
  acde.sens[i] <- coef(sens.direct)["m_d_cz_HQexo"]
  sens.vcov <- seq.g.var(first, sens.direct, "m_econgrowth")
  acde.sens.se[i] <- sqrt(sens.vcov["m_d_cz_HQexo", "m_d_cz_HQexo"])
  acde.sens_in[i] <- coef(sens.direct)["m_d_cz_HQin"]
  acde.sens_in.se[i] <- sqrt(sens.vcov["m_d_cz_HQin", "m_d_cz_HQin"])
}

ci.hi <- acde.sens + 1.96 * acde.sens.se
ci.lo <- acde.sens - 1.96 * acde.sens.se

pdf("Fig_ACDE_rep_out.pdf",width=5,height=5)
plot(rho, acde.sens, type = 'n', xlab = bquote("Correlation between mediator and outcome errors" ~~ (rho)),
     ylab = "Estimated Average Controlled Direct Effects", bty = "n", las = 1, ylim=c(-3,3), xlim=c(-1,1))
polygon(x = c(rho, rev(rho)), y = c(ci.lo, rev(ci.hi)), col = "grey70", border = NA)
lines(rho, acde.sens, lwd = 2)
abline(v=0, lty = 2)
abline(h=0, lty = 2)
dev.off()

#4.REP d_cz_HQin

ci.hi <- acde.sens_in + 1.96 * acde.sens_in.se
ci.lo <- acde.sens_in - 1.96 * acde.sens_in.se

pdf("Fig_ACDE_rep_in.pdf",width=5,height=5)
plot(rho, acde.sens_in, type = 'n', xlab = bquote("Correlation between mediator and outcome errors" ~~ (rho)),
     ylab = "Estimated Average Controlled Direct Effects", bty = "n", las = 1, ylim=c(-3,3), xlim=c(-1,1))
polygon(x = c(rho, rev(rho)), y = c(ci.lo, rev(ci.hi)), col = "grey70", border = NA)
lines(rho, acde.sens_in, lwd = 2)
abline(v=0, lty = 2)
abline(h=0, lty = 2)
dev.off()

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A10: OLS Results: Local Economic Effects of HQ Relocation.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

income1<-felm(ac_log_income_pc ~ l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
income2<-felm(ac_log_income_pc ~ l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
income3<-felm(ac_log_income_pc ~ l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])

wage1<-felm(ac_log_wage_pc ~ l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
wage2<-felm(ac_log_wage_pc ~ l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
wage3<-felm(ac_log_wage_pc ~ l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])

empl1<-felm(ac_log_empl_pc ~ l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
empl2<-felm(ac_log_empl_pc ~ l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
empl3<-felm(ac_log_empl_pc ~ l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])

econg1<-felm(econgrowth ~ l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
econg2<-felm(econgrowth ~ l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
econg3<-felm(econgrowth ~ l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])


stargazer(income1, income2, income3, wage1, wage2, wage3, empl1, empl2, empl3, econg1, econg2, econg3,
          #dep.var.caption = "\\emph{\\emph{DV = Local Economic Indicators}}",
          #dep.var.labels.include = FALSE,
          dep.var.labels = c("$Delta$ Income per capita(logged)", "$Delta$ Wage per capita(logged)","$Delta$ Employment per capita(logged)", "Economic Growth"),
          omit = "^DemVotesMajorPercent",
          order = c("l1d_cz_HQin","l2d_cz_HQin","l3d_cz_HQin", "l1d_cz_HQexo","l2d_cz_HQexo","l3d_cz_HQexo"),
          covariate.labels = c("Headquarters Inflow (t-1)", "Headquarters Inflow (t-2)", "Headquarters Inflow (t-3)",
                               "Headquarters Outflow (t-1)", "Headquarters Outflow (t-2)" , "Headquarters Outflow (t-3)" 
          ),
          float=F,
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          column.sep.width = "10pt",
          notes = "Standard errors clustered by CZ.",notes.align = "l",
          out = "Tab_econeffect_final.tex")


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A11: LDV (Lagged Dependent Variable) Model Results: Local Economic Effects of HQ Relocation.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

ldv_income1<-felm(ac_log_income_pc ~ l1ac_log_income_pc + l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_income2<-felm(ac_log_income_pc ~ l1ac_log_income_pc + l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_income3<-felm(ac_log_income_pc ~ l1ac_log_income_pc + l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])

ldv_wage1<-felm(ac_log_wage_pc ~ l1ac_log_wage_pc + l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_wage2<-felm(ac_log_wage_pc ~ l1ac_log_wage_pc +l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_wage3<-felm(ac_log_wage_pc ~ l1ac_log_wage_pc +l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])

ldv_empl1<-felm(ac_log_empl_pc ~ l1ac_log_empl_pc + l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_empl2<-felm(ac_log_empl_pc ~ l1ac_log_empl_pc + l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_empl3<-felm(ac_log_empl_pc ~ l1ac_log_empl_pc + l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])

ldv_econg1<-felm(econgrowth ~ l1econgrowth + l1d_cz_HQexo + l1d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_econg2<-felm(econgrowth ~ l1econgrowth + l2d_cz_HQexo + l2d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])
ldv_econg3<-felm(econgrowth ~ l1econgrowth + l3d_cz_HQexo + l3d_cz_HQin | cid+year | 0 | czone, data=lecon, weight=lecon$pop[!is.na(lecon$pop)])


stargazer(ldv_income1, ldv_income2, ldv_income3, ldv_wage1, ldv_wage2, ldv_wage3, ldv_empl1, ldv_empl2, ldv_empl3, ldv_econg1, ldv_econg2, ldv_econg3,
          #dep.var.caption = "\\emph{DV = Local Economic Indicators}",
          #dep.var.labels.include = FALSE,
          dep.var.labels = c("$/Delta$ Income per capita(logged)", "$/Delta$ Wage per capita(logged)","$/Delta$ Employment p.c.(logged)", "Economic Growth"),
          omit = "^DemVotesMajorPercent",
          order = c("l1d_cz_HQin","l2d_cz_HQin","l3d_cz_HQin", "l1d_cz_HQexo","l2d_cz_HQexo","l3d_cz_HQexo", "l1ac_log_income_pc", 
                    "l1ac_log_wage_pc","l1ac_log_empl_pc","l1econgrowth"),
          covariate.labels = c("Headquarters Inflow (t-1)", "Headquarters Inflow (t-2)", "Headquarters Inflow (t-3)",
                               "Headquarters Outflow (t-1)", "Headquarters Outflow (t-2)" , "Headquarters Outflow (t-3)" ,
                               "$/Delta$ Income per capita(logged, t-1)", "$/Delta$ Wage per capita(logged, t-1)","$/Delta$ Empl per capita(logged, t-1)", "Econ. Growth (t-1)"
          ),
          float=F,
          #add.lines = list(c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes"),c("County fixed effects","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes")),
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          notes = "Standard errors clustered by CZ. Year and County fixed effects are included.",notes.align = "l",
          out = "Tab_econeffect_ldv_final.tex")

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A17: Responses to the questions, “how much do you think the corporate HQ relocation into your county would hurt or benefit...” and “how much do you think the corporate HQ relocation out of your county would hurt or benefit...” (-3 = hurt very much, 3 = improve very much)
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

surveydata<-read.csv("mturkdata.csv")

a<-surveydata %>%
  summarise_at(c("hq_out_localecon","hq_out_localempl","hq_out_localrep","hq_out_localpride","hq_in_localecon","hq_in_localempl","hq_in_localrep","hq_in_localpride"), mean, na.rm = TRUE)

dat <- data.frame(
  Cate=c("Local Econ", "Employment", "Reputation", "Local Pride", "Local Econ", "Employment", "Reputation", "Local Pride"),
  Level=c("Headquarters Outflow","Headquarters Outflow","Headquarters Outflow","Headquarters Outflow","Headquarters Inflow","Headquarters Inflow","Headquarters Inflow","Headquarters Inflow"))

dat$Cate <- factor(dat$Cate, 
                   levels = c("Local Pride","Reputation","Employment","Local Econ"
                   ))

dat<-cbind(t(a),dat)

ggplot(dat, aes(x=Cate, y=t(a), fill=Level)) + 
  geom_bar(stat="identity", position="identity") +  # error if stat = not included.  don't know why?!
  guides(fill=guide_legend(title="")) +
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c("#99CCFF","#E69F00"))+
  scale_y_continuous(breaks = seq(-3, 3, by = 1), limits = c(-3, 3),expand = c(0, 0))+
  theme(axis.text=element_text(size=15),
        legend.text=element_text(size=15),
        axis.line = element_line(color='black'),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 2, linetype = "solid"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  #ylim(-5,5)+
  coord_flip()

ggsave("tornado_HQ_hurt_benefit.pdf", width=30, height=20, units= "cm", dpi=800)

#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A18: Opinions about drivers of Headquarters Inflow (left) / Headquarters Outflow (right).
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

mean_in<-surveydata %>%
  summarise_at(c("hq_indriver_tax","hq_indriver_labor","hq_indriver_inc","hq_indriver_incother","hq_indriver_soc"), 
               funs(mean), na.rm = TRUE)

sd_in<-surveydata %>%
  summarise_at(c("hq_indriver_tax","hq_indriver_labor","hq_indriver_inc","hq_indriver_incother","hq_indriver_soc"), 
               funs(sd), na.rm = TRUE)
msd_in<-cbind(t(mean_in),t(sd_in))
colnames(msd_in)<-c("mean","sdd")

mean_out<-surveydata %>%
  summarise_at(c("hq_outdriver_tax","hq_outdriver_labor","hq_outdriver_inc","hq_outdriver_incother","hq_outdriver_soc"), 
               funs(mean), na.rm = TRUE)

sd_out<-surveydata %>%
  summarise_at(c("hq_outdriver_tax","hq_outdriver_labor","hq_outdriver_inc","hq_outdriver_incother","hq_outdriver_soc"), 
               funs(sd), na.rm = TRUE)
msd_out<-cbind(t(mean_out),t(sd_out))          
colnames(msd_out)<-c("mean","sdd")

df_in <- data.frame(Level=rep(c("Headquarters Inflow"), each=5),
                    Cate=rep(c("Low\nCorporate Tax", "Low\nLabor Costs", "Generous\nTax Incentives", "Insufficient\nTax Incentives\nOffered by\nOther States", "Low\nSocial Spending"),1),
                    sd=c("SD = 0.82","SD = 1.01","SD =0.87 ","SD = 1.09","SD = 1.07"),
                    len2=c("Mean =  4.05", "Mean =  3.35", "Mean = 4.02", "Mean = 3.42", "Mean = 3.09"))

df_out <- data.frame(Level=rep(c("Headquarters Outflow"), each=5),
                     Cate=rep(c("High\nCorporate Tax", "High\nLabor Costs", "Insufficient\nTax Incentives", "Tax Incentives\nOffered by\nOther States", "High\nSocial Spending"),1),
                     sd=c("SD = 0.85","SD = 1.01","SD =0.88 ","SD = 0.90","SD = 1.04"),
                     len2=c("Mean = 4", "Mean = 3.31", "Mean = 3.83", "Mean = 3.95", "Mean = 3.12"))

df_in<-cbind(df_in,msd_in)
df_in$Cate <- factor(df_in$Cate, levels = c("Low\nCorporate Tax", "Low\nLabor Costs", "Generous\nTax Incentives", "Insufficient\nTax Incentives\nOffered by\nOther States", "Low\nSocial Spending"))

df_out<-cbind(df_out,msd_out)
df_out$Cate <- factor(df_out$Cate, levels = c("High\nCorporate Tax", "High\nLabor Costs", "Insufficient\nTax Incentives", "Tax Incentives\nOffered by\nOther States", "High\nSocial Spending"))


ggplot(data=df_in, aes(x=Cate, y=mean, fill=Cate)) +
  geom_bar(stat="identity", position=position_dodge())+
  #scale_fill_manual(values=c("blue", "red"))+
  geom_text(aes(label=len2), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sd), vjust=3.2, color="white",
            position = position_dodge(0.9), size=3.5)+
  #geom_text(aes(label=dose), vjust=-0.8, color="black",
  #          position = position_dodge(0.9), size=3.5, fontface="bold")+
  xlab("") + ylab("5 Scale (0 = Not at all, 5=Very Much)")+ labs(fill = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=11, face="bold"),
    #axis.text.x = element_blank(),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=14),
    legend.text = element_text(size = 7, colour = "black"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.15, 0.92),
    legend.position="none",
    legend.title = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+labs(x = "")

ggsave("hist_in_driver.pdf", width=20, height=14, units= "cm", dpi=800)


ggplot(data=df_out, aes(x=Cate, y=mean, fill=Cate)) +
  geom_bar(stat="identity", position=position_dodge())+
  #scale_fill_manual(values=c("blue", "red"))+
  geom_text(aes(label=len2), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sd), vjust=3.2, color="white",
            position = position_dodge(0.9), size=3.5)+
  #geom_text(aes(label=dose), vjust=-0.8, color="black",
  #          position = position_dodge(0.9), size=3.5, fontface="bold")+
  xlab("") + ylab("5 Scale (0 = Not at all, 5=Very Much)")+ labs(fill = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=11, face="bold"),
    #axis.text.x = element_blank(),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=14),
    legend.text = element_text(size = 7, colour = "black"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.15, 0.92),
    legend.position="none",
    legend.title = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+labs(x = "")

ggsave("hist_out_driver.pdf", width=20, height=14, units= "cm", dpi=800)



#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A19: Political party that respondents think better attract Headquarters Inflow (left) /deter Headquarters Outflow (right).
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

mean_in_m<-surveydata %>%
  summarise_at(c("r_in_manage","i_in_manage","d_in_manage"), 
               funs(mean), na.rm = TRUE)

sd_in_m<-surveydata %>%
  summarise_at(c("r_in_manage","i_in_manage","d_in_manage"), 
               funs(sd), na.rm = TRUE)
msd_in_m<-cbind(t(mean_in_m),t(sd_in_m))
colnames(msd_in_m)<-c("mean","sdd")

mean_out_m<-surveydata %>%
  summarise_at(c("r_out_manage","i_out_manage","d_out_manage"), 
               funs(mean), na.rm = TRUE)
sd_out_m<-surveydata %>%
  summarise_at(c("r_out_manage","i_out_manage","d_out_manage"), 
               funs(sd), na.rm = TRUE)
msd_out_m<-cbind(t(mean_out_m),t(sd_out_m))
colnames(msd_out_m)<-c("mean","sdd")

df_in_m<- data.frame(supp=rep(c("Republican Party", "Independent","Democratic Party"),1),
                     sd=c("SD = 0.5","SD =0.4","SD =0.45"),
                     len2=c("Mean = 0.51", "Mean = 0.2", "Mean = 0.29"))

df_out_m <- data.frame(supp=rep(c("Republican Party", "Independent","Democratic Party"),1),
                       sd=c("SD = 0.49","SD =0.15","SD =0.48"),
                       len2=c("Mean = 0.46", "Mean = 0.23", "Mean = 0.31"))


df_in_m<-cbind(df_in_m,msd_in_m)
df_out_m<-cbind(df_out_m,msd_out_m)


ggplot(data=df_in_m, aes(x=supp, y=mean, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("blue", "green","red"))+
  geom_text(aes(label=len2), vjust=-0.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sd), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  #geom_text(aes(label=dose), vjust=-0.8, color="black",
  #          position = position_dodge(0.9), size=3.5, fontface="bold")+
  xlab("") + ylab("Proportion")+ labs(fill = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=12, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=14),
    legend.text = element_text(size = 7, colour = "black"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.15, 0.92),
    legend.position="none",
    legend.title = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

ggsave("hist_in_pmanage.pdf", width=20, height=14, units= "cm", dpi=800)


ggplot(data=df_out_m, aes(x=supp, y=mean, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("blue", "green","red"))+
  geom_text(aes(label=len2), vjust=-0.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  geom_text(aes(label=sd), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  #geom_text(aes(label=dose), vjust=-0.8, color="black",
  #          position = position_dodge(0.9), size=3.5, fontface="bold")+
  xlab("") + ylab("Proportion")+ labs(fill = "") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size=12, face="bold"),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=14),
    legend.text = element_text(size = 7, colour = "black"),
    legend.background = element_rect(fill = "white"),
    #legend.position = c(0.15, 0.92),
    legend.position="none",
    legend.title = element_blank(),
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

ggsave("hist_out_pmanage.pdf", width=20, height=14, units= "cm", dpi=800)


#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Table A12: OLS estimation results: partisan differences in the effects of HQ relocation
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

mturk_ols1 <-lm(gov_hq_s ~ treat_in, data=surveydata)
mturk_ols2 <-lm(gov_hq_s ~ treat_in + treat_rep + inrep, data=surveydata)


stargazer(mturk_ols1,mturk_ols2,
          #dep.var.caption = "\\emph{DV = Support for Reelection:}",
          dep.var.labels.include = FALSE,
          #dep.var.labels = c("President", "Senate","House", "Federal Average"),
          #omit = "^DemVotesMajorPercent",
          order = c("treat_in","treat_rep","inrep"),
          covariate.labels = c("Headquarters Inflow", "Republican Governor", "Headquarters Inflow X Republican Governor"),
          float=F,
          add.lines = list(c("Joint H Test","","0.011"),c("","","(0.079)")),
          keep.stat = "n",
          star.cutoffs = c(0.1,0.05,0.01),
          star.char = c("†", "*", "**"),
          omit.table.layout = "n",
          notes = "Standard errors in parentheses.",notes.align = "l",
          out = "mturk_reg_final.tex")




#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# Figure A20: Survey Experiment Results: Partisan Differences in the Effects of HQ Relocation.
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#

surveydata_out <- subset(surveydata, treat_out==1)
surveydata_in <- subset(surveydata, treat_in==1)

out_rep<-lm(gov_hq_s ~ treat_rep, data=surveydata_out)
in_rep<-lm(gov_hq_s ~ treat_rep, data=surveydata_in)

out_rep<-tidy(out_rep, conf.int = TRUE)
out_rep<-out_rep %>% filter(term== "treat_rep") %>% mutate(term="Headquarters Outflow") %>%
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

in_rep<-tidy(in_rep, conf.int = TRUE)
in_rep<-in_rep %>% filter(term== "treat_rep") %>% mutate(term="Headquarters Inflow") %>%
  mutate(lower=estimate-1.96*std.error) %>% mutate(upper=estimate+1.96*std.error) %>%
  mutate(lower90=estimate-1.65*std.error) %>% mutate(upper90=estimate+1.65*std.error)

all<-rbind(out_rep, in_rep)
all$ord <-factor(all$term, levels = c("Headquarters Outflow","Headquarters Inflow"))

all %>% 
  ggplot(aes(estimate, ord, shape=as.factor(ord))) +
  geom_point(position=position_dodge(width = .4),size=4, alpha=0.8) +
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper), position=position_dodge(width = .4), size=0.3) +
  ggstance::geom_pointrangeh(aes(xmin=lower90, xmax=upper90), position=position_dodge(width = .4), size=1) +
  geom_vline(xintercept=0, linetype="solid", color="black") +
  scale_x_continuous("Difference in Support (Under Republican Governor - Under Democratic Governor)", expand=c(0,0), limits = c(-0.35,0.35)) +
  theme(
    axis.text.y = element_text(size=20, face="bold"),
    axis.text.x = element_text(size=18),
    #axis.line.x = element_line(color="black"),
    #axis.line.y = element_line(color="black"),
    axis.title=element_text(size=20),
    legend.position = "none",
    legend.title = element_blank(),
    axis.line = element_line(color='black'),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
    #panel.border = element_blank(),
    #panel.background = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
  )+
  xlab("")+
  ylab("")+
  #  theme_bw()+
  guides(color=guide_legend(reverse = TRUE, override.aes = list(size = 2)))-> p
p

ggsave(p, file="Survey_hq_s.pdf", family="sans", width=30, height=20, units= "cm", dpi=800)

