### Replication code for Study Two in 
### Campbell, R., Cowley, P., Vivyan, N & Wagner, M. (2016) 'Legislator dissent as a valence signal'.
### British Journal of Political Science


### set working directory and options

rm(list = ls()); gc()
options("contrasts" = c("contr.treatment", "contr.treatment"))
setwd("~/Dropbox/MP independence/replication_materials")


### load packages

library(car)
library(ggplot2)
library(rms)
library(stargazer)
library(lmtest)
library(reshape)
library(plyr)

### Create useful functions

add.top <- function(df, new.level){
  to.add <- data.frame(mean = c(NA,NA, 0), ci.lo = c(NA,NA, 0), ci.hi = c(NA,NA, 0),
                       category = rep("", 3), attribute = rep(df$attribute[1],3),
                       level = c("", " ", new.level))
  return(rbind(to.add, df))
}

add.justify <- function(df){
  df$left.justify <- rep(0, nrow(df))
  df$left.justify[2] <- 1
  return(df)
}

## Function to get regression-based AMCE estimates for each attribute level using 
## OLS estimator with clustered SEs (Hainmueller, Hopkins and Yammamoto 2014)
get.amcetab <- function(data, variables){    
  Nvar <- length(variables)
  amce.list <- list(NULL, length = Nvar)
  
  for(i in 1:Nvar){ # get AMCE for each variable attribute
    fmla <- as.formula(paste("mp.preferred ~ ", "factor(", variables[i],")", sep = ""))
    model <- ols(fmla, data = data, x = T, y = T) 
    # NOTE: The data for the model cannot have NAs on any mdoel variable for the the robcov(cluster()) function to work 
    model.clus <- robcov(model, cluster = data$id, method = "efron")
    coef <- model.clus$coef[names(model.clus$coef)!="Intercept"]
    se.clus <- sqrt(diag(model.clus$var))
    se.clus <- se.clus[names(se.clus)!="Intercept"]       
    sub.tab <- data.frame("AMCE" = coef, 
                          "ci.lo" = coef - (1.96*se.clus),
                          "ci.hi" = coef + (1.96*se.clus),
                          "cluster.se" = se.clus)
    sub.tab$category <- names(coef)
    sub.tab <- cbind(sub.tab, colsplit(sub.tab$category, "=", c("attribute","level")))
    sub.tab$level <- as.character(sub.tab$level)    
    if(variables[i] == "mp.rebellion"){# re-order rebellion levels
      sub.tab <- sub.tab[c(2,3,1),]
    }
    row.names(sub.tab) <- NULL
    # add in gaps and baselines
    to.add <- data.frame(AMCE = c(NA,NA, 0), ci.lo = c(NA,NA, 0), ci.hi = c(NA,NA, 0),
                         cluster.se = c(NA,NA, 0),
                         category = rep("", 3), attribute = rep(sub.tab$attribute[1],3),
                         level = c("", " ", "baseline"))
    amce.list [[i]] <- rbind(to.add, sub.tab)
  } 
  amce.tab <- do.call("rbind", amce.list)
  # re-make initial lables column
  amce.tab$category <- paste(amce.tab$attribute, amce.tab$level, sep = ": ")
  # make this into ordered factor
  amce.tab$category <- factor(amce.tab$category, levels = rev(amce.tab$category), order =T)    
  
  return(amce.tab)
}

## Function that calls get.amcetab for multiple predictors and combines results
amce.tab <- function(data, variables, multi = F, same.party = F){
  # data must be a single data frame or a list of data frames (if multi = T)
  # with named elements
  # Also relies on specific ordering of explanatory variables
  if(multi == T & same.party == F){
    amce.tab.list <- list(NA, length = length(data))
    for(i in 1:length(data)){
      tmp <- get.amcetab(data[[i]], variables = variables)
      tmp$set <- rep(names(data)[i], nrow(tmp))
      amce.tab.list[[i]] <- tmp
    }
    amce.tab <- do.call("rbind",amce.tab.list)
    amce.tab$set <- factor(amce.tab$set)
    return(amce.tab)
  }
  if(multi == T & same.party == T){
    amce.tab.list <- list(NA, length = length(data))
    for(i in 1:length(data)){
      vars <- if(grepl("same party", names(data)[i])==T|grepl("Same Party", names(data)[i])==T) variables[2:length(variables)] else variables
      tmp <- get.amcetab(data[[i]], variables = vars)
      tmp$set <- rep(names(data)[i], nrow(tmp))
      amce.tab.list[[i]] <- tmp
    }
    names(amce.tab.list) <- names(data)
    diff.party <- amce.tab.list[grepl("same party", names(amce.tab.list))==F&
                                  grepl("Same Party", names(amce.tab.list))==F]
    same.party <- amce.tab.list[grepl("same party", names(amce.tab.list))==T |
                                  grepl("Same Party", names(amce.tab.list))==T]
    to.add <- data.frame(AMCE = rep(NA, 4), ci.lo = rep(NA, 4), ci.hi =  rep(NA, 4),
                         cluster.se =  rep(NA, 4),
                         category =  diff.party[[1]]$category[1:4], 
                         attribute =  diff.party[[1]]$attribute[1:4], 
                         level = diff.party[[1]]$level[1:4],
                         set = rep(NA, 4))
    for(i in 1:length(data)){                     
      if(grepl("same party", names(data)[i])==T|grepl("Same Party", names(data)[i])==T){
        amce.tab.list[[i]] <- rbind(to.add,amce.tab.list[[i]])
        amce.tab.list[[i]]$set[1:4] <-  amce.tab.list[[i]]$set[5:8]
      }
      else amce.tab.list[[i]] <- amce.tab.list[[i]]
    }     
    amce.tab <- do.call("rbind",amce.tab.list)
    amce.tab$set <- factor(amce.tab$set)
    return(amce.tab)
  } 
  if(multi == F)   {
    get.amcetab(data, variables)
  }
  
}


## Multiple plot function (from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




### Load in Study 2 data: conjoint analysis survey experiment

long.dat <- readRDS("data/study2data.rds")




### Create neat labels for plotting

labels.sub <- c("never", "rarely", "sometimes", "often")

labels.full <- rev(expression(
  "", italic("MP constituency work (baseline = 1 day)"), "2 days", "3 days", "4 days",
  "", italic("MP party (baseline = Conservative)"), "Labour",
  "", italic("MP dissent (baseline = never)"), "rarely", "sometimes", "often",
  "", italic("MP sex (baseline = female)"), "male",
  "", italic("MP tenure (baseline = 3 years)"), "10 years", "21 years"))

effect.label <- "Change in probability of MP being preferred,\n relative to baseline"




### Figure 2 

### Estimate and plot AMCE for MP frequency of dissent

res <- amce.tab(data = long.dat, 
                variables = c("mp.party", "mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = F)
#write.csv(res, "amce-all.csv") # write results to csv file

# subset to dissent effects
res.sub <- res[res$attribute == "mp.rebellion" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)

# plot res.sub
labels <- labels.sub
ggplot(res.sub, aes(x = category, y = AMCE)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), size = 0.8, shape = 21, fill = "white") + 
  labs(y = effect.label, x = "Frequency of MP dissent") + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  ylim(min(res.sub$ci.lo), max(res.sub$ci.hi)) +
  coord_flip() +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=labels)
ggsave("figures/figure2.eps", height = 3.5, width = 5.5)





### Figure 3

### Effect of frequency of dissent conditional on strength of co-partisanship

# set up data
data.list <- list(NULL, length = length(levels(long.dat$mp.congstr)))
for(i in 1:length(levels(long.dat$mp.congstr))){
  data.list[[i]] <- long.dat[!is.na(long.dat$mp.congstr) & long.dat$mp.congstr == levels(long.dat$mp.congstr)[i],]
}
names(data.list) <- levels(long.dat$mp.congstr)

# estimate effects
res <- amce.tab(data = data.list, 
                variables = c("mp.party", "mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = T)
res$mp.congstr <- res$set
res$mp.congstr <- factor(res$set,levels = rev(c("Strong opp. partisan","Weak opp. partisan","Non-attached","Weak co-partisan","Strong co-partisan")), order = T)
# re-order category levels to put attribute of interest at top

# pull out attr of interest
res.sub <- res[res$attribute == "mp.rebellion" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)

# plot
labels <- labels.sub
ggplot(res.sub, aes(x = category, y = AMCE, shape = mp.congstr, fill = mp.congstr)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi),
                  position = position_dodge(width = - 0.6), size = 1) + 
  labs(y = effect.label, x = "Frequency of MP dissent ") + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  scale_x_discrete(labels=labels) +
  scale_shape_manual(name = "Voter-MP partisanship",  
                     values = c(21,21,21,21,21)) +
  scale_fill_manual(name = "Voter-MP partisanship",  
                    values = c("White","Gray70","Gray50","Gray30","Black")) +
  theme(legend.position="right")
ggsave("figures/figure3.eps", height = 5, width = 9)

# Run ols to get F-test and see which interactions are significant
the.zvars <- c("mp.congstr")
for(i in 1:length(the.zvars)){
  z <- the.zvars[i]
  fmla <- as.formula(paste("mp.preferred ~ factor(mp.rebellion)*", z, sep = ""))
  model <- ols(fmla, data = long.dat, x = T, y = T)  
  model.clus <- robcov(model, cluster = long.dat$id, method = "efron")
  print(anova(model.clus, main.effect = T, ss = FALSE ))
  print(model.clus)
}





### Figure 4

### Effects of dissent frequency conditional on implied policy proximity

## Labour MPs: Are dissent effects more positive if voter is to the left of Labour?
data.list <- list(NULL, length = length(levels(long.dat$leftoflab)))
for(i in 1:length(levels(long.dat$leftoflab))){
  data.list[[i]] <- long.dat[long.dat$mp.party == "Labour" & !is.na(long.dat$leftoflab) & long.dat$leftoflab == levels(long.dat$leftoflab)[i],]
}
names(data.list) <- levels(long.dat$leftoflab)

res <- amce.tab(data = data.list, 
                variables = c("mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = T)
res$leftoflab <- factor(res$set,levels = levels(long.dat$leftoflab), order = T)

# pull out attr of interest
res.sub <- res[res$attribute == "mp.rebellion" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)

# plot 
labels <- labels.sub
bottomplot <- ggplot(res.sub, aes(x = category, y = AMCE, shape = leftoflab, fill = leftoflab)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi),
                  position = position_dodge(width = - 0.6), size = 1) + 
  labs(y = effect.label, x = "Frequency of MP dissent ") + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  scale_x_discrete(labels=labels) +
  scale_shape_manual(name = "",  
                     values = c(21,21)) +
  scale_fill_manual(name = "",  
                    values = c("White","Black")) +
  theme(legend.position="bottom")

# Run ols to get F-test
the.vars <- c("mp.preferred", "id","mp.rebellion", "mp.party", "leftoflab")
the.dat <- subset(na.omit(long.dat[,the.vars]), mp.party == "Labour")
fmla <- as.formula(paste("mp.preferred ~ factor(mp.rebellion)*leftoflab"))
model <- ols(fmla, data = the.dat, x = T, y = T)  
model.clus <- robcov(model, cluster = the.dat$id, method = "efron")
print(anova(model.clus))
print(anova(model.clus, main.effect = T, ss = FALSE ), which = "subscripts")


## Conservative MPs: Are dissent effects more positive if voter is to the right of the Conservatives?
data.list <- list(NULL, length = length(levels(long.dat$rightofcon)))
for(i in 1:length(levels(long.dat$rightofcon))){
  data.list[[i]] <- long.dat[long.dat$mp.party == "Conservative" & !is.na(long.dat$rightofcon) & long.dat$rightofcon == levels(long.dat$rightofcon)[i],]
}
names(data.list) <- levels(long.dat$rightofcon)

res <- amce.tab(data = data.list, 
                variables = c("mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = T)
res$rightofcon <- factor(res$set,levels = rev(levels(long.dat$rightofcon)), order = T)

# pull out attr of interest
res.sub <- res[res$attribute == "mp.rebellion" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)

# plot res.sub
labels <- labels.sub
topplot <- ggplot(res.sub, aes(x = category, y = AMCE, shape = rightofcon, fill = rightofcon)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi),
                  position = position_dodge(width = - 0.6), size = 1) + 
  labs(y = effect.label, x = "Frequency of MP dissent ") + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  scale_x_discrete(labels=labels) +
  scale_shape_manual(name = "",  
                     values = c(21,21)) +
  scale_fill_manual(name = "",  
                    values = c("White","Black")) +
  theme(legend.position="bottom")

# Run ols to get F-test
the.vars <- c("mp.preferred", "id","mp.rebellion", "mp.party", "rightofcon")
the.dat <- subset(na.omit(long.dat[,the.vars]), mp.party == "Labour")
fmla <- as.formula(paste("mp.preferred ~ factor(mp.rebellion)*rightofcon"))
model <- ols(fmla, data = the.dat, x = T, y = T)  
model.clus <- robcov(model, cluster = the.dat$id, method = "efron")
print(model.clus)
print(anova(model.clus, main.effect = T, ss = FALSE ), which = "subscripts")

## Put  top and bottom plots together and output
setEPS()
postscript("figures/figure4.eps", width = 6, height = 8)
multiplot(topplot, bottomplot, cols=1)
dev.off()
  
  



### Figure S4: top panel (Online appendix)

### AMCE of frequency of dissent by same- and different-party comparisons

data.list <- list(NULL, length = length(unique(long.dat$same.party)))
for(i in 1:length(unique(long.dat$same.party))){
  data.list[[i]] <- long.dat[long.dat$same.party == unique(long.dat$same.party)[i],]
}
names(data.list) <- c("MPs from different parties","MPs from same party")

res <- amce.tab(data = data.list, 
                variables = c("mp.party", "mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = T, same.party = T)
res$Comparison <- res$set
# pull out attr of interest
res.sub <- res[res$attribute == "mp.rebellion" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)
# plot res.sub
labels <- labels.sub
ggplot(res.sub, aes(x = category, y = AMCE, color = Comparison)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), size = 0.75) + 
  facet_grid( Comparison ~ .) +
  labs(y = effect.label, x = "Frequency of MP dissent ") + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=labels)
ggsave("figures/figureS4top.eps", height = 5, width = 6)





### Figure S5 (online appendix)

### AMCE estimates for all attributes included in study 2 conjoint analysis 

res <- amce.tab(data = long.dat, 
                variables = c("mp.party", "mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = F)
# re-order attributes
res <- rbind(res[res$attribute == "mp.party",],res[res$attribute != "mp.party",]) 
res <- rbind(res[res$attribute == "mp.sex",],res[res$attribute != "mp.sex",]) 
res <- rbind(res[res$attribute == "mp.tenure",],res[res$attribute != "mp.tenure",]) 
res <- rbind(res[res$attribute == "mp.const",],res[res$attribute != "mp.const",]) 
res <- rbind(res[res$attribute == "mp.rebellion",],res[res$attribute != "mp.rebellion",]) 
# turn category in to ordered factor for plotting
res$category <- factor(as.character(res$category), levels = rev(as.character(res$category)), order =T)

# set labels for ggplots
labels.long <- rev(expression(
  "", italic("MP frequency of dissent"), "Rarely vs. Never", "Sometimes vs. Never", "Often vs. Never",
  "", italic("MP time spent on constituency work"), "Two days vs. One day", "Three days vs. One day", "Four days vs. One day",
  "", italic("MP parliamentary experience"), "Ten years vs. Three years ", "Twenty-one years vs. Three years",
  "", italic("MP sex"), "Male vs. Female",
  "", italic("MP party affiliation"), "Labour vs. Conservative"
))


# prep quantities of interest
res <- res[2:nrow(res),] # chop off top empty layer
res <- subset(res, level != "baseline")# remove artificial 'baseline' rows
labels <- labels.long
limits <- aes(ymax = ci.hi, ymin = ci.lo)

# and plot
p <- ggplot(res, aes(x = category, y = AMCE, fill = attribute)) 
p <- p + geom_bar(,stat = "identity") + 
  scale_x_discrete(labels=labels) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  labs(y = effect.label, x = "") +
  coord_flip() + 
  theme(legend.position = "none", text = element_text(size = 15),
        axis.text = element_text(colour = "black"),
        axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(size = 13, vjust = 0))
p <- p + geom_errorbar(limits, width = 0.25) 
ggsave("figures/figureS5.eps", height = 7, width = 8)





### Figure S7 (online appendix)

### Preferences for frequency of dissent by MP party and voter partisanship

data.list <- list(NULL, length = length(levels(long.dat$pidstrength.mpparty)))
for(i in 1:length(levels(long.dat$pidstrength.mpparty))){
  data.list[[i]] <- long.dat[!is.na(long.dat$pidstrength.mpparty) & long.dat$pidstrength.mpparty == levels(long.dat$pidstrength.mpparty)[i],]
}
names(data.list) <- levels(long.dat$pidstrength.mpparty)

res <- amce.tab(data = data.list, 
                variables = c("mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = T)
res$pidstrength.mpparty <- factor(res$set,levels = levels(long.dat$pidstrength.mpparty), order = T)
# to make two-way facet
res <- cbind(res,colsplit(as.character(res$set), split = ":", names = c("pid.strength", "MP party")))
res$pid.strength <- ordered(res$pid.strength, levels = levels(long.dat$pid.strength))
res$pid.strength <- revalue(res$pid.strength, c("Strong Conservative" = "Strong Con",
                                                "Strong Labour" = "Strong Lab", "Strong other party"="Strong Other",
                                                "Weak Conservative" = "Weak Con", "Weak Labour"="Weak Lab",
                                                "Weak other party"="Weak Other", "Non-attached"="Non-attached"))
# pull out attr of interest
res.sub <- res[res$attribute == "mp.rebellion" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)
# plot res.sub
labels <- labels.sub
ggplot(res.sub, aes(x = category, y = AMCE, color = pidstrength.mpparty)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), size = 0.75) + 
  facet_grid(pid.strength ~ MP.party) +
  labs(y = effect.label, x = "Frequency of MP dissent ") + 
  theme(axis.title = element_text(size = 10, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 10), strip.text.y = element_text(size = 8)) +
  theme(legend.position = "none") + 
  scale_x_discrete(labels=labels) 
ggsave("figures/figureS7.eps", height = 7, width = 6)




### Figure S8 (online appendix)

### Preferences for frequency of dissent by MP party and voter left-right position

data.list <- list(NULL, length = length(levels(long.dat$mpparty.lrgroup3)))
for(i in 1:length(levels(long.dat$mpparty.lrgroup3))){
  data.list[[i]] <- long.dat[!is.na(long.dat$mpparty.lrgroup3) & long.dat$mpparty.lrgroup3 == levels(long.dat$mpparty.lrgroup3)[i],]
}
names(data.list) <- levels(long.dat$mpparty.lrgroup3)

res <- amce.tab(data = data.list, 
                variables = c("mp.const", "mp.rebellion", "mp.sex", "mp.tenure")
                , multi = T)
res$mpparty.lrgroup3 <- factor(res$set,levels = levels(long.dat$mpparty.lrgroup3), order = T)

res$mpparty <- t(as.data.frame(strsplit(as.character(res$mpparty.lrgroup3), "\\.")))[,1]
res$lrgroup3 <- t(as.data.frame(strsplit(as.character(res$mpparty.lrgroup3), "\\.")))[,2]
res$mpparty <- factor(res$mpparty, 
                      levels = c("Conservative", "Labour"),
                      labels = c("Con MP", "Lab MP"), ordered = T)
res$lrgroup3 <- factor(res$lrgroup3, 
                       levels = c("Very left-wing","Moderately left-wing", "Centrist", "Moderately right-wing", "Very right-wing"), 
                       ordered = T)

# subplot for MP dissent
res.sub <- res[res$attribute == "mp.rebellion" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)

# plot res.sub
labels <- labels.sub
ggplot(res.sub, aes(x = category, y = AMCE, color = lrgroup3)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), size = 0.75) + 
  facet_grid(mpparty ~ lrgroup3) +
  labs(y = effect.label, x = "Frequency of MP dissent ") + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 11.5)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=labels)
ggsave("figures/figureS8.eps", height = 4.5, width = 10)



### F-tests of interactions between MP frequency of dissent and other MP attributes

model <- ols(mp.preferred ~ factor(mp.rebellion)*factor(mp.sex) + factor(mp.rebellion)*factor(mp.tenure) + factor(mp.rebellion)*factor(mp.party) + factor(mp.rebellion)*factor(mp.const), data = long.dat, x = T, y = T) 
model.clus <- robcov(model, cluster = long.dat$id, method = "efron")
print(anova(model.clus, main.effect = T ), which = "subscripts")













