### Replication code for Study Three in 
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





### Load in data from Study Three

long.dat <- readRDS("data/study3data.rds")




### Create labels for plots

labels.sub.dissent <- c("tends not to speak out", "speaks out internally", "speaks out internally\nand externally")
labels.sub.views <- rev(c("own personal views", "constituents' views"))

labels.full <- rev(expression(
  "", italic("MP constituency work (baseline = 1 day)"), "2 days", "3 days", "4 days",
  "", italic("MP party (baseline = Conservative)"), "Labour",
  "", italic("Policy influence (baseline = own personal views)"), "constituents' views",
  "", italic("MP dissent (baseline = tends not to)"), "internal only", "internal and external",
  "", italic("MP sex (baseline = female)"), "male"))

effect.label <- "Change in probability of MP being preferred,\n relative to baseline"
axislab <- "Type of MP dissent"




### Figure 5

### AMCE estimates for type of MP dissent

res <- amce.tab(data = long.dat, 
                variables = c("mp.party", "mp.const", "mp.views", "mp.dissent", "mp.sex")
                , multi = F)
# subplot for MP dissent
res.sub <- res[res$attribute == "mp.dissent" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)
labels <- labels.sub.dissent
ggplot(res.sub, aes(x = category, y = AMCE)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), size = 0.8, shape = 21, fill = "white") + 
  labs(y = effect.label, x = axislab) + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0), axis.title.y = element_text(vjust = 1)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=labels) 
ggsave("figures/figure5.eps", height = 2.75, width = 5.5)





### Figure 6

### Preferences for MP dissent conditional on motivation for dissent

data.list <- list(NULL, length = length(levels(long.dat$mp.views)))
for(i in 1:length(levels(long.dat$mp.views))){
  data.list[[i]] <- long.dat[long.dat$mp.views == levels(long.dat$mp.views)[i],]
}
names(data.list) <- c("Trustee MP","Delegate MP")
res <- amce.tab(data = data.list, 
                variables = c("mp.party", "mp.const", "mp.dissent", "mp.sex")
                , multi = T, same.party = T)
res$mp.views <- res$set

# pull out attr of interest
res.sub <- res[res$attribute == "mp.dissent" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)
# plot res.sub
labels <- labels.sub.dissent
ggplot(res.sub, aes(x = category, y = AMCE, shape = mp.views, fill = mp.views)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi),
                  position = position_dodge(width = - 0.5), size = 0.8) + 
  labs(y = effect.label, x = axislab) + 
  theme(axis.title = element_text(size = 12, vjust = 0.1, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0), axis.title.y = element_text(vjust = 1)) +
  scale_x_discrete(labels=labels) +
  scale_shape_manual(name = "",  
                     values = c(21,21)) +
  scale_fill_manual(name = "",  
                    values = c("White","Black")) +
  theme(legend.position="bottom")
ggsave("figures/figure6.eps", height = 4, width = 6)

# Corresponding F-test
the.vars <- c("mp.preferred", "id","mp.dissent", "mp.views")
the.dat <- na.omit(long.dat[,the.vars])
model <- ols(mp.preferred ~ factor(mp.dissent)*factor(mp.views), data = the.dat, x = T, y = T)  
model.clus <- robcov(model, cluster = the.dat$id, method = "efron")
print(model.clus)
print(anova(model.clus, main.effect = T, ss = FALSE ), which = "subscripts")





### Figure S4: Bottom panel (Online Appendix) 

### Preferences for MP type of dissent conditional on same- and different-party comparisons

data.list <- list(NULL, length = length(unique(long.dat$same.party)))
for(i in 1:length(unique(long.dat$same.party))){
  data.list[[i]] <- long.dat[long.dat$same.party == unique(long.dat$same.party)[i],]
}
names(data.list) <- c("MPs from same party","MPs from different parties")

res <- amce.tab(data = data.list, 
                variables = c("mp.party", "mp.const", "mp.views", "mp.dissent", "mp.sex")
                , multi = T, same.party = T)
res$Comparison <- res$set
# pull out attr of interest
res.sub <- res[res$attribute == "mp.dissent" & !is.na(res$AMCE),]
res.sub$category <- factor(as.character(res.sub$category), levels = (as.character(res.sub$category)), order =T)
# plot res.sub
labels <- labels.sub.dissent
ggplot(res.sub, aes(x = category, y = AMCE, color = Comparison)) + 
  geom_hline(xintercept = 0, linetype = "dashed", size = 0.5) +
  geom_pointrange(aes(ymin = ci.lo, ymax = ci.hi), size = 0.75) + 
  facet_grid( Comparison ~ .) +
  labs(y = effect.label, x = axislab) + 
  theme(axis.title = element_text(size = 12, face = "bold")) +
  coord_flip() + 
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_text(hjust = 1), # remove ticks and justify
        axis.title.x = element_text(vjust = 0)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=labels)
ggsave("figures/figureS4bottom.eps", height = 5, width = 6)





### Figure S6 (Online Appendix) 

### AMCE estimates for all attributes in Study 3

res <- amce.tab(data = long.dat, 
                variables = c("mp.party", "mp.const", "mp.views", "mp.dissent", "mp.sex")
                , multi = F)
# re-order attributes according to mentions in notes
res <- rbind(res[res$attribute == "mp.party",],res[res$attribute != "mp.party",]) 
res <- rbind(res[res$attribute == "mp.sex",],res[res$attribute != "mp.sex",]) 
res <- rbind(res[res$attribute == "mp.const",],res[res$attribute != "mp.const",]) 
res <- rbind(res[res$attribute == "mp.views",],res[res$attribute != "mp.views",]) 
res <- rbind(res[res$attribute == "mp.dissent",],res[res$attribute != "mp.dissent",])
# convert to factors
res$category <- factor(as.character(res$category), levels = rev(as.character(res$category)), order =T)

# set labels for plots
labels.long <- rev(expression(
  "", italic("MP dissent"), "Internal only vs. Tends not to", "Internal and public vs. Tends not to",
  "", italic("Main policy influence"), "Constituents' views vs. Personal views",
  "", italic("MP time spent on constituency work"), "Two days vs. One day", "Three days vs. One day", "Four days vs. One day",
  "", italic("MP sex"), "Male vs. Female",
  "", italic("MP party affiliation"), "Labour vs. Conservative"))            
labels.sub <- rev(expression(
  "", italic("MP dissent"), "Internal only vs. Tends not to", "Internal and public vs. Tends not to",
  "", italic("Main policy influence"), "Constituents' views vs. Personal views"))
effect.label <- "Effect on support for MP (%)"


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
ggsave("figures/figureS6.eps", height = 7, width = 8)





### F-tests for attribute interactions with MP type of dissent

model <- ols(mp.preferred ~ factor(mp.dissent)*factor(mp.views) + factor(mp.dissent)*factor(mp.const) + + factor(mp.dissent)*factor(mp.party) + + factor(mp.dissent)*factor(mp.sex), data = long.dat, x = T, y = T) 
model.clus <- robcov(model, cluster = long.dat$id, method = "efron")
print(anova(model.clus, main.effect = T ), which = "subscripts")

# Investigate significant interaction for MP type of dissent and MP sex further
print(model.clus)










