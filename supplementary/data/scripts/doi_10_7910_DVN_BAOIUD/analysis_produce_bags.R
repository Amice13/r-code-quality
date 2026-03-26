#install.packages() the following library if they have not been installed
library(dplyr)
library(ggplot2)
library(multcomp)
library(lsr)
library(DescTools)
library(tibble)

#read csv in the current working directory
d <- read.csv("bagresults.csv")
nrow(d)

#get summary statistics
d %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

mean(d$age, na.rm = T)
sd(d$age, na.rm = T)


#exclude who failed attention check (the correct response was 3)
d <- filter(d, attention==3)
nrow(d)

#exclude participants who requested a number of produce bags requested above 2.5 sd
d <- filter(d, numberofproducebag<(mean(numberofproducebag, na.rm = T)+2.5*sd(numberofproducebag, na.rm = T)))
nrow(d)


####check for the shopping cart - get the number of item selected####
shopd <- read.csv("shoppingresults.csv")
length(unique(shopd$code))

shopd <- filter(shopd, code %in% c(d$code))
length(unique(shopd$code))

subjlist <- c(unique(shopd$code))


newShopd <- c()

for (i in 1:length(subjlist)){
  subjd <- filter(shopd, code==subjlist[i])
  subjdSingleRow <-  subjd[1,]
  subjdSingleRow$totalNumItem <-  sum(subjd$itemcount)
  subjdSingleRow$totalTypeItem <-  nrow(subjd)
  subjdSingleRow$totalExpense <-  sum(subjd$itemprice * subjd$itemcount)
  
  newShopd <- rbind(newShopd, subjdSingleRow)
}

#exclude participants who selected a number of items above 2.5 sd
newShopd <- filter(newShopd, newShopd$totalNumItem<(mean(newShopd$totalNumItem)+2.5*sd(newShopd$totalNumItem)))
nrow(newShopd)


# // Condition 0: Control
# // Condition 1: Nudge Attention
# // Condition 2: Nudge Perception
# // Condition 3: Nudge Memory
# // Condition 4: Nudge Effort
# // Condition 5: Nudge Intrinsic motivation 
# // Condition 6: Nudge Extrinsic motivation
# // Condition 7: Sludge Attention
# // Condition 8: Sludge Perception
# // Condition 9: Sludge Memory
# // Condition 10: Sludge Effort
# // Condition 11: Sludge Intrinsic motivation 
# // Condition 12: Sludge Extrinsic motivation

d <- newShopd

#add condition name
d$condName <- ifelse(d$condition==0, "control", 
                     ifelse(d$condition==1, "attention nudge",
                                   ifelse(d$condition==2, "perception nudge",
                                     ifelse(d$condition==3, "memory nudge",
                                            ifelse(d$condition==4, "effort nudge",
                                                   ifelse(d$condition==5, "intrinsic nudge",
                                                          ifelse(d$condition==6, "extrinsic nudge",
                                                                 ifelse(d$condition==7, "attention sludge",
                                                                        ifelse(d$condition==8, "perception sludge",
                                                                               ifelse(d$condition==9, "memory sludge",
                                                                                      ifelse(d$condition==10, "effort sludge",
                                                                                             ifelse(d$condition==11, "intrinsic sludge",
                                                                                                    ifelse(d$condition==12, "extrinsic sludge", NA)))))))))))))




d$condName <- factor(d$condName, levels = c("control", "attention nudge", "perception nudge", "memory nudge",
                                            "effort nudge", "intrinsic nudge",  "extrinsic nudge",
                                            "attention sludge", "perception sludge", "memory sludge",
                                            "effort sludge", "intrinsic sludge",  "extrinsic sludge"))

#add type of intervention
d$nudgesludge <- ifelse(d$condition == 0, "control", 
                        ifelse(d$condition%in%c(1:6), "nudge", "sludge"))

#add six cognitive process
for (i in 1:nrow(d)){
  d$cogproc[i] <-gsub( "\\s.*", "", d$condName[i])
}

d$nudgesludge <- as.factor(d$nudgesludge)
d$cogproc <- as.factor(d$cogproc)

#function for plotting the graph

plotgraph <- function (data, graphname, color){
  graph <- data %>%
    group_by(condName) %>%
    summarise(mean = mean(numberofproducebag),
              sd = sd(numberofproducebag),
              n = n(),
              se = sd / sqrt(n))
  
  
  agraph <- ggplot(graph, aes(x = reorder(condName, mean), y = mean)) +
    geom_bar(stat = "identity",  width=.7, fill = color)+
    # coord_flip()+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                  position=position_dodge(.9)) +
    ylim(0, 5)+
    xlab("") +
    ylab("Mean number of produce bags")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30,  hjust=1))+
    theme(text=element_text(family="Times New Roman", size=22)) #Times New Roman, 12pt, Bold
  
  print(agraph)
  
  ggsave(file=paste(graphname, ".svg", sep = "", collapse = NULL), plot=agraph, width=14, height=6)
  
}

#function for the descriptive statistics table
getTable <- function(d, tableName){
  control <- filter(d, condName=="control")
  controlMean <- mean(control$numberofproducebag)
  
  a <- d %>%
    group_by(condName) %>%
    summarise(mean = sprintf("%.2f", round(mean(numberofproducebag),2)), 
              sd = sprintf("%.2f", round(sd(numberofproducebag),2)),
              n= n(), 
              percDec = sprintf("%.2f", round((mean(numberofproducebag)/controlMean-1)*100,2)),
              se = round(sd(numberofproducebag/n()),1),
              min=min(numberofproducebag), 
              max=max(numberofproducebag)) %>%
    arrange(mean)
  
  names(a)[names(a) == "condName"] <- "Condition"
  names(a)[names(a) == "mean"] <- "Mean"
  names(a)[names(a) == "sd"] <- "SD"
  names(a)[names(a) == "n"] <- "N"
  
  
  write.csv(a, tableName, row.names = F)
  
}

#get the plot and table
#overall
plotgraph(d, "Mean number of produce bag", "#a6a6a6")
getTable(d, "meanoverall.csv")

#liberals
dLib <- filter(d, politic<0)
nrow(dLib)
plotgraph(dLib, "Mean number of produce bag (Liberals)", "#0000ff")
getTable(dLib, "meanlib.csv")

#conservatives
dCon <- filter(d, politic>0)
nrow(dCon)
plotgraph(dCon, "Mean number of produce bag (Conservatives)", "#ff0000")
getTable(dCon, 'meancon.csv')

#independents
dInd <- filter(d, politic==0)
nrow(dInd)
plotgraph(dInd, "Mean number of produce bag (Independent)", "#a000a0")
getTable(dInd, 'meanind.csv')


####analysis of one way ANCOVA####
d$condName <- as.factor(d$condName)

#convert the sassy scale to numeric
d$sassyNum <- ifelse(d$sassy=="alarmed", 6, 
                     ifelse(d$sassy=="concerned", 5,
                            ifelse(d$sassy=="cautious", 4,
                                   ifelse(d$sassy=="disengaged", 3, 
                                          ifelse(d$sassy=="doubtful", 2,
                                                 ifelse(d$sassy=="dismissive", 1, NA))))))

# convert to zscore - standardization
dNumeric <- scale(Filter(is.numeric,d))
colnames(dNumeric) <- paste(colnames(dNumeric),"z",sep="_")
d <- cbind(d, dNumeric)

#function to get the ancova results
getAncovaPvalue <- function(d, pTableName){
  ancova_model <- aov(numberofproducebag_z ~ condName + age_z + gender + likelytouse_z + envfriendly_z + selfuse_z + politic_z + sassyNum_z + totalNumItem_z , data = d)
  print(summary(ancova_model))
  
  postHocs <- glht(ancova_model, linfct = mcp( condName = "Tukey"))
  pq<- summary(postHocs)$test
  pq<- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  colnames(pq) <- c("Estimate", "SE", "t_value", "pvalue")
  pq <- as.data.frame(pq)
  pqSig10<-filter(pq, pq$pvalue <.1 &  pq$pvalue >=.05)
  print(pqSig10)
  
  pqSig05<-filter(pq, pq$pvalue <.05)
  print(pqSig05)
  
  print(min(pq$pvalue))
  
  print(etaSquared(ancova_model))
  
  pResults <- DunnettTest(x=d$numberofproducebag_z, g=d$condName)
  pResultsTable <- pResults$control
  print(pResultsTable)
  pResultsTable <- rownames_to_column(as.data.frame(pResultsTable), "Comparison")
  pResultsTable <- pResultsTable[order(pResultsTable$pval),]
  pResultsTable$pvalLess <- ifelse(pResultsTable$pval<0.001, 1, 0) 
  pResultsTable$pval <- round(pResultsTable$pval, 2)
  pResultsTable$pval <- ifelse(pResultsTable$pvalLess==1, '<0.001', pResultsTable$pval) 
  pResultsTable <- pResultsTable[c(-2:-4, -6)]
  # print(pResultsTable)
  
  write.csv(pResultsTable, pTableName, row.names = F)
  
}

#overall
getAncovaPvalue(d, "poverall.csv")

#liberals
dLib <- filter(d, politic<0)
getAncovaPvalue(dLib, "plibp.csv")

#conservatives
dCon <- filter(d, politic>0)
getAncovaPvalue(dCon, "pconp.csv")

#independents
dInd <- filter(d, politic==0)
getAncovaPvalue(dInd, "pIndp.csv")


####analysis of two way ANCOVA by type of intervention (nudge/sludge) and cognitive processes####
dnocontrol <- filter(d, condName!="control")

#function for two way anova
twowayancova <- function (data){
  
  ancovamodelwithinteraction <- aov(numberofproducebag_z ~ nudgesludge * cogproc + age_z + gender + likelytouse_z + envfriendly_z + selfuse_z + politic_z + sassyNum_z + totalNumItem_z , data = data)
  print(summary(ancovamodelwithinteraction))
  
  print(etaSquared(ancovamodelwithinteraction))
  
  ancova_model <- aov(numberofproducebag_z ~ nudgesludge + cogproc + age_z + gender + likelytouse_z + envfriendly_z + selfuse_z + politic_z + sassyNum_z + totalNumItem_z , data = data)
  print(summary(ancova_model))
  
  postHocs <- glht(ancova_model, linfct = mcp(cogproc = "Tukey"))
  print(summary(postHocs))
}

#function for plotting the graph

plotgraph <- function (data, dependentVar, graphname, color, barWidth, errorWidth){
  graph <- data %>%
    group_by(data[,eval(substitute(dependentVar))]) %>%
    summarise(mean = mean(numberofproducebag),
              sd = sd(numberofproducebag),
              n = n(),
              se = sd / sqrt(n))
  
  names(graph)[1] <- "condname"
  agraph <- ggplot(graph, aes(x = reorder(condname, mean), y = mean)) +
    geom_bar(stat = "identity",  width=barWidth, fill = color)+
    # coord_flip()+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=errorWidth,
                  position=position_dodge(.9)) +
    ylim(0, 5)+
    xlab("") +
    ylab("Mean number of produce bags")+
    theme_classic() +
    theme(axis.text.x = element_text(angle = 30,  hjust=1))+
    theme(text=element_text(family="Times New Roman", size=22))
  
  print(agraph)
  
  ggsave(file=paste(graphname, ".svg", sep = "", collapse = NULL), plot=agraph, width=10, height=5)
  
}

#overall
twowayancova(dnocontrol)
plotgraph(dnocontrol, 'cogproc', "cogprocOverall", "#a6a6a6", 0.7,.2)
plotgraph(dnocontrol, 'nudgesludge', "nudgesludgeOverall", "#a6a6a6", 0.23, .08)

#liberals
dLibnoControl <-filter(dnocontrol, politic<0)
twowayancova(dLibnoControl)
plotgraph(dLibnoControl, 'cogproc', "cogprocLib", "#0000FF", 0.7,.2)
plotgraph(dLibnoControl, 'nudgesludge', "nudgesludgeLib", "#0000FF", 0.23, .08)

#conservatives
dConnoControl <-filter(dnocontrol, politic>0)
twowayancova(dConnoControl)
plotgraph(dConnoControl, 'cogproc', "cogprocCon", "#FF0000", 0.7,.2)
plotgraph(dConnoControl, 'nudgesludge', "nudgesludgeCon", "#FF0000", 0.23, .08)

#independents
dIndnoControl <-filter(dnocontrol, politic==0)
twowayancova(dIndnoControl)
plotgraph(dIndnoControl, 'cogproc', "cogprocInd", "#a000a0", 0.7,.2)
plotgraph(dIndnoControl, 'nudgesludge', "nudgesludgeInd", "#a000a0", 0.23, .08)

