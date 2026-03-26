# This R script contains the code to replicate the aggregate-level analysis of the PRQ Publication: "The Role of State & National Institutional Evaluations in Fostering Collective Accountability Across the U.S. States"

# This R replication file reproduces: 

# Figure 1: Distribution of State Legislative Seat Turnover, 2006-2020

# Figure 2: Distribution of National & State Institutional Job Approval, 2006-2020

# Figure 3: Relationship Between Institutional Evaluations & State Legislative Seat Turnover

# Figure 4: Job Approval Ratings Database Robustness Check of Institutional Evaluations

# Figure 5: Temporal Heterogeneity Relationship Between Presidential Approval & Seat Turnover: (a) By Presidential Administration

# Figure 5: Temporal Heterogeneity Relationship Between Presidential Approval & Seat Turnover: (b) By Year

##### Load Relevant Packages #####

library(ggplot2)
library(panelView)
library(descr)
library(fixest)
library(DataCombine)

##### 1) Load Aggregate Dataset #####

setwd("") # Set the working directory on local machine with the appropriate source data files

load("aggregate_data_state_legislatve.Rdata")

##### 2) Motivating Histograms #####

# Histogram, 2006-2020

x <- subset(mrp_legislative_election_years_merge,mrp_legislative_election_years_merge$upper_chamber == "Lower Chamber",select=c(year,state,upper_chamber,net_chamber_d_prop))

x$context <- paste("Democratic Party",": ",x$upper_chamber,"\nMean = ",round(mean(x$net_chamber_d_prop,na.rm=T),3),"; Median = ",round(median(x$net_chamber_d_prop,na.rm=T),3),"; SD = ",round(sd(x$net_chamber_d_prop,na.rm=T),3),"; Range = ",round(summary(x$net_chamber_d_prop)[1],2)," - ",round(summary(x$net_chamber_d_prop)[6],3),sep="")

lower <- x

x <- subset(mrp_legislative_election_years_merge,mrp_legislative_election_years_merge$upper_chamber == "Upper Chamber",select=c(year,state,upper_chamber,net_chamber_d_prop))

x$context <- paste("Democratic Party",": ",x$upper_chamber,"\nMean = ",round(mean(x$net_chamber_d_prop,na.rm=T),3),"; Median = ",round(median(x$net_chamber_d_prop,na.rm=T),3),"; SD = ",round(sd(x$net_chamber_d_prop,na.rm=T),3),"; Range = ",round(summary(x$net_chamber_d_prop)[1],2)," - ",round(summary(x$net_chamber_d_prop)[6],3),sep="")

upper <- x

x <- rbind(lower,upper)

plot <- ggplot(x,aes(x=net_chamber_d_prop)) + geom_histogram(alpha=.2,fill="black",color="black")  + facet_wrap(~context) + theme_bw() + scale_x_continuous("Percentage of Democratic State Legislative Seats Turned Over in Election Cycle",breaks=seq(-40,30,10),labels=paste(seq(-40,30,10),"%",sep="")) + scale_y_continuous("Frequency") + labs(caption="Data: 2006-2020 State Legislative Complete Elections.")
ggsave(file="Fig1_histogram_state_legislative_seats.png", plot, width = 9, height = 4.5, units = "in")  

# Approval Rating

x <- subset(mrp_estimates,select=c(year,state,pres_mean_approval))
colnames(x) <- c("year","state","approval")
x$approval <- x$approval-50
x$context <- paste("Presidential Approval","\nMean = ",round(mean(x$approval,na.rm=T),3),"; Median = ",round(median(x$approval,na.rm=T),3),"; SD = ",round(sd(x$approval,na.rm=T),3),"; Range = ",round(summary(x$approval)[1],2)," - ",round(summary(x$approval)[6],3),sep="")
pres <- x
pres$order <- 1

x <- subset(mrp_estimates,select=c(year,state,gov_mean_approval))
colnames(x) <- c("year","state","approval")
x$approval <- x$approval-50
x$context <- paste("Gubernatorial Approval","\nMean = ",round(mean(x$approval,na.rm=T),3),"; Median = ",round(median(x$approval,na.rm=T),3),"; SD = ",round(sd(x$approval,na.rm=T),3),"; Range = ",round(summary(x$approval)[1],2)," - ",round(summary(x$approval)[6],3),sep="")
gov <- x
gov$order <- 1

x <- subset(mrp_estimates,select=c(year,state,cong_mean_approval))
colnames(x) <- c("year","state","approval")
x$approval <- x$approval-50
x$context <- paste("Congressional Approval","\nMean = ",round(mean(x$approval,na.rm=T),3),"; Median = ",round(median(x$approval,na.rm=T),3),"; SD = ",round(sd(x$approval,na.rm=T),3),"; Range = ",round(summary(x$approval)[1],2)," - ",round(summary(x$approval)[6],3),sep="")
cong <- x
cong$order <- 1

x <- subset(mrp_estimates,select=c(year,state,state_leg_mean_approval))
colnames(x) <- c("year","state","approval")
x$approval <- x$approval-50
x$context <- paste("State Legislative Approval","\nMean = ",round(mean(x$approval,na.rm=T),3),"; Median = ",round(median(x$approval,na.rm=T),3),"; SD = ",round(sd(x$approval,na.rm=T),3),"; Range = ",round(summary(x$approval)[1],2)," - ",round(summary(x$approval)[6],3),sep="")
sl <- x
sl$order <- 1

x <- rbind(pres,gov,cong,sl)
x$order <- factor(x$order)

x$var <- factor(x$context,levels=c("Presidential Approval\nMean = -6.836; Median = -6.95; SD = 7.592; Range = -28.19 - 14.72","Gubernatorial Approval\nMean = 3.571; Median = 4.522; SD = 10.658; Range = -41.2 - 31.408","Congressional Approval\nMean = -27.842; Median = -28.595; SD = 7.289; Range = -42.98 - -0.201","State Legislative Approval\nMean = -3.639; Median = -2.896; SD = 9.148; Range = -38.42 - 21.544"))

plot <- ggplot(x,aes(x=approval)) + geom_histogram(alpha=.2,fill="black",color="black")  + facet_wrap(~var) + theme_bw() + scale_x_continuous("Institutional Job Approval Rating",breaks=seq(-40,30,10),labels=paste(seq(-40,30,10),"%",sep="")) + scale_y_continuous("Frequency") + labs(caption="Data: 2006-2020 State-level MRP Estimates of Latent Approval.")
ggsave(file="Fig2_histogram_state_mrp_estimates.png", plot, width = 9, height = 4.5, units = "in")

#### 3) MRP Estimates Hypothesis Testing ####

mrp_legislative_election_years_merge$dem_president <- ifelse(mrp_legislative_election_years_merge$year %in% c(2006:2008,2017:2020),0,1)

mrp_legislative_election_years_merge$dem_governor <- ifelse(mrp_legislative_election_years_merge$governor_party == 1,1,ifelse(mrp_legislative_election_years_merge$governor_party == 0, 0,NA))

table(mrp_legislative_election_years_merge$dem_president,mrp_legislative_election_years_merge$governor_party)

mrp_legislative_election_years_merge$dem_chamber <- ifelse(mrp_legislative_election_years_merge$dem_chamber == "Split",NA,mrp_legislative_election_years_merge$dem_chamber)

mrp_legislative_election_years_merge$dem_chamber <- factor(mrp_legislative_election_years_merge$dem_chamber,levels=c("Republican","Democratic"))

mrp_legislative_election_years_merge$dem_congress <- ifelse(mrp_legislative_election_years_merge$year %in% c(2007:2010),"Democratic",ifelse(mrp_legislative_election_years_merge$year %in% c(2006,2015:2018),"Republican",ifelse(mrp_legislative_election_years_merge$year %in% c(2011:2014,2019:2020),"Split",NA)))

mrp_legislative_election_years_merge$dem_congress <- factor(mrp_legislative_election_years_merge$dem_congress,levels=c("Republican","Split","Democratic"))

mrp_legislative_election_years_merge$net_chamber_d_prop <- mrp_legislative_election_years_merge$net_chamber_d_prop*100

mrp_legislative_election_years_merge$pres_mean_approval <- mrp_legislative_election_years_merge$pres_mean_approval-50

mrp_legislative_election_years_merge$gov_mean_approval <- mrp_legislative_election_years_merge$gov_mean_approval-50

mrp_legislative_election_years_merge$cong_mean_approval <- mrp_legislative_election_years_merge$cong_mean_approval-50

mrp_legislative_election_years_merge$state_leg_mean_approval <- mrp_legislative_election_years_merge$state_leg_mean_approval-50

mrp_legislative_election_years_merge$state_chamber <- paste(mrp_legislative_election_years_merge$state,mrp_legislative_election_years_merge$chamber)

x <- subset(state_leg_seats,select=c(year,state,dem_house,dem_senate))

x$dem_legislature <- ifelse(x$dem_house == "Democratic" & x$dem_senate == "Democratic","Democratic",ifelse(x$dem_house == "Republican" & x$dem_senate == "Republican","Republican","Split"))
x$dem_legislature <- factor(x$dem_legislature)

mrp_legislative_election_years_merge <- merge(mrp_legislative_election_years_merge,x,by=c("year","state"))

x <- subset(mrp_legislative_election_years_merge,select=c(year,state,state_chamber,upper_chamber,dem_president, dem_governor,dem_congress,dem_chamber,gov_mean_approval,pres_mean_approval,governor_party,state_leg_mean_approval,cong_mean_approval,net_chamber_d_prop,dem_legislature))

x <- x[order(x$state_chamber,x$year),]
x <- slide(x, Var = "net_chamber_d_prop", slideBy = -1,GroupVar=c("state_chamber"))
colnames(x)[16] <- "lag_net_chamber_d_prop"

x <- subset(x,x$year >= 2006)

# All Chambers

# Model 1
model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
summary(model2, cluster = c("state_chamber"))
iplot(model2,cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~  i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
summary(model3, cluster = c("state_chamber"))
iplot(model3,cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Executive Approval Model"

# Model 4

model4 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + i(dem_congress,cong_mean_approval) + i(dem_legislature,state_leg_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x,cluster = c("state_chamber"))
summary(model4, cluster = c("state_chamber"))
model4 <- summary(model4, cluster = c("state_chamber"))
model4_effects <- data.frame(model4$coeftable)
colnames(model4_effects) <- c("estimate","se","t_value","p_value")
model4_effects$effect <- rownames(model4_effects)
rownames(model4_effects) <- NULL
model4_effects$nobs <- nobs(model4)
model4_effects$model <- "Executive & Legislative Approval"

all_chambers_mrp_model <- rbind(model1_effects,model2_effects,model3_effects,model4_effects)


#  Only Lower Chambers

# Model 1

x1 <- subset(x,x$upper_chamber == "Lower Chamber")

model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model2, cluster = c("state_chamber"))
iplot(model2,cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~  i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model3, cluster = c("state_chamber"))
iplot(model3,cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Executive Approval Model"

# Model 4

model4 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + i(dem_congress,cong_mean_approval) + i(dem_legislature,state_leg_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model4, cluster = c("state_chamber"))
model4 <- summary(model4, cluster = c("state_chamber"))
model4_effects <- data.frame(model4$coeftable)
colnames(model4_effects) <- c("estimate","se","t_value","p_value")
model4_effects$effect <- rownames(model4_effects)
rownames(model4_effects) <- NULL
model4_effects$nobs <- nobs(model4)
model4_effects$model <- "Executive & Legislative Approval"

lower_chambers_mrp_model <- rbind(model1_effects,model2_effects,model3_effects,model4_effects)

# Upper Chamber

# Model 1

x1 <- subset(x,x$upper_chamber == "Upper Chamber")

model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model2, cluster = c("state_chamber"))
iplot(model2,cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~  i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model3, cluster = c("state_chamber"))
iplot(model3,cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Executive Approval Model"

# Model 4

model4 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) + i(dem_congress,cong_mean_approval) + i(dem_legislature,state_leg_mean_approval) + lag_net_chamber_d_prop + year | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model4, cluster = c("state_chamber"))
model4 <- summary(model4, cluster = c("state_chamber"))
model4_effects <- data.frame(model4$coeftable)
colnames(model4_effects) <- c("estimate","se","t_value","p_value")
model4_effects$effect <- rownames(model4_effects)
rownames(model4_effects) <- NULL
model4_effects$nobs <- nobs(model4)
model4_effects$model <- "Executive & Legislative Approval"

upper_chambers_mrp_model <- rbind(model1_effects,model2_effects,model3_effects,model4_effects)

#### 4) JARS hypothesis testing ####

jars_legislative_election_years_merge <- subset(jars_legislative_election_years_merge,jars_legislative_election_years_merge$year >= 1958)

jars_legislative_election_years_merge$dem_president <- ifelse(jars_legislative_election_years_merge$year %in% c(1961:1968,1977:1980,1993:2000,2009:2016),1,0)

jars_legislative_election_years_merge$dem_governor <- ifelse(jars_legislative_election_years_merge$governor_party == 1,1,ifelse(jars_legislative_election_years_merge$governor_party == 0,0,NA))

jars_legislative_election_years_merge$dem_chamber <- ifelse(jars_legislative_election_years_merge$dem_chamber == "Democratic",1,ifelse(jars_legislative_election_years_merge$dem_chamber == "Republican",0,-1))

jars_legislative_election_years_merge$pres_mean_approval <- jars_legislative_election_years_merge$pres_mean_approval-50

jars_legislative_election_years_merge$gov_mean_approval <- jars_legislative_election_years_merge$gov_mean_approval-50

jars_legislative_election_years_merge$state_chamber <- paste(jars_legislative_election_years_merge$state,jars_legislative_election_years_merge$chamber)

jars_legislative_election_years_merge$net_chamber_d_prop <- jars_legislative_election_years_merge$net_chamber_d_prop * 100

x <- subset(jars_legislative_election_years_merge,select=c(year,state,upper_chamber,net_chamber_d_prop,gov_mean_approval,pres_mean_approval,complete_jars_data,gov_party_jars,pres_party_jars,state_chamber,dem_president,dem_governor))

# All Chambers

# Model 1
model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) | factor(state_chamber),data=x,cluster = c("state_chamber"))
summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x,cluster = c("state_chamber"))
summary(model2, cluster = c("state_chamber"))
iplot(model2,cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x,cluster = c("state_chamber"))
summary(model3, cluster = c("state_chamber"))
iplot(model3,cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Executive Approval Model"

all_chambers_jars_model <- rbind(model1_effects,model2_effects,model3_effects)

#  Only Lower Chambers

# Model 1

x1 <- subset(x,x$upper_chamber == "Lower Chamber")

model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model2, cluster = c("state_chamber"))
iplot(model2,cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model3, cluster = c("state_chamber"))
iplot(model3,cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Executive Approval Model"

lower_chambers_jars_model <- rbind(model1_effects,model2_effects,model3_effects)

# Upper Chamber

# Model 1

x1 <- subset(x,x$upper_chamber == "Upper Chamber")

model1 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "Baseline Presidential Approval Model"

# Model 2

model2 <- feols(net_chamber_d_prop ~ i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model2, cluster = c("state_chamber"))
iplot(model2,cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Baseline Gubernatorial Approval Model"

# Model 3

model3 <- feols(net_chamber_d_prop ~ i(dem_president,pres_mean_approval) + i(dem_governor,gov_mean_approval) | factor(state_chamber),data=x1,cluster = c("state_chamber"))
summary(model3, cluster = c("state_chamber"))
iplot(model3,cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Executive Approval Model"

upper_chambers_jars_model <- rbind(model1_effects,model2_effects,model3_effects)

#### 5) Plotting Figures ####

upper_chambers_jars_model$chamber <- "Upper Legislative Chambers"
lower_chambers_jars_model$chamber <- "Lower Legislative Chambers"
all_chambers_jars_model$chamber <- "Pooled Legislative Chambers"

jars_models <- rbind(upper_chambers_jars_model,lower_chambers_jars_model,all_chambers_jars_model)
jars_models$data <- "JARS"

upper_chambers_mrp_model$chamber <- "Upper Legislative Chambers"
lower_chambers_mrp_model$chamber <- "Lower Legislative Chambers"
all_chambers_mrp_model$chamber <- "Pooled Legislative Chambers"

mrp_models <- rbind(upper_chambers_mrp_model,lower_chambers_mrp_model,all_chambers_mrp_model)
mrp_models$data <- "MRP"

x <- rbind(mrp_models,jars_models)

x$lower_90 <- x$estimate - qt(0.95,df=x$nobs) * x$se 
x$upper_90 <- x$estimate + qt(0.95,df=x$nobs) * x$se 

x$lower <- x$estimate - qt(0.975,df=x$nobs) * x$se 
x$upper <- x$estimate + qt(0.975,df=x$nobs) * x$se 

x$color_sig <- ifelse(x$p_value < 0.10,"sig","no sig")
x$shape <- ifelse(x$chamber == "Pooled Legislative Chambers",1,ifelse(x$chamber == "Lower Legislative Chambers",22,23))

y <- subset(x,x$data %in% "JARS")

y$model <- factor(y$model,levels=c("Baseline Presidential Approval Model","Baseline Gubernatorial Approval Model","Executive Approval Model"))
y$effect <- ifelse(y$effect == "dem_governor::0:gov_mean_approval","Gubernatorial Approval x \nRepublican Governor",ifelse(y$effect == "dem_governor::1:gov_mean_approval","Gubernatorial Approval x \nDemocratic Governor",ifelse(y$effect == "dem_president::0:pres_mean_approval","Presidential Approval x \nRepublican President",ifelse(y$effect == "dem_president::1:pres_mean_approval","Presidential Approval x \nDemocratic President",NA))))
y$chamber <- factor(y$chamber,levels=c("Lower Legislative Chambers","Upper Legislative Chambers","Pooled Legislative Chambers"))

y$party <- ifelse(y$effect %in% c("Gubernatorial Approval x \nRepublican Governor","Presidential Approval x \nRepublican President"),"Republican","Democratic")

plot <- ggplot(y,aes(x=effect,y=estimate,group=chamber,alpha=color_sig)) + theme_minimal() + facet_wrap(~model) + geom_linerange(aes(x= effect, ymin = lower_90, ymax = upper_90), lwd  = 1,position = position_dodge(width=0.5)) + geom_pointrange(aes(x= effect, ymin = lower, ymax = upper,shape=chamber,fill=chamber), lwd = 1/2,position = position_dodge(width=0.5)) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect on Net Proportion Seat Loss-Gain by Democratic Party in Election Cycle",breaks=seq(-1,1,0.10)) + scale_fill_manual("Chamber Model",values=rep("white",18)) + scale_alpha_manual("Chamber Model",values=c(0.2,1),guide="none") + labs(caption="Estimates derived from state legislative chamber fixed effects models. \nEach panel articulates three model results for lower, upper, & pooled chamber unit specifications.Total Model N = 9. \n90% & 95% Model CIs estimated from chamber clustered robust standard errors. Darker sharded point estimates significant at p < 0.10.") + scale_shape_manual("Chamber Model",values=c(23,22,21)) + scale_x_discrete("") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(file="Fig4_jars_seat_share_models.png", plot, width = 9, height = 6, units = "in")  
print(plot)

y <- subset(x,x$data %in% "MRP")

y$model <- factor(y$model,levels=c("Baseline Presidential Approval Model","Baseline Gubernatorial Approval Model","Executive Approval Model","Executive & Legislative Approval"))

y <- subset(y,y$effect != "lag_net_chamber_d_prop")
y <- subset(y,y$effect != "year")

y$effect <- ifelse(y$effect == "dem_governor::0:gov_mean_approval","Gubernatorial Approval x \nRepublican Governor",ifelse(y$effect == "dem_governor::1:gov_mean_approval","Gubernatorial Approval x \nDemocratic Governor",ifelse(y$effect == "dem_president::0:pres_mean_approval","Presidential Approval x \nRepublican President",ifelse(y$effect == "dem_president::1:pres_mean_approval","Presidential Approval x \nDemocratic President",ifelse(y$effect == "dem_legislature::Democratic:state_leg_mean_approval","State Legislature Approval x \nDemocratic Legislature",ifelse(y$effect == "dem_legislature::Republican:state_leg_mean_approval","State Legislature Approval x \nRepublican Legislature",ifelse(y$effect == "dem_legislature::Split:state_leg_mean_approval","State Legislature Approval x \nSplit Legislature",ifelse(y$effect == "dem_congress::Democratic:cong_mean_approval","Congressional Approval x \nDemocratic Congress",ifelse(y$effect == "dem_congress::Republican:cong_mean_approval","Congressional Approval x \nRepublican Congress",ifelse(y$effect == "dem_congress::Split:cong_mean_approval","Congressional Approval x \nSplit Congress",NA))))))))))

y$chamber <- factor(y$chamber,levels=c("Lower Legislative Chambers","Upper Legislative Chambers","Pooled Legislative Chambers"))

y$effect <- factor(y$effect,levels=c("State Legislature Approval x \nDemocratic Legislature","State Legislature Approval x \nSplit Legislature","State Legislature Approval x \nRepublican Legislature","Congressional Approval x \nDemocratic Congress","Congressional Approval x \nSplit Congress","Congressional Approval x \nRepublican Congress","Gubernatorial Approval x \nDemocratic Governor","Gubernatorial Approval x \nRepublican Governor","Presidential Approval x \nDemocratic President","Presidential Approval x \nRepublican President"))
                              
y$party <- ifelse(y$effect %in% c("Gubernatorial Approval x \nRepublican Governor","Presidential Approval x \nRepublican President","Congressional Approval x \nRepublican Congress","State Legislature Approval x \nRepublican Legislature"),"Republican",ifelse(y$effect %in% c("Gubernatorial Approval x \nDemocratic Governor","Presidential Approval x \nDemocratic President","Congressional Approval x \nDemocratic Congress","State Legislature Approval x \nDemocratic Legislature"),"Democratic","Split"))

plot <- ggplot(y,aes(x=effect,y=estimate,group=chamber,alpha=color_sig)) + theme_minimal() + facet_wrap(~model) + geom_linerange(aes(x= effect, ymin = lower_90, ymax = upper_90), lwd  = 1,position = position_dodge(width=0.5)) + geom_pointrange(aes(x= effect, ymin = lower, ymax = upper,shape=chamber,fill=chamber), lwd = 1/2,position = position_dodge(width=0.5)) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + theme(legend.position = "bottom") + scale_y_continuous("Estimated Marginal Effect on Net Proportion Seat Loss-Gain by Democratic Party in Election Cycle",breaks=seq(-1,1,0.20)) + scale_fill_manual("Chamber Model",values=rep("white",18)) + scale_alpha_manual("Chamber Model",values=c(0.2,1),guide="none") + labs(caption="Each panel articulates three model results for lower, upper, & pooled chamber unit specifications. \n90% & 95% Model CIs estimated from chamber clustered robust standard errors. Darker sharded point estimates significant at p < 0.10. \nEstimates derived from state legislative chamber fixed effects models. Model includes lagged outcome variable & yearly time-trend. Total Model N = 12.") + scale_shape_manual("Chamber Model",values=c(23,22,21)) + scale_x_discrete("") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(file="Fig3_mrp_seat_share_models.png", plot, width = 9, height = 7, units = "in")  
print(plot)

######### 6) Temporal Heterogenity Figures ######

# Temporal Heterogenity

x <- subset(mrp_legislative_election_years_merge,select=c(year,state,state_chamber,upper_chamber,dem_president, dem_governor,dem_congress,dem_chamber,gov_mean_approval,pres_mean_approval,governor_party,state_leg_mean_approval,cong_mean_approval,net_chamber_d_prop))

x <- x[order(x$state_chamber,x$year),]
x <- slide(x, Var = "net_chamber_d_prop", slideBy = -1,GroupVar=c("state_chamber"))
colnames(x)[15] <- "lag_net_chamber_d_prop"

x <- subset(x,x$year %in% seq(2006,2020,1))
x1 <- x

x1$president <- ifelse(x$year %in% seq(2006,2008,1),"Bush",ifelse(x$year %in% seq(2009,2012,1),"Obama",ifelse(x$year %in% seq(2013,2016,1),"Obama",ifelse(x$year %in% seq(2017,2020,1),"Trump",NA))))

x1$president <- factor(x1$president,levels=c("Bush","Obama","Trump"))

# Model 1

model1 <- feols(net_chamber_d_prop ~ i(president,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=x1,cluster = c("state_chamber"))

summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "All Chambers"

# Model 2

x <- subset(x1,x1$upper_chamber == "Lower Chamber")

model2 <- feols(net_chamber_d_prop ~ i(president,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=x,cluster = c("state_chamber"))

summary(model2, cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Lower Chambers"


# Model 3

x <- subset(x1,x1$upper_chamber == "Upper Chamber")

model3 <- feols(net_chamber_d_prop ~ i(president,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=x,cluster = c("state_chamber"))

summary(model3, cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Upper Chambers"

x <- rbind(model1_effects,model2_effects,model3_effects)

x$lower_90 <- x$estimate - qt(0.95,df=x$nobs) * x$se 
x$upper_90 <- x$estimate + qt(0.95,df=x$nobs) * x$se 

x$lower <- x$estimate - qt(0.975,df=x$nobs) * x$se 
x$upper <- x$estimate + qt(0.975,df=x$nobs) * x$se 

x$color_sig <- ifelse(x$p_value < 0.10,"sig","no sig")

x$model <- factor(x$model,levels=c("Lower Chambers","Upper Chambers","All Chambers"))

x <- subset(x,!(x$effect %in% c("lag_net_chamber_d_prop")))

x$year <- ifelse(x$effect %in% "president::Bush:pres_mean_approval","Pres. Bush",ifelse(x$effect %in% "president::Obama:pres_mean_approval","Pres. Obama",ifelse(x$effect %in% "president::Obama II:pres_mean_approval","Pres. Obama",ifelse(x$effect %in% "president::Trump:pres_mean_approval","Pres. Trump",NA))))

x$party <- ifelse(x$year %in% c("Pres. Bush","Pres. Trump"),"Republican","Democrat")

x$year <- factor(x$year,levels=c("Pres. Bush","Pres. Obama","Pres. Trump"))

x$label <- as.character(round(x$estimate,2))
x$label[x$label %in% "0.5"] <- "0.50"

plot <- ggplot(x,aes(x=year,y=estimate,label=label)) + theme_minimal() + facet_wrap(~model) + geom_linerange(aes(x= year, ymin = lower_90, ymax = upper_90), lwd  = 1) + geom_pointrange(aes(x= year, ymin = lower, ymax = upper), lwd = 1/2, fill="white", shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip() + geom_text(vjust=-1) + scale_y_continuous("Marginal Effect of Presidential Approval on \nDemocratic Percentage State Legislative Turnover",breaks=seq(-1,1,0.25)) + labs(caption="90% & 95% Model CIs estimated from chamber clustered robust standard errors. \nEstimates derived from state legislative chamber fixed effects & lagged outcome variable models. Total Model N = 3.") + scale_x_discrete("")
ggsave(file="Fig5A_pres_interaction_effects.png", plot, width = 9, height = 6, units = "in")  

# Option Two
# Model 1

model1 <- feols(net_chamber_d_prop ~ i(year,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=subset(x1,x1$year %in% seq(2006,2020,2)),cluster = c("state_chamber"))

summary(model1, cluster = c("state_chamber"))
model1 <- summary(model1, cluster = c("state_chamber"))
model1_effects <- data.frame(model1$coeftable)
colnames(model1_effects) <- c("estimate","se","t_value","p_value")
model1_effects$effect <- rownames(model1_effects)
rownames(model1_effects) <- NULL
model1_effects$nobs <- nobs(model1)
model1_effects$model <- "All Chambers"

# Model 2

x <- subset(x1,x1$upper_chamber == "Lower Chamber")

model2 <- feols(net_chamber_d_prop ~ i(year,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=subset(x,x$year %in% seq(2006,2020,2)),cluster = c("state_chamber"))

summary(model2, cluster = c("state_chamber"))
model2 <- summary(model2, cluster = c("state_chamber"))
model2_effects <- data.frame(model2$coeftable)
colnames(model2_effects) <- c("estimate","se","t_value","p_value")
model2_effects$effect <- rownames(model2_effects)
rownames(model2_effects) <- NULL
model2_effects$nobs <- nobs(model2)
model2_effects$model <- "Lower Chambers"


# Model 3

x <- subset(x1,x1$upper_chamber == "Upper Chamber")

model3 <- feols(net_chamber_d_prop ~ i(year,pres_mean_approval) + lag_net_chamber_d_prop | factor(state_chamber),data=subset(x,x$year %in% seq(2006,2020,2)),cluster = c("state_chamber"))

summary(model3, cluster = c("state_chamber"))
model3 <- summary(model3, cluster = c("state_chamber"))
model3_effects <- data.frame(model3$coeftable)
colnames(model3_effects) <- c("estimate","se","t_value","p_value")
model3_effects$effect <- rownames(model3_effects)
rownames(model3_effects) <- NULL
model3_effects$nobs <- nobs(model3)
model3_effects$model <- "Upper Chambers"

x <- rbind(model1_effects,model2_effects,model3_effects)

x$lower_90 <- x$estimate - qt(0.95,df=x$nobs) * x$se 
x$upper_90 <- x$estimate + qt(0.95,df=x$nobs) * x$se 

x$lower <- x$estimate - qt(0.975,df=x$nobs) * x$se 
x$upper <- x$estimate + qt(0.975,df=x$nobs) * x$se 

x$color_sig <- ifelse(x$p_value < 0.10,"sig","no sig")

x$model <- factor(x$model,levels=c("Lower Chambers","Upper Chambers","All Chambers"))

x <- subset(x,!(x$effect %in% c("lag_net_chamber_d_prop")))

x$year <- rep(seq(2006,2020,2),3)

x$party <- ifelse(x$year %in% c(2006:2008,2017:2020),"Republican","Democrat")

x$year <- factor(x$year)

x$label <- round(x$estimate,2)

plot <- ggplot(x,aes(x=year,y=estimate,label=label)) + theme_minimal() + facet_wrap(~model) + geom_linerange(aes(x= year, ymin = lower_90, ymax = upper_90), lwd  = 1) + geom_pointrange(aes(x= year, ymin = lower, ymax = upper), lwd = 1/2, fill="white", shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + coord_flip()  + geom_text(vjust=-1) + scale_y_continuous("Marginal Effect of Presidential Approval on \nDemocratic Percentage State Legislative Turnover",breaks=seq(-1,1,0.25)) + labs(caption="90% & 95% Model CIs estimated from chamber clustered robust standard errors. \nEstimates derived from state legislative chamber fixed effects & lagged outcome variable models. Total Model N = 3.") + scale_x_discrete("")
ggsave(file="Fig5B_pres_interaction_effects_yearly_factor.png", plot, width = 9, height = 6, units = "in")