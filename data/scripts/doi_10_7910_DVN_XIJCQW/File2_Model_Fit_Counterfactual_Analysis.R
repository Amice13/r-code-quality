# This R Script replicates the analysis in the manuscript titled: "Do Quality Candidates and Incumbents Still Matter in the Partisan World? Comparing Trends & Relationship Between Candidate Differentials and Congressional Election Outcomes, 1900-2022" published in the Journal of Political Marketing. Special Issue on the 2022 Midterm Congressional Election.

# The output of this R File are:

# Fig 4: Predictive power of both models in u.S. House and Senate elections.

# Fig 5: The decline of quality challengers in u.S. House and Senate elections.

# Table 1: 2022 U.S. House election quality counterfactual outcomes.

# Table 2: 2022 U.S. Senate election quality counterfactual outcomes.

# Load Packages

library(stargazer)
library(plyr)
library(ggplot2)

# Note: Run the "File1_House_Senate_Models.R" script prior to this R script. This script uses the output environment generated and saved by that R script for the forthcoming analysis.


#### 1) Load House & Senate Election Model Output from Local Dataverse Folder ####

load("/Dataverese Replication/models_special_issue.Rdata")

#### 2) Counterfactual Analysis for the House & Senate ####

x <- jacobson_model_yearly$estimate[jacobson_model_yearly$term == "dem_quality_advantage" & jacobson_model_yearly$year_fe == "2022" & jacobson_model_yearly$chamber == "United States Senate"]

senate_2022 <- subset(senate,senate$year == 2022 & senate$dv_margin < 7.487198 & senate$dv_margin > -7.487198)

senate_2022$dv_margin_counterfactual <- ifelse(senate_2022$dem_quality_advantage == 1,senate_2022$dv_margin - x,ifelse(senate_2022$dem_quality_advantage == -1,senate_2022$dv_margin + x,ifelse(senate_2022$dem_quality_advantage == 0 & senate_2022$dem_seat == 0,senate_2022$dv_margin - x,ifelse(senate_2022$dem_quality_advantage == 0 & senate_2022$dem_seat == 1,senate_2022$dv_margin + x,NA))))

senate_2022$counterfactual_dynamic <- ifelse(senate_2022$dem_quality_advantage == 1,"GOP Quality",ifelse(senate_2022$dem_quality_advantage == -1,"Democratic Quality",ifelse(senate_2022$dem_quality_advantage == 0 & senate_2022$dem_seat == 0,"Democratic Amateur",ifelse(senate_2022$dem_quality_advantage == 0 & senate_2022$dem_seat == 1,"GOP Amateur",NA))))

senate_2022$flip <- ifelse(senate_2022$dv_margin_counterfactual < 0 & senate_2022$dv_margin > 0,"Dem win/Counterfactual Dem Loss",ifelse(senate_2022$dv_margin_counterfactual > 0 & senate_2022$dv_margin < 0,"GOP Win/Counterfactual GOP Loss","No Flip"))

senate_2022 <- subset(senate_2022,!is.na(senate_2022$flip))
senate_2022$advantage_coeff <- as.numeric(x)

senate_2022 <- subset(senate_2022,select=c(year,state,advantage_coeff,flip,dv_margin,dv_margin_counterfactual,dem_incumbency,dem_quality_advantage,counterfactual_dynamic,po1,inc))

# House

x <- jacobson_model_yearly$estimate[jacobson_model_yearly$term == "dem_quality_advantage" & jacobson_model_yearly$year_fe == "2022" & jacobson_model_yearly$chamber == "United States House"]

house_2022 <- subset(house,house$year == 2022 & house$dv_margin < 0.5841841 & house$dv_margin > -0.5841841)

house_2022$dv_margin_counterfactual <- ifelse(house_2022$dem_quality_advantage == 1,house_2022$dv_margin - x,ifelse(house_2022$dem_quality_advantage == -1,house_2022$dv_margin + x,ifelse(house_2022$dem_quality_advantage == 0 & house_2022$dem_seat == 0,house_2022$dv_margin - x,ifelse(house_2022$dem_quality_advantage == 0 & house_2022$dem_seat == 1,house_2022$dv_margin + x,ifelse(house_2022$dem_quality_advantage == 0 & house_2022$dem_seat == -1,house_2022$dv_margin - x,NA)))))

house_2022$counterfactual_dynamic <- ifelse(house_2022$dem_quality_advantage == 1,"GOP Quality",ifelse(house_2022$dem_quality_advantage == -1,"Democratic Quality",ifelse(house_2022$dem_quality_advantage == 0 & house_2022$dem_seat == -1,"Democratic Amateur",ifelse(house_2022$dem_quality_advantage == 0 & house_2022$dem_seat == 1,"GOP Amateur",NA))))

house_2022$flip <- ifelse(house_2022$dv_margin_counterfactual < 0 & house_2022$dv_margin > 0,"Dem win/Counterfactual Dem Loss",ifelse(house_2022$dv_margin_counterfactual > 0 & house_2022$dv_margin < 0,"GOP Win/Counterfactual GOP Loss","No Flip"))

house_2022 <- subset(house_2022,!is.na(house_2022$flip))
house_2022$advantage_coeff <- as.numeric(x)

house_2022 <- subset(house_2022,select=c(year,district,advantage_coeff,flip,dv_margin,dv_margin_counterfactual,dem_incumbency,dem_quality_advantage,counterfactual_dynamic,po1,inc))

house_2022$dem_incumbency <- ifelse(house_2022$dem_incumbency == 1,"Democratic Incumbent",ifelse(house_2022$dem_incumbency == -1,"Republican Incumbent","Open Seat"))

senate_2022$dem_incumbency <- ifelse(senate_2022$dem_incumbency == 1,"Democratic Incumbent",ifelse(senate_2022$dem_incumbency == -1,"Republican Incumbent","Open Seat"))

senate_2022 <- subset(senate_2022,select=c(year,state,advantage_coeff,flip,dv_margin,dv_margin_counterfactual,dem_incumbency,po1,counterfactual_dynamic,inc))

house_2022 <- subset(house_2022,select=c(year,district,advantage_coeff,flip,dv_margin,dv_margin_counterfactual,dem_incumbency,po1,counterfactual_dynamic,inc))

house_2022$po1 <- ifelse(house_2022$dem_incumbency== "Democratic Incumbent" & house_2022$po1 == "Quality Challenger","GOP Quality",ifelse(house_2022$dem_incumbency== "Democratic Incumbent" & house_2022$po1 == "Amateur Challenger","GOP Amateur",ifelse(house_2022$dem_incumbency== "Republican Incumbent" & house_2022$po1 == "Amateur Challenger","Democratic Amateur",ifelse(house_2022$dem_incumbency== "Republican Incumbent" & house_2022$po1 == "Quality Challenger","Democratic Quality",house_2022$po1))))

senate_2022$po1 <- ifelse(senate_2022$dem_incumbency== "Democratic Incumbent" & senate_2022$po1 == "Quality Challenger","GOP Quality",ifelse(senate_2022$dem_incumbency== "Democratic Incumbent" & senate_2022$po1 == "Amateur Challenger","GOP Amateur",ifelse(senate_2022$dem_incumbency== "Republican Incumbent" & senate_2022$po1 == "Amateur Challenger","Democratic Amateur",ifelse(senate_2022$dem_incumbency== "Republican Incumbent" & senate_2022$po1 == "Quality Challenger","Democratic Quality",senate_2022$po1))))

senate_2022$po1 <- gsub("Candidates","",senate_2022$po1)
senate_2022$po1 <- gsub("Candidate","",senate_2022$po1)

house_2022$po1 <- gsub("Candidates","",house_2022$po1)
house_2022$po1 <- gsub("Candidate","",house_2022$po1)

senate_2022$counterfactual_dynamic <- ifelse(senate_2022$po1 == "Only Democratic Quality  (open)" & senate_2022$counterfactual_dynamic == "GOP Quality","Both Quality  (open)",senate_2022$counterfactual_dynamic)

house_2022$counterfactual_dynamic <- ifelse(house_2022$po1 == "Only Democratic Quality  (open)" & house_2022$counterfactual_dynamic == "GOP Quality","Both Quality  (open)",house_2022$counterfactual_dynamic)

house_2022 <- data.frame(house_2022)
senate_2022 <- data.frame(senate_2022)

for(i in c(3,5,6)){
  house_2022[,i] <- round(house_2022[,i],2)
  senate_2022[,i] <- round(senate_2022[,i],2)
}

senate_2022$dem_incumbency <- senate_2022$inc
house_2022$dem_incumbency <- house_2022$inc

senate_2022$inc <- NULL
house_2022$inc <- NULL

rownames(senate_2022) <- NULL
rownames(house_2022) <- NULL

house_2022$year <- NULL
senate_2022$year <- NULL

rownames(senate_2022) <- senate_2022$state
rownames(house_2022) <- house_2022$district

senate_2022$state <- NULL
d <- house_2022$district 
house_2022$district <- NULL

senate_2022$advantage_coeff <- NULL

stargazer(senate_2022,summary =  F,type="text",digits=2,covariate.labels =c("State","Outcome/Counterfactual","Observed Democratic Margin","Counterfactual Democratic Margin","Election Context","Observed Quality Challenge","Counterfactual Quality Challenge"),title = "2022 U.S. Senate Election Quality Counterfactual Outcomes",out="/Dataverese Replication/table2_senate_counterfactual_2022.html",notes= "2022 U.S. Senate Quality Advantage Estimate = 7.49% [95% CI: 7.01, 7.97]")

house_2022$advantage_coeff <- NULL
rownames(house_2022) <- as.character(d)

stargazer(house_2022,summary =  F,type="text",digits=2,covariate.labels =c("District","Outcome/Counterfactual","Observed Democratic Margin","Counterfactual Democratic Margin","Election Context","Observed Quality Challenge","Counterfactual Quality Challenge"),title = "2022 U.S. House Election Quality Counterfactual Outcomes",out="/Dataverese Replication/table1_house_counterfactual_2022.html",notes= "2022 U.S. House Quality Advantage Estimate = 0.58% [95% CI: 0.42, 0.75]")

#### 3) R^2 Analysis of the Models ####

summary(model1 <- lm(ipv_margin~ip_incumbency+ip_quality_advantage+ip_pres_margin+dem_seat + year_fe,data=house))
summary(model2 <- lm(dv_margin~dem_incumbency+dem_quality_advantage+dpres_margin+dem_seat  + year_fe,data=house))

summary(model3 <- lm(ipv_margin~ip_pres_margin+ year_fe,data=house))
summary(model4 <- lm(dv_margin~dpres_margin+ year_fe,data=house))

base_line_house_r2 <- data.frame(year=c("Baseline","Baseline"),adj_r2=c(summary(model1)$adj.r.squared,summary(model2)$adj.r.squared,summary(model3)$adj.r.squared,summary(model4)$adj.r.squared),r2=c(summary(model1)$r.squared,summary(model2)$r.squared,summary(model3)$r.squared,summary(model4)$r.squared),chamber="U.S. House",model=c("Incumbent Party Model","Revised Jacobson Model","Incumbent Party Model","Revised Jacobson Model"),model2=c("Full Model","Full Model","Partisanship Only Model","Partisanship Only Model"))

summary(model1 <- lm(ipv_margin~ip_incumbency+ip_quality_advantage+ip_pres_margin+dem_seat + year_fe,data=senate))
summary(model2 <- lm(dv_margin~dem_incumbency+dem_quality_advantage+dpres_margin+dem_seat  + year_fe,data=senate))

summary(model3 <- lm(ipv_margin~ip_pres_margin+ year_fe,data=senate))
summary(model4 <- lm(dv_margin~dpres_margin+ year_fe,data=senate))

base_line_senate_r2 <- data.frame(year=c("Baseline","Baseline"),adj_r2=c(summary(model1)$adj.r.squared,summary(model2)$adj.r.squared,summary(model3)$adj.r.squared,summary(model4)$adj.r.squared),r2=c(summary(model1)$r.squared,summary(model2)$r.squared,summary(model3)$r.squared,summary(model4)$r.squared),chamber="U.S. Senate",model=c("Incumbent Party Model","Revised Jacobson Model","Incumbent Party Model","Revised Jacobson Model"),model2=c("Full Model","Full Model","Partisanship Only Model","Partisanship Only Model"))

senate_r2_partisanship <- list()
senate_r2_complete <- list()

for(i in seq(1914,2022,2)){
  x <- subset(senate,senate$year == i)
  summary(model1 <- lm(ipv_margin~ip_incumbency+ip_quality_advantage+ip_pres_margin+dem_seat,data=x))
  summary(model2 <- lm(dv_margin~dem_incumbency+dem_quality_advantage+dpres_margin+dem_seat,data=x))
  y <- data.frame(year=c(i),adj_r2=c(summary(model1)$adj.r.squared,summary(model2)$adj.r.squared),r2=c(summary(model1)$r.squared,summary(model2)$r.squared),chamber="U.S. Senate",model=c("Incumbent Party Model","Revised Jacobson Model"),model2=c("Full Model","Full Model"))
  senate_r2_complete[[i]] <- y
  
  summary(model1 <- lm(ipv_margin~ip_pres_margin,data=x))
  summary(model2 <- lm(dv_margin~dpres_margin,data=x))
  y <- data.frame(year=c(i),adj_r2=c(summary(model1)$adj.r.squared,summary(model2)$adj.r.squared),r2=c(summary(model1)$r.squared,summary(model2)$r.squared),chamber="U.S. Senate",model=c("Incumbent Party Model","Revised Jacobson Model"),model2=c("Partisanship Only Model","Partisanship Only Model"))
  senate_r2_partisanship[[i]] <- y
}
 
house_r2_partisanship <- list()
house_r2_complete <- list()

for(i in seq(1900,2022,2)){
  x <- subset(house,house$year == i)
  summary(model1 <- lm(ipv_margin~ip_incumbency+ip_quality_advantage+ip_pres_margin+dem_seat,data=x))
  summary(model2 <- lm(dv_margin~dem_incumbency+dem_quality_advantage+dpres_margin+dem_seat,data=x))
  y <- data.frame(year=c(i),adj_r2=c(summary(model1)$adj.r.squared,summary(model2)$adj.r.squared),r2=c(summary(model1)$r.squared,summary(model2)$r.squared),chamber="U.S. House",model=c("Incumbent Party Model","Revised Jacobson Model"),model2=c("Full Model","Full Model"))
  house_r2_complete[[i]] <- y
  
  summary(model1 <- lm(ipv_margin~ip_pres_margin,data=x))
  summary(model2 <- lm(dv_margin~dpres_margin,data=x))
  y <- data.frame(year=c(i),adj_r2=c(summary(model1)$adj.r.squared,summary(model2)$adj.r.squared),r2=c(summary(model1)$r.squared,summary(model2)$r.squared),chamber="U.S. House",model=c("Incumbent Party Model","Revised Jacobson Model"),model2=c("Partisanship Only Model","Partisanship Only Model"))
  house_r2_partisanship[[i]] <- y
}

house_r2_partisanship <- ldply(house_r2_partisanship)
house_r2_complete <- ldply(house_r2_complete)

senate_r2_partisanship <- ldply(senate_r2_partisanship)
senate_r2_complete <- ldply(senate_r2_complete)

house_r2 <- rbind(house_r2_partisanship,house_r2_complete,base_line_house_r2)
senate_r2 <- rbind(senate_r2_partisanship,senate_r2_complete,base_line_senate_r2)

x <- senate_r2
y <- subset(x,x$year == 1914)
y$year <- 1912
y$adj_r2 <- NA
y$r2 <- NA

x <- rbind(x,y)

x$year <- factor(x$year,levels=c("Baseline",seq(1912,2022,2)))

y <- subset(x,x$model2 == "Partisanship Only Model")
y$model2 <- NULL
colnames(y) <- c("year","part_adj_r2","part_r2","chamber","model")

x <- merge(x,y,by=c("year","chamber","model"))

x$r2_code <- ifelse(x$model2 == "Full Model",x$r2-x$part_r2,x$r2)
y <- subset(x,as.numeric(as.character(x$year)) >= 2012)
y <- subset(y,y$model == "Revised Jacobson Model")
y <- subset(y,y$model2 == "Full Model")

x$model2 <- factor(x$model2,levels=c("Full Model","Partisanship Only Model"))
x$model <- factor(x$model,levels=c("Revised Jacobson Model","Incumbent Party Model"))

plot <- ggplot(x,aes(x=year,y=r2_code,factor=year,fill=model2)) + geom_bar(stat="identity") + theme_bw()  + geom_vline(xintercept = "1912",colour = gray(1/2), lty = 1) + scale_x_discrete("",breaks= c("Baseline",as.character(seq(1922,2022,10))),labels=c("PB",as.character(seq(1922,2022,10)))) + scale_y_continuous(bquote('Cross-Sectional Model'~R^2),limits=c(0,1)) + facet_wrap(~model,ncol = 1) + theme(legend.position = "bottom") + scale_fill_manual("",values=c("gray","black")) + theme(axis.text.x = element_text(size = 8)) + labs(caption=bquote('PB refers to the pooled baseline year fixed-effects model'~R^2~'of all U.S. Senate elections from 1914-2022.'))
ggsave(file="/Dataverese Replication/Fig4b_r2_senate.png", plot, width = 6.5, height = 5, units = "in")


x <- house_r2
y <- subset(x,x$year == 1900)
y$year <- 1898
y$adj_r2 <- NA
y$r2 <- NA

x <- rbind(x,y)

x$year <- factor(x$year,levels=c("Baseline",seq(1898,2022,2)))

y <- subset(x,x$model2 == "Partisanship Only Model")
y$model2 <- NULL
colnames(y) <- c("year","part_adj_r2","part_r2","chamber","model")

x <- merge(x,y,by=c("year","chamber","model"))

x$r2_code <- ifelse(x$model2 == "Full Model",x$r2-x$part_r2,x$r2)
x$model2 <- factor(x$model2,levels=c("Full Model","Partisanship Only Model"))
x$model <- factor(x$model,levels=c("Revised Jacobson Model","Incumbent Party Model"))

plot <- ggplot(x,aes(x=year,y=r2_code,factor=year,fill=model2)) + geom_bar(stat="identity") + theme_bw()  + geom_vline(xintercept = "1898",colour = gray(1/2), lty = 1) + scale_x_discrete("",breaks= c("Baseline",as.character(seq(1902,2022,10))),labels=c("PB",as.character(seq(1902,2022,10)))) + scale_y_continuous(bquote('Cross-Sectional Model'~R^2),limits=c(0,1)) + facet_wrap(~model,ncol = 1) + theme(legend.position = "bottom") + scale_fill_manual("",values=c("gray","black")) + theme(axis.text.x = element_text(size = 8)) + labs(caption=bquote('PB refers to the pooled baseline year fixed-effects model'~R^2~'of all U.S. House elections from 1900-2022.'))
ggsave(file="/Dataverese Replication/Fig4a_r2_house.png", plot, width = 6.5, height = 5, units = "in")  

#### 4) Temporal Heterogenity in House & Senate Quality Challengers ####


house_incumbents <- subset(house,house$inc %in% c("Democratic Incumbent","GOP Incumbent"))

house_incumbents$quality_challenger <- ifelse(house_incumbents$po1 %in% "Quality Challenger",1,ifelse(house_incumbents$po1 %in% "Amateur Challenger",0,NA))

h <- subset(house_incumbents,house_incumbents$year == 2022)

house_incumbents <- subset(house_incumbents,house_incumbents$year != 2022)

h$unopposed_race <- ifelse(h$po1 %in% "No challenger",1,0)
house_incumbents$unopposed_race <- ifelse(is.na(house_incumbents$dv),1,0)

house_incumbents <- rbind(house_incumbents,h)

house_incumbents$dem_control <- ifelse(house_incumbents$year %in% c(1911:1919,1931:1946,1949:1953,1955:1994,2007:2010,2019:2022),1,0)

house_incumbents$dem_seat <- ifelse(house_incumbents$inc %in% c("Democratic Incumbent"),1,0)

house_incumbents$majority_party <- ifelse(house_incumbents$dem_control == house_incumbents$dem_seat,"Majority Party","Minority Party")

x <- ddply(house_incumbents,.(year),summarise,quality=mean(quality_challenger,na.rm=T))

senate_incumbents <- subset(senate,senate$inc %in% c("Appointed Democratic Senator Election","Appointed Republican Senator Election","Democratic Senator re-election","Republican Senator re-election"))

senate_incumbents$quality_challenger <- ifelse(senate_incumbents$po1 %in% "Quality Challenger",1,0)

senate_incumbents$dem_control <- ifelse(senate_incumbents$year %in% c(1913:1919,1933:1947,1949:1953,1955:1981,1987:1995,2001:2002,2007:2015,2021:2022),1,0)

senate_incumbents$dem_seat <- ifelse(senate_incumbents$inc %in% c("Democratic Senator re-election","Appointed Democratic Senator Election"),1,0)

senate_incumbents$majority_party <- ifelse(senate_incumbents$dem_control == senate_incumbents$dem_seat,"Majority Party","Minority Party")

y <- subset(senate_incumbents,senate_incumbents$year %in% seq(1914,2022,2))
y <- subset(y,y$po1 != "No challenger")

y <- ddply(y,.(year),summarise,quality=mean(quality_challenger,na.rm=T))

y$office <- "U.S. Senate"
x$office <- "U.S. House"

quality_challengers <- rbind(x,y)

plot <- ggplot(quality_challengers,aes(x=year,y=quality)) + geom_point(shape=21) + stat_smooth(method="loess") + theme_bw() + facet_wrap(~office) + scale_y_continuous("Proportion of Challengers with Previous Elected Office",breaks=seq(0,1,0.10)) + scale_x_continuous("",breaks=seq(1900,2020,20)) + labs(caption="Data: U.S. House Elections, 1900-2020 & U.S. Senate Elections, 1914-2020") #labs(title="Proportion of Quality Challengers Opposing Incumbents in Congressional Elections",caption="Data: U.S. House Elections, 1900-2020 & U.S. Senate Elections, 1914-2020")
ggsave(file="/Dataverese Replication/Fig5_quality_challengers.png", plot, width = 7.5, height = 4.63, units = "in")  