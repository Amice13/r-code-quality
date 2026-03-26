# This R Script replicates the analysis in the manuscript titled: "Do Quality Candidates and Incumbents Still Matter in the Partisan World? Comparing Trends & Relationship Between Candidate Differentials and Congressional Election Outcomes, 1900-2022" published in the Journal of Political Marketing. Special Issue on the 2022 Midterm Congressional Election.

# The output of this R File are:

# Fig 1: Pooled baseline incumbency advantage, quality advantage, and state partisanship. Dynamics in U.S. House and Senate elections.

# Fig 2: Temporal variation in incumbency advantage, quality advantage, and partisanship. Dynamics in U.S. House and Senate elections.

# Fig 3: Baseline and temporal variation in quality challenges to incumbents in u.S. House and Senate elections.

# Fig 4: Predictive power of both models in u.S. House and Senate elections.

# Fig 5: The decline of quality challengers in u.S. House and Senate elections.

# Load Packages

library(tidyverse)
library(marginaleffects)
library(sandwich)
library(lmtest)

#### 1) Load House & Senate Election Data from Local Dataverse Folder ####

load("/Dataverese Replication/senate_races_1914_2022.Rdata")

load("/Dataverese Replication/house_races_1900_2022.Rdata")

#### 2) Code variables in the Democratic direction ####

house$dem_incumbency <- ifelse(house$inc %in% c("Democratic Incumbent"),1,ifelse(house$inc %in% c("GOP Incumbent"),-1,ifelse(house$inc %in% c("Democratic Open Seat","GOP Open Seat","GOP Incum v. Dem Incum (redistricting)","New Open Seat (reapportioned)","Two Republicans, open seat","Two Democrats, open seat"),0,NA)))

senate$dem_incumbency <- ifelse(senate$inc %in% c("Appointed Democratic Senator Election","Democratic Senator re-election"),1,ifelse(senate$inc %in% c("Appointed Republican Senator Election","Republican Senator re-election"),-1,ifelse(senate$inc %in% c("Democratic open seat","Democratic Vacant seat","3rd Party Open seat","Independent Open seat","Republican open seat","Republican Vacant seat"),0,NA)))

senate$dem_seat <- ifelse(senate$inc %in% c("Appointed Democratic Senator Election","Democratic Senator re-election","Democratic Vacant seat","Democratic open seat"),1,0)

house$dem_seat <- ifelse(house$inc %in% c("Dem Incum v. Dem Challenger","Democratic Incumbent","Democratic Open Seat","Two Democratic Incumbents","Two Democrats, open seat"),1,0)

senate$dem_quality_advantage <- ifelse(senate$dem_incumbency %in% 1 & senate$po1 %in% "Amateur Challenger", 1,ifelse(senate$dem_incumbency %in% -1 & senate$po1 %in% "Amateur Challenger",-1,ifelse(senate$dem_incumbency %in% 1 & senate$po1 %in% "Quality Challenger", 0,ifelse(senate$dem_incumbency %in% -1 & senate$po1 %in% "Quality Challenger",0,ifelse(senate$dem_incumbency %in% 0 & senate$dem_seat %in% 0 & senate$po1 %in% "Quality Challenger",1,ifelse(senate$dem_incumbency %in% 0 & senate$dem_seat %in% 1 & senate$po1 %in% "Quality Challenger",-1,ifelse(senate$po1 %in% "Only Democratic Quality Candidate (open)",1,ifelse(senate$po1 %in% "Only GOP Quality Candidate (open)",-1,ifelse(senate$dem_incumbency %in% 0 & senate$dem_seat %in% 0 & senate$po1 %in% "Amateur Challenger",-1,ifelse(senate$dem_incumbency %in% 0 & senate$dem_seat %in% 1 & senate$po1 %in% "Amateur Challenger",1,ifelse(senate$po1 %in% "Both Amateur candidates (open)",0,ifelse(senate$po1 %in% "Both Quality Candidates (open)",0,NA))))))))))))

senate$dem_quality_advantage [senate$year %in% 2022 & senate$state %in% "UT"] <- -1

house$po1[house$po1 %in% "Both Amateur candidates (open)"] <- "Both Amateur Candidates (open)"

house$dem_quality_advantage <- ifelse(house$dem_incumbency %in% 1 & house$po1 %in% "Amateur Challenger",1,ifelse(house$dem_incumbency %in% -1 & house$po1 %in% "Amateur Challenger",-1,ifelse(house$dem_incumbency %in% 1 & house$po1 %in% "Quality Challenger", 0,ifelse(house$dem_incumbency %in% -1 & house$po1 %in% "Quality Challenger",0,ifelse(house$dem_incumbency %in% 0 & house$dem_seat %in% 1 & house$po1 %in% "Quality Challenger",-1,ifelse(house$dem_incumbency %in% 0 & house$dem_seat %in% 0 & house$po1 %in% "Quality Challenger",1,ifelse(house$po1 %in% "Only Democratic Quality Candidate (open)",1,ifelse(house$po1 %in% "Only GOP Quality Candidate (open)",-1,ifelse(house$dem_incumbency %in% 0 & house$dem_seat %in% 0 & house$po1 %in% "Amateur Challenger",-1,ifelse(house$dem_incumbency %in% 0 & house$dem_seat %in% 1 & house$po1 %in% "Amateur Challenger",1,ifelse(house$po1 %in% "Both Amateur Candidates (open)",0,ifelse(house$po1 %in% "Both Quality Candidates (open)",0,NA))))))))))))

#### 3) Code variables in the Incumbent Party Direction ####

house$ipv <- ifelse(house$dem_seat %in% 1,house$dv,ifelse(house$dem_seat %in% 0,100-house$dv,NA))

house$ipvp <- ifelse(house$dem_seat %in% 1,house$dvp,ifelse(house$dem_seat %in% 0,100-house$dvp,NA))

house$ip_incumbency <- ifelse(house$dem_seat == 1 & house$dem_incumbency == 1,1,ifelse(house$dem_seat == 0 & house$dem_incumbency == -1,1,0))

house$ip_quality_advantage <- ifelse(house$dem_seat == 1 & house$dem_quality_advantage == 1,1,ifelse(house$dem_seat == 0 & house$dem_quality_advantage == -1,1,0))

house$ip_pres <- ifelse(house$dem_seat == 1,house$dpres,ifelse(house$dem_seat == 0,100-house$dpres,NA))

senate$ipv <- ifelse(senate$dem_seat %in% 1,senate$dv,ifelse(senate$dem_seat %in% 0,100-senate$dv,NA))

senate$ipvp <- ifelse(senate$dem_seat %in% 1,senate$dvp,ifelse(senate$dem_seat %in% 0,100-senate$dvp,NA))

senate$ip_incumbency <- ifelse(senate$dem_seat == 1 & senate$dem_incumbency == 1,1,ifelse(senate$dem_seat == 0 & senate$dem_incumbency == -1,1,0))

senate$ip_quality_advantage <- ifelse(senate$dem_seat == 1 & senate$dem_quality_advantage == 1,1,ifelse(senate$dem_seat == 0 & senate$dem_quality_advantage == -1,1,0))

senate$ip_pres <- ifelse(senate$dem_seat == 1,senate$dpres,ifelse(senate$dem_seat == 0,100-senate$dpres,NA))

senate$year_fe <- factor(senate$year,levels=seq(1914,2022,2))

house$year_fe <- factor(house$year,levels=seq(1900,2022,2))

house$dem_seat[house$dem_seat == 0] <- -1

house$dem_seat[house$inc %in% "New Open Seat (reapportioned)"] <- 0

#### 4) Revised Jacobson Model Estimation & Temporal Variation Figures ####

house$dv_margin <- house$dv-(100-house$dv)

house$dpres_margin <- house$dpres-(100-house$dpres)

summary(model <- lm(dv_margin~dem_incumbency+dem_quality_advantage+dpres_margin+dem_seat  + year_fe,data=house))

margins_house_pooled_non_linear <- tidy(marginaleffects(model,variables=c("dem_incumbency","dem_quality_advantage","dpres_margin"),vcov=sandwich::vcovCL(model,~year_fe)))
margins_house_pooled_non_linear$df <- df.residual(model)
margins_house_pooled_non_linear$n <- nobs(model)

summary(model <- lm(dv_margin~dem_incumbency*year_fe+dem_quality_advantage*year_fe+dpres_margin*year_fe+dem_seat ,data=house))

margins_house_year_non_linear <- tidy(marginaleffects(model,variables=c("dem_incumbency","dem_quality_advantage","dpres_margin"),by="year_fe",vcov=sandwich::vcovCL(model,~year_fe),newdata =  datagridcf(year_fe = seq(1900,2022,2))))
margins_house_year_non_linear$df <- df.residual(model)
margins_house_year_non_linear$n <- nobs(model)


# Senate 
senate$dv_margin <- senate$dv-(100-senate$dv)

senate$dpres_margin <- senate$dpres-(100-senate$dpres)

summary(model <- lm(dv_margin~dem_incumbency+dem_quality_advantage+dpres_margin+dem_seat  + year_fe,data=senate))

margins_senate_pooled_non_linear <- tidy(marginaleffects(model,variables=c("dem_incumbency","dem_quality_advantage","dpres_margin"),vcov=sandwich::vcovCL(model,~year_fe)))
margins_senate_pooled_non_linear$df <- df.residual(model)
margins_senate_pooled_non_linear$n <- nobs(model)

summary(model <- lm(dv_margin~dem_incumbency*year_fe+dem_quality_advantage*year_fe+dpres_margin*year_fe+dem_seat ,data=senate))

margins_senate_year_non_linear <- tidy(marginaleffects(model,variables=c("dem_incumbency","dem_quality_advantage","dpres_margin"),by="year_fe",vcov=sandwich::vcovCL(model,~year_fe),newdata =  datagridcf(year_fe = seq(1914,2022,2))))
margins_senate_year_non_linear$df <- df.residual(model)
margins_senate_year_non_linear$n <- nobs(model)

# Figures

margins_senate_pooled_non_linear$chamber <- "United States Senate"
margins_house_pooled_non_linear$chamber <- "United States House"

margins_house_pooled_non_linear$year_fe <- "Baseline"
margins_senate_pooled_non_linear$year_fe <- "Baseline"

margins_senate_year_non_linear$chamber <- "United States Senate"
margins_house_year_non_linear$chamber <- "United States House"

margins_house_pooled_non_linear$contrast <- NA
margins_senate_pooled_non_linear$contrast <- NA

x <- rbind(margins_senate_year_non_linear,margins_house_year_non_linear)

x$var <- ifelse(x$term %in% "dem_incumbency","Incumbency Advantage",ifelse(x$term %in% "dpres_margin","Partisan Advantage",ifelse(x$term %in% "dem_quality_advantage","Quality Advantage",NA)))

x$year_fe <- as.numeric(as.character(x$year_fe))

x$color_sig <- ifelse(x$p.value < 0.10,"sig","no sig")

x1 <- subset(x,x$var %in% "Incumbency Advantage")

plot <- ggplot(x1,aes(x=year_fe,y=estimate,alpha=color_sig)) + facet_grid(~chamber,scales="free") + geom_pointrange(aes(x= year_fe, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_continuous("",breaks=seq(1900,2020,20)) + scale_y_continuous("Incumbency Advantage",breaks=seq(-10,25,5),labels=paste(seq(-10,25,5),"%",sep="")) + scale_alpha_manual("",values=c(0.2,1),guide="none") + geom_smooth(aes(x=year_fe,y=estimate),method="loess",inherit.aes = F) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(plot,file="/Dataverese Replication/Fig2a_jacobson_incumbency_advantage.png", width = 7, height = 4.5, units = "in")

x1 <- subset(x,x$var %in% "Quality Advantage")

plot <- ggplot(x1,aes(x=year_fe,y=estimate,alpha=color_sig)) + facet_grid(~chamber,scales="free") + geom_pointrange(aes(x= year_fe, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_continuous("",breaks=seq(1900,2020,20)) + scale_y_continuous("Quality Advantage",breaks=seq(-10,25,5),labels=paste(seq(-10,25,5),"%",sep="")) + scale_alpha_manual("",values=c(0.2,1),guide="none") + geom_smooth(aes(x=year_fe,y=estimate),method="loess",inherit.aes = F) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(plot,file="/Dataverese Replication/Fig2c_jacobson_quality_advantage.png", width = 7, height = 4.5, units = "in")

x1 <- subset(x,x$var %in% "Partisan Advantage")

plot <- ggplot(x1,aes(x=year_fe,y=estimate)) + facet_grid(~chamber,scales="free") + geom_pointrange(aes(x= year_fe, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_continuous("",breaks=seq(1900,2020,20)) + scale_y_continuous("Partisan Advantage") + scale_alpha_manual("",values=c(0.2,1),guide="none") + geom_smooth(aes(x=year_fe,y=estimate),method="loess",inherit.aes = F) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(plot,file="/Dataverese Replication/Fig2e_jacobson_partisan_advantage.png", width = 7, height = 4.5, units = "in")

jacobson_model_yearly <- x

jacobson_model_pooled <- rbind(margins_house_pooled_non_linear,margins_senate_pooled_non_linear)

#### 5) Incumbent Party Model Estimation & Temporal Variation Figures ####

house$ipv_margin <- house$ipv-(100-house$ipv)

house$ip_pres_margin <- house$ip_pres-(100-house$ip_pres)

summary(model <- lm(ipv_margin~ip_incumbency+ip_quality_advantage+ip_pres_margin+dem_seat + year_fe,data=house))

margins_house_pooled_non_linear <- tidy(marginaleffects(model,variables=c("ip_incumbency","ip_quality_advantage","ip_pres_margin"),vcov=sandwich::vcovCL(model,~year_fe)))

summary(model <- lm(ipv_margin~ip_incumbency*year_fe+ip_quality_advantage*year_fe+ip_pres_margin*year_fe+dem_seat,data=house))

margins_house_year_non_linear <- tidy(marginaleffects(model,variables=c("ip_incumbency","ip_quality_advantage","ip_pres_margin"),by="year_fe",vcov=sandwich::vcovCL(model,~year_fe),newdata =  datagridcf(year_fe = seq(1900,2022,2))))

# Senate 
senate$ipv_margin <- senate$ipv-(100-senate$ipv)

senate$ip_pres_margin <- senate$ip_pres-(100-senate$ip_pres)

summary(model <- lm(ipv_margin~ip_incumbency+ip_quality_advantage+ip_pres_margin+dem_seat + year_fe,data=senate))

margins_senate_pooled_non_linear <- tidy(marginaleffects(model,variables=c("ip_incumbency","ip_quality_advantage","ip_pres_margin"),vcov=sandwich::vcovCL(model,~year_fe)))

summary(model <- lm(ipv_margin~ip_incumbency*year_fe+ip_quality_advantage*year_fe+ip_pres_margin*year_fe+dem_seat,data=senate))

margins_senate_year_non_linear <- tidy(marginaleffects(model,variables=c("ip_incumbency","ip_quality_advantage","ip_pres_margin"),by="year_fe",vcov=sandwich::vcovCL(model,~year_fe),newdata =  datagridcf(year_fe = seq(1914,2022,2))))

# Figures

margins_senate_pooled_non_linear$chamber <- "United States Senate"
margins_house_pooled_non_linear$chamber <- "United States House"

margins_house_pooled_non_linear$year_fe <- "Baseline"
margins_senate_pooled_non_linear$year_fe <- "Baseline"

margins_senate_year_non_linear$chamber <- "United States Senate"
margins_house_year_non_linear$chamber <- "United States House"

margins_house_pooled_non_linear$contrast <- NA
margins_senate_pooled_non_linear$contrast <- NA

x <- rbind(margins_senate_year_non_linear,margins_house_year_non_linear)

x$var <- ifelse(x$term %in% "ip_incumbency","Incumbency Advantage",ifelse(x$term %in% "ip_pres_margin","Partisan Advantage",ifelse(x$term %in% "ip_quality_advantage","Quality Advantage",NA)))

x$year_fe <- as.numeric(as.character(x$year_fe))

x$color_sig <- ifelse(x$p.value < 0.10,"sig","no sig")

x1 <- subset(x,x$var %in% "Incumbency Advantage")

plot <- ggplot(x1,aes(x=year_fe,y=estimate,alpha=color_sig)) + facet_grid(~chamber,scales="free") + geom_pointrange(aes(x= year_fe, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_continuous("",breaks=seq(1900,2020,20)) + scale_y_continuous("Incumbency Advantage",breaks=seq(-30,25,5),labels=paste(seq(-30,25,5),"%",sep="")) + scale_alpha_manual("",values=c(0.2,1),guide="none") + geom_smooth(aes(x=year_fe,y=estimate),method="loess",inherit.aes = F) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(plot,file="/Dataverese Replication/Fig2b_inc_party_incumbency_advantage.png", width = 7, height = 4.5, units = "in")

x1 <- subset(x,x$var %in% "Quality Advantage")

plot <- ggplot(x1,aes(x=year_fe,y=estimate,alpha=color_sig)) + facet_grid(~chamber,scales="free") + geom_pointrange(aes(x= year_fe, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_continuous("",breaks=seq(1900,2020,20)) + scale_y_continuous("Quality Advantage",breaks=seq(-10,25,5),labels=paste(seq(-10,25,5),"%",sep="")) + scale_alpha_manual("",values=c(0.2,1),guide="none") + geom_smooth(aes(x=year_fe,y=estimate),method="loess",inherit.aes = F) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(plot,file="/Dataverese Replication/Fig2d_inc_party_quality_advantage.png", width = 7, height = 4.5, units = "in")

x1 <- subset(x,x$var %in% "Partisan Advantage")

plot <- ggplot(x1,aes(x=year_fe,y=estimate)) + facet_grid(~chamber,scales="free") + geom_pointrange(aes(x= year_fe, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_continuous("",breaks=seq(1900,2020,20)) + scale_y_continuous("Partisan Advantage") + scale_alpha_manual("",values=c(0.2,1),guide="none") + geom_smooth(aes(x=year_fe,y=estimate),method="loess",inherit.aes = F) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(plot,file="/Dataverese Replication/Fig2f_inc_party_partisan_advantage.png", width = 7, height = 4.5, units = "in")

inc_pty_model_yearly <- x

inc_pty_model_pooled <- rbind(margins_house_pooled_non_linear,margins_senate_pooled_non_linear)

#### 6) Quality Challenges Model Estimation & Temporal Variation Figures ####

h_incs <- subset(house,house$ip_incumbency == 1)
h_incs$quality_challenger <- ifelse(h_incs$po1 %in% "Amateur Challenger",0,ifelse(h_incs$po1 %in% "Quality Challenger",1,NA))

summary(model <- lm(ipv_margin~quality_challenger+ip_pres_margin+dem_seat + year_fe,data=h_incs))

margins_house_pooled_non_linear <- tidy(marginaleffects(model,variables=c("quality_challenger","ip_pres_margin"),vcov=sandwich::vcovCL(model,~year_fe)))

summary(model <- lm(ipv_margin~quality_challenger*year_fe+ip_pres_margin*year_fe+dem_seat,data=h_incs))

margins_house_year_non_linear <- tidy(marginaleffects(model,variables=c("quality_challenger","ip_pres_margin"),by="year_fe",vcov=sandwich::vcovCL(model,~year_fe),newdata =  datagridcf(year_fe = seq(1900,2022,2))))

# Senate 

s_incs <- subset(senate,senate$ip_incumbency == 1)
s_incs$quality_challenger <- ifelse(s_incs$po1 %in% "Amateur Challenger",0,ifelse(s_incs$po1 %in% "Quality Challenger",1,NA))

summary(model <- lm(ipv_margin~quality_challenger+ip_pres_margin+dem_seat + year_fe,data=s_incs))

margins_senate_pooled_non_linear <- tidy(marginaleffects(model,variables=c("quality_challenger","ip_pres_margin"),vcov=sandwich::vcovCL(model,~year_fe)))

summary(model <- lm(ipv_margin~quality_challenger*year_fe+ip_pres_margin*year_fe+dem_seat,data=s_incs))

margins_senate_year_non_linear <- tidy(marginaleffects(model,variables=c("quality_challenger","ip_pres_margin"),by="year_fe",vcov=sandwich::vcovCL(model,~year_fe),newdata =  datagridcf(year_fe = seq(1914,2022,2))))

# Figures

margins_senate_pooled_non_linear$chamber <- "United States Senate"
margins_house_pooled_non_linear$chamber <- "United States House"

margins_house_pooled_non_linear$year_fe <- "Baseline"
margins_senate_pooled_non_linear$year_fe <- "Baseline"

margins_senate_year_non_linear$chamber <- "United States Senate"
margins_house_year_non_linear$chamber <- "United States House"

margins_house_pooled_non_linear$contrast <- NA
margins_senate_pooled_non_linear$contrast <- NA

x <- rbind(margins_senate_year_non_linear,margins_house_year_non_linear)

x$var <- ifelse(x$term %in% "ip_pres_margin","Partisan Advantage",ifelse(x$term %in% "quality_challenger","Quality Challenger",NA))

x$year_fe <- as.numeric(as.character(x$year_fe))

x$color_sig <- ifelse(x$p.value < 0.10,"sig","no sig")

x1 <- subset(x,x$var %in% "Quality Challenger")

plot <- ggplot(x1,aes(x=year_fe,y=estimate,alpha=color_sig)) + facet_grid(~chamber,scales="free") + geom_pointrange(aes(x= year_fe, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_continuous("",breaks=seq(1900,2020,20)) + scale_y_continuous("Quality Challenger",breaks=seq(-30,25,5),labels=paste(seq(-30,25,5),"%",sep="")) + scale_alpha_manual("",values=c(0.2,1),guide="none") + geom_smooth(aes(x=year_fe,y=estimate),method="loess",inherit.aes = F) + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5))
ggsave(plot,file="/Dataverese Replication/Fig3b_inc_party_quality_challenger.png", width = 7, height = 4.5, units = "in")

inc_pty_challenger_model_yearly <- x

inc_pty_challenger_model_pooled <- rbind(margins_house_pooled_non_linear,margins_senate_pooled_non_linear)

#### 7) Pooled Baseline Model Figures ####

jacobson_model_pooled$var2 <- ifelse(jacobson_model_pooled$term %in% "dem_incumbency","Incumbency Advantage",ifelse(jacobson_model_pooled$term %in% "dem_quality_advantage","Quality Advantage",ifelse(jacobson_model_pooled$term %in% "dpres_margin","Partisan Advantage",NA)))

jacobson_model_pooled$label <- ifelse(jacobson_model_pooled$p.value < 0.10 & jacobson_model_pooled$p.value > 0.05,paste(round(jacobson_model_pooled$estimate,2),"*",sep=""),ifelse(jacobson_model_pooled$p.value < 0.05 & jacobson_model_pooled$p.value > 0.01,paste(round(jacobson_model_pooled$estimate,2),"**",sep=""),ifelse(jacobson_model_pooled$p.value < 0.01,paste(round(jacobson_model_pooled$estimate,2),"***",sep=""),NA)))

plot <- ggplot(jacobson_model_pooled,aes(x=chamber,y=estimate,label=label)) + facet_wrap(~var2,scales="free") + geom_pointrange(aes(x= chamber, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_discrete("",labels=c("U.S. House","U.S. Senate")) + scale_y_continuous("Parameter Estimates") + scale_alpha_manual("",values=c(0.2,1),guide="none") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + geom_text(hjust=-0.30,size=2.75) + labs(caption="* p < 0.10 ** p < 0.05; *** p < 0.01.")
ggsave(plot,file="/Dataverese Replication/Fig1a_pooled_jacobson_model.png", width = 7, height = 4.5, units = "in")

inc_pty_model_pooled$var2 <- ifelse(inc_pty_model_pooled$term %in% "ip_incumbency","Incumbency Advantage",ifelse(inc_pty_model_pooled$term %in% "ip_quality_advantage","Quality Advantage",ifelse(inc_pty_model_pooled$term %in% "ip_pres_margin","Partisan Advantage",NA)))

inc_pty_model_pooled$label <- ifelse(inc_pty_model_pooled$p.value < 0.10 & inc_pty_model_pooled$p.value > 0.05,paste(round(inc_pty_model_pooled$estimate,2),"*",sep=""),ifelse(inc_pty_model_pooled$p.value < 0.05 & inc_pty_model_pooled$p.value > 0.01,paste(round(inc_pty_model_pooled$estimate,2),"**",sep=""),ifelse(inc_pty_model_pooled$p.value < 0.01,paste(round(inc_pty_model_pooled$estimate,2),"***",sep=""),NA)))

plot <- ggplot(inc_pty_model_pooled,aes(x=chamber,y=estimate,label=label)) + facet_wrap(~var2,scales="free") + geom_pointrange(aes(x= chamber, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_discrete("",labels=c("U.S. House","U.S. Senate")) + scale_y_continuous("Parameter Estimates") + scale_alpha_manual("",values=c(0.2,1),guide="none") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + geom_text(hjust=-0.30,size=2.75) + labs(caption="* p < 0.10 ** p < 0.05; *** p < 0.01.")
ggsave(plot,file="/Dataverese Replication/Fig1b_pooled_inc_pty_model.png", width = 7, height = 4.5, units = "in")

inc_pty_challenger_model_pooled$var2 <- ifelse(inc_pty_challenger_model_pooled$term %in% "quality_challenger","Quality Challenger",ifelse(inc_pty_challenger_model_pooled$term %in% "ip_pres_margin","Partisan Advantage",NA))

inc_pty_challenger_model_pooled$label <- ifelse(inc_pty_challenger_model_pooled$p.value < 0.10 & inc_pty_challenger_model_pooled$p.value > 0.05,paste(round(inc_pty_challenger_model_pooled$estimate,2),"*",sep=""),ifelse(inc_pty_challenger_model_pooled$p.value < 0.05 & inc_pty_challenger_model_pooled$p.value > 0.01,paste(round(inc_pty_challenger_model_pooled$estimate,2),"**",sep=""),ifelse(inc_pty_challenger_model_pooled$p.value < 0.01,paste(round(inc_pty_challenger_model_pooled$estimate,2),"***",sep=""),NA)))

plot <- ggplot(subset(inc_pty_challenger_model_pooled,inc_pty_challenger_model_pooled$var2 %in% "Quality Challenger"),aes(x=chamber,y=estimate,label=label)) + facet_wrap(~var2,scales="free") + geom_pointrange(aes(x= chamber, ymin = conf.low, ymax = conf.high), lwd = 1/2,fill="white",shape=21) + theme_bw() + geom_hline(yintercept = 0, colour = "red", lty = 2) + scale_x_discrete("",labels=c("U.S. House","U.S. Senate")) + scale_y_continuous("Parameter Estimates",breaks=seq(-30,25,5),labels=paste(seq(-30,25,5),"%",sep="")) + scale_alpha_manual("",values=c(0.2,1),guide="none") + theme(axis.text.x = element_text(hjust = 0.5),axis.text.y = element_text(hjust = 0.5)) + geom_text(hjust=-0.30,size=2.75) + labs(caption="* p < 0.10 ** p < 0.05; *** p < 0.01.")
ggsave(plot,file="/Dataverese Replication/Fig3a_pooled_inc_pty_chalenger_model.png", width = 7, height = 4.5, units = "in")

#### 8) Save the R Output for Subsequent Analysis in Next R Script #####

save.image("/Dataverese Replication/models_special_issue.Rdata")
