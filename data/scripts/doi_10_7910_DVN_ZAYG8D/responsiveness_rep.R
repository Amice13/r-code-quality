# Introduction ------------------------------------------------------------
# Replication script to make plots and tables for
# "Elections and Policy Responsiveness: Evidence from Environmental Voting in the U.S. Congress", Review of Policy Research
# by Richard J. McAlexander and Johannes Urpelainen
# Please contact richardmcalexander@gmail.com for any questions
####

#LOAD PACKAGES
library(foreign)
library(lubridate)
library(plyr)
library(dplyr)
library(multiwayvcov)
library(lmtest)
library(plm)
library(ggplot2)
library(stargazer)
library(mgcv)
library(zoo)
library(lfe)
library(choroplethr)
library(interplot)
library(gridExtra)
library(gtable)
library(grid)
require(multiwayvcov)

#LOAD BASE DATA
load("responsiveness_rpr.rdata")

#fix oil and gas contribtuions
final$oilgas_contribution_by_party<-as.numeric(final$party=="R")*final$oilgas_contribution_r+
  as.numeric(final$party!="R")*final$oilgas_contribution_d
final$oilgas_contribution_by_party<-log(final$oilgas_contribution_by_party+1)

final$env_contribution_by_party<-as.numeric(final$party=="R")*final$env_contribution_r+
  as.numeric(final$party!="R")*final$env_contribution_d
final$env_contribution_by_party<-log(final$env_contribution_by_party+1)

final$total_contribution_by_party<-as.numeric(final$party=="R")*final$total_contribution_r+
  as.numeric(final$party!="R")*final$total_contribution_d
final$total_contribution_by_party<-log(final$total_contribution_by_party+1)

final$energy_contribution_by_party<-as.numeric(final$party=="R")*final$energy_contribution_r+
  as.numeric(final$party!="R")*final$energy_contribution_r

#fix NA values for oil and gas contributions
final$oilgas_contribution_d[final$oilgas_contribution_d<0]<-NA
final$oilgas_contribution_r[final$oilgas_contribution_r<0]<-NA
final$oilgas_contribution[final$oilgas_contribution<0]<-NA

#MERGE
final<-merge(final,po_data,all.x = TRUE)


# CREATE DESCRIPTIVE PLOTS OF VOTES ---------------------------------------
monthly_ts<-as.data.frame(seq.Date(from=min(final$date_vector),to=max(final$date_vector),by="month"))
gg_data<-as.data.frame(dplyr::summarise(dplyr::group_by(final,floored_date_vector),n=n()))
colnames(monthly_ts)[1]<-colnames(gg_data)[1]
monthly_ts$floored_date_vector<-floor_date(monthly_ts$floored_date_vector,"month")
monthly_ts<-merge(monthly_ts,gg_data,all.x=TRUE)
monthly_ts[is.na(monthly_ts)]<-0

monthly_ts$election_month<-as.numeric(month(monthly_ts$floored_date_vector)==11 &
                                        (year(monthly_ts$floored_date_vector) %% 2)==0
)

monthly_ts$pres_election_month<-as.numeric(month(monthly_ts$floored_date_vector)==11 &
                                             (year(monthly_ts$floored_date_vector) %% 4)==0
)


monthly_ts$moving_average_24<-rollmean(monthly_ts$n,24,fill=NA)

#create data frame with only moths where there is an election
only_election_months<-filter(monthly_ts,election_month==1) %>%
  dplyr::select(floored_date_vector)
colnames(only_election_months)[1]<-"start"
only_election_months$end<-only_election_months$start+months(1)
only_election_months<-as.data.frame(only_election_months)


#two year moving average with all elections
pdf("votes_per_year_2ma_graph.pdf",height = 5, width = 5)
ggplot()+  #geom_rect(data=only_election_months, aes(xmin =start, xmax =end,ymin=0,ymax= +Inf))+
  geom_point(data=na.omit(monthly_ts),aes(x=floored_date_vector,y=moving_average_24))+
  scale_x_date(breaks=only_election_months$start,
               labels=year(only_election_months$start))+
  theme(axis.text.x=element_text(color=c(rep("black",1),"red"),angle = 45,size=6),
        axis.text.y=element_text(size=6),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey")
  )+
  coord_cartesian(ylim=c(0,1500)) +
  xlab("Year") +
  ylab("Number of Votes")+
  ggtitle("Number of Votes Per Year (Two Year MA)")
dev.off()

#raw data
pdf("votes_per_year_raw_graph.pdf",height = 5, width = 5)
ggplot()+  #geom_rect(data=only_election_months, aes(xmin =start, xmax =end,ymin=0,ymax= +Inf))+
  geom_point(data=na.omit(monthly_ts),aes(x=floored_date_vector,y=n))+
  scale_x_date(breaks=only_election_months$start,
               labels=year(only_election_months$start))+
  theme(axis.text.x=element_text(color=c(rep("black",1),"red"),angle = 45,size=6),
        axis.text.y=element_text(size=6),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey")
  )+
  coord_cartesian(ylim=c(0,6000)) +
  xlab("Year") +
  ylab("Number of Votes") +
  ggtitle("Number of Votes Per Year (Raw Data)")
dev.off()


#x axis: years, y axis: share of pro env
pro_env_count<-dplyr::summarise(dplyr::group_by(filter(final,pro_env==1),floored_date_vector),n=n())
colnames(pro_env_count)[2]<-"pro_env_count"
all_votes_count<-dplyr::summarise(dplyr::group_by(final,floored_date_vector),n=n())
colnames(all_votes_count)[2]<-"all_votes_count"
monthly_ts<-merge(monthly_ts,pro_env_count,all.x=TRUE)
monthly_ts<-merge(monthly_ts,all_votes_count,all.x=TRUE)

monthly_ts$pro_env_count[is.na(monthly_ts$pro_env_count)]<-0
monthly_ts$all_votes_count[is.na(monthly_ts$all_votes_count)]<-0
monthly_ts$share_pro_env<- monthly_ts$pro_env_count/ monthly_ts$all_votes_count

pdf("percent_pro_env_graph.pdf",height = 5, width = 5)
ggplot() +geom_rect(data=only_election_months, aes(xmin =start, xmax =end,ymin=0,ymax= +Inf))+
  geom_point(data=monthly_ts,aes(x=floored_date_vector,y=share_pro_env),size=.5) +
  scale_x_date(breaks=only_election_months$start,
               labels=year(only_election_months$start))+
  theme(axis.text.x=element_text(color=c(rep("black",1),"red"),angle = 45,size=6),
        axis.text.y=element_text(size=6),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey")
  )+
  xlab("Year") +
  ylab("% of pro environmental votes") +
  coord_cartesian(ylim=c(0,1)) +
  ggtitle("Percentage of Pro Environment Votes Per Year")
dev.off()

#x axis: years, y axis: share of republicans voting pro env
only_republicans<-dplyr::summarise(dplyr::group_by(filter(final,party=="R"),floored_date_vector),n=n())
colnames(only_republicans)[2]<-"number_rep_votes"
only_republicans_proenv<-dplyr::summarise(dplyr::group_by(filter(final,party=="R",pro_env==1),floored_date_vector),n=n())
colnames(only_republicans_proenv)[2]<-"number_of_pro_env_rep_votes"
monthly_ts<-merge(monthly_ts,only_republicans,all.x=TRUE)
monthly_ts<-merge(monthly_ts,only_republicans_proenv,all.x=TRUE)
monthly_ts$number_rep_votes[is.na(monthly_ts$number_rep_votes)]<-0
monthly_ts$number_of_pro_env_rep_votes[is.na(monthly_ts$number_of_pro_env_rep_votes)]<-0

monthly_ts$share_rep_pro_env<-monthly_ts$number_of_pro_env_rep_votes/monthly_ts$number_rep_votes

pdf("percent_pro_env_rep_graph.pdf",height = 5, width = 5)
ggplot() +geom_rect(data=only_election_months, aes(xmin =start, xmax =end,ymin=0,ymax= +Inf))+
  geom_point(data=monthly_ts,aes(x=floored_date_vector,y=share_rep_pro_env),size=.5) +
  scale_x_date(breaks=only_election_months$start,
               labels=year(only_election_months$start))+
  theme(axis.text.x=element_text(color=c(rep("black",1),"red"),angle = 45,size=6),
        axis.text.y=element_text(size=6),
        panel.background = element_rect(fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey")
  )+
  xlab("Year") +
  ylab("% of pro environmental votes among republicans") +
  coord_cartesian(ylim=c(0,1))
dev.off()


# MAKE TABLES OF DESCRIPTIVE STATISTICS -----------------------------------
final$issue.f<-as.factor(final$issue)

sink("contributions_summary.tex")
stargazer(dplyr::select(final,oilgas_contribution,oilgas_contribution_d,oilgas_contribution_r),float=FALSE,
          covariate.labels =c("Total Oil and Gas Contributions","Oil and Gas Contributions to Republicans","Oil and Gas Contributions to Democrats"))
sink()


sink("ss_all.tex")
stargazer(
  dplyr::select(final,pro_env,republican_dummy,pres_elect_dummy,distance_election_days,unopposed,
                drilling,lands,other,toxics,water,wildlife,dirty_energy,air,climate_change,clean_energy,oceans,transportation,oilgas_contribution,oilgas_contribution_d,oilgas_contribution_r),float=F,
  omit.stat=c("N"),
  omit.summary.stat = c("N","max","min"),
  covariate.labels =c("Pro Environment Voting","Republican","Presidential Election","Days Until Election","Unopposed",
                      "Drilling","Lands","Other Issue","Toxics","Water","Wildlife","Oil, Gas, and Coal","Air","Climate Change","Clean Energy","Oceans","Transportation","Total Oil and Gas Contributions","Oil and Gas Contributions to Democrats","Oil and Gas Contributions to Republicans"))#,"Issue","Representative Status"))
sink()

sink("ss_election_60.tex")
stargazer(dplyr::filter(dplyr::select(final,pro_env,republican_dummy,election_60,pres_elect_dummy,distance_election_days,unopposed,drilling,lands,other,toxics,water,wildlife,dirty_energy,air,climate_change,clean_energy,oceans,transportation,oilgas_contribution,oilgas_contribution_d,oilgas_contribution_r),election_60==1),float=F,
          omit=c("election"),
          omit.stat=c("N"),
          omit.summary.stat = c("N","max","min"),
          covariate.labels =c("Pro Environment Voting","Republican","Presidential Election","Days Until Election","Unopposed","Drilling","Lands","Other Issue","Toxics","Water","Wildlife","Oil, Gas, and Coal","Air","Climate Change","Oceans","Transportation","Total Oil and Gas Contributions","Oil and Gas Contributions to Democrats","Oil and Gas Contributions to Republicans"))
sink()

sink("ss_election_120.tex")
stargazer(dplyr::filter(dplyr::select(final,pro_env,republican_dummy,election_120,pres_elect_dummy,distance_election_days,unopposed,drilling,lands,other,toxics,water,wildlife,dirty_energy,air,climate_change,clean_energy,oceans,transportation,oilgas_contribution,oilgas_contribution_d,oilgas_contribution_r),election_120==1),float=F,
          omit=c("election"),
          omit.stat=c("N"),
          omit.summary.stat = c("N","max","min"),
          covariate.labels =c("Pro Environment Voting","Republican","Presidential Election","Days Until Election","Unopposed","Drilling","Lands","Other Issue","Toxics","Water","Wildlife","Oil, Gas, and Coal","Air","Climate Change","Oceans","Transportation","Total Oil and Gas Contributions","Oil and Gas Contributions to Democrats","Oil and Gas Contributions to Republicans"))
sink()

####COMMENTED OUT BECAUSE THIS TAKES FOREVER TO RUN.
#MAKE GAM PLOT
# gam1<-gam(pro_env~s(distance_election_days,k=14)+as.factor(m)+as.factor(year)+as.factor(issue)+as.factor(winner),data=dplyr::filter(final))
# pdf("gam_plot.pdf",height = 5, width = 5)
# plot(gam1,xlab="Days Until Election",
#      ylab="Smoothing Function Value",
#      main="Nonlinear effect of election timing \n on pro-environmental voting")
# dev.off()



# #Main results table: overall model, split sample by partisanship --------

#overall
model1<-felm(pro_env ~ election_60 + election_120| winner | 0 | url,data=final,exactDOF=TRUE)
model2<-felm(pro_env ~ election_60 + election_120| winner + year + m + issue + repstatus| 0 | url,data=final,exactDOF=TRUE)

#split by partisanship
#republicans only
model3<-felm(pro_env ~ election_60 + election_120| winner | 0 | url,data=dplyr::filter(final,republican_dummy==1),exactDOF=TRUE)
model4<-felm(pro_env ~ election_60 + election_120| winner + year + m + issue + repstatus| 0 | url,data=dplyr::filter(final,republican_dummy==1),exactDOF=TRUE)

#dems and independents only
model5<-felm(pro_env ~ election_60 + election_120| winner | 0 | url,data=dplyr::filter(final,republican_dummy==0),exactDOF=TRUE)
model6<-felm(pro_env ~ election_60 + election_120| winner + year + m + issue + repstatus| 0 | url,data=dplyr::filter(final,republican_dummy==0),exactDOF=TRUE)

#interactive by partisanship
model7<-felm(pro_env ~ election_60*republican_dummy + election_120*republican_dummy| winner | 0 | url,data=final,exactDOF=TRUE)
model8<-felm(pro_env ~ election_60*republican_dummy + election_120*republican_dummy| winner + year + m + issue + repstatus| 0 | url,data=final,exactDOF=TRUE)

model9<-felm(pro_env ~ election_60+election_120|winner+year+m+issue+repstatus|0|url,data=dplyr::filter(final,republican_dummy==1),exactDOF = TRUE)
model10<-felm(pro_env ~ election_60+election_120|winner+year+m+issue+repstatus|0|url,data=dplyr::filter(final,republican_dummy==0),exactDOF = TRUE)

#split sample models
ss_model1<-felm(pro_env ~ election_60*margin_of_victory| winner + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy==1),exactDOF=TRUE)
ss_model2<-felm(pro_env ~ election_60*margin_of_victory| winner + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy!=1),exactDOF=TRUE)
ss_model3<-felm(pro_env ~ election_60*unopposed| winner + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy==1),exactDOF=TRUE)
ss_model4<-felm(pro_env ~ election_60*unopposed| winner + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy!=1),exactDOF=TRUE)

#split sample with interaction
sink("split_sample.tex")
stargazer(title="Interaction of Margin of Victory/Unopposed and Election Timing for Republicans and Democrats",
          header=F,
          list(ss_model1,ss_model2,ss_model3,ss_model4),
          omit.stat=c("ser"),
          no.space=T,
          add.lines=list(c("Party", "R","D","R","D")),
          font.size = "scriptsize",
          suppress.errors = F,float=F,
          omit.table.layout = "n",
          covariate.labels = c("Election (60 Days)","Margin of Victory","Election (60 Days) * Margin of Victory","Unopposed","Election (60 Days) * Unopposed"),
          dep.var.labels =c("Pro Environment Voting")
)
sink()


#main results
sink("main_results.tex")
stargazer(title = "Election Timing's Effect on Pro Environment Voting",
          header = F,
          list(model1,model2,model3,model4,model5,model6,model7,model8),
          omit="^republican_dummy$",
          omit.stat=c("ser"),
          no.space = TRUE,
          add.lines = list(
            c("Full Fixed Effects?", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes", "No", "Yes"),
            c("Sample","Full", "Full", "R", "R", "D", "D", "Full", "Full")),
          font.size="scriptsize",
          suppress.errors = F,float = F,
          omit.table.layout = "n",
          covariate.labels = c("Election (60 Days)","Election (120 Days)","Election (60 Days) * Republican","Election (120 Days) * Republican"),
          dep.var.labels =c("Pro Environment Voting")
)
sink()


#margin of victory triple interaction/oil
margin1<-felm(pro_env ~ election_60*margin_of_victory*republican_dummy |winner+year+m+issue+repstatus | 0 | url,data=final,exactDOF=TRUE)
oil1<-felm(pro_env ~ election_60*margin_of_victory*republican_dummy |winner+year+m+issue+repstatus | 0 | url,data=final,exactDOF=TRUE)

#summary stats for main results.
main_results_ss<-data.frame(num_legislators=length(unique(final$winner)),
                            num_rep_leg=length(unique(final$winner[final$republican_dummy==1])),
                            num_dem_leg=length(unique(final$winner[final$republican_dummy!=1])),
                            num_roll_call_votes=length(unique(final$url)))

sink("main_results_summary.tex")
stargazer(main_results_ss,float=FALSE,
          covariate.labels =c("Number of Legislators","Number of Republicans","Number of Democrats", "Number of Unique Roll-Call Bills"),
          omit.summary.stat = c("N","max","min")
)
sink()


#make oil and gas tables
oilgas1<-felm(pro_env~election_60*oilgas_contribution_by_party|winner|0|url,data=dplyr::filter(final,republican_dummy==1),exactDOF = TRUE)
oilgas2<-felm(pro_env~election_60*oilgas_contribution_by_party|winner|0|url,data=dplyr::filter(final,republican_dummy!=1),exactDOF = TRUE)
oilgas3<-felm(pro_env~election_60*oilgas_contribution_by_party|winner + year + issue + m |0|url,data=dplyr::filter(final,republican_dummy==1),exactDOF = TRUE)
oilgas4<-felm(pro_env~election_60*oilgas_contribution_by_party|winner + year + issue + m |0|url,data=dplyr::filter(final,republican_dummy!=1),exactDOF = TRUE)

sink("oil_gas_models.tex")
stargazer(title="Interaction of Oil and Gas Contributions and Election Timing for Republicans and Democrats",
          header=F,
          list(oilgas1,oilgas2,oilgas3,oilgas4),
          omit.stat=c("ser"),
          no.space=TRUE,
          add.lines=list(
            c("Party", "R","D","R","D"),
            c("Full Fixed Effects?", "No", "No", "Yes", "Yes")),
          font.size = "scriptsize",
          suppress.errors = F,float=F,
          omit.table.layout = "n",
          covariate.labels = c("Election (60 Days)", "Oil and Gas Contribution","Election (60 Days) * Oil and Gas Contribution"),
          dep.var.labels =c("Pro Environment Voting")
)
sink()


#summary stats for modifiers by party
dems_only<-dplyr::filter(final,republican_dummy==0) %>% 
  dplyr::select(unopposed,margin_of_victory,oilgas_contribution_d)
colnames(dems_only)<-c("unopposed_d","mov_d","oilgas_d")

reps_only<-dplyr::filter(final,republican_dummy==1) %>% 
  dplyr::select(unopposed,margin_of_victory,oilgas_contribution_r)
colnames(reps_only)<-c("unopposed_r","mov_r","oilgas_r")


sink("modifiers_summary_d.tex")
stargazer(dems_only,covariate.labels =c("Unopposed (D)","Margin of Victory (D)","Oil and Gas Contributions to Democrats"),float=FALSE) 
sink()

sink("modifiers_summary_r.tex")
stargazer(reps_only,float=FALSE,covariate.labels =c("Unopposed (R)","Margin of Victory (R)","Oil and Gas Contributions to Republicans")) 
sink()


#make table showing results with bill controls
bill_model1 <- felm(pro_env ~ election_60 + election_120 + number_abstentions| winner + pro_env_vote_y_or_n + amendment + year + m + issue | 0 | url,data=final,exactDOF=TRUE)
bill_model2 <- felm(pro_env ~ election_60*republican_dummy + election_120 + number_abstentions| winner + pro_env_vote_y_or_n + amendment + year + m + issue | 0 | url,data=final,exactDOF=TRUE)
bill_model3 <- felm(pro_env ~ election_60*margin_of_victory + election_120 + number_abstentions| winner + pro_env_vote_y_or_n + amendment + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy==1),exactDOF=TRUE)
bill_model4  <- felm(pro_env ~ election_60*margin_of_victory + election_120 + number_abstentions| winner + pro_env_vote_y_or_n + amendment + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy==0),exactDOF=TRUE)
bill_model5<- felm(pro_env ~ election_60*oilgas_contribution_by_party + election_120 + number_abstentions| winner + pro_env_vote_y_or_n + amendment + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy==1),exactDOF=TRUE)
bill_model6<- felm(pro_env ~ election_60*oilgas_contribution_by_party + election_120 + number_abstentions| winner + pro_env_vote_y_or_n + amendment + year + m + issue | 0 | url,data=dplyr::filter(final,republican_dummy==0),exactDOF=TRUE)


sink("bill_model_check.tex")
stargazer(title="Models with Bill-specific Variables",
          header=F,
          list(bill_model1,bill_model2,bill_model3,bill_model4,bill_model5,bill_model6),
          omit.stat=c("ser"),
          no.space=TRUE,
          add.lines=list(c("Party", "Both","Both","R","D","R","D")),
          #c("Full Fixed Effects?", "No", "No", "Yes", "Yes")),
          font.size = "scriptsize",
          suppress.errors = F,float=F,
          omit.table.layout = "n",
          covariate.labels = c("Election (60 Days)", 
                               "Republican",
                               "Margin of Victory",
                               "Oil and Gas Contribution",
                               "Election (120 Days)",
                               "Number of Abstentions",
                               "Election (60 Days) * Republican",
                               "Election (60 Days) * Margin of Victory",
                               "Election (60 days) * Oil and Gas Contribution"),
          dep.var.labels =c("Pro Environment Voting")
)
sink()


#make dummy variable for bills only related to lands/forest,water,clean energy, air, oceans,
final$water <- as.numeric(grepl(pattern = "water",x = final$issue,ignore.case = TRUE))
final$energy <- as.numeric(grepl(pattern = "energy",x = final$issue,ignore.case = TRUE)) 
final$lands <- as.numeric(grepl(pattern = "lands",x = final$issue,ignore.case = TRUE)) 
final$energy <- as.numeric(grepl(pattern = "energy",x = final$issue,ignore.case = TRUE)) 
final$wildlife <- as.numeric(grepl(pattern = "wildlife",x = final$issue,ignore.case = TRUE)) 
final$clean_energy <- as.numeric(grepl(pattern = "clean",x = final$issue,ignore.case = TRUE)) 
final$dirty_energy <- as.numeric(grepl(pattern = "dirty",x = final$issue,ignore.case = TRUE)) 
final$other <- as.numeric(grepl(pattern = "other",x = final$issue,ignore.case = TRUE)) 
final$transportation <- as.numeric(grepl(pattern = "transportation",x = final$issue,ignore.case = TRUE))
final$ocean <- as.numeric(grepl(pattern = "ocean",x = final$issue,ignore.case = TRUE))
final$toxic <- as.numeric(grepl(pattern = "toxic",x = final$issue,ignore.case = TRUE))
final$air <- as.numeric(grepl(pattern = "air",x = final$issue,ignore.case = TRUE))
final$drilling <- as.numeric(grepl(pattern = "drilling",x = final$issue,ignore.case = TRUE))
final$climatechange <- as.numeric(grepl(pattern = "climate",x = final$issue,ignore.case = TRUE))

final$number_issues <- rowSums(dplyr::select(final,water,lands,wildlife,clean_energy,dirty_energy,other,transportation,ocean,toxic,air,drilling,climatechange))

issue_water1 <- (felm(pro_env ~ election_60 + election_120| winner + year+m| 0 | url,data=filter(final,water==1),exactDOF=TRUE))
issue_water2 <- (felm(pro_env ~ election_60*republican_dummy + election_120*republican_dummy| winner + year +m| 0 | url,data=filter(final,water==1),exactDOF=TRUE))
issue_energy1 <- (felm(pro_env ~ election_60 + election_120| winner + year+m| 0 | url,data=filter(final,energy==1),exactDOF=TRUE))
issue_energy2 <- (felm(pro_env ~ election_60*republican_dummy + election_120*republican_dummy| winner + year +m| 0 | url,data=filter(final,energy==1),exactDOF=TRUE))
issue_lands1 <- (felm(pro_env ~ election_60 + election_120| winner + year+m| 0 | url,data=filter(final,lands==1),exactDOF=TRUE))
issue_lands2 <- (felm(pro_env ~ election_60*republican_dummy + election_120*republican_dummy| winner + year +m| 0 | url,data=filter(final,lands==1),exactDOF=TRUE))
issue_wildlife1 <- (felm(pro_env ~ election_60 + election_120| winner + year+m| 0 | url,data=filter(final,wildlife==1),exactDOF=TRUE))
issue_wildlife2 <- (felm(pro_env ~ election_60*republican_dummy + election_120*republican_dummy| winner + year+m | 0 | url,data=filter(final,wildlife==1),exactDOF=TRUE))

#MAKE TABLE
sink("issue_check.tex")
stargazer(title="Models for Different Issue Tags",
          header=F,
          list(issue_water1,issue_water2,issue_energy1,issue_energy2,issue_lands1,issue_lands2,issue_wildlife1,issue_wildlife2),
          omit.stat=c("ser"),
          no.space=TRUE,
          add.lines=list(c("Issue", "Water","Water","Energy","Energy","Lands","Lands","Wildlife","Wildlife")),
          #c("Full Fixed Effects?", "No", "No", "Yes", "Yes")),
          font.size = "scriptsize",
          suppress.errors = F,float=F,
          omit.table.layout = "n",
          covariate.labels = c("Election (60 Days)",
                               "Republican",
                               "Election (120 Days)",
                               "Election (60 Days) * Republican",
                               "Election (120 Days) * Republican"),
          dep.var.labels =c("Pro Environment Voting")
)
sink()



#ROBUSTNESS CHECK: MORE ISSUE TAGS
issue_number1 <- ((felm(pro_env ~ election_60 + election_120+number_issues| winner + year| 0 | url,final,exactDOF=TRUE)))
issue_number2 <- ((felm(pro_env ~ election_60 + election_120| winner + year+number_issues| 0 | url,final,exactDOF=TRUE)))


sink("bill_model_check.tex")
stargazer(title="Models with Bill-specific Variables",
          header=F,
          list(bill_model1,bill_model2,bill_model3,bill_model4,bill_model5,bill_model6,issue_number1),
          omit.stat=c("ser"),
          no.space=TRUE,
          add.lines=list(c("Party", "Both","Both","R","D","R","D","Both","Both")),
          #c("Full Fixed Effects?", "No", "No", "Yes", "Yes")),
          font.size = "scriptsize",
          suppress.errors = F,float=F,
          omit.table.layout = "n",
          covariate.labels = c("Election (60 Days)", 
                               "Republican",
                               "Margin of Victory",
                               "Oil and Gas Contribution",
                               "Election (120 Days)",
                               "Number of Abstentions",
                               "Election (60 Days) * Republican",
                               "Election (60 Days) * Margin of Victory",
                               "Election (60 days) * Oil and Gas Contribution",
                               "Number of Issue Tags"),
          dep.var.labels =c("Pro Environment Voting")
)
sink()



# Marginal Effects Plots --------------------------------------------------
#SET SEQUENCES
max_sequence<-13
sequence_increment<-0.5

rep_only_ss_data<-dplyr::filter(final,republican_dummy==1) %>%
  dplyr::select(republican_dummy,pro_env,election_60,winner,oilgas_contribution_by_party,url) %>%na.omit()

demo_only_ss_data<-dplyr::filter(final,republican_dummy!=1) %>%
  dplyr::select(republican_dummy,pro_env,election_60,winner,oilgas_contribution_by_party,url) %>%na.omit()


rep_only_ss_data$winner<-as.factor(rep_only_ss_data$winner)
r1<-lm(pro_env ~ election_60*oilgas_contribution_by_party+as.factor(winner),data=rep_only_ss_data)
r2<-lm(pro_env ~ election_60*oilgas_contribution_by_party+as.factor(winner),data=demo_only_ss_data)
get_CL_vcov<-function(model, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  
  #calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate the uj's
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
  
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc*sandwich(model, meat=crossprod(uj)/N)
  return(vcovCL)
}

ses<-vector()
myseq<-seq(0,max_sequence,by=sequence_increment)
myvcov<-cluster.vcov(r1,rep_only_ss_data$url,df_correction = TRUE)

myfun_rep<-function(x){
  y<-coef(r1)[2]+coef(r1)[length(coef(r1))]*x
  return(y)
}

myfun_demo<-function(x){
  y<-coef(r2)[2]+coef(r2)[length(coef(r2))]*x
  return(y)
}

#gets predicted values
output_rep<-vector()
output_rep<-myfun_rep(seq(0,max_sequence,by=sequence_increment))
output_demo<-vector()
output_demo<-myfun_demo(seq(0,max_sequence,by=sequence_increment))

#NOW MUST GET STANDARD ERRORS
#REPUBLICANS
Znew<-seq(0,max_sequence,by=sequence_increment)
## Grab elements of coefficient and vcov matrix
M<-r1
S <- summary(M)
H <- head(S$coefficients, 3)
T <- tail(S$coefficients, 1)
b <- rbind(H, T)

Vcov <- myvcov
Vcov <- as.data.frame(Vcov)
Vcov1 <- Vcov[, c(1:3)]
Vcov2 <- Vcov[, -c(0:0 - length(Vcov))]
Vcov <- cbind(Vcov1, Vcov2)
Vh <- head(Vcov, 3)
Vt <- tail(Vcov, 1)
V <- rbind(Vh, Vt)
b1 <- b[2, 1]
b3 <- b[4, 1]
varb1 <- V[2, 2]
varb3 <- V[4, 4]
covb1b3 <- V[4, 2]

## Calculate ME values
conb <- b1 + b3 * Znew

## Calculate standard errors
conse <- sqrt(varb1 + varb3 * (Znew^2) + 2 * covb1b3 * Znew)

#DEMOCRATS
myvcov_demo<-cluster.vcov(r2,demo_only_ss_data$url,df_correction = TRUE)
S_demo<-summary(r2)
H_demo<-head(S_demo$coefficients,3)
T_demo<-tail(S_demo$coefficients,1)
b_demo<-rbind(H_demo,T_demo)

myvcov_demo<-as.data.frame(myvcov_demo)
myvcov_demo1<-myvcov_demo[,c(1:3)]
myvcov_demo2<-myvcov_demo[,-c(0:0 - length(myvcov_demo))]
myvcov_demo<-cbind(myvcov_demo1,myvcov_demo2)
Vh_demo<-head(myvcov_demo,3)
Vt_demo<- tail(myvcov_demo,1)
V_demo<- rbind(Vh_demo,Vt_demo)

b1_demo <- b_demo[2, 1]
b3_demo <- b_demo[4, 1]
varb1_demo <- V_demo[2, 2]
varb3_demo <- V_demo[4, 4]
covb1b3_demo <- V_demo[4, 2]

conse_demo <- sqrt(varb1_demo + varb3_demo * (Znew^2) + 2 * covb1b3_demo * Znew)

mov_rep<-as.data.frame(summarise(group_by(rep_only_ss_data,oilgas_contribution_by_party)))
mov_rep$oilgas_contribution_by_party<-round(mov_rep$oilgas_contribution_by_party,digits=3)
mov_dem<-as.data.frame(summarise(group_by(demo_only_ss_data,oilgas_contribution_by_party)))
mov_dem$oilgas_contribution_by_party<-round(mov_dem$oilgas_contribution_by_party,digits=3)

se_plot_df_rep<-data.frame(output=output_rep,se=conse,myseq=myseq)
se_plot_df_demo<-data.frame(output=output_demo,se=conse_demo,myseq=myseq)


#MAKE PLOT
g_republican<-ggplot(data=se_plot_df_rep)+
  geom_line(aes(y=output,x=myseq),color="black",show.legend=FALSE,size=1)+
  geom_ribbon(aes(ymin=output-se*1.96,ymax=output+se*1.96,x=myseq),fill="grey",alpha=0.5,show.legend=FALSE)+
  geom_rug(aes(oilgas_contribution_by_party),data=mov_rep,alpha=0.05,color="red")+
  ylim(-.25,.25)+
  xlab("Oil & Gas Contributions (Log)") + ylab("Effect on Pro-Env Voting")+labs(title="Republicans")+
  geom_hline(yintercept=0)
g_republican


g_democrats<-ggplot(data=se_plot_df_demo)+
  geom_line(aes(y=output,x=myseq),color="black",show.legend=FALSE,size=1)+
  geom_ribbon(aes(ymin=output-se*1.96,ymax=output+se*1.96,x=myseq),fill="grey",alpha=0.5,show.legend=FALSE)+
  geom_rug(aes(oilgas_contribution_by_party),data=mov_dem,alpha=0.05,color="blue")+
  xlab("Oil & Gas Contributions (Log)") + ylab("Effect on Pro-Env Voting")+ggtitle("Democrats")+
  geom_hline(yintercept=0)+
  ylim(-.25,.25)
g_democrats

pdf("ss_plot_rep_oilgas.pdf",height = 5, width = 5)
g_republican
dev.off()
pdf("ss_plot_dem_oilgas.pdf",height = 5, width = 5)
g_democrats
dev.off()

