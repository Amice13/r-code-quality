# Load Necessary Packages
library(readstata13)
library(tidyverse)
library(cregg)
library(lme4)
library(psych)
library(sjPlot)
library(ggpubr)

# Load Data
setwd("~/Dropbox/Projects/What is Woke")
data <- read.dta13("Woke Data.dta")

# Code Anti-Minority Prejudice
data$prejudice <- (data$prejudicelazy_2+data$prejudicelazy_3+data$prejudicelazy_4)/3 - data$prejudicelazy_1
data$prejudice_dich <- ifelse(data$prejudice<0,0,1) # Median Split

# Code Conjoint Factors
data$cj_party <- factor(data$cjparty2)
data$cj_race <- factor(data$cjrace2)
data$cj_sexorient <- factor(data$cjsexorient2)
data$cj_gender <- factor(data$cjgender2)
data$cj_religion <- factor(data$cjreligion2)
data$cj_product <- factor(data$cjcommercialproducts2)
data$cj_politician <- factor(data$cjpoliticalfigures2)
data$cj_history <- factor(data$cjhistoricalevents2)
data$cj_careers <- factor(data$cjprofessions2)
data$cj_universities <- factor(data$cjcollege2)
data$cj_movements <- factor(data$cjgrouporg2)
data$cj_policies <- factor(data$cjpoliticalpolicy2)
data$rid <- factor(data$rid)

# Code Conjoint Attrbutes
## Party
data$party_lib <- ifelse(data$cj_party=="Democrat",1,0)
data$party_con <- ifelse(data$cj_party=="Republican",1,0)
data$party_raceprog <- ifelse(data$cj_party=="Democrat",1,0)
data$party_racecon <- ifelse(data$cj_party=="Republican",1,0)
data$party_genderprog <- ifelse(data$cj_party=="Democrat",1,0)
data$party_gendercon <- ifelse(data$cj_party=="Republican",1,0)
## Race
data$race_lib <- ifelse(data$cj_race=="Blacks" | data$cj_race=="Asians",1,0)
data$race_con <- ifelse(data$cj_race=="Whites",1,0)
data$race_raceprog <- ifelse(data$cj_race=="Blacks" | data$cj_race=="Hispanics" | data$cj_race=="Native Americans" | data$cj_race=="Asians",1,0)
data$race_racecon <- ifelse(data$cj_race=="Whites",1,0)
data$race_genderprog <- 0
data$race_gendercon <- ifelse(data$cj_race=="Blacks" | data$cj_race=="Hispanics",1,0)
## Religion
data$religion_lib <- ifelse(data$cj_religion=="Atheists" | data$cj_religion=="Muslims" | data$cj_religion=="Jews" | data$cj_religion=="Agnostics",1,0)
data$religion_con <- ifelse(data$cj_religion=="Christians",1,0)
data$religion_raceprog <- ifelse(data$cj_religion=="Jews",1,0)
data$religion_racecon <- 0
data$religion_genderprog <- ifelse(data$cj_religion=="Atheists" | data$cj_religion=="Agnostics",1,0)
data$religion_gendercon <- ifelse(data$cj_religion=="Muslims" | data$cj_religion=="Christians",1,0)
## Sex Orientation
data$sexorientation_lib <- ifelse(data$cj_sexorient=="Straight",0,1)
data$sexorientation_con <- 0
data$sexorientation_raceprog <- ifelse(data$cj_sexorient=="Straight",0,1)
data$sexorientation_racecon <- 0
data$sexorientation_genderprog <- ifelse(data$cj_sexorient=="Straight",0,1)
data$sexorientation_gendercon <- ifelse(data$cj_sexorient=="Straight",1,0)
## Gender
data$gender_lib <- ifelse(data$cj_gender=="Transgender folks",1,0)
data$gender_con <- ifelse(data$cj_gender=="Cis Men",1,0)
data$gender_raceprog <- 0
data$gender_racecon <- 0
data$gender_genderprog <- ifelse(data$cj_gender=="Transgender",1,0)
data$gender_gendercon <- 0
## Products
data$product_lib <- ifelse(data$cj_product=="Target" | data$cj_product=="Barbie" | data$cj_product=="NBA" | data$cj_product=="CNN" | data$cj_product=="Starbucks" | data$cj_product=="Craft Beer" | data$cj_product=="Barbie" | data$cj_product=="Bud Light",1,0)
data$product_con <- ifelse(data$cj_product=="Fox News" | data$cj_product=="Walmart" | data$cj_product=="Exxon Mobile",1,0)
data$product_raceprog <- ifelse(data$cj_product=="Target" | data$cj_product=="CNN" | data$cj_product=="NBA" | data$cj_product=="NFL",1,0)
data$product_racecon <- ifelse(data$cj_product=="Fox News",1,0)
data$product_genderprog <- ifelse(data$cj_product=="Target" | data$cj_product=="Barbie" | data$cj_product=="Bud Light",1,0)
data$product_gendercon <- 0
## Politicians
data$politician_lib <- ifelse(data$cj_politician=="Biden" | data$cj_politician=="AOC" | data$cj_politician=="Pelosi" | data$cj_politician=="Schumer",1,0)
data$politician_con <- ifelse(data$cj_politician=="Biden" | data$cj_politician=="AOC" | data$cj_politician=="Pelosi" | data$cj_politician=="Schumer",0,1)
data$politician_raceprog <- ifelse(data$cj_politician=="Biden" | data$cj_politician=="AOC" | data$cj_politician=="Pelosi" | data$cj_politician=="Schumer",1,0)
data$politician_racecon <- ifelse(data$cj_politician=="Biden" | data$cj_politician=="AOC" | data$cj_politician=="Pelosi" | data$cj_politician=="Schumer",0,1)
data$politician_genderprog <- ifelse(data$cj_politician=="Biden" | data$cj_politician=="AOC" | data$cj_politician=="Pelosi" | data$cj_politician=="Schumer",1,0)
data$politician_gendercon <- ifelse(data$cj_politician=="Biden" | data$cj_politician=="AOC" | data$cj_politician=="Pelosi" | data$cj_politician=="Schumer",0,1)
## History
data$history_lib <- ifelse(data$cj_history=="Civil Rights Movement",1,0)
data$history_con <- ifelse(data$cj_history=="September 11th",1,0)
data$history_raceprog <- ifelse(data$cj_history=="Civil Rights Movement",1,0)
data$history_racecon <- ifelse(data$cj_history=="Slavery" | data$cj_history=="January 6th",1,0)
data$history_genderprog <- 0
data$history_gendercon <- 0
## Careers
data$career_lib <- ifelse(data$cj_careers=="High School Teacher" | data$cj_careers=="Therapist",1,0)
data$career_con <- ifelse(data$cj_careers=="Trucker" | data$cj_careers=="Financial Analyst" | data$cj_careers=="Plumber",1,0)
data$career_raceprog <- ifelse(data$cj_careers=="Therapist",1,0)
data$career_racecon <- 0
data$career_genderprog <- ifelse(data$cj_careers=="Therapist",1,0)
data$career_gendercon <- ifelse(data$cj_careers=="Trucker",1,0)
## Universities
data$university_lib <- ifelse(data$cj_universities=="Yale" | data$cj_universities=="Texas-Austin" | data$cj_universities=="Harvard" | data$cj_universities=="Cal Berkeley",1,0)
data$university_con <- ifelse(data$cj_universities=="TCU" | data$cj_universities=="BYU" | data$cj_universities=="Notre Dame",1,0)
data$university_raceprog <- ifelse(data$cj_universities=="Yale" | data$cj_universities=="Harvard" | data$cj_universities=="Cal Berkeley",1,0)
data$university_racecon <- ifelse(data$cj_universities=="BYU" | data$cj_universities=="TCU" | data$cj_universities=="SMU",1,0)
data$university_genderprog <- ifelse(data$cj_universities=="Cal Berkeley" | data$cj_universities=="Yale" | data$cj_universities=="Texas-Austin" | data$cj_universities=="Harvard",1,0)
data$university_gendercon <- ifelse(data$cj_universities=="TCU" | data$cj_universities=="BYU" | data$cj_universities=="Notre Dame",1,0)
## Movements
data$movement_lib <- ifelse(data$cj_movements=="Planned Parenthood" | data$cj_movements=="Black Lives Matter" | data$cj_movements=="Antifa" | data$cj_movements=="Sierra Club" | data$cj_movements=="Moms Demand Action",1,0)
data$movement_con <- ifelse(data$cj_movements=="Proud Boys" | data$cj_movements=="NRA" | data$cj_movements=="Neo Nazis" | data$cj_movements=="Moms for Liberty",1,0)
data$movement_raceprog <- ifelse(data$cj_movements=="Antifa" | data$cj_movements=="Black Lives Matter",1,0)
data$movement_racecon <- ifelse(data$cj_movements=="Neo Nazis" | data$cj_movements=="KKK" | data$cj_movements=="NRA",1,0)
data$movement_genderprog <- ifelse(data$cj_movements=="Antifa" | data$cj_movements=="Planned Parenthood" | data$cj_movements=="Black Lives Matter",1,0)
data$movement_gendercon <- ifelse(data$cj_movements=="Neo Nazis" | data$cj_movements=="Proud Boys" | data$cj_movements=="KKK" | data$cj_movements=="Moms for Liberty",1,0)
## Policies
data$policy_lib <- ifelse(data$cj_policies=="Raising the minimum wage" | data$cj_policies=="Universal healthcare" | data$cj_policies=="Admitting more immigrants" | data$cj_policies=="Pro-Choice" | data$cj_policies=="Affirmative action" | data$cj_policies=="Aid for Ukraine",1,0)
data$policy_con <- ifelse(data$cj_policies=="Book bans" | data$cj_policies=="Admitting fewer immigrants" | data$cj_policies=="Pro-Life",1,0)
data$policy_raceprog <- ifelse(data$cj_policies=="Admitting more immigrants" | data$cj_policies=="Universal healthcare" | data$cj_policies=="Raising the minimum wage" | data$cj_policies=="Affirmative action",1,0)
data$policy_racecon <- ifelse(data$cj_policies=="Book bans" | data$cj_policies=="Admitting fewer immigrants",1,0)
data$policy_genderprog <- ifelse(data$cj_policies=="Pro-Choice" | data$cj_policies=="Universal healthcare" | data$cj_policies=="Affirmative action",1,0)
data$policy_gendercon <- ifelse(data$cj_policies=="Book bans" | data$cj_policies=="Pro-Life",1,0)
## Total
data$total_lib <- data$party_lib+data$race_lib+data$sexorientation_lib+data$gender_lib+data$religion_lib+data$product_lib+data$politician_lib+data$history_lib+data$career_lib+data$university_lib+data$movement_lib+data$policy_lib
data$total_con <- data$party_con+data$race_con+data$sexorientation_con+data$gender_con+data$religion_con+data$product_con+data$politician_con+data$history_con+data$career_con+data$university_con+data$movement_con+data$policy_con
data$total_raceprog <- data$party_raceprog+data$race_raceprog+data$sexorientation_raceprog+data$gender_raceprog+data$religion_raceprog+data$product_raceprog+data$politician_raceprog+data$history_raceprog+data$career_raceprog+data$university_raceprog+data$movement_raceprog+data$policy_raceprog
data$total_racecon <- data$party_racecon+data$race_racecon+data$sexorientation_racecon+data$gender_racecon+data$religion_racecon+data$product_racecon+data$politician_racecon+data$history_racecon+data$career_racecon+data$university_racecon+data$movement_racecon+data$policy_racecon
data$total_genderprog <- data$party_genderprog+data$race_genderprog+data$sexorientation_genderprog+data$gender_genderprog+data$religion_genderprog+data$product_genderprog+data$politician_genderprog+data$history_genderprog+data$career_genderprog+data$university_genderprog+data$movement_genderprog+data$policy_genderprog
data$total_gendercon <- data$party_gendercon+data$race_gendercon+data$sexorientation_gendercon+data$gender_gendercon+data$religion_gendercon+data$product_gendercon+data$politician_gendercon+data$history_gendercon+data$career_gendercon+data$university_gendercon+data$movement_gendercon+data$policy_gendercon
data$total_libcon <- (data$total_lib-data$total_con+7)/18
data$total_racepol <- (data$total_raceprog-data$total_racecon+7)/16
data$total_genderpol <- (data$total_genderprog-data$total_gendercon+8)/17

data$total_lib_std <- data$total_lib/11
data$total_con_std <- data$total_con/9
data$total_raceprog_std <- data$total_raceprog/9
data$total_racecon_std <- data$total_racecon/7
data$total_genderprog_std <- data$total_genderprog/9
data$total_gendercon_std <- data$total_gendercon/8


# ANOVA, Basic Conjoint
cj.anova <- aov(cjchoice~cj_party+cj_race+cj_sexorient+cj_gender+cj_religion+cj_product+cj_politician+cj_history+cj_careers+cj_universities+cj_movements+cj_policies+rid,data=data)
### Results
## Party: F(2,10046)=11.974, p<.001
## Race: F(4,10046)=2.114, p=.076
## Sexual Orientation: F(5,10046)=3.852, p=.002
## Gender: F(2,10046)=7.784, p<.001
## Religion: F(6,10046)=1.307, p=.250
## Product: F(18,10046)=1.126, p=.318
## Politician: F(7,10046)=10.569, p<.001
## Historical Event: F(7,10046)=1.609, p=.128
## Career: F(11,10046)=0.286, p=.989
## University: F(8,10046)=1.363, p=.207
## Movement: F(9,10046)=7.953, p<.001
## Policy: F(9,10046)=4.375, p<.001

# Basic Conjoint
formula <- cjchoice~cj_party+cj_race+cj_sexorient+cj_gender+cj_religion+cj_product+cj_politician+cj_history+cj_careers+cj_universities+cj_movements+cj_policies
data$party <- factor(ifelse(data$pid==1,"D",ifelse(data$pid==3,"R",NA)),levels=c("D","R"))
data$party3 <- as.factor(data$pid)

mm<-cj(data, formula, id= ~ CaseID, estimate="mm", h0 = 0.5)
mm.pid<-cj(data, formula, id= ~ CaseID, estimate="mm", h0 = 0.5,by=~party)
mm.pid1<-cj(data, formula, id= ~ CaseID, estimate="mm", h0 = 0.5,by=~party3)
mm.pid.diff <- mm_diffs(data, formula, id= ~ CaseID,  by=~party)
cor.test(mm.pid$estimate[1:100],mm.pid$estimate[101:200]) # r=.312, p=.002
subset(mm.pid.diff,p<.05) # Only 11/100 comparisons significantly different
## Republicans: Reps think they are more woke (B=.038, SE=.016, p=.018)
## Fox News: Reps think they are more woke (B=.089, SE=.045, p=.048)
## Exxon Mobil: Reps think they are less woke (B=-.105, Se=.045, p=.019)
## Slavery: Reps think it is more woke (B=.061, Se=.029, p=.036)
## Notre Dame: Reps think it is less woke (B=-.068, SE=.029, p=.022)
## Proud Boys: Reps think they are more woke (B=.070, SE=.033, p=.033)
## KKK: Reps think they are more woke (B=.104, SE=.032, p=.001)
## NRA: Reps think they are less woke (B=-.076, Se=.032, p=.017)
## BLM: Reps think they are less woke (B=-.082, SE=.034, p=.016)
## Book Bans: Reps think they are more woke (B=.063, SE=.032, p=.049)
## Aid to Israel: Reps think it is less woke (B=-.065, SE=.033, p=.048)
mm$label <- factor(ifelse(mm$feature=="cj_party","Party",ifelse(mm$feature=="cj_race","Race",ifelse(mm$feature=="cj_sexorient","Sexuality",ifelse(mm$feature=="cj_gender","Gender",ifelse(mm$feature=="cj_religion","Religion",ifelse(mm$feature=="cj_product","Product",ifelse(mm$feature=="cj_politician","Politician",ifelse(mm$feature=="cj_history","Event Discussion",ifelse(mm$feature=="cj_careers","Career",ifelse(mm$feature=="cj_universities","University",ifelse(mm$feature=="cj_movements","Movement","Policy"))))))))))),levels=c("Party","Race","Sexuality","Gender","Religion","Product","Politician","Event Discussion","Career","University","Movement","Policy"))

mmplot <- ggplot(mm,aes(x=level,y=estimate)) + geom_point(position=position_dodge(0.9)) + coord_flip() + theme_bw() + geom_errorbar(aes(ymin=lower,ymax=upper),linewidth=0.5,width=0,position=position_dodge(0.9)) + facet_wrap(facets=~label,nrow=4,ncol=3,scales="free_y") + geom_hline(yintercept=0.5,linetype="dashed",color="red") + xlab("") + ylab("Marginal Mean")
ggsave("What is Woke Plot.png",mmplot,device="png",width=10,height=8,units="in")

# Partisan Differences plot
pid.diff <- data.frame(dem=c(mm.pid$estimate[1:100]),
                       rep=c(mm.pid$estimate[101:200]))
plot.piddiff <- ggplot(pid.diff,aes(x=dem,y=rep)) + geom_point() + stat_cor(method="pearson") + geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = TRUE, size = 1) + theme_bw() + xlab("Dem Means") + ylab("Rep Means")
ggsave("Partisan Marginal Means.png",plot.piddiff,device="png",width=6,height=4,units="in")

# Marginal Means by Party
mm.pid1$label <- factor(ifelse(mm$feature=="cj_party","Party",ifelse(mm$feature=="cj_race","Race",ifelse(mm$feature=="cj_sexorient","Sexuality",ifelse(mm$feature=="cj_gender","Gender",ifelse(mm$feature=="cj_religion","Religion",ifelse(mm$feature=="cj_product","Product",ifelse(mm$feature=="cj_politician","Politician",ifelse(mm$feature=="cj_history","Event Discussion",ifelse(mm$feature=="cj_careers","Career",ifelse(mm$feature=="cj_universities","University",ifelse(mm$feature=="cj_movements","Movement","Policy"))))))))))),levels=c("Party","Race","Sexuality","Gender","Religion","Product","Politician","Event Discussion","Career","University","Movement","Policy"))
mm.pid1$pid <- factor(ifelse(mm.pid1$party3==1,"Dem",ifelse(mm.pid1$party3==2,"Ind","Rep")),levels=c("Rep","Ind","Dem"))

mmplot.byparty <- ggplot(mm.pid1,aes(x=level,y=estimate,group=pid,color=pid)) + geom_point(position=position_dodge(0.9)) + coord_flip() + theme_bw() + geom_errorbar(aes(ymin=lower,ymax=upper),linewidth=0.5,width=0,position=position_dodge(0.9)) + scale_color_manual(name="Subgroup",values=c("Red","Grey50","Blue")) + facet_wrap(facets=~label,nrow=4,ncol=3,scales="free_y") + geom_hline(yintercept=0.5,linetype="dashed",color="red") + xlab("") + ylab("Marginal Mean") + theme(legend.position="bottom")
ggsave("Marginal Means by Party.png",mmplot.byparty,device="png",width=10,height=8,units="in")

# Multilevel Model
multi1 <- lmer(cjchoice~total_lib_std+total_con_std+total_genderprog_std+total_gendercon_std+total_raceprog_std+total_racecon_std+(1|CaseID),data=data)
tab_model(multi1)
multi2 <- lmer(cjchoice~total_libcon+total_genderpol+total_racepol+(1|CaseID),data=data)
tab_model(multi2)
data$partyr <- ifelse(data$party=="R",1,0)
multi1a <- lmer(cjchoice~(total_lib_std*partyr)+(total_con_std*partyr)+(total_genderprog_std*partyr)+(total_gendercon_std*partyr)+(total_raceprog_std*partyr)+(total_racecon_std*partyr)+(1|CaseID),data=data)
multi1b <- lmer(cjchoice~total_lib_std+total_con_std+total_genderprog_std+total_gendercon_std+total_raceprog_std+total_racecon_std+(1|CaseID),data=data,subset=partyr==0)
multi1c <- lmer(cjchoice~total_lib_std+total_con_std+total_genderprog_std+total_gendercon_std+total_raceprog_std+total_racecon_std+(1|CaseID),data=data,subset=partyr==1)
multi1d <- lmer(cjchoice~total_lib_std+total_con_std+total_genderprog_std+total_gendercon_std+total_raceprog_std+total_racecon_std+(1|CaseID),data=data,subset=pid==2)


# summary(margins(multi1a,at=list(partyr=0)))
# summary(margins(multi1a,at=list(partyr=1)))
## Net Lib: Democrats - B=.269, SE=.081, p=.001; Republicans - B=.230, SE=.083, p=.006
## Net Con: Democrats - B=.133, SE=.063, p=.037; Republicans - B=-.216, Se=.067, p=.001
## Net Race Prog: Democrats - B=-.032, SE=.071, p=.654; Republicans: B=.048, SE=.073, p=.509
## Net Race Con: Democrats - B=-.225, Se=.054, p<.001; Republicans: B=.094, SE=.056, p=.093
## Net Gender Prog: Democrats - B=.075, SE=.079, p=.344; Republicans: B=.146, Se=.080, p=.069
## Net Gender Con: Democrats - B=-.166, SE=.058, p=.004; Republicans: B=.047, Se=.060, p=.438

multi2a <- lmer(cjchoice~(total_libcon*partyr)+(total_genderpol*partyr)+(total_racepol*partyr)+(1|CaseID),data=data)
multi2b <- lmer(cjchoice~total_libcon+total_genderpol+total_racepol+(1|CaseID),data=data,subset=partyr==0)
multi2c <- lmer(cjchoice~total_libcon+total_genderpol+total_racepol+(1|CaseID),data=data,subset=partyr==1)
multi2d <- lmer(cjchoice~total_libcon+total_genderpol+total_racepol+(1|CaseID),data=data,subset=pid==2)
# summary(margins(multi2a,at=list(partyr=0)))
# summary(margins(multi2a,at=list(partyr=1)))
## Net Dem: Democrats - B=.074, SE=.079, p=.344; Republicans - B=.403, SE=.082, p<.001
## Net Race Prog: Democrats - B=.240, SE=.077, p=.002; Republicans - B=-.036, SE=.081, p=.657
## Net Gender Prog: Democrats - B=.248, Se=.077, p=.001; Republicans: B=.081, SE=.080, p=.308

# Create Stargazer Tables
stargazer(multi1b,multi1c,multi1d,star.cutoffs=c(0.05,0.01,0.001),covariate.labels=c("Democratic Code","Republican Code","Gender Progressive Code","Gender Conservative Code","Racial Progressive Code","Racial Conservative Code"),dep.var.labels=c("Wokeness"))
stargazer(multi2b,multi2c,multi2d,star.cutoffs=c(0.05,0.01,0.001),covariate.labels=c("Net Democratic Code","Net Gender Progrssive Code","Net Racial Progressive Code"),dep.var.labels=c("Wokeness"))

# Plotting Multilevel model
plot.mlm1 <- data.frame(predictor=factor(c("# Dem","# Rep","# Race Prog","# Race Con","# Gender Prog","# Gender Con","# Dem","# Rep","# Race Prog","# Race Con","# Gender Prog","# Gender Con","# Dem","# Rep","# Race Prog","# Race Con","# Gender Prog","# Gender Con"),levels=c("# Gender Con","# Gender Prog","# Race Con","# Race Prog","# Rep","# Dem")),
                        Subgroup=factor(c("Ind","Ind","Ind","Ind","Ind","Ind","Dem","Dem","Dem","Dem","Dem","Dem","Rep","Rep","Rep","Rep","Rep","Rep"),levels=c("Rep","Ind","Dem")),
                        coef=c(.06754,.08641,.19295,-.04619,-.06343,-.180,.23972,.13040,.08081,-.16678,-.01611,-.22492,.23156,-.21205,.14195,.04541,.05595,.09680),
                        se=c(.11242,.08980,.10496,.08071,.09658,.07486,.08210,.06345,.07601,.05741,.06930,.05375,.08421,.06713,.07723,.05988,.07129,.05612))
plot.mlm1$lci <- plot.mlm1$coef-(1.96*plot.mlm1$se)
plot.mlm1$uci <- plot.mlm1$coef+(1.96*plot.mlm1$se) 
mlm.plot1 <- ggplot(plot.mlm1,aes(x=predictor,y=coef,group=Subgroup,color=Subgroup)) + geom_point(position=position_dodge(0.9)) + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0,position=position_dodge(0.9)) + theme_classic() + coord_flip() + geom_hline(yintercept=0,color="red",linetype="dashed") + xlab("") + ylab("Coefficient") + ylim(-0.60,0.60) + scale_color_manual(values=c("Red","Grey50","Blue")) + guides(colour = guide_legend(reverse=T)) + ggtitle("(b)")
ggsave("Coding Results 1.png",mlm.plot1,device="png",height=4,width=6,units="in")

# Plotting Model
plot.mlm2 <- data.frame(predictor=factor(c("Net Dem","Net Gender Prog","Net Race Prog","Net Dem","Net Gender Prog","Net Race Prog","Net Dem","Net Gender Prog","Net Race Prog"),levels=c("Net Race Prog","Net Gender Prog","Net Dem")),
                        Subgroup=factor(c("Ind","Ind","Ind","Dem","Dem","Dem","Rep","Rep","Rep"),levels=c("Rep","Ind","Dem")),
                        coef=c(-.03126,.20855,.16464,.05035,.25896,.25274,.40266,.08240,-.03575),
                        se=c(.11058,.10657,.10734,.07888,.07681,.07626,.08175,.07893,.08059))
plot.mlm2$lci <- plot.mlm2$coef-(1.96*plot.mlm2$se)
plot.mlm2$uci <- plot.mlm2$coef+(1.96*plot.mlm2$se)
mlm.plot2 <- ggplot(plot.mlm2,aes(x=predictor,y=coef,group=Subgroup,color=Subgroup)) + geom_point(position=position_dodge(0.9)) + geom_errorbar(aes(ymin=lci,ymax=uci),size=0.5,width=0,position=position_dodge(0.9)) + theme_classic() + coord_flip() + geom_hline(yintercept=0,color="red",linetype="dashed") + xlab("") + ylab("Coefficient") + ylim(-0.60,0.60) + scale_color_manual(values=c("Red","Grey50","Blue")) + guides(colour = guide_legend(reverse=T)) + ggtitle("(a)")
mlm.plots <- ggpubr::ggarrange(mlm.plot2,mlm.plot1,nrow=2,common.legend=T,legend="bottom")
ggsave("Coding Results 2.png",mlm.plot2,device="png",height=4,width=6,units="in")
ggsave("Coding and Partisanship.png",mlm.plots,device="png",height=6,width=8,units="in")
