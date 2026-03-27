rm(list=ls(all=T))

library(dyn)
library(sandwich)
library(lmtest)
library(maps)
library(mapproj)
library(plyr)
library(gridBase)
library(car)
library(sfsmisc)
library(plm)
library(foreign)
library(mrpdata)
library(mrp)
library(arm)
library(ggplot2)
library(gcookbook)
library(reshape)
library(reshape2)
library(doBy)
library(corrplot)
library(ggmap)
library(tidyr)

##########################
##LOAD DATA
##########################

# Opinion data

opinion_data <- read.dta("lsq_megapoll_use.dta")

# Helper datasets for other US applications of MRP:

data(spmap.states)
data(mrp.regions) 
mrp.census.package <- data(mrp.census)

# Dataset with state level predictor (Caughey Warshaw state liberalism)

statelevel <- read.table("state_policy_idealpoints-all.csv", header=TRUE, sep=",")
statelevel <- subset(statelevel, select=c("median", "year", "abb"))
statelevel <- rename(statelevel, c(abb="state", median="state_ideology"))
statelevel$state_ideology <- rescale(statelevel$state_ideology)
statelevel.1950 <- subset(statelevel, year==1950)
statelevel.1960 <- subset(statelevel, year==1960)
rm(statelevel)

# Poststratification files

mrp.census.1950 <-read.dta("mrp_census_1950.dta")
mrp.census.1960 <-read.dta("mrp_census_1960.dta")

##########################
##PROCESS DATA
##########################

## Recode all missings in opinion data "" or "." to "miss"

for(i in c(names(opinion_data))){
  opinion_data[,i] <- recode(opinion_data[,i],"'' = NA")
}

for(i in c(names(opinion_data))){
  opinion_data[,i] <- recode(opinion_data[,i],"'.' = NA")
}

## Recode all missings in census data "" or "." to "miss"

for(i in c(names(mrp.census.1950))){
  mrp.census.1950[,i] <- recode(mrp.census.1950[,i],"'.' = NA")
}

for(i in c(names(mrp.census.1950))){
  mrp.census.1950[,i] <- recode(mrp.census.1950[,i],"'' = NA")
}

for(i in c(names(mrp.census.1960))){
  mrp.census.1960[,i] <- recode(mrp.census.1960[,i],"'.' = NA")
}

for(i in c(names(mrp.census.1960))){
  mrp.census.1960[,i] <- recode(mrp.census.1960[,i],"'' = NA")
}

##Delete missings from census data and statelevel data

mrp.census.1950 <- na.omit(mrp.census.1950)
mrp.census.1960 <- na.omit(mrp.census.1960)
statelevel.1950 <- na.omit(statelevel.1950)
statelevel.1960 <- na.omit(statelevel.1960)

##Make sure all of the variables are factors

opinion_data <-within(opinion_data,{
  education_nopg <-factor(education_nopg)
  state <- factor(state)
  age <- factor(age)
  sexrace <-factor(sexrace)
})

mrp.census.1950 <- within(mrp.census.1950,{
  age <- factor(age)
  education_nopg <-factor(education_nopg)
  state <- factor(state)
  sexrace <- factor(sexrace)
})

mrp.census.1960 <- within(mrp.census.1960,{
  age <- factor(age)
  education_nopg <-factor(education_nopg)
  state <- factor(state)
  sexrace <- factor(sexrace)
})

statelevel.1950 <-within(statelevel.1950,{
  state <- factor(state)
})

statelevel.1960 <-within(statelevel.1960,{
  state <- factor(state)
})

############################
#MRP ESTIMATES - 1950 CENSUS 
############################

pop_data <- mrp.census.1950
state_data <- statelevel.1950

#Equal pay 1954

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo01954-0530")
survey_data <- subset(survey_data, select=c("q_equalpay_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.equalpay.1954 <- mrp(question~state+sexrace+age+education_nopg,
                         data=survey_data,
                         population=pop_data, 
                         pop.weights="slwt",
                         grouplevel.data.frames=list(state_data),
                         formula.model.update= .~. + state_ideology
)

q_equalpay_1954 <- as.data.frame(cbind(100*poststratify(mrp.equalpay.1954, ~ state)))
q_equalpay_1954$state <- rownames(q_equalpay_1954)
q_equalpay_1954 <- rename(q_equalpay_1954, c(V1="q_equalpay_1954"))

#Equal pay - professions 1954

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo01954-0530")
survey_data <- subset(survey_data, select=c("q_equalpay_professions_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.equalpay.professions.1954 <- mrp(question~state+sexrace+age+education_nopg,
                         data=survey_data,
                         population=pop_data, 
                         pop.weights="slwt",
                         grouplevel.data.frames=list(state_data),
                         formula.model.update= .~. + state_ideology
)

q_equalpay_professions_1954 <- as.data.frame(cbind(100*poststratify(mrp.equalpay.professions.1954, ~ state)))
q_equalpay_professions_1954$state <- rownames(q_equalpay_professions_1954)
q_equalpay_professions_1954 <- rename(q_equalpay_professions_1954, c(V1="q_equalpay_professions_1954"))

#Equal pay - govservice 1954

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo01954-0530")
survey_data <- subset(survey_data, select=c("q_equalpay_govservice_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.equalpay.govservice.1954 <- mrp(question~state+sexrace+age+education_nopg,
                                     data=survey_data,
                                     population=pop_data, 
                                     pop.weights="slwt",
                                     grouplevel.data.frames=list(state_data),
                                     formula.model.update= .~. + state_ideology
)

q_equalpay_govservice_1954 <- as.data.frame(cbind(100*poststratify(mrp.equalpay.govservice.1954, ~ state)))
q_equalpay_govservice_1954$state <- rownames(q_equalpay_govservice_1954)
q_equalpay_govservice_1954 <- rename(q_equalpay_govservice_1954, c(V1="q_equalpay_govservice_1954"))

#Equal pay - factories 1954

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo01954-0530")
survey_data <- subset(survey_data, select=c("q_equalpay_factories_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.equalpay.factories.1954 <- mrp(question~state+sexrace+age+education_nopg,
                                     data=survey_data,
                                     population=pop_data, 
                                     pop.weights="slwt",
                                     grouplevel.data.frames=list(state_data),
                                     formula.model.update= .~. + state_ideology
)

q_equalpay_factories_1954 <- as.data.frame(cbind(100*poststratify(mrp.equalpay.factories.1954, ~ state)))
q_equalpay_factories_1954$state <- rownames(q_equalpay_factories_1954)
q_equalpay_factories_1954 <- rename(q_equalpay_factories_1954, c(V1="q_equalpay_factories_1954"))

#Draft 1954

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo1954-0531")
survey_data <- subset(survey_data, select=c("q_draft_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.draft.1954 <- mrp(question~state+sexrace+age+education_nopg,
                                   data=survey_data,
                                   population=pop_data, 
                                   pop.weights="slwt",
                                   grouplevel.data.frames=list(state_data),
                                   formula.model.update= .~. + state_ideology
)

q_draft_1954 <- as.data.frame(cbind(100*poststratify(mrp.draft.1954, ~ state)))
q_draft_1954$state <- rownames(q_draft_1954)
q_draft_1954 <- rename(q_draft_1954, c(V1="q_draft_1954"))

############################
#MRP ESTIMATES - 1960 CENSUS 
############################

pop_data <- mrp.census.1960
state_data <- statelevel.1960

#Female president 1955

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo01955-0543")
survey_data <- subset(survey_data, select=c("q_fempres_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.fempres.1955 <- mrp(question~state+sexrace+age+education_nopg,
                         data=survey_data,
                         population=pop_data, 
                         grouplevel.data.frames=list(state_data),
                         formula.model.update= .~. + state_ideology
)

q_fempres_1955 <- as.data.frame(cbind(100*poststratify(mrp.fempres.1955, ~ state)))
q_fempres_1955$state <- rownames(q_fempres_1955)
q_fempres_1955 <- rename(q_fempres_1955, c(V1="q_fempres_1955"))

#Female president 1958

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo1958-0604")
survey_data <- subset(survey_data, select=c("q_fempres_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.fempres.1958 <- mrp(question~state+sexrace+age+education_nopg,
                        data=survey_data,
                        population=pop_data, 
                        grouplevel.data.frames=list(state_data),
                        formula.model.update= .~. + state_ideology
)

q_fempres_1958 <- as.data.frame(cbind(100*poststratify(mrp.fempres.1958, ~ state)))
q_fempres_1958$state <- rownames(q_fempres_1958)
q_fempres_1958 <- rename(q_fempres_1958, c(V1="q_fempres_1958"))

#Female president 1959

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo1959-0622")
survey_data <- subset(survey_data, select=c("q_fempres_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.fempres.1959 <- mrp(question~state+sexrace+age+education_nopg,
                        data=survey_data,
                        population=pop_data, 
                        grouplevel.data.frames=list(state_data),
                        formula.model.update= .~. + state_ideology
)

q_fempres_1959 <- as.data.frame(cbind(100*poststratify(mrp.fempres.1959, ~ state)))
q_fempres_1959$state <- rownames(q_fempres_1959)
q_fempres_1959 <- rename(q_fempres_1959, c(V1="q_fempres_1959"))

#Female president 1963

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo1963-0676")
survey_data <- subset(survey_data, select=c("q_fempres_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.fempres.1963 <- mrp(question~state+sexrace+age+education_nopg,
                        data=survey_data,
                        population=pop_data, 
                        grouplevel.data.frames=list(state_data),
                        formula.model.update= .~. + state_ideology
)

q_fempres_1963 <- as.data.frame(cbind(100*poststratify(mrp.fempres.1963, ~ state)))
q_fempres_1963$state <- rownames(q_fempres_1963)
q_fempres_1963 <- rename(q_fempres_1963, c(V1="q_fempres_1963"))

#Equal pay 1962

survey_data <- subset(opinion_data, opinion_data$poll=="usaipo1962-0660")
survey_data <- subset(survey_data, select=c("q_equalpay_yes", "state", "sexrace", "age", "education_nopg"))
colnames(survey_data)[1] <- "question"

mrp.equalpay.1962 <- mrp(question~state+sexrace+age+education_nopg,
                         data=survey_data,
                         population=pop_data, 
                         grouplevel.data.frames=list(state_data),
                         formula.model.update= .~. + state_ideology
                         )

q_equalpay_1962 <- as.data.frame(cbind(100*poststratify(mrp.equalpay.1962, ~ state)))
q_equalpay_1962$state <- rownames(q_equalpay_1962)
q_equalpay_1962 <- rename(q_equalpay_1962, c(V1="q_equalpay_1962"))

#################
#COMBINE ESTIMATES
#################

estimates <- merge(q_equalpay_1954, q_equalpay_factories_1954, by="state")
estimates <- merge(estimates, q_equalpay_professions_1954, by="state")
estimates <- merge(estimates, q_equalpay_govservice_1954, by="state")
estimates <- merge(estimates, q_draft_1954, by="state")
estimates <- merge(estimates, q_fempres_1955, by="state")
estimates <- merge(estimates, q_fempres_1958, by="state")
estimates <- merge(estimates, q_fempres_1959, by="state")
estimates <- merge(estimates, q_equalpay_1962, by="state")
estimates <- merge(estimates, q_fempres_1963, by="state")

save(estimates, file = "TitleVII_opiniondata.LSQ.RDa")

#######################################
#STATE-LEVEL CORRELATION PLOTS 
#######################################

load("TitleVII_opiniondata.LSQ.RDa")

temp <-estimates
temp$state <- NULL
temp$q_equalpay_professions_1954 <- NULL

temp <- rename(temp, c("q_equalpay_1954"="EP (54)", 
                       "q_equalpay_factories_1954"="EP Factories (54)",  
                       "q_equalpay_govservice_1954"="EP Government (54)",
                       "q_draft_1954"="Draft (54)",            
                       "q_fempres_1955"="President (55)",
                       "q_fempres_1958"="President (58)",
                       "q_fempres_1959"="President (59)",
                       "q_equalpay_1962"="EP (62)",
                       "q_fempres_1963"="President (63)"))          

corr_data <- cor(temp)

pdf("corr_state_sex_bw.pdf")  
corrplot(corr_data, type="lower", method="shade", tl.col="black", shade.col=NA, addCoef.col="black", diag=FALSE,   col = gray.colors(100))
  #+ scale_fill_brewer("black", "white")
dev.off()

#######
#MAPS
#######

theme_clean <- function (base_size=12) {
  require(gridBase) 
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank (),
      axis.text = element_blank (),
      panel.background = element_blank (),
      panel.grid = element_blank (),
      axis.ticks = element_blank(),
      panel.margin = unit (0,"lines"),
      plot.margin = unit(c(0,0,0,0),"lines"),
      plot.title = element_text(siz=rel(1), face="bold"),
      complete =TRUE
    )
}

converter <- read.dta("state_converter.dta")
converter <- rename(converter, c(abbr="state"))

mydata <-join(estimates, converter, by="state")
mydata <- rename(mydata, c(state="stateabbr"))
mydata <- rename(mydata, c(lc_name="state"))
states_map <- map_data("state")
states_map <- rename(states_map, c(region="state"))
mapdata <- merge (states_map, mydata, by="state")
mapdata <- arrange(mapdata, group, order)

pdf("map_equalpay_1954_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_equalpay_1954))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Support Equal Pay (1954)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_equalpay_factories_1954_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_equalpay_factories_1954))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Support Equal Pay - Factories (1954)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_equalpay_professions_1954_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_equalpay_professions_1954))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Support Equal Pay - Professions (1954)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_equalpay_govservice_1954_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_equalpay_govservice_1954))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Support Equal Pay - Government Service (1954)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_draft_1954_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_draft_1954))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Support Drafting Women (1954)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_fempres_1955_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_fempres_1955))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Would Vote for Female President (1955)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_fempres_1958_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_fempres_1958))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Would Vote for Female President (1958)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_fempres_1959_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_fempres_1959))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Would Vote for Female President (1959)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_equalpay_1962_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_equalpay_1962))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Support Equal Pay (1962)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

pdf("map_fempres_1963_bw.pdf")  
ggplot(mapdata, aes(x=long, y=lat, group=group, fill=q_fempres_1963))+
  geom_polygon(colour="black")+
  coord_map("polyconic")+
  scale_fill_gradient(low="white", high="black") +
  expand_limits(x = states_map$long, y = states_map$lat) +
  ggtitle("Would Vote for Female President (1963)") +
  labs(fill="% State Population") +
  theme_clean()
dev.off()

####################################################
#DEFINING STATE PARTISANSHIP FOR AVG OPINION GRAPHS
####################################################

Hdata <- read.dta("HL01113D21_BSSE_12.DTA")
Hdata=Hdata[which(Hdata$cong==88),]
Hdata <- subset(Hdata, select=c("cong", "state", "party"))

converter <- read.dta("state_converter.dta")
converter <- rename(converter, c(icpsr="state"))

MCdata <-join(Hdata, converter, by="state")
MCdata <- rename(MCdata, c(state="icpsr"))
MCdata <- rename(MCdata, c(abbr="state"))

MC_totals <- melt(xtabs(~state+party,data=MCdata))
MC_totals_wide <- spread(MC_totals, party, value)

MC_totals_wide <- rename(MC_totals_wide, c("100"="D", "200"="R"))
MC_totals_wide$total <- MC_totals_wide$D+ MC_totals_wide$R
MC_totals_wide$pct_dem <- MC_totals_wide$D/MC_totals_wide$total

MC_totals_wide$Party[MC_totals_wide$pct_dem>=(2/3)] <- "Strongly Democratic"
MC_totals_wide$Party[MC_totals_wide$pct_dem<=(1/3)] <- "Strongly Republican"
MC_totals_wide$Party[MC_totals_wide$pct_dem>(1/3) & MC_totals_wide$pct_dem<(2/3)] <- "Mixed"

###########################################
#GRAPH AVERAGE OPINION BY PARTY AND REGION
###########################################

opinion_avg <- join(estimates, MC_totals_wide, by="state")
opinion_avg <- join(opinion_avg, mrp.regions, by="state")
opinion_avg$region[opinion_avg$state=="MO"] <- "South"  

#Equal Pay - 17 state south

data_equalpay_1954 <-subset(opinion_avg, select=c(q_equalpay_1954, region, Party))
equalpay_1954 <- ddply(data_equalpay_1954, c("region", "Party"), summarise, 
                       N=length(q_equalpay_1954),
                       mean=mean(q_equalpay_1954))
equalpay_1954$year <- "1954"

data_equalpay_1962 <-subset(opinion_avg, select=c(q_equalpay_1962, region, Party))
equalpay_1962 <- ddply(data_equalpay_1962, c("region", "Party"), summarise, 
                       N=length(q_equalpay_1962),
                       mean=mean(q_equalpay_1962))
equalpay_1962$year <- "1962"

equalpay <- rbind(equalpay_1954, equalpay_1962)
equalpay$region <- as.factor(equalpay$region)

pdf("opinion_equalpay_pr_bw.pdf")  
ggplot(equalpay, aes(x=mean, y=region, fill=Party, size=N)) +
  geom_point(shape=21) +
  facet_grid(year~., scales="free_y", space="free_y") +
  scale_fill_manual(values=c("Strongly Republican"="grey85", "Strongly Democratic"="grey25", "Mixed"="grey55"), limits=c("Strongly Democratic", "Mixed", "Strongly Republican"))+
  labs(x="\n % Support Equal Pay \n" , y="") +
  ggtitle("\n Average Public Support for Equal Pay, by Region and Party\n") +
  theme_bw()+
  theme(plot.title=element_text(size=14, hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=12), strip.text=element_text(size=12))
dev.off()

#Equal pay - 13 state south

opinion_avg_13 <- opinion_avg
opinion_avg_13$region[opinion_avg$state=="DE"] <- "Northeast"  
opinion_avg_13$region[opinion_avg$state=="MD"] <- "Northeast"  
opinion_avg_13$region[opinion_avg$state=="MO"] <- "Midwest"  
opinion_avg_13$region[opinion_avg$state=="WV"] <- "Midwest"  

data_equalpay_1954_13 <-subset(opinion_avg_13, select=c(q_equalpay_1954, region, Party))
equalpay_1954_13 <- ddply(data_equalpay_1954_13, c("region", "Party"), summarise, 
                          N=length(q_equalpay_1954),
                          mean=mean(q_equalpay_1954))
equalpay_1954_13$year <- "1954"

data_equalpay_1962_13 <-subset(opinion_avg_13, select=c(q_equalpay_1962, region, Party))
equalpay_1962_13 <- ddply(data_equalpay_1962_13, c("region", "Party"), summarise, 
                          N=length(q_equalpay_1962),
                          mean=mean(q_equalpay_1962))
equalpay_1962_13$year <- "1962"

equalpay_13 <- rbind(equalpay_1954_13, equalpay_1962_13)
equalpay_13$region <- as.factor(equalpay_13$region)

pdf("opinion_equalpay_pr_bw_13.pdf")  
ggplot(equalpay_13, aes(x=mean, y=region, fill=Party, size=N)) +
  geom_point(shape=21) +
  facet_grid(year~., scales="free_y", space="free_y") +
  scale_fill_manual(values=c("Strongly Republican"="grey85", "Strongly Democratic"="grey25", "Mixed"="grey55"), limits=c("Strongly Democratic", "Mixed", "Strongly Republican"))+
  labs(x="\n % Support Equal Pay \n" , y="") +
  ggtitle("\n Average Public Support for Equal Pay (13 State South)\n") +
  theme_bw()+
  theme(plot.title=element_text(size=14, hjust = 0.5), axis.text=element_text(size=12), axis.title=element_text(size=12), strip.text=element_text(size=12))

dev.off()



