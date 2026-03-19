

##########################
####VARIABLE CREATION#####
##########################

### 
###Outcomes
###

#Tongmenghui dummy
dat$tmh.dummy <- dat$tmh
dat$tmh.dummy[which(dat$tmh>0)] <- 1

#Create log standardized Tongmenghui variable
dat$tmh.ln <- scale(log(dat$tmh+1))

#Tongmenghui per capita
dat$tmh.capita <- dat$tmh/dat$pop
dat$tmh.capita.ln <- log(dat$tmh.capita+1)

#All revolutionaries  dummy
dat$nationalist.post05 <- dat$other_groups_0511_main+dat$party_school_0511+dat$tmh
dat$nationalist.post05[which(dat$nationalist.post05>0)] <- 1

#Create log standardized all nationalist group variable
dat$nationalist.post05.ln <- scale(log(dat$nationalist.post05+1))


### 
###Explanatory variables
###

#Missionary cases dummy 
dat$missionary.cases.dummy <- dat$conflict_total
dat$missionary.cases.dummy[which(dat$conflict_total>0)] <- 1

#Log missionary cases (standardized)
dat$missionary.cases.ln <- scale(log(dat$conflict_total+1))

#Log China Inland Mission missionaries (standardized)
dat$china.inland.mission.1865.ln <- scale(log(dat$china_inland_mission_1865+1))

#China Inland Mission dummy variable
dat$china.inland.mission.1865.dummy <- 0
dat$china.inland.mission.1865.dummy[which(dat$china_inland_mission_1865>0)] <- 1

#Create alternate measure for missionaries
dat$missionary_stauffer <- dat$stauffer_year_mean
dat$missionary_stauffer[which(dat$stauffer_year_mean<1905)] <- 1
dat$missionary_stauffer[which(dat$stauffer_year_mean>=1905)] <- 0


### 
###Other variables
###


#Log quota per capita (standardized)
dat$quota.ln <- scale(log(dat$quota/dat$pop80))

#Log books by category
dat$books.total.ln <- scale(log(dat$books.total+1))
dat$books.politics.ln <- scale(log(dat$books.politics+1))
dat$books.foreign.ln <- scale(log(dat$books.foreign+1))

#Log newspapers per capita (standardized)
dat$newspapers.ln <- scale(log(dat$newspapers/dat$pop+1))

#Log prefecture size (standardized)
dat$size.ln <- scale(log(dat$size))

#Log railway distance (standardized)
dat$railwaydist.ln <- scale(log(dat$railwaydist))

#Log jinshi holders per capita
dat$jinshi.cap.ln <- scale(log(dat$jinshi/dat$pop+1))

#Temples per capita
dat$temples.pop <- scale(log(dat$temples/dat$pop+1))

#Rescale population and size
dat$pop.ln <- scale(log(dat$pop))
dat$size.ln <-  scale(log(dat$size))
