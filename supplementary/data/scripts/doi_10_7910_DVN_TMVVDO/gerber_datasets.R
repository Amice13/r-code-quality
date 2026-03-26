library(readstata13)
library(stargazer)

russia<-read.dta13("2011_2012_russia_data_appended.dta")
russia3<-read.dta13("2003-2004 data.dta")
rob<-read.csv("2011_oblast.csv")

russia<-merge(russia,rob,by="id")

###Russians
russia$nRussian<-as.numeric(russia$natcat!=1) ###Not Russians
russia$nRussian[russia$natcat>11]<-NA

russia3$nRussian<-as.numeric(russia3$nation!=1) ###Not russians


###threat index
russia3$threat<-0
russia3$threat[which(russia3$fearunem=="no" &  russia3$fearpov=="no" &
                     russia3$fearecon=="no" &russia3$fearineq=="no" &
                     russia3$fearpric=="no" &russia3$fearpay=="no")]<-1
russia3$threat<-as.factor(russia3$threat)

russia$threat<-0
russia$threat[which(russia$fearunem==0 &  russia$fearpov==0 &
                       russia$fearecon==0 &russia$fearineq==0 &
                       russia$fearpric==0 &russia$fearpay==0)]<-1
russia$threat<-as.factor(russia$threat)


####income
russia$income<-2 ###trichotomize income
russia$income[russia$hinquin==1]<-1
russia$income[russia$hinquin==5]<-3
russia$income<-as.factor(russia$income)
russia$income[russia$hinquin>5]<-NA

russia3$income<-2 ###trichotomize income
russia3$income[russia3$hhinctot<quantile(russia3$hhinctot, probs = c(seq(0,1,.2)), na.rm=T)[2]]<-1
russia3$income[russia3$hhinctot>quantile(russia3$hhinctot, probs = c(seq(0,1,.2)), na.rm=T)[5]]<-3
russia3$income<-as.factor(russia3$income)
russia3$income[is.na(russia3$hhinctot)]<-NA


###not orthodox
russia$northodox<- 1- (russia$relig==2)
russia$northodox[russia$relig>8]<-NA

russia3$northodox<- 1- russia3$orthodox
russia3$northodox[russia3$relig=="not sure"]<-NA

###Putin
russia$putin<-as.numeric(russia$conpmput == 2 | russia$conpmput==3)+1
russia$putin[which(russia$conpmput == 1)]<-3
russia$putin[which(russia$conpmput == 4)]<-1
russia$putin[russia$conpmput>4]<-NA
russia$putin<-as.factor(russia$putin)

russia3$putin<-as.numeric(russia3$conpres=="rather not confident" | russia3$conpres=="rather confident") +1
russia3$putin[which(russia3$conpres == "completely confident")]<-3
russia3$putin[which(russia3$conpres == "absolutely not confident")]<-1
russia3$putin[russia3$conpres=="not sure"]<-NA
russia3$putin<-as.factor(as.numeric(russia3$putin))


####education
russia$education<-2 ###trichotomize education
russia$education[russia$educ5 ==1 ] <- 1
russia$education[russia$educ5 == 5] <- 3
russia$education<-as.factor(russia$education)


russia3$education<-2 ###trichotomize education
russia3$education[as.numeric(russia3$educcat) ==1 ] <- 1
russia3$education[as.numeric(russia3$educcat) > 5] <- 3
russia3$education<-as.factor(russia3$education)

###sex
russia$male<-2-as.numeric(russia$sex)
russia3$male<-2-as.numeric(russia3$sex)


####Age
russia$age_cat <- 2 ###trichotomize age
russia$age_cat[russia$age <22 ] <- 1
russia$age_cat[russia$age > 60] <- 3
russia$age_cat<-as.factor(russia$age_cat)

russia3$age_cat <- 2 ###trichotomize age
russia3$age_cat[russia3$age <22 ] <- 1
russia3$age_cat[russia3$age > 60] <- 3
russia3$age_cat<-as.factor(russia3$age_cat)


###Locality
russia$rural<-0 ###transform localities
russia$rural[russia$urban5 == 5] <- 1
russia$rural<-as.factor(russia$rural)

russia3$rural<-0 ###transform localities
russia3$rural[russia3$urban5 == 5] <- 1
russia3$rural<-as.factor(russia3$rural)

#####Moscow/StP

russia$stp<-as.numeric(russia$oblaststring=="spb city")
russia$moscow<-as.numeric(russia$oblaststring=="moscow      city")

russia3$stp<-as.numeric(russia3$oblast=="spb city")
russia3$moscow<-as.numeric(russia3$oblast=="moscow      city")


####fear
russia$feeljew[russia$feeljew > 5]<-NA
russia$feeljew<-as.factor(russia$feeljew)

russia$feelgyps[russia$feelgyps> 5]<-NA
russia$feelgyps<-as.factor(russia$feelgyps)

russia$feelamer[russia$feelamer > 5]<-NA
russia$feelamer<-as.factor(russia$feelamer)

russia$feelchec[russia$feelchec > 5]<-NA
russia$feelchec<-as.factor(russia$feelchec)

russia$feelswed[russia$feelswed > 5]<-NA
russia$feelswed<-as.factor(russia$feelswed)


russia$feelazer[russia$feelazer > 5]<-NA
russia$feelazer<-as.factor(russia$feelazer)


russia$feelmusl[russia$feelmusl> 5]<-NA
russia$feelmusl<-as.numeric(russia$feelmusl)


russia3$feeljew[russia3$feeljew == "not sure"]<-NA
russia3$feeljew<-as.factor(as.numeric(russia3$feeljew))

russia3$feelgyps[russia3$feelgyps=="not sure"]<-NA
russia3$feelgyps<-as.factor(as.numeric(russia3$feelgyps))

russia3$feelamer[russia3$feelamer=="not sure"]<-NA
russia3$feelamer<-as.factor(as.numeric(russia3$feelamer))

russia3$feelchec[russia3$feelchec=="not sure"]<-NA
russia3$feelchec<-as.factor(as.numeric(russia3$feelchec))

russia3$feelswed[russia3$feelswed=="not sure"]<-NA
russia3$feelswed<-as.factor(as.numeric(russia3$feelswed))

russia3$feelazer[russia3$feelazer=="not sure"]<-NA
russia3$feelazer<-as.factor(as.numeric(russia3$feelazer))

russia3$feelmusl[russia3$feelmusl=="not sure"]<-NA
russia3$feelmusl<-as.factor(as.numeric(russia3$feelmusl))

####unemployed
russia$unemployed<-0
russia$unemployed[which(russia$actnwrk==6 | russia$actnwrk==7)]<-1
russia$unemployed[which(russia$actnwrk==9 | russia$actnwrk==10)]<-NA

russia3$unemployed<-russia3$unemp

###survey weights
russia$weight<-russia$weight2
russia3$weight<-russia3$natwgt

######
r12<- data.frame(
  russia$nRussian, russia$threat, russia$income,  
  russia$northodox, russia$putin, russia$education, russia$male, 
  russia$age_cat, russia$rural, russia$feeljew, russia$feelgyps, 
  russia$feelamer,russia$feelchec, russia$feelswed, russia$feelazer,
  russia$feelmusl,russia$unemployed, russia$oblaststring,russia$moscow,russia$stp, russia$weight)

colnames(r12)<-unlist(strsplit(colnames(r12), split="russia."))[seq(2,2*ncol(r12),2)]
colnames(r12)[18]<-"oblast"

r12$survey<-"2012"


r4<- data.frame(
  russia3$nRussian, russia3$threat, russia3$income,  
  russia3$northodox, russia3$putin, russia3$education, russia3$male, 
  russia3$age_cat, russia3$rural, russia3$feeljew, russia3$feelgyps, 
  russia3$feelamer,russia3$feelchec, russia3$feelswed, russia3$feelazer, 
  russia3$feelmusl,
  russia3$unemployed, russia3$oblast, russia3$moscow,russia3$stp,russia3$weight)

colnames(r4)<-unlist(strsplit(colnames(r4), split="russia3."))[seq(2,2*ncol(r4),2)]

r4$survey<-"2004"

rh<-rbind(r4,r12)
rh$oblast<-factor(rh$oblast)
save(rh, file="gerber_ds.RData")