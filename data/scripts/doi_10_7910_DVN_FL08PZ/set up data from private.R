rm(list = ls())
library(foreign)
setwd("V:/hauser/users/bmjones3/Latest Analysis")

###get catalist data
cat <- read.dta("newCatalist.dta")
cat$id <- paste(cat$idpriv, ifelse(cat$rtype == 1, "G", "S"),
	sep = "")

###get wls data
raw <- read.dta("wls_priv_13_01.dta")

####voting
rec <- c(1,1,1,0,1)
names(rec) <- c("A","E","M","NA","Y")
cat$cat08 <- rec[cat$e2008g_1]
cat$cat10 <- rec[cat$e2010g_1]
cat$cat12 <- rec[cat$e2012g_1]
cat$state <- cat$state_1
cat2 <- cat[,c("id","cat08","cat10","cat12","state")]

###set up data subset
data <- data.frame(id = paste(c(raw$idpriv, raw$idpriv),
	c(rep("G", nrow(raw)), rep("S", nrow(raw))), sep=""),
	stringsAsFactors = FALSE)

###contributing (grads)
load("V:/hauser/users/bmjones3/Campaign Finance/cf_grad_idpriv_29jan2015.rdata")
cf <- cbind(1, cf_grad_idpriv_29jan2015$amount, 
	cf_grad_idpriv_29jan2015$contributor_cfscore,
	give08 = cf_grad_idpriv_29jan2015$cycle == "2008",
	give10 = cf_grad_idpriv_29jan2015$cycle == "2010",
	give12 = cf_grad_idpriv_29jan2015$cycle == "2012")
ag <- aggregate(cf ~ cf_grad_idpriv_29jan2015$idpriv, FUN = sum)
colnames(ag) <- c("idpriv", "n", "amount", "cfideo",
	"give08", "give10", "give12")
ag[,4] <- ag[,4]/ag[,2]
ag$id <- paste(ag$idpriv, "G", sep="")

amount <- ag$amount


#####merge in campaign finance data (sibs)
load("V:/hauser/users/bmjones3/Campaign Finance/cf_ssib_idpriv_29jan2015.rdata")
cf <- cbind(1, cf_ssib_idpriv_29jan2015$amount, 
	cf_ssib_idpriv_29jan2015$contributor_cfscore,
	cf_ssib_idpriv_29jan2015$cycle == "2008",
	cf_ssib_idpriv_29jan2015$cycle == "2010",
	cf_ssib_idpriv_29jan2015$cycle == "2012")
ag2 <- aggregate(cf ~ cf_ssib_idpriv_29jan2015$idpriv, FUN = sum)
colnames(ag2) <- c("idpriv", "n", "amount", "cfideo",
	"give08", "give10", "give12")
ag2[,4] <- ag2[,4]/ag2[,2]
ag2$id <- paste(ag2$idpriv, "S", sep="")

ag <- rbind(ag, ag2)
ag$idpriv <- NULL


amount <- c(amount, ag2$amount)
w <- which(amount > 0 & amount < 1000)
hist(amount[w], xlab = "Contribution Amount (for contributions < $1000)",
	main = '')


w <- grep("federal:", cf_grad_idpriv_29jan2015$seat)
amt <- cf_grad_idpriv_29jan2015$amount[w]
w <- grep("federal:", cf_ssib_idpriv_29jan2015$seat)
amt <- c(amt, cf_ssib_idpriv_29jan2015$amount[w])
w <- which(amt > 0 & amt < 1000)
hist(amt[w], xlab = "Contribution Amount (for contributions < $1000)",
	main = '')

######death
data$death_year <- c(raw$deatyr, raw$xdeatyr)
####persuading
c08 <- c(raw$jzz04rer, raw$pzz04rer)
rec <- c(NA, NA, NA, NA, NA, NA, 1, 0)
data$campaign08 <- rec[c08]

###self reported voting
s08 <- c(raw$jz101rer, raw$pz101rer)
rec <- c(rep(NA,6),0,0,0,1,1)
data$vote08 <- rec[s08]

####cognition
mem1 <- c(raw$gi413re, raw$ci413re)
mem1[which(mem1 < 0)] <- NA
mem2 <- c(raw$gi414re, raw$ci414re)
mem2[which(mem2 < 0)] <- NA
mem2 <- -mem2

mem3 <- c(raw$gi457re, raw$ci457re)
mem3[which(mem3 < 0)] <- NA
mem4 <- c(raw$gi458re, raw$ci458re)
mem4[which(mem4 < 0)] <- NA
mem4 <- -mem4

mem5 <- c(raw$gi503re, raw$ci503re)
mem5[which(mem5 < 0)] <- NA

abs <- array(NA, c(nrow(data), 9))
rec <- c(NA, NA, NA, NA, 0, 1, 2)
for (j in 1:9) {
	abs[,j] <- rec[c(raw[[paste("gi11", j, "re", sep="")]],
		raw[[paste("ci11", j, "re", sep="")]])]
}

let <- c(raw$gi210rec, raw$ci210rec)
let[which(let < 0)] <- NA
cat <- c(raw$gi310rec, raw$ci310rec)
cat[which(cat < 0)] <- NA

cog <- cbind(mem1,mem3,mem5,abs,let,cat)
cog <- scale(cog)
cog2 <- scale(rowMeans(cog, na.rm=TRUE))

data$cog04_unscaled <- as.numeric(cog2)

mem1 <- c(raw$hi413re, raw$ki413re)
mem1[which(mem1 < 0)] <- NA
mem2 <- c(raw$hi414re, raw$ki414re)
mem2[which(mem2 < 0)] <- NA
mem2 <- -mem2

mem3 <- c(raw$hi457re, raw$ki457re)
mem3[which(mem3 < 0)] <- NA
mem4 <- c(raw$hi458re, raw$ki458re)
mem4[which(mem4 < 0)] <- NA
mem4 <- -mem4

mem5 <- c(raw$hi503re, raw$ki503re)
mem5[which(mem5 < 0)] <- NA

it <- c(1, 3, 5,6,8,9)
abs <- array(NA, c(nrow(data), length(it)))
rec <- c(NA, NA, NA, NA, 0, 1, 2)
for (j in 1:length(it)) {
	abs[,j] <- rec[c(raw[[paste("hi11", it[j], "re", sep="")]],
		raw[[paste("ki11", it[j], "re", sep="")]])]
}

let <- c(raw$hi210rec, raw$ki210rec)
let[which(let < 0)] <- NA
cat <- c(raw$hi310rec, raw$ki310rec)
cat[which(cat < 0)] <- NA

cog <- cbind(mem1,mem3,mem5,abs,let,cat)
cog <- scale(cog)
cog2 <- scale(rowMeans(cog, na.rm=TRUE))

data$cog10_unscaled <- as.numeric(cog2)

###merge in scaled versions of cognition
cog <- read.dta("cog_scaled04_10.dta")
cog2 <- data.frame(id = paste(c(cog$id, cog$id),
	c(rep("G",nrow(cog)), rep("S",nrow(cog))), sep=""),
	cog04 = c(cog$cog04, cog$cog04_sib),
	cog10 = c(cog$cog10, cog$cog10_sib), stringsAsFactors = FALSE)

###
w1 <- c(raw$hx472re, raw$kx472re)
w2 <- c(raw$hx473re, raw$kx473re)
wh <- which(w1 < 0 & w2 < 0)

w1[which(w1 < 0)] <- NA
w2[which(w2 < 0)] <- NA
data$walk_speed2 <- rowMeans(cbind(w1, w2), na.rm=TRUE)

data$walk_speed_mis <- 0
data$walk_speed_mis[wh] <- 1


####IQ
iq <- c(raw$gwiiq_bm, raw$swiiq_t)
wh <- which(iq < 0)
iq[wh] <- NA
data$iq <- iq

####hui
data$hui04 <- c(raw$gx202are, raw$cx202are)
data$hui10 <- c(raw$hx202are, raw$kx202are)
data$hui04[which(data$hui04 == -2)] <- NA
data$hui10[which(data$hui10 == -2)] <- NA

#####grit variables
##non response on first wave
qs <- c("plns58q", "plnslkq", "plcertq", "tchcntq",
	"tchinfq", "parcntq", "parinfq", "marplnq",
	"milplnq", "plnsedq", "edwiscq")
nonresp <- c(rep(0, nrow(raw)), rep(NA, nrow(raw)))
for (j in qs) {
	wh <- which(raw[[j]] == "not ascertained")
	nonresp[wh] <- nonresp[wh] + 1
}

qs <- c("algebraq", "geomtryq", "trigq", "biologyq",
	"chmstryq", "physicsq", "englishq", "historyq",
	"socstdyq", "fornlngq", "colprepq", "edcostq",
	"parsup", "plcollq", "plclmnq", "plhied",
	"edboroq", "appliedq", "nameritq", "cebexamq",
	"nccledq", "tchencq", "parencq", "frndplq",
	"hsntrstq")
nonresp2 <- c(rep(0, nrow(raw)), rep(NA, nrow(raw)))
for (j in qs) {
	wh <- which(raw[[j]] == "not ascertained")
	nonresp2[wh] <- nonresp[wh] + 1
}

data$nonresp <- nonresp + nonresp2


##grit scale in 1992
data$grit <- c(raw$mn062rec, raw$np062rec)
wh <- which(data$grit < 0)
data$grit[wh] <- NA

##hs rank
data$hsrank <- c(raw$hsrankq, rep(NA, nrow(raw)))
wh <- which(data$hsrank < 0)
data$hsrank[wh] <- NA
data$hscode <- c(raw$hscode, rep(NA, nrow(raw)))
data$hssize <- c(raw$hssize, rep(NA, nrow(raw)))

summary(lm <- lm(hsrank ~ iq, data = data))
resid <- data$hsrank - (coef(lm)[1] + coef(lm)[2]*data$iq)

data$rank_resid <- resid

###party and ideology
party04 <- c(as.character(raw$iz102rer), 
	as.character(raw$dz102rer))
ideo04 <- c(as.character(raw$iz103rer), 
	as.character(raw$dz103rer))
party10 <- c(as.character(raw$jz102rer), 
	as.character(raw$pz102rer))
ideo10 <- c(as.character(raw$jz103rer), 
	as.character(raw$pz103rer))

rec <- c(-2, NA, -1, 1, 0, 0, NA, 2)
names(rec) <- names(table(party04))

data$party04 <- rec[party04]
nm <- names(rec)
nm[3] <- "independent but lean democrat"
nm[4] <- "independent but lean republican"
names(rec) <- nm
data$party10<- rec[party10]

rec <- c(2, NA, 3, -3, NA, -2, 0, NA, NA, NA, 1, -1)
names(rec) <- names(table(ideo10))

data$ideo10 <- rec[ideo10]
data$ideo04 <- rec[ideo04]
wh <- which(ideo04 == "moderate/middle of the road")
data$ideo04[wh] <- 0

data$ideo <- rowMeans(cbind(data$ideo04, data$ideo10), na.rm=TRUE)
data$party <- rowMeans(cbind(data$party04, data$party10), na.rm=TRUE)

####age
data$age <- c(2015 - (1900+raw$brdxdy), 2015 - 
	(1900+raw$xbrdxdy))
wh <- which(c(raw$brdxdy, raw$xbrdxdy) < 0)
data$age[wh] <- NA
data$zAge <- as.numeric(scale(data$age))
data$zAge2 <- data$zAge^2

####networth deciles (04)
nw <- c(raw$gr100rpci1, raw$cr100rpci1)
cut <- quantile(nw, 1:9/10, na.rm=TRUE)
data$networth04_10 <- as.numeric(cut(nw, c(0,cut,10^10)))
data$networth04 <- nw
data$income75 <- c(raw$yfam74, raw$xyfam76)

####education 04
ed <- c(raw$gb103red, raw$cb103red)
rec <- c(rep(NA,4), 0:21)
data$educ04 <- rec[ed]

####parent's education/income
data$educDad <- c(raw$edfa57q, raw$edfa57q)
vals <- c(NA,NA,7,10,12,13,14,16,18)
data$educDad <- vals[as.numeric(data$educDad)]
data$educDad2 <- c(raw$bmfaedu, raw$bmfaedu)

data$educMom <- c(raw$edmo57q, raw$edmo57q)
data$educMom <- vals[as.numeric(data$educMom)]
data$educMom2 <- c(raw$bmmaedu, raw$bmmaedu)

wh <- which(data$educDad2 < 0)
data$educDad2[wh] <- NA
wh <- which(data$educMom2 < 0)
data$educMom2[wh] <- NA
data$parentsInc57 <- c(raw$pi5760, raw$pi5760)
wh <- which(data$parentsInc57 < 0)
data$parentsInc57[wh] <- NA
dec <- quantile(data$parentsInc57, c((0:10)/10), na.rm=TRUE)
data$parentsInc10 <- cut(data$parentsInc57, dec)
data$parentsInc10 <- as.numeric(data$parentsInc10)

####sex
sex <- c(as.numeric(raw$sexrsp)==1, ifelse(as.numeric(raw$ssbsex) < 3,
	NA, ifelse(as.numeric(raw$ssbsex) == 3, 1, 0)))
data$male <- sex

data$clus <- as.numeric(factor(gsub("(G|S)", "", data$id)))


####get personality variables

data$e92phone <- c(raw$rh001rec,raw$sh001rec)
data$o92phone <- c(raw$rh003rec,raw$sh003rec)
data$n92phone <- c(raw$rh005rec,raw$sh005rec)
data$c92phone <- c(raw$rh007rec,raw$sh007rec)
data$a92phone <- c(raw$rh009rec,raw$sh009rec)

data$e92 <- c(raw$mh001rei,raw$nh001rei)
data$o92 <- c(raw$mh032rei,raw$nh032rei)
data$n92 <- c(raw$mh025rei,raw$nh025rei)
data$c92 <- c(raw$mh017rei,raw$nh017rei)
data$a92 <- c(raw$mh009rei,raw$nh009rei)

data$in92 <- ifelse(is.na(data$e92)|is.na(data$e92phone), 0, 1)

data$e04 <- c(raw$ih001rei,raw$dh001rei)
data$o04 <- c(raw$ih032rei,raw$dh032rei)
data$n04 <- c(raw$ih025rei,raw$dh025rei)
data$c04 <- c(raw$ih017rei,raw$dh017rei)
data$a04 <- c(raw$ih009rei,raw$dh009rei)

data$in04 <- ifelse(is.na(data$e04), 0, 1)

data$e10 <- c(raw$jh001rei,raw$ph001rei)
data$o10 <- c(raw$jh032rei,raw$ph032rei)
data$n10 <- c(raw$jh025rei,raw$ph025rei)
data$c10 <- c(raw$jh017rei,raw$ph017rei)
data$a10 <- c(raw$jh009rei,raw$ph009rei)

data$in10 <- ifelse(is.na(data$e10), 0, 1)

for (j in c('e','o','n','c','a')) {
for (k in c('92phone','92','04','10')) {
	nm <- paste(j,k,sep='')
	data[[nm]][which(data[[nm]]<0)] <- NA
	min <- min(data[[nm]],na.rm=TRUE)
	max <- max(data[[nm]],na.rm=TRUE)
	data[[nm]] <- (data[[nm]]-min)/(max-min)
}
}

##########Civic participation
groups <- c("church.group","church","union",
	"vets", "lodge", "business.civic",
	"pto", "community.center", "ethnic",
	"sports", "country.club", "youth",
	"professional", "political", "neighborhood",
	"charity", "hobby", "other1", "other2")
inds <- c(2:18,20,22)
group.data <- group.data10 <- list()
vals <- c(NA, NA, NA, NA, 0:4)
vals2 <- c(NA, NA, NA, NA, NA, NA, 0:4)
for (j in 1:length(groups)) {
	group.data[[groups[j]]] <- c(raw[[paste("iz0", ifelse(
		inds[j] < 10, 0, ""), inds[j], "rer", sep="")]],
		raw[[paste("dz0", ifelse(
		inds[j] < 10, 0, ""), inds[j], "rer", sep="")]])

	group.data10[[groups[j]]] <- c(raw[[paste("jz0", ifelse(
		inds[j] < 10, 0, ""), inds[j], "rer", sep="")]],
		raw[[paste("pz0", ifelse(
		inds[j] < 10, 0, ""), inds[j], "rer", sep="")]])

	data[[paste("grp_", groups[j], "04", sep="")]] <- vals[group.data[[j]]]
	data[[paste("grp_", groups[j], "10", sep="")]] <- vals2[
		group.data10[[j]]]
}

	

####merge in catalist
data <- merge(data, cat2, by = 'id', all.x = TRUE)
data <- merge(data, cog2, by = 'id', all.x = TRUE)
data <- merge(data, ag, by = "id", all.x = TRUE)

data$fl = data$state == "FL"
data$mn = data$state == "MN"
data$ca = data$state == "CA"
data$il = data$state == "IL"
data$az = data$state == "AZ"
data$tx = data$state == "TX"
data$co = data$state == "CO"
data$other_state = !is.element(data$state, 
	c("FL","MN","CA","IL","AZ","TX","CO","WI"))

wh <- which(data$walk_speed2 > 10)
data$walk_speed_raw <- data$walk_speed2
data$walk_speed2[wh] <- 10

data$walk_speed_all <- data$walk_speed2
data$walk_speed_all[which(data$walk_speed_mis == 1)] <- 0

for (j in c("08", 10, 12)) {
	nm2 <- paste("n.give", j, sep="")
	nm <- paste("give", j, sep="")
	data[[nm2]] <- ifelse(is.na(data[[nm]])|data[[nm]]==0,0, data[[nm]])
	data[[nm]] <- ifelse(is.na(data[[nm]])|data[[nm]]==0, 0, 1)

}

###bring in preliminary signature data
load("V:/hauser/users/bmjones3/id_change_request/coder_results_grad_idpriv.RData")
load("V:/hauser/users/bmjones3/id_change_request/coder_results_ssib_idpriv.RData")

coder_results_grad_idpriv$idpriv <- paste(coder_results_grad_idpriv$idpriv,
	"G", sep="")
coder_results_ssib_idpriv$idpriv <- paste(coder_results_ssib_idpriv$idpriv,
	"S", sep="")
sig <- data.frame(id = c(coder_results_grad_idpriv$idpriv, 
	coder_results_ssib_idpriv$idpriv),
	signer = c(coder_results_grad_idpriv$signer, 
	coder_results_ssib_idpriv$signer), stringsAsFactors = FALSE)

#data <- merge(data, sig, by = 'id', all.x=TRUE)

####change names of colorado for tables
data$colo <- data$co

###remove dead respondents
dv <- c("cat","give")
yr <- c("08","10","12")
y <- c(2008,2010,2012)
names(y) <- yr
for (j in dv) {
	for (k in yr) {
		nm <- paste(j, k, sep="")
		data[[nm]][which(data$death_year < y[k] &
			data$death_year > 1957)] <- NA
	}
}

####calculate acutal walking speed
data$walking_speed <- 2.5/data$walk_speed_raw
data$walking_speed[which(data$walk_speed_mis==1)] <- 0

####dummy for grads
data$grad <- 0
data$grad[grep("G", data$id)] <- 1
