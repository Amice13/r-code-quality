# =================================================
# = clear working memory, load functions and data =
# =================================================

# clear working memory

	rm(list=ls())
	
# source functions and packages

	source("1-Functions.R")
		
# load data

	load("Polls.RData")
	
# ===========================
# = legend for object names =
# ===========================

# ----------------------------------------------------------------------------------------
# variable	values     																		abbreviation
# ----------------------------------------------------------------------------------------
# target		seats/votes																		sea/vot
# party			Conservatives/Labour/Liberal Democrats/Others	con/lab/lib/oth
# model			NAI/NON/GOV/LAG/CHA/LOG/EXP/LIN								nai/non/gov/lag/cha/log/exp/lin
# time			quarter/year/overall													qua/yea/ove
# test set	full ('87-'17) / uns ('87,'92,'01,'15, '17)		ful/uns
# measure		mean absolute error/correct prediction of 		mae/cpw/maj
# 					winner/correct prediction of winner and				
# 					government status (minority/majority)
# ----------------------------------------------------------------------------------------

# =======================================
# = generate regression based forecasts =
# =======================================

# ------------------------------
# create lists for saving models
# ------------------------------

# 2. intentions (non)
	m.vot.con.non = list() 
	m.vot.lab.non = list() 
	m.vot.lib.non = list() 
	m.vot.oth.non = list() 
# 3. intentions (gov)
	m.vot.con.gov = list() 
	m.vot.lab.gov = list() 
	m.vot.lib.gov = list() 
	m.vot.oth.gov = list() 
# 4. intentions (lag)
	m.vot.con.lag = list() 
	m.vot.lab.lag = list() 
	m.vot.lib.lag = list() 
	m.vot.oth.lag = list() 
# 5. intentions (cha)
	m.vot.con.cha = list() 
	m.vot.lab.cha = list() 
	m.vot.lib.cha = list() 
	m.vot.oth.cha = list() 
# 6. intentions (log)
	m.sea.con.log = list() 
	m.sea.lab.log = list() 
	m.sea.lib.log = list() 
	m.sea.oth.log = list()
# 7. expectations (exp)
	m.sea.con.exp = list()
	m.sea.lab.exp = list()
	m.sea.lib.exp = list()
	m.sea.oth.exp = list()
# 8. intentions (lin)
	m.sea.con.lin = list()
	m.sea.lab.lin = list()
	m.sea.lib.lin = list()
	m.sea.oth.lin = list()

# -------------------------------------
# create vectors for saving predictions
# -------------------------------------

# 2. intentions (non)
	Com$vot.con.non = NA
	Com$vot.lab.non = NA
	Com$vot.lib.non = NA
	Com$vot.oth.non = NA
# 3. intentions (gov)
	Com$vot.con.gov = NA
	Com$vot.lab.gov = NA
	Com$vot.lib.gov = NA
	Com$vot.oth.gov = NA
# 4. intentions (lag)
	Com$vot.con.lag = NA
	Com$vot.lab.lag = NA
	Com$vot.lib.lag = NA
	Com$vot.oth.lag = NA
# 5. intentions (cha)
	Com$vot.con.cha = NA
	Com$vot.lab.cha = NA
	Com$vot.lib.cha = NA
	Com$vot.oth.cha = NA
# 6. intentions (log)
	Com$sea.con.log = NA
	Com$sea.lab.log = NA
	Com$sea.lib.log = NA
	Com$sea.oth.log = NA
# 7. expectations (exp)
	Com$sea.con.exp = NA
	Com$sea.lab.exp = NA
	Com$sea.lib.exp = NA
	Com$sea.oth.exp = NA	
# 8. intentions (lin)
	Com$sea.con.lin = NA
	Com$sea.lab.lin = NA
	Com$sea.lib.lin = NA
	Com$sea.oth.lin = NA

#------------------------------------
# fit models and generate predictions
#------------------------------------

# create elections variable

  elections = unique(Com$election)
	
# create starting and stopping values for loop	

	start = 12
	stop = 19

# loop over elections

	for (i in start:stop){
		#----------------
		# select fit data
		#----------------
		sel.fit = with(Com, election<=elections[i] & ava.bot & !is.na(y))
		Fit = Com[sel.fit,]
		#-----------
		# fit models
		#-----------
		# 2. intentions (non)
		m.vot.con.non[[i]] = lm(votes.con ~ int.con, data=Fit) 
		m.vot.lab.non[[i]] = lm(votes.lab ~ int.lab, data=Fit) 
		m.vot.lib.non[[i]] = lm(votes.lib ~ int.lib, data=Fit) 
		m.vot.oth.non[[i]] = lm(votes.oth ~ int.oth, data=Fit) 
		# 3. intentions (gov)
		m.vot.con.gov[[i]] = lm(votes.con ~ I(inc=="con") + int.con, data=Fit) 
		m.vot.lab.gov[[i]] = lm(votes.lab ~ I(inc=="lab") + int.lab, data=Fit) 
		m.vot.lib.gov[[i]] = lm(votes.lib ~ int.lib, data=Fit) 
		m.vot.oth.gov[[i]] = lm(votes.oth ~ int.oth, data=Fit) 
		# 4. intentions (lag)
		m.vot.con.lag[[i]] = lm(votes.con ~ votes.con.lag + int.con, data=Fit) 
		m.vot.lab.lag[[i]] = lm(votes.lab ~ votes.lab.lag + int.lab, data=Fit) 
		m.vot.lib.lag[[i]] = lm(votes.lib ~ votes.lib.lag + int.lib, data=Fit) 
		m.vot.oth.lag[[i]] = lm(votes.oth ~ votes.oth.lag + int.oth, data=Fit) 
		# 5. intentions (cha)
		m.vot.con.cha[[i]] = lm(I(votes.con - votes.con.lag) ~ I(int.con - votes.con.lag), data=Fit) 
		m.vot.lab.cha[[i]] = lm(I(votes.lab - votes.lab.lag) ~ I(int.lab - votes.lab.lag), data=Fit) 
		m.vot.lib.cha[[i]] = lm(I(votes.lib - votes.lib.lag) ~ I(int.lib - votes.lib.lag), data=Fit) 
		m.vot.oth.cha[[i]] = lm(I(votes.oth - votes.oth.lag) ~ I(int.oth - votes.oth.lag), data=Fit) 
		# 6. intentions (log)
		m.sea.con.log[[i]] = lm(log(seats.con) ~ log(seats.con.lag) + log(int.con) + log(int.lab) + split, data=Fit) 
		m.sea.lab.log[[i]] = lm(log(seats.lab) ~ log(seats.lab.lag) + log(int.con) + log(int.lab) + split, data=Fit) 
		m.sea.lib.log[[i]] = lm(log(seats.lib) ~ log(seats.lib.lag) + log(int.con) + log(int.lib) + split, data=Fit)
		constant = 1/650
		m.sea.oth.log[[i]] = lm(log(seats.oth+constant) ~ log(seats.oth.lag+constant) + log(int.con) + log(int.oth) + split, data=Fit) 
		# 7. expectations (exp)
		m.sea.con.exp[[i]] = lm(seats.con ~ seats.con.lag + exp.con + exp.lab, data=Fit)
		m.sea.lab.exp[[i]] = lm(seats.lab ~ seats.lab.lag + exp.con + exp.lab, data=Fit)
		m.sea.lib.exp[[i]] = lm(seats.lib ~ seats.lib.lag + exp.con + exp.lib, data=Fit)
		m.sea.oth.exp[[i]] = lm(seats.oth ~ seats.oth.lag + exp.con + exp.oth, data=Fit)
		# 8. intentions (lin)
		m.sea.con.lin[[i]] = lm(seats.con ~ seats.con.lag + int.con + int.lab, data=Fit)
		m.sea.lab.lin[[i]] = lm(seats.lab ~ seats.lab.lag + int.con + int.lab, data=Fit)
		m.sea.lib.lin[[i]] = lm(seats.lib ~ seats.lib.lag + int.con + int.lib, data=Fit)
		m.sea.oth.lin[[i]] = lm(seats.oth ~ seats.oth.lag + int.con + int.oth, data=Fit)
		#-----------------------
		# select evaluation data
		#-----------------------
		sel.eva = with(Com, election==elections[i+1] & ava.bot & !is.na(y))
		Eva = Com[sel.eva,]
		#----------------------
		# calculate predictions
		#----------------------
		# 2. intentions (non)
		Com$vot.con.non[sel.eva] = predict(m.vot.con.non[[i]], Eva) 
		Com$vot.lab.non[sel.eva] = predict(m.vot.lab.non[[i]], Eva) 
		Com$vot.lib.non[sel.eva] = predict(m.vot.lib.non[[i]], Eva) 
		Com$vot.oth.non[sel.eva] = predict(m.vot.oth.non[[i]], Eva) 
		# 3. intentions (gov)
		Com$vot.con.gov[sel.eva] = predict(m.vot.con.gov[[i]], Eva) 
		Com$vot.lab.gov[sel.eva] = predict(m.vot.lab.gov[[i]], Eva) 
		Com$vot.lib.gov[sel.eva] = predict(m.vot.lib.gov[[i]], Eva) 
		Com$vot.oth.gov[sel.eva] = predict(m.vot.oth.gov[[i]], Eva) 
		# 4. intentions (lag)
		Com$vot.con.lag[sel.eva] = predict(m.vot.con.lag[[i]], Eva) 
		Com$vot.lab.lag[sel.eva] = predict(m.vot.lab.lag[[i]], Eva) 
		Com$vot.lib.lag[sel.eva] = predict(m.vot.lib.lag[[i]], Eva) 
		Com$vot.oth.lag[sel.eva] = predict(m.vot.oth.lag[[i]], Eva) 
		# 5. intentions (cha)
		Com$vot.con.cha[sel.eva] = predict(m.vot.con.cha[[i]], Eva) + Eva$votes.con.lag 
		Com$vot.lab.cha[sel.eva] = predict(m.vot.lab.cha[[i]], Eva) + Eva$votes.lab.lag 
		Com$vot.lib.cha[sel.eva] = predict(m.vot.lib.cha[[i]], Eva) + Eva$votes.lib.lag 
		Com$vot.oth.cha[sel.eva] = predict(m.vot.oth.cha[[i]], Eva) + Eva$votes.oth.lag 
		# 6. intentions (log)
		Com$sea.con.log[sel.eva] = exp(predict(m.sea.con.log[[i]], Eva))
		Com$sea.lab.log[sel.eva] = exp(predict(m.sea.lab.log[[i]], Eva))
		Com$sea.lib.log[sel.eva] = exp(predict(m.sea.lib.log[[i]], Eva))
		Com$sea.oth.log[sel.eva] = exp(predict(m.sea.oth.log[[i]], Eva)) - constant
		# 7. expectations (exp)
		Com$sea.con.exp[sel.eva] = predict(m.sea.con.exp[[i]], Eva)
		Com$sea.lab.exp[sel.eva] = predict(m.sea.lab.exp[[i]], Eva)
		Com$sea.lib.exp[sel.eva] = predict(m.sea.lib.exp[[i]], Eva)
		Com$sea.oth.exp[sel.eva] = predict(m.sea.oth.exp[[i]], Eva)
		# 8. intentions (lin)
		Com$sea.con.lin[sel.eva] = predict(m.sea.con.lin[[i]], Eva)
		Com$sea.lab.lin[sel.eva] = predict(m.sea.lab.lin[[i]], Eva)
		Com$sea.lib.lin[sel.eva] = predict(m.sea.lib.lin[[i]], Eva)
		Com$sea.oth.lin[sel.eva] = predict(m.sea.oth.lin[[i]], Eva)
	}

#------------------------------------------------------------
# check that predictions are larger than 0 and smaller than 1
#------------------------------------------------------------

	vot.non = c("vot.con.non", "vot.lab.non", "vot.lib.non", "vot.oth.non")
	vot.gov = c("vot.con.gov", "vot.lab.gov", "vot.lib.gov", "vot.oth.gov")
	vot.lag = c("vot.con.lag", "vot.lab.lag", "vot.lib.lag", "vot.oth.lag")
	vot.cha = c("vot.con.cha", "vot.lab.cha", "vot.lib.cha", "vot.oth.cha")
	sea.log = c("sea.con.log", "sea.lab.log", "sea.lib.log", "sea.oth.log")
	sea.lin = c("sea.con.lin", "sea.lab.lin", "sea.lib.lin", "sea.oth.lin")
	sea.exp = c("sea.con.exp", "sea.lab.exp", "sea.lib.exp", "sea.oth.exp")

	mean.na(Com[vot.non] >= 0)
	mean.na(Com[vot.gov] >= 0)
	mean.na(Com[vot.lag] >= 0)
	mean.na(Com[vot.cha] >= 0)
	mean.na(Com[sea.log] >= 0)
	mean.na(Com[sea.lin] >= 0)
	mean.na(Com[sea.exp] >= 0)

	mean.na(Com[vot.non] <= 1)
	mean.na(Com[vot.gov] <= 1)
	mean.na(Com[vot.lag] <= 1)
	mean.na(Com[vot.cha] <= 1)
	mean.na(Com[sea.log] <= 1)
	mean.na(Com[sea.lin] <= 1)
	mean.na(Com[sea.exp] <= 1)

#--------------------------------------	
# standardise predictions to sum to one
#--------------------------------------

# 2. intentions (non)
	Com$vot.con.non.std = Com$vot.con.non / apply(Com[vot.non], 1, sum)
	Com$vot.lab.non.std = Com$vot.lab.non / apply(Com[vot.non], 1, sum)
	Com$vot.lib.non.std = Com$vot.lib.non / apply(Com[vot.non], 1, sum)
	Com$vot.oth.non.std = Com$vot.oth.non / apply(Com[vot.non], 1, sum)
# 3. intentions (gov)
	Com$vot.con.gov.std = Com$vot.con.gov / apply(Com[vot.gov], 1, sum)
	Com$vot.lab.gov.std = Com$vot.lab.gov / apply(Com[vot.gov], 1, sum)
	Com$vot.lib.gov.std = Com$vot.lib.gov / apply(Com[vot.gov], 1, sum)
	Com$vot.oth.gov.std = Com$vot.oth.gov / apply(Com[vot.gov], 1, sum)
# 4. intentions (lag)
	Com$vot.con.lag.std = Com$vot.con.lag / apply(Com[vot.lag], 1, sum)
	Com$vot.lab.lag.std = Com$vot.lab.lag / apply(Com[vot.lag], 1, sum)
	Com$vot.lib.lag.std = Com$vot.lib.lag / apply(Com[vot.lag], 1, sum)
	Com$vot.oth.lag.std = Com$vot.oth.lag / apply(Com[vot.lag], 1, sum)
# 5. intentions (cha)
	Com$vot.con.cha.std = Com$vot.con.cha / apply(Com[vot.cha], 1, sum)
	Com$vot.lab.cha.std = Com$vot.lab.cha / apply(Com[vot.cha], 1, sum)
	Com$vot.lib.cha.std = Com$vot.lib.cha / apply(Com[vot.cha], 1, sum)
	Com$vot.oth.cha.std = Com$vot.oth.cha / apply(Com[vot.cha], 1, sum)
# 6. intentions (log)
	Com$sea.con.log.std = Com$sea.con.log / apply(Com[sea.log], 1, sum)
	Com$sea.lab.log.std = Com$sea.lab.log / apply(Com[sea.log], 1, sum)
	Com$sea.lib.log.std = Com$sea.lib.log / apply(Com[sea.log], 1, sum)
	Com$sea.oth.log.std = Com$sea.oth.log / apply(Com[sea.log], 1, sum)
# 7. expectations (exp)
	Com$sea.con.exp.std = Com$sea.con.exp / apply(Com[sea.exp], 1, sum)
	Com$sea.lab.exp.std = Com$sea.lab.exp / apply(Com[sea.exp], 1, sum)
	Com$sea.lib.exp.std = Com$sea.lib.exp / apply(Com[sea.exp], 1, sum)
	Com$sea.oth.exp.std = Com$sea.oth.exp / apply(Com[sea.exp], 1, sum)
# 8. intentions (lin)
	Com$sea.con.lin.std = Com$sea.con.lin / apply(Com[sea.lin], 1, sum)
	Com$sea.lab.lin.std = Com$sea.lab.lin / apply(Com[sea.lin], 1, sum)
	Com$sea.lib.lin.std = Com$sea.lib.lin / apply(Com[sea.lin], 1, sum)
	Com$sea.oth.lin.std = Com$sea.oth.lin / apply(Com[sea.lin], 1, sum)

#----------------------
# check standardisation
#----------------------

	vot.non.std = c("vot.con.non.std", "vot.lab.non.std", "vot.lib.non.std", "vot.oth.non.std")
	vot.gov.std = c("vot.con.gov.std", "vot.lab.gov.std", "vot.lib.gov.std", "vot.oth.gov.std")
	vot.lag.std = c("vot.con.lag.std", "vot.lab.lag.std", "vot.lib.lag.std", "vot.oth.lag.std")
	vot.cha.std = c("vot.con.cha.std", "vot.lab.cha.std", "vot.lib.cha.std", "vot.oth.cha.std")
	sea.log.std = c("sea.con.log.std", "sea.lab.log.std", "sea.lib.log.std", "sea.oth.log.std")
	sea.lin.std = c("sea.con.lin.std", "sea.lab.lin.std", "sea.lib.lin.std", "sea.oth.lin.std")
	sea.exp.std = c("sea.con.exp.std", "sea.lab.exp.std", "sea.lib.exp.std", "sea.oth.exp.std")

	table(apply(Com[vot.non.std], 1, sum))
	table(apply(Com[vot.gov.std], 1, sum))
	table(apply(Com[vot.lag.std], 1, sum))
	table(apply(Com[vot.cha.std], 1, sum))
	table(apply(Com[sea.log.std], 1, sum))
	table(apply(Com[sea.lin.std], 1, sum))
	table(apply(Com[sea.exp.std], 1, sum))

#-------------------------------------
# store variable names of actual seats
#-------------------------------------

	seats = c("seats.con", "seats.lab", "seats.lib", "seats.oth")
	
# ==========================================
# = derive uniform national swing forecast =
# ==========================================	

#----------
# load data
#----------

	load("C83.RData")
	load("C87.RData")
	load("C97.RData")
	load("C10.RData")
	load("C15.RData")

#----------------
# calculate swing
#----------------

# 1. intentions (nai)
	Com$swi.con.nai = with(Com, int.con - votes.con.lag)
	Com$swi.lab.nai = with(Com, int.lab - votes.lab.lag)
	Com$swi.lib.nai = with(Com, int.lib - votes.lib.lag)
	Com$swi.oth.nai = with(Com, int.oth - votes.oth.lag)
# 2. intentions (non)
	Com$swi.con.non = with(Com, vot.con.non.std - votes.con.lag)
	Com$swi.lab.non = with(Com, vot.lab.non.std - votes.lab.lag)
	Com$swi.lib.non = with(Com, vot.lib.non.std - votes.lib.lag)
	Com$swi.oth.non = with(Com, vot.oth.non.std - votes.oth.lag)
# 3. intentions (gov)
	Com$swi.con.gov = with(Com, vot.con.gov.std - votes.con.lag)
	Com$swi.lab.gov = with(Com, vot.lab.gov.std - votes.lab.lag)
	Com$swi.lib.gov = with(Com, vot.lib.gov.std - votes.lib.lag)
	Com$swi.oth.gov = with(Com, vot.oth.gov.std - votes.oth.lag)
# 4. intentions (lag)
	Com$swi.con.lag = with(Com, vot.con.lag.std - votes.con.lag)
	Com$swi.lab.lag = with(Com, vot.lab.lag.std - votes.lab.lag)
	Com$swi.lib.lag = with(Com, vot.lib.lag.std - votes.lib.lag)
	Com$swi.oth.lag = with(Com, vot.oth.lag.std - votes.oth.lag)
# 5. intentions (cha)
	Com$swi.con.cha = with(Com, vot.con.cha.std - votes.con.lag)
	Com$swi.lab.cha = with(Com, vot.lab.cha.std - votes.lab.lag)
	Com$swi.lib.cha = with(Com, vot.lib.cha.std - votes.lib.lag)
	Com$swi.oth.cha = with(Com, vot.oth.cha.std - votes.oth.lag)

#-----------------------------------------
# create vectors for saving predictions
#-----------------------------------------

# 1. intentions (nai)
	Com$sea.con.uns.nai = NA
	Com$sea.lab.uns.nai = NA
	Com$sea.lib.uns.nai = NA
	Com$sea.oth.uns.nai = NA
# 2. intentions (non)
	Com$sea.con.uns.non = NA
	Com$sea.lab.uns.non = NA
	Com$sea.lib.uns.non = NA
	Com$sea.oth.uns.non = NA
# 3. intentions (gov)
	Com$sea.con.uns.gov = NA
	Com$sea.lab.uns.gov = NA
	Com$sea.lib.uns.gov = NA
	Com$sea.oth.uns.gov = NA
# 4. intentions (lag)
	Com$sea.con.uns.lag = NA
	Com$sea.lab.uns.lag = NA
	Com$sea.lib.uns.lag = NA
	Com$sea.oth.uns.lag = NA
# 5. intentions (cha)
	Com$sea.con.uns.cha = NA
	Com$sea.lab.uns.cha = NA
	Com$sea.lib.uns.cha = NA
	Com$sea.oth.uns.cha = NA

#---------------------
# store variable names
#---------------------

	sea.uns.nai = c("sea.con.uns.nai", "sea.lab.uns.nai", "sea.lib.uns.nai", "sea.oth.uns.nai")
	sea.uns.non = c("sea.con.uns.non", "sea.lab.uns.non", "sea.lib.uns.non", "sea.oth.uns.non")
	sea.uns.gov = c("sea.con.uns.gov", "sea.lab.uns.gov", "sea.lib.uns.gov", "sea.oth.uns.gov")
	sea.uns.lag = c("sea.con.uns.lag", "sea.lab.uns.lag", "sea.lib.uns.lag", "sea.oth.uns.lag")
	sea.uns.cha = c("sea.con.uns.cha", "sea.lab.uns.cha", "sea.lib.uns.cha", "sea.oth.uns.cha")

#----------------------------------------------------
# select elections without any major boundary changes
#----------------------------------------------------

	C = list(C83, C87, C97, C10, C15)
	lapply(C, nrow)
	e = c(1987, 1992, 2001, 2015, 2017)

#------------------------------------------------------------
# calculate constituency vote shares and generate predictions	
#------------------------------------------------------------

	for (i in 1:length(e)){
		# select election year
		Sel = Com[Com$election==e[i]&Com$ava.bot&!is.na(Com$y),]
		n = nrow(Sel)
		His = C[[i]] 
		for (j in 1:n){
			# get swing estimate
			nai = Sel[j,c("swi.con.nai", "swi.lab.nai", "swi.lib.nai", "swi.oth.nai")]
			non = Sel[j,c("swi.con.non", "swi.lab.non", "swi.lib.non", "swi.oth.non")]
			gov = Sel[j,c("swi.con.gov", "swi.lab.gov", "swi.lib.gov", "swi.oth.gov")]
			lag = Sel[j,c("swi.con.lag", "swi.lab.lag", "swi.lib.lag", "swi.oth.lag")]
			cha = Sel[j,c("swi.con.cha", "swi.lab.cha", "swi.lib.cha", "swi.oth.cha")]
			# calculate swing in constituency
			s.nai = nai[rep(seq_len(nrow(nai)), each=nrow(His)),]
			s.non = non[rep(seq_len(nrow(non)), each=nrow(His)),]
			s.gov = gov[rep(seq_len(nrow(gov)), each=nrow(His)),]
			s.lag = lag[rep(seq_len(nrow(lag)), each=nrow(His)),]
			s.cha = cha[rep(seq_len(nrow(cha)), each=nrow(His)),]
			S.nai = His + s.nai
			S.non = His + s.non
			S.gov = His + s.gov
			S.lag = His + s.lag
			S.cha = His + s.cha
			# calculate national prediction of seat shares			
			sea.nai = Naive(S.nai)
			sea.non = CurticeFirth(S.non)
			sea.gov = CurticeFirth(S.gov)
			sea.lag = CurticeFirth(S.lag)
			sea.cha = CurticeFirth(S.cha)
			Com[Com$election==e[i]&Com$ava.bot&!is.na(Com$y),][j,sea.uns.nai] = sea.nai
			Com[Com$election==e[i]&Com$ava.bot&!is.na(Com$y),][j,sea.uns.non] = sea.non
			Com[Com$election==e[i]&Com$ava.bot&!is.na(Com$y),][j,sea.uns.gov] = sea.gov
			Com[Com$election==e[i]&Com$ava.bot&!is.na(Com$y),][j,sea.uns.lag] = sea.lag
			Com[Com$election==e[i]&Com$ava.bot&!is.na(Com$y),][j,sea.uns.cha] = sea.cha
		}
	}

# ==============
# = save image =
# ==============

	save.image(file="Forecasts.RData")

# ===================
# = end source code =
# ===================