# ======================================
# = clear working memory and load data =
# ======================================

# clear working memory

	rm(list=ls())
	
# source generated forecasts

	load(file="Forecasts.RData")
	
# ============================
# = calculate accuracy - ful =
# ============================

# select months of test elections where both int 
# and exp are available within 4 years of election

	Sel = Com[Com$election>elections[start] & Com$ava.bot & !is.na(Com$y),]
	
#--------------------------
# mean absolute error (mae)
#--------------------------

# calculate mae by quarter

	mae.sea.exp.qua.ful = mae.by(sea.exp.std, seats, "q")
	mae.sea.lin.qua.ful = mae.by(sea.lin.std, seats, "q")
	mae.sea.log.qua.ful = mae.by(sea.log.std, seats, "q")
	mae.sea.qua.ful = rbind(mae.sea.exp.qua.ful, mae.sea.lin.qua.ful, mae.sea.log.qua.ful)

# calculate mae by year

	mae.sea.exp.yea.ful = mae.by(sea.exp.std, seats, "y")
	mae.sea.lin.yea.ful = mae.by(sea.lin.std, seats, "y")
	mae.sea.log.yea.ful = mae.by(sea.log.std, seats, "y")
	mae.sea.yea.ful = rbind(mae.sea.exp.yea.ful, mae.sea.lin.yea.ful, mae.sea.log.yea.ful)

# calculate mae by election

	mae.sea.exp.ele.ful = mae.by(sea.exp.std, seats, "election")
	mae.sea.lin.ele.ful = mae.by(sea.lin.std, seats, "election")
	mae.sea.log.ele.ful = mae.by(sea.log.std, seats, "election")
	mae.sea.ele.ful = rbind(mae.sea.exp.ele.ful, mae.sea.lin.ele.ful, mae.sea.log.ele.ful)

# calculate mae by year and election

	mae.sea.exp.yea.ele.ful = mae.by.by(sea.exp.std, seats, "y", "election")
	mae.sea.lin.yea.ele.ful = mae.by.by(sea.lin.std, seats, "y", "election")
	mae.sea.log.yea.ele.ful = mae.by.by(sea.log.std, seats, "y", "election")
	mae.sea.yea.ele.ful = rbind(
		rbind(
			mae.sea.exp.yea.ele.ful[1,],
			mae.sea.lin.yea.ele.ful[1,],
			mae.sea.log.yea.ele.ful[1,]
		),
		rbind(
			mae.sea.exp.yea.ele.ful[2,],
			mae.sea.lin.yea.ele.ful[2,],
			mae.sea.log.yea.ele.ful[2,]
		),
		rbind(
			mae.sea.exp.yea.ele.ful[3,],
			mae.sea.lin.yea.ele.ful[3,],
			mae.sea.log.yea.ele.ful[3,]
		),
		rbind(
			mae.sea.exp.yea.ele.ful[4,],
			mae.sea.lin.yea.ele.ful[4,],
			mae.sea.log.yea.ele.ful[4,]
		)
	)

# calculate mae overall

	mae.sea.exp.ove.ful = mae.ov(sea.exp.std, seats)
	mae.sea.lin.ove.ful = mae.ov(sea.lin.std, seats)
	mae.sea.log.ove.ful = mae.ov(sea.log.std, seats)
	mae.sea.ove.ful = rbind(mae.sea.exp.ove.ful, mae.sea.lin.ove.ful, mae.sea.log.ove.ful)

#-----------------------------------
# correct prediction of winner (cpw)
#-----------------------------------

# calculate cpw by quarter

	cpw.sea.exp.qua.ful = cpw.by(sea.exp.std, seats, "q")
	cpw.sea.lin.qua.ful = cpw.by(sea.lin.std, seats, "q")
	cpw.sea.log.qua.ful = cpw.by(sea.log.std, seats, "q")
	cpw.sea.qua.ful = rbind(cpw.sea.exp.qua.ful, cpw.sea.lin.qua.ful, cpw.sea.log.qua.ful)
	
# calculate cpw by year

	cpw.sea.exp.yea.ful = cpw.by(sea.exp.std, seats, "y")
	cpw.sea.lin.yea.ful = cpw.by(sea.lin.std, seats, "y")
	cpw.sea.log.yea.ful = cpw.by(sea.log.std, seats, "y")
	cpw.sea.yea.ful = rbind(cpw.sea.exp.yea.ful, cpw.sea.lin.yea.ful, cpw.sea.log.yea.ful)

# calculate cpw by election

	cpw.sea.exp.ele.ful = cpw.by(sea.exp.std, seats, "election")
	cpw.sea.lin.ele.ful = cpw.by(sea.lin.std, seats, "election")
	cpw.sea.log.ele.ful = cpw.by(sea.log.std, seats, "election")
	cpw.sea.ele.ful = rbind(cpw.sea.exp.ele.ful, cpw.sea.lin.ele.ful, cpw.sea.log.ele.ful)

# calculate cpw by year and election

	cpw.sea.exp.yea.ele.ful = cpw.by.by(sea.exp.std, seats, "y", "election")
	cpw.sea.lin.yea.ele.ful = cpw.by.by(sea.lin.std, seats, "y", "election")
	cpw.sea.log.yea.ele.ful = cpw.by.by(sea.log.std, seats, "y", "election")
	cpw.sea.yea.ele.ful = rbind(
		rbind(
			cpw.sea.exp.yea.ele.ful[1,],
			cpw.sea.lin.yea.ele.ful[1,],
			cpw.sea.log.yea.ele.ful[1,]
		),
		rbind(
			cpw.sea.exp.yea.ele.ful[2,],
			cpw.sea.lin.yea.ele.ful[2,],
			cpw.sea.log.yea.ele.ful[2,]
		),
		rbind(
			cpw.sea.exp.yea.ele.ful[3,],
			cpw.sea.lin.yea.ele.ful[3,],
			cpw.sea.log.yea.ele.ful[3,]
		),
		rbind(
			cpw.sea.exp.yea.ele.ful[4,],
			cpw.sea.lin.yea.ele.ful[4,],
			cpw.sea.log.yea.ele.ful[4,]
		)
	)

# calculate cpw overall

	cpw.sea.exp.ove.ful = cpw.ov(sea.exp.std, seats)
	cpw.sea.lin.ove.ful = cpw.ov(sea.lin.std, seats)
	cpw.sea.log.ove.ful = cpw.ov(sea.log.std, seats)
	cpw.sea.ove.ful = rbind(cpw.sea.exp.ove.ful, cpw.sea.lin.ove.ful, cpw.sea.log.ove.ful)

#--------------------------------------------------
# correct prediction of winner and whether winner
# has majority (maj)
#--------------------------------------------------

# calculate maj by quarter

	maj.sea.exp.qua.ful = maj.by(sea.exp.std, seats, "seats.maj", "q")
	maj.sea.lin.qua.ful = maj.by(sea.lin.std, seats, "seats.maj", "q")
	maj.sea.log.qua.ful = maj.by(sea.log.std, seats, "seats.maj", "q")
	maj.sea.qua.ful = rbind(maj.sea.exp.qua.ful, maj.sea.lin.qua.ful, maj.sea.log.qua.ful)

# calculate maj by year

	maj.sea.exp.yea.ful = maj.by(sea.exp.std, seats, "seats.maj", "y")
	maj.sea.lin.yea.ful = maj.by(sea.lin.std, seats, "seats.maj", "y")
	maj.sea.log.yea.ful = maj.by(sea.log.std, seats, "seats.maj", "y")
	maj.sea.yea.ful = rbind(maj.sea.exp.yea.ful, maj.sea.lin.yea.ful, maj.sea.log.yea.ful)

# calculate maj by election

	maj.sea.exp.ele.ful = maj.by(sea.exp.std, seats, "seats.maj", "election")
	maj.sea.lin.ele.ful = maj.by(sea.lin.std, seats, "seats.maj", "election")
	maj.sea.log.ele.ful = maj.by(sea.log.std, seats, "seats.maj", "election")
	maj.sea.ele.ful = rbind(maj.sea.exp.ele.ful, maj.sea.lin.ele.ful, maj.sea.log.ele.ful)

# calculate maj by year and election

	maj.sea.exp.yea.ele.ful = maj.by.by(sea.exp.std, seats, "seats.maj", "y", "election")
	maj.sea.lin.yea.ele.ful = maj.by.by(sea.lin.std, seats, "seats.maj", "y", "election")
	maj.sea.log.yea.ele.ful = maj.by.by(sea.log.std, seats, "seats.maj", "y", "election")
	maj.sea.yea.ele.ful = rbind(
		rbind(
			maj.sea.exp.yea.ele.ful[1,],
			maj.sea.lin.yea.ele.ful[1,],
			maj.sea.log.yea.ele.ful[1,]
		),
		rbind(
			maj.sea.exp.yea.ele.ful[2,],
			maj.sea.lin.yea.ele.ful[2,],
			maj.sea.log.yea.ele.ful[2,]
		),
		rbind(
			maj.sea.exp.yea.ele.ful[3,],
			maj.sea.lin.yea.ele.ful[3,],
			maj.sea.log.yea.ele.ful[3,]
		),
		rbind(
			maj.sea.exp.yea.ele.ful[4,],
			maj.sea.lin.yea.ele.ful[4,],
			maj.sea.log.yea.ele.ful[4,]
		)
	)

# calculate maj overall

	maj.sea.exp.ove.ful = maj.ov(sea.exp.std, seats, "seats.maj")
	maj.sea.lin.ove.ful = maj.ov(sea.lin.std, seats, "seats.maj")
	maj.sea.log.ove.ful = maj.ov(sea.log.std, seats, "seats.maj")
	maj.sea.ove.ful = rbind(maj.sea.exp.ove.ful, maj.sea.lin.ove.ful, maj.sea.log.ove.ful)	

#-----------------------
# number of observations
#-----------------------

  n.ove.ful = c(nrow(Sel), tapply(Sel$election, Sel$election, length))	
	n.qua.ful = c(nrow(Sel), tapply(Sel$election, Sel$q, length))	
	n.yea.ful = c(nrow(Sel), tapply(Sel$election, Sel$y, length))	

# ============================
# = calculate accuracy - uns =
# ============================

# select elections witout boundary chances where both int and exp are available within 4 years of election

	Sel = Com[Com$election%in%e & Com$ava.bot & !is.na(Com$y),]

#--------------------------
# mean absolute error (mae)
#--------------------------

# calculate mae by quarter

	mae.sea.exp.qua.uns = mae.by(sea.exp.std, seats, "q")
	mae.sea.lin.qua.uns = mae.by(sea.lin.std, seats, "q")
	mae.sea.log.qua.uns = mae.by(sea.log.std, seats, "q")
	mae.sea.nai.qua.uns = mae.by(sea.uns.nai, seats, "q")
	mae.sea.non.qua.uns = mae.by(sea.uns.non, seats, "q")
	mae.sea.gov.qua.uns = mae.by(sea.uns.gov, seats, "q")
	mae.sea.lag.qua.uns = mae.by(sea.uns.lag, seats, "q")
	mae.sea.cha.qua.uns = mae.by(sea.uns.cha, seats, "q")
	mae.sea.qua.uns = rbind(
		mae.sea.exp.qua.uns,
		mae.sea.lin.qua.uns,
		mae.sea.log.qua.uns,
		mae.sea.nai.qua.uns,
		mae.sea.non.qua.uns,
		mae.sea.gov.qua.uns,
		mae.sea.lag.qua.uns,
		mae.sea.cha.qua.uns
	)
	
# calculate mae by year

	mae.sea.exp.yea.uns = mae.by(sea.exp.std, seats, "y")
	mae.sea.lin.yea.uns = mae.by(sea.lin.std, seats, "y")
	mae.sea.log.yea.uns = mae.by(sea.log.std, seats, "y")
	mae.sea.nai.yea.uns = mae.by(sea.uns.nai, seats, "y")
	mae.sea.non.yea.uns = mae.by(sea.uns.non, seats, "y")
	mae.sea.gov.yea.uns = mae.by(sea.uns.gov, seats, "y")
	mae.sea.lag.yea.uns = mae.by(sea.uns.lag, seats, "y")
	mae.sea.cha.yea.uns = mae.by(sea.uns.cha, seats, "y")
	mae.sea.yea.uns = rbind(
		mae.sea.exp.yea.uns,
		mae.sea.lin.yea.uns,
		mae.sea.log.yea.uns,
		mae.sea.nai.yea.uns,
		mae.sea.non.yea.uns,
		mae.sea.gov.yea.uns,
		mae.sea.lag.yea.uns,
		mae.sea.cha.yea.uns
	)

# calculate mae by election

	mae.sea.exp.ele.uns = mae.by(sea.exp.std, seats, "election")
	mae.sea.lin.ele.uns = mae.by(sea.lin.std, seats, "election")
	mae.sea.log.ele.uns = mae.by(sea.log.std, seats, "election")
	mae.sea.nai.ele.uns = mae.by(sea.uns.nai, seats, "election")
	mae.sea.non.ele.uns = mae.by(sea.uns.non, seats, "election")
	mae.sea.gov.ele.uns = mae.by(sea.uns.gov, seats, "election")
	mae.sea.lag.ele.uns = mae.by(sea.uns.lag, seats, "election")
	mae.sea.cha.ele.uns = mae.by(sea.uns.cha, seats, "election")
	mae.sea.ele.uns = rbind(
		mae.sea.exp.ele.uns,
		mae.sea.lin.ele.uns,
		mae.sea.log.ele.uns,
		mae.sea.nai.ele.uns,
		mae.sea.non.ele.uns,
		mae.sea.gov.ele.uns,
		mae.sea.lag.ele.uns,
		mae.sea.cha.ele.uns
		)
	
# calculate mae by year and election

	mae.sea.exp.yea.ele.uns = mae.by.by(sea.exp.std, seats, "y", "election")
	mae.sea.lin.yea.ele.uns = mae.by.by(sea.lin.std, seats, "y", "election")
	mae.sea.log.yea.ele.uns = mae.by.by(sea.log.std, seats, "y", "election")
	mae.sea.nai.yea.ele.uns = mae.by.by(sea.uns.nai, seats, "y", "election")
	mae.sea.non.yea.ele.uns = mae.by.by(sea.uns.non, seats, "y", "election")
	mae.sea.gov.yea.ele.uns = mae.by.by(sea.uns.gov, seats, "y", "election")
	mae.sea.lag.yea.ele.uns = mae.by.by(sea.uns.lag, seats, "y", "election")
	mae.sea.cha.yea.ele.uns = mae.by.by(sea.uns.cha, seats, "y", "election")
	mae.sea.yea.ele.uns = rbind(
		rbind(
			mae.sea.exp.yea.ele.uns[1,],
			mae.sea.lin.yea.ele.uns[1,],
			mae.sea.log.yea.ele.uns[1,],
			mae.sea.nai.yea.ele.uns[1,],
			mae.sea.non.yea.ele.uns[1,],
			mae.sea.gov.yea.ele.uns[1,],
			mae.sea.lag.yea.ele.uns[1,],
			mae.sea.cha.yea.ele.uns[1,]
		),
		rbind(
			mae.sea.exp.yea.ele.uns[2,],
			mae.sea.lin.yea.ele.uns[2,],
			mae.sea.log.yea.ele.uns[2,],
			mae.sea.nai.yea.ele.uns[2,],
			mae.sea.non.yea.ele.uns[2,],
			mae.sea.gov.yea.ele.uns[2,],
			mae.sea.lag.yea.ele.uns[2,],
			mae.sea.cha.yea.ele.uns[2,]
		),
		rbind(
			mae.sea.exp.yea.ele.uns[3,],
			mae.sea.lin.yea.ele.uns[3,],
			mae.sea.log.yea.ele.uns[3,],
			mae.sea.nai.yea.ele.uns[3,],
			mae.sea.non.yea.ele.uns[3,],
			mae.sea.gov.yea.ele.uns[3,],
			mae.sea.lag.yea.ele.uns[3,],
			mae.sea.cha.yea.ele.uns[3,]
		),
		rbind(
			mae.sea.exp.yea.ele.uns[4,],
			mae.sea.lin.yea.ele.uns[4,],
			mae.sea.log.yea.ele.uns[4,],
			mae.sea.nai.yea.ele.uns[4,],
			mae.sea.non.yea.ele.uns[4,],
			mae.sea.gov.yea.ele.uns[4,],
			mae.sea.lag.yea.ele.uns[4,],
			mae.sea.cha.yea.ele.uns[4,]
		)
	)

# calculate mae overall

	mae.sea.exp.ove.uns = mae.ov(sea.exp.std, seats)
	mae.sea.lin.ove.uns = mae.ov(sea.lin.std, seats)
	mae.sea.log.ove.uns = mae.ov(sea.log.std, seats)
	mae.sea.nai.ove.uns = mae.ov(sea.uns.nai, seats)
	mae.sea.non.ove.uns = mae.ov(sea.uns.non, seats)
	mae.sea.gov.ove.uns = mae.ov(sea.uns.gov, seats)
	mae.sea.lag.ove.uns = mae.ov(sea.uns.lag, seats)
	mae.sea.cha.ove.uns = mae.ov(sea.uns.cha, seats)
	mae.sea.ove.uns = rbind(
		mae.sea.exp.ove.uns,
		mae.sea.lin.ove.uns,
		mae.sea.log.ove.uns,
		mae.sea.nai.ove.uns,
		mae.sea.non.ove.uns,
		mae.sea.gov.ove.uns,
		mae.sea.lag.ove.uns,
		mae.sea.cha.ove.uns
		)

#-----------------------------------
# correct prediction of winner (cpw)
#-----------------------------------

# calculate cpw by quarter

	cpw.sea.exp.qua.uns = cpw.by(sea.exp.std, seats, "q")
	cpw.sea.lin.qua.uns = cpw.by(sea.lin.std, seats, "q")
	cpw.sea.log.qua.uns = cpw.by(sea.log.std, seats, "q")
	cpw.sea.nai.qua.uns = cpw.by(sea.uns.nai, seats, "q")
	cpw.sea.non.qua.uns = cpw.by(sea.uns.non, seats, "q")
	cpw.sea.gov.qua.uns = cpw.by(sea.uns.gov, seats, "q")
	cpw.sea.lag.qua.uns = cpw.by(sea.uns.lag, seats, "q")
	cpw.sea.cha.qua.uns = cpw.by(sea.uns.cha, seats, "q")
	cpw.sea.qua.uns = rbind(
		cpw.sea.exp.qua.uns,
		cpw.sea.lin.qua.uns,
		cpw.sea.log.qua.uns,
		cpw.sea.nai.qua.uns,
		cpw.sea.non.qua.uns,
		cpw.sea.gov.qua.uns,
		cpw.sea.lag.qua.uns,
		cpw.sea.cha.qua.uns
	)
	
# calculate cpw by year

	cpw.sea.exp.yea.uns = cpw.by(sea.exp.std, seats, "y")
	cpw.sea.lin.yea.uns = cpw.by(sea.lin.std, seats, "y")
	cpw.sea.log.yea.uns = cpw.by(sea.log.std, seats, "y")
	cpw.sea.nai.yea.uns = cpw.by(sea.uns.nai, seats, "y")
	cpw.sea.non.yea.uns = cpw.by(sea.uns.non, seats, "y")
	cpw.sea.gov.yea.uns = cpw.by(sea.uns.gov, seats, "y")
	cpw.sea.lag.yea.uns = cpw.by(sea.uns.lag, seats, "y")
	cpw.sea.cha.yea.uns = cpw.by(sea.uns.cha, seats, "y")
	cpw.sea.yea.uns = rbind(
		cpw.sea.exp.yea.uns,
		cpw.sea.lin.yea.uns,
		cpw.sea.log.yea.uns,
		cpw.sea.nai.yea.uns,
		cpw.sea.non.yea.uns,
		cpw.sea.gov.yea.uns,
		cpw.sea.lag.yea.uns,
		cpw.sea.cha.yea.uns
	)

# calculate cpw by election

	cpw.sea.exp.ele.uns = cpw.by(sea.exp.std, seats, "election")
	cpw.sea.lin.ele.uns = cpw.by(sea.lin.std, seats, "election")
	cpw.sea.log.ele.uns = cpw.by(sea.log.std, seats, "election")
	cpw.sea.nai.ele.uns = cpw.by(sea.uns.nai, seats, "election")
	cpw.sea.non.ele.uns = cpw.by(sea.uns.non, seats, "election")
	cpw.sea.gov.ele.uns = cpw.by(sea.uns.gov, seats, "election")
	cpw.sea.lag.ele.uns = cpw.by(sea.uns.lag, seats, "election")
	cpw.sea.cha.ele.uns = cpw.by(sea.uns.cha, seats, "election")
	cpw.sea.ele.uns = rbind(
		cpw.sea.exp.ele.uns,
		cpw.sea.lin.ele.uns,
		cpw.sea.log.ele.uns,
		cpw.sea.nai.ele.uns,
		cpw.sea.non.ele.uns,
		cpw.sea.gov.ele.uns,
		cpw.sea.lag.ele.uns,
		cpw.sea.cha.ele.uns
		)

# calculate cpw by year and election

	cpw.sea.exp.yea.ele.uns = cpw.by.by(sea.exp.std, seats, "y", "election")
	cpw.sea.lin.yea.ele.uns = cpw.by.by(sea.lin.std, seats, "y", "election")
	cpw.sea.log.yea.ele.uns = cpw.by.by(sea.log.std, seats, "y", "election")
	cpw.sea.nai.yea.ele.uns = cpw.by.by(sea.uns.nai, seats, "y", "election")
	cpw.sea.non.yea.ele.uns = cpw.by.by(sea.uns.non, seats, "y", "election")
	cpw.sea.gov.yea.ele.uns = cpw.by.by(sea.uns.gov, seats, "y", "election")
	cpw.sea.lag.yea.ele.uns = cpw.by.by(sea.uns.lag, seats, "y", "election")
	cpw.sea.cha.yea.ele.uns = cpw.by.by(sea.uns.cha, seats, "y", "election")
	cpw.sea.yea.ele.uns = rbind(
		rbind(
			cpw.sea.exp.yea.ele.uns[1,],
			cpw.sea.lin.yea.ele.uns[1,],
			cpw.sea.log.yea.ele.uns[1,],
			cpw.sea.nai.yea.ele.uns[1,],
			cpw.sea.non.yea.ele.uns[1,],
			cpw.sea.gov.yea.ele.uns[1,],
			cpw.sea.lag.yea.ele.uns[1,],
			cpw.sea.cha.yea.ele.uns[1,]
		),
		rbind(
			cpw.sea.exp.yea.ele.uns[2,],
			cpw.sea.lin.yea.ele.uns[2,],
			cpw.sea.log.yea.ele.uns[2,],
			cpw.sea.nai.yea.ele.uns[2,],
			cpw.sea.non.yea.ele.uns[2,],
			cpw.sea.gov.yea.ele.uns[2,],
			cpw.sea.lag.yea.ele.uns[2,],
			cpw.sea.cha.yea.ele.uns[2,]
		),
		rbind(
			cpw.sea.exp.yea.ele.uns[3,],
			cpw.sea.lin.yea.ele.uns[3,],
			cpw.sea.log.yea.ele.uns[3,],
			cpw.sea.nai.yea.ele.uns[3,],
			cpw.sea.non.yea.ele.uns[3,],
			cpw.sea.gov.yea.ele.uns[3,],
			cpw.sea.lag.yea.ele.uns[3,],
			cpw.sea.cha.yea.ele.uns[3,]
		),
		rbind(
			cpw.sea.exp.yea.ele.uns[4,],
			cpw.sea.lin.yea.ele.uns[4,],
			cpw.sea.log.yea.ele.uns[4,],
			cpw.sea.nai.yea.ele.uns[4,],
			cpw.sea.non.yea.ele.uns[4,],
			cpw.sea.gov.yea.ele.uns[4,],
			cpw.sea.lag.yea.ele.uns[4,],
			cpw.sea.cha.yea.ele.uns[4,]
		)
	)

# calculate cpw overall

	cpw.sea.exp.ove.uns = cpw.ov(sea.exp.std, seats)
	cpw.sea.lin.ove.uns = cpw.ov(sea.lin.std, seats)
	cpw.sea.log.ove.uns = cpw.ov(sea.log.std, seats)
	cpw.sea.nai.ove.uns = cpw.ov(sea.uns.nai, seats)
	cpw.sea.non.ove.uns = cpw.ov(sea.uns.non, seats)
	cpw.sea.gov.ove.uns = cpw.ov(sea.uns.gov, seats)
	cpw.sea.lag.ove.uns = cpw.ov(sea.uns.lag, seats)
	cpw.sea.cha.ove.uns = cpw.ov(sea.uns.cha, seats)
	cpw.sea.ove.uns = rbind(
		cpw.sea.exp.ove.uns,
		cpw.sea.lin.ove.uns,
		cpw.sea.log.ove.uns,
		cpw.sea.nai.ove.uns,
		cpw.sea.non.ove.uns,
		cpw.sea.gov.ove.uns,
		cpw.sea.lag.ove.uns,
		cpw.sea.cha.ove.uns
		)
		
#--------------------------------------------------
# correct prediction of winner and whether winner
# has majority (maj)
#--------------------------------------------------

# calculate maj by quarter

	maj.sea.exp.qua.uns = maj.by(sea.exp.std, seats, "seats.maj", "q")
	maj.sea.lin.qua.uns = maj.by(sea.lin.std, seats, "seats.maj", "q")
	maj.sea.log.qua.uns = maj.by(sea.log.std, seats, "seats.maj", "q")
	maj.sea.nai.qua.uns = maj.by(sea.uns.nai, seats, "seats.maj", "q")
	maj.sea.non.qua.uns = maj.by(sea.uns.non, seats, "seats.maj", "q")
	maj.sea.gov.qua.uns = maj.by(sea.uns.gov, seats, "seats.maj", "q")
	maj.sea.lag.qua.uns = maj.by(sea.uns.lag, seats, "seats.maj", "q")
	maj.sea.cha.qua.uns = maj.by(sea.uns.cha, seats, "seats.maj", "q")
	maj.sea.qua.uns = rbind(
		maj.sea.exp.qua.uns,
		maj.sea.lin.qua.uns,
		maj.sea.log.qua.uns,
		maj.sea.nai.qua.uns,
		maj.sea.non.qua.uns,
		maj.sea.gov.qua.uns,
		maj.sea.lag.qua.uns,
		maj.sea.cha.qua.uns
	)

# calculate maj by year

	maj.sea.exp.yea.uns = maj.by(sea.exp.std, seats, "seats.maj", "y")
	maj.sea.lin.yea.uns = maj.by(sea.lin.std, seats, "seats.maj", "y")
	maj.sea.log.yea.uns = maj.by(sea.log.std, seats, "seats.maj", "y")
	maj.sea.nai.yea.uns = maj.by(sea.uns.nai, seats, "seats.maj", "y")
	maj.sea.non.yea.uns = maj.by(sea.uns.non, seats, "seats.maj", "y")
	maj.sea.gov.yea.uns = maj.by(sea.uns.gov, seats, "seats.maj", "y")
	maj.sea.lag.yea.uns = maj.by(sea.uns.lag, seats, "seats.maj", "y")
	maj.sea.cha.yea.uns = maj.by(sea.uns.cha, seats, "seats.maj", "y")
	maj.sea.yea.uns = rbind(
		maj.sea.exp.yea.uns,
		maj.sea.lin.yea.uns,
		maj.sea.log.yea.uns,
		maj.sea.nai.yea.uns,
		maj.sea.non.yea.uns,
		maj.sea.gov.yea.uns,
		maj.sea.lag.yea.uns,
		maj.sea.cha.yea.uns
	)

# calculate maj by election

	maj.sea.exp.ele.uns = maj.by(sea.exp.std, seats, "seats.maj", "election")
	maj.sea.lin.ele.uns = maj.by(sea.lin.std, seats, "seats.maj", "election")
	maj.sea.log.ele.uns = maj.by(sea.log.std, seats, "seats.maj", "election")
	maj.sea.nai.ele.uns = maj.by(sea.uns.nai, seats, "seats.maj", "election")
	maj.sea.non.ele.uns = maj.by(sea.uns.non, seats, "seats.maj", "election")
	maj.sea.gov.ele.uns = maj.by(sea.uns.gov, seats, "seats.maj", "election")
	maj.sea.lag.ele.uns = maj.by(sea.uns.lag, seats, "seats.maj", "election")
	maj.sea.cha.ele.uns = maj.by(sea.uns.cha, seats, "seats.maj", "election")
	maj.sea.ele.uns = rbind(
		maj.sea.exp.ele.uns,
		maj.sea.lin.ele.uns,
		maj.sea.log.ele.uns,
		maj.sea.nai.ele.uns,
		maj.sea.non.ele.uns,
		maj.sea.gov.ele.uns,
		maj.sea.lag.ele.uns,
		maj.sea.cha.ele.uns
		)

# calculate maj by year and election

	maj.sea.exp.yea.ele.uns = maj.by.by(sea.exp.std, seats, "seats.maj", "y", "election")
	maj.sea.lin.yea.ele.uns = maj.by.by(sea.lin.std, seats, "seats.maj", "y", "election")
	maj.sea.log.yea.ele.uns = maj.by.by(sea.log.std, seats, "seats.maj", "y", "election")
	maj.sea.nai.yea.ele.uns = maj.by.by(sea.uns.nai, seats, "seats.maj", "y", "election")
	maj.sea.non.yea.ele.uns = maj.by.by(sea.uns.non, seats, "seats.maj", "y", "election")
	maj.sea.gov.yea.ele.uns = maj.by.by(sea.uns.gov, seats, "seats.maj", "y", "election")
	maj.sea.lag.yea.ele.uns = maj.by.by(sea.uns.lag, seats, "seats.maj", "y", "election")
	maj.sea.cha.yea.ele.uns = maj.by.by(sea.uns.cha, seats, "seats.maj", "y", "election")
	maj.sea.yea.ele.uns = rbind(
		rbind(
			maj.sea.exp.yea.ele.uns[1,],
			maj.sea.lin.yea.ele.uns[1,],
			maj.sea.log.yea.ele.uns[1,],
			maj.sea.nai.yea.ele.uns[1,],
			maj.sea.non.yea.ele.uns[1,],
			maj.sea.gov.yea.ele.uns[1,],
			maj.sea.lag.yea.ele.uns[1,],
			maj.sea.cha.yea.ele.uns[1,]
		),
		rbind(
			maj.sea.exp.yea.ele.uns[2,],
			maj.sea.lin.yea.ele.uns[2,],
			maj.sea.log.yea.ele.uns[2,],
			maj.sea.nai.yea.ele.uns[2,],
			maj.sea.non.yea.ele.uns[2,],
			maj.sea.gov.yea.ele.uns[2,],
			maj.sea.lag.yea.ele.uns[2,],
			maj.sea.cha.yea.ele.uns[2,]
		),
		rbind(
			maj.sea.exp.yea.ele.uns[3,],
			maj.sea.lin.yea.ele.uns[3,],
			maj.sea.log.yea.ele.uns[3,],
			maj.sea.nai.yea.ele.uns[3,],
			maj.sea.non.yea.ele.uns[3,],
			maj.sea.gov.yea.ele.uns[3,],
			maj.sea.lag.yea.ele.uns[3,],
			maj.sea.cha.yea.ele.uns[3,]
		),
		rbind(
			maj.sea.exp.yea.ele.uns[4,],
			maj.sea.lin.yea.ele.uns[4,],
			maj.sea.log.yea.ele.uns[4,],
			maj.sea.nai.yea.ele.uns[4,],
			maj.sea.non.yea.ele.uns[4,],
			maj.sea.gov.yea.ele.uns[4,],
			maj.sea.lag.yea.ele.uns[4,],
			maj.sea.cha.yea.ele.uns[4,]
		)
	)

# calculate maj overall

	maj.sea.exp.ove.uns = maj.ov(sea.exp.std, seats, "seats.maj")
	maj.sea.lin.ove.uns = maj.ov(sea.lin.std, seats, "seats.maj")
	maj.sea.log.ove.uns = maj.ov(sea.log.std, seats, "seats.maj")
	maj.sea.nai.ove.uns = maj.ov(sea.uns.nai, seats, "seats.maj")
	maj.sea.non.ove.uns = maj.ov(sea.uns.non, seats, "seats.maj")
	maj.sea.gov.ove.uns = maj.ov(sea.uns.gov, seats, "seats.maj")
	maj.sea.lag.ove.uns = maj.ov(sea.uns.lag, seats, "seats.maj")
	maj.sea.cha.ove.uns = maj.ov(sea.uns.cha, seats, "seats.maj")
	maj.sea.ove.uns = rbind(
		maj.sea.exp.ove.uns,
		maj.sea.lin.ove.uns,
		maj.sea.log.ove.uns,
		maj.sea.nai.ove.uns,
		maj.sea.non.ove.uns,
		maj.sea.gov.ove.uns,
		maj.sea.lag.ove.uns,
		maj.sea.cha.ove.uns
		)		

#-----------------------
# number of observations
#-----------------------

  n.ove.uns = c(nrow(Sel), tapply(Sel$election, Sel$election, length))	
	n.qua.uns = c(nrow(Sel), tapply(Sel$election, Sel$q, length))	
	n.yea.uns = c(nrow(Sel), tapply(Sel$election, Sel$y, length))	

# ==============
# = save image =
# ==============

	save.image(file="Accuracy.RData")	

# ===================
# = end source code =
# ===================