# clear working memory

	rm(list=ls())
	
# source generated forecasts

	load(file="Forecasts.RData")
	
# =====================
# = estimation tables =
# =====================

#--------------------------------------------------
# index elections without any major boundary 
# changes
#--------------------------------------------------

	sel = c(13, 14, 16, 19, 20)-1
	elections[sel]

#--------
# table 1
#--------

# con
	r = m.vot.con.non[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lab
	r = m.vot.lab.non[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lib
	r = m.vot.lib.non[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# oth
	r = m.vot.oth.non[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)

#--------
# table 2
#--------

# con
	r = m.vot.con.gov[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lab
	r = m.vot.lab.gov[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lib
	r = m.vot.lib.gov[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# oth
	r = m.vot.oth.gov[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)

#--------
# table 3
#--------

# con
	r = m.vot.con.lag[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lab
	r = m.vot.lab.lag[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lib
	r = m.vot.lib.lag[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# oth
	r = m.vot.oth.lag[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)

#--------
# table 4
#--------

# con
	r = m.vot.con.cha[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lab
	r = m.vot.lab.cha[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lib
	r = m.vot.lib.cha[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# oth
	r = m.vot.oth.cha[sel]
	names(r) = paste("r", elections[sel], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1997, r.2010,	r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)	

#--------
# table 5
#--------

# con
	r = m.sea.con.log[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lab
	r = m.sea.lab.log[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lib
	r = m.sea.lib.log[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# oth
	r = m.sea.oth.log[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)

#--------
# table 6
#--------

# con
	r = m.sea.con.exp[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lab
	r = m.sea.lab.exp[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lib
	r = m.sea.lib.exp[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# oth
	r = m.sea.oth.exp[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)

#--------
# table 7
#--------

# con
	r = m.sea.con.lin[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lab
	r = m.sea.lab.lin[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# lib
	r = m.sea.lib.lin[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)
# oth
	r = m.sea.oth.lin[start:stop]
	names(r) = paste("r", elections[start:stop], sep=".")	
	list2env(r, envir=.GlobalEnv)
	mtable(
		r.1983, r.1987, r.1992, r.1997, r.2001, r.2005, r.2010, r.2015, 
		digits=2, sdigits=2, summary.stats=c("R-squared", "N"), signif.symbols=FALSE
	)

# ===================
# = accuracy tables =
# ===================

# clear working memory

	rm(list=ls())
	
# load evaluations of forecasts

	load("Accuracy.RData")

#--------
# table 8
#--------

# all elections
	
	cbind(maj.sea.ove.ful, maj.sea.ele.ful)[order(maj.sea.ove.ful, decreasing=TRUE),]
	n.ove.ful
	
# elections with constant constituency boundaries

	cbind(maj.sea.ove.uns, maj.sea.ele.uns)[order(maj.sea.ove.uns, decreasing=TRUE),]
	n.ove.uns

#--------
# table 9
#--------

# all elections

	cbind(maj.sea.ove.ful, maj.sea.qua.ful, maj.sea.yea.ful[,-1])[order(maj.sea.ove.ful, decreasing=TRUE),]
	n.qua.ful
	n.yea.ful[-2]

# elections with constant constituency boundaries

	cbind(maj.sea.ove.uns, maj.sea.qua.uns, maj.sea.yea.uns[,-1])[order(maj.sea.ove.uns, decreasing=TRUE),]
	n.qua.uns
	n.yea.uns

# ====================
# = accuracy figures =
# ====================

#---------
# figure 1
#---------

# calucalate cpw

	Sel = Com[Com$election>=1987,]
	Sel$cpw.log = apply(Sel, 1, function(x){cpw(x[sea.log.std], x[seats])})
	Sel$cpw.lin = apply(Sel, 1, function(x){cpw(x[sea.lin.std], x[seats])})
	Sel$cpw.exp = apply(Sel, 1, function(x){cpw(x[sea.exp.std], x[seats])})
	Sel$cpw.nai = apply(Sel, 1, function(x){cpw(x[sea.uns.nai], x[seats])})
	Sel$cpw.non = apply(Sel, 1, function(x){cpw(x[sea.uns.non], x[seats])})
	Sel$cpw.gov = apply(Sel, 1, function(x){cpw(x[sea.uns.gov], x[seats])})
	Sel$cpw.lag = apply(Sel, 1, function(x){cpw(x[sea.uns.lag], x[seats])})
	Sel$cpw.cha = apply(Sel, 1, function(x){cpw(x[sea.uns.cha], x[seats])})

# reshape data

	Mel = melt(Sel[Sel$months>=-48,], id.vars=c("election", "months"), measure.vars=c("cpw.log", "cpw.lin", "cpw.exp", "cpw.nai", "cpw.non", "cpw.gov", "cpw.lag", "cpw.cha"))
	Mel$pre = as.character(Mel$variable)
	Mel$pre[Mel$pre=="cpw.log"] = "LOG"
	Mel$pre[Mel$pre=="cpw.lin"] = "LIN"
	Mel$pre[Mel$pre=="cpw.exp"] = "EXP"
	Mel$pre[Mel$pre=="cpw.nai"] = "NAI"
	Mel$pre[Mel$pre=="cpw.non"] = "NON"
	Mel$pre[Mel$pre=="cpw.cha"] = "CHA"
	Mel$pre[Mel$pre=="cpw.lag"] = "LAG"
	Mel$pre[Mel$pre=="cpw.gov"] = "GOV"
	Mel$pre = ordered(Mel$pre, levels=c(
		"NAI",
		"NON",
		"CHA",
		"GOV",
		"LAG",
		"LOG",
		"LIN",
		"EXP"
		))

# plot cpw

	pdf("Figure-1.pdf", height=4, width=6)
	levelplot(as.numeric(value) ~ months*pre | as.factor(election), data=Mel, as.table=TRUE, region=TRUE, border=FALSE, cuts=1, colorkey=list(space="top", labels=list(labels=c("                              Correct prediction of winner\nno", "yes"), at=c(0.25, .75)), height=.5, width=.4), xlab="Months until election", ylab="", scales=list(alternating=FALSE, y=list(labels=c(
		"Intentions (NAI)",
		"Intentions (NON)",
		"Intentions (CHA)",
		"Intentions (GOV)",
		"Intentions (LAG)",
		"Intentions (LOG)",
		"Intentions (LIN)",
		"Expectations (EXP)"
	), cex=.8)), layout=c(8, 1))
	dev.off()

#---------
# figure 2
#---------	

# calucalate maj

	Sel = Com[Com$election>=1987,]
	Sel$maj.log = apply(Sel, 1, function(x){maj(x[sea.log.std], x[seats], x["seats.maj"])})
	Sel$maj.lin = apply(Sel, 1, function(x){maj(x[sea.lin.std], x[seats], x["seats.maj"])})
	Sel$maj.exp = apply(Sel, 1, function(x){maj(x[sea.exp.std], x[seats], x["seats.maj"])})
	Sel$maj.nai = apply(Sel, 1, function(x){maj(x[sea.uns.nai], x[seats], x["seats.maj"])})
	Sel$maj.non = apply(Sel, 1, function(x){maj(x[sea.uns.non], x[seats], x["seats.maj"])})
	Sel$maj.gov = apply(Sel, 1, function(x){maj(x[sea.uns.gov], x[seats], x["seats.maj"])})
	Sel$maj.lag = apply(Sel, 1, function(x){maj(x[sea.uns.lag], x[seats], x["seats.maj"])})
	Sel$maj.cha = apply(Sel, 1, function(x){maj(x[sea.uns.cha], x[seats], x["seats.maj"])})

# find out highest maj rate

# reshape data

	Mel = melt(Sel[Sel$months>=-48,], id.vars=c("election", "months"), measure.vars=c("maj.log", "maj.lin", "maj.exp", "maj.nai", "maj.non", "maj.gov", "maj.lag", "maj.cha"))
	Mel$pre = as.character(Mel$variable)
	Mel$pre[Mel$pre=="maj.log"] = "LOG"
	Mel$pre[Mel$pre=="maj.lin"] = "LIN"
	Mel$pre[Mel$pre=="maj.exp"] = "EXP"
	Mel$pre[Mel$pre=="maj.nai"] = "NAI"
	Mel$pre[Mel$pre=="maj.non"] = "NON"
	Mel$pre[Mel$pre=="maj.cha"] = "CHA"
	Mel$pre[Mel$pre=="maj.lag"] = "LAG"
	Mel$pre[Mel$pre=="maj.gov"] = "GOV"
	Mel$pre = ordered(Mel$pre, levels=c(
		"NAI",
		"NON",
		"CHA",
		"GOV",
		"LAG",
		"LOG",
		"LIN",
		"EXP"
		))

# plot maj

	pdf("Figure-2.pdf", height=4, width=6)
	levelplot(as.numeric(value) ~ months*pre | as.factor(election), data=Mel, as.table=TRUE, region=TRUE, border=FALSE, cuts=1, colorkey=list(space="top", labels=list(labels=c("                                  Correct prediction of winner and whether winner has overall majority\nno", "yes"), at=c(0.25, .75)), height=.5, width=.4), xlab="Months until election", ylab="", scales=list(alternating=FALSE, y=list(labels=c(
		"NAI",
		"NON",
		"CHA",
		"GOV",
		"LAG",
		"LOG",
		"LIN",
		"EXP"
	), cex=.8)), layout=c(8, 1))
	dev.off()
	
#---------
# figure 3
#---------

# create mae for each model

	Com$mae.exp = mae.row(Com[sea.exp.std], Com[seats])
	Com$mae.lin = mae.row(Com[sea.lin.std], Com[seats])
	Com$mae.log = mae.row(Com[sea.log.std], Com[seats])
	Com$mae.nai = mae.row(Com[sea.uns.nai], Com[seats])
	Com$mae.non = mae.row(Com[sea.uns.non], Com[seats])
	Com$mae.gov = mae.row(Com[sea.uns.gov], Com[seats])
	Com$mae.lag = mae.row(Com[sea.uns.lag], Com[seats])
	Com$mae.cha = mae.row(Com[sea.uns.cha], Com[seats])

	# sort by availability and average mae

	col = rev(brewer.pal(9, "Greys"))[1:8]

	Sel = Com[Com$election>=1987&!is.na(Com$mae.exp)&Com$ava.bot,]

	mean.na(Sel$mae.exp)
	mean.na(Sel$mae.lin)
	mean.na(Sel$mae.log)

	mean.na(Sel$mae.gov)
	mean.na(Sel$mae.non)
	mean.na(Sel$mae.cha)
	mean.na(Sel$mae.lag)
	mean.na(Sel$mae.nai)

	pdf("Figure-3.pdf", height=6, width=6)
	xyplot(mae.nai + mae.lag + mae.cha + mae.non + mae.gov + mae.log + mae.lin + mae.exp ~ months | as.factor(election), 
	data=Sel, 
	as.table=TRUE, 
	type="b", 
	key=list(
		space="top",
		lines=list(lty=rev(1:8), col=rev(col)),
		points=list(pch=rev(c(16, 1:7)), col=rev(col)),		
		text=list(c(
			"NAI: Intentions in naive UNS model",
			"LAG: Intentions and lagged vote share in non-naive UNS model", 
			"CHA: Intentions and change in vote share in non-naive UNS model", 
			"NON: Intentions in non-naive UNS model",
			"GOV: Intentions and government status in non-naive UNS model",
			"LOG: Intentions and lagged seat share in log-linear model",
			"LIN: Intentions and lagged seat share in linear model",
			"EXP: Expectations and lagged seat share in linear model"
			))
	),
	lty=rev(1:8),
	pch=rev(c(16, 1:7)),
	col=rev(col),
	ylab="Mean absolute error in seat share", 
	xlab="Months until election")
	dev.off()	

# ===================
# = end source code =
# ===================	