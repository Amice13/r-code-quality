# load packages

	library(memisc)
	library(reshape)
	library(RColorBrewer)
  library(lattice)
	lattice.options(default.theme = standard.theme(color = FALSE))
	
# descriptive statistics functions with missing values removed

	length.na = function(x){length(na.omit(x))}
	mean.na = function(x){mean(x, na.rm=TRUE)}
	median.na = function(x){median(x, na.rm=TRUE)}
	sum.na = function(x){sum(x, na.rm=TRUE)}

# functions for calculating cpw rate

	cpw = function(f, o){	
		# f=forecast; o=outcome	
		n.f = length(f)
		n.o = length(o)
		m = (1:n.f)[f==max(f)]==(1:n.o)[o==max(o)]	
		ifelse(length(m)==1, m, ifelse(sum(is.na(m))==n.f, m[1], 1/length(m)))
	}
	cpw.ov = function(f, o){ # f = forecast, o = observed
		round(mean.na(apply(Sel, 1, function(x){cpw(x[f], x[o])}))*100, 0)
	}
	cpw.by = function(f, o, g){ # f = forecast, o = observed, g = group
		round(tapply(apply(Sel, 1, function(x){cpw(x[f], x[o])}), Sel[g], mean.na)*100, 0)
	}
	cpw.by.by = function(f, o, g1, g2){ # f = forecast, o = observed, g1 = first group, g2 = second group
		round(tapply(apply(Sel, 1, function(x){cpw(x[f], x[o])}), list(Sel[g1][,1], Sel[g2][,1]), mean.na)*100, 0)
	}
	
# correct prediction of winner and of minority/majority government

	maj = function(f, o, m){ # f = forecast, o = observed, m = seat share required in great britain for majority in westminster (great britain + northern ireland)
		h = cpw(f, o)
	  s = (max(f)>=m) == (max(o)>=m)
		ifelse(h==TRUE & s==TRUE, TRUE, FALSE)
	}
	maj.ov = function(f, o, m){ # f = forecast, o = observed, m = seat share required in great britain for majority in westminster (great britain + northern ireland)
		round(mean.na(apply(Sel, 1, function(x){maj(x[f], x[o], x[m])}))*100, 0)
	}
	maj.by = function(f, o, m, g){ # f = forecast, o = observed, m = seat share required in great britain for majority in westminster (great britain + northern ireland), g = group
		round(tapply(apply(Sel, 1, function(x){maj(x[f], x[o], x[m])}), Sel[g], mean.na)*100, 0)
	}
	maj.by.by = function(f, o, m, g1, g2){ # f = forecast, o = observed, m = seat share required in great britain for majority in westminster (great britain + northern ireland), g1 = first group, g2 = second group
		round(tapply(apply(Sel, 1, function(x){maj(x[f], x[o], x[m])}), list(Sel[g1][,1], Sel[g2][,1]), mean.na)*100, 0)
	}	

# functions for calculating mean absolute error (mae)

	# mae = function(p,a){mean.na(abs(p-a))} # p=predicted; a=actual
	mae = function(f, o){
		x = Sel[f] # forecasts
		y = Sel[o] # actual
		apply(abs(x-y), 1, mean.na) # monthly mae
	}
	mae.ov = function(f, o){
		x = Sel[f] # forecasts
		y = Sel[o] # actual
		round(mean.na(unlist(abs(x-y)))*100, 1) # overall mae
	}
	mae.by = function(f, o, g){
		x = Sel[f] # forecasts
		y = Sel[o] # actual
		z = Sel[g][,1] # group
		round(tapply(apply(abs(x-y), 1, mean), z, mean.na)*100, 1) # mean of mae by group
	}
	mae.by.by = function(f, o, g1, g2){
		x = Sel[f] # forecasts
		y = Sel[o] # actual
		z1 = Sel[g1][,1] # group
		z2 = Sel[g2][,1] # group
		round(tapply(apply(abs(x-y), 1, mean), list(z1, z2), mean.na)*100, 1) # mean of mae by group
	}
	mae.row = function(f, o){
		err = abs(f-o)
		apply(err, 1, mean.na)
	}

# functions for calculating uniform national swing models
# they assume that the data are vote shares between 0 and 1
# plus that the data are ordered as con, lab, lib, oth
# see curtice and firth (2008: 518f)

	Naive = function(x){
		# calculate predicted winner in constituency
		p = t(apply(x, 1, function(x){x==max(x)}))
		# calculate national prediction of seat shares (with others)
		as.vector((apply(p, 2, sum) / nrow(p)))
	}
	CurticeFirth = function(x){
		# convert data to percentage points
		x = x*100
		# calculate probabilities
		max.v = apply(x, 1, max)
		r = exp(-(((max.v - x)/4)^1.5))
		p = t(apply(r, 1, function(x){x/sum(x)}))
		# calculate national prediction of seat shares (with others)
		as.vector((apply(p, 2, sum) / nrow(p)))
	}
	CurticeFirthBrier = function(x){
		# convert data to percentage points
		x = x*100
		# calculate probabilities
		max.v = apply(x, 1, max)
		r = exp(-(((max.v - x)/4)^1.5))
		p = t(apply(r, 1, function(x){x/sum(x)}))
		# calculate actual winner
		w = t(apply(x, 1, function(x){x==max(x)}))
		# calculate brier score
		apply((p-w)^2, 2, function(x){sum(x)/length(x)})
	}

# ===================
# = end source code =
# ===================