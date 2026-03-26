########################################
# Analysis Replication Code for Table S32 from
# "The Effect of Citizenship on the Long-Term Earnings of Marginalized Immigrants: 
# Quasi-Experimental Evidence from Switzerland"
# Science Advances (DOI: 10.1126/sciadv.aay1610)
# Jens Hainmueller, Dominik Hangartner, and Dalston Ward
# December 2019
########################################

# Originally run in R 3.5.2

library(data.table) # version 1.12.6

setwd("<<Set as appropriate>>")

# Stage 1: Load municipal level data sets (all are available from Federal Office of Statistics www.bfs.admin.ch except for "ballot_box_municipalities.csv, which is based on the list of ballot box municipalites in Hainmueller and Hangartner 2013, APSR)
ballot_box <- fread("ballot_box_municipalities.csv", key = "bfsnr")
fb1 <- fread("foreign_born_1990_2000_clean.csv", key = "bfsnr", drop = "Location")
fb2 <- fread("foreign_born2010_clean.csv", key = "bfsnr", drop = "Location")
nat <- fread("inhabitants_naturalization_clean.csv", key = "bfsnr", drop = "Location")
sec <- fread("sector_shares_clean.csv", key = "bfsnr", drop = "Location")
svp <- fread("SVP_votes_1991_1999_2011_clean.csv", key = "bfsnr", drop = "Location")
emp <- fread("unemployed_1990_2000_clean.csv", key = "bfsnr", drop = "Location")    

### To match the H&H 2013 APSR list
ballot_box[Location %in% c("Ennetmoos", "Heiden", "Unteriberg", "Urnäsch", "Weggis"), ballotbox := 1]
ballot_box[Location == "Niederdorf", ballotbox := 0]

# Calculate mean foreign born population in ballot box and other municipalities
fb1 <- fb1[ballot_box]
fb1[ , ratio_1990 := foreign_1990/total_1990]
fb1[ , ratio_2000 := foreign_2000/total_2000]
pop1 <- fb1[ , lapply(.SD, mean), keyby = ballotbox, .SDcols = c("total_1990", "total_2000")]

# weight the ratio measures by population
ratio90 <- fb1[ , .("ratio90" = weighted.mean(ratio_1990, w = total_1990/sum(total_1990))), keyby = ballotbox]
ratio00 <- fb1[ , .("ratio00" = weighted.mean(ratio_2000, w = total_2000/sum(total_2000))), keyby = ballotbox]

# Repeat above steps for 2010 foreign born measures
fb2 <- fb2[ballot_box]
fb2[ , ratio_2010 := `2010_foreign`/`2010_total`]
pop2 <- fb2[ , lapply(.SD, mean, na.rm = T), keyby = ballotbox, .SDcols = c("2010_total")]
ratio10 <- fb2[ , .("ratio10" = weighted.mean(ratio_2010, w = `2010_total`/sum(`2010_total`, na.rm = T), na.rm = T)), keyby = ballotbox]

# merge foreign born datasets for convenicence in calculating naturalization rates
pops <- fb1[ , .(bfsnr, ballotbox, Location, total_1990, total_2000)][fb2[ , .(bfsnr, `2010_total`)]]

# Calculate naturalization rates
nat <- nat[Type == "foreign"][pops]
nat[ , nat_rate_1990 := (-1*Naturalizations_1990)/Population_jan1_1990]
nat[ , nat_rate_2000 := (-1*Naturalizations_2000)/Population_jan1_2000]
nat[ , nat_rate_2010 := (-1*Naturalizations_2010)/Population_jan1_2010]

nat_90 <- nat[ , .("nat_rate90" = weighted.mean(nat_rate_1990, W = total_1990/sum(total_1990, na.rm = T), na.rm = T)), keyby = ballotbox]
nat_00 <- nat[ , .("nat_rate00" = weighted.mean(nat_rate_2000, W = total_2000/sum(total_2000, na.rm = T), na.rm = T)), keyby = ballotbox]
nat_10 <- nat[ , .("nat_rate10" = weighted.mean(nat_rate_2010, W = `2010_total`/sum(`2010_total`, na.rm = T), na.rm = T)), keyby = ballotbox]

# Calculate labor market structure
sec[ , sectorT := chartr("ä", "a", sectorT)]
sec <- dcast(sec, bfsnr ~ sectorT, value.var = colnames(sec)[-c(1:2)])
sec[ , primary_prct_1995 := employed_total_1995_Primarsektor / (employed_total_1995_Primarsektor + employed_total_1995_Sekundarsektor + employed_total_1995_Tertiarsektor)]
sec[ , primary_prct_2001 := employed_total_2001_Primarsektor / (employed_total_2001_Primarsektor + employed_total_2001_Sekundarsektor + employed_total_2001_Tertiarsektor)]
sec[ , primary_prct_2005 := employed_total_2005_Primarsektor / (employed_total_2005_Primarsektor + employed_total_2005_Sekundarsektor + employed_total_2005_Tertiarsektor)]
sec[ , primary_prct_2008 := employed_total_2008_Primarsektor / (employed_total_2008_Primarsektor + employed_total_2008_Sekundarsektor + employed_total_2008_Tertiarsektor)]

sec[ , secondary_prct_1995 := employed_total_1995_Sekundarsektor / (employed_total_1995_Primarsektor + employed_total_1995_Sekundarsektor + employed_total_1995_Tertiarsektor)]
sec[ , secondary_prct_2001 := employed_total_2001_Sekundarsektor / (employed_total_2001_Primarsektor + employed_total_2001_Sekundarsektor + employed_total_2001_Tertiarsektor)]
sec[ , secondary_prct_2005 := employed_total_2005_Sekundarsektor / (employed_total_2005_Primarsektor + employed_total_2005_Sekundarsektor + employed_total_2005_Tertiarsektor)]
sec[ , secondary_prct_2008 := employed_total_2008_Sekundarsektor / (employed_total_2008_Primarsektor + employed_total_2008_Sekundarsektor + employed_total_2008_Tertiarsektor)]

sec[ , tertiary_prct_1995 := employed_total_1995_Tertiarsektor / (employed_total_1995_Primarsektor + employed_total_1995_Sekundarsektor + employed_total_1995_Tertiarsektor)]
sec[ , tertiary_prct_2001 := employed_total_2001_Tertiarsektor / (employed_total_2001_Primarsektor + employed_total_2001_Sekundarsektor + employed_total_2001_Tertiarsektor)]
sec[ , tertiary_prct_2005 := employed_total_2005_Tertiarsektor / (employed_total_2005_Primarsektor + employed_total_2005_Sekundarsektor + employed_total_2005_Tertiarsektor)]
sec[ , tertiary_prct_2008 := employed_total_2008_Tertiarsektor / (employed_total_2008_Primarsektor + employed_total_2008_Sekundarsektor + employed_total_2008_Tertiarsektor)]

sec <- sec[pops]
setnames(sec, old = "2010_total", new = "total_2010")
sec95 <- sec[ , lapply(.(primary_prct_1995, secondary_prct_1995, tertiary_prct_1995), weighted.mean, w = total_1990/sum(total_1990, na.rm = T), na.rm = T), keyby = ballotbox]
sec01 <-sec[ , lapply(.(primary_prct_2001, secondary_prct_2001, tertiary_prct_2001), weighted.mean, w = total_2000/sum(total_2000, na.rm = T), na.rm = T), keyby = ballotbox]
sec05 <- sec[ , lapply(.(primary_prct_2005, secondary_prct_2005, tertiary_prct_2005), weighted.mean, w = total_2000/sum(total_2000, na.rm = T), na.rm = T), keyby = ballotbox]
sec08 <- sec[!is.na(total_2010) , lapply(.(primary_prct_2008, secondary_prct_2008, tertiary_prct_2008), weighted.mean, w = total_2010/sum(total_2010, na.rm = T), na.rm = T), keyby = ballotbox]

# Calculate SVP Shares
svp <- svp[pops]
setnames(svp, old = "2010_total", new = "total_2010")
svp91 <- svp[ , .("svp91" = weighted.mean(x = vote_prct_1991, w = total_1990/sum(total_1990, na.rm = T), na.rm = T)), keyby = ballotbox]
svp99 <- svp[ , .("svp99" = weighted.mean(x = vote_prct_1999, w = total_2000/sum(total_2000, na.rm = T), na.rm = T)), keyby = ballotbox]
svp11 <- svp[ , .("svp11" = weighted.mean(x = vote_prct_2011, w = total_2010/sum(total_2010, na.rm = T), na.rm = T)), keyby = ballotbox]

# Calculate unemployment rates
emp <- emp[pops]
emp[ , unemp_rate_1990 := unemployed_1990/(unemployed_1990 + employed_1990)]
emp[ , unemp_rate_2000 := unemployed_2000/(unemployed_2000 + employed_2000)]

emp9000 <- emp[ ,.(Year = rep(c(1990, 2000), tiems = 6), "Unemp Rate" = mapply(function(X,W){weighted.mean(X, w = W/sum(W, na.rm = T), na.rm = T)}, X = .(unemp_rate_1990, unemp_rate_2000), W = .(total_1990, total_2000))), keyby = .(Type, ballotbox)]


# Here are all of the pieces separately. The code below does some cleaning of names (for convenience) and then merges everything and prints a table.
pop1
pop2
ratio90
ratio00
ratio10
nat_90
nat_00
nat_10
setnames(sec95, old = paste0("V", 1:3), new = paste0(c("prim", "sec", "tert"), "_prct_", 95))
sec95
setnames(sec01, old = paste0("V", 1:3), new = paste0(c("prim", "sec", "tert"), "_prct_", "01"))
sec01
setnames(sec05, old = paste0("V", 1:3), new = paste0(c("prim", "sec", "tert"), "_prct_", "05"))
sec05
setnames(sec08, old = paste0("V", 1:3), new = paste0(c("prim", "sec", "tert"), "_prct_", "08"))
sec08
svp91
svp99
svp11
emp9000[ , Type := paste0("Uemp_", Type)]
emp_fin <- dcast(emp9000, ballotbox ~ Type + Year, value.var = "Unemp Rate")

out <- pop1[pop2[ratio90[ratio00[ratio10[nat_90[nat_00[nat_10[sec95[sec01[sec05[sec08[svp91[svp99[svp11[emp_fin]]]]]]]]]]]]]]]

xtable::xtable(round(t(out[order(-ballotbox)]), 2))
