#--------------------------------------------------
# Replication material for
# 
# 	Vote Expectations Versus Vote Intentions:
# 	Rival Forecasting Strategies
# 
# 	Andreas E. Murr
# 	University of Warwick
# 	a.murr@warwick.ac.uk
#
# 	Mary Stegmaier
# 	University of Missouri
# 	stegmaierm@missouri.edu
#
# 	Michael S. Lewis-Beck
# 	University of Iowa
# 	michael-lewis-beck@uiowa.edu
# 
# 	25.01.19
#--------------------------------------------------
# 
# 1) Data
#
# C83.RData 		Contains constituency results for 
#								the 1983 election
# C87.RData 		... 1987 election
# C97.RData 		... 1997 election
# C10.RData 		... 2010 election
# C15.RData 		... 2015 election
# Polls.RData		Contains Gallup, CMS, and ComRes
# 							polling data as well as national
# 							election results
#
#-------------------------------------------------
# 
# 2) Codebook
#
# See also the file 6-OnlineAppendix.pdf
#
# Polls.RData
#
# 	election				election year
# 	months					months before election
# 	exp.*** 				voter expectations
# 	int.*** 				voter intentions
# 	split						split in party system
# 	inc							incumbent
# 	seats.***				national seat share
# 	seats.***.lag		national seat share (lagged)
# 	votes.***				national vote share
# 	votes.***.lag		national vote share (lagged)
# 	ava.int					voter intentions available
# 	ava.exp					voter expectations available
# 	ava.bot					both avairable
# 	q								quarters before election
# 	y								years before election
#
# C**.RData
#
# 	Constituency vote shares for Conservatives / 
# 	Labour / Liberal Democrats / Others (con / 
# 	lab / lib / oth)
# 
#--------------------------------------------------
# 
# 3) Software code
#
# 1-Functions.R		Contains all necessary functions
# 2-Forecast.R		Estimates the regression models
# 								and generates the forecasts
# 3-Accuracy.R		Calculates the forecast accuracy
# 4-Main.R				Tabulates the forecast accuracy
#									as reported in the main text
# 5-Appendix.R		Tabulates the regression models
#									and tabulates and plots the 
#									forecast accuracy as reported in
# 								the appendix
#
#--------------------------------------------------
# 
# 4) Derived files
#
# Forecasts.RData		Contains the estimated models
# 									and generated forecasts
# Accuracy.RData		Contains the forecast accuracy
# Figure-1.pdf			Correct prediction of winner
# Figure-2.pdf			Correct prediction of winner 
# 									and whether winner has majority
# Figure-3.pdf			Mean absolute error in seat 
# 									shares
#
#--------------------------------------------------
# 
# 5) Dependencies
#
# 1-Functions.R
# 	loads memisc, reshape, RColorBrewer, and 
#   lattice packages
# 2-Forecast.R
#   loads 1-Functions.R, Polls.RData, and C**.RData
#   saves Forecasts.RData
# 3-Combination.R
#   loads Forecasts.RData
#   saves Accuracy.RData
# 4-Main.R
#   loads Forecasts.RData
#   produces Tables 1 and 2 in main text
# 5-Appendix.R
#   loads Forecasts.RData and Accuracy.RData
#   produces Tables 1 to 9 and Figures 1 to 3 in
# 	online appendix
# 
#--------------------------------------------------