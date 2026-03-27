################################################
# Replication code 
# Article: The Credibility of Party Policy Rhetoric: Survey Experimental Evidence
# Author: Pablo Fernandez-Vazquez
# Purpose: Replicate results in the paper that rely on CROWDSOURCED CODING of PARTY STATEMENTS
# Source Data: 	1) IMM_statements.csv
#				2) NHS_statements.csv 
# Software: R version 3.4.3
################################################


############ INSTALLING AND LOADING PACKAGES
install.packages("foreign", repos = "http://cran.rstudio.com")
install.packages("ggplot2", repos = "http://cran.rstudio.com")

library(foreign)
library(ggplot2)


############ SET Working Directory
	setwd("/Users/pablofernandez/Dropbox/dissertation/survey_experiment/survey_experiment_paper/jop_short_submission/jop_short_submission_replication_files")


############ LOADING DATA

	# IMMIGRATION
	imm <- read.csv("IMM_statements.csv", header=TRUE, stringsAsFactors=FALSE)

	# NHS
	nhs <- read.csv("NHS_statements.csv", header=TRUE, stringsAsFactors=FALSE)


############ FIGURE A.4

### Top-plot IMMIGRATION Statements

	# Select coding items that capture evaluations of whether the statement is CLEAR
		immclar <- imm[, grep("(^c_|^l_).*2$", names(imm))]
	# Proportion who thinks the statement is CLEAR
		imm_clear <- apply(immclar,2,function(x) sum(x==1, na.rm=T)/sum(!is.na(x)))
	# Select coding items that capture placement attributed to meaning of the statement on the issue scale
		immmeanclar <- imm[, grep("(^c_|^l_).*[a-z]$", names(imm))]
	# Compute average placements for each statement
		imm_place <- apply(immmeanclar,2,function(x) mean(x, na.rm=T))

	# Putting together
		immcleargraph <- data.frame(clear=imm_clear, place=imm_place, party=NA)
		immcleargraph$party[grep("^c_",row.names(immcleargraph))] <- "CONSERVATIVE"
		immcleargraph$party[grep("^l_",row.names(immcleargraph))] <- "LABOUR"
		immcleargraph$party <- factor(immcleargraph$party, levels=c("LABOUR", "CONSERVATIVE"))
	# Creating indicator of which statement was selected for each treatment condition
		immcleargraph$selected <- ifelse(rownames(immcleargraph)== "c_number2" | rownames(immcleargraph)== "c_our2", 1, 0)
		immcleargraph$selected <- ifelse(rownames(immcleargraph)== "l_people2" | rownames(immcleargraph)== "l_centuri2", 1, immcleargraph$selected)
	
		ycoord <-seq(1,nrow(immcleargraph))
	
	# Creating the plot
		imm_clarity <- ggplot() + scale_y_continuous(limits=c(0, 1), breaks=seq(0,1,.25), labels=seq(0,1,.25), name="CLARITY of STATEMENT") 
		imm_clarity <- imm_clarity + scale_x_continuous(limits=c(0,10.1), breaks=c(0:10), labels=c("0\nCLOSE\nBORDERS","1","2","3","4","5","6","7","8","9","10\nOPEN\nBORDERS"), name="AVERAGE PLACEMENT")
		imm_clarity <- imm_clarity + geom_point(aes(y=immcleargraph$clear[immcleargraph$selected==0],x=immcleargraph$place[immcleargraph$selected==0], color=immcleargraph$party[immcleargraph$selected==0]), size=4, shape=1, stroke = 1.25) + theme(axis.ticks.y = element_blank())
		imm_clarity <- imm_clarity + geom_point(aes(y=immcleargraph$clear[immcleargraph$selected==1],x=immcleargraph$place[immcleargraph$selected==1], color=immcleargraph$party[immcleargraph$selected==1]), size=5)
		imm_clarity <- imm_clarity + theme(panel.grid.major = element_blank()) + labs(title="MEANING and CLARITY")
		imm_clarity <- imm_clarity + guides(color=guide_legend(title="PARTY")) + theme( panel.background = element_rect(fill = "white", colour = "grey0", size = 1 ), text = element_text(size=9)) + scale_color_manual(values=c("#d50000","#0087dc"))
		imm_clarity
	# Saving plot
		ggsave(plot = imm_clarity, filename = "IMM_clarity.pdf", width=6, height=3.75)



### Bottom-plot NHS Statements

	# Select coding items that capture evaluations of whether the statement is CLEAR
		nhsclar <- nhs[, grep("(^c_|^l_).*2$", names(nhs))]
	# Proportion who thinks the statement is CLEAR
		nhs_clear <- apply(nhsclar,2,function(x) sum(x==1, na.rm=T)/sum(!is.na(x)))
	# Select coding items that capture placement attributed to meaning of the statement on the issue scale
		nhsmeanclar <- nhs[, grep("(^c_|^l_).*[a-z]$", names(nhs))]
	# Compute average placements for each statement
		nhs_place <- apply(nhsmeanclar,2,function(x) mean(x, na.rm=T))

	# Putting together
		nhscleargraph <- data.frame(clear=nhs_clear, place=nhs_place, party=NA)
		nhscleargraph$party[grep("^c_",row.names(nhscleargraph))] <- "CONSERVATIVE"
		nhscleargraph$party[grep("^l_",row.names(nhscleargraph))] <- "LABOUR"
		nhscleargraph$party <- factor(nhscleargraph$party, levels=c("LABOUR", "CONSERVATIVE"))
	
	# Creating indicator of which statement was selected for each treatment condition
		nhscleargraph$select <- ifelse(rownames(nhscleargraph)== "c_goingto2" | rownames(nhscleargraph)== "c_secure2", 1, 0)
		nhscleargraph$select <- ifelse(rownames(nhscleargraph)== "l_tough2" | rownames(nhscleargraph)== "l_time2", 1, nhscleargraph$select)
	
		ycoord <-seq(1,nrow(nhscleargraph))

	# Creating the plot
		nhs_clarity <- ggplot() + scale_y_continuous(limits=c( 0, 1), breaks=seq(0,1,.25), labels=seq(0,1,.25), name="CLARITY of STATEMENT") 
		nhs_clarity <- nhs_clarity + scale_x_continuous(limits=c(0,10.1), breaks=c(0:10), labels=c("0\nDECREASE\nFUNDS","1","2","3","4","5","6","7","8","9","10\nINCREASE\nFUNDS"), name="AVERAGE PLACEMENT")
		nhs_clarity <- nhs_clarity + geom_point(aes(y=nhscleargraph$clear[nhscleargraph$select==0],x=nhscleargraph$place[nhscleargraph$select==0], color=nhscleargraph$party[nhscleargraph$select==0]), size=4, shape=1, stroke = 1.25) + theme(axis.ticks.y = element_blank())
		nhs_clarity <- nhs_clarity + geom_point(aes(y=nhscleargraph$clear[nhscleargraph$select==1],x=nhscleargraph$place[nhscleargraph$select==1], color=nhscleargraph$party[nhscleargraph$select==1]), size=5)
		nhs_clarity <- nhs_clarity + theme(panel.grid.major = element_blank()) + labs(title="MEANING and CLARITY")
		nhs_clarity <- nhs_clarity + guides(color=guide_legend(title="PARTY")) + theme( panel.background = element_rect(fill = "white", colour = "grey0", size = 1 ), text = element_text(size=9) )+ scale_color_manual(values=c("#d50000","#0087dc"))
		nhs_clarity

	# Saving the plot
		ggsave(plot = nhs_clarity, filename = "NHS_nhs_clarity.pdf", width=6, height=3.75)


############ FIGURE A.5

### Top-plot IMMIGRATION Statements

	# Select coding items that capture evaluations of whether the statement is POPULAR
		immcred <- imm[, grep("(^c_|^l_).*3$", names(imm))]
	# Proportion who thinks the statement is POPULAR
		imm_popular <- apply(immcred,2,function(x) sum(x==1, na.rm=T)/sum(!is.na(x)))
	# Select coding items that capture placement attributed to meaning of the statement on the issue scale
		immmeancred <- imm[, grep("(^c_|^l_).*[a-z]$", names(imm))]
	# Compute average placements for each statement
		imm_place <- apply(immmeancred,2,function(x) mean(x, na.rm=T))

	# Putting together
		immpopulargraph <- data.frame(popular=imm_popular, place=imm_place, party=NA)
		immpopulargraph$party[grep("^c_",row.names(immpopulargraph))] <- "CONSERVATIVE"
		immpopulargraph$party[grep("^l_",row.names(immpopulargraph))] <- "LABOUR"
		immpopulargraph$party <- factor(immpopulargraph$party, levels=c("LABOUR", "CONSERVATIVE"))

	# Creating indicator of which statement was selected for each treatment condition	
		immpopulargraph$selected <- ifelse(rownames(immpopulargraph)== "c_number3" | rownames(immpopulargraph)== "c_our3", 1, 0)
		immpopulargraph$selected <- ifelse(rownames(immpopulargraph)== "l_people3" | rownames(immpopulargraph)== "l_centuri3", 1, immpopulargraph$selected)

		ycoord <-seq(1,nrow(immpopulargraph))

		imm_popularity <- ggplot() + scale_y_continuous(limits=c(0, 1), breaks=seq(0,1,.25), labels=seq(0,1,.25), name="POPULARITY") 
		imm_popularity <- imm_popularity +  scale_x_continuous(limits=c(0,10.1), breaks=c(0:10), labels=c("0\nCLOSE\nBORDERS","1","2","3","4","5","6","7","8","9","10\nOPEN\nBORDERS"), name="AVERAGE PLACEMENT")
		imm_popularity <- imm_popularity + geom_point(aes(y=immpopulargraph$popular[immpopulargraph$selected==0],x=immpopulargraph$place[immpopulargraph$selected==0], color=immpopulargraph$party[immpopulargraph$selected==0]), size=4, shape=1, stroke = 1.25) + theme(axis.ticks.y = element_blank())
		imm_popularity <- imm_popularity + geom_point(aes(y=immpopulargraph$popular[immpopulargraph$selected==1],x=immpopulargraph$place[immpopulargraph$selected==1], color=immpopulargraph$party[immpopulargraph$selected==1]), size=5)
		imm_popularity <- imm_popularity + theme(panel.grid.major = element_blank()) + labs(title="MEANING and POPULARITY - IMMIGRATION")
		imm_popularity<- imm_popularity + guides(color=guide_legend(title="PARTY")) + theme( panel.background = element_rect(fill = "white", colour = "grey0", size = 1 ), text = element_text(size=9)) + scale_color_manual(values=c("#d50000","#0087dc")) + scale_fill_manual(values=c("#d50000","#0087dc"))
		imm_popularity
		
	# Saving PLOT
		ggsave(plot = imm_popularity, filename = "IMM_credibility.pdf", width=6, height=3.75 )

### Bottom-plot NHS Statements

	# Select coding items that capture evaluations of whether the statement is POPULAR
		nhscred <- nhs[, grep("(^c_|^l_).*3$", names(nhs))]   
	# Proportion who thinks the statement is POPULAR
		nhs_popular <- apply( nhscred, 2, function(x) sum(x==1, na.rm=T)/sum(!is.na(x) ) )
	# Select coding items that capture placement attributed to meaning of the statement on the issue scale
		nhsmeancred <- nhs[, grep("(^c_|^l_).*[a-z]$", names(nhs))]
	# Compute average placements for each statement
		nhs_place <- apply(nhsmeancred,2,function(x) mean(x, na.rm=T))
	
	# Putting together
		nhspopulargraph <- data.frame(popular=nhs_popular, place=nhs_place, party=NA)
		nhspopulargraph$party[grep("^c_",row.names(nhspopulargraph))] <- "CONSERVATIVE"
		nhspopulargraph$party[grep("^l_",row.names(nhspopulargraph))] <- "LABOUR"
		nhspopulargraph$party <- factor(nhspopulargraph$party, levels=c("LABOUR", "CONSERVATIVE"))
	# Creating indicator of which statement was selected for each treatment condition
		nhspopulargraph$select <- ifelse(rownames(nhspopulargraph)== "c_goingto3" | rownames(nhspopulargraph)== "c_secure3", 1, 0)
		nhspopulargraph$select <- ifelse(rownames(nhspopulargraph)== "l_tough3" | rownames(nhspopulargraph)== "l_time3", 1, nhspopulargraph$select)

		ycoord <-seq(1,nrow(nhspopulargraph))

	# Creating the PLOT
		nhs_popularity <- ggplot() + scale_y_continuous(limits=c(0, 1), breaks=seq(0,1,.25), labels=seq(0,1,.25), name="POPULARITY") 
		nhs_popularity <- nhs_popularity + scale_x_continuous(limits=c(0,10.1), breaks=c(0:10), labels=c("0\nDECREASE\nFUNDS","1","2","3","4","5","6","7","8","9","10\nINCREASE\nFUNDS"), name="AVERAGE PLACEMENT")
		nhs_popularity <- nhs_popularity + geom_point(aes(y= nhspopulargraph$popular[nhspopulargraph$select==0],x=nhspopulargraph$place[nhspopulargraph$select==0], color=nhspopulargraph$party[nhspopulargraph$select==0]), size=4, shape=1, stroke = 1.25) + theme(axis.ticks.y = element_blank())
		nhs_popularity <- nhs_popularity + geom_point(aes(y=nhspopulargraph$popular[nhspopulargraph$select==1],x=nhspopulargraph$place[nhspopulargraph$select==1], color=nhspopulargraph$party[nhspopulargraph$select==1]), size=5)
		nhs_popularity <- nhs_popularity + theme(panel.grid.major = element_blank()) + labs(title="MEANING and POPULARITY - NHS")
		nhs_popularity <- nhs_popularity + guides(color=guide_legend(title="party")) + theme(panel.background = element_rect(fill = "white", colour = "grey0", size = 1 ), text = element_text(size=9))+ scale_color_manual(values=c("#d50000","#0087dc"))
		nhs_popularity

	# Saving PLOT
		ggsave(plot = nhs_popularity, filename = "NHS_credibility.pdf", width=6, height=3.75 )
