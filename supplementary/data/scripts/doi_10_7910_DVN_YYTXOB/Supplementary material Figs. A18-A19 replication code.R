#*****************************************************************************	
# ---------------- Section: Spending descriptives graphs ----
#*****************************************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Load packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls()); gc()
library(foreign)
library(haven)
library(gsynth)
library(panelView)
library(devtools)
library(ggplot2)
library(lmtest)
library(sandwich)
library(fpp)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(showtext)
font_add("lat", "lmroman12-regular.otf")
library(MASS)
library(pcse)
library(haven)
library(simcf)
library(plm)
library(corpcor)
library(readxl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Load data graph 1 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Note: the aggregate descriptive data graphed below are generated in the "A6 spending descriptives extraction" STATA do file

data1 <- read_excel(".../spending descriptives.xlsx", sheet = "public order for R-Stata") # Adjust path 
data1$subgroup <- factor(data1$subgroup, levels=c("pre", "post"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Code graph 1 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
positions <- c("centre-seeking oil (7)", "centre-seeking non-oil (3)", "separatist oil (3)", "separatist non-oil (1)")

p1 <- ggplot(data1,                                      
	       aes(x = group,
	           y = values,
	           fill = subgroup)) +
	  		geom_bar(stat = "identity",
	           position = "dodge", 
	           size = 1, width = 0.4) +
	  		scale_fill_manual(values=c("grey80", "grey40")) +
		  	theme_bw() +
		  	scale_y_continuous(name = "Spending on public order p.c. (log)") +
		  	coord_cartesian(ylim = c(3, 4.8)) +
		  	scale_x_discrete(breaks=unique(data1$group), 
		  					name = NULL, 
    						labels=c("centre-seeking \noil (7)", 
                        	"centre-seeking \nnon-oil (3)", "separatist \noil (3)", "separatist \nnon-oil (1)"),
                        	limits = positions) +
		  	theme(legend.key=element_blank(), legend.title=element_blank(), legend.box="horizontal", legend.position = "bottom", legend.spacing.x = unit(0.4, 'cm')) +
			theme(axis.text=element_text(size=rel(3.1)),
			  	    axis.title=element_text(size=rel(3.1)),
			  		plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
			  		panel.border = element_blank(),
			  		axis.line = element_line(color ="black"),
			  		axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
			  		axis.title.x = element_text(margin = margin(t = 25, r = 0, b = 0, l = 0)),
			  		axis.text.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
			  		axis.text.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0)),
			  		legend.text=element_text(size=rel(3.1)),
			  		text=element_text(family="lat"))

showtext_auto()
pdf("Public order descriptives.pdf",width=16,height=12)
print(p1)
dev.off()
showtext_auto()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Load data graph 2 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data2 <- read_excel(".../spending descriptives.xlsx", sheet = "subsidies for R-Stata") # Adjust path
data2$subgroup <- factor(data2$subgroup, levels=c("pre", "post"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Code graph 2 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
positions <- c("centre-seeking oil (8)", "centre-seeking non-oil (4)", "separatist oil (4)", "separatist non-oil (1)")

p2 <- ggplot(data2,                                      
	       aes(x = group,
	           y = values,
	           fill = subgroup)) +
	  		geom_bar(stat = "identity",
	           position = "dodge", 
	           size = 1, width = 0.4) +
	  		scale_fill_manual(values=c("grey80", "grey40")) +
		  	theme_bw() +
		  	scale_y_continuous(name = "Spending on subsidies & tranfers p.c. (log)", n.breaks=7, labels=c("2.5", "3", "3.5", "4", "4.5", "5", "5.5")) +
		  	coord_cartesian(ylim = c(2.5, 5.5)) +
		  	scale_x_discrete(breaks=unique(data2$group), 
		  					name = NULL, 
    						labels=c("centre-seeking \noil (8)", 
                        	"centre-seeking \nnon-oil (4)", "separatist \noil (4)", "separatist \nnon-oil (1)"),
                        	limits = positions) +
		  	theme(legend.key=element_blank(), legend.title=element_blank(), legend.box="horizontal", legend.position = "bottom", legend.spacing.x = unit(0.4, 'cm')) +
			theme(axis.text=element_text(size=rel(3.1)),
			  	    axis.title=element_text(size=rel(3.1)),
			  		plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
			  		panel.border = element_blank(),
			  		axis.line = element_line(color ="black"),
			  		axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
			  		axis.title.x = element_text(margin = margin(t = 25, r = 0, b = 0, l = 0)),
			  		axis.text.x = element_text(margin = margin(t = 12, r = 0, b = 0, l = 0)),
			  		axis.text.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0)),
			  		legend.text=element_text(size=rel(3.1)),
			  		text=element_text(family="lat"))

showtext_auto()
pdf("Subsidies descriptives.pdf",width=16,height=12)
print(p2)
dev.off()
showtext_auto()

