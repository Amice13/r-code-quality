#*****************************************************************************	
# ---------------- Section: Oman case study graphs ----
#*****************************************************************************

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Load packages ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls()); gc()
#library(RStudioAMI)
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

data1 <- read_excel(".../Budget data.xlsx", sheet = "graph1 for R-Stata") # Adjust path
mdf1 <- reshape2::melt(data1, id.var = "year")
data2 <- subset(data1, select=c("year", "tot_rev", "tot_exp"))
mdf2 <- reshape2::melt(data2, id.var = "year")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Code graph 1 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

showtext_auto()

p1 <- ggplot(data1) +
  		geom_bar(aes(x = year, y = tot_exp_pct_rev, fill = "Total expenditures as % of revenue"), size = 1, width = 0.6, stat = "identity") + scale_fill_manual(values="grey80") +
  		geom_line(aes(x = year, y = tot_rev, linetype="solid"), size = 1, stat = "identity", group = 1, color = "grey40") + 
  		geom_line(aes(x = year, y = tot_exp, linetype="dotted"), size = 1, stat = "identity", group = 1, color = "grey40") + 
  		scale_y_continuous(name = "Percent", sec.axis = sec_axis(~ . * 100 / 150 , name = "Saidi Rials (mil.)"), limits = c(0, 150)) +
  		scale_x_discrete(name = "", labels=c("a" = "1963", "b" = "1969", "c" = "1970", "d" = "1971", "e" = "1972", "f" = "1973")) +
  		theme_bw() +
  		guides(linetype = guide_legend(nrow = 3, byrow = TRUE)) + 
  		scale_linetype_identity(guide = "legend", breaks = c("solid", "dotted"),
  			labels = c("Total revenue (mil. Rials)", "Total expenditure (mil. Rials)")) + 
  		theme(legend.key=element_blank(), legend.title=element_blank(), legend.box="horizontal", legend.position = "bottom") + 
		theme(axis.text=element_text(size=rel(3.1)),
		  	    axis.title=element_text(size=rel(3.1)),
		  		plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
		  		panel.border = element_blank(),
		  		axis.line = element_line(color ="black"),
		  		axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
		  		axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
		  		axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
		  		axis.text.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0)),
		  		legend.text=element_text(size=rel(3.1)),
		  		text=element_text(family="lat"))

showtext_auto()
pdf("Oman spending fig. 1.pdf",width=16,height=12)
print(p1)
dev.off()

showtext_auto()

p2 <- ggplot(mdf2, aes(year, value)) + 
        geom_bar(aes(fill=variable), stat = "identity", position = "dodge", size = 1, width = 0.6) + 
        scale_x_discrete(name = "", labels=c("a" = "1963", "b" = "1969", "c" = "1970", "d" = "1971", "e" = "1972", "f" = "1973")) +
        scale_fill_manual(values=c("grey80", "grey50"), labels = c("Total revenue", "Total expenditure")) +
        theme_bw() +
        theme(legend.key=element_blank(), legend.title=element_blank(), legend.box="horizontal", legend.position = "bottom") + 
        labs(y="Saidi Rials (millions)") +
        theme(axis.text=element_text(size=rel(3.1)),
		  	    axis.title=element_text(size=rel(3.1)),
		  		plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
		  		panel.border = element_blank(),
		  		axis.line = element_line(color ="black"),
		  		axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
		  		axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
		  		axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
		  		axis.text.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
		  		legend.text=element_text(size=rel(3.1)), legend.spacing.x = unit(0.5, "cm"),
		  		text=element_text(family="lat"))

showtext_auto()
pdf("Oman spending fig. 1.pdf",width=16,height=12)
print(p2)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Load data graph 2 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls()); gc()
data1 <- read_excel(".../Budget data.xlsx", sheet = "graph2 for R-Stata") # Adjust path
mdf1 <- reshape2::melt(data1, id.var = "year")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
# ---- Subsection: Code graph 2 ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

showtext_auto()

p2 <- ggplot(mdf1, aes(year, value)) + 
        geom_bar(aes(fill=variable), stat = "identity", position = "dodge", size = 1, width = 0.6) + 
        scale_x_discrete(name = "", labels=c("a" = "1963", "b" = "1968", "c" = "1971", "d" = "1972", "e" = "1973")) +
        scale_fill_manual(values=c("grey80", "grey60", "grey40"), labels = c("Security spending", "Development spending", "Social spending")) +
        theme_bw() +
        theme(legend.key=element_blank(), legend.title=element_blank(), legend.box="vertical", legend.position = "bottom") + 
        guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
        labs(y="Percent of total expenditure") +
        theme(axis.text=element_text(size=rel(3.1)),
		  	    axis.title=element_text(size=rel(3.1)),
		  		plot.margin=unit(c(1.2,1.2,1.2,1.2),"cm"),
		  		panel.border = element_blank(),
		  		axis.line = element_line(color ="black"),
		  		axis.title.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
		  		axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
		  		axis.text.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0)),
		  		axis.text.y = element_text(margin = margin(t = 0, r = 18, b = 0, l = 0)),
		  		legend.text=element_text(size=rel(3.1)), legend.spacing.x = unit(0.5, "cm"),
		  		text=element_text(family="lat"))

showtext_auto()
pdf("Oman spending fig. 2.pdf",width=16,height=12)
print(p2)
dev.off()


