rm(list=ls())
setwd("") ## set working directory

##### Figure 1 #####
rm(list=ls())

library(maps)
library(rgeos)
library(mapdata)
library(maptools)

load("PovertyJJPS_Map.RData")

### Fiscal alleviation
fis = xinjiang@data$fis
county = xinjiang@data$admin_ID
brks = quantile(fis, seq(0,1,0.25), na.rm=TRUE); brks
cols = c("steelblue1", "steelblue2", "steelblue3", "steelblue4"); cols
dens = (2:length(brks))*3

pdf('fig1a.pdf', width = 5, height = 5)
plot(xinjiang, border="grey", col=cols[findInterval(fis, brks, all.inside=TRUE)])
plot(xinjiang2, border="black", lwd=1, add = TRUE)
legend("topleft", legend=c(paste0(0, "-", round(brks,0)[2], " RMB"),
                           paste0(round(brks,0)[2], "-", round(brks,0)[3], " RMB"),
                           paste0(round(brks,0)[3], "-", round(brks,0)[4], " RMB"),
                           paste0(round(brks,0)[4], "-", round(brks,0)[5], " RMB")),
       bty="n", inset=0.05,
       lty=c(1,1,1,1,1), lwd=c(5,5,5,5,5), cex=0.6,
       col=cols)
dev.off()

### Work for relief
work = xinjiang@data$work
county = xinjiang@data$admin_ID
brks = quantile(work, seq(0,1,0.25), na.rm=TRUE); brks
cols = c("steelblue1", "steelblue2", "steelblue3", "steelblue4"); cols
dens = (2:length(brks))*3

pdf('fig1b.pdf', width = 5, height = 5)
plot(xinjiang, border="grey", col=cols[findInterval(work, brks, all.inside=TRUE)])
plot(xinjiang2, border="black", lwd=1, add = TRUE)
legend("topleft", legend=c(paste0(0, "-", round(brks,0)[2], " RMB"),
                           paste0(round(brks,0)[2], "-", round(brks,0)[3], " RMB"),
                           paste0(round(brks,0)[3], "-", round(brks,0)[4], " RMB"),
                           paste0(round(brks,0)[4], "-", round(brks,0)[5], " RMB")),
       bty="n", inset=0.05,
       lty=c(1,1,1,1,1), lwd=c(5,5,5,5,5), cex=0.6,
       col=cols)
dev.off()

##### Figure 2 #####
rm(list=ls())

library(tidyverse)
X = readRDS("PovertyJJPS_Final.RData")
fig2 = X %>%
  filter(year >= 1994 & year < 2001) %>%
  group_by(year) %>%
  summarise(Fiscal = sum(fiscal_allpc_raw, na.rm=T),
            Work = sum(wk_allpc_raw, na.rm=T))

fig2 = t(as.matrix(fig2[,2:3]))
colnames(fig2) = 1994:2000

pdf('fig2.pdf', width=7, height=5)
opar=par()
par(cex=1.2, xpd=T, mar=c(5,4,4,5)+0.1)
barplot(fig2,
        col=c("lightgreen", "darkgreen"),
        ylab="Total Per Capita Amount of Relief Transfers (RMB)",
        xlab="Year")
legend(x=8.5, y=4000,
       legend=c("Work", "Fiscal"),
       fill=c("darkgreen", "lightgreen"), bty="n")
dev.off()

##### Figure 3 #####
rm(list=ls())

library(tidyverse)
X = readRDS("PovertyJJPS_Final.RData")
fig3 = X %>%
  dplyr::select(year, riot_raw) %>%
  filter(year >= 1994 & year < 2001) 

fig3_res = tapply(fig3$riot_raw, fig3$year, sum, na.rm=T)

pdf('fig3.pdf', width=7, height=5)
plot(names(fig3_res), fig3_res, type="l", xlab="Year", ylab="Number of Counties Seen Violence")
dev.off()

##### Tables A1 and A2 #####
rm(list=ls())

library(tidyverse)
X = readRDS("PovertyJJPS_Final.RData") %>%
  filter(year > 1994 & year <= 2001)

main = data.frame(
  fiscal_allpc = X$lg_fiscal_allpc,
  wk_allpc = X$lg_wk_allpc,
  secgrow = X$secgrow,
  admgrow = X$admgrow,
  agrow = X$agrow,
  taxgrow = X$taxgrow,
  conflict = ifelse(X$riot_raw >= 1, 1, 0),
  lg_gdppc = X$lg_gdppc,
  lg_sub = X$lg_sub,
  lg_grow = X$lg_grow,
  s_uyghur = X$s_uyghur,
  density = X$density
)

colnames(main) = c("Fiscal assistance (log)",
                   "Work-for-relief (log)",
                   "Change in per capita security spending",
                   "Change in per capita admin spending",
                   "Change in per capita agricultural production",
                   "Change in per capita local fiscal revenue",
                   "Violence (=1)",
                   "Lagged GDP per capita (log)",
                   "Lagged fiscal dependence (percent)",
                   "Lagged economic growth rate (percent)",
                   "Share of Uyghur (percent)",
                   "Population density (log)")

library(stargazer)
stargazer(main, summary=T, omit.summary.stat=c("p25", "p75")) # Table A1

main.cor = cor(main, use="pairwise.complete.obs")
main.cor[upper.tri(main.cor, diag=F) == T] = NA
colnames(main.cor) = paste("(", 1:ncol(main), ")", sep="")
rownames(main.cor) = paste(colnames(main.cor), rownames(main.cor), sep=" ")

library(stargazer)
stargazer(main.cor, digits=2) # Table A2
