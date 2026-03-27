rm(list = ls())
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(here)
library(ggthemes)

load(here("data","ver_data.RData"))

tt <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni) < 0.01, .(muni_code, year, law_enforcement, marginal_muni, elected_muni, p_elected_muni, dv_homicides_lagged, y2008, y2012)])

# check if there is only one muni/year pair
  identical(nrow(tt),nrow(unique(tt)))

#loop parameters
  sequence=seq(0.001, 0.005, 0.0005)

#The data.frame for 
  dom <- data.frame(stringsAsFactors=FALSE)

for(i in sequence){
  rd.dom <- tt[abs(tt$marginal_muni) <= i]
  domreg <- rdrobust(rd.dom$dv_homicides_lagged,
                     rd.dom$marginal_muni, vce='hc3', p=1, kernel = 'triangular',h = i,bwselect = 'msetwo',
                     covs = cbind(rd.dom$y2008,rd.dom$y2012))
  tempo.a <- data.frame(coef = domreg$coef[[2]], upper = domreg$ci[[6]], lower = domreg$ci[[3]],n = domreg$N_h[[1]] + domreg$N_h[[2]], cuts = domreg$bws[[1]])
  dom = rbind(dom,tempo.a)
}

# add robust
#find out the optimal bandwidth within 0.01
bw <- rdbwselect(tt$dv_homicides_lagged,tt$marginal_muni, bwselect = 'msetwo')
sel = mean(c(bw$bws[[3]], bw$bws[[4]])) # robust bandwidth

robust_est <- rdrobust(tt$dv_homicides_lagged,tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))
rob_est <- data.frame(coef = robust_est$coef[[2]], upper = robust_est$ci[[6]], lower = robust_est$ci[[3]],n = robust_est$N_h[[1]] + robust_est$N_h[[2]], cuts = sel)

data.plot <- data.table(rbind(dom))

# Plot
minimal_obs = 10

plot.rdrobust <- ggplot(data.plot, aes(x = cuts, y = coef)) +
  geom_errorbar(data=data.plot[n>=20],aes(ymax = (upper), ymin = (lower)), width=0,linewidth=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal() +
  scale_colour_grey() +
  theme(legend.position = "none") +
  xlab("Distance Between Winning and Losing Candidates\n(as % of Valid Votes)") +
  ylab('Difference in Homicide Rate\n(Yearly deaths per 100 thousand pop.)') +
  geom_text(data = data.plot[cuts %in% c(min(data.plot[n>=20]$cuts), seq(min(data.plot[n>=20]$cuts), 0.005, 0.0001))],aes(y = 100,label = n),vjust=-.50,size=3,col='gray40') +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim=c(min(data.plot[n>=minimal_obs]$cuts),0.005)) +
  geom_segment(aes(x=0.0002,xend=0.005,y=100, yend=100),size=.05) +
  annotate('text', y = 99, x=0.0045,label = '(n. obs)',size=3, col='gray40',fontface = 'italic') +
  geom_errorbar(data = rob_est, aes(ymax = (upper), ymin = (lower)), width=0,size=.75,position=position_dodge(width=0.9), color = 'red') +
  geom_point(data = rob_est, aes(x = cuts, y = coef), alpha=.9,size=2.5, color = 'red') +
  NULL

plot.rdrobust
#ggsave(here("writing","img","fig_A5_panel_a.pdf"), plot = plot.rdrobust, device = 'pdf',height = 10, width = 10, units = 'cm')

tt_a <- tt[abs(marginal_muni) < 0.0025] # zooming in

pdf(here("writing","img","fig_A5_panel_c.pdf"))
rdplot=rdplot(tt_a$dv_homicides_lagged, tt_a$marginal_muni*100,
              kernel = 'triangular', nbins = 30,
              covs = cbind(tt_a$y2008,tt_a$y2012),
              x.label="Vote Margin (as % of Valid Votes)",
              y.label='Difference in Homicide Rate',
              title='',
              y.lim = c(-15,15),
              p = 1)
dev.off()


# TABLE
#packages for the tables
library(tidyr)
library(kableExtra)
library(modelsummary)
# Load functions for modelsummary 

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = 'Robust Coef.',
    estimate = model$coef[3, 1],
    std.error = model$se[3, 1],
    p.value = model$pv[3, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    #Kernel = model$kernel,
    Bandwidth = paste(round(model$bws[2,2],4) * 100,'%'),
    #Bw.Selection = model$bwselect,
    N.obs = as.character(model$N_h[[1]] + model$N_h[[2]])
  )
  ret
}
# Benchmark

robust_all <- rdrobust(tt$dv_homicides_lagged,tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))

# MSERD

robust_mserd <- rdrobust(tt$dv_homicides_lagged,tt$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
# No previous LOC

tt_a <- tt[p_elected_muni == 0]
robust_p <- rdrobust(tt_a$dv_homicides_lagged,tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008,tt_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt$dv_homicides_lagged,tt$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))


table_loc <- list('Benchmark' = robust_all
                  #'MSE-optimal' = robust_mserd, 
                  #'2nd Polynomial' = robust_2, 
                  #'No previous L&O' = robust_p
                  )

modelsummary(table_loc, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on PAST Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection, unless noted) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)

