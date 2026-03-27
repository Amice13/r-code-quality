# Clear the global environment
rm(list = ls())

# Load required libraries
library(data.table)
library(rdrobust)
library(rddensity)
library(ggplot2)
library(here)
library(ggthemes)
library(tidyr)
library(kableExtra)
library(modelsummary)

# Load data from the file "ver_data.RData" into the R environment
load(here("data", "ver_data.RData"))

# Pre-process the data and perform some basic checks
tt <- unique(ver_b[law_enforcement == 1 & abs(marginal_muni) < 0.01, .(muni_code, year, law_enforcement, marginal_muni, elected_muni, p_elected_muni, dv_homicides, y2008, y2012, max_age)])

# Check if there is only one muni/year pair
identical(nrow(tt), nrow(unique(tt)))

# Define a sequence of values that will be used in a loop
sequence <- seq(0.001, 0.005, 0.0005)

# Create an empty data frame to store the results from the loop
dom <- data.frame(stringsAsFactors = FALSE)

# Loop through the sequence and calculate the RDD estimates for each value
for (i in sequence) {
  rd.dom <- tt[abs(tt$marginal_muni) <= i]
  domreg <- rdrobust(rd.dom$dv_homicides,
                     rd.dom$marginal_muni, vce = 'hc3', p = 1, kernel = 'triangular', h = i, bwselect = 'msetwo',
                     covs = cbind(rd.dom$y2008, rd.dom$y2012))
  tempo.a <- data.frame(coef = domreg$coef[[2]], upper = domreg$ci[[6]], lower = domreg$ci[[3]], n = domreg$N_h[[1]] + domreg$N_h[[2]], cuts = domreg$bws[[1]])
  dom <- rbind(dom, tempo.a)
}

# Calculate robust estimates using the optimal bandwidth within 0.01
bw <- rdbwselect(tt$dv_homicides, tt$marginal_muni, bwselect = 'msetwo')
sel <- mean(c(bw$bws[[3]], bw$bws[[4]])) # Robust bandwidth

robust_est <- rdrobust(tt$dv_homicides, tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008, tt$y2012))

rob_est <- data.frame(coef = robust_est$coef[[2]], upper = robust_est$ci[[6]], lower = robust_est$ci[[3]], n = robust_est$N_h[[1]] + robust_est$N_h[[2]], cuts = sel)

# Prepare data for plotting
data.plot <- data.table(rbind(dom))

# Minimum number of observations for the plot
minimal_obs <- 10

# Create the plot
plot.rdrobust <- ggplot(data.plot, aes(x = cuts, y = coef)) +
  geom_errorbar(data=data.plot[n>=20],aes(ymax = (upper), ymin = (lower)), width=0, linewidth=.75,position=position_dodge(width=0.9)) +
  geom_point(alpha=.9,size=2.5) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal() +
  scale_colour_grey() +
  theme(legend.position = "none") +
  xlab("Distance between Winning and Losing Candidates\n(as % of Valid Votes)") +
  ylab('Difference in Homicide Rate\n(Yearly deaths per 100 thousand pop.)') +
  geom_text(data = data.plot[cuts %in% c(min(data.plot[n>=20]$cuts), seq(min(data.plot[n>=20]$cuts), 0.005, 0.0001))],aes(y = 50,label = n),vjust=-.50,size=3,col='gray40') +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim=c(min(data.plot[n>=minimal_obs]$cuts),0.005)) +
  geom_segment(aes(x=0.0002,xend=0.005,y=50, yend=50),size=.05) +
  annotate('text', y = 49, x=0.0045,label = '(n. obs)',size=3, col='gray40',fontface = 'italic') +
  geom_errorbar(data = rob_est, aes(ymax = (upper), ymin = (lower)), width=0,size=.75,position=position_dodge(width=0.9), color = 'red') +
  geom_point(data = rob_est, aes(x = cuts, y = coef), alpha=.9,size=2.5, color = 'red') +
  NULL

ggsave(here("writing","img","figure_1_panel_a.pdf"), plot = plot.rdrobust, device = 'pdf',height = 10, width = 11, units = 'cm')

# Panel Figure 1, (b)

tt_a <- tt[abs(marginal_muni) < 0.0025] # zooming in

pdf(here("writing","img","figure_1_panel_c.pdf"),width = 5, height = 5)
rdplot=rdplot(tt_a$dv_homicides, tt_a$marginal_muni*100,
              kernel = 'triangular', nbins = 30,
              covs = cbind(tt_a$y2008,tt_a$y2012),
              x.label="Vote Margin (as % of Valid Votes)",
              y.label='Difference in Homicide Rate',
              title='',
              y.lim = c(-15,15),
              p = 1)
dev.off()

#FOR FIGURE B3

#pdf(here("Working papers","2015 policia","writing","img","rdplot_homicides_2.pdf"))
rdplot=rdplot(tt_a$dv_homicides, tt_a$marginal_muni*100, nbins = 30,
              kernel = 'triangular',
              covs = cbind(tt_a$y2008,tt_a$y2012),
              x.label="Vote Margin (as % of Valid Votes)",
              y.label='Difference in Homicide Rate',
              title='',
              y.lim = c(-15,15),
              p = 2)
dev.off()

#pdf(here("Working papers","2015 policia","writing","img","rdplot_homicides_3.pdf"))
rdplot=rdplot(tt_a$dv_homicides, tt_a$marginal_muni*100, nbins = 30,
              kernel = 'triangular',
              covs = cbind(tt_a$y2008,tt_a$y2012),
              x.label="Vote Margin (as % of Valid Votes)",
              y.label='Difference in Homicide Rate',
              title='',
              y.lim = c(-15,15),
              p = 3)
dev.off()

#pdf(here("Working papers","2015 policia","writing","img","rdplot_homicides_4.pdf"))
rdplot=rdplot(tt_a$dv_homicides, tt_a$marginal_muni*100, nbins = 30,
              kernel = 'triangular',
              covs = cbind(tt_a$y2008,tt_a$y2012),
              x.label="Vote Margin (as % of Valid Votes)",
              y.label='Difference in Homicide Rate',
              title='',
              y.lim = c(-15,15),
              p = 4)
dev.off()


# TABLE

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

robust_all <- rdrobust(tt$dv_homicides,tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012))

# MSERD

robust_mserd <- rdrobust(tt$dv_homicides,tt$marginal_muni, all = TRUE, bwselect = 'mserd', 
                         covs = cbind(tt$y2008,tt$y2012))
# No previous LOC

tt_a <- tt[p_elected_muni == 0]
robust_p <- rdrobust(tt_a$dv_homicides,tt_a$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                     covs = cbind(tt_a$y2008,tt_a$y2012))

# second-degree polynomial

robust_2 <- rdrobust(tt$dv_homicides,tt$marginal_muni, all = TRUE, p = 2,
                     bwselect = 'msetwo', 
                     covs = cbind(tt$y2008,tt$y2012))

# level with control
ver_b[, ap_pc := assault_period/pop]
ver_b[, ap_pc := ap_pc * I(100000/3)]
ver_b[, lap_pc := lag_assault_period_1/lag_pop]
ver_b[, lap_pc := lap_pc * I(100000/3)]

ttt <- unique(ver_b[law_enforcement ==1 & abs(marginal_muni) < 0.01, .(muni_code, year, law_enforcement, marginal_muni, elected_muni, p_elected_muni, ap_pc,lap_pc, y2008, y2012)])


robust_control <- rdrobust(ttt$ap_pc, ttt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(ttt$y2008, ttt$y2012, ttt$lap_pc))
summary(robust_control)

# Benchmark

robust_age <- rdrobust(tt$dv_homicides,tt$marginal_muni, all = TRUE, bwselect = 'msetwo', 
                       covs = cbind(tt$y2008,tt$y2012, tt$max_age))


table_loc <- list('Benchmark' = robust_all, 
                         'MSE-optimal' = robust_mserd, 
                         '2nd Polynomial' = robust_2, 
                         'No previous L&O' = robust_p,
                         'Levels with lag' = robust_control,
                         'Age as control' = robust_age)

modelsummary(table_loc, 
             statistic = c("[{std.error}]",'p.value'),
             output = 'latex',
             title = 'Effect of the election of law-and-order candidates on Homicides') %>%
  footnote(general = 'Nonparametric estimations (MSE-two selection) with year fixed effects, robust standard errors in brackets, p-values in parentheses.', threeparttable = TRUE)
