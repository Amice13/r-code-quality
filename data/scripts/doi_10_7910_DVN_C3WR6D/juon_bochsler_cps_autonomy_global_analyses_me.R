############################################################################################################################
########The wrong place at the wrong time? Territorial autonomy and conflict during regime transitions########
#####Global analyses (marginal effects plots)#####
#####Andreas Juon & Daniel Bochsler 2023 (Comparative Political Studies)#####
############################################################################################################################

###Note: run file "juon_bochsler_cps_autonomy_global_analyses.R" before running this file
###Note: this file takes a long time to run; to increase speed, lower number of bootstrap iterations 

####################################################################################
#########1. Libraries#########
####################################################################################

#######1.1 Libraries#######
library(boot)
library(tidyr)
library(fastDummies)
library(glm.predict)
library(ggplot2)
library(dotwhisker)

#######1.2 Functions and definitions#######
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  invisible(combined)
}

time <- seq(0,10,length.out = 11)

name <- character()
for (i in time) {
  step1 <- paste("name <- c(name, 'effect_t",i,"')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name <- c(name, 'effect_t",i,"_diff')", sep="")
  eval(parse(text=c(step1)))
}

#######1.3 Setting up data#######
df_global <- dummy_cols(df_global, select_columns =c("region"))#replace factor variable region with region dummies
df_global <- dummy_cols(df_global, select_columns =c("gwgroupid"))#generate group ID dummies for group-FE model
gwgroupid_levels <- colnames(df_global)[grepl("gwgroupid_",colnames(df_global))]#list of group ID dummies
df_global$tt_vdem2_shock <- 1 * 0.5^((df_global$tt_vdem2_durability) / 3)#calculate again to prevent rounding errors
df_global$tt_vdem2_dem_shock <- 1 * 0.5^((df_global$tt_vdem2_dem_durability) / 3)#calculate again to prevent rounding errors
df_global$tt_vdem2_aut_shock <- 1 * 0.5^((df_global$tt_vdem2_aut_durability) / 3)#calculate again to prevent rounding errors
df_global$tt_vdem2_st_shock <- 1 * 0.5^((df_global$tt_vdem2_st_durability) / 3)#calculate again to prevent rounding errors
subset_data <- subset(df_global, sd_l1 == 0)
subset_data2 <- subset(df_global, violsd_l1 == 0)


####################################################################################
#########2. Main models#########
####################################################################################

###a) sr1_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr1_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),control = list(maxit = 10),na.action = na.exclude)
  prediction <- predict(sr1_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr1_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
sr1_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr1_vdem2_sd_boot))
sr1_vdem2_sd_boot2 <- cbind(sr1_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr1_vdem2_sd_boot2$model <- as.factor(sr1_vdem2_sd_boot2$model)
sr1_vdem2_sd_boot2$model = factor(sr1_vdem2_sd_boot2$model,levels(sr1_vdem2_sd_boot2$model)[c(3,1,2)])
sr1_vdem2_sd_boot2$term <- "SDM onset"

###b) sr1_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr1_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),control = list(maxit = 10),na.action = na.exclude)
  prediction <- predict(sr1_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr1_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
sr1_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr1_vdem2_violsd_boot))
sr1_vdem2_violsd_boot2 <- cbind(sr1_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr1_vdem2_violsd_boot2$model <- as.factor(sr1_vdem2_violsd_boot2$model)
sr1_vdem2_violsd_boot2$model = factor(sr1_vdem2_violsd_boot2$model,levels(sr1_vdem2_violsd_boot2$model)[c(3,1,2)])
sr1_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(sr1_vdem2_sd_boot2, sr1_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.96*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.96*  binary_plot_data$bootSE
binary_plot <- dwplot(binary_plot_data,
                            vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                            dot_args = list(aes(shape = model, colour = model)), 
                            whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02, 0.03))

###d) srg1_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg1_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),control = list(maxit = 10),na.action = na.exclude)
  prediction <- predict(srg1_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg1_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
name <- character()
for (i in time) {
  step1 <- paste("name <- c(name, 'effect_t",i,"')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name <- c(name, 'effect_t",i,"_diff')", sep="")
  eval(parse(text=c(step1)))
}
srg1_vdem2_sd_boot2 <- cbind(type = name, summary(srg1_vdem2_sd_boot))
srg1_vdem2_sd_boot2 <- cbind(srg1_vdem2_sd_boot2, time = rep(seq(0,10),2))
srg1_vdem2_sd_boot2 <- cbind(srg1_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg1_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg1_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg1_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),control = list(maxit = 10),na.action = na.exclude)
  prediction <- predict(srg1_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg1_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
srg1_vdem2_violsd_boot2 <- cbind(type = name, summary(srg1_vdem2_violsd_boot))
srg1_vdem2_violsd_boot2 <- cbind(srg1_vdem2_violsd_boot2, time = rep(seq(0,10),2))
srg1_vdem2_violsd_boot2 <- cbind(srg1_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg1_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(srg1_vdem2_sd_boot2, srg1_vdem2_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.96*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.96*  continuous_plot_data$bootSE
continuous_plot_data$model <- as.factor(continuous_plot_data$model)
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.96* bootSE, ymax = original + 1.96*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.02, 0.04, 0.06))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.96* bootSE, ymax = original + 1.96*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) combine plots into figure
figure2 <- grid.arrange(binary_plot, continuous_plot, heights = c(2/5,3/5))


####################################################################################
#########3. Other conditional factors (appendix B.1)#########
####################################################################################

##########3.1 Autonomy and government inclusion##########

###a) sr7a_vdem2_sd
#autonomy without inclusion
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 1,"autonomy1_inc1" = 0,"autonomy0_inc1" = 0), cbind(subset(subset_data, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 0,"autonomy0_inc1" = 0,"autonomy1_inc1" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7a_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy1_inc0","autonomy1_inc1","autonomy0_inc1", "autonomy1_inc0:d5_tt_vdem2","autonomy1_inc1:d5_tt_vdem2","autonomy0_inc1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7a_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_inc0 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_inc0 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_inc0 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_inc0 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7a_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
sr7a_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7a_vdem2_sd_boot))
sr7a_vdem2_sd_boot2 <- cbind(sr7a_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7a_vdem2_sd_boot2$model <- as.factor(sr7a_vdem2_sd_boot2$model)
sr7a_vdem2_sd_boot2$model = factor(sr7a_vdem2_sd_boot2$model,levels(sr7a_vdem2_sd_boot2$model)[c(3,1,2)])
sr7a_vdem2_sd_boot2$term <- "SDM onset"
#autonomy with inclusion
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 1,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0), cbind(subset(subset_data, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 0,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7a_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy1_inc0","autonomy1_inc1","autonomy0_inc1", "autonomy1_inc0:d5_tt_vdem2","autonomy1_inc1:d5_tt_vdem2","autonomy0_inc1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7a_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_inc1 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_inc1 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_inc1 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_inc1 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7a2_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
sr7a2_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7a2_vdem2_sd_boot))
sr7a2_vdem2_sd_boot2 <- cbind(sr7a2_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7a2_vdem2_sd_boot2$model <- as.factor(sr7a2_vdem2_sd_boot2$model)
sr7a2_vdem2_sd_boot2$model = factor(sr7a2_vdem2_sd_boot2$model,levels(sr7a2_vdem2_sd_boot2$model)[c(3,1,2)])
sr7a2_vdem2_sd_boot2$term <- "SDM onset"

###b) sr7a_vdem2_violsd
#autonomy without inclusion
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 1,"autonomy1_inc1" = 0,"autonomy0_inc1" = 0), cbind(subset(subset_data2, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 0,"autonomy0_inc1" = 0,"autonomy1_inc1" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7a_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy1_inc0","autonomy1_inc1","autonomy0_inc1", "autonomy1_inc0:d5_tt_vdem2","autonomy1_inc1:d5_tt_vdem2","autonomy0_inc1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7a_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_inc0 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_inc0 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_inc0 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_inc0 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7a_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
sr7a_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7a_vdem2_violsd_boot))
sr7a_vdem2_violsd_boot2 <- cbind(sr7a_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7a_vdem2_violsd_boot2$model <- as.factor(sr7a_vdem2_violsd_boot2$model)
sr7a_vdem2_violsd_boot2$model = factor(sr7a_vdem2_violsd_boot2$model,levels(sr7a_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7a_vdem2_violsd_boot2$term <- "Violent SDM onset"
#autonomy with inclusion
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 1,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0), cbind(subset(subset_data2, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 0,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7a_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy1_inc0","autonomy1_inc1","autonomy0_inc1", "autonomy1_inc0:d5_tt_vdem2","autonomy1_inc1:d5_tt_vdem2","autonomy0_inc1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7a_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_inc1 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_inc1 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_inc1 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_inc1 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7a2_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
sr7a2_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7a2_vdem2_violsd_boot))
sr7a2_vdem2_violsd_boot2 <- cbind(sr7a2_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7a2_vdem2_violsd_boot2$model <- as.factor(sr7a2_vdem2_violsd_boot2$model)
sr7a2_vdem2_violsd_boot2$model = factor(sr7a2_vdem2_violsd_boot2$model,levels(sr7a2_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7a2_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
#autonomy without inclusion
binary_plot1_data <- rbind(sr7a_vdem2_sd_boot2, sr7a_vdem2_violsd_boot2)
binary_plot1_data$estimate <- binary_plot1_data$original
binary_plot1_data$conf.low <- binary_plot1_data$original - 1.645*  binary_plot1_data$bootSE
binary_plot1_data$conf.high <- binary_plot1_data$original + 1.645*  binary_plot1_data$bootSE
binary_plot1 <- dwplot(binary_plot1_data,
                       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                       dot_args = list(aes(shape = model, colour = model)), 
                       whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, not included, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.02, 0, 0.02, 0.04, 0.06))
#autonomy with inclusion
binary_plot2_data <- rbind(sr7a2_vdem2_sd_boot2, sr7a2_vdem2_violsd_boot2)
binary_plot2_data$estimate <- binary_plot2_data$original
binary_plot2_data$conf.low <- binary_plot2_data$original - 1.645*  binary_plot2_data$bootSE
binary_plot2_data$conf.high <- binary_plot2_data$original + 1.645*  binary_plot2_data$bootSE
binary_plot2 <- dwplot(binary_plot2_data,
                       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                       dot_args = list(aes(shape = model, colour = model)), 
                       whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, included, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.02, 0, 0.02))

###d) srg7a_vdem2_sd
#autonomy without inclusion
time <- seq(0,10,length.out = 11)
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 1,"autonomy1_inc1" = 0,"autonomy0_inc1" = 0), cbind(subset(subset_data, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 0,"autonomy0_inc1" = 0,"autonomy1_inc1" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7a_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy1_inc0","tt_vdem2_shock*autonomy1_inc1","tt_vdem2_shock*autonomy0_inc1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7a_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_inc0 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_inc0 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7a_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
srg7a_vdem2_sd_boot2 <- cbind(summary(srg7a_vdem2_sd_boot), time = rep(seq(0,10),2))
srg7a_vdem2_sd_boot2 <- cbind(srg7a_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7a_vdem2_sd_boot2$outcome <- "SDM onset"
#autonomy with inclusion
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 1,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0), cbind(subset(subset_data, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 0,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7a_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy1_inc0","tt_vdem2_shock*autonomy1_inc1","tt_vdem2_shock*autonomy0_inc1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7a_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_inc1 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_inc1 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7a2_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
srg7a2_vdem2_sd_boot2 <- cbind(summary(srg7a2_vdem2_sd_boot), time = rep(seq(0,10),2))
srg7a2_vdem2_sd_boot2 <- cbind(srg7a2_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7a2_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg7a_vdem2_violsd
#autonomy without inclusion
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 1,"autonomy1_inc1" = 0,"autonomy0_inc1" = 0), cbind(subset(subset_data2, select=-c(autonomy1_inc0,autonomy1_inc1,autonomy0_inc1)),"autonomy1_inc0" = 0,"autonomy0_inc1" = 0,"autonomy1_inc1" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7a_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy1_inc0","tt_vdem2_shock*autonomy1_inc1","tt_vdem2_shock*autonomy0_inc1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7a_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_inc0 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_inc0 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7a_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
srg7a_vdem2_violsd_boot2 <- cbind(summary(srg7a_vdem2_violsd_boot), time = rep(seq(0,10),2))
srg7a_vdem2_violsd_boot2 <- cbind(srg7a_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7a_vdem2_violsd_boot2$outcome <- "Violent SDM onset"
#autonomy with inclusion
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 1,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0), cbind(subset(subset_data2, select=-c(autonomy1_inc1,autonomy0_inc1,autonomy1_inc0)),"autonomy1_inc1" = 0,"autonomy0_inc1" = 0,"autonomy1_inc0" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7a_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy1_inc0","tt_vdem2_shock*autonomy1_inc1","tt_vdem2_shock*autonomy0_inc1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA","state_control","size","log(distance_border+0.00001)","tek_sup_irre_any","tek_sdm","oil_area_pct",country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7a_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_inc1 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_inc1 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7a2_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
srg7a2_vdem2_violsd_boot2 <- cbind(summary(srg7a2_vdem2_violsd_boot), time = rep(seq(0,10),2))
srg7a2_vdem2_violsd_boot2 <- cbind(srg7a2_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7a2_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
#autonomy without inclusion
continuous_plot1_data <- rbind(srg7a_vdem2_sd_boot2, srg7a_vdem2_violsd_boot2)
continuous_plot1_data$estimate <- continuous_plot1_data$original
continuous_plot1_data$conf.low <- continuous_plot1_data$original - 1.645*  continuous_plot1_data$bootSE
continuous_plot1_data$conf.high <- continuous_plot1_data$original + 1.645*  continuous_plot1_data$bootSE
continuous_plot1_data$model <- as.factor(continuous_plot1_data$model)
continuous_plot11 <- ggplot(data = subset(continuous_plot1_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy, not included, on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.02, 0.04, 0.06, 0.08))
continuous_plot12 <- ggplot(data = subset(continuous_plot1_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy, not included, on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.01, 0, 0.01, 0.02))
continuous_plot1 <- grid_arrange_shared_legend(continuous_plot11, continuous_plot12, ncol=2, nrow=1)
#autonomy with inclusion
continuous_plot2_data <- rbind(srg7a2_vdem2_sd_boot2, srg7a2_vdem2_violsd_boot2)
continuous_plot2_data$estimate <- continuous_plot2_data$original
continuous_plot2_data$conf.low <- continuous_plot2_data$original - 1.645*  continuous_plot2_data$bootSE
continuous_plot2_data$conf.high <- continuous_plot2_data$original + 1.645*  continuous_plot2_data$bootSE
continuous_plot2_data$model <- as.factor(continuous_plot2_data$model)
continuous_plot21 <- ggplot(data = subset(continuous_plot2_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy, included, on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.03, -0.02, -0.01, 0, 0.01))
continuous_plot22 <- ggplot(data = subset(continuous_plot2_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy, included, on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.06, -0.03, 0, 0.03, 0.06))
continuous_plot2 <- grid_arrange_shared_legend(continuous_plot21, continuous_plot22, ncol=2, nrow=1)

###g) combine all plots
figurea1 <- grid.arrange(binary_plot1, continuous_plot1, heights = c(2/5,3/5))
figurea2 <- grid.arrange(binary_plot2, continuous_plot2, heights = c(2/5,3/5))


##########3.2 Autonomy and prior SD violence##########
###a) sr7b_vdem2_sd
#autonomy no prior violence
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv0" = 1, "autonomy1_prevv1" = 0), cbind(subset(subset_data, select=-c(autonomy1_prevv0, autonomy1_prevv1)),"autonomy1_prevv0" = 0, "autonomy1_prevv1" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7b_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy1_prevv0","autonomy1_prevv1", "autonomy1_prevv0:d5_tt_vdem2","autonomy1_prevv1:d5_tt_vdem2","violsd_prev_l1","violsd_prev_l1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7b_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_prevv0 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_prevv0 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_prevv0 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_prevv0 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7b_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
sr7b_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7b_vdem2_sd_boot))
sr7b_vdem2_sd_boot2 <- cbind(sr7b_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7b_vdem2_sd_boot2$model <- as.factor(sr7b_vdem2_sd_boot2$model)
sr7b_vdem2_sd_boot2$model = factor(sr7b_vdem2_sd_boot2$model,levels(sr7b_vdem2_sd_boot2$model)[c(3,1,2)])
sr7b_vdem2_sd_boot2$term <- "SDM onset"
#autonomy prior violence
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv1" = 1, "autonomy1_prevv0" = 0), cbind(subset(subset_data, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv1" = 0, "autonomy1_prevv0" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7b_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy1_prevv0","autonomy1_prevv1", "autonomy1_prevv0:d5_tt_vdem2","autonomy1_prevv1:d5_tt_vdem2","violsd_prev_l1","violsd_prev_l1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7b_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_prevv1 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_prevv1 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_prevv1 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_prevv1 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7b2_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
sr7b2_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7b2_vdem2_sd_boot))
sr7b2_vdem2_sd_boot2 <- cbind(sr7b2_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7b2_vdem2_sd_boot2$model <- as.factor(sr7b2_vdem2_sd_boot2$model)
sr7b2_vdem2_sd_boot2$model = factor(sr7b2_vdem2_sd_boot2$model,levels(sr7b2_vdem2_sd_boot2$model)[c(3,1,2)])
sr7b2_vdem2_sd_boot2$term <- "SDM onset"

###b) sr7b_vdem2_violsd
#autonomy no prior violence
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv0" = 1, "autonomy1_prevv1" = 0), cbind(subset(subset_data2, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv0" = 0, "autonomy1_prevv1" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7b_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy1_prevv0","autonomy1_prevv1", "autonomy1_prevv0:d5_tt_vdem2","autonomy1_prevv1:d5_tt_vdem2","violsd_prev_l1","violsd_prev_l1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7b_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_prevv0 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_prevv0 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_prevv0 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_prevv0 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7b_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
sr7b_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7b_vdem2_violsd_boot))
sr7b_vdem2_violsd_boot2 <- cbind(sr7b_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7b_vdem2_violsd_boot2$model <- as.factor(sr7b_vdem2_violsd_boot2$model)
sr7b_vdem2_violsd_boot2$model = factor(sr7b_vdem2_violsd_boot2$model,levels(sr7b_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7b_vdem2_violsd_boot2$term <- "Violent SDM onset"
#autonomy prior violence
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_prevv1,autonomy1_prevv0)),"autonomy1_prevv1" = 1, "autonomy1_prevv0" = 0), cbind(subset(subset_data2, select=-c(autonomy1_prevv1,autonomy1_prevv0)),"autonomy1_prevv1" = 0, "autonomy1_prevv0" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7b_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy1_prevv0","autonomy1_prevv1", "autonomy1_prevv0:d5_tt_vdem2","autonomy1_prevv1:d5_tt_vdem2","violsd_prev_l1","violsd_prev_l1:d5_tt_vdem2","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7b_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy1_prevv1 == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy1_prevv1 == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy1_prevv1 == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy1_prevv1 == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7b2_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
sr7b2_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7b2_vdem2_violsd_boot))
sr7b2_vdem2_violsd_boot2 <- cbind(sr7b2_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7b2_vdem2_violsd_boot2$model <- as.factor(sr7b2_vdem2_violsd_boot2$model)
sr7b2_vdem2_violsd_boot2$model = factor(sr7b2_vdem2_violsd_boot2$model,levels(sr7b2_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7b2_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
#autonomy no prior violence
binary_plot1_data <- rbind(sr7b_vdem2_sd_boot2, sr7b_vdem2_violsd_boot2)
binary_plot1_data$estimate <- binary_plot1_data$original
binary_plot1_data$conf.low <- binary_plot1_data$original - 1.645*  binary_plot1_data$bootSE
binary_plot1_data$conf.high <- binary_plot1_data$original + 1.645*  binary_plot1_data$bootSE
binary_plot1 <- dwplot(binary_plot1_data,
                       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                       dot_args = list(aes(shape = model, colour = model)), 
                       whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, no prev. viol., conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02, 0.03))
#autonomy prior violence
binary_plot2_data <- rbind(sr7b2_vdem2_sd_boot2, sr7b2_vdem2_violsd_boot2)
binary_plot2_data$estimate <- binary_plot2_data$original
binary_plot2_data$conf.low <- binary_plot2_data$original - 1.645*  binary_plot2_data$bootSE
binary_plot2_data$conf.high <- binary_plot2_data$original + 1.645*  binary_plot2_data$bootSE
binary_plot2 <- dwplot(binary_plot2_data,
                       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                       dot_args = list(aes(shape = model, colour = model)), 
                       whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, prev. viol., conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.05, 0.1, 0.15, 0.2))

###d) srg7b_vdem2_sd
#autonomy no prior violence
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv0" = 1, "autonomy1_prevv1" = 0), cbind(subset(subset_data, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv0" = 0, "autonomy1_prevv1" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7b_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy1_prevv0","tt_vdem2_shock*autonomy1_prevv1","tt_vdem2_shock*violsd_prev_l1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7b_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_prevv0 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_prevv0 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7b_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
srg7b_vdem2_sd_boot2 <- cbind(summary(srg7b_vdem2_sd_boot), time = rep(seq(0,10),2))
srg7b_vdem2_sd_boot2 <- cbind(srg7b_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7b_vdem2_sd_boot2$outcome <- "SDM onset"
#autonomy prior violence
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy1_prevv1,autonomy1_prevv0)),"autonomy1_prevv1" = 1, "autonomy1_prevv0" = 0), cbind(subset(subset_data, select=-c(autonomy1_prevv1,autonomy1_prevv0)),"autonomy1_prevv1" = 0, "autonomy1_prevv0" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7b_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy1_prevv0","tt_vdem2_shock*autonomy1_prevv1","tt_vdem2_shock*violsd_prev_l1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7b_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_prevv1 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_prevv1 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7b2_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)
srg7b2_vdem2_sd_boot2 <- cbind(summary(srg7b2_vdem2_sd_boot), time = rep(seq(0,10),2))
srg7b2_vdem2_sd_boot2 <- cbind(srg7b2_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7b2_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg7b_vdem2_violsd
#autonomy no prior violence
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv0" = 1, "autonomy1_prevv1" = 0), cbind(subset(subset_data2, select=-c(autonomy1_prevv0,autonomy1_prevv1)),"autonomy1_prevv0" = 0, "autonomy1_prevv1" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7b_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy1_prevv0","tt_vdem2_shock*autonomy1_prevv1","tt_vdem2_shock*violsd_prev_l1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7b_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_prevv0 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_prevv0 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7b_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
srg7b_vdem2_violsd_boot2 <- cbind(summary(srg7b_vdem2_violsd_boot), time = rep(seq(0,10),2))
srg7b_vdem2_violsd_boot2 <- cbind(srg7b_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7b_vdem2_violsd_boot2$outcome <- "Violent SDM onset"
#autonomy prior violence
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy1_prevv1,autonomy1_prevv0)),"autonomy1_prevv1" = 1, "autonomy1_prevv0" = 0), cbind(subset(subset_data2, select=-c(autonomy1_prevv1,autonomy1_prevv0)),"autonomy1_prevv1" = 0, "autonomy1_prevv0" =0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7b_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy1_prevv0","tt_vdem2_shock*autonomy1_prevv1","tt_vdem2_shock*violsd_prev_l1","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7b_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy1_prevv1 == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy1_prevv1 == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7b2_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)
srg7b2_vdem2_violsd_boot2 <- cbind(summary(srg7b2_vdem2_violsd_boot), time = rep(seq(0,10),2))
srg7b2_vdem2_violsd_boot2 <- cbind(srg7b2_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7b2_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
#autonomy no prior violence
continuous_plot1_data <- rbind(srg7b_vdem2_sd_boot2, srg7b_vdem2_violsd_boot2)
continuous_plot1_data$estimate <- continuous_plot1_data$original
continuous_plot1_data$conf.low <- continuous_plot1_data$original - 1.645*  continuous_plot1_data$bootSE
continuous_plot1_data$conf.high <- continuous_plot1_data$original + 1.645*  continuous_plot1_data$bootSE
continuous_plot1_data$model <- as.factor(continuous_plot1_data$model)
continuous_plot11 <- ggplot(data = subset(continuous_plot1_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy, no prev. viol., on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.01, 0, 0.01, 0.02, 0.03))
continuous_plot12 <- ggplot(data = subset(continuous_plot1_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy, no prev. viol., on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.01, 0, 0.01, 0.02))
continuous_plot1 <- grid_arrange_shared_legend(continuous_plot11, continuous_plot12, ncol=2, nrow=1)
#autonomy prior violence
continuous_plot2_data <- rbind(srg7b2_vdem2_sd_boot2, srg7b2_vdem2_violsd_boot2)
continuous_plot2_data$estimate <- continuous_plot2_data$original
continuous_plot2_data$conf.low <- continuous_plot2_data$original - 1.645*  continuous_plot2_data$bootSE
continuous_plot2_data$conf.high <- continuous_plot2_data$original + 1.645*  continuous_plot2_data$bootSE
continuous_plot2_data$model <- as.factor(continuous_plot2_data$model)
continuous_plot21 <- ggplot(data = subset(continuous_plot2_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy, prev. viol., on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.05, 0, 0.05, 0.1, 0.15))
continuous_plot22 <- ggplot(data = subset(continuous_plot2_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=10)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy, prev. viol., on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(-0.01, 0, 0.01, 0.02))
continuous_plot22
continuous_plot2 <- grid_arrange_shared_legend(continuous_plot21, continuous_plot22, ncol=2, nrow=1)

###g) combine all plots
figurea3 <- grid.arrange(binary_plot1, continuous_plot1, heights = c(2/5,3/5))
figurea4 <- grid.arrange(binary_plot2, continuous_plot2, heights = c(2/5,3/5))


##########3.3 Autonomy and ethno-regional organizations (I: control for vote share of ethnic/regional parties)##########

###a) sr7c_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7c_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "party_reg_ethnic_vote_share", "autonomy:party_reg_ethnic_vote_share", "d5_tt_vdem2:party_reg_ethnic_vote_share","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7c_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7c_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7c_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7c_vdem2_sd_boot))
sr7c_vdem2_sd_boot2 <- cbind(sr7c_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7c_vdem2_sd_boot2$model <- as.factor(sr7c_vdem2_sd_boot2$model)
sr7c_vdem2_sd_boot2$model = factor(sr7c_vdem2_sd_boot2$model,levels(sr7c_vdem2_sd_boot2$model)[c(3,1,2)])
sr7c_vdem2_sd_boot2$term <- "SDM onset"

###b) sr7c_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7c_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "party_reg_ethnic_vote_share", "autonomy:party_reg_ethnic_vote_share", "d5_tt_vdem2:party_reg_ethnic_vote_share","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7c_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7c_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7c_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7c_vdem2_violsd_boot))
sr7c_vdem2_violsd_boot2 <- cbind(sr7c_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7c_vdem2_violsd_boot2$model <- as.factor(sr7c_vdem2_violsd_boot2$model)
sr7c_vdem2_violsd_boot2$model = factor(sr7c_vdem2_violsd_boot2$model,levels(sr7c_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7c_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(sr7c_vdem2_sd_boot2, sr7c_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot <- dwplot(binary_plot_data,
                      vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                      dot_args = list(aes(shape = model, colour = model)), 
                      whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02, 0.03, 0.04))

###d) srg7c_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7c_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy", "party_reg_ethnic_vote_share", "autonomy:party_reg_ethnic_vote_share", "tt_vdem2_shock:party_reg_ethnic_vote_share","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7c_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7c_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
name <- character()
for (i in time) {
  step1 <- paste("name <- c(name, 'effect_t",i,"')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name <- c(name, 'effect_t",i,"_diff')", sep="")
  eval(parse(text=c(step1)))
}
srg7c_vdem2_sd_boot2 <- cbind(type = name, summary(srg7c_vdem2_sd_boot))
srg7c_vdem2_sd_boot2 <- cbind(srg7c_vdem2_sd_boot2, time = rep(seq(0,10),2))
srg7c_vdem2_sd_boot2 <- cbind(srg7c_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7c_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg7c_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7c_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy", "party_reg_ethnic_vote_share", "autonomy:party_reg_ethnic_vote_share", "tt_vdem2_shock:party_reg_ethnic_vote_share","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7c_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}

srg7c_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7c_vdem2_violsd_boot2 <- cbind(type = name, summary(srg7c_vdem2_violsd_boot))
srg7c_vdem2_violsd_boot2 <- cbind(srg7c_vdem2_violsd_boot2, time = rep(seq(0,10),2))
srg7c_vdem2_violsd_boot2 <- cbind(srg7c_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7c_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(srg7c_vdem2_sd_boot2, srg7c_vdem2_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.645*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.645*  continuous_plot_data$bootSE
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.02, 0.04, 0.06))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) construct overall figure
figurea5 <- grid.arrange(binary_plot, continuous_plot, heights = c(2/5,3/5))


##########3.4 Autonomy and ethno-regional organizations (II: control for number of ethnic organizations)##########

###a) sr7d_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0)  )
newdata <- subset(newdata, in_epr_org == 1)
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0 & in_epr_org == 1)[inds,]
  sr7d_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "lorg_number_any_g", "autonomy:lorg_number_any_g", "d5_tt_vdem2:lorg_number_any_g","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls_org,country_controls_org, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7d_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7d_vdem2_sd_boot <- boot(data = subset(df_global, in_epr_org == 1 & sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7d_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7d_vdem2_sd_boot))
sr7d_vdem2_sd_boot2 <- cbind(sr7d_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7d_vdem2_sd_boot2$model <- as.factor(sr7d_vdem2_sd_boot2$model)
sr7d_vdem2_sd_boot2$model = factor(sr7d_vdem2_sd_boot2$model,levels(sr7d_vdem2_sd_boot2$model)[c(3,1,2)])
sr7d_vdem2_sd_boot2$term <- "SDM onset"

###b) sr7d_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, in_epr_org == 1)
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0 & in_epr_org == 1)[inds,]
  sr7d_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "lorg_number_any_g", "autonomy:lorg_number_any_g", "d5_tt_vdem2:lorg_number_any_g","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls_org,country_controls_org, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7d_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7d_vdem2_violsd_boot <- boot(data = subset(df_global, in_epr_org == 1 & violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7d_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7d_vdem2_violsd_boot))
sr7d_vdem2_violsd_boot2 <- cbind(sr7d_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7d_vdem2_violsd_boot2$model <- as.factor(sr7d_vdem2_violsd_boot2$model)
sr7d_vdem2_violsd_boot2$model = factor(sr7d_vdem2_violsd_boot2$model,levels(sr7d_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7d_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(sr7d_vdem2_sd_boot2, sr7d_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot <- dwplot(binary_plot_data,
                      vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                      dot_args = list(aes(shape = model, colour = model)), 
                      whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

###d) srg7d_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, in_epr_org == 1)
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0 & in_epr_org == 1)[inds,]
  srg7d_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy", "lorg_number_any_g", "autonomy:lorg_number_any_g", "tt_vdem2_shock:lorg_number_any_g","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls_org,country_controls_org, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7d_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7d_vdem2_sd_boot <- boot(data = subset(df_global, in_epr_org == 1 & sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7d_vdem2_sd_boot2 <- cbind(type = name, summary(srg7d_vdem2_sd_boot))
srg7d_vdem2_sd_boot2 <- cbind(srg7d_vdem2_sd_boot2, time = rep(seq(0,10),2))
srg7d_vdem2_sd_boot2 <- cbind(srg7d_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7d_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg7d_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, in_epr_org == 1)
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0 & in_epr_org == 1)[inds,]
  srg7d_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy", "lorg_number_any_g", "autonomy:lorg_number_any_g", "tt_vdem2_shock:lorg_number_any_g","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls_org,country_controls_org, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7d_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7d_vdem2_violsd_boot <- boot(data = subset(df_global, in_epr_org == 1 & violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7d_vdem2_violsd_boot2 <- cbind(type = name, summary(srg7d_vdem2_violsd_boot))
srg7d_vdem2_violsd_boot2 <- cbind(srg7d_vdem2_violsd_boot2, time = rep(seq(0,10),2))
srg7d_vdem2_violsd_boot2 <- cbind(srg7d_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7d_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(srg7d_vdem2_sd_boot2, srg7d_vdem2_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.645*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.645*  continuous_plot_data$bootSE
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) construct overall figure
figurea6 <- grid.arrange(binary_plot, continuous_plot, heights = c(2/5,3/5))


##########3.5 Autonomy and economic inequality##########

###a) sr7e_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7e_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "low_ratio", "autonomy:low_ratio", "d5_tt_vdem2:low_ratio", "high_ratio", "autonomy:high_ratio", "d5_tt_vdem2:high_ratio","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7e_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7e_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7e_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7e_vdem2_sd_boot))
sr7e_vdem2_sd_boot2 <- cbind(sr7e_vdem2_sd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7e_vdem2_sd_boot2$model <- as.factor(sr7e_vdem2_sd_boot2$model)
sr7e_vdem2_sd_boot2$model = factor(sr7e_vdem2_sd_boot2$model,levels(sr7e_vdem2_sd_boot2$model)[c(3,1,2)])
sr7e_vdem2_sd_boot2$term <- "SDM onset"

###b) sr7e_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7e_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "low_ratio", "autonomy:low_ratio", "d5_tt_vdem2:low_ratio", "high_ratio", "autonomy:high_ratio", "d5_tt_vdem2:high_ratio","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7e_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7e_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7e_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7e_vdem2_violsd_boot))
sr7e_vdem2_violsd_boot2 <- cbind(sr7e_vdem2_violsd_boot2, model = c("Autonomy in transition periods (predicted effect)", "Autonomy in other periods (predicted effect)", "Autonomy in transition vs. other periods\n(difference between predicted effects)"))
sr7e_vdem2_violsd_boot2$model <- as.factor(sr7e_vdem2_violsd_boot2$model)
sr7e_vdem2_violsd_boot2$model = factor(sr7e_vdem2_violsd_boot2$model,levels(sr7e_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7e_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(sr7e_vdem2_sd_boot2, sr7e_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot <- dwplot(binary_plot_data,
                      vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                      dot_args = list(aes(shape = model, colour = model)), 
                      whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02, 0.03, 0.04))

###d) srg7e_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7e_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy", "low_ratio", "autonomy:low_ratio", "tt_vdem2_shock:low_ratio", "high_ratio", "autonomy:high_ratio", "tt_vdem2_shock:high_ratio","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7e_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7e_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7e_vdem2_sd_boot2 <- cbind(type = name, summary(srg7e_vdem2_sd_boot))
srg7e_vdem2_sd_boot2 <- cbind(srg7e_vdem2_sd_boot2, time = rep(seq(0,10),2))
srg7e_vdem2_sd_boot2 <- cbind(srg7e_vdem2_sd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7e_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg7e_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7e_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy", "low_ratio", "autonomy:low_ratio", "tt_vdem2_shock:low_ratio", "high_ratio", "autonomy:high_ratio", "tt_vdem2_shock:high_ratio","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7e_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7e_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7e_vdem2_violsd_boot2 <- cbind(type = name, summary(srg7e_vdem2_violsd_boot))
srg7e_vdem2_violsd_boot2 <- cbind(srg7e_vdem2_violsd_boot2, time = rep(seq(0,10),2))
srg7e_vdem2_violsd_boot2 <- cbind(srg7e_vdem2_violsd_boot2, model = rep(c("Autonomy (predicted effect)","Autonomy (difference in predicted effect, compared to effect 10 years after transition)"),each=11))
srg7e_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(srg7e_vdem2_sd_boot2, srg7e_vdem2_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.645*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.645*  continuous_plot_data$bootSE
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.02, 0.04, 0.06))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) construct overall figure
figurea7 <- grid.arrange(binary_plot, continuous_plot, heights = c(2/5,3/5))


##########3.6 Autonomy and cultural cleavages##########

###a) sr7f_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0)  )
newdata <- subset(newdata, !is.na(cleavage_mean))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7f_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "cleavage_mean", "autonomy:cleavage_mean", "d5_tt_vdem2:cleavage_mean","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7f_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7f_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7f_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7f_vdem2_sd_boot))
sr7f_vdem2_sd_boot2 <- cbind(sr7f_vdem2_sd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
sr7f_vdem2_sd_boot2$model <- as.factor(sr7f_vdem2_sd_boot2$model)
sr7f_vdem2_sd_boot2$model = factor(sr7f_vdem2_sd_boot2$model,levels(sr7f_vdem2_sd_boot2$model)[c(3,1,2)])
sr7f_vdem2_sd_boot2$term <- "SDM onset"

###b) sr7f_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, !is.na(cleavage_mean))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7f_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "cleavage_mean", "autonomy:cleavage_mean", "d5_tt_vdem2:cleavage_mean","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7f_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7f_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7f_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7f_vdem2_violsd_boot))
sr7f_vdem2_violsd_boot2 <- cbind(sr7f_vdem2_violsd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
sr7f_vdem2_violsd_boot2$model <- as.factor(sr7f_vdem2_violsd_boot2$model)
sr7f_vdem2_violsd_boot2$model = factor(sr7f_vdem2_violsd_boot2$model,levels(sr7f_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7f_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(sr7f_vdem2_sd_boot2, sr7f_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot <- dwplot(binary_plot_data,
                      vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                      dot_args = list(aes(shape = model, colour = model)), 
                      whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02, 0.03, 0.04))

###d) srg7f_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, !is.na(cleavage_mean))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7f_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy", "cleavage_mean", "autonomy:cleavage_mean", "tt_vdem2_shock:cleavage_mean","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7f_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7f_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7f_vdem2_sd_boot2 <- cbind(type = name, summary(srg7f_vdem2_sd_boot))
srg7f_vdem2_sd_boot2 <- cbind(srg7f_vdem2_sd_boot2, time = rep(seq(0,10),2))
srg7f_vdem2_sd_boot2 <- cbind(srg7f_vdem2_sd_boot2, model = rep(c("Δ in Pr(outcome | time since transition)","Δ in Pr(outcome | time since transition) - Δ in Pr(outcome | time since transition = 10)"),each=11))
srg7f_vdem2_sd_boot2$model <- as.factor(srg7f_vdem2_sd_boot2$model)
srg7f_vdem2_sd_boot2$model = factor(srg7f_vdem2_sd_boot2$model,levels(srg7f_vdem2_sd_boot2$model)[c(2,1)])
srg7f_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg7f_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, !is.na(cleavage_mean))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7f_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy", "cleavage_mean", "autonomy:cleavage_mean", "tt_vdem2_shock:cleavage_mean","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7f_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7f_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7f_vdem2_violsd_boot2 <- cbind(type = name, summary(srg7f_vdem2_violsd_boot))
srg7f_vdem2_violsd_boot2 <- cbind(srg7f_vdem2_violsd_boot2, time = rep(seq(0,10),2))
srg7f_vdem2_violsd_boot2 <- cbind(srg7f_vdem2_violsd_boot2, model = rep(c("Δ in Pr(outcome | time since transition)","Δ in Pr(outcome | time since transition) - Δ in Pr(outcome | time since transition = 10)"),each=11))
srg7f_vdem2_violsd_boot2$model <- as.factor(srg7f_vdem2_violsd_boot2$model)
srg7f_vdem2_violsd_boot2$model = factor(srg7f_vdem2_violsd_boot2$model,levels(srg7f_vdem2_violsd_boot2$model)[c(2,1)])
srg7f_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(srg7f_vdem2_sd_boot2, srg7f_vdem2_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.645*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.645*  continuous_plot_data$bootSE
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.02, 0.04, 0.06))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) construct overall figure
figurea8 <- grid.arrange(binary_plot, continuous_plot, heights = c(2/5,3/5))


##########3.7 Autonomy and changes in government support group##########

###a) sr7g_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0)  )
newdata <- subset(newdata, !is.na(d5_tt_govsup))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  sr7g_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "d5_tt_govsup", "autonomy:d5_tt_govsup","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7g_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7g_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7g_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7g_vdem2_sd_boot))
sr7g_vdem2_sd_boot2 <- cbind(sr7g_vdem2_sd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
sr7g_vdem2_sd_boot2$model <- as.factor(sr7g_vdem2_sd_boot2$model)
sr7g_vdem2_sd_boot2$model = factor(sr7g_vdem2_sd_boot2$model,levels(sr7g_vdem2_sd_boot2$model)[c(3,1,2)])
sr7g_vdem2_sd_boot2$term <- "SDM onset"

###b) sr7g_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, !is.na(d5_tt_govsup))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  sr7g_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "d5_tt_govsup", "autonomy:d5_tt_govsup","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr7g_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr7g_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr7g_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr7g_vdem2_violsd_boot))
sr7g_vdem2_violsd_boot2 <- cbind(sr7g_vdem2_violsd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
sr7g_vdem2_violsd_boot2$model <- as.factor(sr7g_vdem2_violsd_boot2$model)
sr7g_vdem2_violsd_boot2$model = factor(sr7g_vdem2_violsd_boot2$model,levels(sr7g_vdem2_violsd_boot2$model)[c(3,1,2)])
sr7g_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(sr7g_vdem2_sd_boot2, sr7g_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot <- dwplot(binary_plot_data,
                      vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                      dot_args = list(aes(shape = model, colour = model)), 
                      whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

###d) srg7g_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, !is.na(d5_tt_govsup))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  srg7g_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy", "tt_govsup_shock*autonomy","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7g_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7g_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7g_vdem2_sd_boot2 <- cbind(type = name, summary(srg7g_vdem2_sd_boot))
srg7g_vdem2_sd_boot2 <- cbind(srg7g_vdem2_sd_boot2, time = rep(seq(0,10),2))
srg7g_vdem2_sd_boot2 <- cbind(srg7g_vdem2_sd_boot2, model = rep(c("Δ in Pr(outcome | time since transition)","Δ in Pr(outcome | time since transition) - Δ in Pr(outcome | time since transition = 10)"),each=11))
srg7g_vdem2_sd_boot2$outcome <- "SDM onset"

###e) srg7g_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
newdata <- subset(newdata, !is.na(d5_tt_govsup))
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  srg7g_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy", "tt_govsup_shock*autonomy","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(srg7g_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
srg7g_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
srg7g_vdem2_violsd_boot2 <- cbind(type = name, summary(srg7g_vdem2_violsd_boot))
srg7g_vdem2_violsd_boot2 <- cbind(srg7g_vdem2_violsd_boot2, time = rep(seq(0,10),2))
srg7g_vdem2_violsd_boot2 <- cbind(srg7g_vdem2_violsd_boot2, model = rep(c("Δ in Pr(outcome | time since transition)","Δ in Pr(outcome | time since transition) - Δ in Pr(outcome | time since transition = 10)"),each=11))
srg7g_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(srg7g_vdem2_sd_boot2, srg7g_vdem2_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.645*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.645*  continuous_plot_data$bootSE
continuous_plot_data$model <- as.factor(continuous_plot_data$model)
continuous_plot_data$model = factor(continuous_plot_data$model,levels(continuous_plot_data$model)[c(2,4,1,5,3)])
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) construct overall plot
figurea9 <- grid.arrange(binary_plot, continuous_plot, heights = c(2/5,3/5))


####################################################################################
#########4. Probing reverse causation and endogeneity further (appendix B.2)#########
####################################################################################

##########4.1 Excluding groups with (violent) SDM before transition##########
subset_data3 <- subset(df_global, tt_vdem2_ybt_sd2 == 0 & sd_l1 == 0)
subset_data3$tt_vdem2_shock <- 1 * 0.5^((subset_data3$tt_vdem2_durability) / 3)#calculate again to prevent rounding errors
subset_data4 <- subset(df_global, tt_vdem2_ybt_violsd2 == 0 & violsd_l1 == 0)
subset_data4$tt_vdem2_shock <- 1 * 0.5^((subset_data4$tt_vdem2_durability) / 3)#calculate again to prevent rounding errors
subset_data5 <- subset(df_global, d5_tt_vdem2 == 1 & tt_vdem2_ybt_sd2 == 0 & sd_l1 == 0)
subset_data5$tt_vdem2_shock <- 1 * 0.5^((subset_data5$tt_vdem2_durability) / 3)#calculate again to prevent rounding errors
subset_data6 <- subset(df_global, d5_tt_vdem2 == 1 & tt_vdem2_ybt_violsd2 == 0 & violsd_l1 == 0)
subset_data6$tt_vdem2_shock <- 1 * 0.5^((subset_data6$tt_vdem2_durability) / 3)#calculate again to prevent rounding errors

###a) sr5_vdem2_sd
newdata <- rbind( cbind(subset(subset_data3, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data3, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data3, sd_l1 == 0)[inds,]
  sr5_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr5_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr5_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr5_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr5_vdem2_sd_boot))
sr5_vdem2_sd_boot2 <- cbind(sr5_vdem2_sd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
sr5_vdem2_sd_boot2$model <- as.factor(sr5_vdem2_sd_boot2$model)
sr5_vdem2_sd_boot2$model = factor(sr5_vdem2_sd_boot2$model,levels(sr5_vdem2_sd_boot2$model)[c(3,1,2)])
sr5_vdem2_sd_boot2$term <- "SDM onset"

###b) sr5_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data4, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data4, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data4, violsd_l1 == 0)[inds,]
  sr5_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2", "included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr5_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
sr5_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr5_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(sr5_vdem2_violsd_boot))
sr5_vdem2_violsd_boot2 <- cbind(sr5_vdem2_violsd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
sr5_vdem2_violsd_boot2$model <- as.factor(sr5_vdem2_violsd_boot2$model)
sr5_vdem2_violsd_boot2$model = factor(sr5_vdem2_violsd_boot2$model,levels(sr5_vdem2_violsd_boot2$model)[c(3,1,2)])
sr5_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(sr5_vdem2_sd_boot2, sr5_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot1 <- dwplot(binary_plot_data,
                       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                       dot_args = list(aes(shape = model, colour = model)), 
                       whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on\ndichotomous transition period variable\n(all years)") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02, 0.03, 0.04))

###d) sr5b_vdem2_sd
newdata <- rbind( cbind(subset(subset_data5, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data5, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data5, sd_l1 == 0)[inds,]
  sr5b_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("autonomy", "included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr5b_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0, 'prediction'])
  effect_t1
}
sr5b_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr5b_vdem2_sd_boot2 <- cbind(type = c("effect_t1"), summary(sr5b_vdem2_sd_boot))
sr5b_vdem2_sd_boot2 <- cbind(sr5b_vdem2_sd_boot2, model = c("Δ in Pr(outcome), transition periods"))
sr5b_vdem2_sd_boot2$model <- as.factor(sr5b_vdem2_sd_boot2$model)
sr5b_vdem2_sd_boot2$term <- "SDM onset"

###e) sr5b_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data6, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data6, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data6, violsd_l1 == 0)[inds,]
  sr5b_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("autonomy", "included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(sr5b_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0, 'prediction'])
  effect_t1
}
sr5b_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
sr5b_vdem2_violsd_boot2 <- cbind(type = c("effect_t1"), summary(sr5b_vdem2_violsd_boot))
sr5b_vdem2_violsd_boot2 <- cbind(sr5b_vdem2_violsd_boot2, model = c("Δ in Pr(outcome), transition periods"))
sr5b_vdem2_violsd_boot2$model <- as.factor(sr5b_vdem2_violsd_boot2$model)
sr5b_vdem2_violsd_boot2$term <- "Violent SDM onset"

###f) combine dichotomous plot
binary_plot_data <- rbind(sr5b_vdem2_sd_boot2, sr5b_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot2 <- dwplot(binary_plot_data,
                       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                       dot_args = list(aes(shape = model, colour = model)), 
                       whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) effect of autonomy, conditional on\ndichotomous transition period variable\n(transition years)") +
  scale_color_manual(name = "effect type", values = c("black"))+ 
  scale_shape_manual(name = "effect type", values = c(19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.01, 0.02, 0.03, 0.04))

###g) construct overall figure
figurea11 <- grid_arrange_shared_legend(binary_plot1, binary_plot2)


##########4.2 Group-fixed effects##########

###a) lmsr3d_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)#function to be passed to bootstrap: depends on data (different subsets of original data), as given by indices (determined by bootstrap)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  lmsr3d_vdem2_sd <- lm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2","state_control","included","yearf",gwgroupid_levels, sdm_years), collapse = " + "), sep = " ~ ")), data=dat,na.action = na.exclude)
  prediction <- predict(lmsr3d_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
lmsr3d_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
lmsr3d_vdem2_sd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(lmsr3d_vdem2_sd_boot))
lmsr3d_vdem2_sd_boot2 <- cbind(lmsr3d_vdem2_sd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
lmsr3d_vdem2_sd_boot2$model <- as.factor(lmsr3d_vdem2_sd_boot2$model)
lmsr3d_vdem2_sd_boot2$model = factor(lmsr3d_vdem2_sd_boot2$model,levels(lmsr3d_vdem2_sd_boot2$model)[c(3,1,2)])
lmsr3d_vdem2_sd_boot2$term <- "SDM onset"

###b) lmsr3d_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)#function to be passed to bootstrap: depends on data (different subsets of original data), as given by indices (determined by bootstrap)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  lmsr3d_vdem2_violsd <- lm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2", "autonomy", "autonomy:d5_tt_vdem2","state_control","included","yearf",gwgroupid_levels, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat,na.action = na.exclude)
  prediction <- predict(lmsr3d_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  effect_t1 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2 == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2 == 0, 'prediction'])
  output <- c(effect_t1, effect_t0, effect_t1 - effect_t0)
  output
}
lmsr3d_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
lmsr3d_vdem2_violsd_boot2 <- cbind(type = c("effect_t1","effect_t0","effect_diff"), summary(lmsr3d_vdem2_violsd_boot))
lmsr3d_vdem2_violsd_boot2 <- cbind(lmsr3d_vdem2_violsd_boot2, model = c("Δ in Pr(outcome), transition periods","Δ in Pr(outcome), other time periods","Δ in Pr(outcome), transition periods -\nΔ in Pr(outcome), other time periods"))
lmsr3d_vdem2_violsd_boot2$model <- as.factor(lmsr3d_vdem2_violsd_boot2$model)
lmsr3d_vdem2_violsd_boot2$model = factor(lmsr3d_vdem2_violsd_boot2$model,levels(lmsr3d_vdem2_violsd_boot2$model)[c(3,1,2)])
lmsr3d_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(lmsr3d_vdem2_sd_boot2, lmsr3d_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot <- dwplot(binary_plot_data,
                      vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                      dot_args = list(aes(shape = model, colour = model)), 
                      whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on dichotomous transition period variable") +
  scale_color_manual(name = "effect type", values = c("red","black","black"))+ 
  scale_shape_manual(name = "effect type", values = c(17,1,19))+ 
  theme(plot.title.position = "plot",legend.position = "right", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=3,"effect type",reverse=TRUE), colour = guide_legend(nrow=3,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

###d) lmsrg3d_vdem2_sd
newdata <- rbind( cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)#function to be passed to bootstrap: depends on data (different subsets of original data), as given by indices (determined by bootstrap)
{
  dat =  subset(subset_data, sd_l1 == 0)[inds,]
  lmsrg3d_vdem2_sd <- lm(as.formula(paste("sd_onset", paste(c("tt_vdem2_shock*autonomy","state_control","included","yearf",gwgroupid_levels, sdm_years), collapse = " + "), sep = " ~ ")), data=dat,na.action = na.exclude)
  prediction <- predict(lmsrg3d_vdem2_sd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
lmsrg3d_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
lmsrg3d_vdem2_sd_boot2 <- cbind(type = name, summary(lmsrg3d_vdem2_sd_boot))
lmsrg3d_vdem2_sd_boot2 <- cbind(lmsrg3d_vdem2_sd_boot2, time = rep(seq(0,10),2))
lmsrg3d_vdem2_sd_boot2 <- cbind(lmsrg3d_vdem2_sd_boot2, model = rep(c("Δ in Pr(outcome | time since transition)","Δ in Pr(outcome | time since transition) - Δ in Pr(outcome | time since transition = 10)"),each=11))
lmsrg3d_vdem2_sd_boot2$outcome <- "SDM onset"

###e) lmsrg3d_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data2, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)#function to be passed to bootstrap: depends on data (different subsets of original data), as given by indices (determined by bootstrap)
{
  dat =  subset(subset_data2, violsd_l1 == 0)[inds,]
  lmsrg3d_vdem2_violsd <- lm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_shock*autonomy","state_control","included","yearf",gwgroupid_levels, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat,na.action = na.exclude)
  prediction <- predict(lmsrg3d_vdem2_violsd, type ="response", newdata = newdata)#NEED NEWDATA. ALL VALUES IN ORIGINAL DATA FOR ALL COMBINATIONS
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i," <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,")", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i," - effect_t10)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
lmsrg3d_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
lmsrg3d_vdem2_violsd_boot2 <- cbind(type = name, summary(lmsrg3d_vdem2_violsd_boot))
lmsrg3d_vdem2_violsd_boot2 <- cbind(lmsrg3d_vdem2_violsd_boot2, time = rep(seq(0,10),2))
lmsrg3d_vdem2_violsd_boot2 <- cbind(lmsrg3d_vdem2_violsd_boot2, model = rep(c("Δ in Pr(outcome | time since transition)","Δ in Pr(outcome | time since transition) - Δ in Pr(outcome | time since transition = 10)"),each=11))
lmsrg3d_vdem2_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(lmsrg3d_vdem2_sd_boot2, lmsrg3d_vdem2_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.645*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.645*  continuous_plot_data$bootSE
continuous_plot_data$model <- as.factor(continuous_plot_data$model)
continuous_plot_data$model = factor(continuous_plot_data$model,levels(continuous_plot_data$model)[c(2,1)])
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset, conditional on continuous transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10)) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("red","black"))+ 
  scale_fill_manual(name = "effect type", values = c("red","black"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("c) effect of autonomy on violent SDM onset, conditional on continuous transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) construct overall figure
figurea12 <- grid.arrange(binary_plot, continuous_plot, heights = c(2/5,3/5))



####################################################################################
#########5. Unpacking transitions (appendix B.3)#########
####################################################################################

##########5.1 Differentiating between different types of transitions##########
subset_data7 <- subset(df_global, sd_l1 == 0 & !is.na(d5_tt_vdem2_dem_or_aut))
subset_data8 <- subset(df_global, violsd_l1 == 0 & !is.na(d5_tt_vdem2_dem_or_aut))

###a) trans3_vdem2_sd
newdata <- rbind( cbind(subset(subset_data7, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data7, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data7, sd_l1 == 0)[inds,]
  trans3_vdem2_sd <- glm(as.formula(paste("sd_onset", paste(c("d5_tt_vdem2_aut*autonomy","d5_tt_vdem2_dem*autonomy","d5_tt_vdem2_st*autonomy", "autonomy", "included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(trans3_vdem2_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1_dem <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_dem == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_dem == 1, 'prediction'])
  effect_t1_aut <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_aut == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_aut == 1, 'prediction'])
  effect_t1_st <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_st == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_st == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_dem == 0 & newdata$d5_tt_vdem2_aut == 0 & newdata$d5_tt_vdem2_st == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_dem == 0 & newdata$d5_tt_vdem2_aut == 0 & newdata$d5_tt_vdem2_st == 0, 'prediction'])
  output <- c(effect_t1_dem, effect_t1_aut, effect_t1_st, effect_t0, effect_t1_dem - effect_t0, effect_t1_aut - effect_t0, effect_t1_st - effect_t0)
  output
}
trans3_vdem2_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
trans3_vdem2_sd_boot2 <- cbind(type = c("effect_t1_dem","effect_t1_aut","effect_t1_st","effect_t0","effect_diff_dem","effect_diff_aut","effect_diff_st"), summary(trans3_vdem2_sd_boot))
trans3_vdem2_sd_boot2 <- cbind(trans3_vdem2_sd_boot2, model = c("Autonomy in democratization periods\n(predicted effect)","Autonomy in autocratization periods\n(predicted effect)","Autonomy in state creation periods\n(predicted effect)","Autonomy in other periods\n(predicted effect)","Autonomy in democratization vs. other periods\n(difference between predicted effects)","Autonomy in autocratization vs. other periods\n(difference between predicted effects)","Autonomy in state creation vs. other periods\n(difference between predicted effects)"))
trans3_vdem2_sd_boot2$model <- as.factor(trans3_vdem2_sd_boot2$model)
trans3_vdem2_sd_boot2$model = factor(trans3_vdem2_sd_boot2$model,levels(trans3_vdem2_sd_boot2$model)[c(3,1,6,5,4,2,7)])
trans3_vdem2_sd_boot2$term <- "SDM onset"

###b) trans3_vdem2_violsd
newdata <- rbind( cbind(subset(subset_data8, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data8, select=-c(autonomy)),"autonomy" = 0)  )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data8, violsd_l1 == 0)[inds,]
  trans3_vdem2_violsd <- glm(as.formula(paste("violsd_onset", paste(c("d5_tt_vdem2_aut*autonomy","d5_tt_vdem2_dem*autonomy","d5_tt_vdem2_st*autonomy", "included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(trans3_vdem2_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  effect_t1_dem <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_dem == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_dem == 1, 'prediction'])
  effect_t1_aut <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_aut == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_aut == 1, 'prediction'])
  effect_t1_st <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_st == 1, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_st == 1, 'prediction'])
  effect_t0 <- mean(newdata[newdata$autonomy == 1 & newdata$d5_tt_vdem2_dem == 0 & newdata$d5_tt_vdem2_aut == 0 & newdata$d5_tt_vdem2_st == 0, 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$d5_tt_vdem2_dem_or_aut == 0 & newdata$d5_tt_vdem2_st == 0, 'prediction'])
  output <- c(effect_t1_dem, effect_t1_aut, effect_t1_st, effect_t0, effect_t1_dem - effect_t0, effect_t1_aut - effect_t0, effect_t1_st - effect_t0)
  output
}
trans3_vdem2_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
trans3_vdem2_violsd_boot2 <- cbind(type = c("effect_t1_dem","effect_t1_aut","effect_t1_st","effect_t0","effect_diff_dem","effect_diff_aut","effect_diff_st"), summary(trans3_vdem2_violsd_boot))
trans3_vdem2_violsd_boot2 <- cbind(trans3_vdem2_violsd_boot2, model = c("Autonomy in democratization periods\n(predicted effect)","Autonomy in autocratization periods\n(predicted effect)","Autonomy in state creation periods\n(predicted effect)","Autonomy in other periods\n(predicted effect)","Autonomy in democratization vs. other periods\n(difference between predicted effects)","Autonomy in autocratization vs. other periods\n(difference between predicted effects)","Autonomy in state creation vs. other periods\n(difference between predicted effects)"))
trans3_vdem2_violsd_boot2$model <- as.factor(trans3_vdem2_violsd_boot2$model)
trans3_vdem2_violsd_boot2$model = factor(trans3_vdem2_violsd_boot2$model,levels(trans3_vdem2_violsd_boot2$model)[c(3,1,6,5,4,2,7)])
trans3_vdem2_violsd_boot2$term <- "Violent SDM onset"

###c) combine dichotomous plot
binary_plot_data <- rbind(trans3_vdem2_sd_boot2, trans3_vdem2_violsd_boot2)
binary_plot_data$estimate <- binary_plot_data$original
binary_plot_data$conf.low <- binary_plot_data$original - 1.645*  binary_plot_data$bootSE
binary_plot_data$conf.high <- binary_plot_data$original + 1.645*  binary_plot_data$bootSE
binary_plot1_small <- dwplot(subset(binary_plot_data, type == "effect_t0" | type == "effect_t1_aut" | type == "effect_t1_dem" | type == "effect_t1_st"),
                             vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                             dot_args = list(aes(shape = model, colour = model)), 
                             whisker_args = list(aes(colour = model), linetype = 1)) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) effect of autonomy, conditional on\ndichotomous transition period variable\n(0-5)") +
  scale_color_manual(name = "effect type", values = c("black","#7570b3","#d95f02","#1b9e77"))+ 
  scale_shape_manual(name = "effect type", values = c(1,18,15,19))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) + 
  xlab("effect estimate") + ylab("outcome") +
  guides(shape = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(accuracy = 1))

###d) trans3_vdem2g_sd
newdata <- rbind( cbind(subset(subset_data7, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data7, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data7, sd_l1 == 0)[inds,]
  trans3_vdem2g_sd <- glm(as.formula(paste("sd_onset", paste(c("tt_vdem2_dem_shock*autonomy","tt_vdem2_aut_shock*autonomy","tt_vdem2_st_shock*autonomy","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, sdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(trans3_vdem2g_sd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i,"_dem <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_dem_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_dem_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    step2 <- paste("effect_t",i,"_aut <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_aut_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_aut_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    step3 <- paste("effect_t",i,"_st <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_st_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_st_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1,step2,step3)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_dem)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_aut)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_st)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_dem - effect_t10_dem)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_aut - effect_t10_aut)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_st - effect_t10_st)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
trans3_vdem2g_sd_boot <- boot(data = subset(df_global, sd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
name2 <- character()
for (i in time) {
  step1 <- paste("name2 <- c(name2, 'effect_t",i,"_dem')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name2 <- c(name2, 'effect_t",i,"_aut')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name2 <- c(name2, 'effect_t",i,"_st')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name2 <- c(name2, 'effect_t",i,"_dem_diff')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name2 <- c(name2, 'effect_t",i,"_aut_diff')", sep="")
  eval(parse(text=c(step1)))
}
for (i in time) {
  step1 <- paste("name2 <- c(name2, 'effect_t",i,"__st_diff')", sep="")
  eval(parse(text=c(step1)))
}
trans3_vdem2g_sd_boot2 <- cbind(type = name2, summary(trans3_vdem2g_sd_boot))
trans3_vdem2g_sd_boot2 <- cbind(trans3_vdem2g_sd_boot2, time = rep(seq(0,10),6))
trans3_vdem2g_sd_boot2 <- cbind(trans3_vdem2g_sd_boot2, model = rep(c("Autonomy (predicted effect,\ndemocratization periods)","Autonomy (predicted effect,\nautocratization periods)","Autonomy (predicted effect,\nstate creation periods)","Autonomy (difference in predicted effect, compared to effect 10 years after democratic transition)","Autonomy (difference in predicted effect, compared to effect 10 years after autocratic transition)","Autonomy (difference in predicted effect, compared to effect 10 years after state creation)"),each=11))
trans3_vdem2g_sd_boot2$outcome <- "SDM onset"

###e) trans3_vdem2g_violsd
time <- seq(0,10,length.out = 11)
newdata <- rbind( cbind(subset(subset_data8, select=-c(autonomy)),"autonomy" = 1), cbind(subset(subset_data8, select=-c(autonomy)),"autonomy" = 0) )
m1bootfun <- function(dat, inds)
{
  dat =  subset(subset_data8, violsd_l1 == 0)[inds,]
  trans3_vdem2g_violsd <- glm(as.formula(paste("violsd_onset", paste(c("tt_vdem2_dem_shock*autonomy","tt_vdem2_aut_shock*autonomy","tt_vdem2_st_shock*autonomy","included","yearf","region_Asia","region_Europe","region_MENA","region_Oceania","region_SSA",group_controls,country_controls, violsdm_years), collapse = " + "), sep = " ~ ")), data=dat, family=binomial(link="logit"),na.action = na.exclude)
  prediction <- predict(trans3_vdem2g_violsd, type ="response", newdata = newdata)
  newdata <- cbind(newdata, prediction)
  for (i in time) {
    step1 <- paste("effect_t",i,"_dem <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_dem_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_dem_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    step2 <- paste("effect_t",i,"_aut <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_aut_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_aut_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    step3 <- paste("effect_t",i,"_st <- mean(newdata[newdata$autonomy == 1 & newdata$tt_vdem2_st_shock == 1 * 0.5^((",i,") / 3), 'prediction']) - mean(newdata[newdata$autonomy == 0 & newdata$tt_vdem2_st_shock == 1 * 0.5^((",i,") / 3), 'prediction'])",sep="")
    eval(parse(text=c(step1,step2,step3)))
  }
  output <- numeric()
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_dem)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_aut)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_st)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_dem - effect_t10_dem)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_aut - effect_t10_aut)", sep="")
    eval(parse(text=c(step1)))
  }
  for (i in time) {
    step1 <- paste("output <- c(output, effect_t",i,"_st - effect_t10_st)", sep="")
    eval(parse(text=c(step1)))
  }
  output
}
trans3_vdem2g_violsd_boot <- boot(data = subset(df_global, violsd_l1 == 0), statistic = m1bootfun, 500)#do the bootstrap using the original data from which subsets will be created, relying on the function doing subsets and predictions, and x iterations
trans3_vdem2g_violsd_boot2 <- cbind(type = name2, summary(trans3_vdem2g_violsd_boot))
trans3_vdem2g_violsd_boot2 <- cbind(trans3_vdem2g_violsd_boot2, time = rep(seq(0,10),6))
trans3_vdem2g_violsd_boot2 <- cbind(trans3_vdem2g_violsd_boot2, model = rep(c("Autonomy (predicted effect,\ndemocratization periods)","Autonomy (predicted effect,\nautocratization periods)","Autonomy (predicted effect,\nstate creation periods)","Autonomy (difference in predicted effect, compared to effect 10 years after democratic transition)","Autonomy (difference in predicted effect, compared to effect 10 years after autocratic transition)","Autonomy (difference in predicted effect, compared to effect 10 years after state creation)"),each=11))
trans3_vdem2g_violsd_boot2$outcome <- "Violent SDM onset"

###f) combine continuous plot
continuous_plot_data <- rbind(trans3_vdem2g_sd_boot2, trans3_vdem2g_violsd_boot2)
continuous_plot_data$estimate <- continuous_plot_data$original
continuous_plot_data$conf.low <- continuous_plot_data$original - 1.645*  continuous_plot_data$bootSE
continuous_plot_data$conf.high <- continuous_plot_data$original + 1.645*  continuous_plot_data$bootSE
continuous_plot_data$model <- as.factor(continuous_plot_data$model)
continuous_plot_data$model = factor(continuous_plot_data$model,levels(continuous_plot_data$model)[c(6,4,5,3,1,2)])
continuous_plot1 <- ggplot(data = subset(continuous_plot_data,outcome=="SDM onset" & time <= 10 & (model == "Autonomy (predicted effect,\ndemocratization periods)" | model == "Autonomy (predicted effect,\nautocratization periods)" | model == "Autonomy (predicted effect,\nstate creation periods)")) ) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("#7570b3","#d95f02","#1b9e77"))+ 
  scale_fill_manual(name = "effect type", values = c("#7570b3","#d95f02","#1b9e77"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot2 <- ggplot(data = subset(continuous_plot_data,outcome=="Violent SDM onset" & time <= 10 & (model == "Autonomy (predicted effect,\ndemocratization periods)" | model == "Autonomy (predicted effect,\nautocratization periods)" | model == "Autonomy (predicted effect,\nstate creation periods)")) ) + geom_line(aes(x=time, y=estimate,colour=model)) + 
  geom_ribbon(aes(x=time, ymin = original - 1.645* bootSE, ymax = original + 1.645*  bootSE,fill=model), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_bw() + theme(plot.title.position = "plot", text=element_text(family='Times'), legend.position="bottom", axis.title=element_text(size=9), legend.text=element_text(size=9), plot.title =element_text(size=11)) +
  scale_color_manual(name = "effect type", values = c("#7570b3","#d95f02","#1b9e77"))+ 
  scale_fill_manual(name = "effect type", values = c("#7570b3","#d95f02","#1b9e77"))+ 
  guides(fill = guide_legend(nrow=2,"effect type",reverse=TRUE), colour = guide_legend(nrow=2,"effect type",reverse=TRUE)) +
  xlab('time since transition') + ylab('effect estimate') + ggtitle("b) effect of autonomy on violent SDM onset,\nconditional on cont. transition proximity variable") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1))
continuous_plot <- grid_arrange_shared_legend(continuous_plot1, continuous_plot2, ncol=2, nrow=1)

###g) construct overall figure
figurea14 <- grid.arrange(binary_plot1_small, continuous_plot)











