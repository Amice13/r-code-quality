#######################################################################################################
# Ethnic accommodation and the backlash from dominant groups #
# Analysis script #
# Andreas Juon #
# Journal of Conflict Resolution #
# May 2025 #
#######################################################################################################

######################################################################
## Set-up ##
######################################################################

### Import data ###
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))#sets working directory to folder containing this file
source("./functions/backlash_functions.R")#libraries and helper functions that run the article's analyses
backlash_data <- read.csv("./data/accommodation_backlash_maindata.csv")#data used in the main analyses

### Generate factor variables for fixed effects ###
backlash_data$cowcodef <- as.factor(backlash_data$cowcode)
backlash_data$yearf <- as.factor(backlash_data$year)


######################################################################
## Descriptives ##
######################################################################

### Monthly concessions (Figure 1) ###
conc_plot_data <- unique(subset(backlash_data, state_control == 1 & protest_sample == 1)[c("cowcode","year","month","year_month","backlash_no","protest4_no","viol_communal_no","conc_sum","conc_sum_symbolic","conc_sum_nonsymbolic","conc_h_sum_symbolic","conc_h_sum_nonsymbolic","conc_v_sum_symbolic","conc_v_sum_nonsymbolic")])
for (i in c("backlash_no","protest4_no","viol_communal_no","conc_sum","conc_sum_symbolic","conc_sum_nonsymbolic","conc_h_sum_symbolic","conc_h_sum_nonsymbolic","conc_v_sum_symbolic","conc_v_sum_nonsymbolic")) {
  step1 <- paste("conc_plot_data <- conc_plot_data %>% group_by(year_month) %>% mutate(",i," = sum(",i,"))",sep="")
  eval(parse(text=c(step1)))
}
conc_plot_data <- unique(conc_plot_data[c("year","month","year_month","backlash_no","protest4_no","viol_communal_no","conc_sum_symbolic","conc_sum_nonsymbolic","conc_h_sum_symbolic","conc_h_sum_nonsymbolic","conc_v_sum_symbolic","conc_v_sum_nonsymbolic")])
conc_plot_data <- reshape2::melt(data.frame(conc_plot_data), id=c("year","month","year_month")) 
conc_plot_data <- subset(conc_plot_data, variable == "conc_h_sum_symbolic" | variable == "conc_h_sum_nonsymbolic" | variable == "conc_v_sum_symbolic" | variable == "conc_v_sum_nonsymbolic")
conc_plot_data$variable <- as.factor(paste(conc_plot_data$variable))
conc_plot_data$variable = factor(conc_plot_data$variable,levels(conc_plot_data$variable)[c(3,1,4,2)])
concessions_plot <- ggplot(conc_plot_data, aes(x = year_month, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(25,85,145,205,265,325), labels=c("1990","1995","2000","2005","2010","2015")) +
  scale_y_continuous(breaks=c(0, 5, 10), labels=c("0","5","10")) +
  theme_bw() + theme(plot.title = element_text(size=12), text=element_text(family="Times"), legend.position = "bottom", legend.direction="horizontal") + xlab("year") + ylab("number of concessions") +
  scale_fill_manual(name = "concession type", values = c("#9ecae1","#3182bd","#fdae6b","#e6550d"), labels = c("group-blind, vertical", "group-blind, horizontal", "group-based, vertical","group-based, horizontal"))+ 
  guides(fill = guide_legend(nrow=2,reverse = TRUE,byrow=TRUE))
ggsave(concessions_plot + ggtitle(""), file='./figures/descriptives/concessions.pdf', width = 17, height = 8, units="cm",dpi=600)
ggsave(concessions_plot + ggtitle(""), file='./figures/descriptives/concessions.png', width = 17, height = 8, units="cm",dpi=600)

### Monthly backlashes (Figure 2) ###
backlash_plot_data <- unique(subset(backlash_data, state_control == 1 & protest_sample == 1)[c("cowcode","year","month","year_month","backlash_no","protest4_no","viol_communal_no","conc_sum","conc_sum_symbolic","conc_sum_nonsymbolic","conc_h_sum_symbolic","conc_h_sum_nonsymbolic","conc_v_sum_symbolic","conc_v_sum_nonsymbolic")])
for (i in c("backlash_no","protest4_no","viol_communal_no","conc_sum","conc_sum_symbolic","conc_sum_nonsymbolic","conc_h_sum_symbolic","conc_h_sum_nonsymbolic","conc_v_sum_symbolic","conc_v_sum_nonsymbolic")) {
  step1 <- paste("backlash_plot_data <- backlash_plot_data %>% group_by(year_month) %>% mutate(",i," = sum(",i,"))",sep="")
  eval(parse(text=c(step1)))
}
backlash_plot_data <- unique(backlash_plot_data[c("year","month","year_month","backlash_no","protest4_no","viol_communal_no","conc_sum_symbolic","conc_sum_nonsymbolic","conc_h_sum_symbolic","conc_h_sum_nonsymbolic","conc_v_sum_symbolic","conc_v_sum_nonsymbolic")])
backlash_plot_data <- reshape2::melt(data.frame(backlash_plot_data), id=c("year","month","year_month")) 
backlash_plot_data <- subset(backlash_plot_data, variable == "protest4_no" | variable == "viol_communal_no")
backlash_plot_data$variable <- as.factor(paste(backlash_plot_data$variable))
backlash_plot <- ggplot(backlash_plot_data, aes(x = year_month, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(25,85,145,205,265,325), labels=c("1990","1995","2000","2005","2010","2015")) +
  theme_bw() + theme(plot.title = element_text(size=12), text=element_text(family="Times"), legend.position = "bottom") + xlab("year") + ylab("number of events") +
  scale_fill_manual(name = "mobilization type", values = c("#bdbdbd","#636363"), labels = c("anti-government protest","targeted violence"))+ 
  guides(fill = guide_legend(reverse = F))
ggsave(backlash_plot + ggtitle(""), file='./figures/descriptives/mobilization.pdf', width = 17, height = 8, units="cm",dpi=600)
ggsave(backlash_plot + ggtitle(""), file='./figures/descriptives/mobilization.png', width = 17, height = 8, units="cm",dpi=600)

### Descriptive statistics (Table 2) ###
all_vars <- c("backlash_no", "protest4_no", "viol_communal_no", "conc_sum_close3", "conc_sum_symbolic_close3","conc_sum_nonsymbolic_close3", shocks3, controls_c, "lunreg_backlash_no", "nobacklash_months_l1", "year")
all_vars <- unique(all_vars)
descriptives <- subset(backlash_data, state_control == 1 & protest_sample == 1)[,all_vars]
descriptives <- descriptives[complete.cases(descriptives),]
descriptives <- data.frame(descriptives)
stargazer(descriptives, type="text", title="Descriptive statistics.", style = "ajps", 
          covariate.labels = c("No. DG mobilization events","No. DG anti-government protests", "No. DG violent incidents" ,"Concession number","Concession number (group-based)","Concession number (group-blind)","Months to next election (log)","Recent subordinate group protest","Recent civil violence","Battle deaths (last 10y, log)","DN party","DN party in government","Democracy level","Abs. size (log)","GDP p.c. (log)","GDP growth","Regional DG mobilization events (log)","Months without DG mobilization","Year"),
          out = c("./tables/descriptives/descriptive_stats.tex","./tables/descriptives/descriptive_stats.html"))


######################################################################
## Run main analysis (Figure 3, Table 3) ##
######################################################################

main_analysis <- run_analysis()


######################################################################
## Appendix 1: Reverse causation and endogeneity ##
######################################################################

### Appendix 1.1: Reverse analysis (Table A1, Figure A1) ###
bl_conc <- glm(as.formula(paste("conc", paste(c("ld60_d13_backlash_no","ld60_d13_nsc_viol_civil_no","ld60_d13_nsc_protest2_no","ltt_nextelec",controls_c,"no_conc_months_l1","I(no_conc_months_l1^2)","I(no_conc_months_l1^3)","cowcodef","yearf"), collapse = " + "), sep = " ~ ")), family = "binomial", data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cl_bl_conc <- data.frame(cluster.se(bl_conc, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))[, 2])
bl_conc_symbolic <- glm(as.formula(paste("conc_symbolic", paste(c("ld60_d13_backlash_no","ld60_d13_nsc_viol_civil_no","ld60_d13_nsc_protest2_no","ltt_nextelec",controls_c,"no_conc_symbolic_months_l1","I(no_conc_symbolic_months_l1^2)","I(no_conc_symbolic_months_l1^3)","cowcodef","yearf"), collapse = " + "), sep = " ~ ")), family = "binomial", data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cl_bl_conc_symbolic <- data.frame(cluster.se(bl_conc_symbolic, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))[, 2])
bl_conc_nonsymbolic <- glm(as.formula(paste("conc_nonsymbolic", paste(c("ld60_d13_backlash_no","ld60_d13_nsc_viol_civil_no","ld60_d13_nsc_protest2_no","ltt_nextelec",controls_c,"no_conc_nonsymbolic_months_l1","I(no_conc_nonsymbolic_months_l1^2)","I(no_conc_nonsymbolic_months_l1^3)","cowcodef","yearf"), collapse = " + "), sep = " ~ ")), family = "binomial", data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cl_bl_conc_nonsymbolic <- data.frame(cluster.se(bl_conc_nonsymbolic, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))[, 2])
stargazer(bl_conc, bl_conc_symbolic, bl_conc_nonsymbolic,  
          se = c(cl_bl_conc, cl_bl_conc_symbolic, cl_bl_conc_nonsymbolic), 
          dep.var.labels.include = F, type="text",  order=c("d60_",vars.order),  model.names = F, model.numbers = F, 
          column.labels = c("Model 1 Concession", "Model 2 Concession (group-based)", "Model 3 Concession (group-blind)"), 
          title="Reverse analysis: Dominant group mobilization, subordinate group civil violence and protests, and the provision of concessions to subordinate groups.", omit=c("cowcodef", "yearf","nobacklash_months_l1_l1","nobacklash_violent_months_l1_l1"), style = "ajps", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), add.lines = list(c("Country-FE","yes","yes","yes","yes"),c("Year-FE","yes","yes","yes","yes")), 
          covariate.labels = c("Dominant group mobilization events (last 5y, log)","Subordinate group civil violence incidents (last 5y, log)","Subordinate group protests (last 5y, log)","DN party","DN party in government","Months to next election (log)","Battle deaths (last 10y, log)","Democracy level","Abs. size (log)","GDP p.c. (log)","GDP growth"), notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE's in parentheses; cubic terms for country-wise years without (group-based/group-blind) concessions included but not reported."), notes.append=FALSE, out = c("./tables/results/app1.1_reverse.html", "./tables/results/app1.1_reverse.tex"))
bl_conc_me <- avg_comparisons(bl_conc, variables = list(ld60_d13_backlash_no = "minmax", ld60_d13_nsc_viol_civil_no = "minmax", ld60_d13_nsc_protest2_no = "minmax"), vcov = cluster.vcov(bl_conc, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)))
bl_conc_me$term <- c("dominant group\nmobilization events","subordinate group\nprotests","subordinate group\ncivil violence incidents")
bl_conc_me$depvar <- "concession"
bl_conc_me$xpos <- 1
bl_conc_symbolic_me <- avg_comparisons(bl_conc_symbolic, variables = list(ld60_d13_backlash_no = "minmax", ld60_d13_nsc_viol_civil_no = "minmax", ld60_d13_nsc_protest2_no = "minmax"), vcov = cluster.vcov(bl_conc_symbolic, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)))
bl_conc_symbolic_me$term <- c("dominant group\nmobilization events","subordinate group\nprotests","subordinate group\ncivil violence incidents")
bl_conc_symbolic_me$depvar <- "concession (group-based)"
bl_conc_symbolic_me$xpos <- 2
bl_conc_nonsymbolic_me <- avg_comparisons(bl_conc_nonsymbolic, variables = list(ld60_d13_backlash_no = "minmax", ld60_d13_nsc_viol_civil_no = "minmax", ld60_d13_nsc_protest2_no = "minmax"), vcov = cluster.vcov(bl_conc_nonsymbolic, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)))
bl_conc_nonsymbolic_me$term <- c("dominant group\nmobilization events","subordinate group\nprotests","subordinate group\ncivil violence incidents")
bl_conc_nonsymbolic_me$depvar <- "concession (group-blind)"
bl_conc_nonsymbolic_me$xpos <- 3
reverse_me <- rbind.fill(bl_conc_me, bl_conc_symbolic_me, bl_conc_nonsymbolic_me)
reverse_me$upper = reverse_me$estimate + 1.645 * reverse_me$std.error
reverse_me$lower = reverse_me$estimate - 1.645 * reverse_me$std.error
reverse_me$AME = reverse_me$estimate
reverse_me$xpos <- ifelse(reverse_me$term == "dominant group\nmobilization events", reverse_me$xpos - 0.2, reverse_me$xpos)
reverse_me$xpos <- ifelse(reverse_me$term == "subordinate group\nprotests", reverse_me$xpos + 0.2, reverse_me$xpos)
reverse_me_plot <- ggplot(data = reverse_me)+
  geom_errorbar(mapping=aes(x=xpos, ymin=upper, ymax=lower, group = factor(term), colour = factor(term)), width=0.15, size=1) + 
  geom_point(mapping=aes(x=xpos, y=AME, group = factor(term), colour = factor(term), fill = factor(term), shape = factor(term)), size=4) +
  theme_bw() + theme(plot.title = element_text(size=12), axis.title=element_text(size=8)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(text=element_text(family="Times"), plot.title =element_text(size=12), legend.position = "bottom") + 
  xlab("type of concession") + ylab("change in predicted\nprobability of concession") +
  scale_colour_manual(values = c("black","darkgrey","darkgrey"))+ 
  scale_fill_manual(values = c("black","darkgrey","darkgrey")) + 
  guides(colour = guide_legend(reverse = F, title=NULL), fill = guide_legend(reverse = F, title=NULL), shape = guide_legend(reverse = F, title=NULL) ) +
  scale_x_continuous(breaks=c(1,2,3), labels=c("concession","concession (group-based)","concession (group-blind)"))
ggsave(reverse_me_plot, file='./figures/margins/app1.1_reverse_analysis_plot.pdf', width = 17, height = 7, units="cm",dpi=600)
ggsave(reverse_me_plot, file='./figures/margins/app1.1_reverse_analysis_plot.png', width = 17, height = 7, units="cm",dpi=600)

### Appendix 1.2: Causal sensitivity analyses (Figure A2) ###

#### a) m1a (concessions)
#dof (country clusters)
dof_est <- subset(backlash_data, state_control == 1 & protest_sample == 1)
dof_est <- unique(dof_est[c("cowcode")])
#outcome regression
sens_m1a_3 <- lm(as.formula(paste("backlash_no", paste(c("conc_sum_close3",shocks3,controls_c,backlash_months,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cluster.se(sens_m1a_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))#t statistic of minority protest: 2.6581; t statistic of conc_sum_close3: 2.2129 / coefficient 7.9169e-02 / se: 3.5776e-02
#treatment regression
sens_m1a_3_treatment <- lm(as.formula(paste("conc_sum_close3", paste(c(shocks3,controls_c,backlash_months,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cluster.se(sens_m1a_3_treatment, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))#t statistic of minority protest: 2.2322
#compute (sensemakr manual, page 17)
r2yxj.dx <- partial_r2(t_statistic = 2.6581, dof = nrow(dof_est)-1)#outcome
r2dxj.x <- partial_r2(t_statistic = 2.2322, dof = nrow(dof_est)-1)#treatment
bounds <- ovb_partial_r2_bound(r2dxj.x = r2dxj.x, r2yxj.dx = r2yxj.dx, kd = 1:3, ky = 1:3, bound_label = paste(1:3, "x", "benchmark"))
bound.values <- adjusted_estimate(estimate = 7.9169e-02,se =3.5776e-02,dof = nrow(dof_est)-1,r2dz.x = bounds$r2dz.x,r2yz.dx = bounds$r2yz.dx)
jpeg("./figures/sensitivity/app1.2a_sens_m1a_3.png", width = 10, height = 10, units="cm", res=300)
ovb_contour_plot(estimate = 7.9169e-02,se = 3.5776e-02,dof = nrow(dof_est)-1, sensitivity.of = "t-value",t.threshold = 1.645)
add_bound_to_contour(bounds, bound_value = bound.values)
dev.off()
#### b) m2a (group-based concessions)
#outcome regression
sens_m2a_3_group <- lm(as.formula(paste("backlash_no", paste(c("conc_sum_symbolic_close3",shocks3,controls_c,backlash_months,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cluster.se(sens_m2a_3_group, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))#dof: 28538; t statistic of minority protest: 2.6549; conc_symbolic_close3:  coefficient 1.6862e-01 / se: 6.1414e-02
#treatment regression
sens_m1a_3_group_treatment <- lm(as.formula(paste("conc_sum_symbolic_close3", paste(c(shocks3,controls_c,backlash_months,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cluster.se(sens_m1a_3_group_treatment, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))#dof: 28539; t statistic of minority protest: 1.9761
#compute (sensemakr manual, page 17)
r2yxj.dx <- partial_r2(t_statistic = 2.6549, dof = nrow(dof_est)-1)#outcome
r2dxj.x <- partial_r2(t_statistic = 1.9761, dof = nrow(dof_est)-1)#treatment
bounds <- ovb_partial_r2_bound(r2dxj.x = r2dxj.x, r2yxj.dx = r2yxj.dx, kd = 1:3, ky = 1:3, bound_label = paste(1:3, "x", "benchmark"))
bound.values <- adjusted_estimate(estimate = 1.6862e-01,se = 6.1414e-02,dof = nrow(dof_est)-1,r2dz.x = bounds$r2dz.x,r2yz.dx = bounds$r2yz.dx)
jpeg("./figures/sensitivity/app1.2b_sens_m2a_3_group.png", width = 10, height = 10, units="cm", res=300)
ovb_contour_plot(estimate = 1.6862e-01,se = 6.1414e-02,dof = nrow(dof_est)-1, sensitivity.of = "t-value",t.threshold = 1.645) #,lim = 0.15, lim.y = 0.6
add_bound_to_contour(bounds, bound_value = bound.values)
dev.off()

### Appendix 1.3: Difference-in-differences approach (Figure A3) ###
m1a_3_fect <- fect(data = subset(backlash_data, state_control == 1 & protest_sample == 1),
                   as.formula(paste("backlash_no", paste(c("conc_close3",shocks3,backlash_months), collapse = " + "), sep = " ~ ")),
                   index = c("cowcode", "year_month"),method = "fe",se = T,alpha=0.1,na.rm = F,min.T0 = 1
)
m2a_3_symbolic_fect <- fect(data = subset(backlash_data, state_control == 1 & protest_sample == 1),
                            as.formula(paste("backlash_no", paste(c("conc_symbolic_close3","conc_nonsymbolic_close3",shocks3,backlash_months), collapse = " + "), sep = " ~ ")),
                            index = c("cowcode", "year_month"),method = "fe",se = T,alpha=0.1,na.rm = F,min.T0 = 1
)
m2a_3_nonsymbolic_fect <- fect(data = subset(backlash_data, state_control == 1 & protest_sample == 1),
                               as.formula(paste("backlash_no", paste(c("conc_nonsymbolic_close3","conc_symbolic_close3",shocks3,backlash_months), collapse = " + "), sep = " ~ ")),
                               index = c("cowcode", "year_month"),method = "fe",se = T,alpha=0.1,na.rm = F,min.T0 = 1
)
m1a_3_fect_out <- get_texreg(m1a_3_fect, mnm_data2)
m2a_3_symbolic_fect_out <- get_texreg(m2a_3_symbolic_fect, mnm_data2)
m2a_3_nonsymbolic_fect_out <- get_texreg(m2a_3_nonsymbolic_fect, mnm_data2)
var.labs = list(conc_close3 = "Concession", 
                conc_symbolic_close3 = "Concession (group-based)", 
                  conc_nonsymbolic_close3 =  "Concession (group-blind)")
texreg(list(m1a_3_fect_out, m2a_3_symbolic_fect_out, m2a_3_nonsymbolic_fect_out),
       file="./tables/results/app1.3_results_fect.tex",
       caption = "Ethnic accommodation and the number of mobilization events involving the dominant group [FECT estimates].",
       custom.coef.map = var.labs,
       booktabs = T, use.packages = F, digits = 2,
       label = "tab:results_m2",
       caption.above = T,
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "+",
       custom.gof.rows = list(
         "Method" = c("FECT", "FECT","FECT")
       ) 
)
#plot event plot
m1a_3_fect_plot <- plot(m1a_3_fect, main = "",type = "gap", count =F,ylab = "effect estimate", xlab ="time before/after 'treatment'",plot.ci="0.9", start0=T, xlim = c(-20,20)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text=element_text(family='Times'), legend.position="bottom", plot.title = element_text(size=10), axis.title = element_text(size=10), axis.text = element_text(size=10)) + 
  ylab("change in predicted\nnumber of dominant\ngroup mobilization events") +  xlab("months before/after concession")
m2a_3_symbolic_fect_plot <- plot(m2a_3_symbolic_fect, main = "",type = "gap", count =F,ylab = "effect estimate", xlab ="time before/after 'treatment'",plot.ci="0.9", start0=T, xlim = c(-20,20)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text=element_text(family='Times'), legend.position="bottom", plot.title = element_text(size=10), axis.title = element_text(size=10), axis.text = element_text(size=10)) + 
  ylab("change in predicted\nnumber of dominant\ngroup mobilization events") +  xlab("months before/after group-based concession")
m2a_3_nonsymbolic_fect_plot <- plot(m2a_3_nonsymbolic_fect, main = "",type = "gap", count =F,ylab = "effect estimate", xlab ="time before/after 'treatment'",plot.ci="0.9", start0=T, xlim = c(-20,20)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text=element_text(family='Times'), legend.position="bottom", plot.title = element_text(size=10), axis.title = element_text(size=10), axis.text = element_text(size=10)) + 
  ylab("change in predicted\nnumber of dominant\ngroup mobilization events") +  xlab("months before/after group-blind concession")
fect_plot <- grid.arrange(m1a_3_fect_plot + ggtitle("a) All concessions"), 
                          m2a_3_symbolic_fect_plot + ggtitle("b) Group-based concessions"), 
                          m2a_3_nonsymbolic_fect_plot + ggtitle("c) Group-blind concessions"), 
                          nrow = 3, ncol = 1)
ggsave(fect_plot, file='./figures/margins/app1.3_fect_plot.pdf', width = 17, height = 15, units="cm",dpi=600)
ggsave(fect_plot, file='./figures/margins/app1.3_fect_plot.png', width = 17, height = 15, units="cm",dpi=600)

### Appendix 1.4: Subsample analyses (post-conflict vs stable contexts) ###
#conflict contexts
cont1_m1a_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_close3",shocks3,controls_c,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, d10_violence_civil_c == 1 & state_control == 1 & protest_sample == 1))
cl_cont1_m1a_3 <- data.frame(cluster.se(cont1_m1a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 1 & state_control == 1 & protest_sample == 1)$cowcode))[, 2])
cont1_m2a_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_symbolic_close3","conc_sum_nonsymbolic_close3",shocks3,controls_c,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, d10_violence_civil_c == 1 & state_control == 1 & protest_sample == 1))
cl_cont1_m2a_3 <- data.frame(cluster.se(cont1_m2a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 1 & state_control == 1 & protest_sample == 1)$cowcode))[, 2])
#stable contexts
cont2_m1a_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_close3",shocks3,controls_c,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, d10_violence_civil_c == 0 & state_control == 1 & protest_sample == 1))
cl_cont2_m1a_3 <- data.frame(cluster.se(cont2_m1a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 0 & state_control == 1 & protest_sample == 1)$cowcode))[, 2])
cont2_m2a_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_symbolic_close3","conc_sum_nonsymbolic_close3",shocks3,controls_c,"cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, d10_violence_civil_c == 0 & state_control == 1 & protest_sample == 1))
cl_cont2_m2a_3 <- data.frame(cluster.se(cont2_m2a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 0 & state_control == 1 & protest_sample == 1)$cowcode))[, 2])

### Results appendices 1.3 and 1.4 ###
stargazer(cont2_m1a_3, cont2_m2a_3, cont1_m1a_3, cont1_m2a_3,  
          se = c(cl_cont2_m1a_3, cl_cont2_m2a_3, cl_cont1_m1a_3, cl_cont1_m2a_3), 
          type="text", dep.var.labels.include = F,  order=vars.order,  model.names = F, model.numbers = F, 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"), title="Ethnic accommodation and the number of mobilization events involving the dominant group [post-conflict vs. stable contexts subsample analyses].", 
          omit=c("cowcodef", "yearf","nobacklash_months_l1","nobacklash_violent_months_l1"), style = "ajps", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), 
          notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE's in parentheses; cubic terms for group-wise months without mobilization included but not reported."), notes.append=FALSE,
          out = c("./tables/results/app1.3_1.4_endo.html", "./tables/results/app1.3_1.4_endo.tex"))

### Margins appendices 1.3 and 1.4
cont2_m1a_3_me <- avg_comparisons(cont2_m1a_3, variables = list(conc_sum_close3 = c(0,1)), vcov = cluster.vcov(cont2_m1a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 0 & state_control == 1 & protest_sample == 1)$cowcode)))
cont2_m1a_3_me$term <- "concession number"
cont2_m2a_3_me <- avg_comparisons(cont2_m2a_3,  variables = list(conc_sum_symbolic_close3 = c(0,1), conc_sum_nonsymbolic_close3 = c(0,1)), vcov = cluster.vcov(cont2_m2a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 0 & state_control == 1 & protest_sample == 1)$cowcode)))
cont2_m2a_3_me$term <- c("concession number (group-blind)", "concession number (group-based)")
cont1_m1a_3_me <- avg_comparisons(cont1_m1a_3, variables = list(conc_sum_close3 = c(0,1)), vcov = cluster.vcov(cont1_m1a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 1 & state_control == 1 & protest_sample == 1)$cowcode)))
cont1_m1a_3_me$term <- "concession number"
cont1_m2a_3_me <- avg_comparisons(cont1_m2a_3,  variables = list(conc_sum_symbolic_close3 = c(0,1), conc_sum_nonsymbolic_close3 = c(0,1)), vcov = cluster.vcov(cont1_m2a_3, as.integer(subset(backlash_data, d10_violence_civil_c == 1 & state_control == 1 & protest_sample == 1)$cowcode)))
cont1_m2a_3_me$term <- c("concession number (group-blind)", "concession number (group-based)")
cowy_cont12_m_me <- rbind.fill(cont1_m1a_3_me, cont1_m2a_3_me, cont2_m1a_3_me, cont2_m2a_3_me)
cowy_cont12_m_me$upper = cowy_cont12_m_me$estimate + 1.645 * cowy_cont12_m_me$std.error
cowy_cont12_m_me$lower  =cowy_cont12_m_me$estimate - 1.645 * cowy_cont12_m_me$std.error
cowy_cont12_m_me$AME = cowy_cont12_m_me$estimate
cowy_cont12_m_me$context <- rep(c(1,2),each=3)
cowy_cont12_m_me$context <- ifelse(cowy_cont12_m_me$term == "concession number", cowy_cont12_m_me$context - 0.2, cowy_cont12_m_me$context)
cowy_cont12_m_me$context <- ifelse(cowy_cont12_m_me$term == "concession number (group-blind)", cowy_cont12_m_me$context + 0.2, cowy_cont12_m_me$context)
cowy_cont12_m_me_plot <- ggplot(data = cowy_cont12_m_me)+
  geom_errorbar(mapping=aes(x=context, ymin=upper, ymax=lower, group = factor(term), colour = factor(term)), width=0.15, size=1) + 
  geom_point(mapping=aes(x=context, y=AME, group = factor(term), colour = factor(term), fill = factor(term), shape = factor(term)), size=4) +
  theme_bw() + theme(plot.title = element_text(size=12), axis.title=element_text(size=8)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(text=element_text(family="Times"), plot.title =element_text(size=12), legend.position = "bottom") + 
  xlab("specification") + ylab("change in predicted number of\ndominant group mobilization events") +
  scale_colour_manual(values = c("#8856a7","#e34a33","#2b8cbe"))+ 
  scale_fill_manual(values = c("#8856a7","#e34a33","#2b8cbe")) + 
  guides(colour = guide_legend(reverse = F, title=NULL), fill = guide_legend(reverse = F, title=NULL), shape = guide_legend(reverse = F, title=NULL) ) +
  scale_x_continuous(breaks=c(1,2), labels=c("subsample:\npost-conflict","subsample:\nstable"))
ggsave(cowy_cont12_m_me_plot, file='./figures/margins/app1.4_subsamples.png', width = 17, height = 7, units="cm",dpi=600)
ggsave(cowy_cont12_m_me_plot, file='./figures/margins/app1.4_subsamples.pdf', width = 17, height = 7, units="cm",dpi=600)


######################################################################
# Appendix 2: Alternative explanations #
######################################################################

### Appendix 2.1: Major constitutional and political shifts (Figure A5)
results_app2.1_controls_shift <- run_analysis(term_controls_additional = c("month_const_shock3_close3","month_election_shock3_close3","month_coup_shock3_close3","pcf_shock_close3","month_govsup_change_shock3_close3"),
                            model_label = "app2.1_controls_shift", model_name = " [Additional controls for major constitutional and political shfits]")

### Appendix 2.2: Mobilization unrelated to ethnic issues (Figure A6)
results_app2.2_protest_psi <- run_analysis(DV = "I(protest4_psi1_no+viol_communal_no)", model = "poisson",#convergence issues so switched to poisson
                                           model_label = "app2.2_protest_psi", model_name = " [protests related to institutions and minority rights]")

### Appendix 2.3: Violence initiated by subordinate groups (Figure A7)
results_app2.3_minviol <- run_analysis(DV = "protest4_no",
                            model_label = "app2.3_minviol", model_name = " [Alternative dependent variable: anti-government protests]")

### Appendix 2.4: Elite opposition to restrictive, ethnically based power-sharing (Figures A8-A9)
results_app2.4_conch <- run_analysis(term_conc = "conc_h_sum",
                                   model_label = "app2.4_conch", model_name = " [horizontal concessions]")
results_app2.4_concv <- run_analysis(term_conc = "conc_v_sum",
                                   model_label = "app2.4_concv", model_name = " [vertical concessions]")

######################################################################
# Appendix 3.1: Alterations to independent variables #
######################################################################

### time window: 1 month (Figure A10) ###
results_app3.1_tw1 <- run_analysis(time_window = "_close1",
                                   model_label = "app3.1_tw1", model_name = " [1-month time window]")

### time window: 2 months (Figure A11) ###
results_app3.1_tw2 <- run_analysis(time_window = "_close2",
                                   model_label = "app3.1_tw2", model_name = " [2-month time window]")

### time window: 6 months (Figure A12) ###
results_app3.1_tw6 <- run_analysis(time_window = "_close6",
                                   model_label = "app3.1_tw6", model_name = " [6-month time window]")

### time window: 12 months (Figure A13) ###
results_app3.1_tw12 <- run_analysis(time_window = "_close12",
                                   model_label = "app3.1_tw12", model_name = " [12-month time window]")

### dichotomous concession indicator (Figure A14) ###
results_app3.1_dummy <- run_analysis(term_conc = "conc",
                                   model_label = "app3.1_dummy", model_name = " [dichotomous indicator]")

### only concessions in the *past* three months (Figure A15) ###
results_app3.1_bwd <- run_analysis(term_conc = "conc_sum_bwd", term_time = c(),
                                     model_label = "app3.1_bwd", model_name = " [only past concessions]")

### continuous measure for the logged number of dominant nationalist parties (Figure A16, Table A2) ###
ldnp_m1a_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_close3", shocks3, controls_c_lmnm_partyno, backlash_months, "cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
ldnp_m1b_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_close3 * lmnm_partyno", shocks3, controls_c_lmnm_partyno, backlash_months, "cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
ldnp_m2a_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_symbolic_close3", "conc_sum_nonsymbolic_close3", shocks3, controls_c_lmnm_partyno, backlash_months, "cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
ldnp_m2b_3 <- glm.nb(as.formula(paste("backlash_no", paste(c("conc_sum_symbolic_close3 * lmnm_partyno", "conc_sum_nonsymbolic_close3 * lmnm_partyno", shocks3, controls_c_lmnm_partyno, backlash_months, "cowcodef","yearf"), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = subset(backlash_data, state_control == 1 & protest_sample == 1))
cl_ldnp_m1a_3 <- data.frame(cluster.se(ldnp_m1a_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))[, 2])
cl_ldnp_m1b_3 <- data.frame(cluster.se(ldnp_m1b_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))[, 2])
cl_ldnp_m2a_3 <- data.frame(cluster.se(ldnp_m2a_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))[, 2])
cl_ldnp_m2b_3 <- data.frame(cluster.se(ldnp_m2b_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))[, 2])
ldnp_m1b_3_jointsig <- linearHypothesis(ldnp_m1b_3, "conc_sum_close3 + conc_sum_close3:lmnm_partyno = 0", test = "Chisq", vcov = cluster.vcov(ldnp_m1b_3, subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))
ldnp_m2b_3_jointsig1 <- linearHypothesis(ldnp_m2b_3, "conc_sum_symbolic_close3 + conc_sum_symbolic_close3:lmnm_partyno = 0", test = "Chisq", vcov = cluster.vcov(ldnp_m2b_3, subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))
ldnp_m2b_3_jointsig2 <- linearHypothesis(ldnp_m2b_3, "conc_sum_nonsymbolic_close3 + lmnm_partyno:conc_sum_nonsymbolic_close3 = 0", test = "Chisq", vcov = cluster.vcov(ldnp_m2b_3, subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode))
stargazer(ldnp_m1a_3, ldnp_m1b_3, ldnp_m2a_3, ldnp_m2b_3,  se = c(cl_ldnp_m1a_3, cl_ldnp_m1b_3, cl_ldnp_m2a_3, cl_ldnp_m2b_3),
          type="text", dep.var.labels.include = F,  order=vars.order,  model.names = F, model.numbers = F,
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
          title=paste("Ethnic accommodation and the number of mobilization events involving the dominant group [interaction with logged number of dominant nationalist parties].", sep =""),
          omit=c("cowcodef", "yearf","nobacklash_months_l1","nobacklash_violent_months_l1"), style = "ajps",
          star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"),
          add.lines = list(c("Country-FE","yes","yes","yes","yes"),
                           c("Year-FE","yes","yes","yes","yes"),
                           c("Wald-Test Chisq","","","",""),
                           c("Joint sig. int. concession","",add_stars(round(ldnp_m1b_3_jointsig[4][2,],3)),"",""),
                           c("Joint sig. int. concession (group-based)","","","",add_stars(ldnp_m2b_3_jointsig1[4][2,])),
                           c("Joint sig. int. concession (group-blind)","","","",add_stars(ldnp_m2b_3_jointsig2[4][2,]))
          ),
          covariate.labels = c(labels_conc_default, labels_dnp_default, labels_controls_default),
          notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE's in parentheses; cubic terms for group-wise months without mobilization included but not reported."), notes.append=FALSE,
          out = c("./tables/results/app3.1_ldnp.html","./tables/results/app3.1_ldnp.tex")
)
ldnp_m1a_3_me <- avg_comparisons(ldnp_m1a_3, variables = list(conc_sum_close3 = c(0,1)), vcov = cluster.vcov(ldnp_m1a_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)))
ldnp_m1a_3_me$term <- "concession"
ldnp_m1a_3_me$mnm_partyno <- -1.5
ldnp_m1b_3_me <- avg_comparisons(ldnp_m1b_3,  variables = list(conc_sum_close3 = c(0,1)), by = "lmnm_partyno", vcov = cluster.vcov(ldnp_m1b_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)))
ldnp_m1b_3_me$term <- "concession"
ldnp_m1b_3_me$mnm_partyno <- seq(0, 10)
ldnp_m2a_3_me <- avg_comparisons(ldnp_m2a_3,  variables = list(conc_sum_symbolic_close3 = c(0,1), conc_sum_nonsymbolic_close3 = c(0,1)), vcov = cluster.vcov(ldnp_m2a_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)))
ldnp_m2a_3_me$term <- c("concession (group-blind)", "concession (group-based)")
ldnp_m2a_3_me$mnm_partyno <- -1.5
ldnp_m2b_3_me <- avg_comparisons(ldnp_m2b_3, variables = list(conc_sum_symbolic_close3 = c(0,1), conc_sum_nonsymbolic_close3 = c(0,1)), by = "lmnm_partyno", vcov = cluster.vcov(ldnp_m2b_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)))
ldnp_m2b_3_me$term <- rep(c("concession (group-blind)", "concession (group-based)"),each=11)
ldnp_m2b_3_me$mnm_partyno <- rep(seq(0, 10),2)
m_me <- rbind.fill(ldnp_m1a_3_me, ldnp_m2a_3_me, ldnp_m1b_3_me, ldnp_m2b_3_me)
m_me$mnm_partyno <- ifelse(m_me$term == "concession", m_me$mnm_partyno - 0.2, m_me$mnm_partyno)
m_me$mnm_partyno <- ifelse(m_me$term == "concession (group-blind)", m_me$mnm_partyno + 0.2, m_me$mnm_partyno)
m_me$upper = m_me$estimate + 1.645 * m_me$std.error
m_me$lower  =m_me$estimate - 1.645 * m_me$std.error
m_me$AME = m_me$estimate
m_me_plot <- ggplot(data = subset(m_me, mnm_partyno <= 5.2))+
  geom_errorbar(mapping=aes(x=mnm_partyno, ymin=upper, ymax=lower, group = factor(term), colour = factor(term)), width=0.15, size=1) + 
  geom_point(mapping=aes(x=mnm_partyno, y=AME, group = factor(term), colour = factor(term), fill = factor(term), shape = factor(term)), size=4) +
  theme_bw() + theme(plot.title = element_text(size=12), axis.title=element_text(size=8)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(text=element_text(family="Times"), plot.title =element_text(size=12), legend.position = "bottom") + 
  xlab("number of dominant nationalist parties") + ylab("change in predicted number of\ndominant group mobilization events") +
  scale_colour_manual(values = c("#8856a7","#e34a33","#2b8cbe"))+ 
  scale_fill_manual(values = c("#8856a7","#e34a33","#2b8cbe")) + 
  guides(colour = guide_legend(reverse = F, title=NULL), fill = guide_legend(reverse = F, title=NULL), shape = guide_legend(reverse = F, title=NULL) ) +
  scale_x_continuous(breaks=c(-1.5, 0, 1, 2, 3, 4, 5), labels=c("unconditional","0","1","2","3","4","5"))
ggsave(m_me_plot, file='./figures/margins/app3.1_ldnp.png', width = 17, height = 7, units="cm",dpi=600)
ggsave(m_me_plot, file='./figures/margins/app3.1_ldnp.pdf', width = 17, height = 7, units="cm",dpi=600)

wald_ldnp_m1b_3_me <- avg_comparisons(ldnp_m1b_3,  variables = list(conc_sum_close3 = c(0,1)), by = "lmnm_partyno", vcov = cluster.vcov(ldnp_m1b_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)), hypothesis = "pairwise")
wald_ldnp_m2b_3_me_symbolic <- avg_comparisons(ldnp_m2b_3, variables = list(conc_sum_symbolic_close3 = c(0,1)), by = "lmnm_partyno", vcov = cluster.vcov(ldnp_m2b_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)), hypothesis = "pairwise")
wald_ldnp_m2b_3_me_nonsymbolic <- avg_comparisons(ldnp_m2b_3, variables = list(conc_sum_nonsymbolic_close3 = c(0,1)), by = "lmnm_partyno", vcov = cluster.vcov(ldnp_m2b_3, as.integer(subset(backlash_data, state_control == 1 & protest_sample == 1)$cowcode)), hypothesis = "pairwise")
wald_ldnp <- rbind(wald_ldnp_m1b_3_me, wald_ldnp_m2b_3_me_symbolic, wald_ldnp_m2b_3_me_nonsymbolic)
wald_ldnp$term_modified <- rep(c("0-1","0-2","0-3","0-4","0-5","0-6","0-7","0-8","0-9","0-10",
                                      "1-2","1-3","1-4","1-5","1-6","1-7","1-8","1-9","1-10",
                                      "2-3","2-4","2-5","2-6","2-7","2-8","2-9","2-10",
                                      "3-4","3-5","3-6","3-7","3-8","3-9","3-10",
                                      "4-5","4-6","4-7","4-8","4-9","4-10",
                                      "5-6","5-7","5-8","5-9","5-10",
                                      "6-7","6-8","6-9","6-10",
                                      "7-8","7-9","7-10",
                                      "8-9","8-10",
                                      "9-10"), 3)
wald_ldnp$description <- ""
for (i in 1:nrow(wald_ldnp)) {
  #print(i)
  wald_ldnp$description[i] <- paste(round(wald_ldnp$estimate[i],3), " (",round(wald_ldnp$std.error[i],3), ")", get_stars(wald_ldnp$p.value[i]), sep ="")
}
wald_ldnp$conc_type <- rep(c("Concession number", "Concession number (group-based)", "Concession number (group-blind)"), each =55)
wald_ldnp <- wald_ldnp[c("conc_type","term_modified","description")]
wald_ldnp2 <- spread(wald_ldnp, conc_type, description)
write.csv(wald_ldnp2, file ="./tables/results/app3.1_wald_interaction.csv")


######################################################################
# Appendix 3.2: Alterations to dependent variable (dominant group mobilization) #
######################################################################

### Only major dominant group mobilization events (Figure A17) ###
results_app3.2_major <- run_analysis(DV = "backlash_major_no",
                                   model_label = "app3.2_major", model_name = " [major mobilization incidents]")

### Only violent dominant group mobilization events (Figure A18) ###
results_app3.2_violent <- run_analysis(DV = "backlash_violent_no", term_time =c(),#convergence issues omitted time vars
                                      model_label = "app3.2_violent", model_name = " [violent mobilization events]")

### Only non-violent group mobilization events (Figure A19) ###
results_app3.2_nonviolent <- run_analysis(DV = "backlash_nonviolent_no",
                                       model_label = "app3.2_non_violent", model_name = " [non-violent mobilization events]")


######################################################################
# Appendix 3.3: Sample alterations #
######################################################################

### Any group that has been politically-dominant in the last 12 months (Figure A20) ###
results_app3.3_scd12 <- run_analysis(analysis_data = subset(backlash_data, state_control_last12m == 1 & protest_sample == 1), term_time = c(),
                                      model_label = "app3.3_sc_last12", model_name = " [including groups that lost dominance in the last 12 months]")

### Only demographic plurality groups (Figure A21) ###
results_app3.3_plurality <- run_analysis( analysis_data = subset(backlash_data, noplurality == 0 & protest_sample == 1), term_time = c(),
                                      model_label = "app3.3_plurality", model_name = " [only demographic plurality groups]")

### Only electoral and liberal democracies (Figure A22) ###
results_app3.3_democracies <- run_analysis(analysis_data = subset(backlash_data, v2x_regime > 1 & state_control == 1 & protest_sample == 1),
                                         model_label = "app3.3_democracies", model_name = " [only electoral or liberal democracies]")


######################################################################
# Appendix 3.4: Incorporation of additional control variables #
######################################################################

### Controlling for additional structural characteristics (Figure A23) ###
results_app3.4_ac1 <- run_analysis(term_controls_additional = c("tt_vdem2_aut","tt_vdem2_dem","unemployment_mod","gini"),
                                           model_label = "app3.4_ac1", model_name = " [additional structural controls]")

### Accounting for the wider bargaining environment (Figure A24) ###
results_app3.4_ac2 <- run_analysis(term_controls_additional = c("tek_sup_any_c","lsdm_any_c_no","size_diff_nsc","lstate_control_years"),
                                   model_label = "app3.4_ac2", model_name = " [additional bargaining controls]")

### Controlling for additional interactions with number of dominant nationalist parties (Figure A25) ###
results_app3.4_ac3 <- run_analysis(term_controls_additional = c("mnm_party_exists * tt_vdem2_dem","mnm_party_exists * ltt_nextelec","mnm_party_exists * month_const_shock3_close3"),
                                   model_label = "app3.4_ac3", model_name = " [additional controls for dominant nationalists' influence]")


######################################################################
# Appendix 3.5: Alternative specifications #
######################################################################

### Linear count model (logged number of dominant group mobilization events) (Figure A26) ###
results_app3.5_ols <- run_analysis(model = "ols",
                                     model_label = "app3.5_ols", model_name = " [OLS model]")

### Logistic model (incidence of at least one dominant group mobilization event) (Figure A27) ###
results_app3.5_logit <- run_analysis(DV = "backlash", model = "logit",
                                     model_label = "app3.5_logit", model_name = " [binary dependent variable and logistic specification]")

### Country year-level specification (Figure A28) ###
cydata <- subset(backlash_data, state_control == 1 & protest_sample == 1)
cydata <- cydata %>% group_by(cowcode,gwgroupid,year) %>% mutate(#summing up independent and dependent variable from country-month to country-year level
  backlash_no = sum(backlash_no),
  conc_sum_close3 = sum(conc_sum),
  conc_sum_symbolic_close3 = sum(conc_sum_symbolic),
  conc_sum_nonsymbolic_close3 = sum(conc_sum_nonsymbolic)
)
cydata <- unique(cydata[c("cowcode","gwgroupid","year","cowcodef","yearf","backlash_no","conc_sum_close3","conc_sum_symbolic_close3","conc_sum_nonsymbolic_close3",
                          "ld10_bdead_allc", "mnm_party_exists", "mnm_partygov",  "vdem_libdem", "lsize_abs", "lgdppc", "gdppc_change" )])
results_app3.5_cylevel <- run_analysis(analysis_data=cydata, term_shocks = c(), term_time = c(),
                                   model_label = "app3.5_cylevel", model_name = " [country year-level estimation]")



######################################################################
## Appendix 4: Individual-level analyses (Figure 4, Table A4) ##
######################################################################

### read in survey data ###
survey_data <- read.csv("./data/accommodation_backlash_surveydata.csv")

### add country and group data ###
backlash_data$gwgroupid <- as.character(backlash_data$gwgroupid)
survey_data2 <- left_join(survey_data, backlash_data[unique(c("cowcode","gwgroupid","year","month","year_month",
                                                                  "state_control","v2x_regime","tt_vdem2_dem","dem_ep",
                                                                  "conc_const_sum","conc_symbolic_const_sum","conc_nonsymbolic_const_sum",
                                                                  controls_c, shocks3))], 
                            by=c("gwgroupid","year","month"))

### define independent variables and sample indicator ###
survey_data2$conc_const_sum1 <- ifelse(survey_data2$conc_const_sum >= 1, 1, 0)
survey_data2$conc_symbolic_const_sum1 <- ifelse(survey_data2$conc_symbolic_const_sum >= 1, 1, 0)
survey_data2$conc_nonsymbolic_const_sum1 <- ifelse(survey_data2$conc_nonsymbolic_const_sum >= 1, 1, 0)
survey_data2$sample <- ifelse((survey_data2$v2x_regime > 0 |  survey_data2$tt_vdem2_dem == 1 | survey_data2$dem_ep == 1) & survey_data2$number_group0.8 >= 50, 1, 0)
survey_data2$cowcodef <- as.factor(paste(survey_data2$cowcode))
survey_data2$yearf <- as.factor(paste(survey_data2$year))
survey_data2$cowcode_year <- as.factor(paste(survey_data2$cowcode, survey_data2$year, sep=""))

### run individual level analyses ###
ind_m1a <- glmer(as.formula(paste("protest1", paste(c(re_fe, "conc_const_sum1", controls_c, shocks3, controls_i), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),nAGQ = 0, data = subset(survey_data2,state_control == 1 & sample == 1 & protest_d != 0))
ind_m1a_aut <- glmer(as.formula(paste("protest1", paste(c(re_fe, "conc_const_sum1 * authoritarian", controls_c, shocks3, controls_i), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),nAGQ = 0, data = subset(survey_data2,state_control == 1 & sample == 1 & protest_d != 0))
ind_m1a_rw <- glmer(as.formula(paste("protest1", paste(c(re_fe, "conc_const_sum1 * rightwing",controls_c, shocks3, controls_i), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),nAGQ = 0, data = subset(survey_data2,state_control == 1 & sample == 1 & protest_d != 0))
ind_m2a <- glmer(as.formula(paste("protest1", paste(c(re_fe,"conc_symbolic_const_sum1","conc_nonsymbolic_const_sum1", controls_c, shocks3, controls_i,"yearf"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),nAGQ = 0, data = subset(survey_data2,state_control == 1 & sample == 1 & protest_d != 0))
ind_m2a_aut <- glmer(as.formula(paste("protest1", paste(c(re_fe,"conc_symbolic_const_sum1 * authoritarian","conc_nonsymbolic_const_sum1 * authoritarian", controls_c, shocks3, controls_i,"yearf"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),nAGQ = 0, data = subset(survey_data2,state_control == 1 & sample == 1 & protest_d != 0))
ind_m2a_rw <- glmer(as.formula(paste("protest1", paste(c(re_fe,"conc_symbolic_const_sum1 * rightwing","conc_nonsymbolic_const_sum1 * rightwing", controls_c, shocks3, controls_i,"yearf"), collapse = " + "), sep = " ~ ")), family=binomial(link="logit"),nAGQ = 0, data = subset(survey_data2,state_control == 1 & sample == 1 & protest_d != 0))
stargazer(ind_m1a, ind_m1a_aut, ind_m1a_rw, ind_m2a, ind_m2a_aut, ind_m2a_rw, type="html", dep.var.labels.include = F,  order=c("conc_constorclose","conc_symbolic_constorclose","conc_nonsymbolic_constorclose","conc_const_sum1","conc_symbolic_const_sum1","conc_nonsymbolic_const_sum1"),  model.names = F, model.numbers = F, 
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"), 
          title="Individual-level results.", omit=c("cowcodef", "yearf","surveyf","unregionf","surveywave","nobacklash_months_l1","nobacklash_violent_months_l1"), 
          style = "ajps", star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"), add.lines = list(c("Country-FE","yes","yes","yes","yes","yes","yes","yes","yes"),c("Year-FE","yes","yes","yes","yes","yes","yes","yes","yes"),c("Survey wave-FE","yes","yes","yes","yes","yes","yes","yes","yes")),
          out = c("./tables/results/app4_indresults.tex","./tables/results/app4_indresults.html"))

####margins
##all conc
ind_m1a_me <- data.frame(margins_summary(ind_m1a, variables = c("conc_const_sum1")))
ind_m1a_me$term <- "concession"
ind_m1a_me$model <- "all"
ind_m1a_me$xmapping <- 1
ind_m1a_aut_me <- data.frame(margins_summary(ind_m1a_aut, variables = c("conc_const_sum1"), at = list(authoritarian = c(0,1))))
ind_m1a_aut_me$term <- "concession"
ind_m1a_aut_me$model <- c("non-authoritarians","authoritarians")
ind_m1a_aut_me$xmapping <- c(4,2)
ind_m1a_rw_me <- data.frame(margins_summary(ind_m1a_rw, variables = c("conc_const_sum1"), at = list(rightwing = c(0,1))))
ind_m1a_rw_me$term <- "concession"
ind_m1a_rw_me$model <- c("non-rightwingers","rightwingers")
ind_m1a_rw_me$xmapping <- c(5,3)
##group based vs blind
ind_m2a_me <- data.frame(margins_summary(ind_m2a, variables = c("conc_symbolic_const_sum1","conc_nonsymbolic_const_sum1")))
ind_m2a_me$term <- c("concession (group-blind)","concession (group-based)")
ind_m2a_me$model <- "all"
ind_m2a_me$xmapping <- c(1)
ind_m2a_aut_me <- data.frame(margins_summary(ind_m2a_aut, variables = c("conc_symbolic_const_sum1","conc_nonsymbolic_const_sum1"), at = list(authoritarian = c(0,1))))
ind_m2a_aut_me$term <- c("concession (group-blind)","concession (group-blind)","concession (group-based)","concession (group-based)")
ind_m2a_aut_me$model <- c("non-authoritarians","authoritarians","non-authoritarians","authoritarians")
ind_m2a_aut_me$xmapping <- c(4,2,4,2)
ind_m2a_rw_me <- data.frame(margins_summary(ind_m2a_rw, variables = c("conc_symbolic_const_sum1","conc_nonsymbolic_const_sum1"), at = list(rightwing = c(0,1))))
ind_m2a_rw_me$term <- c("concession (group-blind)","concession (group-blind)","concession (group-based)","concession (group-based)")
ind_m2a_rw_me$model <- c("non-rightwingers","rightwingers","non-rightwingers","rightwingers")
ind_m2a_rw_me$xmapping <- c(5,3,5,3)
##all together
ind_me <- rbind.fill(ind_m1a_me, ind_m1a_aut_me, ind_m1a_rw_me, ind_m2a_me, ind_m2a_aut_me, ind_m2a_rw_me)
#ind_me$term <- as.factor(ind_me$term)
ind_me$xmapping <- ifelse(ind_me$term == "concession", ind_me$xmapping - 0.2, ind_me$xmapping)
ind_me$xmapping <- ifelse(ind_me$term == "concession (group-blind)", ind_me$xmapping + 0.2, ind_me$xmapping)
ind_me_main <- ind_me
ind_me_main_plot <- ggplot(data = ind_me_main)+
  geom_errorbar(mapping=aes(x=xmapping, ymin=upper, ymax=lower, group = factor(term), colour = factor(term)), width=0.2, size=1) + 
  geom_point(mapping=aes(x=xmapping, y=AME, group = factor(term), colour = factor(term), fill = factor(term), shape = factor(term)), size=4) +
  theme_bw() + theme(plot.title = element_text(size=12), axis.title=element_text(size=8)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
  theme(text=element_text(family="Times"), plot.title =element_text(size=12), legend.position = "bottom") + 
  xlab("respondents") + ylab("change in predicted probability of\nattending protests") +
  scale_colour_manual(values = c("#8856a7","#e34a33","#2b8cbe"))+ 
  scale_fill_manual(values = c("#8856a7","#e34a33","#2b8cbe")) + 
  guides(colour = guide_legend(reverse = F, title=NULL), fill = guide_legend(reverse = F, title=NULL), shape = guide_legend(reverse = F, title=NULL) ) +
  scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("all","authoritarians","rightwingers","non-authoritarians","non-rightwingers"))
ggsave(ind_me_main_plot, file='./figures/margins/app4_ind_me_main_plot.png', width = 17, height = 7, units="cm",dpi=600)


