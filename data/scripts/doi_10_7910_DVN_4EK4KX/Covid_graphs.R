#This script generates Figure 3

#Thanksgiving campaign

treatment="T1"

var="asinh_two_weeks_cases"
regression_dates = c("2020-11-26","2020-12-14","2020-12-28","2021-01-11")
models = list()
p_treated = c()
sd_errors = list()
pvals = list()
j=1

coef_graph = c()
lb=c()
ub = c()

for (regression_date in regression_dates){
  
  
  formula_str = paste0(var," ~   treated +  baseline_th_log_cases+ factor(user_loc)")
  
  data_reg = data %>% filter(date == regression_date) 
  data_reg$treated = data_reg[[paste0("treated_",treatment)]]
  reg1 =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
  models[[j]]=reg1
  reg1_summary <- summary(reg1)
  coef = reg1_summary[,"Estimate"]
  
  pvals[[j]] <- reg1_summary[, 'Pr(>|t|)']
  sd_errors[[j]] = reg1_summary[, 'Std. Error']
  
  coef_graph = c(coef_graph,coef["treated"]) 
  ub = c(ub,coef["treated"]+ 1.96*sd_errors[[j]]["treated"])
  lb = c(lb,coef["treated"]- 1.96*sd_errors[[j]]["treated"])
  
  
  j=j+1
  
}

date = c(1:4)
df = as.data.frame(cbind(coef_graph,ub,lb,date))

x_labels = c("NOV 13-NOV 26","DEC 01-DEC 14","DEC 15-DEC 28","DEC 29-JAN 11")
ggplot(df, aes(x=date, y=coef_graph)) +
  ggtitle("Asinh(Fortnightly Cases)")+
  geom_errorbar(aes(ymin=lb, ymax=ub), width=.5) +
  geom_line() +
  geom_point(shape=21, size=6, fill=c("grey50","red","grey50","grey50")) + xlab("Period") +
  ylab("Coefficient") +
  scale_x_continuous(breaks=c(1:4),
                     labels=x_labels)+
  theme(legend.justification=c(1,0),
        legend.position=c(1,0))    + theme_classic() + geom_hline(yintercept=0)

last_plot()
ggsave(paste0("../Output/Covid/fortnights_graph_",treatment,".png"), width = 10, height = 4)




#Christmas campaign with thanksgiving baseline
  
  treatment="X"
  
  var="asinh_two_weeks_cases"
  regression_dates = c("2020-11-26","2020-12-10","2020-12-24","2021-01-14","2021-01-28","2021-02-11")
  models = list()
  p_treated = c()
  sd_errors = list()
  pvals = list()
  j=1
  
  coef_graph = c()
  lb=c()
  ub = c()
  
  for (regression_date in regression_dates){
    
    
    formula_str = paste0(var," ~   treated +  baseline_th_log_cases+ factor(user_loc)")
    
    data_reg = data %>% filter(date == regression_date) 
    data_reg$treated = data_reg[[paste0("treated_",treatment)]]
    reg1 =   lm.cluster(formula = as.formula(formula_str), data = data_reg,cluster="zip")
    models[[j]]=reg1
    reg1_summary <- summary(reg1)
    coef = reg1_summary[,"Estimate"]
    
    pvals[[j]] <- reg1_summary[, 'Pr(>|t|)']
    sd_errors[[j]] = reg1_summary[, 'Std. Error']
    
    coef_graph = c(coef_graph,coef["treated"]) 
    ub = c(ub,coef["treated"]+ 1.96*sd_errors[[j]]["treated"])
    lb = c(lb,coef["treated"]- 1.96*sd_errors[[j]]["treated"])
    
    
    j=j+1
    
  }
  
  date = c(1:6)
  df = as.data.frame(cbind(coef_graph,ub,lb,date))
  
  x_labels = c("NOV 13-NOV 26","NOV 27-DEC 10","DEC 11-DEC 24","JAN 01-JAN 14","JAN 15-JAN 28","JAN 29-FEB 11")
  ggplot(df, aes(x=date, y=coef_graph)) +
    ggtitle("Asinh(Fortnightly Cases)")+
    geom_errorbar(aes(ymin=lb, ymax=ub), width=.5) +
    geom_line() +
    geom_point(shape=21, size=6, fill=c("grey50","grey50","grey50","red","grey50","grey50")) + xlab("Period") +
    ylab("Coefficient") +
    scale_x_continuous(breaks=c(1:6),
                       labels=x_labels)+
    theme(legend.justification=c(1,0),
          legend.position=c(1,0))    + theme_classic() + geom_hline(yintercept=0)
  
  last_plot()
  ggsave(paste0("../Output/Covid/fortnights_graph_",treatment,"_th_bl.png"), width = 10, height = 4)
  
  
