#This script generates Figure 2 and Figure S2

Sys.setlocale("LC_TIME", "en_US.UTF-8") 

## Christmas campaign
height = list()
height$leave_home=-0.6
height$movement_ch=-2
for (var in c("leave_home","movement_ch")){
  

  end_date = "2020-12-31"
  nb_days = 18
  
  
  # Treatment effect
  formula = as.formula(paste0(var," ~ factor(date) + factor(date):high_county_X + factor(date):baseline_th_",var))
  
  data_reg = data %>% filter(date>="2020-12-14") %>% filter(date<=end_date) %>% filter(!is.na(high_county_X))
  reg = lm.cluster(formula = formula, data = data_reg, cluster = "user_loc")
  reg_summary <- summary(reg)
  rows_keep = grepl("high_county", rownames(reg_summary), fixed=TRUE)
  reg_summary = reg_summary[rows_keep,]
  coef = reg_summary[,"Estimate"]
  stderr = reg_summary[,"Std. Error"]
  ub = coef + 1.96*stderr
  lb = coef - 1.96*stderr
  
  date = seq(as.Date("2020/12/14"), by = "day", length.out = nb_days)
  df = as.data.frame(cbind(coef,ub,lb,date))
  df$date = as.Date(df$date, origin = "1970-01-01")
  
  ggplot(df, aes(x=date, y=coef)) +
    geom_errorbar(aes(ymin=lb, ymax=ub), width=.5) +
    geom_line(colour="blue") +
    geom_point(shape=21, size=3, fill="white") + xlab("Date") +
    ylab("Coefficient") +
    theme(legend.justification=c(1,0),
          legend.position=c(1,0))   + theme_classic() + scale_x_date(date_breaks = "5 days", date_labels =  "%d %b %Y") + geom_hline(yintercept=0)+
    geom_vline(xintercept = as.numeric(df$date[11]), linetype=2, color = "blue", size=0.8)+
    geom_text(x = as.numeric(df$date[12])+0.1, y = height[[var]], label = "Christmas Eve", color = "blue")
  last_plot()
  ggsave(paste0("../Output/Mobility/X_",var,".png"), width = 10, height = 4)
  
  
  
}







## Thanksgiving campaign

height = list()
height$leave_home=0.6
height$movement_ch=-2
for (var in c("leave_home","movement_ch")){
  
  end_date = "2020-11-30"
  nb_days = 17
  
  
  # Treatment effect
  formula = as.formula(paste0(var," ~ factor(date) + factor(date):high_county_T1 + factor(date):baseline_th_",var))
  data_reg = data %>% filter(date>="2020-11-14") %>% filter(date<=end_date) 
  reg = lm.cluster(formula = formula, data = data_reg, cluster = "user_loc")
  reg_summary <- summary(reg)
  rows_keep = grepl("high_county_T1", rownames(reg_summary), fixed=TRUE)
  reg_summary = reg_summary[rows_keep,]
  coef = reg_summary[,"Estimate"]
  stderr = reg_summary[,"Std. Error"]
  ub = coef + 1.96*stderr
  lb = coef - 1.96*stderr
  
  
  date = seq(as.Date("2020/11/14"), by = "day", length.out = nb_days)
  df = as.data.frame(cbind(coef,ub,lb,date))
  df$date = as.Date(df$date, origin = "1970-01-01")
  
  ggplot(df, aes(x=date, y=coef)) +
    geom_errorbar(aes(ymin=lb, ymax=ub), width=.5) +
    geom_line(colour="blue") +
    geom_point(shape=21, size=3, fill="white") + xlab("Date") +
    ylab("Coefficient") +
    theme(legend.justification=c(1,0),
          legend.position=c(1,0))    + theme_classic()+ scale_x_date(date_breaks = "5 days", date_labels =  "%d %b %Y") + geom_hline(yintercept=0) +
    geom_vline(xintercept = as.numeric(df$date[13]), linetype=2, color = "blue", size=0.8)+
    geom_text(x = as.numeric(df$date[14]), y = height[[var]], label = "Thanksgiving", color = "blue")
  
  last_plot()
  ggsave(paste0("../Output/Mobility/T1_",var,".png"), width = 10, height = 4)

  
}

