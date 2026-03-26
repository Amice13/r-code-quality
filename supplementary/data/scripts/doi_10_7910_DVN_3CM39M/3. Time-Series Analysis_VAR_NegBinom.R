# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(vars, tseries, tsm, TSstudio, forecast, mFilter, tidyverse, readxl, writexl, cowplot, quanteda,
       janitor, readtext, irr, bit64, igraph, ggraph, ggplot2, tscount,
       beepr, pbapply, rtweet, lmtest, vrtest, pracma, lubridate, quanteda.textplots, tscount, bruceR)

# disable scientific notation of numbers
options(scipen=999)

##### 1. Vectorautoregressions #####
effects_plot = function(model, y_list, ci = "95", sopa = T, xlim = 150, xlim_pos = F, xlim_pos_val = NULL, xbreaks = 25, lang = NULL, HC_se = F){
  
  if(xlim_pos == F){
    xlim_pos_val = xlim
  }
  
  sum_mod = summary(model)
  
  coeff_list = vector(mode = "list")
  plot_list = vector(mode = "list")
  
  for(i in 1:length(y_list)){
    coeff_list[[y_list[i]]] = sum_mod[["varresult"]][[y_list[i]]][["coefficients"]]
    
    coeff_list[[y_list[i]]] = data.frame(coeff_list[[y_list[i]]])
    
    coeff_list[[y_list[i]]]$var = rownames(coeff_list[[y_list[i]]])
    
    if(HC_se == T){
      ## Robust standard errors
      se_hc = diag(sqrt(vcovHC(model1)))
      
      covariates_length = (length(se_hc)/length(y_list))
      
      coeff_list[[y_list[i]]]$se = se_hc[((i-1)*covariates_length + 1):(i*covariates_length)]
    } else {
      coeff_list[[y_list[i]]]$se = coeff_list[[y_list[i]]][["Std..Error"]]
    }
    
    if(ci == "95"){
      coeff_list[[y_list[i]]][["ci_lo"]] = coeff_list[[y_list[i]]][["Estimate"]] - (1.96*coeff_list[[y_list[i]]]$se)
      coeff_list[[y_list[i]]][["ci_up"]] = coeff_list[[y_list[i]]][["Estimate"]] + (1.96*coeff_list[[y_list[i]]]$se)
    } else if (ci == "90"){
      coeff_list[[y_list[i]]][["ci_lo"]] = coeff_list[[y_list[i]]][["Estimate"]] - (1.64*coeff_list[[y_list[i]]]$se)
      coeff_list[[y_list[i]]][["ci_up"]] = coeff_list[[y_list[i]]][["Estimate"]] + (1.64*coeff_list[[y_list[i]]]$se)
    }
    
    coeff_list[[y_list[i]]]$r2 = sum_mod[["varresult"]][[y_list[i]]][["r.squared"]]
    
    if(sopa == T){
      
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..Internet.Blackout"] = "PLAT: Internet Blackout"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..American.Censorship.Day...PARL..SOPA.Hearings"] = "PLAT: American Censorship Day\n& PARL: SOPA Hearings"
      #coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..SOPA.introduced"] = "PARL: SOPA introduced"
      #coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..PIPA.proceeds"] = "PARL: PIPA proceeds"
      #coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..PIPA.introduced"] = "PARL: PIPA introduced"
      #coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..PIPA...COICA.hearings"] = "PARL: PIPA & COICA hearings"
      
      coeff_list[[y_list[i]]] = subset(coeff_list[[y_list[i]]],
                                       var == "PLAT: American Censorship Day\n& PARL: SOPA Hearings" | 
                                         var == "PLAT: Internet Blackout")# | 
                                         #var == "PARL: PIPA introduced" | 
                                         #var == "PARL: PIPA & COICA hearings" | 
                                         #var == "PARL: SOPA introduced" | 
                                         #var == "PARL: PIPA proceeds")
      
      level_order = c("PLAT: American Censorship Day\n& PARL: SOPA Hearings", 
                      "PLAT: Internet Blackout")#, "PARL: PIPA introduced", 
                      #"PARL: PIPA & COICA hearings", "PARL: SOPA introduced", 
                      #"PARL: PIPA proceeds")
      
      title = NULL
      
      y_text = "PLAT: American Censorship Day\n& PARL: SOPA Hearings"
      
    } else {
      
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..Multiple.Platforms.Blackout"] = "PLAT: March Blackouts"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..Wikipedia.Blackouts...PARL..EP.re.opens.debate"] = "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..YouTube.Ads"] = "PLAT: YouTube Ads"
      #coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..EP.proceeds.to.trilogue"] = "PARL: EP proceeds to trilogue"
      #coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..EP.passes.EUCD"] = "PARL: EP passes EUCD"
      #coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..Council.approves.EUCD"] = "PARL: Council approves EUCD"
      
      coeff_list[[y_list[i]]] = subset(coeff_list[[y_list[i]]],
                                       var == "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate" | 
                                         var == "PLAT: YouTube Ads" |
                                         var == "PLAT: March Blackouts")# |
                                         #var == "PARL: EP proceeds to trilogue" |
                                         #var == "PARL: EP passes EUCD" |
                                         #var == "PARL: Council approves EUCD")
      
      level_order = c("PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate",
                      "PLAT: YouTube Ads", "PLAT: March Blackouts")#,
                      #"PARL: EP proceeds to trilogue", "PARL: EP passes EUCD",
                      #"PARL: Council approves EUCD")
      
      title = as.character(lang)
      
      y_text = "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate"
      
    }
    
    plot_list[[i]] = ggplot(data = coeff_list[[i]], aes(y = factor(var, level = level_order))) + 
      geom_vline(xintercept = 0, size = 0.25) + 
      geom_segment(aes(yend = factor(var, level = level_order), x = ci_lo, xend = ci_up), 
                   size = 1, color = "grey") +
      geom_point(aes(x = Estimate), size = 3) +
      scale_x_continuous(limits = c(-xlim,xlim_pos_val), breaks = c(seq(-xlim, xlim_pos_val, by = xbreaks))) +
      ylab("") +
      xlab("Estimate") +
      ggtitle(title, subtitle = paste("Dependent Variable:", y_list[[i]], sep = " ")) +
      theme_classic() +
      annotate("text", x = (xlim_pos_val-xbreaks), y = y_text,
               label = paste0("R2:\n", as.character(round(coeff_list[[i]]$r2[1],
                                                          digits = 3))))
    
  }
  
  #for(i in 2:length(y_list)){
    
  #  plot_list[[i]] = plot_list[[i]] + theme(axis.text.y = element_blank())
    
  #}
  
  return(plot_list)
}

tab_mod_inner = function(model, var, HC_se = T){
  # get model summary
  sum_mod = summary(model)
  
  #extract coefficients
  results = data.frame(sum_mod[["varresult"]][[var]][["coefficients"]])
  
  if(HC_se == T){
    ## Robust standard errors
    robust = diag(sqrt(vcovHC(model)))
    
    ## subset to only variable
    needle = paste0(var, ":")
    
    robust = subset(robust, grepl(needle, names(robust)))
    
    # change rownames for merging
    names(robust) = gsub(needle, "", names(robust))
    names(robust) = gsub("\\(Intercept\\)", "const", names(robust))
    
    robust = data.frame(robust)
    
    colnames(robust) = "se"
    
    # merge with results df
    results = merge(results, robust, by = 'row.names')
    
    # drop rownames column
    results = data.frame(results, row.names = 1)
    
    # compute p based on robust standard errors
    results$z = results$Estimate/results$se
    results$p = exp(-0.717*abs(results$z) - 0.416*abs(results$z)^2)
    
  } else {
    # rename
    names(results)[2] = "se"
    names(results)[4] = "p"
  }
  
  # rounding
  results = round(results, 3)
  # stars for p values
  results$p_star = ifelse(results$p < 0.1, ".", "") 
  results$p_star = ifelse(results$p < 0.05, "*", results$p_star) 
  results$p_star = ifelse(results$p < 0.01, "**", results$p_star) 
  # merge estimate, standard errors, and stars into a single column
  results$output = paste0(results$Estimate, results$p_star, " (", results$se, ")")
  # delete all other columns
  results = select(results, output)
  
  # extract r2
  r2 = data.frame(sum_mod[["varresult"]][[var]][["r.squared"]])
  # rename and round
  names(r2)[1] = "output"
  rownames(r2) = "r2"
  r2 = round(r2, 3)
  
  # extract f
  f = t(data.frame(sum_mod[["varresult"]][[var]][["fstatistic"]]))
  # merge f statistics and degrees of freedom
  f = data.frame(paste0(round(f[1],2), "(df=",round(f[2]),";",round(f[3]),")"))
  # rename and round
  names(f)[1] = "output"
  rownames(f) = "F-Statistic"
  
  # extract residual standard error
  resse = data.frame(sum_mod[["varresult"]][[var]][["sigma"]])
  # rename and round
  names(resse)[1] = "output"
  rownames(resse) = "Resid. Std. Error"
  resse = round(resse, 3)
  # create output
  resse$output = paste0(resse$output, "(df=",sum_mod[["varresult"]][[var]][["df"]][2],")")
  
  #Log likelihood (full mod)
  loglik = data.frame(sum_mod[["logLik"]])
  # rename and round
  names(loglik)[1] = "output"
  rownames(loglik) = "Log Likelihood"
  loglik = round(loglik, 3)
  
  # Observations
  obs = data.frame(sum_mod[["obs"]])
  # rename and round
  names(obs)[1] = "output"
  rownames(obs) = "Observations"
  
  # bind 
  results = rbind(results, obs, r2, resse, f, loglik)
  
  names(results)[1] = var
  
  return(results)
}

tab_mod = function(model, sopa = F, HC_se = T){
  
  var1 = tab_mod_inner(model = model, var = "diff_tweets", HC_se = HC_se)
  var2 = tab_mod_inner(model = model, var = "diff_news", HC_se = HC_se)
  var3 = tab_mod_inner(model = model, var = "diff_IG_plat", HC_se = HC_se)
  var4 = tab_mod_inner(model = model, var = "diff_IG_rh_cr", HC_se = HC_se)
  
  if(sopa == T){
    var5 = tab_mod_inner(model = model, var = "diff_congress", HC_se = HC_se)
  } else {
    var5 = tab_mod_inner(model = model, var = "diff_meps", HC_se = HC_se)
  }
  
  output = cbind(var1, var2, var3, var4, var5)
  
  output$names = rownames(output)
  
  return(output)
}

############# a) SOPA #############
sopa_ts = read.csv("SOPA/SOPA_Time-Series_Data_Days.csv")

##### i) Create Intervention Covariates #####
# a) parliamentary events
sopa_ts$days[sopa_ts$time_var == "2011-05-12"] # PIPA Introduced
sopa_ts$days[sopa_ts$time_var == "2011-06-22"] # PIPA hearings
sopa_ts$days[sopa_ts$time_var == "2011-06-22"] # Hearings COICA
sopa_ts$days[sopa_ts$time_var == "2011-10-26"] # Introduction SOPA
sopa_ts$days[sopa_ts$time_var == "2011-12-17"] # PIPA proceeds to senate

parl_events = interv_covariate(n = nrow(sopa_ts), tau = c(235, 276, 402, 454),
                               delta = c(0.5,0.5,0.5,0.5))

colnames(parl_events)[1] = "PARL: PIPA introduced"
colnames(parl_events)[2] = "PARL: PIPA & COICA hearings"
colnames(parl_events)[3] = "PARL: SOPA introduced"
colnames(parl_events)[4] = "PARL: PIPA proceeds"

# b) Platform actions
sopa_ts$days[sopa_ts$time_var == "2011-11-16"] # Comittee Hearings SOPA & American Censorship Day

sopa_ts$days[sopa_ts$time_var == "2012-01-17"] #--> although the blackout happened on January 18 it was announced on January 17

plat_actions = interv_covariate(n = nrow(sopa_ts), tau = c(423, 485), delta = c(0.5,0.5))


colnames(plat_actions)[1] = "PLAT: American Censorship Day\n& PARL: SOPA Hearings"
colnames(plat_actions)[2] = "PLAT: Internet Blackout"

# Create covariate vector
weekend = ifelse(sopa_ts$weekday == 6, 1, 0)
weekend = ifelse(sopa_ts$weekday == 7, 1, weekend)

xreg = cbind(plat_actions, parl_events, weekend)

######## ii) VAR ########
# Difference between pro platform and anti platform tweets
diff_tweets = sopa_ts$anti_plat_perc_tweets - sopa_ts$pro_plat_perc_tweets
diff_IG_plat = sopa_ts$anti_plat_perc_IG_plat - sopa_ts$pro_plat_perc_IG_plat
diff_IG_rh_cr = sopa_ts$anti_plat_perc_IG_rh_cr - sopa_ts$pro_plat_perc_IG_rh_cr
diff_news = sopa_ts$anti_plat_perc_news - sopa_ts$pro_plat_perc_news
diff_congress = sopa_ts$anti_plat_perc_congress - sopa_ts$pro_plat_perc_congress

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_congress = na.omit(ts(diff_congress, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_plat, diff_IG_rh_cr, diff_news, diff_congress))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_congress)

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_congress)

y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_congress")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 1, type = "const", exogen = xreg)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, lags.pt = 12, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1, lags.multi = 12, multivariate.only = F)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, sopa = T, HC_se = T)
write_xlsx(reg_table, "SOPA/SOPA_VAR_results.xlsx")

# Plots
sopa_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 200, xbreaks = 50,
                              xlim_pos = T, xlim_pos_val = 100, HC_se = T)

sopa_plot = plot_grid(sopa_plot_list[[1]],sopa_plot_list[[2]],sopa_plot_list[[5]],
                      ncol = 3, rel_widths = c(1.25,1,1))

sopa_tweets = sopa_plot_list[[1]]
sopa_news = sopa_plot_list[[2]] + theme(axis.text.y = element_blank())
sopa_congress = sopa_plot_list[[5]] + theme(axis.text.y = element_blank())

sopa_plot = plot_grid(sopa_tweets,sopa_news,sopa_congress,
                      ncol = 3, rel_widths = c(1.5,1,1))

ggsave("SOPA/SOPA_VAR_Intervention_05.png", sopa_plot, dpi = 640, width = 12, height = 4)
ggsave("SOPA/SOPA_VAR_Intervention_05.pdf", sopa_plot, dpi = 640, width = 12, height = 4)

# Granger Causality
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_congress", vcovHC(model1))

############# b) EUCD #############
eucd_ts_weeks = read.csv("EUCD/EUCD_Time-Series_Data_Weeks.csv")
eucd_ts = read.csv("EUCD/EUCD_Time-Series_Data_Days.csv")

######## i) Create Intervention Covariates########
# a) parliamentary events
eucd_ts$days[eucd_ts$time_var == "2018-09-12"] # EP votes to proceed to trilogue
eucd_ts$days[eucd_ts$time_var == "2019-03-26"] # EP passes directive
eucd_ts$days[eucd_ts$time_var == "2019-04-15"] # Council approves final directive

parl_events = interv_covariate(n = length(unique(eucd_ts$days)), tau = c(255, 450, 470),
                               delta = c(0.5,0.5,0.5))

colnames(parl_events)[1] = "PARL: EP proceeds to trilogue"
colnames(parl_events)[2] = "PARL: EP passes EUCD"
colnames(parl_events)[3] = "PARL: Council approves EUCD"

# b) Platform actions
eucd_ts$days[eucd_ts$time_var == "2018-07-03"] # Italian Wiki Blackout on 03.07, others announced on 03.07 and followed on 4. and 5.
eucd_ts$days[eucd_ts$time_var == "2018-10-22"] # Youtube Banners/Ads
eucd_ts$days[eucd_ts$time_var == "2019-03-21"] # Blackouts

plat_actions = interv_covariate(n = length(unique(eucd_ts$days)), tau = c(184, 295, 445), delta = c(0.5, 0.85, 0.5))

colnames(plat_actions)[1] = "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate"
colnames(plat_actions)[2] = "PLAT: YouTube Ads"
colnames(plat_actions)[3] = "PLAT: Multiple Platforms Blackout"

# Create covariate vector
weekend = ifelse(subset(eucd_ts, lang == "all")$weekday.x == 6, 1, 0)
weekend = ifelse(subset(eucd_ts, lang == "all")$weekday.x == 7, 1, weekend)

xreg = cbind(plat_actions, parl_events, weekend)

######## ii) VAR ########
##### I) All #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "all")$anti_plat_perc_tweets - subset(eucd_ts, lang == "all")$pro_plat_perc_tweets
diff_IG_plat = subset(eucd_ts, lang == "all")$anti_plat_perc_IG_plat - subset(eucd_ts, lang == "all")$pro_plat_perc_IG_plat
diff_IG_rh_cr = subset(eucd_ts, lang == "all")$anti_plat_perc_IG_rh_cr - subset(eucd_ts, lang == "all")$pro_plat_perc_IG_rh_cr
diff_news = subset(eucd_ts, lang == "all")$anti_plat_perc_news - subset(eucd_ts, lang == "all")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "all")$anti_plat_perc_meps - subset(eucd_ts, lang == "all")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets)
length(diff_meps[diff_meps != 0])/length(diff_tweets)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_plat, diff_IG_rh_cr, diff_news, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)

y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 2, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = T)
write_xlsx(reg_table, "EUCD/VAR/EUCD_ALL_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 250, xlim_pos_val = 250,
                              xlim_pos = T, HC_se = T, sopa = F, xbreaks = 100,
                              lang = "All")

eucd_plot_all = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                          ncol = 3, rel_widths = c(1.5,1,1))

# Granger Causality
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_meps", vcovHC(model1))

##### II) English #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "en")$anti_plat_perc_tweets - subset(eucd_ts, lang == "en")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "en")$anti_plat_perc_news - subset(eucd_ts, lang == "en")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "en")$anti_plat_perc_meps - subset(eucd_ts, lang == "en")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets)
length(diff_meps[diff_meps != 0])/length(diff_tweets)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_plat, diff_IG_rh_cr, diff_news, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 2, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = F)
write_xlsx(reg_table, "EUCD/VAR/EUCD_EN_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 250, HC_se = F, sopa = F, xbreaks = 100,
                              lang = "English")

eucd_plot_en = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

# Granger Causality
causality(model1, cause = "diff_tweets")
causality(model1, cause = "diff_news")
causality(model1, cause = "diff_IG_plat")
causality(model1, cause = "diff_IG_rh_cr")
causality(model1, cause = "diff_meps")

##### III) German #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "de")$anti_plat_perc_tweets - subset(eucd_ts, lang == "de")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "de")$anti_plat_perc_news - subset(eucd_ts, lang == "de")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "de")$anti_plat_perc_meps - subset(eucd_ts, lang == "de")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets)
length(diff_meps[diff_meps != 0])/length(diff_tweets)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_plat, diff_IG_rh_cr, diff_news, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 3, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = T)
write_xlsx(reg_table, "EUCD/VAR/EUCD_DE_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 250, xbreaks = 100,
                              xlim_pos = T, xlim_pos_val = 250, HC_se = T, sopa = F,
                              lang = "German")

eucd_plot_de = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

de_tweets = eucd_plot_list[[1]]
de_news = eucd_plot_list[[2]]
de_meps = eucd_plot_list[[5]]

# Granger Causality
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_meps", vcovHC(model1))

##### IV) French #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "fr")$anti_plat_perc_tweets - subset(eucd_ts, lang == "fr")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "fr")$anti_plat_perc_news - subset(eucd_ts, lang == "fr")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "fr")$anti_plat_perc_meps - subset(eucd_ts, lang == "fr")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets) # --> under 5%
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets)
length(diff_meps[diff_meps != 0])/length(diff_tweets)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_rh_cr, diff_news, diff_IG_plat, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 9, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = T)
write_xlsx(reg_table, "EUCD/VAR/EUCD_FR_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 250, xbreaks = 100,
                              xlim_pos = T, xlim_pos_val = 250, HC_se = T, sopa = F,
                              lang = "French")

eucd_plot_fr = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

fr_tweets = eucd_plot_list[[1]]
fr_news = eucd_plot_list[[2]]
fr_meps = eucd_plot_list[[5]]

# Granger Causality
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_meps", vcovHC(model1))

##### V) Polish #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "pl")$anti_plat_perc_tweets - subset(eucd_ts, lang == "pl")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "pl")$anti_plat_perc_news - subset(eucd_ts, lang == "pl")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "pl")$anti_plat_perc_meps - subset(eucd_ts, lang == "pl")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets)
length(diff_meps[diff_meps != 0])/length(diff_tweets)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_plat, diff_news, diff_IG_rh_cr, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 3, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = T)
write_xlsx(reg_table, "EUCD/VAR/EUCD_PL_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 300, xbreaks = 50,
                              xlim_pos = T, xlim_pos_val = 250, HC_se = T, sopa = F,
                              lang = "Polish")

eucd_plot_pl = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

# Granger Causality
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_meps", vcovHC(model1))

##### VI) Dutch #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "nl")$anti_plat_perc_tweets - subset(eucd_ts, lang == "nl")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "nl")$anti_plat_perc_news - subset(eucd_ts, lang == "nl")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "nl")$anti_plat_perc_meps - subset(eucd_ts, lang == "nl")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_news)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_IG_plat)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_IG_rh_cr)
length(diff_meps[diff_meps != 0])/length(diff_meps)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 3, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = T)
write_xlsx(reg_table, "EUCD/VAR/EUCD_NL_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 200, xbreaks = 50,
                              xlim_pos = T, xlim_pos_val = 150, HC_se = T, sopa = F,
                              lang = "Dutch")

eucd_plot_nl = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

# Granger Causality
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_meps", vcovHC(model1))

##### VII) Italian #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "it")$anti_plat_perc_tweets - subset(eucd_ts, lang == "it")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "it")$anti_plat_perc_news - subset(eucd_ts, lang == "it")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "it")$anti_plat_perc_meps - subset(eucd_ts, lang == "it")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets)
length(diff_meps[diff_meps != 0])/length(diff_tweets)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_plat, diff_IG_rh_cr, diff_news, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 4, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = T)
write_xlsx(reg_table, "EUCD/VAR/EUCD_IT_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 250, xbreaks = 100,
                              xlim_pos = T, xlim_pos_val = 250, HC_se = T, sopa = F,
                              lang = "Italian")

eucd_plot_it = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

it_tweets = eucd_plot_list[[1]]
it_news = eucd_plot_list[[2]]
it_meps = eucd_plot_list[[5]]

# Granger Causality
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_meps", vcovHC(model1))

##### VIII) Spanish #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "es")$anti_plat_perc_tweets - subset(eucd_ts, lang == "es")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "es")$anti_plat_perc_news - subset(eucd_ts, lang == "es")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "es")$anti_plat_perc_meps - subset(eucd_ts, lang == "es")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets)
length(diff_meps[diff_meps != 0])/length(diff_tweets)

diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_IG_plat, diff_IG_rh_cr, diff_news, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_IG_plat) # no unit root
adf.test(diff_IG_rh_cr) # no unit root
adf.test(diff_news) # no unit root
adf.test(diff_meps) # no unit root

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 2, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = T)
write_xlsx(reg_table, "EUCD/VAR/EUCD_ES_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 250, xbreaks = 100,
                              xlim_pos = T, xlim_pos_val = 250, HC_se = T, sopa = F,
                              lang = "Spanish")

eucd_plot_es = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

es_tweets = eucd_plot_list[[1]]
es_news = eucd_plot_list[[2]]
es_meps = eucd_plot_list[[5]]

# Granger Causality
causality(model1, cause = "diff_tweets", vcovHC(model1))
causality(model1, cause = "diff_news", vcovHC(model1))
causality(model1, cause = "diff_IG_plat", vcovHC(model1))
causality(model1, cause = "diff_IG_rh_cr", vcovHC(model1))
causality(model1, cause = "diff_meps", vcovHC(model1))

##### IX) Portuguese #####
# Difference between pro platform and anti platform tweets
diff_tweets = subset(eucd_ts, lang == "pt")$anti_plat_perc_tweets - subset(eucd_ts, lang == "pt")$pro_plat_perc_tweets
diff_news = subset(eucd_ts, lang == "pt")$anti_plat_perc_news - subset(eucd_ts, lang == "pt")$pro_plat_perc_news
diff_meps = subset(eucd_ts, lang == "pt")$anti_plat_perc_meps - subset(eucd_ts, lang == "pt")$pro_plat_perc_meps

# How many zeros?
length(diff_tweets[diff_tweets != 0])/length(diff_tweets)
length(diff_news[diff_news != 0])/length(diff_tweets)
length(diff_IG_plat[diff_IG_plat != 0])/length(diff_tweets)
length(diff_IG_rh_cr[diff_IG_rh_cr != 0])/length(diff_tweets) # -> under 2.5%
length(diff_meps[diff_meps != 0])/length(diff_tweets)

# define time-series
diff_tweets = na.omit(ts(diff_tweets, frequency = 1))
diff_news = na.omit(ts(diff_news, frequency = 1))
diff_IG_plat = na.omit(ts(diff_IG_plat, frequency = 1))
diff_IG_rh_cr = na.omit(ts(diff_IG_rh_cr, frequency = 1))
diff_meps = na.omit(ts(diff_meps, frequency = 1))

# plot time series
plot(cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps))
# there is likely no drift and no trend (https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend)

# Test for stationarity
adf.test(diff_tweets) # no unit root
adf.test(diff_news)
adf.test(diff_IG_plat)
adf.test(diff_IG_rh_cr)
adf.test(diff_meps)

# model selection and estimation
var_data = cbind(diff_tweets, diff_news, diff_IG_plat, diff_IG_rh_cr, diff_meps)
y_list = c("diff_tweets", "diff_news", "diff_IG_plat", "diff_IG_rh_cr", "diff_meps")

# determine number of lags to use
info_data = VARselect(var_data, lag.max = 12, type = "const", exogen = xreg)
info_data$selection

# model
model1 = VAR(var_data, p = 4, type = "const", exogen = xreg)
summary(model1)

# diagnostics
# Serial Correlation in the residuals? 
mod1_serial = serial.test(model1, type = "PT.adjusted")
mod1_serial

# Heteroscedasticity in the residuals? 
mod1_het = arch.test(model1)
mod1_het

# residuals normally distributed?
mod1_norm = normality.test(model1, multivariate.only = T)
mod1_norm

# structural break in residuals?
mod1_cusum = stability(model1, type="OLS-CUSUM")
plot(mod1_cusum)

# Result table
reg_table = tab_mod(model1, HC_se = F)
write_xlsx(reg_table, "EUCD/VAR/EUCD_PT_VAR_results.xlsx")

# Plots
eucd_plot_list = effects_plot(model1, y_list = y_list, ci = "95", xlim = 250, xbreaks = 100,
                              xlim_pos = T, xlim_pos_val = 250, HC_se = F, sopa = F,
                              lang = "Portuguese")

eucd_plot_pt = plot_grid(eucd_plot_list[[1]],eucd_plot_list[[2]],eucd_plot_list[[5]],
                         ncol = 3, rel_widths = c(1.5,1,1))

pt_tweets = eucd_plot_list[[1]]
pt_news = eucd_plot_list[[2]]
pt_meps = eucd_plot_list[[5]]

# Granger Causality
causality(model1, cause = "diff_tweets")
causality(model1, cause = "diff_news")
causality(model1, cause = "diff_IG_plat")
causality(model1, cause = "diff_IG_rh_cr")
causality(model1, cause = "diff_meps")

eucd_plot = plot_grid(#eucd_plot_all,
  eucd_plot_de,
  #eucd_plot_pl, 
  eucd_plot_fr, 
  #eucd_plot_en,
  eucd_plot_it, 
  eucd_plot_pt, 
  #eucd_plot_nl, 
  eucd_plot_es,
  ncol = 1)

ggsave("EUCD/VAR/EUCD_VAR_Intervention_05.png", eucd_plot, dpi = 640, width = 12, height = 18)
ggsave("EUCD/VAR/EUCD_VAR_Intervention_05.pdf", eucd_plot, dpi = 640, width = 12, height = 18)

## Plot only significant effects
# news
fr_news = fr_news + theme(axis.text.y = element_blank())

eucd_news = plot_grid(de_news, fr_news, rel_widths = c(1.25,1), nrow = 1) 

#tweets
pt_tweets = pt_tweets + theme(axis.text.y = element_blank())
es_tweets = es_tweets + theme(axis.text.y = element_blank())

eucd_tweets = plot_grid(it_tweets, pt_tweets, es_tweets, nrow = 1, rel_widths = c(1.33,1,1))

#meps
fr_meps = fr_meps + theme(axis.text.y = element_blank())
pt_meps = pt_meps + theme(axis.text.y = element_blank())

eucd_meps = plot_grid(de_meps, fr_meps, pt_meps, nrow = 1, rel_widths = c(1.33,1,1))

eucd_plot = plot_grid(eucd_news, eucd_meps, eucd_tweets, ncol = 1, labels = c("Newspaper", "MEPs", "Tweets"))
eucd_plot

ggsave("EUCD/VAR/EUCD_VAR_Intervention_red.png", eucd_plot, dpi = 640, width = 12, height = 8)
ggsave("EUCD/VAR/EUCD_VAR_Intervention_red.pdf", eucd_plot, dpi = 640, width = 12, height = 8)

############################ 2. Negative Binomial Regressions ##########################################
tab_mod_count = function(model){
  # get model summary
  sum_mod = summary(model)
  
  # extract coefficients
  results = data.frame(sum_mod[["coefficients"]])
  
  # compute z statistic and p value
  results$z = results$Estimate/results$Std.Error
  results$p = exp(-0.717*results$z - 0.416*(results$z^2))
  
  # rounding
  results = round(results, 3)
  # stars for p values
  results$p_star = ifelse(results$p < 0.1, ".", "") 
  results$p_star = ifelse(results$p < 0.05, "*", results$p_star) 
  results$p_star = ifelse(results$p < 0.01, "**", results$p_star) 
  # merge estimate, standard errors, and stars into a single column
  results$output = paste0(results$Estimate, results$p_star, " (", results$Std.Error, ")")
  # delete all other columns
  results = select(results, output)
  
  # Observations
  obs = data.frame(length(sum_mod[["residuals"]]) - length(sum_mod[["call"]][["model"]][["past_obs"]]))
  # rename and round
  names(obs)[1] = "output"
  rownames(obs) = "Observations"
  
  # Log Likelihood
  loglik = data.frame(sum_mod[["logLik"]][1])
  # rename and round
  names(loglik)[1] = "output"
  rownames(loglik) = "Log Likelihood"
  loglik = round(loglik, 3)
  
  # aic
  aic = data.frame(sum_mod[["AIC"]])
  # rename and round
  names(aic)[1] = "output"
  rownames(aic) = "Akaike's Information Criterion"
  aic = round(aic, 3)
  
  # bic
  bic = data.frame(sum_mod[["BIC"]])
  # rename and round
  names(bic)[1] = "output"
  rownames(bic) = "Bayesian Information Criterion"
  bic = round(bic, 3)
  
  # qic
  qic = data.frame(sum_mod[["QIC"]])
  # rename and round
  names(qic)[1] = "output"
  rownames(qic) = "Quasi Information Criterion"
  qic = round(qic, 3)
  
  # bind 
  results = rbind(results, obs, loglik, aic, bic, qic)
  
  names(results)[1] = as.character(sum_mod[["call"]][["ts"]])
  
  results$names = rownames(results)
  
  return(results)
}

effect_plot = function(model, xlim, xlim_pos = F, xlim_pos_val = NULL, ylab = NULL, xbreak = 2, title = "", subtitle, sopa, week = T, ci90 = F){
  
  if(xlim_pos == F){
    xlim_pos_val = xlim
  }
  
  names(model$coefficients)[names(model$coefficients) == "PLAT: Multiple Platforms Blackout"] = "PLAT: March Blackouts"
  names(model$coefficients)[names(model$coefficients) == "PLAT: Multiple Platforms Blackout &\nPARL: EP passes EUCD"] = "PLAT: March Blackouts &\nPARL: EP passes EUCD"
  
  data = summary(model)$coefficients
  data$var = rownames(data)
  
  if(sopa == T){
    data = subset(data, var == "PLAT: American Censorship Day\n& PARL: SOPA Hearings" | var == "PLAT: Internet Blackout") #|
    # var == "PARL: PIPA introduced" | var == "PARL: PIPA & COICA hearings" | var == "PARL: SOPA introduced" | var == "PARL: PIPA proceeds")
    
    level_order = c("PLAT: American Censorship Day\n& PARL: SOPA Hearings", "PLAT: Internet Blackout")#, "PARL: PIPA introduced", "PARL: PIPA & COICA hearings", "PARL: SOPA introduced", "PARL: PIPA proceeds") 
  } else if(week == T){
    data = subset(data, var == "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate" |
                    var == "PLAT: YouTube Ads" | var == "PLAT: March Blackouts &\nPARL: EP passes EUCD")# | 
                    #var == "PARL: EP proceeds to trilogue" | var == "PARL: Council approves EUCD")
    
    level_order = c("PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate", "PLAT: YouTube Ads", 
                    "PLAT: March Blackouts &\nPARL: EP passes EUCD")#, "PARL: EP proceeds to trilogue",
                    #"PARL: Council approves EUCD") 
  } else {
    data = subset(data, var == "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate" | 
                    var == "PLAT: YouTube Ads" | var == "PLAT: March Blackouts") #| 
                    #var == "PARL: EP proceeds to trilogue" | var == "PARL: EP passes EUCD" | 
                    #var == "PARL: Council approves EUCD")
    
    level_order = c("PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate", "PLAT: YouTube Ads", 
                    "PLAT: March Blackouts")#, "PARL: EP proceeds to trilogue", "PARL: EP passes EUCD",
                    #"PARL: Council approves EUCD") 
  }
  
  if(ci90 == T){
    data$ci_lo = data$Estimate-(1.64*data$Std.Error)
    data$ci_up = data$Estimate+(1.64*data$Std.Error)
    
  } else {
    data$ci_lo = data$`CI(lower)`
    data$ci_up = data$`CI(upper)`
  }
  
  plot = ggplot(data = data, aes(y = factor(var, level = level_order))) + 
    geom_vline(xintercept = 0, size = 0.25) + 
    geom_segment(aes(yend = factor(var, level = level_order), x = ci_lo, xend = ci_up), size = 1, color = "grey") +
    geom_point(aes(x = Estimate), size = 3) +
    scale_x_continuous(limits = c(-xlim,xlim_pos_val), breaks = c(seq(-xlim, xlim, by = xbreak))) +
    ylab(ylab) +
    xlab("Estimate") +
    ggtitle(title, subtitle = subtitle) +
    theme_classic() 
  
  
  if(is.null(ylab)){
    plot = plot + theme(axis.text.y = element_blank())
  }
  
  return(plot)
}


######## i) SOPA ########
sopa_ts_weeks = read.csv("SOPA/SOPA_Time-Series_Data_Weeks.csv")
sopa_ts = read.csv("SOPA/SOPA_Time-Series_Data_Days.csv")

##### I) Preparation #####
## days
# Create Intervention Covariates
# a) parliamentary events
sopa_ts$days[sopa_ts$time_var == "2011-05-12"] # PIPA Introduced
sopa_ts$days[sopa_ts$time_var == "2011-06-22"] # PIPA hearings
sopa_ts$days[sopa_ts$time_var == "2011-06-22"] # Hearings COICA
sopa_ts$days[sopa_ts$time_var == "2011-10-26"] # Introduction SOPA
sopa_ts$days[sopa_ts$time_var == "2011-12-17"] # PIPA proceeds to senate

parl_events = interv_covariate(n = nrow(sopa_ts), tau = c(235, 276, 402, 454),
                               delta = c(0.5,0.5,0.5,0.5))

colnames(parl_events)[1] = "PARL: PIPA introduced"
colnames(parl_events)[2] = "PARL: PIPA & COICA hearings"
colnames(parl_events)[3] = "PARL: SOPA introduced"
colnames(parl_events)[4] = "PARL: PIPA proceeds"

# b) Platform actions
sopa_ts$days[sopa_ts$time_var == "2011-11-16"] # Comittee Hearings SOPA & American Censorship Day

sopa_ts$days[sopa_ts$time_var == "2012-01-17"] #--> although the blackout happened on January 18 it was announced on January 17

plat_actions = interv_covariate(n = nrow(sopa_ts), tau = c(423, 485), delta = c(0.5,0.5))


colnames(plat_actions)[1] = "PLAT: American Censorship Day\n& PARL: SOPA Hearings"
colnames(plat_actions)[2] = "PLAT: Internet Blackout"

# Create covariate vector
xreg = cbind(plat_actions, parl_events)

## weeks
# a) parliamentary events
sopa_ts_weeks$weeks[sopa_ts_weeks$time_var == format(as.Date("2011-05-12"), "%Y-%U")]
sopa_ts_weeks$weeks[sopa_ts_weeks$time_var == format(as.Date("2011-06-22"), "%Y-%U")]
sopa_ts_weeks$weeks[sopa_ts_weeks$time_var == format(as.Date("2011-10-26"), "%Y-%U")]
sopa_ts_weeks$weeks[sopa_ts_weeks$time_var == format(as.Date("2011-12-17"), "%Y-%U")]

parl_events_week = interv_covariate(n = nrow(sopa_ts_weeks), tau = c(35, 41, 59, 66),
                                    delta = c(0.5,0.5,0.5, 0.5))

colnames(parl_events_week)[1] = "PARL: PIPA introduced"
colnames(parl_events_week)[2] = "PARL: PIPA & COICA hearings"
colnames(parl_events_week)[3] = "PARL: SOPA introduced"
colnames(parl_events_week)[4] = "PARL: PIPA proceeds"

# b) platform actions
sopa_ts_weeks$weeks[sopa_ts_weeks$time_var == format(as.Date("2011-11-16"), "%Y-%U")] # Comittee Hearings SOPA & American Censorship Day
sopa_ts_weeks$weeks[sopa_ts_weeks$time_var == format(as.Date("2012-01-17"), "%Y-%U")]#--> although the blackout happened on January 18 it was announced on January 17

plat_actions_week = interv_covariate(n = nrow(sopa_ts_weeks), tau = c(62, 71), delta = c(0.5,0.5))

colnames(plat_actions_week)[1] = "PLAT: American Censorship Day\n& PARL: SOPA Hearings"
colnames(plat_actions_week)[2] = "PLAT: Internet Blackout"

# Create covariate vector
xreg_week = cbind(plat_actions_week, parl_events_week)

# create dependent variable
count_tweets = sopa_ts$count_tweets
count_news = sopa_ts_weeks$count_news

##### II) Analysis #####
## Tweets
# Run Poisson Regression
count_pois = tsglm(count_tweets, model = list(past_obs = c(1:2)),
                   xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_nbin = tsglm(count_tweets, model = list(past_obs = c(1:2)),
                   xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_nbin), main = "ACF of response residuals (NegBin)")
#marcal(proplat_tweets_pois, main = "Marginal calibration")
#lines(marcal(proplat_tweets_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_pois), NegBin = scoring(count_nbin))

# Negative Binomial Model is Appropriate
summary(count_nbin)
tab_tweets = tab_mod_count(count_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_nbin)[["coefficients"]]), (exp(summary(count_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot the effects
tweets_plot = effect_plot(count_nbin, xlim = 10, ylab = "Intervention", 
                          title = NULL, subtitle = "Number of Tweets", 
                          sopa = T, ci90 = F, week = F) 

## News
# Run Poisson Regression
count_pois = tsglm(count_news, model = list(past_obs = c(1:2)),
                   xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_nbin = tsglm(count_news, model = list(past_obs = c(1:2)),
                   xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_nbin), main = "ACF of response residuals (NegBin)")
#marcal(proplat_tweets_pois, main = "Marginal calibration")
#lines(marcal(proplat_tweets_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_pois), NegBin = scoring(count_nbin))

# Negative Binomial Model is Appropriate
summary(count_nbin)
tab_news = tab_mod_count(count_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_nbin)[["coefficients"]]), (exp(summary(count_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot the effects
news_plot = effect_plot(count_nbin, xlim = 10, ylab = "Intervention", 
                        title = NULL, subtitle = "Number of Articles", 
                        sopa = T, ci90 = F) + theme(axis.text.y = element_blank())

sopa_plot = plot_grid(tweets_plot, news_plot, ncol = 2, rel_widths = c(1.25,1))

ggsave("SOPA/SOPA_Count_Plot.png", sopa_plot, height = 4, width = 12, dpi = 640)
ggsave("SOPA/SOPA_Count_Plot.pdf", sopa_plot, height = 4, width = 12, dpi = 640)

tab_sopa = cbind(tab_news, tab_tweets)

write_xlsx(tab_sopa, "SOPA/SOPA_Count_results.xlsx")

######## i) EUCD ########
eucd_ts_weeks = read.csv("EUCD/EUCD_Time-Series_Data_Weeks.csv")
eucd_ts = read.csv("EUCD/EUCD_Time-Series_Data_Days.csv")

##### I) Preparation #####
## days
# Create Intervention Covariates
# a) parliamentary events
eucd_ts$days[eucd_ts$time_var == "2018-09-12"] # EP votes to proceed to trilogue
eucd_ts$days[eucd_ts$time_var == "2019-03-26"] # EP passes directive
eucd_ts$days[eucd_ts$time_var == "2019-04-15"] # Council approves final directive

parl_events = interv_covariate(n = length(unique(eucd_ts$days)), tau = c(255, 450, 470),
                               delta = c(0.5,0.5,0.5))

colnames(parl_events)[1] = "PARL: EP proceeds to trilogue"
colnames(parl_events)[2] = "PARL: EP passes EUCD"
colnames(parl_events)[3] = "PARL: Council approves EUCD"

# b) Platform actions
eucd_ts$days[eucd_ts$time_var == "2018-07-03"] # Italian Wiki Blackout on 03.07, others announced on 03.07 and followed on 4. and 5.
eucd_ts$days[eucd_ts$time_var == "2018-10-22"] # Youtube Banners/Ads
eucd_ts$days[eucd_ts$time_var == "2019-03-21"] # Blackouts

plat_actions = interv_covariate(n = length(unique(eucd_ts$days)), tau = c(184, 295, 445), delta = c(0.5, 0.85, 0.5))

colnames(plat_actions)[1] = "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate"
colnames(plat_actions)[2] = "PLAT: YouTube Ads"
colnames(plat_actions)[3] = "PLAT: Multiple Platforms Blackout"

xreg = cbind(plat_actions, parl_events)

## weeks
# a) parliamentary events
eucd_ts_weeks$weeks[eucd_ts_weeks$time_var == format(as.Date("2018-09-12"), "%Y-%U")] # EP votes to proceed to trilogue
eucd_ts_weeks$weeks[eucd_ts_weeks$time_var == format(as.Date("2019-03-26"), "%Y-%U")] # EP passes directive
eucd_ts_weeks$weeks[eucd_ts_weeks$time_var == format(as.Date("2019-04-15"), "%Y-%U")] # Council approves final directive

parl_events_week = interv_covariate(n = length(unique(eucd_ts_weeks$weeks)), tau = c(37, 69),
                                    delta = c(0.5,0.5))

colnames(parl_events_week)[1] = "PARL: EP proceeds to trilogue"
colnames(parl_events_week)[2] = "PARL: Council approves EUCD"

# b) platform actions
eucd_ts_weeks$weeks[eucd_ts_weeks$time_var == format(as.Date("2018-07-03"), "%Y-%U")] # Italian Wiki Blackout on 03.07, others announced on 03.07 and followed on 4. and 5.
eucd_ts_weeks$weeks[eucd_ts_weeks$time_var == format(as.Date("2018-10-22"), "%Y-%U")] # Youtube Banners/Ads
eucd_ts_weeks$weeks[eucd_ts_weeks$time_var == format(as.Date("2019-03-21"), "%Y-%U")] # Blackouts

plat_actions_week = interv_covariate(n = length(unique(eucd_ts_weeks$weeks)), tau = c(27, 43, 65), delta = c(0.5, 0.85, 0.5))

colnames(plat_actions_week)[1] = "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate"
colnames(plat_actions_week)[2] = "PLAT: YouTube Ads"
colnames(plat_actions_week)[3] = "PLAT: Multiple Platforms Blackout &\nPARL: EP passes EUCD"

# Create covariate vector
xreg_week = cbind(plat_actions_week, parl_events_week)

# create dependent variables
count_news_all = subset(eucd_ts_weeks, lang == "all")$count_news
count_news_en = subset(eucd_ts_weeks, lang == "en")$count_news
count_news_de = subset(eucd_ts_weeks, lang == "de")$count_news
count_news_es = subset(eucd_ts_weeks, lang == "es")$count_news
count_news_fr = subset(eucd_ts_weeks, lang == "fr")$count_news
count_news_it = subset(eucd_ts_weeks, lang == "it")$count_news
count_news_nl = subset(eucd_ts_weeks, lang == "nl")$count_news
count_news_pl = subset(eucd_ts_weeks, lang == "pl")$count_news
count_news_pt = subset(eucd_ts_weeks, lang == "pt")$count_news

count_tweets_all = subset(eucd_ts, lang == "all")$count_tweets
count_tweets_en = subset(eucd_ts, lang == "en")$count_tweets
count_tweets_de = subset(eucd_ts, lang == "de")$count_tweets
count_tweets_es = subset(eucd_ts, lang == "es")$count_tweets
count_tweets_fr = subset(eucd_ts, lang == "fr")$count_tweets
count_tweets_it = subset(eucd_ts, lang == "it")$count_tweets
count_tweets_nl = subset(eucd_ts, lang == "nl")$count_tweets
count_tweets_pl = subset(eucd_ts, lang == "pl")$count_tweets
count_tweets_pt = subset(eucd_ts, lang == "pt")$count_tweets

##### II) Analysis Tweets #####
##### All #####
# Run Poisson Regression
count_tweets_all_pois = tsglm(count_tweets_all, model = list(past_obs = c(1:4)),
                              xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_all_nbin = tsglm(count_tweets_all, model = list(past_obs = c(1:4)),
                              xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_all_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_all_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_all_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_all_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_all_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_all_pois), NegBin = scoring(count_tweets_all_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_all_nbin)
tab_all_tweets = tab_mod_count(count_tweets_all_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_tweets_all_nbin)[["coefficients"]]),
      (exp(summary(count_tweets_all_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot Effects
tweets_all_plot = effect_plot(count_tweets_all_nbin, sopa = F, xlim = 10,
                              ylab = "Intervention", title = "All",
                              subtitle = "Number of Tweets", ci90 = F, week = F)

##### DE #####
# Run Poisson Regression
count_tweets_de_pois = tsglm(count_tweets_de, model = list(past_obs = c(1:4)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_de_nbin = tsglm(count_tweets_de, model = list(past_obs = c(1:4)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_de_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_de_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_de_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_de_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_de_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_de_pois), NegBin = scoring(count_tweets_de_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_de_nbin)
tab_de_tweets = tab_mod_count(count_tweets_de_nbin)

## Plot Effects
tweets_de_plot = effect_plot(count_tweets_de_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "German",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

##### FR #####
# Run Poisson Regression
count_tweets_fr_pois = tsglm(count_tweets_fr, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_fr_nbin = tsglm(count_tweets_fr, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_fr_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_fr_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_fr_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_fr_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_fr_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_fr_pois), NegBin = scoring(count_tweets_fr_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_fr_nbin)
tab_fr_tweets = tab_mod_count(count_tweets_fr_nbin)

## Plot Effects
tweets_fr_plot = effect_plot(count_tweets_fr_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "French",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

##### IT #####
# Run Poisson Regression
count_tweets_it_pois = tsglm(count_tweets_it, model = list(past_obs = c(1)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_it_nbin = tsglm(count_tweets_it, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_it_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_it_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_it_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_it_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_it_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_it_pois), NegBin = scoring(count_tweets_it_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_it_nbin)
tab_it_tweets = tab_mod_count(count_tweets_it_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_tweets_it_nbin)[["coefficients"]]),
      (exp(summary(count_tweets_it_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot Effects
tweets_it_plot = effect_plot(count_tweets_it_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "Italian",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

##### PL #####
# Run Poisson Regression
count_tweets_pl_pois = tsglm(count_tweets_pl, model = list(past_obs = c(1:7)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_pl_nbin = tsglm(count_tweets_pl, model = list(past_obs = c(1:7)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_pl_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_pl_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_pl_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_pl_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_pl_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_pl_pois), NegBin = scoring(count_tweets_pl_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_pl_nbin)
tab_pl_tweets = tab_mod_count(count_tweets_pl_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_tweets_pl_nbin)[["coefficients"]]),
      (exp(summary(count_tweets_pl_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot Effects
tweets_pl_plot = effect_plot(count_tweets_pl_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "Polish",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

##### NL #####
# Run Poisson Regression
count_tweets_nl_pois = tsglm(count_tweets_nl, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_nl_nbin = tsglm(count_tweets_nl, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_nl_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_nl_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_nl_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_nl_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_nl_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_nl_pois), NegBin = scoring(count_tweets_nl_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_nl_nbin)
tab_nl_tweets = tab_mod_count(count_tweets_nl_nbin)

## Plot Effects
tweets_nl_plot = effect_plot(count_tweets_nl_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "Dutch",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

##### EN #####
# Run Poisson Regression
count_tweets_en_pois = tsglm(count_tweets_en, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_en_nbin = tsglm(count_tweets_en, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_en_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_en_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_en_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_en_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_en_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_en_pois), NegBin = scoring(count_tweets_en_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_en_nbin)
tab_en_tweets = tab_mod_count(count_tweets_en_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_tweets_en_nbin)[["coefficients"]]),
      (exp(summary(count_tweets_en_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot Effects
tweets_en_plot = effect_plot(count_tweets_en_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "English",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

##### ES #####
# Run Poisson Regression
count_tweets_es_pois = tsglm(count_tweets_es, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_es_nbin = tsglm(count_tweets_es, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_es_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_es_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_es_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_es_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_es_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_es_pois), NegBin = scoring(count_tweets_es_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_es_nbin)
tab_es_tweets = tab_mod_count(count_tweets_es_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_tweets_es_nbin)[["coefficients"]]),
      (exp(summary(count_tweets_es_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot Effects
tweets_es_plot = effect_plot(count_tweets_es_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "Spanish",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

##### PT #####
# Run Poisson Regression
count_tweets_pt_pois = tsglm(count_tweets_pt, model = list(past_obs = c(1)),
                             xreg = xreg, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_tweets_pt_nbin = tsglm(count_tweets_pt, model = list(past_obs = c(1:2)),
                             xreg = xreg, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_tweets_pt_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_tweets_pt_pois, main = "Marginal calibration")
#lines(marcal(count_tweets_pt_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_tweets_pt_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_tweets_pt_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_tweets_pt_pois), NegBin = scoring(count_tweets_pt_nbin))

# Negative Binomial Model is Appropriate
summary(count_tweets_pt_nbin)
tab_pt_tweets = tab_mod_count(count_tweets_pt_nbin)

## Plot Effects
tweets_pt_plot = effect_plot(count_tweets_pt_nbin, sopa = F, xlim = 10,
                             ylab = "Intervention", title = "Portuguese",
                             subtitle = "Number of Tweets", ci90 = F, week = F)

eucd_tweets_plot = plot_grid(tweets_all_plot, tweets_de_plot, tweets_pl_plot, tweets_fr_plot, tweets_en_plot, 
                             tweets_it_plot, tweets_pt_plot, tweets_nl_plot, tweets_es_plot, ncol = 1)

##### II) Analysis News #####
##### All #####
# Run Poisson Regression
count_news_all_pois = tsglm(count_news_all, model = list(past_obs = c(1)),
                            xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_all_nbin = tsglm(count_news_all, model = list(past_obs = c(1)),
                            xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_all_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_all_pois, main = "Marginal calibration")
#lines(marcal(count_news_all_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_all_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_all_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_all_pois), NegBin = scoring(count_news_all_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_all_nbin)
tab_all_news = tab_mod_count(count_news_all_nbin)

# Transform coefficients to percentage change
cbind(rownames(summary(count_news_all_nbin)[["coefficients"]]),
      (exp(summary(count_news_all_nbin)[["coefficients"]][["Estimate"]])-1)*100)

## Plot Effects
news_all_plot = effect_plot(count_news_all_nbin, sopa = F, xlim = 10,
                            ylab = "Intervention", title = "All",
                            subtitle = "Number of News Articles", ci90 = F)

##### DE #####
# Run Poisson Regression
count_news_de_pois = tsglm(count_news_de, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_de_nbin = tsglm(count_news_de, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_de_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_de_pois, main = "Marginal calibration")
#lines(marcal(count_news_de_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_de_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_de_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_de_pois), NegBin = scoring(count_news_de_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_de_nbin)
tab_de_news = tab_mod_count(count_news_de_nbin)

## Plot Effects
news_de_plot = effect_plot(count_news_de_nbin, sopa = F, xlim = 10,
                           ylab = "Intervention", title = "German",
                           subtitle = "Number of News Articles", ci90 = F)

##### FR #####
# Run Poisson Regression
count_news_fr_pois = tsglm(count_news_fr, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_fr_nbin = tsglm(count_news_fr, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_fr_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_fr_pois, main = "Marginal calibration")
#lines(marcal(count_news_fr_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_fr_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_fr_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_fr_pois), NegBin = scoring(count_news_fr_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_fr_nbin)
tab_fr_news = tab_mod_count(count_news_fr_nbin)

## Plot Effects
news_fr_plot = effect_plot(count_news_fr_nbin, sopa = F, xlim = 10,
                           ylab = "Intervention", title = "French",
                           subtitle = "Number of News Articles", ci90 = F)

##### IT #####
# Run Poisson Regression
count_news_it_pois = tsglm(count_news_it, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_it_nbin = tsglm(count_news_it, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_it_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_it_pois, main = "Marginal calibration")
#lines(marcal(count_news_it_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_it_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_it_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_it_pois), NegBin = scoring(count_news_it_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_it_nbin)
tab_it_news = tab_mod_count(count_news_it_nbin)

## Plot Effects
news_it_plot = effect_plot(count_news_it_nbin, sopa = F, xlim = 10,
                           ylab = "Intervention", title = "Italian",
                           subtitle = "Number of News Articles", ci90 = F)

##### PL #####
# Run Poisson Regression
count_news_pl_pois = tsglm(count_news_pl, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_pl_nbin = tsglm(count_news_pl, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_pl_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_pl_pois, main = "Marginal calibration")
#lines(marcal(count_news_pl_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_pl_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_pl_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_pl_pois), NegBin = scoring(count_news_pl_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_pl_nbin)
tab_pl_news = tab_mod_count(count_news_pl_nbin)

## Plot Effects
news_pl_plot = effect_plot(count_news_pl_nbin, sopa = F, xlim = 10,
                           ylab = "Intervention", title = "Polish",
                           subtitle = "Number of News Articles", ci90 = F)

##### NL #####
# Run Poisson Regression
count_news_nl_pois = tsglm(count_news_nl, model = list(past_obs = c(1:3)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_nl_nbin = tsglm(count_news_nl, model = list(past_obs = c(1:3)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_nl_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_nl_pois, main = "Marginal calibration")
#lines(marcal(count_news_nl_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_nl_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_nl_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_nl_pois), NegBin = scoring(count_news_nl_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_nl_nbin)
tab_nl_news = tab_mod_count(count_news_nl_nbin)

## Plot Effects
news_nl_plot = effect_plot(count_news_nl_nbin, sopa = F, xlim = 15,
                           ylab = "Intervention", title = "Dutch",
                           subtitle = "Number of News Articles", ci90 = F)

##### EN #####
# Run Poisson Regression
count_news_en_pois = tsglm(count_news_en, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_en_nbin = tsglm(count_news_en, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_en_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_en_pois, main = "Marginal calibration")
#lines(marcal(count_news_en_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_en_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_en_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_en_pois), NegBin = scoring(count_news_en_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_en_nbin)
tab_en_news = tab_mod_count(count_news_en_nbin)

## Plot Effects
news_en_plot = effect_plot(count_news_en_nbin, sopa = F, xlim = 10,
                           ylab = "Intervention", title = "English",
                           subtitle = "Number of News Articles", ci90 = F)

##### ES #####
# Run Poisson Regression
count_news_es_pois = tsglm(count_news_es, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_es_nbin = tsglm(count_news_es, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_es_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_es_pois, main = "Marginal calibration")
#lines(marcal(count_news_es_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_es_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_es_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_es_pois), NegBin = scoring(count_news_es_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_es_nbin)
tab_es_news = tab_mod_count(count_news_es_nbin)

## Plot Effects
news_es_plot = effect_plot(count_news_es_nbin, sopa = F, xlim = 40, xbreak = 10,
                           ylab = "Intervention", title = "Spanish",
                           subtitle = "Number of News Articles", ci90 = F)

##### PT #####
# Run Poisson Regression
count_news_pt_pois = tsglm(count_news_pt, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "poisson", link = "log")

# Run Negative Binomial Regression
count_news_pt_nbin = tsglm(count_news_pt, model = list(past_obs = c(1)),
                           xreg = xreg_week, distr = "nbinom", link = "log")

# Diagnostics
acf(residuals(count_news_pt_nbin), main = "ACF of response residuals (NegBin)")
#marcal(count_news_pt_pois, main = "Marginal calibration")
#lines(marcal(count_news_pt_nbin, plot = F), lty = "dashed")
#legend("bottomright", legend = c("Pois", "NegBin"), lwd = 1, lty = c("solid", "dashed"))
pit(count_news_pt_pois, ylim = c(0, 10), main = "PIT Poisson")
pit(count_news_pt_nbin, ylim = c(0, 5), main = "PIT Negative Binomial")

rbind(Poisson = scoring(count_news_pt_pois), NegBin = scoring(count_news_pt_nbin))

# Negative Binomial Model is Appropriate
summary(count_news_pt_nbin)
tab_pt_news = tab_mod_count(count_news_pt_nbin)

## Plot Effects
news_pt_plot = effect_plot(count_news_pt_nbin, sopa = F, xlim = 10,
                           ylab = "Intervention", title = "Portuguese",
                           subtitle = "Number of News Articles", ci90 = F)


eucd_news_plot = plot_grid(news_all_plot, news_de_plot, news_pl_plot, news_fr_plot, news_en_plot, 
                           news_it_plot, news_pt_plot, news_nl_plot, news_es_plot, ncol = 1)

eucd_plot = plot_grid(eucd_tweets_plot,eucd_news_plot, ncol = 2)

ggsave("EUCD/VAR/EUCD_Count_Plot.png", eucd_plot, height = 28, width = 18, dpi = 640)
ggsave("EUCD/VAR/EUCD_Count_Plot.pdf", eucd_plot, height = 28, width = 18, dpi = 640)

write_xlsx(tab_all_tweets, "EUCD/VAR/EUCD_Count_all_tweets_results.xlsx")
write_xlsx(tab_de_tweets, "EUCD/VAR/EUCD_Count_de_tweets_results.xlsx")
write_xlsx(tab_pl_tweets, "EUCD/VAR/EUCD_Count_pl_tweets_results.xlsx")
write_xlsx(tab_fr_tweets, "EUCD/VAR/EUCD_Count_fr_tweets_results.xlsx")
write_xlsx(tab_en_tweets, "EUCD/VAR/EUCD_Count_en_tweets_results.xlsx")
write_xlsx(tab_it_tweets, "EUCD/VAR/EUCD_Count_it_tweets_results.xlsx")
write_xlsx(tab_pt_tweets, "EUCD/VAR/EUCD_Count_pt_tweets_results.xlsx")
write_xlsx(tab_nl_tweets, "EUCD/VAR/EUCD_Count_nl_tweets_results.xlsx")
write_xlsx(tab_es_tweets, "EUCD/VAR/EUCD_Count_es_tweets_results.xlsx")

write_xlsx(tab_all_news, "EUCD/VAR/EUCD_Count_all_news_results.xlsx")
write_xlsx(tab_de_news, "EUCD/VAR/EUCD_Count_de_news_results.xlsx")
write_xlsx(tab_pl_news, "EUCD/VAR/EUCD_Count_pl_news_results.xlsx")
write_xlsx(tab_fr_news, "EUCD/VAR/EUCD_Count_fr_news_results.xlsx")
write_xlsx(tab_en_news, "EUCD/VAR/EUCD_Count_en_news_results.xlsx")
write_xlsx(tab_it_news, "EUCD/VAR/EUCD_Count_it_news_results.xlsx")
write_xlsx(tab_pt_news, "EUCD/VAR/EUCD_Count_pt_news_results.xlsx")
write_xlsx(tab_nl_news, "EUCD/VAR/EUCD_Count_nl_news_results.xlsx")
write_xlsx(tab_es_news, "EUCD/VAR/EUCD_Count_es_news_results.xlsx")

## Plot only significant effects
# news
eucd_news_plot = news_all_plot

# tweets
tweets_en_plot = tweets_en_plot + theme(axis.text.y = element_blank())
tweets_it_plot = tweets_it_plot + theme(axis.text.y = element_blank())

eucd_tweets_plot = plot_grid(tweets_all_plot, tweets_en_plot, tweets_es_plot, tweets_it_plot, nrow = 2,
                             rel_widths = c(1.25,1,1.25,1))

eucd_plot = plot_grid(eucd_news_plot, eucd_tweets_plot, ncol = 1, rel_heights = c(1,2), labels = c("Newspaper", "Tweets"))

ggsave("EUCD/VAR/EUCD_Count_Plot_red.png", eucd_plot, height = 8, width = 12, dpi = 640)
ggsave("EUCD/VAR/EUCD_Count_Plot_red.pdf", eucd_plot, height = 8, width = 12, dpi = 640)
