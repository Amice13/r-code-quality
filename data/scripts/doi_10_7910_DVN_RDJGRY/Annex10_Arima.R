source('scripts/cleaning_and_management/libraries.R') 

expanel <- readRDS("data/Delphi survey/expanel.rds") 

# Migration data: Frontex and Eurostat

# all flows
flows <- readRDS("data/Migration data/clean(Frontex+Eurostat)/forecast.rds")

expanel <- expanel %>% 
  dplyr::select(variable, type, scenario_name, value, analised_sample, wave, id) %>% 
  filter(variable!="prob", 
         type =="flow",
         wave=="wave2",
         analised_sample==1) %>% 
  mutate(scenario_name = str_replace(scenario_name, "Scenario ", "")) %>% 
  group_by(id, variable) %>% 
  summarise(value = mean(value, na.rm = T))



# FUNCTION --------------------------------------------------------------------

# note for linear forecast: 
# to avoid negative values, use log transform https://stats.stackexchange.com/questions/145383/getting-negative-predicted-values-after-linear-regression

# define flows
flows_ch <- unique(flows$flow)

# add customised limits - necessary for a better visualisation of asylum and irregular 
lims_linear_low <- c(1501008,
                     450000,
                     30000,
                     160000,
                     140000)

lims_linear_high <- c(4500000, 
                      1800000, 
                      200000, 
                      1900000,
                      2000000)

Ytitle <- c("Total immigration (millions)" ,
            "Labour immigration (thousands)",
            "High-skilled immigration (thousands)",
            "Irregular border crossings (thousands)",
            "Asylum applications (thousands)")

# scales

scaleFUN_million <- label_number(scale = 1 / 1e6,
                                 accuracy = 1)

scaleFUN_th <- label_number(scale = 1 / 1e3,
                            accuracy = 1)

Scales <- c(scaleFUN_million, scaleFUN_th, scaleFUN_th, scaleFUN_th, scaleFUN_th )

# titles

Arima_title_1 <- c("ARIMA (0,1,0)", "", "", "", "")

Linear_title_1 <- c("Linear", "", "", "", "")

Linearout_title_1 <- c("Linear with smoothed 2015/16", "", "", "", "")

Label_extended <- c("2030 forecast extended", "", "", "", "")

recorded <- c("Recoded flow", "", "", "", "")

recorded_y <- c(2900000, NULL, NULL, NULL, NULL)

recorded_x <- c(2012, NULL, NULL, NULL, NULL )

Yposition <- c(4000000, 1600000,150000,1500000,1500000)

tema1 <- theme(legend.position = "bottom",
               legend.title = element_text(size = 12),
               legend.text = element_text(size = 12),
               axis.line.y = element_blank(),
               # axis.title.y = element_text(angle = 0,
               #                             vjust = .5),
               panel.grid.major.y = element_line(colour = "grey80", size = 0.5),
               axis.ticks.y = element_line(colour = "grey80", size = 0.5),
               axis.title.x.top = element_text(hjust = 0), # takes a lot of space, so better only title
               plot.title = element_text(size = 14, hjust = 0.5),
               plot.title.position = "plot",
               text=element_text(size=16,  family="Times New Roman"))

tema2 <- theme(legend.position = "none",
               legend.title = element_text(size = 12),
               legend.text = element_text(size = 12),
               axis.line.y = element_blank(),
               panel.grid.major.y = element_line(colour = "grey80"),
               #axis.ticks.y = element_line(colour = "grey80", size = 0.5),
               axis.ticks.y = element_blank(),
               axis.text.y=element_blank(),
               # axis.title.x.top = element_text(margin = margin(0, 0, 0, 0))
               plot.title = element_text(size = 14, hjust = 0.5),
               plot.subtitle = element_text(size = 14, hjust = 0.5),
               text=element_text(size=16,  family="Times New Roman")) 

colors <- c("95%" = "#d9d9d9",
            "80%" = "#bdbdbd",
            "50% with mean forecast" = "#969696")



######################################################################################################################
#### ARIMA and linear forecasts compared to expert estimates (mean and confidence intervals) ####

ci_colors <- c("95%" = .05,
               "80%" = .2,
               "50% with mean estimate" = NA)


# --------------------------------------------------------------------------------------------
# ARIMA  -------------------------------------------------------------------------------------

forecast_fun = function(d, lim_low, lim_high, ytitle, scales, arima_title_1, yposition, label_extended,
                        Recorded, Recorded_y, Recorded_x){
  
  tot <- flows %>% 
    filter(flow==d)
  
  tsdata_tot <- tot %>% 
    dplyr::select(val) %>% 
    ts(start = head(tot$year, n = 1))
  
  tot_arima <- arima(log(tsdata_tot), c(0,1,0), method = "ML")
  
  # mean point forecast - bias adjusted = T 
  
  tot_forecast <- forecast::forecast(tot_arima, 
                                     h = 2030-tail(tot$year, n = 1),
                                     lambda = 0, 
                                     biasadj = T,
                                     level = c(50, 80, 95))
  
  bd <- as_tibble(tot_forecast) %>% 
    mutate(year = (tail(tot$year, n = 1)+1):2030)
  
  # add expert box plots
  
  expanel_total <- expanel %>% 
    drop_na() %>% 
    ungroup() %>% 
    filter(variable==d) %>% 
    summarise(
      cl_95=mean_cl_normal(value, conf.int=.95),
      cl_80=mean_cl_normal(value, conf.int=.80),
      cl_50=mean_cl_normal(value, conf.int=.50))
  
  # visualisation 
  
  ggplot() +
    geom_line(data=tot, aes(x=year,y=val), size=1.5)+
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 95`,ymax=`Hi 95`, fill = "95%")) +
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 80`,ymax=`Hi 80`, fill = "80%"))+
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 50`,ymax=`Hi 50`, fill = "50% with mean forecast"))+
    geom_line(data = bd,
              aes(x=year, y =`Point Forecast`),
              size = 1)+
    
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_50$y,
                      ymin = expanel_total$cl_50$ymin,
                      ymax = expanel_total$cl_50$ymax,
                      alpha = "50% with mean estimate"),
                  colour = NA,
                  fill="#00CCFF")+
    
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_80$y,
                      ymin = expanel_total$cl_80$ymin,
                      ymax = expanel_total$cl_80$ymax,
                      alpha = "80%"),
                  colour = NA,
                  fill="#00CCFF")+
    
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_95$y,
                      ymin = expanel_total$cl_95$ymin,
                      ymax = expanel_total$cl_95$ymax,
                      alpha = "95%"),
                  colour = "black",
                  fill="#00CCFF")+
    
    geom_text(
      aes(
        y=Recorded_y,
        x=Recorded_x,
        label = Recorded),
      color="black")+
    coord_cartesian(
      clip = "on",
      ylim=c(lim_low, lim_high)) +
    labs(title = NULL,
         subtitle = ytitle,
         x=NULL, 
         y=NULL)+
    scale_y_continuous(labels = scales) +
    scale_x_continuous(breaks = c(2010, 2015, 2020, 2025, 2030),
                       label = c("2010", "15", "20", "25", "2030")) +
    theme_classic(base_size = 12)   +
    tema1 +
    scale_fill_manual(values = colors,
                      name = "Prediction intervals") +
    
    scale_alpha_manual(values = ci_colors,
                       name = "Mean expert estimates and confidence intervals")+
    
    guides(fill = guide_legend(nrow = 1),
           alpha = guide_legend(nrow = 1))
}

list_forecasts_arima <- pmap(list(flows_ch, 
                                  lims_linear_low, 
                                  lims_linear_high, 
                                  Ytitle,
                                  Scales, 
                                  Arima_title_1,
                                  Yposition,
                                  Label_extended,
                                  recorded, 
                                  recorded_y, 
                                  recorded_x) , 
                             forecast_fun)

list_forecasts_arima[[2]]

# --------------------------------------------------------------------------------------------
# LINEAR COMPLETE DATA -----------------------------------------------------------------------

# define function

forecast_fun_lin = function(d, lim_low, lim_high,
                            # year_label,
                            scales, linear_title_1, yposition, label_extended){
  
  tot <- flows %>% 
    filter(flow==d)
  
  tot_ts <- ts(tot, start = head(tot$year, n = 1))
  
  fit.consBest <- forecast::tslm(
    val ~ year,
    data=tot_ts)
  
  # out of sample years
  newdata <- data.frame(
    year = c((tail(tot$year,n=1)+1):2030))
  
  # object with forecasted linear including confidence levels for prediction intervals (80 and 95)
  tot_forecast <- forecast::forecast(fit.consBest, 
                                     newdata = newdata,
                                     level = c(50, 80, 95))
  
  bd <- as_tibble(tot_forecast) %>% 
    mutate(year = (tail(tot$year, n = 1)+1):2030)
  
  # add expert box plots
  
  expanel_total <- expanel %>% 
    drop_na() %>% 
    ungroup() %>% 
    filter(variable==d) %>% 
    summarise(
      cl_95=mean_cl_normal(value, conf.int=.95),
      cl_80=mean_cl_normal(value, conf.int=.80),
      cl_50=mean_cl_normal(value, conf.int=.50))
  
  # visualisation 
  
  ggplot() +
    geom_line(data=tot, aes(x=year,y=val), size=1.5)+
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 95`,ymax=`Hi 95`, fill = "95%")) +
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 80`,ymax=`Hi 80`, fill = "80%"))+
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 50`,ymax=`Hi 50`, fill = "50% with mean forecast"))+
    geom_line(data = bd,
              aes(x=year, y =`Point Forecast`),
              size = 1)+
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_50$y,
                      ymin = expanel_total$cl_50$ymin,
                      ymax = expanel_total$cl_50$ymax,
                      alpha = "50% with mean estimate"),
                  colour = NA,
                  fill="#00CCFF")+
    
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_80$y,
                      ymin = expanel_total$cl_80$ymin,
                      ymax = expanel_total$cl_80$ymax,
                      alpha = "80%"),
                  colour = NA,
                  fill="#00CCFF")+
    
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_95$y,
                      ymin = expanel_total$cl_95$ymin,
                      ymax = expanel_total$cl_95$ymax,
                      alpha = "95%"),
                  colour = "black",
                  fill="#00CCFF")+
    coord_cartesian(
      clip = "on",
      ylim=c(lim_low, lim_high)) +
    labs(title = NULL,
         x= NULL, #linear_title_1,
         y=NULL)+
    scale_y_continuous(labels = scales) +
    scale_x_continuous(breaks = c(2010, 2015, 2020, 2025, 2030),
                       label = c("2010", "15", "20", "25", "2030")) +
    theme_classic(base_size = 12)   +
    tema2 +
    scale_fill_manual(values = colors,
                      name = "Prediction intervals") +
    scale_alpha_manual(values = ci_colors,
                       name = "Mean expert estimates and confidence intervals")
  
}

list_forecasts <- pmap(list(flows_ch, 
                            lims_linear_low, 
                            lims_linear_high, 
                            # Year_label,
                            Scales,
                            Linear_title_1,
                            Yposition,
                            Label_extended), 
                       
                       forecast_fun_lin)

list_forecasts[[5]]


# --------------------------------------------------------------------------------------------
# Linear with imputed outliers 2015 and 2016 -------------------------------------------------

flows_imp <- flows %>% 
  group_by(flow) %>% 
  mutate(val = 
           case_when(year == 2015 | year==2016 ~ NA_real_,
                     T ~ val),
         # Missing Value Imputation by Weighted Moving Average using 4 observations (2 left, 2 right)  
         val = na_ma(val,k=2, weighting = "simple"))

forecast_fun_out = function(d, lim_low, lim_high, scales, linearout_title_1, yposition, label_extended){
  
  tot <- flows_imp %>% 
    filter(flow==d)
  
  tot_ts <- ts(tot, start = head(tot$year, n = 1))
  
  fit.consBest <- forecast::tslm(
    val ~ year,
    data=tot_ts)
  
  # out of sample years
  newdata <- data.frame(
    year = c((tail(tot$year,n=1)+1):2030))
  
  # object with forecasted linear including confidence levels for prediction intervals (80 and 95)
  tot_forecast <- forecast::forecast(fit.consBest, 
                                     newdata = newdata,
                                     level = c(50, 80, 95))
  
  bd <- as_tibble(tot_forecast) %>% 
    mutate(year = (tail(tot$year, n = 1)+1):2030)
  
  # add expert box plots
  
  expanel_total <- expanel %>% 
    drop_na() %>% 
    ungroup() %>% 
    filter(variable==d) %>% 
    summarise(
      cl_95=mean_cl_normal(value, conf.int=.95),
      cl_80=mean_cl_normal(value, conf.int=.80),
      cl_50=mean_cl_normal(value, conf.int=.50))
  
  # visualisation 
  
  ggplot() +
    geom_line(data=tot, aes(x=year,y=val), size=1.5)+
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 95`,ymax=`Hi 95`, fill = "95%")) +
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 80`,ymax=`Hi 80`, fill = "80%"))+
    geom_ribbon(data = bd,
                aes(x = year, ymin=`Lo 50`,ymax=`Hi 50`, fill = "50% with mean forecast"))+
    geom_line(data = bd,
              aes(x=year, y =`Point Forecast`),
              size = 1)+
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_50$y,
                      ymin = expanel_total$cl_50$ymin,
                      ymax = expanel_total$cl_50$ymax,
                      alpha = "50% with mean estimate"),
                  colour = NA,
                  fill="#00CCFF")+
    
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_80$y,
                      ymin = expanel_total$cl_80$ymin,
                      ymax = expanel_total$cl_80$ymax,
                      alpha = "80%"),
                  colour = NA,
                  fill="#00CCFF")+
    
    geom_crossbar(aes(x=2030,
                      y=expanel_total$cl_95$y,
                      ymin = expanel_total$cl_95$ymin,
                      ymax = expanel_total$cl_95$ymax,
                      alpha = "95%"),
                  colour = "black",
                  fill="#00CCFF")+
    coord_cartesian(
      clip = "on",
      ylim=c(lim_low, lim_high)) +
    labs(title = NULL,
         x= NULL, #linearout_title_1,
         y=NULL)+
    scale_y_continuous(labels = scales) +
    scale_x_continuous(breaks = c(2010, 2015, 2020, 2025, 2030),
                       label = c("2010", "15", "20", "25", "2030")) +
    theme_classic(base_size = 12)   +
    tema2 +
    scale_fill_manual(values = colors,
                      name = "Prediction intervals") +
    scale_alpha_manual(values = ci_colors,
                       name = "Mean expert estimates and confidence intervals")
}

list_forecasts_imp <- pmap(list(flows_ch, 
                                lims_linear_low, 
                                lims_linear_high, 
                                Scales,
                                Linearout_title_1,
                                Yposition,
                                Label_extended) , 
                           forecast_fun_out)

list_forecasts_imp[[1]]

# -----------------------------------------------------------------------------------------

# Put all together

### attempt 3

final <- list_forecasts_arima[[1]] + ggtitle("ARIMA(0,1,0)")+
  list_forecasts[[1]] + ggtitle("Linear")+ 
  list_forecasts_imp[[1]] + ggtitle("Linear with smoothed 2015/16")+
  
  list_forecasts_arima[[2]]+
  list_forecasts[[2]]+
  list_forecasts_imp[[2]]+ 
  
  list_forecasts_arima[[3]] + 
  list_forecasts[[3]]+
  list_forecasts_imp[[3]]+ 
  
  list_forecasts_arima[[4]]+
  list_forecasts[[4]]+
  list_forecasts_imp[[4]]+
  
  list_forecasts_arima[[5]]+
  list_forecasts[[5]]+ xlab("Year")+  #labs(subtitle = "Year")+ theme(plot.title.position = )
  list_forecasts_imp[[5]]+
  
  plot_layout(nrow = 5, ncol = 3) +
  plot_layout(guides='collect') &
  theme(legend.position='bottom',
        legend.box = "vertical")



cairo_ps("output/figures/Annex10_Arima.eps", 
         family = "Times New Roman",
         width = 10, height = 10)

final
dev.off()
