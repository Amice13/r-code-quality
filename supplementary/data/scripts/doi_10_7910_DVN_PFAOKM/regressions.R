options(stringsAsFactors = F)

library(stargazer)#+
library(lmtest) #+
library(sandwich) #+
library(sjPlot)#+
library(ggplot2)#+
require(gridExtra)#+
library(patchwork)
library(tidyverse)

df_main <- read_csv("data/stats_table.csv", 
                               col_types = cols(Year = col_date(format = "%Y-%m-%d"), 
                                                Time = col_date(format = "%Y-%m-%d")))


lm_exchange_rate <- lm(log1p(`Exchange Rate`) ~ QE + Central_Bank + Paragraph, data=df_main)
lm_house_prices <- lm(log1p(`House Prices`) ~ QE + Central_Bank + Paragraph, data=df_main)
lm_climate <- lm(log1p(`Climate Change`) ~ QE + Central_Bank + Paragraph, data=df_main)
lm_private_debt <- lm(log1p(`Corporate Debt`) ~ QE + Central_Bank + Paragraph, data=df_main)

lm_transparency <- lm(log1p(Transparency) ~ QE +  Paragraph + Central_Bank, data=df_main)
lm_macroprudential <- lm(log1p(Macroprudential) ~ QE +  Paragraph + Central_Bank, data=df_main)
lm_share <- lm(log1p(`Share Prices`) ~ QE +  Paragraph + Central_Bank, data=df_main)
lm_fiscal_policy <- lm(log1p(`Fiscal Policy`) ~ QE + Paragraph + Central_Bank, data=df_main)
lm_independence <- lm(log1p(`Independence`) ~ QE + Paragraph + Central_Bank, data=df_main)

rob_a<- coeftest(lm_house_prices, vcov = vcovHC(lm_house_prices, "HC0"))
rob_b<- coeftest(lm_exchange_rate, vcov = vcovHC(lm_exchange_rate, "HC0"))
rob_c<- coeftest(lm_climate, vcov = vcovHC(lm_climate, "HC0"))
rob_d<- coeftest(lm_private_debt, vcov = vcovHC(lm_private_debt, "HC0"))

rob_f<- coeftest(lm_share, vcov = vcovHC(lm_share, "HC0"))
rob_g<- coeftest(lm_macroprudential, vcov = vcovHC(lm_macroprudential, "HC0"))
rob_h<- coeftest(lm_transparency, vcov = vcovHC(lm_transparency, "HC0"))
rob_i<- coeftest(lm_independence, vcov = vcovHC(lm_independence, "HC0"))

##############################################################################
##############################################################################

lm_exchange_rate1 <- lm(log1p(`Exchange Rate`) ~ QE + Central_Bank   + Paragraph + Inflation  + Interest_Rate + log1p(GDP) + Fin_develop + as.factor(Year) + NEER  , data=df_main)
lm_house_prices1 <- lm(log1p(`House Prices`) ~ QE + Central_Bank  + Paragraph + Inflation + Interest_Rate + log1p(GDP) + Fin_develop  + Property_prices + as.factor(Year) , data=df_main)
lm_private_debt1 <- lm(log1p(`Corporate Debt`) ~ QE + Central_Bank  + Paragraph + Inflation + Interest_Rate + log1p(GDP) +  Fin_develop + as.factor(Year)  , data=df_main)
lm_climate1 <- lm(log1p(`Climate Change`) ~ QE + Central_Bank  + Paragraph + Inflation + Interest_Rate + log1p(GDP)  + Fin_develop + as.factor(Year) , data=df_main)

lm_share1 <- lm(log1p(`Share Prices`) ~ QE + Central_Bank + Paragraph + Inflation + Interest_Rate + log1p(GDP) + Fin_develop +as.factor(Year) , data=df_main)
lm_fiscal_policy1 <- lm(log1p(`Fiscal Policy`) ~ QE + Central_Bank + Paragraph + Inflation + Interest_Rate + log1p(GDP) + Fin_develop + as.factor(Year) , data=df_main)
lm_transparency1 <- lm(log1p(Transparency) ~ QE + Central_Bank + Paragraph + Inflation + Interest_Rate + log1p(GDP) + Fin_develop + as.factor(Year) , data=df_main)
lm_macroprudential1 <- lm(log1p(Macroprudential) ~QE + Central_Bank + Paragraph + Inflation + Interest_Rate + log1p(GDP) + Fin_develop + as.factor(Year)  , data=df_main)
lm_independence1 <- lm(log1p(`Independence`) ~ QE + Paragraph + Central_Bank + Inflation + Interest_Rate + log1p(GDP) + Fin_develop + as.factor(Year), data=df_main)

rob_a1<- coeftest(lm_house_prices1, vcov = vcovHC(lm_house_prices1, "HC0"))
rob_b1<- coeftest(lm_exchange_rate1, vcov = vcovHC(lm_exchange_rate1, "HC0"))
rob_c1<- coeftest(lm_climate1, vcov = vcovHC(lm_climate1, "HC0"))
rob_d1<- coeftest(lm_private_debt1, vcov = vcovHC(lm_private_debt1, "HC0"))

rob_f1<- coeftest(lm_share1, vcov = vcovHC(lm_share1, "HC0"))
rob_g1<- coeftest(lm_macroprudential1, vcov = vcovHC(lm_macroprudential1, "HC0"))
rob_h1<- coeftest(lm_transparency1, vcov = vcovHC(lm_transparency1, "HC0"))
rob_i1<- coeftest(lm_independence1, vcov = vcovHC(lm_independence1, "HC0"))

stargazer(lm_house_prices, lm_house_prices1, lm_exchange_rate, lm_exchange_rate1, lm_climate, lm_climate1,lm_private_debt,lm_private_debt1,
          dep.var.labels.include = FALSE,column.labels = c("House prices", "House prices", "Exchange rate", "Exchange rate", "Global warming", "Global warming","Corporate Debt","Corporate Debt"),type = "html",out = "OLSallBaselineControl1.html",omit = c("Central_Bank","Year","Constant","Time"),
          se = list(rob_a[,"Std. Error"], rob_a1[,"Std. Error"], rob_b[,"Std. Error"], rob_b1[,"Std. Error"], rob_c[,"Std. Error"], rob_c1[,"Std. Error"], rob_d[,"Std. Error"], rob_d1[,"Std. Error"]), title = "Baseline models and control variables")

stargazer(lm_share, lm_share1,lm_macroprudential, lm_macroprudential1, dep.var.labels.include = FALSE,column.labels = c("Share prices","Share prices","Macroprudential","Macroprudential","Independence"),type = "html",out = "OLSallBaselineControl2.html",omit = c("Central_Bank","Year","Constant"), 
          se = list(rob_f[,"Std. Error"],rob_f1[,"Std. Error"],rob_g[,"Std. Error"],rob_g1[,"Std. Error"]), title = "Baseline models and control variables")

##############################################################################
##############################################################################

# Interaction models

df_main$ECBBoJ <- ifelse(df_main$Central_Bank %in% c("ECB","Bank of Japan"), 1, 0)
df_main$FedBoERiks <- ifelse(df_main$Central_Bank %in% c("FED","Bank of England", "Bank of Sweden"), 1, 0)
df_main$ECBBoJBoE <- ifelse(df_main$Central_Bank %in% c("ECB","Bank of Japan","Bank of England"), 1, 0)
df_main$Fed <- ifelse(df_main$Central_Bank %in% c("FED"), 1, 0)

##############################################################################

lm_exchange_rate <- lm(log1p(`Exchange Rate`) ~ QE + QE:ECBBoJ + ECBBoJ + Paragraph + Central_Bank, data=df_main)
lm_house_prices <- lm(log1p(`House Prices`) ~ QE + QE:FedBoERiks + FedBoERiks + Paragraph + Central_Bank, data=df_main)
lm_climate <- lm(log1p(`Climate Change`) ~ QE + QE:ECBBoJBoE + ECBBoJBoE + Paragraph + Central_Bank, data=df_main)
lm_corporate_debt <- lm(log1p(`Corporate Debt`) ~ QE + QE:ECBBoJBoE + ECBBoJBoE + Paragraph + Central_Bank, data=df_main)

rob_a<- coeftest(lm_house_prices, vcov = vcovHC(lm_house_prices, "HC0"))
rob_b<- coeftest(lm_exchange_rate, vcov = vcovHC(lm_exchange_rate, "HC0"))
rob_c<- coeftest(lm_climate, vcov = vcovHC(lm_climate, "HC0"))
rob_d<- coeftest(lm_corporate_debt, vcov = vcovHC(lm_corporate_debt, "HC0"))

##############################################################################

lm_exchange_rate1 <- lm(log1p(`Exchange Rate`) ~ QE + QE:ECBBoJ + ECBBoJ + Paragraph + Central_Bank + as.factor(Year) + log1p(GDP) + Inflation + Interest_Rate + Fin_develop + NEER, data=df_main)
lm_house_prices1 <- lm(log1p(`House Prices`) ~ QE + QE:FedBoERiks + FedBoERiks + Paragraph + Central_Bank + as.factor(Year) + log1p(GDP) + Inflation + Interest_Rate + Fin_develop + Property_prices, data=df_main)
lm_climate1 <- lm(log1p(`Climate Change`) ~ QE + QE:ECBBoJBoE + ECBBoJBoE + Paragraph + Central_Bank + as.factor(Year) + log1p(GDP) + Inflation + Fin_develop + Interest_Rate, data=df_main)
lm_corporate_debt1 <- lm(log1p(`Corporate Debt`) ~ QE + QE:ECBBoJBoE + ECBBoJBoE + Paragraph + Central_Bank + as.factor(Year) + log1p(GDP) + Inflation + Fin_develop + Interest_Rate, data=df_main)

rob_a1<- coeftest(lm_house_prices1, vcov = vcovHC(lm_house_prices1, "HC0"))
rob_b1<- coeftest(lm_exchange_rate1, vcov = vcovHC(lm_exchange_rate1, "HC0"))
rob_c1<- coeftest(lm_climate1, vcov = vcovHC(lm_climate1, "HC0"))
rob_d1<- coeftest(lm_corporate_debt1, vcov = vcovHC(lm_corporate_debt1, "HC0"))

stargazer(lm_house_prices, lm_house_prices1, lm_exchange_rate, lm_exchange_rate1, 
          lm_climate, lm_climate1, lm_corporate_debt1, lm_corporate_debt1,
          dep.var.labels.include = FALSE,
          column.labels = c("House prices","House Prices","Exchange rate","Exchange rate","Climate change","Climate change","Corporate debt","Corporate debt"),
          type = "latex",
          out = "FurtherHypothesis_interact.tex",
          omit = c("Central_Bank","Year","Constant"),
          se = list(rob_a[,"Std. Error"],rob_a1[,"Std. Error"],rob_b[,"Std. Error"],rob_b1[,"Std. Error"],rob_c[,"Std. Error"],rob_c1[,"Std. Error"],rob_d[,"Std. Error"],rob_d1[,"Std. Error"]), title = "Testing for country-specific factors: Selected QE central banks versus non-QE central banks (Interact!)")

stargazer(lm_house_prices, lm_house_prices1, lm_exchange_rate, lm_exchange_rate1, 
          lm_climate, lm_climate1, lm_corporate_debt, lm_corporate_debt1,
          dep.var.labels.include = FALSE,
          column.labels = c("House prices","House Prices","Exchange rate","Exchange rate","Climate change","Climate change","Corporate debt","Corporate debt"),
          type = "html",
          out = "FurtherHypothesis_interact_new_groups.html",
          omit = c("Central_Bank","Year","Constant"),
          se = list(rob_a[,"Std. Error"],rob_a1[,"Std. Error"],rob_b[,"Std. Error"],rob_b1[,"Std. Error"],rob_c[,"Std. Error"],rob_c1[,"Std. Error"],rob_d[,"Std. Error"],rob_d1[,"Std. Error"]), title = "Testing for country-specific factors: Selected QE central banks versus non-QE central banks (Interact!)")

#Interaction plot
plot1 <- qplot(x=x,y=predicted,linetype=group,data = x[x$Topic%in%c("House prices","Exchange rate"),]) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),width=0.2) +
  facet_wrap(~fct_rev(Topic),scales = "free") +
  xlab("") +
  ylab("Predicted frequency") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(labels=c("0" = "Non-QE", "1" = "QE"),breaks = c(0,1))

plot2 <- qplot(x=x,y=predicted,linetype=group,data = x[x$Topic%in%c("Climate change","Corporate debt"),]) + 
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),width=0.2) +
  facet_wrap(~Topic,scales = "free") +
  xlab("Time period: Presence or absence of QE program") +
  ylab("Predicted frequency") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  scale_x_continuous(labels=c("0" = "Non-QE", "1" = "QE"),breaks = c(0,1))

plot1 / plot2

ggsave("plots/interactions.eps", device = "eps")
