############################################

##  Get start and end dates from main code 
Data.Start <- "2012-05-18"
Data.End <- "2013-06-28"

Data <- pdfetch_YAHOO(c("^GSPC", "CMCSA", "CHTR", "T", "VZ", "CTL", "GOOGL", "MSFT", "AAPL", "FB"), fields = c("close"), from= as.Date(Data.Start), to = as.Date(Data.End), interval = "1d")

Data <- as.data.frame(Data)

names(Data) <- c("SP500", "Comcast", "Charter", "Att", "Verizon", "Century" , "Google", "Microsoft", "Apple", "Facebook")

Data <- Data %>% 
  mutate(Data, lnSP500 = log(SP500)) %>%
  mutate(Data, lnComcast = log(Comcast)) %>%
  mutate(Data, lnCharter = log(Charter)) %>% 
  mutate(Data, lnAtt = log(Att)) %>% 
  mutate(Data, lnVerizon = log(Verizon)) %>% 
  mutate(Data, lnCentury = log(Century)) %>% 
  mutate(Data, lnGoogle = log(Google)) %>% 
  mutate(Data, lnMicrosoft = log(Microsoft)) %>%
  mutate(Data, lnApple = log(Apple)) %>% 
  mutate(Data, lnFacebook = log(Facebook))

column_order <- c("SP500", "lnSP500", "Comcast", "lnComcast", "Charter", "lnCharter", "Att", "lnAtt", "Verizon", "lnVerizon", "Century", "lnCentury", "Google", "lnGoogle", "Microsoft", "lnMicrosoft", "Apple", "lnApple", "Facebook", "lnFacebook")
Data <- Data[, column_order]

Data <- Data %>% 
  mutate(Data, lnSP500.Diff = lnSP500 - lag(lnSP500)) %>%
  mutate(Data, lnComcast.Diff = lnComcast - lag(lnComcast)) %>% 
  mutate(Data, lnCharter.Diff = lnCharter - lag(lnCharter)) %>% 
  mutate(Data, lnAtt.Diff = lnAtt - lag(lnAtt)) %>%
  mutate(Data, lnVerizon.Diff = lnVerizon - lag(lnVerizon)) %>% 
  mutate(Data, lnCentury.Diff = lnVerizon - lag(lnVerizon)) %>% 
  mutate(Data, lnGoogle.Diff = lnGoogle - lag(lnGoogle)) %>% 
  mutate(Data, lnMicrosoft.Diff = lnMicrosoft - lag(lnMicrosoft)) %>% 
  mutate(Data, lnApple.Diff = lnApple - lag(lnApple)) %>% 
  mutate(Data, lnFacebook.Diff = lnFacebook - lag(lnFacebook)) 

column_order <- c("SP500", "lnSP500", "lnSP500.Diff", "Comcast", "lnComcast", "lnComcast.Diff", "Charter", "lnCharter", "lnCharter.Diff",  "Att", "lnAtt", "lnAtt.Diff", "Verizon", "lnVerizon", "lnVerizon.Diff", "Century", "lnCentury", "lnCentury.Diff", "Google", "lnGoogle", "lnGoogle.Diff", "Microsoft", "lnMicrosoft", "lnMicrosoft.Diff",  "Apple", "lnApple", "lnApple.Diff", "Facebook", "lnFacebook", "lnFacebook.Diff")
Data <- Data[, column_order]
Dates <- rownames(Data)



### Yahoo Data


Yahoo_Dummy_Data <- read_csv("~/Dropbox/Jenna/R Files/Yahoo Dummy Data.csv")
Yahoo_Dummy_Data <- Yahoo_Dummy_Data[278:1, 3]
Data <- cbind(Data, Yahoo_Dummy_Data)
Data <- Data %>% 
  mutate(lnYahoo = log(Yahoo)) %>%
  mutate(lnYahoo.Diff = lnYahoo - lag(lnYahoo))


### AOL Data


AOL_Dummy_Data <- read_csv("~/Dropbox/Jenna/R Files/AOL Dummy Data.csv")
AOL_Dummy_Data <- AOL_Dummy_Data[278:1, ]
Data <- cbind(Data, AOL_Dummy_Data) 
Data <- Data %>%
  mutate(lnAOL = log(AOL)) %>% 
  mutate(lnAOL.Diff = lnAOL - lag(lnAOL))



#Let 6/14 and 6/22 be the event dates of the protests and complaint respectively 
Data$LM1 <- ifelse(Dates == "2013-06-06", 1, 0)
Data$LD <- ifelse(Dates == "2013-06-07", 1, 0)
Data$LP1 <- ifelse(Dates == "2013-06-10", 1, 0)
Data$LP2 <- ifelse(Dates == "2013-06-11", 1, 0)
Data$LP3 <- ifelse(Dates == "2013-06-12", 1, 0)
Data$PM1 <- ifelse(Dates == "2013-06-14", 1, 0)
Data$PP1 <- ifelse(Dates == "2013-06-17", 1, 0)
Data$PP2 <- ifelse(Dates == "2013-06-18", 1, 0)
Data$PP3 <- ifelse(Dates == "2013-06-19", 1, 0)
Data$CM1 <- ifelse(Dates == "2013-06-21", 1, 0)
Data$CP1 <- ifelse(Dates == "2013-06-24", 1, 0)
Data$CP2 <- ifelse(Dates == "2013-06-25", 1, 0)
Data$CP3 <- ifelse(Dates == "2013-06-26", 1, 0)


################################################
## Parameter estimation
## Regressions of each stock on the index

LNSP500.Event.Diff <- Data$lnSP500.Diff
##MO: What does this do?

Comcast.Est.Reg <- lm(lnComcast.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Comcast.Est.Reg)
#Comcast.Est.Reg.Intercept <- Comcast.Est.Reg$coefficients[1]
#Comcast.Est.Reg.Slope <- Comcast.Est.Reg$coefficients[2]
Comcast.T.DVM <- (summary(Comcast.Est.Reg)$coefficients[,1]/summary(Comcast.Est.Reg)$coefficients[,2])[-(1:2)]


Charter.Est.Reg <- lm(lnCharter.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Charter.Est.Reg)
#Charter.Est.Reg.Intercept <- Charter.Est.Reg$coefficients[1]
#Charter.Est.Reg.Slope <- Charter.Est.Reg$coefficients[2]
Charter.T.DVM <- (summary(Charter.Est.Reg)$coefficients[,1]/summary(Charter.Est.Reg)$coefficients[,2])[-(1:2)]

Att.Est.Reg <- lm(lnAtt.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Att.Est.Reg)
#Att.Est.Reg.Intercept <- Att.Est.Reg$coefficients[1]
#Att.Est.Reg.Slope <- Att.Est.Reg$coefficients[2]
ATT.T.DVM <- (summary(Att.Est.Reg)$coefficients[,1]/summary(Att.Est.Reg)$coefficients[,2])[-(1:2)]


Verizon.Est.Reg <- lm(lnVerizon.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Verizon.Est.Reg)
#Verizon.Est.Reg.Intercept <- Verizon.Est.Reg$coefficients[1]
#Verizon.Est.Reg.Slope <- Verizon.Est.Reg$coefficients[2]
Verizon.T.DVM <- (summary(Verizon.Est.Reg)$coefficients[,1]/summary(Verizon.Est.Reg)$coefficients[,2])[-(1:2)]


Century.Est.Reg <- lm(lnCentury.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Century.Est.Reg)
#Century.Est.Reg.Intercept <- Century.Est.Reg$coefficients[1]
#Century.Est.Reg.Slope <- Century.Est.Reg$coefficients[2]
Century.T.DVM <- (summary(Century.Est.Reg)$coefficients[,1]/summary(Century.Est.Reg)$coefficients[,2])[-(1:2)]


Google.Est.Reg <- lm(lnGoogle.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Google.Est.Reg)
#Google.Est.Reg.Intercept <- Google.Est.Reg$coefficients[1]
#Google.Est.Reg.Slope <- Google.Est.Reg$coefficients[2]
Google.T.DVM <- (summary(Google.Est.Reg)$coefficients[,1]/summary(Google.Est.Reg)$coefficients[,2])[-(1:2)]


Microsoft.Est.Reg <- lm(lnMicrosoft.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Microsoft.Est.Reg)
#Microsoft.Est.Reg.Intercept <- Microsoft.Est.Reg$coefficients[1]
#Microsoft.Est.Reg.Slope <- Microsoft.Est.Reg$coefficients[2]
Microsoft.T.DVM <- (summary(Microsoft.Est.Reg)$coefficients[,1]/summary(Microsoft.Est.Reg)$coefficients[,2])[-(1:2)]


Apple.Est.Reg <- lm(lnApple.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Apple.Est.Reg)
#Apple.Est.Reg.Intercept <- Apple.Est.Reg$coefficients[1]
#Apple.Est.Reg.Slope <- Apple.Est.Reg$coefficients[2]
Apple.T.DVM <- (summary(Apple.Est.Reg)$coefficients[,1]/summary(Apple.Est.Reg)$coefficients[,2])[-(1:2)]


Facebook.Est.Reg <- lm(lnFacebook.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Facebook.Est.Reg)
#Facebook.Est.Reg.Intercept <- Facebook.Est.Reg$coefficients[1]
#Facebook.Est.Reg.Slope <- Facebook.Est.Reg$coefficients[2]
Facebook.T.DVM <- (summary(Facebook.Est.Reg)$coefficients[,1]/summary(Facebook.Est.Reg)$coefficients[,2])[-(1:2)]


Yahoo.Est.Reg <- lm(lnYahoo.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(Yahoo.Est.Reg)
#Yahoo.Est.Reg.Intercept <- Yahoo.Est.Reg$coefficients[1]
#Yahoo.Est.Reg.Slope <- Yahoo.Est.Reg$coefficients[2]
Yahoo.T.DVM <- (summary(Yahoo.Est.Reg)$coefficients[,1]/summary(Yahoo.Est.Reg)$coefficients[,2])[-(1:2)]


AOL.Est.Reg <- lm(lnAOL.Diff ~ lnSP500.Diff + LM1 + LD + LP1 +LP2 + LP3 + PM1 + PP1 + PP2 + PP3 + CM1 + CP1 + CP2 + CP3, data = Data)
#summary(AOL.Est.Reg)
#AOL.Est.Reg.Intercept <- AOL.Est.Reg$coefficients[1]
#AOL.Est.Reg.Slope <- AOL.Est.Reg$coefficients[2]
AOL.T.DVM <- (summary(AOL.Est.Reg)$coefficients[,1]/summary(AOL.Est.Reg)$coefficients[,2])[-(1:2)]

## clear regression results
rm(Comcast.Est.Reg, Charter.Est.Reg, Att.Est.Reg, Verizon.Est.Reg, Century.Est.Reg, Google.Est.Reg, 
   Microsoft.Est.Reg, Apple.Est.Reg, Facebook.Est.Reg, Yahoo.Est.Reg, AOL.Est.Reg)


###  Make single table
DVM.Provider.TStats <- data.frame(Comcast = Comcast.T.DVM,
                         Charter = Charter.T.DVM,
                         Century = Century.T.DVM,
                         ATT = ATT.T.DVM,
                         Verizon = Verizon.T.DVM)
#names(DVM.Provider.TStats) <- c("Day", "Comcast", "Charter", "Century", "ATT", "Verizon")

DVM.Site.TStats <- data.frame(Google = Google.T.DVM,
                         Microsoft = Microsoft.T.DVM,
                         Apple = Apple.T.DVM,
                         Facebook = Facebook.T.DVM,
                         Yahoo = Yahoo.T.DVM,
                         AOL = AOL.T.DVM)

# names(DVM.Site.TStats) <- c("Day", "Google", "Microsoft", "Apple", "Facebook", "Yahoo", "AOL")