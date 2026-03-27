setwd("~/Dropbox/research/Denmark Housing Populism/replicationfiles")

##########
#
# Replication R command file for:
# Sheltering Populists? House Prices and the Support for Populist Parties,
# Journal of Politics
#
# Software: R version 4.0.2
#
# Authors: Ben Ansell, Frederik Hjorth, Jacob Nyrup & Martin Vinæs Larsen
#
# Date: 2021/06/15
#
##########

# Detach packages

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

# Clear the R environment

rm(list = ls())

# Load packages

pacman::p_load(tidyverse,openxlsx,extrafont,janitor,ggrepel,lubridate,scales,sensemakr,haven,zoo)

###
# Figure 1 ---
###

## Plot 1 (Upper left) ---

df_plot1 <- read.xlsx("data_figure1_plot1.xlsx") %>%
  mutate(quarter = excel_numeric_to_date(quarter)) %>% 
  gather(key=country,value=value,Denmark,Finland,Norway,Sweden) %>% 
  dplyr::rename(unit = country)

df_plot1$names <- NA
df_plot1$names[192] <- df_plot1$unit[192] 
df_plot1$names[390] <- df_plot1$unit[390]
df_plot1$names[585] <- df_plot1$unit[585]
df_plot1$names[783] <- df_plot1$unit[783]

plot1 <- ggplot(df_plot1,(aes(x=quarter,y=value,group=unit))) + 
         geom_line(aes(linetype=unit)) + 
         theme_bw() +
         theme(legend.position="none",
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               strip.text.y = element_blank()) +
         scale_x_date(name="",
                      limits=as.Date(c("1995-01-01", "2018-12-31")),
                      breaks=as.Date(c("1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01")),
                      date_labels = "%Y") +
         ylim(c(100,520)) + 
         ggrepel::geom_text_repel(aes(label = names),size=3,segment.color = "transparent") + 
         ylab("House price index (1995 = 100)")

## Plot 2 (Upper right) ---

df_plot2 <- read.xlsx("data_figure1_plot2.xlsx") %>% 
  mutate(quarter = as.Date(quarter,format ="%d.%m.%Y")) %>% 
  dplyr::select(quarter,unit=Party,value = Voteshare)

plot2 <- ggplot(df_plot2,(aes(x=quarter,y=value,group=unit))) + 
         geom_line(aes(linetype=unit)) + 
         theme_bw() +
         theme(legend.position="none",
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               strip.background = element_blank(),
               strip.text.y = element_blank()) +
         scale_x_date(name="",limits=as.Date(c("1995-01-01", "2018-12-31")),
                      breaks=as.Date(c("1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01")),date_labels = "%Y") +
         ylab("Vote share in national elections") +
         scale_y_continuous(limits = c(0,25),labels = function(x) paste0(x, "%")) + 
         annotate("text", x = as.Date(as.yearqtr("1998 Q2")), y = 13, label = "Danish People's Party (DK)",size=3) +
         annotate("text", x = as.Date(as.yearqtr("1998 Q1")), y = 18, label = "Progress Party (NO)",size=3) +
         annotate("text", x = as.Date(as.yearqtr("1999 Q1")), y = 3, label = "Sweden Democrats (SE)",size=3) +
         annotate("text", x = as.Date(as.yearqtr("2005 Q3")), y = 8, label = "True Finns (FI)",size=3)
        
## Plot 3 (Lower left) ---

df_plot3_sales <- read.xlsx("data_figure1_plot3_1.xlsx") %>%
  dplyr::rename("n_parcel"="Parcel-/rakkehus","n_flat"="Ejerlejlighed") %>%
  mutate(n_flat = as.numeric(na_if(n_flat, "..")),
         n_parcel = as.numeric(na_if(n_parcel, "..")))
  
sales_denmark_total <- df_plot3_sales %>% group_by(Municipality) %>%
                       summarize(sum_flat = sum(n_flat,na.rm=TRUE),
                       sum_parcel = sum(n_parcel,na.rm=TRUE)) %>%
                       mutate(share_flat = sum_flat/(sum_flat+sum_parcel),
                       share_parcel = sum_parcel/(sum_flat+sum_parcel)) # Create a weight for < 2004

df_plot3_prices <- read.xlsx("data_figure1_plot3_2.xlsx") %>%
  dplyr::rename("price_parcel"="Parcel-/rakkehus","price_flat"="Ejerlejlighed") %>%
  mutate(price_parcel = as.numeric(na_if(price_parcel, "..")),
         price_flat = as.numeric(na_if(price_flat, ".."))) # Load houseprices for Denmark

df_plot3_both <- left_join(df_plot3_prices,df_plot3_sales,by=c("Time","Municipality")) %>% 
  left_join(.,sales_denmark_total[,c(1,4:5)],by=c("Municipality")) %>%
  mutate(price = ((price_parcel*n_parcel)+(price_flat*n_flat))/(n_parcel+n_flat),
         price_estimated = price_parcel * share_parcel + price_flat * share_flat,
         year = substr(Time,0,4),
         Time = as.Date(as.yearqtr(sub("K"," Q",Time)))) # Combine the two data sets and calculate an estimated price

df_plot3_both$price_combined <- ifelse(as.numeric(df_plot3_both$year) < 2004,
                                       df_plot3_both$price_estimated,df_plot3_both$price)

df_plot3_both$price_combined <- ifelse(is.na(df_plot3_both$price_flat),df_plot3_both$price_parcel,
                                      ifelse(is.na(df_plot3_both$price_parcel), df_plot3_both$price_flat,
                                             df_plot3_both$price_combined))

df_plot3 <- df_plot3_both %>% group_by(Time) %>% 
  summarise(price = list(enframe(quantile(price_combined, probs=c(0.1,0.5,0.9),na.rm=TRUE)))) %>% 
  unnest %>%
  dplyr::rename(percentile=name) %>%
  mutate(percentile = recode(percentile,`90%`="90th percentile",`50%`="50th percentile",`10%`="10th percentile"))

plot3 <- ggplot(df_plot3,(aes(x=Time,y=value,group=percentile))) + geom_line(aes(linetype=percentile)) + 
         theme_bw() +
         scale_x_date(name="",limits=as.Date(c("1995-01-01", "2019-12-31")),breaks=as.Date(c("1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01")),date_labels = "%Y") +
         scale_y_continuous(name="Price Per Square Meter (DKK)",breaks=c(0,5000,10000,15000,20000,25000,30000),
                     limits=c(0,30000),labels=comma) +
         theme(legend.title=element_blank(),
               legend.position="",
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),strip.background = element_blank(),
               strip.text.y = element_blank(),
               plot.title = element_text(size=10)) +
         annotate("text", x = as.Date(as.yearqtr("2013 Q1")), y = 2800, label = "10th percentile",size=3) +
         annotate("text", x = as.Date(as.yearqtr("2015 Q1")), y = 13000, label = "50th percentile",size=3) +
         annotate("text", x = as.Date(as.yearqtr("2014 Q1")), y = 24000, label = "90th percentile",size=3) +
         ggtitle("Denmark (zip code)")

## Plot 4 (Lower right) ---

df_plot4 <-  read.xlsx("data_figure1_plot4.xlsx") %>% filter(year == 2019) %>%
  dplyr::rename(wealth=4,families = 5, adults = 6, children = 7, income_gross = 8, income_netto = 9) %>% 
  mutate(wealth_perperson = wealth/(adults+children),
         income_perperson_gross = income_gross/(adults+children),
         income_perperson_netto = income_netto/(adults+children)) %>%
  dplyr::select(ownership,wealth,income_gross,income_netto,wealth_perperson,income_perperson_gross,income_perperson_netto) %>%
  pivot_longer(-ownership) %>% 
  mutate(ownership = recode(ownership,"Boligejer" = "Owners","Lejer (incl. andelsbolig)"="Renters"),
         name = recode(name,"wealth"="Wealth (household)","wealth_perperson"="Wealth (per person)",
                       "income_gross"="Gross income (household)","income_netto"="Disposable income (household)","income_perperson_gross" = "Gross income (per person)",
                       "income_perperson_netto" = "Disposable income (per person)"),
         name = fct_relevel(name,
                            "Disposable income (per person)","Gross income (per person)","Wealth (per person)",
                            "Disposable income (household)","Gross income (household)",
                            "Wealth (household)"),
         ownership = fct_relevel(ownership,"Renters","Owners"))


df_plot4_2 <- df_plot4 %>% filter(name %in% c("Wealth (household)","Gross income (household)","Disposable income (household)")) %>%
  mutate(name = recode(name,"Wealth (household)" = "Wealth", "Gross income (household)" = "Gross income","Disposable income (household)" = "Disposable income"))

plot4 <- ggplot(df_plot4_2,aes(x=ownership,y=value,fill=name)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  labs(x="",y="") +
  scale_y_continuous(limits=c(0,7500000),labels = function(x) paste0(x/10000)) +
  theme(legend.title=element_blank(),
        legend.position="",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text=element_text(size=10)) +
  scale_fill_brewer(palette="Greys") +
  coord_flip() +
  geom_text(aes(label=paste0(name," (",comma(round(value,0))," DKK)")), position=position_dodge(width=1), hjust =-0.05,size=3)

## Print plot ---

ggplot2::ggsave(
  'figure1.pdf',
  gridExtra::grid.arrange(plot1,plot2,plot3,plot4),
  width = 12,
  height = 5
)

###
# Appendix E ---
###

### Figure E1 ---

## Plot 1 - Share owner

df_plote1_1 <- read.xlsx("data_figuree1_plot1.xlsx") %>% 
             mutate(share_owner = total_owner/(total_owner+total_renter),
                    region = recode(region,"North Denmark Region"="North Denmark\nRegion",
                    "Region of Southern Denmark"="Region of\nSouthern Denmark",
                    "Central Denmark Region"="Central Denmark\nRegion"),
                    region = fct_relevel(region,
                    "Total","Capital Region","Region Zealand","Central Denmark\nRegion",
                    "North Denmark\nRegion","Region of\nSouthern Denmark",
                    ))

plote1_1 <- ggplot(df_plote1_1,aes(x=region,y=share_owner)) +
  geom_col() +
  theme_bw() +
  labs(x="",y="Share of population who live in an owned house") +
  scale_y_continuous(limits = c(0,1),labels = function(x) paste0(x*100, "%")) +
  theme(legend.title=element_blank(),
        legend.position="",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.text=element_text(size=12)) +
  geom_text(aes(label=paste(round(share_owner,2)*100,"%")), position=position_dodge(width=0.9), vjust=-.80)

## Plot 2 - Age distribution

df_plote1_1 <- read.xlsx("data_figuree1_plot2.xlsx") %>% filter(year == 2019) %>% 
  mutate(share_owner = owner/(owner+renter),share_renter=renter/(owner+renter),
         age = gsub(" aar","",age),
         age = gsub(" og derover","\\+",age)) %>%
         dplyr::select(age,share_owner,share_renter) %>% 
         pivot_longer(-age) %>% mutate(name = fct_relevel(name,
                                       "share_renter","share_owner"),
                                       age = fct_relevel(age,"0-5","6-11","12-17","18-24","25-29",
                                       "30-39","40-49","50-59","60-69","70-79","80-89","90+"))

plote1_2 <- ggplot(df_plote1_1,aes(x=age,y=value, group = name,fill = name)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  labs(x="Age group",y="") +
  scale_y_continuous(limits = c(0,1),labels = function(x) paste0(x*100, "%")) +
  theme(legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.text=element_text(size=12),
        legend.text=element_text(size=12)) +
  scale_fill_brewer(palette="Greys",labels = c("Share who live in a rented house","Share who live in an owned house"))

## Print Figure E1 ---

ggsave(
  'figuree1.pdf',
  gridExtra::grid.arrange(plote1_1,plote1_2),
  width = 12,
  height = 10
)

### Figure E2 ---

## Plot 1 - Income and wealth

df_plote2_2 <- df_plot4

plote2_1 <- ggplot(df_plote2_2,aes(x=ownership,y=value,fill=name)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  labs(x="",y="DKK 10,000") +
  scale_y_continuous(limits=c(0,7500000),labels = function(x) paste0(x/10000)) +
  theme(legend.title=element_blank(),
        legend.position="",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.text=element_text(size=12)) +
  scale_fill_brewer(palette="Greys") +
  coord_flip() +
  geom_text(aes(label=paste0(name," (",comma(round(value,0))," DKK)")), position=position_dodge(width=1), hjust =-0.05,size=4)

## Plot 2 - Income groups

df_plote2_1 <- read.xlsx("data_figuree2_plot2.xlsx") %>% 
  mutate(share_under200 = under200/total,
         share_200_299 = `200-299`/total,
         share_300_399 = `300-399`/total,
         share_400_499 = `400-499`/total,
         share_500_599 = `500-599`/total,
         share_over600 = `over600`/total) %>% 
  dplyr::select(1,2,10:15) %>%
  pivot_longer(-c(householdtype,ownership)) %>%
  mutate(ownership = recode(ownership,"Beboet af ejer" = "Owners","Beboet af lejer"="Renters"),
         householdtype = recode(householdtype,"Familier i alt"="Total",
                                "Enlige i alt"="Single person",
                                "Par i alt" ="Couples"),
         householdtype = fct_relevel(householdtype,"Total","Couples","Single person"),
         name = fct_relevel(name,"share_over600","share_500_599","share_400_499","share_300_399","share_200_299","share_under200")
  ) %>% dplyr::rename("Income group"="name")

plote2_2 <- ggplot(df_plote2_1,aes(x=ownership,y=value, group = `Income group`,fill = `Income group`)) +
  geom_bar(position="stack", stat="identity") +
  theme_bw() +
  facet_grid(~householdtype) +
  labs(x="",y="") +
  scale_y_continuous(limits = c(0,1),labels = function(x) paste0(x*100, "%")) +
  theme(legend.title = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        axis.text=element_text(size=12),
        legend.text=element_text(size=12),
        strip.text = element_text(size=12)) +
  scale_fill_brewer(palette="Greys",labels = c("More than 600,000 DKK","500,000-599,999 DKK","400,000-499,999 DKK",
                                               "300,000-399,999 DKK","200,000-299,999 DKK","Less than 200,000 DKK"))

## Print Figure E2 ---

ggsave(
  'figuree2.pdf',
  gridExtra::grid.arrange(plote2_1,plote2_2),
  width = 12,
  height = 10
)

### Figure E3 ---

## Denmark ---

denmark_percentile <- df_plot3_both %>% group_by(Time) %>% # Using data from Figure 1
  summarise(price = list(enframe(quantile(price_combined, probs=c(0.1,0.5,0.9,0.99),na.rm=TRUE)))) %>% 
  unnest %>%
  plyr::rename(c("name"="percentile")) %>%
  mutate(percentile = dplyr::recode(percentile,`99%`="99th percentile",`90%`="90th percentile",`50%`="50th percentile",`10%`="10th percentile"))

plot_denmark <- ggplot(denmark_percentile,(aes(x=Time,y=value,group=percentile))) + geom_line(aes(linetype=percentile)) + theme_bw() +
  scale_x_date(name="",limits=as.Date(c("1995-01-01", "2018-12-31")),breaks=as.Date(c("1995-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01")),date_labels = "%Y") +
  scale_y_continuous(name="Price Per Square Meter (DKK)",breaks=c(0,10000,20000,30000,40000,50000),
                     limits=c(0,45000),labels=comma) +
  theme(legend.title=element_blank(),
        legend.position="",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        plot.title = element_text(size=10)) +
  annotate("text", x = as.Date(as.yearqtr("2013 Q1")), y = 2800, label = "10th percentile",size=3) +
  annotate("text", x = as.Date(as.yearqtr("2015 Q1")), y = 13000, label = "50th percentile",size=3) +
  annotate("text", x = as.Date(as.yearqtr("2014 Q1")), y = 24000, label = "90th percentile",size=3) +
  annotate("text", x = as.Date(as.yearqtr("2014 Q1")), y = 40000, label = "99th percentile",size=3) +
  ggtitle("Denmark (zip code)")

## Sweden ---

df_sverige <- read.xlsx("data_figuree3_sweden.xlsx") %>% mutate(price = as.numeric(na_if(price,"..")))

sweden_percentile <- df_sverige %>% group_by(year) %>% 
  summarise(price = list(enframe(quantile(price, probs=c(0.1,0.5,0.9,0.99),na.rm=TRUE)))) %>% 
  unnest %>%
  plyr::rename(c("name"="percentile")) %>%
  mutate(percentile = dplyr::recode(percentile,`99%`="99th percentile",`90%`="90th percentile",`50%`="50th percentile",`10%`="10th percentile"),value = value*1000,
         date = as.Date(ISOdate(year, 1, 1)))

plot_sweden <- ggplot(sweden_percentile,(aes(x=date,y=value,group=percentile))) + geom_line(aes(linetype=percentile)) + theme_bw() +
  scale_x_date(name="",limits=as.Date(c("1996-01-01", "2020-01-01")),breaks=as.Date(c("1996-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01")),date_labels = "%Y") +
  scale_y_continuous(name="Average price for a dwelling (SEK)",breaks=c(0,2000000,4000000,6000000,8000000,10000000),
                     limits=c(0,10000000),labels=comma) +
  theme(legend.title=element_blank(),
        legend.position="",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        plot.title = element_text(size=10)) +
  ggtitle("Sweden (municipality)")

## Norway ---

housing_no <- read.xlsx("data_figuree3_norway.xlsx")

# Remove values without price

for (i in 1:nrow(housing_no)){
  if(is.na(housing_no$Price_Detachedhouses[i])) {housing_no$N_Detachedhouses[i] <- NA}
  if(is.na(housing_no$Price_Rowhouses[i])) {housing_no$N_Rowhouses[i] <- NA}
  if(is.na(housing_no$Price_Multidwelling[i])) {housing_no$N_Multidwelling[i] <- NA}
}

housing_no$Detachedhousestemp <- housing_no$Price_Detachedhouses*housing_no$N_Detachedhouses
housing_no$Rowhousestemp <- housing_no$Price_Rowhouses*housing_no$N_Rowhouses
housing_no$Multidwellingtemp <- housing_no$Price_Multidwelling*housing_no$N_Multidwelling
housing_no$Price <- rowSums(housing_no[,c(9:11)],na.rm=TRUE)/rowSums(housing_no[,c(6:8)],na.rm=TRUE)
housing_no <- housing_no[,c(1,2,12)]

df_norway <- housing_no %>%
  dplyr::select(year=Year,Municipality,price=Price)

norway_percentile <- df_norway %>% group_by(year) %>% 
  summarise(price = list(enframe(quantile(price, probs=c(0.1,0.5,0.9,0.99),na.rm=TRUE)))) %>% 
  unnest %>%
  plyr::rename(c("name"="percentile")) %>%
  mutate(percentile = dplyr::recode(percentile,`99%`="99th percentile",`90%`="90th percentile",`50%`="50th percentile",`10%`="10th percentile"),
         date = as.Date(ISOdate(year, 1, 1)))

plot_norway <- ggplot(norway_percentile,(aes(x=date,y=value,group=percentile))) + geom_line(aes(linetype=percentile)) + theme_bw() +
  scale_x_date(name="",limits=as.Date(c("2002-01-01", "2018-01-01")),breaks=as.Date(c("1996-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01")),date_labels = "%Y") +
  scale_y_continuous(name="Price Per Square Meter (NOK))",breaks=c(0,10000,20000,30000,40000,50000),
                     limits=c(0,50000),labels=comma) +
  theme(legend.title=element_blank(),
        legend.position="",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        plot.title = element_text(size=10)) +
  ggtitle("Norway (municipality)")

## Finland ---

df_finland <- read.xlsx("data_figuree3_finland.xlsx") %>%
  mutate(price = Price) %>%
  dplyr::select(year=Year,Municipality,price)

finland_percentile <- df_finland %>% group_by(year) %>% 
  summarise(price = list(enframe(quantile(price, probs=c(0.1,0.5,0.9,0.99),na.rm=TRUE)))) %>% 
  unnest %>%
  plyr::rename(c("name"="percentile")) %>%
  mutate(percentile = dplyr::recode(percentile,`99%`="99th percentile",`90%`="90th percentile",`50%`="50th percentile",`10%`="10th percentile"),
         date = as.Date(ISOdate(year, 1, 1)))

plot_finland <- ggplot(finland_percentile,(aes(x=date,y=value,group=percentile))) + geom_line(aes(linetype=percentile)) + theme_bw() +
  scale_x_date(name="",limits=as.Date(c("2000-01-01", "2018-01-01")),breaks=as.Date(c("1996-01-01","2000-01-01","2005-01-01","2010-01-01","2015-01-01","2020-01-01")),date_labels = "%Y") +
  scale_y_continuous(name="Price Per Square Meter (EUR))",breaks=c(0,1000,2000,3000),
                     limits=c(0,3500),labels=comma) +
  theme(legend.title=element_blank(),
        legend.position="",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y = element_blank(),
        plot.title = element_text(size=10)) +
  ggtitle("Finland (municipality)")

## Print plot ---

ggsave(
  'figuree3.pdf',
  gridExtra::grid.arrange(plot_denmark,plot_sweden,plot_norway,plot_finland),
  width = 10,
  height = 10
)

###
# Appendix F ---
###

## Load data ---

df_appf <- read_dta("data_dk_ziplvl.dta") %>% dplyr::rename("Median Income"=difmedianinc)

## Run regression ---

reg_appf <- lm(dfdif ~ hpdif + difhigh + diflow + diflow_service + diflow_manuf + difhighlvl + difpopdens + difunemprate + `Median Income` + difsolo + diftenppl + difnwimm + as.factor(year), data=df_appf)

## Run sensemakr ---

model_appf <- sensemakr(reg_appf,treatment = "hpdif",benchmark_covariates = "`Median Income`",kd = c(1,5,10,15,24), ky=c(1,5,10,15,24))

## Print output ---

# Plot

pdf("figuref1.pdf", height=6, width=8)
plot(model_appf,xlab=expression('Hypothetical partial R'^2*'of unobserved confounder(s) with the treatment'),
     ylab=expression('Hypothetical partial R'^2*' of unobserved confounder(s) with the outcome'))
dev.off()

# Regression

writeLines(ovb_minimal_reporting(model_appf,digits=3,verbose=TRUE,
                      format=c("latex")),"tablef1.tex")
