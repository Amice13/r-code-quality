####################################################
# Code for "Chance Encounters" (by Hoyt Long)
# Published in Journal of Cultural Analytics (2021)
# Includes all code for analysis and visualizations
# Associated data provided in the "Data" folder
####################################################

#set working directory to wherever this R file is
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#load libraries
library(ggplot2)
library(scales)
library(openxlsx)
library(plyr)
library(tidyverse)
library(reshape2)
library(directlabels)
library(readr)
library(ggrepel)
library(spatstat)
library(e1071)
library(ggrepel)
library(packcircles)
#library(dplyr)

##############################################
# Analyze Bibliographic Data for Translations 
##############################################

#read in translation metadata
trans_meta <- openxlsx::read.xlsx("Data/MasterListFinal.xlsx", sheet = 1)

#filter out zenshu volumes
trans_meta <- trans_meta %>% filter(is.na(ENTRY_TYPE))

#filter out entries with no publication dates
trans_meta <- trans_meta %>% filter(!is.na(PUBL_START))

#create subset with just Taisho/Showa data
TS_meta <- trans_meta %>% filter(INDEX == "NDL_TS")

#####################################################
# Figure 1 (translations as % of total publications)
#####################################################

#Import yearly publication data obtained from Shuppan Nenkan
shuppan <- openxlsx::read.xlsx("Data/Shuppan_Nenkan_Data.xlsx", sheet = 1)

#Add book amounts to newspaper/magazine amounts for Meiji period
shuppan$TOTALS <- shuppan$TOTAL_BOOKS + shuppan$NEWS_MAGS

#Get yearly counts for translation metadata, divided by period
year_counts <- trans_meta %>%
  group_by(PUBL_START, INDEX) %>%
  summarize(N = length(TITLE))

year_counts <- as.data.frame(year_counts)

#cutoff first 14 rows of shuppan, since we don't have data for that period
shuppan <- shuppan[14:88,]
#make yearly count data parallel to shuppan data
year_counts <- year_counts[12:86,]

#merge total publications with translation counts
year_counts$TOTALS <- shuppan$TOTALS
#calculate ratio
year_counts$ratio <- year_counts$N / year_counts$TOTALS

#replace missing years with NA
year_counts[year_counts==Inf]<-NA

#produce moving average of the ratio
ratio_movavg <- year_counts %>%
  mutate(lag1 = lag(ratio),
         lag2 = lag(ratio,2),
         movavg = ((lag1+lag2)/2) * 100)

#a transformation function
scaleFUN <- function(x) sprintf("%.1f", x)

#plot the results up to 1943
p <- ggplot(data = ratio_movavg[ratio_movavg$PUBL_START < 1943,], mapping = aes(x = PUBL_START, y = movavg, color=INDEX))
p + geom_line(mapping = aes(group = INDEX), size=1) +
  scale_x_continuous(breaks = c(1880,1890,1900,1910,1920,1930,1940)) +
  scale_y_continuous(labels=scaleFUN) +
  labs(x="", y="Percent of Total Publications", color = "", title="") +
  scale_color_manual(labels = c("MEIJI", "NDL_TS"), values = c("red", "blue")) +
  theme(legend.position = "top", axis.text=element_text(size=12), title=element_text(size=12))

##############################################################
# Figure 2 (distance in years from weighted median birth year)
##############################################################

#filter for titles where author birth is known and author has 3 or more titles
meta_birth <- TS_meta %>% filter(!is.na(AUTH_BIRTH))

#calculate the weighted median birth year for each year
w_median_birth <- meta_birth %>%
  group_by(PUBL_START, AUTH_BIRTH) %>%
  summarize(N = n())

w_median_birth <- w_median_birth %>%
  group_by(PUBL_START) %>%
  summarize(Z = weighted.median(x = AUTH_BIRTH, w = N))

#produce moving average
w_median_birth <- w_median_birth %>%
  mutate(lag1 = lag(Z),
         #lag2 = lag(Z,2),
         movavg = (Z+lag1)/2)

#calculate difference between year and median_birth for that year to get "pastness" measure
w_median_birth$dist <- w_median_birth$movavg - w_median_birth$PUBL_START 

p <- ggplot(data = w_median_birth, mapping = aes(x = PUBL_START, y = dist))
p + geom_line(size=1) +
  scale_x_continuous(breaks = c(1910,1920,1930,1940,1950)) +
  labs(x="", y="Years into the Past", color = "", title="") +
  theme(legend.position = "bottom", axis.text=element_text(size=12), title=element_text(size=12))

################################################################################
# Figure 3 (packed circle chart)
# https://www.r-graph-gallery.com/306-custom-circle-packing-with-one-level.html
################################################################################

TS_subset <- TS_meta %>% filter(PUBL_START <= 1927)
total_entries <- dim(TS_subset)[1]

#calculate proportional representation for each unique author
auth_ratio <- TS_subset %>%
  group_by(AUTHOR_NUM) %>%
  summarize(ratio = length(AUTHOR_NUM)/total_entries)

#filter out authors with only single work
auth_ratio <- auth_ratio %>% filter(ratio > 1/total_entries)

#add author metadata (last name, whether included in anthology or not)
auth_meta <- openxlsx::read.xlsx("Data/Foreign_Author_Metadata.xlsx", sheet = 1)
auth_ratio <- merge(auth_meta[, c("AUTHOR_NUM","AUTHOR_LAST", "SHINCHOSHA")], auth_ratio, by="AUTHOR_NUM")
auth_ratio[auth_ratio=="NOT_INCLUDED"]<-NA

#rank authors and keep author names for top N
auth_ratio <- auth_ratio[with(auth_ratio, order(-ratio)), ]
auth_ratio <- auth_ratio %>% mutate(LABEL = ifelse(is.na(auth_ratio$SHINCHOSHA), NA, auth_ratio$AUTHOR_LAST))
auth_ratio[1:50,"LABEL"] <- auth_ratio[1:50,"AUTHOR_LAST"]

#prepare data for packed circle plot
packing <- circleProgressiveLayout(auth_ratio$ratio, sizetype = 'area')
packing$radius <- 0.95*packing$radius
auth_ratio <- cbind(auth_ratio, packing)

#test for non-linear relationship
#plot(auth_ratio$radius, auth_ratio$ratio)

dat.gg <- circleLayoutVertices(packing, npoints=50) #this value sets no. of lines per datapoint
dat.gg$value <- rep(auth_ratio$SHINCHOSHA, each=51) #repeat values 51 times to correspond with npoints setting

ggplot() +
  #make bubbles
  geom_polygon(data = dat.gg, aes(x, y, group=id, fill=value), color = "grey", alpha = 0.6) +
  scale_fill_manual(values = c("light blue","pink","white")) +  #reset these to light green, light yellow, white?
  geom_text(data = auth_ratio, aes(x, y, size=ratio, label=LABEL)) +
  scale_size_continuous(range = c(2,8)) +
  theme_void() +
  theme(legend.position="none") +
  coord_equal()

##########################
# Figures 4-8 (chow tests)
##########################

#filter out everything between 1943 and 1945
new_meta <- TS_meta %>% filter(PUBL_START <= 1942 | PUBL_START >= 1946)

#input list of authors included in Shinchosha series and corresponding ID
auth_meta <- 
  openxlsx::read.xlsx("Data/Foreign_Author_Metadata.xlsx", sheet = 1)

#make sure auth_meta is sorted by NO_TITLES
attach(auth_meta)
auth_meta <- auth_meta[order(-NO_TITLES),]
detach(auth_meta)

#select the part of Shinchosha to analyze
parts <- c("PART_ONE","PART_TWO","NOT_INCLUDED")
part <- parts[1]
#select threshold year to analyze (1929 for PART_ONE and NOT category; 1932 for PART_TWO)
breakpoints <- c(1929, 1932)
thresh <- breakpoints[1]

#set number of authors to analyze (16 authors for PART_ONE and NOT; only do 9 authors for PART_TWO)
num_authors <- 16

#get author metadata
auth_meta <- auth_meta %>% filter(SHINCHOSHA == part)
auth_meta <- auth_meta[1:num_authors,]
auth_meta$PVALUE <- NA

#do a chow test for each author 
for (auth_id in auth_meta$AUTHOR_NUM){
  
  auth_ratio <- new_meta %>%
    group_by(PUBL_START) %>%
    summarize(RATIO = length(TITLE[AUTHOR_NUM==auth_id]) / length(TITLE)) %>%
    mutate(lag1 = lag(RATIO),
           lag2 = lag(RATIO,2),
           movavg = (lag1+lag2)/2) #calculate 2 year moving avg
  
  #run three regressions on all data and each half of data
  auth_ratio <- auth_ratio[3:41,]  #need to cut first 2 rows due to NA values
  attach(auth_ratio)
  r.reg = lm(movavg ~ PUBL_START)
  ur.reg1 = lm(movavg ~ PUBL_START, data = auth_ratio[auth_ratio$PUBL_START <= thresh,])
  ur.reg2 = lm(movavg ~ PUBL_START, data = auth_ratio[auth_ratio$PUBL_START > thresh,])
  detach(auth_ratio)
  
  ## Calculate sum of squared residuals for each regression
  SSR = NULL
  SSR$r = r.reg$residuals^2
  SSR$ur1 = ur.reg1$residuals^2
  SSR$ur2 = ur.reg2$residuals^2
  
  ## K is the number of regressors in our model
  K = r.reg$rank
  
  ## Computing the Chow test statistic (F-test)
  numerator = ( sum(SSR$r) - (sum(SSR$ur1) + sum(SSR$ur2)) ) / K
  denominator = (sum(SSR$ur1) + sum(SSR$ur2)) / (nrow(auth_ratio) - 2*K)
  chow = numerator / denominator
  
  ## Calculate P-value
  p_val <- 1 - pf(chow, K, (nrow(auth_ratio) - 2*K))
  
  auth_meta[auth_meta$AUTHOR_NUM==auth_id,"PVALUE"] <- p_val
}

#round off p-values
auth_meta$PVALUE <- round(auth_meta$PVALUE, digits = 3)

#create list to store all plots
plots <- vector("list", num_authors)
index <- 1

#GRAPH the regression results for each author
for (auth_id in auth_meta$AUTHOR_NUM){
  
  auth_ratio <- new_meta %>%
    group_by(PUBL_START) %>%
    summarize(RATIO = length(TITLE[AUTHOR_NUM==auth_id]) / length(TITLE)) %>%
    mutate(lag1 = lag(RATIO),
           lag2 = lag(RATIO,2),
           movavg = (lag1+lag2)/2)
  
  #set boolean for before cut_point and after
  auth_ratio$cut_point <- auth_ratio$PUBL_START <= thresh
  
  plot_title <- paste(auth_meta[auth_meta$AUTHOR_NUM==auth_id, "AUTHOR_LAST"], ", p-value = ",
                      auth_meta[auth_meta$AUTHOR_NUM==auth_id, "PVALUE"], sep="")
  
  #" (", auth_meta[auth_meta$AUTHOR_NUM==auth_id, "NO_TITLES"], " works), p-value = ",
  #auth_meta[auth_meta$AUTHOR_NUM==auth_id, "PVALUE"], sep="")
  
  p <- ggplot(data = auth_ratio, mapping = aes(x = PUBL_START, y = movavg, color = cut_point)) +
    geom_point(size=2) +
    scale_x_continuous(breaks = c(1870,1880,1890,1900,1910,1920,1930,1940,1950,1960)) +
    labs(x="", y="", title=plot_title) +
    theme(legend.position = "", axis.text=element_text(size=12), title=element_text(size=12,face="bold")) +
    geom_smooth(method = "lm")
  
  plots[[index]] <- p
  index <- index + 1
}

#print all the plots as multiples
library(cowplot)
cowplot::plot_grid(plotlist = plots, nrow = 4, ncol = 4)
#cowplot::plot_grid(plotlist = plots, nrow = 3, ncol = 3)

#set directory where you want to output figures
#setwd("Data/")
jpeg(filename="Figure5.jpg", 
    type="Xlib",
    units="in", 
    width=12,    
    height=12,
    quality=75,
    pointsize=10, 
    res=200)
cowplot::plot_grid(plotlist = plots, nrow = 4, ncol = 4)
#cowplot::plot_grid(plotlist = plots, nrow = 3, ncol = 3)
dev.off()

#12, 12, res=200, ps=10

#run Chow Tests on all authors in each Part (figures 6, 7)

#select which Part to analyze
part <- parts[1]

#select threshold year to analyze (1929 for PART_ONE and NOT category; 1932 for PART_TWO)
thresh <- breakpoints[1] 

#reload the author metadata
auth_meta <- 
  openxlsx::read.xlsx("Data/Foreign_Author_Metadata.xlsx", sheet = 1)

#make sure auth_meta is sorted by NO_TITLES
attach(auth_meta)
auth_meta <- auth_meta[order(-NO_TITLES),]
detach(auth_meta)

#subset to get only authors in specific Part of Shinchosha anthology
auth_meta <- auth_meta %>% filter(SHINCHOSHA == part)

#perform a CHOW test for authors in this Part
auth_ratio <- new_meta %>%
  group_by(PUBL_START) %>%
  summarize(RATIO = length(TITLE[AUTHOR_NUM %in% auth_meta$AUTHOR_NUM]) / length(TITLE)) %>%
  mutate(lag1 = lag(RATIO),
         lag2 = lag(RATIO,2),
         movavg = (lag1+lag2)/2) #calculate 2 year moving avg

#run three regressions (all data and each half of data)
auth_ratio <- auth_ratio[3:41,]  #need to cut first 2 rows due to NA values
attach(auth_ratio)
r.reg = lm(movavg ~ PUBL_START)
ur.reg1 = lm(movavg ~ PUBL_START, data = auth_ratio[auth_ratio$PUBL_START <= thresh,])
ur.reg2 = lm(movavg ~ PUBL_START, data = auth_ratio[auth_ratio$PUBL_START > thresh,])
detach(auth_ratio)

## Calculate sum of squared residuals for each regression
SSR = NULL
SSR$r = r.reg$residuals^2
SSR$ur1 = ur.reg1$residuals^2
SSR$ur2 = ur.reg2$residuals^2

## K is the number of regressors in our model
K = r.reg$rank

## Computing the Chow test statistic (F-test)
numerator = ( sum(SSR$r) - (sum(SSR$ur1) + sum(SSR$ur2)) ) / K
denominator = (sum(SSR$ur1) + sum(SSR$ur2)) / (nrow(auth_ratio) - 2*K)
chow = numerator / denominator

## Calculate P-value
p_val <- 1 - pf(chow, K, (nrow(auth_ratio) - 2*K))

#Graph results 
#create a categorical variable based on before or after threshold year
auth_ratio$cut_point <- auth_ratio$PUBL_START <= thresh

#a transformation function
scaleFUN <- function(x) sprintf("%1.1f", 100*x)

p_val <- round(p_val, digits = 3)
plot_title <- paste("Part Two Authors", ", p-value = ", p_val, sep="")

q <- ggplot(data = auth_ratio, mapping = aes(x = PUBL_START, y = movavg, color = cut_point))
q + geom_point(size=2) +
  scale_x_continuous(breaks = c(1910,1920,1930,1940,1950,1960)) +
  labs(x="", y="Percent of Total Translations", title=plot_title) +
  scale_y_continuous(labels=scaleFUN) +
  #scale_color_manual(values = c("black", "darkgrey")) +
  theme(legend.position = "", axis.text=element_text(size=12), title=element_text(size=12)) +
  geom_smooth(method = "lm", se=FALSE)

################################################################################
# Figure 9 (packed circle chart)
# https://www.r-graph-gallery.com/306-custom-circle-packing-with-one-level.html
################################################################################

TS_subset <- TS_meta %>% filter(PUBL_START <= 1918)
total_entries <- dim(TS_subset)[1]

#calculate proportional representation for each unique author
auth_ratio <- TS_subset %>%
  group_by(AUTHOR_NUM) %>%
  summarize(ratio = length(AUTHOR_NUM)/total_entries)

#filter out authors with only single work
auth_ratio <- auth_ratio %>% filter(ratio > 1/total_entries)

#add author metadata (last name, whether included in anthology or not)
auth_meta <- openxlsx::read.xlsx("Data/Foreign_Author_Metadata.xlsx", sheet = 1)
auth_ratio <- merge(auth_meta[, c("AUTHOR_NUM","AUTHOR_LAST", "SENTEI_TOSHO")], auth_ratio, by="AUTHOR_NUM")

#rank authors and keep author names for top N
auth_ratio <- auth_ratio[with(auth_ratio, order(-ratio)), ]
auth_ratio <- auth_ratio %>% mutate(LABEL = ifelse(is.na(auth_ratio$SENTEI_TOSHO), NA, auth_ratio$AUTHOR_LAST))
#label the top 30, regardless of inclusion in library association list
auth_ratio[1:30,"LABEL"] <- auth_ratio[1:30,"AUTHOR_LAST"]

#prepare data for packed circle plot
packing <- circleProgressiveLayout(auth_ratio$ratio, sizetype = 'area')
packing$radius <- 0.95*packing$radius
auth_ratio <- cbind(auth_ratio, packing)

#test for non-linear relationship
#plot(auth_ratio$radius, auth_ratio$ratio)

dat.gg <- circleLayoutVertices(packing, npoints=50) #this value sets no. of lines per datapoint
dat.gg$value <- rep(auth_ratio$SENTEI_TOSHO, each=51) #repeat values 51 times to correspond with npoints setting

ggplot() +
  #make bubbles
  geom_polygon(data = dat.gg, aes(x, y, group=id, fill=value), color = "grey", alpha = 0.6) +
  scale_fill_manual(values = c("light blue","white")) +
  geom_text(data = auth_ratio, aes(x, y, size=ratio, label=LABEL)) +
  scale_size_continuous(range = c(1.5,8)) +
  theme_void() +
  theme(legend.position="none") +
  coord_equal()

#############
# Figure 10
#############

#import count data for VIAF and Japan; grab works marked "top50"; sort by author last name so they are aligned
#this lets us compare the top 50 most translated authors in Japan with all of VIAF
viaf_meta <- openxlsx::read.xlsx("Data/VIAF_AUTH_1912_1955.xlsx", sheet = 1)
total_viaf <- sum(viaf_meta$COUNT) - (.136 * sum(viaf_meta$COUNT)) #the 13% is mean of 3 weighted random samples
total_viaf <- round(total_viaf, digits = 0)
viaf_top50 <- viaf_meta %>% filter(TOP50_JAPAN=="top50")
viaf_top50 <- viaf_top50[ order(viaf_top50$AUTHOR), ]

auth_meta <- openxlsx::read.xlsx("Data/Foreign_Author_Metadata.xlsx", sheet = 1)
total_tds <- sum(auth_meta$UNQ_EXP)
jpn_top50 <- auth_meta %>% filter(TOP50_JAPAN=="top50")
jpn_top50 <- jpn_top50[ order(jpn_top50$AUTHOR_LAST), ]

#will store estimates of proportional difference in jpn_top50 dataframe
jpn_top50$low_bound <- vector("list", 50)  #lower CI of proportional difference
jpn_top50$upp_bound <- vector("list", 50)  #upper CI of proportional difference
jpn_top50$prop_viaf <- vector("list", 50)  #estimated proportion in VIAF data
jpn_top50$prop_tds <- vector("list", 50)   #estimates proportion in Japan data
jpn_top50$prop_diff <- vector("list", 50)  #difference between estimates
jpn_top50$pvalue <- vector("list", 50)     #p-value of 2 sample proportion test

#do statistical test for each author
for (i in 1:50){
  no_trans <- c(viaf_top50$COUNT[i], jpn_top50$UNQ_EXP[i])
  no_total <- c(total_viaf, total_tds)
  
  fit <- prop.test(no_trans, no_total)  #two sample proportion test
  
  #use conf intervals to calculate lower and upper bounds of difference in estimate ratios
  jpn_top50$low_bound[[i]] <- (fit$estimate[1] - fit$conf.int[2]) / fit$estimate[1] 
  jpn_top50$upp_bound[[i]] <- (fit$estimate[1] - fit$conf.int[1]) / fit$estimate[1]
  jpn_top50$prop_viaf[[i]] <- unname(fit$estimate[1])
  jpn_top50$prop_tds[[i]] <- unname(fit$estimate[2])
  jpn_top50$prop_diff[[i]] <- unname(fit$estimate[2]) - unname(fit$estimate[1])
  jpn_top50$pvalue[[i]] <- fit$p.value
}

#now plot the top 30 based on proportional difference
jpn_top50$low_bound <- unlist(jpn_top50$low_bound)
jpn_top50$upp_bound <- unlist(jpn_top50$upp_bound)
jpn_top50$mean_prop_diff <- (jpn_top50$low_bound + jpn_top50$upp_bound) / 2  #find midpoint

#sort by mean_prop_diff proportional difference
jpn_top50 <- jpn_top50[ order(-jpn_top50$mean_prop_diff), ]

p <- ggplot(data = jpn_top50[1:30,], mapping = aes(x = reorder(AUTHOR_LAST, mean_prop_diff), y = mean_prop_diff))
p + geom_pointrange(mapping = aes(ymin = low_bound, ymax = upp_bound)) +
  labs(x= "", y= "Proportional Difference Compared to VIAF Data") + coord_flip() +
  theme(axis.text=element_text(size=10), title=element_text(size=10))

######################################################
# get author rank by % of translations in given period
######################################################

TS_subset <- TS_meta %>% filter(PUBL_START <= 1937)

auth_ratio <- TS_subset %>%
  group_by(AUTHOR_NUM) %>%
  summarize(ratio = length(AUTHOR_NUM)/total_entries)

#add author metadata (last name, whether included in anthology or not)
auth_meta <- openxlsx::read.xlsx("Data/Foreign_Author_Metadata.xlsx", sheet = 1)
auth_ratio <- merge(auth_meta[, c("AUTHOR_NUM","AUTHOR_LAST")], auth_ratio, by="AUTHOR_NUM")

attach(auth_ratio)
auth_ratio <- auth_ratio[order(-ratio),]
detach(auth_ratio)

auth_ratio[100,]

##############################################
# Code to perform Rank Correlation Analysis
##############################################

#load libraries
library(Hmisc)
library(ircor)

#read in author rank data
meta <- openxlsx::read.xlsx("Data/RankData.xlsx", sheet = 1)

# Calculate kendall's tau with treatment for ties
rankings <- c("TITLE_RANK","NDL_RANK")
tau_matrix <- data.frame(matrix(nrow = 2, ncol = 2))
rownames(tau_matrix) <- rankings
colnames(tau_matrix) <- rankings
for (i in 1:length(rankings)){
  for (j in 1:length(rankings)){
    tau_matrix[i, j] = tau_b(meta[, rankings[i]], meta[, rankings[j]])
  }
}

tau_matrix