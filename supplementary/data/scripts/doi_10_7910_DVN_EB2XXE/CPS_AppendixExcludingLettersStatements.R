# Code for CPS article Appendix robustness checks, excluding statement protests: Figures A1, A2, A3, A5

# List of required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "reshape2", "plyr", "janitor", "xtable", "ggpubr",  "scales")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
}

# Load all packages
lapply(required_packages, library, character.only = TRUE)

setwd('~/Dropbox/CPS/CPS_Materials') # set working directory for the project

# start with the cleaned data from Step 1
HUNdata <- read.csv(file="HungarianProtests_cleaned.csv", header=TRUE) 

# For Figures  A1, A2, A3, A5 robustness checks, let's remove categories v02416 (Open letters, statements and appeals to authorities or community)
# and v02422 Cyber actions (e-mail petitions, partisan websites) and v02499 (data unavailable)
HUNdata<- subset(HUNdata, v02401==1 | v02402==1 | v02403==1 | v02404==1 | v02405==1 | v02406==1 | v02407==1 |
   v02408==1 | v02409==1 | v02410==1 | v02411==1 | v02412==1 |  v02413==1|  v02414==1 |  v02415==1 |
   v02417==1 | v02418==1 | v02419==1 | v02420==1 | v02421==1 | v02423==1| v02424==1 | v02425==1 )

# (1) Create 4 datasets by subsetting our cleaned protest data: (1) right, (2) mainright, (3) radright, (4) left
right <- subset(HUNdata, right_all==1)
mainright <- subset(HUNdata, rightist==1)
radright <- subset(HUNdata, radrightist==1)
left <- subset(HUNdata, leftist==1)

# Figure 2 data preparation
# Aggregate radical right protest count by year, then aggregate average number of protesters, average protest duration, average salience by year
protest_selsojobb <- ddply(HUNdata,.(v004year),summarize,count.protest_selsojobb=sum(radrightist))
numprotest_selsojobb <- ddply(HUNdata,.(v004year),summarize, num.radright.protestors=mean(number_protestors[radrightist==1], na.rm=T))
duration_selsojobb <- ddply(HUNdata,.(v004year),summarize, dur.radright.protest=mean(protest_duration[radrightist==1], na.rm=T))
salience_selsojobb <- ddply(HUNdata,.(v004year),summarize, salience.radright.protestors=mean(salience[radrightist==1], na.rm=T))

# Aggregate mainstream right protest count by year, then aggregate average number of protesters, average protest duration, average salience by year
protest_centerjobb <- ddply(HUNdata,.(v004year),summarize,count.protest_jobb=sum(rightist))
numprotest_centerjobb <- ddply(HUNdata,.(v004year),summarize, num.rightist.protestors=mean(number_protestors[rightist==1], na.rm=T))
duration_centerjobb <- ddply(HUNdata,.(v004year),summarize, dur.rightist.protest=mean(protest_duration[rightist==1], na.rm=T))
salience_centerjobb <- ddply(HUNdata,.(v004year),summarize, salience.rightist.protestors=mean(salience[rightist==1], na.rm=T))

# Aggregate left protest count by year, then aggregate average number of protesters, average protest duration, average salience by year
protest_bal <- ddply(HUNdata,.(v004year),summarize,count.protest_bal=sum(leftist))
numprotest_bal <- ddply(HUNdata,.(v004year),summarize, num.leftist.protestors=mean(number_protestors[leftist==1], na.rm=T))
duration_bal <- ddply(HUNdata,.(v004year),summarize, dur.leftist.protest=mean(protest_duration[leftist==1], na.rm=T))
salience_bal <- ddply(HUNdata,.(v004year),summarize, salience.leftist.protestors=mean(salience[leftist==1], na.rm=T))

# Put this all in a dataframe of partisan protest yearly volume and average yearly size
partisanDF_size <- data.frame(protest_bal,protest_centerjobb, protest_selsojobb, numprotest_selsojobb, numprotest_centerjobb, numprotest_bal)

# Keep only the necessary (non-redundant v004year columns) after combining these dataframes 
# and filter out the first few years where we have NAs for partisan protest, starting with the 1994 election year
partisanDF_size <- subset(partisanDF_size, select=c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb", "num.radright.protestors", "num.rightist.protestors", "num.leftist.protestors"))
partisanDF_size <-subset(partisanDF_size, v004year>1993)

#let's reshape these dataframes: one for the counts, one for the number of partisan protestors
partisan_counts <- reshape2::melt(partisanDF_size[,c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb")], id=c("v004year"))
partisan_size <- reshape2::melt(partisanDF_size[,c("v004year", "num.radright.protestors", "num.rightist.protestors", "num.leftist.protestors")], id=c("v004year"))

partisan_counts <- partisan_counts %>%
  mutate(Ideology = recode(variable, count.protest_bal = 'Left', count.protest_jobb = 'Mainstream right', count.protest_selsojobb =  'Radical right' ))
partisan_counts  <- subset(partisan_counts , select=c("v004year", "value", "Ideology"))
partisan_counts <-partisan_counts %>% 
  dplyr::rename("Count" = "value")

partisan_size <- partisan_size %>%
  mutate(Ideology = recode(variable, num.leftist.protestors = 'Left', num.rightist.protestors = 'Mainstream right', num.radright.protestors =  'Radical right' ))
partisan_size  <- subset(partisan_size , select=c("v004year", "value", "Ideology"))
partisan_size <- partisan_size %>% 
  dplyr::rename("Size (participants)"= "value")

df_countssize <- merge(partisan_counts, partisan_size)

BalJobb_size <- ggplot(df_countssize, aes(v004year, y = Count, color = Ideology)) +
  geom_line() +
  geom_point(aes(size = `Size (participants)`)) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 1998, 2002, 2006, 2010)) +
  scale_size(range = c(0, 15)) +
  scale_color_grey(
    name = "Ideology",
    labels = c("Left", "Mainstream right", "Radical right"),
    start = 0.9, end = 0.2
  ) +
  labs(
    title = "Partisan Protests in Hungary, 1994-2011",
    subtitle = "Bubble Size Represents Average Annual Partisan Protest Size",
    x = "Year", 
    y = "Count of Protests",
    color = "Ideology"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_classic(base_size = 12)

BalJobb_size

# Put this all in a dataframe of partisan protest yearly volume and average yearly duration
partisanDF_duration <- data.frame(protest_bal,protest_centerjobb, protest_selsojobb, duration_selsojobb, duration_centerjobb, duration_bal)
partisanDF_duration <- subset(partisanDF_duration, select=c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb", "dur.radright.protest", "dur.rightist.protest", "dur.leftist.protest"))
partisanDF_duration <- subset(partisanDF_duration, v004year>1993)

partisan_duration <- reshape2::melt(partisanDF_duration[,c("v004year", "dur.radright.protest", "dur.rightist.protest", "dur.leftist.protest")], id=c("v004year"))

partisan_duration <- partisan_duration %>%
  mutate(Ideology = recode(variable, dur.leftist.protest = 'Left', dur.rightist.protest = 'Mainstream right', dur.radright.protest =  'Radical right' ))
partisan_duration  <- subset(partisan_duration , select=c("v004year", "value", "Ideology"))
partisan_duration <- partisan_duration %>% 
  dplyr::rename("Duration (days)"= "value")

df_countsduration <- merge(partisan_counts, partisan_duration)

BalJobb_dur <- ggplot(df_countsduration, aes(v004year, y = Count, color = Ideology)) +
  geom_line() +
  geom_point(aes(size = `Duration (days)`)) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 1998, 2002, 2006, 2010)) +
  scale_size(range = c(0, 15)) +
  scale_color_grey(
    name = "Ideology",
    labels = c("Left", "Mainstream right", "Radical right"),
    start = 0.9, end = 0.2
  ) +
  labs(
    title = "Partisan Protests in Hungary, 1994-2011",
    subtitle = "Bubble Size Represents Average Annual Partisan Protest Duration",
    x = "Year", 
    y = "Count of Protests",
    color = "Ideology"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_classic(base_size = 12)

BalJobb_dur

# Put this all in a dataframe of partisan protest yearly volume and average yearly salience
partisanDF_salience <- data.frame(protest_bal,protest_centerjobb, protest_selsojobb, salience_selsojobb, salience_centerjobb, salience_bal)
partisanDF_salience <- subset(partisanDF_salience, select=c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb", "salience.radright.protestors", "salience.rightist.protestors", "salience.leftist.protestors"))
partisanDF_salience <-subset(partisanDF_salience, v004year>1993)

partisan_salience <- reshape2::melt(partisanDF_salience[,c("v004year", "salience.radright.protestors", "salience.rightist.protestors", "salience.leftist.protestors")], id=c("v004year"))

partisan_salience <- partisan_salience %>%
  mutate(Ideology = recode(variable, salience.leftist.protestors = 'Left', salience.rightist.protestors = 'Mainstream right', salience.radright.protestors =  'Radical right' ))
partisan_salience  <- subset(partisan_salience , select=c("v004year", "value", "Ideology"))
partisan_salience <- partisan_salience %>% 
  dplyr::rename("Salience (0-9)"= "value")

df_countssalience <- merge(partisan_counts, partisan_salience)

BalJobb_salience <- ggplot(df_countssalience, aes(v004year, y = Count, color = Ideology)) +
  geom_line() +
  geom_point(aes(size = `Salience (0-9)`)) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 1998, 2002, 2006, 2010)) +
  scale_size(range = c(0, 10)) +
  scale_color_grey(
    name = "Ideology",
    labels = c("Left", "Mainstream right", "Radical right"),
    start = 0.9, end = 0.2
  ) +
  labs(
    title = "Partisan Protests in Hungary, 1994-2011",
    subtitle = "Bubble Size Represents Average Annual Partisan Protest Salience",
    x = "Year",
    y = "Count of Protests",
    color = "Ideology"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_classic(base_size = 12)

BalJobb_salience

# Now put all three sub-figures (size, duration, salience) together for Figure 2 in the main text
FigureA1 <- ggarrange(BalJobb_size, BalJobb_dur, BalJobb_salience,
                         labels = c("", ""),
                         ncol = 1, nrow = 3,
                         common.legend =F,
                         legend = "right")
FigureA1
ggsave(FigureA1, file="./FigureA1_BubblePlots.pdf", height = 13, width = 8, dpi = 250)

# Now let's create Figure 4, on left and right protest demands. These are derived from the v080 variables classifying demands.

discriminatory <- subset(HUNdata, v08025==1)
antielite <- subset(HUNdata, v08023==1)
antisystem <- subset(HUNdata, v08024==1)
antilegalsystem <- subset(HUNdata, v08022==1)
antiliberalism <- subset(HUNdata, v08010==1)

protest_discriminatory <- ddply(discriminatory,.(v004year),summarize,count.protest_discriminatory=sum(right_all))
protest_antielite <- ddply(antielite,.(v004year),summarize,count.protest_antielite=sum(right_all))
protest_antisystem <- ddply(antisystem,.(v004year),summarize,count.protest_antisystem=sum(right_all))
protest_antilegalsystem <- ddply(antilegalsystem,.(v004year),summarize,count.protest_antilegalsystem=sum(right_all))
protest_antiliberalism <- ddply(antiliberalism,.(v004year),summarize,count.protest_antiliberalism=sum(right_all))

# Combine datasets into a list
datasets <- list(protest_antiliberalism, protest_antilegalsystem, protest_discriminatory, protest_antielite, protest_antisystem)

# Merge all datasets by "id" using full join
protestdemands_df <- purrr::reduce(datasets, full_join, by = "v004year")
protestdemands_df[is.na(protestdemands_df)] <- 0
print(protestdemands_df)

colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antiliberalism'] <- 'Anti-liberal'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antilegalsystem'] <- 'Anti-legal system'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_discriminatory'] <- 'Discriminatory'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antielite'] <- 'Anti-elite'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antisystem'] <- 'Anti-system'
long_protestdemands_df <- reshape2::melt(protestdemands_df,id="v004year")
long_protestdemands_df$Ideology <- 'Mainstream or radical right'

AllRight <- ggplot(long_protestdemands_df, aes(v004year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 2002, 2010)) +
  ylim(0, 150) +
  scale_color_grey(
    name = "Protest type",
    labels = c("Anti-liberal", "Anti-legal system", "Discriminatory", "Anti-elite", "Anti-system"),
    start = 0.8, end = 0.2
  ) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    title = "Right Protests by Demand, 1994-2011",
    x = "Year",
    y = "Count of Protests"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.text.x = element_text(size = 8)
  )

AllRight


# Now let's do the same analysis but for leftist demands in leftist protests
democracy <- subset(HUNdata, v08011==1)
individualrights <- subset(HUNdata, v08014==1)
law <- subset(HUNdata, v08013==1)
socialequity <- subset(HUNdata, v08015==1)
workdignity <- subset(HUNdata, v08005==1)

protest_democracy_left <- ddply(democracy,.(v004year),summarize,count.protest_democracy=sum(leftist))
protest_individualrights_left <- ddply(individualrights,.(v004year),summarize,count.protest_individualrights=sum(leftist))
protest_law_left <- ddply(law,.(v004year),summarize,count.protest_law=sum(leftist))
protest_socialequity_left <- ddply(socialequity,.(v004year),summarize,count.protest_socialequity=sum(leftist))
protest_workdignity_left <- ddply(workdignity,.(v004year),summarize,count.protest_workdignity=sum(leftist))

# Combine datasets into a list
datasets_left <- list(protest_workdignity_left, protest_socialequity_left, protest_individualrights_left,
                      protest_democracy_left,  protest_law_left)

# Merge all datasets by "id" using full join
protestdemands_df_left<- purrr::reduce(datasets_left, full_join, by = "v004year")
protestdemands_df_left[is.na(protestdemands_df_left)] <- 0
print(protestdemands_df_left)

colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_workdignity'] <- 'Labor rights'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_democracy'] <- 'Pro-democracy'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_individualrights'] <- 'Individual rights'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_socialequity'] <- 'Social equity'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_law'] <- 'Pro-legal system'
long_protestdemands_df_left <- reshape2::melt(protestdemands_df_left,id="v004year")

Left <- ggplot(long_protestdemands_df_left, aes(v004year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 2002, 2010)) +
  ylim(0, 150) +
  scale_color_grey(
    name = "Protest type",
    labels = c("Labor rights", "Social equity", "Individual rights", "Pro-democracy", "Pro-legal system"),
    start = 0.8, end = 0.2
  ) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    title = "Left Protests by Demand, 1994-2011",
    x = "Year",
    y = "Count of Protests"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.text.x = element_text(size = 8)
  )

Left

FigureA3 <- ggarrange(AllRight, Left,
                     labels = c("", ""),
                     ncol = 1, nrow = 2,
                     common.legend =F,
                     legend = "right")
FigureA3
ggsave(FigureA3, file="./FigureA3_ProtestDemands.pdf", height = 10, width = 8, dpi = 250)


# Now we move on to data preparation for Figure 3
left$ideology <-'Left'
right$ideology <-'Right'
agghun_R<-ddply(right,.(v004year),numcolwise(sum, na.rm = TRUE))			# Aggregate numeric variables 
agghun_L<-ddply(left,.(v004year),numcolwise(sum, na.rm = TRUE))				# Aggregate numeric variables 

# Figure on methods of protest
protest_methods_R <-data.frame(agghun_R,"action_nondisruptive","action_disruptive")
protest_methods_R <- subset(protest_methods_R, select=c("v004year", "action_nondisruptive","action_disruptive"))
long_protest_methods_R <- reshape2::melt(protest_methods_R,id="v004year")
long_protest_methods_R$Ideology <-'Right'

protest_methods_L <-data.frame(agghun_L,"action_nondisruptive","action_disruptive")
protest_methods_L <- subset(protest_methods_L, select=c("v004year", "action_nondisruptive","action_disruptive"))
long_protest_methods_L <- reshape2::melt(protest_methods_L,id="v004year")
long_protest_methods_L$Ideology <-'Left'

long_protest_methods <-rbind(long_protest_methods_R, long_protest_methods_L)
colnames(long_protest_methods)[colnames(long_protest_methods) == "variable"] ="Method"
long_protest_methods$Method<-as.character(long_protest_methods$Method)
long_protest_methods$Method[long_protest_methods$Method=='action_nondisruptive']<-'Non-disruptive'
long_protest_methods$Method[long_protest_methods$Method=='action_disruptive']<-'Disruptive'

top_FigureA2 <- ggplot(long_protest_methods,aes(v004year, y=value, color=Ideology, linetype=Method)) +
  geom_line() +geom_point(size=3) + xlab("Year") + ylab("Count of Protests") +
  scale_x_continuous(limits=c(1994,2011), breaks=c(1994,1998,2002,2006,2010)) + scale_color_grey() +
  theme_classic(base_size = 12)+
  ggtitle("Protest Methods in Hungary, 1994-2011") + guides(linetype= guide_legend(order = 1), 
                                                            color = guide_legend(order = 2))
top_FigureA2

# v026 asks if protest actions were legal. "1" means no (e.g., illegal)
protest_jobb_illeg <- ddply(right,.(v004year),summarize,count.protest_jobbilleg=sum(v026==1))
protest_jobb_viol <- ddply(right,.(v004year),summarize,count.protest_jobbviolent=sum(action_violent==1))
protest_bal_illeg <- ddply(left,.(v004year),summarize,count.protest_balilleg=sum(v026==1))
protest_bal_viol <- ddply(left,.(v004year),summarize,count.protest_balviolent=sum(action_violent==1))

partisanDF_jobb <- merge(protest_jobb_illeg, protest_jobb_viol,  by="v004year", all = T)
partisanDF_bal <- merge(protest_bal_illeg, protest_bal_viol,  by="v004year", all = T)
partisanDF_types  <- merge(partisanDF_bal, partisanDF_jobb,  by="v004year", all = T)
long_partisanDFtypes <- reshape2::melt(partisanDF_types,id="v004year")

long_partisanDFtypes$Ideology[long_partisanDFtypes$variable=="count.protest_jobbilleg" |
                                long_partisanDFtypes$variable=="count.protest_jobbviolent"] <- "Right"
long_partisanDFtypes$Ideology[long_partisanDFtypes$variable=="count.protest_balilleg" |
                                long_partisanDFtypes$variable=="count.protest_balviolent"] <- "Left"

long_partisanDFtypes$Method<-NA
long_partisanDFtypes$Method[long_partisanDFtypes$variable=='count.protest_jobbilleg' | long_partisanDFtypes$variable=='count.protest_balilleg']<-'Illegal'
long_partisanDFtypes$Method[long_partisanDFtypes$variable=='count.protest_jobbviolent' | long_partisanDFtypes$variable=='count.protest_balviolent']<-'Aggressive'

bottom_FigureA2 <- ggplot(long_partisanDFtypes,aes(v004year, y=value, color=Ideology, linetype=Method)) +
  geom_line() +geom_point(size=3) + xlab("Year") + ylab("Count of Protests") +
  scale_x_continuous(limits=c(1994,2011), breaks=c(1994,1998,2002,2006,2010)) + scale_color_grey() +
  theme_classic(base_size = 12)+
  ggtitle("Extreme Disruptive Protests in Hungary, 1994-2011") + guides(linetype= guide_legend(order = 1), 
                                                                        color = guide_legend(order = 2))
bottom_FigureA2

FigureA2 <- ggarrange(top_FigureA2, bottom_FigureA2,
                         labels = c("", ""),
                         ncol = 1, nrow = 2,
                         common.legend =F,
                         legend = "right")
FigureA2 

ggsave(FigureA2, file="./FigureA2_ProtestMethods.pdf", height = 10, width = 8, dpi = 250)

# Create a new cleaned year, month, day variable.
# This includes recording (as NA) 2 cases of "0" and 1 case of "19" for month (presumably due to input error)
# We will also visualize election periods by shading those periods.
HUNdata$year <- HUNdata$v004year
HUNdata$month <- HUNdata$v004month
HUNdata$month[HUNdata$month==0 | HUNdata$month==19] <- NA 
HUNdata$day <- HUNdata$v004day

HUNdata_Left<- subset(HUNdata, leftist==1)
testHUN_L <- ddply(HUNdata_Left, .(year, month), summarise, count.protest=length(unique(uniqueID)))
testHUN_L$day <- "01"
testHUN_L$month[testHUN_L$month==1] <- "01"
testHUN_L$month[testHUN_L$month==2] <- "02"
testHUN_L$month[testHUN_L$month==3] <- "03"
testHUN_L$month[testHUN_L$month==4] <- "04"
testHUN_L$month[testHUN_L$month==5] <- "05"
testHUN_L$month[testHUN_L$month==6] <- "06"
testHUN_L$month[testHUN_L$month==7] <- "07"
testHUN_L$month[testHUN_L$month==8] <- "08"
testHUN_L$month[testHUN_L$month==9] <- "09"
testHUN_L$month[testHUN_L$month==10] <- "10"
testHUN_L$month[testHUN_L$month==11] <- "11"
testHUN_L$month[testHUN_L$month==12] <- "12"
testHUN_L <- subset(testHUN_L, testHUN_L$year!=1988)
testHUN_L <- na.omit(testHUN_L)
testHUN_L$date <- strptime(paste(testHUN_L$year, testHUN_L$month, testHUN_L$day, sep="-"), 
                           format="%Y-%m-%d")

HUNdata_Rad<- subset(HUNdata, radrightist==1)
testHUN_Rad <- ddply(HUNdata_Rad, .(year, month), summarise, count.protest=length(unique(uniqueID)))
testHUN_Rad$day <- "01"
testHUN_Rad$month[testHUN_Rad$month==1] <- "01"
testHUN_Rad$month[testHUN_Rad$month==2] <- "02"
testHUN_Rad$month[testHUN_Rad$month==3] <- "03"
testHUN_Rad$month[testHUN_Rad$month==4] <- "04"
testHUN_Rad$month[testHUN_Rad$month==5] <- "05"
testHUN_Rad$month[testHUN_Rad$month==6] <- "06"
testHUN_Rad$month[testHUN_Rad$month==7] <- "07"
testHUN_Rad$month[testHUN_Rad$month==8] <- "08"
testHUN_Rad$month[testHUN_Rad$month==9] <- "09"
testHUN_Rad$month[testHUN_Rad$month==10] <- "10"
testHUN_Rad$month[testHUN_Rad$month==11] <- "11"
testHUN_Rad$month[testHUN_Rad$month==12] <- "12"
testHUN_Rad <- subset(testHUN_Rad, testHUN_Rad$year!=1988)
testHUN_Rad <- na.omit(testHUN_Rad)
testHUN_Rad$date <- strptime(paste(testHUN_Rad$year, testHUN_Rad$month, testHUN_Rad$day, sep="-"), 
                             format="%Y-%m-%d")

HUNdata_Right<- subset(HUNdata, rightist==1)
testHUN_Right <- ddply(HUNdata_Right, .(year, month), summarise, count.protest=length(unique(uniqueID)))
testHUN_Right$day <- "01"
testHUN_Right$month[testHUN_Right$month==1] <- "01"
testHUN_Right$month[testHUN_Right$month==2] <- "02"
testHUN_Right$month[testHUN_Right$month==3] <- "03"
testHUN_Right$month[testHUN_Right$month==4] <- "04"
testHUN_Right$month[testHUN_Right$month==5] <- "05"
testHUN_Right$month[testHUN_Right$month==6] <- "06"
testHUN_Right$month[testHUN_Right$month==7] <- "07"
testHUN_Right$month[testHUN_Right$month==8] <- "08"
testHUN_Right$month[testHUN_Right$month==9] <- "09"
testHUN_Right$month[testHUN_Right$month==10] <- "10"
testHUN_Right$month[testHUN_Right$month==11] <- "11"
testHUN_Right$month[testHUN_Right$month==12] <- "12"
testHUN_Right <- subset(testHUN_Right, testHUN_Right$year!=1988)
testHUN_Right <- na.omit(testHUN_Right)
testHUN_Right$date <- strptime(paste(testHUN_Right$year, testHUN_Right$month, testHUN_Right$day, sep="-"), 
                               format="%Y-%m-%d")

testHUN_Right<-subset(testHUN_Right, year > 2009)
testHUN_Right$ideology <- "Mainstream right"
testHUN_Rad<-subset(testHUN_Rad, year > 2009)
testHUN_Rad$ideology <- "Radical right"
testHUN_L<-subset(testHUN_L, year > 2009)
testHUN_L$ideology <- "Left"

FideszEra_leftright <- dplyr::full_join(testHUN_L, testHUN_Right)
FideszEra_protests <- dplyr::full_join(FideszEra_leftright, testHUN_Rad)

FigureA5 <- ggplot(FideszEra_protests, aes(x=as.POSIXct(date), y=count.protest, color=ideology)) + geom_point(size=2) + geom_line(size=1) + 
  xlab("Year and month") + ylab("Count of Protests") + ggtitle("Monthly partisan protests in Hungary (2010-2011)") + 
  scale_color_grey(name="Ideology",labels=c("Left","Mainstream right", "Radical right"),start=.8,end=.2) +
  scale_x_datetime(labels=date_format("%b %Y"),
                   breaks=c(as.POSIXct("2010/01/01"), as.POSIXct("2010/05/01"),
                            as.POSIXct("2010/09/01"),
                            as.POSIXct("2011/01/01"),as.POSIXct("2011/05/01"),
                            as.POSIXct("2011/09/01")),
                   limits=c(as.POSIXct("2010/01/01"),as.POSIXct("2011/12/01"))) + 
  ggplot2::annotate("rect", xmin=as.POSIXct("2010/07/01"), xmax=as.POSIXct("2011/04/01"), ymin=0, ymax=30, alpha=0.2) +
  geom_vline(xintercept = as.POSIXct("2010/04/01")) +
  ggplot2:: annotate("text", x =as.POSIXct("2010/05/10") , y =20, label="2010 \n election")   +
  theme_classic(base_size=12)
FigureA5
ggsave(FigureA5, file="./FigureA5.pdf", height = 6, width = 9, dpi = 250)


