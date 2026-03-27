##############################################
### basic R code for the visualisation in:

# Vink, M., Schakel, A., Reichel, D., DeGroot, R., and Luk, C. (2019):
# The international diffusion of expatriate dual citizenship,
# Migration Studies

#This script reproduces Figures 1, 2, 3 and A1.

#Additional file needed for replication of Figure A1: 'data_figure3'
#See 'Vink_Schakel_Reichel_Chun_deGroot_2019_replication_Stata_do-file' for replication of data for Figure 3.
##############################################


library(ggplot2)
library(scales)
library(dplyr)
library(openxlsx)
library(tidyr)
# December 2018

# set working directory where all the files are stored
# insert the directory instead of PATH with the following command: setwd("PATH") 
setwd("PATH")

# read the dataset
#NB note that the Figures draw on the full dataset (i.e. not the censored event data file)
dat <- read.csv("Vink_Schakel_Reichel_Chun_deGroot_2019_full.csv",
                stringsAsFactors = FALSE, na.strings = "")

# --------------------------------

## region coding update
dat$world_region2 <- ifelse(dat$world_region == 1, "Africa",
                            ifelse(dat$world_region == 2, "Asia",
                                   ifelse(dat$world_region == 3, "Europe",
                                          ifelse(dat$world_region == 6, "Oceania",
                                                 ifelse(dat$world_region == 4 | dat$world_region == 5,
                                                        "America", NA)))))

dat$world_region2[dat$ISO3 %in% c("DDR", "YUG", "CSK")] <- "Europe"


#------------------------------------------------------
# -----------------------------------------------------
daty <- dat %>%
  group_by(Year) %>%
  summarise(meanDualcit_Year = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()
daty

datyr <- dat %>%
  group_by(Year, world_region2) %>%
  summarise(meanDualcit_Year = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()
datyr

############ Figure 1

p1 <- ggplot(daty, aes(Year, meanDualcit_Year)) + 
  geom_line(size=1.5) +
  theme_bw() +
  geom_line(data=na.omit(datyr), 
            aes(Year, meanDualcit_Year, group=world_region2))+
  geom_text(data=subset(datyr, Year==2015), 
            aes(Year, meanDualcit_Year, label=world_region2, vjust=-0.4, hjust = 0))+
  scale_y_continuous("Percent countries allowing dual citizenship",
                     labels = percent)+
  scale_x_continuous(breaks = seq(1960, 2030, 10))+
  annotate(geom = "text", x = 1990, y = 0.43, label = "World", fontface = "bold", size = 5)
p1

# Figure A 1
# countries that always existed
dat$filternotexist <- ifelse(dat$Dualcit_binary==0, 1, 0)
dat <- dat %>%
  group_by(ISO3) %>%
  mutate(filternotexist2 = sum(filternotexist, na.rm = TRUE)) %>%
  ungroup()

dat$filternotexist2 <- ifelse(dat$filternotexist2>0, 1, 0)

datex <- subset(dat, filternotexist2==0)
datexy <- datex %>%
  group_by(Year) %>%
  summarise(meanDualcit_Year = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()

datexyr <- datex %>%
  group_by(Year, world_region2) %>%
  summarise(meanDualcit_Year = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()
datexyr

p1b <- ggplot(datexy, aes(Year, meanDualcit_Year)) + 
  geom_line(size=1.5) +
  theme_bw() +
  geom_line(data=na.omit(datexyr), 
            aes(Year, meanDualcit_Year, group=world_region2)) + #, colour = factor(world_region)))+
  geom_text(data=subset(datexyr, Year==2015), 
            aes(Year-1, meanDualcit_Year, label=world_region2, vjust=-0.3))+
  scale_y_continuous("Percent countries allowing dual citizenship",
                     labels = percent)+
  scale_x_continuous(breaks = seq(1960, 2019, 10))+
  annotate(geom = "text", x = 1999, y = 0.6, label = "World", fontface = "bold")
p1b


# -------------------------------------------------------------
# Figure 2
# changes dotplot
dat <- dat %>%
  group_by(ISO3) %>%
  mutate(changeany = Dualcit_binary2 != lag(Dualcit_binary2),
         changewhich = ifelse(changeany == TRUE & Dualcit_binary2 == 1, "allowing dual citizenship",
                              ifelse(changeany == TRUE & Dualcit_binary2 == 0, "restricting dual citizenship",
                                     "no change"))) %>%
  ungroup()

datchange <- subset(dat, changeany == TRUE) %>% dplyr::select(Year, ISO3, Dualcit_binary2,
                                                              changeany, changewhich,
                                                              world_region2)

dattemp <- dat %>%
  group_by(ISO3) %>%
  summarise(meandc = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(change = meandc > 0 & meandc < 1,
         alw = meandc == 0,
         nev = meandc == 1)

p2 <- ggplot(datchange, aes(Year, fill = changewhich))+
  geom_dotplot(binwidth=1, stackgroups = TRUE, binpositions = "all")+
  scale_x_continuous(breaks=seq(1960, 2018, 10))+
  scale_y_continuous("Number of changes",
                     limits=c(0,10), breaks = seq(0,10,3))+
  scale_fill_manual("Change to:", values = c("grey", "black")) +
  theme_bw()+
  coord_fixed(ratio=1)
p2

# -------------------------------------------------------------
# Figure 3

d <- read.xlsx("data_figure3.xlsx", sheet = 1) %>%
  gather(response, value, No:Yes)

p3 <- ggplot(d, aes(years, value, linetype = response, group = response)) +
  geom_line() +
  facet_wrap(~variable, ncol = 2) +
  theme_bw() +
  scale_linetype("") +
  scale_y_continuous("Dual citizenship acceptance", limits = c(0,1)) +
  scale_x_continuous("Years", limits = c(0,65), breaks = seq(0,60,10)) +
  theme(legend.position = "none", 
        strip.background = element_rect(fill = "white")) +
  geom_text(data = filter(d, years == max(d$years)), aes(years, value, label = response),
            hjust = -0.2)
p3


# run on Windows 10 operating system
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# package versions
# tidyr_0.8.2    openxlsx_4.1.0 bindrcpp_0.2.2 dplyr_0.7.8    scales_1.0.0  ggplot2_3.1.0

