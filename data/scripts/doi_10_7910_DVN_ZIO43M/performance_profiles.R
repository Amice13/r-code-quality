#######################################################################
# Copyright (C) 2018  George Githinji

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################################

library(tidyverse)
library(hrbrthemes)
library(patchwork)
library(extrafont)
library(reshape2)
loadfonts()

#reference <- read.csv('data/myrefrence_in_column.csv')
#variants.list <- read.csv('data/variant.list.csv')

#profile1.truth_table <- read.csv("data/artificial_data/truth_table.csv") %>%
#profile1.truth_table <- read.csv("data/dataset1_truth_table.csv") %>%
#profile1.truth_table <- read.csv("data/dataset1_low_truth_table.csv") %>%
profile1.truth_table <- read.csv("data/dataset1_moderate_truth_table.csv") %>%
#profile1.truth_table <- read.csv("data/dataset1_standard_truth_table.csv") %>%
#profile1.truth_table <- read.csv("data/new_minority_datasets/artificial_dataset1/minority_dataset1_truth_table.csv") %>%
#profile1.truth_table <- read.csv("data/new_minority_datasets/artificial_dataset1/minority_dataset1_moderate_truth_table.csv") %>%
  mutate(coverage = if_else( sample == "sample1",20, 
                            if_else(sample == "sample2",50,
                                   if_else(sample == "sample3",100,
                                          if_else(sample == "sample4",500,
                                                 if_else(sample == "sample5",1000,
                                                        if_else(sample == "sample6",2000,
                                                               if_else(sample == "sample7", 5000, 10000)
                                                        )))))))
         

profile1.plot1 <- ggplot(profile1.truth_table,aes(factor(coverage),sensitivity,colour = caller,group = caller)) +
  geom_point(colour = 'black',size=2) +
  geom_smooth(method = "loess", color = 'black',se = FALSE) +
  ylim(0,1) +
  labs(x="Coverage",y="Sensitivity",tag="A") +
  theme_ipsum() +
  theme(text = element_text(size = 20),
        plot.subtitle=element_text(size=20, face="bold"),
        axis.text.x = element_text(hjust = 1,angle = 90)) +
  facet_wrap( ~ caller)


#Scatterplot TPR vs PPV for all the samples using facet_wrap function
profile1.plot2 <- ggplot(profile1.truth_table, aes(factor(coverage),precision,group = caller)) + 
  geom_point(size=2,colour="black") + 
  geom_smooth(method = "loess", color = 'black',se = FALSE) +
  ylim(0,1) +
  labs(x="Coverage",y="Precision",tag="D") + 
  theme_ipsum() +
  theme(text = element_text(size = 20),
        plot.subtitle=element_text(size=20, face="bold"),
        axis.text.x = element_text(hjust = 1,angle = 90)) +
  facet_wrap(~caller)


#profile2.truth_table <- read.csv("data/error_profile _data/error_profile2_data/em_samples_truth_table.csv") %>%
#profile2.truth_table <- read.csv("data/dataset2_truth_table.csv") %>%
#profile2.truth_table <- read.csv("data/dataset2_low_truth_table.csv") %>%
profile2.truth_table <- read.csv("data/dataset2_moderate_truth_table.csv") %>%
#profile2.truth_table <- read.csv("data/dataset2_standard_truth_table.csv") %>%
#profile2.truth_table <- read.csv("data/new_minority_datasets/artificial_dataset2/artificial_dataset2_truth_table.csv") %>%
#profile2.truth_table <- read.csv("data/new_minority_datasets/artificial_dataset2/minority_dataset2_moderate_truth_table.csv") %>%
  mutate(coverage = ifelse( sample == "sample1",20,
                            ifelse(sample == "sample2",50,
                                   ifelse(sample == "sample3",100,
                                          ifelse(sample == "sample4",500,
                                                 ifelse(sample == "sample5",1000,
                                                        ifelse(sample == "sample6",2000,
                                                               ifelse(sample == "sample7", 5000, 10000)
                                                               )))))))

profile2.plot1 <- ggplot(profile2.truth_table,aes(factor(coverage),sensitivity,colour = caller,group = caller)) +
  geom_point(color = 'black',size=2) +
  geom_smooth(method = "loess", color = 'black',se = FALSE) +
  ylim(0,1) +
  labs(x="Coverage",y="Sensitivity",subtitle="B") +
  theme_ipsum() +
  theme(text = element_text(size = 20),
        plot.subtitle=element_text(size=20, face="bold"),
        axis.text.x = element_text(hjust = 1,angle = 90)) +
  facet_wrap( ~ caller)

#Scatterplot TPR vs PPV for all the samples using facet_wrap function
profile2.plot2 <- ggplot(profile2.truth_table, aes(factor(coverage),precision,group = caller)) + 
  geom_point(size=2,colour="black") + 
  geom_smooth(method = "loess", color = 'black',se = FALSE) +
  ylim(0,1) +
  labs(x="Coverage",y="Precision",subtitle="E") + 
  theme_ipsum() +
  theme(text = element_text(size = 20),
        plot.subtitle=element_text(size=20, face="bold"),
        axis.text.x = element_text(hjust = 1,angle = 90)) +
  facet_wrap(~caller)


#profile3.truth_table <- read.csv("data/dataset3_truth_table.csv") %>%
#profile3.truth_table <- read.csv("data/dataset3_low_truth_table.csv") %>%
profile3.truth_table <- read.csv("data/dataset3_moderate_truth_table.csv") %>%
#profile3.truth_table <- read.csv("data/dataset3_standard_truth_table.csv") %>%
#profile3.truth_table <- read.csv("data/new_minority_datasets/artificial_dataset2/artificial_dataset2_truth_table.csv") %>%
#profile3.truth_table <- read.csv("data/new_minority_datasets/artificial_dataset3/minority_dataset3_moderate_truth_table.csv") %>%
  mutate(coverage = ifelse( sample == "sample1",20,
                            ifelse(sample == "sample2",50,
                                   ifelse(sample == "sample3",100,
                                          ifelse(sample == "sample4",500,
                                                 ifelse(sample == "sample5",1000,
                                                        ifelse(sample == "sample6",2000,
                                                               ifelse(sample == "sample7", 5000, 10000)
                                                        )))))))

profile3.plot1 <- ggplot(profile3.truth_table,aes(factor(coverage),sensitivity,colour = caller,group = caller)) +
  geom_point(color = 'black',size=2) +
  geom_smooth(method = "loess", color = 'black',se = FALSE) +
  ylim(0,1) +
  labs(x="Coverage",y="Sensitivity",subtitle="C") +
  theme_ipsum() +
  theme(text = element_text(size = 20),
        plot.subtitle=element_text(size=20, face="bold"),
        axis.text.x = element_text(hjust = 1,angle = 90)) +
  facet_wrap( ~ caller)

#Scatterplot TPR vs PPV for all the samples using facet_wrap function
profile3.plot2 <- ggplot(profile3.truth_table, aes(factor(coverage),precision,group = caller)) + 
  geom_point(size=2,colour="black") + 
  geom_smooth(method = "loess", color = 'black',se = FALSE) +
  labs(x="Coverage",y="Precision",subtitle="F") + 
  ylim(0,1) +
  theme_ipsum() +
  theme(text = element_text(size = 20),
        plot.subtitle=element_text(size=20, face="bold"),
        axis.text.x = element_text(hjust = 1,angle = 90)) +
  facet_wrap(~caller)


pdf(file="figures/figure4_moderate.pdf",height = 15,width = 20, onefile = FALSE)
profile1.plot1 + profile2.plot1 + profile3.plot1 + 
  profile1.plot2 + profile2.plot2 + profile3.plot2 + plot_layout(nrow = 2)
dev.off()

