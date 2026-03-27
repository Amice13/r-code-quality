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
library(reshape2)
library(ggridges)
library(hrbrthemes)
library(ggbeeswarm)

# Import datasets
spiked_variants <- read.csv('data/variant.list.csv')
variant_calls <- read.csv('data/error_profile_data/error_profile0_data/all_error_profile0_variants.csv')

# spiked variants from the new simulations
#spiked_variants <- read_csv("data/new_minority_datasets/minority_variant_list.csv")
#variants_calls <- read_csv("data/new_minority_datasets/artificial_dataset2/artificial_dataset2_variants.csv") 

#Combine the two datasets with a full join. We are interested with positions.
combined <- full_join(variant_calls,spiked_variants,by=c("position")) %>%
  filter(!is.na(alternative.y)) %>%
        # !position == 655) %>%
  rename(spiked = freq.y,
         called = freq.x,
         spiked_nucleotide = alternative.y,
         called_nucleotide = alternative.x) %>%
  select(position,sampleName,caller,spiked,called) %>%
  gather("frequency_type","frequencies",spiked,called) %>%
  #mutate(coverage = factor(sampleName, levels = c("X20","X50","X100","X500","X1000","X2000","X5000","X10000"))) %>%
  mutate(coverage = factor(sampleName, levels = c("sample1","sample2","sample3","sample4","sample5","sample6","sample7","sample8"))) %>%
  arrange(coverage) %>%
  filter(!is.na(caller))
  
 ridge_plots <- ggplot(combined,aes(frequencies,caller,fill=frequency_type)) +
   geom_density_ridges(alpha = .7, colour = "white") +
   scale_fill_manual(values = c("#ff0000", "#0000ff"),name=" ") +
   scale_x_continuous() +
   xlim(c(0,1)) +
   theme_ipsum_rc() +
   facet_wrap(~coverage,nrow = 2)
 
 sample1_freqs <- combined %>%
   filter(sampleName == "sample1")
 
 sample2_freqs <- combined %>%
    filter(sampleName == "sample2")

 sample3_freqs <- combined %>%
   filter(sampleName == "sample3")
 
 sample4_freqs <- combined %>%
   filter(sampleName == "sample4")
 
 sample5_freqs <- combined %>%
   filter(sampleName == "sample5")
 
 sample6_freqs <- combined %>%
   filter(sampleName == "sample6")
 
 sample7_freqs <- combined %>%
   filter(sampleName == "sample7")
 
 sample8_freqs <- combined %>%
   filter(sampleName == "sample8")

 # Figure 5
 sample1.plot <- ggplot(sample1_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X20",tag="A") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()

 sample2.plot <- ggplot(sample2_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X50",tag="B") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()
 
 sample3.plot <- ggplot(sample3_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X100",tag="C") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()
 
 sample4.plot <- ggplot(sample4_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X500",tag="D") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()
 
 sample5.plot <- ggplot(sample5_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X1000",tag="E") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()
 
 sample6.plot <- ggplot(sample6_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X2000",tag="F") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()
 
 sample7.plot <- ggplot(sample7_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X5000",tag="G") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()
 
 sample8.plot <- ggplot(sample8_freqs,aes(frequency_type,frequencies)) +
   geom_beeswarm(size=0.4,priority='density') +
   labs(x="",y="",subtitle="X10000",tag="H") +
   facet_wrap(~caller,nrow = 1) +
   theme_ipsum()
 
pdf(file="figures/figure5.pdf",height = 15,width = 20, onefile = FALSE)
 sample1.plot + sample2.plot + sample3.plot + sample4.plot + 
 sample5.plot + sample6.plot + sample7.plot + sample8.plot +
   plot_layout(nrow = 4)
 dev.off()
 