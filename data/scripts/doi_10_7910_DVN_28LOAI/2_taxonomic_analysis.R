# load required packages
library(tidyverse)
library(tools)
library(scales)
library(viridis)
library(stringr)

# create a function to read taxonomy
read_taxonomy <- function(file){
  kraken_classification <- read_tsv(file,col_names = FALSE) %>%
    rename(percentage_fragments_covered = X1,
           fragments_covered = X2,
           fragments_assigned = X3,
           rank_code=X4,
           taxonomic_code=X5,
           scientific_name=X6) %>%
    mutate(sample_name=unlist(strsplit(basename(file_path_sans_ext(file)),split ="[.]"))[1])
}

# create path to kraken files
kraken_files <- list.files("kraken_direct/",pattern = ".txt",full.names = T) #for the samples that underwent direct extraction. Edit to fit centrifugal processing

# get kraken reports
kraken_reports   <- lapply(kraken_files,read_taxonomy) %>%
  bind_rows()  %>%
  mutate(log2_fragments_covered = log2(fragments_covered))

# Farmiliarizing with the data distribution
overview_dta <- kraken_reports %>%
  filter(rank_code == "U" | rank_code == "R")

#subsettin the main domains in the data
domains_count <- kraken_reports  %>%
  filter(rank_code == "D",
         percentage_fragments_covered > 0)
#making a bargraph of the main domains
ggplot(domains_count,aes(sample_name,fragments_covered,fill=scientific_name)) +
  geom_bar(stat = "identity",position = position_dodge2(preserve = "single")) +
  scale_y_log10(labels=comma) +
  scale_fill_manual(values=c("Bacteria"="#999999","Eukaryota"="#E69F00","Viruses"="#56B4E9","Archaea"="black"),
                    name="") +
  labs(x="Barcodes",y="log2 fragments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(face = "bold",size = 12),
        axis.title.x = element_text(face = "bold",size = 12),
        legend.position="bottom")+ labs(tag = "B", plot.tag.position = c(0.2, -0.1))

#subsetting the main genera in the dataset
genus_count <- kraken_reports  %>%
  filter(rank_code == "G",
         scientific_name != "Homo",
         fragments_covered > 30)

#subset the proportion of human reads in each barcode
genus_human_count <- kraken_reports  %>%
  filter(rank_code == "G",
         scientific_name == "Homo",
         fragments_covered > 30)

Orthopneumovirus <- kraken_reports  %>%
  filter(rank_code == "G",
         scientific_name == "Orthopneumovirus",
         fragments_covered > 0)
write.csv(Orthopneumovirus, "orthopneumovirus_direct.csv", row.names = FALSE) #generate these reads from the direct RNA approach similarly

#export the data and edit
#I added a "treatment" column to the file for 1-13: untreated while 14-24; Treated, as clarified in the metadata files
write.csv(genus_human_count, file = "genus_human_count.csv", row.names = FALSE)

#read back edited data
human_count_data <- read.csv("genus_centrifugalhuman_count.csv")
human_count_data <- read.csv("genus_directhuman_count.csv")


# making a boxplot of treatment against percentage fragments covered from the host reads
P <- ggboxplot(human_count_data, "Treatment","percentage_fragments_covered",
               color = "Treatment", palette =c("#508578", "#C84248"),
               add = "jitter", linetype = "solid", Family = "Palatino Linotype", add.params = list(),
               error.plot = "pointrange", legand = NULL, size = NULL, width = 0.7, notch = FALSE, outlier.shape = 20, facet.by = NULL,
               panel.labs = NULL, short.panel.labs = TRUE,bxp.errorbar = FALSE, bxp.errorbar.width = 0.4, ggtheme = theme_pubr())+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + stat_compare_means()+
  theme(legend.text = element_text(size = 10, colour = "black", face = "italic"), legend.text.align = 0)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 10))+
  theme(axis.text = element_text(colour = "black", size = 10))+
  theme(axis.line = element_line())+
  theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = NULL, size = 1))+
  theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
  theme(legend.justification = "top")+
  theme(legend.position = "right")+
  theme(legend.key = element_rect(fill = "white"))+
  theme(legend.title = element_text(face = NULL, size = 10))+theme(panel.background = element_blank(), axis.text = element_blank())+
  theme(axis.text = element_text(colour = "black", size = 10)+
          theme(axis.line = element_line())+
          theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = "grey"))+
          theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
          theme(axis.title.y = element_text(size = 10, face = "plain", angle = 90))+
          theme(axis.title.x = element_text(size = 10, angle = 0)))+
  stat_compare_means()
P + aes(x = fct_inorder(Treatment)) + theme(legend.position = "none") + xlab("Turbo DNase Treatment") + ylab("% Fragments Covered")+labs(tag = "B", plot.tag.position = c(0.2, -0.1))

# making a boxplot of method (centrifugal or direct) against percentage fragment covered from the host reads
P <- ggboxplot(host_depletion, "method","percentage_fragments_covered",
               color = "method", palette =c("#508578", "#C84248"),
               add = "jitter", linetype = "solid", Family = "Palatino Linotype", add.params = list(),
               error.plot = "pointrange", legand = NULL, size = NULL, width = 0.7, notch = FALSE, outlier.shape = 20, facet.by = NULL,
               panel.labs = NULL, short.panel.labs = TRUE,bxp.errorbar = FALSE, bxp.errorbar.width = 0.4, ggtheme = theme_pubr())+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + stat_compare_means()+
  theme(legend.text = element_text(size = 10, colour = "black", face = "italic"), legend.text.align = 0)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 10))+
  theme(axis.text = element_text(colour = "black", size = 10))+
  theme(axis.line = element_line())+
  theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = NULL, size = 1))+
  theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
  theme(legend.justification = "top")+
  theme(legend.position = "right")+
  theme(legend.key = element_rect(fill = "white"))+
  theme(legend.title = element_text(face = NULL, size = 10))+theme(panel.background = element_blank(), axis.text = element_blank())+
  theme(axis.text = element_text(colour = "black", size = 10)+
          theme(axis.line = element_line())+
          theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = "grey"))+
          theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
          theme(axis.title.y = element_text(size = 10, face = "plain", angle = 90))+
          theme(axis.title.x = element_text(size = 10, angle = 0)))+
  stat_compare_means()+facet_grid(~Treatment)
P + aes(x = fct_inorder(method)) + theme(legend.position = "none") + xlab("Method") + ylab("% Fragments Covered")+labs(tag = "A", plot.tag.position = c(0.2, -0.1))

# read orthopneumovirus data into R
RSV_reads <- read.csv("orthopneumovirus_centrifugal.csv", sep = ',', header = TRUE)
RSV_reads1 <- read.csv("orthopneumovirus_direct.csv", sep = ',', header = TRUE)

# making a script of method (centrifugal or direct) against percentage fragments covered from the RSV reads.
P <- ggboxplot(RSV_reads, "method","percentage_fragments_covered",
               color = "method", palette =c("#508578", "#C84248"),
               add = "jitter", linetype = "solid", Family = "Palatino Linotype", add.params = list(),
               error.plot = "pointrange", legand = NULL, size = NULL, width = 0.7, notch = FALSE, outlier.shape = 20, facet.by = NULL,
               panel.labs = NULL, short.panel.labs = TRUE,bxp.errorbar = FALSE, bxp.errorbar.width = 0.4, ggtheme = theme_pubr())+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + stat_compare_means()+
  theme(legend.text = element_text(size = 10, colour = "black", face = "italic"), legend.text.align = 0)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 10))+
  theme(axis.text = element_text(colour = "black", size = 10))+
  theme(axis.line = element_line())+
  theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = NULL, size = 1))+
  theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
  theme(legend.justification = "top")+
  theme(legend.position = "right")+
  theme(legend.key = element_rect(fill = "white"))+
  theme(legend.title = element_text(face = NULL, size = 10))+theme(panel.background = element_blank(), axis.text = element_blank())+
  theme(axis.text = element_text(colour = "black", size = 10)+
          theme(axis.line = element_line())+
          theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = "grey"))+
          theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
          theme(axis.title.y = element_text(size = 10, face = "plain", angle = 90))+
          theme(axis.title.x = element_text(size = 10, angle = 0)))+
  stat_compare_means()+facet_grid(~Treatment)
P + aes(x = fct_inorder(method)) + theme(legend.position = "none") + xlab("Method") + ylab("% Fragments Covered")+labs(tag = "B", plot.tag.position = c(0.2, -0.1))


# making a boxplot of treatment against percentage fragments covered
P <- ggboxplot(RSV_reads, "Treatment","percentage_fragments_covered",
               color = "Treatment", palette =c("#508578", "#C84248"),
               add = "jitter", linetype = "solid", Family = "Palatino Linotype", add.params = list(),
               error.plot = "pointrange", legand = NULL, size = NULL, width = 0.7, notch = FALSE, outlier.shape = 20, facet.by = NULL,
               panel.labs = NULL, short.panel.labs = TRUE,bxp.errorbar = FALSE, bxp.errorbar.width = 0.4, ggtheme = theme_pubr())+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) + stat_compare_means()+
  theme(legend.text = element_text(size = 10, colour = "black", face = "italic"), legend.text.align = 0)+
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, size = 10))+
  theme(axis.text = element_text(colour = "black", size = 10))+
  theme(axis.line = element_line())+
  theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = NULL, size = 1))+
  theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
  theme(legend.justification = "top")+
  theme(legend.position = "right")+
  theme(legend.key = element_rect(fill = "white"))+
  theme(legend.title = element_text(face = NULL, size = 10))+theme(panel.background = element_blank(), axis.text = element_blank())+
  theme(axis.text = element_text(colour = "black", size = 10)+
          theme(axis.line = element_line())+
          theme(panel.background = element_rect(fill = "white"),plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.background = element_rect(colour = "grey"))+
          theme(axis.ticks.length.y = unit(.25, "cm"), axis.ticks.length.x = unit(.25, "cm"), axis.text.x = element_text(margin = margin(t = .3, unit = "cm")))+
          theme(axis.title.y = element_text(size = 10, face = "plain", angle = 90))+
          theme(axis.title.x = element_text(size = 10, angle = 0)))+
  stat_compare_means()
P + aes(x = fct_inorder(Treatment)) + theme(legend.position = "none") + xlab("Turbo DNase Treatment") + ylab("% Fragments Covered")+labs(tag = "A", plot.tag.position = c(0.2, -0.1))




