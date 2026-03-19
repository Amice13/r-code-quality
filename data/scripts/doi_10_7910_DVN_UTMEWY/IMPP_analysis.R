library(plyr)
library(ggplot2)
library(cowplot)
library(seqLogo)

# Intra-motif positional preference analysis

#setwd("C:/Users/OscarJavier/Dropbox/DemethylationKinetics_Scripts")
setwd("~/Dropbox/DemethylationKinetics_Scripts/Oscar/Plot")

data_raw <- read.delim("SlopeSummary_fixed.txt",stringsAsFactors = FALSE)

data <- data_raw[c(1,2,3,4,5,6,7,8,9,10)]

data$chr1 <- substring(data$Motif,1,1)
data$chr2 <- substring(data$Motif,2,2)
data$chr5 <- substring(data$Motif,5,5)
data$chr6 <- substring(data$Motif,6,6)

#######################################################################
##### Function #####

plot_degenerated_kmer <- function(data,x_axis,y_axis,position,y_labels,plot_name){
  plot = ggplot(data=data, aes(x=x_axis,y=y_axis,group=position)) +
    geom_line(aes(color=position))+
    geom_point(aes(color=position))+
    scale_color_manual(values=c("#526EFF","#32C12C","#D40C00","#FFCD00")) + 
    theme_classic() +
    ggtitle(as.character(plot_name)) +
    theme(plot.title = element_text(size=30,hjust = 0.5),
          axis.title.x=element_blank(),axis.title.y = element_text(size = 24),
          axis.text.x = element_text(size=18,angle = 60,hjust = 1),
          axis.text.y = element_text(size=18),
          legend.title = element_blank(), legend.text = element_text(size = 24)) +
    ylab("Demethylation velocity") +
    
    ylim(-1.5,0.5) +
    scale_x_discrete(limits=1:64, labels = y_labels[,1])
  return(plot)
}


#######################################################################
##### Position -1 Ordered #####
data$group <- paste(data$chr1,"NCG",data$chr5,data$chr6,sep = "")
group_names <- unique(data$group)
group_code <- seq(1:length(group_names))

groups_mean <- aggregate(data[2:10],list(data$group),mean)
groups_ordered_all_72 <- groups_mean[order(-groups_mean$ALL_72),]
groups_ordered_CGI_72 <- groups_mean[order(-groups_mean$CGI_72),]
groups_ordered_non_CGI_72 <- groups_mean[order(-groups_mean$non_CGI_72),]
groups_ordered_all_Dec <- groups_mean[order(-groups_mean$ALL_Dec),]
groups_ordered_CGI_Dec <- groups_mean[order(-groups_mean$CGI_Dec),]
groups_ordered_non_CGI_Dec <- groups_mean[order(-groups_mean$non_CGI_Dec),]

data$group_code_all_72 <- mapvalues(data$group, from=groups_ordered_all_72$Group.1, to=group_code)
data$group_code_all_72 <- as.integer(data$group_code_all_72)

data$group_code_CGI_72 <- mapvalues(data$group, from=groups_ordered_CGI_72$Group.1, to=group_code)
data$group_code_CGI_72 <- as.integer(data$group_code_CGI_72)

data$group_code_non_CGI_72 <- mapvalues(data$group, from=groups_ordered_non_CGI_72$Group.1, to=group_code)
data$group_code_non_CGI_72 <- as.integer(data$group_code_non_CGI_72)

data$group_code_all_Dec  <- mapvalues(data$group, from=groups_ordered_all_Dec$Group.1, to=group_code)
data$group_code_all_Dec <- as.integer(data$group_code_all_Dec)

data$group_code_CGI_Dec  <- mapvalues(data$group, from=groups_ordered_CGI_Dec$Group.1, to=group_code)
data$group_code_CGI_Dec <- as.integer(data$group_code_CGI_Dec)

data$group_code_non_CGI_Dec <- mapvalues(data$group, from=groups_ordered_non_CGI_Dec$Group.1, to=group_code)
data$group_code_non_CGI_Dec <- as.integer(data$group_code_non_CGI_Dec)

plot_1 <- plot_degenerated_kmer(data,data$group_code_all_72,data$ALL_72,data$chr2,groups_ordered_all_72,"All ordered 72h - position -1")
plot_1

plot_2 <- plot_degenerated_kmer(data,data$group_code_CGI_72,data$CGI_72,data$chr2,groups_ordered_CGI_72,"CGI ordered 72h - position -1")
plot_2

plot_3 <- plot_degenerated_kmer(data,data$group_code_non_CGI_72,data$non_CGI_72,data$chr2,groups_ordered_non_CGI_72,"Non CGI / position -1")
plot_3

plot_4 <- plot_degenerate_kmer(data,data$group_code_all_Dec,data$ALL_Dec,data$chr2,groups_ordered_all_Dec,"All ordered Dec - position -1")
plot_4

plot_5 <- plot_degenerate_kmer(data,data$group_code_CGI_Dec,data$CGI_Dec,data$chr2,groups_ordered_CGI_Dec,"CGI ordered Dec - position -1")
plot_5

plot_6 <- plot_degenerate_kmer(data,data$group_code_non_CGI_Dec,data$non_CGI_Dec,data$chr2,groups_ordered_non_CGI_Dec,"Non CGI ordered Dec - position -1")
plot_6

#######################################################################
##### Position +1 Ordered #####
data$group <- paste(data$chr1,data$chr2,"CGN",data$chr6,sep = "")
group_names <- unique(data$group)
group_code <- seq(1:length(group_names))

groups_mean <- aggregate(data[2:10],list(data$group),mean)
groups_ordered_all_72 <- groups_mean[order(-groups_mean$ALL_72),]
groups_ordered_CGI_72 <- groups_mean[order(-groups_mean$CGI_72),]
groups_ordered_non_CGI_72 <- groups_mean[order(-groups_mean$non_CGI_72),]
groups_ordered_all_Dec <- groups_mean[order(-groups_mean$ALL_Dec),]
groups_ordered_CGI_Dec <- groups_mean[order(-groups_mean$CGI_Dec),]
groups_ordered_non_CGI_Dec <- groups_mean[order(-groups_mean$non_CGI_Dec),]

data$group_code_all_72 <- mapvalues(data$group, from=groups_ordered_all_72$Group.1, to=group_code)
data$group_code_all_72 <- as.integer(data$group_code_all_72)

data$group_code_CGI_72 <- mapvalues(data$group, from=groups_ordered_CGI_72$Group.1, to=group_code)
data$group_code_CGI_72 <- as.integer(data$group_code_CGI_72)

data$group_code_non_CGI_72 <- mapvalues(data$group, from=groups_ordered_non_CGI_72$Group.1, to=group_code)
data$group_code_non_CGI_72 <- as.integer(data$group_code_non_CGI_72)

data$group_code_all_Dec  <- mapvalues(data$group, from=groups_ordered_all_Dec$Group.1, to=group_code)
data$group_code_all_Dec <- as.integer(data$group_code_all_Dec)

data$group_code_CGI_Dec  <- mapvalues(data$group, from=groups_ordered_CGI_Dec$Group.1, to=group_code)
data$group_code_CGI_Dec <- as.integer(data$group_code_CGI_Dec)

data$group_code_non_CGI_Dec <- mapvalues(data$group, from=groups_ordered_non_CGI_Dec$Group.1, to=group_code)
data$group_code_non_CGI_Dec <- as.integer(data$group_code_non_CGI_Dec)

plot_1 <- plot_degenerated_kmer(data,data$group_code_all_72,data$ALL_72,data$chr5,groups_ordered_all_72,"All ordered 72h - position +1")
plot_1

plot_2 <- plot_degenerated_kmer(data,data$group_code_CGI_72,data$CGI_72,data$chr5,groups_ordered_CGI_72,"CGI ordered 72h - position +1")
plot_2

plot_3 <- plot_degenerated_kmer(data,data$group_code_non_CGI_72,data$non_CGI_72,data$chr5,groups_ordered_non_CGI_72,"Non CGI / position +1")
plot_3

plot_4 <- plot_degenerated_kmer(data,data$group_code_all_Dec,data$ALL_Dec,data$chr5,groups_ordered_all_Dec,"All ordered Dec - position +1")
plot_4

plot_5 <- plot_degenerated_kmer(data,data$group_code_CGI_Dec,data$CGI_Dec,data$chr5,groups_ordered_CGI_Dec,"CGI ordered Dec - position +1")
plot_5

plot_6 <- plot_degenerated_kmer(data,data$group_code_non_CGI_Dec,data$non_CGI_Dec,data$chr5,groups_ordered_non_CGI_Dec,"Non CGI ordered Dec - position +1")
plot_6

#######################################################################
##### Position -2 Ordered #####
data$group <- paste("N",data$chr2,"CG",data$chr5,data$chr6,sep = "")
group_names <- unique(data$group)
group_code <- seq(1:length(group_names))

groups_mean <- aggregate(data[2:10],list(data$group),mean)
groups_ordered_all_72 <- groups_mean[order(-groups_mean$ALL_72),]
groups_ordered_CGI_72 <- groups_mean[order(-groups_mean$CGI_72),]
groups_ordered_non_CGI_72 <- groups_mean[order(-groups_mean$non_CGI_72),]
groups_ordered_all_Dec <- groups_mean[order(-groups_mean$ALL_Dec),]
groups_ordered_CGI_Dec <- groups_mean[order(-groups_mean$CGI_Dec),]
groups_ordered_non_CGI_Dec <- groups_mean[order(-groups_mean$non_CGI_Dec),]

data$group_code_all_72 <- mapvalues(data$group, from=groups_ordered_all_72$Group.1, to=group_code)
data$group_code_all_72 <- as.integer(data$group_code_all_72)

data$group_code_CGI_72 <- mapvalues(data$group, from=groups_ordered_CGI_72$Group.1, to=group_code)
data$group_code_CGI_72 <- as.integer(data$group_code_CGI_72)

data$group_code_non_CGI_72 <- mapvalues(data$group, from=groups_ordered_non_CGI_72$Group.1, to=group_code)
data$group_code_non_CGI_72 <- as.integer(data$group_code_non_CGI_72)

data$group_code_all_Dec  <- mapvalues(data$group, from=groups_ordered_all_Dec$Group.1, to=group_code)
data$group_code_all_Dec <- as.integer(data$group_code_all_Dec)

data$group_code_CGI_Dec  <- mapvalues(data$group, from=groups_ordered_CGI_Dec$Group.1, to=group_code)
data$group_code_CGI_Dec <- as.integer(data$group_code_CGI_Dec)

data$group_code_non_CGI_Dec <- mapvalues(data$group, from=groups_ordered_non_CGI_Dec$Group.1, to=group_code)
data$group_code_non_CGI_Dec <- as.integer(data$group_code_non_CGI_Dec)

plot_1 <- plot_degenerated_kmer(data,data$group_code_all_72,data$ALL_72,data$chr1,groups_ordered_all_72,"All ordered 72h - position -2")
plot_1

plot_2 <- plot_degenerated_kmer(data,data$group_code_CGI_72,data$CGI_72,data$chr1,groups_ordered_CGI_72,"CGI ordered 72h - position -2")
plot_2

plot_3 <- plot_degenerated_kmer(data,data$group_code_non_CGI_72,data$non_CGI_72,data$chr1,groups_ordered_non_CGI_72,"Non CGI / position -2")
plot_3

plot_4 <- plot_degenerated_kmer(data,data$group_code_all_Dec,data$ALL_Dec,data$chr1,groups_ordered_all_Dec,"All ordered Dec - position -2")
plot_4

plot_5 <- plot_degenerated_kmer(data,data$group_code_CGI_Dec,data$CGI_Dec,data$chr1,groups_ordered_CGI_Dec,"CGI ordered Dec - position -2")
plot_5

plot_6 <- plot_degenerated_kmer(data,data$group_code_non_CGI_Dec,data$non_CGI_Dec,data$chr1,groups_ordered_non_CGI_Dec,"Non CGI ordered Dec - position -2")
plot_6

#######################################################################
##### Position +2 Ordered #####
data$group <- paste(data$chr1,data$chr2,"CG",data$chr5,"N",sep = "")
group_names <- unique(data$group)
group_code <- seq(1:length(group_names))

groups_mean <- aggregate(data[2:10],list(data$group),mean)
groups_ordered_all_72 <- groups_mean[order(-groups_mean$ALL_72),]
groups_ordered_CGI_72 <- groups_mean[order(-groups_mean$CGI_72),]
groups_ordered_non_CGI_72 <- groups_mean[order(-groups_mean$non_CGI_72),]
groups_ordered_all_Dec <- groups_mean[order(-groups_mean$ALL_Dec),]
groups_ordered_CGI_Dec <- groups_mean[order(-groups_mean$CGI_Dec),]
groups_ordered_non_CGI_Dec <- groups_mean[order(-groups_mean$non_CGI_Dec),]

data$group_code_all_72 <- mapvalues(data$group, from=groups_ordered_all_72$Group.1, to=group_code)
data$group_code_all_72 <- as.integer(data$group_code_all_72)

data$group_code_CGI_72 <- mapvalues(data$group, from=groups_ordered_CGI_72$Group.1, to=group_code)
data$group_code_CGI_72 <- as.integer(data$group_code_CGI_72)

data$group_code_non_CGI_72 <- mapvalues(data$group, from=groups_ordered_non_CGI_72$Group.1, to=group_code)
data$group_code_non_CGI_72 <- as.integer(data$group_code_non_CGI_72)

data$group_code_all_Dec  <- mapvalues(data$group, from=groups_ordered_all_Dec$Group.1, to=group_code)
data$group_code_all_Dec <- as.integer(data$group_code_all_Dec)

data$group_code_CGI_Dec  <- mapvalues(data$group, from=groups_ordered_CGI_Dec$Group.1, to=group_code)
data$group_code_CGI_Dec <- as.integer(data$group_code_CGI_Dec)

data$group_code_non_CGI_Dec <- mapvalues(data$group, from=groups_ordered_non_CGI_Dec$Group.1, to=group_code)
data$group_code_non_CGI_Dec <- as.integer(data$group_code_non_CGI_Dec)


plot_1 <- plot_degenerated_kmer(data,data$group_code_all_72,data$ALL_72,data$chr6,groups_ordered_all_72,"All ordered 72h - position +2")
plot_1

plot_2 <- plot_degenerated_kmer(data,data$group_code_CGI_72,data$CGI_72,data$chr6,groups_ordered_CGI_72,"CGI ordered 72h - position +2")
plot_2

plot_3 <- plot_degenerated_kmer(data,data$group_code_non_CGI_72,data$non_CGI_72,data$chr6,groups_ordered_non_CGI_72,"Non CGI / position +2")
plot_3

plot_4 <- plot_degenerate_kmer(data,data$group_code_all_Dec,data$ALL_Dec,data$chr6,groups_ordered_all_Dec,"All ordered Dec - position +2")
plot_4

plot_5 <- plot_degenerate_kmer(data,data$group_code_CGI_Dec,data$CGI_Dec,data$chr6,groups_ordered_CGI_Dec,"CGI ordered Dec - position +2")
plot_5

plot_6 <- plot_degenerate_kmer(data,data$group_code_non_CGI_Dec,data$non_CGI_Dec,data$chr6,groups_ordered_non_CGI_Dec,"Non CGI ordered Dec - position +2")
plot_6

###########################################################
##########
# Logo #

# Functions
count_table <- function(table,column){
  A=sum(table[,column]=="A")
  C=sum(table[,column]=="C")
  G=sum(table[,column]=="G")
  T=sum(table[,column]=="T")
  return(rbind(A,C,G,T))
}

matrix_max <- function(data_raw,column,position){
  data_temp <- data_raw
  names(data_temp)[names(data_temp) == column] <- "dataset"
  data_max <- data.frame(data_temp %>% group_by(group) %>% slice(which.min(dataset)))
  data_max_sum <- data.frame("Nucleotide"=c("A","C","G","T"),"Values"=count_table(data_max,position))
  return(data_max_sum)
}

matrix_min <- function(data_raw,column,position){
  data_temp <- data_raw
  names(data_temp)[names(data_temp) == column] <- "dataset"
  data_min <- data.frame(data_temp %>% group_by(group) %>% slice(which.max(dataset)))
  data_min_sum <- data.frame("Nucleotide"=c("A","C","G","T"),"Values"=count_table(data_min,position))
  return(data_min_sum)
  
}

# SeqLogo Max 72h TET
### Minus 1 ###
data$group <- paste(data$chr1,"NCG",data$chr5,data$chr6,sep = "")
minus_1 <- matrix_max(data,"ALL_72","chr2")

### Minus 2 ###
data$group <- paste("N",data$chr2,"CG",data$chr5,data$chr6,sep = "")
minus_2 <- matrix_max(data,"ALL_72","chr1")

### Plus 1 ###
data$group <- paste(data$chr1,data$chr2,"CGN",data$chr6,sep = "")
plus_1 <- matrix_max(data,"ALL_72","chr5")

### Plus 2 ###
data$group <- paste(data$chr1,data$chr2,"CG",data$chr5,"N",sep = "")
plus_2 <- matrix_max(data,"ALL_72","chr6")

# Logo plot
matrix_max_data <- data.frame("1"=minus_2$Values/sum(minus_2$Values),
                              "2"=minus_1$Values/sum(minus_1$Values),
                              "3"=c(0,1,0,0),
                              "4"=c(0,0,1,0),
                              "5"=plus_1$Values/sum(plus_1$Values),
                              "6"=plus_2$Values/sum(plus_2$Values))

row.names(matrix_max_data) <- c("A","C","G","T")
matrix_max_data
seqLogo(matrix_max_data,ic.scale=FALSE)

# SeqLogo Max 72h TET
### Minus 1 ###
data$group <- paste(data$chr1,"NCG",data$chr5,data$chr6,sep = "")
minus_1 <- matrix_max(data,"non_CGI_72","chr2")

### Minus 2 ###
data$group <- paste("N",data$chr2,"CG",data$chr5,data$chr6,sep = "")
minus_2 <- matrix_max(data,"non_CGI_72","chr1")

### Plus 1 ###
data$group <- paste(data$chr1,data$chr2,"CGN",data$chr6,sep = "")
plus_1 <- matrix_max(data,"non_CGI_72","chr5")

### Plus 2 ###
data$group <- paste(data$chr1,data$chr2,"CG",data$chr5,"N",sep = "")
plus_2 <- matrix_max(data,"non_CGI_72","chr6")

# Logo plot
matrix_max_data <- data.frame("1"=minus_2$Values/sum(minus_2$Values),
                              "2"=minus_1$Values/sum(minus_1$Values),
                              "3"=c(0,1,0,0),
                              "4"=c(0,0,1,0),
                              "5"=plus_1$Values/sum(plus_1$Values),
                              "6"=plus_2$Values/sum(plus_2$Values))

row.names(matrix_max_data) <- c("A","C","G","T")
matrix_max_data
seqLogo(matrix_max_data,ic.scale=FALSE)

# SeqLogo min 72h TET
### Minus 1 ###
data$group <- paste(data$chr1,"NCG",data$chr5,data$chr6,sep = "")
minus_1 <- matrix_min(data,"non_CGI_72","chr2")

### Minus 2 ###
data$group <- paste("N",data$chr2,"CG",data$chr5,data$chr6,sep = "")
minus_2 <- matrix_min(data,"non_CGI_72","chr1")

### Plus 1 ###
data$group <- paste(data$chr1,data$chr2,"CGN",data$chr6,sep = "")
plus_1 <- matrix_min(data,"non_CGI_72","chr5")

### Plus 2 ###
data$group <- paste(data$chr1,data$chr2,"CG",data$chr5,"N",sep = "")
plus_2 <- matrix_min(data,"non_CGI_72","chr6")

# Logo plot
matrix_min_data <- data.frame("1"=minus_2$Values/sum(minus_2$Values),
                              "2"=minus_1$Values/sum(minus_1$Values),
                              "3"=c(0,1,0,0),
                              "4"=c(0,0,1,0),
                              "5"=plus_1$Values/sum(plus_1$Values),
                              "6"=plus_2$Values/sum(plus_2$Values))

row.names(matrix_min_data) <- c("A","C","G","T")
matrix_min_data
seqLogo(matrix_min_data,ic.scale=FALSE)

######## Data Dec ############
# SeqLogo Max

### Minus 1 ###
data$group <- paste(data$chr1,"NCG",data$chr5,data$chr6,sep = "")
minus_1 <- matrix_max(data,"ALL_Dec","chr2")

### Minus 2 ###
data$group <- paste("N",data$chr2,"CG",data$chr5,data$chr6,sep = "")
minus_2 <- matrix_max(data,"ALL_Dec","chr1")

### Plus 1 ###
data$group <- paste(data$chr1,data$chr2,"CGN",data$chr6,sep = "")
plus_1 <- matrix_max(data,"ALL_Dec","chr5")

### Plus 2 ###
data$group <- paste(data$chr1,data$chr2,"CG",data$chr5,"N",sep = "")
plus_2 <- matrix_max(data,"ALL_Dec","chr6")

# Logo plot
matrix_max_data <- data.frame("1"=minus_2$Values/sum(minus_2$Values),
                              "2"=minus_1$Values/sum(minus_1$Values),
                              "3"=c(0,1,0,0),
                              "4"=c(0,0,1,0),
                              "5"=plus_1$Values/sum(plus_1$Values),
                              "6"=plus_2$Values/sum(plus_2$Values))

row.names(matrix_max_data) <- c("A","C","G","T")
matrix_max_data
seqLogo(matrix_max_data,ic.scale=FALSE)

# SeqLogo min
### Minus 1 ###
data$group <- paste(data$chr1,"NCG",data$chr5,data$chr6,sep = "")
minus_1 <- matrix_min(data,"ALL_Dec","chr2")

### Minus 2 ###
data$group <- paste("N",data$chr2,"CG",data$chr5,data$chr6,sep = "")
minus_2 <- matrix_min(data,"ALL_Dec","chr1")

### Plus 1 ###
data$group <- paste(data$chr1,data$chr2,"CGN",data$chr6,sep = "")
plus_1 <- matrix_min(data,"ALL_Dec","chr5")

### Plus 2 ###
data$group <- paste(data$chr1,data$chr2,"CG",data$chr5,"N",sep = "")
plus_2 <- matrix_min(data,"ALL_Dec","chr6")

# Logo plot
matrix_min_data <- data.frame("1"=minus_2$Values/sum(minus_2$Values),
                              "2"=minus_1$Values/sum(minus_1$Values),
                              "3"=c(0,1,0,0),
                              "4"=c(0,0,1,0),
                              "5"=plus_1$Values/sum(plus_1$Values),
                              "6"=plus_2$Values/sum(plus_2$Values))

row.names(matrix_min_data) <- c("A","C","G","T")
matrix_min_data
seqLogo(matrix_max_data,ic.scale=FALSE)