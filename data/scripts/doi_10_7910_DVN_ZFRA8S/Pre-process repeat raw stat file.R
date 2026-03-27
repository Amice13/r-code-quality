
rm(list=ls())
#Preload and process the original stat files for each stage
repeat.type.family <- read.table(file="processed/repeat_class_family", sep="\t")
#Sort the repeats based on their classes
repeat.type.family <- repeat.type.family[order(repeat.type.family[ ,2]), ]
#some TEs have duplicated names, although they belong to different repeat types. 3 TEs were removed.
repeat.type.family <- repeat.type.family[!duplicated(repeat.type.family[[1]]), ]
rownames(repeat.type.family) <- repeat.type.family$V1

##NOTE:the saved output files in all types of samples recorded ratio of CAST allele!! 

#MC cross -----------------------------------------------------------------------------------------------------
#load zygote raw stat file
auto.RawData <- read.table(file="raw_counts/MC.zygote_stat.txt", sep="\t", fill=T)

auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")
#remove sample 3 and 4, because of low quality
auto.RawData <- auto.RawData[which(auto.RawData$sample != 3 & auto.RawData$sample != 4), ]
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_zygote_R_output.txt", sep ="\t")

#Load early2cell raw stat files
female.sample <- c(1,4,5,99)
male.sample <- c(2,3,6)
auto.RawData <- read.table(file="raw_counts/MC.early2cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]

auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_early2cell_R_output.txt", sep ="\t")


#load late2cell raw stat file
female.sample <- c(3,4,5,7,9,99)
male.sample <- c(2,6,8,10,11)
auto.RawData <- read.table(file="raw_counts/MC.late2cell_stat.txt", sep="\t", fill=T)

# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]

auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_late2cell_R_output.txt", sep ="\t")


#Load 4cell raw Stat
female.sample <- c(4,7,12,15,16,17,99)
male.sample <- c(1,2,3,5,8,9,10,11,13,14,18)
auto.RawData <- read.table(file="raw_counts/MC.4cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_4cell_R_output.txt", sep ="\t")

#Load 8cell raw Stat
female.sample <- c(6,10,12,15,19,20,99)
male.sample <- c(7,8,9,11,13,14,16,17,18)
auto.RawData <- read.table(file="raw_counts/MC.8cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_8cell_R_output.txt", sep ="\t")


#Load 16cell raw Stat
female.sample <- c(2,3,5,6,7,99)
male.sample <- c(1,4,8)
auto.RawData <- read.table(file="raw_counts/MC.16cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_16cell_R_output.txt", sep ="\t")


#Load earlyBlast raw Stat
female.sample <- c(1,4,5,8,10,99)
male.sample <- c(2,3,6,7,9,11)
auto.RawData <- read.table(file="raw_counts/MC.earlyBlast_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","paternal","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_earlyBlast_R_output.txt", sep ="\t")

#ES cell -----------------------------------------------------------------------------
### 1. IF analyzed using REF and CAST genome:
#Load Day0 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day0_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day0_R_output.txt", sep ="\t")



#Load Day1 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day1_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day1_R_output.txt", sep ="\t")



#Load Day2 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day2_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day2_R_output.txt", sep ="\t")



#Load Day3 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day3_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day3_R_output.txt", sep ="\t")



#Load Day4 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day4_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day4_R_output.txt", sep ="\t")



#Load Day6 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day6_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day6_R_output.txt", sep ="\t")



#Load Day8 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day8_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day8_R_output.txt", sep ="\t")



#Load Day10 raw Stat
female.sample <- c(1,2,99)
auto.RawData <- read.table(file="raw_counts/ES.day10_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_day10_R_output.txt", sep ="\t")




#CM cross -----------------------------------------------------------------------
#load late2cell raw stat file
female.sample <- c(1,2,3,5,8,99)
male.sample <- c(4,6,7,9,10)
auto.RawData <- read.table(file="raw_counts/CM.late2cell_stat.txt", sep="\t", fill=T)

# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]

auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_CM_late2cell_R_output.txt", sep ="\t")


#Load 4cell raw Stat
female.sample <- c(1,2,3,4,5,99)
male.sample <- c(6,7,9)
auto.RawData <- read.table(file="raw_counts/CM.4cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_CM_4cell_R_output.txt", sep ="\t")

#Load 8cell raw Stat
female.sample <- c(1,2,5,6,7,8,9,10,11,99)
male.sample <- c(3,4)
auto.RawData <- read.table(file="raw_counts/CM.8cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_CM_8cell_R_output.txt", sep ="\t")


#Load 16cell raw Stat
female.sample <- c(1,2,6,10,13,14,15,99)
male.sample <- c(3,4,5,7,8,11,12)
auto.RawData <- read.table(file="raw_counts/CM.16cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_CM_16cell_R_output.txt", sep ="\t")


#Load earlyBlast raw Stat
female.sample <- c(3,6,11,13,15,99)
male.sample <- c(1,2,4,5,7,9,12,14)
auto.RawData <- read.table(file="raw_counts/CM.earlyBlast_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_CM_earlyBlast_R_output.txt", sep ="\t")




#upon reviewers request, we repeated the alignment against de novo assembled 129 (MUS) and CAST genome. Due to assembly of MUS is not as good as mm10. Data from this 
#alignment may not be as good as the original one. 



#Xist KO (Option1 (original): algined using ref (mm10) and de novo CAST genome)------------------------------------------------------------------------------

#Load KO-late2cell raw Stat
female.sample <- c(7,13,17,18,19,22,99)
auto.RawData <- read.table(file="raw_counts/KO.late2cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_KO_late2cell_R_output.txt", sep ="\t")


#Load KO-4cell raw Stat
female.sample <- c(4,7,8,9,11,13,99)
auto.RawData <- read.table(file="raw_counts/KO.4cell_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_KO_4cell_R_output.txt", sep ="\t")


#Load KO-8cell raw Stat
female.sample <- c(1,17,19,22,23,25,99)
auto.RawData <- read.table(file="raw_counts/KO.8cell_stat.txt", sep="\t", fill=T, head=F)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_KO_8cell_R_output.txt", sep ="\t")


#Load KO-eB raw Stat
female.sample <- c(5,9,11,16,17,18,19,99)
auto.RawData <- read.table(file="raw_counts/KO.earlyBlast_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 7] - auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "ref") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "ref"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","ref+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
#write.table(auto.RawData, file="processed/rep_KO_earlyBlast_R_output.txt", sep ="\t")



#Fibroblast -----------------------------------------------------------------------------
### analyzed using MUS and CAST genome:
#Loadraw Stat
female.sample <- c(1,2)
auto.RawData <- read.table(file="raw_counts/Fibroblast_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 7] - auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]

auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","ref","p-value","mus+cas","CAST","skew.ratio","rep_type")

#filter out neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_Fibroblast_R_output.txt", sep ="\t")

#---------------------------------------------------------------------------------

#Xist KO (Option2 (Rev2): re-algined using de novo 129 (MUS) and de novo CAST genome)------------------------------------------------------------------------------

#Load KO-late2cell raw Stat
female.sample <- c(7,13,17,18,19,22)
auto.RawData <- read.table(file="raw_counts/KO.late2cell_MUS.CAST_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 7] - auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","mus","p-value","mus+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_KO_late2cell_MUS.CAST_R_output.txt", sep ="\t")


#Load KO-4cell raw Stat
female.sample <- c(4,7,8,9,11,13)
auto.RawData <- read.table(file="raw_counts/KO.4cell_MUS.CAST_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 7] - auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","mus","p-value","mus+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_KO_4cell_MUS.CAST_R_output.txt", sep ="\t")


#Load KO-8cell raw Stat
female.sample <- c(1,17,19,22,23,25)
auto.RawData <- read.table(file="raw_counts/KO.8cell_MUS.CAST_stat.txt", sep="\t", fill=T, head=F)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 7] - auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","mus","p-value","mus+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_KO_8cell_MUS.CAST_R_output.txt", sep ="\t")


#Load KO-eB raw Stat
female.sample <- c(5,9,11,16,17,18,19)
auto.RawData <- read.table(file="raw_counts/KO.earlyBlast_MUS.CAST_stat.txt", sep="\t", fill=T)
# Collect female samples only
auto.RawData <- auto.RawData[(auto.RawData[ ,1] %in% female.sample), ]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 9] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 7] - auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
auto.RawData[which(auto.RawData[ ,3] == "cas") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "cas"), 7] - auto.RawData[which(auto.RawData[ ,3] == "cas"), 5]
auto.RawData[which(auto.RawData[ ,3] == "mus") , 10] <- auto.RawData[which(auto.RawData[ ,3] == "mus"), 5]
#always calculate CAST ratio, which is column 9. If use ref, choose the 10th column.
auto.RawData[ ,11] <- auto.RawData[ ,9]
auto.RawData[ ,12] <- auto.RawData[ ,11] / auto.RawData[ ,7]
auto.RawData <- auto.RawData[ , c(1,2,3,4,9,10,8,7,11,12)]
auto.RawData[ ,11] <- repeat.type.family[(match(auto.RawData[, 4], repeat.type.family[ ,1])), 2]
colnames(auto.RawData) <- c("sample","Chr","allele","rep_name","cas","mus","p-value","mus+cas","CAST","skew.ratio","rep_type")
#filterout neutral reads from this table
auto.RawData <- auto.RawData[which(auto.RawData[ ,"allele"] != "neutral"), ]
#filter out reps whose total allelic reads are less than 5
auto.RawData <- auto.RawData[which(auto.RawData[ ,8] >= 5), ]
write.table(auto.RawData, file="processed/rep_KO_earlyBlast_MUS.CAST_R_output.txt", sep ="\t")



