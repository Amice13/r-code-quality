###############################################
#model used for daily accounted crimes
###############################################
#clear all variables in workspace
rm(list=ls()) 
#load libraries
#library(readxl) 
#library(forecast)
#library(fpp2)
#library(tidyverse)
#library(dplyr)
#library(ggpubr)
#library(car)
#library(ggsci)
#library(ggplot2)


###############################################
#read DB in a dataset named 'bd'

#csv from Macbook
bd<- read.csv(header = TRUE, sep = ",", "/Users/fred/Desktop/BH/2-Entry.csv")

#xlsx from Macbook
#bd<- read_xlsx("/Users/fred/Downloads/R Studio/ARIMA.xlsx")

#csv from Windows
#bd<- read.csv("C:/Users/Frederico/Desktop/BH/2-Entry.csv")


###############
#Check for Normal Distribution

#Visual Histogram with normal distribution curve
library(ggplot2)
library(tibble)
#get n 
n<-stat.desc(bd[,2:10], basic=TRUE,desc=TRUE,norm=TRUE)
n=n[1,]

#create basic histogram data just to get the frequencies count
hist<-hist(bd[,2],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,2]), max(bd[,2]), length.out=n[,1])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,2]), sd(bd[,2]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist1<-ggplot(bd, aes(x = bd[,2])) + 
  geom_histogram(bins = round(sqrt(n[,1])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Roubos por semana") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=20, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )

#create basic histogram data just to get the frequencies count
hist<-hist(bd[,3],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,3]), max(bd[,3]), length.out=n[,2])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,3]), sd(bd[,3]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist2<-ggplot(bd, aes(x = bd[,3])) + 
  geom_histogram(bins = round(sqrt(n[,2])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Furtos por semana") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=14, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )

#create basic histogram data just to get the frequencies count
hist<-hist(bd[,4],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,4]), max(bd[,4]), length.out=n[,3])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,4]), sd(bd[,4]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist3<-ggplot(bd, aes(x = bd[,4])) + 
  geom_histogram(bins = round(sqrt(n[,2])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Homicidios por semana") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=20, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )

#create basic histogram data just to get the frequencies count
hist<-hist(bd[,5],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,5]), max(bd[,5]), length.out=n[,4])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,5]), sd(bd[,5]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist4<-ggplot(bd, aes(x = bd[,5])) + 
  geom_histogram(bins = round(sqrt(n[,4])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Roubos a transeunte") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=20, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )

#create basic histogram data just to get the frequencies count
hist<-hist(bd[,6],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,6]), max(bd[,6]), length.out=n[,5])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,6]), sd(bd[,6]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist5<-ggplot(bd, aes(x = bd[,6])) + 
  geom_histogram(bins = round(sqrt(n[,5])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Roubos a veiculos") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=20, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )

#create basic histogram data just to get the frequencies count
hist<-hist(bd[,7],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,7]), max(bd[,7]), length.out=n[,6])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,7]), sd(bd[,7]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist6<-ggplot(bd, aes(x = bd[,7])) + 
  geom_histogram(bins = round(sqrt(n[,6])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Roubos a residencias") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=20, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )

#create basic histogram data just to get the frequencies count
hist<-hist(bd[,8],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,8]), max(bd[,8]), length.out=n[,7])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,8]), sd(bd[,8]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist7<-ggplot(bd, aes(x = bd[,8])) + 
  geom_histogram(bins = round(sqrt(n[,7])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Furtos a transeunte") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=20, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )


#create basic histogram data just to get the frequencies count
hist<-hist(bd[,9],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,9]), max(bd[,9]), length.out=n[,8])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,9]), sd(bd[,9]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist8<-ggplot(bd, aes(x = bd[,9])) + 
  geom_histogram(bins = round(sqrt(n[,8])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Furtos a veiculos") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=20, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )


#create basic histogram data just to get the frequencies count
hist<-hist(bd[,10],  plot = FALSE)

#normalize x values from min to max in variable
x <- seq(min(bd[,10]), max(bd[,10]), length.out=n[,9])
#normalize y values over normalized x using Z-scores
data <- with(bd, tibble(x = x, y = dnorm(x, mean(bd[,10]), sd(bd[,10]))))
#transform density y values in frequency y values
data[,2] <- data[,2]*(max(hist$counts)/max(data[,2]))*0.65

hist9<-ggplot(bd, aes(x = bd[,10])) + 
  geom_histogram(bins = round(sqrt(n[,9])), fill = "blue", color = "white", size = 0.2) + 
  geom_line(data = data, aes(x = x, y = y), color = "red", size = 1) + 
  ggtitle("") + xlab("Furtos a residencias") +
  ylab("Frequencia") + 
  theme(
    plot.title = element_text(color="navyblue", size=6, face="bold.italic", hjust=0.5),
    axis.title.x = element_text(color="blue", size=6, face="bold"),
    axis.title.y = element_text(color="#993333", size=6, face="bold")
  )

rm(n)+rm(hist)+rm(x)+rm(data)
hist<-ggarrange(hist1,hist2,hist3,hist4,hist5,hist6,hist7,hist8,hist9+rremove("x.text"),labels = c("", "", ""),ncol = 3, nrow = 3)

hist

rm(hist1,hist2,hist3,hist4,hist5,hist6,hist7,hist8,hist9)


###############################################
#Create combined box-plot charts
#mynames<-c("Roubo", "Furto", "Homicidio")
#boxplot(bd[,2:4], xlab = "Natureza",ylab = "Taxa semanal de crimes",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5, names = mynames)$out
#mynames<-c("Roubo a transeunte", "Roubo a Veiculos","Roubo a residencia")
#boxplot(bd[,5:7], xlab = "Natureza",ylab = "Taxa semanal de crimes",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5, names = mynames)$out
#mynames<-c("Furto a transeunte", "Furto a veiculo", "Furto a residencia")
#boxplot(bd[,8:10], xlab = "Natureza",ylab = "Taxa semanal de crimes",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5, names = mynames)$out




#Create individual boxplot charts
#outpch => outlier shape = 8 gives asterisk 
#outcex = 0.5)
#boxplot(bd[,2], xlab = "Roubo",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,3], xlab = "Furto",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,4], xlab = "Homicidio",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,5], xlab = "Roubo a transeunte",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,6], xlab = "Roubo a veiculo",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,7], xlab = "Roubo a residencia",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,8], xlab = "Furto a transeunte",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,9], xlab = "Furto a veiculo",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out
#boxplot(bd[,10], xlab = "Furto a residencia",whiskcol = "blue", outcol = "dark red", outpch = 8, outcex = 0.5)$out

#library(devtools)
#library(usethis)
library(ggplot2)
library(ggpubr)

bxp1 <-ggboxplot(bd[,2],ylab="",xlab="Roubo",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp2 <-ggboxplot(bd[,3],ylab="",xlab="Furto",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp3 <-ggboxplot(bd[,4],ylab="",xlab="Homicidio",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp4 <-ggboxplot(bd[,5],ylab="",xlab="Roubo a transeunte",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp5 <-ggboxplot(bd[,6],ylab="",xlab="Roubo a veiculo",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp6 <-ggboxplot(bd[,7],ylab="",xlab="Roubo a residencia",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp7 <-ggboxplot(bd[,8],ylab="",xlab="Furto a transeunte",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp8 <-ggboxplot(bd[,9],ylab="",xlab="Furto a veiculo",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
bxp9 <-ggboxplot(bd[,10],ylab="",xlab="Furto a residencia",color = "black",fill="gray",palette = "rgb", bxp.errorbar=TRUE,whiskcol=" blue", linetype = "solid",outlier.shape=8,outlier.colour="dark red",outlier.size=1.0) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(),axis.ticks = element_blank(),axis.line = element_blank(),panel.border = element_rect(color = "black",fill = NA,size = 1))
#Create chart joining all previous plots
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
bxp<-ggarrange(bxp1,bxp2,bxp3,bxp4,bxp5,bxp6,bxp7,bxp8,bxp9+rremove("x.text"),labels = c("", "", ""),ncol = 3, nrow = 3)
rm(bxp1)+rm(bxp2)+rm(bxp3)+rm(bxp4)+rm(bxp5)+rm(bxp6)+rm(bxp7)+rm(bxp8)+rm(bxp9)
bxp




###############################################
#Explore data visualy

#Check normality
library(car)
qqPlot(bd[,2],
       main = "Actual Quantiles x Normal Quatiles", ylab = "Crime distribution")
#As close as the points fall approximately along this reference line, 
# more probably the data has normality.

#Density Distribution (alternative normality check)
library(ggplot2)
ggdensity(bd[,2],
          main = "Density plot of crimes",
          xlab = "Weekly crime rate")


###############################################
#Export charts
###############################################
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
file.copy(from=plots.png.paths, to="/Users/fred/Desktop/BH/Charts/")
#file.copy(from=plots.png.paths, to="C:/Users/Frederico/Desktop/BH/Charts/")

plots.png.details <- file.info(plots.png.paths)
plots.png.details <- plots.png.details[order(plots.png.details$ctime),]
valid.png.details <- plots.png.details %>% filter(size>0)

sorted.png.names <- gsub(plots.dir.path, "/Users/fred/Desktop/BH/Charts/", row.names(valid.png.details), fixed=TRUE)
#sorted.png.names <- gsub(plots.dir.path, "C:/Users/Frederico/Desktop/BH/Charts/", row.names(valid.png.details), fixed=TRUE)

numbered.png.names <- paste0("/Users/fred/Desktop/BH/Charts/", "3.", 1:length(sorted.png.names), ".png")
#numbered.png.names <- paste0("C:/Users/Frederico/Desktop/BH/Charts/","3.", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)

# Remove empty.png that was auto-generated
file.remove('/Users/fred/Desktop/BH/Charts/empty.png')
