## R Script for:
##"Who Wrote the Rules for the Trans-Pacific Partnership"
##Todd Allee and Andrew D. Lugg

##Note: You will need to install the following packages if not already installed.
##This can be done using this syntax: install.packages("package_name") 

library(ggplot2)
library(multcomp)
library(gplots)
library(coefplot)
library(DTK)
library(plyr)
library(reshape2)
library(gridExtra)
library(grid)

####Heatmap code (for figure 1 in manuscript)

##Bring in data
Data_frame <- read.csv("~/heatmap_data_rp.csv", stringsAsFactors = FALSE)

##Specify the relevant variables
keep <- c("doc1", "doc2", "perfectmatch_percent_doc1")
df_new <- Data_frame[keep]

##change order of list here before convering to matrix
final_list<- c("999", "543","84", "96", "187", "188", "218", "241", "551", "628", "637", "643", "645", "658", ##US 
               "520", "518", "517", "519", "146", "207", "495", "523", "826", "829", "842", ##Japan
               "164", "165", "168", "163", "162","161", "166", "798", "797", "852",  ##Canada
               "82", "83", "75", "795", "911", "950", ##Australia
               "390", "509", "330", ##Mexico
               "599", "598", "800", "951", "953", ##Malaysia
               "631", "396", "534", "493", "550", "641", "228", "475", "644", "814", "874", ##singapore
               "202", "205", "208", "199", "206", "802", "858", ##chile
               "771", "227", "819", "830", "810", ##peru
               "632", "222", "825") ##new zealand


##Put the list into matrix
nm_final<-matrix(NA, nrow=length(final_list), ncol=length(final_list), dimnames=list(final_list, final_list))

##now fill the matrix with a loop 
for(i in 1:length(df_new[,1])){
  nm_final[grep(paste(df_new[i,1]),rownames(nm_final)), grep(paste(df_new[i,2]),colnames(nm_final))]<-df_new[i,3]
  nm_final[grep(paste(df_new[i,2]),rownames(nm_final)), grep(paste(df_new[i,1]),colnames(nm_final))]<-df_new[i,3]
}
##Fill in diagonal with 1s
diag(nm_final) = 1
View(nm_final)
##Validating that it worked
isSymmetric(nm_final)

##apply new rownames 
r1<-c("TPP", "US Jordan","US Australia", "US Bahrain",  "CAFTA", "CAFTA-DR", "US Chile", "US Colombia", "US Korea", "US Morocco", "US Oman", #US
      "US Panama", "US Peru", "US Singapore", #US
      "Japan Singapore", "Japan Mexico", "Japan Malaysia",  "Japan Philippines",  "Japan Brunei",  "Japan Chile",  "Japan Indonesia", #Japan
      "Japan Vietnam", "Japan India", "Japan Peru", "Japan Australia", #Japan
      "Canada EFTA","Canada Israel", "Canada Peru", "Canada Costa Rica", "Canada Colombia", "Canada Chile", "Canada Jordan", "Canada Panama", "Canada Honduras", "Canada Korea", #Canada
      "Australia Singapore", "Australia Thailand", "Australia Chile", "Australia Malaysia", "Australia Korea", "ASEAN-Aus-NZ", #Australia
      "Mexico EFTA", "Mexico Israel", "Mexico EC", #Mexico
      "Malaysia Pakistan", "Malaysia New Zealand", "Malaysia Chile", "Malaysia India", "Malaysia Turkey", #Malaysia
      "Singapore NZ", "Singapore EFTA",  "Singapore Jordan", "Singapore India", "Singapore Korea", "Singapore Panama", "Singapore China", "Singapore GCC", "Singapore Peru", "Singapore Costa Rica", "Singapore EC", #Singapore
      "Chile EC", "Chile EFTA", "Chile Korea", "Chile China", "Chile India", "Chile Turkey", "Chile Hong Kong", #Chile
      "Peru Thailand", "Peru China", "Peru EFTA", "Peru Korea", "Peru Col-EC", #Peru
      "NZ Thailand", "NZ China", "NZ Hong Kong") #New Zealand

rownames(nm_final)<-r1
colnames(nm_final)<-r1

#melt for ggplot
melted <- melt(nm_final)
head(melted)

##FIGURE 1
##Final Code for final heatmap figure 1 in manuscript
ggplot(data = melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 4, hjust = 1, family="Calibri", face="bold"),
        axis.text.y = element_text(size = 4, family="Calibri", face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(.9, 0),
        legend.position = c(1.15, 0.35),
        legend.direction = "vertical",
        legend.title=element_text(size=7, family="Calibri", face="bold"),
        legend.text=element_text(size=6, family="Calibri", face="bold")) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 4))+
  ggtitle("") +
  theme(plot.title = element_text(face="bold", size=8, hjust=0, vjust=0)) +
  coord_fixed()


##Bring in the data for rest of analysis
df <- read.csv("~/TPP_data_RP.csv", stringsAsFactors = FALSE)

## sort data in order of percent overlap with TPP
df_sorted <- df[order(df$pm_percent_pta, decreasing=T), ]
##Sort Data by agreement for Figure 2
df_sorted$name <- reorder(df_sorted$name, df_sorted$pm_percent_pta)
df_sorted$country <-reorder(df_sorted$country, df_sorted$pm_percent_pta)

##FIGURE 2 
##Percent replicated by agreement
ggplot(df_sorted, aes(x = pm_percent_pta, y = factor(name))) +
  geom_point(aes(color = factor(usa_dum))) + 
  scale_color_manual(values=c("black", "blue")) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, vjust = .25),
        axis.text.y = element_text(size = 4),
        axis.title.y = element_blank(),
        legend.text= element_text(size=6),
        legend.title=element_text(size=6),
        legend.position = "none") +
  xlab("Percent Overlap")  +
  ggtitle("Textual Overlap Between PTAs and TPP")


##Looking at country level patterns
avg<-aggregate( formula = pm_percent_pta~country, 
                data = df,
                FUN = mean )

avg$country <-reorder(avg$country, avg$pm_percent_pta)

avg[order(avg$country,decreasing = TRUE),]

##Assessing Statistical significance of country differences

##Step 1, Performing ANOVA for equal and unequal variances
oneway.test(df$pm_percent_pta ~ df$country)
oneway.test(df$pm_percent_pta ~ df$country, var.equal = TRUE)


##Step 2, Performing TukeyHSD test for group differences
m<-aov(df$pm_percent_pta ~ df$country)
summary(m)
TukeyHSD(m)

#US mean statistically different than all other group means; p<.05

##Visualizing the data
##Put the Data in order and Label US
avg$country <-reorder(avg$country, avg$pm_percent_pta)
us <- subset(avg, country == "USA")
round<-round(avg$pm_percent_pta, digits=0)
round<-sort(round, decreasing=TRUE)

##Figure 3 in manuscript
ggplot(avg, aes(x = pm_percent_pta, y = factor(country))) +
  geom_point(color = "black") + 
  geom_point(data=us, color="blue") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10, vjust = .25, family="Calibri", face="bold"),
        axis.text.y = element_text(size = 10, family="Calibri", face = "bold"),
        axis.title.y = element_blank(),
        legend.position = "none") +
  #geom_text(aes(label=round, vjust=-.35)) +
  xlab("Percent Replicated in TPP")  +
  ggtitle("")

#Assessing robustness of patterns -- Table 1 in manuscript
#Note: Some calculations done in excel

##This caluculates match % with relaxed paramters 
avg_relaxed<-aggregate( formula = om_percent_pta~country, 
                             data = df,
                             FUN = mean )

avg_relaxed$country <-reorder(avg_relaxed$country, avg_relaxed$om_percent_pta)
avg_relaxed[order(avg_relaxed$country,decreasing = TRUE),]


##Subsections comparisons
##Basis for Table 2 in manuscript

#Dispute Settlement (1)
avg<-aggregate( formula = ds_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$ds_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$ds_pm_percent_pta)
print(avg_sorted)

#Anti-dumping (2)
avg<-aggregate( formula = ad_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$ad_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$ad_pm_percent_pta)
print(avg_sorted)

#e-commerce (3)
avg<-aggregate( formula = ecom_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$ecom_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$ecom_pm_percent_pta)
print(avg_sorted)

#environment (4)
avg<-aggregate( formula = env_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$env_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$env_pm_percent_pta)
print(avg_sorted)

#Financial Services (5)
avg<-aggregate( formula = fin_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$fin_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$fin_pm_percent_pta)
print(avg_sorted)

#Investment (6)
avg<-aggregate( formula = invs_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$invs_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$invs_pm_percent_pta)
print(avg_sorted)

#IPR (7)
avg<-aggregate( formula = ipr_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$ipr_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$ipr_pm_percent_pta)
print(avg_sorted)

#Labor (8)
avg<-aggregate( formula = lab_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$lab_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$lab_pm_percent_pta)
print(avg_sorted)

#Movement (9)
avg<-aggregate( formula = move_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$move_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$move_pm_percent_pta)
print(avg_sorted)

#Procurement (10)
avg<-aggregate( formula = proc_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$proc_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$proc_pm_percent_pta)
print(avg_sorted)

#Safeguards (11)
avg<-aggregate( formula = safe_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$safe_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$safe_pm_percent_pta)
print(avg_sorted)

#Services (12)
avg<-aggregate( formula = serv_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$serv_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$serv_pm_percent_pta)
print(avg_sorted)
 
#SPS (13)
avg<-aggregate( formula = sps_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$sps_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$sps_pm_percent_pta)
print(avg_sorted)

#Technical Barriers to Trade (14)
avg<-aggregate( formula = tbt_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$tbt_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$tbt_pm_percent_pta)
print(avg_sorted)

#Telcoms (15)
avg<-aggregate( formula = tele_pm_percent_pta~country, 
                data = df,
                FUN = mean )
avg_sorted <- avg[order(avg$tele_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$tele_pm_percent_pta)
print(avg_sorted)

##Making the investment Plots
###Investment - individual agreements -- right side of plot 4
invs_sort <- df[order(df$invs_pm_percent_pta, decreasing=T), ]
invs_sort <- invs_sort[1:13,]
invs_sort$name <- reorder(invs_sort$name, invs_sort$invs_pm_percent_pta)
us_invs <- subset(invs_sort, country == "USA")
invs <- ggplot(invs_sort, aes(x = invs_pm_percent_pta, y = factor(name))) +
  geom_point() + 
  geom_point(data=us_invs, color="blue") +
  scale_x_continuous(limits = c(75, 90))+
  theme_bw() + theme(axis.title.x = element_text(size = 10, vjust = .25,family="Calibri", face="bold"),
                     axis.text.y = element_text(size = 10, family="Calibri", face="bold"),
                     axis.title.y = element_blank(),
                     plot.title = element_text(size=12, family="Calibri", face="bold"))+
  xlab("Percent Replicated in TPP") + ggtitle("10 Closest-Match Agreements")  

##Investment -- country averages -- left side of plot 4
avg<-aggregate( formula = invs_pm_percent_pta~country, 
                data = df,
                FUN = mean )

avg_sorted <- avg[order(avg$invs_pm_percent_pta, decreasing=T), ]
avg_sorted$country <- reorder(avg_sorted$country, avg_sorted$invs_pm_percent_pta)
us_invs_avg <- subset(avg_sorted, country == "USA")
invs_avgs <- ggplot(avg_sorted, aes(x = invs_pm_percent_pta, y = factor(country))) +
  geom_point() +
  scale_x_continuous(limits=c(20, 80))+
  geom_point(data=us_invs_avg, color="blue") +
  theme_bw() + theme(axis.title.x = element_text(size = 10, vjust = .25, family="Calibri", face="bold"),
                     axis.text.y = element_text(size = 10, family="Calibri", face="bold"),
                     axis.title.y = element_blank(),
                     plot.title = element_text(size=12, family="Calibri", face="bold"))+
  xlab("Percent Replicated in TPP") + ggtitle("Country Averages") 

##FIURE 4
##Function to put the two plots together side-by-side
grid.arrange(invs_avgs, invs, ncol=2) 
