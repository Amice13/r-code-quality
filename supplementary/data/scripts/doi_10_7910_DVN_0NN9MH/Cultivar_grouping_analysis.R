# ---------------------
# Clean the environment
# ---------------------
rm(list=ls())
gc()
# Load libraries
library(readxl)  #read excel file
library(plyr)  #data manipulation
library(dplyr)  #data manipulation
library(ggplot2)  #plotting
library(ggpmisc)  #plotting stats
library(ggpubr)  #combining plots for publication figures
library(ggiraph)  #interactive ggplot
library(ggiraphExtra)  #interactive ggplot

## Set working directories
# Main Folder
default.wd <- c("C:/Users/jrafa/OneDrive/Desktop/Main/Mills_et_al_2018_cultivar_data")

# Read files
setwd(default.wd)

# Select crop
#crop <- c("Maize","Rice","Soybean","Wheat")[1]
crop <- c("Maize","Rice","Soybean","Wheat")

# Loop over all crops
for(i in 1:length(crop)){
# Read data
my_data <- read_excel("Cultivar dose response relationships_Mills et al_jg.xlsx",sheet=2) #read 2nd sheet
my_data <- my_data[,c(1,3:6)] #subset only necessary data
Rel_Yield_Percent <- my_data$Rel_Yield*100 #convert to percent
my_data <- cbind(my_data,Rel_Yield_Percent) #add percent column to dataframe

sim_data <- read_excel("Cultivar dose response relationships_Mills et al_jg.xlsx",sheet=3) #read 3rd sheet

weibull_data <- read_excel("Cultivar dose response relationships_Mills et al_jg.xlsx",sheet=4) #read 4th sheet

# Subset crop
my_data <- my_data[my_data$Crop == crop[i],]
sim_data <- sim_data[sim_data$Crop == crop[i],]
weibull_data <- weibull_data[weibull_data$Crop == crop[i],]
# Set levels for plot legend
sim_data$O3_Sensitivity <- factor(sim_data$O3_Sensitivity,levels=c("Tolerant","Intermediate","Sensitive"))

# Plot ozone vs relative yield
ggplot(my_data, aes(Mean_7, Rel_Yield_Percent, color = Cultivar)) +
  geom_point(alpha = 0.75) + #make points slightly transparent
  xlab("M7 Ozone (ppb)") +
  ylab("Relative yield (%)")+
  theme_classic()+
  theme(axis.text = element_text(size=12,face="bold",color="black"),
        axis.title = element_text(size=16,face="bold",color="black"),
        plot.title = element_text(size=16,face="bold",color="black"),
        legend.title = element_text(size=12,face="bold",color="black"),
        legend.text = element_text(size=12,face="bold",color="black"))

## Linear regression of cultivars

# Break up data by Cultivar, then fit the specified linear model to each Cultivar and return a list
models <- dlply(my_data, c("Cultivar"), function(df) 
  lm(Rel_Yield_Percent ~ Mean_7, data = df))
# Apply coef to each model and return a data frame
cv_slopes <- ldply(models, coef)
# Remove any na values, i.e., cultivars with only a single point, only applies to soybean
cv_slopes <- na.omit(cv_slopes)
# Rename column for slope
colnames(cv_slopes)[colnames(cv_slopes) == "Mean_7"] <- "Slope"
# Print the summary of each model
#l_ply(models, summary, .print = TRUE)

# Classify O3 cultivar sensitivity based on the slope of the linear regressions of cultivars
cv_o3_class <- cv_slopes %>% 
  mutate(O3_Sensitivity = case_when(Slope < quantile(Slope, 0.33) ~ "Sensitive",
                                 Slope >= quantile(Slope, 0.33) & Slope <= quantile(Slope, 0.66) ~ "Intermediate",
                                 Slope > quantile(Slope, 0.66) ~ "Tolerant"))

# Barplot
b1 <- ggplot(cv_o3_class,aes(y=Cultivar,fill=O3_Sensitivity))+
  geom_bar()+
  scale_y_discrete(limits = rev)+
  scale_x_continuous(limits=c(0,1),breaks=c(0,1))+
  xlab("") +
  ggtitle(crop[i])+
  theme_classic()+
  theme(axis.text = element_text(size=12,face="bold",color="black"),
        axis.title = element_text(size=16,face="bold",color="black"),
        plot.title = element_text(size=16,face="bold",color="black"),
        legend.title = element_text(size=12,face="bold",color="black"),
        legend.text = element_text(size=12,face="bold",color="black"))
b1
# Output table of linear regressions and cultivar O3 sensitivities
write.csv(cv_o3_class,paste0(crop[i],"_cultivar_linear_fits_and_o3_sensitvities.csv"),row.names=FALSE,quote=FALSE)

# Merge crop dataset with o3 cultivar classification
merged_data <- merge(x=my_data,y=cv_o3_class,by="Cultivar",all=TRUE)
# Remove any na values, i.e., cultivars with only a single point, only applies to soybean
merged_data <- na.omit(merged_data)
# Add type of data
merged_data$Type <- "Literature \n(linear fit)"
# Reorder levels for correct plot legend order
merged_data$O3_Sensitivity <- factor(merged_data$O3_Sensitivity, levels = c("Tolerant","Intermediate","Sensitive"))

## Linear regression plots
# Set crop axis scales
if(crop[i] == "Maize"){
  #scale_x <- scale_x_continuous(limits=c(0,140),breaks=seq(0,140,20))
  scale_x <- scale_x_continuous(limits=c(0,180),breaks=seq(0,180,20))
  scale_y <- scale_y_continuous(limits=c(0,120),breaks=seq(0,120,20))
  legend_guide <- guides(color=guide_legend(ncol=1))
}
if(crop[i] == "Rice"){
  scale_x <- scale_x_continuous(limits=c(0,180),breaks=seq(0,180,20))
  scale_y <- scale_y_continuous(limits=c(0,120),breaks=seq(0,120,20))
  legend_guide <- guides(color=guide_legend(ncol=2))
}
if(crop[i] == "Soybean"){
  #scale_x <- scale_x_continuous(limits=c(0,160),breaks=seq(0,160,20))
  scale_x <- scale_x_continuous(limits=c(0,180),breaks=seq(0,180,20))
  scale_y <- scale_y_continuous(limits=c(0,140),breaks=seq(0,140,20))
  legend_guide <- guides(color=guide_legend(ncol=2))
}
if(crop[i] == "Wheat"){
  #scale_x <- scale_x_continuous(limits=c(0,120),breaks=seq(0,120,20))
  scale_x <- scale_x_continuous(limits=c(0,180),breaks=seq(0,180,20))
  scale_y <- scale_y_continuous(limits=c(0,120),breaks=seq(0,120,20))
  legend_guide <- guides(color=guide_legend(ncol=2))
}

# Plot based on individual cultivars
p1 <- ggplot(my_data,aes(y=Rel_Yield_Percent,x=Mean_7,color=Cultivar))+
  geom_point()+
  stat_smooth(method="lm",se=F)+
  xlab("M7 Ozone (ppb)") +
  ylab("Relative yield (%)")+
  scale_x +
  scale_y +
  legend_guide +
  ggtitle(crop[i])+
  theme_classic()+
  theme(axis.text = element_text(size=12,face="bold",color="black"),
        axis.title = element_text(size=12,face="bold",color="black"),
        axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        plot.title = element_text(size=14,face="bold",color="black"),
        legend.title = element_text(size=8,face="bold",color="black"),
        legend.text = element_text(size=8,face="bold",color="black"),
        legend.key.size = unit(0.7, 'lines'))
p1
assign(paste0("linear_plot_",crop[i]),p1)

# Plot based on O3 cultivar sensitivity
p2 <- ggplot(merged_data,aes(y=Rel_Yield_Percent,x=Mean_7,color=O3_Sensitivity,grp.label=O3_Sensitivity))+
  geom_point()+
  scale_color_manual(name="O3 Sensitivity",values=c("#56B4E9","#E69F00","#CC79A7"))+ #colorblind safe colors
  stat_smooth(method="lm",se=TRUE,aes(fill=O3_Sensitivity,color=O3_Sensitivity,linetype=Type))+
  #stat_poly_eq(aes(label = paste("bold(",after_stat(grp.label), "*\"_Lit:\")*",
  #                               after_stat(eq.label), "*\", \"*",
  #                               after_stat(rr.label), sep = "")),label.x="right") +
  #stat_poly_eq(aes(label = paste(after_stat(grp.label), "*\"_Lit:\"*",
  #                               after_stat(eq.label), "*\", \"*",
  #                               after_stat(rr.label), sep = "")),label.x="right") +
  scale_fill_manual(name="O3 Sensitivity",values = c("#56B4E9","#E69F00","#CC79A7")) +
  geom_line(data=sim_data,aes(y=Rel_Yield_Percent,x=Mean_7,color=O3_Sensitivity,linetype=Type),
            linewidth=1)+
  scale_linetype_manual(name="Data Source",values=c(3,1))+
  scale_x +
  scale_y +
  xlab("M7 Ozone (ppb)") +
  ylab("Relative yield (%)")+
  ggtitle(crop[i])+
  theme_classic()+
  theme(axis.text = element_text(size=16,face="bold",color="black"),
        axis.title = element_text(size=16,face="bold",color="black"),
        plot.title = element_text(size=16,face="bold",color="black"),
        legend.title = element_text(size=12,face="bold",color="black"),
        legend.text = element_text(size=12,face="bold",color="black"))
p2
assign(paste0("plot_",crop[i]),p2)

# Plot based on O3 cultivar sensitivity
p3 <- ggplot(merged_data,aes(y=Rel_Yield_Percent,x=Mean_7,color=O3_Sensitivity,grp.label=O3_Sensitivity))+
  geom_point()+
  scale_color_manual(name="O3 Sensitivity",values=c("#56B4E9","#E69F00","#CC79A7"))+ #colorblind safe colors
  stat_smooth(method="lm",se=TRUE,aes(fill=O3_Sensitivity,color=O3_Sensitivity),linetype=3)+
  #stat_poly_eq(aes(label = paste("bold(",after_stat(grp.label), "*\"_Lit:\")*",
  #                               after_stat(eq.label), "*\", \"*",
  #                               after_stat(rr.label), sep = "")),label.x="right") +
  stat_poly_eq(aes(label = paste(after_stat(grp.label), "*\":\"*",
                                 after_stat(eq.label), "*\", \"*",
                                 after_stat(rr.label), sep = "")),label.x="left",label.y=c(0.11,0.06,0.01)) +
  scale_fill_manual(name="O3 Sensitivity",values = c("#56B4E9","#E69F00","#CC79A7")) +
  #geom_line(data=sim_data,aes(y=Rel_Yield_Percent,x=Mean_7,color=O3_Sensitivity,linetype=Type),
  #          linewidth=1)+
  #scale_linetype_manual(name="Data Source",values=c(3))+
  scale_x +
  scale_y +
  xlab("M7 Ozone (ppb)") +
  ylab("Relative yield (%)")+
  ggtitle(crop[i])+
  theme_classic()+
  theme(axis.text = element_text(size=16,face="bold",color="black"),
        axis.title = element_text(size=16,face="bold",color="black"),
        plot.title = element_text(size=16,face="bold",color="black"),
        legend.title = element_text(size=12,face="bold",color="black"),
        legend.text = element_text(size=12,face="bold",color="black"))
p3
assign(paste0("lit_plot_",crop[i]),p3)

# Plot based on O3 cultivar sensitivity with weibull function comparison
p4 <- ggplot(merged_data,aes(y=Rel_Yield_Percent,x=Mean_7,color=O3_Sensitivity,grp.label=O3_Sensitivity))+
  geom_point()+
  scale_color_manual(name="O3 Sensitivity",values=c("#56B4E9","#E69F00","#CC79A7"))+ #colorblind safe colors
  stat_smooth(method="lm",se=TRUE,aes(fill=O3_Sensitivity,color=O3_Sensitivity,linetype=Type))+
  #stat_poly_eq(aes(label = paste("bold(",after_stat(grp.label), "*\"_Lit:\")*",
  #                               after_stat(eq.label), "*\", \"*",
  #                               after_stat(rr.label), sep = "")),label.x="right") +
  #stat_poly_eq(aes(label = paste(after_stat(grp.label), "*\"_Lit:\"*",
  #                               after_stat(eq.label), "*\", \"*",
  #                               after_stat(rr.label), sep = "")),label.x="right") +
  scale_fill_manual(name="O3 Sensitivity",values = c("#56B4E9","#E69F00","#CC79A7")) +
  geom_line(data=sim_data,aes(y=Rel_Yield_Percent,x=Mean_7,color=O3_Sensitivity,linetype=Type),
            linewidth=1)+
  geom_line(data=weibull_data,aes(y=Rel_Yield_Percent,x=Mean_7,linetype=Type),color="black",
            linewidth=1)+
  scale_linetype_manual(name="Data Source",values=c(3,1,2))+
  scale_x +
  scale_y +
  xlab("M7 Ozone (ppb)") +
  ylab("Relative yield (%)")+
  ggtitle(crop[i])+
  theme_classic()+
  theme(axis.text = element_text(size=16,face="bold",color="black"),
        axis.title = element_text(size=16,face="bold",color="black"),
        plot.title = element_text(size=16,face="bold",color="black"),
        legend.title = element_text(size=12,face="bold",color="black"),
        legend.text = element_text(size=12,face="bold",color="black"))
p4
assign(paste0("weibull_plot_",crop[i]),p4)


# End loop of crops
}

## Combine ggplots of crops into one figure
theme_set(theme_pubr())
#figure <- ggarrange(plot_Maize,plot_Rice,plot_Soybean,plot_Wheat,
#                    labels=c("(a)","(b)","(c)","(d)"),ncol=2,nrow=2,
#                    common.legend=TRUE,legend="right")
#figure

figure2 <- ggarrange(linear_plot_Maize,linear_plot_Rice,linear_plot_Soybean,linear_plot_Wheat,
                    labels=c("(a)","(b)","(c)","(d)"),ncol=2,nrow=2,
                    common.legend=FALSE,legend="right")
figure2

figure3 <- ggarrange(lit_plot_Maize,lit_plot_Rice,lit_plot_Soybean,lit_plot_Wheat,
                     labels=c("(a)","(b)","(c)","(d)"),ncol=2,nrow=2,
                     common.legend=TRUE,legend="right")
figure3

figure4 <- ggarrange(weibull_plot_Maize,weibull_plot_Rice,weibull_plot_Soybean,weibull_plot_Wheat,
                     labels=c("(a)","(b)","(c)","(d)"),ncol=2,nrow=2,
                     common.legend=TRUE,legend="right")
figure4

# Export figures
#ggsave("Figure8-Simulated_yield_loss_comparison.png",figure,width=4,height=4,units="in",scale=3)
ggsave("FigureS2-Cultivar_linear_regressions.png",figure2,width=4,height=4,units="in",scale=3)
ggsave("FigureS3-Literature_O3_sensitivity.png",figure3,width=4,height=4,units="in",scale=3)
ggsave("Figure8-Literature_simulated_weibull_comparison.png",figure4,width=4,height=4,units="in",scale=3)
