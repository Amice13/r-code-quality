# AUTHOR: Yeraldi Loera, yeraldiloera@gmail.com
# PURPOSE: Wedgebilled woodcreeper feather metal analysis 
# DETAILS: Metal analysis by Dartmouth trace metal analysis lab
# QUESTION: Are the WBWC feathers from Tiputini, Ecuador (near oil) and Nouragues, French Guiana (near gold) contaminated despite their protection (compared to Andean reference)?

####SET UP####

#PACKAGES#
library(readxl) #reading data
library(tidyverse) #data manipulation
library(ggplot2) #plotting
library(ggpubr) #advanced plotting
library(rstatix) #statistical testing
library(AICcmodavg) #aic testibng
library(sp) #working with spatial data
library(amt) #transforming to coordinate systems
library(raster) #transforming to raster
library(rgdal) #reading in raster data
library(rasterVis) #plotting raster map
library(rgeos) #calculating distances

#my graph aesthetics
myfonts <- theme(
  plot.title = element_text(family = "Times", size = (20)),
  legend.title = element_text(face="bold", family = "Times", size = (15)),
  legend.text = element_text(family = "Times", size = (15)),
  axis.title = element_text(family = "Times", size = (15)),
  axis.text = element_text(family = "Times", size = (15))
)

mypalette <- c("royalblue4","gold2","green4")


####DATA OVERVIEW####

#upload full database (both batches, meta data and results combined)
full_data<-as.data.frame(read_xlsx("FULL_Metal_Results_Data.xlsx"))
dim(full_data)

#replace baseline detection low (BDL) with NA's
full_data<-replace(full_data,full_data == "BDL", NA)

#make metal data numeric
full_data[,5:27]<-lapply(full_data[,5:27],as.numeric)

#scale by metal
full_data[,5:27]<-scale(full_data[,5:27])

#look over database
colnames(full_data) 

#23 metals
metals<-colnames(full_data)[5:27]
length(metals)

#metadata
metadata<-colnames(full_data)[28:42]
metadata

#rename sites with long names
full_data[full_data=="Sangay National Park Macas"] <- "Sangay"
full_data[full_data=="San Rafael Falls"] <- "San Rafael"

#order the groups for plotting
full_data$Group<- factor(full_data$Group, levels=c("Tiputini (near oil)","Reference","Nouragues (near gold)"))


####NORMALITY TEST FOR FULL LOAD####

#density plot shows a tail of full metal load
ggdensity(full_data$Full_load, 
          main = "Density plot of Full HM Load",
          xlab = "Full HM Load")

#tail also seen on a qq plot
ggqqplot(full_data$Full_load,
         main = "qqplot of Full HM Load",
         xlab = "Full HM Load")

#Shapiro-Wilkes test for normality: Sig different so not normal
shapiro.test(full_data$Full_load) #W = 0.75715, p-value = 3.116e-11

#f test for unequal variances show unequal variances. 
var.test(Full_load ~ Experimental.Notes, full_data, 
         alternative = "two.sided") #F = 8.5715, num df = 43, denom df = 50, p-value = 6.603e-12


####AIC TEST FOR AOV BY GROUP (test vs control) and interaction by feather weight = NEEDS NON-PARAMETRIC KRUSKAL####

#by full metal load... 
one.way <- aov(Full_load ~ Experimental.Notes, data = full_data)
summary(one.way) #Df = 1, F = 22.43, p=7.81e-06 ***

two.way <- aov(Full_load ~ Experimental.Notes + Feather.Weight, data = full_data)
summary(two.way) #Df = 41, F=18.788, p=6.71e-05 ***

interaction <- aov(Full_load ~ Experimental.Notes*Feather.Weight, data = full_data)
summary(interaction) #Df=1, F=17.46, p=0.000172 ***

blocking <- aov(Full_load ~ Experimental.Notes + Feather.Weight + Country.code, data = full_data)
summary(blocking) #Df=1, F=19.398, p=5.45e-05 ***

#AIC model test (lowest is best). Do not need to include feather weight.
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way feather", "interaction", "blocking")
results<-aictab(model.set, modnames = model.names)
results

#plot
par(mfrow=c(2,2))
plot(one.way)
par(mfrow=c(1,1))

#mean and sd of featherweight 
mean(as.numeric(full_data$Feather.Weight)) #0.005741053
sd(as.numeric(full_data$Feather.Weight)) #0.002404849


####TEST AND PLOT METALS EC and FG SEPARATELY####

#####ECUADOR ONLY#####

#subset for ecuador control and test
EC_data<-subset(full_data, full_data$Group != "Nouragues (near gold)")

#F test for unequal variances show unequal variances in control vs test. Nonparametric tests will be ran.
var.test(Full_load ~ Group, EC_data, 
         alternative = "two.sided") #F = 3.423, num df = 19, denom df = 50, p-value = 0.0005058

#full load by group
EC_All<-wilcox.test(EC_data$Full_load ~ EC_data$Group)
EC_All #W = 665, p-value = 0.04828


#individual metal comparisons where Tiputini > reference

##Co##
EC_Co<-wilcox.test(EC_data$Co ~ EC_data$Group)
EC_Co #W = 799, p-value = 0.0002262

##Fe##
EC_Fe<-wilcox.test(EC_data$Fe ~ EC_data$Group)
EC_Fe #W = 735, p-value = 0.004108

##Ni##
EC_Ni<-wilcox.test(EC_data$Ni ~ EC_data$Group)
EC_Ni #W = 836, p-value = 3.172e-05

##V##
EC_V<-wilcox.test(EC_data$V ~ EC_data$Group)
EC_V #W = 789, p-value = 0.0003694

##Cu##
EC_Cu<-wilcox.test(EC_data$Cu ~ EC_data$Group)
EC_Cu #W = 705, p-value = 0.01291

##Ba##
EC_Ba<-wilcox.test(EC_data$Ba ~ EC_data$Group)
EC_Ba #W = 750, p-value = 0.002203

##Cd##
EC_Cd<-wilcox.test(EC_data$Cd ~ EC_data$Group)
EC_Cd #W = 790, p-value = 0.0003532

##Mn##
EC_Mn<-wilcox.test(EC_data$Mn ~ EC_data$Group)
EC_Mn #W = 792, p-value = 0.0003202

##U##
EC_U<-wilcox.test(EC_data$U ~ EC_data$Group)
EC_U #W = 663, p-value = 5.327e-05


#individual metal comparisons where Tiputini < reference

##Zn##
EC_Zn<-wilcox.test(EC_data$Zn ~ EC_data$Group)
EC_Zn #W = 245, p-value = 0.0007221

##As##
EC_As<-wilcox.test(EC_data$As ~ EC_data$Group)
EC_As #W = 330, p-value = 0.00268

##Se##
EC_Se<-wilcox.test(EC_data$Se ~ EC_data$Group)
EC_Se #W = 190, p-value = 4.425e-05

##Sb##
EC_Sb<-wilcox.test(EC_data$Sb ~ EC_data$Group)
EC_Sb #W = 94, p-value = 1.032e-07

##Tl##
EC_Tl<-wilcox.test(EC_data$Tl ~ EC_data$Group)
EC_Tl #W = 306, p-value = 0.007047

##Cs##
EC_Cs<-wilcox.test(EC_data$Cs ~ EC_data$Group)
EC_Cs #W = 122, p-value = 7.296e-07

#not sig different

##Pb##
EC_Pb<-wilcox.test(EC_data$Pb ~ EC_data$Group)
EC_Pb #W = 410, p-value = 0.2034

##Al##
EC_Al<-wilcox.test(EC_data$Al ~ EC_data$Group)
EC_Al #W = 618, p-value = 0.1694

##Hg##
EC_Hg<-wilcox.test(EC_data$Hg ~ EC_data$Group)
EC_Hg #W = 378, p-value = 0.09272

##Cr##
EC_Cr<-wilcox.test(EC_data$Cr ~ EC_data$Group)
EC_Cr #W = 437, p-value = 0.3541

##Sr##
EC_Sr<-wilcox.test(EC_data$Sr ~ EC_data$Group)
EC_Sr #W = 568, p-value = 0.4623

##Mo##
EC_Mo<-wilcox.test(EC_data$Mo ~ EC_data$Group)
EC_Mo #W = 456, p-value = 0.4941

##Ag##
EC_Ag<-wilcox.test(EC_data$Ag ~ EC_data$Group)
EC_Ag #W = 455, p-value = 0.4818

##Sn##
EC_Sn<-wilcox.test(EC_data$Sn ~ EC_data$Group)
EC_Sn #W = 504, p-value = 0.944


#####FG AND EC CONTROL ONLY#####

#subset for all except ecuador test
FG_data<-subset(full_data, full_data$Group != "Tiputini (near oil)")

#f test for unequal variances show EQUAL variances surprisingly...
var.test(Full_load ~ Group, FG_data, 
         alternative = "two.sided") #F = 0.09816, num df = 50, denom df = 23, p-value = 1.293e-11

#full load by group
FG_All<-wilcox.test(FG_data$Full_load ~ FG_data$Group)
FG_All #W = 149, p-value = 1.497e-07


#individual metal comparisons where Nouragues > reference

##Ni##
FG_Ni<-wilcox.test(FG_data$Ni ~ FG_data$Group)
FG_Ni #W = 16, p-value = 1.346e-11

##Pb##
FG_Pb<-wilcox.test(FG_data$Pb ~ FG_data$Group)
FG_Pb #W = 241, p-value = 2.576e-05

##Cu##
FG_Cu<-wilcox.test(FG_data$Cu ~ FG_data$Group)
FG_Cu #W = 34, p-value = 5.413e-11

##Zn##
FG_Zn<-wilcox.test(FG_data$Zn ~ FG_data$Group)
FG_Zn #W = 40, p-value = 8.529e-11

##Mn##
FG_Mn<-wilcox.test(FG_data$Mn ~ FG_data$Group)
FG_Mn #W = 307, p-value = 0.0005433

##Fe##
FG_Fe<-wilcox.test(FG_data$Fe ~ FG_data$Group)
FG_Fe #W = 130, p-value = 4.532e-08

##Cr##
FG_Cr<-wilcox.test(FG_data$Cr ~ FG_data$Group)
FG_Cr #W = 145, p-value = 1.168e-07

##Co##
FG_Co<-wilcox.test(FG_data$Co ~ FG_data$Group)
FG_Co #W = 331, p-value = 0.001443

##U##
FG_U<-wilcox.test(FG_data$U ~ FG_data$Group)
FG_U #W = 306, p-value = 5.431e-08

##Se##
FG_Se<-wilcox.test(FG_data$Se ~ FG_data$Group)
FG_Se #W = 2, p-value = 4.435e-12

##V##
FG_V<-wilcox.test(FG_data$V ~ FG_data$Group)
FG_V #W = 294, p-value = 0.0003098

##Hg##
FG_Hg<-wilcox.test(FG_data$Hg ~ FG_data$Group)
FG_Hg #W = 115, p-value = 1.701e-08

##Cs##
FG_Cs<-wilcox.test(FG_data$Cs ~ FG_data$Group)
FG_Cs #W = 392, p-value = 0.01267


#where nouragues < reference

##Sr##
FG_Sr<-wilcox.test(FG_data$Sr ~ FG_data$Group)
FG_Sr #W = 1047, p-value = 8.017e-07

##Sn##
FG_Sn<-wilcox.test(FG_data$Sn ~ FG_data$Group)
FG_Sn #W = 952, p-value = 0.0001153

##Ba##
FG_Ba<-wilcox.test(FG_data$Ba ~ FG_data$Group)
FG_Ba #W = 894, p-value = 0.001388

##Tl##
FG_Tl<-wilcox.test(FG_data$Tl ~ FG_data$Group)
FG_Tl #W = 881, p-value = 0.001294

##Sb##
FG_Sb<-wilcox.test(FG_data$Sb ~ FG_data$Group)
FG_Sb #W = 1001, p-value = 9.897e-06

##As##
FG_As<-wilcox.test(FG_data$As ~ FG_data$Group)
FG_As #W = 828, p-value = 0.001085

##Cd##
FG_Cd<-wilcox.test(FG_data$Cd ~ FG_data$Group)
FG_Cd #W = 817, p-value = 0.0202


#not significantly different

##Al##
FG_Al<-wilcox.test(FG_data$Al ~ FG_data$Group)
FG_Al #W = 550, p-value = 0.4849

##Mo##
FG_Mo<-wilcox.test(FG_data$Mo ~ FG_data$Group)
FG_Mo #W = 627, p-value = 0.8692

##Ag##
FG_Ag<-wilcox.test(FG_data$Ag ~ FG_data$Group)
FG_Ag #W = 607, p-value = 0.9588


####SUMMARY PLOTTING####

#####PLOTTING 3 GROUPS BY METAL #####

#Mean HM load
Full_Group<- kruskal.test(full_data$Full_load ~ full_data$Group)
Full_Group #Kruskal-Wallis chi-squared = 29.612, df = 2, p-value = 3.714e-07

# Pairwise comparisons
Full_wilcox <- full_data %>% 
  wilcox_test(Full_load ~ Group) 
Full_wilcox$statistic

Full_wilcox <- Full_wilcox %>% add_xy_position(x = "Group")

FULL_PLOT<-ggboxplot(full_data, x = "Group", y = "Full_load", fill="Group",outlier.shape = NA,
          ylab = "Mean Heavy Metal Load (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.6, alpha=0.7,width=0.1,) +
  stat_pvalue_manual(Full_wilcox, hide.ns = TRUE) +
  theme(legend.position='upper right') + myfonts +scale_fill_manual(values=c('royalblue4','gold2','green4'))
  # +labs(caption = get_pwc_label(Full_wilcox))
FULL_PLOT

ggsave("FullComparison.png", width = 8, height= 6, units = "in")




#not sig

#Al#
Al_Group<- kruskal.test(full_data$Al ~ full_data$Group)
Al_Group #Kruskal-Wallis chi-squared = 2.0694, df = 2, p-value = 0.3553

# Pairwise comparisons
Al_wilcox <- full_data %>% 
  wilcox_test(Al ~ Group) 
Al_wilcox

Al_wilcox <- Al_wilcox %>% add_xy_position(x = "Group")

Al_PLOT<-ggboxplot(full_data, x = "Group", y = "Al", color="Group",palette=c('royalblue4','gold2','green4'),
                     ylab = "Al Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Al_wilcox, hide.ns = TRUE) +
  labs( title = "Al Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=0.3553",
        caption = get_pwc_label(Al_wilcox)
  )+theme(legend.position='upper right') + myfonts
Al_PLOT


#Mo#
Mo_Group<- kruskal.test(full_data$Mo ~ full_data$Group)
Mo_Group #Kruskal-Wallis chi-squared = 0.63856, df = 2, p-value = 0.7267

# Pairwise comparisons
Mo_wilcox <- full_data %>% 
  wilcox_test(Mo ~ Group) 
Mo_wilcox

Mo_wilcox <- Mo_wilcox %>% add_xy_position(x = "Group")

Mo_PLOT<-ggboxplot(full_data, x = "Group", y = "Mo", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Mo Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Mo_wilcox, hide.ns = TRUE) +
  labs( title = "Mo Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=0.7267",
        caption = get_pwc_label(Mo_wilcox)
  )+theme(legend.position='upper right') + myfonts
Mo_PLOT


#Ag#
Ag_Group<- kruskal.test(full_data$Ag ~ full_data$Group)
Ag_Group #Kruskal-Wallis chi-squared = 2.339, df = 2, p-value = 0.3105

# Pairwise comparisons
Ag_wilcox <- full_data %>% 
  wilcox_test(Ag ~ Group) 
Ag_wilcox

Ag_wilcox <- Ag_wilcox %>% add_xy_position(x = "Group")

Ag_PLOT<-ggboxplot(full_data, x = "Group", y = "Ag", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Ag Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Ag_wilcox, hide.ns = TRUE) +
  labs( title = "Ag Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=0.3105",
        caption = get_pwc_label(Ag_wilcox)
  )+theme(legend.position='upper right') + myfonts
Ag_PLOT


##rest are sig##

#V#
V_Group<- kruskal.test(full_data$V ~ full_data$Group)
V_Group #Kruskal-Wallis chi-squared = 19.867, df = 2, p-value = 4.853e-05

# Pairwise comparisons
V_wilcox <- full_data %>% 
  wilcox_test(V ~ Group) 
V_wilcox

V_wilcox <- V_wilcox %>% add_xy_position(x = "Group")

V_PLOT<-ggboxplot(full_data, x = "Group", y = "V", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "V Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(V_wilcox, hide.ns = TRUE) +
  labs( title = "V Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=4.853e-05",
        caption = get_pwc_label(V_wilcox)
  )+theme(legend.position='upper right') + myfonts
V_PLOT


#Cr#
Cr_Group<- kruskal.test(full_data$Cr ~ full_data$Group)
Cr_Group #Kruskal-Wallis chi-squared = 33.518, df = 2, p-value = 5.269e-08

# Pairwise comparisons
Cr_wilcox <- full_data %>% 
  wilcox_test(Cr ~ Group) 
Cr_wilcox

Cr_wilcox <- Cr_wilcox %>% add_xy_position(x = "Group")

Cr_PLOT<-ggboxplot(full_data, x = "Group", y = "Cr", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Cr Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Cr_wilcox, hide.ns = TRUE) +
  labs( title = "Cr Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=5.269e-08",
        caption = get_pwc_label(Cr_wilcox)
  )+theme(legend.position='upper right') + myfonts
Cr_PLOT


#Mn#
Mn_Group<- kruskal.test(full_data$Mn ~ full_data$Group)
Mn_Group #Kruskal-Wallis chi-squared = 19.23, df = 2, p-value = 6.674e-05

# Pairwise comparisons
Mn_wilcox <- full_data %>% 
  wilcox_test(Mn ~ Group) 
Mn_wilcox

Mn_wilcox <- Mn_wilcox %>% add_xy_position(x = "Group")

Mn_PLOT<-ggboxplot(full_data, x = "Group", y = "Mn", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Mn Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Mn_wilcox, hide.ns = TRUE) +
  labs( title = "Mn Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=6.674e-05",
        caption = get_pwc_label(Mn_wilcox)
  )+theme(legend.position='upper right') + myfonts
Mn_PLOT


#Fe#
Fe_Group<- kruskal.test(full_data$Fe ~ full_data$Group)
Fe_Group #Kruskal-Wallis chi-squared = 33.861, df = 2, p-value = 4.437e-08

# Pairwise comparisons
Fe_wilcox <- full_data %>% 
  wilcox_test(Fe ~ Group) 
Fe_wilcox

Fe_wilcox <- Fe_wilcox %>% add_xy_position(x = "Group")

Fe_PLOT<-ggboxplot(full_data, x = "Group", y = "Fe", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Fe Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Fe_wilcox, hide.ns = TRUE) +
  labs( title = "Fe Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=4.437e-08",
        caption = get_pwc_label(Fe_wilcox)
  )+theme(legend.position='upper right') + myfonts
Fe_PLOT


#Co#
Co_Group<- kruskal.test(full_data$Co ~ full_data$Group)
Co_Group #Kruskal-Wallis chi-squared = 18.142, df = 2, p-value = 0.000115

# Pairwise comparisons
Co_wilcox <- full_data %>% 
  wilcox_test(Co ~ Group) 
Co_wilcox

Co_wilcox <- Co_wilcox %>% add_xy_position(x = "Group")

Co_PLOT<-ggboxplot(full_data, x = "Group", y = "Co", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Co Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Co_wilcox, hide.ns = TRUE) +
  labs( title = "Co Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=0.000115",
        caption = get_pwc_label(Co_wilcox)
  )+theme(legend.position='upper right') + myfonts
Co_PLOT


#Ni#
Ni_Group<- kruskal.test(full_data$Ni ~ full_data$Group)
Ni_Group #Kruskal-Wallis chi-squared = 59.866, df = 2, p-value = 1.001e-13

# Pairwise comparisons
Ni_wilcox <- full_data %>% 
  wilcox_test(Ni ~ Group) 
Ni_wilcox

Ni_wilcox <- Ni_wilcox %>% add_xy_position(x = "Group")

Ni_PLOT<-ggboxplot(full_data, x = "Group", y = "Ni", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Ni Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Ni_wilcox, hide.ns = TRUE) +
  labs( title = "Ni Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=1.001e-13",
        caption = get_pwc_label(Ni_wilcox)
  )+theme(legend.position='upper right') + myfonts
Ni_PLOT


#Cu#
Cu_Group<- kruskal.test(full_data$Cu ~ full_data$Group)
Cu_Group #Kruskal-Wallis chi-squared = 48.861, df = 2, p-value = 2.454e-11

# Pairwise comparisons
Cu_wilcox <- full_data %>% 
  wilcox_test(Cu ~ Group) 
Cu_wilcox

Cu_wilcox <- Cu_wilcox %>% add_xy_position(x = "Group")

Cu_PLOT<-ggboxplot(full_data, x = "Group", y = "Cu", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Cu Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Cu_wilcox, hide.ns = TRUE) +
  labs( title = "Cu Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=2.454e-11",
        caption = get_pwc_label(Cu_wilcox)
  )+theme(legend.position='upper right') + myfonts
Cu_PLOT


#Zn#
Zn_Group<- kruskal.test(full_data$Zn ~ full_data$Group)
Zn_Group #Kruskal-Wallis chi-squared = 53.509, df = 2, p-value = 2.402e-12

# Pairwise comparisons
Zn_wilcox <- full_data %>% 
  wilcox_test(Zn ~ Group) 
Zn_wilcox

Zn_wilcox <- Zn_wilcox %>% add_xy_position(x = "Group")

Zn_PLOT<-ggboxplot(full_data, x = "Group", y = "Zn", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Zn Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Zn_wilcox, hide.ns = TRUE) +
  labs( title = "Zn Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=2.402e-12",
        caption = get_pwc_label(Zn_wilcox)
  )+theme(legend.position='upper right') + myfonts
Zn_PLOT


#Se#
Se_Group<- kruskal.test(full_data$Se ~ full_data$Group)
Se_Group #Kruskal-Wallis chi-squared = 62.412, df = 2, p-value = 2.801e-14

# Pairwise comparisons
Se_wilcox <- full_data %>% 
  wilcox_test(Se ~ Group) 
Se_wilcox

Se_wilcox <- Se_wilcox %>% add_xy_position(x = "Group")

Se_PLOT<-ggboxplot(full_data, x = "Group", y = "Se", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Se Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Se_wilcox, hide.ns = TRUE) +
  labs( title = "Se Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=2.801e-14",
        caption = get_pwc_label(Se_wilcox)
  )+theme(legend.position='upper right') + myfonts
Se_PLOT


#Sr#
Sr_Group<- kruskal.test(full_data$Sr ~ full_data$Group)
Sr_Group #Kruskal-Wallis chi-squared = 29.028, df = 2, p-value = 4.973e-07

# Pairwise comparisons
Sr_wilcox <- full_data %>% 
  wilcox_test(Sr ~ Group) 
Sr_wilcox

Sr_wilcox <- Sr_wilcox %>% add_xy_position(x = "Group")

Sr_PLOT<-ggboxplot(full_data, x = "Group", y = "Sr", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Sr Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Sr_wilcox, hide.ns = TRUE) +
  labs( title = "Sr Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=4.973e-07",
        caption = get_pwc_label(Sr_wilcox)
  )+theme(legend.position='upper right') + myfonts
Sr_PLOT


#Cd#
Cd_Group<- kruskal.test(full_data$Cd ~ full_data$Group)
Cd_Group #Kruskal-Wallis chi-squared = 21.784, df = 2, p-value = 1.861e-05

# Pairwise comparisons
Cd_wilcox <- full_data %>% 
  wilcox_test(Cd ~ Group) 
Cd_wilcox

Cd_wilcox <- Cd_wilcox %>% add_xy_position(x = "Group")

Cd_PLOT<-ggboxplot(full_data, x = "Group", y = "Cd", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Cd Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Cd_wilcox, hide.ns = TRUE) +
  labs( title = "Cd Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=1.861e-05",
        caption = get_pwc_label(Cd_wilcox)
  )+theme(legend.position='upper right') + myfonts
Cd_PLOT


#Sn#
Sn_Group<- kruskal.test(full_data$Sn ~ full_data$Group)
Sn_Group #Kruskal-Wallis chi-squared = 16.009, df = 2, p-value = 0.000334

# Pairwise comparisons
Sn_wilcox <- full_data %>% 
  wilcox_test(Sn ~ Group) 
Sn_wilcox

Sn_wilcox <- Sn_wilcox %>% add_xy_position(x = "Group")

Sn_PLOT<-ggboxplot(full_data, x = "Group", y = "Sn", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Sn Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Sn_wilcox, hide.ns = TRUE) +
  labs( title = "Sn Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=0.000334",
        caption = get_pwc_label(Sn_wilcox)
  )+theme(legend.position='upper right') + myfonts
Sn_PLOT


#Sb#
Sb_Group<- kruskal.test(full_data$Sb ~ full_data$Group)
Sb_Group #Kruskal-Wallis chi-squared = 39.098, df = 2, p-value = 3.236e-09

# Pairwise comparisons
Sb_wilcox <- full_data %>% 
  wilcox_test(Sb ~ Group) 
Sb_wilcox

Sb_wilcox <- Sb_wilcox %>% add_xy_position(x = "Group")

Sb_PLOT<-ggboxplot(full_data, x = "Group", y = "Sb", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Sb Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Sb_wilcox, hide.ns = TRUE) +
  labs( title = "Sb Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=3.236e-09",
        caption = get_pwc_label(Sb_wilcox)
  )+theme(legend.position='upper right') + myfonts
Sb_PLOT


#Cs#
Cs_Group<- kruskal.test(full_data$Cs ~ full_data$Group)
Cs_Group #Kruskal-Wallis chi-squared = 37.877, df = 2, p-value = 5.957e-09

# Pairwise comparisons
Cs_wilcox <- full_data %>% 
  wilcox_test(Cs ~ Group) 
Cs_wilcox

Cs_wilcox <- Cs_wilcox %>% add_xy_position(x = "Group")

Cs_PLOT<-ggboxplot(full_data, x = "Group", y = "Cs", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Cs Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Cs_wilcox, hide.ns = TRUE) +
  labs( title = "Cs Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=5.957e-09",
        caption = get_pwc_label(Cs_wilcox)
  )+theme(legend.position='upper right') + myfonts
Cs_PLOT


#Ba#
Ba_Group<- kruskal.test(full_data$Ba ~ full_data$Group)
Ba_Group #Kruskal-Wallis chi-squared = 23.993, df = 2, p-value = 6.167e-06

# Pairwise comparisons
Ba_wilcox <- full_data %>% 
  wilcox_test(Ba ~ Group) 
Ba_wilcox

Ba_wilcox <- Ba_wilcox %>% add_xy_position(x = "Group")

Ba_PLOT<-ggboxplot(full_data, x = "Group", y = "Ba", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Ba Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Ba_wilcox, hide.ns = TRUE) +
  labs( title = "Ba Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=6.167e-06",
        caption = get_pwc_label(Ba_wilcox)
  )+theme(legend.position='upper right') + myfonts
Ba_PLOT


#Hg#
Hg_Group<- kruskal.test(full_data$Hg ~ full_data$Group)
Hg_Group #Kruskal-Wallis chi-squared = 41.68, df = 2, p-value = 8.896e-10

# Pairwise comparisons
Hg_wilcox <- full_data %>% 
  wilcox_test(Hg ~ Group) 
Hg_wilcox

Hg_wilcox <- Hg_wilcox %>% add_xy_position(x = "Group")

Hg_PLOT<-ggboxplot(full_data, x = "Group", y = "Hg", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Hg Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Hg_wilcox, hide.ns = TRUE) +
  labs( title = "Hg Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=8.896e-10",
        caption = get_pwc_label(Hg_wilcox)
  )+theme(legend.position='upper right') + myfonts
Hg_PLOT


#Tl#
Tl_Group<- kruskal.test(full_data$Tl ~ full_data$Group)
Tl_Group #Kruskal-Wallis chi-squared = 14.091, df = 2, p-value = 0.0008713

# Pairwise comparisons
Tl_wilcox <- full_data %>% 
  wilcox_test(Tl ~ Group) 
Tl_wilcox

Tl_wilcox <- Tl_wilcox %>% add_xy_position(x = "Group")

Tl_PLOT<-ggboxplot(full_data, x = "Group", y = "Tl", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Tl Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Tl_wilcox, hide.ns = TRUE) +
  labs( title = "Tl Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=0.0008713",
        caption = get_pwc_label(Tl_wilcox)
  )+theme(legend.position='upper right') + myfonts
Tl_PLOT



#Pb#
Pb_Group<- kruskal.test(full_data$Pb ~ full_data$Group)
Pb_Group #Kruskal-Wallis chi-squared = 27.249, df = 2, p-value = 1.21e-06


# Pairwise comparisons
Pb_wilcox <- full_data %>% 
  wilcox_test(Pb ~ Group) 
Pb_wilcox

Pb_wilcox <- Pb_wilcox %>% add_xy_position(x = "Group")

Pb_PLOT<-ggboxplot(full_data, x = "Group", y = "Pb", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "Pb Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(Pb_wilcox, hide.ns = TRUE) +
  labs( title = "Pb Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=1.21e-06",
        caption = get_pwc_label(Pb_wilcox)
  )+theme(legend.position='upper right') + myfonts
Pb_PLOT


#U#
U_Group<- kruskal.test(full_data$U ~ full_data$Group)
U_Group #Kruskal-Wallis chi-squared = 29.237, df = 2, p-value = 4.479e-07

# Pairwise comparisons
U_wilcox <- full_data %>% 
  wilcox_test(U ~ Group) 
U_wilcox

U_wilcox <- U_wilcox %>% add_xy_position(x = "Group")

U_PLOT<-ggboxplot(full_data, x = "Group", y = "U", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "U Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(U_wilcox, hide.ns = TRUE) +
  labs( title = "U Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=4.479e-07",
        caption = get_pwc_label(U_wilcox)
  )+theme(legend.position='upper right') + myfonts
U_PLOT


#As#
As_Group<- kruskal.test(full_data$As ~ full_data$Group)
As_Group #Kruskal-Wallis chi-squared = 18.683, df = 2, p-value = 8.771e-05

# Pairwise comparisons
As_wilcox <- full_data %>% 
  wilcox_test(As ~ Group) 
As_wilcox

As_wilcox <- As_wilcox %>% add_xy_position(x = "Group")

As_PLOT<-ggboxplot(full_data, x = "Group", y = "As", color="Group",palette=c('royalblue4','gold2','green4'),
                   ylab = "As Concentration (ug/g)", xlab = "Group")+ geom_jitter(color="black", size=0.4, alpha=0.9) +
  stat_pvalue_manual(As_wilcox, hide.ns = TRUE) +
  labs( title = "As Concentration by Group" ,
        subtitle = "Kruskal-Wallis, p=8.771e-05",
        caption = get_pwc_label(As_wilcox)
  )+theme(legend.position='upper right') + myfonts
As_PLOT



####LOOK OVER RESULTS####
metals

#ignoring tip and nouragues comparisons

Al_PLOT #notsig
V_PLOT  #sig kruskal, FG>Control, Tip>control
Cr_PLOT #sig kruskal, FG>control
Mn_PLOT #sig kruskal, FG>Control,Tip>control
Fe_PLOT #sig kruskal, FG>Control,Tip>control
Co_PLOT #sig kruskal, FG>Control,Tip>control 
Ni_PLOT #sig kruskal, FG>Control,Tip>control
Cu_PLOT #sig kruskal, FG>Control,Tip>control
Zn_PLOT #sig kruskal, FG>Control,Tip<control
As_PLOT #NOT ENOUGH DATA. sig for ref >tip
Se_PLOT #sig kruskal, FG>control Tip<control
Sr_PLOT #sig kruskal, FG<control
Mo_PLOT #not sig
Ag_PLOT #not sig
Cd_PLOT #sig kruskal, Tip> Control, controll>FG
Sn_PLOT #sig kruskal,  control > fg
Sb_PLOT #sig kruskal, control > tip and Fg
Cs_PLOT #sig kruskal, FG> control, control > tip
Ba_PLOT #sig kruskal, Tip>control, control>FG
Hg_PLOT #sig kruskal, fg > control
Tl_PLOT #sig kruskal, contro>Fg control > ec
Pb_PLOT #sig kruskal, control>tip, fg > control
U_PLOT #sig kruskal, fg>control, tip > controlS



#####PLOTTING ALL EC and FG SEPARATELY#####

#make a dataset with the mean and se per metal per group
Reference<-subset(full_data, Group == "Reference")
Tiputini<-subset(full_data, Group == "Tiputini (near oil)")

#metal means and se for each metal per group
Reference<-Reference[,5:27]
Reference_db<-as.data.frame(metals)
Reference_db$means<-colMeans(Reference[sapply(Reference, is.numeric)])
Reference_db$se<-standard_error <- apply(Reference, 2, function(x) sd(x) / sqrt(length(x)))
Reference_db$Group<-c("Reference")

Tiputini<-Tiputini[,5:27]
Tiputini_db<-as.data.frame(metals)
Tiputini_db$means<-colMeans(Tiputini[sapply(Tiputini, is.numeric)])
Tiputini_db$se<-standard_error <- apply(Tiputini, 2, function(x) sd(x) / sqrt(length(x)))
Tiputini_db$Group<-c("Tiputini (near oil)")

EC_plottingdf<-rbind(Reference_db, Tiputini_db)

#order the groups
EC_plottingdf$Group = factor(EC_plottingdf$Group, levels=c("Tiputini (near oil)", "Reference"))

##split up into two graphs for better axis...###

sig1<-c("Zn","Mn","Fe","Ba", "Cu")
sig2<-c("Ni","Cd","Se","Co","V","As","U")
sig3<-c("Sb","Tl","Cs")

EC_sigplottingdf1<-filter(EC_plottingdf, metals %in% sig1)
EC_sigplottingdf2<-filter(EC_plottingdf, metals %in% sig2)
EC_sigplottingdf3<-filter(EC_plottingdf, metals %in% sig3)

#level the bars
EC_sigplottingdf1$metals <- factor(EC_sigplottingdf1$metals, levels = sig1)
EC_sigplottingdf2$metals <- factor(EC_sigplottingdf2$metals, levels = sig2)
EC_sigplottingdf3$metals <- factor(EC_sigplottingdf3$metals, levels = sig3)

pbar1<- ggplot(EC_sigplottingdf1, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(title="Ecuador Wedgebilled Woodcreeper Feather Metal Concentrations (Kruskal-Wallis p<0.05)", y="Mean Concentration (ug/g)")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2'))+myfonts

pbar1


#####Second graph

pbar2<- ggplot(EC_sigplottingdf2, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(x="metals", y="Mean Concentration (ug/g)")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2'))+
myfonts

pbar2


#####Third graph

pbar3<- ggplot(EC_sigplottingdf3, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(x="metals", y="Mean Concentration (ug/g)")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2'))+
myfonts

pbar3


##PLOT TOGETHER##

ggarrange(pbar1, pbar2, pbar3,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3)




##FRENCH GUIANA##
Reference<-subset(full_data, Group == "Reference")
Nouragues<-subset(full_data, Group == "Nouragues (near gold)")

#metal means and se for each metal per group
Reference<-Reference[,5:27]
Reference_db<-as.data.frame(metals)
Reference_db$means<-colMeans(Reference[sapply(Reference, is.numeric)])
Reference_db$se<-standard_error <- apply(Reference, 2, function(x) sd(x) / sqrt(length(x)))
Reference_db$Group<-c("Reference")

Nouragues<-Nouragues[,5:27]
Nouragues_db<-as.data.frame(metals)
Nouragues_db$means<-colMeans(Nouragues[sapply(Nouragues, is.numeric)])
Nouragues_db$se<-standard_error <- apply(Nouragues, 2, function(x) sd(x) / sqrt(length(x)))
Nouragues_db$Group<-c("Nouragues (near gold)")

FG_plottingdf<-rbind(Reference_db, Nouragues_db)

#order the groups
FG_plottingdf$Group = factor(FG_plottingdf$Group, levels=c("Nouragues (near gold)", "Reference"))


##split up into two graphs for better axis...###
sig1<-c("Zn","Mn","Fe","Cu","Ni")
sig2<-c("Cr","Pb","Se","Co","Cd")
sig3<-c("Hg","V","As","U")
sig4<-c("Ba","Sr","Sn","Sb","Tl","Cs")

FG_sigplottingdf1<-filter(FG_plottingdf, metals %in% sig1)
FG_sigplottingdf2<-filter(FG_plottingdf, metals %in% sig2)
FG_sigplottingdf3<-filter(FG_plottingdf, metals %in% sig3)
FG_sigplottingdf4<-filter(FG_plottingdf, metals %in% sig4)


#level the bars
FG_sigplottingdf1$metals <- factor(FG_sigplottingdf1$metals, levels = sig1)
FG_sigplottingdf2$metals <- factor(FG_sigplottingdf2$metals, levels = sig2)
FG_sigplottingdf3$metals <- factor(FG_sigplottingdf3$metals, levels = sig3)
FG_sigplottingdf4$metals <- factor(FG_sigplottingdf4$metals, levels = sig4)


##first graph

pbar1<- ggplot(FG_sigplottingdf1, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(title="French Guiana Wedgebilled Woodcreeper Feather Metal Concentrations (Kruskal-Wallis p<0.05)", y="Mean Concentration (ug/g)")+
  theme_classic() +
  scale_fill_manual(values=c('green4','gold2'))+
myfonts

pbar1


#####Second graph

pbar2<- ggplot(FG_sigplottingdf2, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(x="metals", y="Mean Concentration (ug/g)")+
  theme_classic() +
  scale_fill_manual(values=c('green4','gold2'))+
myfonts

pbar2


#####Third graph

pbar3<- ggplot(FG_sigplottingdf3, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(x="metals", y="Mean Concentration (ug/g)")+
  theme_classic() +
  scale_fill_manual(values=c('green4','gold2'))+
myfonts

pbar3


#####Fourth graph

pbar4<- ggplot(FG_sigplottingdf4, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(x="metals", y="Mean Concentration (ug/g)")+
  theme_classic() +
  scale_fill_manual(values=c('green4','gold2'))+
myfonts

pbar4

##PLOT TOGETHER##

ggarrange(pbar1, pbar2, pbar3, pbar4,
          labels = c("A", "B", "C","D"),
          ncol = 1, nrow = 4)

ggarrange(pbar1, pbar2, pbar3, pbar4,
          labels = c("A", "B", "C","D"),
          ncol = 1, nrow = 4)




####COMBINED EC AND FG SUMMARY FOR PAPER####
#table
table<-cbind(Tiputini_db, Reference_db, Nouragues_db)
colnames(table)<-c("Metal","Tiputini Mean","Tiputini StdErr","Group","Metal","Reference Mean","Reference StdErr","Group","Metal","Nouragues Mean","Nouragues StdErr","Group")

columns_to_keep <- c("Metal","Tiputini Mean","Tiputini StdErr","Reference Mean","Reference StdErr","Nouragues Mean","Nouragues StdErr")
table <- table[, columns_to_keep, drop = FALSE]

write.csv(table, "HMtable.csv", row.names = FALSE)


combineddf<-rbind(EC_plottingdf, FG_plottingdf)
combineddf$Group = factor(combineddf$Group, levels=c("Tiputini (near oil)", "Reference", "Nouragues (near gold)"))


pbar1<- ggplot(combineddf, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(title="Ecuador Wedgebilled Woodcreeper Feather Metal Concentrations (Kruskal-Wallis p<0.05)", y="Mean Concentration (ug/g)", x="Metals")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2','green4'))+myfonts

pbar1



##split up into two graphs for better axis...###
sig1<-c("Zn","Mn","Fe","Al","Cu")

sig2<-c("Ba","Sr","Ni","Cr","Pb")

sig3<-c("Se","Sn","Cd","Co","Hg","Sb")

sig4<-c("V","Mo","Tl","Cs","Ag","As","U")

combineddf1<-filter(combineddf, metals %in% sig1)
combineddf2<-filter(combineddf, metals %in% sig2)
combineddf3<-filter(combineddf, metals %in% sig3)
combineddf4<-filter(combineddf, metals %in% sig4)


# #level the bars
combineddf1$metals <- factor(combineddf1$metals, levels = sig1)
combineddf2$metals <- factor(combineddf2$metals, levels = sig2)
combineddf3$metals <- factor(combineddf3$metals, levels = sig3)
combineddf4$metals <- factor(combineddf4$metals, levels = sig4)


###### First graph

pbar1<- ggplot(combineddf1, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs( y="Mean Concentration (ug/g)", x="Metals")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2','green4'))+myfonts
pbar1



#####Second graph

pbar2<- ggplot(combineddf2, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs( y="Mean Concentration (ug/g)", x="Metals")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2','green4'))+myfonts
pbar2


#####Third graph

pbar3<- ggplot(combineddf3, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(y="Mean Concentration (ug/g)", x="Metals")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2','green4'))+myfonts
pbar3


#####Fourth graph

pbar4<- ggplot(combineddf4, aes(x=metals, y=means, fill=Group)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=means-se, ymax=means+se), width=.2,
                position=position_dodge(.9)) +
  labs(y="Mean Concentration (ug/g)", x="Metals")+
  theme_classic() +
  scale_fill_manual(values=c('royalblue4','gold2','green4'))+myfonts
pbar4


ggarrange(pbar1, pbar2, pbar3, pbar4,
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")


ggsave("Figure2.png", width = 15, height = 10, units = "in")




####SPATIAL DATA FOR GROUPING...TIPUTINI IS HIGH RISK COMPARED TO CONTROLS####

#READ IN OIL DATA FROM OUR WORLD IN DATA (SPECIAL PERMISSION FOR THIS DATA- ALREADY PRUNED FOR ECUADOR ONLY)
oil <- read_excel('~/Library/CloudStorage/GoogleDrive-yl0518@princeton.edu/.shortcut-targets-by-id/1LJjchwY-EphSn7Gv624wVwwe8gEtAHn_/Ecuador Oil Pollution Project/Raw Data/Oil Data/Oil_data_Ecuador_GEM.xlsx')

plot(Latitude ~ Longitude, oil)


#Subset Ecuador
oil_aoi <- oil[oil$Country == 'Ecuador',]
summary(oil_aoi)

#remove datapoints with missing data (none)
oil_aoi<-oil_aoi[!(is.na(oil_aoi$Latitude)),]
summary(oil_aoi)

#rename
oil<-oil_aoi

# transform the dataframe to a "track" which is preferred by amt package 
#EPSG:4326, also known as the WGS84 projection is a coordinate system used in Google Earth and GSP systems
oil_amt <- make_track(oil, .x = Longitude, .y = Latitude, crs = 'EPSG:4326' )

summary(oil_amt)

# project data from WGS1984 to UTM Zone 17S (Ecuador-based mapping)
oil_amt_utm <- transform_coords(oil_amt, crs_from = 4326, crs_to = 32717)

# make a kerenel density map from the oil lat and longs
oil_kde <- hr_kde(oil_amt_utm)

#plot the "utilization distribution"
plot(oil_kde$ud)

#get full dataset
samples<-full_data

#subset by Ecuador samples
samples<-samples[samples$`Country.code`=="Ecuador",]

summary(as.numeric(samples$Lat))

#indicate lat and long
samples$Lat<-as.numeric(samples$Lat)
samples$Lon<-as.numeric(samples$Lon)

# make spatial data in coordiinate system used before (long, lat)
samples_sp <- SpatialPointsDataFrame(samples[,c(42,41)],
                                     samples,
                                     proj4string = CRS('EPSG:4326'))
plot(samples_sp)

# transform to UTM Zone 17S Ecador
samples_utm <- spTransform(samples_sp, CRS('EPSG:32717'))
plot(samples_utm)
colnames(samples_utm@coords)

# transform kde to raster
oil_rast <- raster(oil_kde$ud)

# extract value of the kernel denisty estimation for the sample points (long,lat)
samples_utm$kde_values <- raster::extract(oil_rast, samples_utm@coords[,c(1,2)])
summary(samples_utm)
samples_utm@data

listofkde<-as.data.frame(samples_utm$kde_values)
listofkde

plot(oil_rast)
points(samples_utm)

#add Ecuador outline
#read in ecuador map
ecmap<-readOGR("~/Library/CloudStorage/GoogleDrive-yl0518@princeton.edu/.shortcut-targets-by-id/1LJjchwY-EphSn7Gv624wVwwe8gEtAHn_/Ecuador Oil Pollution Project/Code & Results/EC_Shapefile/sb978rn7613.shp")

#what system is it in
proj4string(ecmap)

#transform to CRS zone 17 used before
ec_projected<- spTransform(ecmap, CRS('EPSG:32717'))

#now in zone 17 UTM ecuador projected
proj4string(ec_projected)

#plot heatmap then ecuador outline then points
plot(oil_rast)
plot(ec_projected, add=TRUE)
plot(samples_utm, add=TRUE)

#save
pdf("~/Library/CloudStorage/GoogleDrive-yl0518@princeton.edu/.shortcut-targets-by-id/1LJjchwY-EphSn7Gv624wVwwe8gEtAHn_/Ecuador Oil Pollution Project/Code & Results/Figures/Rasterizedplot.pdf")
plot(oil_rast )
plot(ec_projected, add=TRUE)
plot(samples_utm, add=TRUE)
dev.off()

#instead of plotting in r, you can write out the files and load into acrmap
#writeRaster(oil_rast, "oil_raster.tif")
#shapefile(samples_utm, "sampledata.shp")
#shapefile(ec_projected, "ecmapdata.shp")



#####PLOTTING KDE MAP#####
#plotting in r using rastervis
pdf("~/Library/CloudStorage/GoogleDrive-yl0518@princeton.edu/.shortcut-targets-by-id/1LJjchwY-EphSn7Gv624wVwwe8gEtAHn_/Ecuador Oil Pollution Project/Code & Results/Figures/KDPLOT.pdf")
masked_oil<- mask(oil_rast, ec_projected)
p <- levelplot(masked_oil, layers = 1, margin=T, contour=TRUE, colorkey = list(title = "kde") , main = "Kernel Density Plot of Oil Fields in Ecuador",  xlab="Longitude (UTM 17N)", ylab="Latitude (UTM 17N)")
p + latticeExtra::layer(sp.lines(ec_projected, col="white", lwd=0.5)) + latticeExtra::layer(sp.lines(samples_utm, col="white", lwd=0.5)) 


dev.off()

##calculating distance from kernel center
test<-as.data.frame(oil_kde$data)
test2<-as.data.frame(oil_kde$ud)
test3<-cbind(test,test2)
test4<-test3[which.max(test3$lyr.1),]

#largest kernel ud value
test4

#x and y location for that max ud value
x<-as.numeric(960578.9)
y<-as.numeric(9964177)

#make spatial point for that coordinate 
largestkde<-SpatialPoints(coords=test4[,1:2],CRS('EPSG:32717'))
largestkde

#gdistance from each site to kernel peak
distances<-gDistance(largestkde,samples_utm, byid=TRUE)

#get the distances for each location, then do an anova across all sites by full heavy metal load (boxplot) thena. linear model of averaged heavy metal load within a pop and the KDE distance values as a scatterplot. 
samples_utm$distances<-distances

samples_df<-samples_utm@data

colnames(samples_df)

#look at city distances

citydistances<-samples_df %>% distinct(`City/Town`, .keep_all=TRUE)
citydistances<-cbind(citydistances$`City/Town`, citydistances$distances)
citydistances

# "Sangay National Park Macas" "259783.704607078" meters
# "Miazal"                     "276488.84296328"  meters
# "Tiputini"                   "72177.4837829124" meters
# "Hollin River"               "104363.594230283" meters
# "San Rafael Falls"           "83560.0101596611" meters



####KDE AND METAL ANALYSIS####

#rename
all_data<- samples_df

###Analysis###

##Linear mixed effects model of how KDE affects full load by site
all_data

#lm of full indiv load sig to KDE (p= 0.03557)
linearmodel<- lm(Full_load ~ kde_values, data = all_data)
summary(linearmodel)
linearmodel
# Residual standard error: 181.2 on 69 degrees of freedom
# Multiple R-squared:  0.06245,	Adjusted R-squared:  0


##Pearsons correlation coefficient of how KDE and full load are related
cortest <- cor.test(all_data$Full_load, all_data$kde_values, 
                    method = "pearson")
cortest
# t = 2.1439, df = 69, p-value = 0.03557
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.01762983 0.45658851
# sample estimates:
#   cor :0.2499057


#plot#
all_data$Location.Code <- factor(all_data$Location.Code , levels=c("Tiputini","Hollin","San Rafael","Sangay","Miazal"))
unique(all_data$Location.Code)


#scatterplot
p <- ggplot(all_data, aes(x=kde_values, y=Full_load, color = Location.Code)) +
  geom_point() + 
 # scale_color_manual(values=c("red3","violetred3","orchid4","darkorchid4", "black"))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  theme_classic()+ 
  labs(title = "LMM Model of Full Contamination Load by KDE Values per Site" ,  subtitle = "r2=0.06245, p-value=0.03557", x="KDE Value", y="Full HM Load")

#manually add the lmm regression line
p + xlim(0,2.352535e-11) +geom_abline(intercept = 3.969e+02, slope = 0.0553,, color="grey", linetype="dashed") 


#####ALL by POP / KDE####

#aov by experimental group
All_aov <- aov(Full_load ~ Experimental.Notes, data=all_data)
summary(All_aov)

#perform Bartlett's test for unequal variances...shows sig different variances!
bartlett.test(all_data$Full_load ~ all_data$Location.Code)
#Bartlett's K-squared = 30.851, df = 4, p-value = 3.283e-06

#welch's anova for unequal variances...
All_aov <- oneway.test(Full_load ~ Location.Code, data=all_data, var.equal = FALSE)
All_aov

#posthoc test!
#Games-Howell multiple comparisons method.The Games-Howell post hoc test, like Welch’s analysis of variance, does not require the groups to have equal standard deviations.
all_data %>% games_howell_test(Full_load ~ Location.Code)

#regular anova...not meaninful here...
All_aov <- aov(Full_load ~ Location.Code, data=all_data)
All_aov

# Pairwise comparisons
All_post<-all_data %>% games_howell_test(Full_load ~ Location.Code)
All_post

All_post <-  All_post %>% mutate(y.position = "y.position")

All_plot<-ggboxplot(all_data, x = "Location.Code", y = "Full_load", color="kde_values",
          xlab = "Site",ylab = "Mean Heavy Metal Load (ug/g)") + stat_pvalue_manual(All_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+ labs(title = "Mean Heavy Metal Load by Site (KDE values)" ,subtitle = "AOV, F=4.139, p=0.0457",
       caption = get_pwc_label(All_post))+theme(legend.position="right") + gradient_color(c("purple4", "red3"))+myfonts

All_plot
ggsave("kdeplot.png", width = 8, height= 6, units = "in")

#no sig differences in full load across pops

All_plot<-ggplot(all_data, aes(x = kde_values, y = Full_load, color=Location.Code))+
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  labs(title = "Mean Heavy Metal Load by Site (KDE values)" ,
       x = "KDE Values",y = "Mean Heavy Metal Load (ug/g)", subtitle = "AOV, p=0.0254")+theme(legend.position="right")

All_plot



#####DO THIS FOR ALL METALS##
##do this for each metals to see if they are related to kde values
metals<-colnames(all_data[6:28])
metals


##V sig (tip-hol, tip-sang, tip-mia)
#welch's anova for unequal variances...
V_aov <- oneway.test(V ~ Location.Code, data=all_data, var.equal = FALSE)
V_aov
V_post<-all_data %>% games_howell_test(V ~ Location.Code)
V_post

max(all_data$V)

V_post <- V_post  %>% mutate(y.position = c(0.6,0.62,0.64,0.68,0.70,0.72,0.74,0.76, 0.78, 0.80))
V_plot<-ggboxplot(all_data, x = "Location.Code", y = "V", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "V (ug/g)") + stat_pvalue_manual(V_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(V_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Co sig (tip-sang, tip-mia)
#welch's anova for unequal variances...
Co_aov <- oneway.test(Co ~ Location.Code, data=all_data, var.equal = FALSE)
Co_aov
Co_post<-all_data %>% games_howell_test(Co ~ Location.Code)
Co_post

max(all_data$Co)

Co_post <- Co_post  %>% mutate(y.position = c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9))
Co_plot<-ggboxplot(all_data, x = "Location.Code", y = "Co", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Co (ug/g)") + stat_pvalue_manual(Co_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Co_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Al not sig
#welch's anoAla for unequal Alariances...
Al_aov <- oneway.test(Al ~ Location.Code, data=all_data, var.equal = FALSE)
Al_post<-all_data %>% games_howell_test(Al ~ Location.Code)
Al_post

max(all_data$Al)

Al_post <- Al_post  %>% mutate(y.position = c(140))
Al_plot<-ggboxplot(all_data, x = "Location.Code", y = "Al", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Al (ug/g)") + stat_pvalue_manual(Al_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Al_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Cr not sig
#welch's anova for unequal variances...
Cr_aov <- oneway.test(Cr ~ Location.Code, data=all_data, var.equal = FALSE)
Cr_post<-all_data %>% games_howell_test(Cr ~ Location.Code)
Cr_post

max(all_data$Cr)

Cr_post <- Cr_post  %>% mutate(y.position = c(3))
Cr_plot<-ggboxplot(all_data, x = "Location.Code", y = "Cr", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Cr (ug/g)") + stat_pvalue_manual(Cr_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Cr_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Mn tip-sanr, tip-sang
#welch's anova for unequal variances...
Mn_aov <- oneway.test(Mn ~ Location.Code, data=all_data, var.equal = FALSE)
Mn_post<-all_data %>% games_howell_test(Mn ~ Location.Code)
Mn_post

max(all_data$Mn)

Mn_post <- Mn_post  %>% mutate(y.position = c(610,620,630,640,650,660,670,680,690,700))
Mn_plot<-ggboxplot(all_data, x = "Location.Code", y = "Mn", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Mn (ug/g)") + stat_pvalue_manual(Mn_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Mn_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Fe not sig
#welch's anova for unequal variances...
Fe_aov <- oneway.test(Fe ~ Location.Code, data=all_data, var.equal = FALSE)
Fe_post<-all_data %>% games_howell_test(Fe ~ Location.Code)
Fe_post

max(all_data$Fe)

Fe_post <- Fe_post  %>% mutate(y.position = c(200))
Fe_plot<-ggboxplot(all_data, x = "Location.Code", y = "Fe", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Fe (ug/g)") + stat_pvalue_manual(Fe_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Fe_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Ni sig (tip-hol, tip-sanr, tip-sang)
#welch's anova for unequal variances...
Ni_aov <- oneway.test(Ni ~ Location.Code, data=all_data, var.equal = FALSE)
Ni_post<-all_data %>% games_howell_test(Ni ~ Location.Code)
Ni_post

max(all_data$Ni)

Ni_post <- Ni_post  %>% mutate(y.position = c(6.5,7,7.5,8,8.5,9,9.5,10,10.5,11))
Ni_plot<-ggboxplot(all_data, x = "Location.Code", y = "Ni", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Ni (ug/g)") + stat_pvalue_manual(Ni_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Ni_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Cu sig (tip-hol)
#welch's anova for unequal variances...
Cu_aov <- oneway.test(Cu ~ Location.Code, data=all_data, var.equal = FALSE)
Cu_post<-all_data %>% games_howell_test(Cu ~ Location.Code)
Cu_post

max(all_data$Cu)

Cu_post <- Cu_post  %>% mutate(y.position = c(40))
Cu_plot<-ggboxplot(all_data, x = "Location.Code", y = "Cu", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Cu (ug/g)") + stat_pvalue_manual(Cu_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Cu_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Zn sig (tip-hol, tip-sanr)
#welch's anova for unequal variances...
Zn_aov <- oneway.test(Zn ~ Location.Code, data=all_data, var.equal = FALSE)
Zn_post<-all_data %>% games_howell_test(Zn ~ Location.Code)
Zn_post

max(all_data$Zn)

Zn_post <- Zn_post  %>% mutate(y.position = c(310,320,330,340,350,360,370,380,390,400))
Zn_plot<-ggboxplot(all_data, x = "Location.Code", y = "Zn", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Zn (ug/g)") + stat_pvalue_manual(Zn_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Zn_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


# ##As= NA
# #welch's anova for unequal variances...
# As_aov <- oneway.test(As ~ Location.Code, data=all_data, var.equal = FALSE)
# As_post<-all_data %>% games_howell_test(As ~ Location.Code)
# As_post
# 
# As_post <- As_post  %>% mutate(y.position = c(0.5,0.52,0.54,0.58,0.60,0.62,0.64,0.66, 0.68, 0.70))
# As_plot<-ggboxplot(all_data, x = "Location.Code", y = "As", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
#           xlab = "KDE Values",ylab = "As (ug/g)") + stat_pvalue_manual(As_post, label = "p.adj.signif", hide.ns = TRUE) +
#   geom_jitter(color="black", size=0.4, alpha=0.9)+
#   labs(title = "Full HM load by site (KDE values)" ,
#        caption = get_pwc_label(As_post)
#   )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Se sig (tip-hol)
#welch's anova for unequal variances...
Se_aov <- oneway.test(Se ~ Location.Code, data=all_data, var.equal = FALSE)
Se_post<-all_data %>% games_howell_test(Se ~ Location.Code)
Se_post

max(all_data$Se)

Se_post <- Se_post  %>% mutate(y.position = c(1.5))
Se_plot<-ggboxplot(all_data, x = "Location.Code", y = "Se", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Se (ug/g)") + stat_pvalue_manual(Se_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Se_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Sr sig (tip-hol,hol-mia)
#welch's anova for unequal variances...
Sr_aov <- oneway.test(Sr ~ Location.Code, data=all_data, var.equal = FALSE)
Sr_post<-all_data %>% games_howell_test(Sr ~ Location.Code)
Sr_post

max(all_data$Sr)

Sr_post <- Sr_post  %>% mutate(y.position = c(75,77,79,81,83,85,87,89,91,93))
Sr_plot<-ggboxplot(all_data, x = "Location.Code", y = "Sr", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Sr (ug/g)") + stat_pvalue_manual(Sr_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Sr_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Mo not sig
#welch's anova for unequal variances...
Mo_aov <- oneway.test(Mo ~ Location.Code, data=all_data, var.equal = FALSE)
Mo_post<-all_data %>% games_howell_test(Mo ~ Location.Code)
Mo_post

max(all_data$Mo)

Mo_post <- Mo_post  %>% mutate(y.position = c(0.5,0.52,0.54,0.58,0.60,0.62,0.64,0.66, 0.68, 0.70))
Mo_plot<-ggboxplot(all_data, x = "Location.Code", y = "Mo", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Mo (ug/g)") + stat_pvalue_manual(Mo_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Mo_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Ag not sig
#welch's anova for unequal variances...
Ag_aov <- oneway.test(Ag ~ Location.Code, data=all_data, var.equal = FALSE)
Ag_post<-all_data %>% games_howell_test(Ag ~ Location.Code)
Ag_post

max(all_data$Ag)

Ag_post <- Ag_post  %>% mutate(y.position = c(0.5,0.52,0.54,0.58,0.60,0.62,0.64,0.66, 0.68, 0.70))
Ag_plot<-ggboxplot(all_data, x = "Location.Code", y = "Ag", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Ag (ug/g)") + stat_pvalue_manual(Ag_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Ag_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Cd sig (tip-hol, tip-sanr, tip-sang)
#welch's anova for unequal variances...
Cd_aov <- oneway.test(Cd ~ Location.Code, data=all_data, var.equal = FALSE)
Cd_post<-all_data %>% games_howell_test(Cd ~ Location.Code)
Cd_post

max(all_data$Cd)

Cd_post <- Cd_post  %>% mutate(y.position = c(5,5.5,6,6.5,7,7.5,8,8.5,9,9.5))
Cd_plot<-ggboxplot(all_data, x = "Location.Code", y = "Cd", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Cd (ug/g)") + stat_pvalue_manual(Cd_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Cd_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Sn sig (hol-mia)
#welch's anova for unequal variances...
Sn_aov <- oneway.test(Sn ~ Location.Code, data=all_data, var.equal = FALSE)
Sn_post<-all_data %>% games_howell_test(Sn ~ Location.Code)
Sn_post

max(all_data$Sn)

Sn_post <- Sn_post  %>% mutate(y.position = c(5,5.5,6,6.5,7,7.5,8,8.5,9,9.5))
Sn_plot<-ggboxplot(all_data, x = "Location.Code", y = "Sn", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Sn (ug/g)") + stat_pvalue_manual(Sn_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Sn_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Sb sig (tip-hol, tip-sanr, tip-sang, hol-mia, sanr-mia, sang-mia)
#welch's anova for unequal variances...
Sb_aov <- oneway.test(Sb ~ Location.Code, data=all_data, var.equal = FALSE)
Sb_post<-all_data %>% games_howell_test(Sb ~ Location.Code)
Sb_post

max(all_data$Sb)

Sb_post <- Sb_post  %>% mutate(y.position = c(0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7))
Sb_plot<-ggboxplot(all_data, x = "Location.Code", y = "Sb", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Sb (ug/g)") + stat_pvalue_manual(Sb_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Sb_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Cs sig (tip-hol, tip-sanr, tip-sang, hol-mia,sanr-mia, sang-mia)
#welch's anova for unequal variances...
Cs_aov <- oneway.test(Cs ~ Location.Code, data=all_data, var.equal = FALSE)
Cs_post<-all_data %>% games_howell_test(Cs ~ Location.Code)
Cs_post

max(all_data$Cs)

Cs_post <- Cs_post  %>% mutate(y.position = c(0.1,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19))
Cs_plot<-ggboxplot(all_data, x = "Location.Code", y = "Cs", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Cs (ug/g)") + stat_pvalue_manual(Cs_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Cs_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Ba sig (tip-sanrl, tip-sang, sang-hol)
#welch's anova for unequal variances...
Ba_aov <- oneway.test(Ba ~ Location.Code, data=all_data, var.equal = FALSE)
Ba_post<-all_data %>% games_howell_test(Ba ~ Location.Code)
Ba_post

max(all_data$Ba)

Ba_post <- Ba_post  %>% mutate(y.position = c(200,205,210,215,220,225,230,235,240,245))
Ba_plot<-ggboxplot(all_data, x = "Location.Code", y = "Ba", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Ba (ug/g)") + stat_pvalue_manual(Ba_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Ba_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Hg sig (hol-mia, sang-mia)
#welch's anova for unequal variances...
Hg_aov <- oneway.test(Hg ~ Location.Code, data=all_data, var.equal = FALSE)
Hg_post<-all_data %>% games_howell_test(Hg ~ Location.Code)
Hg_post

max(all_data$Hg)

Hg_post <- Hg_post  %>% mutate(y.position = c(0.5,0.52,0.54,0.58,0.60,0.62,0.64,0.66, 0.68, 0.70))
Hg_plot<-ggboxplot(all_data, x = "Location.Code", y = "Hg", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Hg (ug/g)") + stat_pvalue_manual(Hg_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Hg_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Tl sig ((tip-hol), tip-sang, sang-mia)
#welch's anova for unequal variances...
Tl_aov <- oneway.test(Tl ~ Location.Code, data=all_data, var.equal = FALSE)
Tl_post<-all_data %>% games_howell_test(Tl ~ Location.Code)
Tl_post

max(all_data$Tl)

Tl_post <- Tl_post  %>% mutate(y.position = c(0.72,0.73,0.74,0.75,0.76,0.77,0.78,0.79,0.80,0.81))
Tl_plot<-ggboxplot(all_data, x = "Location.Code", y = "Tl", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Tl (ug/g)") + stat_pvalue_manual(Tl_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Tl_post)
  )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


##Pb not sig
#welch's anova for unequal variances...
Pb_aov <- oneway.test(Pb ~ Location.Code, data=all_data, var.equal = FALSE)
Pb_post<-all_data %>% games_howell_test(Pb ~ Location.Code)
Pb_post

max(all_data$Pb)

Pb_post <- Pb_post  %>% mutate(y.position = c(10))
Pb_plot<-ggboxplot(all_data, x = "Location.Code", y = "Pb", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
          xlab = "KDE Values",ylab = "Pb (ug/g)") + stat_pvalue_manual(Pb_post, label = "p.adj.signif", hide.ns = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  labs(title = "Full HM load by site (KDE values)" ,
       caption = get_pwc_label(Pb_post)
  ) +theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


# ##U sig NA
# #welch's anova for unequal variances...
# U_aov <- oneway.test(U ~ Location.Code, data=all_data, var.equal = FALSE)
# U_post<-all_data %>% games_howell_test(U ~ Location.Code)
# U_post
# 
# 
# U_post <- U_post  %>% mutate(y.position = c(0.5,0.52,0.54,0.58,0.60,0.62,0.64,0.66, 0.68, 0.70))
# U_plot<-ggboxplot(all_data, x = "Location.Code", y = "U", color="kde_values",title = "T test of full contamination load by KDE values per site" ,
#           xlab = "KDE Values",ylab = "U (ug/g)") + stat_pvalue_manual(U_post, label = "p.adj.signif", hide.ns = TRUE) +
#   geom_jitter(color="black", size=0.4, alpha=0.9)+
#   labs(title = "Full HM load by site (KDE values)" ,
#        caption = get_pwc_label(U_post)
#   )+theme(legend.position="right")+ gradient_color(c("purple4", "red3"))


###ALL PLOTS

All_plot

Al_plot
V_plot
Cr_plot
Mn_plot
Fe_plot
Co_plot
Ni_plot
Cu_plot
Zn_plot
As_plot # too many NA's
Se_plot
Sr_plot
Mo_plot
Ag_plot
Cd_plot
Sn_plot
Sb_plot
Cs_plot
Ba_plot
Hg_plot
Tl_plot
Pb_plot
U_plot # too many NA's


# #quickplots
# metals
# 
# ggboxplot(all_data, x = "Location.Code", y = "Al", color="Location.Code") #kde trend
# ggboxplot(all_data, x = "Location.Code", y = "V", color="Location.Code") #san rafael is high then according to kde
# ggboxplot(all_data, x = "Location.Code", y = "Cr", color="Location.Code") #opposite of kde?
# ggboxplot(all_data, x = "Location.Code", y = "Mn", color="Location.Code") #tip>miazal>hollin>sangay>sanrafael
# ggboxplot(all_data, x = "Location.Code", y = "Fe", color="Location.Code") #same as mn
# ggboxplot(all_data, x = "Location.Code", y = "Co", color="Location.Code") #tiputini is super high then the rest lowww
# ggboxplot(all_data, x = "Location.Code", y = "Ni", color="Location.Code") #tip, miazzal, sangay, sanrafael, hollin
# ggboxplot(all_data, x = "Location.Code", y = "Cu", color="Location.Code") #tiputini and miazal, then the rest
# ggboxplot(all_data, x = "Location.Code", y = "Zn", color="Location.Code") #sanrafael, then hollin and sngay then tip and miz
# ggboxplot(all_data, x = "Location.Code", y = "As", color="Location.Code") #high san rafael then hollin and sangay and the rest
# ggboxplot(all_data, x = "Location.Code", y = "Se", color="Location.Code") #rafael then hollin and miazal sangau then tiputini
# ggboxplot(all_data, x = "Location.Code", y = "Sr", color="Location.Code") #tip and hollin then the rest down kde
# ggboxplot(all_data, x = "Location.Code", y = "Mo", color="Location.Code") #san rafael then hollin, tip, sanga, miaz
# ggboxplot(all_data, x = "Location.Code", y = "Ag", color="Location.Code") #sang, rafael and miazal, then tip and hollin
# ggboxplot(all_data, x = "Location.Code", y = "Cd", color="Location.Code") #tip and miazal, then the rest
# ggboxplot(all_data, x = "Location.Code", y = "Sn", color="Location.Code") #all about the same
# ggboxplot(all_data, x = "Location.Code", y = "Sb", color="Location.Code") #hollin san rafael an sangau high and tip and miaz low
# ggboxplot(all_data, x = "Location.Code", y = "Cs", color="Location.Code") #hollin sanaf and sang high then tip an dmiaz
# ggboxplot(all_data, x = "Location.Code", y = "Ba", color="Location.Code") #hollin, tip, then miazal and rest
# ggboxplot(all_data, x = "Location.Code", y = "Hg", color="Location.Code") #hollin,sanraf, sangay, miaz
# ggboxplot(all_data, x = "Location.Code", y = "Tl", color="Location.Code") #all low
# ggboxplot(all_data, x = "Location.Code", y = "Pb", color="Location.Code") #all low
# ggboxplot(all_data, x = "Location.Code", y = "U", color="Location.Code") #tiputini high then rest




#####EACH METAL BY RISK GROUPS (high medium low)#####

##Add Risk category by KDE
#add a new group by Kernel density distances (hollin and sanrafael as "medium risk" and miazal and sangay as "low risk")
Location.Code<-c("Tiputini", "Hollin", "San Rafael","Sangay", "Miazal")
Risks<-c("High","Medium","Medium","Low","Low")
df<-data.frame(Location.Code,Risks)
full_data<-merge(all_data, df, by="Location.Code")
##THIS IS ONLY FOR ECUADOR

#All weighted by featherweight 
All_kruskal_weighted <- weighted_mannwhitney(all_data, Full_load,Risks, Feather.Weight)
All_kruskal_weighted

All_kruskal <- all_data %>% kruskal_test(Full_load ~ Risks)
All_kruskal

# Pairwise comparisons
All_wilcox <- all_data %>% 
  wilcox_test(Full_load ~ Risks, p.adjust.method = "bonferroni") 
All_wilcox

ggboxplot(all_data, x = "Risks", y = "Full_load")

pdf("Figures/All_Kruskalgroup.pdf")

All_wilcox <-  All_wilcox %>% mutate(y.position = c(4000, 4000, 4000))

ggboxplot(all_data, x = "Risks", y = "Full_load",
          ylab = "Full Heavy Metal Load", xlab = "Risks") + stat_pvalue_manual(All_wilcox, label = "p.adj", hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = All_kruskal_weighted$p.value,
       caption = get_pwc_label(All_wilcox)
       
  )
dev.off()



###ELEMENTS BY RISK GROUPS###

##TEST SIG > CONTROL##

##Co##
EC_Co_kruskal_weighted <- weighted_mannwhitney(EC_data, Co,Risks, Feather.Weight)
EC_Co_kruskal_weighted

EC_Co_kruskal <- EC_data %>% kruskal_test(Co ~ Risks)
EC_Co_kruskal

# Pairwise comparisons
EC_Co_wilcox <- EC_data %>% 
  wilcox_test(Co ~ Risks, p.adjust.method = "bonferroni") 
EC_Co_wilcox

pdf("Figures/EC_Co_KruskalRisks.pdf")
EC_Co_wilcox <- EC_Co_wilcox %>% mutate(y.position = 1,2,3)
ggboxplot(EC_data, x = "Risks", y = "Co",
          ylab = "Co Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Co_wilcox, hide.ns = TRUE,  y.position = "y.position") +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Co_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Co_wilcox)
  )
dev.off()

EC_Co_wilcox <- EC_Co_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Co",
          ylab = "Co Concentration", xlab = "Risks") +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Co_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Co_wilcox)
  )


##Fe##
EC_Fe_kruskal_weighted <- weighted_mannwhitney(EC_data, Fe,Risks, Feather.Weight)
EC_Fe_kruskal_weighted

EC_Fe_kruskal <- EC_data %>% kruskal_test(Fe ~ Risks)
EC_Fe_kruskal

# Pairwise comparisons
EC_Fe_wilcox <- EC_data %>% 
  wilcox_test(Fe ~ Risks, p.adjust.method = "bonferroni") 
EC_Fe_wilcox

pdf("Figures/EC_Fe_KruskalRisks.pdf")
EC_Fe_wilcox <- EC_Fe_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Fe",
          ylab = "Fe Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Fe_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Fe_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Fe_wilcox)
  )
dev.off()

EC_Fe_wilcox <- EC_Fe_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Fe",
          ylab = "Fe Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Fe_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Fe_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Fe_wilcox)
  )


##Ni##
EC_Ni_kruskal_weighted <- weighted_mannwhitney(EC_data, Ni,Risks, Feather.Weight)
EC_Ni_kruskal_weighted

EC_Ni_kruskal <- EC_data %>% kruskal_test(Ni ~ Risks)
EC_Ni_kruskal

# Pairwise comparisons
EC_Ni_wilcox <- EC_data %>% 
  wilcox_test(Ni ~ Risks, p.adjust.method = "bonferroni") 
EC_Ni_wilcox

pdf("Figures/EC_Ni_KruskalRisks.pdf")
EC_Ni_wilcox <- EC_Ni_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Ni",
          ylab = "Ni Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Ni_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Ni_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Ni_wilcox)
  )
dev.off()

EC_Ni_wilcox <- EC_Ni_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Ni",
          ylab = "Ni Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Ni_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Ni_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Ni_wilcox)
  )

##V##
EC_V_kruskal_weighted <- weighted_mannwhitney(EC_data, V,Risks, Feather.Weight)
EC_V_kruskal_weighted

EC_V_kruskal <- EC_data %>% kruskal_test(V ~ Risks)
EC_V_kruskal

# Pairwise comparisons
EC_V_wilcox <- EC_data %>% 
  wilcox_test(V ~ Risks, p.adjust.method = "bonferroni") 
EC_V_wilcox

pdf("Figures/EC_V_KruskalRisks.pdf")
EC_V_wilcox <- EC_V_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "V",
          ylab = "V Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_V_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_V_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_V_wilcox)
  )
dev.off()

EC_V_wilcox <- EC_V_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "V",
          ylab = "V Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_V_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_V_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_V_wilcox)
  )


##Cu##
EC_Cu_kruskal_weighted <- weighted_mannwhitney(EC_data, Cu,Risks, Feather.Weight)
EC_Cu_kruskal_weighted

EC_Cu_kruskal <- EC_data %>% kruskal_test(Cu ~ Risks)
EC_Cu_kruskal

# Pairwise comparisons
EC_Cu_wilcox <- EC_data %>% 
  wilcox_test(Cu ~ Risks, p.adjust.method = "bonferroni") 
EC_Cu_wilcox

pdf("Figures/EC_Cu_KruskalRisks.pdf")
EC_Cu_wilcox <- EC_Cu_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cu",
          ylab = "Cu Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cu_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cu_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cu_wilcox)
  )
dev.off()

EC_Cu_wilcox <- EC_Cu_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cu",
          ylab = "Cu Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cu_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cu_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cu_wilcox)
  )



##Ba##
EC_Ba_kruskal_weighted <- weighted_mannwhitney(EC_data, Ba,Risks, Feather.Weight)
EC_Ba_kruskal_weighted

EC_Ba_kruskal <- EC_data %>% kruskal_test(Ba ~ Risks)
EC_Ba_kruskal

# Pairwise comparisons
EC_Ba_wilcox <- EC_data %>% 
  wilcox_test(Ba ~ Risks, p.adjust.method = "bonferroni") 
EC_Ba_wilcox

pdf("Figures/EC_Ba_KruskalRisks.pdf")
EC_Ba_wilcox <- EC_Ba_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Ba",
          ylab = "Ba Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Ba_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Ba_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Ba_wilcox)
  )
dev.off()

EC_Ba_wilcox <- EC_Ba_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Ba",
          ylab = "Ba Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Ba_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Ba_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Ba_wilcox)
  )


##Cd##
EC_Cd_kruskal_weighted <- weighted_mannwhitney(EC_data, Cd,Risks, Feather.Weight)
EC_Cd_kruskal_weighted

EC_Cd_kruskal <- EC_data %>% kruskal_test(Cd ~ Risks)
EC_Cd_kruskal

# Pairwise comparisons
EC_Cd_wilcox <- EC_data %>% 
  wilcox_test(Cd ~ Risks, p.adjust.method = "bonferroni") 
EC_Cd_wilcox

pdf("Figures/EC_Cd_KruskalRisks.pdf")
EC_Cd_wilcox <- EC_Cd_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cd",
          ylab = "Cd Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cd_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cd_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cd_wilcox)
  )
dev.off()

EC_Cd_wilcox <- EC_Cd_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cd",
          ylab = "Cd Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cd_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cd_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cd_wilcox)
  )


##Mn##
EC_Mn_kruskal_weighted <- weighted_mannwhitney(EC_data, Mn,Risks, Feather.Weight)
EC_Mn_kruskal_weighted

EC_Mn_kruskal <- EC_data %>% kruskal_test(Mn ~ Risks)
EC_Mn_kruskal

# Pairwise comparisons
EC_Mn_wilcox <- EC_data %>% 
  wilcox_test(Mn ~ Risks, p.adjust.method = "bonferroni") 
EC_Mn_wilcox

pdf("Figures/EC_Mn_KruskalRisks.pdf")
EC_Mn_wilcox <- EC_Mn_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Mn",
          ylab = "Mn Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Mn_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Mn_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Mn_wilcox)
  )
dev.off()

EC_Mn_wilcox <- EC_Mn_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Mn",
          ylab = "Mn Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Mn_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Mn_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Mn_wilcox)
  )



##U##
EC_U_kruskal_weighted <- weighted_mannwhitney(EC_data, U, Risks, Feather.Weight)
EC_U_kruskal_weighted

EC_U_kruskal <- EC_data %>% kruskal_test(U ~ Risks)
EC_U_kruskal

# Pairwise comparisons
EC_U_wilcox <- EC_data %>% 
  wilcox_test(U ~ Risks, p.adjust.method = "bonferroni") 
EC_U_wilcox

pdf("Figures/EC_U_KruskalRisks.pdf")
EC_U_wilcox <- EC_U_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "U",
          ylab = "U Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_U_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_U_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_U_wilcox)
  )
dev.off()

EC_U_wilcox <- EC_U_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "U",
          ylab = "U Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_U_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_U_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_U_wilcox)
  )



##CONTROL SIG > TEST##

##Zn##
EC_Zn_kruskal_weighted <- weighted_mannwhitney(EC_data, Zn,Risks, Feather.Weight)
EC_Zn_kruskal_weighted

EC_Zn_kruskal <- EC_data %>% kruskal_test(Zn ~ Risks)
EC_Zn_kruskal

# Pairwise comparisons
EC_Zn_wilcox <- EC_data %>% 
  wilcox_test(Zn ~ Risks, p.adjust.method = "bonferroni") 
EC_Zn_wilcox

pdf("Figures/EC_Zn_KruskalRisks.pdf")
EC_Zn_wilcox <- EC_Zn_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Zn",
          ylab = "Zn Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Zn_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Zn_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Zn_wilcox)
  )
dev.off()

EC_Zn_wilcox <- EC_Zn_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Zn",
          ylab = "Zn Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Zn_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Zn_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Zn_wilcox)
  )


##As##
EC_As_kruskal_weighted <- weighted_mannwhitney(EC_data, As,Risks, Feather.Weight)
EC_As_kruskal_weighted

EC_As_kruskal <- EC_data %>% kruskal_test(As ~ Risks)
EC_As_kruskal

# Pairwise comparisons
EC_As_wilcox <- EC_data %>% 
  wilcox_test(As ~ Risks, p.adjust.method = "bonferroni") 
EC_As_wilcox

pdf("Figures/EC_As_KruskalRisks.pdf")
EC_As_wilcox <- EC_As_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "As",
          ylab = "As Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_As_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_As_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_As_wilcox)
  )
dev.off()

EC_As_wilcox <- EC_As_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "As",
          ylab = "As Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_As_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_As_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_As_wilcox)
  )



##Se##
EC_Se_kruskal_weighted <- weighted_mannwhitney(EC_data, Se,Risks, Feather.Weight)
EC_Se_kruskal_weighted

EC_Se_kruskal <- EC_data %>% kruskal_test(Se ~ Risks)
EC_Se_kruskal

# Pairwise comparisons
EC_Se_wilcox <- EC_data %>% 
  wilcox_test(Se ~ Risks, p.adjust.method = "bonferroni") 
EC_Se_wilcox

pdf("Figures/EC_Se_KruskalRisks.pdf")
EC_Se_wilcox <- EC_Se_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Se",
          ylab = "Se Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Se_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Se_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Se_wilcox)
  )
dev.off()

EC_Se_wilcox <- EC_Se_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Se",
          ylab = "Se Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Se_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Se_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Se_wilcox)
  )


##Sb##
EC_Sb_kruskal_weighted <- weighted_mannwhitney(EC_data, Sb,Risks, Feather.Weight)
EC_Sb_kruskal_weighted

EC_Sb_kruskal <- EC_data %>% kruskal_test(Sb ~ Risks)
EC_Sb_kruskal

# Pairwise comparisons
EC_Sb_wilcox <- EC_data %>% 
  wilcox_test(Sb ~ Risks, p.adjust.method = "bonferroni") 
EC_Sb_wilcox

pdf("Figures/EC_Sb_KruskalRisks.pdf")
EC_Sb_wilcox <- EC_Sb_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Sb",
          ylab = "Sb Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Sb_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Sb_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Sb_wilcox)
  )
dev.off()

EC_Sb_wilcox <- EC_Sb_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Sb",
          ylab = "Sb Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Sb_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Sb_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Sb_wilcox)
  )



##Tl##
EC_Tl_kruskal_weighted <- weighted_mannwhitney(EC_data, Tl,Risks, Feather.Weight)
EC_Tl_kruskal_weighted

EC_Tl_kruskal <- EC_data %>% kruskal_test(Tl ~ Risks)
EC_Tl_kruskal

# Pairwise comparisons
EC_Tl_wilcox <- EC_data %>% 
  wilcox_test(Tl ~ Risks, p.adjust.method = "bonferroni") 
EC_Tl_wilcox

pdf("Figures/EC_Tl_KruskalRisks.pdf")
EC_Tl_wilcox <- EC_Tl_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Tl",
          ylab = "Tl Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Tl_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Tl_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Tl_wilcox)
  )
dev.off()

EC_Tl_wilcox <- EC_Tl_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Tl",
          ylab = "Tl Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Tl_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Tl_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Tl_wilcox)
  )


##Cs##
EC_Cs_kruskal_weighted <- weighted_mannwhitney(EC_data, Cs,Risks, Feather.Weight)
EC_Cs_kruskal_weighted

EC_Cs_kruskal <- EC_data %>% kruskal_test(Cs ~ Risks)
EC_Cs_kruskal

# Pairwise comparisons
EC_Cs_wilcox <- EC_data %>% 
  wilcox_test(Cs ~ Risks, p.adjust.method = "bonferroni") 
EC_Cs_wilcox

pdf("Figures/EC_Cs_KruskalRisks.pdf")
EC_Cs_wilcox <- EC_Cs_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cs",
          ylab = "Cs Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cs_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cs_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cs_wilcox)
  )
dev.off()

EC_Cs_wilcox <- EC_Cs_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cs",
          ylab = "Cs Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cs_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cs_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cs_wilcox)
  )


##NOT SIG##


##Pb##
EC_Pb_kruskal_weighted <- weighted_mannwhitney(EC_data, Pb,Risks, Feather.Weight)
EC_Pb_kruskal_weighted

EC_Pb_kruskal <- EC_data %>% kruskal_test(Pb ~ Risks)
EC_Pb_kruskal

# Pairwise comparisons
EC_Pb_wilcox <- EC_data %>% 
  wilcox_test(Pb ~ Risks, p.adjust.method = "bonferroni") 
EC_Pb_wilcox

pdf("Figures/EC_Pb_KruskalRisks.pdf")
EC_Pb_wilcox <- EC_Pb_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Pb",
          ylab = "Pb Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Pb_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Pb_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Pb_wilcox)
  )
dev.off()

EC_Pb_wilcox <- EC_Pb_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Pb",
          ylab = "Pb Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Pb_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Pb_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Pb_wilcox)
  )


##Al##
EC_Al_kruskal_weighted <- weighted_mannwhitney(EC_data, Al,Risks, Feather.Weight)
EC_Al_kruskal_weighted

EC_Al_kruskal <- EC_data %>% kruskal_test(Al ~ Risks)
EC_Al_kruskal

# Pairwise comparisons
EC_Al_wilcox <- EC_data %>% 
  wilcox_test(Al ~ Risks, p.adjust.method = "bonferroni") 
EC_Al_wilcox

pdf("Figures/EC_Al_KruskalRisks.pdf")
EC_Al_wilcox <- EC_Al_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Al",
          ylab = "Al Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Al_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Al_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Al_wilcox)
  )
dev.off()

EC_Al_wilcox <- EC_Al_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Al",
          ylab = "Al Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Al_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Al_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Al_wilcox)
  )


##Hg##
EC_Hg_kruskal_weighted <- weighted_mannwhitney(EC_data, Hg,Risks, Feather.Weight)
EC_Hg_kruskal_weighted

EC_Hg_kruskal <- EC_data %>% kruskal_test(Hg ~ Risks)
EC_Hg_kruskal

# Pairwise comparisons
EC_Hg_wilcox <- EC_data %>% 
  wilcox_test(Hg ~ Risks, p.adjust.method = "bonferroni") 
EC_Hg_wilcox

pdf("Figures/EC_Hg_KruskalRisks.pdf")
EC_Hg_wilcox <- EC_Hg_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Hg",
          ylab = "Hg Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Hg_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Hg_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Hg_wilcox)
  )
dev.off()

EC_Hg_wilcox <- EC_Hg_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Hg",
          ylab = "Hg Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Hg_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Hg_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Hg_wilcox)
  )



##Cr##
EC_Cr_kruskal_weighted <- weighted_mannwhitney(EC_data, Cr,Risks, Feather.Weight)
EC_Cr_kruskal_weighted

EC_Cr_kruskal <- EC_data %>% kruskal_test(Cr ~ Risks)
EC_Cr_kruskal

# Pairwise comparisons
EC_Cr_wilcox <- EC_data %>% 
  wilcox_test(Cr ~ Risks, p.adjust.method = "bonferroni") 
EC_Cr_wilcox

pdf("Figures/EC_Cr_KruskalRisks.pdf")
EC_Cr_wilcox <- EC_Cr_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cr",
          ylab = "Cr Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cr_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cr_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cr_wilcox)
  )
dev.off()

EC_Cr_wilcox <- EC_Cr_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Cr",
          ylab = "Cr Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Cr_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Cr_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Cr_wilcox)
  )



##Sr##
EC_Sr_kruskal_weighted <- weighted_mannwhitney(EC_data, Sr,Risks, Feather.Weight)
EC_Sr_kruskal_weighted

EC_Sr_kruskal <- EC_data %>% kruskal_test(Sr ~ Risks)
EC_Sr_kruskal

# Pairwise comparisons
EC_Sr_wilcox <- EC_data %>% 
  wilcox_test(Sr ~ Risks, p.adjust.method = "bonferroni") 
EC_Sr_wilcox

pdf("Figures/EC_Sr_KruskalRisks.pdf")
EC_Sr_wilcox <- EC_Sr_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Sr",
          ylab = "Sr Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Sr_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Sr_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Sr_wilcox)
  )
dev.off()

EC_Sr_wilcox <- EC_Sr_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Sr",
          ylab = "Sr Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Sr_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Sr_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Sr_wilcox)
  )


##Mo##
EC_Mo_kruskal_weighted <- weighted_mannwhitney(EC_data, Mo,Risks, Feather.Weight)
EC_Mo_kruskal_weighted

EC_Mo_kruskal <- EC_data %>% kruskal_test(Mo ~ Risks)
EC_Mo_kruskal

# Pairwise comparisons
EC_Mo_wilcox <- EC_data %>% 
  wilcox_test(Mo ~ Risks, p.adjust.method = "bonferroni") 
EC_Mo_wilcox

pdf("Figures/EC_Mo_KruskalRisks.pdf")
EC_Mo_wilcox <- EC_Mo_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Mo",
          ylab = "Mo Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Mo_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Mo_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Mo_wilcox)
  )
dev.off()

EC_Mo_wilcox <- EC_Mo_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Mo",
          ylab = "Mo Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Mo_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Mo_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Mo_wilcox)
  )



##Ag##
EC_Ag_kruskal_weighted <- weighted_mannwhitney(EC_data, Ag,Risks, Feather.Weight)
EC_Ag_kruskal_weighted

EC_Ag_kruskal <- EC_data %>% kruskal_test(Ag ~ Risks)
EC_Ag_kruskal

# Pairwise comparisons
EC_Ag_wilcox <- EC_data %>% 
  wilcox_test(Ag ~ Risks, p.adjust.method = "bonferroni") 
EC_Ag_wilcox

pdf("Figures/EC_Ag_KruskalRisks.pdf")
EC_Ag_wilcox <- EC_Ag_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Ag",
          ylab = "Ag Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Ag_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Ag_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Ag_wilcox)
  )
dev.off()

EC_Ag_wilcox <- EC_Ag_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Ag",
          ylab = "Ag Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Ag_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Ag_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Ag_wilcox)
  )


##Sn##
EC_Sn_kruskal_weighted <- weighted_mannwhitney(EC_data, Sn,Risks, Feather.Weight)
EC_Sn_kruskal_weighted

EC_Sn_kruskal <- EC_data %>% kruskal_test(Sn ~ Risks)
EC_Sn_kruskal

# Pairwise comparisons
EC_Sn_wilcox <- EC_data %>% 
  wilcox_test(Sn ~ Risks, p.adjust.method = "bonferroni") 
EC_Sn_wilcox

pdf("Figures/EC_Sn_KruskalRisks.pdf")
EC_Sn_wilcox <- EC_Sn_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Sn",
          ylab = "Sn Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Sn_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Sn_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Sn_wilcox)
  )
dev.off()

EC_Sn_wilcox <- EC_Sn_wilcox %>% add_xy_position(x = "Risks")
ggboxplot(EC_data, x = "Risks", y = "Sn",
          ylab = "Sn Concentration", xlab = "Risks") +
  stat_pvalue_manual(EC_Sn_wilcox, hide.ns = TRUE) +
  labs(title = "Weighted Kruskal-Wallis" ,        subtitle = EC_Sn_kruskal_weighted$p.value,
       caption = get_pwc_label(EC_Sn_wilcox)
  )



#all wilcox tests...
EC_Zn_wilcox
EC_Mn_wilcox
EC_Fe_wilcox
EC_Al_wilcox
EC_Cu_wilcox
EC_Ba_wilcox
EC_Sr_wilcox
EC_Ni_wilcox
EC_Cr_wilcox
EC_Pb_wilcox
EC_Se_wilcox
EC_Sn_wilcox
EC_Cd_wilcox
EC_Co_wilcox
EC_Hg_wilcox
EC_Sb_wilcox
EC_V_wilcox
EC_Mo_wilcox
EC_Tl_wilcox
EC_Cs_wilcox
EC_Ag_wilcox
EC_As_wilcox
EC_U_wilcox


