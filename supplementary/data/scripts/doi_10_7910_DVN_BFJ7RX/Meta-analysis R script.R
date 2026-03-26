##META-ANALYSES SCRIPT
##PREPARED BY REAGAN MOGIRE
##LAST EDITS: 20TH MAY 2019

setwd('~') #set working directory
library(meta)
library(ggplot2)

##########################################################################################################################################
#META-ANALYSIS OF PREVALENCE

## 75 nmol/L cutoff

mydata=read.csv("prev75.csv")


(mymtprop <- metaprop(event=t75d, n=n, studlab=StudyC,comb.fixed = F, byvar=droplevels(Agegroup),pscale = 100, data=mydata, sm="PFT"))

forest(mymtprop, print.byvar = F , just="center", align= "r", smlab = "",xlab = "Prevalence (%)", sortvar = StudyC, text.random = "Random effects meta-analyis", col.by="black", colgap = "10mm", xlim = c(0,100),
       leftlabs = c("Study", "Cases", "n"), rightlabs = c("Prevalence (%)","95% CI", "Weight (%)"), test.subgroup.random = T, print.Q.subgroup= TRUE)

# export pdf  9 by 13


## Test for publication bias

# Meta-anlayisis for publiation bias testing, removed subgroups
(mymtprop <- metaprop(event=t75d, n=n, comb.fixed = F, studlab=Study,pscale = 100, data=mydata)) 

forest(mymtprop)

funnel(mymtprop)

metabias(mymtprop, method.bias = "linreg", plotit = T) #https://www.statsdirect.com/help/meta_analysis/bias_detection.htm

#####################################################################
##50 nmol/L cut-off

mydata=read.csv("prev50.csv")

(mymtprop <- metaprop(event=t50d, n=n, studlab=StudyC, comb.fixed = F, byvar=droplevels(Agegroup),pscale = 100, data=mydata, sm="PFT"))

forest(mymtprop, print.byvar = F , just="center", align= "r", smlab = "",xlab = "Prevalence (%)", sortvar = StudyC, text.random = "Random effects meta-analyis", col.by="black", colgap = "10mm", xlim = c(0,100),
       leftlabs = c("Study", "Cases", "n"), rightlabs = c("Prevalence (%)","95% CI", "Weight (%)"), test.subgroup.random = T, print.Q.subgroup= TRUE)

#pdf 13 by 15


## Test for publication bias

# Meta-anlayisis for publiation bias testing, removed subgroups
(mymtprop <- metaprop(event=t50d, n=n, comb.fixed = F, studlab=Study,pscale = 100, data=mydata))

forest(mymtprop)

funnel(mymtprop)

metabias(mymtprop, method.bias = "linreg", plotit = T) #PDF 8 by 5

#####################################################################
##30 nmol/L cut-off

mydata=read.csv("prev30.csv")

(mymtprop <- metaprop(event=td, n=n, studlab=StudyC, comb.fixed = F, byvar=droplevels(Agegroup),  pscale = 100, data=mydata, sm="PFT"))


forest(mymtprop, print.byvar = F , just="center", align= "r", smlab = "",xlab = "Prevalence (%)", sortvar = StudyC, text.random = "Random effects meta-analyis", col.by="black", colgap = "10mm", xlim = c(0,100),
       leftlabs = c("Study", "Cases", "n"), rightlabs = c("Prevalence (%)","95% CI", "Weight (%)"), test.subgroup.random = T, print.Q.subgroup= TRUE)

# PDF export 13 by 8 


## Test for publication bias

# Meta-anlayisis for publiation bias testing, removed subgroups
(mymtprop <- metaprop(event=td, n=n, comb.fixed = F, studlab=Study,pscale = 100, data=mydata))

forest(mymtprop)

funnel(mymtprop)

metabias(mymtprop, method.bias = "linreg", plotit = T)


##########################################################################################################################################

#ANALYSIS OF CONTINUOUS VITAMIN D LEVELS


## By age groups
mydata=read.csv("continousD.csv")
mymtcont <- metamean (n=n, mean = mean, sd= sd, studlab = studyC, byvar = agegroup, comb.fixed = F, data = mydata)

forest(mymtcont, ref= NA, print.byvar = F , just="center", align= "r", sortvar = studyC, text.random = "Random effects meta-analysis", col.by="black", colgap.forest.left = "10mm", xlim = c(0,140), smlab = "",
       rightlabs = c("Vitamin D levels (nmol/L)","95% CI", "Weight (%)"), xlab = "Mean vitamin D levels (nmol/L)", test.subgroup.random = T, print.Q.subgroup= TRUE)

#PDF 15 by 25


## By setting 

mydata=read.csv("continousD.csv")
mydata=mydata[-8] #remove "assay" variable because it also has missing data
mydata=na.omit(mydata) ## create new dataset without missing data
mymtcont <- metamean (n=n, mean = mean, sd= sd, studlab = studyC, byvar = setting, comb.fixed = F, data = mydata)

forest(mymtcont, ref= NA, print.byvar = F , just="center", align= "r", sortvar = studyC, text.random = "Random effects meta-analysis", col.by="black", colgap.forest.left = "12mm", xlim = c(0,140), smlab = "",
       rightlabs = c("Vitamin D levels (nmol/L)","95% CI", "Weight (%)"), xlab = "Mean vitamin D levels (nmol/L)", test.subgroup.random = T, print.Q.subgroup= TRUE)

#PDF 12 by 15

## By region
mydata=read.csv("continousD.csv")
mymtcont <- metamean (n=n, mean = mean, sd= sd, studlab = studyC, byvar = Region, comb.fixed = F, data = mydata)

forest(mymtcont, ref= NA, print.byvar = F , just="center", sortvar = studyC, text.random = "Random effects meta-analysis", col.by="black", colgap.forest.left = "10mm", xlim = c(0,140), smlab = "",
       rightlabs = c("Vitamin D levels (nmol/L)","95% CI", "Weight (%)"), xlab = "Mean vitamin D levels (nmol/L)", test.subgroup.random = T, print.Q.subgroup= TRUE)

#PDF 12 by 20
## Compare North and Southern African countries vs West, East and Central African countries
mydata=read.csv("continousD.csv")
mymtcont <- metamean (n=n, mean = mean, sd= sd, studlab = studyC, byvar = latitude, comb.fixed = F, data = mydata)

forest(mymtcont, ref= NA, print.byvar = F , just="center", sortvar = studyC, text.random = "Random effects meta-analysis", col.by="black", colgap.forest.left = "10mm", xlim = c(0,140), smlab = "",
       rightlabs = c("Vitamin D levels (nmol/L)","95% CI", "Weight (%)"), xlab = "Mean vitamin D levels (nmol/L)", test.subgroup.random = T, print.Q.subgroup= TRUE)

#PDF 12 by 20


 ########################################################################################################
 #Meta-analyis of Mean Differences
 
# Mother vs child
 
mydata= read.csv("mother_infant_MD.csv", header = T)
 
mymtcont <- metacont(n.e = NM , mean.e = MeanM, sd.e = SDM, n.c = NI, mean.c = MeanI, sd.c = SDI, 
                     comb.fixed = F,data= mydata, studlab = StudyC )
 
 forest(mymtcont, ref= NA, print.byvar = F , just="center",digits.sd = 2, 
        sortvar = StudyC, text.random = "Random effects meta-analysis",
        col.by="black", xlim = c(-20,80), smlab = "", colgap = "5mm",colgap.forest.right = "1mm",
        rightlabs = c("MD in 25(OH)D (nmol/L)","95% CI", "Weight (%)"),
        lab.c = ("Infants"),lab.e = ("Mothers"), 
        xlab = "MD in 25(OH)D (nmol/L)",
        test.subgroup.random = T, print.Q.subgroup= TRUE)
 
 #PDF 13 vs 5
 # Male vs female
 
 mydata= read.csv("Gender_MD.csv", header = T)
 
 mymtcont <- metacont(n.e = NM , mean.e = MeanM, sd.e = SDM, n.c = NF, mean.c = MeanF, sd.c = SDF, 
                      comb.fixed = F,data= mydata, studlab = StudyC )
 
 forest(mymtcont, ref= NA, print.byvar = F , just="center",digits.sd = 2, 
        sortvar = StudyC, text.random = "Random effects meta-analysis",
        col.by="black", xlim = c(-30,40), smlab = "", colgap = "5mm",colgap.forest.right = "1mm",
        rightlabs = c("MD in 25(OH)D (nmol/L)","95% CI", "Weight (%)"),
        lab.c = ("Females"),lab.e = ("Males"), 
        xlab = "MD in 25(OH)D (nmol/L)",
        test.subgroup.random = T, print.Q.subgroup= TRUE)
 #PDF 13 vs 5
 
 # Urban vs rural 
 
 mydata= read.csv("setting_MD.csv", header = T)
 
 mymtcont <- metacont(n.e = NR , mean.e = MeanR, sd.e = SDR, n.c = NU, mean.c = MeanU, sd.c = SDU, 
                      comb.fixed = F,data= mydata, studlab = StudyC )
 
 forest(mymtcont, ref= NA, print.byvar = F , just="center",digits.sd = 2, 
        sortvar = StudyC, text.random = "Random effects meta-analysis",
        col.by="black", xlim = c(-20,80), smlab = "", colgap = "5mm",colgap.forest.right = "1mm",
        rightlabs = c("MD in 25(OH)D (nmol/L)","95% CI", "Weight (%)"),
        lab.c = ("Urban"),lab.e = ("Rural"), 
        xlab = "MD in 25(OH)D (nmol/L)",
        test.subgroup.random = T, print.Q.subgroup= TRUE)
 
           #################END################################
 ##########################################################################################################