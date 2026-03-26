library(dplyr)
library(stringr)
library(FactoMineR)
library(factoextra)
library(bigutilsr)

raw <- read.csv("F75_admission_fecal_metabolomics.csv", check.names=F)
fmk <- read.csv("F75_admission_fecal_marker.csv")
strata <-read.csv("F75_pair_key_pwater.csv")
metgroup <- read.csv("F75_metgroup.csv")
##################################################################################################################################
# Determine percentage of detection per metabolite; Remove metabolites undetectable in >=80% of samples
##################################################################################################################################

#Calculate percent LODs

###########################
## replace 0s by NA
###########################
fun.lod<-function(x){ifelse(raw[,x]==0,NA,raw[,x])}
names(raw)
df1<-cbind(raw[1],sapply(2:69,fun.lod))
colnames(df1)<-colnames(raw)

###############################
# Calculate percent by group
###############################

undetected_met <- df1 %>% 
  left_join(strata, by="subjid") %>%
  group_by(death) %>%
  summarise(across(everything(), ~round(mean(is.na(.)*100),0))) %>%
  select(-death, -subjid)%>%
  select(where(~all(. >=80))) %>%
  names(.)

###############################
#Remove 7 undetected metabolites 
###############################

df2 <- df1 %>% select(-any_of(undetected_met))

##################################################################################################################################
# Check CV and remove %cv>30%
##################################################################################################################################

# Calculate %CV=sd/mean per metabolite 
names(df2)
fun_cv<-function(x){mean(x, na.rm=T)}
fun_sd<-function(x){sd(x,na.rm=T)}
mean_m=sapply(df2[2:62],fun_cv)
sd_m=sapply(df2[2:62],fun_sd)
sum(sd_m/mean_m>30, na.rm=T) # all below 12%, thus no additional metabolite is removed.


##################################################################################################################################
#Detect outliers based on PCA and hierrachial clustering (single linkage)
##################################################################################################################################

# log10 transform dataset
fun.log=function(x) {log(x,10)}
df2_log=cbind(df2[1],sapply(df2[2:62],function(x) log(x)))

###############################
# PCA 
###############################

## PCA visualization
X<-df2_log[,2:62]
row.names(X)<-df2_log$subjid

pca<-PCA(X, scale.unit = TRUE)
fviz_pca_ind(pca,  addEllipses = TRUE, ellipse.level = 0.99) # outliers: ID_271, ID_31

## Detect outliers that are >6sd away from mean
U<-pca$ind$coord #extract PCs for each observation
outlier.inx <- apply(U, 2, function(x) which( (abs(x - mean(x,na.rm=T)) / sd(x,na.rm=T)) > 6)) %>%Reduce(union, .)
df2_log[outlier.inx,]$subjid # Subject ID_271 is a clear outlier with >6 sd away from mean distance

## The same outlier is picked up as outlier by Mahalanobis distance
# dist <- mahalanobis(U,colMeans(U),cov(U)) 
# mhd.outlier.inx <- (pchisq(dist, df = 5, lower.tail = FALSE) < (0.05 / length(dist))) # Bonferroni corrected p-val threshold
# df2_log[mhd.outlier.inx,]

###############################
# Hieracchical clustering 
###############################
row.names(df2_log)<-as.character(df2_log$subjid) #to better see which id is 'outlier'
hc.single <- hclust(dist(df2_log[,2:62]), method="single") 
plot(hc.single , main=" Single Linkage ", xlab="", sub ="", cex =.9) #ID_271, ID_31 again also detected as outliers

###############################
# Remove 2 outliers
###############################
df3 <- df2 %>% filter(!subjid %in% c("ID_271", "ID_31"))


##################################################################################################################################
# Clean column name format
##################################################################################################################################

colnames(df3)<-gsub("3-","three_",colnames(df3))
colnames(df3)<-gsub("4-","four_",colnames(df3))
colnames(df3)<-gsub("5-","five_",colnames(df3))
colnames(df3)<-gsub("N,N-","NN_",colnames(df3))
colnames(df3)<-gsub("N2-","beta_",colnames(df3))
colnames(df3)<-gsub("myo-","myo_",colnames(df3))
colnames(df3)<-gsub("methionine","Methionine",colnames(df3))
colnames(df3)[2:62]<-paste0("F_", colnames(df3[,2:62]))


##################################################################################################################################
# Impute for for variables with <10% missing data among samples with at least one of metabolomics or fecal marker measured
##################################################################################################################################

df4 <- merge(df3, fmk%>%select(subjid, grep("^E_", names(fmk))), by="subjid", all=T)

#check % missing per variable
names(df4)
fun.missing <- function(x){round(100*sum(is.na(x))/length(x),2)}
sum(sapply(df4[-1], fun.missing)>=10) #0, i.e. all variable <10% missing data

set.seed(337)
imp.col<-grep("^E_|^F_",names(df4))
df4.imp<-df4
sum(is.na(df4.imp[imp.col]))  #241 missing values (2.8% of data)
pre<-caret::preProcess(df4.imp[imp.col], method = "bagImpute")
df4.imp[imp.col]<-predict(pre,df4.imp[imp.col])

df5 <- df4.imp

##################################################################################################################################
# Merge in strata
##################################################################################################################################

df6 <- df5 %>% 
  left_join(strata%>%select(subjid, strata), by="subjid")

### remove stratas where either case or control in a pair is completely missing
unpaired <- df6 %>% 
  group_by(strata)%>%
  summarise(n=n()) %>%
  filter(n!=2) %>%
  pull(strata)

df6 <- df6 %>% filter(!strata %in% unpaired)

##################################################################################################################################
# Prepare master data file Log transform metabolites and fecal markers in dry weight concentrations
##################################################################################################################################


###################################
# Calculate fecal marker dry weight 
###################################

df7 <- df6 %>%
  left_join(strata%>%select(subjid, pwater), by="subjid")

sum(is.na(df7$pwater)) #one sample missing pwater
#impute one pwater value

set.seed(446)
imp.col<-grep("pwater|^E_|^F_",names(df7))
df7.imp<-df7
sum(is.na(df7)) #1
pre<-caret::preProcess(df7.imp[imp.col], method = "bagImpute")
df7.imp[imp.col]<-predict(pre,df7.imp[imp.col])
sum(is.na(df7.imp)) 
df8 <- df7.imp

df9 <- df8 %>%
  mutate(E_MPO = E_MPO*(100-pwater),
         E_AAT = E_AAT*(100-pwater),
         E_CAL = E_CAL*(100-pwater)) %>%
  select(-strata, -pwater)


############################################
# Calculate and Add Ratio Columns for f_omics 
############################################
fomic.inx<-grep("F_",names(df9))
temp<-df9[fomic.inx]
temp2<-matrix(nrow=nrow(temp),ncol=ncol(temp))%>%as.data.frame(); names(temp2)<-paste0("rat_",names(temp))
for (i in 1:ncol(temp)) {
  temp2[i]<-temp[i]/rowSums(temp,na.rm=T)
}
df10<-cbind(df9,temp2)
names(df10)


########################################################################################################################
# Calculate derived summary mets
########################################################################################################################

######### total met output from row
df10$sum_F_met=rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total scfa/bcfa (sum of acetate, butyrate, propionate, valerate, isobutyrate, and isovalerate)
df10$sum_F_scfa=rowSums(df10%>%select(F_Acetate,F_Butyrate,F_Propionate,F_Valerate,F_Isobutyrate,F_Isovalerate),
                             na.rm=T)
df10$sum_F_scfa_lactate_formate=rowSums(df10%>%select(F_Acetate,F_Butyrate,F_Propionate,F_Valerate,F_Isobutyrate,F_Isovalerate,F_Lactate,F_Formate),
                                             na.rm=T)
#ratio of total scfa/bcfa relative to total met
df10$rratF_scfa=rowSums(df10%>%
                               select(F_Acetate,F_Butyrate,F_Propionate,F_Valerate,F_Isobutyrate,F_Isovalerate),
                             na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)
df10$rratF_scfa_lactate_formate=rowSums(df10%>%
                                               select(F_Acetate,F_Butyrate,F_Propionate,F_Valerate,F_Isobutyrate,F_Isovalerate,F_Lactate,F_Formate),
                                             na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total amino acid and derivatives
df10$sum_F_aa=rowSums(df10%>%select(F_Alanine,F_Aspartate,F_Isoleucine,F_Leucine,F_Lysine,F_Methionine,
                                              F_phenylalanine,F_Proline,F_Serine,F_Threonine,F_Tryptophan,F_Tyrosine,F_Valine,
                                              F_Glutamate,F_Glutamine,F_Glycine
                                              #,F_NN_Dimethylglycine,F_Sarcosine,F_beta_Alanine
),na.rm=T)
#ratio of total amino acid relative to total met
df10$rratF_aa=rowSums(df10%>%
                             select(F_Alanine,F_Aspartate,F_Isoleucine,F_Leucine,F_Lysine,F_Methionine,
                                    F_phenylalanine,F_Proline,F_Serine,F_Threonine,F_Tryptophan,F_Tyrosine,F_Valine,
                                    F_Glutamate,F_Glutamine,F_Glycine
                                    #,F_NN_Dimethylglycine,F_Sarcosine,F_beta_Alanine
                             ),na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total essential amino acids #histidine, isoleucine, leucine, lysine, methionine, phenylalanine, threonine, tryptophan, and valine.
df10$sum_F_ea=rowSums(df10%>%select(F_Isoleucine,F_Leucine,F_Lysine,F_Methionine,
                                              F_phenylalanine,F_Threonine,F_Tryptophan,F_Valine
),na.rm=T)
#ratio of total essential amino acids relative to total met
df10$rratF_ea=rowSums(df10%>%
                             select(F_Isoleucine,F_Leucine,F_Lysine,F_Methionine,
                                    F_phenylalanine,F_Threonine,F_Tryptophan,F_Valine
                             ),na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total conditional essential amino acids arginine, cysteine, glutamine, tyrosine, glycine, ornithine, proline, and serine
df10$sum_F_ca=rowSums(df10%>%select(F_Proline,F_Serine,F_Tyrosine),na.rm=T)

######### total nonessential amino acids alanine, arginine, asparagine, aspartic acid, cysteine, glutamic acid, glutamine, glycine, proline, serine, and tyrosine
df10$sum_F_na=rowSums(df10%>%select(F_Alanine,F_Aspartate,
                                              F_Proline,F_Serine,F_Tyrosine,
                                              F_Glutamate,F_Glutamine,F_Glycine),na.rm=T)
#ratio of total nonessential amino acids relative to total met
df10$rratF_na=rowSums(df10%>%
                             select(F_Alanine,F_Aspartate,
                                    F_Proline,F_Serine,F_Tyrosine,
                                    F_Glutamate,F_Glutamine,F_Glycine),na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)


######### total TCA related 3-Hydroxybutyrate, Fumarate, Lactate, Methylmalonate, Pyruvate, Succinate
df10$sum_F_tca=rowSums(df10%>%select(F_three_Hydroxybutyrate, F_Fumarate, F_Lactate,
                                               F_Methylmalonate, F_Pyruvate, F_Succinate),na.rm=T)
#ratio of total TCA relative to total met
df10$rratF_tca=rowSums(df10%>%
                              select(F_three_Hydroxybutyrate, F_Fumarate, F_Lactate,
                                     F_Methylmalonate, F_Pyruvate, F_Succinate),na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total Carboxylic acid
cb<-metgroup%>%filter(subclass=="carboxylic acid")%>%mutate(met=paste0("F_",met))
df10$sum_F_cb=rowSums(df10[,names(df10)%in% cb$met],na.rm=T)
#ratio of total Carboxylic acid to total met
df10$rratF_cb=rowSums(df10[,names(df10)%in% cb$met],na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total carbohydrate
ch<-metgroup%>%filter(subclass=="carbohydrate")%>%mutate(met=paste0("F_",met))
df10$sum_F_ch=rowSums(df10[,names(df10)%in% ch$met],na.rm=T)
#ratio of total Carboxylic acid to total met
df10$rratF_ch=rowSums(df10[,names(df10)%in% ch$met],na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total alchohol
al<-metgroup%>%filter(subclass=="alchohol")%>%mutate(met=paste0("F_",met))
df10$sum_F_al=rowSums(df10[,names(df10)%in% al$met],na.rm=T)
#ratio of total Carboxylic acid to total met
df10$rratF_al=rowSums(df10[,names(df10)%in% al$met],na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total amine
am<-metgroup%>%filter(subclass=="amine")%>%mutate(met=paste0("F_",met))
df10$sum_F_am=rowSums(df10[,names(df10)%in% am$met],na.rm=T)
#ratio of total Carboxylic acid to total met
df10$rratF_am=rowSums(df10[,names(df10)%in% am$met],na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)

######### total nucleobase
nb<-metgroup%>%filter(subclass=="nucleobase")%>%mutate(met=paste0("F_",met))
df10$sum_F_nb=rowSums(df10[,names(df10)%in% nb$met],na.rm=T)
#ratio of total Carboxylic acid to total met
df10$rratF_nb=rowSums(df10[,names(df10)%in% nb$met],na.rm=T)/rowSums(df10%>%select(grep("^F_",names(.))),na.rm=T)


#############################
# log 10 transformation mets
##############################
fun.log <- function(x) {log(x,10)}
df11<-df10%>%mutate_at(vars(grep("F_|sum_|rratF_",names(.))), fun.log)
df11<-df11%>%arrange(subjid) 



################################################################  END OF SCRIPT