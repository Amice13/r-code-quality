##Script to reproduce community analysis of the article:

#ES: Composición y estructura florística de una porción de selva baja caducifolia en Tecomán, Colima, México
#EN: Composition and floristic structure of a portion of dry a forest at Tecomán, Colima, Mexico

#Script author: Leopardi-Verde, Carlos L.
#Facultad de Ciencias Biologicas y Agropecuarias, Universidad de Colima, Km. 40 Autopista Colima-Manzanillo, Crucero de Tecoman, Tecoman 28930, Colima, México.
#DATE: 2019-12-09
#Last update: 2020-06-28

###If you find an error or have doubts, please send me an email at cleopardi@ucol.mx  


####Recommended/required libraries

require(vegan)
require(vegetarian)
library(pvclust)
library(graphics)
library(MASS)
library(plyr)
library(lattice)
library(permute)

####Some necessary functions taken from this source: https://github.com/ibartomeus/fundiv/blob/master/R/Evenness.R:


Eve <- function(A, scales = c(1)){
  out <- data.frame(EinvD = rep(NA, nrow(A)),
                    EJ = rep(NA, nrow(A)),
                    Evar = rep(NA, nrow(A)),
                    Ea1 = rep(NA, nrow(A)))
  out$EinvD <- EinvD(A)
  out$EJ <- EJ(A)
  out$Evar <- Evar(A)
  out$Ea1 <- as.numeric(Ea0(A))
  if(length(scales) > 1){
    out[,5:(4+length(scales))] <- Ea0(A, scales = scales)
  }
  rownames(out) <- rownames(A)
  out
}
  
#' @export
#' 
Ea0  <- function(A, scales = c(1)){ 
  require(vegan)
  exp(renyi(A, scales = scales, hill = FALSE)) / 
    exp(renyi(A, scales = c(0), hill = FALSE))
  }

#' @export
EinvD <- function(A) diversity(A, "invsimpson")/specnumber(A)

#' @export
EJ <- function(A){
  eve <- rep(NA,nrow(A))
  for (k in 1:nrow(A)){
    if(specnumber(A[k,]) == 1){
      eve[k] <- NA
    }else{
      eve[k] <- diversity(A[k,])/log(specnumber(A[k,]))
    }
  }
  eve  
}

#' @export
#' 
Evar <- function(A){
  v <- rep(NA, nrow(A)) 
  for(k in 1:nrow(A)) {
    a <- rep(NA, ncol(A)) 
    b <- a  
    d <- a
    for(i in 1:ncol(A)) {
      a[i] <- if(A[k,i] != 0) log(A[k,i]) else 0 
    }
    S <- sum(A[k,] > 0)
    for(i in 1:ncol(A)) {
      b[i] <- a[i]/S
    }
    c <- sum(b)
    for(i in 1:ncol(A)) {
      d[i] <- if(A[k,i] !=0) (a[i]-c)^2/S else 0 
    }
    f <- sum(d)
    v[k] <- (1-2/pi*atan(f))   
  }
  v 
}

##### BEGIN

######DATA LOAD:

res<-read.csv(file.choose(), header=TRUE)  ## Load data to R

res<-na.omit(res) ##Omit NA in data set

#####################################################################
#####################################################################
#####################################################################
######ANALYSIS

###EVENESS AND DIVERSITY CALCULATIONS

Eve(res[,2:76]) ### We select here InvSimpson (Simson equitability)


diversity(res[,2:76])  ##Shannon index
diversity(res[,2:76], "simpson") ##Simpson 1-D index


###ß DIVERSITY 

beta_w <-betadiver(res[,2:76],"w") #Whittakers ß

#####################################################################
#####################################################################
#####################################################################
##Richness estimators -- NOT SHOWN IN ARTICLE

pool <- poolaccum(res[,2:76], permutations=10000)
summary(pool, display = "chao")
plot(pool, display=c("jack1", "jack2","chao","boot"))

#Other way to do richness estimation: 

sprich_tot <-specpool(res[,2:76]) ##Richness estimation to ALL sites.
sprich_cac <-specpool(res[1:2,2:76]) ##Richness estimation to by site.
sprich_hel <-specpool(res[3:4,2:76])
sprich_hui <-specpool(res[5:6,2:76])


##Species accumulation curve

sp0 <-specaccum(res[,2:76], "collector") ##Classic curve 
plot(sp0)

##If you want to test other species acumulation models, try:
sp1 <- specaccum(res[,2:76])
sp2 <- specaccum(res[,2:76], "random")
sp2
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

#####################################################################
#####################################################################
#####################################################################
##DATA PREPARATION FOR IVI ANALYSIS:

#res<-read.table(file.choose(), sep="\t", header=TRUE, na.strings = c("", "NA")) ##Use if it neccesary reload original data.

res.nam <- data.frame(res[,-1], row.names=res[,1]) ##Assign the first column as names

res_cmp<-read.table(file.choose(), sep="\t", header=TRUE, na.strings = c("", "NA"))
res.nam_cmp <- data.frame(res_cmp[,-1], row.names=res_cmp[,1])

col_vector <- factor((res.nam_cmp$res_cmp....1), levels=unique(res.nam_cmp$res_cmp....1))
col_vector

res.nam.t<-as.data.frame(t(res.nam)) ##The matrix is transposed and maintained as a dataframe, if this step is not done the calculation does not work

colnames(res.nam) #The names of the columns of each matrix are reviewed

colnames(res.nam.t)

typeof(res.nam) #The type of object is reviewed, must be lists
typeof(res.nam.t)

#####################################################################
#####################################################################
#####################################################################
##IVI CALCULATIONS

#IVI FOR EAST SLOPE (DATA NICKNAME: Cactus)
### We separated the data from the East Slope

##Data preparation:

cac<-res.nam.t[,1:2]

cac.cl<-cac[which(rowSums(cac) > 0),] #Rows with zeroes are now removed (absent species in both transects)


##Analysis:

#The relative abundance is now calculated, for this according to the formula of Williams-Linera et al. (2005) it is necessary to calculate the absolute abundance and then the relative abundance. Thus for each species: A = N/E, where N is the total abundance and E is the sampled area, but since we have two transects and are more or less similar we will treat it as follows A (t1+t2)/400 (number of individuals of the two transects between the total sampled area).

cacAAb<-rowSums(cac.cl)/400

#The relative abundance is now calculated, the formula AR = (A / sumA) is used

cacAr<-(cacAAb/sum(cacAAb)) ##Here is the relative abundance

sum(cacAr) #Review, this should sum 1.

##Now the relative frequency is calculated, for which you must first calculate the absolute frequency that is given F = Pi / Pt, where P is the number of sites where the species is present and Pt is the total number of sampled sites. Then the relative frequency is Fr = Fi/ΣF. 

cac.bin<-cac[which(rowSums(cac) > 0),] ##We create an object with the necessary dimensions and data and then transform it into binary

cac.bin[cac.bin > 0] <- 1 ##Binary Transformation


FAcac<-rowSums(cac.bin)/2 ##Here is the absolute frequency.

Frcac<-FAcac/sum(FAcac) #Here is the relative frequency

sum(Frcac) #Review, this should sum 1.

###The IVI is now calculated by the formula IVI=(ARi+FRi)

IVIcac<-(cacAr+Frcac) 

Por.IVI.cac<-((IVIcac/sum(IVIcac))*100) #Percentaje of importance

sum(Por.IVI.cac) #Review, this should sum 100%

barplot(sort(Por.IVI.cac, decreasing = TRUE),ylab="Porcentaje de Importancia",las=2, col="black") ##It generates an ordered histogram that illustrates the %IVI.

#####################################################################
#IVI FOR CENTRAL AREA (DATA NICKNAME: Helechos)
### We separated the data from the Central Area

##Data preparation:

hel<-res.nam.t[,3:4]

hel.cl<-hel[which(rowSums(hel) > 0),] ##Rows with zeroes are now removed (absent species in both transects)

##Analysis:


helAAb<-rowSums(hel.cl)/400

helAr<-(helAAb/sum(helAAb)) ##Here is the relative abundance

sum(helAr)

hel.bin<-hel[which(rowSums(hel) > 0),] ##We create an object with the necessary dimensions and data and then transform it into binary

hel.bin[hel.bin > 0] <- 1 ##Binary Transformation

Fhel<-rowSums(hel.bin)/2 ##Here is the absolute frequency.

Frhel<-Fhel/sum(Fhel) ##Here is the relative frequency

sum(Frhel) #Review

IVIhel<-(helAr+Frhel)

Por.IVI.Hel<-((IVIhel/sum(IVIhel))*100)

sum(Por.IVI.Hel) 

barplot(sort(Por.IVI.Hel, decreasing = TRUE),ylab="Porcentaje de importancia",las=2, col="black") ##It generates an ordered histogram that illustrates the %IVI.

#####################################################################
#IVI FOR WEST PLAIN (DATA NICKNAME: Huizaches)
### We separated the data from the West Plain

Huz<-res.nam.t[,5:6]

Huz.cl<-Huz[which(rowSums(Huz) > 0),] ##Rows with zeroes are now removed (absent species in both transects)

##Analysis:

HuzAAb<-rowSums(Huz.cl)/400

HuzAr<-(HuzAAb/sum(HuzAAb)) ##Here is the relative abundance

sum(HuzAr) #Review

Huz.bin<-Huz[which(rowSums(Huz) > 0),] ##We create an object with the necessary dimensions and data and then transform it into binary

Huz.bin[Huz.bin > 0] <- 1 ##Binary Transformation


FHuz<-rowSums(Huz.bin)/2 ##Here is the absolute frequency.

FrHuiz<-FHuz/sum(FHuz) ##Here is the relative frequency

sum(FrHuiz) #Review

IVIHuz<-(HuzAr+FrHuiz)

Por.IVI.Huiz<-((IVIHuz/sum(IVIHuz))*100)

sum(Por.IVI.Huiz) ##Review

barplot(sort(Por.IVI.Huiz, decreasing = TRUE),ylab="Porcentaje de importancia",las=2, xlim = c(0,40), col="black") ##It generates an ordered histogram that illustrates the IVI.


###########OUTPUT FIGURES

pdf(file="Por.IVI_cac.pdf") ###PDF output for the IVI histogram for east slope
barplot(sort(Por.IVI.cac, decreasing = TRUE),ylab="Porcentaje de importancia",las=2, col="black") 
dev.off()


pdf(file="Por.IVI_hel.pdf") #PDF output for the IVI histogram for central area
barplot(sort(Por.IVI.Hel, decreasing = TRUE),ylab="Porcentaje de importancia",las=2, col="black") ##It generates an ordered histogram that illustrates the %IVI.
dev.off()


pdf(file="Por.IVI_Hui.pdf") #PDF output for the IVI histogram for West plain
barplot(sort(Por.IVI.Huiz, decreasing=TRUE),ylab="Porcentaje de importancia",las=2, xlim = c(0,40), col="black") ##It generates an ordered histogram that illustrates the IVI.
dev.off()


#####################################################################
#####################################################################
#####################################################################
####STATICAL COMPARISON AMONG AREAS (ANOSIM AND NMDS):


#####NMDS ANALYSIS
#####Distance calculation:

dis.r <- vegdist(res.nam, method = "chao") #Distance calculations.

Res_NMDS <- monoMDS(dis.r, k=3, model = "local", maxit=10000) #NMDS calculation

stressplot(Res_NMDS) #NMDS verification
 
plot(Res_NMDS) ###NMDS basic output


######## Additional edit features
col_palette <- palette()[col_vector] #Color palette
col_palette

pl<-ordiplot(Res_NMDS, choices = c(1, 2), display = 'sites', type = 'n')
points(pl, what = 'sites', col = col_palette, pch = 16)
ordipointlabel(pl, display = "sites", scaling = "symm", add = TRUE)
legend('bottomright', legend=unique(col_vector), col=unique(col_palette), pch = 16) ##Enhanced output


#####ANOSIM ANALYSIS

h.m<- how(nperm=30000, complete=TRUE, maxperm = 30000, minperm = 10000) #Optional, this was used to enforce the maximum possible permutations.

m.an<-anosim(res.nam, res.nam_cmp$res_cmp....1, permutations = h.m, distance = "bray") ##ANOSIM analysis

summary(m.an)  #ANALYSIS OUTPUT (TEXT)
plot(m.an) #Analysis output; this figure was not shown.  


#####################################################################
#####################################################################
#####################################################################
####ADDITIONAL ANALYSIS ABOUT PHENOLOGY AND GROWTH HABITS


#####Histogram growth habits

hab<-read.csv(file.choose(), header=TRUE)  ## Load data file

areas <- factor((hab$Area), levels=unique(hab$Area)) ##Factor definition

##SUBSET DATA DEFINITION BY AREA

cact<-subset(hab, Area == "Cactus") #EAST SLOPE
helec<-subset(hab, Area == "Helechos") #CENTRAL AREA
huiz<-subset(hab, Area == "Huizache") #WEST PLAIN

cactc<-count(cact$Habito) ##Count function

cactc_noname <- as.matrix(as.data.frame(lapply(cactc, as.numeric))) #Matrix construction

rownames(cactc_noname) <- c("Arbol", "Arbusto", "Hierba", "Trepadora")

cactc.nam <- cactc_noname[,-1]

barplot(cactc.nam, ylab="Número de especies", xlab="Hábito") #OUTPUT

pdf(file="habito_cac.pdf")
barplot(cactc.nam, ylab="Número de especies", xlab="Hábito")
dev.off()

####

helec<-count(helec$Habito)

helec_noname <- as.matrix(as.data.frame(lapply(helec, as.numeric)))

rownames(helec_noname) <- c("Arbol", "Arbusto", "Hierba", "Trepadora")

helec.nam <- helec_noname[,-1]

barplot(helec.nam, ylab="Número de especies", xlab="Hábito")

pdf(file="habito_hel.pdf")
barplot(helec.nam, ylab="Número de especies", xlab="Hábito")
dev.off()


###

huiz<-count(huiz$Habito)

huiz_noname <- as.matrix(as.data.frame(lapply(huiz, as.numeric)))

rownames(huiz_noname) <- c("Arbol", "Arbusto", "Hierba", "Hierba_Epifita", "Trepadora")

huiz.nam <- huiz_noname[,-1]

barplot(huiz.nam, ylab="Número de especies", xlab="Hábito")

pdf(file="habito_huiz.pdf")
barplot(huiz.nam, ylab="Número de especies", xlab="Hábito")
dev.off()

###Phenology of east slope.

fen<-read.csv(file.choose(), header=TRUE)  ## Load data file.

## Factor definition 

cact<-subset(fen, area == "Cactus")

##EAST AREA PHENOLOGY

cact2<-cact[,-1]
cact3<-cact2[,-1]
cact4<-cact3[,-1]


cact.n<-cact[,3]

cact.fac<-cact[,1]

cactc_noname <- as.matrix(as.data.frame(lapply(cact4, as.numeric)))

rownames(cactc_noname) <- cact.n

cactc.nam <- cactc_noname

mycols <- c("gray","black")


barplot(cactc.nam, col = mycols[cact.fac], ylab="Frecuencia absoluta", xlab="Estado fenológico", beside = TRUE)
legend("topright", legend = c("Muestreo 1", "Muestreo 2"), fill = c("gray", "black")) 




pdf(file="fenología_cac.pdf")

barplot(cactc.nam, col = mycols[cact.fac], ylab="Frecuencia absoluta", xlab="Estado fenológico", beside = TRUE)
legend("topright", legend = c("Muestreo 1", "Muestreo 2"), fill = c("gray", "black"))

dev.off()


##CENTRAL AREA PHENOLOGY

helec<-subset(fen, area == "Helechos")


helec2<-helec[,-1]
helec3<-helec2[,-1]
helec4<-helec3[,-1]


helec.n<-helec[,3]

helec.fac<-helec[,1]

helec_noname <- as.matrix(as.data.frame(lapply(helec4, as.numeric)))

rownames(helec_noname) <- helec.n

helec.nam <- helec_noname

mycols <- c("gray","black")

barplot(helec.nam, col = mycols[helec.fac], ylab="Frecuencia absoluta", xlab="Estado fenológico", beside = TRUE)
legend("topright", legend = c("Muestreo 1", "Muestreo 2"), fill = c("gray", "black")) 


pdf(file="fenología_cac.pdf")

barplot(helec.nam, col = mycols[helec.fac], ylab="Frecuencia absoluta", xlab="Estado fenológico", beside = TRUE)
legend("topright", legend = c("Muestreo 1", "Muestreo 2"), fill = c("gray", "black"))

dev.off()

##WEST PLAIN PHENOLOGY

huiz<-subset(fen, area == "Huizache")


huiz2<-huiz[,-1]
huiz3<-huiz2[,-1]
huiz4<-huiz3[,-1]


huiz.n<-huiz[,3]

huiz.fac<-huiz[,1]

huiz_noname <- as.matrix(as.data.frame(lapply(huiz4, as.numeric)))

rownames(huiz_noname) <- huiz.n

huiz.nam <- huiz_noname

mycols <- c("gray","black")

barplot(huiz.nam, col = mycols[huiz.fac], ylab="Frecuencia absoluta", xlab="Estado fenológico", beside = TRUE)
legend("topright", legend = c("Muestreo 1", "Muestreo 2"), fill = c("gray", "black")) 


pdf(file="fenología_cac.pdf")

barplot(huiz.nam, col = mycols[huiz.fac], ylab="Frecuencia absoluta", xlab="Estado fenológico", beside = TRUE)
legend("topright", legend = c("Muestreo 1", "Muestreo 2"), fill = c("gray", "black")) 

dev.off()


##### END
