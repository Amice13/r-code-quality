##Script to reproduce  analysis of the article:

#Agrestal weeds of Colima: distribution and biogeographic composition 

#Script author: Leopardi-Verde, Carlos L.
#Facultad de Ciencias Biologicas y Agropecuarias, Universidad de Colima, Km. 40 Autopista Colima-Manzanillo, Crucero de Tecoman, Tecoman 28930, Colima, México.
#DATE: 2020-10-01

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


we<-read.csv(file.choose(), header=TRUE)  ## Load data to R /// If you have a read error, please convert encoding of the input file to UTF8

we<-na.omit(we) ##Omit NA in data set


we.nam <- data.frame(we[,-1], row.names=we[,1]) ##Assign the first column as names


we_cmp<-read.table(file.choose(), sep="\t", header=TRUE, na.strings = c("", "NA")) #Load here the file "Structure_complementary groups.txt"

we.nam_cmp <- data.frame(we_cmp[,-1], row.names=we_cmp[,1])

col_vector <- factor((we.nam_cmp$we_cmp....1), levels=unique(we.nam_cmp$we_cmp....1))
col_vector

we.nam.t<-as.data.frame(t(we.nam)) ##The matrix is transposed and maintained as a dataframe, if this step is not done the calculation does not work

colnames(we.nam) #The names of the columns of each matrix are reviewed

colnames(we.nam.t)

typeof(we.nam) #The type of object is reviewed, must be lists
typeof(we.nam.t)



dis.r <- dist(we.nam, method="binary")
 #Distance calculations.

Res_NMDS <- metaMDS(dis.r, trymax = 10000, distance = "jaccard", binary = TRUE)#, k=3, model = "local", maxit=10000) #NMDS calculation

plot(Res_NMDS, "sites")
text(Res_NMDS, "sites", pos=3)


stwesplot(Res_NMDS) #NMDS verification
 
plot(Res_NMDS) ###NMDS basic output


######## Additional edit featuwe
col_palette <- palette()[col_vector] #Color palette
col_palette

pch_site<-c(1:8)[factor(col_vector)]
pch_site
#the argument that control the plotting symbols is pch

pl<-ordiplot(Res_NMDS, choices = c(1, 2), display = 'sites', type = 'n')
points(pl, what = 'sites', col = col_palette, pch = pch_site)
#ordipointlabel(pl, display = "sites", scaling = "symm", add = TRUE)
ordispider(Res_NMDS, col_vector, scaling = "symmetric", col=unique(col_palette), draw="polygon", label =FALSE)
ordihull(Res_NMDS, col_vector, scaling = "symmetric", col=unique(col_palette), draw="polygon", label =TRUE)
#legend('bottomright', legend=unique(col_vector), col=unique(col_palette), pch = pch_site) ##Enhanced output

pdf(file="malezas_NMDS-OK.pdf")

pl<-ordiplot(Res_NMDS, choices = c(1, 2), display = 'sites', type = 'n')
points(pl, what = 'sites', col = col_palette, pch = pch_site)
#ordipointlabel(pl, display = "sites", scaling = "symm", add = TRUE)
ordispider(Res_NMDS, col_vector, scaling = "symmetric", col=unique(col_palette), draw="polygon", label =FALSE)
ordihull(Res_NMDS, col_vector, scaling = "symmetric", col=unique(col_palette), draw="polygon", label =TRUE)
#legend('bottomright', legend=unique(col_vector), col=unique(col_palette), pch = pch_site) ##Enhanced output

dev.off()

pdf(file="malezas2_NMDS-SIN ETIQUETAS.pdf")
pl<-ordiplot(Res_NMDS, choices = c(1, 2), display = 'sites', type = 'n')
points(pl, what = 'sites', col = col_palette, pch = pch_site)
#ordipointlabel(pl, display = "sites", scaling = "symm", add = TRUE)
ordispider(Res_NMDS, col_vector, scaling = "symmetric", col=unique(col_palette), draw="polygon", label =FALSE)
ordihull(Res_NMDS, col_vector, scaling = "symmetric", col=unique(col_palette), draw="polygon", label =FALSE)
legend('bottomright', legend=unique(col_vector), col=unique(col_palette), pch = pch_site) ##Enhanced output

dev.off()
