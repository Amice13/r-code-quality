#Scrip para genética de poblaciones de Fusarium
#Elaborado por: Dr. Carlos Luis Leopardi Verde
#				Facultad de Ciencias Biológicas y Agropecuarias
#				Universidad de Colima
#Fecha: 13-09-2019

#install.packages(c("fields","RColorBrewer","mapplots"))
#library(devtools)
#source("http://bioconductor.org/biocLite.R")
#BiocManager::install("LEA")

source("http://membres-timc.imag.fr/Olivier.Francois/Conversion.R")
source("http://membres-timc.imag.fr/Olivier.Francois/POPSutilities.R")
#

library(devtools)
library(LEA)
library(mapplots)
library(maps)
require(sp)
require(mapdata)
library(latticeExtra)
library(colorspace)
#require(rgeos)
#require(rgdal)
library(ade4)
library(pvclust)
library(graphics)
library(vegan)

#obj.snmf = snmf("secondary_contact.geno", K = 3, alpha = 100, project = "new") 

#qmatrix = Q(obj.snmf, K = 3)

#struct2geno(file = file.choose(), TESS = FALSE, diploid = FALSE, FORMAT = 1, extra.row = 0, extra.col = 3, output = "fusarium.geno") ## Se utiliza para hacer la transformación

###Determinar el número de poblaciones ancestrales
obj.snmf = snmf("fusarium.geno", K = 1:10, ploidy = 1, entropy = T, alpha = 500, project = "new") 

plot(obj.snmf, col = "blue4", cex = 1.4, pch = 19)



##Aquí se debe leer el resultado de la entropía, el más bajo es el mejor. En este caso K=2, igual que sugiere Structure.


#barchart(s, K = 2, run = 1, xlab = "Individuals")



##### Esto es para determinar estructura

obj.snmf = snmf("fusarium.geno", K = 2, alpha = 500, project = "new")
qmatrix = Q(obj.snmf, K = 2)

barplot(t(qmatrix), col = c("orange","green"), border = NA, space = 0.1, xlab = "Individuals", ylab = "Admixture coefficients")


pdf(file="estructura_barras.pdf", width = 10, height = 3, paper = "USr")

barplot(t(qmatrix), col = c("orange","green"), border = NA, space = 0.1, xlab = "Individuals", ylab = "Admixture coefficients")

dev.off()


coord = read.table("fusarium_coordenadas.coord")
pop = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) ##Etiqueta las coordenadas por cada población, es un número entero por cada individuo de cada población.
as.double(pop) ##Transforma las etiquedas de los datos al tipo correcto de objeto


#pop = rep(1:60, each = 10)


K=2
Npop = length(unique(pop))
qpop = matrix(NA, ncol = K, nrow = Npop) 
coord.pop = matrix(NA, ncol = 2, nrow = Npop) 
	for (i in unique(pop)){
		qpop[i,] = apply(qmatrix[pop == i,], 2, mean) 
		coord.pop[i,] = apply(coord[pop == i,], 2, mean)}


MEX.edo <- readShapePoly(file.choose())

proj <- CRS(' +proj=longlat +ellps=WGS84')
fus<-map("worldHires",xlim=c(-107.5,-85), ylim=c(15.5,25), col="gray90", fill=TRUE)

#plot(coord, xlab = "Longitude", ylab = "Latitude", type = "n")
map(add = TRUE, col = "grey90", fill = TRUE)
plot(MEX.edo, add=TRUE)

for (i in 1:Npop){
	add.pie(z = qpop[i,], x = coord.pop[i,1], y = coord.pop[i,2], labels = "",
	col = c("orange","green"))}

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
map.axes()

####################################

pdf(file="distr.pop_fusarium.pdf")

fus<-map("worldHires",xlim=c(-107.5,-85), ylim=c(15.5,25), col="gray90", fill=TRUE)

#plot(coord, xlab = "Longitude", ylab = "Latitude", type = "n")
map(add = TRUE, col = "grey90", fill = TRUE)
plot(MEX.edo, add=TRUE)

for (i in 1:Npop){
	add.pie(z = qpop[i,], x = coord.pop[i,1], y = coord.pop[i,2], labels = "",
	col = c("orange","green"))}

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
map.axes()

dev.off()


#####Mapa con puntos de muestreo

fus.dots <- read.csv(file.choose())

fus<-map("worldHires",xlim=c(-107.5,-85), ylim=c(15.5,25), col="gray90", fill=TRUE)

map(add = TRUE, col = "grey90", fill = TRUE)
#plot(MEX.edo, add=TRUE)

points(fus.dots$lon, fus.dots$lat, pch=20, col="red", cex=1)  #plot my sample sites

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
map.axes()


###Mapa de distribución de sitios de colecta

pdf(file="distr.dots_fusarium.pdf")

fus<-map("worldHires",xlim=c(-107.5,-85), ylim=c(15.5,25), col="gray90", fill=TRUE)

map(add = TRUE, col = "grey90", fill = TRUE)
plot(MEX.edo, add=TRUE)

points(fus.dots$lon, fus.dots$lat, pch=20, col="red", cex=1)  #plot my sample sites

map.scale(grconvertX(0.5,"npc"), grconvertY(0.08, "npc"),col="black", metric = TRUE, ratio=FALSE, relwidth=0.2)
map.axes()

dev.off()
###############

##Script Similitud genética Fusarium


rapds <- read.csv(file.choose(),header=TRUE,sep=",", fileEncoding="latin1") #El archivo de Manzo tiene problemas con la codificación de caracteres...

rapds<-na.omit(rapds)


##Cálculo de distancias solitas

vegdist(rapds[,-1], method="morisita")



fit.rapds<-pvclust(rapds[,-1], method.hclust="average",method.dist="canberra", nboot=10000) ## binario en este caso es igual a Camberra

par(cex=0.8, mar=c(5, 4, 4, 2))

plot(fit.rapds, cex=0.6)

pvrect(fit.rapds, alpha=.95)


pdf(file="Foc_clust-OK.pdf")

par(cex=0.8, mar=c(5, 4, 4, 2))

plot(fit.rapds, cex=0.6)

pvrect(fit.rapds, alpha=.95)

dev.off()

