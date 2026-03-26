###Script para análisis de poblaciones Pseudocercospora - Análisis de matriz con alelos de resistencia.
###Creado por: Carlos L. Leopardi Verde
###Universidad de Colima, Facultad de Ciencias Biológicas y agropecuarias
###Fecha de creación: 21 de abril de 2022
###Datos a utilizar: Los proporcionados por el Dr. Gilberto Manzo, autor principal de la publicación.

library(adegenet)

myco<-read.table(file.choose()) ##Lee texto delimitado por tabuladores

myco.nam <- data.frame(myco[,-1], row.names=myco[,1])

myco.gid<-df2genind(myco.nam, ploidy=1, pop=myco.nam$V2, type="PA")

foo.BIC <- find.clusters(myco.gid, n.iter=10000000, stat = "BIC") #Número de PC retenidos= 5, n.clust=5, n.iter=10000000, choose=FALSE

##Se retienen cinco componentes porque son los que tienen la mayor cantidad de información.

plot(foo.BIC$Kstat, type="o", xlab="number of clusters (K)", ylab="BIC",col="blue", main="Detection based on BIC")

pdf(file="clusters_myco_alelos_resistencia_manzo.pdf")

plot(foo.BIC$Kstat, type="o", xlab="number of clusters (K)", ylab="BIC",col="blue", main="Detection based on BIC")

dev.off()

dapc1 <- dapc(myco.gid, foo.BIC$grp) #Se retienen 5 PCs y 3 funciones discriminantes

scatter(dapc1)

scatter(dapc1, pch=17:22) ##posi.da="topleft", por si es necesario cambiar la posición.


pdf(file="dapc_myco_manzo.pdf")

scatter(dapc1, pch=17:22)

dev.off()


assignplot(dapc1)

##Para explorar con más cuidado la asignación a cada población hipotética:

dapc1$grp

write.csv(dapc1$grp, "agrupación de ind por poblaciones hipotéticas.csv", row.names = TRUE)

pdf(file="asignación a poblaciones.pdf")

assignplot(dapc1)

dev.off()


lab <- pop(myco.gid)

compoplot(dapc1, posi=list(x=0,y=1.2), txt.leg=paste("Cluster", 1:5), lab=lab, ncol=1, xlab="individuals", col=funky(5), show.lab=TRUE)


pdf(file="compoplot_estructura_myco_alelos resistencia_manzo.pdf")

compoplot(dapc1, posi=list(x=0,y=1.2), txt.leg=paste("Cluster", 1:5), lab=lab, ncol=1, xlab="individuals", col=funky(5), show.lab=TRUE)

dev.off()

##Estas líneas son complementarias, realmente no se ocupa correrlas.
###grp <- find.clusters(myco.gid, n.pca = 100, choose = FALSE, stat = "BIC")
###plot(grp$Kstat, type = "o", xlab = "number of clusters (K)",
#     ylab = "BIC",
#     main = "find.clusters on a genlight object\n(two groups)")

##Ahora para el AMOVA:

###########################################################################
###########################################################################

library(pegas)

##Ahora convertimos los datos utilizados en adgenet a pegas, pues los paquetes son complementarios:

#####Primero es necesario eliminar la columna que contiene el vetor de población.

myco.peg <- read.loci(file.choose(), loci.sep = "\t", col.loci = 2:34, col.pop = 35, row.names = 1)

##myco.peg<-genind2loci(myco.gid) Puede utilizarse, pero es mejor cargarlo directo.

myco.haplo<-haplotype(myco.peg, locus = 1:33, check.phase=TRUE) ##Calcula los haplotipos, según esto hay 45 en este caso.

write.csv(myco.haplo, "haplotipos_myco.csv", row.names = TRUE)

myco.haplo.d<-dist.haplotype.loci(myco.haplo)

###Ahora tratamos de hacer el una red de expansión mínima (En este caso rmst)

red.simple <- rmst(myco.haplo.d) ##Esto muestra la red, vamos bien.

red.msn <- msn(myco.haplo.d)

sz <- attr(myco.haplo, "freq") ##Extrae las frecuencias de los haplotipos.

#pop.lab <- rep(paste0(1:4), each = 20)
#region <- rep(c("regA", "regB"), each = 40)

#sz <- sz[pop.lab] ##Tal como está acá es imposible asociarlas.

##Ahora tratamos de hacer una red más compleja:

#Se inclyen las frecuencias en la red:

plot(red.simple, size = sz)



#nam<-myco[,1] #nombres de individuos.

m.pop<-myco[,2] ## poblaciones

factor(m.pop)

sz.lab <- sz[m.pop] ## Combina las etiquetas con las frecuencias





P <- haploFreq(myco.peg, haplo = myco.haplo) ##poblaciones

R <- haploFreq(myco.peg, fac = reg, haplo = h) ##regiones

plot(myco.peg, size = sz, pie = P, legend = c(-25, 30))

#Package 'hierfstat' Checar este paquete, puede resultar útil.

#Además, averiguar que es el Rst. Aparece en las posibilidades de calculo de pegas. Pegas no puede calcular el Fst de datos haploides.
