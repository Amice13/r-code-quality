###########################################
###########################################
# Codigo para el capítulo
# "La cartografía y las ciencias sociales"
###########################################
###########################################

rm(list=ls())

library(rgdal)
source("Mapping_Applications.R")
source("Spatial_Utilities.R")
source("mymap.R")

load("Datos_Cartografia_CS.RData")


################################################################################
################################################################################
# Mapa 1 - voto PP y PSOE en Salamanca
################################################################################
################################################################################


windowsFonts(helvetica=windowsFont("Arial"))

par(mfrow=c(1,2))
mapa(di, di$p_pp, labels = di$di, legpos = c(-5.67,40.94), lab.cex = 2)
text(-5.735,40.99, "(a) PP (%)", family="helvetica", adj = 0, cex=2)

mapa(di, di$p_psoe, labels = di$di, palette = "Reds", legpos = c(-5.67,40.94), lab.cex = 2)
text(-5.735,40.99, "(b) PSOE (%)", family="helvetica", adj = 0, cex=2)

par(mfrow=c(1,1))


################################################################################
################################################################################
# Mapa 2 - voto PP y PSOE en Salamanca por sección
################################################################################
################################################################################


windowsFonts(helvetica=windowsFont("Arial"))

par(mfrow=c(1,2))
par(mar=rep(0,4))
mapa(sa, sa$p_pp, frame2=di, border = "grey70", legpos = c(-5.67,40.94), lab.cex = 2)
text(-5.735,40.99, "(a) PP (%)", family="helvetica", adj = 0, cex=2)
mapa(sa, sa$p_psoe, frame2=di, border = "grey70", palette="Reds", legpos = c(-5.67,40.94), lab.cex = 2)
text(-5.735,40.99, "(b) PSOE (%)", family="helvetica", adj = 0, cex=2)
par(mfrow=c(1,1))



################################################################################
################################################################################
# Mapa 3 - voto PP, Podemos y 64 más en Salamanca por sección 
################################################################################
################################################################################


par(mfrow=c(1,3))
mapa(sa, sa$p_pp, frame2=di, border = "grey70", cex.leg = 1.6, legpos = c(-5.665,40.955))
text(-5.735,40.99, "(a) PP", family="helvetica", adj = 0, cex=2)

mapa(sa, sa$p_up, frame2=di, border = "grey70", palette="Purples", cex.leg = 1.6, legpos = c(-5.665,40.955))
text(-5.735,40.99, "(b) 64+ años", family="helvetica", adj = 0, cex=2)

mapa(sa, sa$p_64m, frame2=di, border = "grey70", palette="Greens", cex.leg = 1.6, legpos = c(-5.665,40.955))
text(-5.735,40.99, "(c) Podemos", family="helvetica", adj = 0, cex=2)
par(mfrow=c(1,1))


################################################################################
################################################################################
# Figura 1 - Scatterplot voto PP y Mayores de 64 anios
################################################################################
################################################################################

col <- c("royalblue","red2","#FFCC00","#44AA00")
sa$col <- NA
sa$col[sa$p_64m<mean(sa$p_64m) & sa$p_pp<mean(sa$p_pp)] <- col[1]
sa$col[sa$p_64m>mean(sa$p_64m) & sa$p_pp>mean(sa$p_pp)] <- col[2]
sa$col[sa$p_64m<mean(sa$p_64m) & sa$p_pp>mean(sa$p_pp)] <- col[3]
sa$col[sa$p_64m>mean(sa$p_64m) & sa$p_pp<mean(sa$p_pp)] <- col[4]

par(mar=rep(4,4))
plot(sa$p_64m, sa$p_pp, axes=F, xlab="Mayores de 64 años (%)", ylab="Voto al PP (%)", pch=19, col=sa$col)
abline(v=mean(sa$p_64m), h=mean(sa$p_pp), col="grey40")
abline(reg = lm(sa$p_pp~sa$p_64m), lwd=2)
text(x=55,y=72, labels = paste("r=",round(cor(sa$p_64m, sa$p_pp, method="spearman"),3)))


################################################################################
################################################################################
# Mapa 4 - voto PP y Podemos en Salamanca por sección combinado con edad
################################################################################
################################################################################

par(mfrow=c(1,2))
qdmap(sa, sa$p_pp, sa$p_64m, frame = di, cor.pos = c(-5.71,40.99), lang = "es", cex.leg = 1.3, leg.pos = c(-5.67,40.935))

qdmap(sa, sa$p_up, sa$p_64m, frame = di, cor.pos = c(-5.71,40.99), lang = "es", cex.leg = 1.3, leg.pos = c(-5.67,40.935))

par(mfrow=c(1,1))


################################################################################
################################################################################
# Mapa 5 - British Overseas Territories
################################################################################
################################################################################


par(mar=rep(0,4))
plot(w, col="grey90", border="transparent")
plot(wc, col="darkgreen", border="transparent", add=T)
plot(bot, pch=19, col="red", cex=1, add=T)

text(coordinates(bot), labels = bot$territory, pos = bot$pos, cex = 1.2)

legend(60,75,legend = "Territorios Británicos de Ultramar", pch=19, col="red", bty="n", x.intersp = 0.2, cex=1.2)
legend(60,70,legend = "Mancomunidad de Naciones", pch=19, col="darkgreen", bty="n", x.intersp = 0.2, cex=1.2)


################################################################################
################################################################################
# Mapa 6 - Hotspots criminalidad en Washington
################################################################################
################################################################################

par(mfrow=c(1,3))
kernelmap(cr, bl, bl, alpha = 0.0045)

kernelmap(cr[cr$OFFENSE%in%c("THEFT/OTHER","THEFT F/AUTO"),], bl, bl, alpha = 0.0045)

kernelmap(cr[cr$OFFENSE%in%c("HOMICIDE","ASSAULT W/DANGEROUS WEAPON"),], bl, bl, alpha = 0.0045)

par(mfrow=c(1,1))



################################################################################
################################################################################
# Mapa 7 - Conglomerados espaciales voto PP, PSOE en Salamanca
################################################################################
################################################################################


par(mfrow=c(1,2))
lisamap(sa, sa$p_pp, frame = di, lang = "es")
plot(di, lwd=2, add=T)
lisamap(sa, sa$p_psoe, frame=di)
plot(di, lwd=2, add=T)
par(mfrow=c(1,1))


################################################################################
################################################################################
# Mapa 8 - Visitas presidenciales
################################################################################
################################################################################


par(mfrow=c(1,2))
par(mar=rep(0,4))
plot(e)
plot(pDi, add=T, lwd=pDi$lwd, col="red")

propmap(bDi, bDi$dilma, add=T, col="red", trans=95, leg.pos=c(-75.5,-11), title.leg="Visitas", y.int=0.9, leg.brks = 2, leg.cex = 1.2)
legend("bottomleft", col="red", lwd=c(10,5.7,1),legend=c(7,4,1), bty="n", title="Viajes", cex=1, y.intersp=0.5, x.intersp=0.5,title.adj=0.35 )


plot(e)
plot(pAe, add=T, lwd=pAe$lwd, col="blue")
propmap(bAe, bAe$aecio, add=T, col="blue", trans=95, leg.pos=c(-75.5,-11), title.leg="Visitas", y.int=0.9, leg.brks = 2, leg.cex = 1.2, symbol.bg = "blue")
legend("bottomleft", col="blue", lwd=c(10,5.7,1),legend=c(7,4,1), bty="n", title="Viajes", cex=1, y.intersp=0.5, x.intersp=0.5,title.adj=0.35 )
par(mfrow=c(1,1))




################################################################################
################################################################################
# Mapa 9 - Mapa de buffer y Thiessen Polygons
################################################################################
################################################################################



windowsFonts(helvetica=windowsFont("Arial"))

par(mfrow=c(1,2))
par(mar=rep(0,4))

plot(sa)
plot(bf, col="orange", border="transparent", add=T)
plot(esc, pch=19, col="red", add=T)
plot(sa, add=T)
text(-5.735,40.99, "(a) Buffer", family="helvetica", adj = 0)


plot(sa, border="grey90")
plot(vo, add=T)
plot(esc, pch=19, col="red", add=T)
text(-5.735,40.99, "(b) Thiessen", family="helvetica", adj = 0)

par(mfrow=c(1,1))



################################################################################
################################################################################
# Mapa 10 - Mapa dasimétrico del voto en Madrid
################################################################################
################################################################################


library(maptools)
library(sp)

windowsFonts(helvetica=windowsFont("Arial"))

mapa(md, md$p_pp, border="transparent", legpos = c(452776,4465289), cex.leg = 2, frame1=mad, bordframe1 = "grey80")
text(463064,4498051, labels = "PP", cex = 6)
text(coordinates(mad), labels = mad$Texto, col="red", family="helvetica")

