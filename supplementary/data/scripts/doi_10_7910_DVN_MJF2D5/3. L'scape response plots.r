############# LOAD DATA AND LIBRARIES
setwd("")
load("data_st2.R")
load("response_DFs_norichact2.R")
WetlandsW <- read.csv("Batsinsects.csv")
library(lme4)

# SORT EACH VARIABLE OF INTEREST, GET UNIQUE VALUES
Distbushland <- sort(unique(data_st$Distbushland))
TreeCoverage <- sort(unique(data_st$TreeCoverage))
Light5km  <- sort(unique(data_st$Light5km))
Habitat  <- sort(unique(data_st$Habitat))

### CREATE LISTS THAT CONTAIN EACH VARIABLE
Predictors <- list(Habitat, Light5km, Distbushland, TreeCoverage)

#### DEFINE THE SPECIES RICHNESS MODEL AND RESPONSES
sp_rich <- glmer(BatSpecies ~ Habitat + Light5km + Distbushland + TreeCoverage + Moon + (1|Season) + (1|Site), control=glmerControl(optimizer="bobyqa"),family = poisson, data =data_st)
b <- fixef(sp_rich)
rich_hab1 <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 
rich_hab2 <- b[1]+ b[2] * 1 + b[3] * 0 + b[4] * 0 + b[5] * 0 + b[6] * 0 
rich_light <- b[1]+ b[2] * 0 + b[3] * Light5km + b[4] * 0 + b[5] * 0 + b[6] * 0 
rich_bush <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * Distbushland + b[5] * 0 + b[6] * 0 
rich_tree <- b[1]+ b[2] * 0 + b[3] * 0 + b[4] * 0 + b[5] * TreeCoverage + b[6] * 0 
erich_light <- exp(rich_light)
erich_bush <- exp(rich_bush)
erich_tree <- exp(rich_tree)
erich_hab1 <- exp(rich_hab1)
erich_hab2 <- exp(rich_hab2)

U <- chol(as.matrix(vcov.merMod(sp_rich)))

#################################################### PLOTS
windows(10, 8)
par(mfrow = c(2, 3), mar = c(3, 2, 2, 1), mgp = c(2.5, 1, 0), oma = c(0, 3, 0, 0)) # SET UP THE INDIVIDUAL PLOT WINDOWS AND MARGINS

##### HABITAT
sd <- sqrt(apply(U %*% rbind(rep(1,length(Habitat)), Habitat, 
                             rep(0,length(Habitat)), rep(0,length(Habitat)),
                             rep(0,length(Habitat)), rep(0,length(Habitat))),2, function(x)sum(x^2)))

heights <- c(erich_hab1, erich_hab2) 
xlocs <- barplot(heights, names.arg = c("Wetland", "Non-wetland"), ylim = c(0, 15), axis.lty=1, xlab = "", font.lab = 2, cex.lab = 1.1, ylab = "", main = "a)") 
arrows(xlocs[1], exp(rich_hab1-2*sd[1]), xlocs[1], exp(rich_hab1+2*sd[1]),length=0.05,angle=90, code=3)
arrows(xlocs[2], exp(rich_hab2-2*sd[2]), xlocs[2], exp(rich_hab2+2*sd[2]),length=0.05,angle=90, code=3)
mtext("Species richness", side = 2, cex = 0.8, font = 2, line = 3)

##### LIGHT LEVELS
sd <- sqrt(apply(U %*% rbind(rep(1,length(Light5km)), rep(0,length(Light5km)), 
                             Light5km, rep(0,length(Light5km)),
                             rep(0,length(Light5km)), rep(0,length(Light5km))),2, function(x)sum(x^2)))
plot (data_st$Light5km, data_st$BatSpecies, xlab="", ylab = "", type = "n", ylim = c(0,15), axes = F, font.lab = 2, cex.lab = 1.1, main = "b)")
lines(Light5km, erich_light, lwd=1, lty = 1) 
polygon(c(Light5km, sort(Light5km, decreasing=T)),c(sort(exp(rich_light-2*sd), decreasing=T), sort(exp(rich_light+2*sd), decreasing=F)),col=rgb(0,0,0,0.3), border=NA)
axis(2, cex.axis = 1.1)
xlab.new <- c(0, 10, 20, 30, 40, 50, 60)
xat.new <-((xlab.new) - mean(WetlandsW$Light5km, na.rm = T))/ sd(WetlandsW$Light5km, na.rm = T)
axis(1,at=xat.new,labels=xlab.new, cex.axis = 1)
box(bty = "L")

##### Dist to bushland
sd <- sqrt(apply(U %*% rbind(rep(1,length(Distbushland)), rep(0,length(Distbushland)), 
                             rep(0,length(Distbushland)), Distbushland, 
                             rep(0,length(Distbushland)), rep(0,length(Distbushland))),2, function(x)sum(x^2)))
plot (data_st$Distbushland, data_st$BatSpecies, xlab="", ylab = "", type = "n", ylim = c(0,15), axes = F, font.lab = 2, cex.lab = 1.1, main = "c)")
lines(Distbushland, erich_bush, lwd=1, lty = 1) 
polygon(c(Distbushland, sort(Distbushland, decreasing=T)),c(sort(exp(rich_bush-2*sd), decreasing=T), sort(exp(rich_bush+2*sd), decreasing=F)),col=rgb(0,0,0,0.3), border=NA)
axis(2, cex.axis = 1.1)
xlab.new <- c(0, 2000, 4000, 6000, 8000, 10000)
xat.new <-((xlab.new) - mean(WetlandsW$Distbushland, na.rm = T))/ sd(WetlandsW$Distbushland, na.rm = T)
axis(1,at=xat.new,labels=xlab.new, cex.axis = 0.8)
box(bty = "L")


### INDIVIDUAL SPECIES PLOTS hab light, sz, bush, tree, wat
## 1, 2, 4, 5

############################################################################## HABITAT
par(mar = c(4, 2, 1, 1)) 

response <- responses_norichact[[1]]

response1 <- responses_norichact[[1]][,1]
response2 <- responses_norichact[[1]][,2]
xs <- xlocs[,1]

plot(xs, c(min(response1), max(response1)), type = "n", ylab = "", xlab = "Site type", ylim = c(-0.5, 4), axes = F, font.lab = 2, cex.lab = 1.1, xlim = c(0.3, 2.3), main = "d)")
mtext("Scaled species activity", side = 2, cex = 0.8, font = 2, line = 3)
axis(2, cex.axis = 1)
axis(1,at=c(xs),labels=c("Wetland", "Non-wetland"), cex.axis = 1)
back = c("red3", "brown", "royalblue1", "seagreen", "gold1", "purple4", "black", "palevioletred2")
for (b in 1:length(response1)){
  points(xlocs, c(response1[b], response2[b]), col = back[b], bg = back[b], pch = 21, cex = 1)
}
box(bty = "L")

for (b in 1:length(response1)){
  lines(xlocs, c(response1[b], response2[b]), col = back[b], lty = 2)
}

legend(1.8,4, c("Cg", "Vv", "Cm", "Scot", "CgMp", "Nyct", "Moo", "Vd"), pch = 22, pt.bg = c("red3", "brown", "seagreen", "gold1", "purple4", "black", "palevioletred2", "royalblue1"), col = c("red3", "brown", "seagreen", "gold1", "purple4", "black", "palevioletred2", "royalblue1"), bty = "n")



#points(xlocs, c(response1[b], response2[b]), col = back[b], bg = back[b], pch = 21, cex = 2)

################################################################### LIGHT
response_all <- responses_norichact[[2]]
response <- response_all[,c(2:4, 6, 8:10)]
plot(Predictors[[2]], response[,1], type = "n", ylab = "", xlab = "Light (VNIR)", ylim = c(-4, 4), axes = F, font.lab = 2, xlim = c(min(Predictors[[2]]), max(Predictors[[2]])), cex.lab = 1.1, main = "e)")
axis(2, cex.axis = 0.8)
xlab.new <- c(0, 10, 20, 30, 40, 50, 60)
xat.new <-((xlab.new) - mean(WetlandsW$Light5km, na.rm = T))/ sd(WetlandsW$Light5km, na.rm = T)
axis(1,at=xat.new,labels=xlab.new, cex.axis = 1)
for (b in 1:length(response)){
  lines(Predictors[[2]], response[,b])
}
lines(Predictors[[2]], response_all[,1], col = "grey70", lwd = 2)
lines(Predictors[[2]], response_all[,5], lty = 2)
lines(Predictors[[2]], response_all[,7], lty = 1, lwd = 2)
box(bty = "L")
legend(-3, -3, c("Cg", "Aa", "CgMp"), col = c("grey70", "black", "black"), lwd = c(2, 1, 2), lty = c(1, 2, 1), cex = 0.8, bty = "n") #### 


############################################################# DISTBUSHLAND

responses <- responses_norichact[[4]]
response <- responses[,c(1:4, 6:7)]
plot(Predictors[[3]], response[,1], type = "n", ylab = "", xlab = "Distance to bushland (m)", ylim = c(-3, 3), axes = F, font.lab = 2, xlim = c(min(Predictors[[3]]), max(Predictors[[3]])), cex.lab = 1.1, main = "f)")
axis(2, cex.axis = 0.8)
xlab.new <- c(0, 2000, 4000, 6000, 8000, 10000)
xat.new <-((xlab.new) - mean(WetlandsW$Distbushland, na.rm = T))/ sd(WetlandsW$Distbushland, na.rm = T)
axis(1,at=xat.new,labels=xlab.new, cex.axis = 0.8)
for (b in 1:length(response)){
  lines(Predictors[[3]], response[,b])
}
box(bty = "L")

# ################################################## TREES

dev.copy2pdf(file = "Landscape responses.pdf")
