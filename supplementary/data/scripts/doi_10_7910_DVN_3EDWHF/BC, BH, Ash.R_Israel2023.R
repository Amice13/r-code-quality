########### Multiple Hypothesis Testing for Israel 2023 by shingo hamanaka 20250612 ##########

##store the results of your conjoint analysis #cjoint_pool
result.summary <- summary(cjoint_pool) 



point.e <- (result.summary$amce$Estimate)
names(point.e) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

sd.errors <- (result.summary$amce$`Std. Err`)
names(sd.errors) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

p <- (result.summary$amce$`Pr(>|z|)`)
names(p) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)



#### Bonferroni correction ####ここではcjoint_poolを使っている
plot(cjoint_pool, ci = 1 - 0.05 / length(point.e))



#### Benjamini-Hochberg procedure ####
bh <- p.adjust(p, method = "BH")
print(round(bh[bh <= .05], digits = 3))

#### Adaptive Shrinkage for pooled ####
library(ashr)

ash.norm <- ash(point.e, sd.errors, mixcompdist = "normal")
ash.point <- ash.norm$result$PosteriorMean
names(ash.point) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)
ash.CI <- ashci(ash.norm)
rownames(ash.CI) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

par(mar = c(3, 11, 0, 0))
plot(NA
     , xlim = c(-.4, .5)
     , ylim = c(.5, length(ash.point) + 0.5)
     , type = "n", yaxt = "n", xlab = "", ylab = "", axes = FALSE)
abline(v = 0, col = "gray")
abline(h = 1:length(ash.point), lwd = .6, col = "gray80")
segments(x0 = ash.CI[, 1]
         , x1 = ash.CI[, 2]
         , y0 = length(ash.point):1
         , y1 = length(ash.point):1
         , lwd = 2, col = "black")
points(x = ash.point
       , y = length(ash.point):1
       , pch = 19, cex = 1.5, col = "black")
axis(1, at = seq(-0.8,0.6, by=0.1), labels = seq(-0.8,0.6, by=0.1))
axis(side = 2, 
     at = length(ash.point):1
     , labels = paste(result.summary$amce$Attribute, result.summary$amce$Level, sep = "\n")
     , las = 1, cex.axis = 0.8)

print(ash.point)

##store the results of your conjoint analysis # Anti-Bibi Group
result.summary <- summary(cjoint_antibibi) 



point.e <- (result.summary$amce$Estimate)
names(point.e) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

sd.errors <- (result.summary$amce$`Std. Err`)
names(sd.errors) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

p <- (result.summary$amce$`Pr(>|z|)`)
names(p) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)



#### Bonferroni correction ####
plot(cjoint_antibibi, ci = 1 - 0.05 / length(point.e))


#### Adaptive Shrinkage for antibibi ####
library(ashr)

ash.norm <- ash(point.e, sd.errors, mixcompdist = "normal")
ash.point <- ash.norm$result$PosteriorMean
names(ash.point) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)
ash.CI <- ashci(ash.norm)
rownames(ash.CI) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

par(mar = c(3, 11, 0, 0))
plot(NA
     , xlim = c(-.8, .4)
     , ylim = c(.5, length(ash.point) + 0.5)
     , type = "n", yaxt = "n", xlab = "", ylab = "", axes = FALSE)
abline(v = 0, col = "gray")
abline(h = 1:length(ash.point), lwd = .6, col = "gray80")
segments(x0 = ash.CI[, 1]
         , x1 = ash.CI[, 2]
         , y0 = length(ash.point):1
         , y1 = length(ash.point):1
         , lwd = 2, col = "black")
points(x = ash.point
       , y = length(ash.point):1
       , pch = 19, cex = 1.5, col = "black")
axis(1, at = seq(-0.8,0.6, by=0.1), labels = seq(-0.8,0.6, by=0.1))
axis(side = 2, 
     at = length(ash.point):1
     , labels = paste(result.summary$amce$Attribute, result.summary$amce$Level, sep = "\n")
     , las = 1, cex.axis = 0.8)

print(ash.point)

##store the results of your conjoint analysis # Pro-Natanyahu
result.summary <- summary(cjoint_probibi) 



point.e <- (result.summary$amce$Estimate)
names(point.e) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

sd.errors <- (result.summary$amce$`Std. Err`)
names(sd.errors) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

p <- (result.summary$amce$`Pr(>|z|)`)
names(p) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)



#### Bonferroni correction ####ここではcjoint_poolを使っている
plot(cjoint_probibi, ci = 1 - 0.05 / length(point.e))




#### Adaptive Shrinkage for probibi ####
library(ashr)

ash.norm <- ash(point.e, sd.errors, mixcompdist = "normal")
ash.point <- ash.norm$result$PosteriorMean
names(ash.point) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)
ash.CI <- ashci(ash.norm)
rownames(ash.CI) <- paste(result.summary$amce$Attribute, result.summary$amce$Level)

par(mar = c(3, 11, 0, 0))
plot(NA
     , xlim = c(-.4, .5)
     , ylim = c(.5, length(ash.point) + 0.5)
     , type = "n", yaxt = "n", xlab = "", ylab = "", axes = FALSE)
abline(v = 0, col = "gray")
abline(h = 1:length(ash.point), lwd = .6, col = "gray80")
segments(x0 = ash.CI[, 1]
         , x1 = ash.CI[, 2]
         , y0 = length(ash.point):1
         , y1 = length(ash.point):1
         , lwd = 2, col = "black")
points(x = ash.point
       , y = length(ash.point):1
       , pch = 19, cex = 1.5, col = "black")
axis(1, at = seq(-0.8,0.6, by=0.1), labels = seq(-0.8,0.6, by=0.1))
axis(side = 2, 
     at = length(ash.point):1
     , labels = paste(result.summary$amce$Attribute, result.summary$amce$Level, sep = "\n")
     , las = 1, cex.axis = 0.8)

print(ash.point)
