#### mediation script                        
#load libraries
library(mediation)
library(gtsummary)
set.seed(2021)



##########################################################################################
## USA Survey ##
usa <- read.csv("usa_main.csv")  ## import .csv for US survey
keep <- c("female", "party", "education","environment", "anti_protest", "concede", 
          "repress", "natsec", "intervene", "age", "commit")
keep1 = usa[keep]
usa1 <- keep1[complete.cases(keep1),]

## Running Mediation Analysis

## Commitment mediator
  #concede
med.usa.c1 <- lm(commit ~ intervene + age + environment + anti_protest + female + 
                          education + party, data = usa1)
out.usa.c1 <- lm(concede ~ commit + intervene + age + environment + anti_protest + 
                           female + education + party, data = usa1)
usa.med.commit1 <- mediate(med.usa.c1, out.usa.c1, treat = "intervene", 
                           mediator = "commit", robustSE = TRUE, sims = 999)

  # repress
med.usa.c2 <- lm(commit ~ intervene + age + environment + anti_protest + female + 
                 education + party, data = usa1)
out.usa.c2 <- lm(repress ~ commit + intervene + age + environment + anti_protest + 
                 female + education + party, data = usa1)

usa.med.commit2 <- mediate(med.usa.c2, out.usa.c2, treat = "intervene", 
                           mediator = "commit", robustSE = TRUE, sims = 999)

## Energy security mediator
  # concede
med.usa.ns1 <- lm(natsec ~ intervene + age + environment + anti_protest + female + 
                 education + party, data = usa1)
out.usa.ns1 <- lm(concede ~ natsec + intervene + age + environment + anti_protest + 
                            female + education + party, data = usa1)
usa.med.natsec1 <- mediate(med.usa.ns1, out.usa.ns1, treat = "intervene", 
                           mediator = "natsec", robustSE = TRUE, sims = 999)

  # repress
med.usa.ns2 <- lm(natsec ~ intervene + age + environment + anti_protest + female + 
                 education + party, data = usa1)
out.usa.ns2 <- lm(repress ~ natsec + intervene + age + environment + anti_protest + 
                            female + education + party, data = usa1)
usa.med.natsec2 <- mediate(med.usa.ns2, out.usa.ns2, treat = "intervene", 
                           mediator = "natsec", robustSE = TRUE, sims = 999)


##########################################################################################
## Canada Survey
can <- read.csv("can_main.csv")  ## import .csv for US survey
keep <- c("repress", "concede", "commit","intervene", "age", "environment", 
          "anti_protest", "female", "education", "party", "natsec")
keep1 = can[keep]
can1 <- keep1[complete.cases(keep1),]



## Commitment mediator
  # concede
med.can.c1 <- lm(commit ~ intervene + age + environment + anti_protest + 
                          female + education + party, data = can1)
out.can.c1 <- lm(concede ~ commit + intervene + age + environment + anti_protest + 
                           female + education + party, data = can1)
can.med.commit1 <- mediate(med.can.c1, out.can.c1, treat = "intervene", 
                           mediator = "commit", robustSE = TRUE, sims = 999)

  # repress
med.can.c2 <- lm(commit ~ intervene + age + environment + anti_protest + female + 
                          education + party, data = can1)
out.can.c2 <- lm(repress ~ commit + intervene + age + environment + anti_protest + 
                           female + education + party, data = can1)
can.med.commit2 <- mediate(med.can.c2, out.can.c2, treat = "intervene", 
                           mediator = "commit", robustSE = TRUE, sims = 999)

## Natsec mediator
  # concede
med.can.ns1 <- lm(natsec ~ intervene + age + environment + anti_protest + female + 
                           education + party, data = can1)
out.can.ns1 <- lm(concede ~ natsec + intervene + age + environment + anti_protest + 
                            female + education + party, data = can1)
can.med.natsec1 <- mediate(med.can.ns1, out.can.ns1, treat = "intervene", 
                           mediator = "natsec", robustSE = TRUE, sims = 999)


  # repress
med.can.ns2 <- lm(natsec ~ intervene + age + environment + anti_protest + female + 
                           education + party, data = can1)
out.can.ns2 <- lm(repress ~ natsec + intervene + age + environment + anti_protest + 
                            female + education + party, data = can1)
can.med.natsec2 <- mediate(med.can.ns2, out.can.ns2, treat = "intervene", 
                           mediator = "natsec", robustSE = TRUE, sims = 999)

##########################################################################################

## Plotting mediator results for Figure 4 in main text -- see generated pdf
pdf(file = "Figure4.pdf", width = 8, height = 5)  # saving pdf plot
par(mfrow=c(2,4), oma = c(3, 1, 2, 1)) # Graph window to 2 rows and 4 columns

## Commitment mediator
plot(usa.med.commit1, xlab="Effect", xlim = c(-.3, .1),      
     col="royalblue", family="Times", main="(a) Concede (US)") # USA: concede
plot(usa.med.commit2, xlab="Effect", xlim = c(-.1, .35),     
     col="royalblue", family="Times", main="(b) Repress (US)") # USA: repress
plot(can.med.commit1, xlab="Effect", xlim = c(-.3, .1), 
     col="firebrick1", family="Times", main="(c) Concede (CA)") # Canada: concede
plot(can.med.commit2, xlab="Effect", xlim = c(-.1, .45), 
     col="firebrick1", family="Times", main="(d) Repress (CA)") # Canada: concede
mtext("Credibility of Protester Commitment", side=3, line=0, outer = TRUE, cex=1.1, family="Times")
## Natsec mediator
plot(usa.med.natsec1, xlab="Effect", xlim = c(-.3, .1), 
     col="royalblue", family="Times", main="(e) Concede (US)") # USA: concede 
plot(usa.med.natsec2, xlab="Effect", xlim = c(-.1, .35), 
     col="royalblue", family="Times", main="(f) Repress (US)") # USA: repress 
plot(can.med.natsec1, xlab="Effect", xlim = c(-.3, .1), 
     col="firebrick1", family="Times", main="(g) Concede (CA)") # Canada: concede
plot(can.med.natsec2, xlab="Effect", xlim = c(-.1, .45), 
     col="firebrick1", family="Times", main="(h) Repress (CA)") # Canada: repress
mtext("Energy Security", side=3, line=-17.2, outer = TRUE, cex=1.1, family="Times")

dev.off()

##########################################################################################
## Generating Table A6.1

  # commitment
usa_commit1 <- summary(usa.med.commit1)
can_commit1 <- summary(can.med.commit1)

usa_commit2 <- summary(usa.med.commit2)
can_commit2 <- summary(can.med.commit2)

  # energy security
usa_natsec1 <- summary(usa.med.natsec1)
can_natsec1 <- summary(can.med.natsec1)

usa_natsec2 <- summary(usa.med.natsec2)
can_natsec2 <- summary(can.med.natsec2)

## Display results for constructing Table A6.1
	#top 2 tables (USA sample)
usa_commit1; usa_commit2
	#top 2 tables (CAN sample)
can_commit1; can_commit2
	#bottom 2 tables (USA sample)
usa_natsec1; usa_natsec2
	#bottom 2 tables (CAN sample)
can_natsec1; can_natsec2


##########################################################################################
## Sensitivity Analysis for Figures A6.1 and A6.2

sens.usa.commit1 <- medsens(usa.med.commit1, rho.by = 0.05) 
sens.usa.commit2 <- medsens(usa.med.commit2, rho.by = 0.05)
sens.usa.natsec1 <- medsens(usa.med.natsec1, rho.by = 0.05)
sens.usa.natsec2 <- medsens(usa.med.natsec2, rho.by = 0.05)

sens.can.commit1 <- medsens(can.med.commit1, rho.by = 0.05)
sens.can.commit2 <- medsens(can.med.commit2, rho.by = 0.05)
sens.can.natsec1 <- medsens(can.med.natsec1, rho.by = 0.05)
sens.can.natsec2 <- medsens(can.med.natsec2, rho.by = 0.05)

## Plotting Sensitivity Analysis for Appendix A6

##### Figure A6.1
## USA sample


# Plotting mediator results for Figure 6.1 (top half) in A6.1
pdf(file = "FigureA6.1ab.pdf", width = 6, height = 3.5)  # saving pdf plot
windowsFonts(Times = windowsFont("Times New Roman"))  # Font to Times New Roman
par(mfrow=c(1,2)) # Graph window to 2 rows and 1 columns
  plot(sens.usa.commit1, sens.par = "rho", 
     family="Times", main="(a) Concede")    # (a) USA: concede outcome
  plot(sens.usa.commit2, sens.par = "rho",
     family="Times", main="(b) Repress")    # (b) USA: repress outcome
dev.off()  
  

# Plotting mediator results for Figure 6.1 (bottom half) in A6.1
pdf(file = "FigureA6.1cd.pdf", width = 6, height = 3.5)  # saving pdf plot
windowsFonts(Times = windowsFont("Times New Roman"))  # Font to Times New Roman
par(mfrow=c(1,2)) # Graph window to 2 rows and 1 columns
  plot(sens.usa.natsec1, sens.par = "rho",
     family="Times", main="(c) Concede")   # (c) USA: concede outcome
  plot(sens.usa.natsec2, sens.par = "rho",
     family="Times", main="(d) Repress")   # (d) USA: repress outcome
dev.off()  


##### Figure A6.2
  # Canadian sample     

# Plotting mediator results for Figure 6.2 (top half) in A6.2
pdf(file = "FigureA6.2ab.pdf", width = 6, height = 3.5)  # saving pdf plot
windowsFonts(Times = windowsFont("Times New Roman"))  # Font to Times New Roman
par(mfrow=c(1,2)) # Graph window to 2 rows and 1 columns
  plot(sens.can.commit1, sens.par = "rho",
     family="Times", main="(a) Concede")   # (a) Canadian: concede outcome
  plot(sens.can.commit2, sens.par = "rho",
     family="Times", main="(b) Repress")   # (b) Canadian: concede outcome
dev.off()


# Plotting mediator results for Figure 6.2 (bottom half) in A6.2
pdf(file = "FigureA6.2cd.pdf", width = 6, height = 3.5)  # saving pdf plot
windowsFonts(Times = windowsFont("Times New Roman"))  # Font to Times New Roman
par(mfrow=c(1,2)) # Graph window to 2 rows and 1 columns
  plot(sens.can.natsec1, sens.par = "rho",
     family="Times", main="(c) Concede")   # (c) Canadian: concede outcome
  plot(sens.can.natsec2, sens.par = "rho",
     family="Times", main="(d) Repress")   # (d) Canadian: repress outcome 
dev.off()  