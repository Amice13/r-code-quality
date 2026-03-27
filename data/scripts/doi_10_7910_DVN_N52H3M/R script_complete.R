setwd("C:\\Documents and Settings\\John\\My Documents\\Server\\Selection\\JELS Submission")
#KLEIN-HUME GRAPHS
klein <- read.dta("Fear of Reversal Data.dta")
attach.all(klein)

#each cells are number of cases

#Liberal Lower Court
#
#                      liberal dec     conservative dec
#
#compliant               A                 B
#
#non-compliant           C               D
#
#Conservative Lower Court
#
#                     liberal decision    conservative dec
#
#compliant                 E                   F
#     
#non-compliant              G                   H

a <- length(upheld[comply==1 & upheld==0 & lib.panel==1])
b <- length(upheld[comply==1 & upheld==1 & lib.panel==1])
c <- length(upheld[comply==0 & upheld==0 & lib.panel==1])
d <- length(upheld[comply==0 & upheld==1 & lib.panel==1])
e <- length(upheld[comply==1 & upheld==0 & lib.panel==0])
f <- length(upheld[comply==1 & upheld==1 & lib.panel==0])
g <- length(upheld[comply==0 & upheld==0 & lib.panel==0])
h <- length(upheld[comply==0 & upheld==1 & lib.panel==0])
lib.con2 <- 100*c((a+c)/(a+b+c+d), (e+g)/(e+f+g+h)) #percent or liberal decisions
per.noncomp <- 100 - (100 *c((a+b)/(a+b+c+d), (e+f)/(e+f+g+h))) #percent non-compliance by lower courts
#get counts
noncomp.lib <- 1-mean(comply[upheld==0])
noncomp.con <- 1- mean(comply[upheld==1])
noncomp.all <- 1-mean(comply)
#matrices for stacked bars
noncomply.decisions <- 100*cbind(c(noncomp.all, 1-noncomp.all), c(noncomp.lib,1-noncomp.lib), c(noncomp.con, 1-noncomp.con))
noncomply.decisions.noall <- 100*cbind(c(noncomp.lib,1-noncomp.lib), c(noncomp.con, 1-noncomp.con))
#vectors for simple bars (don't add up to 100)
noncompcase <- 100*c(noncomp.all, noncomp.lib, noncomp.con)
#Direction of non-compliance by Panel Ideology
non.comp.direction <- 100 * c(c/(c+d),g/(g+h))
#The Non-Compliance Puzzle
##
#1) #[][]              % Liberal outcomes: lib pans
##   []                % Liberal outcomes: conser pans 

#2) #[][]              % Noncomp outcomes: lib pans
##   ][]               % Noncomp outcomes: con pans

#3) [][]             Percent of Con Decisions that are non-compliant
#    [][]            Percent of Lib Decisions that are non-compliant

#4) #[][][][][][][]]   % non-compliant conserv decisions by liberal panels
##   [][][             % noncomp lib decision by conser panels.
##
#5) #[][][][][][]]    % liberal outcomes: liberal panels making non-compliant decisions
#   #[][][][]          % liberal outcomes: conser panels making non-compliant decisions
##
#0   20   40   60   80   100 

#need to combine into matrix; first make sure we have all necessary vectors
#need to invert vectors to make sure bars appear in right order
lib.con2a <- c(lib.con2[2], lib.con2[1])
per.noncomp.a <- c(per.noncomp[2], per.noncomp[1])
noncompcase2 <- 100*c(noncomp.con, noncomp.lib)
wrong.comply <- 100 * c(mean(1-upheld[lib.panel==0 & comply==0]),
    mean(upheld[lib.panel==1 & comply==0]))
non.comp.outcome <- 100 * cbind(c(d/(d+h), c(c/(c+g))))

#invert order so that bars appear from top to bottom in bar graph

graph.grand <- cbind(wrong.comply,  noncompcase2, per.noncomp, lib.con2a)

##################################################

#GRAND BAR GRAPH - different shading across 4 groups

pdf(file="cpuzzle_grand.pdf", width = 7, height = 5.5) 

par(mfrow=c(1,1), mar=c(4,1,2,2))
bar.grand <- barplot(graph.grand, main = "",
    xlim = c(0,100), xlab = "", beside=T, 
    axisnames=FALSE, axes =F, horiz=T,
    #col = c(gray(.6), gray(.6),
    #gray(.6), gray(.6), gray(.6), gray(.6),gray(.6), gray(.6))
    )
axis(1, at=c(0,20,40,60,80,100), c(0,20,40,60,80,100), cex = 1, las = 1, tick = T)#las tells R to make the labels on the y-axis horizontal
# bar.grand
#     [,1] [,2] [,3] [,4] [,5]
#[1,]  1.5  4.5  7.5 10.5 13.5
#[2,]  2.5  5.5  8.5 11.5 14.5
#graph.grand
#non.comp.outcome    wrong.comply noncompcase2 per.noncomp lib.con2a
#[1,] 50.00000      37.2093       14.17323      23.45133    12.50000
#[2,] 61.90476      50.9434       60.86957      19.19643    18.14159

text.size <- 1
text.offset <- 2.5
text(graph.grand[2,4] + text.offset , bar.grand[2,4], "% of decisions by liberal panels that are liberal", adj=0, cex = text.size)
text(graph.grand[1,4] + text.offset , bar.grand[1,4], "% of decisions by conservative panels that are liberal", adj=0, cex = text.size)

text(graph.grand[2,3] + text.offset , bar.grand[2,3], "% of decisions by liberal panels that are non-compliant",adj=0, cex = text.size)
text(graph.grand[1,3] + text.offset , bar.grand[1,3], "% of decisions by conservative panels that are non-compliant", adj=0, cex = text.size)

text(graph.grand[2,2] + text.offset , bar.grand[2,2], "% of liberal decisions that\nare non-compliant",  adj=0, cex = text.size)
text(graph.grand[1,2] + text.offset , bar.grand[1,2], "% of conservative decisions that are non-compliant", adj=0, cex = text.size)

text(graph.grand[2,1] + text.offset , bar.grand[2,1] + .1, "% of non-compliant decisions by\nliberal panels that are conservative",
     adj=0, cex = text.size)
text(graph.grand[1,1] + text.offset , bar.grand[1,1] - .1, "% of non-compliant decisions by\nconservative panels that are liberal", 
    adj=0, cex = text.size)

dev.off()

#################################################################
#################

#   SIMULATION PLOTS

#note: for now, only use "out-of-sample" predictions
#note: flips, falsepos and falseneg are in percentage terms -- need to adjust for graphs
#note: set sim.n.all manually, since it comes from length of simulation data base

simulation.data <- read.dta("simulation_data_base.dta")
sim.n.all <- nrow(simulation.data)#get length of simulation data

#1) All Cases Selected

all.cases <- read.dta("sim_results_all_cases.dta")
detach()
attach.all(all.cases)

    #individual coefficients and standard errors for each coefficient
all.home.coef <-  sim.b.home
all.home.se <- sim.se.home
all.business.coef <- sim.b.business
all.business.se <- sim.se.business
all.person.coef <- sim.b.person
all.person.se <- sim.se.person
all.car.coef <- sim.b.car
all.car.se <- sim.se.car
all.extent.coef <- sim.b.extent
all.extent.se <- sim.se.extent 
all.warrant.coef <-  sim.b.warrant
all.warrant.se <- sim.se.warrant
all.incident.coef <- sim.b.incident
all.incident.se <- sim.se.incident
all.afterlaw.coef <- sim.b.afterlaw
all.afterlaw.se <- sim.se.afterlaw
all.unlawful.coef <- sim.b.unlawful
all.unlawful.se <- sim.se.unlawful 
all.except.coef <-  sim.b.except
all.except.se <- sim.se.except
all.change.coef <- sim.b.change
all.change.se <- sim.se.change
all.probcaus.coef <- sim.b.probcaus
all.probcaus.se <- sim.se.probcaus
all.home.impact <- sim.home.avgimpact
all.except.impact <- sim.except.avgimpact

##########################################################################

#2)Random results

random <- read.dta("sim_results_random.dta")
#random <- random[random$sim.n.in == 300,] #use if you only want observation where
            #n of in-sample == 300

detach()
attach.all(random)
random.n.in <- sim.n.in # get number of insample cases (note that they are all random)
random.n.out <- sim.n.out #number of out-of-sample cases (n = 12310)
random.flips.out <- sim.caseflips.out #percent of out-of-sample caseflips
random.falsepos.out <-  sim.falsepos.out #percent of false positives
random.falseneg.out <-  sim.falseneg.out # percent of false negatives

    #individual coefficients
random.home.coef <-  sim.b.home
random.home.se <- sim.se.home
random.business.coef <- sim.b.business
random.business.se <- sim.se.business
random.person.coef <- sim.b.person
random.person.se <- sim.se.person
random.car.coef <- sim.b.car
random.car.se <- sim.se.car
random.extent.coef <- sim.b.extent
random.extent.se <- sim.se.extent 
random.warrant.coef <-  sim.b.warrant
random.warrant.se <- sim.se.warrant
random.incident.coef <- sim.b.incident
random.incident.se <- sim.se.incident
random.afterlaw.coef <- sim.b.afterlaw
random.afterlaw.se <- sim.se.afterlaw
random.unlawful.coef <- sim.b.unlawful
random.unlawful.se <- sim.se.unlawful 
random.except.coef <-  sim.b.except
random.except.se <- sim.se.except
random.change.coef <- sim.b.change
random.change.se <- sim.se.change
random.probcaus.coef <- sim.b.probcaus
random.probcaus.se <- sim.se.probcaus
random.home.impact <- sim.home.avgimpact
random.except.impact <- sim.except.avgimpact

#get results for percent conservative decisions in-sample vs. all cases

random.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
random.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
random.consbias <- random.construe.in - random.construe.all #define conservative bias: actual # of 1's - true number of 1's


#3)close cases, all

close.all <- read.dta("sim_results_close_all.dta")
#close.all <- close.all[close.all$sim.n.in == 300,]
detach()
attach.all(close.all)
close.all.n.in <- sim.n.in #number of total cases in each logit
close.all.random <- sim.randomcases #number of random cases in each logit
close.all.chosen <- sim.chosencases #number of chosen cases in each logit

close.all.n.out<- sim.n.out
close.all.flips.out <- sim.caseflips.out
close.all.falsepos.out <-  sim.falsepos.out
close.all.falseneg.out <-  sim.falseneg.out


    #individual coefficients
    
close.all.home.coef <-  sim.b.home
close.all.home.se <- sim.se.home
close.all.business.coef <- sim.b.business
close.all.business.se <- sim.se.business
close.all.person.coef <- sim.b.person
close.all.person.se <- sim.se.person
close.all.car.coef <- sim.b.car
close.all.car.se <- sim.se.car
close.all.extent.coef <- sim.b.extent
close.all.extent.se <- sim.se.extent 
close.all.warrant.coef <-  sim.b.warrant
close.all.warrant.se <- sim.se.warrant
close.all.incident.coef <- sim.b.incident
close.all.incident.se <- sim.se.incident
close.all.afterlaw.coef <- sim.b.afterlaw
close.all.afterlaw.se <- sim.se.afterlaw
close.all.unlawful.coef <- sim.b.unlawful
close.all.unlawful.se <- sim.se.unlawful 
close.all.except.coef <-  sim.b.except
close.all.except.se <- sim.se.except
close.all.change.coef <- sim.b.change
close.all.change.se <- sim.se.change
close.all.probcaus.coef <- sim.b.probcaus
close.all.probcaus.se <- sim.se.probcaus
close.all.home.impact <- sim.home.avgimpact
close.all.except.impact <- sim.except.avgimpact

#get results for percent conservative decisions in-sample vs. all cases

close.all.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
close.all.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
close.all.consbias <- close.all.construe.in - close.all.construe.all #define conservative bias: actual # of 1's - true number of 1's


#4)close liberal cases

close.lib <- read.dta("sim_results_close_liberal.dta")
#close.lib <- close.lib[close.lib$sim.n.in == 300,]
detach()
attach.all(close.lib)
close.lib.n.in <- sim.n.in #number of total cases in each logit
close.lib.random <- sim.randomcases #number of random cases in each logit
close.lib.chosen <- sim.chosencases #number of chosen cases in each logit

close.lib.n.out<- sim.n.out
close.lib.flips.out <- sim.caseflips.out
close.lib.falsepos.out <-  sim.falsepos.out
close.lib.falseneg.out <-  sim.falseneg.out

    #individual coefficients
close.lib.home.coef <-  sim.b.home
close.lib.home.se <- sim.se.home
close.lib.business.coef <- sim.b.business
close.lib.business.se <- sim.se.business
close.lib.person.coef <- sim.b.person
close.lib.person.se <- sim.se.person
close.lib.car.coef <- sim.b.car
close.lib.car.se <- sim.se.car
close.lib.extent.coef <- sim.b.extent
close.lib.extent.se <- sim.se.extent 
close.lib.warrant.coef <-  sim.b.warrant
close.lib.warrant.se <- sim.se.warrant
close.lib.incident.coef <- sim.b.incident
close.lib.incident.se <- sim.se.incident
close.lib.afterlaw.coef <- sim.b.afterlaw
close.lib.afterlaw.se <- sim.se.afterlaw
close.lib.unlawful.coef <- sim.b.unlawful
close.lib.unlawful.se <- sim.se.unlawful 
close.lib.except.coef <-  sim.b.except
close.lib.except.se <- sim.se.except
close.lib.change.coef <- sim.b.change
close.lib.change.se <- sim.se.change
close.lib.probcaus.coef <- sim.b.probcaus
close.lib.probcaus.se <- sim.se.probcaus
close.lib.home.impact <- sim.home.avgimpact
close.lib.except.impact <- sim.except.avgimpact

#get results for percent conservative decisions in-sample vs. all cases

close.lib.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
close.lib.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
close.lib.consbias <- close.lib.construe.in - close.lib.construe.all #define conservative bias: actual # of 1's - true number of 1's


#5)close conservative cases

close.con <- read.dta("sim_results_close_conservative.dta")
#close.con <- close.con[close.con$sim.n.in == 300,]
detach()
attach.all(close.con)
close.con.n.in <- sim.n.in #number of total cases in each logit
close.con.random <- sim.randomcases #number of random cases in each logit
close.con.chosen <- sim.chosencases #number of chosen cases in each logit

close.con.n.out<- sim.n.out
close.con.flips.out <- sim.caseflips.out
close.con.falsepos.out <-  sim.falsepos.out
close.con.falseneg.out <-  sim.falseneg.out

    #individual coefficients
close.con.home.coef <-  sim.b.home
close.con.home.se <- sim.se.home
close.con.business.coef <- sim.b.business
close.con.business.se <- sim.se.business
close.con.person.coef <- sim.b.person
close.con.person.se <- sim.se.person
close.con.car.coef <- sim.b.car
close.con.car.se <- sim.se.car
close.con.extent.coef <- sim.b.extent
close.con.extent.se <- sim.se.extent 
close.con.warrant.coef <-  sim.b.warrant
close.con.warrant.se <- sim.se.warrant
close.con.incident.coef <- sim.b.incident
close.con.incident.se <- sim.se.incident
close.con.afterlaw.coef <- sim.b.afterlaw
close.con.afterlaw.se <- sim.se.afterlaw
close.con.unlawful.coef <- sim.b.unlawful
close.con.unlawful.se <- sim.se.unlawful 
close.con.except.coef <-  sim.b.except
close.con.except.se <- sim.se.except
close.con.change.coef <- sim.b.change
close.con.change.se <- sim.se.change
close.con.probcaus.coef <- sim.b.probcaus
close.con.probcaus.se <- sim.se.probcaus
close.con.home.impact <- sim.home.avgimpact
close.con.except.impact <- sim.except.avgimpact

#get results for percent conservative decisions in-sample vs. all cases

close.con.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
close.con.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
close.con.consbias <- close.con.construe.in - close.con.construe.all #define conservative bias: actual # of 1's - true number of 1's


#6)anomalous, all

anomalous.all <- read.dta("sim_results_anomalous_all.dta")
#anomalous.all <- anomalous.all[anomalous.all$sim.n.in == 300,]
detach()
attach.all(anomalous.all)
anomalous.all.n.in <- sim.n.in #number of total cases in each logit
anomalous.all.random <- sim.randomcases #number of random cases in each logit
anomalous.all.chosen <- sim.chosencases #number of chosen cases in each logit

anomalous.all.n.out<- sim.n.out
anomalous.all.flips.out <- sim.caseflips.out
anomalous.all.falsepos.out <-  sim.falsepos.out
anomalous.all.falseneg.out <-  sim.falseneg.out

    #individual coefficients
anomalous.all.home.coef <-  sim.b.home
anomalous.all.home.se <- sim.se.home
anomalous.all.business.coef <- sim.b.business
anomalous.all.business.se <- sim.se.business
anomalous.all.person.coef <- sim.b.person
anomalous.all.person.se <- sim.se.person
anomalous.all.car.coef <- sim.b.car
anomalous.all.car.se <- sim.se.car
anomalous.all.extent.coef <- sim.b.extent
anomalous.all.extent.se <- sim.se.extent 
anomalous.all.warrant.coef <-  sim.b.warrant
anomalous.all.warrant.se <- sim.se.warrant
anomalous.all.incident.coef <- sim.b.incident
anomalous.all.incident.se <- sim.se.incident
anomalous.all.afterlaw.coef <- sim.b.afterlaw
anomalous.all.afterlaw.se <- sim.se.afterlaw
anomalous.all.unlawful.coef <- sim.b.unlawful
anomalous.all.unlawful.se <- sim.se.unlawful 
anomalous.all.except.coef <-  sim.b.except
anomalous.all.except.se <- sim.se.except
anomalous.all.change.coef <- sim.b.change
anomalous.all.change.se <- sim.se.change
anomalous.all.probcaus.coef <- sim.b.probcaus
anomalous.all.probcaus.se <- sim.se.probcaus
anomalous.all.home.impact <- sim.home.avgimpact
anomalous.all.except.impact <- sim.except.avgimpact

#get results for percent conservative decisions in-sample vs. all cases

anomalous.all.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
anomalous.all.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
anomalous.all.consbias <- anomalous.all.construe.in - anomalous.all.construe.all #define conservative bias: actual # of 1's - true number of 1's


#7)anomalous conservative

anomalous.con <- read.dta("sim_results_anomalous_conservative.dta")
#anomalous.con <- anomalous.con[anomalous.con$sim.n.in == 300,]
detach()
attach.all(anomalous.con)
anomalous.con.n.in <- sim.n.in #number of total cases in each logit
anomalous.con.random <- sim.randomcases #number of random cases in each logit
anomalous.con.chosen <- sim.chosencases #number of chosen cases in each logit

anomalous.con.n.out<- sim.n.out
anomalous.con.flips.out <- sim.caseflips.out
anomalous.con.falsepos.out <-  sim.falsepos.out
anomalous.con.falseneg.out <-  sim.falseneg.out

    #individual coefficients
anomalous.con.home.coef <-  sim.b.home
anomalous.con.home.se <- sim.se.home
anomalous.con.business.coef <- sim.b.business
anomalous.con.business.se <- sim.se.business
anomalous.con.person.coef <- sim.b.person
anomalous.con.person.se <- sim.se.person
anomalous.con.car.coef <- sim.b.car
anomalous.con.car.se <- sim.se.car
anomalous.con.extent.coef <- sim.b.extent
anomalous.con.extent.se <- sim.se.extent 
anomalous.con.warrant.coef <-  sim.b.warrant
anomalous.con.warrant.se <- sim.se.warrant
anomalous.con.incident.coef <- sim.b.incident
anomalous.con.incident.se <- sim.se.incident
anomalous.con.afterlaw.coef <- sim.b.afterlaw
anomalous.con.afterlaw.se <- sim.se.afterlaw
anomalous.con.unlawful.coef <- sim.b.unlawful
anomalous.con.unlawful.se <- sim.se.unlawful 
anomalous.con.except.coef <-  sim.b.except
anomalous.con.except.se <- sim.se.except
anomalous.con.change.coef <- sim.b.change
anomalous.con.change.se <- sim.se.change
anomalous.con.probcaus.coef <- sim.b.probcaus
anomalous.con.probcaus.se <- sim.se.probcaus
anomalous.con.home.impact <- sim.home.avgimpact
anomalous.con.except.impact <- sim.except.avgimpact

#get results for percent conservative decisions in-sample vs. all cases

anomalous.con.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
anomalous.con.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
anomalous.con.consbias <- anomalous.con.construe.in - anomalous.con.construe.all #define conservative bias: actual # of 1's - true number of 1's


#8)anomalous liberal

anomalous.lib <- read.dta("sim_results_anomalous_liberal.dta")
#anomalous.lib <- anomalous.lib[anomalous.lib$sim.n.in == 300,]
detach()
attach.all(anomalous.lib)
anomalous.lib.n.in <- sim.n.in #number of total cases in each logit
anomalous.lib.random <- sim.randomcases #number of random cases in each logit
anomalous.lib.chosen <- sim.chosencases #number of chosen cases in each logit

anomalous.lib.n.out<- sim.n.out
anomalous.lib.flips.out <- sim.caseflips.out
anomalous.lib.falsepos.out <-  sim.falsepos.out
anomalous.lib.falseneg.out <-  sim.falseneg.out

    #individual coefficients
anomalous.lib.home.coef <-  sim.b.home
anomalous.lib.home.se <- sim.se.home
anomalous.lib.business.coef <- sim.b.business
anomalous.lib.business.se <- sim.se.business
anomalous.lib.person.coef <- sim.b.person
anomalous.lib.person.se <- sim.se.person
anomalous.lib.car.coef <- sim.b.car
anomalous.lib.car.se <- sim.se.car
anomalous.lib.extent.coef <- sim.b.extent
anomalous.lib.extent.se <- sim.se.extent 
anomalous.lib.warrant.coef <-  sim.b.warrant
anomalous.lib.warrant.se <- sim.se.warrant
anomalous.lib.incident.coef <- sim.b.incident
anomalous.lib.incident.se <- sim.se.incident
anomalous.lib.afterlaw.coef <- sim.b.afterlaw
anomalous.lib.afterlaw.se <- sim.se.afterlaw
anomalous.lib.unlawful.coef <- sim.b.unlawful
anomalous.lib.unlawful.se <- sim.se.unlawful 
anomalous.lib.except.coef <-  sim.b.except
anomalous.lib.except.se <- sim.se.except
anomalous.lib.change.coef <- sim.b.change
anomalous.lib.change.se <- sim.se.change
anomalous.lib.probcaus.coef <- sim.b.probcaus
anomalous.lib.probcaus.se <- sim.se.probcaus
anomalous.lib.home.impact <- sim.home.avgimpact
anomalous.lib.except.impact <- sim.except.avgimpact

#get results for percent conservative decisions in-sample vs. all cases

anomalous.lib.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
anomalous.lib.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
anomalous.lib.consbias <- anomalous.lib.construe.in - anomalous.lib.construe.all #define conservative bias: actual # of 1's - true number of 1's


#9) conservative, no exceptions, home
    #use "except" for short

except.data <- read.dta("sim_results_home_no_except.dta")
#except.data <- except.data[except.data$sim.n.in == 300,]
detach()
attach.all(except.data)
except.n.in <- sim.n.in #number of total cases in each logit
except.random <- sim.randomcases #number of random cases in each logit
except.chosen <- sim.chosencases #number of chosen cases in each logit

except.n.out<- sim.n.out
except.flips.out <- sim.caseflips.out
except.falsepos.out <-  sim.falsepos.out
except.falseneg.out <-  sim.falseneg.out

    #individual coefficients
except.home.coef <-  sim.b.home
except.home.se <- sim.se.home
except.business.coef <- sim.b.business
except.business.se <- sim.se.business
except.person.coef <- sim.b.person
except.person.se <- sim.se.person
except.car.coef <- sim.b.car
except.car.se <- sim.se.car
except.extent.coef <- sim.b.extent
except.extent.se <- sim.se.extent 
except.warrant.coef <-  sim.b.warrant
except.warrant.se <- sim.se.warrant
except.incident.coef <- sim.b.incident
except.incident.se <- sim.se.incident
except.afterlaw.coef <- sim.b.afterlaw
except.afterlaw.se <- sim.se.afterlaw
except.unlawful.coef <- sim.b.unlawful
except.unlawful.se <- sim.se.unlawful 
except.except.coef <-  sim.b.except
except.except.se <- sim.se.except
except.change.coef <- sim.b.change
except.change.se <- sim.se.change
except.probcaus.coef <- sim.b.probcaus
except.probcaus.se <- sim.se.probcaus
except.home.impact <- sim.home.avgimpact
except.except.impact <- sim.except.avgimpact


#get results for percent conservative decisions in-sample vs. out-of-sample
#get results for percent conservative decisions in-sample vs. all cases

except.construe.in <- sim.truenumones.in #percent of conservative decisions for in sample 
except.construe.all <- sim.truenumones ##percent of conservative decisions for all cases
except.consbias <- except.construe.in - except.construe.all #define conservative bias: actual # of 1's - true number of 1's


###############################################################

###############################################################

###############################################################

    #           SIGNIFICANCE
#For each sel strat,  % of sims in which the coeff is signif in the
#predicted direction. 

#get counts of "significant" differences
#be sure to differentiate btw. positive and negative true coefficients
#positive: warrant, incident, afterlaw, unlawful, except, change

#negative: home, business, person, car, extent, probcaus

#NOTE: IN FUTURE: FIX UNLAWFUL SO THAT SIGNIFICANCE CAN GO
    #IN EITHER DIRECTION (BUT UNLAWFUL IS NOT GRAPHED ...)

crit.value <- qnorm(.975)

#1) all cases

all.home <- ifelse((all.home.coef < -crit.value*all.home.se), 1,0)
all.sig.home <- 100*(sum(all.home, na.rm=TRUE)/length(all.home[!is.na(all.home)]))
all.business <- ifelse((all.business.coef < -crit.value*all.business.se), 1,0)
all.sig.business <- 100*(sum(all.business, na.rm=TRUE)/length(all.business[!is.na(all.business)]))
all.person <- ifelse((all.person.coef < -crit.value*all.person.se), 1,0)
all.sig.person <- 100*(sum(all.person, na.rm=TRUE)/length(all.person[!is.na(all.person)]))
all.car <- ifelse((all.car.coef < -crit.value*all.car.se), 1,0)
all.sig.car <- 100*(sum(all.car, na.rm=TRUE)/length(all.car[!is.na(all.car)]))
all.extent <- ifelse((all.extent.coef < -crit.value*all.extent.se), 1,0)
all.sig.extent <- 100*(sum(all.extent, na.rm=TRUE)/length(all.extent[!is.na(all.extent)]))  
all.warrant <- ifelse((all.warrant.coef > crit.value*all.warrant.se), 1,0)
all.sig.warrant <- 100*(sum(all.warrant, na.rm=TRUE)/length(all.warrant[!is.na(all.warrant)]))
all.incident <- ifelse((all.incident.coef > crit.value*all.incident.se), 1,0)
all.sig.incident <- 100*(sum(all.incident, na.rm=TRUE)/length(all.incident[!is.na(all.incident)]))
all.afterlaw <- ifelse((all.afterlaw.coef > crit.value*all.afterlaw.se), 1,0)
all.sig.afterlaw <- 100*(sum(all.afterlaw, na.rm=TRUE)/length(all.afterlaw[!is.na(all.afterlaw)]))
all.unlawful <- ifelse((all.unlawful.coef > crit.value*all.unlawful.se), 1,0)
all.sig.unlawful <- 100*(sum(all.unlawful, na.rm=TRUE)/length(all.unlawful[!is.na(all.unlawful)]))
all.except <- ifelse((all.except.coef > crit.value*all.except.se), 1,0)
all.sig.except <- 100*(sum(all.except,na.rm=TRUE)/length(all.except[!is.na(all.except)]))
all.probcaus <- ifelse(((all.probcaus.coef < -crit.value*all.probcaus.se) | (all.probcaus.coef > crit.value*all.probcaus.se)), 1,0)
all.sig.probcaus <- 100*(sum(all.probcaus, na.rm=TRUE)/length(all.probcaus[!is.na(all.probcaus)]))
all.change <- ifelse((all.change.coef > crit.value*all.change.se), 1,0)
all.sig.change <- 100*(sum(all.change, na.rm=TRUE)/length(all.change[!is.na(all.change)]))
             
 
#2) random

random.home <- ifelse((random.home.coef < -crit.value*random.home.se), 1,0)
random.sig.home <- 100*(sum(random.home, na.rm=TRUE)/length(random.home[!is.na(random.home)]))
random.business <- ifelse((random.business.coef < -crit.value*random.business.se), 1,0)
random.sig.business <- 100*(sum(random.business, na.rm=TRUE)/length(random.business[!is.na(random.business)]))
random.person <- ifelse((random.person.coef < -crit.value*random.person.se), 1,0)
random.sig.person <- 100*(sum(random.person, na.rm=TRUE)/length(random.person[!is.na(random.person)]))
random.car <- ifelse((random.car.coef < -crit.value*random.car.se), 1,0)
random.sig.car <- 100*(sum(random.car, na.rm=TRUE)/length(random.car[!is.na(random.car)]))
random.extent <- ifelse((random.extent.coef < -crit.value*random.extent.se), 1,0)
random.sig.extent <- 100*(sum(random.extent, na.rm=TRUE)/length(random.extent[!is.na(random.extent)]))  
random.warrant <- ifelse((random.warrant.coef > crit.value*random.warrant.se), 1,0)
random.sig.warrant <- 100*(sum(random.warrant, na.rm=TRUE)/length(random.warrant[!is.na(random.warrant)]))
random.incident <- ifelse((random.incident.coef > crit.value*random.incident.se), 1,0)
random.sig.incident <- 100*(sum(random.incident, na.rm=TRUE)/length(random.incident[!is.na(random.incident)]))
random.afterlaw <- ifelse((random.afterlaw.coef > crit.value*random.afterlaw.se), 1,0)
random.sig.afterlaw <- 100*(sum(random.afterlaw, na.rm=TRUE)/length(random.afterlaw[!is.na(random.afterlaw)]))
random.unlawful <- ifelse((random.unlawful.coef > crit.value*random.unlawful.se), 1,0)
random.sig.unlawful <- 100*(sum(random.unlawful, na.rm=TRUE)/length(random.unlawful[!is.na(random.unlawful)]))
random.except <- ifelse((random.except.coef > crit.value*random.except.se), 1,0)
random.sig.except <- 100*(sum(random.except,na.rm=TRUE)/length(random.except[!is.na(random.except)]))
random.probcaus <- ifelse(((random.probcaus.coef < -crit.value*random.probcaus.se) | (random.probcaus.coef > crit.value*random.probcaus.se)), 1,0)
random.sig.probcaus <- 100*(sum(random.probcaus, na.rm=TRUE)/length(random.probcaus[!is.na(random.probcaus)]))
random.change <- ifelse((random.change.coef > crit.value*random.change.se), 1,0)
random.sig.change <- 100*(sum(random.change, na.rm=TRUE)/length(random.change[!is.na(random.change)]))
         
     
#3) Close cases (all)

close.all.home <- ifelse((close.all.home.coef < -crit.value*close.all.home.se), 1,0)
close.all.sig.home <- 100*(sum(close.all.home, na.rm=TRUE)/length(close.all.home[!is.na(close.all.home)]))
close.all.business <- ifelse((close.all.business.coef < -crit.value*close.all.business.se), 1,0)
close.all.sig.business <- 100*(sum(close.all.business, na.rm=TRUE)/length(close.all.business[!is.na(close.all.business)]))
close.all.person <- ifelse((close.all.person.coef < -crit.value*close.all.person.se), 1,0)
close.all.sig.person <- 100*(sum(close.all.person, na.rm=TRUE)/length(close.all.person[!is.na(close.all.person)]))
close.all.car <- ifelse((close.all.car.coef < -crit.value*close.all.car.se), 1,0)
close.all.sig.car <- 100*(sum(close.all.car, na.rm=TRUE)/length(close.all.car[!is.na(close.all.car)]))
close.all.extent <- ifelse((close.all.extent.coef < -crit.value*close.all.extent.se), 1,0)
close.all.sig.extent <- 100*(sum(close.all.extent, na.rm=TRUE)/length(close.all.extent[!is.na(close.all.extent)]))  
close.all.warrant <- ifelse((close.all.warrant.coef > crit.value*close.all.warrant.se), 1,0)
close.all.sig.warrant <- 100*(sum(close.all.warrant, na.rm=TRUE)/length(close.all.warrant[!is.na(close.all.warrant)]))
close.all.incident <- ifelse((close.all.incident.coef > crit.value*close.all.incident.se), 1,0)
close.all.sig.incident <- 100*(sum(close.all.incident, na.rm=TRUE)/length(close.all.incident[!is.na(close.all.incident)]))
close.all.afterlaw <- ifelse((close.all.afterlaw.coef > crit.value*close.all.afterlaw.se), 1,0)
close.all.sig.afterlaw <- 100*(sum(close.all.afterlaw, na.rm=TRUE)/length(close.all.afterlaw[!is.na(close.all.afterlaw)]))
close.all.unlawful <- ifelse((close.all.unlawful.coef > crit.value*close.all.unlawful.se), 1,0)
close.all.sig.unlawful <- 100*(sum(close.all.unlawful, na.rm=TRUE)/length(close.all.unlawful[!is.na(close.all.unlawful)]))
close.all.except <- ifelse((close.all.except.coef > crit.value*close.all.except.se), 1,0)
close.all.sig.except <- 100*(sum(close.all.except,na.rm=TRUE)/length(close.all.except[!is.na(close.all.except)]))
close.all.probcaus <- ifelse(((close.all.probcaus.coef < -crit.value*close.all.probcaus.se) | (close.all.probcaus.coef > crit.value*close.all.probcaus.se)), 1,0)
close.all.sig.probcaus <- 100*(sum(close.all.probcaus, na.rm=TRUE)/length(close.all.probcaus[!is.na(close.all.probcaus)]))
close.all.change <- ifelse((close.all.change.coef > crit.value*close.all.change.se), 1,0)
close.all.sig.change <- 100*(sum(close.all.change, na.rm=TRUE)/length(close.all.change[!is.na(close.all.change)]))
                            
              
#4) Close Liberal Cases

close.lib.home <- ifelse((close.lib.home.coef < -crit.value*close.lib.home.se), 1,0)
close.lib.sig.home <- 100*(sum(close.lib.home, na.rm=TRUE)/length(close.lib.home[!is.na(close.lib.home)]))
close.lib.business <- ifelse((close.lib.business.coef < -crit.value*close.lib.business.se), 1,0)
close.lib.sig.business <- 100*(sum(close.lib.business, na.rm=TRUE)/length(close.lib.business[!is.na(close.lib.business)]))
close.lib.person <- ifelse((close.lib.person.coef < -crit.value*close.lib.person.se), 1,0)
close.lib.sig.person <- 100*(sum(close.lib.person, na.rm=TRUE)/length(close.lib.person[!is.na(close.lib.person)]))
close.lib.car <- ifelse((close.lib.car.coef < -crit.value*close.lib.car.se), 1,0)
close.lib.sig.car <- 100*(sum(close.lib.car, na.rm=TRUE)/length(close.lib.car[!is.na(close.lib.car)]))
close.lib.extent <- ifelse((close.lib.extent.coef < -crit.value*close.lib.extent.se), 1,0)
close.lib.sig.extent <- 100*(sum(close.lib.extent, na.rm=TRUE)/length(close.lib.extent[!is.na(close.lib.extent)]))  
close.lib.warrant <- ifelse((close.lib.warrant.coef > crit.value*close.lib.warrant.se), 1,0)
close.lib.sig.warrant <- 100*(sum(close.lib.warrant, na.rm=TRUE)/length(close.lib.warrant[!is.na(close.lib.warrant)]))
close.lib.incident <- ifelse((close.lib.incident.coef > crit.value*close.lib.incident.se), 1,0)
close.lib.sig.incident <- 100*(sum(close.lib.incident, na.rm=TRUE)/length(close.lib.incident[!is.na(close.lib.incident)]))
close.lib.afterlaw <- ifelse((close.lib.afterlaw.coef > crit.value*close.lib.afterlaw.se), 1,0)
close.lib.sig.afterlaw <- 100*(sum(close.lib.afterlaw, na.rm=TRUE)/length(close.lib.afterlaw[!is.na(close.lib.afterlaw)]))
close.lib.unlawful <- ifelse((close.lib.unlawful.coef > crit.value*close.lib.unlawful.se), 1,0)
close.lib.sig.unlawful <- 100*(sum(close.lib.unlawful, na.rm=TRUE)/length(close.lib.unlawful[!is.na(close.lib.unlawful)]))
close.lib.except <- ifelse((close.lib.except.coef > crit.value*close.lib.except.se), 1,0)
close.lib.sig.except <- 100*(sum(close.lib.except,na.rm=TRUE)/length(close.lib.except[!is.na(close.lib.except)]))
close.lib.probcaus <- ifelse(((close.lib.probcaus.coef < -crit.value*close.lib.probcaus.se) | (close.lib.probcaus.coef > crit.value*close.lib.probcaus.se)), 1,0)
close.lib.sig.probcaus <- 100*(sum(close.lib.probcaus, na.rm=TRUE)/length(close.lib.probcaus[!is.na(close.lib.probcaus)]))
close.lib.change <- ifelse((close.lib.change.coef > crit.value*close.lib.change.se), 1,0)
close.lib.sig.change <- 100*(sum(close.lib.change, na.rm=TRUE)/length(close.lib.change[!is.na(close.lib.change)]))
             
         
#5) near, wantone

close.con.home <- ifelse((close.con.home.coef < -crit.value*close.con.home.se), 1,0)
close.con.sig.home <- 100*(sum(close.con.home, na.rm=TRUE)/length(close.con.home[!is.na(close.con.home)]))
close.con.business <- ifelse((close.con.business.coef < -crit.value*close.con.business.se), 1,0)
close.con.sig.business <- 100*(sum(close.con.business, na.rm=TRUE)/length(close.con.business[!is.na(close.con.business)]))
close.con.person <- ifelse((close.con.person.coef < -crit.value*close.con.person.se), 1,0)
close.con.sig.person <- 100*(sum(close.con.person, na.rm=TRUE)/length(close.con.person[!is.na(close.con.person)]))
close.con.car <- ifelse((close.con.car.coef < -crit.value*close.con.car.se), 1,0)
close.con.sig.car <- 100*(sum(close.con.car, na.rm=TRUE)/length(close.con.car[!is.na(close.con.car)]))
close.con.extent <- ifelse((close.con.extent.coef < -crit.value*close.con.extent.se), 1,0)
close.con.sig.extent <- 100*(sum(close.con.extent, na.rm=TRUE)/length(close.con.extent[!is.na(close.con.extent)]))  
close.con.warrant <- ifelse((close.con.warrant.coef > crit.value*close.con.warrant.se), 1,0)
close.con.sig.warrant <- 100*(sum(close.con.warrant, na.rm=TRUE)/length(close.con.warrant[!is.na(close.con.warrant)]))
close.con.incident <- ifelse((close.con.incident.coef > crit.value*close.con.incident.se), 1,0)
close.con.sig.incident <- 100*(sum(close.con.incident, na.rm=TRUE)/length(close.con.incident[!is.na(close.con.incident)]))
close.con.afterlaw <- ifelse((close.con.afterlaw.coef > crit.value*close.con.afterlaw.se), 1,0)
close.con.sig.afterlaw <- 100*(sum(close.con.afterlaw, na.rm=TRUE)/length(close.con.afterlaw[!is.na(close.con.afterlaw)]))
close.con.unlawful <- ifelse((close.con.unlawful.coef > crit.value*close.con.unlawful.se), 1,0)
close.con.sig.unlawful <- 100*(sum(close.con.unlawful, na.rm=TRUE)/length(close.con.unlawful[!is.na(close.con.unlawful)]))
close.con.except <- ifelse((close.con.except.coef > crit.value*close.con.except.se), 1,0)
close.con.sig.except <- 100*(sum(close.con.except,na.rm=TRUE)/length(close.con.except[!is.na(close.con.except)]))
close.con.probcaus <- ifelse(((close.con.probcaus.coef < -crit.value*close.con.probcaus.se) | (close.con.probcaus.coef > crit.value*close.con.probcaus.se)), 1,0)
close.con.sig.probcaus <- 100*(sum(close.con.probcaus, na.rm=TRUE)/length(close.con.probcaus[!is.na(close.con.probcaus)]))
close.con.change <- ifelse((close.con.change.coef > crit.value*close.con.change.se), 1,0)
close.con.sig.change <- 100*(sum(close.con.change, na.rm=TRUE)/length(close.con.change[!is.na(close.con.change)]))
             

#6) Anomalous cases, all

anomalous.all.home <- ifelse((anomalous.all.home.coef < -crit.value*anomalous.all.home.se), 1,0)
anomalous.all.sig.home <- 100*(sum(anomalous.all.home, na.rm=TRUE)/length(anomalous.all.home[!is.na(anomalous.all.home)]))
anomalous.all.business <- ifelse((anomalous.all.business.coef < -crit.value*anomalous.all.business.se), 1,0)
anomalous.all.sig.business <- 100*(sum(anomalous.all.business, na.rm=TRUE)/length(anomalous.all.business[!is.na(anomalous.all.business)]))
anomalous.all.person <- ifelse((anomalous.all.person.coef < -crit.value*anomalous.all.person.se), 1,0)
anomalous.all.sig.person <- 100*(sum(anomalous.all.person, na.rm=TRUE)/length(anomalous.all.person[!is.na(anomalous.all.person)]))
anomalous.all.car <- ifelse((anomalous.all.car.coef < -crit.value*anomalous.all.car.se), 1,0)
anomalous.all.sig.car <- 100*(sum(anomalous.all.car, na.rm=TRUE)/length(anomalous.all.car[!is.na(anomalous.all.car)]))
anomalous.all.extent <- ifelse((anomalous.all.extent.coef < -crit.value*anomalous.all.extent.se), 1,0)
anomalous.all.sig.extent <- 100*(sum(anomalous.all.extent, na.rm=TRUE)/length(anomalous.all.extent[!is.na(anomalous.all.extent)]))  
anomalous.all.warrant <- ifelse((anomalous.all.warrant.coef > crit.value*anomalous.all.warrant.se), 1,0)
anomalous.all.sig.warrant <- 100*(sum(anomalous.all.warrant, na.rm=TRUE)/length(anomalous.all.warrant[!is.na(anomalous.all.warrant)]))
anomalous.all.incident <- ifelse((anomalous.all.incident.coef > crit.value*anomalous.all.incident.se), 1,0)
anomalous.all.sig.incident <- 100*(sum(anomalous.all.incident, na.rm=TRUE)/length(anomalous.all.incident[!is.na(anomalous.all.incident)]))
anomalous.all.afterlaw <- ifelse((anomalous.all.afterlaw.coef > crit.value*anomalous.all.afterlaw.se), 1,0)
anomalous.all.sig.afterlaw <- 100*(sum(anomalous.all.afterlaw, na.rm=TRUE)/length(anomalous.all.afterlaw[!is.na(anomalous.all.afterlaw)]))
anomalous.all.unlawful <- ifelse((anomalous.all.unlawful.coef > crit.value*anomalous.all.unlawful.se), 1,0)
anomalous.all.sig.unlawful <- 100*(sum(anomalous.all.unlawful, na.rm=TRUE)/length(anomalous.all.unlawful[!is.na(anomalous.all.unlawful)]))
anomalous.all.except <- ifelse((anomalous.all.except.coef > crit.value*anomalous.all.except.se), 1,0)
anomalous.all.sig.except <- 100*(sum(anomalous.all.except,na.rm=TRUE)/length(anomalous.all.except[!is.na(anomalous.all.except)]))
anomalous.all.probcaus <- ifelse(((anomalous.all.probcaus.coef < -crit.value*anomalous.all.probcaus.se) | (anomalous.all.probcaus.coef > crit.value*anomalous.all.probcaus.se)), 1,0)
anomalous.all.sig.probcaus <- 100*(sum(anomalous.all.probcaus, na.rm=TRUE)/length(anomalous.all.probcaus[!is.na(anomalous.all.probcaus)]))
anomalous.all.change <- ifelse((anomalous.all.change.coef > crit.value*anomalous.all.change.se), 1,0)
anomalous.all.sig.change <- 100*(sum(anomalous.all.change, na.rm=TRUE)/length(anomalous.all.change[!is.na(anomalous.all.change)]))
           

#7) Anomalous, Liberal Cases

anomalous.lib.home <- ifelse((anomalous.lib.home.coef < -crit.value*anomalous.lib.home.se), 1,0)
anomalous.lib.sig.home <- 100*(sum(anomalous.lib.home, na.rm=TRUE)/length(anomalous.lib.home[!is.na(anomalous.lib.home)]))
anomalous.lib.business <- ifelse((anomalous.lib.business.coef < -crit.value*anomalous.lib.business.se), 1,0)
anomalous.lib.sig.business <- 100*(sum(anomalous.lib.business, na.rm=TRUE)/length(anomalous.lib.business[!is.na(anomalous.lib.business)]))
anomalous.lib.person <- ifelse((anomalous.lib.person.coef < -crit.value*anomalous.lib.person.se), 1,0)
anomalous.lib.sig.person <- 100*(sum(anomalous.lib.person, na.rm=TRUE)/length(anomalous.lib.person[!is.na(anomalous.lib.person)]))
anomalous.lib.car <- ifelse((anomalous.lib.car.coef < -crit.value*anomalous.lib.car.se), 1,0)
anomalous.lib.sig.car <- 100*(sum(anomalous.lib.car, na.rm=TRUE)/length(anomalous.lib.car[!is.na(anomalous.lib.car)]))
anomalous.lib.extent <- ifelse((anomalous.lib.extent.coef < -crit.value*anomalous.lib.extent.se), 1,0)
anomalous.lib.sig.extent <- 100*(sum(anomalous.lib.extent, na.rm=TRUE)/length(anomalous.lib.extent[!is.na(anomalous.lib.extent)]))  
anomalous.lib.warrant <- ifelse((anomalous.lib.warrant.coef > crit.value*anomalous.lib.warrant.se), 1,0)
anomalous.lib.sig.warrant <- 100*(sum(anomalous.lib.warrant, na.rm=TRUE)/length(anomalous.lib.warrant[!is.na(anomalous.lib.warrant)]))
anomalous.lib.incident <- ifelse((anomalous.lib.incident.coef > crit.value*anomalous.lib.incident.se), 1,0)
anomalous.lib.sig.incident <- 100*(sum(anomalous.lib.incident, na.rm=TRUE)/length(anomalous.lib.incident[!is.na(anomalous.lib.incident)]))
anomalous.lib.afterlaw <- ifelse((anomalous.lib.afterlaw.coef > crit.value*anomalous.lib.afterlaw.se), 1,0)
anomalous.lib.sig.afterlaw <- 100*(sum(anomalous.lib.afterlaw, na.rm=TRUE)/length(anomalous.lib.afterlaw[!is.na(anomalous.lib.afterlaw)]))
anomalous.lib.unlawful <- ifelse((anomalous.lib.unlawful.coef > crit.value*anomalous.lib.unlawful.se), 1,0)
anomalous.lib.sig.unlawful <- 100*(sum(anomalous.lib.unlawful, na.rm=TRUE)/length(anomalous.lib.unlawful[!is.na(anomalous.lib.unlawful)]))
anomalous.lib.except <- ifelse((anomalous.lib.except.coef > crit.value*anomalous.lib.except.se), 1,0)
anomalous.lib.sig.except <- 100*(sum(anomalous.lib.except,na.rm=TRUE)/length(anomalous.lib.except[!is.na(anomalous.lib.except)]))
anomalous.lib.probcaus <- ifelse(((anomalous.lib.probcaus.coef < -crit.value*anomalous.lib.probcaus.se) | (anomalous.lib.probcaus.coef > crit.value*anomalous.lib.probcaus.se)), 1,0)
anomalous.lib.sig.probcaus <- 100*(sum(anomalous.lib.probcaus, na.rm=TRUE)/length(anomalous.lib.probcaus[!is.na(anomalous.lib.probcaus)]))
anomalous.lib.change <- ifelse((anomalous.lib.change.coef > crit.value*anomalous.lib.change.se), 1,0)
anomalous.lib.sig.change <- 100*(sum(anomalous.lib.change, na.rm=TRUE)/length(anomalous.lib.change[!is.na(anomalous.lib.change)]))
            

#8) Anomalous, Conservative Cases

anomalous.con.home <- ifelse((anomalous.con.home.coef < -crit.value*anomalous.con.home.se), 1,0)
anomalous.con.sig.home <- 100*(sum(anomalous.con.home, na.rm=TRUE)/length(anomalous.con.home[!is.na(anomalous.con.home)]))
anomalous.con.business <- ifelse((anomalous.con.business.coef < -crit.value*anomalous.con.business.se), 1,0)
anomalous.con.sig.business <- 100*(sum(anomalous.con.business, na.rm=TRUE)/length(anomalous.con.business[!is.na(anomalous.con.business)]))
anomalous.con.person <- ifelse((anomalous.con.person.coef < -crit.value*anomalous.con.person.se), 1,0)
anomalous.con.sig.person <- 100*(sum(anomalous.con.person, na.rm=TRUE)/length(anomalous.con.person[!is.na(anomalous.con.person)]))
anomalous.con.car <- ifelse((anomalous.con.car.coef < -crit.value*anomalous.con.car.se), 1,0)
anomalous.con.sig.car <- 100*(sum(anomalous.con.car, na.rm=TRUE)/length(anomalous.con.car[!is.na(anomalous.con.car)]))
anomalous.con.extent <- ifelse((anomalous.con.extent.coef < -crit.value*anomalous.con.extent.se), 1,0)
anomalous.con.sig.extent <- 100*(sum(anomalous.con.extent, na.rm=TRUE)/length(anomalous.con.extent[!is.na(anomalous.con.extent)]))  
anomalous.con.warrant <- ifelse((anomalous.con.warrant.coef > crit.value*anomalous.con.warrant.se), 1,0)
anomalous.con.sig.warrant <- 100*(sum(anomalous.con.warrant, na.rm=TRUE)/length(anomalous.con.warrant[!is.na(anomalous.con.warrant)]))
anomalous.con.incident <- ifelse((anomalous.con.incident.coef > crit.value*anomalous.con.incident.se), 1,0)
anomalous.con.sig.incident <- 100*(sum(anomalous.con.incident, na.rm=TRUE)/length(anomalous.con.incident[!is.na(anomalous.con.incident)]))
anomalous.con.afterlaw <- ifelse((anomalous.con.afterlaw.coef > crit.value*anomalous.con.afterlaw.se), 1,0)
anomalous.con.sig.afterlaw <- 100*(sum(anomalous.con.afterlaw, na.rm=TRUE)/length(anomalous.con.afterlaw[!is.na(anomalous.con.afterlaw)]))
anomalous.con.unlawful <- ifelse((anomalous.con.unlawful.coef > crit.value*anomalous.con.unlawful.se), 1,0)
anomalous.con.sig.unlawful <- 100*(sum(anomalous.con.unlawful, na.rm=TRUE)/length(anomalous.con.unlawful[!is.na(anomalous.con.unlawful)]))
anomalous.con.except <- ifelse((anomalous.con.except.coef > crit.value*anomalous.con.except.se), 1,0)
anomalous.con.sig.except <- 100*(sum(anomalous.con.except,na.rm=TRUE)/length(anomalous.con.except[!is.na(anomalous.con.except)]))
anomalous.con.probcaus <- ifelse(((anomalous.con.probcaus.coef < -crit.value*anomalous.con.probcaus.se) | (anomalous.con.probcaus.coef > crit.value*anomalous.con.probcaus.se)), 1,0)
anomalous.con.sig.probcaus <- 100*(sum(anomalous.con.probcaus, na.rm=TRUE)/length(anomalous.con.probcaus[!is.na(anomalous.con.probcaus)]))
anomalous.con.change <- ifelse((anomalous.con.change.coef > crit.value*anomalous.con.change.se), 1,0)
anomalous.con.sig.change <- 100*(sum(anomalous.con.change, na.rm=TRUE)/length(anomalous.con.change[!is.na(anomalous.con.change)]))
              
          
#9) conservative no exceptions home

except.home <- ifelse((except.home.coef < -crit.value*except.home.se), 1,0)
except.sig.home <- 100*(sum(except.home, na.rm=TRUE)/length(except.home[!is.na(except.home)]))
except.business <- ifelse((except.business.coef < -crit.value*except.business.se), 1,0)
except.sig.business <- 100*(sum(except.business, na.rm=TRUE)/length(except.business[!is.na(except.business)]))
except.person <- ifelse((except.person.coef < -crit.value*except.person.se), 1,0)
except.sig.person <- 100*(sum(except.person, na.rm=TRUE)/length(except.person[!is.na(except.person)]))
except.car <- ifelse((except.car.coef < -crit.value*except.car.se), 1,0)
except.sig.car <- 100*(sum(except.car, na.rm=TRUE)/length(except.car[!is.na(except.car)]))
except.extent <- ifelse((except.extent.coef < -crit.value*except.extent.se), 1,0)
except.sig.extent <- 100*(sum(except.extent, na.rm=TRUE)/length(except.extent[!is.na(except.extent)]))  
except.warrant <- ifelse((except.warrant.coef > crit.value*except.warrant.se), 1,0)
except.sig.warrant <- 100*(sum(except.warrant, na.rm=TRUE)/length(except.warrant[!is.na(except.warrant)]))
except.incident <- ifelse((except.incident.coef > crit.value*except.incident.se), 1,0)
except.sig.incident <- 100*(sum(except.incident, na.rm=TRUE)/length(except.incident[!is.na(except.incident)]))
except.afterlaw <- ifelse((except.afterlaw.coef > crit.value*except.afterlaw.se), 1,0)
except.sig.afterlaw <- 100*(sum(except.afterlaw, na.rm=TRUE)/length(except.afterlaw[!is.na(except.afterlaw)]))
except.unlawful <- ifelse((except.unlawful.coef > crit.value*except.unlawful.se), 1,0)
except.sig.unlawful <- 100*(sum(except.unlawful, na.rm=TRUE)/length(except.unlawful[!is.na(except.unlawful)]))
except.except <- ifelse((except.except.coef > crit.value*except.except.se), 1,0)
except.sig.except <- 100*(sum(except.except,na.rm=TRUE)/length(except.except[!is.na(except.except)]))
except.probcaus <- ifelse(((except.probcaus.coef < -crit.value*except.probcaus.se) | (except.probcaus.coef > crit.value*except.probcaus.se)), 1,0)
except.sig.probcaus <- 100*(sum(except.probcaus, na.rm=TRUE)/length(except.probcaus[!is.na(except.probcaus)]))
except.change <- ifelse((except.change.coef > crit.value*except.change.se), 1,0)
except.sig.change <- 100*(sum(except.change, na.rm=TRUE)/length(except.change[!is.na(except.change)]))
            
###################################################################################
                    #####################################
                    
                    #DIFFERENCE
                    
       ###################################################################################
                    #####################################
                    
#FIRST DEFINE "TRUTH" COEFFICIENTS

home.truth <- -2.77
business.truth <- -2.34
person.truth<-  -1.94
car.truth <- -1.94
extent.truth <- -1.53
warrant.truth <- 1.73
incident.truth <- 2.89
afterlaw.truth <- 1.03
unlawful.truth <- 0
probcaus.truth <- 0
except.truth <- 1.37
change.truth <- .29

#Get counts for # of times coefficient do lie within "truth" +/- ~2 se's from "all cases" regression
#100 = perfect, 0 = bad

#1) all cases

all.home.2 <- ifelse((home.truth > all.home.coef - crit.value*all.home.se & home.truth < all.home.coef +crit.value*all.home.se), 1,0)
all.dif.home <- 100*(sum(all.home.2, na.rm=TRUE)/length(all.home.2[!is.na(all.home.2)]))
all.business.2 <- ifelse((business.truth > all.business.coef - crit.value*all.business.se & business.truth < all.business.coef +crit.value*all.business.se), 1,0)
all.dif.business <- 100*(sum(all.business.2, na.rm=TRUE)/length(all.business.2[!is.na(all.business.2)]))
all.person.2 <- ifelse((person.truth > all.person.coef - crit.value*all.person.se & person.truth < all.person.coef +crit.value*all.person.se), 1,0)
all.dif.person <- 100*(sum(all.person.2, na.rm=TRUE)/length(all.person.2[!is.na(all.person.2)]))
all.car.2 <- ifelse((car.truth > all.car.coef - crit.value*all.car.se & car.truth < all.car.coef +crit.value*all.car.se), 1,0)
all.dif.car <- 100*(sum(all.car.2, na.rm=TRUE)/length(all.car.2[!is.na(all.car.2)]))
all.extent.2 <- ifelse((extent.truth > all.extent.coef - crit.value*all.extent.se & extent.truth < all.extent.coef +crit.value*all.extent.se), 1,0)
all.dif.extent <- 100*(sum(all.extent.2, na.rm=TRUE)/length(all.extent.2[!is.na(all.extent.2)]))
all.warrant.2 <- ifelse((warrant.truth > all.warrant.coef - crit.value*all.warrant.se & warrant.truth < all.warrant.coef +crit.value*all.warrant.se), 1,0)
all.dif.warrant <- 100*(sum(all.warrant.2, na.rm=TRUE)/length(all.warrant.2[!is.na(all.warrant.2)]))
all.incident.2 <- ifelse((incident.truth > all.incident.coef - crit.value*all.incident.se & incident.truth < all.incident.coef +crit.value*all.incident.se), 1,0)
all.dif.incident <- 100*(sum(all.incident.2, na.rm=TRUE)/length(all.incident.2[!is.na(all.incident.2)]))
all.afterlaw.2 <- ifelse((afterlaw.truth > all.afterlaw.coef - crit.value*all.afterlaw.se & afterlaw.truth < all.afterlaw.coef +crit.value*all.afterlaw.se), 1,0)
all.dif.afterlaw <- 100*(sum(all.afterlaw.2, na.rm=TRUE)/length(all.afterlaw.2[!is.na(all.afterlaw.2)]))
all.unlawful.2 <- ifelse((unlawful.truth > all.unlawful.coef - crit.value*all.unlawful.se & unlawful.truth < all.unlawful.coef +crit.value*all.unlawful.se), 1,0)
all.dif.unlawful <- 100*(sum(all.unlawful.2, na.rm=TRUE)/length(all.unlawful.2[!is.na(all.unlawful.2)]))
all.probcaus.2 <- ifelse((probcaus.truth > all.probcaus.coef - crit.value*all.probcaus.se & probcaus.truth < all.probcaus.coef +crit.value*all.probcaus.se), 1,0)
all.dif.probcaus <- 100*(sum(all.probcaus.2, na.rm=TRUE)/length(all.probcaus.2[!is.na(all.probcaus.2)]))
all.except.2 <- ifelse((except.truth > all.except.coef - crit.value*all.except.se & except.truth < all.except.coef +crit.value*all.except.se), 1,0)
all.dif.except <- 100*(sum(all.except.2, na.rm=TRUE)/length(all.except.2[!is.na(all.except.2)]))
all.change.2 <- ifelse((change.truth > all.change.coef - crit.value*all.change.se & change.truth < all.change.coef +crit.value*all.change.se), 1,0)
all.dif.change <- 100*(sum(all.change.2, na.rm=TRUE)/length(all.change.2[!is.na(all.change.2)]))


#2) random

random.home.2 <- ifelse((home.truth > random.home.coef - crit.value*random.home.se & home.truth < random.home.coef + crit.value*random.home.se), 1,0)
random.dif.home <- 100*(sum(random.home.2, na.rm=TRUE)/length(random.home.2[!is.na(random.home.2)]))
random.business.2 <- ifelse((business.truth > random.business.coef - crit.value*random.business.se & business.truth < random.business.coef +crit.value*random.business.se), 1,0)
random.dif.business <- 100*(sum(random.business.2, na.rm=TRUE)/length(random.business.2[!is.na(random.business.2)]))
random.person.2 <- ifelse((person.truth > random.person.coef - crit.value*random.person.se & person.truth < random.person.coef +crit.value*random.person.se), 1,0)
random.dif.person <- 100*(sum(random.person.2, na.rm=TRUE)/length(random.person.2[!is.na(random.person.2)]))
random.car.2 <- ifelse((car.truth > random.car.coef - crit.value*random.car.se & car.truth < random.car.coef +crit.value*random.car.se), 1,0)
random.dif.car <- 100*(sum(random.car.2, na.rm=TRUE)/length(random.car.2[!is.na(random.car.2)]))
random.extent.2 <- ifelse((extent.truth > random.extent.coef - crit.value*random.extent.se & extent.truth < random.extent.coef +crit.value*random.extent.se), 1,0)
random.dif.extent <- 100*(sum(random.extent.2, na.rm=TRUE)/length(random.extent.2[!is.na(random.extent.2)]))
random.warrant.2 <- ifelse((warrant.truth > random.warrant.coef - crit.value*random.warrant.se & warrant.truth < random.warrant.coef +crit.value*random.warrant.se), 1,0)
random.dif.warrant <- 100*(sum(random.warrant.2, na.rm=TRUE)/length(random.warrant.2[!is.na(random.warrant.2)]))
random.incident.2 <- ifelse((incident.truth > random.incident.coef - crit.value*random.incident.se & incident.truth < random.incident.coef +crit.value*random.incident.se), 1,0)
random.dif.incident <- 100*(sum(random.incident.2, na.rm=TRUE)/length(random.incident.2[!is.na(random.incident.2)]))
random.afterlaw.2 <- ifelse((afterlaw.truth > random.afterlaw.coef - crit.value*random.afterlaw.se & afterlaw.truth < random.afterlaw.coef +crit.value*random.afterlaw.se), 1,0)
random.dif.afterlaw <- 100*(sum(random.afterlaw.2, na.rm=TRUE)/length(random.afterlaw.2[!is.na(random.afterlaw.2)]))
random.unlawful.2 <- ifelse((unlawful.truth > random.unlawful.coef - crit.value*random.unlawful.se & unlawful.truth < random.unlawful.coef +crit.value*random.unlawful.se), 1,0)
random.dif.unlawful <- 100*(sum(random.unlawful.2, na.rm=TRUE)/length(random.unlawful.2[!is.na(random.unlawful.2)]))
random.probcaus.2 <- ifelse((probcaus.truth > random.probcaus.coef - crit.value*random.probcaus.se & probcaus.truth < random.probcaus.coef +crit.value*random.probcaus.se), 1,0)
random.dif.probcaus <- 100*(sum(random.probcaus.2, na.rm=TRUE)/length(random.probcaus.2[!is.na(random.probcaus.2)]))
random.except.2 <- ifelse((except.truth > random.except.coef - crit.value*random.except.se & except.truth < random.except.coef +crit.value*random.except.se), 1,0)
random.dif.except <- 100*(sum(random.except.2, na.rm=TRUE)/length(random.except.2[!is.na(random.except.2)]))
random.change.2 <- ifelse((change.truth > random.change.coef - crit.value*random.change.se & change.truth < random.change.coef +crit.value*random.change.se), 1,0)
random.dif.change <- 100*(sum(random.change.2, na.rm=TRUE)/length(random.change.2[!is.na(random.change.2)]))

#3) Close, all

close.all.home.2 <- ifelse((home.truth > close.all.home.coef - crit.value*close.all.home.se & home.truth < close.all.home.coef +crit.value*close.all.home.se), 1,0)
close.all.dif.home <- 100*(sum(close.all.home.2, na.rm=TRUE)/length(close.all.home.2[!is.na(close.all.home.2)]))
close.all.business.2 <- ifelse((business.truth > close.all.business.coef - crit.value*close.all.business.se & business.truth < close.all.business.coef +crit.value*close.all.business.se), 1,0)
close.all.dif.business <- 100*(sum(close.all.business.2, na.rm=TRUE)/length(close.all.business.2[!is.na(close.all.business.2)]))
close.all.person.2 <- ifelse((person.truth > close.all.person.coef - crit.value*close.all.person.se & person.truth < close.all.person.coef +crit.value*close.all.person.se), 1,0)
close.all.dif.person <- 100*(sum(close.all.person.2, na.rm=TRUE)/length(close.all.person.2[!is.na(close.all.person.2)]))
close.all.car.2 <- ifelse((car.truth > close.all.car.coef - crit.value*close.all.car.se & car.truth < close.all.car.coef +crit.value*close.all.car.se), 1,0)
close.all.dif.car <- 100*(sum(close.all.car.2, na.rm=TRUE)/length(close.all.car.2[!is.na(close.all.car.2)]))
close.all.extent.2 <- ifelse((extent.truth > close.all.extent.coef - crit.value*close.all.extent.se & extent.truth < close.all.extent.coef +crit.value*close.all.extent.se), 1,0)
close.all.dif.extent <- 100*(sum(close.all.extent.2, na.rm=TRUE)/length(close.all.extent.2[!is.na(close.all.extent.2)]))
close.all.warrant.2 <- ifelse((warrant.truth > close.all.warrant.coef - crit.value*close.all.warrant.se & warrant.truth < close.all.warrant.coef +crit.value*close.all.warrant.se), 1,0)
close.all.dif.warrant <- 100*(sum(close.all.warrant.2, na.rm=TRUE)/length(close.all.warrant.2[!is.na(close.all.warrant.2)]))
close.all.incident.2 <- ifelse((incident.truth > close.all.incident.coef - crit.value*close.all.incident.se & incident.truth < close.all.incident.coef +crit.value*close.all.incident.se), 1,0)
close.all.dif.incident <- 100*(sum(close.all.incident.2, na.rm=TRUE)/length(close.all.incident.2[!is.na(close.all.incident.2)]))
close.all.afterlaw.2 <- ifelse((afterlaw.truth > close.all.afterlaw.coef - crit.value*close.all.afterlaw.se & afterlaw.truth < close.all.afterlaw.coef +crit.value*close.all.afterlaw.se), 1,0)
close.all.dif.afterlaw <- 100*(sum(close.all.afterlaw.2, na.rm=TRUE)/length(close.all.afterlaw.2[!is.na(close.all.afterlaw.2)]))
close.all.unlawful.2 <- ifelse((unlawful.truth > close.all.unlawful.coef - crit.value*close.all.unlawful.se & unlawful.truth < close.all.unlawful.coef +crit.value*close.all.unlawful.se), 1,0)
close.all.dif.unlawful <- 100*(sum(close.all.unlawful.2, na.rm=TRUE)/length(close.all.unlawful.2[!is.na(close.all.unlawful.2)]))
close.all.probcaus.2 <- ifelse((probcaus.truth > close.all.probcaus.coef - crit.value*close.all.probcaus.se & probcaus.truth < close.all.probcaus.coef +crit.value*close.all.probcaus.se), 1,0)
close.all.dif.probcaus <- 100*(sum(close.all.probcaus.2, na.rm=TRUE)/length(close.all.probcaus.2[!is.na(close.all.probcaus.2)]))
close.all.except.2 <- ifelse((except.truth > close.all.except.coef - crit.value*close.all.except.se & except.truth < close.all.except.coef +crit.value*close.all.except.se), 1,0)
close.all.dif.except <- 100*(sum(close.all.except.2, na.rm=TRUE)/length(close.all.except.2[!is.na(close.all.except.2)]))
close.all.change.2 <- ifelse((change.truth > close.all.change.coef - crit.value*close.all.change.se & change.truth < close.all.change.coef +crit.value*close.all.change.se), 1,0)
close.all.dif.change <- 100*(sum(close.all.change.2, na.rm=TRUE)/length(close.all.change.2[!is.na(close.all.change.2)]))

#4) Close Liberal

close.lib.home.2 <- ifelse((home.truth > close.lib.home.coef - crit.value*close.lib.home.se & home.truth < close.lib.home.coef +crit.value*close.lib.home.se), 1,0)
close.lib.dif.home <- 100*(sum(close.lib.home.2, na.rm=TRUE)/length(close.lib.home.2[!is.na(close.lib.home.2)]))
close.lib.business.2 <- ifelse((business.truth > close.lib.business.coef - crit.value*close.lib.business.se & business.truth < close.lib.business.coef +crit.value*close.lib.business.se), 1,0)
close.lib.dif.business <- 100*(sum(close.lib.business.2, na.rm=TRUE)/length(close.lib.business.2[!is.na(close.lib.business.2)]))
close.lib.person.2 <- ifelse((person.truth > close.lib.person.coef - crit.value*close.lib.person.se & person.truth < close.lib.person.coef +crit.value*close.lib.person.se), 1,0)
close.lib.dif.person <- 100*(sum(close.lib.person.2, na.rm=TRUE)/length(close.lib.person.2[!is.na(close.lib.person.2)]))
close.lib.car.2 <- ifelse((car.truth > close.lib.car.coef - crit.value*close.lib.car.se & car.truth < close.lib.car.coef +crit.value*close.lib.car.se), 1,0)
close.lib.dif.car <- 100*(sum(close.lib.car.2, na.rm=TRUE)/length(close.lib.car.2[!is.na(close.lib.car.2)]))
close.lib.extent.2 <- ifelse((extent.truth > close.lib.extent.coef - crit.value*close.lib.extent.se & extent.truth < close.lib.extent.coef +crit.value*close.lib.extent.se), 1,0)
close.lib.dif.extent <- 100*(sum(close.lib.extent.2, na.rm=TRUE)/length(close.lib.extent.2[!is.na(close.lib.extent.2)]))
close.lib.warrant.2 <- ifelse((warrant.truth > close.lib.warrant.coef - crit.value*close.lib.warrant.se & warrant.truth < close.lib.warrant.coef +crit.value*close.lib.warrant.se), 1,0)
close.lib.dif.warrant <- 100*(sum(close.lib.warrant.2, na.rm=TRUE)/length(close.lib.warrant.2[!is.na(close.lib.warrant.2)]))
close.lib.incident.2 <- ifelse((incident.truth > close.lib.incident.coef - crit.value*close.lib.incident.se & incident.truth < close.lib.incident.coef +crit.value*close.lib.incident.se), 1,0)
close.lib.dif.incident <- 100*(sum(close.lib.incident.2, na.rm=TRUE)/length(close.lib.incident.2[!is.na(close.lib.incident.2)]))
close.lib.afterlaw.2 <- ifelse((afterlaw.truth > close.lib.afterlaw.coef - crit.value*close.lib.afterlaw.se & afterlaw.truth < close.lib.afterlaw.coef +crit.value*close.lib.afterlaw.se), 1,0)
close.lib.dif.afterlaw <- 100*(sum(close.lib.afterlaw.2, na.rm=TRUE)/length(close.lib.afterlaw.2[!is.na(close.lib.afterlaw.2)]))
close.lib.unlawful.2 <- ifelse((unlawful.truth > close.lib.unlawful.coef - crit.value*close.lib.unlawful.se & unlawful.truth < close.lib.unlawful.coef +crit.value*close.lib.unlawful.se), 1,0)
close.lib.dif.unlawful <- 100*(sum(close.lib.unlawful.2, na.rm=TRUE)/length(close.lib.unlawful.2[!is.na(close.lib.unlawful.2)]))
close.lib.probcaus.2 <- ifelse((probcaus.truth > close.lib.probcaus.coef - crit.value*close.lib.probcaus.se & probcaus.truth < close.lib.probcaus.coef +crit.value*close.lib.probcaus.se), 1,0)
close.lib.dif.probcaus <- 100*(sum(close.lib.probcaus.2, na.rm=TRUE)/length(close.lib.probcaus.2[!is.na(close.lib.probcaus.2)]))
close.lib.except.2 <- ifelse((except.truth > close.lib.except.coef - crit.value*close.lib.except.se & except.truth < close.lib.except.coef +crit.value*close.lib.except.se), 1,0)
close.lib.dif.except <- 100*(sum(close.lib.except.2, na.rm=TRUE)/length(close.lib.except.2[!is.na(close.lib.except.2)]))
close.lib.change.2 <- ifelse((change.truth > close.lib.change.coef - crit.value*close.lib.change.se & change.truth < close.lib.change.coef +crit.value*close.lib.change.se), 1,0)
close.lib.dif.change <- 100*(sum(close.lib.change.2, na.rm=TRUE)/length(close.lib.change.2[!is.na(close.lib.change.2)]))


#5) Close Conservative

close.con.home.2 <- ifelse((home.truth > close.con.home.coef - crit.value*close.con.home.se & home.truth < close.con.home.coef +crit.value*close.con.home.se), 1,0)
close.con.dif.home <- 100*(sum(close.con.home.2, na.rm=TRUE)/length(close.con.home.2[!is.na(close.con.home.2)]))
close.con.business.2 <- ifelse((business.truth > close.con.business.coef - crit.value*close.con.business.se & business.truth < close.con.business.coef +crit.value*close.con.business.se), 1,0)
close.con.dif.business <- 100*(sum(close.con.business.2, na.rm=TRUE)/length(close.con.business.2[!is.na(close.con.business.2)]))
close.con.person.2 <- ifelse((person.truth > close.con.person.coef - crit.value*close.con.person.se & person.truth < close.con.person.coef +crit.value*close.con.person.se), 1,0)
close.con.dif.person <- 100*(sum(close.con.person.2, na.rm=TRUE)/length(close.con.person.2[!is.na(close.con.person.2)]))
close.con.car.2 <- ifelse((car.truth > close.con.car.coef - crit.value*close.con.car.se & car.truth < close.con.car.coef +crit.value*close.con.car.se), 1,0)
close.con.dif.car <- 100*(sum(close.con.car.2, na.rm=TRUE)/length(close.con.car.2[!is.na(close.con.car.2)]))
close.con.extent.2 <- ifelse((extent.truth > close.con.extent.coef - crit.value*close.con.extent.se & extent.truth < close.con.extent.coef +crit.value*close.con.extent.se), 1,0)
close.con.dif.extent <- 100*(sum(close.con.extent.2, na.rm=TRUE)/length(close.con.extent.2[!is.na(close.con.extent.2)]))
close.con.warrant.2 <- ifelse((warrant.truth > close.con.warrant.coef - crit.value*close.con.warrant.se & warrant.truth < close.con.warrant.coef +crit.value*close.con.warrant.se), 1,0)
close.con.dif.warrant <- 100*(sum(close.con.warrant.2, na.rm=TRUE)/length(close.con.warrant.2[!is.na(close.con.warrant.2)]))
close.con.incident.2 <- ifelse((incident.truth > close.con.incident.coef - crit.value*close.con.incident.se & incident.truth < close.con.incident.coef +crit.value*close.con.incident.se), 1,0)
close.con.dif.incident <- 100*(sum(close.con.incident.2, na.rm=TRUE)/length(close.con.incident.2[!is.na(close.con.incident.2)]))
close.con.afterlaw.2 <- ifelse((afterlaw.truth > close.con.afterlaw.coef - crit.value*close.con.afterlaw.se & afterlaw.truth < close.con.afterlaw.coef +crit.value*close.con.afterlaw.se), 1,0)
close.con.dif.afterlaw <- 100*(sum(close.con.afterlaw.2, na.rm=TRUE)/length(close.con.afterlaw.2[!is.na(close.con.afterlaw.2)]))
close.con.unlawful.2 <- ifelse((unlawful.truth > close.con.unlawful.coef - crit.value*close.con.unlawful.se & unlawful.truth < close.con.unlawful.coef +crit.value*close.con.unlawful.se), 1,0)
close.con.dif.unlawful <- 100*(sum(close.con.unlawful.2, na.rm=TRUE)/length(close.con.unlawful.2[!is.na(close.con.unlawful.2)]))
close.con.probcaus.2 <- ifelse((probcaus.truth > close.con.probcaus.coef - crit.value*close.con.probcaus.se & probcaus.truth < close.con.probcaus.coef +crit.value*close.con.probcaus.se), 1,0)
close.con.dif.probcaus <- 100*(sum(close.con.probcaus.2, na.rm=TRUE)/length(close.con.probcaus.2[!is.na(close.con.probcaus.2)]))
close.con.except.2 <- ifelse((except.truth > close.con.except.coef - crit.value*close.con.except.se & except.truth < close.con.except.coef +crit.value*close.con.except.se), 1,0)
close.con.dif.except <- 100*(sum(close.con.except.2, na.rm=TRUE)/length(close.con.except.2[!is.na(close.con.except.2)]))
close.con.change.2 <- ifelse((change.truth > close.con.change.coef - crit.value*close.con.change.se & change.truth < close.con.change.coef +crit.value*close.con.change.se), 1,0)
close.con.dif.change <- 100*(sum(close.con.change.2, na.rm=TRUE)/length(close.con.change.2[!is.na(close.con.change.2)]))


#6) Anomalous all

anomalous.all.home.2 <- ifelse((home.truth > anomalous.all.home.coef - crit.value*anomalous.all.home.se & home.truth < anomalous.all.home.coef +crit.value*anomalous.all.home.se), 1,0)
anomalous.all.dif.home <- 100*(sum(anomalous.all.home.2, na.rm=TRUE)/length(anomalous.all.home.2[!is.na(anomalous.all.home.2)]))
anomalous.all.business.2 <- ifelse((business.truth > anomalous.all.business.coef - crit.value*anomalous.all.business.se & business.truth < anomalous.all.business.coef +crit.value*anomalous.all.business.se), 1,0)
anomalous.all.dif.business <- 100*(sum(anomalous.all.business.2, na.rm=TRUE)/length(anomalous.all.business.2[!is.na(anomalous.all.business.2)]))
anomalous.all.person.2 <- ifelse((person.truth > anomalous.all.person.coef - crit.value*anomalous.all.person.se & person.truth < anomalous.all.person.coef +crit.value*anomalous.all.person.se), 1,0)
anomalous.all.dif.person <- 100*(sum(anomalous.all.person.2, na.rm=TRUE)/length(anomalous.all.person.2[!is.na(anomalous.all.person.2)]))
anomalous.all.car.2 <- ifelse((car.truth > anomalous.all.car.coef - crit.value*anomalous.all.car.se & car.truth < anomalous.all.car.coef +crit.value*anomalous.all.car.se), 1,0)
anomalous.all.dif.car <- 100*(sum(anomalous.all.car.2, na.rm=TRUE)/length(anomalous.all.car.2[!is.na(anomalous.all.car.2)]))
anomalous.all.extent.2 <- ifelse((extent.truth > anomalous.all.extent.coef - crit.value*anomalous.all.extent.se & extent.truth < anomalous.all.extent.coef +crit.value*anomalous.all.extent.se), 1,0)
anomalous.all.dif.extent <- 100*(sum(anomalous.all.extent.2, na.rm=TRUE)/length(anomalous.all.extent.2[!is.na(anomalous.all.extent.2)]))
anomalous.all.warrant.2 <- ifelse((warrant.truth > anomalous.all.warrant.coef - crit.value*anomalous.all.warrant.se & warrant.truth < anomalous.all.warrant.coef +crit.value*anomalous.all.warrant.se), 1,0)
anomalous.all.dif.warrant <- 100*(sum(anomalous.all.warrant.2, na.rm=TRUE)/length(anomalous.all.warrant.2[!is.na(anomalous.all.warrant.2)]))
anomalous.all.incident.2 <- ifelse((incident.truth > anomalous.all.incident.coef - crit.value*anomalous.all.incident.se & incident.truth < anomalous.all.incident.coef +crit.value*anomalous.all.incident.se), 1,0)
anomalous.all.dif.incident <- 100*(sum(anomalous.all.incident.2, na.rm=TRUE)/length(anomalous.all.incident.2[!is.na(anomalous.all.incident.2)]))
anomalous.all.afterlaw.2 <- ifelse((afterlaw.truth > anomalous.all.afterlaw.coef - crit.value*anomalous.all.afterlaw.se & afterlaw.truth < anomalous.all.afterlaw.coef +crit.value*anomalous.all.afterlaw.se), 1,0)
anomalous.all.dif.afterlaw <- 100*(sum(anomalous.all.afterlaw.2, na.rm=TRUE)/length(anomalous.all.afterlaw.2[!is.na(anomalous.all.afterlaw.2)]))
anomalous.all.unlawful.2 <- ifelse((unlawful.truth > anomalous.all.unlawful.coef - crit.value*anomalous.all.unlawful.se & unlawful.truth < anomalous.all.unlawful.coef +crit.value*anomalous.all.unlawful.se), 1,0)
anomalous.all.dif.unlawful <- 100*(sum(anomalous.all.unlawful.2, na.rm=TRUE)/length(anomalous.all.unlawful.2[!is.na(anomalous.all.unlawful.2)]))
anomalous.all.probcaus.2 <- ifelse((probcaus.truth > anomalous.all.probcaus.coef - crit.value*anomalous.all.probcaus.se & probcaus.truth < anomalous.all.probcaus.coef +crit.value*anomalous.all.probcaus.se), 1,0)
anomalous.all.dif.probcaus <- 100*(sum(anomalous.all.probcaus.2, na.rm=TRUE)/length(anomalous.all.probcaus.2[!is.na(anomalous.all.probcaus.2)]))
anomalous.all.except.2 <- ifelse((except.truth > anomalous.all.except.coef - crit.value*anomalous.all.except.se & except.truth < anomalous.all.except.coef +crit.value*anomalous.all.except.se), 1,0)
anomalous.all.dif.except <- 100*(sum(anomalous.all.except.2, na.rm=TRUE)/length(anomalous.all.except.2[!is.na(anomalous.all.except.2)]))
anomalous.all.change.2 <- ifelse((change.truth > anomalous.all.change.coef - crit.value*anomalous.all.change.se & change.truth < anomalous.all.change.coef +crit.value*anomalous.all.change.se), 1,0)
anomalous.all.dif.change <- 100*(sum(anomalous.all.change.2, na.rm=TRUE)/length(anomalous.all.change.2[!is.na(anomalous.all.change.2)]))

#7) Anomalous  liberal

anomalous.lib.home.2 <- ifelse((home.truth > anomalous.lib.home.coef - crit.value*anomalous.lib.home.se & home.truth < anomalous.lib.home.coef +crit.value*anomalous.lib.home.se), 1,0)
anomalous.lib.dif.home <- 100*(sum(anomalous.lib.home.2, na.rm=TRUE)/length(anomalous.lib.home.2[!is.na(anomalous.lib.home.2)]))
anomalous.lib.business.2 <- ifelse((business.truth > anomalous.lib.business.coef - crit.value*anomalous.lib.business.se & business.truth < anomalous.lib.business.coef +crit.value*anomalous.lib.business.se), 1,0)
anomalous.lib.dif.business <- 100*(sum(anomalous.lib.business.2, na.rm=TRUE)/length(anomalous.lib.business.2[!is.na(anomalous.lib.business.2)]))
anomalous.lib.person.2 <- ifelse((person.truth > anomalous.lib.person.coef - crit.value*anomalous.lib.person.se & person.truth < anomalous.lib.person.coef +crit.value*anomalous.lib.person.se), 1,0)
anomalous.lib.dif.person <- 100*(sum(anomalous.lib.person.2, na.rm=TRUE)/length(anomalous.lib.person.2[!is.na(anomalous.lib.person.2)]))
anomalous.lib.car.2 <- ifelse((car.truth > anomalous.lib.car.coef - crit.value*anomalous.lib.car.se & car.truth < anomalous.lib.car.coef +crit.value*anomalous.lib.car.se), 1,0)
anomalous.lib.dif.car <- 100*(sum(anomalous.lib.car.2, na.rm=TRUE)/length(anomalous.lib.car.2[!is.na(anomalous.lib.car.2)]))
anomalous.lib.extent.2 <- ifelse((extent.truth > anomalous.lib.extent.coef - crit.value*anomalous.lib.extent.se & extent.truth < anomalous.lib.extent.coef +crit.value*anomalous.lib.extent.se), 1,0)
anomalous.lib.dif.extent <- 100*(sum(anomalous.lib.extent.2, na.rm=TRUE)/length(anomalous.lib.extent.2[!is.na(anomalous.lib.extent.2)]))
anomalous.lib.warrant.2 <- ifelse((warrant.truth > anomalous.lib.warrant.coef - crit.value*anomalous.lib.warrant.se & warrant.truth < anomalous.lib.warrant.coef +crit.value*anomalous.lib.warrant.se), 1,0)
anomalous.lib.dif.warrant <- 100*(sum(anomalous.lib.warrant.2, na.rm=TRUE)/length(anomalous.lib.warrant.2[!is.na(anomalous.lib.warrant.2)]))
anomalous.lib.incident.2 <- ifelse((incident.truth > anomalous.lib.incident.coef - crit.value*anomalous.lib.incident.se & incident.truth < anomalous.lib.incident.coef +crit.value*anomalous.lib.incident.se), 1,0)
anomalous.lib.dif.incident <- 100*(sum(anomalous.lib.incident.2, na.rm=TRUE)/length(anomalous.lib.incident.2[!is.na(anomalous.lib.incident.2)]))
anomalous.lib.afterlaw.2 <- ifelse((afterlaw.truth > anomalous.lib.afterlaw.coef - crit.value*anomalous.lib.afterlaw.se & afterlaw.truth < anomalous.lib.afterlaw.coef +crit.value*anomalous.lib.afterlaw.se), 1,0)
anomalous.lib.dif.afterlaw <- 100*(sum(anomalous.lib.afterlaw.2, na.rm=TRUE)/length(anomalous.lib.afterlaw.2[!is.na(anomalous.lib.afterlaw.2)]))
anomalous.lib.unlawful.2 <- ifelse((unlawful.truth > anomalous.lib.unlawful.coef - crit.value*anomalous.lib.unlawful.se & unlawful.truth < anomalous.lib.unlawful.coef +crit.value*anomalous.lib.unlawful.se), 1,0)
anomalous.lib.dif.unlawful <- 100*(sum(anomalous.lib.unlawful.2, na.rm=TRUE)/length(anomalous.lib.unlawful.2[!is.na(anomalous.lib.unlawful.2)]))
anomalous.lib.probcaus.2 <- ifelse((probcaus.truth > anomalous.lib.probcaus.coef - crit.value*anomalous.lib.probcaus.se & probcaus.truth < anomalous.lib.probcaus.coef +crit.value*anomalous.lib.probcaus.se), 1,0)
anomalous.lib.dif.probcaus <- 100*(sum(anomalous.lib.probcaus.2, na.rm=TRUE)/length(anomalous.lib.probcaus.2[!is.na(anomalous.lib.probcaus.2)]))
anomalous.lib.except.2 <- ifelse((except.truth > anomalous.lib.except.coef - crit.value*anomalous.lib.except.se & except.truth < anomalous.lib.except.coef +crit.value*anomalous.lib.except.se), 1,0)
anomalous.lib.dif.except <- 100*(sum(anomalous.lib.except.2, na.rm=TRUE)/length(anomalous.lib.except.2[!is.na(anomalous.lib.except.2)]))
anomalous.lib.change.2 <- ifelse((change.truth > anomalous.lib.change.coef - crit.value*anomalous.lib.change.se & change.truth < anomalous.lib.change.coef +crit.value*anomalous.lib.change.se), 1,0)
anomalous.lib.dif.change <- 100*(sum(anomalous.lib.change.2, na.rm=TRUE)/length(anomalous.lib.change.2[!is.na(anomalous.lib.change.2)]))

#8) Anomalous  conservative

anomalous.con.home.2 <- ifelse((home.truth > anomalous.con.home.coef - crit.value*anomalous.con.home.se & home.truth < anomalous.con.home.coef +crit.value*anomalous.con.home.se), 1,0)
anomalous.con.dif.home <- 100*(sum(anomalous.con.home.2, na.rm=TRUE)/length(anomalous.con.home.2[!is.na(anomalous.con.home.2)]))
anomalous.con.business.2 <- ifelse((business.truth > anomalous.con.business.coef - crit.value*anomalous.con.business.se & business.truth < anomalous.con.business.coef +crit.value*anomalous.con.business.se), 1,0)
anomalous.con.dif.business <- 100*(sum(anomalous.con.business.2, na.rm=TRUE)/length(anomalous.con.business.2[!is.na(anomalous.con.business.2)]))
anomalous.con.person.2 <- ifelse((person.truth > anomalous.con.person.coef - crit.value*anomalous.con.person.se & person.truth < anomalous.con.person.coef +crit.value*anomalous.con.person.se), 1,0)
anomalous.con.dif.person <- 100*(sum(anomalous.con.person.2, na.rm=TRUE)/length(anomalous.con.person.2[!is.na(anomalous.con.person.2)]))
anomalous.con.car.2 <- ifelse((car.truth > anomalous.con.car.coef - crit.value*anomalous.con.car.se & car.truth < anomalous.con.car.coef +crit.value*anomalous.con.car.se), 1,0)
anomalous.con.dif.car <- 100*(sum(anomalous.con.car.2, na.rm=TRUE)/length(anomalous.con.car.2[!is.na(anomalous.con.car.2)]))
anomalous.con.extent.2 <- ifelse((extent.truth > anomalous.con.extent.coef - crit.value*anomalous.con.extent.se & extent.truth < anomalous.con.extent.coef +crit.value*anomalous.con.extent.se), 1,0)
anomalous.con.dif.extent <- 100*(sum(anomalous.con.extent.2, na.rm=TRUE)/length(anomalous.con.extent.2[!is.na(anomalous.con.extent.2)]))
anomalous.con.warrant.2 <- ifelse((warrant.truth > anomalous.con.warrant.coef - crit.value*anomalous.con.warrant.se & warrant.truth < anomalous.con.warrant.coef +crit.value*anomalous.con.warrant.se), 1,0)
anomalous.con.dif.warrant <- 100*(sum(anomalous.con.warrant.2, na.rm=TRUE)/length(anomalous.con.warrant.2[!is.na(anomalous.con.warrant.2)]))
anomalous.con.incident.2 <- ifelse((incident.truth > anomalous.con.incident.coef - crit.value*anomalous.con.incident.se & incident.truth < anomalous.con.incident.coef +crit.value*anomalous.con.incident.se), 1,0)
anomalous.con.dif.incident <- 100*(sum(anomalous.con.incident.2, na.rm=TRUE)/length(anomalous.con.incident.2[!is.na(anomalous.con.incident.2)]))
anomalous.con.afterlaw.2 <- ifelse((afterlaw.truth > anomalous.con.afterlaw.coef - crit.value*anomalous.con.afterlaw.se & afterlaw.truth < anomalous.con.afterlaw.coef +crit.value*anomalous.con.afterlaw.se), 1,0)
anomalous.con.dif.afterlaw <- 100*(sum(anomalous.con.afterlaw.2, na.rm=TRUE)/length(anomalous.con.afterlaw.2[!is.na(anomalous.con.afterlaw.2)]))
anomalous.con.unlawful.2 <- ifelse((unlawful.truth > anomalous.con.unlawful.coef - crit.value*anomalous.con.unlawful.se & unlawful.truth < anomalous.con.unlawful.coef +crit.value*anomalous.con.unlawful.se), 1,0)
anomalous.con.dif.unlawful <- 100*(sum(anomalous.con.unlawful.2, na.rm=TRUE)/length(anomalous.con.unlawful.2[!is.na(anomalous.con.unlawful.2)]))
anomalous.con.probcaus.2 <- ifelse((probcaus.truth > anomalous.con.probcaus.coef - crit.value*anomalous.con.probcaus.se & probcaus.truth < anomalous.con.probcaus.coef +crit.value*anomalous.con.probcaus.se), 1,0)
anomalous.con.dif.probcaus <- 100*(sum(anomalous.con.probcaus.2, na.rm=TRUE)/length(anomalous.con.probcaus.2[!is.na(anomalous.con.probcaus.2)]))
anomalous.con.except.2 <- ifelse((except.truth > anomalous.con.except.coef - crit.value*anomalous.con.except.se & except.truth < anomalous.con.except.coef +crit.value*anomalous.con.except.se), 1,0)
anomalous.con.dif.except <- 100*(sum(anomalous.con.except.2, na.rm=TRUE)/length(anomalous.con.except.2[!is.na(anomalous.con.except.2)]))
anomalous.con.change.2 <- ifelse((change.truth > anomalous.con.change.coef - crit.value*anomalous.con.change.se & change.truth < anomalous.con.change.coef +crit.value*anomalous.con.change.se), 1,0)
anomalous.con.dif.change <- 100*(sum(anomalous.con.change.2, na.rm=TRUE)/length(anomalous.con.change.2[!is.na(anomalous.con.change.2)]))
  
#9) home, no exceptions

except.home.2 <- ifelse((home.truth > except.home.coef - crit.value*except.home.se & home.truth < except.home.coef +crit.value*except.home.se), 1,0)
except.dif.home <- 100*(sum(except.home.2, na.rm=TRUE)/length(except.home.2[!is.na(except.home.2)]))
except.business.2 <- ifelse((business.truth > except.business.coef - crit.value*except.business.se & business.truth < except.business.coef +crit.value*except.business.se), 1,0)
except.dif.business <- 100*(sum(except.business.2, na.rm=TRUE)/length(except.business.2[!is.na(except.business.2)]))
except.person.2 <- ifelse((person.truth > except.person.coef - crit.value*except.person.se & person.truth < except.person.coef +crit.value*except.person.se), 1,0)
except.dif.person <- 100*(sum(except.person.2, na.rm=TRUE)/length(except.person.2[!is.na(except.person.2)]))
except.car.2 <- ifelse((car.truth > except.car.coef - crit.value*except.car.se & car.truth < except.car.coef +crit.value*except.car.se), 1,0)
except.dif.car <- 100*(sum(except.car.2, na.rm=TRUE)/length(except.car.2[!is.na(except.car.2)]))
except.extent.2 <- ifelse((extent.truth > except.extent.coef - crit.value*except.extent.se & extent.truth < except.extent.coef +crit.value*except.extent.se), 1,0)
except.dif.extent <- 100*(sum(except.extent.2, na.rm=TRUE)/length(except.extent.2[!is.na(except.extent.2)]))
except.warrant.2 <- ifelse((warrant.truth > except.warrant.coef - crit.value*except.warrant.se & warrant.truth < except.warrant.coef +crit.value*except.warrant.se), 1,0)
except.dif.warrant <- 100*(sum(except.warrant.2, na.rm=TRUE)/length(except.warrant.2[!is.na(except.warrant.2)]))
except.incident.2 <- ifelse((incident.truth > except.incident.coef - crit.value*except.incident.se & incident.truth < except.incident.coef +crit.value*except.incident.se), 1,0)
except.dif.incident <- 100*(sum(except.incident.2, na.rm=TRUE)/length(except.incident.2[!is.na(except.incident.2)]))
except.afterlaw.2 <- ifelse((afterlaw.truth > except.afterlaw.coef - crit.value*except.afterlaw.se & afterlaw.truth < except.afterlaw.coef +crit.value*except.afterlaw.se), 1,0)
except.dif.afterlaw <- 100*(sum(except.afterlaw.2, na.rm=TRUE)/length(except.afterlaw.2[!is.na(except.afterlaw.2)]))
except.unlawful.2 <- ifelse((unlawful.truth > except.unlawful.coef - crit.value*except.unlawful.se & unlawful.truth < except.unlawful.coef +crit.value*except.unlawful.se), 1,0)
except.dif.unlawful <- 100*(sum(except.unlawful.2, na.rm=TRUE)/length(except.unlawful.2[!is.na(except.unlawful.2)]))
except.probcaus.2 <- ifelse((probcaus.truth > except.probcaus.coef - crit.value*except.probcaus.se & probcaus.truth < except.probcaus.coef +crit.value*except.probcaus.se), 1,0)
except.dif.probcaus <- 100*(sum(except.probcaus.2, na.rm=TRUE)/length(except.probcaus.2[!is.na(except.probcaus.2)]))
except.except.2 <- ifelse((except.truth > except.except.coef - crit.value*except.except.se & except.truth < except.except.coef +crit.value*except.except.se), 1,0)
except.dif.except <- 100*(sum(except.except.2, na.rm=TRUE)/length(except.except.2[!is.na(except.except.2)]))
except.change.2 <- ifelse((change.truth > except.change.coef - crit.value*except.change.se & change.truth < except.change.coef +crit.value*except.change.se), 1,0)
except.dif.change <- 100*(sum(except.change.2, na.rm=TRUE)/length(except.change.2[!is.na(except.change.2)]))

#NOTES
#for difference, reverse so that 0 = good (never rejecting the truth)
#also, vector orders runs in reverse order from that in which the strategies are presented in each graph
    #(i.e. cons., non-expcepted home is the 1st element and all cases is the last element

#HOME 

home.sig.vec <- c(except.sig.home, anomalous.con.sig.home,  anomalous.lib.sig.home,
    anomalous.all.sig.home, close.con.sig.home,close.lib.sig.home,  
    close.all.sig.home, random.sig.home, all.sig.home)
home.dif.vec <- 100 -c(except.dif.home, anomalous.con.dif.home,  anomalous.lib.dif.home,
    anomalous.all.dif.home, close.con.dif.home,close.lib.dif.home,  
    close.all.dif.home, random.dif.home, all.dif.home)

#business 

business.sig.vec <- c(except.sig.business, anomalous.con.sig.business,  anomalous.lib.sig.business,
    anomalous.all.sig.business, close.con.sig.business,close.lib.sig.business,  
    close.all.sig.business, random.sig.business, all.sig.business)
business.dif.vec <- 100 -c(except.dif.business, anomalous.con.dif.business,  anomalous.lib.dif.business,
    anomalous.all.dif.business, close.con.dif.business,close.lib.dif.business,  
    close.all.dif.business, random.dif.business, all.dif.business)

#person 

person.sig.vec <- c(except.sig.person, anomalous.con.sig.person,  anomalous.lib.sig.person,
    anomalous.all.sig.person, close.con.sig.person,close.lib.sig.person,  
    close.all.sig.person, random.sig.person, all.sig.person)
person.dif.vec <- 100 -c(except.dif.person, anomalous.con.dif.person,  anomalous.lib.dif.person,
    anomalous.all.dif.person, close.con.dif.person,close.lib.dif.person,  
    close.all.dif.person, random.dif.person, all.dif.person)

#car 

car.sig.vec <- c(except.sig.car, anomalous.con.sig.car,  anomalous.lib.sig.car,
    anomalous.all.sig.car, close.con.sig.car,close.lib.sig.car,  
    close.all.sig.car, random.sig.car, all.sig.car)
car.dif.vec <- 100 -c(except.dif.car, anomalous.con.dif.car,  anomalous.lib.dif.car,
    anomalous.all.dif.car, close.con.dif.car,close.lib.dif.car,  
    close.all.dif.car, random.dif.car, all.dif.car)

#extent 

extent.sig.vec <- c(except.sig.extent, anomalous.con.sig.extent,  anomalous.lib.sig.extent,
    anomalous.all.sig.extent, close.con.sig.extent,close.lib.sig.extent,  
    close.all.sig.extent, random.sig.extent, all.sig.extent)
extent.dif.vec <- 100 -c(except.dif.extent, anomalous.con.dif.extent,  anomalous.lib.dif.extent,
    anomalous.all.dif.extent, close.con.dif.extent,close.lib.dif.extent,  
    close.all.dif.extent, random.dif.extent, all.dif.extent)

#warrant 

warrant.sig.vec <- c(except.sig.warrant, anomalous.con.sig.warrant,  anomalous.lib.sig.warrant,
    anomalous.all.sig.warrant, close.con.sig.warrant,close.lib.sig.warrant,  
    close.all.sig.warrant, random.sig.warrant, all.sig.warrant)
warrant.dif.vec <- 100 -c(except.dif.warrant, anomalous.con.dif.warrant,  anomalous.lib.dif.warrant,
    anomalous.all.dif.warrant, close.con.dif.warrant,close.lib.dif.warrant,  
    close.all.dif.warrant, random.dif.warrant, all.dif.warrant)

#incident 

incident.sig.vec <- c(except.sig.incident, anomalous.con.sig.incident,  anomalous.lib.sig.incident,
    anomalous.all.sig.incident, close.con.sig.incident,close.lib.sig.incident,  
    close.all.sig.incident, random.sig.incident, all.sig.incident)
incident.dif.vec <- 100 -c(except.dif.incident, anomalous.con.dif.incident,  anomalous.lib.dif.incident,
    anomalous.all.dif.incident, close.con.dif.incident,close.lib.dif.incident,  
    close.all.dif.incident, random.dif.incident, all.dif.incident)

#afterlaw 

afterlaw.sig.vec <- c(except.sig.afterlaw, anomalous.con.sig.afterlaw,  anomalous.lib.sig.afterlaw,
    anomalous.all.sig.afterlaw, close.con.sig.afterlaw,close.lib.sig.afterlaw,  
    close.all.sig.afterlaw, random.sig.afterlaw, all.sig.afterlaw)
afterlaw.dif.vec <- 100 -c(except.dif.afterlaw, anomalous.con.dif.afterlaw,  anomalous.lib.dif.afterlaw,
    anomalous.all.dif.afterlaw, close.con.dif.afterlaw,close.lib.dif.afterlaw,  
    close.all.dif.afterlaw, random.dif.afterlaw, all.dif.afterlaw)

#unlawful 

unlawful.sig.vec <- c(except.sig.unlawful, anomalous.con.sig.unlawful,  anomalous.lib.sig.unlawful,
    anomalous.all.sig.unlawful, close.con.sig.unlawful,close.lib.sig.unlawful,  
    close.all.sig.unlawful, random.sig.unlawful, all.sig.unlawful)
unlawful.dif.vec <- 100 -c(except.dif.unlawful, anomalous.con.dif.unlawful,  anomalous.lib.dif.unlawful,
    anomalous.all.dif.unlawful, close.con.dif.unlawful,close.lib.dif.unlawful,  
    close.all.dif.unlawful, random.dif.unlawful, all.dif.unlawful)

#probcaus 

probcaus.sig.vec <- c(except.sig.probcaus, anomalous.con.sig.probcaus,  anomalous.lib.sig.probcaus,
    anomalous.all.sig.probcaus, close.con.sig.probcaus,close.lib.sig.probcaus,  
    close.all.sig.probcaus, random.sig.probcaus, all.sig.probcaus)
probcaus.dif.vec <- 100 -100 - c(except.dif.probcaus, anomalous.con.dif.probcaus,  anomalous.lib.dif.probcaus,
    anomalous.all.dif.probcaus, close.con.dif.probcaus,close.lib.dif.probcaus,  
    close.all.dif.probcaus, random.dif.probcaus, all.dif.probcaus)

#except 

except.sig.vec <- c(except.sig.except, anomalous.con.sig.except,  anomalous.lib.sig.except,
    anomalous.all.sig.except, close.con.sig.except,close.lib.sig.except,  
    close.all.sig.except, random.sig.except, all.sig.except)
except.dif.vec <- 100 -c(except.dif.except, anomalous.con.dif.except,  anomalous.lib.dif.except,
    anomalous.all.dif.except, close.con.dif.except,close.lib.dif.except,  
    close.all.dif.except, random.dif.except, all.dif.except)

#change 

change.sig.vec <- c(except.sig.change, anomalous.con.sig.change,  anomalous.lib.sig.change,
    anomalous.all.sig.change, close.con.sig.change,close.lib.sig.change,  
    close.all.sig.change, random.sig.change, all.sig.change)
change.dif.vec <- 100 -c(except.dif.change, anomalous.con.dif.change,  anomalous.lib.dif.change,
    anomalous.all.dif.change, close.con.dif.change,close.lib.dif.change,  
    close.all.dif.change, random.dif.change, all.dif.change)


##############################################################################################
##########################
y.names <- c("Conservative, non-excepted home", "Anomalous conservative cases",
     "Anomalous liberal cases", "Anomalous cases", "Close conservative cases", 
        "Close liberal cases", "Close cases", "Random selection", "All cases")  #vector for ylabels

#SIGNIFICANCE
#use line plot

#only use home business person extent warrant incident afterlaw except change

pdf("fact_sig_final.pdf", width=7, height=9)
#pdf("fact_sig_final_for_slides.pdf", width=7, height=7)

left.panel.width <- 6.5
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)), 
    widths = rbind(c(left.panel.width,3,3,
    left.panel.width,3,3,left.panel.width,3,3)), respect = F)
#layout.show(9)
y.axis <- 1:9
cex.point <- .8
yaxis.label.size <- 1.1
xaxis.label.size <- 1
plot.type <- "n"
bottom.margin <- 4
left.margin <- 1 #for non-left hand panels only
left.left.margin <- 16.1 #only for only left panels
top.margin <- 2
right.margin <- 1.1

#HOME
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))
plot(home.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "", axes =F, xaxs="i")
segments(-1, y.axis, home.sig.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = y.names, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Home", 3, line = .3, cex = 1, font=2)
box()

#business
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(business.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, business.sig.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Business", 3, line = .3, cex = 1, font=2)
box()

#person
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(person.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, person.sig.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Person", 3, line = .3, cex = 1, font=2)
box()

#EXTENT
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))#start 2nd row
plot(extent.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, extent.sig.vec, y.axis, lwd=2) 
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = y.names, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Extent of Search", 3, line = .3, cex = 1, font=2)
box()

#warrant
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(warrant.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, warrant.sig.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Warrant", 3, line = .3, cex = 1, font=2)
box()

#incident
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(incident.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, incident.sig.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Incident\nto Arrest", 3, line = .3, cex = 1, font=2)
box()

#AFTERLAW
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))#start 3rd row
plot(afterlaw.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, afterlaw.sig.vec, y.axis, lwd=2) 
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = y.names, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("After\nLawful Arrest", 3, line = .3, cex = 1, font=2)
box()

#Exception
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(except.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, except.sig.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Exception to\nWarrant Requirement", 3, line = .3, cex = 1, font=2)
box()

#change
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(change.sig.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, change.sig.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Membership\nChange", 3, line = .3, cex = 1, font=2)
box()

dev.off()
####################################################################################
########################################################
########################################

#DIFFERENCE FROM TRUTH

#use line plot

#only use home business person extent warrant incident afterlaw except change
pdf("fact_dif_final.pdf", width=7, height=9)
#pdf("fact_dif_final_for_slides.pdf", width=7, height=7)
left.panel.width <- 6.5
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)), 
    widths = rbind(c(left.panel.width,3,3,
    left.panel.width,3,3,left.panel.width,3,3)), respect = F)
#layout.show(9)
y.axis <- 1:9
cex.point <- .8
yaxis.label.size <- 1.1
xaxis.label.size <- 1
plot.type <- "n"
bottom.margin <- 4
left.margin <- 1 #for non-left hand panels only
left.left.margin <- 16.1 #only for only left panels
top.margin <- 2
right.margin <- 1.1

#HOME
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))
plot(home.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "", axes =F, xaxs="i")
segments(-1, y.axis, home.dif.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = y.names, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Home", 3, line = .3, cex = 1, font=2)
box()

#business
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(business.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, business.dif.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Business", 3, line = .3, cex = 1, font=2)
box()

#person
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(person.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, person.dif.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Person", 3, line = .3, cex = 1, font=2)
box()

#EXTENT
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))#start 2nd row
plot(extent.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, extent.dif.vec, y.axis, lwd=2) 
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = y.names, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Extent of Search", 3, line = .3, cex = 1, font=2)
box()

#warrant
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(warrant.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, warrant.dif.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Warrant", 3, line = .3, cex = 1, font=2)
box()

#incident
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(incident.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, incident.dif.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Incident\nto Arrest", 3, line = .3, cex = 1, font=2)
box()

#AFTERLAW
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))#start 3rd row
plot(afterlaw.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, afterlaw.dif.vec, y.axis, lwd=2) 
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = y.names, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("After\nLawful Arrest", 3, line = .3, cex = 1, font=2)
box()

#Exception
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(except.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, except.dif.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Exception to\nWarrant Requirement", 3, line = .3, cex = 1, font=2)
box()

#change
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(change.dif.vec, y.axis, main = "",  type=plot.type, pch = 19, cex=cex.point,
    xlim = c(0,100), ylim = c(1,9),  xlab = "", ylab = "",axes =F, xaxs="i")
segments(-1, y.axis, change.dif.vec, y.axis, lwd=2)    
#abline(h=y.axis, lty =2, col ="gray")
axis(1, at=c(0,25,50,75,100), c(0,25,50,75,"100%"), cex.axis = xaxis.label.size, las = 1, tick = T, line = 0,  mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = T, line = 0, las = 1, 
    cex.axis = yaxis.label.size, mgp = c(3,.5,0))
mtext("Membership\nChange", 3, line = .3, cex = 1, font=2)
box()

dev.off()
####################################################################################
########################################################
########################################

###SET UP DISTRIBUTION GRAPHS

#ALL CASES
all.home.median.coef <- median(all.home.coef, na.rm = T)
all.home.median.se <- median(all.home.se, na.rm = T)
all.home.25 <- quantile(all.home.coef,.25, na.rm = T)
all.home.75 <- quantile(all.home.coef,.74999, na.rm = T)
all.business.median.coef <- median(all.business.coef, na.rm = T)
all.business.median.se <- median(all.business.se, na.rm = T)
all.business.25 <- quantile(all.business.coef,.25, na.rm = T)
all.business.75 <- quantile(all.business.coef,.74999, na.rm = T)
all.person.median.coef <- median(all.person.coef, na.rm = T)
all.person.median.se <- median(all.person.se, na.rm = T)
all.person.25 <- quantile(all.person.coef,.25, na.rm = T)
all.person.75 <- quantile(all.person.coef,.74999, na.rm = T)
all.car.median.coef <- median(all.car.coef, na.rm = T)
all.car.median.se <- median(all.car.se, na.rm = T)
all.car.25 <- quantile(all.car.coef,.25, na.rm = T)
all.car.75 <- quantile(all.car.coef,.74999, na.rm = T)
all.extent.median.coef <- median(all.extent.coef, na.rm = T)
all.extent.median.se <- median(all.extent.se, na.rm = T)
all.extent.25 <- quantile(all.extent.coef,.25, na.rm = T)
all.extent.75 <- quantile(all.extent.coef,.74999, na.rm = T)
all.warrant.median.coef <- median(all.warrant.coef, na.rm = T)
all.warrant.median.se <- median(all.warrant.se, na.rm = T)
all.warrant.25 <- quantile(all.warrant.coef,.25, na.rm = T)
all.warrant.75 <- quantile(all.warrant.coef,.74999, na.rm = T)
all.incident.median.coef <- median(all.incident.coef, na.rm = T)
all.incident.median.se <- median(all.incident.se, na.rm = T)
all.incident.25 <- quantile(all.incident.coef,.25, na.rm = T)
all.incident.75 <- quantile(all.incident.coef,.74999, na.rm = T)
all.afterlaw.median.coef <- median(all.afterlaw.coef, na.rm = T)
all.afterlaw.median.se <- median(all.afterlaw.se, na.rm = T)
all.afterlaw.25 <- quantile(all.afterlaw.coef,.25, na.rm = T)
all.afterlaw.75 <- quantile(all.afterlaw.coef,.74999, na.rm = T)
all.unlawful.median.coef <- median(all.unlawful.coef, na.rm = T)
all.unlawful.median.se <- median(all.unlawful.se, na.rm = T)
all.unlawful.25 <- quantile(all.unlawful.coef,.25, na.rm = T)
all.unlawful.75 <- quantile(all.unlawful.coef,.74999, na.rm = T)
all.probcaus.median.coef <- median(all.probcaus.coef, na.rm = T)
all.probcaus.median.se <- median(all.probcaus.se, na.rm = T)
all.probcaus.25 <- quantile(all.probcaus.coef,.25, na.rm = T)
all.probcaus.75 <- quantile(all.probcaus.coef,.74999, na.rm = T)
all.except.median.coef <- median(all.except.coef, na.rm = T)
all.except.median.se <- median(all.except.se, na.rm = T)
all.except.25 <- quantile(all.except.coef,.25, na.rm = T)
all.except.75 <- quantile(all.except.coef,.74999, na.rm = T)
all.change.median.coef <- median(all.change.coef, na.rm = T)
all.change.median.se <- median(all.change.se, na.rm = T)
all.change.25 <- quantile(all.change.coef,.25, na.rm = T)
all.change.75 <- quantile(all.change.coef,.74999, na.rm = T)


#2) random

random.home.median.coef <- median(random.home.coef, na.rm = T)
random.home.median.se <- median(random.home.se, na.rm = T)
random.home.25 <- quantile(random.home.coef,.25, na.rm = T)
random.home.75 <- quantile(random.home.coef,.74999, na.rm = T)
random.business.median.coef <- median(random.business.coef, na.rm = T)
random.business.median.se <- median(random.business.se, na.rm = T)
random.business.25 <- quantile(random.business.coef,.25, na.rm = T)
random.business.75 <- quantile(random.business.coef,.74999, na.rm = T)
random.person.median.coef <- median(random.person.coef, na.rm = T)
random.person.median.se <- median(random.person.se, na.rm = T)
random.person.25 <- quantile(random.person.coef,.25, na.rm = T)
random.person.75 <- quantile(random.person.coef,.74999, na.rm = T)
random.car.median.coef <- median(random.car.coef, na.rm = T)
random.car.median.se <- median(random.car.se, na.rm = T)
random.car.25 <- quantile(random.car.coef,.25, na.rm = T)
random.car.75 <- quantile(random.car.coef,.74999, na.rm = T)
random.extent.median.coef <- median(random.extent.coef, na.rm = T)
random.extent.median.se <- median(random.extent.se, na.rm = T)
random.extent.25 <- quantile(random.extent.coef,.25, na.rm = T)
random.extent.75 <- quantile(random.extent.coef,.74999, na.rm = T)
random.warrant.median.coef <- median(random.warrant.coef, na.rm = T)
random.warrant.median.se <- median(random.warrant.se, na.rm = T)
random.warrant.25 <- quantile(random.warrant.coef,.25, na.rm = T)
random.warrant.75 <- quantile(random.warrant.coef,.74999, na.rm = T)
random.incident.median.coef <- median(random.incident.coef, na.rm = T)
random.incident.median.se <- median(random.incident.se, na.rm = T)
random.incident.25 <- quantile(random.incident.coef,.25, na.rm = T)
random.incident.75 <- quantile(random.incident.coef,.74999, na.rm = T)
random.afterlaw.median.coef <- median(random.afterlaw.coef, na.rm = T)
random.afterlaw.median.se <- median(random.afterlaw.se, na.rm = T)
random.afterlaw.25 <- quantile(random.afterlaw.coef,.25, na.rm = T)
random.afterlaw.75 <- quantile(random.afterlaw.coef,.74999, na.rm = T)
random.unlawful.median.coef <- median(random.unlawful.coef, na.rm = T)
random.unlawful.median.se <- median(random.unlawful.se, na.rm = T)
random.unlawful.25 <- quantile(random.unlawful.coef,.25, na.rm = T)
random.unlawful.75 <- quantile(random.unlawful.coef,.74999, na.rm = T)
random.probcaus.median.coef <- median(random.probcaus.coef, na.rm = T)
random.probcaus.median.se <- median(random.probcaus.se, na.rm = T)
random.probcaus.25 <- quantile(random.probcaus.coef,.25, na.rm = T)
random.probcaus.75 <- quantile(random.probcaus.coef,.74999, na.rm = T)
random.except.median.coef <- median(random.except.coef, na.rm = T)
random.except.median.se <- median(random.except.se, na.rm = T)
random.except.25 <- quantile(random.except.coef,.25, na.rm = T)
random.except.75 <- quantile(random.except.coef,.74999, na.rm = T)
random.change.median.coef <- median(random.change.coef, na.rm = T)
random.change.median.se <- median(random.change.se, na.rm = T)
random.change.25 <- quantile(random.change.coef,.25, na.rm = T)
random.change.75 <- quantile(random.change.coef,.74999, na.rm = T)

#3) near, all

close.all.home.median.coef <- median(close.all.home.coef, na.rm = T)
close.all.home.median.se <- median(close.all.home.se, na.rm = T)
close.all.home.25 <- quantile(close.all.home.coef,.25, na.rm = T)
close.all.home.75 <- quantile(close.all.home.coef,.74999, na.rm = T)
close.all.business.median.coef <- median(close.all.business.coef, na.rm = T)
close.all.business.median.se <- median(close.all.business.se, na.rm = T)
close.all.business.25 <- quantile(close.all.business.coef,.25, na.rm = T)
close.all.business.75 <- quantile(close.all.business.coef,.74999, na.rm = T)
close.all.person.median.coef <- median(close.all.person.coef, na.rm = T)
close.all.person.median.se <- median(close.all.person.se, na.rm = T)
close.all.person.25 <- quantile(close.all.person.coef,.25, na.rm = T)
close.all.person.75 <- quantile(close.all.person.coef,.74999, na.rm = T)
close.all.car.median.coef <- median(close.all.car.coef, na.rm = T)
close.all.car.median.se <- median(close.all.car.se, na.rm = T)
close.all.car.25 <- quantile(close.all.car.coef,.25, na.rm = T)
close.all.car.75 <- quantile(close.all.car.coef,.74999, na.rm = T)
close.all.extent.median.coef <- median(close.all.extent.coef, na.rm = T)
close.all.extent.median.se <- median(close.all.extent.se, na.rm = T)
close.all.extent.25 <- quantile(close.all.extent.coef,.25, na.rm = T)
close.all.extent.75 <- quantile(close.all.extent.coef,.74999, na.rm = T)
close.all.warrant.median.coef <- median(close.all.warrant.coef, na.rm = T)
close.all.warrant.median.se <- median(close.all.warrant.se, na.rm = T)
close.all.warrant.25 <- quantile(close.all.warrant.coef,.25, na.rm = T)
close.all.warrant.75 <- quantile(close.all.warrant.coef,.74999, na.rm = T)
close.all.incident.median.coef <- median(close.all.incident.coef, na.rm = T)
close.all.incident.median.se <- median(close.all.incident.se, na.rm = T)
close.all.incident.25 <- quantile(close.all.incident.coef,.25, na.rm = T)
close.all.incident.75 <- quantile(close.all.incident.coef,.74999, na.rm = T)
close.all.afterlaw.median.coef <- median(close.all.afterlaw.coef, na.rm = T)
close.all.afterlaw.median.se <- median(close.all.afterlaw.se, na.rm = T)
close.all.afterlaw.25 <- quantile(close.all.afterlaw.coef,.25, na.rm = T)
close.all.afterlaw.75 <- quantile(close.all.afterlaw.coef,.74999, na.rm = T)
close.all.unlawful.median.coef <- median(close.all.unlawful.coef, na.rm = T)
close.all.unlawful.median.se <- median(close.all.unlawful.se, na.rm = T)
close.all.unlawful.25 <- quantile(close.all.unlawful.coef,.25, na.rm = T)
close.all.unlawful.75 <- quantile(close.all.unlawful.coef,.74999, na.rm = T)
close.all.probcaus.median.coef <- median(close.all.probcaus.coef, na.rm = T)
close.all.probcaus.median.se <- median(close.all.probcaus.se, na.rm = T)
close.all.probcaus.25 <- quantile(close.all.probcaus.coef,.25, na.rm = T)
close.all.probcaus.75 <- quantile(close.all.probcaus.coef,.74999, na.rm = T)
close.all.except.median.coef <- median(close.all.except.coef, na.rm = T)
close.all.except.median.se <- median(close.all.except.se, na.rm = T)
close.all.except.25 <- quantile(close.all.except.coef,.25, na.rm = T)
close.all.except.75 <- quantile(close.all.except.coef,.74999, na.rm = T)
close.all.change.median.coef <- median(close.all.change.coef, na.rm = T)
close.all.change.median.se <- median(close.all.change.se, na.rm = T)
close.all.change.25 <- quantile(close.all.change.coef,.25, na.rm = T)
close.all.change.75 <- quantile(close.all.change.coef,.74999, na.rm = T)     
     
#4) near, wantzero

close.lib.home.median.coef <- median(close.lib.home.coef, na.rm = T)
close.lib.home.median.se <- median(close.lib.home.se, na.rm = T)
close.lib.home.25 <- quantile(close.lib.home.coef,.25, na.rm = T)
close.lib.home.75 <- quantile(close.lib.home.coef,.74999, na.rm = T)
close.lib.business.median.coef <- median(close.lib.business.coef, na.rm = T)
close.lib.business.median.se <- median(close.lib.business.se, na.rm = T)
close.lib.business.25 <- quantile(close.lib.business.coef,.25, na.rm = T)
close.lib.business.75 <- quantile(close.lib.business.coef,.74999, na.rm = T)
close.lib.person.median.coef <- median(close.lib.person.coef, na.rm = T)
close.lib.person.median.se <- median(close.lib.person.se, na.rm = T)
close.lib.person.25 <- quantile(close.lib.person.coef,.25, na.rm = T)
close.lib.person.75 <- quantile(close.lib.person.coef,.74999, na.rm = T)
close.lib.car.median.coef <- median(close.lib.car.coef, na.rm = T)
close.lib.car.median.se <- median(close.lib.car.se, na.rm = T)
close.lib.car.25 <- quantile(close.lib.car.coef,.25, na.rm = T)
close.lib.car.75 <- quantile(close.lib.car.coef,.74999, na.rm = T)
close.lib.extent.median.coef <- median(close.lib.extent.coef, na.rm = T)
close.lib.extent.median.se <- median(close.lib.extent.se, na.rm = T)
close.lib.extent.25 <- quantile(close.lib.extent.coef,.25, na.rm = T)
close.lib.extent.75 <- quantile(close.lib.extent.coef,.74999, na.rm = T)
close.lib.warrant.median.coef <- median(close.lib.warrant.coef, na.rm = T)
close.lib.warrant.median.se <- median(close.lib.warrant.se, na.rm = T)
close.lib.warrant.25 <- quantile(close.lib.warrant.coef,.25, na.rm = T)
close.lib.warrant.75 <- quantile(close.lib.warrant.coef,.74999, na.rm = T)
close.lib.incident.median.coef <- median(close.lib.incident.coef, na.rm = T)
close.lib.incident.median.se <- median(close.lib.incident.se, na.rm = T)
close.lib.incident.25 <- quantile(close.lib.incident.coef,.25, na.rm = T)
close.lib.incident.75 <- quantile(close.lib.incident.coef,.74999, na.rm = T)
close.lib.afterlaw.median.coef <- median(close.lib.afterlaw.coef, na.rm = T)
close.lib.afterlaw.median.se <- median(close.lib.afterlaw.se, na.rm = T)
close.lib.afterlaw.25 <- quantile(close.lib.afterlaw.coef,.25, na.rm = T)
close.lib.afterlaw.75 <- quantile(close.lib.afterlaw.coef,.74999, na.rm = T)
close.lib.unlawful.median.coef <- median(close.lib.unlawful.coef, na.rm = T)
close.lib.unlawful.median.se <- median(close.lib.unlawful.se, na.rm = T)
close.lib.unlawful.25 <- quantile(close.lib.unlawful.coef,.25, na.rm = T)
close.lib.unlawful.75 <- quantile(close.lib.unlawful.coef,.74999, na.rm = T)
close.lib.probcaus.median.coef <- median(close.lib.probcaus.coef, na.rm = T)
close.lib.probcaus.median.se <- median(close.lib.probcaus.se, na.rm = T)
close.lib.probcaus.25 <- quantile(close.lib.probcaus.coef,.25, na.rm = T)
close.lib.probcaus.75 <- quantile(close.lib.probcaus.coef,.74999, na.rm = T)
close.lib.except.median.coef <- median(close.lib.except.coef, na.rm = T)
close.lib.except.median.se <- median(close.lib.except.se, na.rm = T)
close.lib.except.25 <- quantile(close.lib.except.coef,.25, na.rm = T)
close.lib.except.75 <- quantile(close.lib.except.coef,.74999, na.rm = T)
close.lib.change.median.coef <- median(close.lib.change.coef, na.rm = T)
close.lib.change.median.se <- median(close.lib.change.se, na.rm = T)
close.lib.change.25 <- quantile(close.lib.change.coef,.25, na.rm = T)
close.lib.change.75 <- quantile(close.lib.change.coef,.74999, na.rm = T)

#5) near, wantone

close.con.home.median.coef <- median(close.con.home.coef, na.rm = T)
close.con.home.median.se <- median(close.con.home.se, na.rm = T)
close.con.home.25 <- quantile(close.con.home.coef,.25, na.rm = T)
close.con.home.75 <- quantile(close.con.home.coef,.74999, na.rm = T)
close.con.business.median.coef <- median(close.con.business.coef, na.rm = T)
close.con.business.median.se <- median(close.con.business.se, na.rm = T)
close.con.business.25 <- quantile(close.con.business.coef,.25, na.rm = T)
close.con.business.75 <- quantile(close.con.business.coef,.74999, na.rm = T)
close.con.person.median.coef <- median(close.con.person.coef, na.rm = T)
close.con.person.median.se <- median(close.con.person.se, na.rm = T)
close.con.person.25 <- quantile(close.con.person.coef,.25, na.rm = T)
close.con.person.75 <- quantile(close.con.person.coef,.74999, na.rm = T)
close.con.car.median.coef <- median(close.con.car.coef, na.rm = T)
close.con.car.median.se <- median(close.con.car.se, na.rm = T)
close.con.car.25 <- quantile(close.con.car.coef,.25, na.rm = T)
close.con.car.75 <- quantile(close.con.car.coef,.74999, na.rm = T)
close.con.extent.median.coef <- median(close.con.extent.coef, na.rm = T)
close.con.extent.median.se <- median(close.con.extent.se, na.rm = T)
close.con.extent.25 <- quantile(close.con.extent.coef,.25, na.rm = T)
close.con.extent.75 <- quantile(close.con.extent.coef,.74999, na.rm = T)
close.con.warrant.median.coef <- median(close.con.warrant.coef, na.rm = T)
close.con.warrant.median.se <- median(close.con.warrant.se, na.rm = T)
close.con.warrant.25 <- quantile(close.con.warrant.coef,.25, na.rm = T)
close.con.warrant.75 <- quantile(close.con.warrant.coef,.74999, na.rm = T)
close.con.incident.median.coef <- median(close.con.incident.coef, na.rm = T)
close.con.incident.median.se <- median(close.con.incident.se, na.rm = T)
close.con.incident.25 <- quantile(close.con.incident.coef,.25, na.rm = T)
close.con.incident.75 <- quantile(close.con.incident.coef,.74999, na.rm = T)
close.con.afterlaw.median.coef <- median(close.con.afterlaw.coef, na.rm = T)
close.con.afterlaw.median.se <- median(close.con.afterlaw.se, na.rm = T)
close.con.afterlaw.25 <- quantile(close.con.afterlaw.coef,.25, na.rm = T)
close.con.afterlaw.75 <- quantile(close.con.afterlaw.coef,.74999, na.rm = T)
close.con.unlawful.median.coef <- median(close.con.unlawful.coef, na.rm = T)
close.con.unlawful.median.se <- median(close.con.unlawful.se, na.rm = T)
close.con.unlawful.25 <- quantile(close.con.unlawful.coef,.25, na.rm = T)
close.con.unlawful.75 <- quantile(close.con.unlawful.coef,.74999, na.rm = T)
close.con.probcaus.median.coef <- median(close.con.probcaus.coef, na.rm = T)
close.con.probcaus.median.se <- median(close.con.probcaus.se, na.rm = T)
close.con.probcaus.25 <- quantile(close.con.probcaus.coef,.25, na.rm = T)
close.con.probcaus.75 <- quantile(close.con.probcaus.coef,.74999, na.rm = T)
close.con.except.median.coef <- median(close.con.except.coef, na.rm = T)
close.con.except.median.se <- median(close.con.except.se, na.rm = T)
close.con.except.25 <- quantile(close.con.except.coef,.25, na.rm = T)
close.con.except.75 <- quantile(close.con.except.coef,.74999, na.rm = T)
close.con.change.median.coef <- median(close.con.change.coef, na.rm = T)
close.con.change.median.se <- median(close.con.change.se, na.rm = T)
close.con.change.25 <- quantile(close.con.change.coef,.25, na.rm = T)
close.con.change.75 <- quantile(close.con.change.coef,.74999, na.rm = T)


#6) unusual, all

anomalous.all.home.median.coef <- median(anomalous.all.home.coef, na.rm = T)
anomalous.all.home.median.se <- median(anomalous.all.home.se, na.rm = T)
anomalous.all.home.25 <- quantile(anomalous.all.home.coef,.25, na.rm = T)
anomalous.all.home.75 <- quantile(anomalous.all.home.coef,.74999, na.rm = T)
anomalous.all.business.median.coef <- median(anomalous.all.business.coef, na.rm = T)
anomalous.all.business.median.se <- median(anomalous.all.business.se, na.rm = T)
anomalous.all.business.25 <- quantile(anomalous.all.business.coef,.25, na.rm = T)
anomalous.all.business.75 <- quantile(anomalous.all.business.coef,.74999, na.rm = T)
anomalous.all.person.median.coef <- median(anomalous.all.person.coef, na.rm = T)
anomalous.all.person.median.se <- median(anomalous.all.person.se, na.rm = T)
anomalous.all.person.25 <- quantile(anomalous.all.person.coef,.25, na.rm = T)
anomalous.all.person.75 <- quantile(anomalous.all.person.coef,.74999, na.rm = T)
anomalous.all.car.median.coef <- median(anomalous.all.car.coef, na.rm = T)
anomalous.all.car.median.se <- median(anomalous.all.car.se, na.rm = T)
anomalous.all.car.25 <- quantile(anomalous.all.car.coef,.25, na.rm = T)
anomalous.all.car.75 <- quantile(anomalous.all.car.coef,.74999, na.rm = T)
anomalous.all.extent.median.coef <- median(anomalous.all.extent.coef, na.rm = T)
anomalous.all.extent.median.se <- median(anomalous.all.extent.se, na.rm = T)
anomalous.all.extent.25 <- quantile(anomalous.all.extent.coef,.25, na.rm = T)
anomalous.all.extent.75 <- quantile(anomalous.all.extent.coef,.74999, na.rm = T)
anomalous.all.warrant.median.coef <- median(anomalous.all.warrant.coef, na.rm = T)
anomalous.all.warrant.median.se <- median(anomalous.all.warrant.se, na.rm = T)
anomalous.all.warrant.25 <- quantile(anomalous.all.warrant.coef,.25, na.rm = T)
anomalous.all.warrant.75 <- quantile(anomalous.all.warrant.coef,.74999, na.rm = T)
anomalous.all.incident.median.coef <- median(anomalous.all.incident.coef, na.rm = T)
anomalous.all.incident.median.se <- median(anomalous.all.incident.se, na.rm = T)
anomalous.all.incident.25 <- quantile(anomalous.all.incident.coef,.25, na.rm = T)
anomalous.all.incident.75 <- quantile(anomalous.all.incident.coef,.74999, na.rm = T)
anomalous.all.afterlaw.median.coef <- median(anomalous.all.afterlaw.coef, na.rm = T)
anomalous.all.afterlaw.median.se <- median(anomalous.all.afterlaw.se, na.rm = T)
anomalous.all.afterlaw.25 <- quantile(anomalous.all.afterlaw.coef,.25, na.rm = T)
anomalous.all.afterlaw.75 <- quantile(anomalous.all.afterlaw.coef,.74999, na.rm = T)
anomalous.all.unlawful.median.coef <- median(anomalous.all.unlawful.coef, na.rm = T)
anomalous.all.unlawful.median.se <- median(anomalous.all.unlawful.se, na.rm = T)
anomalous.all.unlawful.25 <- quantile(anomalous.all.unlawful.coef,.25, na.rm = T)
anomalous.all.unlawful.75 <- quantile(anomalous.all.unlawful.coef,.74999, na.rm = T)
anomalous.all.probcaus.median.coef <- median(anomalous.all.probcaus.coef, na.rm = T)
anomalous.all.probcaus.median.se <- median(anomalous.all.probcaus.se, na.rm = T)
anomalous.all.probcaus.25 <- quantile(anomalous.all.probcaus.coef,.25, na.rm = T)
anomalous.all.probcaus.75 <- quantile(anomalous.all.probcaus.coef,.74999, na.rm = T)
anomalous.all.except.median.coef <- median(anomalous.all.except.coef, na.rm = T)
anomalous.all.except.median.se <- median(anomalous.all.except.se, na.rm = T)
anomalous.all.except.25 <- quantile(anomalous.all.except.coef,.25, na.rm = T)
anomalous.all.except.75 <- quantile(anomalous.all.except.coef,.74999, na.rm = T)
anomalous.all.change.median.coef <- median(anomalous.all.change.coef, na.rm = T)
anomalous.all.change.median.se <- median(anomalous.all.change.se, na.rm = T)
anomalous.all.change.25 <- quantile(anomalous.all.change.coef,.25, na.rm = T)
anomalous.all.change.75 <- quantile(anomalous.all.change.coef,.74999, na.rm = T)


#7) unusual, want zero

anomalous.lib.home.median.coef <- median(anomalous.lib.home.coef, na.rm = T)
anomalous.lib.home.median.se <- median(anomalous.lib.home.se, na.rm = T)
anomalous.lib.home.25 <- quantile(anomalous.lib.home.coef,.25, na.rm = T)
anomalous.lib.home.75 <- quantile(anomalous.lib.home.coef,.74999, na.rm = T)
anomalous.lib.business.median.coef <- median(anomalous.lib.business.coef, na.rm = T)
anomalous.lib.business.median.se <- median(anomalous.lib.business.se, na.rm = T)
anomalous.lib.business.25 <- quantile(anomalous.lib.business.coef,.25, na.rm = T)
anomalous.lib.business.75 <- quantile(anomalous.lib.business.coef,.74999, na.rm = T)
anomalous.lib.person.median.coef <- median(anomalous.lib.person.coef, na.rm = T)
anomalous.lib.person.median.se <- median(anomalous.lib.person.se, na.rm = T)
anomalous.lib.person.25 <- quantile(anomalous.lib.person.coef,.25, na.rm = T)
anomalous.lib.person.75 <- quantile(anomalous.lib.person.coef,.74999, na.rm = T)
anomalous.lib.car.median.coef <- median(anomalous.lib.car.coef, na.rm = T)
anomalous.lib.car.median.se <- median(anomalous.lib.car.se, na.rm = T)
anomalous.lib.car.25 <- quantile(anomalous.lib.car.coef,.25, na.rm = T)
anomalous.lib.car.75 <- quantile(anomalous.lib.car.coef,.74999, na.rm = T)
anomalous.lib.extent.median.coef <- median(anomalous.lib.extent.coef, na.rm = T)
anomalous.lib.extent.median.se <- median(anomalous.lib.extent.se, na.rm = T)
anomalous.lib.extent.25 <- quantile(anomalous.lib.extent.coef,.25, na.rm = T)
anomalous.lib.extent.75 <- quantile(anomalous.lib.extent.coef,.74999, na.rm = T)
anomalous.lib.warrant.median.coef <- median(anomalous.lib.warrant.coef, na.rm = T)
anomalous.lib.warrant.median.se <- median(anomalous.lib.warrant.se, na.rm = T)
anomalous.lib.warrant.25 <- quantile(anomalous.lib.warrant.coef,.25, na.rm = T)
anomalous.lib.warrant.75 <- quantile(anomalous.lib.warrant.coef,.74999, na.rm = T)
anomalous.lib.incident.median.coef <- median(anomalous.lib.incident.coef, na.rm = T)
anomalous.lib.incident.median.se <- median(anomalous.lib.incident.se, na.rm = T)
anomalous.lib.incident.25 <- quantile(anomalous.lib.incident.coef,.25, na.rm = T)
anomalous.lib.incident.75 <- quantile(anomalous.lib.incident.coef,.74999, na.rm = T)
anomalous.lib.afterlaw.median.coef <- median(anomalous.lib.afterlaw.coef, na.rm = T)
anomalous.lib.afterlaw.median.se <- median(anomalous.lib.afterlaw.se, na.rm = T)
anomalous.lib.afterlaw.25 <- quantile(anomalous.lib.afterlaw.coef,.25, na.rm = T)
anomalous.lib.afterlaw.75 <- quantile(anomalous.lib.afterlaw.coef,.74999, na.rm = T)
anomalous.lib.unlawful.median.coef <- median(anomalous.lib.unlawful.coef, na.rm = T)
anomalous.lib.unlawful.median.se <- median(anomalous.lib.unlawful.se, na.rm = T)
anomalous.lib.unlawful.25 <- quantile(anomalous.lib.unlawful.coef,.25, na.rm = T)
anomalous.lib.unlawful.75 <- quantile(anomalous.lib.unlawful.coef,.74999, na.rm = T)
anomalous.lib.probcaus.median.coef <- median(anomalous.lib.probcaus.coef, na.rm = T)
anomalous.lib.probcaus.median.se <- median(anomalous.lib.probcaus.se, na.rm = T)
anomalous.lib.probcaus.25 <- quantile(anomalous.lib.probcaus.coef,.25, na.rm = T)
anomalous.lib.probcaus.75 <- quantile(anomalous.lib.probcaus.coef,.74999, na.rm = T)
anomalous.lib.except.median.coef <- median(anomalous.lib.except.coef, na.rm = T)
anomalous.lib.except.median.se <- median(anomalous.lib.except.se, na.rm = T)
anomalous.lib.except.25 <- quantile(anomalous.lib.except.coef,.25, na.rm = T)
anomalous.lib.except.75 <- quantile(anomalous.lib.except.coef,.74999, na.rm = T)
anomalous.lib.change.median.coef <- median(anomalous.lib.change.coef, na.rm = T)
anomalous.lib.change.median.se <- median(anomalous.lib.change.se, na.rm = T)
anomalous.lib.change.25 <- quantile(anomalous.lib.change.coef,.25, na.rm = T)
anomalous.lib.change.75 <- quantile(anomalous.lib.change.coef,.74999, na.rm = T)

#8) unusual, want one

anomalous.con.home.median.coef <- median(anomalous.con.home.coef, na.rm = T)
anomalous.con.home.median.se <- median(anomalous.con.home.se, na.rm = T)
anomalous.con.home.25 <- quantile(anomalous.con.home.coef,.25, na.rm = T)
anomalous.con.home.75 <- quantile(anomalous.con.home.coef,.74999, na.rm = T)
anomalous.con.business.median.coef <- median(anomalous.con.business.coef, na.rm = T)
anomalous.con.business.median.se <- median(anomalous.con.business.se, na.rm = T)
anomalous.con.business.25 <- quantile(anomalous.con.business.coef,.25, na.rm = T)
anomalous.con.business.75 <- quantile(anomalous.con.business.coef,.74999, na.rm = T)
anomalous.con.person.median.coef <- median(anomalous.con.person.coef, na.rm = T)
anomalous.con.person.median.se <- median(anomalous.con.person.se, na.rm = T)
anomalous.con.person.25 <- quantile(anomalous.con.person.coef,.25, na.rm = T)
anomalous.con.person.75 <- quantile(anomalous.con.person.coef,.74999, na.rm = T)
anomalous.con.car.median.coef <- median(anomalous.con.car.coef, na.rm = T)
anomalous.con.car.median.se <- median(anomalous.con.car.se, na.rm = T)
anomalous.con.car.25 <- quantile(anomalous.con.car.coef,.25, na.rm = T)
anomalous.con.car.75 <- quantile(anomalous.con.car.coef,.74999, na.rm = T)
anomalous.con.extent.median.coef <- median(anomalous.con.extent.coef, na.rm = T)
anomalous.con.extent.median.se <- median(anomalous.con.extent.se, na.rm = T)
anomalous.con.extent.25 <- quantile(anomalous.con.extent.coef,.25, na.rm = T)
anomalous.con.extent.75 <- quantile(anomalous.con.extent.coef,.74999, na.rm = T)
anomalous.con.warrant.median.coef <- median(anomalous.con.warrant.coef, na.rm = T)
anomalous.con.warrant.median.se <- median(anomalous.con.warrant.se, na.rm = T)
anomalous.con.warrant.25 <- quantile(anomalous.con.warrant.coef,.25, na.rm = T)
anomalous.con.warrant.75 <- quantile(anomalous.con.warrant.coef,.74999, na.rm = T)
anomalous.con.incident.median.coef <- median(anomalous.con.incident.coef, na.rm = T)
anomalous.con.incident.median.se <- median(anomalous.con.incident.se, na.rm = T)
anomalous.con.incident.25 <- quantile(anomalous.con.incident.coef,.25, na.rm = T)
anomalous.con.incident.75 <- quantile(anomalous.con.incident.coef,.74999, na.rm = T)
anomalous.con.afterlaw.median.coef <- median(anomalous.con.afterlaw.coef, na.rm = T)
anomalous.con.afterlaw.median.se <- median(anomalous.con.afterlaw.se, na.rm = T)
anomalous.con.afterlaw.25 <- quantile(anomalous.con.afterlaw.coef,.25, na.rm = T)
anomalous.con.afterlaw.75 <- quantile(anomalous.con.afterlaw.coef,.74999, na.rm = T)
anomalous.con.unlawful.median.coef <- median(anomalous.con.unlawful.coef, na.rm = T)
anomalous.con.unlawful.median.se <- median(anomalous.con.unlawful.se, na.rm = T)
anomalous.con.unlawful.25 <- quantile(anomalous.con.unlawful.coef,.25, na.rm = T)
anomalous.con.unlawful.75 <- quantile(anomalous.con.unlawful.coef,.74999, na.rm = T)
anomalous.con.probcaus.median.coef <- median(anomalous.con.probcaus.coef, na.rm = T)
anomalous.con.probcaus.median.se <- median(anomalous.con.probcaus.se, na.rm = T)
anomalous.con.probcaus.25 <- quantile(anomalous.con.probcaus.coef,.25, na.rm = T)
anomalous.con.probcaus.75 <- quantile(anomalous.con.probcaus.coef,.74999, na.rm = T)
anomalous.con.except.median.coef <- median(anomalous.con.except.coef, na.rm = T)
anomalous.con.except.median.se <- median(anomalous.con.except.se, na.rm = T)
anomalous.con.except.25 <- quantile(anomalous.con.except.coef,.25, na.rm = T)
anomalous.con.except.75 <- quantile(anomalous.con.except.coef,.74999, na.rm = T)
anomalous.con.change.median.coef <- median(anomalous.con.change.coef, na.rm = T)
anomalous.con.change.median.se <- median(anomalous.con.change.se, na.rm = T)
anomalous.con.change.25 <- quantile(anomalous.con.change.coef,.25, na.rm = T)
anomalous.con.change.75 <- quantile(anomalous.con.change.coef,.74999, na.rm = T)
          
#9) home, no exceptions

except.home.median.coef <- median(except.home.coef, na.rm = T)
except.home.median.se <- median(except.home.se, na.rm = T)
except.home.25 <- quantile(except.home.coef,.25, na.rm = T)
except.home.75 <- quantile(except.home.coef,.74999, na.rm = T)
except.business.median.coef <- median(except.business.coef, na.rm = T)
except.business.median.se <- median(except.business.se, na.rm = T)
except.business.25 <- quantile(except.business.coef,.25, na.rm = T)
except.business.75 <- quantile(except.business.coef,.74999, na.rm = T)
except.person.median.coef <- median(except.person.coef, na.rm = T)
except.person.median.se <- median(except.person.se, na.rm = T)
except.person.25 <- quantile(except.person.coef,.25, na.rm = T)
except.person.75 <- quantile(except.person.coef,.74999, na.rm = T)
except.car.median.coef <- median(except.car.coef, na.rm = T)
except.car.median.se <- median(except.car.se, na.rm = T)
except.car.25 <- quantile(except.car.coef,.25, na.rm = T)
except.car.75 <- quantile(except.car.coef,.74999, na.rm = T)
except.extent.median.coef <- median(except.extent.coef, na.rm = T)
except.extent.median.se <- median(except.extent.se, na.rm = T)
except.extent.25 <- quantile(except.extent.coef,.25, na.rm = T)
except.extent.75 <- quantile(except.extent.coef,.74999, na.rm = T)
except.warrant.median.coef <- median(except.warrant.coef, na.rm = T)
except.warrant.median.se <- median(except.warrant.se, na.rm = T)
except.warrant.25 <- quantile(except.warrant.coef,.25, na.rm = T)
except.warrant.75 <- quantile(except.warrant.coef,.74999, na.rm = T)
except.incident.median.coef <- median(except.incident.coef, na.rm = T)
except.incident.median.se <- median(except.incident.se, na.rm = T)
except.incident.25 <- quantile(except.incident.coef,.25, na.rm = T)
except.incident.75 <- quantile(except.incident.coef,.74999, na.rm = T)
except.afterlaw.median.coef <- median(except.afterlaw.coef, na.rm = T)
except.afterlaw.median.se <- median(except.afterlaw.se, na.rm = T)
except.afterlaw.25 <- quantile(except.afterlaw.coef,.25, na.rm = T)
except.afterlaw.75 <- quantile(except.afterlaw.coef,.74999, na.rm = T)
except.unlawful.median.coef <- median(except.unlawful.coef, na.rm = T)
except.unlawful.median.se <- median(except.unlawful.se, na.rm = T)
except.unlawful.25 <- quantile(except.unlawful.coef,.25, na.rm = T)
except.unlawful.75 <- quantile(except.unlawful.coef,.74999, na.rm = T)
except.probcaus.median.coef <- median(except.probcaus.coef, na.rm = T)
except.probcaus.median.se <- median(except.probcaus.se, na.rm = T)
except.probcaus.25 <- quantile(except.probcaus.coef,.25, na.rm = T)
except.probcaus.75 <- quantile(except.probcaus.coef,.74999, na.rm = T)
except.except.median.coef <- median(except.except.coef, na.rm = T)
except.except.median.se <- median(except.except.se, na.rm = T)
except.except.25 <- quantile(except.except.coef,.25, na.rm = T)
except.except.75 <- quantile(except.except.coef,.74999, na.rm = T)
except.change.median.coef <- median(except.change.coef, na.rm = T)
except.change.median.se <- median(except.change.se, na.rm = T)
except.change.25 <- quantile(except.change.coef,.25, na.rm = T)
except.change.75 <- quantile(except.change.coef,.74999, na.rm = T)

#CREATE VECTORS FOR DISTRIBUTION PLOT




#we're also going to plot truth on top, so add true values to median plots and NA's to se, 25, 75 vectors
#HOME

home.median <- c(except.home.median.coef, anomalous.con.home.median.coef, anomalous.lib.home.median.coef,
    anomalous.all.home.median.coef, close.con.home.median.coef, close.lib.home.median.coef, 
    close.all.home.median.coef, random.home.median.coef, all.home.median.coef)
home.se <-  c(except.home.median.se, anomalous.con.home.median.se, anomalous.lib.home.median.se,
    anomalous.all.home.median.se, close.con.home.median.se, close.lib.home.median.se, 
   close.all.home.median.se, random.home.median.se, all.home.median.se)     
home.25 <- c(except.home.25, anomalous.con.home.25, anomalous.lib.home.25,
    anomalous.all.home.25, close.con.home.25, close.lib.home.25, 
    close.all.home.25, random.home.25, all.home.25)
home.75 <- c(except.home.75, anomalous.con.home.75, anomalous.lib.home.75,
    anomalous.all.home.75, close.con.home.75, close.lib.home.75, 
    close.all.home.75, random.home.75, all.home.75)


#BUSINESS

business.median <- c(except.business.median.coef, anomalous.con.business.median.coef, anomalous.lib.business.median.coef,
    anomalous.all.business.median.coef, close.con.business.median.coef, close.lib.business.median.coef, 
    close.all.business.median.coef, random.business.median.coef, all.business.median.coef)
business.se <-  c(except.business.median.se, anomalous.con.business.median.se, anomalous.lib.business.median.se,
    anomalous.all.business.median.se, close.con.business.median.se, close.lib.business.median.se, 
   close.all.business.median.se, random.business.median.se, all.business.median.se)     
business.25 <- c(except.business.25, anomalous.con.business.25, anomalous.lib.business.25,
    anomalous.all.business.25, close.con.business.25, close.lib.business.25, 
    close.all.business.25, random.business.25, all.business.25)
business.75 <- c(except.business.75, anomalous.con.business.75, anomalous.lib.business.75,
    anomalous.all.business.75, close.con.business.75, close.lib.business.75, 
    close.all.business.75, random.business.75, all.business.75)
    
#PERSON

person.median <- c(except.person.median.coef, anomalous.con.person.median.coef, anomalous.lib.person.median.coef,
    anomalous.all.person.median.coef, close.con.person.median.coef, close.lib.person.median.coef, 
    close.all.person.median.coef, random.person.median.coef, all.person.median.coef)
person.se <-  c(except.person.median.se, anomalous.con.person.median.se, anomalous.lib.person.median.se,
    anomalous.all.person.median.se, close.con.person.median.se, close.lib.person.median.se, 
   close.all.person.median.se, random.person.median.se, all.person.median.se)     
person.25 <- c(except.person.25, anomalous.con.person.25, anomalous.lib.person.25,
    anomalous.all.person.25, close.con.person.25, close.lib.person.25, 
    close.all.person.25, random.person.25, all.person.25)
person.75 <- c(except.person.75, anomalous.con.person.75, anomalous.lib.person.75,
    anomalous.all.person.75, close.con.person.75, close.lib.person.75, 
    close.all.person.75, random.person.75, all.person.75)
  
 
#CAR


car.median <- c(except.car.median.coef, anomalous.con.car.median.coef, anomalous.lib.car.median.coef,
    anomalous.all.car.median.coef, close.con.car.median.coef, close.lib.car.median.coef, 
    close.all.car.median.coef, random.car.median.coef, all.car.median.coef)
car.se <-  c(except.car.median.se, anomalous.con.car.median.se, anomalous.lib.car.median.se,
    anomalous.all.car.median.se, close.con.car.median.se, close.lib.car.median.se, 
   close.all.car.median.se, random.car.median.se, all.car.median.se)     
car.25 <- c(except.car.25, anomalous.con.car.25, anomalous.lib.car.25,
    anomalous.all.car.25, close.con.car.25, close.lib.car.25, 
    close.all.car.25, random.car.25, all.car.25)
car.75 <- c(except.car.75, anomalous.con.car.75, anomalous.lib.car.75,
    anomalous.all.car.75, close.con.car.75, close.lib.car.75, 
    close.all.car.75, random.car.75, all.car.75)

     
#EXTENT


extent.median <- c(except.extent.median.coef, anomalous.con.extent.median.coef, anomalous.lib.extent.median.coef,
    anomalous.all.extent.median.coef, close.con.extent.median.coef, close.lib.extent.median.coef, 
   close.all.extent.median.coef, random.extent.median.coef, all.extent.median.coef)
extent.se <-  c(except.extent.median.se, anomalous.con.extent.median.se, anomalous.lib.extent.median.se,
    anomalous.all.extent.median.se, close.con.extent.median.se, close.lib.extent.median.se, 
   close.all.extent.median.se, random.extent.median.se, all.extent.median.se)     
extent.25 <- c(except.extent.25, anomalous.con.extent.25, anomalous.lib.extent.25,
    anomalous.all.extent.25, close.con.extent.25, close.lib.extent.25, 
    close.all.extent.25, random.extent.25, all.extent.25)
extent.75 <- c(except.extent.75, anomalous.con.extent.75, anomalous.lib.extent.75,
    anomalous.all.extent.75, close.con.extent.75, close.lib.extent.75, 
    close.all.extent.75, random.extent.75, all.extent.75)
    
       
#WARRANT


warrant.median <- c(except.warrant.median.coef, anomalous.con.warrant.median.coef, anomalous.lib.warrant.median.coef,
    anomalous.all.warrant.median.coef, close.con.warrant.median.coef, close.lib.warrant.median.coef, 
   close.all.warrant.median.coef, random.warrant.median.coef, all.warrant.median.coef)
warrant.se <-  c(except.warrant.median.se, anomalous.con.warrant.median.se, anomalous.lib.warrant.median.se,
    anomalous.all.warrant.median.se, close.con.warrant.median.se, close.lib.warrant.median.se, 
   close.all.warrant.median.se, random.warrant.median.se, all.warrant.median.se)     
warrant.25 <- c(except.warrant.25, anomalous.con.warrant.25, anomalous.lib.warrant.25,
    anomalous.all.warrant.25, close.con.warrant.25, close.lib.warrant.25, 
    close.all.warrant.25, random.warrant.25, all.warrant.25)
warrant.75 <- c(except.warrant.75, anomalous.con.warrant.75, anomalous.lib.warrant.75,
    anomalous.all.warrant.75, close.con.warrant.75, close.lib.warrant.75, 
    close.all.warrant.75, random.warrant.75, all.warrant.75)

    
#INCIDENT


incident.median <- c(except.incident.median.coef, anomalous.con.incident.median.coef, anomalous.lib.incident.median.coef,
    anomalous.all.incident.median.coef, close.con.incident.median.coef, close.lib.incident.median.coef, 
   close.all.incident.median.coef, random.incident.median.coef, all.incident.median.coef)
incident.se <-  c(except.incident.median.se, anomalous.con.incident.median.se, anomalous.lib.incident.median.se,
    anomalous.all.incident.median.se, close.con.incident.median.se, close.lib.incident.median.se, 
   close.all.incident.median.se, random.incident.median.se, all.incident.median.se)     
incident.25 <- c(except.incident.25, anomalous.con.incident.25, anomalous.lib.incident.25,
    anomalous.all.incident.25, close.con.incident.25, close.lib.incident.25, 
    close.all.incident.25, random.incident.25, all.incident.25)
incident.75 <- c(except.incident.75, anomalous.con.incident.75, anomalous.lib.incident.75,
    anomalous.all.incident.75, close.con.incident.75, close.lib.incident.75, 
    close.all.incident.75, random.incident.75, all.incident.75)


#AFTERLAW


afterlaw.median <- c(except.afterlaw.median.coef, anomalous.con.afterlaw.median.coef, anomalous.lib.afterlaw.median.coef,
    anomalous.all.afterlaw.median.coef, close.con.afterlaw.median.coef, close.lib.afterlaw.median.coef, 
   close.all.afterlaw.median.coef, random.afterlaw.median.coef, all.afterlaw.median.coef)
afterlaw.se <-  c(except.afterlaw.median.se, anomalous.con.afterlaw.median.se, anomalous.lib.afterlaw.median.se,
    anomalous.all.afterlaw.median.se, close.con.afterlaw.median.se, close.lib.afterlaw.median.se, 
   close.all.afterlaw.median.se, random.afterlaw.median.se, all.afterlaw.median.se)     
afterlaw.25 <- c(except.afterlaw.25, anomalous.con.afterlaw.25, anomalous.lib.afterlaw.25,
    anomalous.all.afterlaw.25, close.con.afterlaw.25, close.lib.afterlaw.25, 
    close.all.afterlaw.25, random.afterlaw.25, all.afterlaw.25)
afterlaw.75 <- c(except.afterlaw.75, anomalous.con.afterlaw.75, anomalous.lib.afterlaw.75,
    anomalous.all.afterlaw.75, close.con.afterlaw.75, close.lib.afterlaw.75, 
    close.all.afterlaw.75, random.afterlaw.75, all.afterlaw.75)

    
#UNLAWFUL


unlawful.median <- c(except.unlawful.median.coef, anomalous.con.unlawful.median.coef, anomalous.lib.unlawful.median.coef,
    anomalous.all.unlawful.median.coef, close.con.unlawful.median.coef, close.lib.unlawful.median.coef, 
   close.all.unlawful.median.coef, random.unlawful.median.coef, all.unlawful.median.coef)
unlawful.se <-  c(except.unlawful.median.se, anomalous.con.unlawful.median.se, anomalous.lib.unlawful.median.se,
    anomalous.all.unlawful.median.se, close.con.unlawful.median.se, close.lib.unlawful.median.se, 
   close.all.unlawful.median.se, random.unlawful.median.se, all.unlawful.median.se)     
unlawful.25 <- c(except.unlawful.25, anomalous.con.unlawful.25, anomalous.lib.unlawful.25,
    anomalous.all.unlawful.25, close.con.unlawful.25, close.lib.unlawful.25, 
    close.all.unlawful.25, random.unlawful.25, all.unlawful.25)
unlawful.75 <- c(except.unlawful.75, anomalous.con.unlawful.75, anomalous.lib.unlawful.75,
    anomalous.all.unlawful.75, close.con.unlawful.75, close.lib.unlawful.75, 
    close.all.unlawful.75, random.unlawful.75, all.unlawful.75)

    
#PROBCAUS


probcaus.median <- c(except.probcaus.median.coef, anomalous.con.probcaus.median.coef, anomalous.lib.probcaus.median.coef,
    anomalous.all.probcaus.median.coef, close.con.probcaus.median.coef, close.lib.probcaus.median.coef, 
   close.all.probcaus.median.coef, random.probcaus.median.coef, all.probcaus.median.coef)
probcaus.se <-  c(except.probcaus.median.se, anomalous.con.probcaus.median.se, anomalous.lib.probcaus.median.se,
    anomalous.all.probcaus.median.se, close.con.probcaus.median.se, close.lib.probcaus.median.se, 
   close.all.probcaus.median.se, random.probcaus.median.se, all.probcaus.median.se)     
probcaus.25 <- c(except.probcaus.25, anomalous.con.probcaus.25, anomalous.lib.probcaus.25,
    anomalous.all.probcaus.25, close.con.probcaus.25, close.lib.probcaus.25, 
    close.all.probcaus.25, random.probcaus.25, all.probcaus.25)
probcaus.75 <- c(except.probcaus.75, anomalous.con.probcaus.75, anomalous.lib.probcaus.75,
    anomalous.all.probcaus.75, close.con.probcaus.75, close.lib.probcaus.75, 
    close.all.probcaus.75, random.probcaus.75, all.probcaus.75)

    
#EXCEPT


except.median <- c(except.except.median.coef, anomalous.con.except.median.coef, anomalous.lib.except.median.coef,
    anomalous.all.except.median.coef, close.con.except.median.coef, close.lib.except.median.coef, 
   close.all.except.median.coef, random.except.median.coef, all.except.median.coef)
except.se <-  c(except.except.median.se, anomalous.con.except.median.se, anomalous.lib.except.median.se,
    anomalous.all.except.median.se, close.con.except.median.se, close.lib.except.median.se, 
   close.all.except.median.se, random.except.median.se, all.except.median.se)     
except.25 <- c(except.except.25, anomalous.con.except.25, anomalous.lib.except.25,
    anomalous.all.except.25, close.con.except.25, close.lib.except.25, 
    close.all.except.25, random.except.25, all.except.25)
except.75 <- c(except.except.75, anomalous.con.except.75, anomalous.lib.except.75,
    anomalous.all.except.75, close.con.except.75, close.lib.except.75, 
    close.all.except.75, random.except.75, all.except.75)

    
#CHANGE


change.median <- c(except.change.median.coef, anomalous.con.change.median.coef, anomalous.lib.change.median.coef,
    anomalous.all.change.median.coef, close.con.change.median.coef, close.lib.change.median.coef, 
   close.all.change.median.coef, random.change.median.coef, all.change.median.coef)
change.se <-  c(except.change.median.se, anomalous.con.change.median.se, anomalous.lib.change.median.se,
    anomalous.all.change.median.se, close.con.change.median.se, close.lib.change.median.se, 
   close.all.change.median.se, random.change.median.se, all.change.median.se)     
change.25 <- c(except.change.25, anomalous.con.change.25, anomalous.lib.change.25,
    anomalous.all.change.25, close.con.change.25, close.lib.change.25, 
    close.all.change.25, random.change.25, all.change.25)
change.75 <- c(except.change.75, anomalous.con.change.75, anomalous.lib.change.75,
    anomalous.all.change.75, close.con.change.75, close.lib.change.75, 
    close.all.change.75, random.change.75, all.change.75)


# FACT DISTRIBUTION GRAPH

#WITH CONSTANT 7 point SCALE

#add true weights to top row of each plots

point.type <- 19
line.width <- 1.4
cex.point <- 1 #point size
yaxis.label.size <- 1.1
xaxis.label.size <- 1
bottom.margin <- 4 
left.margin <- .8 #for non-left hand panels only
left.left.margin <- 16.1 #only for only left panels
top.margin <- 2
right.margin <- .4
title.size <- 1
x.lim.adjust <- .2

y.names <- c("Conservative, non-excepted home", "anomalous conservative cases",
     "Anomalous liberal Cases", "Anomalous cases", "Close conservative cases", 
        "Close liberal cases", "Close cases", "Random selection", "All cases")  #vector for ylabels
#create index to use for y axes in plots.
y.axis <- seq(1:9)

pdf("fact_betas_final.pdf", width = 9.5, height =9)
#pdf("fact_betas_final_for_slides.pdf", width = 8, height =7)

left.width <- 6.3
layout(rbind(c(1:4), c(5:8), c(9:12)), 
    widths = rbind(c(left.width,3,3,3,left.width,3,3,3,left.width,3,3,3)), respect = F)
#layout.show(12)
#HOME
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))
plot(home.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(home.median - home.se)- x.lim.adjust , 
    max(home.median + home.se + x.lim.adjust)), xlab="", ylab ="")
segments((home.median - home.se),y.axis, (home.median + home.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(home.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(home.25, y.axis,home.75, y.axis, lwd = 1.5)
segments(home.truth, 0, home.truth, 9, lty=2) #draw line for true value
axis(1, at=c(-6:6), labels = c(-6:6), cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels = y.names, tick = T, yaxs="r", las=1, 
    cex.axis=yaxis.label.size, mgp = c(2,.7,0) )
mtext("Home", 3, line = .4, cex = title.size , font=2)
box()

#BUSINESS
par (mar = c(bottom.margin, left.margin, top.margin, right.margin)) 
plot(business.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(business.median - business.se)- x.lim.adjust , 
    max(business.median + business.se + x.lim.adjust)), xlab="", ylab ="")
segments((business.median - business.se),y.axis, (business.median + business.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(business.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(business.25, y.axis,business.75, y.axis, lwd = 1.5)
segments(business.truth, 0, business.truth, 9, lty=2) #draw line for truth
axis(1, at=c(-6:6), labels = c(-6:6), cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Business", 3, line = .4, cex = title.size , font=2)
box()

#PERSON
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(person.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(person.median - person.se)- x.lim.adjust , 
    max(person.median + person.se + x.lim.adjust)), xlab="", ylab ="")
segments((person.median - person.se),y.axis, (person.median + person.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(person.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(person.25, y.axis,person.75, y.axis, lwd = 1.5)
segments(person.truth, 0, person.truth, 9, lty=2) #draw line for truth
axis(1, at=c(-6:6), labels = c(-6:6),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Person", 3, line = .4, cex = title.size , font=2)
box()

#CAR
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(car.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(car.median - car.se)- x.lim.adjust , 
    max(car.median + car.se + x.lim.adjust)), xlab="", ylab ="")
segments((car.median - car.se),y.axis, (car.median + car.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(car.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(car.25, y.axis,car.75, y.axis, lwd = 1.5)
segments(car.truth, 0, car.truth, 9, lty=2) #draw line for truth
axis(1, at=c(-6:6), labels = c(-6:6),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Car", 3, line = .4, cex = title.size , font=2)
box()

#EXTENT
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))
plot(extent.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(extent.median - extent.se)- x.lim.adjust , 
    max(extent.median + extent.se + x.lim.adjust)), xlab="", ylab ="")
segments((extent.median - extent.se),y.axis, (extent.median + extent.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(extent.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(extent.25, y.axis,extent.75, y.axis, lwd = 1.5)
segments(extent.truth, 0, extent.truth, 9, lty=2) #draw line for truth
axis(1, at=c(-6:6), labels = c(-6:6),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels = y.names, tick = T, yaxs="r", las=1, 
    cex.axis=yaxis.label.size, mgp = c(2,.7,0))
mtext("Extent of Search", 3, line = .4, cex = title.size , font=2)
box()

#WARRANT
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(warrant.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(warrant.median - warrant.se)- x.lim.adjust , 
    max(warrant.median + warrant.se + x.lim.adjust)), xlab="", ylab ="")
segments((warrant.median - warrant.se),y.axis, (warrant.median + warrant.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(warrant.median, y.axis,  pch = point.type, cex.axis = xaxis.label.size,
     bg = "white", cex = cex.point)    
#segments(warrant.25, y.axis,warrant.75, y.axis, lwd = 1.5)
segments(warrant.truth, 0, warrant.truth, 9, lty=2) #draw line for truth
axis(1, at=c(-6:6), labels = c(-6:6),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Warrant", 3, line = .4, cex = title.size , font=2)
box()

#INCIDENT
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(incident.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(incident.median - incident.se)- x.lim.adjust , 
    max(incident.median + incident.se + x.lim.adjust)), xlab="", ylab ="")
segments((incident.median - incident.se),y.axis, (incident.median + incident.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(incident.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(incident.25, y.axis,incident.75, y.axis, lwd = 1.5)
segments(incident.truth, 0, incident.truth, 9, lty=2) #draw line for truth
axis(1, at=c(-6:6), labels = c(-6:6),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Incident to\nArrest", 3, line = .4, cex = title.size , font=2)
box()

#AFTERLAW
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(afterlaw.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(afterlaw.median - afterlaw.se)- x.lim.adjust , 
    max(afterlaw.median + afterlaw.se + x.lim.adjust)), xlab="", ylab ="")
segments((afterlaw.median - afterlaw.se),y.axis, (afterlaw.median + afterlaw.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(afterlaw.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(afterlaw.25, y.axis,afterlaw.75, y.axis, lwd = 1.5)
segments(afterlaw.truth, 0, afterlaw.truth, 9, lty=2) #draw line for truth
axis(1, at=seq(-1,2.5, by = .5), labels = c(-1, "-.5", 0, ".5", 1, "1.5", 2, "2.5"),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("After Lawful\nArrest", 3, line = .4, cex = title.size , font=2)
box()

#Unlawful
par (mar = c(bottom.margin, left.left.margin, top.margin, right.margin))
plot(unlawful.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(unlawful.median - unlawful.se)- x.lim.adjust , 
    max(unlawful.median + unlawful.se + x.lim.adjust)), xlab="", ylab ="")
segments((unlawful.median - unlawful.se),y.axis, (unlawful.median + unlawful.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(unlawful.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(unlawful.25, y.axis,unlawful.75, y.axis, lwd = 1.5)
segments(unlawful.truth, 0, unlawful.truth, 9, lty=2) #draw line for truth
axis(1, at=seq(-1,2.5, by = .5), labels = c(-1, "-.5", 0, ".5", 1, "1.5", 2, "2.5"),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels = y.names, tick = T, yaxs="r", las=1, 
    cex.axis=yaxis.label.size, mgp = c(2,.7,0))
mtext("After\nUnlawful Arrest", 3, line = .4, cex = title.size, font=2)
box()

#PROBCAUS
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(probcaus.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(probcaus.median - probcaus.se)- x.lim.adjust , 
    max(probcaus.median + probcaus.se + x.lim.adjust)), xlab="", ylab ="")
segments((probcaus.median - probcaus.se),y.axis, (probcaus.median + probcaus.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(probcaus.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(probcaus.25, y.axis,probcaus.75, y.axis, lwd = 1.5)
segments(probcaus.truth, 0, probcaus.truth, 9, lty=2) #draw line for truth
axis(1, at=seq(-1,2.5, by = .5), labels = c(-1, "-.5", 0, ".5", 1, "1.5", 2, "2.5"),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Probable Cause", 3, line = .4, cex = title.size, font=2)
box()


#EXCEPT
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(except.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(except.median - except.se)- x.lim.adjust , 
    max(except.median + except.se + x.lim.adjust)), xlab="", ylab ="")
segments((except.median - except.se),y.axis, (except.median + except.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(except.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(except.25, y.axis,except.75, y.axis, lwd = 1.5)
segments(except.truth, 0, except.truth, 9, lty=2) #draw line for truth
axis(1, at=seq(-1,2.5, by = .5), labels = c(-1, "-.5", 0, ".5", 1, "1.5", 2, "2.5"),cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Exception to\nWarrant Requirement", 3, line = .4, cex = title.size, font=2)
box()

#CHANGE
x.lim.adjust <-.01
par (mar = c(bottom.margin, left.margin, top.margin, right.margin))
plot(change.median, y.axis, type = "n", xaxs = "i",
    axes =F,  xlim = c(min(change.median - change.se)- x.lim.adjust , 
    max(change.median + change.se + x.lim.adjust)), xlab="", ylab ="")
segments((change.median - change.se),y.axis, (change.median + change.se), y.axis,
    lwd=line.width, xpd = NA)#draw bars for se's
points(change.median, y.axis,  pch = point.type, cex = cex.point, bg = "white")    
#segments(change.25, y.axis,change.75, y.axis, lwd = 1.5)
segments(change.truth, 0, change.truth, 9, lty=2) #draw line for truth
axis(1, at=seq(-.15,.75, by = .15), labels = c(".15", 0, ".15", ".3", ".45", ".6", ".75")
    ,cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, xaxs="r", mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels =F, tick = T, yaxs="r", las=1, cex.axis=yaxis.label.size)
mtext("Membership\nChange", 3, line = .4, cex = title.size, font=2)
box()

dev.off()

###########################################################
####################################################################################################################
#############################3

#GRAPH THE IMPACT OF HOUSE AND EXCEPT

home.impact.median <- 100*c(median(except.home.impact), median(anomalous.con.home.impact), median(anomalous.lib.home.impact),
    median(anomalous.all.home.impact), median(close.con.home.impact), median(close.lib.home.impact), 
    median(close.all.home.impact), median(random.home.impact), median(all.home.impact))
   
home.impact.25 <-   100*c(quantile(except.home.impact, .25),  quantile(anomalous.con.home.impact, .25),
    quantile(anomalous.lib.home.impact, .25), quantile(anomalous.all.home.impact, .25), 
    quantile(close.con.home.impact, .25), quantile(close.lib.home.impact, .25),
    quantile(close.all.home.impact, .25), quantile(random.home.impact, .25), quantile(all.home.impact, .25))

home.impact.75 <-   100*c(quantile(except.home.impact, .7499),  quantile(anomalous.con.home.impact, .7499),
    quantile(anomalous.lib.home.impact, .7499), quantile(anomalous.all.home.impact, .7499), 
    quantile(close.con.home.impact, .7499), quantile(close.lib.home.impact, .7499),
    quantile(close.all.home.impact, .7499), quantile(random.home.impact, .7499), quantile(all.home.impact, .7499))

except.impact.median <- 100*c(median(except.except.impact), median(anomalous.con.except.impact), median(anomalous.lib.except.impact),
    median(anomalous.all.except.impact), median(close.con.except.impact), median(close.lib.except.impact), 
    median(close.all.except.impact), median(random.except.impact), median(all.except.impact))
    
except.impact.25 <-   100*c(quantile(except.except.impact, .25),  quantile(anomalous.con.except.impact, .25),
    quantile(anomalous.lib.except.impact, .25), quantile(anomalous.all.except.impact, .25), 
    quantile(close.con.except.impact, .25), quantile(close.lib.except.impact, .25),
    quantile(close.all.except.impact, .25), quantile(random.except.impact, .25), quantile(all.except.impact, .25))

except.impact.75 <-   100*c(quantile(except.except.impact, .7499),  quantile(anomalous.con.except.impact, .7499),
    quantile(anomalous.lib.except.impact, .7499), quantile(anomalous.all.except.impact, .7499), 
    quantile(close.con.except.impact, .7499), quantile(close.lib.except.impact, .7499),
    quantile(close.all.except.impact, .7499), quantile(random.except.impact, .7499), quantile(all.except.impact, .7499))

y.axis <- 1:9
point.type <- 19
line.width <- 1.3
cex.point <- 1 #point size
yaxis.label.size <- 1
xaxis.label.size <- 1
y.names <- c("Conservative, non-excepted home", "Anomalous conservative cases",
     "Anomalous liberal cases", "Anomalous cases", "Close conservative cases", 
        "Close liberal Cases", "Close cases", "Random selection", "All cases") 
        
pdf("fact_impact_final.pdf", width =10, height =4)

layout(rbind(c(1:2)), 
    widths = rbind(c(5,3)), respect = F)
#HOME
par (mar = c(4, 14, 2, .5)) 
plot(home.impact.median, y.axis, type = "n", axes =F, 
    xlim = c(-60,10), frame = T,
    xlab="", ylab ="")#call empty plot
#segments(flips.min, y.axis2,flips.max, y.axis2, lty = 5, lwd = 1.5)
segments(home.impact.25, y.axis,home.impact.75, y.axis, lwd = line.width)
segments(100*median(all.home.impact), 0, 100*median(all.home.impact), 9.2, lty=3) # drop line through median from all.cases
points(home.impact.median, y.axis, pch = 19 , cex = cex.point,  bg = "white") #plot median
axis(1, at=seq(-60,10, by=10), labels = seq(-60,10, by=10), cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels = y.names, tick = T, yaxs="r", las=1, 
    cex.axis=yaxis.label.size, mgp = c(2,.7,0))
mtext("Home", 3, line = .4, cex = 1.2, font=2)
mtext("Average Percent Change\nin Probability", 1,  line = 3, cex = 1)
box(bty = "l")

#EXCEPT
par (mar = c(4, 1.5, 2, .5))
plot(except.impact.median, y.axis, type = "n", axes =F, 
    xlim = c(-12,20), frame = T,
    xlab="", ylab ="")#call empty plot
points(except.impact.median, y.axis, pch = 19 , cex = cex.point) #plot median
#segments(flips.min, y.axis2,flips.max, y.axis2, lty = 5, lwd = 1.5)
segments(except.impact.25, y.axis,except.impact.75, y.axis, lwd = line.width)
segments(100*median(all.except.impact), 0, 100*median(all.except.impact), 9.2, lty=3) # drop line through median from all.cases
axis(1, at=seq(-10,20, by=10), labels = seq(-10,20, by=10), cex.axis = xaxis.label.size, las = 1, tick = T, 
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis, labels = F, tick = T, yaxs="r", line = 0)
mtext("Exception", 3, line = .4, cex = 1.2, font=2)
mtext("Average Percent Change\nin Probability", 1,  line = 3, cex = 1)
box(bty = "l")

dev.off()

#LOOKING AT PERCENT CONSERVATIVE IN-SAMPlE VERSUS OUT OF Sample

#create vectors by strategies
#flip sign so we can express in terms of liberalism

libbias.medians <- - ( 100 *c(median(except.consbias),  median(anomalous.con.consbias),
    median(anomalous.lib.consbias), median(anomalous.all.consbias), 
    median(close.con.consbias), median(close.lib.consbias),
    median(close.all.consbias), median(random.consbias)))

libbias.25 <- - (100*c(quantile(except.consbias, .25),  quantile(anomalous.con.consbias, .25),
    quantile(anomalous.lib.consbias, .25), quantile(anomalous.all.consbias, .25), 
    quantile(close.con.consbias, .25), quantile(close.lib.consbias, .25),
    quantile(close.all.consbias, .25), quantile(random.consbias, .25)))
  
libbias.75 <- - (100*c(quantile(except.consbias, .749),  quantile(anomalous.con.consbias, .749),
    quantile(anomalous.lib.consbias, .749), quantile(anomalous.all.consbias, .749), 
    quantile(close.con.consbias, .749), quantile(close.lib.consbias, .749),
    quantile(close.all.consbias, .749), quantile(random.consbias, .749)))
    
libbias.min <- - (100*c(min(except.consbias),  min(anomalous.con.consbias),
    min(anomalous.lib.consbias), min(anomalous.all.consbias), 
    min(close.con.consbias), min(close.lib.consbias),
    min(close.all.consbias), min(random.consbias)))

libbias.max <- - (100*c(max(except.consbias),  max(anomalous.con.consbias),
    max(anomalous.lib.consbias), max(anomalous.all.consbias), 
    max(close.con.consbias), max(close.lib.consbias),
    max(close.all.consbias), max(random.consbias)))


y.axis2<-c(1:8) #create indicator for y-axis
y.names.noall <- c("Conservative, non-excepted home", "Anomalous conservative cases",
     "Anomalous liberal cases", "Anomalous cases", "Close conservative cases", 
        "Close liberal cases", "Close cases", "Random selection")  #vector for ylabels

pdf("libbias_final.pdf", width=9, height = 5)
par(mfrow=c(1,1), mar=c(4,16,.5,1))
#LIBERAL BIAS

plot(libbias.medians, y.axis2, type = "n", axes =F, 
    xlim = c(-20,40), xaxs = "i",
    xlab="", ylab ="")#call empty plot
segments(libbias.min, y.axis2,libbias.max, y.axis2, lty = 5, lwd = 1.8)
segments(libbias.25, y.axis2,libbias.75, y.axis2, lwd = 3.5)
segments(0, 0, 0, 8.5, lty=3) # drop line through minimum
points(libbias.medians, y.axis2, pch = 21, cex  = 1.1, bg = "white") #plot median
axis(1, at= seq(-40,40, by =10), #set up vector x-axis labels
   labels = seq(-40,40, by =10), cex = 1.2, las = 1, tick = T, 
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis2, labels = y.names.noall, tick = T, yaxs="r", line = 0, las = 1, cex.axis = 1.2, 
    mgp = c(2,.5,0))
#mtext("All Misclassifications", 3, line = .4, cex = 1.1, font=2)
mtext("Percent Liberal Bias", 1,  line = 2.5, cex = 1.3)
box(bty = "l")

dev.off()


##################################################################################################
#Case Flips
#CREATE VECTORS OF MEDIANS, 25th and 75th percentil

flips.medians <- 100*c(median(except.flips.out),  median(anomalous.con.flips.out),
    median(anomalous.lib.flips.out), median(anomalous.all.flips.out), 
    median(close.con.flips.out), median(close.lib.flips.out),
    median(close.all.flips.out), median(random.flips.out))

flips.25 <- 100*c(quantile(except.flips.out, .25),  quantile(anomalous.con.flips.out, .25),
    quantile(anomalous.lib.flips.out, .25), quantile(anomalous.all.flips.out, .25), 
    quantile(close.con.flips.out, .25), quantile(close.lib.flips.out, .25),
    quantile(close.all.flips.out, .25), quantile(random.flips.out, .25))
  
flips.75 <- 100*c(quantile(except.flips.out, .749),  quantile(anomalous.con.flips.out, .749),
    quantile(anomalous.lib.flips.out, .749), quantile(anomalous.all.flips.out, .749), 
    quantile(close.con.flips.out, .749), quantile(close.lib.flips.out, .749),
    quantile(close.all.flips.out, .749), quantile(random.flips.out, .749))
    
flips.min <- 100*c(min(except.flips.out),  min(anomalous.con.flips.out),
    min(anomalous.lib.flips.out), min(anomalous.all.flips.out), 
    min(close.con.flips.out), min(close.lib.flips.out),
    min(close.all.flips.out), min(random.flips.out))

flips.max <- 100*c(max(except.flips.out),  max(anomalous.con.flips.out),
    max(anomalous.lib.flips.out), max(anomalous.all.flips.out), 
    max(close.con.flips.out), max(close.lib.flips.out),
    max(close.all.flips.out), max(random.flips.out))
  
falsepos.medians <- 100*c(median(except.falsepos.out),  median(anomalous.con.falsepos.out),
    median(anomalous.lib.falsepos.out), median(anomalous.all.falsepos.out), 
    median(close.con.falsepos.out), median(close.lib.falsepos.out),
    median(close.all.falsepos.out), median(random.falsepos.out))

falsepos.25 <- 100*c(quantile(except.falsepos.out, .25),  quantile(anomalous.con.falsepos.out, .25),
    quantile(anomalous.lib.falsepos.out, .25), quantile(anomalous.all.falsepos.out, .25), 
    quantile(close.con.falsepos.out, .25), quantile(close.lib.falsepos.out, .25),
    quantile(close.all.falsepos.out, .25), quantile(random.falsepos.out, .25))
  
falsepos.75 <- 100*c(quantile(except.falsepos.out, .749),  quantile(anomalous.con.falsepos.out, .749),
    quantile(anomalous.lib.falsepos.out, .749), quantile(anomalous.all.falsepos.out, .749), 
    quantile(close.con.falsepos.out, .749), quantile(close.lib.falsepos.out, .749),
    quantile(close.all.falsepos.out, .749), quantile(random.falsepos.out, .749))

falsepos.min <- 100*c(min(except.falsepos.out),  min(anomalous.con.falsepos.out),
    min(anomalous.lib.falsepos.out), min(anomalous.all.falsepos.out), 
    min(close.con.falsepos.out), min(close.lib.falsepos.out),
    min(close.all.falsepos.out), min(random.falsepos.out))

falsepos.max <- 100*c(max(except.falsepos.out),  max(anomalous.con.falsepos.out),
    max(anomalous.lib.falsepos.out), max(anomalous.all.falsepos.out), 
    max(close.con.falsepos.out), max(close.lib.falsepos.out),
    max(close.all.falsepos.out), max(random.falsepos.out))

falseneg.medians <- 100*c(median(except.falseneg.out),  median(anomalous.con.falseneg.out),
    median(anomalous.lib.falseneg.out), median(anomalous.all.falseneg.out), 
    median(close.con.falseneg.out), median(close.lib.falseneg.out),
    median(close.all.falseneg.out), median(random.falseneg.out))

falseneg.25 <- 100*c(quantile(except.falseneg.out, .25),  quantile(anomalous.con.falseneg.out, .25),
    quantile(anomalous.lib.falseneg.out, .25), quantile(anomalous.all.falseneg.out, .25), 
    quantile(close.con.falseneg.out, .25), quantile(close.lib.falseneg.out, .25),
    quantile(close.all.falseneg.out, .25), quantile(random.falseneg.out, .25))
  
falseneg.75 <- 100*c(quantile(except.falseneg.out, .749),  quantile(anomalous.con.falseneg.out, .749),
    quantile(anomalous.lib.falseneg.out, .749), quantile(anomalous.all.falseneg.out, .749), 
    quantile(close.con.falseneg.out, .749), quantile(close.lib.falseneg.out, .749),
    quantile(close.all.falseneg.out, .749), quantile(random.falseneg.out, .749))
  
falseneg.min <- 100*c(min(except.falseneg.out),  min(anomalous.con.falseneg.out),
    min(anomalous.lib.falseneg.out), min(anomalous.all.falseneg.out), 
    min(close.con.falseneg.out), min(close.lib.falseneg.out),
    min(close.all.falseneg.out), min(random.falseneg.out))

falseneg.max <- 100*c(max(except.falseneg.out),  max(anomalous.con.falseneg.out),
    max(anomalous.lib.falseneg.out), max(anomalous.all.falseneg.out), 
    max(close.con.falseneg.out), max(close.lib.falseneg.out),
    max(close.all.falseneg.out), max(random.falseneg.out))  
      
#3 graphs in a row: All Caseflips, Falsepos (Falsely Conservative), Falseneg (Falsely Liberal)
#drop line through random

x.numbers <- seq(0,100, by =10) #set up vector x-axis labels

pdf("flips_final.pdf", width = 7, height = 8)
cex.point <- 1.1
par(mfrow=c(3,1), mar=c(4,16.1,3,2))
#ALL CASEFLIPS
plot(flips.medians, y.axis2, type = "n", axes =F, 
    xlim = c(0,70),  xlab="", ylab ="", xaxs="r")#call empty plot
segments(flips.min, y.axis2,flips.max, y.axis2, lty = 2, lwd = 1)
segments(flips.25, y.axis2,flips.75, y.axis2, lwd = 2.5)
points(flips.medians, y.axis2, pch = 21, cex  = cex.point, bg = "white") #plot median
#segments(min(flips.min), 0, min(flips.min), 8.2, lty=3) # drop line through minimum
axis(1, at=x.numbers, labels = x.numbers, cex.axis = 1.1, las = 1, tick = T, 
    mgp = c(2,.5,0), xaxs="r")#mgp moves label closer to tick marks
axis(2, at = y.axis2, labels = y.names.noall, tick = T, yaxs="r", line = 0, las = 1, cex.axis = 1.1, mgp = c(2,.8,14))
mtext("All Misclassifications", 3, line = 1, cex = 1.1, font=2)
mtext("Percent Misclassifications", 1,  line = 1.8, cex = .8)
box(bty = "l")

#ALL FALsEPOS

plot(falsepos.medians, y.axis2, type = "n", axes =F, 
    xlim = c(0,70),   xlab="", ylab ="", xaxs="r" )#call empty plot
segments(falsepos.min, y.axis2,falsepos.max, y.axis2, lty = 2, lwd = 1)
segments(falsepos.25, y.axis2,falsepos.75, y.axis2, lwd = 2.5)
points(falsepos.medians, y.axis2, pch = 21 , cex  =  cex.point, bg = "white") #plot median
#segments(min(falsepos.min), 0, min(falsepos.min),8.2, lty=3) # drop line through min of minimums
axis(1, at=x.numbers, labels = x.numbers, cex.axis = 1.1, las = 1, tick = T, 
    mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis2, labels = y.names.noall, tick = T, yaxs="r", line = 0, las = 1, cex.axis = 1.1, mgp = c(2,.8,14))
mtext("Falsely Conservative Classifications", 3, line = 1, cex = 1.1, font=2)
mtext("Percent Misclassifications", 1,  line = 1.8, cex = .8)
box(bty = "l")

#ALL falseneg

plot(falseneg.medians, y.axis2, type = "n", axes =F, 
    xlim = c(0,70), xlab="", ylab ="", xaxs="r")#call empty plot
segments(falseneg.min, y.axis2,falseneg.max, y.axis2, lty = 2, lwd = 1)
segments(falseneg.25, y.axis2,falseneg.75, y.axis2, lwd = 2.5)
points(falseneg.medians, y.axis2, pch = 21 , cex  = cex.point,bg = "white") #plot median
#segments(min(falseneg.min), 0, min(falseneg.min),8.2, lty=3) # drop line through min of minimums
axis(1, at=x.numbers, labels = x.numbers, cex.axis = 1.1, las = 1, tick = T, 
    line = 0, mgp = c(2,.5,0))#mgp moves label closer to tick marks
axis(2, at = y.axis2, labels = y.names.noall, tick = T, yaxs="r", las = 1, cex.axis = 1.1, mgp = c(2,.8,14))
mtext("Falsely Liberal Classifications", 3, line = .1, cex = 1.1, font=2)
mtext("Percent Misclassifications", 1,  line = 1.8, cex = .8)
box(bty = "l")

dev.off()

#get estimates for Table 1: We want mean, median and min for random cases and selected cases

#Random Cases
    #Means
print(mean(random.n.in))
print(mean(close.all.random))
print(mean(close.lib.random))
print(mean(close.con.random))
print(mean(anomalous.all.random))
print(mean(anomalous.lib.random))
print(mean(anomalous.con.random))
print(mean(except.random))
    #Medians
print(median(random.n.in))
print(median(close.all.random))
print(median(close.lib.random))
print(median(close.con.random))
print(median(anomalous.all.random))
print(median(anomalous.lib.random))
print(median(anomalous.con.random))
print(median(except.random))
    #Min
print(min(random.n.in))
print(min(close.all.random))
print(min(close.lib.random))
print(min(close.con.random))
print(min(anomalous.all.random))
print(min(anomalous.lib.random))
print(min(anomalous.con.random))
print(min(except.random))

#Chosen Cases
    #Means
print(mean(close.all.chosen))
print(mean(close.lib.chosen))
print(mean(close.con.chosen))
print(mean(anomalous.all.chosen))
print(mean(anomalous.lib.chosen))
print(mean(anomalous.con.chosen))
print(mean(except.chosen))
    #Medians
print(median(close.all.chosen))
print(median(close.lib.chosen))
print(median(close.con.chosen))
print(median(anomalous.all.chosen))
print(median(anomalous.lib.chosen))
print(median(anomalous.con.chosen))
print(median(except.chosen))
    #Min
print(min(close.all.chosen))
print(min(close.lib.chosen))
print(min(close.con.chosen))
print(min(anomalous.all.chosen))
print(min(anomalous.lib.chosen))
print(min(anomalous.con.chosen))
print(min(except.chosen))

#All Cases (Combined N's)
    #Mean

print(mean(random.n.in))
print(mean(close.all.n.in))
print(mean(close.lib.n.in))
print(mean(close.con.n.in))
print(mean(anomalous.all.n.in))
print(mean(anomalous.lib.n.in))
print(mean(anomalous.con.n.in))
print(mean(except.n.in))
     #Median
print(median(random.n.in))
print(median(close.all.n.in))
print(median(close.lib.n.in))
print(median(close.con.n.in))
print(median(anomalous.all.n.in))
print(median(anomalous.lib.n.in))
print(median(anomalous.con.n.in))
print(median(except.n.in))

     #Min
print(min(random.n.in))
print(min(close.all.n.in))
print(min(close.lib.n.in))
print(min(close.con.n.in))
print(min(anomalous.all.n.in))
print(min(anomalous.lib.n.in))
print(min(anomalous.con.n.in))
print(min(except.n.in))


#histograms of sim n's

pdf("hists_in.pdf", height = 10, width = 10)
par(mfrow = c(3,3))

hist(random.n.in)
hist(close.all.n.in)
hist(close.lib.n.in)
hist(close.con.n.in)
hist(anomalous.all.n.in)
hist(anomalous.lib.n.in)
hist(anomalous.con.n.in)
hist(except.n.in)

dev.off()

quantile(random.n.in, probs = c(.05))
quantile(close.all.n.in, probs = c(.05))
quantile(close.lib.n.in, probs = c(.05))
quantile(close.con.n.in, probs = c(.05))
quantile(anomalous.all.n.in, probs = c(.05))
quantile(anomalous.lib.n.in, probs = c(.05))
quantile(anomalous.con.n.in, probs = c(.05))
quantile(except.n.in, probs = c(.05))
