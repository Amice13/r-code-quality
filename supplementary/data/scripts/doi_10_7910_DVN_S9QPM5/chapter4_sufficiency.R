#~~~~~  Qualitative Comparative Analysis (QCA) Using R: A Beginner’s Guide     ~~~~~

#            Ioana-Elena Oana, Carsten Q. Schneider, Eva Thomann 


#~~~~~  Chapter 4 - Sufficient Conditions  ~~~~~ 



# Building hypothetical example:


library(QCA)
library(SetMethods)


# Consistent XY plot

Name <- c("Amanda","Bob","Carl","Cecilia","Ana","Alex","Kim")
X <- c(0.6,0.6,0.2,0.1,0.9,0.7,0.3)
Y <- c(0.7,0.6,0.9,0.3,1,0.8,0.4)
SF1 <- data.frame(X, Y)
row.names(SF1) <- Name

xy.plot("X", "Y", 
        data = SF1, 
        main = "XY plot - sufficiency", 
        ylab = "Arriving in time",
        xlab = "Living close by",
        necessity = FALSE,
        fontsize = 6)


# Inconsistent XY plot

SF2 <- rbind(SF1, c(0.75, 0.65), c(0.55, 0.45),  c(0.35, 0.25))
row.names(SF2)[8:10] <- c("Mark","Dee", "Fiona")

xy.plot("X", "Y", 
        data = SF2, 
        main = "XY plot - sufficiency", 
        ylab = "Arriving in time",
        xlab = "Living close by",
        necessity = FALSE,
        fontsize = 6)



# data for XY plots for simultaneous subset relation

Name <- c("Dan","Claire","Amanda","Bob","Carl","Cecilia","Ana","Mark","Jim","Joe")
X <- c(0.45, 0.2,0.1,0.2,0.1,0.4,0.6,0.25,0.65,0.6)
Y <- c(0.55, 0.9,0.7,0.4,0.3,0.3,0.7,0.1,0.65,0.35)
SF3 <- data.frame(X, Y)
row.names(SF3) <- Name

xy.plot("X", "Y", 
        data = SF3, 
        main = "XY plot - sufficiency", 
        ylab = "Arriving in time",
        xlab = "Living close by",
        necessity = FALSE,
        fontsize = 6)

xy.plot("X", "~Y", 
        data = SF3, 
        main = "XY plot - sufficiency", 
        ylab = "NOT Arriving in time",
        xlab = "Living close by",
        necessity = FALSE,
        fontsize = 6)



# data for XY plots for high and low coverage

xy.plot("X", "Y", 
        data = SF3, 
        main = "XY plot - sufficiency", 
        ylab = "Arriving in time",
        xlab = "Living close by",
        necessity = FALSE,
        fontsize = 6)

Name <- c("Dan","Claire","Amanda","Bob","Carl","Cecilia","Ana","Mark","Jim","Joe")
X4 <- c(0,0,0.1,0.05,0.1,0,0.6,0.25,0.45,0.1)
Y4 <- c(0.55,0.9,0.7,0.4,0.3,0.3,0.7,0.1,0.65,0.35)
SF4 <- data.frame(X4, Y4)
row.names(SF4) <- Name

xy.plot("X4", "Y4", 
        data = SF4, 
        main = "XY plot - sufficiency", 
        ylab = "Arriving in time",
        xlab = "Coming by bike",
        necessity = FALSE,
        fontsize = 6)



#### 1. Single sufficient conditions: ####


# Analysis of sufficiency for the presence of the outcome (Y):

# For the presence a single condition:
data("STUF")

QCAfit(STUF$A, 
       STUF$Y,
       cond.lab = "A",
       necessity = FALSE)


# For an entire dataframe:

QCAfit(x = STUF[,1:5], 
       y = STUF$Y,
       necessity = FALSE)



# Visualizing single sufficient condition:

xy.plot(x = "A",
        y = "Y",
        data = STUF, 
        necessity = FALSE, 
        jitter = TRUE,
        xlab = "L",
        ylab = "INT",
        main = "Sufficiency of L for INT")

xy.plot(x = "~A",
        y = "Y",
        data = STUF, 
        necessity = FALSE, 
        jitter = TRUE,
        xlab = "~A",
        ylab = "INT",
        main = "Sufficiency of ~A for INT")


#### 2. Sufficiency Analysis ####


# Building hypothetical example

# Y = arriving in time
# X = living close by
# A = arriving by bike
# C = family norm of being punctual

Name <- c("Amanda","Bob","Carl","Cecilia","Ana","Alex","Mark","Dee","Fiona","Kim","Joe","Jess","Frank")
X <- c(0.6,0.3,0.8,0,0.9,0.7,0.75,0.55,0.35,0.4,0.4,0.45,0.6)
A <- c(0.8,0.7,1,0.6,0.1,0.2,0.7,0.3,0.7,0.9,0.4,0.45,0.35)
C <- c(0.7,0.7,0.3,0.2,0.2,0.4,0.6,0.3,0.2,0.6,0.4,0.55,0.6)
Y <- c(0.7,0.8,0.9,0.3,1,0.8,0.65,0.45,0.75,0.6,0.2,0.2,0.2)
SF5 <- data.frame(X, A, C, Y)
row.names(SF5) <- Name
SF5

# write.csv(SF5, file = "team_fs.CSV")

# NB: tweak further to get dev coverage cases and 
# non-uniquely covered typical cases

property.cube(SF5[,c("X","A","C")], 
              highlight.3d = FALSE, 
              main = NULL, 
              labs = TRUE,
              dot.cex=0.8,
              dot.offset = 0.7)

TT_y <- truthTable(data = SF5, 
                     outcome="Y", 
                     conditions = c("X","A","C"), 
                     incl.cut = 0.8, 
                     pri.cut = 0.51,
                     n.cut = 1, 
                     sort.by = "incl",
                     show.cases = TRUE)
TT_y


stargazerTT(TT_y)


sol_y <- minimize(input =  TT_y, 
                    details = TRUE)
sol_y

stargazerSol(sol_y,
             outcome = "Y")

pimplot(data = SF5,
        outcome = "Y",
        results = sol_y,
        all_labels = TRUE,
        ttrows = "5",
        jitter = TRUE)

pimplot(data = SF5,
        outcome = "Y",
        results = sol_y,
        all_labels = TRUE,
        ttrows = "3",
        jitter = TRUE,
        fontsize = 6)

sol_y$PIchart

pimplot(data = SF5,
        outcome = "Y",
        results = sol_y,
        all_labels = TRUE,
        jitter = TRUE,
        fontsize = 6)


QCAradar(results = sol_y, 
         outcome = "Y",
         fit = FALSE)


# Building hypothetical example Pajunen, for model ambiguity

modambig <- read.csv("modambig.csv", row.names = 1)
head(modambig)

# truth table
tt_ny <- truthTable(paj, outcome='~Y',
                    conditions =c("X","A","C"), 
                    incl.cut=0.8,
                    pri.cut = 0.52,
                    sort.by = c("incl","n"),
                    show.cases = FALSE,
                    complete = TRUE)
tt_ny


stargazerTT(tt_ny, show.cases = FALSE)

# Sufficient conservative solution, with parameters of fit

sol_ny <- minimize(tt_ny)
sol_ny

sol_ny$PIchart


#### 2.1 SA and ESA - Student Example, full dataset ####

# Load data from SetMethods:

data(STUF)
head(STUF)
names(STUF) <- c("X", "A", "C", "D", "E","Y")
# ~Y = not arriving in time for practice

# X = living close by
# A = arriving by bike instead of in parents' car
# C = from family with punctuality as a norm
# D = seasoned player
# E = goalkeeper

conds <- c("X", "A", "C", "D", "E")


#### 2.1.1 Analyis of outcome ~Y ####


####  Truth table ~Y #### 
TT_ny <- truthTable(data = STUF,
                       outcome  = "~Y",
                       conditions = conds,
                       incl.cut = 0.8,
                       n.cut = 2,
                       pri.cut = 0.5,
                       sort.by = c("OUT", "incl"),
                       complete = TRUE)
TT_ny

stargazerTT(TT_ny, show.cases = FALSE)


# Note ttrow 25: it has the negated necessary 
# condition (X*A), yet is included in minimization 
# This creates a sufficient term without nec cond
# which still stays in even til the end of ESA



####  Minimization ~Y ####

# Produce the conservative solution

sol_nyc <- minimize(TT_ny,
                       details = TRUE)
sol_nyc

# term 5 with negated nec cond

# Produce the most parsimonious solution

sol_nyp <- minimize(TT_ny,
                       details = TRUE,
                       include = "?",
                       row.dom = TRUE)
sol_nyp

# two models if row.dom = TRUE is not used
# term 2 without nec cond


# Check the PI chart
sol_nyp$PIchart


# Check the simplifying assumptions
sol_nyp$SA

# three rows contradict statement of necessity (27, 29, 31)

# Plot the most parsimonious solution

pimplot(data = STUF,
        results = sol_nyp,
        outcome = "~Y",
        jitter = TRUE)


# Produce the intermediate solution with the following
# direcional expectations 0, 0, 0, 1, 0

sol_nyi <- minimize(TT_ny,
                       details = TRUE,
                       include = "?",
                       dir.exp = "~X, ~A, ~C, D, ~E",
                       row.dom = TRUE)
sol_nyi

# term 3 without nec cond

# Check the easy counterfactuals

sol_nyi$i.sol$C1P1$EC

# one row contradicts statement of necessity (27)


#### 2.1.2 Analyis of outcome Y ####


####  Truth table Y #### 

TT_y <- truthTable(data = STUF,
                      outcome  = "Y",
                      conditions = conds,
                      incl.cut = 0.8,
                      n.cut = 2,
                      pri.cut = 0.45,
                      sort.by = c("OUT", "incl"),
                      complete = TRUE,
                      show.cases = FALSE)
TT_y

# for presentational purposes, we set PRI to 0.45 in order 
# to include ttrow 24 into analysis of Y. Else the 
# results become too simple for Y 


####  Minimization Y #### 

# Produce the conservative solution

sol_yc <- minimize(TT_y, 
                   details = TRUE)
sol_yc

# Produce the most parsimonious solution

sol_yp <- minimize(TT_y, 
                      details = TRUE, 
                      include = "?",
                      row.dom = TRUE)
sol_yp


# model ambiguity (2 models, despite row dom)
# Use M1

# Check the simplifying assumptions

sol_yp$SA


# Produce the intermediate solution with the following 
# directional expactations 1, 1, 1, 0, 1

sol_yi <- minimize(TT_y, 
                      details = TRUE, 
                      include = "?",
                      dir.exp = c(1, 1, 1, 0, 1))
sol_yi

# Check the easy counterfactuals

sol_yi$i.sol$C1P1$EC
sol_yi$i.sol$C1P2$EC

# just one easy counterfactual


#### 2.1.3 ESA for ~Y ####


#### Contradictory assumptions (most parsimonious) #### 

CSA <- intersect(rownames(sol_nyp$SA$M1), rownames(sol_yp$SA$M1))
CSA

LR.intersect(sol_nyp, sol_yp)
# 23 and 31


# for the intermediate it would be
CSAi <- intersect(rownames(sol_nyi$i.sol$C1P1$EC), rownames(sol_yi$i.sol$C1P1$EC))
CSAi

LR.intersect(sol_nyi, sol_yi)

# no contradictory easy counterfactuals. IS for Y only uses one EC (row 26)


TT_nyesa <- esa(oldtt =  TT_ny,
               contrad_rows = c(CSA))
TT_nyesa


####  Simultaneous subset relations #### 

SSR<-intersect(rownames(TT_ny$tt)[TT_ny$tt$OUT==1],rownames(TT_y$tt)[TT_y$tt$OUT==1])
SSR

# because we set PRI at 0.5, no simultaneous subset relations occur
# If there are, they can be excluded with the contrad_rows argument



####  Contradicting statement of necessity #### 


# analysis of necessity ~Y
SUIN_ny <- superSubset(data = STUF,
                       outcome  = "~Y",
                       conditions = conds,
                       incl.cut = 0.906,
                       cov.cut = 0.8,
                       ron.cut = 0.8,
                       depth = 2)
SUIN_ny

# ~X+~A
# not living close by or not arriving by bike are SUIN conditions
# oerarching concept: being from another town is necessary for arriving late

pimplot(data = STUF,
         outcome = "Y",
         results = SUIN_ny,
         necessity = TRUE)


sol_nyp$SA
# three remainders contradict (27, 29, 31)
# 31 has already been taken care of with CSA
# there is also observed OUT=1 row 25 contradicting 
# necessity claim. Let's exclude that via the contrad_rows
# command. Ultimately, observed rows should also be deleted 
# via the nec_cond argument

TT_nyesa <- esa(TT_ny,
                contrad_rows = c(CSA),
                nec_cond = "~X+~A")
TT_nyesa


####  Impossible remainders #### 

TT_nyesa <- esa(TT_ny,
                contrad_rows = c(CSA, '25'),
                nec_cond = "~X+~A",
                untenable_LR = "~X*A*~C*~D")

TT_nyesa

stargazerTT(TT_nyesa)

# After excluding all remainders that would yield untenable remainders
# we are still left with 10 logical remainder rows available


####  Enhanced solutions ~ Y #### 

# conservative - needed because we excluded the observed tt row 25
sol_nycesa <- minimize(TT_nyesa,
                       details = TRUE)
sol_nycesa


# PS
sol_nypesa <- minimize(TT_nyesa,
                       details = TRUE,
                       include = "?",
                       row.dom = TRUE)
sol_nypesa

sol_nypesa$SA

# not contradicting nec claim, no CSA, no impossible remainder assumptions


# intermediate ~Y
# directional expectations 0, 0, 0, 1, 0

sol_nyiesa <- minimize(TT_nyesa,
                       details = TRUE,
                       include = "?",
                       dir.exp = "~X, ~A, ~C, D, ~E")
sol_nyiesa

# Check the easy counterfactuals

sol_nyiesa$i.sol$C1P1$EC

# different from PS
# no untenable assumptions

# assumptions dropped = untenable assumptions

untenable_EC <- LR.intersect(sol_nyi,sol_nyiesa)
untenable_EC


