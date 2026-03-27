## Replication file for main results of 
## "The impact of the Black Lives Matter movement on missions and activities of environmental NGOs, 2011-2023"
## By Elizabeth Echavarría, Joannie Tremblay-Boire, Nives Dolšak, and Aseem Prakash
## May 31st, 2025

# Setup
rm(list = ls()) # clear the environment

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set current folder as your wd.

# Libraries

library(tidyverse)
library(MASS)         # For mvrnorm() and polr()
library(magrittr)
library(simcf)        # For extractdata() and lagpanel(); installable from included package archives
library(tile)         # Data visualization; installable from included package archives
library(RColorBrewer) # Data visualization
library(patchwork)    # Data visualization
library(ggstance)
library(plyr)
library(dplyr)
library(tidyr)
library(texreg)       # For tables
library(kableExtra)   # For tables
library(arsenal)      # For tables
library(nnet)         # For robustness check (multinomial model)

#source("support/theme_caviz.R")

# Lading data
engos11 <- read.csv("data/engos11.csv", header = TRUE, sep= "," , dec = ".", 
                    stringsAsFactors=F)

engos17 <- read.csv("data/engos17.csv", header = TRUE, sep= "," , dec = ".", 
                    stringsAsFactors=F)

engos23 <- read.csv("data/engos23.csv", header = TRUE, sep= "," , dec = ".", 
                    stringsAsFactors=F)

# ----------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
## TABLE 2
# wrangling
data_desc <- full_join(engos11, engos17)
data_desc <- full_join(data_desc, engos23)

data_desc$issuean <- as.integer(data_desc$issuean)

# transform revenue in revenue in thousands $
data_desc$rev_thou <- data_desc$revenue/1000

data_desc <- data_desc %>%
  mutate(scope_labels = case_when(scope == 0 ~ "Subnational",
                                  scope == 1 ~ "National",
                                  scope == 2 ~ "International"))

labels(data_desc) <- c(rev_thou = 'Revenue (x1000)',
                    age = 'Age',
                    issuean = 'N. Issue Areas',
                    scope_labels = 'Geog. Scope')

data_desc$group <- "All Data"

# Desc stats table
desctable <- tableby(group ~ rev_thou + age + issuean + scope_labels, 
                     data=data_desc, 
                     strata = year,
                     digits = 1, 
                     total = FALSE
)

summary(desctable, text = TRUE, title = "Descriptive Statistics Numeric Variables")

# ----
## TABLE 3: Issue areas distribution
# wrangle data
issue_cols <- c("year", "decarb", "land", "freshwater", "oceans", "wildlife", 
                "flora_plants", "sust", "energy", "food", "recycl", "garbage", 
                "transport", "recr", "health")

df_issues <- data_desc[, issue_cols]

summary_table <- df_issues %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(across(everything(), ~sum(. == 1, na.rm = TRUE)))

names(summary_table) <- c("Year", "Decarbonization", "Land", "Freshwater", "Oceans", "Wildlife", 
                          "Flora / Plants", "Sustainability", "Energy", "Food", "Recycling", 
                          "Garbage", "Transportation", "Recreation", "Health")

long_tbl <- summary_table %>%
  pivot_longer(
    cols = -Year,
    names_to = "issue_area",
    values_to = "ngo_count"
  )

final_tbl <- long_tbl %>%
  pivot_wider(
    names_from = Year,
    values_from = ngo_count
  )

print(final_tbl)

# ----
## FIGURE 1
visuals <- data_desc %>%
  dplyr::select(ein, taxpayer_name, year, dim_1_mod, dim_2_mod) %>%
  pivot_longer(cols = c(dim_1_mod, dim_2_mod),
               names_to = "Dimension",
               values_to = "Level") %>%
  dplyr::group_by(year) %>%
  dplyr::count(Dimension, Level)

distrib <-
  ggplot(visuals, aes(fill = factor(Level,
                                    levels = c("0 = No integration",
                                               "1 = As mission consolidation",
                                               "2 = As mission shift",
                                               "1 = Performative integration",
                                               "2 = Substantive integration")), 
                      y=n, x=as.factor(year), label=n)) +
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "#3e3838") +
  facet_wrap(~factor(Dimension, levels = c('dim_1_mod', 'dim_2_mod')),
             labeller = as_labeller(c(dim_2_mod='Dimension 2',
                                      dim_1_mod='Dimension 1'))) +
  scale_fill_manual(values = c("grey",
                               "lightblue",
                               "blue",
                               "lightgreen",
                               "#009632"
  )) +
  labs(fill="Level", 
       title = "Figure 1. Distribution of ENGOS across integration dimensions") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

distrib

# ----
## FIGURE 2. 
## Panel a. POLL DATA
survey <- read.csv("data/Surveydata_b.csv")

pollfig <-
  ggplot(data = subset(survey, !is.na(Percentage)), aes(x=Date, y=Percentage)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(limits = c(20, 70)) +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10),
                       labels = c("1988", "1992", "1995", "1996", "1997", "2012",
                                  "2013", "2014", "2020", "2021")) +
    theme_minimal() +
    labs(title = "A. Percentage that believe Black people do not receive \nequal treatment in criminal justice law. \n(ABC News / Washington Post / AP)") +
    geom_vline(xintercept = c(7, 9), linetype = "dashed", color="darkgrey") +
    annotate("text", x=7, y=65,
             label="First use \nof #BLM.",
             size = 3.5,
             hjust = 1,
             fontface = "italic") +
    annotate("text", x=9, y=40,
             label = "George Floyd \nMurder. \nBLM Support \nPeaks.",
             size = 3.5,
             hjust = 1,
             fontface = "italic") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 0.1))

## Panel b. GOOGLE TRENDS 
trend_data <- read.csv("data/multiTimeline.csv")

trend_data$month <- as.Date(paste0(trend_data$month, '-01'))

trendfig <-
  ggplot(trend_data, aes(x = month, y = score)) +
    geom_line() +
    geom_point() +
    labs(title = 'B. Google Trend Score Over Time "BLM"', y = 'Score') +
    annotate("text", y = 95, x = as.Date('2020-06-01'),
             label = "June 2020. \nBLM Support \nPeaks.",
             size = 3.5,
             hjust = 1,
             fontface = "italic") +
    coord_cartesian(xlim = c(as.Date('2013-01-01'), as.Date('2023-12-01'))) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90, hjust = 0.1),
          axis.title.x = element_blank()) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")

## Combining both
fig2 <-
  pollfig + trendfig
  

# ----------------------------------------------------------------------------
# ORDERED LOGIT MODELS
## Specifications
### DIM 1: no -> consol -> drift
m1 <- dim_1 ~ age + issuean + scope + revenue_log1

### DIM 2: no -> perf -> subst
m2 <- dim_2 ~ age + issuean + scope + revenue_log1

# extracting data for later with un-modified DVs
mdata1 <- extractdata(m1, engos11, na.rm = TRUE)
mdata2 <- extractdata(m1, engos17, na.rm = TRUE)
mdata3 <- extractdata(m1, engos23, na.rm = TRUE)
mdata4 <- extractdata(m2, engos11, na.rm = TRUE)
mdata5 <- extractdata(m2, engos17, na.rm = TRUE)
mdata6 <- extractdata(m2, engos23, na.rm = TRUE)


# converting DVs to factor for polr
engos11 <- engos11 %>%
  mutate_at(c('dim_1', 'dim_2'), as.factor)

engos17 <- engos17 %>%
  mutate_at(c('dim_1', 'dim_2'), as.factor)

engos23 <- engos23 %>%
  mutate_at(c('dim_1', 'dim_2'), as.factor)

# Estimation using polr package to create TABLE 4.
glm.m1 <- polr(m1,
               data = engos11,
               method = "probit",
               na.action = na.omit,
               Hess = TRUE)

glm.m2 <- polr(m1,
               data = engos17,
               method = "probit",
               na.action = na.omit,
               Hess = TRUE)

glm.m3 <- polr(m1,
               data = engos23,
               method = "probit",
               na.action = na.omit,
               Hess = TRUE)

glm.m4 <- polr(m2,
               data = engos11,
               method = "probit",
               na.action = na.omit,
               Hess = TRUE)

glm.m5 <- polr(m2,
               data = engos17,
               method = "probit",
               na.action = na.omit,
               Hess = TRUE)

glm.m6 <- polr(m2,
               data = engos23,
               method = "probit",
               na.action = na.omit,
               Hess = TRUE)

# Results here are found in TABLE 4
screenreg(list(glm.m1, glm.m2, glm.m3, glm.m4, glm.m5, glm.m6))

# ----------------------------------
# Now we use an optimizer function to directly obtain likelihoods and prepare FIGURES 3 and 4

## Likelihood function for 3 category ordered probit
llk.oprobit3 <- function(param, x, y) {
  # preliminaries
  os <- rep(1, nrow(x))
  x <- cbind(os, x)  
  b <- param[1:ncol(x)]
  t2 <- param[(ncol(x)+1)]
  
  # probabilities and penalty function
  xb <- x%*%b
  p1 <- log(pnorm(-xb))
  if (t2<=0)  p2 <- -(abs(t2)*10000)    # penalty function to keep t2>0
  else p2 <- log(pnorm(t2-xb)-pnorm(-xb))
  p3 <- log(1-pnorm(t2-xb)) 

  
  # -1 * log likelihood (optim is a minimizer)
  -sum(cbind(y==0,y==1,y==2) * cbind(p1,p2,p3))
}

# ----------------
# GETTING DATA FOR FIGURE 3
# M1, M2, M3 (Dimension 1, 2011, 2017, 2023)
# nothing = 0, mission consolidation (SJ Env) = 1, mission expansion (SJ No Env) = 2.

# M1 (2011)
y <- mdata1$dim_1
x <- mdata1 %>% dplyr::select(age, issuean, scope, 
                              revenue_log1) %>% as.matrix()

ls.result1 <- lm(m1, data=mdata1) # use ls estimates as starting values

stval1 <- c(coef(ls.result1),1)  # initial guesses
oprobit.m1 <- optim(stval1, 
                    llk.oprobit3, 
                    method="BFGS", 
                    x=x, 
                    y=y, 
                    hessian=TRUE)

pe_m1 <- oprobit.m1$par                # point estimates
vc_m1 <- solve(oprobit.m1$hessian)     # var-cov matrix
se_m1 <- sqrt(diag(vc_m1))             # standard errors
ll_m1 <- -oprobit.m1$value             # likelihood at maximum

# M2 (2017)
y <- mdata2$dim_1
x <- mdata2 %>% dplyr::select(age, issuean, scope, 
                              revenue_log1) %>% as.matrix()

ls.result2 <- lm(m1, data=mdata2) 

stval2 <- c(coef(ls.result2),1)
oprobit.m2 <- optim(stval2, 
                    llk.oprobit3, 
                    method="BFGS", 
                    x=x, 
                    y=y, 
                    hessian=TRUE)

pe_m2 <- oprobit.m2$par                
vc_m2 <- solve(oprobit.m2$hessian)     
se_m2 <- sqrt(diag(vc_m2))             
ll_m2 <- -oprobit.m2$value             

# M3 (2023)
y <- mdata3$dim_1
x <- mdata3 %>% dplyr::select(age, issuean, scope, 
                              revenue_log1) %>% as.matrix()

ls.result3 <- lm(m1, data=mdata3) 

stval3 <- c(coef(ls.result3),1)
oprobit.m3 <- optim(stval3, 
                    llk.oprobit3, 
                    method="BFGS", 
                    x=x, 
                    y=y, 
                    hessian=TRUE)

pe_m3 <- oprobit.m3$par                
vc_m3 <- solve(oprobit.m3$hessian)     
se_m3 <- sqrt(diag(vc_m3))                 
ll_m3 <- -oprobit.m3$value

# First differences plot (Fig 3)
# showing change in probability of SJ integration 
# Create example counterfactuals -- for diffs
sims <- 10000

xhyp <- cfMake(m1, mdata1, nscen=5)

xhyp <- cfName(xhyp, "Consolidated (35) / Young (5)", scen=1)
xhyp <- cfChange(xhyp, "age",
                 x=35,
                 xpre=5,
                 scen=1)

xhyp <- cfName(xhyp, "Generalist (3 issue areas) / Specialist (1)", scen=2)
xhyp <- cfChange(xhyp, "issuean",
                 x=3,
                 xpre=1,
                 scen=2)

xhyp <- cfName(xhyp, "National / Subnational", scen=3)
xhyp <- cfChange(xhyp, "scope", x=1, xpre=0, scen=3)

xhyp <- cfName(xhyp, "International / Subnational", scen=4)
xhyp <- cfChange(xhyp, "scope", x=2, xpre=0, scen=4)

xhyp <- cfName(xhyp, "Large revenue (+ 1 sd) / Mean", scen=5)
xhyp <- cfChange(xhyp, "revenue_log1",
                 x=mean(na.omit(mdata1$revenue_log1))+sd(na.omit(mdata1$revenue_log1)),
                 xpre=mean(na.omit(mdata1$revenue_log1)),
                 scen=5)

# draw parameters from predictive distributions M1
simbetas1 <- mvrnorm(sims, pe_m1, vc_m1)

# Simulate first differences (all three categories) M1
oprobit.fd1 <- oprobitsimfd(xhyp, simbetas1, cat=3)

FDs_M1 <- as.data.frame(oprobit.fd1$pe)
FDs_M1$var <- row.names(xhyp$x)
FDs_M1 <- FDs_M1 %>%
  dplyr::select(var, everything())


# draw parameters from predictive distributions M2
simbetas2 <- mvrnorm(sims, pe_m2, vc_m2) 

# Simulate first differences (all three categories) M2
oprobit.fd2 <- oprobitsimfd(xhyp, simbetas2, cat=3)

FDs_M2 <- as.data.frame(oprobit.fd2$pe)
FDs_M2$var <- row.names(xhyp$x)
FDs_M2 <- FDs_M2 %>%
  dplyr::select(var, everything())

# draw parameters from predictive distributions M3
simbetas3 <- mvrnorm(sims, pe_m3, vc_m3) 

# Simulate first differences (all three categories) M3
oprobit.fd3 <- oprobitsimfd(xhyp, simbetas3, cat=3)

FDs_M3 <- as.data.frame(oprobit.fd3$pe)
FDs_M3$var <- row.names(xhyp$x)
FDs_M3 <- FDs_M3 %>%
  dplyr::select(var, everything())


# Tile

## Nice colors
brewer <- brewer.pal(9, "Set1")
red <- brewer[1]
blue <- brewer[2]
green <- brewer[3]
purple <- brewer[4]
orange <- brewer[5]
nicegray <- "gray45"

sortedc <- rev(order(oprobit.fd1$pe[,2]))
scenNames <- row.names(xhyp$x)

trace1 <- ropeladder(x = oprobit.fd1$pe[sortedc,2],
                     lower = oprobit.fd1$lower[sortedc,2],
                     upper = oprobit.fd1$upper[sortedc,2],
                     labels = scenNames[sortedc],
                     sublabels="2011",
                     sublabelsyoffset=0.04,
                     col=orange,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     plot=1
)

trace2 <- ropeladder(x = oprobit.fd2$pe[sortedc,2],
                     lower = oprobit.fd2$lower[sortedc,2],
                     upper = oprobit.fd2$upper[sortedc,2],
                     labels = scenNames[sortedc],
                     sublabels = "2017",
                     sublabelsyoffset = 0.04,
                     sublabelsxoffset = 0.1,
                     col=blue,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     entryheight=0.40,
                     subentryheight=.8,
                     plot=1
)

trace3 <- ropeladder(x = oprobit.fd3$pe[sortedc,2],
                     lower = oprobit.fd3$lower[sortedc,2],
                     upper = oprobit.fd3$upper[sortedc,2],
                     labels = scenNames[sortedc],
                     sublabels = "2023",
                     sublabelsyoffset = -0.04,
                     col=green,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     entryheight=0.40,
                     subentryheight=.8,
                     plot=1
)

trace4 <- ropeladder(x = oprobit.fd1$pe[sortedc,3],
                     lower = oprobit.fd1$lower[sortedc,3],
                     upper = oprobit.fd1$upper[sortedc,3],
                     labels = scenNames[sortedc],
                     sublabels="2011",
                     sublabelsyoffset=0.04,
                     col=orange,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     plot=1
)

trace5 <- ropeladder(x = oprobit.fd2$pe[sortedc,3],
                     lower = oprobit.fd2$lower[sortedc,3],
                     upper = oprobit.fd2$upper[sortedc,3],
                     labels = scenNames[sortedc],
                     sublabels = "2017",
                     sublabelsyoffset = 0.04,
                     sublabelsxoffset = -0.1,
                     col=blue,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     entryheight=0.40,
                     subentryheight=.8,
                     plot=1
)

trace6 <- ropeladder(x = oprobit.fd3$pe[sortedc,3],
                     lower = oprobit.fd3$lower[sortedc,3],
                     upper = oprobit.fd3$upper[sortedc,3],
                     labels = scenNames[sortedc],
                     sublabels = "2023",
                     sublabelsyoffset = -0.04,
                     col=green,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     entryheight=0.40,
                     subentryheight=.8,
                     plot=1
)

sigMark1 <- oprobit.fd1$pe[sortedc,2]
is.na(sigMark1) <- (oprobit.fd1$lower[sortedc,2]>0)
traceSig1 <- ropeladder(x=sigMark1,
                        col="white",
                        group=1,
                        plot=1)

sigMark2 <- oprobit.fd2$pe[sortedc,2]
is.na(sigMark2) <- (oprobit.fd2$lower[sortedc,2]>0)
traceSig2 <- ropeladder(x=sigMark2,
                        col="white",
                        group=2,
                        plot=1)

sigMark3 <- oprobit.fd3$pe[sortedc,2]
is.na(sigMark3) <- (oprobit.fd3$lower[sortedc,2]>0)
traceSig3 <- ropeladder(x=sigMark3,
                        col="white",
                        group=3,
                        plot=1)

sigMark4 <- oprobit.fd1$pe[sortedc,3]
is.na(sigMark4) <- (oprobit.fd1$lower[sortedc,3]>0)
traceSig4 <- ropeladder(x=sigMark4,
                        col="white",
                        group=1,
                        plot=1)

sigMark5 <- oprobit.fd2$pe[sortedc,3]
is.na(sigMark5) <- (oprobit.fd2$lower[sortedc,3]>0)
traceSig5 <- ropeladder(x=sigMark5,
                        col="white",
                        group=2,
                        plot=1)

sigMark6 <- oprobit.fd3$pe[sortedc,3]
is.na(sigMark6) <- (oprobit.fd3$lower[sortedc,3]>0)
traceSig6 <- ropeladder(x=sigMark6,
                        col="white",
                        group=3,
                        plot=1)

vertmark <- linesTile(x=c(0,0), y=c(0,1), plot=1)

# FIGURE 3 COMBINES THE TWO PLOTS THAT RESULT FROM THE CODE BELOW
file <- "first_diffs_dim_1_level_1"
tile(trace1, trace2, trace3, vertmark, traceSig1, traceSig2, traceSig3,
     limits=c(-0.18,0.25),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, at=seq(from=-0.10, to=0.25, by=0.05),
                  labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxis=list(at=seq(from=-0.10, to=0.25, by=0.05), 
                labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxistitle=list(labels="difference in probability of mission consol. (1)"),
     topaxistitle=list(labels="difference in probability of mission consol. (1)"),
     plottitle=list(labels="Integration of SJ"),
     width=list(plot=2),
     height=list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5),
     output=list(outfile=file, width=6.75)
)


file <- "first_diffs_dim_1_level_2"
tile(trace4, trace5, trace6, vertmark, traceSig4, traceSig5, traceSig6,
     limits=c(-0.18,0.25),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, at=seq(from=-0.10, to=0.25, by=0.05),
                  labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxis=list(at=seq(from=-0.10, to=0.25, by=0.05), 
                labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxistitle=list(labels="difference in probability of mission drift (2)"),
     topaxistitle=list(labels="difference in probability of mission drift (2)"),
     plottitle=list(labels="Integration of SJ"),
     width=list(plot=2),
     height=list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5),
     output=list(outfile=file, width=6.75)
)


# -----------------------------------------------------------------------------
# GETTING DATA FOR FIGURE 4
# M4, M5, M6 (Dimension 2, 2011, 2017, 2023)
# nothing = 0, performative = 1, substantive = 2.

# M4 (2011)
y <- mdata4$dim_2
x <- mdata4 %>% dplyr::select(age, issuean, scope, 
                                revenue_log1) %>% as.matrix()

ls.result4 <- lm(m2, data=mdata4) 

stval4 <- c(coef(ls.result4),1)
oprobit.m4 <- optim(stval4, 
                     llk.oprobit3, 
                     method="BFGS", 
                     x=x, 
                     y=y, 
                     hessian=TRUE)

pe_m4 <- oprobit.m4$par                
vc_m4 <- solve(oprobit.m4$hessian)     
se_m4 <- sqrt(diag(vc_m4))                 
ll_m4 <- -oprobit.m4$value

# -----
# M5 (2017)
y <- mdata5$dim_2
x <- mdata5 %>% dplyr::select(age, issuean, scope, 
                              revenue_log1) %>% as.matrix()

ls.result5 <- lm(m2, data=mdata5)

stval5 <- c(coef(ls.result5),1)
oprobit.m5 <- optim(stval5, 
                    llk.oprobit3, 
                    method="BFGS", 
                    x=x, 
                    y=y, 
                    hessian=TRUE)

pe_m5 <- oprobit.m5$par                
vc_m5 <- solve(oprobit.m5$hessian)     
se_m5 <- sqrt(diag(vc_m5))             
ll_m5 <- -oprobit.m5$value

# ------
# M6 (2023)
y <- mdata6$dim_2
x <- mdata6 %>% dplyr::select(age, issuean, scope, 
                              revenue_log1) %>% as.matrix()

ls.result6 <- lm(m2, data=mdata6) 

stval6 <- c(coef(ls.result6),1)
oprobit.m6 <- optim(stval6, 
                    llk.oprobit3, 
                    method="BFGS", 
                    x=x, 
                    y=y, 
                    hessian=TRUE)

pe_m6 <- oprobit.m6$par                
vc_m6 <- solve(oprobit.m6$hessian)     
se_m6 <- sqrt(diag(vc_m6))                 
ll_m6 <- -oprobit.m6$value

# First differences plot 
# showing change in probability of integration 
# Create example counterfactuals -- for diffs
xhyp <- cfMake(m2, mdata4, nscen=5)

xhyp <- cfName(xhyp, "Consolidated (35) / Young (5)", scen=1)
xhyp <- cfChange(xhyp, "age",
                 x=35,
                 xpre=5,
                 scen=1)

xhyp <- cfName(xhyp, "Generalist (3 issue areas) / Specialist (1)", scen=2)
xhyp <- cfChange(xhyp, "issuean",
                 x=3,
                 xpre=1,
                 scen=2)

xhyp <- cfName(xhyp, "National / Subnational", scen=3)
xhyp <- cfChange(xhyp, "scope", x=1, xpre=0, scen=3)

xhyp <- cfName(xhyp, "International / Subnational", scen=4)
xhyp <- cfChange(xhyp, "scope", x=2, xpre=0, scen=4)

xhyp <- cfName(xhyp, "Large revenue (+ 1 sd) / Mean", scen=5)
xhyp <- cfChange(xhyp, "revenue_log1",
                 x=mean(na.omit(mdata1$revenue_log1))+sd(na.omit(mdata1$revenue_log1)),
                 xpre=mean(na.omit(mdata1$revenue_log1)),
                 scen=5)

# draw parameters from predictive distributions M4
simbetas4 <- mvrnorm(sims, pe_m4, vc_m4) 

# Simulate first differences (all three categories) M4
oprobit.fd4 <- oprobitsimfd(xhyp, simbetas4, cat=3)

FDs_M4 <- as.data.frame(oprobit.fd4$pe)
FDs_M4$var <- row.names(xhyp$x)
FDs_M4 <- FDs_M4 %>%
  dplyr::select(var, everything())


# draw parameters from predictive distributions M5
simbetas5 <- mvrnorm(sims, pe_m5, vc_m5) 

# Simulate first differences (all three categories) M5
oprobit.fd5 <- oprobitsimfd(xhyp, simbetas5, cat=3)

FDs_M5 <- as.data.frame(oprobit.fd5$pe)
FDs_M5$var <- row.names(xhyp$x)
FDs_M5 <- FDs_M5 %>%
  dplyr::select(var, everything())


# draw parameters from predictive distributions M6
simbetas6 <- mvrnorm(sims, pe_m6, vc_m6) 

# Simulate first differences (all three categories) M6
oprobit.fd6 <- oprobitsimfd(xhyp, simbetas6, cat=3)

FDs_M6 <- as.data.frame(oprobit.fd6$pe)
FDs_M6$var <- row.names(xhyp$x)
FDs_M6 <- FDs_M6 %>%
  dplyr::select(var, everything())

# --------
# Tile ropeladder plot
sortedc <- rev(order(oprobit.fd4$pe[,2]))
scenNames <- row.names(xhyp$x)

trace1 <- ropeladder(x = oprobit.fd4$pe[sortedc,2],
                      lower = oprobit.fd4$lower[sortedc,2],
                      upper = oprobit.fd4$upper[sortedc,2],
                      labels = scenNames[sortedc],
                      sublabels="2011",
                      sublabelsyoffset=0.04,
                      col=orange,
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

trace2 <- ropeladder(x = oprobit.fd5$pe[sortedc,2],
                      lower = oprobit.fd5$lower[sortedc,2],
                      upper = oprobit.fd5$upper[sortedc,2],
                      labels = scenNames[sortedc],
                      sublabels = "2017",
                      sublabelsyoffset = 0.04,
                      sublabelsxoffset = 0.1,
                      col=blue,
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      entryheight=0.40,
                      subentryheight=.8,
                      plot=1
)

trace3 <- ropeladder(x = oprobit.fd6$pe[sortedc,2],
                     lower = oprobit.fd6$lower[sortedc,2],
                     upper = oprobit.fd6$upper[sortedc,2],
                     labels = scenNames[sortedc],
                     sublabels = "2023",
                     sublabelsyoffset = -0.04,
                     col=green,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     entryheight=0.40,
                     subentryheight=.8,
                     plot=1
)

trace4 <- ropeladder(x = oprobit.fd4$pe[sortedc,3],
                     lower = oprobit.fd4$lower[sortedc,3],
                     upper = oprobit.fd4$upper[sortedc,3],
                     labels = scenNames[sortedc],
                     sublabels="2011",
                     sublabelsyoffset=0.04,
                     col=orange,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     plot=1
)

trace5 <- ropeladder(x = oprobit.fd5$pe[sortedc,3],
                     lower = oprobit.fd5$lower[sortedc,3],
                     upper = oprobit.fd5$upper[sortedc,3],
                     labels = scenNames[sortedc],
                     sublabels = "2017",
                     sublabelsyoffset = 0.04,
                     sublabelsxoffset = -0.1,
                     col=blue,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     entryheight=0.40,
                     subentryheight=.8,
                     plot=1
)

trace6 <- ropeladder(x = oprobit.fd6$pe[sortedc,3],
                     lower = oprobit.fd6$lower[sortedc,3],
                     upper = oprobit.fd6$upper[sortedc,3],
                     labels = scenNames[sortedc],
                     sublabels = "2023",
                     sublabelsyoffset = -0.04,
                     col=green,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     entryheight=0.40,
                     subentryheight=.8,
                     plot=1
)

sigMark1 <- oprobit.fd4$pe[sortedc,2]
is.na(sigMark1) <- (oprobit.fd4$lower[sortedc,2]>0)
traceSig1 <- ropeladder(x=sigMark1,
                        col="white",
                        group=1,
                        plot=1)

sigMark2 <- oprobit.fd5$pe[sortedc,2]
is.na(sigMark2) <- (oprobit.fd5$lower[sortedc,2]>0)
traceSig2 <- ropeladder(x=sigMark2,
                        col="white",
                        group=2,
                        plot=1)

sigMark3 <- oprobit.fd6$pe[sortedc,2]
is.na(sigMark3) <- (oprobit.fd6$lower[sortedc,2]>0)
traceSig3 <- ropeladder(x=sigMark3,
                        col="white",
                        group=3,
                        plot=1)

sigMark4 <- oprobit.fd4$pe[sortedc,3]
is.na(sigMark4) <- (oprobit.fd4$lower[sortedc,3]>0)
traceSig4 <- ropeladder(x=sigMark4,
                        col="white",
                        group=1,
                        plot=1)

sigMark5 <- oprobit.fd5$pe[sortedc,3]
is.na(sigMark5) <- (oprobit.fd5$lower[sortedc,3]>0)
traceSig5 <- ropeladder(x=sigMark5,
                        col="white",
                        group=2,
                        plot=1)

sigMark6 <- oprobit.fd6$pe[sortedc,3]
is.na(sigMark6) <- (oprobit.fd6$lower[sortedc,3]>0)
traceSig6 <- ropeladder(x=sigMark6,
                        col="white",
                        group=3,
                        plot=1)

vertmark <- linesTile(x=c(0,0), y=c(0,1), plot=1)

# FIGURE 4 COMBINES THE TWO PLOTS THAT RESULT FROM THE CODE BELOW
file <- "first_diffs_dim_2_level_1"
tile(trace1, trace2, trace3, vertmark, traceSig1, traceSig2, traceSig3,
     limits=c(-0.18,0.3),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, at=seq(from=-0.10, to=0.25, by=0.05),
                  labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxis=list(at=seq(from=-0.10, to=0.25, by=0.05), 
                labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxistitle=list(labels="difference in probability of perf. integration"),
     topaxistitle=list(labels="difference in probability of perf. integration"),
     plottitle=list(labels="Integration of SJ"),
     width=list(plot=2),
     height=list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5),
     output=list(outfile=file, width=6.75)
)


file <- "first_diffs_dim_2_level_2"
tile(trace4, trace5, trace6, vertmark, traceSig4, traceSig5, traceSig6,
     limits=c(-0.18,0.3),
     gridlines=list(type="xt"),
     topaxis=list(add=TRUE, at=seq(from=-0.10, to=0.25, by=0.05),
                  labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxis=list(at=seq(from=-0.10, to=0.25, by=0.05), 
                labels=c("-10%" ,"-5%" ,"0%", "+5%", "+10%", "+15%", "+20%", " ")),
     xaxistitle=list(labels="difference in probability of subst. integration"),
     topaxistitle=list(labels="difference in probability of subst. integration"),
     plottitle=list(labels="Integration of SJ"),
     width=list(plot=2),
     height=list(plottitle=3,xaxistitle=3.5,topaxistitle=3.5),
     output=list(outfile=file, width=6.75)
)

## ---------------------------------------------------------------------------
# Robustness check
# Multinomial model

# Establishing reference categories in all six dataframes using categorical vars
engos11 <-
  engos11 %>%
  dplyr::mutate(dim_1_mod = factor(dim_1_mod), 
         dim_1_mod = relevel(dim_1_mod, ref="0 = No integration"))

engos11 <-
  engos11 %>%
  dplyr::mutate(dim_2_mod = factor(dim_2_mod), 
         dim_2_mod = relevel(dim_2_mod, ref="0 = No integration"))

engos17 <-
  engos17 %>%
  dplyr::mutate(dim_1_mod = factor(dim_1_mod), 
         dim_1_mod = relevel(dim_1_mod, ref="0 = No integration"))

engos17 <-
  engos17 %>%
  dplyr::mutate(dim_2_mod = factor(dim_2_mod),
         dim_2_mod = relevel(dim_2_mod, ref="0 = No integration"))

engos23 <-
  engos23 %>%
  dplyr::mutate(dim_1_mod = factor(dim_1_mod), 
         dim_1_mod = relevel(dim_1_mod, ref="0 = No integration"))

engos23 <-
  engos23 %>%
  dplyr::mutate(dim_2_mod = factor(dim_2_mod),
         dim_2_mod = relevel(dim_2_mod, ref="0 = No integration"))


# Estimating MNL using the nnet library
## Specifications
### DIM 1: no -> consol -> drift
m1 <- dim_1_mod ~ age + issuean + scope + revenue_log1

### DIM 2: no -> perf -> subst
m2 <- dim_2_mod ~ age + issuean + scope + revenue_log1

# Estimations
mnl_1 <- nnet::multinom(m1, data = engos11, Hess=TRUE)
mnl_2 <- nnet::multinom(m1, data = engos17, Hess=TRUE)
mnl_3 <- nnet::multinom(m1, data = engos23, Hess=TRUE)

mnl_4 <- nnet::multinom(m2, data = engos11, Hess=TRUE)
mnl_5 <- nnet::multinom(m2, data = engos17, Hess=TRUE)
mnl_6 <- nnet::multinom(m2, data = engos23, Hess=TRUE)


# Extracting statistics for all six models
extract_multinom_summary <- function(model) {
  s <- summary(model)
  coefs <- s$coefficients
  ses <- s$standard.errors
  z_scores <- coefs / ses
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  # Convert to long format for easier reading
  df <- as.data.frame(as.table(coefs))
  names(df)[3] <- "coefficient"
  df$std_error <- as.vector(ses)
  df$z_score <- as.vector(z_scores)
  df$p_value <- as.vector(p_values)
  return(df)
}

models <- list(mnl_1, mnl_2, mnl_3, mnl_4, mnl_5, mnl_6)
results <- lapply(models, extract_multinom_summary)

# the results below are tidyed and shown in tables 5 (models 1-3), and 6 (models 4-6)
results[]



