# R-Script for "Introducing the EU Exit Index measuring each member state’s propensity to leave the European Union"
# to be published in "European Union Politics" in 2021
# Author: Markus Gastinger, University of Salzburg (http://markus-gastinger.eu/) 

rm(list=ls())
cat("\014")
dev.off()
require(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Sys.setenv(LANG = "en")
options(max.print=100000, "scipen" = 100)

library(correlation)
library(ggplot2)
library(svglite)
library(psych)
library(tidyverse)
library(countrycode)
library(stargazer)
library(factoextra)
library(GPArotation)


#### Script body -----------------------------------------------------------------------------------------------------------------------------------

print.tables <- FALSE # set to true to print raw tables to the working directory (additional styling performed outside R); will also plot figures

## vector with all indicators
m1 <- c("future","future.eu","identity","citizen","image","migration","trust",
        "euro","trade.eu","rating","niip","budget",
        "polconiii","parties","Council","EP")


# social indicators
soc <- c("future","future.eu","identity","citizen","image","migration","trust")

# economic indicators
eco <- c("euro","trade.eu","rating","niip","budget")

# political indicators
pol <- c("polconiii","parties","Council","EP")


#### Descriptive statistics and normalization  --------------------------------------------------------------------------------------------------------
df <- readRDS("data_raw.rds")
df$country <- countrycode(df$country,"eurostat","country.name")


### descriptives (necessary for normalization) 
df.summary <- df[m1] %>%
  describe(quant=c(.25,.75)) %>%
  rownames_to_column(., var = "indicator") %>%
  select(-vars) 


### normalization

## minmax

# higher values, higher exit propensity
df.minmax <- df

pos.relationship <- c("future","future.eu","identity","citizen","image","migration","trust","niip","parties","Council","EP")
for (i in pos.relationship){
  df.minmax[i] <- ((df[i] - df.summary[df.summary$indicator == i, "min"])/df.summary[df.summary$indicator == i, "range"])*100
}

# higher values, lower exit propensity
neg.relationship <- c("euro","trade.eu","rating","budget","polconiii")
for (i in neg.relationship){
  df.minmax[i] <- ((df.summary[df.summary$indicator == i, "max"] - df[i])/df.summary[df.summary$indicator == i, "range"])*100
}


### Descriptives
# the descriptive statistics for the EU Exit Index itself, which are also included in Table A2, are computed further below

table.summary <- df.summary[df.summary$indicator %in% m1, c("indicator","n","mean","sd","min","Q0.25","Q0.75","max")]

if(print.tables){
  if (!dir.exists("./tables/")) dir.create("./tables/")
  stargazer(table.summary,
            type = "html",
            out = "./tables/TableA2.doc",
            title = "TABLE A2. Descriptive statistics",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}


#### Correlation matrix, Cronbach, KMO --------------------------------------------------------------------------------------------------------

## correlation matrix

correlation(df[,m1], method = "auto", ci = 0.95) # generally pearson, but point-biserial between Eurozone membership (dichotomous indicator) and other (continuous) indicators
cor <- correlation(df[,m1], method = "auto", ci = 0.95)
summary(cor)

if(print.tables){
  stargazer(summary(cor),
            type = "html",
            out = "./tables/TableA3.doc",
            title = "TABLE A3. Correlation matrix",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}

## Cronbach’s alpha 

alpha <- alpha(df[,m1], keys = neg.relationship) # warning message without effect
alpha$total$std.alpha # 0.72

## Kaiser-Meyer-Olkin’s measure of sampling adequacy 

KMO.stat <- KMO(df[,m1])
KMO.stat$MSA # 0.69

## Barlett’s test of sphericity

cortest.bartlett(df[,m1]) # Chi-squared 1732; p-value < 0.001; warning message without effect


#### Principal component analysis ----------------------------------------------------------------------------------------------------------------
# going forward, I'll only be interested in the normalized indicators (no impact on PCA, but facilitates the computation of index points below)

df.raw <- df
df <- df.minmax

## model 1
pca.prcomp <- prcomp(df[m1], center = TRUE, scale. = TRUE)
summary(pca.prcomp)

pca.prcomp.eigen <- get_eigenvalue(pca.prcomp)
pca.prcomp.eigen$PC <- seq(1,length(m1),1)
pca.prcomp.eigen[,c(4,1,2,3)] # easier alternative to psych below to get table with eigenvalues and variation measures

# compare with psych (identical results)

psych.none <- principal(df[m1], nfactor = length(m1), rotate = "none")
psych.none.vaccounted <- as.data.frame(unclass(psych.none$Vaccounted))
psych.none.vaccounted # identical with output above

if(print.tables){
  stargazer(pca.prcomp.eigen[,c(4,1,2,3)] ,
            type = "html",
            out = "./tables/TableA4.doc",
            title = "TABLE A4. Eigenvalues and variances of the principal component analysis",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
  
  # Figure A1 in the online Appendix
  f.a1 <- ggplot(pca.prcomp.eigen, aes(x=seq(1,16,1), y=eigenvalue)) +
          geom_line() +
          geom_point(size = 3, fill="white", shape=21) +
          theme_bw() +
          scale_x_continuous(breaks=seq(0,16,1)) +
          xlab("Principal component") + ylab("Eigenvalue") +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank())
  
  ggsave(file="./tables/FigureA1.svg", plot=f.a1, width=10, height=9)
  f.a1
}

nf <- 3 # number of factors


#### Weights König & Ohr 2013 ------------------------------------------------------------------------------------------------------------------

psych.none <- principal(df[m1], nfactor = nf, rotate = "none")
psych.none.loadings <- as.data.frame(unclass(psych.none$loadings))

psych.promax <- principal(df[m1], nfactor = nf, rotate = "promax") 
# an *oblique* rotation method; 'promax' (small p) applies Kaiser normalization (see documentation)
psych.promax.loadings <- as.data.frame(unclass(psych.promax$loadings))
psych.promax.loadings

psych.promax.vaccounted <- as.data.frame(unclass(psych.promax$Vaccounted))
psych.promax.vaccounted

col <- colnames(psych.promax.loadings)

  for (i in col){
    psych.promax.loadings[paste0(i,"^2")] <- psych.promax.loadings[i]^2
  }
  
  for (i in col){
    psych.promax.loadings[paste0(i,"^2.unitized")] <- (psych.promax.loadings[paste0(i,"^2")])/sum(psych.promax.loadings[paste0(i,"^2")])
  }

psych.promax.loadings

psych.promax.weights <- data.frame(matrix(ncol = nf, nrow = 1))
x <- c(paste0("RC", 1:nf,".weight"))
colnames(psych.promax.weights) <- x

for (i in col){
  psych.promax.weights[paste0(i,".weight")] <- psych.promax.vaccounted[1,i]/rowSums(psych.promax.vaccounted)[1] 
}

psych.promax.weights

col.select <- grepl("unitized",colnames(psych.promax.loadings))
psych.promax.loadings.weights <- psych.promax.loadings[,col.select]
colnames(psych.promax.loadings.weights) <- gsub("\\^.*","",colnames(psych.promax.loadings.weights))

psych.promax.loadings.weights.uni <- psych.promax.loadings.weights
psych.promax.loadings.weights.uni$RC1 <- psych.promax.loadings.weights.uni$RC1 * psych.promax.weights[1,1]
psych.promax.loadings.weights.uni$RC2 <- psych.promax.loadings.weights.uni$RC2 * psych.promax.weights[1,2]
psych.promax.loadings.weights.uni$RC3 <- psych.promax.loadings.weights.uni$RC3 * psych.promax.weights[1,3]
psych.promax.loadings.weights.uni

psych.promax.loadings.weights.uni$weight <- psych.promax.loadings.weights.uni$RC1 + psych.promax.loadings.weights.uni$RC2 + psych.promax.loadings.weights.uni$RC3
psych.promax.loadings.weights.uni$indicator <- rownames(psych.promax.loadings.weights.uni)
rownames(psych.promax.loadings.weights.uni) <- NULL

psych.promax.weights <- psych.promax.loadings.weights.uni[,c("weight","indicator")]
psych.promax.weights

## Table 1 ------------------------------------------------------------------------------------------------------------------------------------

psych.promax.loadings$indicator <- rownames(psych.promax.loadings)
tmp1 <- psych.promax.loadings.weights.uni
colnames(tmp1)[grepl("RC\\d", colnames(tmp1), perl = TRUE)] <- paste0(colnames(tmp1)[grepl("RC\\d", colnames(tmp1), perl = TRUE)],"^2.uni")
table1 <- merge(psych.promax.loadings[, grepl("RC\\d(?!\\^)|indicator", colnames(psych.promax.loadings), perl = TRUE)], tmp1, by = "indicator")

table1.vaccounted <- psych.promax.vaccounted[c(1,4),]
table1.vaccounted$indicator <- rownames(table1.vaccounted) # I only call variances 'indicators' at this point to facilitate setting up the table below
rownames(table1.vaccounted) <- NULL
table1.vaccounted <- table1.vaccounted %>% select(indicator, everything())

col <- colnames(table1[grepl("RC\\d\\^|weight", colnames(table1), perl = TRUE)])
  for (i in col){
    table1.vaccounted[i] <- NA
  }

table1 <- rbind(table1,table1.vaccounted)
rm(table1.vaccounted)

col <- append(m1,c("SS loadings","Proportion Explained"))
table1 <- table1[match(col, table1$indicator),] # only gets the rows in the right order

table1$dimension[table1$indicator == "future"] <- sum(table1[table1$indicator %in% soc, "weight"])
table1$dimension[table1$indicator == "euro"] <- sum(table1[table1$indicator %in% eco, "weight"])
table1$dimension[table1$indicator == "polconiii"] <- sum(table1[table1$indicator %in% pol, "weight"])
table1

if(print.tables){
    stargazer(table1,
              type = "html",
              out = "./tables/Table1.doc",
              title = "TABLE 1. Rotated factor loadings and computed weights",
              rownames = FALSE,
              summary = FALSE,
              digits = 2,
              notes = "Source: Author's calculations.",
              style = "io"
             )
}


## Table 2 ------------------------------------------------------------------------------------------------------------------------------------

df.KonigOhr <- df

for (i in psych.promax.weights$indicator){
  df.KonigOhr[,i] <- df[,i] * psych.promax.weights[psych.promax.weights$indicator == i, "weight"]
}

col <- append(c("country","year"), m1)
colnames(df.KonigOhr)
df.KonigOhr <- df.KonigOhr[, col]

df.KonigOhr$score <- rowSums(df.KonigOhr[m1])
df.KonigOhr <- df.KonigOhr %>% 
  group_by(year) %>% 
  mutate(rank = rank(-score))

df.KonigOhr$score.soc <- rowSums(df.KonigOhr[soc])
df.KonigOhr <- df.KonigOhr %>% 
  group_by(year) %>% 
  mutate(rank.soc = rank(-score.soc))

df.KonigOhr$score.eco <- rowSums(df.KonigOhr[eco])
df.KonigOhr <- df.KonigOhr %>% 
  group_by(year) %>% 
  mutate(rank.eco = rank(-score.eco))

df.KonigOhr$score.pol <- rowSums(df.KonigOhr[pol])
df.KonigOhr <- df.KonigOhr %>% 
  group_by(year) %>% 
  mutate(rank.pol = rank(-score.pol))

df.KonigOhr.max <- df.KonigOhr %>% 
  ungroup() %>% 
  select(-year) %>%  
  group_by(country) %>%
  mutate_at(vars(contains("rank")), min) %>%
  mutate_if(is.numeric, max) %>%
  distinct()

tmp1 <- df.KonigOhr[df.KonigOhr$year == 2015, !names(df.KonigOhr) %in% c("year",m1)]
tmp2 <- df.KonigOhr.max[, !names(df.KonigOhr.max) %in% m1]
colnames(tmp2)[2:ncol(tmp2)] <- paste0(colnames(tmp2)[2:ncol(tmp2)], ".max")

table2 <- merge(tmp1, tmp2, by="country")
rm(tmp1,tmp2)
table2 <- table2[order(table2$rank),]

table2 <- table2[,c(1,3,2,5,4,7,6,9,8,11,10,13,12,15,14,17,16)]
table2

if(print.tables){
  stargazer(table2,
            type = "html",
            out = "./tables/Table2.doc",
            title = "TABLE 2. Results of the EU Exit Index for 2015 and maximum values across all six years",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}
  
# additional information included in Table A2 (added manually outside R)

length(df.KonigOhr$score)
mean(df.KonigOhr$score)
sd(df.KonigOhr$score)
quantile(df.KonigOhr$score, probs = c(0, 0.25, 0.75, 1))

# trend between 2014 and 2019 (used in the discussion of results)

for(i in 2015:2019){
  assign(paste0("df.KonigOhr.diff",i), df.KonigOhr %>% 
  filter(year %in% c(2014,i)) %>%
  group_by(country) %>% 
  mutate_at(vars(contains("score")), list(diff = diff)) %>%  
  filter(year == i) %>% 
  rename_at(vars(contains("score")), ~paste0(., ".",i)) %>% 
  select(country,contains("diff")) %>%
  distinct())

}

df.KonigOhr.diff <- merge(df.KonigOhr.diff2015,df.KonigOhr.diff2016,by = "country")
df.KonigOhr.diff <- merge(df.KonigOhr.diff,df.KonigOhr.diff2017,by = "country")
df.KonigOhr.diff <- merge(df.KonigOhr.diff,df.KonigOhr.diff2018,by = "country")
df.KonigOhr.diff <- merge(df.KonigOhr.diff,df.KonigOhr.diff2019,by = "country")
rm(df.KonigOhr.diff2015,df.KonigOhr.diff2016,df.KonigOhr.diff2017,df.KonigOhr.diff2018,df.KonigOhr.diff2019)

df.KonigOhr.diff <- df.KonigOhr.diff %>% select(country,contains("soc"),contains("2019"),everything())

df.KonigOhr.diff[nrow(df.KonigOhr.diff)+1,(2:ncol(df.KonigOhr.diff))] <- apply(df.KonigOhr.diff[,2:ncol(df.KonigOhr.diff)], 2, mean)


#### Figure 1 -----------------------------------------------------------------------------------------------------------------
# set print.tables to TRUE at beginning of script to plot

if(print.tables){
  highlighted.size <- 1
  highlighted.alpha <- 0.3
  
  ## Figure 1
  
  f.1 <- ggplot(df.KonigOhr, aes(x=year, y=score, group=country)) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          scale_x_continuous(limits = c(2014,2019), expand = c(0, 0)) +
          theme(plot.margin=unit(c(0,1,0,1),"cm")) +
          geom_line(data = subset(df.KonigOhr, !(country %in% c("United Kingdom","Austria", "Czechia","Cyprus"))), size=0.5, aes(group = country), color = "black", alpha = 0.2) +
          geom_line(data = subset(df.KonigOhr, country %in% "United Kingdom"), color = "black", size = highlighted.size, linetype = "longdash") +
          geom_line(data = subset(df.KonigOhr, country %in% "Austria"), color = "black", size = highlighted.size, linetype = "dotted") +
          geom_line(data = subset(df.KonigOhr, country %in% "Czechia"), color = "black", size = highlighted.size, linetype = "dashed") +
          geom_line(data = subset(df.KonigOhr, country %in% "Cyprus"), color = "black", size = highlighted.size, linetype = "solid") +
        
          labs(
               title = NULL,
               x = "Year",
               y = "EU Exit Index points",
               color = NULL
              )

  ggsave(file="./tables/Figure1.svg", plot=f.1, width=6, height=4, dpi = 600)
  f.1
}


#### Sensitivity to individual indicators -------------------------------------------------------------------------------------------

col.select <- append(m1,c("rank","score"))
col.select <- append("country",col.select)
df.r.indicators <- as.data.frame(df.KonigOhr.max[,col.select])
df.r.indicators$score <- round(df.r.indicators$score, 2)

tmp <- NULL
tmp1 <- NULL
tmp2 <- NULL

for (i in m1){
  df.r.indicators[paste0("score.-",i)] <- round(df.r.indicators$score - df.r.indicators[i], 2)
  df.r.indicators[paste0("rank.-",i)] <- rank(-df.r.indicators[paste0("score.-",i)])
  
  for(j in 1:nrow(df.r.indicators)){
    df.r.indicators[j,paste0("m-",i)] <- paste0(df.r.indicators[j,paste0("score.-",i)]," (",df.r.indicators[j,paste0("rank.-",i)],")")
  }
  
  tmp <- append(tmp, paste0("m-",i))
  tmp1 <- c(paste0("rank.-",i), paste0("score.-",i))
  tmp2 <- append(tmp2,tmp1)
}

col.select <- append(c("country","rank","score"),tmp)
df.r.indicators <- df.r.indicators[order(-df.r.indicators$score),col.select]  

table.a5.1 <- as.data.frame(df.r.indicators[,c(1:11)])
table.a5.2 <- as.data.frame(df.r.indicators[,c(1:3,12:19)])

if(print.tables){
  stargazer(table.a5.1,
            type = "html",
            out = "./tables/TableA5.1.doc",
            title = "TABLE A5. Sensitivity of the EU Exit Index to individual indicators",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
  
  stargazer(table.a5.2,
            type = "html",
            out = "./tables/TableA5.2.doc",
            title = "TABLE A5. Sensitivity of the EU Exit Index to individual indicators (cont.)",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}


#### Weights Nicoletti et al. 2000 -----------------------------------------------------------------------------------------

psych.varimax <- principal(df[m1], nfactor = nf, rotate = "Varimax")
# an *orthogonal* rotation method; 'Varimax' (wiht capital V!) applies a Kaiser normalization (see documentation)
psych.varimax.loadings <- as.data.frame(unclass(psych.varimax$loadings))
psych.varimax.loadings

psych.varimax.vaccounted <- as.data.frame(unclass(psych.varimax$Vaccounted))
psych.varimax.vaccounted

col <- colnames(psych.varimax.loadings)

for (i in col){
  psych.varimax.loadings[paste0(i,"^2")] <- psych.varimax.loadings[i]^2
}

for (i in col){
  psych.varimax.loadings[paste0(i,"^2.unitized")] <- (psych.varimax.loadings[paste0(i,"^2")])/sum(psych.varimax.loadings[paste0(i,"^2")])
}

psych.varimax.loadings

psych.varimax.weights <- data.frame(matrix(ncol = nf, nrow = 1))
x <- c(paste0("RC", 1:nf,".weight"))
colnames(psych.varimax.weights) <- x

for (i in col){
  psych.varimax.weights[paste0(i,".weight")] <- psych.varimax.vaccounted[1,i]/rowSums(psych.varimax.vaccounted)[1] 
}
psych.varimax.weights

col.select <- grepl("unitized",colnames(psych.varimax.loadings)) # keeps only squared factor loading scaled to unity sum
psych.varimax.loadings.weights <- psych.varimax.loadings[,col.select]
colnames(psych.varimax.loadings.weights) <- gsub("\\^.*","",colnames(psych.varimax.loadings.weights))
psych.varimax.loadings.weights

# this will make sure that every row has only one maximum
for (i in 1:nrow(psych.varimax.loadings.weights)){
  ifelse(max(psych.varimax.loadings.weights[i,1:nf]) == psych.varimax.loadings.weights[i,1], psych.varimax.loadings.weights[i,2:3] <- 0,
  ifelse(max(psych.varimax.loadings.weights[i,1:nf]) == psych.varimax.loadings.weights[i,2], psych.varimax.loadings.weights[i,c(1,3)] <- 0,
  ifelse(max(psych.varimax.loadings.weights[i,1:nf]) == psych.varimax.loadings.weights[i,3], psych.varimax.loadings.weights[i,c(1:2)] <- 0, NA)
  ))
}

psych.varimax.loadings.weights

# re-unitize the weights
psych.varimax.loadings.weights.uni = as.data.frame(apply(psych.varimax.loadings.weights,2,function(x){x/sum(x)}))
psych.varimax.loadings.weights.uni

for (i in 1:nrow(psych.varimax.loadings.weights.uni)){
  psych.varimax.loadings.weights.uni$weight[i] <- 
    ifelse(max(psych.varimax.loadings.weights.uni[i,1:nf]) == psych.varimax.loadings.weights.uni[i,1], psych.varimax.loadings.weights.uni[i,1] * psych.varimax.weights[1],
    ifelse(max(psych.varimax.loadings.weights.uni[i,1:nf]) == psych.varimax.loadings.weights.uni[i,2], psych.varimax.loadings.weights.uni[i,2] * psych.varimax.weights[2],
    ifelse(max(psych.varimax.loadings.weights.uni[i,1:nf]) == psych.varimax.loadings.weights.uni[i,3], psych.varimax.loadings.weights.uni[i,3] * psych.varimax.weights[3], NA)
    ))
}
psych.varimax.loadings.weights.uni

psych.varimax.loadings.weights.uni$weight <- as.numeric(psych.varimax.loadings.weights.uni$weight)
psych.varimax.loadings.weights.uni$indicator <- rownames(psych.varimax.loadings.weights.uni)
rownames(psych.varimax.loadings.weights.uni) <- NULL
psych.varimax.loadings.weights.uni

psych.varimax.weights <- psych.varimax.loadings.weights.uni[,c("weight","indicator")]
psych.varimax.weights


### Table A6

psych.varimax.loadings$indicator <- rownames(psych.varimax.loadings)
table.a6 <- merge(psych.varimax.loadings, psych.varimax.weights, by = "indicator")
table.a6 <- table.a6[,!grepl("RC\\d\\^2(?!\\.)", colnames(table.a6), perl = TRUE)] # removes the RCx^2 columns that are *not* unitized (not reported in the paper)

table.a6.vaccounted <- psych.varimax.vaccounted[c(1,4),]
table.a6.vaccounted$indicator <- rownames(table.a6.vaccounted) # I call variances 'indicators' at this point only to facilitate setting up the table below
rownames(table.a6.vaccounted) <- NULL

table.a6.vaccounted <- table.a6.vaccounted %>% select(indicator, everything())

col <- colnames(table.a6[grepl("\\^|weight", colnames(table.a6), perl = TRUE)])
for (i in col){
  table.a6.vaccounted[i] <- NA
}

table.a6 <- rbind(table.a6,table.a6.vaccounted)
rm(table.a6.vaccounted)

col <- append(m1,c("SS loadings","Proportion Explained"))
table.a6 <- table.a6[match(col, table.a6$indicator),] # only gets the rows in the right order

table.a6$dimension[table.a6$indicator == "future"] <- sum(table.a6[table.a6$indicator %in% soc, "weight"])
table.a6$dimension[table.a6$indicator == "euro"] <- sum(table.a6[table.a6$indicator %in% eco, "weight"])
table.a6$dimension[table.a6$indicator == "polconiii"] <- sum(table.a6[table.a6$indicator %in% pol, "weight"])
table.a6

if(print.tables){
  stargazer(table.a6,
            type = "html",
            out = "./tables/TableA6.doc",
            title = "TABLE A6. Rotated factor loadings and computed weights (Nicoletti et al. 2000)",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}


### Table A7

df.Nicoletti <- df

for (i in psych.varimax.weights$indicator){
  df.Nicoletti[,i] <- df[,i] * psych.varimax.weights[psych.varimax.weights$indicator == i, "weight"]
}

col <- append(c("country","year"), m1)
colnames(df.Nicoletti)
df.Nicoletti <- df.Nicoletti[, col]

df.Nicoletti$score <- rowSums(df.Nicoletti[m1])
df.Nicoletti <- df.Nicoletti %>% 
  group_by(year) %>% 
  mutate(rank = rank(-score))

df.Nicoletti$score.soc <- rowSums(df.Nicoletti[soc])
df.Nicoletti <- df.Nicoletti %>% 
  group_by(year) %>% 
  mutate(rank.soc = rank(-score.soc))

df.Nicoletti$score.eco <- rowSums(df.Nicoletti[eco])
df.Nicoletti <- df.Nicoletti %>% 
  group_by(year) %>% 
  mutate(rank.eco = rank(-score.eco))

df.Nicoletti$score.pol <- rowSums(df.Nicoletti[pol])
df.Nicoletti <- df.Nicoletti %>% 
  group_by(year) %>% 
  mutate(rank.pol = rank(-score.pol))

df.Nicoletti.max <- df.Nicoletti %>% 
  ungroup() %>% 
  select(-year) %>%  
  group_by(country) %>%
  mutate_at(vars(contains("rank")), min) %>%
  mutate_if(is.numeric, max) %>%
  distinct()

tmp1 <- df.Nicoletti[df.Nicoletti$year == 2015, !names(df.Nicoletti) %in% c("year",m1)]
tmp2 <- df.Nicoletti.max[, !names(df.Nicoletti.max) %in% m1]
colnames(tmp2)[2:ncol(tmp2)] <- paste0(colnames(tmp2)[2:ncol(tmp2)], ".max")

table.a7 <- merge(tmp1, tmp2, by="country")
rm(tmp1,tmp2)
table.a7 <- table.a7[order(table.a7$rank),]

table.a7 <- table.a7[,c(1,3,2,5,4,7,6,9,8,11,10,13,12,15,14,17,16)]
table.a7

if(print.tables){
  stargazer(table.a7,
            type = "html",
            out = "./tables/TableA7.doc",
            title = "TABLE A7. Results of the EU Exit Index for 2015 and maximum values across all six years (Nicoletti et al. 2000)",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}


#### Unweighted indicators --------------------------------------------------------------------------------------------------------------

df.r1 <- df.minmax
df.r1$score.soc <- rowSums(df.r1[soc])/length(soc)
df.r1$score.eco <- rowSums(df.r1[eco])/length(eco)
df.r1$score.pol <- rowSums(df.r1[pol])/length(pol)
df.r1$score <- (df.r1$score.soc * (1/3)) + (df.r1$score.eco * (1/3)) + (df.r1$score.pol * (1/3))

df.r1 <- df.r1 %>% 
  group_by(year) %>% 
  mutate(rank = rank(-score))

df.r <- df.r1 %>% 
  ungroup() %>% 
  select(country, score, rank) %>%  
  group_by(country) %>%
  mutate_at(vars(score), max) %>% 
  mutate_at(vars(rank), min) %>% 
  rename_at(vars(score), ~"s.r1") %>%
  rename_at(vars(rank), ~"r.r1") %>%
  distinct()


## sensitivity analysis indicators (unweighted)

df.r1.1 <- df.r1

for (i in m1){
  
  if(i %in% soc){df.r1.1[paste0("score.-",i)] <- df.r1.1$score - (df.r1.1[i]*(1/length(soc)*1/3))}
  if(i %in% eco){df.r1.1[paste0("score.-",i)] <- df.r1.1$score - (df.r1.1[i]*(1/length(eco)*1/3))}
  if(i %in% pol){df.r1.1[paste0("score.-",i)] <- df.r1.1$score - (df.r1.1[i]*(1/length(pol)*1/3))}
  
    for (j in unique(df$year)){
      df.r1.1[df.r1.1$year == j, paste0("rank.-",i)] <- rank(-df.r1.1[df.r1.1$year == j, paste0("score.-",i)])
    }

}

df.tmp <- df.r1.1[,!names(df.r1.1) %in% m1]
df.tmp <- df.tmp[,!names(df.tmp) %in% c("score.soc","score.eco","score.pol","score","rank")]

df.tmp <- list(.vars=lst(vars(contains("rank")), vars(contains("score"))),
               .funs=lst(min, max)) %>% pmap(~df.tmp %>% 
               group_by(country) %>% summarise_at(.x, .y)) %>% 
               reduce(inner_join)

colnames(df.tmp) <- gsub("rank","r",colnames(df.tmp))
colnames(df.tmp) <- gsub("score","s",colnames(df.tmp))

df.r <- inner_join(df.r, df.tmp, by = "country")

for (i in m1){
  for(j in 1:nrow(df.r.indicators)){
    df.r[j,paste0("m-",i)] <- paste0(round(df.r[j,paste0("s.-",i)],2)," (",df.r[j,paste0("r.-",i)],")")
  }
}

col.select <- append(c("country","r.r1","s.r1"),tmp)

df.r.indicators <- df.r[order(-df.r["s.r1"]), col.select]
df.r.indicators["s.r1"] <- round(df.r.indicators["s.r1"], 2)

table.a8.1 <- as.data.frame(df.r.indicators[,c(1:11)])
table.a8.2 <- as.data.frame(df.r.indicators[,c(1:3,12:19)])
table.a8.1
table.a8.2

if(print.tables){
  stargazer(table.a8.1,
            type = "html",
            out = "./tables/TableA8.1.doc",
            title = "TABLE A8. Sensitivity of the EU Exit Index to unweighted indicators",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
  
  stargazer(table.a8.2,
            type = "html",
            out = "./tables/TableA8.2.doc",
            title = "TABLE A8. Sensitivity of the EU Exit Index to unweighted indicators (cont.)",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}


##### z-score normalization -----------------------------------------------------------------------------------------------------

df.zscore <- df.raw

for (i in pos.relationship){
  df.zscore[i] <- ((df.zscore[i] - df.summary[df.summary$indicator == i, "mean"])/df.summary[df.summary$indicator == i, "sd"])
}

for (i in neg.relationship){
  df.zscore[i] <- ((df.zscore[i] - df.summary[df.summary$indicator == i, "mean"])/df.summary[df.summary$indicator == i, "sd"])*-1
}

df.zscore.weighted <- df.zscore[,col]

for (i in m1){
  df.zscore.weighted[,i] <- df.zscore[,i] * psych.promax.weights[psych.promax.weights$indicator == i, "weight"]
}

df.zscore.weighted$score <- rowSums(df.zscore.weighted[m1])
df.zscore.weighted <- df.zscore.weighted %>% 
  group_by(year) %>% 
  mutate(rank = rank(-score))

df.zscore.weighted$score.soc <- rowSums(df.zscore.weighted[soc])
df.zscore.weighted <- df.zscore.weighted %>% 
  group_by(year) %>% 
  mutate(rank.soc = rank(-score.soc))

df.zscore.weighted$score.eco <- rowSums(df.zscore.weighted[eco])
df.zscore.weighted <- df.zscore.weighted %>% 
  group_by(year) %>% 
  mutate(rank.eco = rank(-score.eco))

df.zscore.weighted$score.pol <- rowSums(df.zscore.weighted[pol])
df.zscore.weighted <- df.zscore.weighted %>% 
  group_by(year) %>% 
  mutate(rank.pol = rank(-score.pol))

df.zscore.weighted.max <- df.zscore.weighted %>% 
  ungroup() %>% 
  select(-year) %>%  
  group_by(country) %>%
  mutate_at(vars(contains("rank")), min) %>%
  mutate_if(is.numeric, max) %>%
  distinct()

tmp1 <- df.zscore.weighted[df.zscore.weighted$year == 2015, !names(df.zscore.weighted) %in% c("year",m1)]
tmp2 <- df.zscore.weighted.max[, !names(df.zscore.weighted.max) %in% m1]
colnames(tmp2)[2:ncol(tmp2)] <- paste0(colnames(tmp2)[2:ncol(tmp2)], ".max")

table.a9 <- merge(tmp1, tmp2, by="country")
rm(tmp1,tmp2)
table.a9 <- table.a9[order(table.a9$rank),]

table.a9 <- table.a9[,c(1,3,2,5,4,7,6,9,8,11,10,13,12,15,14,17,16)]
table.a9

if(print.tables){
  stargazer(table.a9,
            type = "html",
            out = "./tables/TableA9.doc",
            title = "TABLE A9. Results of the EU Exit Index for 2015 and maximum values across all six years (z-score)",
            rownames = FALSE,
            summary = FALSE,
            digits = 2,
            notes = "Source: Author's calculations.",
            style = "io"
           )
}
 
mean(df.zscore.weighted$score)
sd(df.zscore.weighted$score)


## Session information
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")