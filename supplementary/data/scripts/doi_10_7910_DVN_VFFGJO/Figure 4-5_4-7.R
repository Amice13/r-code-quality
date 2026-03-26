############################################################################################################################################
## Replication Data for:                                                                                                                   #
## Wawro, Gregory, and Ira Katznelson. Time Counts: Quantitative Analysis for Historical Social Science. Princeton University Press, 2022. #
############################################################################################################################################

rm(list = ls())
library(gplots)
library(plm)
library(foreign)
library(ggplot2)
set.seed(711996)

## NOMINATE models
year  <- seq(1884, 1940, 4)
gjdat <-  read.dta("../Data/GailmardJenkins.dta")
gjdat_nom <- subset(gjdat, year>=1880 & year<=1940 & duplicate==0 & state != "" & jcr_b != "NA")
gjdat_nom = transform(gjdat_nom,time = time - 2)
gjdat_nom = transform(gjdat_nom, stateno = statedupl/10)
gjdat_nom = transform(gjdat_nom,time1880 = as.numeric(time>=1))
gjdat_nom = transform(gjdat_nom,time1884 = as.numeric(time>=2))
gjdat_nom = transform(gjdat_nom,time1888 = as.numeric(time>=3))
gjdat_nom = transform(gjdat_nom,time1892 = as.numeric(time>=4))
gjdat_nom = transform(gjdat_nom,time1896 = as.numeric(time>=5))
gjdat_nom = transform(gjdat_nom,time1900 = as.numeric(time>=6))
gjdat_nom = transform(gjdat_nom,time1904 = as.numeric(time>=7))
gjdat_nom = transform(gjdat_nom,time1908 = as.numeric(time>=8))
gjdat_nom = transform(gjdat_nom,time1912 = as.numeric(time>=9))
gjdat_nom = transform(gjdat_nom,time1916 = as.numeric(time>=10))
gjdat_nom = transform(gjdat_nom,time1920 = as.numeric(time>=11))
gjdat_nom = transform(gjdat_nom,time1924 = as.numeric(time>=12))
gjdat_nom = transform(gjdat_nom,time1928 = as.numeric(time>=13))
gjdat_nom = transform(gjdat_nom,time1932 = as.numeric(time>=14))
gjdat_nom = transform(gjdat_nom,time1936 = as.numeric(time>=15))
gjdat_nom = transform(gjdat_nom,time1940 = as.numeric(time>=16))

gjdat_nom = transform(gjdat_nom,tpr_time1880 = tpr*as.numeric(time>=1))
gjdat_nom = transform(gjdat_nom,tpr_time1884 = tpr*as.numeric(time>=2))
gjdat_nom = transform(gjdat_nom,tpr_time1888 = tpr*as.numeric(time>=3))
gjdat_nom = transform(gjdat_nom,tpr_time1892 = tpr*as.numeric(time>=4))
gjdat_nom = transform(gjdat_nom,tpr_time1896 = tpr*as.numeric(time>=5))
gjdat_nom = transform(gjdat_nom,tpr_time1900 = tpr*as.numeric(time>=6))
gjdat_nom = transform(gjdat_nom,tpr_time1904 = tpr*as.numeric(time>=7))
gjdat_nom = transform(gjdat_nom,tpr_time1908 = tpr*as.numeric(time>=8))
gjdat_nom = transform(gjdat_nom,tpr_time1912 = tpr*as.numeric(time>=9))
gjdat_nom = transform(gjdat_nom,tpr_time1916 = tpr*as.numeric(time>=10))
gjdat_nom = transform(gjdat_nom,tpr_time1920 = tpr*as.numeric(time>=11))
gjdat_nom = transform(gjdat_nom,tpr_time1924 = tpr*as.numeric(time>=12))
gjdat_nom = transform(gjdat_nom,tpr_time1928 = tpr*as.numeric(time>=13))
gjdat_nom = transform(gjdat_nom,tpr_time1932 = tpr*as.numeric(time>=14))
gjdat_nom = transform(gjdat_nom,tpr_time1936 = tpr*as.numeric(time>=15))
gjdat_nom = transform(gjdat_nom,tpr_time1940 = tpr*as.numeric(time>=16))

gjdat_nom = transform(gjdat_nom,jcr_b_time1880 = jcr_b*as.numeric(time>=1))
gjdat_nom = transform(gjdat_nom,jcr_b_time1884 = jcr_b*as.numeric(time>=2))
gjdat_nom = transform(gjdat_nom,jcr_b_time1888 = jcr_b*as.numeric(time>=3))
gjdat_nom = transform(gjdat_nom,jcr_b_time1892 = jcr_b*as.numeric(time>=4))
gjdat_nom = transform(gjdat_nom,jcr_b_time1896 = jcr_b*as.numeric(time>=5))
gjdat_nom = transform(gjdat_nom,jcr_b_time1900 = jcr_b*as.numeric(time>=6))
gjdat_nom = transform(gjdat_nom,jcr_b_time1904 = jcr_b*as.numeric(time>=7))
gjdat_nom = transform(gjdat_nom,jcr_b_time1908 = jcr_b*as.numeric(time>=8))
gjdat_nom = transform(gjdat_nom,jcr_b_time1912 = jcr_b*as.numeric(time>=9))
gjdat_nom = transform(gjdat_nom,jcr_b_time1916 = jcr_b*as.numeric(time>=10))
gjdat_nom = transform(gjdat_nom,jcr_b_time1920 = jcr_b*as.numeric(time>=11))
gjdat_nom = transform(gjdat_nom,jcr_b_time1924 = jcr_b*as.numeric(time>=12))
gjdat_nom = transform(gjdat_nom,jcr_b_time1928 = jcr_b*as.numeric(time>=13))
gjdat_nom = transform(gjdat_nom,jcr_b_time1932 = jcr_b*as.numeric(time>=14))
gjdat_nom = transform(gjdat_nom,jcr_b_time1936 = jcr_b*as.numeric(time>=15))
gjdat_nom = transform(gjdat_nom,jcr_b_time1940 = jcr_b*as.numeric(time>=16))

lm.full.out1884 <- lm(sdwnom1 ~ tpr + tpr_time1884 + jcr_b + jcr_b_time1884 + time1884 + time + elect, data=gjdat_nom)
lm.full.out1888 <- lm(sdwnom1 ~ tpr + tpr_time1888 + jcr_b + jcr_b_time1888 + time1888 + time + elect, data=gjdat_nom)
lm.full.out1892 <- lm(sdwnom1 ~ tpr + tpr_time1892 + jcr_b + jcr_b_time1892 + time1892 + time + elect, data=gjdat_nom)
lm.full.out1896 <- lm(sdwnom1 ~ tpr + tpr_time1896 + jcr_b + jcr_b_time1896 + time1896 + time + elect, data=gjdat_nom)
lm.full.out1900 <- lm(sdwnom1 ~ tpr + tpr_time1900 + jcr_b + jcr_b_time1900 + time1900 + time + elect, data=gjdat_nom)
lm.full.out1904 <- lm(sdwnom1 ~ tpr + tpr_time1904 + jcr_b + jcr_b_time1904 + time1904 + time + elect, data=gjdat_nom)
lm.full.out1908 <- lm(sdwnom1 ~ tpr + tpr_time1908 + jcr_b + jcr_b_time1908 + time1908 + time + elect, data=gjdat_nom)
lm.full.out1912 <- lm(sdwnom1 ~ tpr + tpr_time1912 + jcr_b + jcr_b_time1912 + time1912 + time + elect, data=gjdat_nom)
lm.full.out1916 <- lm(sdwnom1 ~ tpr + tpr_time1916 + jcr_b + jcr_b_time1916 + time1916 + time + elect, data=gjdat_nom)
lm.full.out1920 <- lm(sdwnom1 ~ tpr + tpr_time1920 + jcr_b + jcr_b_time1920 + time1920 + time + elect, data=gjdat_nom)
lm.full.out1924 <- lm(sdwnom1 ~ tpr + tpr_time1924 + jcr_b + jcr_b_time1924 + time1924 + time + elect, data=gjdat_nom)
lm.full.out1928 <- lm(sdwnom1 ~ tpr + tpr_time1928 + jcr_b + jcr_b_time1928 + time1928 + time + elect, data=gjdat_nom)
lm.full.out1932 <- lm(sdwnom1 ~ tpr + tpr_time1932 + jcr_b + jcr_b_time1932 + time1932 + time + elect, data=gjdat_nom)
lm.full.out1936 <- lm(sdwnom1 ~ tpr + tpr_time1936 + jcr_b + jcr_b_time1936 + time1936 + time + elect, data=gjdat_nom)
lm.full.out1940 <- lm(sdwnom1 ~ tpr + tpr_time1940 + jcr_b + jcr_b_time1940 + time1940 + time + elect, data=gjdat_nom)

gjdat_nom.plm <- plm.data(gjdat_nom,index = c("state"))
gjdat_nom.fe  <- plm(sdwnom1 ~ tpr + tpr_xvii + jcr_b + jcr_b_xvii + as.numeric(time) + elect, 
                     data = gjdat_nom.plm, model = "within", effect = "twoways")

plm.full.out <- plm(sdwnom1 ~ tpr + tpr_xvii + jcr_b + jcr_b_xvii + elect + time1884 +
                      time1888 + time1892 + time1896 + time1900 + time1904 + time1908 +
                      time1912 + time1916 + time1920 + time1924 + time1928 + time1932 +
                      time1936 + time1940, data=gjdat_nom.plm, model = "within")

plm.pool.out <- plm(sdwnom1 ~ tpr + tpr_xvii + jcr_b + jcr_b_xvii + elect + time1884 +
                      time1888 + time1892 + time1896 + time1900 + time1904 + time1908 +
                      time1912 + time1916 + time1920 + time1924 + time1928 + time1932 +
                      time1936 + time1940, data=gjdat_nom.plm, model = "pooling")

pFtest(plm.full.out,plm.pool.out)

plm.full.out1884 <- plm(sdwnom1 ~ tpr + tpr_time1884 + jcr_b + jcr_b_time1884 + time1884 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1888 <- plm(sdwnom1 ~ tpr + tpr_time1888 + jcr_b + jcr_b_time1888 + time1888 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1892 <- plm(sdwnom1 ~ tpr + tpr_time1892 + jcr_b + jcr_b_time1892 + time1892 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1896 <- plm(sdwnom1 ~ tpr + tpr_time1896 + jcr_b + jcr_b_time1896 + time1896 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1900 <- plm(sdwnom1 ~ tpr + tpr_time1900 + jcr_b + jcr_b_time1900 + time1900 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1904 <- plm(sdwnom1 ~ tpr + tpr_time1904 + jcr_b + jcr_b_time1904 + time1904 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1908 <- plm(sdwnom1 ~ tpr + tpr_time1908 + jcr_b + jcr_b_time1908 + time1908 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1912 <- plm(sdwnom1 ~ tpr + tpr_time1912 + jcr_b + jcr_b_time1912 + time1912 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1916 <- plm(sdwnom1 ~ tpr + tpr_time1916 + jcr_b + jcr_b_time1916 + time1916 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1920 <- plm(sdwnom1 ~ tpr + tpr_time1920 + jcr_b + jcr_b_time1920 + time1920 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1924 <- plm(sdwnom1 ~ tpr + tpr_time1924 + jcr_b + jcr_b_time1924 + time1924 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1928 <- plm(sdwnom1 ~ tpr + tpr_time1928 + jcr_b + jcr_b_time1928 + time1928 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1932 <- plm(sdwnom1 ~ tpr + tpr_time1932 + jcr_b + jcr_b_time1932 + time1932 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1936 <- plm(sdwnom1 ~ tpr + tpr_time1936 + jcr_b + jcr_b_time1936 + time1936 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")
plm.full.out1940 <- plm(sdwnom1 ~ tpr + tpr_time1940 + jcr_b + jcr_b_time1940 + time1940 + as.numeric(time) + elect, data=gjdat_nom.plm, model="within")

## Senate delegation models
gjdat_del <- subset(gjdat, year>=1880 & year<=1940 & duplicate==0 & state != "" )
gjdat_del = transform(gjdat_del,time = time - 2)
gjdat_del = transform(gjdat_del,stateno = statedupl/10)

pdf(paste(file="../Figures/Figure 4-7.pdf"))
boxplot(lnsmaxdist ~ year, ylab="Within Delegation Distance", xlab="Year", data=gjdat_del)
dev.off()

gjdat_del = transform(gjdat_del,time1880 = as.numeric(time>=1))
gjdat_del = transform(gjdat_del,time1884 = as.numeric(time>=2))
gjdat_del = transform(gjdat_del,time1888 = as.numeric(time>=3))
gjdat_del = transform(gjdat_del,time1892 = as.numeric(time>=4))
gjdat_del = transform(gjdat_del,time1896 = as.numeric(time>=5))
gjdat_del = transform(gjdat_del,time1900 = as.numeric(time>=6))
gjdat_del = transform(gjdat_del,time1904 = as.numeric(time>=7))
gjdat_del = transform(gjdat_del,time1908 = as.numeric(time>=8))
gjdat_del = transform(gjdat_del,time1912 = as.numeric(time>=9))
gjdat_del = transform(gjdat_del,time1916 = as.numeric(time>=10))
gjdat_del = transform(gjdat_del,time1920 = as.numeric(time>=11))
gjdat_del = transform(gjdat_del,time1924 = as.numeric(time>=12))
gjdat_del = transform(gjdat_del,time1928 = as.numeric(time>=13))
gjdat_del = transform(gjdat_del,time1932 = as.numeric(time>=14))
gjdat_del = transform(gjdat_del,time1936 = as.numeric(time>=15))
gjdat_del = transform(gjdat_del,time1940 = as.numeric(time>=16))

lm.full.out1884 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1884 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1888 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1888 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1892 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1892 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1896 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1896 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1900 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1900 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1904 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1904 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1908 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1908 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1912 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1912 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1916 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1916 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1920 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1920 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1924 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1924 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1928 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1928 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1932 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1932 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1936 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1936 + as.numeric(time) + elect_str_both, data=gjdat_del)
lm.full.out1940 <- lm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + time1940 + as.numeric(time) + elect_str_both, data=gjdat_del)

gjdat_del.plm <- plm.data(gjdat_del,index=c("state"))
plm.full.out  <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1884 +
                      time1888 + time1892 + time1896 + time1900 + time1904 + time1908 +
                      time1912 + time1916 + time1920 + time1924 + time1928 + time1932 +
                      time1936 + time1940 + as.numeric(time), data=gjdat_del.plm, model="within")

plm.pool.out <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1884 +
                      time1888 + time1892 + time1896 + time1900 + time1904 + time1908 +
                      time1912 + time1916 + time1920 + time1924 + time1928 + time1932 +
                      time1936 + time1940 + as.numeric(time), data=gjdat_del.plm, model="pooling")

pFtest(plm.full.out,plm.pool.out)

plm.full.out1884 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1884 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1888 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1888 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1892 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1892 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1896 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1896 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1900 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1900 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1904 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1904 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1908 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1908 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1912 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1912 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1916 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1916 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1920 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1920 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1924 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1924 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1928 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1928 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1932 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1932 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1936 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1936 + as.numeric(time), data=gjdat_del.plm, model="within")
plm.full.out1940 <- plm(lnsmaxdist ~ tpr + tpr2 + jcr_b + jcr_b2 + elect_str_both + time1940 + as.numeric(time), data=gjdat_del.plm, model="within")

ests <- c(coef(plm.full.out1884)[length(coef(plm.full.out1884))-1],
          coef(plm.full.out1888)[length(coef(plm.full.out1888))-1],
          coef(plm.full.out1892)[length(coef(plm.full.out1892))-1],
          coef(plm.full.out1896)[length(coef(plm.full.out1896))-1],
          coef(plm.full.out1900)[length(coef(plm.full.out1900))-1],
          coef(plm.full.out1904)[length(coef(plm.full.out1904))-1],
          coef(plm.full.out1908)[length(coef(plm.full.out1908))-1],
          coef(plm.full.out1912)[length(coef(plm.full.out1912))-1],
          coef(plm.full.out1916)[length(coef(plm.full.out1916))-1],
          coef(plm.full.out1920)[length(coef(plm.full.out1920))-1],
          coef(plm.full.out1924)[length(coef(plm.full.out1924))-1],
          coef(plm.full.out1928)[length(coef(plm.full.out1928))-1],
          coef(plm.full.out1932)[length(coef(plm.full.out1932))-1],
          coef(plm.full.out1936)[length(coef(plm.full.out1936))-1],
          coef(plm.full.out1940)[length(coef(plm.full.out1940))-1])

ses <- c(sqrt(diag(vcov((plm.full.out1884))))[length(sqrt(diag(vcov((plm.full.out1884)))))-1],
         sqrt(diag(vcov((plm.full.out1888))))[length(sqrt(diag(vcov((plm.full.out1888)))))-1],
         sqrt(diag(vcov((plm.full.out1892))))[length(sqrt(diag(vcov((plm.full.out1892)))))-1],
         sqrt(diag(vcov((plm.full.out1896))))[length(sqrt(diag(vcov((plm.full.out1896)))))-1],
         sqrt(diag(vcov((plm.full.out1900))))[length(sqrt(diag(vcov((plm.full.out1900)))))-1],
         sqrt(diag(vcov((plm.full.out1904))))[length(sqrt(diag(vcov((plm.full.out1904)))))-1],
         sqrt(diag(vcov((plm.full.out1908))))[length(sqrt(diag(vcov((plm.full.out1908)))))-1],
         sqrt(diag(vcov((plm.full.out1912))))[length(sqrt(diag(vcov((plm.full.out1912)))))-1],
         sqrt(diag(vcov((plm.full.out1916))))[length(sqrt(diag(vcov((plm.full.out1916)))))-1],
         sqrt(diag(vcov((plm.full.out1920))))[length(sqrt(diag(vcov((plm.full.out1920)))))-1],
         sqrt(diag(vcov((plm.full.out1924))))[length(sqrt(diag(vcov((plm.full.out1924)))))-1],
         sqrt(diag(vcov((plm.full.out1928))))[length(sqrt(diag(vcov((plm.full.out1928)))))-1],
         sqrt(diag(vcov((plm.full.out1932))))[length(sqrt(diag(vcov((plm.full.out1932)))))-1],
         sqrt(diag(vcov((plm.full.out1936))))[length(sqrt(diag(vcov((plm.full.out1936)))))-1],
         sqrt(diag(vcov((plm.full.out1940))))[length(sqrt(diag(vcov((plm.full.out1940)))))-1])

yrlabs  <- c("1884", "", "1892", "", "1900", "", "1908", "", "1916", "",  "1924", "", "1932", "", "1940")
xtics   <- seq(1884,1940,4)
df      <- cbind.data.frame(year,ests,ses) 

coefplot  <- ggplot(df, aes(x = year, y = ests)) +
  geom_hline(yintercept = 0, lty = 3, color = 'black') +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin = ests - 1.96 * ses, ymax = ests + 1.96 * ses), width = 0) +
  scale_x_continuous(breaks = xtics, labels = yrlabs) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme_bw() +
  labs(y = "Estimates", x = "Year") + 
  theme(text=element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_text(size = 13, color="black"),
        axis.text.x = element_text(size = 13, color="black"))

ggsave(coefplot, file="../Figures/Figure 4-5.pdf", width = 8, height = 6, units = "in")
