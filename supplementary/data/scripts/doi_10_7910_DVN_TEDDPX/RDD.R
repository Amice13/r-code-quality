### merge agreement percentages with survey data

# load data
#load("df2.RData")
#load("rdd_agreement.RData")


# remove second VAA attempts from vaa data

#rdd_agreement <- rdd_agreement %>%
 # arrange(ids, vaa_started_date) %>%  # Arrange by id and start date
#  distinct(ids, .keep_all = TRUE)

#rdd_agreement$ids <- as.character(rdd_agreement$ids)


# merge data
#rdf <- left_join(df2, rdd_agreement, by = "ids")

# save merged data
#save(rdf, file = "rdf.RData")


# load merged data
load("rdf.RData")


# turn agreement percentage characters into numeric values for all parties
rdf$leftperc_eu <- as.numeric(gsub("%", "", rdf$Left_EU))
rdf$eppperc_eu <- as.numeric(gsub("%", "", rdf$EPP_EU))
rdf$ecrperc_eu <- as.numeric(gsub("%", "", rdf$ECR_EU))
rdf$renewperc_eu <- as.numeric(gsub("%", "", rdf$Renew_EU))
rdf$sdperc_eu <- as.numeric(gsub("%", "", rdf$SD_EU))
rdf$idperc_eu <- as.numeric(gsub("%", "", rdf$ID_EU))
rdf$greenperc_eu <- as.numeric(gsub("%", "", rdf$Greens_EU))
rdf$leftperc_de <- as.numeric(gsub("%", "", rdf$Left_DE))
rdf$cduperc_de <- as.numeric(gsub("%", "", rdf$CDU_DE))
rdf$spdperc_de <- as.numeric(gsub("%", "", rdf$SPD_DE))
rdf$fdpperc_de <- as.numeric(gsub("%", "", rdf$FDP_DE))
rdf$afdperc_de <- as.numeric(gsub("%", "", rdf$AfD_DE))
rdf$greenperc_de <- as.numeric(gsub("%", "", rdf$Greens_DE))
rdf$lfiperc_fr <- as.numeric(gsub("%", "", rdf$La_France_Insoumise_FR))
rdf$eeperc_fr <- as.numeric(gsub("%", "", rdf$Europe_Ecologie_FR))
rdf$psperc_fr <- as.numeric(gsub("%", "", rdf$Parti_Socialiste_FR))
rdf$renaissanceperc_fr <- as.numeric(gsub("%", "", rdf$Renaissance_FR))
rdf$lrperc_fr <- as.numeric(gsub("%", "", rdf$Les_Republicains_FR))
rdf$rnperc_fr <- as.numeric(gsub("%", "", rdf$Rassamblement_National_FR))
rdf$reconqueteperc_fr <- as.numeric(gsub("%", "", rdf$Reconquete_FR))
rdf$fratelliperc_it <- as.numeric(gsub("%", "", rdf$Fratelli_dItalia_IT))
rdf$pdperc_it <- as.numeric(gsub("%", "", rdf$Partito_Democratico_IT))
rdf$movimentoperc_it <- as.numeric(gsub("%", "", rdf$Movimento_5_Stelle_IT))
rdf$legaperc_it <- as.numeric(gsub("%", "", rdf$Lega_IT))
rdf$forzaperc_it <- as.numeric(gsub("%", "", rdf$Forza_Italia_IT))

rdf2 <- rdf # save full data for heterogeneity analysis later

# reduce data to respondents who completed the VAA
rdf <- rdf %>% 
  filter(rnperc_fr > -1) # works with any party, since agreement percentages are calculated for all of them as long as respondents finished the VAA









#### create sub data for EU party families ####
# Reshape the dataset
rdf_eu <- rdf %>%
  pivot_longer(
    cols = ends_with("perc_eu"),  # Select columns ending with "perc_eu"
    names_to = "vaa_variable",        # New column for variable names 
    values_to = "agreement_perc"  # New column for values
  )


# create running variable from agreement percentages (2nd highest agreement percentage is -1, i.e., 0 is the minimum value needed to be the top-recommended party)
rdf_eu <- rdf_eu %>%
  group_by(ids) %>%
  mutate(
    # Get the second-highest unique value
    second_highest = unique(sort(agreement_perc, decreasing = TRUE))[2],
    # Subtract the second-highest value from all values
    rd_rv = agreement_perc - second_highest -1
  ) %>%
  ungroup()


# create treatment variable
rdf_eu$rd_treat <- NA
rdf_eu$rd_treat[rdf_eu$rd_rv > -1] <- 1
rdf_eu$rd_treat[rdf_eu$rd_rv < 0] <- 0



# define votechoice variable 
rdf_eu$votechoice <- NA

rdf_eu$country <- as.character(rdf_eu$country)

rdf_eu$votechoice[(rdf_eu$country == "1" & rdf_eu$v105 == 5) | 
                    (rdf_eu$country == "2" & rdf_eu$v105 == 1) | 
                    (rdf_eu$country == "3" & rdf_eu$v105 == 5)] <- "EPP"

rdf_eu$votechoice[(rdf_eu$country == "1" & rdf_eu$v105 == 7) | 
                    (rdf_eu$country == "3" & rdf_eu$v105 == 1)] <- "ECR"

rdf_eu$votechoice[(rdf_eu$country == "1" & rdf_eu$v105 == 6) | 
                    (rdf_eu$country == "2" & rdf_eu$v105 == 6) | 
                    (rdf_eu$country == "3" & rdf_eu$v105 == 4)] <- "ID"

rdf_eu$votechoice[(rdf_eu$country == "1" & rdf_eu$v105 == 3) | 
                    (rdf_eu$country == "2" & rdf_eu$v105 == 2) | 
                    (rdf_eu$country == "3" & rdf_eu$v105 == 2)] <- "SD"

rdf_eu$votechoice[(rdf_eu$country == "1" & rdf_eu$v105 == 1) | 
                    (rdf_eu$country == "2" & rdf_eu$v105 == 5)] <- "Left"

rdf_eu$votechoice[(rdf_eu$country == "1" & rdf_eu$v105 == 2) | 
                    (rdf_eu$country == "2" & rdf_eu$v105 == 4)] <- "Green"

rdf_eu$votechoice[(rdf_eu$country == "1" & rdf_eu$v105 == 4) | 
                    (rdf_eu$country == "2" & rdf_eu$v105 == 3)] <- "Renew"


# create rd votechoice dummy 1 (0 when respondent did not vote for party)
rdf_eu$rd_votechoice_1 <- 0

rdf_eu$rd_votechoice_1[(rdf_eu$vaa_variable == "ecrperc_eu" & rdf_eu$votechoice == "ECR") | 
                         (rdf_eu$vaa_variable == "eppperc_eu" & rdf_eu$votechoice == "EPP") |
                         (rdf_eu$vaa_variable == "greenperc_eu" & rdf_eu$votechoice == "Green") |
                         (rdf_eu$vaa_variable == "idperc_eu" & rdf_eu$votechoice == "ID") |
                         (rdf_eu$vaa_variable == "leftperc_eu" & rdf_eu$votechoice == "Left") |
                         (rdf_eu$vaa_variable == "renewperc_eu" & rdf_eu$votechoice == "Renew") |
                         (rdf_eu$vaa_variable == "sdperc_eu" & rdf_eu$votechoice == "SD") ] <- 1



# create rd votechoice dummy 2 (0 when respondent voted for other vaa family, NA when respondent did not vote for any vaa family)
rdf_eu$rd_votechoice_2 <- NA

rdf_eu$rd_votechoice_2[rdf_eu$votechoice == "ECR" | 
                         rdf_eu$votechoice == "EPP" |
                        rdf_eu$votechoice == "Green" |
                         rdf_eu$votechoice == "ID" |
                        rdf_eu$votechoice == "Left" |
                         rdf_eu$votechoice == "Renew" |
                         rdf_eu$votechoice == "SD" ] <- 0

rdf_eu$rd_votechoice_2[(rdf_eu$vaa_variable == "ecrperc_eu" & rdf_eu$votechoice == "ECR") | 
                         (rdf_eu$vaa_variable == "eppperc_eu" & rdf_eu$votechoice == "EPP") |
                         (rdf_eu$vaa_variable == "greenperc_eu" & rdf_eu$votechoice == "Green") |
                         (rdf_eu$vaa_variable == "idperc_eu" & rdf_eu$votechoice == "ID") |
                         (rdf_eu$vaa_variable == "leftperc_eu" & rdf_eu$votechoice == "Left") |
                         (rdf_eu$vaa_variable == "renewperc_eu" & rdf_eu$votechoice == "Renew") |
                         (rdf_eu$vaa_variable == "sdperc_eu" & rdf_eu$votechoice == "SD") ] <- 1












#### create sub data for national parties ####

# Reshape the dataset
rdf_nat <- rdf %>%
  pivot_longer(cols = ends_with(c("perc_de", "perc_fr", "perc_it")),
               names_to = "vaa_variable",
               values_to = "agreement_perc") %>%
  mutate(country_var = case_when(
    country == 1 & grepl("perc_fr$", vaa_variable) ~ "fr",
    country == 2 & grepl("perc_de$", vaa_variable) ~ "de",
    country == 3 & grepl("perc_it$", vaa_variable) ~ "it",
    TRUE ~ NA_character_  # Filter out irrelevant rows later
  )) %>%
  filter(!is.na(country_var))


# create running variable from agreement percentages (2nd highest agreement percentage is -1, i.e., 0 is the minimum value needed to be the top-recommended party)
rdf_nat <- rdf_nat %>%
  group_by(ids) %>%
  mutate(
    # Get the second-highest unique value
    second_highest = unique(sort(agreement_perc, decreasing = TRUE))[2],
    # Subtract the second-highest value from all values
    rd_rv = agreement_perc - second_highest -1
  ) %>%
  ungroup()

# create treatment variable
rdf_nat$rd_treat <- NA
rdf_nat$rd_treat[rdf_nat$rd_rv > -1] <- 1
rdf_nat$rd_treat[rdf_nat$rd_rv < 0] <- 0

rdf_nat$country <- as.character(rdf_nat$country)


# create rd votechoice dummy 1 (0 when respondent did not vote for party)
rdf_nat$rd_votechoice_1 <- 0

rdf_nat$rd_votechoice_1[(rdf_nat$vaa_variable == "afdperc_de" & rdf_nat$v105 == 6) |
                          (rdf_nat$vaa_variable == "cduperc_de" & rdf_nat$v105 == 1) |
                          (rdf_nat$vaa_variable == "fdpperc_de" & rdf_nat$v105 == 3) |
                          (rdf_nat$vaa_variable == "greenperc_de" & rdf_nat$v105 == 4) |
                          (rdf_nat$vaa_variable == "leftperc_de" & rdf_nat$v105 == 5) |
                          (rdf_nat$vaa_variable == "spdperc_de" & rdf_nat$v105 == 2) |
                          (rdf_nat$vaa_variable == "eeperc_fr" & rdf_nat$v105 == 2) |
                          (rdf_nat$vaa_variable == "lfiperc_fr" & rdf_nat$v105 == 1) |
                          (rdf_nat$vaa_variable == "lrperc_fr" & rdf_nat$v105 == 5) |
                          (rdf_nat$vaa_variable == "psperc_fr" & rdf_nat$v105 == 3) |
                          (rdf_nat$vaa_variable == "reconqueteperc_fr" & rdf_nat$v105 == 7) |
                          (rdf_nat$vaa_variable == "renaissanceperc_fr" & rdf_nat$v105 == 4) |
                          (rdf_nat$vaa_variable == "rnperc_fr" & rdf_nat$v105 == 6) |
                          (rdf_nat$vaa_variable == "forzaperc_it" & rdf_nat$v105 == 5) |
                          (rdf_nat$vaa_variable == "fratelliperc_it" & rdf_nat$v105 == 1) |
                          (rdf_nat$vaa_variable == "legaperc_it" & rdf_nat$v105 == 4) |
                          (rdf_nat$vaa_variable == "movimentoperc_it" & rdf_nat$v105 == 3) |
                          (rdf_nat$vaa_variable == "pdperc_it" & rdf_nat$v105 == 2)] <- 1



# create rd votechoice dummy 2 (0 when respondent voted for other vaa party, NA when respondent did not vote for any vaa party)

rdf_nat$rd_votechoice_2 <- NA

rdf_nat$rd_votechoice_2[(rdf_nat$country == "1" & rdf_nat$v105 < 8) |
                          (rdf_nat$country == "2" & rdf_nat$v105 < 7) |
                          (rdf_nat$country == "3" & rdf_nat$v105 < 6)] <- 0


rdf_nat$rd_votechoice_2[(rdf_nat$vaa_variable == "afdperc_de" & rdf_nat$v105 == 6) |
                          (rdf_nat$vaa_variable == "cduperc_de" & rdf_nat$v105 == 1) |
                          (rdf_nat$vaa_variable == "fdpperc_de" & rdf_nat$v105 == 3) |
                          (rdf_nat$vaa_variable == "greenperc_de" & rdf_nat$v105 == 4) |
                          (rdf_nat$vaa_variable == "leftperc_de" & rdf_nat$v105 == 5) |
                          (rdf_nat$vaa_variable == "spdperc_de" & rdf_nat$v105 == 2) |
                          (rdf_nat$vaa_variable == "eeperc_fr" & rdf_nat$v105 == 2) |
                          (rdf_nat$vaa_variable == "lfiperc_fr" & rdf_nat$v105 == 1) |
                          (rdf_nat$vaa_variable == "lrperc_fr" & rdf_nat$v105 == 5) |
                          (rdf_nat$vaa_variable == "psperc_fr" & rdf_nat$v105 == 3) |
                          (rdf_nat$vaa_variable == "reconqueteperc_fr" & rdf_nat$v105 == 7) |
                          (rdf_nat$vaa_variable == "renaissanceperc_fr" & rdf_nat$v105 == 4) |
                          (rdf_nat$vaa_variable == "rnperc_fr" & rdf_nat$v105 == 6) |
                          (rdf_nat$vaa_variable == "forzaperc_it" & rdf_nat$v105 == 5) |
                          (rdf_nat$vaa_variable == "fratelliperc_it" & rdf_nat$v105 == 1) |
                          (rdf_nat$vaa_variable == "legaperc_it" & rdf_nat$v105 == 4) |
                          (rdf_nat$vaa_variable == "movimentoperc_it" & rdf_nat$v105 == 3) |
                          (rdf_nat$vaa_variable == "pdperc_it" & rdf_nat$v105 == 2)] <- 1




### RDD ###
rdf_eu_rd <- rdf_eu %>% 
  drop_na(rd_rv)


eu_1 <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_1)

eu_2 <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_2)


# main specification with controls 

## Gender
rdf_eu_rd$v76 <- as.character(rdf_eu_rd$v76)

## Education
rdf_eu_rd$v75_2[rdf_eu_rd$v75_2 > 50 | rdf_eu_rd$v75_2 < 10] <- NA # excluding outliers and unrealistic values

eu_2c <- RDestimate(rd_votechoice_2 ~ rd_rv | country + age + v76 + v75_2, data = rdf_eu_rd, cutpoint = 0, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_2c)



rdf_nat_rd <- rdf_nat %>% 
  drop_na(rd_rv)


nat_1 <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_1)

nat_2 <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_2)

# main specification with controls 

## Gender
rdf_nat_rd$v76 <- as.character(rdf_nat_rd$v76)

## Education
rdf_nat_rd$v75_2[rdf_nat_rd$v75_2 > 50 | rdf_nat_rd$v75_2 < 10] <- NA # excluding outliers and unrealistic values

nat_2c <- RDestimate(rd_votechoice_2 ~ rd_rv | country + age + v76 + v75_2, data = rdf_nat_rd, cutpoint = 0, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_2c)


#customized plot function from rdd package
plot.RD <- function(x,gran=400,bins=100,which=1,range,...) {
  frm<-FALSE
  if("frame" %in% names(x$call)) frm<-eval.parent(x$call$frame)
  if(!frm){
    x$call$frame<-TRUE
    x$call$verbose<-FALSE
    x<-eval.parent(x$call)
  }
  d<-as.data.frame(x$frame)
  
  if(length(x$na.action)>0)
    d<-d[-x$na.action,]
  
  if("kernel" %in% names(x$call)) 
    kern<-eval.parent(x$call$kernel)
  else 
    kern<-"triangular"
  
  if("cutpoint" %in% names(x$call)) 
    cut<-eval.parent(x$call$cutpoint)
  else
    cut<-0
  
  bw<-x$bw[1]
  
  if(missing(range)) {
    range<-c(cut-10*bw,cut+10*bw)
    if(range[1]<min(d$X)) range[1]<-min(d$X)
    if(range[2]>max(d$X)) range[2]<-max(d$X)
  }
  
  if(range[1]=="min")
    range[1]<-min(d$X)
  if(range[2]=="max")
    range[2]<-max(d$X)
  range<-as.double(range)
  
  rdplot<-function(d) {
    d.l<-data.frame(X=d$X[d$X<cut],Y=d$Y[d$X<cut])
    lval<-seq(range[1],cut,length.out=(gran%/%2))
    lest<-vector(length=(gran%/%2))
    llwr<-vector(length=(gran%/%2))
    lupr<-vector(length=(gran%/%2))
    for(i in 1:(gran%/%2)) {
      sub<-d.l$X>=(lval[i]-bw) & d.l$X<=(lval[i]+bw)
      w<-kernelwts(X=d.l$X[sub],center=lval[i],bw=bw,kernel=kern)
      ly<-d.l$Y[sub]
      lx<-d.l$X[sub]
      if(length(lx)<=2)
        pred<-rep(NA,3)
      else
        pred<-predict(lm(ly~lx,weights=w),interval="confidence",newdata=data.frame(lx=lval[i]))
      lest[i]<-pred[1]
      llwr[i]<-pred[2]
      lupr[i]<-pred[3]
    }
    
    d.r<-data.frame(X=d$X[d$X>=cut],Y=d$Y[d$X>=cut])
    rval<-seq(cut,range[2],length.out=(gran%/%2))
    rest<-vector(length=(gran%/%2))
    rlwr<-vector(length=(gran%/%2))
    rupr<-vector(length=(gran%/%2))
    for(i in 1:(gran%/%2)) {
      sub<-d.r$X>=(rval[i]-bw) & d.r$X<=(rval[i]+bw)
      w<-kernelwts(X=d.r$X[sub],center=rval[i],bw=bw,kernel=kern)
      ry<-d.r$Y[sub]
      rx<-d.r$X[sub]
      if(length(rx)<=2)
        pred<-rep(NA,3)
      else
        pred<-predict(lm(ry~rx,weights=w),interval="confidence",newdata=data.frame(rx=rval[i]))
      rest[i]<-pred[1]
      rlwr[i]<-pred[2]
      rupr[i]<-pred[3]
    }
    
    #plot to the left
    if(length(unique(d$Y))==2) {
      #DO THIS for when the outcome is dichotomous
      ep<-(max(d$X)-min(d$X))/(2*bins)
      nX<-seq(min(d$X)-ep,max(d$X)+ep,length=bins+1)
      nY<-rep(NA,length(nX))
      for(i in (1:(length(nX)-1))){
        if(sum(!is.na(d$Y[d$X>nX[i] & d$X<=nX[i+1]]))==0)
          next
        nY[i]<-sum(d$Y[d$X>nX[i] & d$X<=nX[i+1]],na.rm=TRUE)/sum(!is.na(d$Y[d$X>nX[i] & d$X<=nX[i+1]]))
      }
      sub<-nX>=range[1] & nX<=range[2]
      subl<-lval>=range[1] & lval<=range[2]
      subr<-rval>=range[1] & rval<=range[2]
      plot(nX,nY,
           type="p",pch=20,cex=0,col="black",
           xlim=c(range[1],range[2]),
           ylim=c(min(c(llwr[subl],rlwr[subr]),na.rm=T),
                  max(c(lupr[subl],rupr[subr]),na.rm=T)),
           xlab=NA,
           ylab=NA,
           main=NA
      )
    } else {
      subl<-lval>=range[1] & lval<=range[2]
      subr<-rval>=range[1] & rval<=range[2]
      plot(d$X,d$Y,
           type="p",pch=20,cex=0,col="black",
           xlim=c(range[1],range[2]),
           ylim=c(min(c(llwr[subl],rlwr[subr]),na.rm=T),
                  max(c(lupr[subl],rupr[subr]),na.rm=T)),
           xlab=NA,
           ylab=NA,
           main=NA
      )
    } 
    #plot to the left
    lines(lval,lest,
          lty=1,lwd=6,col="#B22222",type="l"
    )
    
    lines(lval,llwr,
          lty=2,lwd=3,col="#B22222",type="l"
    )
    lines(lval,lupr,
          lty=2,lwd=3,col="#B22222",type="l"
    )
    
    #plot to the right
    lines(rval,rest,
          lty=1,lwd=6,col="#228B22",type="l"
    )
    lines(rval,rlwr,
          lty=2,lwd=3,col="#228B22",type="l"
    )
    lines(rval,rupr,
          lty=2,lwd=3,col="#228B22",type="l"
    )
  }
  if(x$type=="sharp" | 1%in%which){
    rdplot(d)
    dev.flush()
  }
  if(x$type=="fuzzy" & 2%in%which){
    d$Y<-d$Z
    rdplot(d)
    dev.flush()
  }
}

# rdd plot
par(mfrow = c(1,2))
plot(eu_1, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_1, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)



plot(eu_2, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_2, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)





## Estimate statistical power at different bandwidths

#EU
eu_out <- rdf_eu_rd$rd_votechoice_2
eu_run <- rdf_eu_rd$rd_rv

# for 100 bw
power100 <- rdpower(data=cbind(eu_out,eu_run), samph=cbind(100,100))
power50 <- rdpower(data=cbind(eu_out,eu_run), samph=cbind(50,50))
power25 <- rdpower(data=cbind(eu_out,eu_run), samph=cbind(25,25))


#Nat
nat_out <- rdf_nat_rd$rd_votechoice_2
nat_run <- rdf_nat_rd$rd_rv

# for 100 bw
power100 <- rdpower(data=cbind(nat_out,nat_run), samph=cbind(100,100))
power50 <- rdpower(data=cbind(nat_out,nat_run), samph=cbind(50,50))
power25 <- rdpower(data=cbind(nat_out,nat_run), samph=cbind(25,25))




## Estimate RDDs at different bandwidths

#EU Party Families
eu_2b <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 90, cluster = rdf_eu_rd$ids)
summary(eu_2b)

eu_2c <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 80, cluster = rdf_eu_rd$ids)
summary(eu_2c)

eu_2d <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 70, cluster = rdf_eu_rd$ids)
summary(eu_2d)

eu_2e <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 60, cluster = rdf_eu_rd$ids)
summary(eu_2e)

eu_2f <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 50, cluster = rdf_eu_rd$ids)
summary(eu_2f)

eu_2g <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 35, cluster = rdf_eu_rd$ids)
summary(eu_2g)

eu_2h <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = 0, bw = 27, cluster = rdf_eu_rd$ids)
summary(eu_2h)



# National Parties
nat_2b <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 90, cluster = rdf_nat_rd$ids)
summary(nat_2b)

nat_2c <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 80, cluster = rdf_nat_rd$ids)
summary(nat_2c)

nat_2d <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 70, cluster = rdf_nat_rd$ids)
summary(nat_2d)

nat_2e <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 60, cluster = rdf_nat_rd$ids)
summary(nat_2e)

nat_2f <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 50, cluster = rdf_nat_rd$ids)
summary(nat_2f)

nat_2g <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 35, cluster = rdf_nat_rd$ids)
summary(nat_2g)

nat_2h <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = 0, bw = 26, cluster = rdf_nat_rd$ids)
summary(nat_2h)



## Check ranges from (2nd/3rd) lowest to highest agreement

# EU
full_range_eu <- rdf_eu_rd %>%
  group_by(ids) %>%
  summarise(range_diff = max(agreement_perc, na.rm = TRUE) - min(agreement_perc, na.rm = TRUE)) %>%
  summarise(avg_diff = mean(range_diff, na.rm = TRUE))
full_range_eu


trunc_range_eu_2nd <- rdf_eu_rd %>%
  group_by(ids) %>%
  summarise(
    second_lowest = sort(unique(agreement_perc))[2],
    highest = max(agreement_perc, na.rm = TRUE),
    diff = highest - second_lowest
  ) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE))
trunc_range_eu_2nd # ~35

trunc_range_eu_3rd <- rdf_eu_rd %>%
  group_by(ids) %>%
  summarise(
    third_lowest = sort(unique(agreement_perc))[3],
    highest = max(agreement_perc, na.rm = TRUE),
    diff = highest - third_lowest
  ) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE))
trunc_range_eu_3rd # ~27




# Nat
full_range_nat <- rdf_nat_rd %>%
  group_by(ids) %>%
  summarise(range_diff = max(agreement_perc, na.rm = TRUE) - min(agreement_perc, na.rm = TRUE)) %>%
  summarise(avg_diff = mean(range_diff, na.rm = TRUE))
full_range_nat 


trunc_range_nat_2nd <- rdf_nat_rd %>%
  group_by(ids) %>%
  summarise(
    second_lowest = sort(unique(agreement_perc))[2],
    highest = max(agreement_perc, na.rm = TRUE),
    diff = highest - second_lowest
  ) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE))
trunc_range_nat_2nd # ~35


trunc_range_nat_3rd <- rdf_nat_rd %>%
  group_by(ids) %>%
  summarise(
    third_lowest = sort(unique(agreement_perc))[3],
    highest = max(agreement_perc, na.rm = TRUE),
    diff = highest - third_lowest
  ) %>%
  summarise(avg_diff = mean(diff, na.rm = TRUE))
trunc_range_nat_3rd # ~26



## Placebo cutoffs

#EU Party Families
# get median rv value of control group
control_df_eu <- rdf_eu_rd %>% 
  filter(rd_rv < 0)

median_eu <- median(control_df_eu$rd_rv)

# estimate placebo tests
eu_2medianp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = median_eu, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_2medianp)

eu_2bp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = -15, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_2bp)

eu_2cp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = -20, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_2cp)

eu_2dp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = -25, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_2dp)

eu_2ep <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd, cutpoint = -30, bw = 100, cluster = rdf_eu_rd$ids)
summary(eu_2ep)



# National Parties
# get median rv value of control group
control_df_nat <- rdf_nat_rd %>% 
  filter(rd_rv < 0)

median_nat <- median(control_df_nat$rd_rv)

# estimate placebo tests
nat_2medianp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = median_nat, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_2medianp)

nat_2bp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = -15, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_2bp)

nat_2cp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = -20, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_2cp)

nat_2dp <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = -25, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_2dp)

nat_2ep <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd, cutpoint = -30, bw = 100, cluster = rdf_nat_rd$ids)
summary(nat_2ep)





### RDD by country ###

## Germany 

rdf_eu_rd_de <- rdf_eu_rd %>% 
  filter(country == "2")

eu_1_de <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_eu_rd_de, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_de$ids)
summary(eu_1_de)

eu_2_de <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd_de, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_de$ids)
summary(eu_2_de)


rdf_nat_rd_de <- rdf_nat_rd %>% 
  filter(country == "2")

nat_1_de <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_nat_rd_de, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_de$ids)
summary(nat_1_de)

nat_2_de <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd_de, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_de$ids)
summary(nat_2_de)

# rdd plot
par(mfrow = c(1,2))
plot(eu_1_de, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_1_de, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)



plot(eu_2_de, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_2_de, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)






## France 

rdf_eu_rd_fr <- rdf_eu_rd %>% 
  filter(country == "1")

eu_1_fr <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_eu_rd_fr, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_fr$ids)
summary(eu_1_fr)

eu_2_fr <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd_fr, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_fr$ids)
summary(eu_2_fr)


rdf_nat_rd_fr <- rdf_nat_rd %>% 
  filter(country == "1")

nat_1_fr <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_nat_rd_fr, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_fr$ids)
summary(nat_1_fr)

nat_2_fr <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd_fr, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_fr$ids)
summary(nat_2_fr)

# rdd plot
par(mfrow = c(1,2))
plot(eu_1_fr, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_1_fr, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)



plot(eu_2_fr, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_2_fr, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)




## Italy 

rdf_eu_rd_it <- rdf_eu_rd %>% 
  filter(country == "3")

eu_1_it <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_eu_rd_it, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_it$ids)
summary(eu_1_it)

eu_2_it <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd_it, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_it$ids)
summary(eu_2_it)


rdf_nat_rd_it <- rdf_nat_rd %>% 
  filter(country == "3")

nat_1_it<- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_nat_rd_it, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_it$ids)
summary(nat_1_it)

nat_2_it <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd_it, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_it$ids)
summary(nat_2_it)

# rdd plot
par(mfrow = c(1,2))
plot(eu_1_it, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_1_it, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)



plot(eu_2_it, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)

plot(nat_2_it, gran = 25, range = c(-25,25))
abline(v=0, col="black", lwd = 4, lty = 1)










## RDDs after excluding respondents with NAs in the pre-electoral vote intention variable

## EU
rdf_eu_rd_cut <- rdf_eu_rd %>% 
  filter(v12 < 8)

eu_1_cut <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_eu_rd_cut, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_cut$ids)
summary(eu_1_cut)

eu_2_cut <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd_cut, cutpoint = 0, bw = 100, cluster = rdf_eu_rd_cut$ids)
summary(eu_2_cut)


## National
rdf_nat_rd_cut <- rdf_nat_rd %>% 
  filter(v12 < 8)

nat_1_cut <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_nat_rd_cut, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_cut$ids)
summary(nat_1_cut)

nat_2_cut <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_nat_rd_cut, cutpoint = 0, bw = 100, cluster = rdf_nat_rd_cut$ids)
summary(nat_2_cut)












## RDDs split by ideological closeness between top- and second-recommended party
rdf_eu_rd$lr_party <- NA
rdf_eu_rd$lr_party[rdf_eu_rd$vaa_variable == "leftperc_eu" |
                     rdf_eu_rd$vaa_variable == "sdperc_eu" |
                     rdf_eu_rd$vaa_variable == "greenperc_eu"] <- "left"

rdf_eu_rd$lr_party[rdf_eu_rd$vaa_variable == "eppperc_eu" |
                     rdf_eu_rd$vaa_variable == "ecrperc_eu" |
                     rdf_eu_rd$vaa_variable == "idperc_eu" |
                     rdf_eu_rd$vaa_variable == "renewperc_eu"] <- "right"
table(rdf_eu_rd$lr_party, rdf_eu_rd$vaa_variable)


aligned_by_id <- rdf_eu_rd %>%
  group_by(ids) %>%
  mutate(rnk = dense_rank(desc(agreement_perc))) %>%
  reframe(
    aligned_one_two = {
      n1 <- sum(rnk == 1L)   
      n2 <- sum(rnk == 2L)   
      if (n1 != 1L || n2 != 1L) {
        NA_integer_          
      } else {
        p1 <- lr_party[rnk == 1L][1]
        p2 <- lr_party[rnk == 2L][1]
        as.integer(p1 == p2) 
      }
    }
  )

rdf_eu_rd <- rdf_eu_rd %>%
  left_join(aligned_by_id, by = "ids")


rdf_eu_rd_aligned <- rdf_eu_rd %>% 
  filter(aligned_one_two == 1)

rdf_eu_rd_unaligned <- rdf_eu_rd %>% 
  filter(aligned_one_two == 0)


eu_1_aligned <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_eu_rd_aligned, 
                           cutpoint = 0, bw = 100, cluster = rdf_eu_rd_aligned$ids)
summary(eu_1_aligned)

eu_2_aligned <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd_aligned, 
                           cutpoint = 0, bw = 100, cluster = rdf_eu_rd_aligned$ids)
summary(eu_2_aligned)




eu_1_unaligned <- RDestimate(rd_votechoice_1 ~ rd_rv, data = rdf_eu_rd_unaligned, 
                           cutpoint = 0, bw = 100, cluster = rdf_eu_rd_unaligned$ids)
summary(eu_1_unaligned)

eu_2_unaligned <- RDestimate(rd_votechoice_2 ~ rd_rv, data = rdf_eu_rd_unaligned, 
                           cutpoint = 0, bw = 100, cluster = rdf_eu_rd_unaligned$ids)
summary(eu_2_unaligned)






