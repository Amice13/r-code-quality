##########
#### Assessing Ballot Structure and Split Ticket Voting
#### Barnes, Tchintian, Alles
#### Journal of Politics, forthcoming.
##########

#### updated: 06/25/2016

#### Regression Models
#### by Year & Party

for(j in 1:length(match.method)){
  
  #### Method Directories
  
  DinD.store.dir <- paste(DinD.dir, paste(match.folder[j], "Models"), sep="/")
  dir.create(DinD.store.dir, showWarnings=F)
  
  ### Data subset
  
  loop.dat <- subset(DinD.data, matching.method == unique(match.method)[j])
  loop.dat <- subset(loop.dat, gis.method == unique(dat$gis.method)[m])
  loop.dat <- subset(loop.dat, year == unique(dat$year)[k] | year == unique(dat$year)[k] - 4)
  
  loop.dat$year_dummy <- ifelse(loop.dat$year==2011, yes=1, no=0)
  
  ### Difference in Difference table
  
  store <- data.frame(matrix(ncol=2, nrow=2))
  colnames(store) <- c("control.group", "treated.group")
  year <- c()
  
  for(yd in c(0,1)){
    year[yd+1] <- ifelse(yd==1, 2011, unique(loop.dat$year)[unique(loop.dat$year)!=2011])
    
    for(ed in c(0,1)){
      store[yd+1, ed+1] <- signif(sapply(split(loop.dat, loop.dat$year_dummy==yd & loop.dat$electronic.device==ed), 
                                         function(x){weighted.mean(x$split.share, w = x$weights)}), digits=5)[2]
      
      
    }
  }
  
  store <- orderBy(~ treat, data = data.frame(year, treat=c(0,1), store))
  
  store[3,3:4] <- round(as.numeric(store[2,3:4]-store[1,3:4]),4)
  store$difference <- round(store[,"treated.group"] - store[,"control.group"],4)
  store$year <- with(store, ifelse(is.na(year),"--",year))
  
  electronic.device <- with(store, ifelse(year==2007, "No Implementation", 
                                          ifelse(year==2011, "Partial Implementation", 
                                                 ifelse(year==2015, "Full Implementation",
                                                        "Difference"))))
  store <- data.frame(electronic.device, store)
  store <- store[!colnames(store)%in%"treat"]
  
  
  ### Weighted T-Test
  
  loop.dat$group <- with(loop.dat, paste(ifelse(year_dummy==1,"t","c"),ifelse(electronic.device==1,"t","c"), sep=""))
  
  tc_cc <- wtd.t.test(x=subset(loop.dat, group=="tc")$split.share, y=subset(loop.dat, group=="cc")$split.share, 
             weight=subset(loop.dat, group=="tc")$weights, weighty=subset(loop.dat, group=="cc")$weights,
             samedata=F)
  
  tt_ct <- wtd.t.test(x=subset(loop.dat, group=="tt")$split.share, y=subset(loop.dat, group=="ct")$split.share, 
             weight=subset(loop.dat, group=="tt")$weights, weighty=subset(loop.dat, group=="ct")$weights,
             samedata=F)
  
  ct_cc <- wtd.t.test(x=subset(loop.dat, group=="ct")$split.share, y=subset(loop.dat, group=="cc")$split.share, 
             weight=subset(loop.dat, group=="ct")$weights, weighty=subset(loop.dat, group=="cc")$weights,
             samedata=F)
  
  tt_tc <- wtd.t.test(x=subset(loop.dat, group=="tt")$split.share, y=subset(loop.dat, group=="tc")$split.share, 
             weight=subset(loop.dat, group=="tt")$weights, weighty=subset(loop.dat, group=="tc")$weights,
             samedata=F)
  
  t.test <- rbind(data.frame(group="Device: Control. Year: Treated vs. Control", 
                             Difference=tc_cc$additional[1], data.frame(t(tc_cc$coefficients)), 
                             row.names=NULL),
                  data.frame(group="Device: Treatment. Year: Treated vs. Control", 
                             Difference=tt_ct$additional[1], data.frame(t(tt_ct$coefficients)), 
                             row.names=NULL),
                  data.frame(group="Year: Control. Device: Treatment vs. Control", 
                             Difference=ct_cc$additional[1], data.frame(t(ct_cc$coefficients)), 
                             row.names=NULL),
                  data.frame(group="Year: Treated. Device: Treatment vs. Control", 
                             Difference=tt_tc$additional[1], data.frame(t(tt_tc$coefficients)), 
                             row.names=NULL))

  t.test$t.value <- with(t.test, round(t.value,4))
  t.test$df <- with(t.test, round(df,4))
  t.test$Difference <- with(t.test, round(Difference,4))
  t.test$p.value <- with(t.test, format(p.value, digits=5))

  rm(tc_cc, tt_ct, ct_cc, tt_tc)
  
  ### Interaction model
  
  interact.mod <- lm(split.share ~ year_dummy + electronic.device + year_dummy * electronic.device, 
                     data = loop.dat,  weight=weights)
  
  
  ### Table 1 (Article) and Tables D1, D2 & D3 (Appendix)
  
  rep.file <- paste("Diff-in-Diff", paste(unique(loop.dat$year), collapse="-"), 
                    paste(gis.acro, match.method[j], sep="."))
  if("file"%in%ls()) rep.file <- paste(rep.file, file, sep="_")
  
  
  cat("Difference in Difference table",
      "-------------------------------",
      paste("Party Sample:",sample),
      paste("Matching Method:",match.folder[j]),
      paste("GIS Method:",gis.method),
      paste("Years:",paste(unique(loop.dat$year), collapse="-")), "",
      capture.output(store), "",
      "\nWeighted Student's t-Test:",
      "---------------------------",
      capture.output(t.test),
      "\nInteraction model:",
      "-------------------",
      capture.output(summary(interact.mod)),
      file = paste(DinD.store.dir, paste(rep.file, "txt", sep="."), sep="/"), 
      sep="\n", append=F)

  
  rm(yd, ed, electronic.device, year, interact.mod, store, t.test, loop.dat, rep.file, DinD.store.dir)
  
  
}

rm(j)

