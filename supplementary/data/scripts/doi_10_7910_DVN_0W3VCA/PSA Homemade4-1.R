##FOR POTENTIAL USERS: YOU NEED TO AMEND THE DIRECTORIES##

rm(list=ls())
setwd("C:/Users/skl/Dropbox/@@Aufgaben/02aPROJEKTE TOP/PJ 3i Replication Study/@MEA/CE Analysis/RCode")

pfad.bild <- "C:\\Users\\skl\\Dropbox\\@@Aufgaben\\02aPROJEKTE TOP\\PJ 3i Replication Study\\@MEA\\CE Analysis\\Bilder\\"

source("PSA_Functions.R")

###############Specifying Inputs
n.psa         <- 10000 # 100 # 1000
#WTP           <- seq(0,5000,10) # Vector with the lambda valules /willingness to pay
WTP           <- c(seq(0,1000,10),seq(1100,10000,100))
dependent     <- c("Log caloric intake", "HDDS", "DDI",	"FCS",	"PFC")
cost.scenario <- "base.case"


#dependent <- "Log caloric intake"

par(family="sans",font=1,font.main=1)
par(oma=c(0,0,0,0))


##########costs
{costs.m <-NULL
costs.se <-NULL

if(cost.scenario == "base.case"){
costs.m         <- c(17.97, 68.75, 19.61)
costs.se        <- 0.3 * costs.m 
}

names(costs.m)    <- c("cash","food","vouchers")
names(costs.se)   <- c("cash","food","vouchers")
}

##########Effects Data -> "effects"
{effects <- vector("list",5)
names(effects) <- dependent
effects <- lapply(effects,function(x)   list("m"= matrix(nrow=1,ncol=3, dimnames=list(c("row.1"),c("cash","food","vouchers"))),
                                             "cov"= matrix(nrow=3,ncol=3, dimnames=list(c("cash","food","vouchers"),c("cash","food","vouchers")))
                                               ))
effects[["Log caloric intake"]]$m     <- c(0.12,0.21,0.18)
effects[["HDDS"]]$m            <- c(0.47,0.61,0.6)
effects[["DDI"]]$m            <-  c(2.64,2.36,3.13)
effects[["FCS"]]$m            <-  c(6.57,6.96,9.56)
effects[["PFC"]]$m            <-  c(-0.02,-0.05,-0.04) *-1 #(for ease of interpreation)

effects[["Log caloric intake"]]$cov  <- rbind(c(0.0014132,0.00088,0.0008396),c(0.00088,0.0017623,0.0008443),c(0.0008396,0.0008443,0.001525))
effects[["HDDS"]]$cov           <- rbind(c(0.0130392,0.0097238,0.0098744),c(0.0097238,0.014538,0.0098887),c(0.0098744,0.0098887,0.0144767))
effects[["DDI"]]$cov          <- rbind(c(0.1742087,0.1090727,0.1090064),c(0.1090727,0.1965672,0.1103768),c(0.1090064,0.1103768,0.2018117))
effects[["FCS"]]$cov           <- rbind(c(1.675531,0.6713726,0.6796979),c(0.6713726,1.480792,0.7092519),c(0.6796979,0.7092519,1.93101))
effects[["PFC"]]$cov            <- rbind(c(0.0002637,0.0001517,0.0001535),c(0.0001517,0.0002562,0.0001562),c(0.0001535,0.0001562,0.0002262))

  for(dep in 1:length(dependent)){
    names(effects[[dependent[dep]]]$m) <- c("cash","food","vouchers")
    dimnames(effects[[dependent[dep]]]$cov) <-      list(c("cash","food","vouchers"),c("cash","food","vouchers"))
  }
lapply(effects,function(x) sum(x$m)) #to check with Excel
sum(sapply(effects,function(x) x$m[["cash"]])) #to check with Excel
lapply(effects,function(x) sum(x$cov)) #to check with Excel
}

#######################################
######Efficiency Frontier
######################plot
win.metafile(file = paste(pfad.bild,"EFpub",".wmf",sep=""))#, width=12, height=8)
{par(mfrow=c(3,2))
par(mar=c(5,4,2,2))

for(dep in 1:length(dependent)){
plot(effects[[dependent[dep]]]$m,costs.m,pch="",xlab="effect",ylim=c(0,80), xlim=c(0,max(effects[[dependent[dep]]]$m)*1.10),ylab="costs",pty="m")
text(effects[[dependent[dep]]]$m["cash"],costs.m["cash"],"C")
text(effects[[dependent[dep]]]$m["food"],costs.m["food"],"F")
text(effects[[dependent[dep]]]$m["vouchers"],costs.m["vouchers"],"V")
title(dependent[dep])
print(effects[[dependent[dep]]]$m)
}
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", c("cash","food","vouchers"),pch = c("C","F","V"))
}
dev.off()
################################# 
###PSA

##draw costs 
{##output ist data frame with cost draws

gamma_mom <- function(mean, sd){
  if (mean > 0){
    theta <- sd^2/mean
    kappa <- mean/theta
  } else{
    stop("Mean must be positive")
  }
  return(list(kappa = kappa, theta = theta))
}
#preparing the input for the draw using mean and SE to calculate methods of Gamma Distribution
costs.psa.input               <- NULL
costs.psa.input               <- list("cash"=NA,"food"=NA,"vouchers"=NA)
costs.psa.input[["cash"]]     <- gamma_mom(costs.m["cash"],costs.se["cash"]) 
costs.psa.input[["food"]]     <- gamma_mom(costs.m["food"],costs.se["food"]) 
costs.psa.input[["vouchers"]] <- gamma_mom(costs.m["vouchers"],costs.se["vouchers"]) 

draws.costs <- NULL
draws.costs <- cbind(rgamma(n.psa, shape = costs.psa.input[["cash"]]$kappa, scale = costs.psa.input[["cash"]]$theta),
                     rgamma(n.psa, shape = costs.psa.input[["food"]]$kappa, scale = costs.psa.input[["food"]]$theta),
                     rgamma(n.psa, shape = costs.psa.input[["vouchers"]]$kappa, scale = costs.psa.input[["vouchers"]]$theta))

colnames(draws.costs) <- c("cash","food","vouchers")
head(draws.costs)            
apply(draws.costs,2,mean) 
apply(draws.costs,2,sd) 
}

#draw effects
draws.effects           <- NULL
draws.effects <- vector("list",5)
names(draws.effects) <- dependent
for(dep in 1:length(dependent)){

  draws.effects[[dependent[dep]]]   <- as.data.frame(rmnorm(n.psa,effects[[dependent[dep]]]$m,effects[[dependent[dep]]]$cov,sqrt=NULL))
}
head(draws.effects)

#############################

###plot CE Example

win.metafile(file = paste(pfad.bild,"CEPlanePub",".wmf",sep=""))#, width=4, height=4)
{par(mfrow=c(1,2))
par(mar=c(5,4,2,2))

plot(draws.effects[[dependent[1]]][,"food"],draws.costs[,"food"],pch = "F", xlab="effect", ylab="costs",  axes=TRUE,ylim=c(-1,200),xlim=c(-.05,.4))
points(draws.effects[[dependent[1]]][,"cash"],draws.costs[,"cash"],pch = "C")
points( draws.effects[[dependent[1]]][,"vouchers"],draws.costs[,"vouchers"],pch = "V")

plot(draws.effects[[dependent[1]]][,"food"][1:500],draws.costs[,"food"][1:500],pch = "F", xlab="effect", ylab="costs",  axes=TRUE,ylim=c(-1,200),xlim=c(-.05,.4))
points(draws.effects[[dependent[1]]][,"cash"][1:500],draws.costs[,"cash"][1:500],pch = "C")
points( draws.effects[[dependent[1]]][,"vouchers"][1:500],draws.costs[,"vouchers"][1:500],pch = "V")
box()
}
dev.off()

 

######################
## CEAC
{
CEAC.list        <- vector("list",5)                 #create list object
names(CEAC.list) <- dependent                        #name first level of list 
CEAC.list        <- lapply(CEAC.list,function(x)   list("NMB"=NULL,"NMB.max.share"=NULL, "CEAC.share"=NULL))  #create sublists 


for(dep in 1:length(dependent)){#for each dep variabl 
  
  {NMB <-list() 
  
  for(i in 1:length(WTP)){#loop for each lambda caculate the NMB
    NMB[[paste(WTP[i])]] <- as.data.frame(WTP[i] * draws.effects[[dependent[dep]]] - draws.costs) #### NMB = WTP * effect - costs => for each WTP, the matrix of effects / costs is used
    }#rsult ist that for lambda has a dataframe with the NMB
  
 {NMB.max.share <- list() 
    for(i in 1:length(WTP)){ #for each lambda, calculates for each simulation run the optimal intervention
    NMB.max.share[[paste(WTP[i])]] <-colnames(NMB[[paste(WTP[i])]])[apply(NMB[[paste(WTP[i])]],1,which.max)]} #for each run, I calculate which.max and yield the column name 
  }
  
  
  CEAC.share            <- matrix(ncol=4,nrow=length(WTP))
  colnames(CEAC.share)  <- c("WTP","cash","food","vouchers")
  
  for(i in 1:length(WTP)){ #for each lambda, counts how often an intervention was optimal
    CEAC.share[i,] <- cbind(WTP[i],  
                            sum(str_count(NMB.max.share[[paste(WTP[i])]],string="cash")),
                            sum(str_count(NMB.max.share[[paste(WTP[i])]],string="food")),
                            sum(str_count(NMB.max.share[[paste(WTP[i])]],string="vouchers")))
  }
  CEAC.list[[dependent[dep]]]$NMB            <-NMB
  CEAC.list[[dependent[dep]]]$NMB.max.share  <- NMB.max.share
  CEAC.list[[dependent[dep]]]$CEAC.share     <- CEAC.share 
  rm(NMB,NMB.max.share,CEAC.share)
    }
}
}



#########plot CEAC
win.metafile(file = paste(pfad.bild,"CEACpub",".wmf",sep=""))#, width=12, height=8)
{par(mfrow=c(3,2))
  par(mar=c(5,4,2,2))
  
 
  for(dep in 1:length(dependent)){

plot(CEAC.list[[dependent[dep]]]$CEAC.share[,"WTP"], CEAC.list[[dependent[dep]]]$CEAC.share[,"cash"]/n.psa,type="l",log="x",lty="solid",lwd=2,ylim=c(0,1),ylab=c("Probability","of being cost-effective"), xlab=c("WTP (in USD; log scale)"))
lines(CEAC.list[[dependent[dep]]]$CEAC.share[,"WTP"],CEAC.list[[dependent[dep]]]$CEAC.share[,"food"]/n.psa,lty="longdash",lwd=2)
lines(CEAC.list[[dependent[dep]]]$CEAC.share[,"WTP"],CEAC.list[[dependent[dep]]]$CEAC.share[,"vouchers"]/n.psa,lty="dotted",lwd=2)
title(dependent[dep])

}

plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center", c("cash","food","vouchers"),lty = c("solid","longdash","dotted"))
}
dev.off()
##########################################

#CEAF
{ CEAF.list        <- vector("list",5)   #create list object
  names(CEAF.list) <- dependent          #name first level of list 
  CEAF.list        <- lapply(CEAF.list,function(x)   list("NMB.mean"=NULL,"CEAF"=NULL))#, "CEAC.share"=NULL)) #create sublists

  for(dep in 1:length(dependent)) { 

        NMB.mean <-list()
        for(i in 1:length(WTP)){#loop for each lambda caculate the NMB based on MEAN draws
          NMB.mean[[paste(WTP[i])]] <-  WTP[i] * apply(draws.effects[[dependent[dep]]],2,mean)  - apply(draws.costs,2,mean)  
        }#list that for lambda has a dataframe with the NMB

    CEAF              <- as.data.frame(matrix(ncol=2,nrow=length(WTP)))
    colnames(CEAF)    <- c("WTP","CEAC.Share")
                   
  for( i in 1:length(WTP)){
        CEAF[i,] <- cbind(WTP[i],
                    CEAC.list[[dependent[dep]]]$CEAC.share[i,names(which.max(NMB.mean[[paste(WTP[i])]]))]) # for a given WTP, which Strat has the highest NMB
      }
      
  CEAF.list[[dependent[dep]]]$CEAF     <- CEAF
  CEAF.list[[dependent[dep]]]$NMB.mean <- NMB.mean
  rm(CEAF)
  }
}

  
#plot(CEAF.list[[dependent[dep]]]$CEAF[,"WTP"],CEAF.list[[dependent[dep]]]$CEAF[,"CEAC.Share"]/n.psa,type="l",ylim=c(0,1))
#lines(CEAF.list[[dependent[dep]]]$CEAC.share[,"WTP"],CEAF.list[[dependent[dep]]]$CEAC.share[,"food"]/n.psa,type="l")

#####EVPI
{
EVPI.list        <- vector("list",5)
names(EVPI.list) <- dependent  
 
  for(dep in 1:length(dependent)) { 
        EVPI           <- as.data.frame(matrix(ncol=4,nrow=length(WTP)))
        colnames(EVPI) <- c("WTP","NMB.perfect","NMB.Current","EVPI")

              for(i in 1:length((WTP))){
                EVPI[i,"WTP"]         <-  WTP[i]
                EVPI[i,"NMB.perfect"] <-  mean(apply((CEAC.list[[dependent[dep]]]$NMB[[paste(WTP[i])]]),1,max))
                EVPI[i,"NMB.Current"] <-  max(CEAF.list[[dependent[dep]]]$NMB.mean[[paste(WTP[i])]]) 
                EVPI[i,"EVPI"]        <-  EVPI[i,"NMB.perfect"] -   EVPI[i,"NMB.Current"]
              }
        
     EVPI.list[[dependent[dep]]]$EVPI <- EVPI
     rm(EVPI)
  }
}

#clipboard(CEAC.list[[dependent[dep]]]$NMB[[paste(WTP[i])]])
 


#########plot EVPI
win.metafile(file = paste(pfad.bild,"EVPIpublogxy",".wmf",sep=""))#, width=12, height=8)
{par(mfrow=c(3,2))
  par(mar=c(5,4,2,2))
  
  
  for(dep in 1:length(dependent)){
    plot(EVPI.list[[dependent[dep]]]$EVPI[,"WTP"],EVPI.list[[dependent[dep]]]$EVPI[,"EVPI"] , type="l", log="yx",ylab=c("EVPI","(in USD; log scale)"), xlab=c("WTP (in USD; log scale)") )
     title(dependent[dep])
    
  }
  
  plot(0,type='n',axes=FALSE,ann=FALSE)
  #legend("center", c("cash","food","vouchers"),lty = c("solid","longdash","dotted"))
}
dev.off()



clipboard(c(EVPI.list[[dependent[1]]]$EVPI[,"WTP"],EVPI.list[[dependent[1]]]$EVPI[,"EVPI"]))
########
### evpi mit NHB

NMB <-list()

for(i in 1:length(WTP)){#loop for each lambda caculate the NMB
  NMB[[paste(WTP[i])]] <- as.data.frame(draws.effects - draws.costs/ WTP[i] )
}#l

NMB.mean <-list()

for(i in 1:length(WTP)){#loop for each lambda caculate the NMB based on MEAN draws
  NMB.mean[[paste(WTP[i])]] <-   apply(draws.effects,2,mean)  - apply(draws.costs,2,mean) / WTP[i] 
}#list that for lambda has a dataframe with the NMB

EVPI           <- as.data.frame(matrix(ncol=4,nrow=length(WTP)))
colnames(EVPI) <- c("WTP","NMB.perfect","NMB.Current","EVPI")

for(i in 1:length((WTP))){
  EVPI[i,"WTP"] <- WTP[i]
  EVPI[i,"NMB.perfect"] <-  mean(apply((NMB[[paste(WTP[i])]]),1,max))
  EVPI[i,"NMB.Current"] <-  max(NMB.mean[[paste(WTP[i])]])
  EVPI[i,"EVPI"]        <-  EVPI[i,"NMB.perfect"] -   EVPI[i,"NMB.Current"]
}

plot(EVPI[,"WTP"],EVPI[,"EVPI"]     , type="l")

