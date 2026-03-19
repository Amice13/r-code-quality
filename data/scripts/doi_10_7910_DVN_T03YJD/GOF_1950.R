#######################################
### GOF 1950 Model estimation

# get data
load(file="scripts/cERGM_data.RData")
load(file="scripts/Model_fit_results.RData")

library(statnet)
library(MLmetrics)
library(xtable)
library(tc.ergmterms)

set.seed(12123)
# there is a case in 1962 marked as being from 1963, and one from 1971 marked as 1972
scc1[3364,11] <- 1962
scc1[4648,11] <- 1971

year.total <- scc1[,11]-1936 #-1936 => the first year 1937 is 1, 1938 is 2 aso, term column 

t <- 14 # 14 refers to 1950
print(t+1936)

cases <- max(which(year.total==t)) # 14 corresponds to 1950

# simple assignment of time periods to cases
years <- year.total[1:cases] 

# going to need a sender time matrix covariate
sender.time <- matrix(years,length(years),length(years),byrow=F)
year <- matrix(years, length(years),length(years),byrow=F)

# extract the network up to time t
AM <- adjacency.matrix[which(years <= t),which(years <= t)]

net.t <- network(AM) 

#calculate outdegree
o.degree <- rowSums(AM)

# fix the outdegrees of time t as 0
last.t <- which(years==t)

o.degree[last.t] <- 0

# indicate outdegree and term as a node attribute
net.t <- set.vertex.attribute(net.t,"o.degree", o.degree)
net.t <- set.vertex.attribute(net.t,"term", years)

# subset MQ matrix
mq.t <- mq.matrix[which(years <= t),which(years <= t)]
# subset same issue matrix
same.issue.area.t <- same.issue.area[which(years <= t),which(years <= t)]
# subset year diff matrix
year.diff.t <- year.diff.matrix[which(years <= t),which(years <= t)]
# subset year diff matrix square
year.diff.square.t <- year.diff.t^2
# subset sender time matrix
sender.time.t <- sender.time[which(years <= t),which(years <= t)]
# subset year matrix
year.t<- year[which(years <= t),which(years <= t)]
# subset same opinion writer matrix
same.opinion.writer.t<- same.opinion.writer[which(years <= t),which(years <= t)]

#### set vertex attributes
# same issue area
net.t <- set.vertex.attribute(net.t,"SameIssueArea", scc1[which(years <= t),41])
# abs diff of MQ score
net.t <- set.vertex.attribute(net.t,"AbsDiffMQscores", scc1[which(years <= t),65])
# number justices that voted for the case
net.t <- set.vertex.attribute(net.t,"NumberJusticesPro", scc1[which(years <= t),52])
# overruled covariate
net.t <- set.vertex.attribute(net.t,"Overruled", Overruled.matrix[which(years <= t),t])
# sender time
net.t <- set.vertex.attribute(net.t,"sender.time", sender.time.t[,1])
# Majority Opinion Writer
net.t <- set.vertex.attribute(net.t,"MajOpWriter", scc1[which(years <= t),49])

not.fixed <- network(1*(sender.time == t))

li <- li50[[1]]

# fitting glm
model <- ergm(net.t~edges+ mutual + nodeicov("o.degree")+ difftransties("term")+gwidegree(1, fixed=TRUE)+ dgwesp(0.15, fixed=TRUE, type="OSP") +
                edgecov(mq.t)+
                edgecov(year.diff.t)+ edgecov(year.diff.square.t )+ 
                nodeicov('AbsDiffMQscores')+ nodeicov('NumberJusticesPro')+ 
                nodeicov('Overruled')+
                edgecov(same.opinion.writer.t)  + edgecov(same.issue.area.t) ,
              constraints=~fixallbut(not.fixed), control=control.ergm(MCMC.samplesize=10000,MCMLE.maxit=50, init=li), eval.loglik=FALSE) #
summary(model) 

# get statistics
erg.est <- summary(net.t~edges+ mutual + nodeicov("o.degree")+ difftransties("term")+gwidegree(1, fixed=TRUE)+ 
                     dgwesp(0.15, fixed=TRUE, type="OSP") +
                     edgecov(mq.t)+
                     edgecov(year.diff.t)+ edgecov(year.diff.square.t )+ nodeicov('AbsDiffMQscores')+ nodeicov('NumberJusticesPro')+ 
                     nodeicov('Overruled')+edgecov(same.opinion.writer.t) + edgecov(same.issue.area.t)  )
erg.est

sim.stat <- simulate(model, nsim=1000, output='stats' , coef= coef(model),   #istar(2)+mutual + ostar(2)+ triangles
                     constraints=~fixallbut(not.fixed), basis= net.t, control=control.simulate.ergm(MCMC.burnin=2000000, MCMC.interval=500000) )

sim <- simulate(model, nsim=1000, coef= coef(model),  
                constraints=~fixallbut(not.fixed), basis= net.t, control=control.simulate.ergm(MCMC.burnin=2000000, MCMC.interval=500000) )

# code for simulated networks
ideg.matrix <- matrix(0, 1000, 21)
colnames(ideg.matrix)<- 0:20

odeg.matrix <- matrix(0, 1000, 21)
colnames(odeg.matrix)<- 0:20

esp.matrix.ost <- matrix(0, 1000, 21)
colnames(esp.matrix.ost)<- 0:20

esp.matrix.osp <- matrix(0, 1000, 21)
colnames(esp.matrix.osp)<- 0:20

for(i in 1:1000){
   A <- as.matrix.network(sim[[i]])
  
  # determine the unfixed nodes
  unfixed <- which(years==t)
  
  # get outdegree of these nodes
  unfixed.odeg <- rowSums(A[unfixed,])
  
  # get indegree of these nodes
  unfixed.ideg <- colSums(A[unfixed,])
  
  net.t <- sim[[i]]
  
  # get edgewise shared partner distribution
  full.esp <- summary(net.t ~ esp(0:20)) # this is esp dist if entire network is unfixed
  
  # get esp dist of t-1
  A.red <- A[-unfixed, -unfixed]
  net.r <- network(A.red) 
  
  # get edgewise shared partner distribution
  red.esp <- summary(net.r ~ esp(0:20))
  
  unfixed.esp.ost <- full.esp-red.esp
  
  esp.matrix.ost[i,] <- unfixed.esp.ost
  
  w <- which(sender.time[,1]==t)  
  
  esp <- rep(0,21)
  for(u in w){
    
    for(j in 1:nrow(sender.time)){
      if(A[u,j]==1){
        es <- 0
        for(k in 1:nrow(sender.time)){
          if( A[u,j]*A[u,k]*A[j,k]==1){
            es <- es+1 
          } # if ==1
        } # for k
        if(es< 21){
          esp[es+1] <- esp[es+1] +1
        } # end if(es<21)
      }# end if(A[i,j]==1)
    } #for j
  }# for u
  
  
  esp.matrix.osp[i,] <- esp
  
  for(k in 0:20){
    s<- which(names(table(unfixed.odeg))==k   )
    r<- which(names(table(unfixed.ideg))==k   )
    
    if(length(s)!=0){
      odeg.matrix[i,k+1] <- table(unfixed.odeg)[s]
    }
    
    if(length(r)!=0){
      ideg.matrix[i,k+1] <- table(unfixed.ideg)[r]
    }
  }
}

li <- list(odeg.matrix, ideg.matrix, esp.matrix.ost, esp.matrix.osp, model, sim.stat, erg.est)


#################################################
#################################################
###     GOF for 1950 for OSP and OST Plotting
#################################################
#################################################


# calculate indegree, outdegree and esp of unfixed nodes of observed network

year.total <- scc1[,11]-1936 #-1936 => the first year 1937 is 1, 1938 is 2 aso, term column 

set.seed(12123)

t <-  14 # 79 stands for 2015, 14 for 1950
print(t+1936)

cases <- max(which(year.total==t)) 

# simple assignment of time periods to cases
years <- year.total[1:cases] 

# going to need a sender time matrix covariate
sender.time <- matrix(years,length(years),length(years),byrow=F)
year <- matrix(years, length(years),length(years),byrow=F)

# extract the network up to time t
AM <- adjacency.matrix[which(years <= t),which(years <= t)]

# determine the unfixed nodes
unfixed <- which(years==t)

# get outdegree of these nodes
unfixed.odeg <- rowSums(AM[unfixed,])
#hist(unfixed.odeg)

# get indegree of these nodes
unfixed.ideg <- colSums(AM[unfixed,])
#hist(unfixed.ideg)

net.t <- network(AM) 

# get edgewise shared partner distribution
full.esp <- summary(net.t ~ esp(0:20))   # this is esp dist if entire network is unfixed

# get esp dist of t-1
AM.red <- AM[-unfixed, -unfixed]
net.r <- network(AM.red) 

# get edgewise shared partner distribution
red.esp <- summary(net.r ~ esp(0:20))

unfixed.esp.ost <- full.esp-red.esp

w <- which(sender.time[,1]==t)  

esp <-rep(0,21)
for(u in w){
  
  for(j in 1:nrow(sender.time)){
    if(AM[u,j]==1){
      es <- 0
      for(k in 1:nrow(sender.time)){
        if( AM[u,j]*AM[u,k]*AM[j,k]==1){
          es <- es+1 
        } # if ==1
      } # for k
      if(es< 21){
        esp[es+1] <- esp[es+1] +1
      } # end if(es<21)
    }# end if(AM[u,j]==1)
  } #for j
}# for i

unfixed.esp.osp <- esp

#####################################################
### get cluster results
#####################################################


odeg.matrix <- li[[1]]
ideg.matrix <- li[[2]]
esp.matrix.ost <- li[[3]]
esp.matrix.osp <- li[[4]]

# get observed vector into right form
ideg.obs <- rep(0, 21)
odeg.obs <- rep(0,21)
for(k in 0:20){
  s <- which(names(table(unfixed.odeg))==k   ) 
  r <- which(names(table(unfixed.ideg))==k   )
  
  if(length(s)!=0){
    odeg.obs[k+1] <- table(unfixed.odeg)[s]
  }

  if(length(r)!=0){
    ideg.obs[k+1] <- table(unfixed.ideg)[r]
  }
}


save(li, odeg.matrix, ideg.matrix, esp.matrix.ost, esp.matrix.osp, odeg.obs, ideg.obs, unfixed.esp.ost, unfixed.esp.osp, file="GOF_1950.RData")



