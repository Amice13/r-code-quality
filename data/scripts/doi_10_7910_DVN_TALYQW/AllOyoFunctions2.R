#' Subsets data by year
#' 
#' Subsets a dataset by a year given columns D1 and D2
#' 
#' @param dataset a matrix or data frame with start and end year columns given by `cols`
#' @param cols numeric vector of length 2 with the indices of start and end year columns
#' @param year year to subset dataset by
#' @export
subset.by.year <- function(dataset, cols, year){
  c1 <- cols[1] # d1 or D1 or date1
  c2 <- cols[2] # d2 or D2 or date2
  newdataset<-dataset[ dataset[,c1] <= year & dataset[,c2]>=year,]
  return(newdataset)
}

#' Normalizes neighbor matrix
#' 
#' Divides each row of tmat by the sum of the row
#' 
#' @param tmat a neighbor/network/transition matrix
#' @export
normalize.tmat <- function(tmat){
  for(i in 1:nrow(tmat)){
    if(sum(tmat[i,])>0){
      tmat[i,] <- tmat[i,] / sum(tmat[i,])
    }
  }
  return(tmat)
}

project.locations <- function(locs){
  B<-SpatialPoints(locs, proj4string= CRS("+init=epsg:3857"))
  C<-spTransform(B, CRS("+proj=longlat +datum=WGS84"))
  as.data.frame(C)
}

remove.states <- function(trans.mat, TR.xy, Node.names){
  y <- rowSums(trans.mat)
  yy <- c()
  for(i in 1:length(y)){
    if(y[i]<=0.001){
      #print(i)
      yy <- c(yy, i)
    }
  }
  for(i in 1:length(yy)){
    trans.mat <- trans.mat[,-yy[length(yy)-i+1]]
    trans.mat <- trans.mat[-yy[length(yy)-i+1],]
    TR.xy <- TR.xy[-yy[length(yy)-i+1],]
    Node.names <- Node.names[-yy[length(yy)-i+1]]
  }
  return(list(trans.mat=trans.mat, TR.xy=TR.xy, Node.names=Node.names))
}

#' Augment transition matrix with artificial absorbing states
#'
#' This function takes the network information and creates a markov chain, 
#' returning a list with a transition matrix augmented with artificial absorbing
#' states, the coordinates of the network states, and the names of these states, 
#' as well as information specifying which artifical absorbing states were created.
#' 
#' @param tmat transition matrix
#' @param TR.xy xy coordinate locations for nodes specified in transition matrix
#' @param Node.names character vector with names of the nodes
#' @export
augment <- function(tmat=tmat, TR.xy = TR.xy, Node.names = Node.names){
  if(is.data.frame(tmat)){
    trans.mat<-data.matrix(tmat)
  }
  if(is.matrix(tmat)){
    trans.mat <- tmat
  }
  #print(dim(trans.mat))
  #print(as.numeric(rowSums(trans.mat)))
  
  # get rid of unconnected states
  #aug1 <- remove.states(trans.mat, TR.xy, Node.names)
  #trans.mat <- aug1$trans.mat
  #print(dim(trans.mat))
  #print(as.numeric(rowSums(trans.mat)))
  #TR.xy <- aug1$TR.xy
  #Node.names <- aug1$Node.names
  
  if(any(rowSums(trans.mat)!=1)){
    trans.mat <- normalize.tmat(trans.mat)
    #print(as.numeric(rowSums(trans.mat)))
  }
  
  crnames <- Node.names # names(tmat)
  rownames(trans.mat)<- crnames
  colnames(trans.mat)<- crnames
  
  mc <- new("markovchain", states = rownames(trans.mat), transitionMatrix = trans.mat, name="TradeRoute")
  AS<-absorbingStates(mc)
  
  ### do we need these lines if input is proper?
  # tmat<-tmat+t(tmat)
  # tmat<-diag(c(1/rowSums(tmat)),nr=dim(tmat)[1])%*%data.matrix(tmat)
  ### no, use function normalize.tmat()
  
  actions.mat<-trans.mat
  actions.mat[tmat>0]<-1 # do we need actions.mat?
  aug.mat<-trans.mat
  
  rownames(aug.mat)<-rownames(trans.mat);colnames(aug.mat)<-rownames(trans.mat)
  absstates<-c()
  absstatesnew <- c()
  
  ## add AS one at a time
  for (i in 1:length(AS)){
    #new col and row (bind_)
    augdim<-dim(aug.mat)[1]
    aug.mat<-cbind(aug.mat,rep(0,augdim))
    aug.mat<-rbind(aug.mat,rep(0,augdim+1))
    
    #name the new col and row
    rownames(aug.mat)[augdim+1] <- colnames(aug.mat)[augdim+1] <- paste0(AS[i],'ABS')
    #fill the 1 from the old state to the new absorbant
    #find an absorbing state
    oldrow<-which(rownames(trans.mat)==AS[i])
    # link old state to new abs state and remove old 1
    aug.mat[oldrow,(augdim+1)] <- 1
    aug.mat[oldrow,oldrow] <- 0
    # diagonal 1 for new abs state
    aug.mat[(augdim+1),(augdim+1)] <- 1
    
    TR.xy<-rbind(TR.xy, TR.xy[oldrow,])
    Node.names <- as.character(Node.names)
    Node.names<- c(Node.names, paste0(AS[i],'ABS'))
    absstates<-c(absstates,oldrow)
    absstatesnew<-c(absstatesnew,augdim+1)
  }
  return(list(aug.mat=aug.mat, absstates=absstates, absstatesnew=absstatesnew,
              AS=AS, TR.xy=TR.xy, Node.names=Node.names))
  
}

################################################################
#################KRIG OF CONFLICT< GENERATE SAMPLE individualsS####################

### should we subset conf by year or do this externally?

#' Creates conflict intensity density surface from conflict data
#'
#' Takes in conflict intensity data, kriging parameters, and predicts a surface on a 
#' fine grid to be used as a density function for sampling. Returns a list containing
#' the kriged surface and the locations in which it is defined.
#' 
#' @param conf a three column matrix or data frame containing the xy coordinates
#' and the conflict intensity value for each location
#' @param krig.params a list with components nu, ran, tau2, and sill
#' @param fine.loc a two column matrix of locations at which the ecdf is predicted
#' @export
conf2krigV2 <- function(conf=conf,  krig.params=krig.params, 
                        x.loc=seq(min(conf[,1])-1,max(conf[,2])+1,length.out=100),
                        y.loc=seq(min(conf[,2])-1,max(conf[,2])+1,length.out=100)
                        ){
  #confbyyear<-conf[(conf$d1<=year & conf$d2>=year),]
  #assign(paste0('conf',year),confbyyear)  # for plotting multiple years
  
  #PLOT CONFLICT DATA WITHIN YEAR; COLOR CODE POINTS
  #quartz(width=15,height=5)
  #par(mfrow=c(1,3),mar=c(.2,.2,1.5,.2))
  #loc <- data.frame(conf[,])
  
  #KRIGING SETUP (distance matrices)
  krig.conf <- conf[,3]
  krig.loc <- conf[,1:2]
  obs.dist<-rdist(krig.loc)
  fine.loc<-expand.grid(x.loc,y.loc)
  fine.to.obs<-rdist(fine.loc,krig.loc)
  #tvario<-variog(coords=krig.loc,data=krig.conf)
  #eyefit(tvario)
  
  #KRIG PARAMETERS (not rigorous nu/range, variofit on sill/tausq)
  matern.nu <- krig.params$nu
  matern.range <- krig.params$ran
  matern.tausq <- krig.params$tausq
  matern.sill <- krig.params$sill
  matern.sigmasq<-matern.sill-matern.tausq
  sigma0<-matern.sigmasq*
    Matern(fine.to.obs,range=matern.range,smoothness=matern.nu)
  sigma<-matern.sigmasq*
    Matern(obs.dist,range=matern.range,smoothness=matern.nu)+
    diag(matern.tausq,nrow(obs.dist))
  krig<-sigma0%*%solve(sigma)%*%krig.conf
  # ECDF
  krig<-krig/sum(krig)
  return( list(krig=krig, x.loc=x.loc, y.loc=y.loc, conf=conf, krig.params=krig.params) )
}

### function that splits conf into subset by year and calls conf2krigV2

conf2krig.byyear <- function(conf=conf, year=year, krig.params=krig.params){
  #confbyyear<-conf[(conf$d1<=year & conf$d2>=year),]
  confbyyear <- subset.by.year(conf, c(1,2), year)
  #assign(paste0('conf',year),confbyyear)  # for plotting multiple years
  
  #PLOT CONFLICT DATA WITHIN YEAR; COLOR CODE POINTS
  #quartz(width=15,height=5)
  #par(mfrow=c(1,3),mar=c(.2,.2,1.5,.2))
  loc <- data.frame(cbind(confbyyear$x,confbyyear$y))
  
  #KRIGING SETUP (distance matrices)
  krig.conf <- confbyyear$ci[confbyyear$ci>4]
  krig.conf[krig.conf==10] <- 7
  krig.loc<-loc[confbyyear$ci>4,]
  obs.dist<-rdist(krig.loc)
  x.loc<-seq(min(loc$X1),max(loc$X1),length.out=500)
  y.loc<-seq(min(loc$X2),max(loc$X2),length.out=500)
  fine.loc<-expand.grid(x.loc,y.loc)
  fine.to.obs<-rdist(fine.loc,krig.loc)
  #tvario<-variog(coords=krig.loc,data=krig.conf)
  #eyefit(tvario)
  
  #KRIG PARAMETERS (not rigorous nu/range, variofit on sill/tausq)
  matern.nu <- krig.params$nu
  matern.range <- krig.params$ran
  matern.tausq <- krig.params$tausq
  matern.sill <- krig.params$sill
  matern.sigmasq<-matern.sill-matern.tausq
  sigma0<-matern.sigmasq*
    Matern(fine.to.obs,range=matern.range,smoothness=matern.nu)
  sigma<-matern.sigmasq*
    Matern(obs.dist,range=matern.range,smoothness=matern.nu)+
    diag(matern.tausq,length(krig.conf))
  krig<-sigma0%*%solve(sigma)%*%krig.conf
  # ECDF
  krig<-krig/sum(krig)
  return( list(krig=krig, x.loc=x.loc, y.loc=y.loc) )
}

##EMPIRICAL DISTRIBUTION  #####NOT MEMORY FRIENDLY

#' Draws n samples from the normalized krig density function
#' 
#' Takes a surface krig, normalizes it to be a pdf, and draws n samples,
#' returning the locations of the n samples
#' 
#' @param nsim number of samples to draw
#' @param krig a surface 
#' @export
ecdf.sim <- function(nsim=1000, obj){
  #which.max(krig.ecdf[krig.ecdf<rand])
  #ECDF SAMPLE
  krig.ecdf<-cumsum(obj$krig)
  ru.samp<-runif(nsim,min=0,max=1)
  krigecdfmin<-function(uniform){
    outloc<-which.max(krig.ecdf[krig.ecdf<uniform])
    return(outloc)
  }
  kmin <- Vectorize(krigecdfmin)
  sim.vlocs <- unlist(kmin( ru.samp))
  fine.locs <- expand.grid(obj$x.loc, obj$y.loc)
  #print(str(fine.locs))
  #print(str(sim.vlocs))
  sim.locs <- data.matrix(fine.locs[sim.vlocs,] )
  #points(sim.locs$Var1,sim.locs$Var2,pch=21,bg='white',cex=.3)
  names(sim.locs) <- NULL
  dimnames(sim.locs) <- c(NULL, NULL)
  return(sim.locs)
}




############MDP Reward and Probability Matrices#################

#' 
#' 
#' Takes the augmented transition matrix, the kriged intensity surface, and 
#' a list with parameters defining the MDP, and then creates the markov 
#' decision process, returning a matrix with the numeber of rows equal to the
#' number of states in the network and the number of columns equal to steps.
#' 
#' @param aug the augmented transition matrix
#' @param krig the kriged conflict intensity surface
#' @param MDP.params a list containing 
mdpV2 <- function(aug=aug, krig=krg, MDP.params= MDP.params, steps=15 ){
  kmax<-max(krig$krig)
  actions.loc<-which(aug$aug.mat> 0, arr.ind=TRUE)
  #actions.loc holds rows for each valid action 
  #(e.g. the movement from loc i to loc j, another row for loc i to loc i, etc.
  n.actions<-dim(actions.loc)[1]
  n.nodes<-dim(aug$aug.mat)[1]
  P.arr<-R.arr<-array(0, c(n.nodes,n.nodes,n.actions))
  
  ###FILL DEFAULT PROBABILITIES, REWARDS
  for (actnum in 1:n.actions){
    loc1.ind<-as.numeric(actions.loc[actnum,1])
    loc2.ind<-as.numeric(actions.loc[actnum,2])
    loc1.xy<-matrix(aug$TR.xy[actions.loc[actnum,1],1:2],nc=2)
    loc2.xy<-matrix(aug$TR.xy[actions.loc[actnum,2],1:2],nc=2)
    #distance b/w two locations
    dist.12<-rdist(loc1.xy,loc2.xy)
    loc1.xy <- unlist(loc1.xy)
    loc2.xy <- unlist(loc2.xy)
    
    #points along line b/t locations
    lines.xy<-cbind(seq(loc1.xy[1],loc2.xy[1],length.out=20),
                    seq(loc1.xy[2],loc2.xy[2],length.out=20))
    #approx line integral
    intsurf <- list(x=krig$x.loc,y=krig$y.loc,z=matrix(krig$krig,
                                                       nrow=length(krig$x.loc), ncol=length(krig$y.loc)))
    #image.plot(intsurf); points(lines.xy)
    is.out<-interp.surface(intsurf,loc=lines.xy)
    if(any(is.na(is.out))){
      is.out[is.na(is.out)] <-0
    }
    
    #probabilty you go where your actions is trying to go
    P.arr[loc1.ind,loc2.ind,actnum]<- MDP.params$pij
    P.arr[loc1.ind,loc1.ind,actnum]<- 1-MDP.params$pij
    #cost of that movement
    confcost<-ifelse(dist.12==0,0,max(is.out[!is.na(is.out)]))
    R.arr[loc1.ind,loc2.ind,actnum]<- MDP.params$rij.dist*dist.12*(1+MDP.params$rij.conf*confcost/kmax)
    R.arr[loc1.ind,loc1.ind,actnum]<- MDP.params$rii
    #R.arr[loc1.ind,loc2.ind,actnum]<- (-10*dist.12-10*3*dist.12*confcost/kmax)
    #R.arr[loc1.ind,loc1.ind,actnum]<-.01
    #R.arr[loc1.ind,loc2.ind,actnum]<-0
  }
  #FILL END REWARDS
  for (rewardnum in 1:length(aug$AS)){
    actnum<-dim(actions.loc)[1]+1-2*rewardnum
    action<-actions.loc[actnum,]
    R.arr[action[1],action[2],actnum]<-MDP.params$rend[rewardnum]
  }
  end.rewards<-rep(0,n.nodes)
  
  ###################MDP for all input locations#######################
  #mdp.out2<-mdp_finite_horizon(P.arr,R.arr,discount=1,N=100,h=end.rewards)
  #mdp.out2$policy[,1]
  mdp.out<-mdp_policy_iteration(P=P.arr,R=R.arr,discount=1)
  length(mdp.out$policy)
  
  loopactions<-steps
  nodechains<-matrix(NA, nr=dim(aug$TR.xy)[1],nc=loopactions)
  for (allnodes in 1:dim(aug$TR.xy)[1]){
    node.current<- allnodes
    nodes<- node.current
    for (stepnum in 1:(loopactions-1)){
      action.to.take<-mdp.out$policy[node.current]
      node.to<-actions.loc[action.to.take,2]
      #print(c(action.to.take, actions.loc[action.to.take,]))
      node.current<-node.to
      nodes<-c(nodes,node.current)
    }
    nodechains[allnodes,]<-nodes
    #print(as.vector(Node.names[nodes]))
  }
  return( list(nodechains=nodechains, mpd.out=mdp.out ))
}

##### MDP function for returning an individual individuals's absoribing state
#' 
#' 
#' 
#' 
#' @param nsim number of samples
#' @param sim.locs locations of the n samples
#' @param augmented transiton matrix
#' @param krig kriged conflict intensity surface
#' @param MDP.params
#' @param rwd.distrb means and variances of the rewards for the absorbing states
#' @export 
mdp.individuals <- function(nsim=nsim, sim.locs=sim.locs, aug=aug, krig=krg, 
                            MDP.params= MDP.params, 
                            rwd.distrb =rwd.distrb, steps=15 ){
  paths<-matrix(nc=4,nr=nsim)
  paths[,1:2]<-as.numeric(unlist(sim.locs))
  dists<-rdist(paths[,1:2], aug$TR.xy[,1:2])
  #whichnode<-apply(dists,1,function(x) which( x== min(x), arr.ind=TRUE)) # same as below
  whichnode<-apply(dists,1,which.min)
  paths[,3]<-whichnode
  kmax<-max(krig$krig) # for dividing confcost in rewards
  actions.loc<-which(aug$aug.mat> 0, arr.ind=TRUE)
  #actions.loc holds rows for each valid action 
  #(e.g. the movement from loc i to loc j, another row for loc i to loc i, etc.
  n.actions<-dim(actions.loc)[1]
  n.nodes<-dim(aug$aug.mat)[1]
  for(kk in 1:nsim){
    P.arr<-R.arr<-array(0, c(n.nodes,n.nodes,n.actions))
    
    ###FILL DEFAULT PROBABILITIES, REWARDS
    for (actnum in 1:n.actions){
      loc1.ind<-as.numeric(actions.loc[actnum,1])
      loc2.ind<-as.numeric(actions.loc[actnum,2])
      loc1.xy<-matrix(aug$TR.xy[actions.loc[actnum,1],1:2],nc=2)
      loc2.xy<-matrix(aug$TR.xy[actions.loc[actnum,2],1:2],nc=2)
      #distance b/w two locations
      dist.12<-rdist(loc1.xy,loc2.xy)
      loc1.xy <- unlist(loc1.xy)
      loc2.xy <- unlist(loc2.xy)
      #points along line b/t locations
      lines.xy<-cbind(seq(loc1.xy[1],loc2.xy[1],length.out=20),
                      seq(loc1.xy[2],loc2.xy[2],length.out=20))
      #approx line integral
      intsurf <- list(x=krig$x.loc,y=krig$y.loc,z=matrix(krig$krig,nrow=length(krig$x.loc), ncol=length(krig$y.loc)))
      is.out<-interp.surface(intsurf,loc=lines.xy)
      if(any(is.na(is.out))){
        is.out <- rep(0, length(is.out))
      }
      #pij is probabilty you go where your action is trying to go to loc2 
      P.arr[loc1.ind,loc2.ind,actnum]<- MDP.params$pij
      P.arr[loc1.ind,loc1.ind,actnum]<- 1-MDP.params$pij # prob of staying at loc1
      #cost of that movement
      confcost<-ifelse(dist.12==0,0,max(is.out[!is.na(is.out)]))
      R.arr[loc1.ind,loc2.ind,actnum]<- MDP.params$rij.dist*dist.12*(1+MDP.params$rij.conf*confcost/kmax)
      R.arr[loc1.ind,loc1.ind,actnum]<- MDP.params$rii
      #R.arr[loc1.ind,loc2.ind,actnum]<- (-10*dist.12-10*3*dist.12*confcost/kmax)
      #R.arr[loc1.ind,loc1.ind,actnum]<-.01
      #R.arr[loc1.ind,loc2.ind,actnum]<-0
    }
    #FILL END REWARDS
    for (rewardnum in 1:length(aug$AS)){
      actnum<-dim(actions.loc)[1]+1-2*rewardnum
      action<-actions.loc[actnum,]
      R.arr[action[1],action[2],actnum]<- rnorm(length(aug$AS), rwd.distrb[1], rwd.distrb[2] )[rewardnum] #### changes this line from MDP.params$rend[rewardnum] to 
    }
    end.rewards<-rep(0,n.nodes)
    
    ###################MDP for all input locations#######################
    #mdp.out2<-mdp_finite_horizon(P.arr,R.arr,discount=1,N=100,h=end.rewards)
    #mdp.out2$policy[,1]
    mdp.out<-mdp_policy_iteration(P=P.arr,R=R.arr,discount=1)
    
    
    loopactions<-steps
    nodechains<-matrix(NA, nr=1,nc=loopactions)
    
    node.current<- paths[kk,3]
    nodes<- node.current
    for (stepnum in 1:(loopactions-1)){
      action.to.take<-mdp.out$policy[node.current]
      node.to<-actions.loc[action.to.take,2]
      #print(c(action.to.take, actions.loc[action.to.take,]))
      node.current<-node.to
      nodes<-c(nodes,node.current)
    }
    paths[kk,4]<-nodes[loopactions]
    print(kk)
  }
  return( paths )
}


##### Conflict Kriging Heat Map
conf.heat.map <- function(krg, Cities, conf, coltab){
  quilt.plot(expand.grid(krg$x.loc, krg$y.loc), nx=length(krg$x.loc),
             ny= length(krg$y.loc), add.legend=FALSE,
             krg$krig,main=paste0('Yoruba Diaspora ',year),#,' Conflicts and Smoothed Conflict Estimator'),
             xaxt='n', yaxt='n', col=coltab, bty='n' )
  plot.shapefiles()
  #world(add=TRUE)
  #Add conflict points
#  conf<-conf[(conf$d1<=year & conf$d2>=year),]
#  conf0 <- conf[conf$ci==0,]
#  conf1 <- conf[conf$ci==1,]
  conf5 <- conf[conf[,3]==min(conf[,3]),]
  conf10 <- conf[conf[,3]==max(conf[,3]),]
  
  #plot(cbind(conf1$x, conf1$y), main=paste0("Conflicts in",i), 
  #     pch=21, col="yellow", bg="yellow",xaxt='n',yaxt='n')
  points(cbind(conf5$x, conf5$y), pch=21, col="black", bg="black")
  points(cbind(conf10$x, conf10$y), pch=4, col="black",cex=1, lwd=2)
  #points(cbind(conf0$x, conf0$y), pch=21, col="green", bg="green")
  
  #Label "Large Cities"
  #Cities <- left_join(Cities, map.locs2, by=c('Name'='name'))
  Cities<-Cities[Cities$Fyear <= year,]
  points(cbind(Cities$X,Cities$Y), pch='*',bg='gray50', col='gray50',cex=2)
  text(cbind(Cities$X + 0.08,Cities$Y+0.08),labels=Cities$Name,cex=.7)
}


###################simple arrow function#######################
arr2 = function(nodes, aug){
  rle.out <- rle(nodes)$lengths
  nodes <- nodes[1:(length(nodes)+1 - rle.out[length(rle.out)])]
  # if (length(nodes)<3){
  #   print(paste0('started at ',rownames(aug$aug.mat)[nodes[1]]))
  #   return()
  # }
  string1 <- aug$Node.names[nodes[length(nodes)]]
  substring1 <- substr(string1, 1, nchar(string1)-3)
  p.names <- paste0("The individuals went from ", aug$Node.names[nodes[1]], " to ", substring1 )
  plot(aug$TR.xy[,1:2], main=p.names, axes=FALSE)
  plot.shapefiles()
  xy <- aug$TR.xy[nodes,1:2]
  names <- rownames(aug)[nodes]
  #par(mfrow=c(1,1), mar=c(.2,.2,1.5,.2))
  points(xy, col="gray80", pch=2, bg="gray80", cex=2.3)
  if(length(nodes)-2 > 1){
    for( i in 1:(length(nodes)-2)){
      arrows(xy[i,1], xy[i,2], x1=xy[i+1, 1], y1=xy[i+1, 2], length=0.1, col="red")
    }
  }else{
    arrows(xy[1,1], xy[1,2], x1=xy[1+1, 1], y1=xy[1+1, 2], length=0.1, col="red")
  }
  #print(c('individuals path:', names))
  #sprintf("The individuals went from %s to %s", nodes[1], nodes[length(nodes)])
}



##################make arrow/conflict map####################
arrow.plot <- function(aug, krig, nodechains, Cities, coltab, steps=15){
  plot(aug$TR.xy[,1:2], col='gray40', xaxt='n', yaxt='n', xlab='', ylab='',
       bty='n', main=paste0('Most likely path, ',year ))#'Optimal route/ decision process')
  #Label "Large Cities"
  plot.shapefiles()
  points(cbind(Cities$X,Cities$Y), pch=24,bg='gray40', col='gray40',cex=.7)
  text(cbind(Cities$X,Cities$Y+0.2),labels=Cities$Name,cex=.7)
  points(aug$TR.xy[aug$absstates,1:2], col=coltab, pch=21,bg=coltab, cex=2)
  #contour the conflict
  kz <- matrix(krig$krig,
               nrow=length(krig$x.loc), ncol=length(krig$y.loc))
  contour(x=krig$x.loc, y=krig$y.loc, z=kz, 
          add=TRUE, col='gray50', lty=2, labels='') # gray90
  absstates.aug<-aug$absstatesnew
  for(j in 1:dim(nodechains)[1]){
    nodes<-nodechains[j,]
    plotcolor<-coltab[which(nodes[steps]==absstates.aug)]
    rle.out <- rle(nodes)$lengths
    nodes <- nodes[1:(length(nodes)+1 - rle.out[length(rle.out)])]
    if (length(nodes)<3){ # no arrows
      print('<3')
    }else{
      xy <- aug$TR.xy[nodes,1:2]
      for( i in 1:(length(nodes)-2)){
        arrows(xy[i,1], xy[i,2], x1=xy[i+1, 1], y1=xy[i+1, 2], length=0.1, col=plotcolor,lwd=2)
      }  
    }
  }
  #plot(ocean, add=TRUE)
  #Label Absorbing States
  text( cbind(aug$TR.xy[aug$absstates,1],(aug$TR.xy[aug$absstates,2])+0.2), labels=aug$AS,cex=.7)#,col=coltab)
}


###################individual simulations end node map#######################
#' Plotting individuals, colored by end node
#' 
#' For static/fixed/deterministic rewards
sim.plot <- function(nsim, year, sim.locs, aug, nodechains, Cities, 
                     tmat, coltab, steps=15){
  paths<-matrix(nc=4,nr=nsim)
  paths[,1:2]<-as.numeric(unlist(sim.locs))
  dists<-rdist(paths[,1:2], aug$TR.xy[,1:2])
  #whichnode<-apply(dists,1,function(x) which( x== min(x), arr.ind=TRUE))
  whichnode<-apply(dists,1,which.min)
  paths[,3]<-whichnode
  paths[,4]<-nodechains[paths[,3],steps]
  # new plot
  plot(aug$TR.xy[,1:2], col='white', xaxt='n', yaxt='n',
       bty='n', main=paste0('Simluated individuals colored by end node location, ',year), xlab='', ylab='') #'Individuals and their Exit Locations')
  plot.shapefiles()
  # plot simulated individuals by color
  for (i in 1:dim(paths)[1]){
    startloc<-paths[i,3]
    endloc<-paths[i,4]
    points(paths[i,1],paths[i,2], col=coltab[endloc-dim(tmat)[1]], cex=.6)
    #points(cbind(paths[i,1],paths[i,2]), col=coltab[endloc-dim(tmat)[1]], cex=.6)
  }
  #plot(ocean, add=TRUE)
  # Label "Large Cities" with text
  Cities<-Cities[Cities$Fyear <= year,]
  points(cbind(Cities$X,Cities$Y), pch=24,bg='gray40', col='gray40',cex=.7)
  text(cbind(Cities$X,Cities$Y+0.2),labels=Cities$Name,cex=.7)
  # absorbing states by color
  points(aug$TR.xy[aug$absstates,1:2], col=coltab, pch=21,bg=coltab, cex=2)
  #Label Absorbing States
  text(cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]+0.2), labels=aug$AS,cex=.7)#,col=coltab)
  totals <- table(paths[,4])
  totals2 <- c()
  for(i in 1:length(aug$AS)){
    index1 <- as.character(aug$absstatesnew[i])
    totals2[i] <- totals[ index1 ]
  }
  text(cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]+0.35), labels=totals2/sum(totals),cex=.7)#,col=coltab)
  
}


# get.google.coords <- function(places){
#   # places is a char vector of places 
#   place.coords <- data.frame(long = numeric(),
#                              lat = numeric(),
#                              name = character(),
#                              stringsAsFactors = F)
#   
#   for ( i in 1:length(places) ){
#     place.full <-paste0(places[i])  
#     # Get Coordinates
#     coords <- geocode(place.full, source = 'google')
#     place.coords[i,1:2] <- coords[1:2]
#     place.coords[i,3] <- places[i]
#     Sys.sleep(20)
#   }
#   
#   return(place.coords)
# }




#' 
#' 
#' 
conf2kde <- function(conf=conf, year=year, bwd=bwd, lims=NULL){
  confbyyear<-conf[(conf$d1<=year & conf$d2>=year),]
  #assign(paste0('conf',year),confbyyear)  # for plotting multiple years
  
  #PLOT CONFLICT DATA WITHIN YEAR; COLOR CODE POINTS
  #quartz(width=15,height=5)
  #par(mfrow=c(1,3),mar=c(.2,.2,1.5,.2))
  loc <- data.frame(cbind(confbyyear$x,confbyyear$y))
  
  #KRIGING SETUP (distance matrices)
  krig.conf <- confbyyear$ci[confbyyear$ci>4]
  krig.conf[krig.conf==10] <- 7
  krig.loc<-loc[confbyyear$ci>4,]
  if(is.null(lims)){
    lims <- c(range(krig.loc[,1], range(krig.loc[,2])))
  }
  kdee <- kde2d.weighted(krig.loc[,1], krig.loc[,2],h=bwd, n = 500, lims = lims,
                         w=1/krig.conf)
  
  x.loc <- kdee$x
  y.loc <- kdee$y
  kdez <- c(kdee$z)
  kdez<-kdez/sum(kdez)
  return( list(kdez=kdez, x.loc=x.loc, y.loc=y.loc) )
}

#' Plots edges of the trade network based on the augmented transition matrix
#' 
#' This function
#' 
#' @param aug A list with components aug.mat, an augmented transition matrix,
#' and TR.xy, the locations of the nodes in the transition matrix.
#' @export
plot.edges <- function(aug){
  for(i in 1:nrow(aug$aug.mat)){
    for(j in 1:ncol(aug$aug.mat)){
      if(aug$aug.mat[i,j] != 0){
        segments(x0=aug$TR.xy[i,1], y0=aug$TR.xy[i,2], 
                 x1 =aug$TR.xy[j,1], y1 = aug$TR.xy[j,2], lty=3, lwd=1,
                 col='gray60')
      }
    }
  }
  
}

#' Plots shapefiles
#' 
#' This function plots the shapesfiles specified in the folder
#' 
#' @export
plot.shapefiles <- function(){
  dbc <- adjustcolor('dodgerblue1', alpha.f=0.46)
  dbc2 <- adjustcolor('dodgerblue1', alpha.f=0.46)
  plot(rivers, add=TRUE, col=dbc, bg=dbc,  lwd=0.5, border=0 )
  plot(ocean, add=TRUE, col=dbc2, bg=dbc2, lwd=0.5, border=0 )
  plot(zoom, add=TRUE, col=dbc, bg=dbc, lwd=0.5, border=0 )
  plot(lakes, add=TRUE, col=dbc, bg=dbc ,  lwd=0.5, border=0 )
}


#' Function for plotting conditional location maps for a given year
#' 
#' Main function used to power the shiny app
#' 
#' @param paths an nx4 matrix containing the x and y coordinates, and
#' the indices indicating the start and end path of each individual
#' @param bwd the bandwidth to be passed to the 2dkde function
#' @param locs a vector of indices indicating which absorbing states to compute
#' the conditional probability map for
#' @param Cities a three column matrix containing the names, xy coordinates, and 
#' founding year of the big cities to be plotted with annotation
#' @param krig
cond.loc.plot<-function(paths, bwd=2, locs, Cities,
                        krig, aug, borders, year, shiptotals,
                        conf,
                        add.conf=FALSE, #add.conf.heat=FALSE,
                        add.network=FALSE,
                        add.contour=FALSE, 
                        add.borders=FALSE,
                        path){
  `%notin%` <- Negate(`%in%`)
  path2 <- paste0(path, 'shinydata/forShiny',year,'.rda')
  #print(path2)
  load(path2)
  paths <- object.list$paths
  #conf <- data.list$conf
  #Cities <- data.list$Cities
  Cities <- Cities[Cities$Name %notin% c('Lagos', 'Porto Novo', 'Ouidah'),]
  #shiptotals <- data.list$shiptotals
  aug <- object.list$aug
  krig <- object.list$krg
  #key <- which(endsWith(colnames(aug$aug.mat), "ABS"))
  #locs <- key[locs]
  xlim.krg <- c(2, 6.564340)#range(krig$x.loc)# + c(-1, 1)
  ylim.krg <- c(5.5, 10)#c(6.376370, 8.967498)#range(krig$y.loc)# + c(-1, 1)
  # if(isTRUE(add.conf.heat)){
  #   par(mfrow=c(1,2), mar=c(0,1,1,2), oma=c(0,1,0,1))
  # }
  par(mar=c(0,1,1,2), oma=c(0,1,0,1))
  plot(0,0, col='white', xlab='', ylab='',
       bty='n', xaxt='n', yaxt='n', 
       xlim=xlim.krg, ylim=ylim.krg,
       asp=1, main='')
       #main=paste0(' Approx. conditional probability given port'))
  #locs<-69
  #print(locs)
  
  ## get only persons which ended up in set of locations defined by locs
  path.subset<- paths[paths[,4] %in% locs,]
  if (dim(path.subset)[1]!=0){
    kde.out <- kde2d(path.subset[,1],path.subset[,2],h=bwd, n=200, lims=c(xlim.krg + c(-2,2), ylim.krg + c(-2,2)))
    kde.grid<-expand.grid(as.numeric(kde.out$x), as.numeric(kde.out$y))
    
    
    # kde.z <- kde.out$z
    # kde.z[kde.z<0.5e-1] <- NA
    # x <- raster(kde.z, xmn=min(kde.grid[,1]), xmx=max(kde.grid[,1]), ymn=min(kde.grid[,2]), ymx=max(kde.grid[,2]))
    # plot(x, add=TRUE, col=brewer.blues(100))
    quilt.plot(kde.grid[,1], kde.grid[,2], z=kde.out$z, col=blues,#pals::brewer.blues(100),
               bty='n', xaxt='n', yaxt='n',
               nx= length(as.numeric(kde.out$x)), ny=length(as.numeric(kde.out$y)), 
               xlim=xlim.krg, ylim=ylim.krg,
               add=TRUE)
  }
  
  plot.shapefiles()
  
  if(isTRUE(add.borders)){
    borders.this.year <- borders[as.numeric(substr(borders@data$D1, 1, 4)) <= year & 
                                   as.numeric(substr(borders@data$D2, 1, 4)) >= year, ]
    
    raster::plot(borders.this.year,
                 col=borders.this.year@data$cols, 
                 add=TRUE, border=0, lwd=0.01)
  }
  
  
  # if(is.null(lims)){
  #   lims <- c(range(kde.grid[,1], range(kde.grid[,2])))
  # }
  

  if(isTRUE(add.contour) ){
    aa <- (range(krig$krig, finite = TRUE)[1]+ 2*(range(krig$krig)[2] - range(krig$krig)[1])/5)
    bb <- (range(krig$krig, finite = TRUE)[1]+ 3*(range(krig$krig)[2] - range(krig$krig)[1])/5)
    cc <- (range(krig$krig, finite = TRUE)[1]+ 4*(range(krig$krig)[2] - range(krig$krig)[1])/5)
    dd <- (range(krig$krig, finite = TRUE)[1]+ 5*(range(krig$krig)[2] - range(krig$krig)[1])/5)
    contour(x=krig$x.loc, y=krig$y.loc, z=matrix(krig$krig, nrow=length(krig$x.loc),
                                               ncol=length(krig$y.loc)),
            col='darkred', add=TRUE, levels= c(aa,bb,cc,dd),
            drawlabels=FALSE)
  }
  Cities<-Cities[Cities$Fyear <= year,]
  #text(cbind(Cities$X,Cities$Y+0.22),labels=Cities$Name,cex=.7)
  if(isTRUE(add.network)){
    
    #adj.vec <- ifelse(aug$TR.xy[aug$absstates,2]>7.5 | (aug$AS %in% c('Jakin', 'Badagry', 'Lagos')), -0.15, 0.15)
    adj.vec <- ifelse(aug$TR.xy[aug$absstates,2]<10 , -0.15, 0.15)
    
    dt.temp <- cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]+adj.vec)
    lbs.temp <- aug$AS
    
    # ind.temp <- !(lbs.temp %in% c('Ouidah', 'Lagos'))
    # lbs.temp <- lbs.temp[ind.temp]
    # dt.temp <- dt.temp[ind.temp,]
    #text(dt.temp, labels=lbs.temp,cex=1)#,col=coltab)
    text(dt.temp, labels=lbs.temp,cex=0.8, srt=-45, adj=c(0,1)) #pos=4)#,col=coltab)
    
    adj.vec2 <- ifelse(Cities$Y<10, 0.1, -0.1) # <7.3
    #text(cbind(Cities$X ,Cities$Y+adj.vec2),labels=Cities$Name,cex=1)
    
    plot.edges(aug)
    # points(cbind(Cities$X,Cities$Y), pch=22,bg='white', col='black',cex=1)
    # points(cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]), pch=22,bg='white', col='black',cex=1)
    points(cbind(Cities$X,Cities$Y), pch=16, col='black',cex=0.5) # 1
    text(cbind(Cities$X ,Cities$Y+adj.vec2),labels=Cities$Name,cex=0.8)
    
    points(cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]), pch=16, col='black',cex=0.5) # 1
    
    points(aug$TR.xy[,1:2], cex=0.5, pch=16)
    legend(x=2.1 , y=5.9 , cex = 0.5, ncol = 2, pt.cex=1.5, 
           legend = c('Absorbing State', 'Other City', 'Battle', 'Town Destroyed' ),
           pch = c(16, 22, 21, 24), #fill=c('black', NA, NA, 'white'), 
           col=c('black', 'black', 'black', 'black'), 
           pt.bg = c('black', 'white', 'red', 'red') )
    
  }

  # absorbing states by color
  
  coltab<-gb #glasbey(n=length(aug$AS)) #color table for absorbing states
  # print(aug$absstates[locs - index.shift])
  coltab[4] <- 'gray40'
  coltab <- rep('black', length(coltab))
  xyxy <- data.matrix(aug$TR.xy[locs,1:2])
  if(ncol(xyxy)!=2){
    xyxy <- t(xyxy)
  }
  #print(xyxy)
  if(dim(path.subset)[1]!=0){
    if(is.matrix(xyxy)){
      for(i in 1:nrow(xyxy)){
        points(xyxy[i,1], xyxy[i,2], col=coltab[i], pch=21, bg=coltab[i], cex=1)
      }
    }else{
      points(xyxy[1], xyxy[2], col='red', pch=21, bg='red', cex=2)
    }
    
  }
  #Label Absorbing States
  
  xyxy2 <- data.matrix(aug$TR.xy[aug$absstatesnew,1:2])
  for(i in 1:nrow(xyxy2)){
        points(xyxy2[i,1], xyxy2[i,2], col=coltab[i], pch=21, bg=coltab[i], cex=0.5)
      }


  if(isFALSE(add.network)){
    #adj.vec <- ifelse(aug$TR.xy[aug$absstates,2]>7.5 | (aug$AS %in% c('Jakin', 'Badagry', 'Lagos')), -0.15, 0.15)
    adj.vec <- ifelse(aug$TR.xy[aug$absstates,2]<10 , -0.15, 0.15)
    dt.temp <- cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]+adj.vec)#+adj.vec)
    lbs.temp <- aug$AS
    # ind.temp <- !(lbs.temp %in% c('Ouidah', 'Lagos'))
    # lbs.temp <- lbs.temp[ind.temp]
    # dt.temp <- dt.temp[ind.temp,]
    text(dt.temp, labels=lbs.temp,cex=0.8, srt=-45, adj=c(0,1)) #pos=4)#,col=coltab)
    
    adj.vec2 <- ifelse(Cities$Y<10, 0.1, -0.1)
    points(cbind(Cities$X,Cities$Y), pch=22,bg='white', col='black',cex=1)
    text(cbind(Cities$X ,Cities$Y+adj.vec2),labels=Cities$Name,cex=0.8)

    legend(x=2.1 , y=5.9 , cex = 0.5, ncol = 2, pt.cex=1.5, 
           legend = c('Absorbing State', 'Other City', 'Battle', 'Town Destroyed' ),
           pch = c(16, 22, 21, 24), #fill=c('black', NA, NA, 'white'), 
           col=c('black', 'black', 'black', 'black'), 
           pt.bg = c('black', 'white', 'red', 'red') )
  }

  # if(isTRUE(add.conf.heat)){
  #   # image.plot(x=krg$x.loc, y=krg$y.loc, z=matrix(krg$krig, nrow=length(krg$x.loc),
  #   #                                               ncol=length(krg$y.loc)),
  #   #            col=pals::brewer.orrd(100))
  #   krg.grid <- expand.grid(x=krig$x.loc, y=krig$y.loc)
  #   coltab<- two.colors( 256, middle="yellow",start='white',end='red2')
  #   quilt.plot(x=krg.grid[,1], y=krg.grid[,2], z=krig$krig,
  #              col=reds,#pals::brewer.reds(100),#coltab, 
  #              xlim=xlim.krg, ylim=ylim.krg,
  #              add.legend=TRUE, bty='n', xaxt='n', yaxt='n', 
  #              xlab='', ylab='', main='Conflict Heat Map')
  #   plot.shapefiles()
  #   if(isTRUE(add.conf)){
  #     # fix this part with parameters
  #     confbyyear<-conf[(conf$d1<=year & conf$d2>=year & conf$ci > 1),]
  #     loc <- data.frame(cbind(confbyyear$x,confbyyear$y))
  # 
  #     conf <- conf[conf$`conf$ci` > 1,]
  #     pech <- ifelse(conf$`conf$ci`==max(conf$`conf$ci`), 2, 1)
  #     points(conf[,1:2], pch=pech, col='black')
  #   }
  #   
  #   Cities<-Cities[Cities$Fyear <= year,]
  #   #points(cbind(Cities$X,Cities$Y), pch=22,bg='white', col='black',cex=1.35)
  #   points(cbind(Cities$X,Cities$Y), pch=26, col='black',cex=1.35)
  #   #text(cbind(Cities$X,Cities$Y+0.22),labels=Cities$Name,cex=.7)
  #   
  #   text(cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]+adj.vec), labels=aug$AS,cex=1)#,col=coltab)
  #   text(cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]+adj.vec), labels=aug$AS,cex=1)#,col=coltab)
  #   
  # }
  
  
  if(isTRUE(add.conf )){#& !add.conf.heat)){
    #confbyyear<-conf[(conf$d1<=year & conf$d2>=year & conf$ci > 1),]
    #loc <- data.frame(cbind(confbyyear$x,confbyyear$y))
    conf <- subset.by.year(conf, c(1,2), year)
    conf <- cbind(project.locations(conf[,6:7]), conf$ci)
    pech <- ifelse(conf$`conf$ci`==3, 24, 21)
    points(conf[,1:2], pch=pech, col='black', bg='red')
  }
  
  
  shiptotalsbyyear <- shiptotals[(shiptotals$D1<=year &shiptotals$D2>=year),]
  stts <- group_by( shiptotalsbyyear, Port) %>% summarise(Total=sum(Ind))
  #text(min(xlim.krg)+2, max(ylim.krg), paste(stts$Port, collapse='  '), cex=1)
  #text(min(xlim.krg)+2, max(ylim.krg)-0.5,paste(stts$Total, collapse='          '), cex=1)

}


#' Plots individuals, colored by end node
#' 
#' For random rewards
#' 
plot.paths <- function(aug, paths, tmat, year, Cities, coltab){
  plot(aug$TR.xy[,1:2], col='white', xaxt='n', yaxt='n',
       xlab='', ylab='', bty='n')#, main='Slaves and their Exit Locations')
  # plot simulated slaves by color
  for (i in 1:dim(paths)[1]){
    startloc<-paths[i,3]
    endloc<-paths[i,4]
    points(paths[i,1],paths[i,2], col=coltab[endloc-dim(tmat)[1]], cex=.6)
    #points(cbind(paths[i,1],paths[i,2]), col=coltab[endloc-dim(tmat)[1]], cex=.6)
  }
  table(paths[,4])
  #plot(ocean, add=TRUE)
  # Label "Large Cities" with text
  Cities<-Cities[Cities$Fyear <= year,]
  points(cbind(Cities$X,Cities$Y), pch=24,bg='gray40', col='gray40',cex=.7)
  text(cbind(Cities$X,Cities$Y+0.2),labels=Cities$Name,cex=.7)
  # absorbing states by color
  points(aug$TR.xy[aug$absstates,1:2], col=coltab, pch=21,bg=coltab, cex=2)
  #text(aug$TR.xy[aug$absstates,],labels=Cities$Name,cex=.7)
  #Label Absorbing States
  text(cbind(aug$TR.xy[aug$absstates,1],aug$TR.xy[aug$absstates,2]+0.1), labels=aug$AS,cex=.7)#,col=coltab)
}

