#######################################
#######################################
#######     ETHNO PAPER      ##########
#######################################
#######################################
rm(list=ls())
library(Hmisc)
library("sandwich")
library(stargazer)
library(lmtest)

# little function to recode
rec <- function(x, from, to, fct=F) { 
  i  <- match(x, from)
  if (fct==TRUE) {factor(to[i],levels=to)
  } else {to[i]}
}
# same as table but showing NAs
tbl <- function(...) table(..., useNA='ifany')

# function to compute clustered and/or robust standard errors
robust.se <- function(model, cluster){
  if(is.null(cluster)) 
  {print("no cluster, computing robust SE")
    return(list(NA, coeftest(model, vcov = vcovHC(model,type="HC1"))))} #my addition: if there is no cluster variable, use robust SEs
  require(sandwich)
  require(lmtest)
  cluster <- factor(as.character(cluster))  #my addition
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}

# function to generate tables of coefficients from regressions
pt.tbl <- function(obj,selcoef="D",addinf=c(2:length(obj[[1]])),coefnames=NULL,stars=TRUE,digits=3,starlabels=c("***","**","*"," "),
                   starbreaks=c(-0.1,0.01,0.05,0.1,1),addinfnames=names(obj[[1]])[addinf],cnames=paste("{",1:length(obj),"}",sep=""))  {
  
  options(scipen=999)
  ## find out length of selcoefs
  model <- obj[[1]][[1]]
  if (length(na.omit(match(selcoef,x=dimnames(model)[[1]])))>0) {
    I <- na.omit(match(selcoef,dimnames(model)[[1]]))   
  } else {
    I <- unique(unlist(lapply(selcoef,grep,x=dimnames(model)[[1]])))    
  }
  ncoef <- length(I)
  nrows <- 2*ncoef + length(addinf)
  final.table <- array(0,dim=c(nrows,length(obj)))
  if (length(coefnames)==0) {
    coefnames <- dimnames(model)[[1]][I]
  }  else {coefnames=coefnames}
  
  for (j in 1:length(obj)) {  
    model <- obj[[j]][[1]]
    if (length(na.omit(match(selcoef,x=dimnames(model)[[1]])))>0) {
      I <- na.omit(match(selcoef,dimnames(model)[[1]]))   
    } else {
      I <- unique(unlist(lapply(selcoef,grep,x=dimnames(model)[[1]])))    
    }
    pvals <- array(t(model[I,4]))
    model <- array(t(model[I,c(1:2)])); 
    # model <- as.character(format(model,scientific=FALSE,digits=2,nsmall=2))
    model <- as.character(round(model,digits=digits))
    model[2*(1:ncoef)] <- gsub(" ","",paste("(", model[2*(1:ncoef)], ")", sep=""))
    
    ## stars
    if (stars==TRUE) {
      model[2*(1:ncoef)-1] <- paste(model[2*(1:ncoef)-1],cut(pvals,breaks=starbreaks,labels=starlabels),sep="")
    }
    
    final.table[1:(2*ncoef),j] <- model
    
    if (!is.null(addinf)) {
      final.table[(2*ncoef+1):((2*ncoef)+length(addinf)),j] <- unlist(obj[[j]][addinf])
    }
  }
  
  colnames(final.table) <- cnames
  rownames(final.table) <- c(array(t(cbind(coefnames,rep("",length(coefnames))))),addinfnames)
  final.table <- apply(final.table,2,gsub,pattern="\\_",replacement="\\\\_")
  final.table <- apply(final.table,2,gsub,pattern="NA|\\(NA\\)",replacement="")
  final.table[is.na(final.table)] <- ""
  rownames(final.table) <- gsub(pattern="\\_",replacement="\\\\_",rownames(final.table))
  
  print(final.table)
  return(final.table)
}

# function to export table into html
tbl.htm <- function(tbls,file=gsub("\\)","",gsub("list\\(","",deparse(substitute(tbls)))),laddinf,nvars=NULL,title=" ",n.rgroup=NULL,rgroup="",
                    longtable=FALSE,lpage=40,booktabs=TRUE,extracolheads=NULL,landscape=FALSE,size=NULL,stars=T,n.cgroup=NULL,cgroup="",
                    label=gsub("\\)","",gsub("list\\(","",deparse(substitute(tbls)))),caption="",insert.bottom="")  {
  
  require(htmlTable)
  tbl <- do.call("rbind", tbls)
  Iomit <- which(apply(tbl,1,function(x)sum(x!=""))==0)
  if (length(Iomit)>0) {tbl <- tbl[-Iomit,]}
  
  if (is.null(n.rgroup)) {
    n.rgroup <- c((nrow(tbl)-laddinf),laddinf)
    if (laddinf==0) {n.rgroup <- NULL}
  } 
  # else{print(nrow(tbl))}
  
  w <- htmlTable(tbl,file=paste(file,".html",sep=""),
                 rgroup=rgroup, n.rgroup = n.rgroup,cgroup=cgroup, n.cgroup = n.cgroup,
                 label=label,
                 caption=caption)
}



dir <- "Folder_where_the_data_is"
setwd(dir)

dte <- read.csv("data_analysis.csv",header = T)

###########################
####  Cluster Analysis  ###
###########################
### vbles for cluster analysis
chractv <- c("individual_exchq","interaction_domainsq","interaction_freqq","dyadicq","verticalq",
             "broker_impq","broker_interestq",
             "affectiveq",
             "get_moneyq","get_infrastructureq","get_servicesq","get_insurance_protq", "get_employmq",
             "give_voteq","give_laborq","give_loyaltyq",
             "coercionq_act","coercionq_pas") #

dtc <- dte[complete.cases(dte[,chractv]),] 

## Create new id vble (idc)
dtc$idc <- 1:nrow(dtc)

## data for cluster analysis
dtcl <- dtc[,chractv]
## center data
means <- apply(dtcl,2,mean);sds <- apply(dtcl,2,sd);dtcl <- scale(dtcl,center=means,scale=sds)

dtcl.dist <- dist(dtcl)
dtcl.hclust <- hclust(dtcl.dist)


#### Table 1: Characteristics of Clusters ####
nclust <- 5
groups <- cutree(dtcl.hclust,nclust)
groups <- rec(groups,from=as.numeric(names(sort(tbl(groups),decr=T))),to=1:5)

## Average characteristics in each group
ag <- aggregate(dtcl,list(groups),mean)
names_char_all <- c("Individual exchange","Additional domains of interaction","Frequent interaction","Dyadic","Hierarchical",
                    "Broker Important","Broker interests aligned to client","Affective relation",
                    "Client gets money","Client gets infrastructure","Client gets gov services","Client gets insurance/ protection","Client gets employment",
                    "Client gives vote","Client gives labor","Client gives loyalty",
                    "Coercion Threats","Coercion Withdrawal")
names_map <- cbind(names(ag)[-1],names_char_all)
names(ag)[-1] <- names_char_all

agst <- t(ag[,-1])

types <- matrix(" ",nrow(agst),ncol(agst))
for (j in seq(along=1:ncol(agst))) {
  agstordj <- agst[order(abs(agst[,j]),decreasing=T),j]
  namesj <- names(agstordj);
  namesj[which(agstordj<0)] <- paste("No",namesj[which(agstordj<0)])
  types[,j] <- namesj
  types[which(abs(agstordj)<0.34),j] <- ""
}
types <- types[!apply(types,1,function(x)all(x=="")),]

cnames_bench <- c("Vote buying","Relational","Collective","Traditional","Modern coercive")
colnames(types) <- cnames_bench
htmlTable(types,file="types_benchmark.html",title="",label="types_benchmark",landscape=F,caption="Most important characteristics of clusters")
## Elements in the table were manually made bold if the respective component in agst has value >  0.8 in absolute value ##



###########################
####        PCA         ###
###########################
pca <- princomp(dtcl,cor=T)

## Table 2: PCA. Most important loadings ###
pcs <- cbind(loadings(pca)[,1],-loadings(pca)[,2])
loadings <- matrix(" ",nrow(pcs),ncol(pcs))
colnames(loadings) <- c("Equal-Unequal","Individual-Universal")
rownames(pcs) <- names_char_all
for (j in seq(along=1:ncol(pcs))) {
  pcsordj <- pcs[order(abs(pcs[,j]),decreasing=T),j]
  namesj <- names(pcsordj);
  namesj[which(pcsordj<0)] <- paste("No",namesj[which(pcsordj<0)])
  loadings[,j] <- namesj
  loadings[which(abs(pcsordj)<0.3),j] <- ""
}
loadings <- loadings[!apply(loadings,1,function(x)all(x=="")),]
htmlTable(loadings,file="loadings.html",title="",label="loadings",caption="PCA. Most important loadings")


## Figure 1. The location of clientelism clusters on the two first PCA dimensions ##
dtc$dim_v <- pca$score[,1]
dtc$dim_h <- -pca$score[,2]

png("clusters2D_benchmark.png",height=480,width=480)
plot(dtc$dim_h,dtc$dim_v,type="p",pch=groups,col=groups,
     xlab="Individual-Universal dimension (2nd Principal Component)",ylab="Equal-Unequal dimension (1st Principal Component)")
legend("topright",legend=cnames_bench,pch=c(1:nclust),col=c(1:nclust))
# text(dtc$dim_h,dtc$dim_v,label=dtc$idc)
text(dtc$dim_h[grep("lazar",dtc$author)],dtc$dim_v[grep("lazar",dtc$author)],label="Lazar",col=1)
text(dtc$dim_h[grep("auyero",dtc$author)[1]],dtc$dim_v[grep("auyero",dtc$author)[1]],label="Auyero",col=2) ## the first Auyero (inner circle)
text(dtc$dim_h[grep("gay",dtc$author)],dtc$dim_v[grep("gay",dtc$author)],label="Gay",col=3)
dev.off()



################################
####  Normative Evaluation  ####
################################
evalv1 <- c("good_dealq","agencyq","alt_cl_num")
evalv_names <- c("Good deal","Agency","Alternatives")

## Table 3. Clientelism Dimensions and Client Welfare ##
regs <- function(outc,outcname=outc,dat) {
  datt <- na.omit(dat[,c(outc,"dim_h","dim_v","author")])
  rggd <- lm(paste(outc,"~dim_h+dim_v"),data=datt)
  aa <- list(model=robust.se(rggd,datt$author)[[2]],
             outcome=outcname,
             N=nrow(datt))
  return(aa)
}

mdls <- mdls2 <- list(NULL)
for (j in seq(along=evalv1)) {
  mdls[[j]] <- regs(outc=evalv1[j],dat=dtc,outcname =evalv_names[j])
}
coefnamess <- c("Individual-Universal dimension","Equal-Unequal dimension")
tbrg <- pt.tbl(mdls,selcoef="dim",coefnames=coefnamess,addinf=c(2,3))
w <- tbl.htm(list(tbrg),file="rgs_eval",rgroup=c("Analysis using all variables",""),n.rgroup=c(4,2))
sink("rgs_eval.html")
print(w)
sink()

### Table 4. Agency and Good Deal within Types of Clientelism ###
rggg <- lm(good_dealq~agencyq+factor(groups),data=dtc)
coeftest(rggd, vcov. = vcovHC(rggd, type = 'HC1'))
rsegg <- diag(sqrt(vcovHC(rggg, type = "HC1")))
sink("agency_good_deal.html")
stargazer(rggg,type="html",covariate.labels=c("Agency",cnames_bench[-1]), dep.var.labels ="Good deal",
          se = list(rsegg),keep.stat = c("n"),intercept.bottom = TRUE)
sink()



