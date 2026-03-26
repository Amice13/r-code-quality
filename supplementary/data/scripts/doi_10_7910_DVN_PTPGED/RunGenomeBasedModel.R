#Build the genome-based model on GIGANTEA.cpp/GIGANTEA.R
#by Akio Onogi, June 2018.

#read input files##########################################################################
Y <- as.matrix(read.csv("Y.csv", header=T, row.names=1))
Temp <- read.csv("Temperature.csv", header=T, row.names=1)[, 6]
Samplingpoint <- read.csv("Samplingpoint.csv", header=T, row.names=1)[, 1]
Geno <- as.matrix(read.csv("Geno.csv", header=T, row.names=1))

#number of individuals
Nl <- ncol(Y)


#Create inputs of GenomeBasedModel#########################################################
Input <- matrix(c(Samplingpoint, Temp),nc=1)
#all individuals share the same input information
Input <- Input[, rep(1, Nl)]

Freevec <- c(length(Samplingpoint), length(Temp))
Parametername <- c("a", "b", "pl", "th", "os", "ol")
Np <- length(Parametername)

#apply EBL for all parameters
Methodcode <- rep(2, Np)
#use the values in Nagano et al. (2012) as the reference values
Referencevalues <- c(12.8, -8.47, 371, 14.78, 21.2*60, 7.73*60)

Lowerlimit<-c(-1e+20, -1e+20, 0, -1e+20, 0, 0)
Upperlimit<-c(1e+20, 1e+20, 1440, 1e+20, 1440, 1440)
#0 < pl < 1440
#0 < os < 1440
#0 < ol < 1440

Missing<-999999


#add IDs in Y and Geno#####################################################################
Y<-rbind(1:Nl,Y)
Geno<-rbind(1:Nl,Geno)


#Run GenomeBasedModel#####################################################################
Rcpp::sourceCpp('GIGANTEA.cpp')
source("GIGANTEA.R")
library(GenomeBasedModel)
#will take 50 to 60 min.
Result.full <- GenomeBasedModel(Input, Freevec, Y, Missing, Np, Geno, Methodcode, Referencevalues, GIGANTEA_R, 
                                     Lowerlimit = Lowerlimit, Upperlimit = Upperlimit,
                                Nloop=10, Nite_MPI=100, Nite_MPI_last=300)

#distributions of inferred parameters
Para.mean <- NULL
for(i in 1:Np){
  Para.mean <- rbind(Para.mean, apply(Result.full$Para[[i]], 2, mean))
}

hist(Para.mean[1,])
hist(Para.mean[2,])
hist(Para.mean[3,])#true value is 371 for all individuals
hist(Para.mean[4,])
hist(Para.mean[5,])#true value is 1272 for all individuals 
hist(Para.mean[6,])


#compare with the true values
Para.true <- as.matrix(read.csv("Parameters.csv", header=T, row.names=1))
nrmse<-function(x,y){
  sqrt(mean((x-y)^2,na.rm=T))/abs(mean(y))
}

Stat.comparison <- matrix(NA, 10, 12)
colnames(Stat.comparison) <- paste(rep(Parametername, each=2), c("cor", "nrmse"), sep="_")
for(i in 1:Np){
  Stat.comparison [1, c(i*2 - 1, i*2)] <- c(cor(Para.mean[i,],Para.true[i,]), nrmse(Para.mean[i,],Para.true[i,]))
}
Stat.comparison[1,]


#see detected QTLs
QTL <- as.list(numeric(6))
names(QTL) <- Parametername
par(mfrow=c(2, 2))
par(mar=c(4, 4, 2, 1))
for(i in c(1, 2, 4, 6)){
  QTL[[i]] <- as.matrix(read.csv(paste("QTL.", Parametername[i], ".csv", sep=""),header=T, row.names=1))
  
  #estimated SNP effects
  plot(abs(Result.full$Genome[[i]]$Beta), main=Parametername[i], xlab="SNPs", ylab="Abs. effect size")
  
  #simulated QTL positions
  abline(v = QTL[[i]][,1], lty=2)
}


#Simulate missing in Y#######################################################################
Missingprop <- seq(0.1, 0.9, 0.1)

Result.missing <- as.list(numeric(length(Missingprop)))
names(Result.missing) <- Missingprop
Result.missingimputed <- as.list(numeric(length(Missingprop)))
names(Result.missingimputed) <- Missingprop

#select observations to be missed
Remove <- as.list(numeric(length(Missingprop)))
names(Remove) <- Missingprop
for(prop in Missingprop){
  Remove[[as.character(prop)]] <- matrix(FALSE, 25, Nl)
  Nremove <- round(Nl * prop)
  for(i in 1:25){
    Remove[[as.character(prop)]][i, sample(1:Nl, Nremove, replace=F)] <- TRUE
  }
}

for(prop in Missingprop[-1]){
  cat(prop, "\n")
  Y.missed <- Y
  Y.missed[-1,][Remove[[as.character(prop)]]] <- Missing
  
  #skip missing values
  Result.missing[[as.character(prop)]] <- GenomeBasedModel(Input, Freevec, Y.missed, Missing, Np, Geno, Methodcode, Referencevalues, GIGANTEA_cpp, 
                                                                Imputemissing=FALSE, Lowerlimit=Lowerlimit, Upperlimit=Upperlimit)
  
  #impute missing values with Monte Carlo simulation
  Result.missingimputed[[as.character(prop)]] <- GenomeBasedModel(Input, Freevec, Y.missed, Missing, Np, Geno, Methodcode, Referencevalues, GIGANTEA_cpp, 
                                                                       Imputemissing=TRUE, Lowerlimit=Lowerlimit, Upperlimit=Upperlimit)
}

#calculates stats (correlation and normalized RMSE)
for(j in 1:9){
  
  Para.mean <- NULL
  for(i in 1:Np){
    Para.mean <- rbind(Para.mean, apply(Result.missing[[j]]$Para[[i]], 2, mean))
  }
  
  for(i in 1:Np){
    Stat.comparison [j+1, c(i*2 - 1, i*2)] <- c(cor(Para.mean[i, ], Para.true[i, ]), nrmse(Para.mean[i, ], Para.true[i, ]))
  }
}
rownames(Stat.comparison)<-paste("MissingProp",seq(0,0.9,0.1),sep="")
Stat.comparison


Stat.comparison.imputed <- Stat.comparison
for(j in 1:9){
  
  Para.mean <- NULL
  for(i in 1:Np){
    Para.mean <- rbind(Para.mean, apply(Result.missingimputed[[j]]$Para[[i]], 2, mean))
  }
  
  for(i in 1:Np){
    Stat.comparison.imputed [j+1, c(i*2 - 1, i*2)] <- c(cor(Para.mean[i,],Para.true[i,]), nrmse(Para.mean[i,],Para.true[i,]))
  }
}
Stat.comparison.imputed



#Try another optimization#######################################################################
#Nelder-Mead by optim
GIGANTEA_optim <- function(parameters, input, freevec, y, missing){
  
  output <- GIGANTEA_cpp(input, freevec, parameters)
  use <- y != missing
  sum((y[use] - output[use])^2)
}

#initial values are generated by fluctuating reference values
#Nini points are generated
Nini <- 100
Initialvalues <- NULL
for(i in 1:Nini){
  Initialvalues <- rbind(Initialvalues, Referencevalues + rnorm(Np, 0, apply(Para.true, 1, sd)))
}

#optimize
Result.NM <- matrix(0, Nl, Np + 1)
for(i in 1:Nl){
  cat(i,"\n")
  temp <- matrix(0, Nini, Np + 1)
  for(j in 1:Nini){
    v <- optim(Initialvalues[j,], GIGANTEA_optim, input = Input[,i], freevec = Freevec, y = Y[-1, i], missing = Missing, method = "Nelder-Mead")
    temp [j, 1] <- v$value
    temp [j, -1] <- v$par
  }
  Result.NM[i, ] <- temp[which.min(temp[, 1]), ]
}


Result.NM.missing <- as.list(numeric(length(Missingprop)))
names(Result.NM.missing) <- Missingprop
for(prop in Missingprop[-9]){
  Y.missed <- Y
  Y.missed[-1,][Remove[[as.character(prop)]]] <- Missing 
  
  Result.NM.missing[[as.character(prop)]] <- matrix(0, Nl, Np + 1)
  for(i in 1:Nl){
    cat(prop,i,"\n")
    temp <- matrix(0, Nini, Np + 1)
    for(j in 1:Nini){
      v <- try(optim(Initialvalues[j,], GIGANTEA_optim, input = Input[,i], freevec = Freevec, y = Y.missed[-1, i], missing = Missing, method = "Nelder-Mead"))
      if(class(v) == "try-error"){
        temp [j, ] <- 1e+10
      }else{
        temp [j, 1] <- v$value
        temp [j, -1] <- v$par      
      }
    }
    Result.NM.missing[[as.character(prop)]][i, ] <- temp[which.min(temp[, 1]), ]
  }
}
rm(v, temp)
#The optimizer stops with some initial values. These were skipped using try function.
#The optimizer also returns fatal errors in the following conditions. These were also skiped
#prop = 0.9, i = 10, j = 92, 93, 94, and 95


#summarize the results
Stat.comparison.NM <- matrix(NA, 10, 12)
colnames(Stat.comparison.NM) <- colnames(Stat.comparison)
for(i in 1:Np){
  Stat.comparison.NM [1, c(i*2 - 1, i*2)] <- c(cor(Result.NM[, i+1], Para.true[i, ]), nrmse(Result.NM[, i+1], Para.true[i, ]))
}
for(j in 1:9){
  for(i in 1:Np){
    Stat.comparison.NM [j+1, c(i*2 - 1, i*2)] <- c(cor(Result.NM.missing[[j]][, i+1],Para.true[i,]), nrmse(Result.NM.missing[[j]][, i+1],Para.true[i,]))
  }
}
Stat.comparison.NM



#plot the results#################################################################################
#compare correlation
plot(Stat.comparison[, 1], type="o", pch=1, col=1, ylim=c(-0.2, 1), xaxt="n", xlab="Missing prop.", ylab="Correlation", main="Correlation (missing skipped in GBM)")
for(i in c(2, 4, 6)){
  points(Stat.comparison[, i * 2 - 1], type="o", pch=i, col=i)
}
for(i in c(1, 2, 4, 6)){
  points(Stat.comparison.NM[, i * 2 - 1], type="o", pch=i, col=i, lty=2)
}
axis(1, at=1:10, labels=seq(0, 0.9, 0.1))
legend(2, 0.4, pch=c(1, 2, 4, 6), legend=Parametername[c(1, 2, 4, 6)], col=c(1, 2, 4, 6))
legend(4, 0.25, lty=1:2, legend=c("GBM","NM"))


plot(Stat.comparison.imputed[, 1], type="o", pch=1, col=1, ylim=c(-0.2, 1), xaxt="n", xlab="Missing prop.", ylab="Correlation", main="Correlation (missing imputed in GBM)")
for(i in c(2, 4, 6)){
  points(Stat.comparison.imputed[, i * 2 - 1], type="o", pch=i, col=i)
}
for(i in c(1, 2, 4, 6)){
  points(Stat.comparison.NM[, i * 2 - 1], type="o", pch=i, col=i, lty=2)
}
axis(1, at=1:10, labels=seq(0, 0.9, 0.1))
legend(2, 0.4, pch=c(1, 2, 4, 6), legend=Parametername[c(1, 2, 4, 6)], col=c(1, 2, 4, 6))
legend(4, 0.25, lty=1:2, legend=c("GBM","NM"))


#compare Normalized RMSE
plot(Stat.comparison[, 2], type="o", pch=1, col=1, ylim=c(0, 3.2), xaxt="n", xlab="Missing prop.", ylab="Normalized RMSE", main="Normalized RMSE (missing skipped in GBM)")
for(i in c(2, 4, 6)){
  points(Stat.comparison[, i * 2], type="o", pch=i, col=i)
}
for(i in c(1, 2, 4, 6)){
  points(Stat.comparison.NM[, i * 2], type="o", pch=i, col=i, lty=2)
}
axis(1, at=1:10, labels=seq(0, 0.9, 0.1))
legend(2, 2.5, pch=c(1,2,4,6), legend=Parametername[c(1,2,4,6)], col=c(1,2,4,6))
legend(4, 2.5, lty=1:2, legend=c("GBM","NM"))

plot(Stat.comparison.imputed[, 2], type="o", pch=1, col=1, ylim=c(0, 3.2), xaxt="n", xlab="Missing prop.", ylab="Normalized RMSE", main="Normalized RMSE (missing imputed in GBM)")
for(i in c(2, 4, 6)){
  points(Stat.comparison.imputed[, i * 2], type="o", pch=i, col=i)
}
for(i in c(1, 2, 4, 6)){
  points(Stat.comparison.NM[, i * 2], type="o", pch=i, col=i, lty=2)
}
axis(1, at=1:10, labels=seq(0, 0.9, 0.1))
legend(2, 2.5, pch=c(1,2,4,6), legend=Parametername[c(1,2,4,6)], col=c(1,2,4,6))
legend(4, 2.5, lty=1:2, legend=c("GBM","NM"))


#NM removed for ease of visualization 
plot(Stat.comparison[, 2], type="o", pch=1, col=1, ylim=c(0, 0.4), xaxt="n", xlab="Missing prop.", ylab="Normalized RMSE", main="Normalized RMSE (missing skipped in GBM)")
for(i in 2:6){
  points(Stat.comparison[, i * 2], type="o", pch=i, col=i)
}
axis(1, at=1:10, labels=seq(0, 0.9, 0.1))
legend(2, 0.35, pch=c(1,2,4,6), legend=Parametername[c(1,2,4,6)], col=c(1,2,4,6))


plot(Stat.comparison.imputed[, 2], type="o", pch=1, col=1, ylim=c(0,0.4), xaxt="n", xlab="Missing prop.", ylab="Normalized RMSE", main="Normalized RMSE (missing imputed in GBM)")
for(i in 2:6){
  points(Stat.comparison.imputed[, i * 2], type="o", pch=i, col=i)
}
axis(1, at=1:10, labels=seq(0, 0.9, 0.1))
legend(2, .35, pch=1:6, legend=Parametername, col=1:6)

