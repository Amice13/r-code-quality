######################################################################

##      PROJECT TITLE:    MIDAS                                        
##      CODE AUTHOR:      ALEX STENLAKE & THOMAS S. ROBINSON                        

##      DESCRIPTION:      KROPKO ET AL. TEST
##                        NB: CODE ADAPTED FROM KROPKO ET AL. (2014)

######################################################################

library(mi)
library(Amelia)
library(mvnmle)
library(MASS)
library(norm)
library(nnet)
library(arm)

source("kropko/helper2.R")
source("kropko/midas_handler_cont.R")

set.seed(89)

nfull <- 3L
part <- 1
iterations <- 1000
restrict <- "triangular"
if(restrict=="triangular") {NMAR <- FALSE}
if(restrict=="none") {NMAR <- TRUE}
N <- 1000
str <- 0
runs <- 1

corrs <- c("low","high") # Loop through both correlations

for (corr in corrs){
  
if (!file.exists("kropko/data_tmp")) {
  dir.create("kropko/data_tmp")
} else {
  unlink("kropko/data_tmp", recursive = TRUE)
  dir.create("kropko/data_tmp")
}
    
results <- as.numeric()
resfile <- paste(c("results",part,".csv"), collapse="")

midas_dgp <- list()

resnames <- c("iteration", "variable.type", "N", "n.cat", "n.partial", "NMAR", "strong", "mean.CPC", "method", "imputation.method", 
	"mv.match", "mv.match.bias", "Pr.rmse", "Pr.bias", "coef1", "coef1.bias", "coef1.mns", "coef2", "coef2.bias", "coef2.mns", 
	"fit1.all", "fit1.all.bias", "fit2.all", "fit2.all.bias", "fit1.av", "fit1.av.bias", "fit2.av", "fit2.av.bias", "time")

if(corr == "high"){
  eta <- 1000
  outname <- paste(c("kropko/highcorr_results.csv"), collapse="")
} else {
  eta <- 300
  outname <- paste(c("kropko/lowcorr_results.csv"), collapse="")
}

for(it in 1:iterations) {
    
    print("   ")
    print("Iteration:")
    print(it)
    print("##########")
    print("   ")
	itnum <- it + (part-1)*iterations
	for(npart in c(4)){#c(0,2,4,6)){
	    print("Num partial")
	    print(npart)
		for(im in c("ppd")){
				
	#CONTINUOUS
			dgp.co <- .dgp(N=N, N_FULL=nfull, N_PARTIAL=npart, restrict=restrict, type="continuous", pr_miss=.25, imp_meth=im,
			               strong=str, eta_in= eta)
			
			cc.res <- .completecase(dgp.co)
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "Complete Cases", im,  NA, 
					NA, NA, NA, cc.res$coef1, cc.res$coef1.bias, cc.res$coef1.mns, cc.res$coef2, cc.res$coef2.bias, cc.res$coef2.mns, 
					NA, NA, NA, NA, cc.res$fit1.av, cc.res$fit1.av.bias, cc.res$fit2.av, cc.res$fit2.av.bias, NA))
					
			# LAZY MIDAS INPUT
			midas.res <- .midas(dgp.co, m=10)
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "Midas", im,
			                             midas.res$mvmatch, midas.res$mvmatch.bias, midas.res$pr.rmse, midas.res$pr.bias,
			                             midas.res$coef1, midas.res$coef1.bias, midas.res$coef1.mns,
			                             midas.res$coef2, midas.res$coef2.bias, midas.res$coef2.mns,
			                             midas.res$fit1.all, midas.res$fit1.all.bias, midas.res$fit2.all, midas.res$fit2.all.bias,
			                             midas.res$fit1.av, midas.res$fit1.av.bias, midas.res$fit2.av, midas.res$fit2.av.bias, midas.res$time))

			midas_dgp[[it]] <- dgp.co
			
			amelia.res <- .amelia(dgp.co, m=10, command="amelia")
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "Amelia", im, 
				amelia.res$mvmatch, amelia.res$mvmatch.bias, amelia.res$pr.rmse, amelia.res$pr.bias, 
				amelia.res$coef1, amelia.res$coef1.bias, amelia.res$coef1.mns, 
				amelia.res$coef2, amelia.res$coef2.bias, amelia.res$coef2.mns,
				amelia.res$fit1.all, amelia.res$fit1.all.bias, amelia.res$fit2.all, amelia.res$fit2.all.bias, 
				amelia.res$fit1.av, amelia.res$fit1.av.bias, amelia.res$fit2.av, amelia.res$fit2.av.bias, amelia.res$time))
			
			norm.res <- .amelia(dgp.co, m=10, ncat=ncat, command="norm")
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "NORM", im, 
				norm.res$mvmatch, norm.res$mvmatch.bias, norm.res$pr.rmse, norm.res$pr.bias, 
				norm.res$coef1, norm.res$coef1.bias, norm.res$coef1.mns, 
				norm.res$coef2, norm.res$coef2.bias, norm.res$coef2.mns,
				norm.res$fit1.all, norm.res$fit1.all.bias, norm.res$fit2.all, norm.res$fit2.all.bias, 
				norm.res$fit1.av, norm.res$fit1.av.bias, norm.res$fit2.av, norm.res$fit2.av.bias, norm.res$time))

			mcar.res <- .mi(dgp=dgp.co, m=10, chains=5, iter=30, est="MNL", usena=FALSE, mcar=TRUE)
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "MCAR", im, 
				mcar.res$mvmatch, mcar.res$mvmatch.bias, mcar.res$pr.rmse, mcar.res$pr.bias, 
				mcar.res$coef1, mcar.res$coef1.bias, mcar.res$coef1.mns, 
				mcar.res$coef2, mcar.res$coef2.bias, mcar.res$coef2.mns,
				mcar.res$fit1.all, mcar.res$fit1.all.bias, mcar.res$fit2.all, mcar.res$fit2.all.bias, 
				mcar.res$fit1.av, mcar.res$fit1.av.bias, mcar.res$fit2.av, mcar.res$fit2.av.bias, mcar.res$time))
			
			mnl.res <- .mi(dgp=dgp.co, m=10, chains=5, iter=30, est="MNL", usena=FALSE, mcar=FALSE)
			results <- rbind(results, c(itnum, dgp.co$type, N, NA, npart, NMAR, str, dgp.co$nmar, "MI: MNL", im, 
				mnl.res$mvmatch, mnl.res$mvmatch.bias, mnl.res$pr.rmse, mnl.res$pr.bias, 
				mnl.res$coef1, mnl.res$coef1.bias, mnl.res$coef1.mns, 
				mnl.res$coef2, mnl.res$coef2.bias, mnl.res$coef2.mns,
				mnl.res$fit1.all, mnl.res$fit1.all.bias, mnl.res$fit2.all, mnl.res$fit2.all.bias, 
				mnl.res$fit1.av, mnl.res$fit1.av.bias, mnl.res$fit2.av, mnl.res$fit2.av.bias, mnl.res$time))
			
		}
	}
}
colnames(results) <- resnames
write.csv(results, file=outname, sep=",", col.names=TRUE, row.names=FALSE)

print(paste0("Results saved for ",corr," correlation simulation."))
}

# unlink("kropko/data_tmp", recursive = TRUE)
