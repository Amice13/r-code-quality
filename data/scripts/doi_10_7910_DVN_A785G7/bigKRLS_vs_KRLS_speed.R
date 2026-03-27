# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

# all scripts in the replication materials assume bigKRLS 3.0.0 or higher
require(pacman)                   
p_load(bigKRLS, update = TRUE)
p_load(rbenchmark, KRLS, ggplot2)

###################################
# speed testing bigKRLS vs. KRLS  #
###################################

set.seed(2017)
N <- 4000
Pcontinuous <- 10
Pbinary <- 10
X <- cbind(matrix(runif(N*Pcontinuous), ncol=Pcontinuous),
           matrix(sample(0:1, N*Pbinary, replace = T), ncol=Pbinary))
y <- X %*% runif(Pcontinuous + Pbinary) + rnorm(N)

# does 4 trials at N = 1000, 2000... 4000
# each round of results in two rows, one for bigKRLS::bigKRLS, one for KRLS::krls

results <- matrix(nrow=10, ncol=10)
Nsample <- seq(1000, N, 1000)
results <- cbind(results, unlist(lapply(Nsample, rep, 2)), Pbinary, Pcontinuous)
rownames(results) <- paste(rep(c("bigKRLS_N", "krls_N"), 5), Nsample, sep="")

dryrun <- benchmark(krls(X[1:100,], y[1:100]), 
                 bigKRLS(y[1:100], X[1:100,]), replications = 10)
colnames(results)[1:7] <- colnames(dryrun)[2:8]

for(i in 1:length(Nsample)){
  
  tmp <- benchmark(krls(X[1:Nsample[i], ], y[1:Nsample[i]], print.level=2), 
                   bigKRLS(y[1:Nsample[i]], X[1:Nsample[i], ], eigtrunc=0), # assumes bigKRLS 3.0 or higher
                   replications = 1)
  
  results[((2*i - 1):(2*i)), 1:7] <- as.numeric(as.matrix(tmp[,2:8]))
  cat("\n\nfinished", i, "\n\n")
  print(results[1:(2*i), ])
  cat("\n\n")
  write.csv(results, file="krls_speedtests.csv")
}

# graph found in paper

if("speed_results.RData" %in% dir()){
  
  load("speed_results.RData")
  speedplot <- ggplot(data=graph, aes(x = N, y = Runtime, group = Algorithm, colour=Algorithm)) +
    geom_line() + geom_point(aes(shape=Algorithm)) + theme_minimal() + ylab("Runtime in Minutes")
  speedplot 

  speedplot_bw <- ggplot(data=graph, aes(x = N, y = Runtime, group = Algorithm, linetype=Algorithm, colour=Algorithm)) +
    geom_line() + geom_point(aes(shape=Algorithm)) + theme_minimal() + ylab("Runtime in Minutes") + scale_color_grey()
  speedplot_bw
  
}else{
  message("Please set or specify the working directory to the Figure_3 folder to load the original results.")
}


