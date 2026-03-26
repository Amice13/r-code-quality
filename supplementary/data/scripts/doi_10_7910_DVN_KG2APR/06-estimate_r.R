# Authors:     Original code written by Patrick Ball
# Maintainers: modified and maintained by: Anita R Gohdes
# Copyright:   2015, HRDAG, GPL v2 or later
# ============================================

library(dga)
library(chron)

data(graphs4)

make.dga.results <- function(strata) {
   num.lists <- 4
   delta <- 1/2^num.lists
   results <- list()
   posts <- list()
   for (i in 1:nrow(strata$overlap.counts)) {
      Y <- array(strata$overlap.counts[i,], dim=rep(2, num.lists))
      Nk <- sum(Y)
      top_N <- sum(Y) * 10
      Nmissing <- 1:top_N
      N <- Nmissing + sum(Y)
      weights <- bma.cr(Y, Nmissing, delta, graphs4)
      tmp <- apply(weights, 1, sum)
      post_prob <- apply(weights, 2, sum)
      m0s_sample_from_post <- sample(
          Nmissing, size=1000, replace=TRUE, prob=post_prob)
      nhat_sample_from_post <- m0s_sample_from_post + sum(Y)
      
      nhat <- round(mean(nhat_sample_from_post, na.rm=FALSE), 0)
      CIs <- quantile(nhat_sample_from_post,
                      probs=c(0.025, 0.975), na.rm=TRUE)
      CI_lo <- round(CIs[[1]], 0)
      CI_hi <- round(CIs[[2]], 0)
      stratum <- rownames(strata$overlap.counts)[i]
      processed <- list(
          stratum = stratum, Nk = Nk, nhat=nhat, CI_lo=CI_lo, CI_hi=CI_hi)
      results[[stratum]] <- processed
   }

   return(list(results))
}

load("classified_data.RData")

## remove other killings, only targeted & untargeted killings
data <- data[code==1 | code==2, ]
overlaps <- data[, c("in_1", "in_2", "in_3", "in_4")]
data[, stratum := paste(governorate, code, date2w, sep="_")]
data[, stratum := gsub(" ", ".", stratum)]
strata <- make.strata(overlaps, locations=data$stratum)

ests <- make.dga.results(strata)
results_df<- do.call(rbind.data.frame, ests[[1]])
results_df$stratum <- trimws(results_df$stratum)

results_df$label <- as.character(results_df$stratum)

### split up the stratum label to get gov, date + violtype separately
strata.labels <- strsplit(results_df$label, "_")
strata.labels <- do.call(rbind.data.frame, strata.labels)
names(strata.labels) <- c("governorate", "violtype", "date")
results_df <- cbind(strata.labels, results_df)

data <- data.table(results_df)

### only keep relevant cols
data <- data[, list(gov=governorate, date2w=date, code=violtype, nhat, hi=CI_hi, lo=CI_lo, Nk)]

data <- reshape(data, idvar = c("gov", "date2w"),
                timevar = "code", direction = "wide")

data <- as.data.frame(data)
names(data) <- gsub("[[:punct:]]1", ".untarg", names(data))
names(data) <- gsub("[[:punct:]]2", ".targ", names(data))

save(data, file="estimates-dga-date2w.Rdata")



  
  

   

