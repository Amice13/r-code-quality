# *********************************************************************
# Replication code for Systemic Discrimination Among Large U.S. Employers
# Patrick M. Kline, Evan K. Rose, Christopher R. Walters
# April, 2022

# This code produces p-values for one- and two-sided firm-specific tests
# of discrimation using various methods, including randomization inference.
# The output is used by table8.R to form q-value estimates.

# It must be run in the top level of the replication archive directory.
# *********************************************************************

# Load library
library(haven)
library(tidyverse)
library(qvalue)
library(rvalues)
library(foreach)
library(doSNOW)
library(lfe)
library(RColorBrewer)
library(ggtext)
library(ggplot2)
library(miceadds)
library(sandwich)
library(gtools)
library(exactRankTests)

set.seed(56721)
nloops <- 10000
ncores <- 50
dori <- TRUE

# Load up the data
df <- read_dta(file='data/data.dta')

# Drop apps sent out of order (~170 total)
df <- df %>% group_by(job_id,pair) %>% 
  mutate(napps = n(),
         nblack = sum(black)) %>% ungroup()
df <- df %>% filter(napps != 2 | nblack == 1)

# Iterate over attributes
attributes <- c("black", "female", "over40")
for (k in 1:length(attributes)) {
  tostudy <- attributes[k]

  ### First run app-level regresion with job clustered SES
  m <- lm.cluster(df,
            paste("cb ~ factor(firm_id) + factor(firm_id):factor(1-",tostudy,") - 1", sep=""),
            "job_id")
  firm_effects <- data.frame("app_level_est"=coef(m), "app_level_se"=sqrt(diag(m$vcov))) %>%
      rownames_to_column(var='coef') %>%  filter(grepl(":",coef)) %>%
      extract(coef, "firm_id", "\\(firm_id\\)([0-9]+)", remove=TRUE)

  firm_effects['app_level_twoside'] = 2*pt(abs(firm_effects[,"app_level_est"]/firm_effects[,"app_level_se"]), length(unique(df$job_id))-1, lower.tail=FALSE)
  firm_effects['app_level_oneside'] = pt(firm_effects[,"app_level_est"]/firm_effects[,"app_level_se"], length(unique(df$job_id))-1, lower.tail=FALSE)

  ### Now add job-level regression with robust SES
  jobs <- df %>% group_by(firm_id,wave,job_id) %>%
    summarize(black_cb = sum(cb*.data[[tostudy]])/sum(.data[[tostudy]]),
              white_cb = sum(cb*(1-.data[[tostudy]]))/sum(1-.data[[tostudy]])) %>%
    mutate(white_m_black = white_cb - black_cb) %>% drop_na()
  m <- lm(white_m_black ~ factor(firm_id) - 1, data=jobs)
  vcv = vcovHC(m, type="HC1")
  firm_effects <- data.frame("job_level_est"=coef(m), "job_level_se"=sqrt(diag(vcv))) %>%
      rownames_to_column(var='coef') %>%  filter(grepl("firm_id",coef)) %>%
      extract(coef, "firm_id", "\\(firm_id\\)([0-9]+)", remove=TRUE) %>% right_join(firm_effects, by="firm_id")

  firm_effects['job_level_twoside'] = 2*pt(abs(firm_effects[,"job_level_est"]/firm_effects[,"job_level_se"]), length(unique(df$job_id))-1, lower.tail=FALSE)
  firm_effects['job_level_oneside'] = pt(firm_effects[,"job_level_est"]/firm_effects[,"job_level_se"], length(unique(df$job_id))-1, lower.tail=FALSE)
  print(head(firm_effects))

  # Get Fisher exact p-values
  fisher <- function(fid) {
    tmp <- df %>% filter(firm_id == fid) 
    tmp <- table(tmp[[tostudy]],tmp$cb)
    fisher_p <- fisher.test(tmp)$p.value
    fisher_p_greater <- fisher.test(tmp, alternative="greater")$p.value
    fisher_p_less <- fisher.test(tmp, alternative="less")$p.value
    return(cbind(firm_id=fid, fisher_p=fisher_p, fisher_p_greater=fisher_p_greater, fisher_p_less=fisher_p_less))
  }
  fpvals <- as.data.frame(do.call(rbind,lapply(firm_effects$firm_id,FUN=fisher)))
  firm_effects <- firm_effects %>% left_join(fpvals, by="firm_id")

  # Add simple difference in means with correct DOF
  means <- df %>% group_by(firm_id, job_id, .data[[tostudy]]) %>% summarize(mean_cb=mean(cb))
  means <- means %>% spread(.data[[tostudy]],mean_cb) %>% drop_na()

  ttester <- function(fid) {
    tmp <- means %>% filter(firm_id == fid)
    test <- t.test(tmp[["1"]],tmp[["0"]], paired=TRUE)
    p <- test$p.value
    se <- test$stderr
    p_greater <- t.test(tmp[["0"]],tmp[["1"]], paired=TRUE, alternative="greater")$p.value
    p_less <- t.test(tmp[["0"]],tmp[["1"]], paired=TRUE, alternative="less")$p.value
    return(cbind(firm_id=fid, ttest_p=p, ttest_se=se, ttest_p_greater=p_greater, ttest_p_less=p_less))
  }

  fttests <- as.data.frame(do.call(rbind,lapply(firm_effects$firm_id,FUN=ttester)))
  firm_effects <- firm_effects %>% left_join(fttests, by="firm_id")

  # Get sign rank test
  if (tostudy == "black") {
    pairs <- df %>% group_by(firm_id, job_id, pair, .data[[tostudy]]) %>% summarize(cb=sum(cb))
    pairs <- pairs %>% spread(.data[[tostudy]],cb) %>% drop_na()  

    signranker <- function(fid) {
      tmp <- pairs %>% filter(firm_id == fid)
      tmp$d <- tmp[["0"]] - tmp[["1"]]
      pval <- wilcox.test(tmp$d)$p.value
      exactp <- wilcox.exact(tmp$d, exact=TRUE)$p.value

      pval_greater <- wilcox.test(tmp$d, alternative='greater')$p.value
      exactp_greater <- wilcox.exact(tmp$d, exact=TRUE, alternative='greater')$p.value

      return(cbind(firm_id=fid, signrank_p=pval, signrank_p_greater=pval_greater,
            signrank_p_exact=exactp, signrank_p_exact_greater=exactp_greater))
    }

    signrank <- as.data.frame(do.call(rbind,lapply(firm_effects$firm_id,FUN=signranker)))
    firm_effects <- firm_effects %>% left_join(signrank, by="firm_id")

    pairs <- pairs %>% group_by(firm_id, job_id) %>% summarize(`0`=sum(`0`), `1`=sum(`1`))

    signranker <- function(fid) {
      tmp <- pairs %>% filter(firm_id == fid)
      tmp$d <- tmp[["0"]] - tmp[["1"]]
      pval <- wilcox.exact(tmp$d, exact=FALSE)$p.value
      exactp <- wilcox.exact(tmp$d, exact=TRUE)$p.value

      pval_greater <- wilcox.exact(tmp$d, exact=FALSE, alternative='greater')$p.value
      exactp_greater <- wilcox.exact(tmp$d, exact=TRUE, alternative='greater')$p.value

      return(cbind(firm_id=fid, job_signrank_p=pval, job_signrank_p_greater=pval_greater,
            job_signrank_p_exact=exactp, job_signrank_p_exact_greater=exactp_greater))
    }

    signrank <- as.data.frame(do.call(rbind,lapply(firm_effects$firm_id,FUN=signranker)))
    firm_effects <- firm_effects %>% left_join(signrank, by="firm_id")
  }

  # Get RI p-values
  myCluster <- makeCluster(ncores, type="SOCK") # number of cores to use
  registerDoSNOW(myCluster)
  pb <- txtProgressBar(max = nloops, style = 3)
  progress <- function(n) {
      setTxtProgressBar(pb, n)
      Sys.sleep(0.01)
      flush.console()
    }
  opts <- list(progress = progress)

  ptm <- proc.time()
  ri_effects <- foreach(s = 1:nloops, .combine='rbind',
      .options.snow=opts, .packages=c("tidyverse","miceadds","sandwich", "gtools")) %dopar% {

    # Generate RI dataset
    ri <- select(df, app_id, firm_id, job_id, wave, order, pair, tostudy, cb)
    ri$rep <- s
    if (tostudy != "black") {
      ri[tostudy] <- permute(ri[[tostudy]])
    } else {
      ri[tostudy] <- rbinom(nrow(ri), 1, 0.5)*(ri$order %% 2)
      ri <- ri %>% group_by(rep, job_id, pair) %>% mutate(nblack = sum(black), napps=n())
      ri$black <- (ri$order %% 2 == 1)*ri$black + (ri$order %% 2 == 0)*(ri$nblack == 0)
      ri$black <- (ri$napps == 2)*ri$black + (ri$napps == 1)*rbinom(nrow(ri), 1, 0.5)
    }

    # First run app-level mean
    m <- lm(paste("cb ~ factor(firm_id) + factor(firm_id):factor(1-",tostudy,") - 1", sep=""),
              ri)
    ri_loop <- data.frame("app_level_est"=coef(m)) %>%
        rownames_to_column(var='coef') %>%  filter(grepl(":",coef)) %>%
        extract(coef, "firm_id", "\\(firm_id\\)([0-9]+)", remove=TRUE)

    # Now add job-level means 
    ri_jobs <- ri %>% group_by(firm_id,wave,job_id) %>%
      summarize(black_cb = sum(cb*.data[[tostudy]])/sum(.data[[tostudy]]),
                white_cb = sum(cb*(1-.data[[tostudy]]))/sum(1-.data[[tostudy]])) %>%
      mutate(white_m_black = white_cb - black_cb) %>% drop_na()
    m <- lm(white_m_black ~ factor(firm_id) - 1, data=ri_jobs)
    ri_loop <- data.frame("job_level_est"=coef(m)) %>%
        rownames_to_column(var='coef') %>%  filter(grepl("firm_id",coef)) %>%
        extract(coef, "firm_id", "\\(firm_id\\)([0-9]+)", remove=TRUE)  %>% right_join(ri_loop, by="firm_id")

    ri_loop$rep <- s
    ri_loop
  }
  stopCluster(myCluster)
  print(proc.time() - ptm)

  # Save RI effects
  write_csv(ri_effects, paste("dump/",tostudy,"_ri_effects.csv",sep=""))

  # Join true firm effects and get p-values
  ri_effects <- firm_effects %>% select(firm_id, app_level_est, job_level_est) %>%
        left_join(ri_effects, by='firm_id')
  pvalues_firm <- ri_effects %>% group_by(firm_id) %>%
    summarize(
      app_level_ri_oneside = mean(job_level_est.y > job_level_est.x),
      app_level_ri_twoside = mean(abs(job_level_est.y) > abs(job_level_est.x)),
      job_level_ri_oneside = mean(job_level_est.y > job_level_est.x),
      job_level_ri_twoside = mean(abs(job_level_est.y) > abs(job_level_est.x)),
      ) 
  pvalues_firm <- pvalues_firm %>% left_join(firm_effects, by="firm_id")
  pvalues_firm$firm_id <- as.integer(pvalues_firm$firm_id)

  write_csv(pvalues_firm, paste("dump/",tostudy,"p-values.csv",sep=""))
}

