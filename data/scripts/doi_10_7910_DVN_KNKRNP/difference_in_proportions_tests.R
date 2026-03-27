# Script to replicate Chi-squared test results from Bergner, Desmarais, and Hird
# Journal of Behavioral Public Administration

# total click throughs over total opens
## activists
x_activists_journal_original_peer <- c(6901,7519,7363)
n_activists_journal_original_peer <- c(29183,33031,32895)
pair_ps_activists <- matrix(NA,3,3)
for(i in 2:3){
	for(j in 1:2)
	pair_ps_activists[i,j] <- prop.test(x_activists_journal_original_peer[c(i,j)],n_activists_journal_original_peer[c(i,j)])$p.value
}
# difference between control and peer review is not significant


click_activists <- x_activists_journal_original_peer
noclick_activists <-  n_activists_journal_original_peer-x_activists_journal_original_peer

table_activists <- as.table(rbind(click_activists,noclick_activists))

dimnames(table_activists) <- list(action=c("click","noclick"),condition=c("control","journal","peerreview"))

activists_test <- chisq.test(table_activists)

activists_test$observed 

activists_proportions <- x_activists_journal_original_peer/n_activists_journal_original_peer

## experts
x_experts_journal_original_peer <- c(301,402,341)
n_experts_journal_original_peer <- c(1919,2039,1945) 
# pairwise differences
pair_ps_experts <- matrix(NA,3,3)
for(i in 2:3){
	for(j in 1:2)
	pair_ps_experts[i,j] <- prop.test(x_experts_journal_original_peer[c(i,j)],n_experts_journal_original_peer[c(i,j)])$p.value
}
# difference between control and peer review is not significant

click_experts <- x_experts_journal_original_peer
noclick_experts <-  n_experts_journal_original_peer-x_experts_journal_original_peer

table_experts <- as.table(rbind(click_experts,noclick_experts))

dimnames(table_experts) <- list(action=c("click","noclick"),condition=c("control","journal","peerreview"))

experts_test <- chisq.test(table_experts)

experts_test$observed 

experts_proportions <- x_experts_journal_original_peer/n_experts_journal_original_peer