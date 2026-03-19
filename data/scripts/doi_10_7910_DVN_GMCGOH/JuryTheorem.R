# ###############################
# Condorcet's
# Jury Theorem
# for a Class regarding Democracy
# Yuta KAMAHARA
# May 21, 2019 
# ###############################


remove(list=ls())

# Condorcet's Jury Theorem with the odd number of less than 102 juries  (N <= 101)
result <- data.frame(n=NA, prob=NA)
for (i in seq(3, 101, 2)){ # You can choose the maximum number of juries up to 169
  n4 <- NULL
  for (j in ceiling(i/2):i){ # ceiling(i/2): majority
    n1 <- j
    n2 <- (factorial(i)/(factorial(j)*factorial(i-j)))*(0.6^j)*((1-0.6)^(i-j)) # fatorical() only for less than 170
    n3 <- cbind(n1, n2)
    n4 <- rbind(n4, n3)
  }
  result[floor(i/2), 1] <- i
  result[floor(i/2), 2] <- sum(n4[,2], na.rm=TRUE)
}

#png("condorcet.png")
plot(result$n, result$prob, type="l", xlim=c(min(result$n),max(result$n)), ylim=c(0.00,1.00), 
     xlab="N", ylab="Prob. of Correct Judgement", main="Condorcet's Jury Theorem (p=0.6)")
abline(h=0.5, col="red")
#dev.off()

# prob. of correct judgement is the same as that of the coin flip = 0.5
result0.5 <- data.frame(n=NA, prob=NA)
for (i in seq(3, 101, 2)){
  n4_0.5 <- NULL
  for (j in ceiling(i/2):i){
    n1_0.5 <- j
    n2_0.5 <- (factorial(i)/(factorial(j)*factorial(i-j)))*(0.5^j)*((1-0.5)^(i-j))
    n3_0.5 <- cbind(n1_0.5, n2_0.5)
    n4_0.5 <- rbind(n4_0.5, n3_0.5)
  }
  result0.5[floor(i/2), 1] <- i
  result0.5[floor(i/2), 2] <- sum(n4_0.5[,2], na.rm=TRUE)
}

plot(result0.5$n, result0.5$prob, type="l", xlim=c(min(result0.5$n), max(result0.5$n)), ylim=c(0.00,1.00), xlab="N", ylab="Prob. of Correct Judgement")

# prob. of correct judgement is less than that of the coin flip = 0.49
result0.49 <- data.frame(n=NA, prob=NA)
for (i in seq(3, 101, 2)){
  n4_0.49 <- NULL
  for (j in ceiling(i/2):i){
    n1_0.49 <- j
    n2_0.49 <- (factorial(i)/(factorial(j)*factorial(i-j)))*(0.49^j)*((1-0.49)^(i-j))
    n3_0.49 <- cbind(n1_0.49, n2_0.49)
    n4_0.49 <- rbind(n4_0.49, n3_0.49)
  }
  result0.49[floor(i/2), 1] <- i
  result0.49[floor(i/2), 2] <- sum(n4_0.49[,2], na.rm=TRUE)
}

#png("condorcet04.png")
plot(result0.49$n, result0.49$prob, type="l", xlim=c(min(result0.49$n), max(result0.49$n)), ylim=c(0.00,1.00), 
     xlab="N", ylab="Prob. of Correct Judgement", main="Condorcet's Jury Theorem (p=0.49)")
abline(h=0.5, col="red")
#dev.off()

# prob. of correct judgement is less than that of coin flip = 0.4
result0.4 <- data.frame(n=NA, prob=NA)
for (i in seq(3, 101, 2)){
  n4_0.4 <- NULL
  for (j in ceiling(i/2):i){
    n1_0.4 <- j
    n2_0.4 <- (factorial(i)/(factorial(j)*factorial(i-j)))*(0.4^j)*((1-0.4)^(i-j))
    n3_0.4 <- cbind(n1_0.4, n2_0.4)
    n4_0.4 <- rbind(n4_0.4, n3_0.4)
  }
  result0.4[floor(i/2), 1] <- i
  result0.4[floor(i/2), 2] <- sum(n4_0.4[,2], na.rm=TRUE)
}

#png("condorcet04.png")
plot(result0.4$n, result0.4$prob, type="l", xlim=c(min(result0.4$n), max(result0.4$n)), ylim=c(0.00,1.00), 
     xlab="N", ylab="Prob. of Correct Judgement", main="Condorcet's Jury Theorem (p=0.4)")
abline(h=0.5, col="red")
#dev.off()

# Condorcet's Jury Theorem with the odd number of less than 694 juries  (N <= 693) because of the limiation of R's display.
# So, potentially, we can obtain the infinite number of juries.
# But, for more than 693 juries, R exhibits probibality 1!
result2 <- data.frame(n=NA, prob=NA)
for (i in seq(3, 693, 2)){
  n4.693 <- NULL
  for (j in ceiling(i/2):i){
    n1.693 <- j
    n2.693 <- exp(lfactorial(i)-(lfactorial(j)+lfactorial(i-j)))*(0.6^j)*((1-0.6)^(i-j)) # using exponential and log of factorial()
    n3.693 <- cbind(n1.693, n2.693)
    n4.693 <- rbind(n4.693, n3.693)
  }
  result2[floor(i/2), 1] <- i
  result2[floor(i/2), 2] <- sum(n4.693[,2], na.rm=TRUE)
}

#png("condorcet2.png")
plot(result2$n, result2$prob, type="l", xlim=c(min(result2$n),max(result2$n)), ylim=c(0.00,1.00), 
     xlab="N", ylab="Prob. of Correct Judgement", main="Condorcet's Jury Theorem (p=0.6)")
abline(h=0.5, col="red")
#dev.off()
