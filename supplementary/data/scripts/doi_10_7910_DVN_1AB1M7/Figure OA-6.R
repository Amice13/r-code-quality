library(MASS)
library(ggplot2)
library(patchwork)

#####################################
# Appendix C5                       #
#####################################

## Correlated
L <- 2
set.seed(1200)
Sigma <- diag(L)
Sigma[1,2] <- Sigma[2, 1] <- 0.8
X <-  mvrnorm(n = 30, mu = rep(0, L), Sigma = Sigma)
X_use <- apply(X, 2, scale)
colnames(X_use) <- c("X1", "X2")
X_use <- as.data.frame(X_use)

## Independent
L <- 2
set.seed(1500)
Sigma <- diag(L)
X <-  mvrnorm(n = 30, mu = rep(0, L), Sigma = Sigma)
X_ind <- apply(X, 2, scale)
colnames(X_ind) <- c("X1", "X2")
X_ind <- as.data.frame(X_ind)

## Non-Linear
set.seed(2000)
Sigma <- diag(L)
X  <-  seq(from = -2, to = 0, by = 0.1)
X2 <-  dnorm(seq(from = -2, to = 2, by = 0.2)) + rnorm(n = length(X), sd = 0.05)
X_2  <-  seq(from = 0, to = 2, by = 0.1)
X2_2 <-  dnorm(seq(from = -2, to = 2, by = 0.2)) + rnorm(n = length(X), sd = 0.05)
X_non <- cbind(c(X, X_2), c(X2, X2_2))
colnames(X_non) <- c("X1", "X2")
X_non <- as.data.frame(X_non)

p1 <- ggplot(X_use, aes(x=X1, y=X2)) +
  geom_point(size=3, shape=19) + theme_bw() +
  xlab("Observed variable") + ylab("Unobserved variable") +
  theme(axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text = element_text(color = "black", size = 10),
        title = element_text(size = 13, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.5, 1.2, 0.5, 0.5, "cm"),
        panel.grid = element_blank()) +
  ggtitle("Correlated")

p2 <- ggplot(X_ind, aes(x=X1, y=X2)) +
  geom_point(size=3, shape=19) + theme_bw() +
  xlab("Observed variable") + ylab("Unobserved variable") +
  theme(axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text = element_text(color = "black", size = 10),
        title = element_text(size = 13, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.5, 1.2,0.5, 0.5, "cm"),
        panel.grid = element_blank()) +
  ggtitle("Independent")

p3 <- ggplot(X_non, aes(x=X1, y=X2)) +
  geom_point(size=3, shape=19) + theme_bw() +
  xlab("Observed variable") + ylab("Unobserved variable") +
  theme(axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 15)),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.text = element_text(color = "black", size = 10),
        title = element_text(size = 13, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.5,0.5,0.5, 0.5, "cm"),
        panel.grid = element_blank()) +
  ggtitle("Highly Non-Linear")

#####################################
# Figure OA-6                       #
#####################################

pdf("../Figures/Figure OA-6.pdf", height = 4, width = 4)
p1
p2
p3
dev.off()