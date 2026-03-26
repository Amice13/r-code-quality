library(VGAM)
library(emdbook)
library(BenfordTests)

# Prioritize beta binomial from emdbook
rbetabinom <- emdbook::rbetabinom
dbetabinom <- emdbook::dbetabinom

# Generate correct raw frequency plot:
mfrq <- function(x, n){
  y <- table(x)
  sort(y, decreasing=TRUE)[1:n]
}

rfplot <- function(x, title = ""){
  plot(as.numeric(names(table(x))), table(x), type = "h", ylim = c(0, max(table(x))+25), main = title, ylab = "Number of precincts", xlab = "Vote-shares of the United Russia", xaxt = 'n', bty ="n", col = "grey30", yaxt = "n")
    y <- mfrq(x, 9)
    abline(v=seq(0,1,by=0.1), lty=3, col = "grey")
    fracs <- fractions(as.numeric(names(y)))
    text(as.numeric(names(y)), y+15, fracs, cex=0.9, font = 2)
    axis(1, at=seq(0,1,by=0.1), seq(0,1,by=0.1))
    axis(2, at = seq(0, 400, by = 100))
}

# Wrapper function that plots a precinct level PMF for a precinct of size n and a draw of population level outcomes in 50,000 precincts (used to generate Figure 2 and the related figures in the Supplementary Appendix)

bplot <- function(n){
  pdf(paste("latex/figures/examples_paper", n, ".pdf", sep=""), width = 9.5, height = 4.5)
  N <- 50000
  X <- S(n)
  y <- sapply(X, p, N = n, turnout = .5, support = .59)
  par(mfrow = c(1, 2), mar = c(4, 4, 2, 0), mgp = c(2,1,0))
  title = paste("Probability mass function \n", n, "voters")
  plot(X, y, type = "h", xlim = c(0.4, 0.8), xaxt = "n", ylab = "Probability", xlab = "Vote-share", main = title, bty = "n")
  axis(1, at = seq(-1, 2, by = 0.1))

  nn <- sample(100:1500, N, replace = TRUE)
  t <- rbinom(N, nn, rbeta(n, 2, 2))
  v <- rbinom(N, t,  rbeta(n, 2, 1))
  y <- v/t
  y <- y[y > 0 & y < 1]

  title = "Population distribution \n across 50,000 precincts"
  plot(as.numeric(names(table(y))), table(y), type = "h", lwd = .8, xaxt = "n", yaxt = "n", bty = "n", xlim = c(0, 1), xlab = "Vote-share", ylab = "Frequency in the population", main = title)
  axis(1, at = seq(-1, 2, by = .1))
  axis(2, at = c(0, seq(50, max(table(y)) + 100, by = 50)))
  dev.off()
}


# Produce a plot from the sample
pplot <- function(x, title = ""){
  par(mar = c(2, 0, 2, 0))
  plot(as.numeric(names(table(x))), table(x), type = "h", yaxt = "n", ylab = "", bty = "n", xlab = "", main = title, lwd = 1.2)
}

# Conditionally uniform
f1 <- function(n){
  t <- sample(1:n, 1)
  v <- sample(0:t, 1)
  v/t
}

# Binomial
f2 <- function(n){
  t <- rbinom(10000, n, .5)
  v <- rbinom(10000, t, .59)
  v/t
}

# Beta-Binomial
f3 <- function(n, theta = 10){
  t <- rbetabinom(10000, size = n, prob = .5,  theta = theta)
  v <- rbetabinom(10000, size = t, prob = .59, theta = theta)
  v/t
}

# Hyper-geometric
f4 <- function(n){
  t <- rhyper(1, .75*n, .25*n, k = (0.5/0.75)*n)
  v <- rhyper(1, .75*t, .25*t, k = (0.59/0.75)*t)
  v/t
}


# Truncated poisson
f5 <- function(n){
  t <- rpois(100000, 500)
  t <- t[t < n]
  t <- sample(t, 10000)
  v <- rpois(10000, 0.59*t)
  V <- v[v < t]
  T <- t[v < t]
  V/T
}

# Uniform on {1/4n, 3/4n} and {1/4}
f6 <- function(n){
  t <- sample((n/4):(3*n/4), 1)
  v <- sample(floor(0.4*t):floor(0.7*t), 1)
  v/t
}

# Population distributions given variable levels of dispersion
pop.plot <- function(theta, N){

  nn <- sample(500:2000, N, replace = TRUE)
  t <- VGAM::rbetabinom(N, size = nn, prob = rbeta(N, 2, 2), rho = theta)
  v <- VGAM::rbetabinom(N, size = t,  prob = rbeta(N, 2, 1), rho = theta)

  y <- v/t
  y <- y[y > 0 & y < 1]

  title = paste("Population distribution (50,000 precincts) ","\n Overdispersion", "=", theta)
  plot(as.numeric(names(table(y))), table(y), type = "h", lwd = .8, xaxt = "n", yaxt = "n", bty = "n", xlim = c(0, 1), xlab = "Vote-share", ylab = "Frequency in the population", main = title)
  axis(1, at = seq(-1, 2, by = .1))
}

# For computations of relative probabilities
# Calculate probability of a fraction x, given expected turnout and expected support and the binomial generative model
p <- function(x, N, turnout, support){
  if(x == 0) return(sum(dbinom(0, 0:N, support)*dbinom(0:N, N, turnout)))
  if(x == 1) return(sum(dbinom(1:N, 1:N, support)*dbinom(1:N, N, turnout)))
  f <- as.numeric(unlist(strsplit(as.character(fractions(x)), '/')))
  if(f[2] > N) return(0)
  else return(sum(sapply(1:floor(N/f[2]), function(s) dbinom(f[1]*s, f[2]*s, support)*dbinom(f[2]*s, N, turnout))))
}

# Same as function p, only assumes beta-binomial model
g <- function(x, N, turnout, support, theta){
  if(x == 0) return(sum(dbetabinom(0, size = 0:N, prob = support, theta = theta)*dbetabinom(0:N, size = N, prob = turnout, theta = theta)))
  if(x == 1) return(sum(dbetabinom(1:N, size = 1:N, prob = support, theta  = theta)*dbetabinom(1:N, size = N, prob = turnout, theta = theta)))
  f <- as.numeric(unlist(strsplit(as.character(fractions(x)), '/')))
  if(f[2] > N) return(0)
  else return(sum(sapply(1:floor(N/f[2]), function(s) dbetabinom(f[1]*s, size = f[2]*s, prob = support, theta = theta)*dbetabinom(f[2]*s, size = N, prob = turnout, theta = theta))))
}

# Creates a sample space for outcomes when voting population is of size N
S <- function(N){
  m <- expand.grid(0:N, 1:N)
  m <- m[m[,1] <= m[,2], ]
  unique(sort(m[,1]/m[,2]))
}


# Relative probability of a low-order fraction:
# Binomial model
f.1 <- function(n) p(0.60, N = n, 0.5, 0.59)/p(0.59, N = n, 0.5, 0.59)
# Beta-binomial model
f.2 <- function(n, theta) g(0.60, N = n, 0.5, 0.59, theta = theta)/g(0.59, N = n, 0.5, 0.59, theta = theta)

# Extract n most frequent fractions from x
mfrq <- function(x, n){
  y <- table(x)
  sort(y, decreasing=TRUE)[1:n]
}

### Digit-based tests
ldig <- function(x){
  x <- as.character(x)
  as.numeric(substr(x, nchar(x), nchar(x)))
}

last.dig <- function(x){
  x <- as.character(x)
  y <- as.numeric(substr(x, nchar(x), nchar(x)))
  chisq.test(table(y), simulate.p.value = TRUE)
}

bdig <- function(x, n){
  null <- sapply(0:9, function(d) sum(sapply(10^(n-2):(10^(n-1) - 1), function(k) log10(1 + 1/(10*k + d)))))
  x <- x[x > 0]
  y <- signifd(x, n)
  if(n == 1) null <- log10(1 + 1/1:9)
  y <- as.numeric(substr(as.character(y), n, n), levels = 0:9)
  chisq.test(table(factor(y)), p = null, simulate.p.value = TRUE)
}

# Function to simulate synthetic data
synth <- function(r){
  N <- sample(500:1500, 50000, replace = TRUE)
  n <- length(N)
  t <- rbinom(n, N, rbeta(length(N), 2, 2))
  alpha <- c(8, 7)
  beta  <- c(8, 3)
  K <- 2
  w <- c(.7, .3)
  k <- sample(1:K, n, prob = w, replace = TRUE)
  p <- rbeta(n, alpha[k], beta[k])
  v <- rbinom(n, t, p)
  data <- subset(data.frame(N, t, v), t > 0)
  if(r > 0){
    W <-  seq(0.6, .95, by = 0.05)
    w0 <- which(!as.character(data$v/data$t)%in%as.character(W))
    v  <- as.integer(sample(W, length(w0), replace = TRUE, prob = c(1, 3, 3, 1/2, 1/2, 3, 3, 5))*data$t[w0])
    z <- sapply(v/data$t[w0], function(z) min(abs(z-W)))
    ww <- order(z)[1:round(r*n)]
    data$v[w0][ww] <- v[ww]
  }
  data
}