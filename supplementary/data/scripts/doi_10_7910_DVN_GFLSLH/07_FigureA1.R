#### 1. Setting Seed ####
## Set random number generator seed
set.seed(519)

#### 2. DGP setup ####
## create values of correlation 
r <- seq(-.95,.95, length=100)

## create values of estimate 2
x2 <- seq(0,5, length=100)

## collect all combinations of r and x2
eg <- expand.grid(r = r, x2=x2)

#### 3. Go through all cases ####
## initialize output objects
pct_olap <- sig_diff <- NULL

## Loop over all rows of eg
for(i in 1:nrow(eg)){
  ## create variance-covariance matrix for estimates
  ## identity matrix with r[i] as the covariance
  S <- diag(2)
  S[1,2] <- S[2,1] <- eg[i,1]
  
  ## create confidence interval for estimate of zero
  int1 <- c(-1,1)*qnorm(.975)
  
  ## create confidence interval for hypothetical 
  ## estimate from eg
  int2 <- eg[i,2] + c(-1,1)*qnorm(.975)
  
  ## calculate the percentage overlap
  pct_olap <- c(pct_olap, (int1[2] - int2[1])/diff(int1))
  
  ## calculate the z-statistic for the difference 
  ## between the two hypothetical estimates
  z_stat <- eg[i,2]/sqrt(S[1,1] + S[2,2] - 2*S[1,2])
  
  ## calculate and save the two-sided p-value. 
  sig_diff <- c(sig_diff, 2*pnorm(z_stat, lower.tail=FALSE))
}

#### 4. Data for plot and plot ####
## For values where percent overlap is negative, make it zero. 
pct_olap <- ifelse(pct_olap < 0, 0, pct_olap)

## add percentage overlap and p-value to eg
eg$pct_olap <- pct_olap
eg$pval <- sig_diff

eg %>% 
  ## keep only significant results
  filter(pval < .05) %>% 
  ## group by correlation
  group_by(r) %>% 
  ## keep the maximum percentage overlap
  slice_max(pct_olap) %>% 
  ## make graph
  ggplot(aes(x=r, y=pct_olap)) + 
  geom_smooth(se=FALSE, color="black") + 
  theme_classic() + 
  scale_y_continuous(labels=scales::label_percent())+ 
  labs(x="Covariance Between Estimates", 
       y="Highest Percentage of Overlap")
ggsave("results/FigureA1.png", height=6, width=6, units="in", dpi=300)

