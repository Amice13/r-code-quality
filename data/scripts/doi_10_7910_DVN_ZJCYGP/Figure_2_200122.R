#####################################################################
##  Zhirnov, Moral, Sedashov - Taking Distributions Seriously ######
##  Replication of figure 2 (January 20, 2022) ###
#####################################################################

rm(list=ls())
start.time <- Sys.time()

library(ggplot2)
theme_set(theme_bw())
set.seed(123)

prprob <- function(x) {
  plogis(8*(x-0.65))
}
me <- function(x) {
  8*prprob(x)*(1-prprob(x))
}

df <- list()
df[[1]] <- data.frame(t=1, x = rbeta(1000,5,1))
df[[2]] <- data.frame(t=2, x = rbeta(1000,2,5))
df[[3]] <- data.frame(t=3, x = rbeta(1000,5,5))
df[[4]] <- data.frame(t=4, x = rbeta(1000,0.1,0.1))
df <- do.call("rbind", df)

dfx <- data.frame(x = seq(0,1,by=0.02))
dfx$y <- prprob(dfx$x)
dfx$dist <- 1


df$me <- me(df$x)
df$dist <- 1

dfm <- within(df, {
  x <- me
  dist <- 2
  })

mean_x <- aggregate(x ~ t + dist, data=df, FUN=mean)
colnames(mean_x)[colnames(mean_x)=="x"] <- "mean_x"
mean_x$lab <- "E(X)"
mean_x$yco <- 1 

mem <- within(mean_x, {
  mean_x <- me(mean_x)
  dist <- 2
  lab <- "MEM" 
})

ame <- aggregate(x ~ t + dist, data = dfm, FUN=mean)
colnames(ame)[colnames(ame)=="x"] <- "ame" 
ame$xco <- ame$ame-.095 
ame$yco <- 0.9
ame$lab <- "AME"

dfg <- rbind(df, dfm)
mean_x <- rbind(mean_x, mem)
mean_x$xco <- mean_x$mean_x + 0.095

tlab <- paste0("Scenario ", 1:4)
names(tlab) <- 1:4

dlab <- c("Distribution of X","Distribution of the Marginal Effects of X")
names(dlab) <- 1:2

pic <- ggplot(dfg) + 
  geom_histogram(aes(x=x, y=..ndensity..), color="white", fill="lightgray") +
  geom_vline(aes(xintercept=mean_x), data=mean_x, color="#FC717F") +
  geom_text(aes(x=xco, y=yco, label=lab), data=mean_x, color="#FC717F", size=3) + 
  geom_vline(aes(xintercept=ame), data=ame, color="#529EFF", linetype="dashed") + 
  geom_text(aes(x=xco, y=yco, label=lab), data=ame, color="#529EFF", size=3) + 
  geom_line(mapping=aes(x=x,y=y), data=dfx, size=0.6) +
  labs(y=element_blank(), x=element_blank()) +
  facet_grid(t~dist, labeller=labeller(t=tlab, dist=dlab), scales="free_x")
ggsave("Figure_2.eps", pic, width=8, height=11)

Sys.time()-start.time

