library(MASS)
library(spsR)
library(GGally)

L <- 2
set.seed(2000)
X <-  mvrnorm(n = 20, mu = rep(0, L), Sigma = diag(L))
X_use <- apply(X, 2, scale)
colnames(X_use) <- c("X1", "X2")

st_11 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X1", "smaller than or equal to", -1.5))
st_12 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X1", "larger than or equal to", 1.5))
st_13 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X1", "between", c(-0.5, 0.5)))
st_21 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X2", "smaller than or equal to", -1.5))
st_22 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X2", "larger than or equal to", 1.5))
st_23 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X2", "between", c(-0.5, 0.5)))
st_11 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X1", "smaller than or equal to", -1.25))
st_12 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X1", "larger than or equal to", 1.25))
st_21 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X2", "smaller than or equal to", -1.25))
st_22 <- stratify_sps(X = X_use,
                      num_site = list("at least", 1),
                      condition = list("X2", "larger than or equal to", 1.25))

out_2 <- sps(X = X_use, N_s = 5, stratify = list(st_11, st_12, st_13,
                                                 st_21, st_22, st_23))

#####################################
# Figure 2                          #
#####################################

sps_plot2 <- function(out, leg = 'bottom'){
    N <- length(out$internal$ss)
    Xp <- as.data.frame(out$internal$X)
    columns <- colnames(Xp)
    
    col_use <- rep("Not Selected", N)
    col_use[out$internal$ss == 1] <- "Selected"
    Xp$summary_var <- col_use
    N_s <- sum(out$internal$ss)
    N_r <- N - N_s
    colname_use <- colnames(Xp)
    colname_use[colname_use == "summary_var"] <- "Summary"
    p <- ggpairs(Xp, aes(color = Xp$summary_var, shape = Xp$summary_var), 
                 columnLabels = NULL, upper = "blank", progress = FALSE, 
                 lower = list(combo = wrap("facethist", binwidth = 0.2), 
                              continuous = wrap(ggally_points, size = 2)), 
                 axisLabels = c("show"),
                 legend = c(2,1)) + 
      scale_shape_manual(values = c(16, 17)) + 
      theme(panel.background = element_rect(fill = "white", colour = NA), 
            panel.border = element_rect(fill = NA, colour = "grey20"), 
            strip.background = element_rect(fill = "grey85", colour = "grey20"), 
            axis.text = element_text(color = "black"), 
            axis.title = element_text(color = "black"), 
            legend.key = element_rect(fill = "white", colour = NA),
            legend.title = element_blank(),
            legend.position = leg) 
      for (i in 1:p$nrow) {
        for (j in 1:p$ncol) {
          p[i, j] <- p[i, j] + scale_fill_manual(values = c(adjustcolor("black", 1), adjustcolor("red", 1))) + 
            scale_color_manual(values = c("black", "red"))
        }
      }
      p[ncol(Xp), ncol(Xp)] <- NULL
      for (i in 1:ncol(Xp)) {
        p[ncol(Xp), i] <- p[ncol(Xp), i] + theme(axis.text.y = element_blank(), 
                                                 axis.ticks.y = element_blank())
      }
      ncl <- ncol(Xp) - 1
      d_t_l <- list()
      for (i in 1:ncl) {
        d_t_l[[i]] <- data.frame(x = 0.5, y = 0.5, colname = colnames(Xp)[i])
        p[i, i] <- ggplot(data = d_t_l[[i]], aes(x, y, label = colname)) + geom_text(size = 3) + 
          theme(axis.text.x = element_blank(), 
                axis.ticks.x = element_blank(), panel.grid = element_blank(), 
                axis.title = element_text(color = "black"))
      }
      for (i in 1:ncol(Xp)) {
        p[1, i] <- p[1, i] + theme(axis.text.y = element_blank(), 
                                   axis.ticks.y = element_blank())
      }
      suppressWarnings(return(p))
}

pdf("../Figures/Figure 2 (a).pdf", height = 4.5, width = 4)
f2a <- sps_plot2(out_2)
print(f2a)
dev.off()

L <- 4
set.seed(2000)
X2 <-  mvrnorm(n = 20, mu = rep(0, L), Sigma = diag(L))
X2_use <- apply(X2, 2, scale)
colnames(X2_use) <- c("X1", "X2", "X3", "X4")

st_large <- st_small <- st_mid <- list()
st_equal_l <- st_equal_r <- list()
for(v in 1:4){
  st_large[[v]] <- stratify_sps(X = X2_use,
                                num_site = list("at least", 1),
                                condition = list(colnames(X2_use)[v], "larger than or equal to", 1))
  st_small[[v]] <- stratify_sps(X = X2_use,
                                num_site = list("at least", 1),
                                condition = list(colnames(X2_use)[v], "smaller than or equal to", -1))
  st_mid[[v]] <- stratify_sps(X = X2_use,
                              num_site = list("at least", 1),
                              condition = list(colnames(X2_use)[v], "between", c(-0.5, 0.5)))

  st_equal_l[[v]] <- stratify_sps(X = X2_use,
                                  num_site = list("at least", 2),
                                  condition = list(colnames(X2_use)[v], "larger than or equal to", 0))

  st_equal_r[[v]] <- stratify_sps(X = X2_use,
                                  num_site = list("at least", 2),
                                  condition = list(colnames(X2_use)[v], "smaller than or equal to", 0))
}
st_combined <- c(st_large, st_small, st_mid, st_equal_l, st_equal_r)

out_4 <- sps(X = X2_use, N_s = 5, seed = 1234, stratify = st_combined)

pdf("../Figures/Figure 2 (b).pdf", height = 4.8, width = 5)
sps_plot2(out_4, leg = 'none')
dev.off()


