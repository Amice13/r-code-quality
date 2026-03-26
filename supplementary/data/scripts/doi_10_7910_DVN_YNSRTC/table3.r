### Generate counterfactual equilibrium predictions about amount of violence in
### each municipality

date()
library("tidyverse")
library("foreach")
library("Formula")
library("haven")
library("rootSolve")
sessionInfo()

data_vict <- read_stata("violence_t_export.dta")
data_phat <- read_csv("phats.csv")
data_boot <- read_stata("boot_res_region_v.dta")

## Clean data and merge with original p'hats
data_vict <- data_vict %>%
    select(muni_code, victim_farc, victim_paras, nbi_t, royalties_t, coca_t,
           share_left, lpobl_tot_t, time, army_dist, gini_i, dmr, evlp, gcaribe,
           gpacifica, gorinoquia, gamazonia, farc_dist, paras_dist) %>%
    na.omit() %>%
    left_join(data_phat, by = c("time", "muni_code"))

## Extract model components in same order as in bootstrap file
f <- as.Formula(victim_farc + victim_paras ~
                    nbi_t + royalties_t + coca_t + share_left + lpobl_tot_t + army_dist + dmr + gini_i + time + evlp + gcaribe + gpacifica + gorinoquia + gamazonia |
                    farc_dist - 1 |
                    paras_dist - 1)
mf <- model.frame(formula = f, data = data_vict, na.action = na.fail)
y <- model.part(f, data = mf, lhs = 1)
y_farc <- y[, 1]
y_para <- y[, 2]
X_all <- model.matrix(f, data = mf, rhs = 1)
X_farc <- cbind(X_all, model.matrix(f, data = mf, rhs = 2))
X_para <- cbind(X_all, model.matrix(f, data = mf, rhs = 3))

## Incorporate conditional choice probabilities
X_farc <- cbind(X_farc, data_vict$pP)
colnames(X_farc)[ncol(X_farc)] <- "p_para"
colnames(X_farc) <- paste0("u_farc:", colnames(X_farc))
X_para <- cbind(X_para, data_vict$pF)
colnames(X_para)[ncol(X_para)] <- "p_farc"
colnames(X_para) <- paste0("u_para:", colnames(X_para))

## Put intercepts last to be consistent with bootstrap coefficients
X_farc <- X_farc[, c(2:ncol(X_farc), 1)]
X_para <- X_para[, c(2:ncol(X_para), 1)]


## Solve for an equilibrium in a given municipality under the specified
## parameters (assuming both complementarities non-zero, otherwise we can use
## easier methods)
Psi <- function(p, b_farc, b_para, x_farc, x_para, idx_farc, idx_para) {
    p_farc <- p[1]
    p_para <- p[2]
    x_farc[idx_farc] <- p_para
    x_para[idx_para] <- p_farc
    psi_1 <- plogis(sum(x_farc * b_farc)) - p_farc
    psi_2 <- plogis(sum(x_para * b_para)) - p_para
    c(psi_1, psi_2)
}

d_Psi <- function(p, b_farc, b_para, x_farc, x_para, idx_farc, idx_para) {
    p_farc <- p[1]
    p_para <- p[2]
    x_farc[idx_farc] <- p_para
    x_para[idx_para] <- p_farc
    pp_farc <- plogis(sum(x_farc * b_farc))
    pp_para <- plogis(sum(x_para * b_para))
    rbind(c(-1, pp_farc * (1 - pp_farc) * b_farc[idx_farc]),
          c(pp_para * (1 - pp_para) * b_para[idx_para], -1))
}

solve_Psi <- function(b_farc, b_para, x_farc, x_para, idx_farc, idx_para) {
    ## Generate initial guess from best response to first stage
    p <- plogis(c(sum(x_farc * b_farc), sum(x_para * b_para)))

    ## Newton method
    ans <- multiroot(f = Psi,
                     start = p,
                     jacfunc = d_Psi,
                     b_farc = b_farc,
                     b_para = b_para,
                     x_farc = x_farc,
                     x_para = x_para,
                     idx_farc = idx_farc,
                     idx_para = idx_para)
    ans$root
}

solve_all <- function(b, X_farc, X_para, idx, avg = TRUE) {
    ## Extract individual coefficient vectors and indices
    b_farc <- head(b, ncol(X_farc))
    b_para <- tail(b, -ncol(X_farc))
    idx_farc <- idx[1]
    idx_para <- idx[2] - ncol(X_farc)

    ## Under estimated model parameters
    pp_est <- foreach (i = seq(nrow(X_farc)), .combine = "rbind") %do% {
        solve_Psi(b_farc = b_farc,
                  b_para = b_para,
                  x_farc = X_farc[i, ],
                  x_para = X_para[i, ],
                  idx_farc = idx_farc,
                  idx_para = idx_para)
    }

    ## If FARC's b.r. doesn't depend on paramilitary entry
    b_farc_nof <- b_farc
    b_farc_nof[idx_farc] <- 0
    pp_farc_nof <- plogis(drop(X_farc %*% b_farc_nof))
    X_para_nof <- X_para
    X_para_nof[, idx_para] <- pp_farc_nof
    pp_para_nof <- plogis(drop(X_para_nof %*% b_para))
    pp_nof <- cbind(pp_farc_nof, pp_para_nof)

    ## If paramilitary's b.r. doesn't depend on FARC entry
    b_para_nop <- b_para
    b_para_nop[idx_para] <- 0
    pp_para_nop <- plogis(drop(X_para %*% b_para_nop))
    X_farc_nop <- X_farc
    X_farc_nop[, idx_farc] <- pp_para_nop
    pp_farc_nop <- plogis(drop(X_farc_nop %*% b_farc))
    pp_nop <- cbind(pp_farc_nop, pp_para_nop)

    ## If no cross-dependencies
    pp_nob <- cbind(pp_farc_nof, pp_para_nop)

    ## Construct output
    if (avg) {
        ans <- c(colMeans(pp_est),
                 colMeans(pp_nop),
                 colMeans(pp_nof),
                 colMeans(pp_nob))

        ## Name scheme: [whose predicted probabilities]_[whose strategic
        ## sensitivities are included]
        names(ans) <- c("farc_both",
                        "para_both",
                        "farc_farc",
                        "para_farc",
                        "farc_para",
                        "para_para",
                        "farc_none",
                        "para_none")
    } else {
        ans <- list(pp_both = pp_est,
                    pp_farc = pp_nop,
                    pp_para = pp_nof,
                    pp_none = pp_nob)
    }

    ans
}

solve_boot <- function(B, X_farc, X_para, idx) {
    foreach (i = seq(nrow(B)), .combine = "rbind") %do% {
        solve_all(b = B[i, ],
                  X_farc = X_farc,
                  X_para = X_para,
                  idx = idx,
                  avg = TRUE)
    }
}


## Obtain main model estimate and bootstrap distribution
B <- as.matrix(data_boot[1:501, 1:34])
b_main <- B[1, ]
B_boot <- B[-1, ]
idx <- c(colnames(X_farc), colnames(X_para))
idx <- c(which(idx == "u_farc:p_para"), which(idx == "u_para:p_farc"))
est_main <- solve_all(b = b_main, X_farc = X_farc, X_para = X_para, idx = idx)
est_boot <- solve_boot(B = B_boot, X_farc = X_farc, X_para = X_para, idx = idx)

## Calculate bootstrap confidence intervals
se_boot <- apply(est_boot, 2, sd)
ci_boot <- cbind(est_main - 1.96 * se_boot, est_main + 1.96 * se_boot)

## Make table of counterfactual predictions
tab <- tibble(term = names(est_main),
              est = est_main,
              lwr = ci_boot[, 1],
              upr = ci_boot[, 2]) %>%
  separate(term, into = c("group", "incl"))

entry <- function(group, incl) {
    dat <- filter(tab, group == !! group, incl == !! incl)
    stopifnot(nrow(dat) == 1)
    est <- sprintf("%.2f", dat$est)
    lwr <- sprintf("%.2f", dat$lwr)
    upr <- sprintf("%.2f", dat$upr)
    paste0(est, " [", lwr, ", ", upr, "]")
}

ans <- c(
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "& \\multicolumn{2}{c}{Average Pr(Victimization)} \\\\",
    "\\cmidrule{2-3}",
    "& FARC & AUC \\\\",
    "\\midrule",
    paste("Baseline &", entry("farc", "both"), "&", entry("para", "both"), "\\\\"),
    paste("Without strategic interdependence &", entry("farc", "none"), "&", entry("para", "none"), "\\\\"),
    "\\bottomrule",
    "\\end{tabular}"
)

writeLines(ans)

date()
