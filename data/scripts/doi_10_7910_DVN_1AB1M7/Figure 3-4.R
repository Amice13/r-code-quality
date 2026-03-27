library(tidyverse)
library(metafor)
library(spsR)
library(patchwork)
library(xtable)
library(GGally)

load('../Output/naumann_sps.rdata')

#####################################
# Figure 3                          #
#####################################

sps_plot1 <- function(out, title = NULL, columns = NULL){

    N <- length(out$internal$ss)
    Xp <- as.data.frame(out$internal$X)

    if(is.null(columns) == TRUE){
      columns <- colnames(Xp)
    }else{
      Xp <- Xp[, columns]
    }

    col_use <- rep("Not", N)
    col_use[out$internal$ss == 1] <- "Selected"
    Xp$summary_var <- col_use
    N_s <- sum(out$internal$ss)
    N_r <- N - N_s
    colname_use <- colnames(Xp)
    colname_use[colname_use == "summary_var"] <- "Summary"

    p <- ggpairs(Xp, aes(color = Xp$summary_var, shape = Xp$summary_var),
                 columnLabels = NULL,   
                 upper = "blank",
                 progress = FALSE,
                 lower = list(combo = wrap("facethist", binwidth = 0.2),
                              continuous = wrap(ggally_points, size = 2)),
                 axisLabels = c("show")) +  
      scale_shape_manual(values = c(16, 17)) +
      theme(panel.background = element_rect(fill = "white", colour = NA),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            strip.background = element_rect(fill = "grey85", colour = "grey20"),
            axis.text = element_text(color = 'black'),
            axis.title = element_text(color = 'black'),
            legend.key = element_rect(fill = "white", colour = NA)) +
      ggtitle(label = paste0(title))

      for(i in 1:p$nrow) {
        for(j in 1:p$ncol){
          p[i,j] <- p[i,j] +
            scale_fill_manual(values = c(adjustcolor("black", 1),
                                         adjustcolor("red", 1))) +
            scale_color_manual(values = c("black", "red"))
        }
      }

      d_t <- data.frame(x = 0.5, y = 0.5, colname = paste0("N = ",N,"\nSelect = ", N_s))
      p[ncol(Xp), ncol(Xp)] <- NULL
      for (i in 1:ncol(Xp)) {
        p[ncol(Xp),i] <- p[ncol(Xp),i] + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
      }

      ncl <- ncol(Xp) - 1
      d_t_l <- list()
      for(i in 1:ncl){
        d_t_l[[i]] <- data.frame(x = 0.5, y = 0.5, colname = colnames(Xp)[i])
        p[i, i] <- ggplot(data = d_t_l[[i]], aes(x, y, label= colname)) +
          geom_text(size = 4, lineheight = 0.85) +
          theme(axis.text.x  = element_blank(),
                axis.ticks.x = element_blank(),
                panel.grid   = element_blank(),
                axis.title = element_text(color = 'black'))
      }

      for (i in 1:ncol(Xp)) {
        p[1,i] <- p[1,i] + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
      }
      suppressWarnings(print(p))
}


pdf("../Figures/Figure 3.pdf", height = 8.5, width = 8.5)
colnames(out$internal$X) <- c('GDP', 'Immigration', 'Unemploy-\nment', 'Fiscal\nexposure',
                              'Immigration\nsupport', 'Age', 'Education')
sps_plot1(out)
dev.off()

#####################################
# Figure 4                          #
#####################################

# Site-Specific ATEs
selected_mat  <- est_mat[out$selected_sites, ]
selected_mat_ord  <- selected_mat[order(selected_mat[,1]), ]
obs_estimate_ord <- selected_mat_ord[, 1]
obs_se_ord <- selected_mat_ord[, 2]
country_name <- rownames(selected_mat_ord)
country_name[2] <- "Czechia"

df <- data.frame(Country = factor(country_name, levels = unique(country_name)),
                 Estimates = obs_estimate_ord,
                 SE = obs_se_ord)
ylim_use <- range(c(df[, "Estimates"] - 1.96*df[, "SE"],
                    df[, "Estimates"] + 1.96*df[, "SE"]))

p_selected <- ggplot(data=df, aes(x = Country, y = Estimates)) +
  geom_point(pch = 19, size = 3.25) +
  geom_errorbar(aes(ymin = Estimates - 1.96 * SE, ymax = Estimates + 1.96 * SE),
                position = position_dodge(width = 0.6), width = 0.2, linewidth = 1.25) +
  ylim(0, 0.5) +
  labs(x = "", y = "Estimates", title = '(a) Site-specific ATEs') +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'Times',
                                  margin = margin(t = 0, r = 0, b = 11, l = 0, unit = "pt"))) +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 12.5, color = 'black', margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"))) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
  theme(panel.background = element_rect(fill = 'white', colour = "black"),
        panel.grid = element_blank())

# Average Effects
est_rss <- rma(yi = est_mat[, 1],
               vi = est_mat[, 2]^2, method = "REML")
est_overall <- c(sps_est$average_site_ATE[1], est_rss$beta)
se_overall  <- c(sps_est$average_site_ATE[2], est_rss$se)

# Subgroup Effects
sub <- as.numeric(X_use[, "Fiscal Exposure"] > 0)
est_rss_1 <- rma(yi = est_mat[sub == 1, 1],
                 vi = est_mat[sub == 1, 2]^2, method = "REML")

est_1 <- c(sps_est_sub$subgroup_average_site_ATE["subgroup = 1", 1], est_rss_1$beta)
se_1  <- c(sps_est_sub$subgroup_average_site_ATE["subgroup = 1", 2], est_rss_1$se)

est_rss_0 <- rma(yi = est_mat[sub == 0, 1],
                 vi = est_mat[sub == 0, 2]^2, method = "REML")

est_0 <- c(sps_est_sub$subgroup_average_site_ATE["subgroup = 0", 1], est_rss_0$beta)
se_0  <- c(sps_est_sub$subgroup_average_site_ATE["subgroup = 0", 2], est_rss_0$se)


## Average-Site ATE
method_use <- c("Estimates\nby SPS", "Experimental\nbenchmark")
df_avg <- data.frame(Method = factor(method_use, levels = unique(method_use)),
                     Estimates = est_overall,
                     SE = se_overall)

p_avg <- ggplot(data=df_avg, aes(x = Method, y = Estimates)) +
  geom_point(pch = c(19, 15), size = 3.25, colour = c("black", "red")) +
  geom_errorbar(aes(ymin = Estimates - 1.96 * SE, ymax = Estimates + 1.96 * SE),
                position = position_dodge(width = 0.6), width = 0.1, linewidth = 1.25,
                colour = c("black", "red")) +
  ylim(0, 0.5) +
  labs(x = "", y = "Estimates", title = "(b) Average-site ATEs") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = 'Times',
                                  margin = margin(t = 0, r = 0, b = 11, l = 0, unit = "pt")),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 12.5, color = 'black', margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"))) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
  geom_text(x = 1, y = 0.49, label = "Estimates", size = 4) +
  geom_text(x = 1, y = 0.44, label = "by SPS", size = 4) +
  geom_text(x = 2, y = 0.49, label = "Experimental", size = 4, colour = "red") +
  geom_text(x = 2, y = 0.44, label = "benchmark", size = 4, colour = "red") +
  theme(panel.background = element_rect(fill = 'white', colour = "black"),
        panel.grid = element_blank())

# Subgroup Average-Site ATEs
use_index <- c(0.75, 1.25, 2.25, 2.75)

df_sub <- data.frame(Index = use_index,
                     Estimates = c(est_0, est_1),
                     SE = c(se_0, se_1))

p_sub <- ggplot(data=df_sub, aes(x = Index, y = Estimates)) +
  geom_point(pch = c(19, 15, 19, 15), size = 3.25, colour = c("black", "red", "black", "red")) +
  geom_errorbar(aes(ymin = Estimates - 1.96 * SE, ymax = Estimates + 1.96 * SE),
                position = position_dodge(width = 0.6), width = 0.1, linewidth = 1.25,
                colour = c("black", "red", "black", "red")) +
  ylim(0, 0.5) +
  labs(x = "", y = "Estimates", title = "(c) Subgroup average-site ATEs") +
  theme(plot.title = element_text(size = 14, hjust = 0.5, family = "Times",
                                  margin = margin(t = 0, r = 0, b = 11, l = 0, unit = "pt"))) +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 12.5, color = 'black', margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"))) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black") +
  theme(panel.background = element_rect(fill = 'white', colour = "black"),
        panel.grid = element_blank()) +
  scale_x_continuous(labels= c("Low \n fiscal exposure", "High \n fiscal exposure"), breaks = c(1, 2.5), limits = c(0.5, 3))

pdf("../Figures/Figure 4.pdf", height = 6, width = 8.5)
p_selected / (p_avg + p_sub) #+ plot_layout(widths = c(2.6,2,2))
dev.off()


#####################################
# In-Text Analyses                  #
#####################################


# SPS selected sites for multi-country experiment on immigration:
out$selected_sites

# Range of site-specific ATE estimates 
round(min(df$Estimates)*100,1)
round(max(df$Estimates)*100,1)

# Estimated average-site ATE using SPS estimator (and 95% confidence interval)
round(df_avg[1,2]*100,1)
round((df_avg$Estimates[1] - 1.96 * df_avg$SE[1])*100,1)
round((df_avg$Estimates[1] + 1.96 * df_avg$SE[1])*100,1)

# Estimate from the empirical benchmark (and 95% confidence interval)
round(df_avg[2,2]*100,1)
round((df_avg$Estimates[2] - 1.96 * df_avg$SE[2])*100,1)
round((df_avg$Estimates[2] + 1.96 * df_avg$SE[2])*100,1)

# P-value from cross validation
df$Country <- ifelse(df$Country == "Czechia", "Czech Republic", as.character(df$Country))
rownames(df) <- df$Country
df_cv <- df[,2:3]
df_cv <- df_cv[out$selected_sites,]
cv <- sps_cv(out, estimates_selected = df_cv, seed = 1234)
cv$p_value

