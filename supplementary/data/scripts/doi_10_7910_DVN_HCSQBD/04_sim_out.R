rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

sim_out_start <- Sys.time()

# figure 1

load("sim_v4b.RData")

dimnames(out) <- list(stats = c("Bias", "RMSE", "coverage", "correlation"),
                      Method = c("simave", "PCA", "binIRT", "grm", "hgrm"),
                      nitems = c("5", "10", "20", "40", "80")) 

out2b <- as.data.frame(out) %>%
  mutate(stats = rownames(.)) %>%
  gather(measure, value, - stats) %>%
  separate(measure, c("Method", "nitems")) %>%
  mutate(nitems = as.numeric(nitems),
         value = ifelse(stats=="RMSE", sqrt(value), value),
         stats = factor(stats, levels = c("Bias", "RMSE", "coverage", "correlation"),
                        labels = c(expression(paste("Bias: E(", hat(gamma[1]), " - ", gamma[1], ")")),
                                   expression(paste("RMSE: ", sqrt(paste("E(", hat(gamma[1]), " - ", gamma[1], ")"^2)))),
                                   expression(paste("Coverage of 95", "%", " CI")),
                                   expression(paste("Average Cor(", hat(theta[i]), " , ", theta[i], ")")))),
         Method = factor(Method, levels = c("simave", "PCA", "binIRT", "grm", "hgrm"),
                         labels = c("Simple Average + Regression", "PCA + Regression",
                                    "Binary IRT + Regression",
                                    "Graded Response Model + Regression",
                                    "Hierarchical Graded Response Model")))

load("sim_v4a.RData")

dimnames(out) <- list(stats = c("Bias", "RMSE", "coverage", "correlation"),
                      Method = c("simave", "PCA", "binIRT", "grm", "hgrm"),
                      nitems = c("5", "10", "20", "40", "80")) 

out2a <- as.data.frame(out) %>%
  mutate(stats = rownames(.)) %>%
  gather(measure, value, - stats) %>%
  separate(measure, c("Method", "nitems")) %>%
  mutate(nitems = as.numeric(nitems),
         value = ifelse(stats=="RMSE", sqrt(value), value),
         stats = factor(stats, levels = c("Bias", "RMSE", "coverage", "correlation"),
                        labels = c(expression(paste("Bias: E(", hat(gamma[1]), " - ", gamma[1], ")")),
                                   expression(paste("RMSE: ", sqrt(paste("E(", hat(gamma[1]), " - ", gamma[1], ")"^2)))),
                                   expression(paste("Coverage of 95", "%", " CI")),
                                   expression(paste("Average Cor(", hat(theta[i]), " , ", theta[i], ")")))),
         Method = factor(Method, levels = c("simave", "PCA", "binIRT", "grm", "hgrm"),
                         labels = c("Simple Average + Regression", "PCA + Regression",
                                    "Binary IRT + Regression",
                                    "Graded Response Model + Regression",
                                    "Hierarchical Graded Response Model")))

out2 <- rbind(mutate(out2a, spec = "normal"), mutate(out2b, spec = "uniform")) %>%
  mutate(spec = factor(spec, labels = c(expression(paste("Normal ", theta[i])), expression(paste("Uniform ", theta[i], " (A Misspecified Model)")))))

out2_add <- data.frame(stats = "paste(\"Coverage of 95\", \"%\", \" CI\")", Method = "hgrm", nitems = 20, value = 1.1)

colors <- rep("grey40", 6)

ggplot(out2, aes(x = nitems, y = value)) +
  geom_point(aes(col = Method, shape = Method), size =3) +
  geom_line(aes(col = Method, linetype = Method), size = 1) +
  geom_blank(data = out2_add) +
  scale_x_log10(breaks = c(5, 10, 20, 40, 80)) +
  scale_color_manual(values = colors) +
  facet_grid(stats ~ spec, scales = "free_y", switch = "y", labeller = label_parsed) +
  labs(x = "Number of Items", y = "") +
  theme_minimal(base_size = 18) +
  guides(col = guide_legend(nrow = 3, byrow = TRUE, title = ""),
         shape = guide_legend(nrow = 3, byrow = TRUE, title = ""),
         linetype = guide_legend(nrow = 3, byrow = TRUE, title = "")) +
  theme(legend.position = "bottom")

ggsave("figure1.tiff", width = 9, height = 12)

# figure A1

load("sim_perf_out.RData")

dimnames(out) <- list(`Sample Size` = c("500", "2500", "5000", "7500", "10000"),
                      `# items` = c("5", "10", "20", "40"),
                      `Method` = c("hltm", "hgrm", "MCMC"))

out2 <- as.data.frame(out) %>%
  mutate(`Sample Size` = rownames(.)) %>%
  gather(measure, value, -`Sample Size`) %>%
  separate(measure, c("# items", "Method")) %>%
  mutate(`Sample Size` = as.numeric(`Sample Size`),
         `# items` = paste("J =", `# items`),
         `# items` = factor(`# items`, levels = c("J = 5", "J = 10", "J = 20", "J = 40")),
         Method = factor(Method, levels = c("MCMC", "hltm", "hgrm"),
                         labels = c("MCMC, Dichotomized Data (MCMCpack::MCMCirtHier1d)",
                                    "MMLE-EM, Dichotomized Data (hIRT::hltm)",
                                    "MMLE-EM, Ordinal Data (hIRT::hgrm)")))

ggplot(out2, aes(x = `Sample Size`, y = value/60)) +
  geom_point(aes(col = Method, shape = Method), size = 3) +
  geom_line(aes(col = Method, linetype = Method), size = 1) +
  scale_color_manual(values = colors) +
  facet_wrap(~`# items`) +
  scale_x_continuous(breaks = c(500, 2500, 5000, 7500, 10000)) +
  ylab("Computation Time (minutes)")+
  theme_minimal(base_size = 18)+
  guides(col = guide_legend(nrow = 3, byrow = TRUE, title = ""),
         shape = guide_legend(nrow = 3, byrow = TRUE, title = ""),
         linetype = guide_legend(nrow = 3, byrow = TRUE, title = "")) +
  theme(legend.position = "bottom")

sim_out_end <- Sys.time()

sim_out_time <- sim_out_end - sim_out_start

ggsave("figureS1.tiff", width = 9, height = 7.5)
