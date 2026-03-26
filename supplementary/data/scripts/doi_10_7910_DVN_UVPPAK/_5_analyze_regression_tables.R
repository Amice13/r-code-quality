library(brew)
library(cowplot)
library(ggpattern)
library(ggplot2)
library(grid)
library(gridExtra)

path_input=".//"
path_aff_changes="./affiliation_changes.csv"
path_figures="./output/figures/"
path_tables="./output/tables/"
path_statistics="./output/numbers/"

# Definitions
n_post <- 4
n_pre <- 4

# Functions
strip_letters <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}
extract_estimates_and_ses <- function(data) {
  did_estimates <- as.vector(sapply(data[2, -1], FUN = strip_letters))
  did_se <- as.vector(sapply(data[3, -1], FUN = strip_letters))
  return(list(estimates = did_estimates, ses = did_se))
}
compute_elasticities <- function(means, estimates, ses) {
  did_elast <- (sinh(asinh(means) + estimates) / means - 1) * 100
  did_elast_se <- (ses * cosh(asinh(means) + estimates) / means) * 100
  did_elast_t <- did_elast / did_elast_se
  did_elast_p <- 2 * pnorm(-abs(did_elast_t))
  
  list(Est = did_elast, Std = did_elast_se, T = did_elast_t, P = did_elast_p)
}
format_p_value <- function(x) {
  if (x >= 0.001) {
    paste("p =", format(round(x, 3), nsmall = 3))
  } else {
    "p < 0.001"
  }
}
p_to_stars <- function(p, thres = c(0.1, 0.05, 0.01)) {
  n_stars <- sum(p < thres)
  return(strrep("*", n_stars))
}

# Regression table template
regression_table_template <- "
\\begin{tabular}{p{0.3\\textwidth}<%= paste(rep('c', n_columns), collapse = '') %>}
\\toprule
<%= header_string %> & <%= paste(column_names, collapse = ' & ') %>\\\\
\\midrule
treatment $\\times$ post (\\%) & <%= paste(estimate_values, collapse = ' & ') %>\\\\
& (<%= paste(stderr_values, collapse = ') & (') %>)\\\\
\\midrule
Scientist Fixed Effects & <%= paste(rep('\\\\checkmark', n_columns), collapse = ' & ') %>\\\\
Publication Year Fixed & <%= paste(rep('\\\\checkmark', n_columns), collapse = ' & ') %>\\\\
Academic Age Fixed Effects & <%= paste(rep('\\\\checkmark', n_columns), collapse = ' & ') %>\\\\
&&\\\\
Observations & <%= paste(observations, collapse = ' & ') %>\\\\
Scientists & <%= paste(scientists, collapse = ' & ') %>\\\\
Incidents & <%= paste(incidents, collapse = ' & ') %>\\\\
\\bottomrule
\\end{tabular}
"
template_file <- tempfile()
writeLines(regression_table_template, template_file)
generate_table <- function(column_names, estimates, std_errors, obs, scientists, incs, header_string) {
  n_columns <- length(column_names)

  estimate_values <- sprintf("%.3f", estimates)
  stderr_values <- sprintf("%.3f", std_errors)
  observations <- prettyNum(obs, big.mark = ",", scientific = FALSE)
  scientists <- prettyNum(scientists, big.mark = ",", scientific = FALSE)
  incidents <- prettyNum(incs, big.mark = ",", scientific = FALSE)

  output <- capture.output(brew(template_file))
  return(paste(output, collapse = "\n"))
}

#############################
# 1. Simple Event Study Graph
#############################

data <- read.csv(file.path(path_input, "did_leads_and_lags_pubs.csv"))
stats <- read.csv(file.path(path_input, "did_leads_and_lags_pubs.txt"))

# Compute semi-elasticities

coef_indices <- c(15:12*2,1:11*2)
did_estimates <- as.vector(sapply(data[coef_indices, 2], FUN=strip_letters))[1:(n_pre + n_post + 1)]
se_indices <- coef_indices + 1
did_se <- as.vector(sapply(data[se_indices, 2], FUN=strip_letters))[1:(n_pre + n_post + 1)]

elasticities <- compute_elasticities(stats$pub_mean, did_estimates, did_se)

# Split into pre and post periods
ll_did_elast_pre <- c(elasticities$Est[1:n_pre], rep(NA, n_post + 1))
ll_did_elast_post <- c(rep(NA, n_pre), elasticities$Est[(n_pre + 1):(n_pre + n_post + 1)])

ll_did_elast_se_pre <- c(elasticities$Std[1:n_pre], rep(NA, n_post + 1))
ll_did_elast_se_post <- c(rep(NA, n_pre), elasticities$Std[(n_pre + 1):(n_pre + n_post + 1)])

# Combine data into a data frame
ll_periods <- seq(from = -n_pre, to = n_post, by = 1)
years_labels <- as.character(ll_periods)
data <- data.frame(
  years_labels = factor(years_labels, levels = years_labels[order(seq_along(ll_periods))]),
  ll_did_elast_pre,
  ll_did_elast_se_pre,
  ll_did_elast_post,
  ll_did_elast_se_post
)

# Create plot (in color)
event_pubs <- ggplot(data, aes(x = years_labels, group = 1)) +
  geom_line(aes(y = ll_did_elast_pre), color = "#009E73") +
  geom_line(aes(y = ll_did_elast_post), color = "#009E73") +
  geom_point(aes(y = ll_did_elast_pre), color = "#009E73") +
  geom_point(aes(y = ll_did_elast_post), color = "#009E73") +
  geom_ribbon(
    aes(y = ll_did_elast_pre, ymin = ll_did_elast_pre - 1.96 * ll_did_elast_se_pre, ymax = ll_did_elast_pre + 1.96 * ll_did_elast_se_pre),
    alpha = 0.5, colour = "#009E73", linetype = "dashed", fill = "#009E73"
  ) +
  geom_ribbon(
    aes(y = ll_did_elast_post, ymin = ll_did_elast_post - 1.96 * ll_did_elast_se_post, ymax = ll_did_elast_post + 1.96 * ll_did_elast_se_post),
    alpha = 0.5, colour = "#009E73", linetype = "dashed", fill = "#009E73"
  ) +
  scale_y_continuous(
    name = "Relative publication difference (%)",
    breaks = seq(from = -60, to = 40, by = 10)
  ) +
  scale_x_discrete(
    name = "Year relative to Outcome Year",
    breaks = seq(from = -5, to = n_post, by = 1)
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14)
  ) +
  geom_hline(yintercept = 0)

# Save plot
pdf(file = file.path(path_figures, "event_scientists-pubs.pdf"), width = 10, height = 6)
print(event_pubs)
dev.off()

# Create plot (in black-white)
event_pubs <- ggplot(data, aes(x = years_labels, group = 1)) +
  geom_line(aes(y = ll_did_elast_pre), colour = "black", linetype = "dashed") +
  geom_line(aes(y = ll_did_elast_post), colour = "black") +
  geom_point(aes(y = ll_did_elast_pre), shape = 1) +
  geom_point(aes(y = ll_did_elast_post), shape = 16) +
  geom_ribbon(
    aes(y = ll_did_elast_pre, ymin = ll_did_elast_pre - 1.96 * ll_did_elast_se_pre, ymax = ll_did_elast_pre + 1.96 * ll_did_elast_se_pre),
    alpha = 0.5, colour = "black", linetype = "dotted", fill = "grey80"
  ) +
  geom_ribbon(
    aes(y = ll_did_elast_post, ymin = ll_did_elast_post - 1.96 * ll_did_elast_se_post, ymax = ll_did_elast_post + 1.96 * ll_did_elast_se_post),
    alpha = 0.5, colour = "black", linetype = "dotted", fill = "grey60"
  ) +
  scale_y_continuous(
    name = "Relative publication difference (%)",
    breaks = seq(from = -60, to = 40, by = 10)
  ) +
  scale_x_discrete(
    name = "Year relative to Outcome Year",
    breaks = seq(from = -5, to = n_post, by = 1)
  ) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14, face = "plain"),
    title = element_text(face="bold")
  ) +
  ggtitle("A")

#######################
# 2. Coefplot coauthors
#######################

data_pp <- read.csv(file.path(path_input, "did_pubs_pre_post.csv"))
results <- extract_estimates_and_ses(data_pp)
main <- compute_elasticities(stats$pub_mean, results$estimates, results$ses)

data_c <- read.csv(file.path(path_input, "did_coauthors.csv"))
results_c <- extract_estimates_and_ses(data_c)
stats_c <- read.csv(file.path(path_input, "did_coauthors.txt"), row.names = "spec")
coauthors <- compute_elasticities(stats_c$num_coauthors_mean, results_c$estimates, results_c$ses)

# Generate plot data
data_graph <- data.frame(coauthors)
labels <- factor(c("With female\ncoauthors", "With male\ncoauthors"),
                 levels = c("With female\ncoauthors", "With male\ncoauthors"))
data_graph$labels <- labels
data_graph <- data_graph[order(as.character(data_graph$labels), decreasing = TRUE), ]
data_graph$asterisk <- sapply(data_graph$P, p_to_stars)

# Make plot
coef_coauth <- ggplot(data_graph, aes(x = labels, y = Est)) +
  geom_point(shape = 21, size = 10, fill = "white") +
  geom_errorbar(
    aes(ymin = Est - 1.96 * Std, ymax = Est + 1.96 * Std),
    color = "black",
    linewidth = 2,
    width = 0.2) +
  scale_y_continuous(name = "Average effect on collaborations (%)", breaks = seq(from = -50, to = 5, by = 5)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_text(
    aes(x = labels, y = Est + 1.96 * Std + 1, label = asterisk),
    size = 5,
    vjust = -0.5
  ) +
  theme_bw() +
  theme(
    title = element_text(face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14, face = "plain"),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
  ) +
  ggtitle("B") +
  xlab(NULL)

########################
# 3. Affiliation changes
########################

data <- read.csv(path_aff_changes)

treated_dist <- colMeans(subset(data, treated == 1, select = -treated)) * 100
control_dist <- colMeans(subset(data, treated == 0, select = -treated)) * 100

labels <- c(
  'No Publication', 'Same University', 'Non-University Inst.',
  'Other University\n Lower rank', 'Other University\n Higher rank',
  'Other University\n Same/similar rank'
)

data_graph <- data.frame(
  value = c(rbind(treated_dist, control_dist)),
  condition = rep(labels, each = 2),
  Groups = rep(c("treated", "control"), 6)
)
data_graph$condition <- factor(data_graph$condition, levels = labels)
data_graph$Groups <- factor(data_graph$Groups, levels = c("control","treated"))

p_vals <- sapply(
  names(data)[1:(length(names(data)) - 1)],
  function(var) summary(lm(as.formula(paste(var, "~ treated")), data = data))$coef['treated', 4]
)

p_vals_char <- sapply(p_vals, format_p_value)

# Create barplots
i <- 1.5 #y distance to higher bar
j <- 0.2 #y distance to any bar

bar_change <- ggplot(data_graph, aes(x = condition, y = value, fill = Groups, pattern = Groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar_pattern(
    aes(pattern = ifelse(Groups == "control", "stripe", NA_character_)),
    stat = "identity",
    position = "dodge",
    pattern_density = 0.1,
    pattern_spacing = 0.02,
    pattern_fill = "grey90",
    pattern_color = "grey90",
    show.legend = FALSE,
  ) +
  scale_fill_grey(start = 0.8, end = 0.4, name = "") +
  scale_y_continuous(
    name = "Share of Researchers (%)",
    breaks = seq(0, 65, 5),
    limits = c(0, 65)
  ) +
  # Bar: No publications
  geom_segment(aes(x = 0.75, xend = 0.75, y = min(control_dist[1], treated_dist[1]) + j, yend = max(control_dist[1], treated_dist[1]) + i + j)) +
  geom_segment(aes(x = 0.75, xend = 1.25, y = max(control_dist[1], treated_dist[1]) + i + j, yend = max(control_dist[1], treated_dist[1]) + i + j)) +
  geom_segment(aes(x = 1.25, xend = 1.25, y = max(control_dist[1], treated_dist[1]) + i + j, yend = max(control_dist[1], treated_dist[1] + j))) +
  annotate("text", x = 1, y = max(control_dist[1], treated_dist[1]) + i + j + 2, size = 4, label = p_vals_char[1]) +
  # Bar: Same university
  geom_segment(aes(x = 1.75, xend = 1.75, y = max(control_dist[2], treated_dist[2]) + j, yend = max(control_dist[2], treated_dist[2]) + i + j)) +
  geom_segment(aes(x = 1.75, xend = 2.25, y = max(control_dist[2], treated_dist[2]) + i + j, yend = max(control_dist[2], treated_dist[2]) + i + j)) +
  geom_segment(aes(x = 2.25, xend = 2.25, y = max(control_dist[2], treated_dist[2]) + i + j, yend = min(control_dist[2], treated_dist[2] + j))) +
  annotate("text", x = 2, y = max(control_dist[2], treated_dist[2]) + i + j + 2, size = 4, label = p_vals_char[2]) +
  # Bar: Non-university change
  geom_segment(aes(x = 2.75, xend = 2.75, y = min(control_dist[3], treated_dist[3]) + j, yend = max(control_dist[3], treated_dist[3]) + i + j)) +
  geom_segment(aes(x = 2.75, xend = 3.25, y = max(control_dist[3], treated_dist[3]) + i + j, yend = max(control_dist[3], treated_dist[3]) + i + j)) +
  geom_segment(aes(x = 3.25, xend = 3.25, y = max(control_dist[3], treated_dist[3]) + i + j, yend = max(control_dist[3], treated_dist[3] + j))) +
  annotate("text", x = 3, y = max(control_dist[3], treated_dist[3]) + i + j + 2, size = 4, label = p_vals_char[3]) +
  # Bar: Lower rank change
  geom_segment(aes(x = 3.75, xend = 3.75, y = min(control_dist[4], treated_dist[4]) + j, yend = max(control_dist[4], treated_dist[4]) + i + j)) +
  geom_segment(aes(x = 3.75, xend = 4.25, y = max(control_dist[4], treated_dist[4]) + i + j, yend = max(control_dist[4], treated_dist[4]) + i + j)) +
  geom_segment(aes(x = 4.25, xend = 4.25, y = max(control_dist[4], treated_dist[4]) + i + j, yend = max(control_dist[4], treated_dist[4] + j))) +
  annotate("text", x = 4, y = max(control_dist[4], treated_dist[4]) + i + j + 2, size = 4, label = p_vals_char[4]) +
  # Bar: Higher rank change
  geom_segment(aes(x = 4.75, xend = 4.75, y = max(control_dist[5], treated_dist[5]) + j, yend = max(control_dist[5], treated_dist[5]) + i + j)) +
  geom_segment(aes(x = 4.75, xend = 5.25, y = max(control_dist[5], treated_dist[5]) + i + j, yend = max(control_dist[5], treated_dist[5]) + i + j)) +
  geom_segment(aes(x = 5.25, xend = 5.25, y = max(control_dist[5], treated_dist[5]) + i + j, yend = min(control_dist[5], treated_dist[5] + j))) +
  annotate("text", x = 5, y = max(control_dist[5], treated_dist[5]) + i + j + 2, size = 4, label = p_vals_char[5]) +
  # Bar: Same rank change
  geom_segment(aes(x = 5.75, xend = 5.75, y = max(control_dist[6], treated_dist[6]) + j, yend = max(control_dist[6], treated_dist[6]) + i + j)) +
  geom_segment(aes(x = 5.75, xend = 6.25, y = max(control_dist[6], treated_dist[6]) + i + j, yend = max(control_dist[6], treated_dist[6]) + i + j)) +
  geom_segment(aes(x = 6.25, xend = 6.25, y = max(control_dist[6], treated_dist[6]) + i + j, yend = min(control_dist[6], treated_dist[6] + j))) +
  annotate("text", x = 6, y = max(control_dist[6], treated_dist[6]) + i + j + 2, size = 4, label = p_vals_char[6]) +
  # Aesthetics
  xlab("Affiliation 2-5 years after Outcome Year") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14, face = "plain"),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    panel.grid.major.x = element_blank(),
    title = element_text(face = "bold")
  ) +
  ggtitle("C")

# Compute stat for all 'other' university move
treated_dist["Oth_Univ"] <- sum(treated_dist[4:6])
control_dist["Oth_Univ"] <- sum(control_dist[4:6])

data$Oth_Univ <- data$Oth_Univ_Const + data$Oth_Univ_Down + data$Oth_Univ_Up
p_oth_univ <- summary(lm("Oth_Univ~treated", data))$coef['treated', 4]
p_vals_char$Oth_Univ <- format_p_value(p_oth_univ)

# Write stats
for (name in names(treated_dist)) {
  fname <- file.path(path_statistics, paste0("perp_", name, ".txt"))
  writeLines(paste0(round(treated_dist[[name]], 1), "\\%"), fname)
}
for (name in names(control_dist)) {
  fname <- file.path(path_statistics, paste0("control_", name, ".txt"))
  writeLines(paste0(round(control_dist[[name]], 1), "\\%"), fname)
}
for (name in names(p_vals_char)) {
  fname <- file.path(path_statistics, paste0("Diff_", name, "_P.txt"))
  value <- substr(p_vals_char[[name]], 3, nchar(p_vals_char[name]))
  writeLines(value, fname)
}

##################
# 4. Combine plots
##################

# Combine publication and affiliation graphs
upper <- plot_grid(event_pubs, coef_coauth, rel_widths = c(0.65, 0.35), nrow=1)
out <- plot_grid(upper, bar_change, rel_heights = c(0.5, 0.5), nrow=2)

pdf(file=file.path(path_figures, "panel_scientists.pdf"), width=10, height=10)
print(out)
dev.off()

######################
# 5. Semi-elasticities
######################

# Publications
data_p <- read.csv(file.path(path_input, "did_pubs_pre_post.csv"))
results_p <- extract_estimates_and_ses(data_p)
stats_p <- read.csv(file.path(path_input, "did_leads_and_lags_pubs.txt"))
pubs <- compute_elasticities(stats_p$pub_mean, results_p$estimates, results_p$ses)
pubs <- as.data.frame(pubs)
row.names(pubs) <- c("pubs")

# Coauthors
coauthors <- as.data.frame(coauthors)
row.names(coauthors) <- row.names(stats_c)

# Combine and write statistic files
output <- as.data.frame(rbind(pubs, coauthors))
output$Est <- round(as.numeric(output$Est), 2)
output$T <- round(as.numeric(output$T), 2)
output$P <- round(as.numeric(output$P), 3)
output$Est <- paste0(output$Est, "\\%")
output$P <- paste0("=", output$P)
output$P <- gsub("^=0$", "<0.001", output$P)
print(output)
output <- output[, !names(output) %in% c("Std")]
for (i in 1:nrow(output)) {
  coef_name <- row.names(output)[i]
  coef_name <- paste0(toupper(substr(coef_name, 1, 1)), substr(coef_name, 2, nchar(coef_name)))
  for (col in colnames(output)) {
    value <- output[i, col]
    file_name <- paste0("perp", coef_name, "Effect", col, ".txt")
    file_path <- file.path(path_statistics, file_name)
    writeLines(as.character(value), file_path)
  }
}

# Regression table
main_table <- generate_table(
  header_string = "Dependent variable",
  column_names = c("$\\sinh^{-1}$(publ. count)", "$\\sinh^{-1}$(male coauthors)", "$\\sinh^{-1}$(female coauthors)"),
  estimates = c(pubs["Est"], coauthors["malecoauthors", "Est"], coauthors["femalecoauthors", "Est"]),
  std_errors = c(pubs["Std"], coauthors["malecoauthors", "Std"], coauthors["femalecoauthors", "Std"]),
  obs = c(stats_p["n_obs"], stats_c["malecoauthors", "n_obs"], stats_c["femalecoauthors", "n_obs"]),
  scientists = c(stats_p["d_scientists"], stats_c["malecoauthors", "d_scientists"], stats_c["femalecoauthors", "d_scientists"]),
  incs = c(stats_p["d_incidents"], stats_c["malecoauthors", "d_incidents"], stats_c["femalecoauthors", "d_incidents"])
)
write(main_table, file = file.path(path_tables, "reg_scientist.tex"))
