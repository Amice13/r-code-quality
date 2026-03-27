library(brew)
library(cowplot)
library(ggplot2)
library(grid)
library(gridExtra)

###############################
# Content:
# 1. Semi-elasticities for OLS DiD estimates
# 2. Semi-elasticities for Poisson DiD estimates
# 3. Event study graph in color with 4 pre-years and 5 post-years in color
# 4. Panel graph:
# - Panel A: Event graph in grey
# - Panel B: Coefplot by gender of citing author
# - Panel C: Coefplot by distance of coauthor
# - Panel D: Coefplot by male dominance of field/gender citing author
# 5. Event study with citation matching

# Definitions
path_input <- "./"
path_figures <- "./output/figures/"
path_tables <- "./output/tables/"
path_statistics <- "./output/numbers/"

# Functions
return_stats <- function(s) {
  c(mean(s), sd(s), quantile(s, c(.1, .25, .5, .75, .9)))
}
strip_letters <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
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
p_to_stars <- function(p, thres = c(0.1, 0.05, 0.01)) {
  n_stars <- sum(p < thres)
  return(strrep("*", n_stars))
}

# Regression table template
regression_table_template <- "
\\begin{tabular}{p{0.3\\textwidth}<%= paste(rep('c', n_columns), collapse = '') %>}
\\toprule
<%= if (custom_row != '') custom_row else '' %>
<%= header_string %> & <%= paste(column_names, collapse = ' & ') %>\\\\
\\midrule
treatment $\\times$ post (\\%) & <%= paste(estimate_values, collapse = ' & ') %>\\\\
& (<%= paste(stderr_values, collapse = ') & (') %>)\\\\
\\midrule
Article Fixed Effects & <%= paste(rep('\\\\checkmark', n_columns), collapse = ' & ') %>\\\\
Journal Issue $\\times$ Citation Year Fixed Effects & <%= paste(rep('\\\\checkmark', n_columns), collapse = ' & ') %>\\\\
&&\\\\
Observations & <%= paste(observations, collapse = ' & ') %>\\\\
Articles & <%= paste(articles, collapse = ' & ') %>\\\\
Incidents & <%= paste(incidents, collapse = ' & ') %>\\\\
\\bottomrule
\\end{tabular}
"
template_file <- tempfile()
writeLines(regression_table_template, template_file)
generate_table <- function(column_names, estimates, std_errors, obs, arts, incs, header_string, custom_row = '') {
  n_columns <- length(column_names)
  
  estimate_values <- sprintf("%.3f", estimates)
  stderr_values <- sprintf("%.3f", std_errors)
  observations <- prettyNum(obs, big.mark = ",", scientific = FALSE)
  articles <- prettyNum(arts, big.mark = ",", scientific = FALSE)
  incidents <- prettyNum(incs, big.mark = ",", scientific = FALSE)
  
  output <- capture.output(brew(template_file))
  return(paste(output, collapse = "\n"))
}


############################################
# 1. Semi-elasticities for OLS DiD estimates
############################################

# Define container
df <- data.frame(Est = numeric(), Std = numeric(), T = numeric(), P = numeric())
stats <- data.frame(Obs = numeric(), Articles = numeric(), Incidents = numeric())

# Main estimates
data <- read.csv(file.path(path_input, "did_pre_post.csv"))
results <- extract_estimates_and_ses(data)
meta <- read.csv(file.path(path_input, "did_pre_post.txt"), row.names="spec")
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)
temp_results <- as.data.frame(elasticities, stringsAsFactors = FALSE)
row.names(temp_results) <- row.names(meta)
df <- rbind(df, temp_results)
stats <- rbind(stats, meta[, c("n_obs", "d_articles", "d_incidents")])

# Different dependent variables
data <- read.csv(file.path(path_input, "did_dep_vars.csv"))
results <- extract_estimates_and_ses(data)
meta <- read.csv(file.path(path_input, "did_dep_vars.txt"), row.names="spec")
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)
temp_results <- as.data.frame(elasticities, stringsAsFactors = FALSE)
row.names(temp_results) <- row.names(meta)
df <- rbind(df, temp_results)

# Social Distance by gender
data <- read.csv(file.path(path_input, "did_soc_dist_gender.csv"))
results <- extract_estimates_and_ses(data)
meta <- read.csv(file.path(path_input, "did_soc_dist_gender.txt"), row.names="spec")
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)
temp_results <- as.data.frame(elasticities, stringsAsFactors = FALSE)
row.names(temp_results) <- row.names(meta)
df <- rbind(df, temp_results)

# Incident
data <- read.csv(file.path(path_input, "did_pre_post_incident.csv"))
meta <- read.csv(file.path(path_input, "did_pre_post_incident.txt"), row.names="spec")
coefficient_indices <- c(1:11) * 2
did_estimates <- as.vector(sapply(data[coefficient_indices, 2], FUN = strip_letters))
did_se <- as.vector(sapply(data[coefficient_indices + 1, 2], FUN = strip_letters))
elasticities <- compute_elasticities(meta$cit_mean, did_estimates, did_se)
writeLines(as.character(round(min(elasticities$P[2:5]), 3)),
           file.path(path_statistics, "PreTrendPMin.txt"))
writeLines(as.character(round(max(elasticities$P[2:5]), 3)),
           file.path(path_statistics, "PreTrendPMax.txt"))
temp_results <- head(as.data.frame(elasticities, stringsAsFactors = FALSE), 1)
row.names(temp_results) <- row.names(meta)
df <- rbind(df, temp_results)
stats <- rbind(stats, meta[, c("n_obs", "d_articles", "d_incidents")])

# 1y, 2y, 3y anticipation
data <- read.csv(file.path(path_input, "did_pre_post_anticip.csv"))
results <- extract_estimates_and_ses(data)
meta <- read.csv(file.path(path_input, "did_pre_post.txt"), row.names="spec")
elasticities <- compute_elasticities(meta$cit_mean[1], results$estimates, results$ses)
temp_results <- as.data.frame(elasticities, stringsAsFactors = FALSE)
row.names(temp_results) <- c('anticip_one_year', 'anticip_two_year', 'anticip_three_year')
df <- rbind(df, temp_results)
stats <- rbind(stats, meta[, c("n_obs", "d_articles", "d_incidents")])

# Stayers vs leavers
data <- read.csv(file.path(path_input, "did_stayers_leavers.csv"))
results <- extract_estimates_and_ses(data)
meta <- read.csv(file.path(path_input, "did_stayers_leavers.txt"), row.names="spec")
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)
temp_results <- as.data.frame(elasticities, stringsAsFactors = FALSE)
row.names(temp_results) <- row.names(meta)
df <- rbind(df, temp_results)
stats <- rbind(stats, meta[, c("n_obs", "d_articles", "d_incidents")])

# Big newspaper vs small newspaper coverage
data <- read.csv(file.path(path_input, "did_media.csv"))
results <- extract_estimates_and_ses(data)
meta <- read.csv(file.path(path_input, "did_media.txt"), row.names="spec")
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)
temp_results <- as.data.frame(elasticities, stringsAsFactors = FALSE)
row.names(temp_results) <- row.names(meta)
df <- rbind(df, temp_results)
stats <- rbind(stats, meta[, c("n_obs", "d_articles", "d_incidents")])

# Truncation
data <- read.csv(file.path(path_input, "did_trunc.csv"))
results <- extract_estimates_and_ses(data)
meta <- read.csv(file.path(path_input, "did_trunc.txt"), row.names="spec")
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)
temp_results <- as.data.frame(elasticities, stringsAsFactors = FALSE)
row.names(temp_results) <- row.names(meta)
df <- rbind(df, temp_results)
stats <- rbind(stats, meta[, c("n_obs", "d_articles", "d_incidents")])

# Combine and write statistic files
output <- df
output$Est <- round(output$Est, 2)
output$T <- round(output$T, 2)
output$P <- round(output$P, 3)
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
    file_name <- paste0(coef_name, "Effect", col, ".txt")
    file_path <- file.path(path_statistics, file_name)
    writeLines(as.character(value), file_path)
  }
}

# Combine and write regression files
main_table <- generate_table(
  header_string = "Citations by gender",
  column_names = c("All", "Male", "Female"),
  estimates = c(df["main", "Est"], df["male_cit", "Est"], df["female_cit", "Est"]),
  std_errors = c(df["main", "Std"], df["male_cit", "Std"], df["female_cit", "Std"]),
  obs = rep(stats["main", "n_obs"], 3),
  arts = rep(stats["main", "d_articles"], 3),
  incs = rep(stats["main", "d_incidents"], 3)
)
write(main_table, file = file.path(path_tables, "reg_main-ols.tex"))

dist_table <- generate_table(
  custom_row = "Cit. by coauthorship dist. & \\multicolumn{3}{c}{Dist=1}& \\multicolumn{3}{c}{Dist=2} & \\multicolumn{3}{c}{Dist$\\geq$3} \\\\",
  header_string = "Citations by gender of first author",
  column_names = rep(c("All", "Male", "Female"), 3),
  estimates = c(df["dist_one", "Est"], df["dist_one_male_cit", "Est"], df["dist_one_female_cit", "Est"],
                df["dist_two", "Est"], df["dist_two_male_cit", "Est"], df["dist_two_female_cit", "Est"],
                df["dist_three", "Est"], df["dist_three_male_cit", "Est"], df["dist_three_female_cit", "Est"]),
  std_errors = c(df["dist_one", "Std"], df["dist_one_male_cit", "Std"], df["dist_one_female_cit", "Std"],
                 df["dist_two", "Std"], df["dist_two_male_cit", "Std"], df["dist_two_female_cit", "Std"],
                 df["dist_three", "Std"], df["dist_three_male_cit", "Std"], df["dist_three_female_cit", "Std"]),
  obs = rep(stats["main", "n_obs"], 9),
  arts = rep(stats["main", "d_articles"], 9),
  incs = rep(stats["main", "d_incidents"], 9)
)
write(dist_table, file = file.path(path_tables, "reg_socdist-ols.tex"))

field_table <- generate_table(
  custom_row = "Articles by field of publication & \\multicolumn{3}{c}{Not male-dominated fields} & \\multicolumn{3}{c}{Male-dominated fields} \\\\",
  header_string = "Citations by gender of first author",
  column_names = rep(c("All", "Male", "Female"), 2),
  estimates = c(df["non_male_dom", "Est"], df["male_cit_not_male_dom", "Est"], df["female_cit_not_male_dom", "Est"],
                df["male_dom", "Est"], df["male_cit_male_dom", "Est"], df["female_cit_male_dom", "Est"]),
  std_errors = c(df["non_male_dom", "Std"], df["male_cit_not_male_dom", "Std"], df["female_cit_not_male_dom", "Std"],
                 df["male_dom", "Std"], df["male_cit_male_dom", "Std"], df["female_cit_male_dom", "Std"]),
  obs = rep(c(stats["non_male_dom", "n_obs"], stats["male_dom", "n_obs"]), each = 3),
  arts = rep(c(stats["non_male_dom", "d_articles"], stats["male_dom", "d_articles"]), each = 3),
  incs = rep(c(stats["non_male_dom", "d_incidents"], stats["male_dom", "d_incidents"]), each = 3)
)
write(field_table, file = file.path(path_tables, "reg_field-ols.tex"))

trunc_table <- generate_table(
  header_string = "Citations truncated $x$ years after Outcome year",
  column_names = c("4 years", "6 years", "8 years", "10 years", "12 years"),
  estimates = c(df["trunc_four_years", "Est"], df["trunc_six_years", "Est"], df["trunc_eight_years", "Est"],
                df["trunc_ten_years", "Est"], df["trunc_twelve_years", "Est"]),
  std_errors = c(df["trunc_four_years", "Std"], df["trunc_six_years", "Std"], df["trunc_eight_years", "Std"],
                 df["trunc_ten_years", "Std"], df["trunc_twelve_years", "Std"]),
  obs = c(stats["trunc_four_years", "n_obs"], stats["trunc_six_years", "n_obs"], stats["trunc_eight_years", "n_obs"],
          stats["trunc_ten_years", "n_obs"], stats["trunc_twelve_years", "n_obs"]),
  arts = c(stats["trunc_four_years", "d_articles "], stats["trunc_six_years", "d_articles "], stats["trunc_eight_years", "d_articles "],
           stats["trunc_ten_years", "d_articles "], stats["trunc_twelve_years", "d_articles "]),
  incs = c(stats["trunc_four_years", "d_incidents"], stats["trunc_six_years", "d_incidents"], stats["trunc_eight_years", "d_incidents"],
           stats["trunc_ten_years", "d_incidents"], stats["trunc_twelve_years", "d_incidents"])
)
write(trunc_table, file = file.path(path_tables, "reg_truncation-ols.tex"))

media_table <- generate_table(
  custom_row = "Sample & \\multicolumn{2}{c}{Incident reported by} & \\multicolumn{2}{c}{Accused scientist} \\\\",
  header_string = "",
  column_names = c("large newspapers", "small newspapers", "stops publishing", "keeps publishing"),
  estimates = c(df["big_newspaper", "Est"], df["small_newspaper", "Est"], df["stop_publ", "Est"], df["cont_publ", "Est"]),
  std_errors = c(df["big_newspaper", "Std"], df["small_newspaper", "Std"], df["stop_publ", "Std"], df["cont_publ", "Std"]),
  obs = c(stats["big_newspaper", "n_obs"], stats["small_newspaper", "n_obs"], stats["stop_publ", "n_obs"], stats["cont_publ", "n_obs"]),
  arts = c(stats["big_newspaper", "d_articles"], stats["small_newspaper", "d_articles"], stats["stop_publ", "d_articles"], stats["cont_publ", "d_articles"]),
  incs = c(stats["big_newspaper", "d_incidents"], stats["small_newspaper", "d_incidents"], stats["stop_publ", "d_incidents"], stats["cont_publ", "d_incidents"])
)
write(media_table, file = file.path(path_tables, "reg_mediastayer-ols.tex"))

anticipation_table <- generate_table(
  custom_row = "Treatment year definition & Incident & \\multicolumn{3}{c}{Outcome year}  & Minimum of \\\\",
  header_string = "",
  column_names = c("allegedly occurred", "minus 1", "minus 2", "minus 3", "earliest newspaper art. and outcome year"),
  estimates = c(df["incident", "Est"], df["anticip_one_year", "Est"], df["anticip_two_year", "Est"], df["anticip_three_year", "Est"], df["anticip_earliest_n_p", "Est"]),
  std_errors = c(df["incident", "Std"], df["anticip_one_year", "Std"], df["anticip_two_year", "Std"], df["anticip_three_year", "Std"], df["anticip_earliest_n_p", "Std"]),
  obs = rep(stats["incident", "n_obs"], 5),
  arts = rep(stats["incident", "d_articles"], 5),
  incs = rep(stats["incident", "d_incidents"], 5)
)
write(anticipation_table, file = file.path(path_tables, "reg_anticipation-ols.tex"))


################################################
# 2. Semi-elasticities for Poisson DiD estimates
################################################

data <- read.csv(file.path(path_input, "poisson_main.csv"))
meta <- read.csv(file.path(path_input, "poisson_main.txt"), row.names="spec")
results <- extract_estimates_and_ses(data)
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)
poisson <- as.data.frame(elasticities, stringsAsFactors = FALSE)
colnames(poisson) <- c('Est', 'Std', 'T', 'P')
row.names(poisson) <- row.names(meta)

# Combine and write statistic files
output <- poisson
output$Est <- round(output$Est, 2)
output$T <- round(output$T, 2)
output$P <- round(output$P, 3)
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
    file_name <- paste0(coef_name, "Effect", col, ".txt")
    file_path <- file.path(path_statistics, file_name)
    writeLines(as.character(value), file_path)
  }
}

# Combine and write regression files
poisson_table <- generate_table(
  header_string = "Citations by gender",
  column_names = c("All", "Male", "Female"),
  estimates = poisson$Est,
  std_errors = poisson$Std,
  obs = rep(meta$n_obs[1], 3),
  arts = rep(meta$d_articles[1], 3),
  incs = rep(meta$d_incidents[1], 3)
)
write(poisson_table, file = file.path(path_tables, "reg_main-poisson.tex"))

#############################
# 3. Simple Event Study Graph
#############################

data <- read.csv(file.path(path_input, "did_leads_and_lags_all.csv"))
meta <- read.csv(file.path(path_input, "did_leads_and_lags_dep_mean.txt"))
indices <- c(18, 16, 14, 12, 2, 4, 6, 8, 10)
estimates <- as.vector(sapply(data[indices, 2], FUN = strip_letters))
ses <- as.vector(sapply(data[indices + 1, 2], FUN = strip_letters))
n_post <- 4
elasticities <- compute_elasticities(meta$cit_mean, estimates, ses)
elasticities <- as.data.frame(elasticities, stringsAsFactors = FALSE)
data_pre <- elasticities[1:4, ]
colnames(data_pre) <- paste0("pre_", colnames(data_pre))
data_post <- elasticities[5:nrow(elasticities), ]
colnames(data_post) <- paste0("post_", colnames(data_post))
data_graph <- merge(data_pre, data_post, by = "row.names", all = TRUE)
data_graph <- subset(data_graph, select = -Row.names)
data_graph$labels <- as.character(seq(from = -4, to = n_post, by = 1))
data_graph$labels <- factor(data_graph$labels,
                            levels = data_graph$labels)

color <- "#999999"
s <- ggplot(data_graph, aes(x = labels, group = 1)) +
  geom_line(aes(y = pre_Est), color = color) +
  geom_line(aes(y = post_Est), color = color) +
  geom_point(aes(y = pre_Est), color = color) +
  geom_point(aes(y = post_Est), color = color) +
  geom_ribbon(aes(y = pre_Est, ymin = pre_Est - 1.96 * pre_Std, ymax = pre_Est + 1.96 * pre_Std),
              alpha = 0.5, colour = color, linetype = "dashed", fill = color) +
  geom_ribbon(aes(y = post_Est, ymin = post_Est - 1.96 * post_Std, ymax = post_Est + 1.96 * post_Std),
              alpha = 0.5, colour = color, linetype = "dashed", fill = color) +
  scale_y_continuous(name = "Average effect on citations (%)", breaks = seq(from = -16, to = 6, by = 1)) +
  scale_x_discrete(name = "Year relative to Outcome Year", breaks = seq(from = -4, to = n_post, by = 1)) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18, face = "plain"))

pdf(file = file.path(path_figures, "event_study_citations.pdf"), width = 10, height = 6)
print(s)
dev.off()

# Write statistics
output <- data_graph[!is.na(data_graph$post_Est), ]
output <- output[order(output$post_Est), ]
output <- output[c(1, nrow(output)), c("post_Est", "post_P")]
row.names(output) <- c("max", "min")
output$post_Est <- round(output$post_Est, 2)
output$post_Est <- paste0(output$post_Est, "\\%")
output$post_P <- round(output$post_P, 3)
output$post_P <- paste0("=", output$post_P)
output$post_P <- gsub("^=0$", "<0.001", output$post_P)
for (i in 1:nrow(output)) {
  coef_name <- row.names(output)[i]
  for (col in colnames(output)) {
    value <- output[i, col]
    file_name <- paste("Main", col, coef_name, sep = "_")
    file_path <- file.path(path_statistics, paste0(file_name, ".txt"))
    writeLines(as.character(value), file_path)
  }
}

##########
# 4. Panel
##########

out_a <- s +
  theme_bw() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18, face = "plain"),
    axis.title.y = element_blank(),
    title = element_text(face = "bold"),
  ) +
  ggtitle("A")

size_of_points <- 5
size_of_bars <- 2
width_of_bars <- 0.2
dodge_bars <- 0.75
dodge_fac <- 3
color_panel <- c("grey40", "grey10", "grey80")

# Panel B: Main result, by male and female author
x_labels <- c("Articles in all fields")
group_labels <- c("male", "all", "female")
data_graph <- data.frame(
  estimate = c(df["male_cit", "Est"], df["main", "Est"], df["female_cit", "Est"]),
  error = c(df["male_cit", "Std"], df["main", "Std"], df["female_cit", "Std"]),
  significance = c(df["male_cit", "P"], df["main", "P"], df["female_cit", "P"]),
  condition = factor(rep(x_labels, 3), levels = x_labels),
  gender = factor(group_labels, levels = group_labels)
)
data_graph$asterisk <- sapply(data_graph$significance, p_to_stars)

out_b <- ggplot(data_graph, aes(fill = gender, x = condition, color = gender, group = gender)) +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * error, ymax = estimate + 1.96 * error),
    position = position_dodge(width = dodge_bars),
    stat = "identity", width = width_of_bars, size = size_of_bars) +
  scale_colour_manual("Gender of citing\nfirst author", values = color_panel) +
  geom_point(
    aes(y = estimate, shape = gender),
    position = position_dodge(width = dodge_bars),
    stat = "identity",
    show.legend = FALSE,
    size = size_of_points) +
  scale_y_continuous(name = "Average effect on citations (%)", limits = c(-8.5, 0.5), breaks = seq(from = -8, to = 0, by = 2)) +
  geom_hline(yintercept = 0) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    panel.grid.major.x = element_blank(),
    title = element_text(face = "bold")
  ) +
  ggtitle("B") +
  annotate("text", x = 1 - dodge_bars / dodge_fac, y = data_graph$estimate[1] + 1.96 * data_graph$error[1] + 1, size = 10, label = data_graph$asterisk[1]) +
  annotate("text", x = 1, y = data_graph$estimate[2] + 1.96 * data_graph$error[2] + 1, size = 10, label = data_graph$asterisk[2]) +
  annotate("text", x = 1 + dodge_bars / dodge_fac, y = data_graph$estimate[3] + 1.96 * data_graph$error[3] + 1, size = 10, label = data_graph$asterisk[3])

# Panel C: By social distance and by male and female author
data_graph <- data.frame(
  estimate = c(df["dist_one_male_cit", "Est"], df["dist_one", "Est"], df["dist_one_female_cit", "Est"],
               df["dist_two_male_cit", "Est"], df["dist_two", "Est"], df["dist_two_female_cit", "Est"],
               df["dist_three_male_cit", "Est"], df["dist_three", "Est"], df["dist_three_female_cit", "Est"]),
  error = c(df["dist_one_male_cit", "Std"], df["dist_one", "Std"], df["dist_one_female_cit", "Std"],
            df["dist_two_male_cit", "Std"], df["dist_two", "Std"], df["dist_two_female_cit", "Std"],
            df["dist_three_male_cit", "Std"], df["dist_three", "Std"], df["dist_three_female_cit", "Std"]),
  significance = c(df["dist_one_male_cit", "P"], df["dist_one", "P"], df["dist_one_female_cit", "P"],
                   df["dist_two_male_cit", "P"], df["dist_two", "P"], df["dist_two_female_cit", "P"],
                   df["dist_three_male_cit", "P"], df["dist_three", "P"], df["dist_three_female_cit", "P"]),
  condition = factor(rep(c("Distance 1", "Distance 2", "Distance >=3"), each = 3), levels = c("Distance 1", "Distance 2", "Distance >=3")),
  gender = factor(rep(c("male", "all", "female"), 3), levels = c("male", "all", "female"))
)
data_graph$asterisk <- sapply(data_graph$significance, p_to_stars)

out_c <- ggplot(data_graph, aes(fill = gender, x = condition, color = gender, group = gender)) +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * error, ymax = estimate + 1.96 * error),
    position = position_dodge(width = dodge_bars),
    stat = "identity", width = width_of_bars, size = size_of_bars) +
  scale_colour_manual("Gender of citing\nfirst author", values = color_panel) +
  geom_point(
    aes(y = estimate, shape = gender),
    position = position_dodge(width = dodge_bars),
    stat = "identity",
    show.legend = FALSE,
    size = size_of_points) +
  scale_y_continuous(name = "Average effect on citations (%)", limits = c(-50, 5), breaks = seq(from = -50, to = 5, by = 5)) +
  scale_x_discrete(name = "Citations by distance in the coauthorship network") +
  geom_hline(yintercept = 0) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(size = 16, face = "plain"),
    axis.title.y = element_blank()) +
  ggtitle("C") +
  annotate("text", x = 1 - dodge_bars / dodge_fac, y = data_graph$estimate[1] + 1.96 * data_graph$error[1] + 1, size = 10, label = data_graph$asterisk[1]) +
  annotate("text", x = 1, y = data_graph$estimate[2] + 1.96 * data_graph$error[2] + 1, size = 10, label = data_graph$asterisk[2]) +
  annotate("text", x = 1 + dodge_bars / dodge_fac, y = data_graph$estimate[3] + 1.96 * data_graph$error[3] + 1, size = 10, label = data_graph$asterisk[3]) +
  annotate("text", x = 2 - dodge_bars / dodge_fac, y = data_graph$estimate[4] + 1.96 * data_graph$error[4] + 1, size = 10, label = data_graph$asterisk[4]) +
  annotate("text", x = 2, y = data_graph$estimate[5] + 1.96 * data_graph$error[5] + 1, size = 10, label = data_graph$asterisk[5]) +
  annotate("text", x = 2 + dodge_bars / dodge_fac, y = data_graph$estimate[6] + 1.96 * data_graph$error[6] + 1, size = 10, label = data_graph$asterisk[6])

# Panel D: Male-dominated fields, by male and female author
x_labels <- c("Articles in not-male-dominated fields", "Articles in male-dominated fields")
group_labels <- c("male", "all", "female")
data_graph <- data.frame(
  estimate = c(df["male_cit_not_male_dom", "Est"], df["non_male_dom", "Est"], df["female_cit_not_male_dom", "Est"],
               df["male_cit_male_dom", "Est"], df["male_dom", "Est"], df["female_cit_male_dom", "Est"]),
  error = c(df["male_cit_not_male_dom", "Std"], df["non_male_dom", "Std"], df["female_cit_not_male_dom", "Std"],
            df["male_cit_male_dom", "Std"], df["male_dom", "Std"], df["female_cit_male_dom", "Std"]),
  significance = c(df["male_cit_not_male_dom", "P"], df["non_male_dom", "P"], df["female_cit_not_male_dom", "P"],
                   df["male_cit_male_dom", "P"], df["male_dom", "P"], df["female_cit_male_dom", "P"]),
  condition = factor(c(rep(x_labels[1], 3), rep(x_labels[2], 3)), levels = x_labels),
  gender = factor(rep(group_labels, 2), levels = group_labels)
)
data_graph$asterisk <- sapply(data_graph$significance, p_to_stars)
shapes <- c(16, 17, 15)

out_d <- ggplot(data_graph, aes(x = condition, y = estimate, group = gender)) +
  geom_errorbar(
    aes(ymin = estimate - 1.96 * error, ymax = estimate + 1.96 * error, color = gender),
    position = position_dodge(width = dodge_bars),
    stat = "identity",
    width = width_of_bars,
    size = size_of_bars
  ) +
  geom_point(
    aes(shape = gender, color = gender),
    position = position_dodge(width = dodge_bars),
    stat = "identity",
    size = size_of_points
  ) +
  scale_color_manual(name = "Gender of first author of citing article", values = color_panel) +
  scale_shape_manual(name = "Gender of first author of citing article", values = shapes) +
  guides(
    color = guide_legend(override.aes = list(linetype = 1, shape = shapes)),
    shape = "none"
  ) +
  scale_y_continuous(
    name   = "Average effect on citations (%)",
    limits = c(-10, 8),
    breaks = seq(-10, 8, 2)
  ) +
  geom_hline(yintercept = 0) +
  theme_bw(base_size = 14) +
  theme(
    legend.box.background = element_rect(),
    legend.box.margin = margin(5),
    legend.background = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title = element_blank()
  ) +
  ggtitle("D") +
  annotate("text", x = 1 - dodge_bars / dodge_fac, y = data_graph$estimate[1] + 1.96 * data_graph$error[1] + 1, size = 10, label = data_graph$asterisk[1]) +
  annotate("text", x = 1, y = data_graph$estimate[2] + 1.96 * data_graph$error[2] + 1, size = 10, label = data_graph$asterisk[2]) +
  annotate("text", x = 1 + dodge_bars / dodge_fac, y = data_graph$estimate[3] + 1.96 * data_graph$error[3] + 1, size = 10, label = data_graph$asterisk[3])

mylegend <- g_legend(out_d)

p <- plot_grid(out_a, out_c, rel_widths = c(0.4, 0.6), nrow = 1)
y_grob <- textGrob("Average effect on citations (%)", gp = gpar(fontsize = 18), rot = 90)
upper_temp <- grid.arrange(arrangeGrob(p, left = y_grob))
upper <- plot_grid(upper_temp, mylegend, nrow = 2, rel_heights = c(0.85, 0.15))

p_lower <- plot_grid(out_b, out_d + theme(legend.position = "none"), rel_widths = c(0.4, 0.6), nrow = 1)
lower <- grid.arrange(arrangeGrob(p_lower, left = y_grob))

pdf(file = file.path(path_figures, "panel_citations.pdf"), width = 14, height = 10)
plot_grid(upper, lower, nrow = 2, rel_heights = c(0.57, 0.43))
dev.off()

######################
# 5. Citation Matching
######################

# Event study graph
data <- read.csv(file.path(path_input, "did_leads_and_lags_all_cit_matched.csv"))
meta <- read.csv(file.path(path_input, "did_leads_and_lags_dep_mean_cit_matched.txt"))

indices <- c(18, 16, 14, 12, 2, 4, 6, 8, 10)
estimates <- as.vector(sapply(data[indices, 2], FUN = strip_letters))
ses <- as.vector(sapply(data[indices + 1, 2], FUN = strip_letters))
n_pre <- 4
n_post <- 4
elasticities <- compute_elasticities(meta$cit_mean, estimates, ses)
elasticities <- as.data.frame(elasticities, stringsAsFactors = FALSE)
data_pre <- elasticities[1:4, ]
colnames(data_pre) <- paste0("pre_", colnames(data_pre))
data_post <- elasticities[5:nrow(elasticities), ]
colnames(data_post) <- paste0("post_", colnames(data_post))
data_graph <- merge(data_pre, data_post, by = "row.names", all = TRUE)
data_graph <- subset(data_graph, select = -Row.names)
data_graph$labels <- as.character(seq(from = -4, to = n_post, by = 1))
data_graph$labels <- factor(data_graph$labels,
                            levels = data_graph$labels)

color_pre <- "#0072B2"
color_post <- "#D55E00"
s_2 <- ggplot(data_graph, aes(x = labels, group = 1)) +
  geom_line(aes(y = pre_Est), color = color_pre) +
  geom_line(aes(y = post_Est), color = color_post) +
  geom_point(aes(y = pre_Est), color = color_pre) +
  geom_point(aes(y = post_Est), color = color_post) +
  geom_ribbon(aes(y = pre_Est, ymin = pre_Est - 1.96 * pre_Std, ymax = pre_Est + 1.96 * pre_Std),
              alpha = 0.5, colour = color_pre, linetype = "dashed", fill = color_pre) +
  geom_ribbon(aes(y = post_Est, ymin = post_Est - 1.96 * post_Std, ymax = post_Est + 1.96 * post_Std),
              alpha = 0.5, colour = color_post, linetype = "dashed", fill = color_post) +
  scale_y_continuous(name = "Average effect on citations (%)", breaks = seq(from = -16, to = 6, by = 1)) +
  scale_x_discrete(name = "Year relative to Outcome Year", breaks = seq(from = -n_pre, to = n_post, by = 1)) +
  geom_hline(yintercept = 0) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 18, face = "plain"), title = element_text(size = 18, face = "plain"))

pdf(file = file.path(path_figures, "event_study_citations_cit_matched.pdf"), width = 7, height = 5)
print(s_2)
dev.off()

# Statistics for point estimates
data <- read.csv(file.path(path_input, "did_pre_post_all_cit_matched.csv"))
results <- extract_estimates_and_ses(data)
elasticities <- compute_elasticities(meta$cit_mean, results$estimates, results$ses)

file_path <- file.path(path_statistics, "MatchedEffectEst.txt")
writeLines(paste0(round(elasticities$Est, 2), "\\%"), file_path)
file_path <- file.path(path_statistics, "MatchedEffectT.txt")
writeLines(as.character(round(elasticities$T, 2)), file_path)
file_path <- file.path(path_statistics, "MatchedEffectP.txt")
p_val <- paste0("=", round(elasticities$P, 3))
p_val <- gsub("^=0$", "<0.001", p_val)
writeLines(p_val, file_path)
