library(haven)
library(ggplot2)
library(readxl)
library(cowplot)
library(grid)
library(gridExtra)
library(xtable)

###############################
# Content:
# 1. Table summarizing perpetrators
# 2. Table summarizing treated articles
# 3. Graph on earliest newspaper outcome year
# 4. Outcome year distribution
# 5. Event study graph with citation pre-trend matching

path_input <- "./citation_dataset.dta"
path_output <- "./output/"

# Functions
return_stats <- function(s) {
  stats <- c(mean(s), sd(s), quantile(s, c(.1, .25, .5, .75, .9)))
  round(stats, 2)
}
return_inv_quantile <- function(year, outcome_years) {
  uniroot(function(x) quantile(outcome_years, x) - year, lower = 0, upper = 1)$root
}
cap_year <- function(x) {
  pmax(pmin(x, 10), -10)
}

# Load and filter data
data <- read_dta(path_input)

data_treated = subset(data, treated == 1)
data_trend = subset(data, year_rel_treatment >= -10 & year_rel_treatment <= 10)

####################
# 1. Scientist level
####################

# Aggregate and compute necessary statistics
f <- aggregate(eid ~ incident_id_num + earliest_pub_year + outcome_year,
               data = data_treated, FUN = function(x) length(unique(x)))
f$num_pubs <- f$eid
f$academic_age <- f$outcome_year - f$earliest_pub_year
f$num_pubs_per_year <- f$num_pubs / f$academic_age

# Co-authors
data_treated$co_authors <- data_treated$num_auth - 1
g <- aggregate(co_authors ~ eid + incident_id_num + earliest_pub_year + outcome_year + num_auth,
               data = data_treated, FUN = max)
f_2 <- aggregate(co_authors ~ incident_id_num + earliest_pub_year + outcome_year,
                 data = g, FUN = sum)

# Merge
f_3 <- merge(f, f_2[, c('incident_id_num', 'co_authors')], by = "incident_id_num")
f_3$num_coauthors_per_pub <- f_3$co_authors / f_3$num_pubs

# Define column names and create an empty dataframe
cols <- c('', 'Mean', 'Std. dev.', 'p10', 'p25', 'p50', 'p75', 'p90')
df <- data.frame(matrix(ncol = length(cols), nrow = 0))
colnames(df) <- cols

# Populate the dataframe with the computed statistics
df[1, ] <- c('Year of First Publication', return_stats(f_3$earliest_pub_year))
df[2, ] <- c('Career Age at Treatment', return_stats(f_3$academic_age))
df[3, ] <- c('Publications per year', return_stats(f_3$num_pubs_per_year))
df[4, ] <- c('Coauthors per publication', return_stats(f_3$num_coauthors_per_pub))

df_xtable <- xtable(df)
fname <- file.path(path_output, "/tables/summary_citation_scientist.tex")
print.xtable(df_xtable, file = fname, include.rownames = FALSE,
             include.colnames = FALSE, only.contents = TRUE, hline.after = NULL,
             booktabs = TRUE, comment = FALSE, timestamp = FALSE)

##################
# 2. Article level
##################

e <- subset(data_treated, citation_year < outcome_year)
fname <- file.path(path_output, "/numbers/N_of_Articles.txt")
writeLines(as.character(formatC(length(unique(e$eid)), format = "d", big.mark = ",")), fname)
t <- aggregate(cit_count ~ eid + pub_year + incident_id_num + outcome_year,
               data = e, FUN = sum)
t$years_prior <- t$outcome_year - t$pub_year
t$cit_count_per_year <- t$cit_count / t$years_prior
t_source_bin <- aggregate(cit_count ~ eid + pub_year + incident_id_num + outcome_year + source_bin,
                          data = e, FUN = sum)

df <- data.frame(matrix(ncol = length(cols), nrow = 0))
colnames(df_2) <- cols

df[1, ] <- c('Year of Publication', return_stats(t$pub_year))
df[2, ] <- c('Publication Age at Treatment', return_stats(t$years_prior))
df[3, ] <- c('Citations per year', return_stats(t$cit_count_per_year))
df[4, ] <- c('Journal-rank percentile', return_stats(t_source_bin$source_bin))

df_xtable <- xtable(df)
fname <- file.path(path_output, "./tables/summary_citation_article.tex")
print.xtable(df_xtable, file = fname, include.rownames = FALSE,
             include.colnames = FALSE, only.contents = TRUE, hline.after = NULL,
             booktabs = TRUE, comment = FALSE, timestamp = FALSE)

############################
# 3. Earliest newspaper year
############################

m <- aggregate(big_newspaper ~ lexisnexis + incident_id_num + outcome_year + earliest_lexisnexis_year,
               data = data_treated, FUN = max)
m$np_rel_outcome <- m$earliest_lexisnexis_year - m$outcome_year
m$np_rel_outcome_trunc <- pmin(pmax(m$np_rel_outcome, -3), 5)

num_newspaper <- nrow(m)
num_newspaper_prior <- nrow(subset(m, np_rel_outcome < 0))
num_newspaper_equal <- nrow(subset(m, np_rel_outcome == 0))
num_newspaper_after <- nrow(subset(m, np_rel_outcome > 0))
num_newspaper_more_than_2y_later <- nrow(subset(m, np_rel_outcome > 2))
num_newspaper_more_than_2y_earlier <- nrow(subset(m, np_rel_outcome < -2))

cat("Proportion with publication year equal to outcome:", num_newspaper_equal / num_newspaper, "\n")
cat("Proportion with publication year after outcome:", num_newspaper_after / num_newspaper, "\n")
cat("Proportion with publication year prior to outcome:", num_newspaper_prior / num_newspaper, "\n")
cat("Proportion with publication year within 2 years of outcome:", 
    (num_newspaper - num_newspaper_more_than_2y_later - num_newspaper_more_than_2y_earlier - num_newspaper_equal) / num_newspaper, "\n")
cat("Cases in LexisNexis:", nrow(subset(m, lexisnexis > 0)) / num_newspaper, "\n")
cat("Cases with big newspaper:", nrow(subset(m, big_newspaper > 0)) / num_newspaper, "\n")

h <- ggplot(m, aes(x = np_rel_outcome_trunc)) +
  geom_histogram(color = "gray60", breaks = c(-3.5, -2.5, -1.5, -0.5, 0.5, 1:5 + 0.5)) +
  scale_x_continuous(name = "Year of earliest newspaper article\nrelative to Outcome Year",
                     breaks = -3:5, labels = c("<=-3", as.character(-2:4), ">=5")) +
  scale_y_continuous(name = "Number of incidents") +
  theme(axis.title = element_text(size = 14))

pdf(file = file.path(path_output, "figures/hist_newspaper-outcome.pdf"), width = 5, height = 5)
print(h)
dev.off()

##############################
# 4. Outcome Year Distribution
##############################

pdf(file = file.path(path_output, "figures/cdf_outcomeyear.pdf"), width = 7, height = 7)

yrs <- 1998:2019
cum_yrs <- sapply(yrs, function(y) return_inv_quantile(y, f_3$outcome_year))

plot(stepfun(c(1998, yrs, 2020), c(0, 0, cum_yrs, 1)), 
     do.points = FALSE, xaxt = "n", yaxt = "n", xlim = c(1998, 2020), 
     col = "green", ylab = "Cumulative Share", xlab = "Outcome Year", cex.lab = 1.5)
axis(side = 1, at = seq(1998, 2020, by = 5), las = 1)
axis(side = 2, at = seq(0, 1, by = 0.2), las = 2)
dev.off()

#################
# 5. Trend graphs
#################

data_trend$year_rel_treatment_capped <- sapply(data_trend$year_rel_treatment, cap_year)

data_cits <- aggregate(cit_count ~ year_rel_treatment + treated, data_trend, mean)
colnames(data_cits) <- c("year", "treated", "cits")

g <- ggplot(data_cits, aes(x = year, y = cits, colour = factor(treated))) +
  geom_line() +
  geom_point(shape = 17) +
  scale_color_manual(name = NULL, labels = c("control", "treated"), values = c("black", "red")) +
  ylab("Average citations") +
  xlab("Year relative to Outcome year") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = -10:10, labels = as.character(-10:10)) +
  ylim(0, 4)

pdf(file = file.path(path_output, "figures/line_trends.pdf"), width = 7, height = 4)
print(g)
dev.off()
