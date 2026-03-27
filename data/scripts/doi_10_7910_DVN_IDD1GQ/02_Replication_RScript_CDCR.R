#' ---
#' title: "Replication log for *Detecting Anomalies in Data on Government Violence*"
#' author: "Kanisha D. Bond, Courtenay R. Conrad, Dylan Moses, and Joel W. Simmons"
#' date: "May 10, 2021"
#' ---
#################################################################
##                        Preliminaries                        ##
#################################################################
# Load packages -----------------------------------------------------------
#' # Preliminaries
#' ### Load packages
#' An easy way to load all the libraries used here and ensure they're installed
#' at the start is to use the `pacman` library. The line below will check to see
#' if `pacman` is installed; if not, it will install it.

if (!require("pacman")) install.packages("pacman")

# Now, load the relevant packages
p_load(
  # Data tidying and import packages
  tidyverse, 
  magrittr, 
  rio,
  broom,
  janitor,
  # Benford-related packages
  BenfordTests,
  boot,
  pcal,
  # Graphs and tables packages
  ggthemes,
  ggrepel,
  kableExtra, 
  here, # An easy way to navigate file directories
  install = TRUE # Attempt to install packages not found in the library already
)

# Set theme for graphs
theme_set(theme_tufte())
# Set seed 
set.seed(811133) # For replication
my_seed <- 811133

# Am I in the right directory?
here() # Should point to "Replication_BonConMosSim"

# Load useful functions ----------------------------------------------------

#' ### Load some useful functions 
#' 
#' Two of the tests of Benford's Law used in the paper come from the
#' `BenfordTests` package. However, we noticed that when calculating the
#' proportion of numeral *i* in digit *d*, the function divides the frequency of
#' *i* by n, but n can *include missing data*. Better to drop missing data, we
#' think. Accordingly, below we create a function `chisq_benftest`, which is 
#' literally the same thing as `BenfordTest::chisq.benftest` except that we have 
#' changed the calculation of n from `n <- length(first_digits)` to `n <-`
#' `length(first_digits[!is.na(first_digits)])`. Now, NA's are dropped from the
#' calculation of n. (This change doesn't affect the substantive results that we
#' report.)

# Function to calculate the chi-squared test
chisq_benftest <- 
  function (x = NULL, digits = 1, pvalmethod = "asymptotic", pvalsims = 10000) {
    if (!is.numeric(x)) {
      stop("x must be numeric.")
    }
    pvalmethod <- pmatch(pvalmethod, c("asymptotic", "simulate"))
    if (is.na(pvalmethod)) {
      stop("invalid 'pvalmethod' argument")
    }
    if ((length(pvalsims) != 1)) {
      stop("'pvalsims' argument takes only single integer!")
    }
    if ((length(digits) != 1)) {
      stop("'digits' argument takes only single integer!")
    }
    first_digits <- signifd(x, digits)
    n <- length(first_digits[!is.na(first_digits)]) # CHANGED LINE
    freq_of_digits <- table(c(first_digits, signifd.seq(digits))) - 
      1
    rel_freq_of_digits <- freq_of_digits/n
    rel_freq_of_digits_H0 <- pbenf(digits)
    chi_square <- n * sum((rel_freq_of_digits - rel_freq_of_digits_H0)^2/rel_freq_of_digits_H0)
    if (pvalmethod == 1) {
      pval <- 1 - pchisq(chi_square, df = length(signifd.seq(digits)) - 
                           1)
    }
    if (pvalmethod == 2) {
      dist_chisquareH0 <- simulateH0(teststatistic = "chisq", 
                                     n = n, digits = digits, pvalsims = pvalsims)
      pval <- 1 - sum(dist_chisquareH0 <= chi_square)/length(dist_chisquareH0)
    }
    RVAL <- list(statistic = c(chisq = chi_square), p.value = pval, 
                 method = "Chi-Square Test for Benford Distribution", 
                 data.name = deparse(substitute(x)))
    class(RVAL) <- "htest"
    return(RVAL)
  }


#' We do the same thing for the `BenfordTest::edist.benftest`, creating the
#' function `edist_benftest`.
# Function to calculate the Morrow (2014) Euclidean Distance measure.
edist_benftest <-  
  function (x = NULL, digits = 1, pvalmethod = "simulate", pvalsims = 10000) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }
  pvalmethod <- pmatch(pvalmethod, c("simulate"))
  if (is.na(pvalmethod)) {
    stop("invalid 'pvalmethod' argument")
  }
  if ((length(pvalsims) != 1)) {
    stop("'pvalsims' argument takes only single integer!")
  }
  if ((length(digits) != 1)) {
    stop("'digits' argument takes only single integer!")
  }
  first_digits <- signifd(x, digits)
  n <- length(first_digits[!is.na(first_digits)]) # CHANGED LINE
  freq_of_digits <- table(c(first_digits, signifd.seq(digits))) - 
    1
  rel_freq_of_digits <- freq_of_digits/n
  rel_freq_of_digits_H0 <- pbenf(digits)
  d_star <- sqrt(n) * sqrt(sum((rel_freq_of_digits - rel_freq_of_digits_H0)^2))
  if (pvalmethod == 1) {
    dist_d_star_H0 <- simulateH0(teststatistic = "edist", 
                                 n = n, digits = digits, pvalsims = pvalsims)
    pval <- 1 - sum(dist_d_star_H0 <= d_star)/length(dist_d_star_H0)
  }
  RVAL <- list(statistic = c(d_star = d_star), p.value = pval, 
               method = "Euclidean Distance Test for Benford Distribution", 
               data.name = deparse(substitute(x)))
  class(RVAL) <- "htest"
  return(RVAL)
}

#' Next, we work with the bootstrapping function `boot`, which we use to
#' bootstrapping 95% CI around the first digit means: It turns out, via
#' https://stackoverflow.com/questions/18341569/r-calculate-the-standard-error-using-bootstrap
#' that "If you'll look at the help for boot then you'll see that your function must
#' be able to receive the data and an index. So, you need to write your own
#' function." The simple function below accomplishes those tasks. It's taken
#' from the same web page above. The function automatically drops missing values.
get_boot_mean <- function(x, i){mean(x[i], na.rm = TRUE)}

# Load & clean the CDCR data  --------------------------------------------------

#' ### Load & clean the CDCR data
#' Load the data.
df_force <- import("03_CDCR_Uses_of_Force_2008-2018_v2.dta") 

#' Now, we create  a column indicating what type of institution each facility
#' is. See https://www.cdcr.ca.gov/adult-operations/ for details.
df_force %<>%
  mutate(institution_type = case_when(institution %in% c(
    "CIW", "FSP", "CHCF", "CMF", "CCWF") ~ "female",
    institution %in% c(
      "ASP", "CAL", "CEN", "CTF", "CVSP",
      "ISP", "MCSP", "PVSP", "SOL") ~ "gen_pop", 
      institution %in% c(
        "CAC", "CCI", "COR", "HDSP", "KVSP", 
        "LAC", 'PBSP', "SAC", "SATF", "SVSP") ~ "high_sec", 
        institution %in% c(
          "CCC", "CIM", "CMC", "CRC", "DVI", 
          "NKSP", "RJD", "SCC", "SQ", "WSP") ~ "reception", 
    institution == "VSPW" & year <= 2012 ~ "female", 
    institution == "VSPW" & year >= 2013 ~ "gen_pop"
  )
  )
   
# Our data stop in May of 2018. Since we aggregate the data by year, we drop
# all of 2018
df_force %<>% filter(year <= 2017)

# Create data frame of the Benford distribution -------------------------

#' ### Create data frame of the Benford distribution
#' Obviously, we'll be comparing the empirical distribution to Benford's, so we 
#' create the Benford distribution to facilitate that.
df_benford <- 
  data.frame(digit = 1:9) %>%
  mutate(benford = log10(1 + 1 / digit)) 

# View the Benford distributon
df_benford

# Create data frame with distributions in aggregate and by year --------

#' ### Create data frame comparing empirical distributions to Benford by year.

#' This table is actually Table C1 in the Appendix, but we also use it
#' elsewhere, so we create here at the start. Start by merging `df_benford` and
#' the empirical distribution of the first digit of the aggregated data.
df_props_year_combined <-
  df_benford %>%
  # `signifd` is from BenfordTests and extracts the first significant digit
  # `tabyl` just creates a table of the proportions of those digits
  bind_cols(tabyl(signifd(df_force$total, digits = 1), show_na = FALSE)[3]) %>%
  rename("all_years" = percent)

#' Now, we calculate the digit frequencies for each year between 2008-2017
#' separately. The outcome is a list and is given the "list" prefix accordingly.
list_props_year_separate <- 
  by(data = df_force, INDICES = as.factor(df_force$year),
     function(df_force) 
       tabyl(signifd(x = df_force$total, digits = 1), show_na = FALSE)[3])

#' Now, combine `df_props_year_combined` with `list_props_year_separate` into
#' one table. Do so by extracting the results from df_props_year into a data
#' frame.
df_props_year_combined %<>% 
  bind_cols(map_df(list_props_year_separate, function(x) magrittr::extract(x))) 

#' Round columns to the 3rd digit. 
df_props_year_combined %<>% mutate_at(2:13, round, 3)

#' View the data
df_props_year_combined
# Create data frame with distributions in aggregate and by type --------

#' ### Create data frame with distributions in aggregate and by type
df_props_type_combined <- 
  df_benford %>% 
  bind_cols(tabyl(signifd(df_force$total, digits = 1), show_na = FALSE)[3]) %>%
  rename("all_institutions" = percent)

#' Now, calculate the digit frequencies for each institution type separtely
#' The outcome is a list and is given the "list" prefix accordingly.
list_props_type_separate <- 
  by(data = df_force, INDICES = as.factor(df_force$institution_type),
     function(df_force) 
       tabyl(signifd(x = df_force$total, digits = 1), show_na = FALSE)[3])

#' Now, combine df_props_type_combined with list_props_type_combined into one 
#' table. Do so by extracting the results from df_props_type_combine into a data
#' frame. 
df_props_type_combined %<>% 
  bind_cols(map_df(list_props_type_separate, function(x) magrittr::extract(x))) 

#' Round columns to the 3rd digit. 
df_props_type_combined %<>% mutate_at(2:7, round, 3)

#' View the data
df_props_type_combined

#################################################################
##                           Figures                           ##
#################################################################

#' # Figures

# Figure 1: Digit Distribution for Full Sample ------------------------------
#' ### Figure 1: Digit Distribution for Full Sample

# Make Figure 1
plot_props_allyears <-
  df_props_year_combined %>%
  select(Digit = digit,
         Benford = benford,
         "Observed" = all_years) %>%
  pivot_longer(-Digit, names_to = "type", values_to = "prop") %>%
  ggplot(., aes(x  = Digit, y = prop)) +
  geom_line(aes(linetype = type)) +
  scale_x_continuous(breaks = 1:9) +
  scale_linetype_discrete(name = NULL) +
  labs(x = "Digit",
       y = "Proportion", 
       title = "Figure 1")

# View the plot
plot_props_allyears


# Figure 2: Digit Distributions By Year ---------------------

#' ### Figure 2: Digit Distributions by Year

# To make Figure 2, first reshape `df_prop_year_combined``, created above, from
# "wide" to "long"
df_props_year_combined_long <-
  pivot_longer(df_props_year_combined,
               -digit,
               names_to = "year",
               values_to = "prop") %>%
  filter(year != "all_years")


# Make the plot
plot_props_year_combined <-
  # We have to first drop the Benford distribution, as it'll become just another
  # facet instead of an overlay.  We'll bring it back in later.
  ggplot(data = subset(df_props_year_combined_long, year != "benford"),
         aes(x = digit)) +
  geom_line(aes(
    y = prop,
    group = year,
    linetype = "Observed"
  ),
  color = "black") +
  # Re-overlay the Benford distribution
  geom_line(aes(y = log10(1 + 1 / digit),
                linetype = "Benford"),
            color = "black") +
  facet_wrap(~ year) +
  scale_x_continuous(breaks = 1:9) +
  scale_linetype_manual(values = c("Observed" = "dashed",
                                   "Benford" = "solid"),
                        name = NULL) +
  labs(x = "Digit", y = "Proportion", title = "Figure 2")

# View the plot
plot_props_year_combined

# Figure 3: Digit Distributions By Institution Type ---------

#' ### Figure 3: Digit Distributions by Institution Type

# As we did with Figure 2, we start making Figure 3 by reshaping the
# `df_props_type_combined` data frame from "wide" to "long".
df_props_type_combined_long <-
  pivot_longer(df_props_type_combined,
               -digit,
               names_to = "type",
               values_to = "prop")
  
# Make the graph. **NOTE**: Use `ggrepel` to make the labels, but this requires
# making a new label column == 1 when digit == 1 and NA for all else.  Also
# there's no need to graph "all institutions" as that's the same information 
# that's in Figure 1.

plot_props_type_combined <-
  df_props_type_combined_long  %>%
  filter(type != "all_institutions") %>%
  mutate(
    label = if_else(digit == 1,
                    as.character(type), NA_character_),
    label = recode(
      label,
      gen_pop = "Gen Pop",
      female = "Female",
      high_sec = "High Sec",
      reception = "Reception",
      benford = "Benford"
    )
  ) %>%
  # Make the graph
  ggplot(.,
         aes(
           x = digit,
           y = prop,
           group = type,
           label = label
         )) +
  geom_line(aes(color = type, linetype = type),
            show.legend = FALSE) +
  # Add the labels
  geom_label_repel(
    nudge_x = 1,
    direction = "both",
    size = 3,
    segment.size = 0.25
  ) +
  scale_color_grey() +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Digit", y = "Proportion", title = "Figure 3")

# View the plot
plot_props_type_combined

#################################################################
##                           Tables                            ##
#################################################################

#' # Table 1
#' We build Table 1 in several separate steps.
# Table 1A: Chi-Squared Tests  -------------------

#' ### Table 1A: Calculate the $\chi^{2}$ test statistics

# First, calculate chi-squared statistics for the entire sample
chi_full <- chisq_benftest(x = df_force$total, digits = 1, "asymptotic")

# View results
chi_full

# Second, calculate chi-squared statistics for each year separately.
chi_year <- 
  by(data = df_force, INDICES = as.factor(df_force$year),
     function(x)
       chisq_benftest(x$total, digits = 1, "asymptotic"))

# View the results
chi_year

# Third, extract the chi_year results into a a single data frame. Then, append
# the year column to the end. Then, append the aggregated results from
# `chi_full` to the bottom row. Label the bottom row "all years" in the "year"
# column. To preserve space, select only the year and test statistic columns.
# Rename the test statistic "chi" for clarity when merging later.
df_chi_year <- 
  map_df(chi_year, function(x) magrittr::extract(x)) %>% # data frame of yearly results
  bind_cols(data.frame(year = rownames(chi_year))) %>% # add column with year identifiers
  bind_rows(data.frame((chi_full[1:3]))) %>% # add row for results when all years are combined 
  mutate(year = as.character(year), 
         year = ifelse(is.na(year), "All Years", year)) %>% # Label NA -> "All Years" in year column.
  select(year, "chi" = statistic) # select just the necessary columns

# View results
df_chi_year

# Table 1B: Cho and Gaines Euclidean Distance  --------

#' ### Table 1B: Calculate Cho & Gaines Euclidean Distance tests

#' Recall, that we've already created a table that calculates the digit
#' proportions for every year. It is `df_props_year_combined`. So, we simply
#' need to run the relevant test on every column of that data frame.
df_ed_year <- 
  map_df(df_props_year_combined, function(x)
    (sqrt(sum((x - df_props_year_combined$benford) ^ 2
    ))) / 1.036)

#' Format `df_ed_year` for later merging by reshaping the data frame to "long"
#' format, selecting the relevant variables, renaming stat as "cg_d" for clarity
#' when merging later, rename the "all_years" observation to "All Years".
df_ed_year %<>% 
  gather(key = year, value = stat, 3:13) %>% 
  select(year, "cg_d" = stat) %>% 
  mutate(year = ifelse(year == "all_years", "All Years", year))

# View the data
df_ed_year

# Table 1C: Morrow Euclidean Distance  -----------

#' ### Table 1C: Calculate Morrow Euclidean Distance tests
# First, calculate Morrow's ED test for the full sample
morrow_full <- edist_benftest(df_force$total, digits = 1)

# View the results
morrow_full

# Second, calculate Morrow's ED test for each year
morrow_year <-
  by(data = df_force, INDICES = as.factor(df_force$year),
     function(x)
       edist_benftest(x$total, digits = 1))

# View the results
morrow_year

# Extract the yearly results into a data frame. Append the results from
# `morrow_full` to the bottom row. Label the "all years" observation in the
# "year" column to "All Years". Select the relevant variables and rename
# statistic to "morrow" for clarity when merging later.
df_morrow_year <- 
  map_df(morrow_year, function(x) magrittr::extract(x)) %>% 
  bind_cols(data.frame(year = rownames(morrow_year))) %>% 
  bind_rows(data.frame((morrow_full[1:3]))) %>% 
  mutate(year = as.character(year), 
         year = ifelse(is.na(year), "All Years", year)) %>% 
  select(year, "morrow" = statistic)

# View the results
df_morrow_year

# Table 1D: Nonparametric bootstap  --------------

#' ### Table 1D: Nonparametric bootsrap results

# Run the bootstrap on the full sample
# Set the seed first
set.seed(my_seed)
boot_full <- tidy(
  boot(
    data = df_force$firstdigit[df_force$firstdigit > 0],
    R = 1000,
    statistic = get_boot_mean
  ),
  conf.int = TRUE,
  conf.method = "basic",
  conf.level = 0.95
)

# View the results
boot_full

# Run the bootstrap by year
set.seed(my_seed)
boot_year <-
  by(data = df_force, INDICES = as.factor(df_force$year),
     function(x)
       tidy(
         boot(
           data = x$firstdigit[x$firstdigit > 0],
           R = 1000,
           statistic = get_boot_mean
         ),
         conf.int = TRUE,
         conf.method = "basic",
         conf.level = 0.95
       ))

# View the results
boot_year

#' Format for later merging. Extract the yearly results into a data frame.
#' Append the results from chi_full to the bottom row. Label the "all years"
#' observation in the "year" column.
df_boot_year <-
  map_df(boot_year, function(x)
    magrittr::extract(x)) %>%
  bind_cols(enframe(rownames(boot_year), value = "year")) %>%
  bind_rows(boot_full) %>%
  mutate(year = ifelse(is.na(year), "All Years", year))

#' For the final table, combine "conf.low" and "conf.high" into one cell.
df_boot_year %<>% 
  mutate_at(vars(conf.low, conf.high), round, 3) %>% 
  unite(ci, conf.low, conf.high, sep = ", ") %>% 
  mutate_if(is.numeric, round, digit = 3)

#' Extract the relevant content for Table 3. Rename statistic as "boot".
df_boot_year %<>%
  select(-c(bias, std.error, name)) %>% 
  rename("boot" = statistic) %>% 
  select(year, boot, ci)

# View the results
df_boot_year

# Table 1: Make table  -----------------------------------------------------

#' ### MAKE TABLE 1
#' Merge all the tests into one data frame. Make "year" a factor variable and 
#' make "All Years" the base category.  This allows sorting so that All Years is 
#' the first observation. Round all numeric columns to 2 digits.
table_props_year_combined <-  
  df_chi_year %>% 
  full_join(df_ed_year, by = "year") %>% 
  full_join(df_morrow_year, by = "year") %>% 
  left_join(df_boot_year, by = "year") %>% 
  mutate(year = fct_relevel(year, "All Years")) %>% 
  arrange(year) %>% 
  mutate_if(is.numeric, round, 2)

#' Add number of observations
table_props_year_combined <-
  table_props_year_combined %>%
  mutate(year = as.character(year)) %>%
  # Now, add a column that is a count of observations for each year
  left_join(
    df_force %>%
      filter(!is.na(total)) %>%
      tabyl(year) %>% # Calculate the number of observations per year
      magrittr::extract(1:2) %>% # extract the first two columns (year and count)
      mutate(year = as.character(year)), # make year a character variable for merging
    by = "year" # merge by year
  ) %>%
  mutate(n = ifelse(is.na(n), 4068, n)) # fill in the missing value for "All Years"

#' **Finally, we can view Table 1**.
kbl(
  table_props_year_combined,
  escape = FALSE,
  booktabs = TRUE,
  linesep = "",
  caption = "Table 1: Tests of Conformity to Benford's Law By Year"
) %>%
  kable_styling(full_width = FALSE) %>% 
  kable_minimal() %>%
  column_spec(6, color =c(rep("black", 9), "red", "red")) %>% 
  column_spec(2, color =c(rep("black", 7), "red", "black", "red", "red")) %>% 
  column_spec(3, color =c(rep("black", 9), "red", "red")) %>% 
  column_spec(4, color =c(rep("black", 9), "red", "red")) %>% 
  add_header_above(c(" " = 4, "Digit Mean Test" = 2, ""), bold = TRUE) %>%
  footnote(
    general = "Cells display test statistics in the $\\chi^2$ and $d^{*}_N$ columns, 
    proportions in the $d^*$ column, and the first digit mean and bootstrapped confidence
    interval in the Digit Mean Test columns. Values in red font indicate results 
    consistent with the data being distributed according to 
    Benford's Law.", 
    threeparttable = T,
    escape = FALSE
  )

# Table 2: Institution Type ------------------------
#' # Table 2
#' To make Table 2, we repeat the process used to make Table 1, but for
#' institution type.  We'll run the code here with less commentary.
#' 
# Chi-squared test by institution by type
df_chi_type <- 
  by(data = df_force, INDICES = as.factor(df_force$institution_type),
     function(x)
       chisq_benftest(x$total, digits = 1, "asymptotic")) 

# Extract results into a data frame and clean
df_chi_type <- 
  map_df(df_chi_type, function(x)
  magrittr::extract(x)) %>%
  bind_cols(data.frame(institution_type = rownames(df_chi_type))) %>%
  mutate_if(is.numeric, round, 3) %>%
  select("chi" = statistic, institution_type)

# Cho & Gaines ED test by institution type.
df_ed_type <- 
  map_df(df_props_type_combined, function(x)
  (sqrt(sum((x - df_props_type_combined$benford) ^ 2 ))) / 1.036) %>%
  gather(key = institution_type, value = stat, 4:7) %>%
  select(institution_type, "cg_d" = stat)

# Morrow's ED test by institution type.
df_morrow_type <-
  by(data = df_force, INDICES = as.factor(df_force$institution_type),
     function(x)
       edist_benftest(x$total, digits = 1)) 

# Extract the Morrow results into a data frame and clean

df_morrow_type <- 
  map_df(df_morrow_type, function(x) magrittr::extract(x)) %>% 
  bind_cols(data.frame(institution_type = rownames(df_morrow_type))) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  select("morrow" = statistic, institution_type)

# Nonparametric bootstrap by institution type
set.seed(my_seed)
df_boot_type <- 
  by(data = df_force, INDICES = as.factor(df_force$institution_type),
     function(x)
       tidy(
         boot(
           data = x$firstdigit[x$firstdigit > 0],
           R = 1000,
           statistic = get_boot_mean
         ),
         conf.int = TRUE,
         conf.method = "basic",
         conf.level = 0.95
       ))

# Extract the bootstrapped results into a data frame and clean
set.seed(my_seed)
df_boot_type <- 
  map_df(df_boot_type, function(x) magrittr::extract(x)) %>%
  bind_cols(enframe(rownames(df_boot_type), value = "institution_type")) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  unite(ci, conf.low, conf.high, sep = ", ") %>% 
  mutate_if(is.numeric, round, digit = 3) %>% 
  select(institution_type, boot = statistic, ci) 
  
# Create the table
table_props_type_combined <-
  df_chi_type %>% 
  full_join(df_ed_type, by = "institution_type") %>% 
  full_join(df_morrow_type, by = "institution_type") %>% 
  full_join(df_boot_type, by = "institution_type") %>% 
  select(institution_type, everything()) %>% 
  mutate_if(is.numeric, round, 2)

# Add number of observations
table_props_type_combined <- 
  table_props_type_combined %>% 
   left_join(
    df_force %>% 
      filter(!is.na(total)) %>% 
      tabyl(institution_type) %>% 
      magrittr::extract(1:2), 
    by = "institution_type"
  ) 

# Rename institution type to sentence case.
table_props_type_combined$institution_type <-
  recode(
  table_props_type_combined$institution_type,
  female = "Female",
  gen_pop = "Gen Pop",
  high_sec = "High Sec",
  reception = "Reception"
)

# Finally, Table 2
kbl(
  table_props_type_combined,
  escape = FALSE,
  booktabs = TRUE,
  linesep = "", 
  caption = "Table 2: Tests of Conformity to Benford's First Digit Distribution"
) %>% 
  kable_styling(full_width = FALSE) %>% 
  kable_minimal() %>% 
  add_header_above(c(" " = 4, "Digit Mean Test" = 2, ""), bold = TRUE) %>%
  footnote(
    general = "Cells display test statistics in the $\\chi^2$ and $d^{*}_N$ columns, 
    proportions in the $d^*$ column, and the first digit mean and bootstrapped confidence
    interval in the Digit Mean Test columns. Values in red font indicate results 
    consistent with the data being distributed according to 
    Benford's Law.", 
    threeparttable = T,
    escape = FALSE
  )

##################################################################
##                          Appendices                          ##
##################################################################
#' # Appendices



# Table A1: Total Reported Incidents by Type ------------------------------
#' ### Table A1: Total Reported Incidents by Type
# First, make a function to create cumulative sums that skip over missing values
# https://stat.ethz.ch/pipermail/r-help//2009-November/412205.html
cum_na <- function(x) {
  x[which(is.na(x))] <- 0
  return(cumsum(x))
}

# Read in a "clean" version of the data just to make sure.
df_force_clean <- import("03_CDCR_Uses_of_Force_2008-2018_v2.dta") 

# Select the variables of interest for the table
df_force_incident <- 
  df_force_clean %>%
  select(oc,
         physical,
         launcher,
         baton,
         cn,
         other,
         warning,
         hydro,
         noncon,
         shots)

# Create a table of cumulative summaries of the counts of each of the incidents, making
# sure to "skip" over missing values. Once, we've created the cumulative sums, 
# we'll grab the last row of each column - the cumulative sum. Then, rename
# the columns. Then, reshape the data from wide to long and re-rename the new
# columns.

tab_incident <- 
  df_force_incident %>% 
  map_df(cum_na) %>%
  tail(1) %>%
  rename("Use of Oleoresin Capsicum (Pepper Spray)" = oc, 
         "Physical Force" = physical, 
         "Discharge of a 37mm and/or 40mm Launcher" = launcher, 
         "Use of a Baton" = baton, 
         "Use of Chloroacetophenone (Tear Gas or Chemical Cace)" = cn, 
         "Other Force Options Not Otherwise Mentioned" = other, 
         "Firing of Semi-Automatic Rifle Shots (Warning)" = warning, 
         "Use of High-Pressure Water Hose System" = hydro, 
         "Use of Non-Conventional Force" = noncon, 
         "Firing of Semi-Automatic Rifle Shots (Contact Intended)" = shots) %>% 
  pivot_longer(cols = 1:10) %>% 
  rename("Total Incidents" = value, 
         "Type of Force" = name)

# # Next, grab the "zero" incidents. For these, we filter the data, keeping any
# row where any column equals 0. Then, tabulate the number of observations, 
# keeping the first column (the count of 0 incidents). Bind that column to the 
# `tab_incident` table and rearrange the columns.
tab_incident <- 
  df_force_incident %>% 
  filter_all(any_vars(. == 0)) %>% 
  map_df(table) %>% 
  select("Zero Incidents" = `0`) %>% 
  bind_cols(tab_incident) %>% 
  relocate(`Type of Force`, `Total Incidents`, `Zero Incidents`)

# View the table
kbl(tab_incident, 
    caption = "Table A1: Total Reported Incidents by Type") %>% 
  kable_styling() %>% 
  kable_minimal()


# Figure A1: Uses of Force Over Time By Institution ----------------------------
#' ### Figure A1: Uses of Force Over Time By Institution

plot_force_institution_year_v2 <- 
  df_force %>% 
  group_by(institution, year) %>% 
  summarize(total_year_sum = sum(total)) %>% 
  ggplot(., aes(x = as.factor(year), y = total_year_sum, group = institution)) + 
  geom_line() + 
  labs(x = "Year", y = "Yearly Sum of Total Uses of Force") + 
  facet_wrap(~ institution) + 
  scale_x_discrete(breaks = c(2008, 2012, 2016)) + 
  labs(title = "Table 3")

# View the plot
plot_force_institution_year_v2

# Table B1: List of Institutions by Institution Type ----------------------

#' ### Table B1: List of Institutions by Institution Type
#' For data, see https://www.cdcr.ca.gov/adult-operations/

# Make the data frame
df_names <- data.frame(
  stringsAsFactors = FALSE,
  institution_name = c(
    "Avenal State Prison",
    "California City Correctional Facility (CAC)",
    "California Correctional Center (CCC)",
    "California Correctional Institution",
    "California Health Care Facility, Stockton",
    "California Institution for Men (CIM)",
    "California Institution for Women (CIW)",
    "California Men's Colony (CMC)",
    "California Medical Facility (CMF)",
    "California Rehabilitation Center (CRC)",
    "California State Prison, Corcoran (COR)",
    "California State Prison, Los Angeles County (LAC)",
    "California State Prison, Sacramento (SAC)",
    "California State Prison, Solano (SOL)",
    "California Substance Abuse Treatment Facility and State Prison, Corcoran (SATF-CSP,
                 Corcoran)",
    "Calipatria State Prison (CAL)",
    "California State Prison, Centinela (CEN)",
    "Central California Women's Facility (CCWF)",
    "Chuckawalla Valley State Prison (CVSP)",
    "Correctional Training Facility (CTF)",
    "Deuel Vocational Institution (DVI)",
    "Folsom State Prison (FSP)",
    "High Desert State Prison (HDSP)",
    "Ironwood State Prison (ISP)",
    "Kern Valley State Prison (KVSP)",
    "Mule Creek State Prison (MCSP)",
    "North Kern State Prison (NKSP)",
    "Pelican Bay State Prison (PBSP)",
    "Pleasant Valley State Prison (PVSP)",
    "Richard J. Donovan Correctional Facility (RJD)",
    "Salinas Valley State Prison (SVSP)",
    "San Quentin State Prison (SQ)",
    "Sierra Conservation Center (SCC)",
    "Valley State Prison (VSP)",
    "Wasco State Prison-Reception Center (WSP)",
    "California Out of State Correctional Facility",
    "La Palma Correctional Center"
  ),
  institution = c(
    "ASP",
    "CAC",
    "CCC",
    "CCI",
    "CHCF",
    "CIM",
    "CIW",
    "CMC",
    "CMF",
    "CRC",
    "COR",
    "LAC",
    "SAC",
    "SOL",
    "SATF",
    "CAL",
    "CEN",
    "CCWF",
    "CVSP",
    "CTF",
    "DVI",
    "FSP",
    "HDSP",
    "ISP",
    "KVSP",
    "MCSP",
    "NKSP",
    "PBSP",
    "PVSP",
    "RJD",
    "SVSP",
    "SQ",
    "SCC",
    "VSPW",
    "WSP",
    "OSCF",
    "LPCC"
  )
)

# Remove parentheses and content within from "inst_name"
df_names$institution_name <- 
  gsub("\\s*\\([^\\)]+\\)","",as.character(df_names$institution_name))

# Merge df_names with df_force
df_force  %<>%  left_join(df_names, by = "institution")

# Create a data frame of unique institution names
table_names <- df_force %>% 
  select(institution_name, institution_type) %>% 
  group_by(institution_name, institution_type) %>% 
  distinct()
  
# Concatenate all the institution names for each group. Then, select the unique
# rows.
table_names %<>% 
  group_by(institution_type) %>% 
  mutate("names_concat" = paste0(institution_name, collapse = "; ")) %>% 
  select(-institution_name) %>% 
  distinct()

# Rename institution_type to sentence case.
table_names$institution_type <- table_names %$% 
  recode(
    institution_type,
    gen_pop = "Gen Pop",
    female = "Female",
    high_sec = "High Sec",
    reception = "Reception")

# View the table
kbl(table_names, 
    caption = "Table B1: List of Institutions by Institution Type") %>% 
  kable_styling(font_size = 12) %>% 
  kable_minimal()

# Table C1: Benford Law & Proportions in Aggregate and by Year --------------------

#' ### Table C1: Benford Law & Proportions in Aggregate and by Year
#' This is just the `df_prop_year_combined` table we created at the very
#' start

kbl(df_props_year_combined, 
    caption = "Table C1: Distribution of First Digits of Total Uses of Force in California Prisons, Compared to Benford’s Law") %>% 
  kable_styling(full_width = FALSE, bootstrap_options = "striped") %>% 
  kable_minimal()

# Table D1: Additional Tests ----------------------------------------------

#' ### Table D1: Additional Tests

table_robust <- 
  table_props_year_combined %>%
  mutate(year = as.character(year)) %>% 
  bind_rows(table_props_type_combined) %>% 
  mutate(year = ifelse(is.na(year), institution_type, year)) %>% 
  select(ID = year, chi) %>% 
  mutate(PV =  pchisq(chi, df = 8, lower = FALSE), # frequentist p-value from chi-squared stat
         ULB = pcal(PV, prior_prob = 0.5), # Calculate Ultimate Lower Bound
         bayes_fct = bcal(PV), # Use Bayes Factors
         "FDR p" = p.adjust(PV, method = "fdr")) # Correct for false discovery

# View results
kbl(table_robust, 
    digits = 2, 
    caption = "Table D1: Additional Tests of Conformity to Benford’s Law By Year and Institution Type") %>% 
  kable_styling(full_width = FALSE) %>% 
  kable_minimal()


# Session Info ------------------------------------------------------------

sessionInfo()
