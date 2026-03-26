################################################################################
##    Replication script for:
##      How to detect heterogeneity in conjoint experiments
##
##    Authors:
##      Thomas Robinson (LSE; t.robinson7@lse.ac.uk)
##      Raymond Duch (Nuffield College, Oxford)
##      
################################################################################
##
##    Please see README for replication environment, package versions,
##    and runtimes.
##
################################################################################

#### 0. Package dependencies ####
library(tidyverse)
library(cowplot)
library(ggpubr)
library(randomForestSRC)
library(rpart)
library(rpart.plot)
library(broom)
library(xtable)
library(cregg)
library(grf)

## ggrgl requires non-CRAN installs of the following dependencies:
# remotes::install_github('coolbutuseless/devout')
# remotes::install_github('coolbutuseless/devoutrgl')
# remotes::install_github('coolbutuseless/triangular')
# remotes::install_github('coolbutuseless/snowcrash')
# remotes::install_github('coolbutuseless/cryogenic')
# remotes::install_github('coolbutuseless/ggrgl', ref='main')
library(ggrgl)

# install.packages('cjbart')
library(cjbart)

# Set random number generator (RNG) function
# Note: Changing the RNG is necessary to ensure replicability across Windows and
#       Unix-like systems when using the BART package

RNGkind("L'Ecuyer-CMRG")
set.seed(89)

t_start <- Sys.time()

#### Figure 1 ####

# Please see package requirements above

# Load vaccine conjoint data
vacc_data <- readRDS("data/nbh_clean_conjoint_global.rds") %>%
  select(-all_of(c("person", "ans", "candidate","weights")))

## Panel (a)

# Function to estimate subset models
est_mod <- function(country_level = NA, ideology_level = NA, text_label) {
  
  reg_data <- vacc_data
  
  if (!(is.na(country_level))) {
    
    reg_data <- filter(reg_data, country == country_level)
    
  }
  
  if (!(is.na(ideology_level))) {
    if (ideology_level == "Left") {
      reg_data <- filter(reg_data, ideology <= 5)
    } else if (ideology_level == "Right") {
      reg_data <- filter(reg_data, ideology > 5)
    } else {
      stop("Incorrect ideology value provided.")
    }
  }
  
  coef <- reg_data %>%
    mutate(income = relevel(income, ref = "Average income level")) %>%
    glm(select ~ vulnerability + transmission + income + occupation + age_category,
        data = .) %>%
    summary(.) %>%
    .$coefficients
  
  out <- coef[rownames(coef) == "incomeLowest 20% income level",c("Estimate","Std. Error")]
  
  return(out)
}

# Generate subsets
data_subsets <- data.frame(
  country = c(NA, unique(vacc_data$country), NA, NA),
  ideology_level = c(NA, rep(NA, length(unique(vacc_data$country))), "Left", "Right"),
  text_label = c("All", unique(vacc_data$country), "Left/Centre", "Right")
)

# Run models and bind
mod_results <- cbind(data_subsets,
                     do.call("rbind", pmap(data_subsets, est_mod))) %>%
  mutate(fac = case_when(!is.na(country) ~ "Income",
                         !is.na(ideology_level) ~ "Ideology",
                         TRUE ~ "All"),
         text_label = factor(text_label,
                             levels = c("All",
                                        "Left/Centre", "Right",
                                        sort(unique(vacc_data$country)))))

# Generate panel a
panel_a <- ggplot(mod_results %>% filter(fac %in% c("Ideology","All")),
                  aes(x = Estimate, y = text_label, color = fac)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Estimate - 1.96*`Std. Error`,
                     xmax = Estimate + 1.96*`Std. Error`),
                 height = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_discrete(limits=rev) +
  scale_color_manual(values = c("gray14","coral1")) +
  labs(x = "AMCE",
       y = "Data subset") +
  theme_minimal() +
  theme(strip.text.y = element_blank(),
        legend.position = "none")

plot(panel_a)

ggsave(plot = panel_a, filename = "figures/figure1_a.png",
       width = 8, height = 2, dpi = 300)

## Panel (b)

# Visualise differences using 3d tiles
panel_b <- vacc_data %>%
  filter(income == "Lowest 20% income level",
         age < 100) %>%
  group_by(country, ideology) %>%
  summarise(prop = mean(select, na.rm = TRUE),
            obs = n()) %>%
  drop_na(country, ideology) %>%
  ggplot(aes(x = ideology, y = country, fill = prop)) +
  geom_tile_z(aes(z = obs), extrude = TRUE,
              extrude_z = 0.01) +
  scale_fill_gradient2(low = "dodgerblue3", mid = "grey", high = "firebrick1", midpoint = 0.5) +
  labs(x = "Ideology",
       y = "Country",
       fill = "Marginal mean") +
  theme_ggrgl() +
  theme(legend.position = "bottom")

# Generate 3d rotation matrix for replicability
view_matrix <- matrix(c(0.994128585, -0.03148808, 0.1035212,    0,
                        -0.009477008,  0.92770642, 0.3731898,    0,
                        -0.107788339, -0.37197983, 0.9219613,    0,
                        0,  0, 0, 1), ncol = 4, byrow = TRUE)

# Plot
devoutrgl::rgldev(filename = 'figures/figure1_b.png',
                  dpi = 300,
                  fov=30,
                  zscale = 3,
                  zoom = 0.8,
                  close_window = TRUE,
                  show_window = TRUE,
                  view3d_args = list(userMatrix = view_matrix))

panel_b
invisible(dev.off())

## Get counts

# No. of obs
vacc_data %>% 
  filter(income == "Lowest 20% income level",
         age < 100) %>% 
  group_by(ind_inc, ideology, income) %>% 
  summarise(prop = mean(select, na.rm = TRUE),
            obs = n()) %>% 
  .$obs %>% sum(.)

# No. of participants
vacc_data %>% 
  filter(income == "Lowest 20% income level",
         age < 100) %>% 
  .$id %>% unique(.) %>% length(.)

#### Figure 2 ####

set.seed(89)

# Function for simulating subject preferences and resulting conjoint data
preference_sim <- function(subjects = 500, rounds = 5, seed = 89) {
  
  set.seed(seed)
  
  # Define individual-level utilities
  utilities <- data.frame(id = 1:subjects,
                          c1 = rbinom(subjects, 1, 0.5),
                          c2 = runif(subjects, -1,1)) %>% 
    mutate(X1_1 = ifelse(c1 == 1, 
                         rnorm(subjects, 1, sd = 1),
                         rnorm(subjects, -1, sd = 1)),
           X2_1 = rnorm(subjects, abs(c2-0.2), sd = 1),
           X3_1 = rnorm(subjects, 0,sd = 0.5))
  
  # Use utilities to determine hypothetical conjoint behaviour
  conjoint_data <- data.frame(id = rep(1:subjects, each = rounds*2),
                              round = rep(1:rounds, each = 2),
                              profile = 1:2,
                              X1 = rbinom(subjects*rounds*2, 1, 0.5),
                              X2 = rbinom(subjects*rounds*2, 1, 0.5),
                              X3 = rbinom(subjects*rounds*2, 1, 0.5)) %>% 
    left_join(utilities, by = "id") %>% 
    mutate(U = X1*X1_1 + X2*X2_1 + X3*X3_1 + rnorm(subjects*rounds*2, 0,0.0005)) %>% 
    group_by(id, round) %>%
    mutate(Y = ifelse(U == max(U),1,0)) %>% 
    ungroup()
  
  # Mutate data for cleaner presentation
  train_data <- conjoint_data %>% select(id, X1, X2, X3, c1, c2, Y) %>% 
    mutate(X1 = ifelse(X1 == 1, "A1: Binary heterogeneity (c1)","a"),
           X2 = ifelse(X2 == 1, "A2: Interval heterogeneity (c2)","c"),
           X3 = ifelse(X3 == 1, "A3: Random heterogeneity","e"))
  
  # Run heterogeneity model
  pref_mod <- cjbart(train_data, Y = "Y", id = "id")
  
  # Generate IMCES
  het_detect <- IMCE(train_data, pref_mod, 
                     attribs = c("X1","X2","X3"), 
                     ref_levels = c("a","c","e"),
                     cores = 8)
  
  return(het_detect)
  
}

# Run simulation example
example_sim <- preference_sim(seed = 89)

# Generate plot
plot_c1 <- plot(example_sim) +
  aes(color = as.factor(c1)) +
  facet_wrap(~level, ncol = 2) +
  scale_color_manual(values = c("dodgerblue2","firebrick2")) +
  labs(x = "", color = expression(c[1])) +
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

ggsave(plot = plot_c1, "figures/figure_2.pdf", width = 7, height = 3)

t_f2 <- Sys.time()

#### Figure 3 ####

# Define colours for plot
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Hainmueller coef. comparison
data("immigration")
immigration$LangPos <- NULL
immigration$PriorPos <- NULL
immigration$profile <- NULL
immigration$contest_no <- NULL

immig_attribs <- c("Education","Gender","CountryOfOrigin",
                   "ReasonForApplication","JobPlans","JobExperience","Job",
                   "PriorEntry","LanguageSkills")

immig_refs <- sapply(immig_attribs, function (x) levels(immigration[[x]])[1])

set.seed(89)

immig_model <- cjbart::cjbart(data = immigration,
                              Y = "ChosenImmigrant",
                              id = "CaseID")

immig_amces <- AMCE(data = immigration,
                    model = immig_model,
                    attribs = immig_attribs,
                    ref_levels = immig_refs,
                    cores = 8)

immig_lpm <- lm(immigration,
                formula = paste0("ChosenImmigrant ~ ", paste0(immig_attribs, collapse = " + "))) %>% 
  summary(.) %>% 
  .$coefficients %>% 
  as.data.frame(.) %>% 
  mutate(level = row.names(.),
         feature = NA) %>% 
  filter(level != "(Intercept)") %>% 
  rename(estimate = Estimate)

# String formatting for presentation
for (i in immig_attribs) {
  immig_lpm$feature <- ifelse(str_starts(immig_lpm$level, i), 
                              i, 
                              immig_lpm$feature)
  immig_lpm$level <- ifelse(str_starts(immig_lpm$level, i), 
                            sub(i,"",immig_lpm$level), 
                            immig_lpm$level)
  
}

immig_lpm$feature <- gsub("([a-z])([A-Z])","\\1 \\2",immig_lpm$feature)

# Generate intervals
immig_lpm$moe <- immig_lpm$`Std. Error`*1.96
immig_lpm$lpm_intvl <- paste0("[", 
                              format(round(immig_lpm$estimate - immig_lpm$moe,2), 
                                     nsmall = 2, trim = TRUE),
                              ",",
                              format(round(immig_lpm$estimate + immig_lpm$moe,2), 
                                     nsmall = 2, trim = TRUE),
                              "]")

cjbart_immig <- immig_amces$amces %>% 
  mutate(cjbart_intvl = paste0("[",
                               format(round(as.numeric(AMCE_lower), 2), nsmall = 2, trim = TRUE),
                               ",",
                               format(round(as.numeric(AMCE_upper), 2), nsmall = 2, trim = TRUE),
                               "]"))

# Comparison table used later for Appendix tables
comparison <- merge(
  immig_lpm[,c("feature","level","estimate", "lpm_intvl")], 
  cjbart_immig[,c("level","AMCE", "cjbart_intvl")]
) %>% 
  mutate(study = "Hainmueller et al (2014)")

colnames(comparison) <- c("level","feature","lpm","lpm_intvl","cjbart","cjbart_intvl","study")

## Vaccine AMCEs

vacc_data <- readRDS("data/nbh_clean_conjoint_global.rds") %>%
  select(-all_of(c("person", "ans", "candidate","weights")))

het_mod <- cjbart(vacc_data, Y = "select", id  = "id", cores = 8, seed = 89)

gc()

t2 <- Sys.time()
vacc_imces <- IMCE(vacc_data, het_mod,
                   attribs = c("vulnerability","transmission","income","occupation","age_category"),
                   ref_levels = c("Average risk of COVID-19 death",
                                  "Average risk of catching and transmitting the COVID-19 virus",
                                  "Average income level",
                                  "Not working",
                                  "25 years old"), cores = 8) # was 2
t3 <- Sys.time()

imce_sched <- vacc_imces$imce

vacc_amces <- AMCE(vacc_data, 
                   het_mod,
                   attribs = c("vulnerability","transmission","income","occupation","age_category"),
                   ref_levels = c("Average risk of COVID-19 death",
                                  "Average risk of catching and transmitting the COVID-19 virus",
                                  "Average income level",
                                  "Not working",
                                  "25 years old"), 
                   cores = 8) # was 2

cjbart_vacc <- vacc_amces[[1]] %>% 
  mutate(cjbart_intvl = paste0("[",
                               format(round(as.numeric(AMCE_lower), 2), nsmall = 2, trim = TRUE),
                               ",",
                               format(round(as.numeric(AMCE_upper), 2), nsmall = 2, trim = TRUE),
                               "]")) %>% 
  rename(cjbart = AMCE)


vacc_lm <- lm(vacc_data %>% mutate(income = relevel(income, ref = "Average income level")),
              formula = "select ~ vulnerability + transmission + income + occupation + age_category") %>% 
  summary(.) %>% 
  .$coefficients %>% 
  as.data.frame(.) %>% 
  mutate(level = row.names(.)) %>%
  mutate(feature = case_when(str_starts(level, "vulnerability") ~ "Vulnerability",
                             str_starts(level, "transmission") ~ "Transmission",
                             str_starts(level, "income") ~ "Income",
                             str_starts(level, "occupation") ~ "Occupation",
                             str_starts(level, "age_category") ~ "Age")) %>% 
  filter(!is.na(feature)) %>% 
  mutate(level = case_when(str_starts(level, "vulnerability") ~ sub("vulnerability","",level),
                           str_starts(level, "transmission") ~ sub("transmission","",level),
                           str_starts(level, "income") ~ sub("income","",level),
                           str_starts(level, "occupation") ~ sub("occupation","",level),
                           str_starts(level, "age_category") ~ sub("age_category","",level)))

vacc_lm$moe <- vacc_lm$`Std. Error`*1.96
vacc_lm$lpm_intvl <- paste0("[", 
                            format(round(vacc_lm$Estimate - vacc_lm$moe,2), 
                                   nsmall = 2, trim = TRUE),
                            ",",
                            format(round(vacc_lm$Estimate + vacc_lm$moe,2), 
                                   nsmall = 2, trim = TRUE),
                            "]")

vacc_table <- vacc_lm %>% 
  rename(lpm = Estimate) %>% 
  left_join(cjbart_vacc, by = "level") %>%
  mutate(study = "Duch et al (2021)") %>% 
  select(feature, level, lpm, lpm_intvl, cjbart, cjbart_intvl, study) %>% 
  mutate(`Difference (% of Parametric)` = 100*(cjbart-lpm)/lpm)

# Merge for figures

ggList <- lapply(list(comparison, vacc_table), function(i) {
  ggplot(i, aes(x = lpm, y = cjbart, color = feature)) +
    facet_grid(. ~ study) +
    geom_abline(linetype = "dashed", color = "grey") +
    geom_point(alpha=0.8) +
    scale_color_manual(values = cbPalette) +
    labs(x = "Conventional AMCE",
         y = ifelse("Hainmueller et al (2014)" %in% i$study,"BART-derived AMCE","")) +
    guides(color=guide_legend(nrow=3,byrow=TRUE, title = NULL))+
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text.x = element_text(
            size = 12, color = "black", face = "bold"
          ))
})

# Plot combined figures
plot_grid(plotlist = ggList, ncol = 2,
          align = 'hv')

ggsave("figures/figure_3.pdf", width = 10, height = 4)

t_f3 <- Sys.time()

#### Figure 4 ####

set.seed(89)

# VIMP analysis
full_results <- het_vimp(vacc_imces) 

# Tidy labels
covar_lookup <- data.frame(covar_name = c("country","age","gender","ideology",
                                          "ind_inc","education","hes_covid_2",
                                          "wtp_access","wtp_private",
                                          "int_pol_implem_6"),
                           covar_label = c("Country","Age","Gender","Ideology",
                                           "Income","Education","Hesitancy",
                                           "WTP Access","WTP Private",
                                           "Mandatory Vaccination"))

final_results <- full_results$results %>% 
  left_join(covar_lookup, by = c("covar" = "covar_name")) %>% 
  mutate(lab_text = ifelse(importance < 10,"",round(importance,2)),
         
         outcome = case_when(Level == "Moderate risk (Twice the average risk of catching and transmitting the COVID-19 virus)" ~ "Moderate risk of transmission",
                             Level == "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)" ~ "High risk of transmission",
                             TRUE ~ Level),
         Attribute = case_when(Attribute == "age_category" ~ "Age",
                               Attribute == "income" ~ "Income",
                               Attribute == "occupation" ~ "Occupation",
                               Attribute == "transmission" ~ "Transmit",
                               Attribute == "vulnerability" ~ "Death"))

ggplot(final_results, aes(x = covar_label, y = outcome, fill = importance)) +
  facet_grid(Attribute ~ ., space = "free", scales = "free", switch = "y") +
  geom_tile() +
  scale_fill_gradient(low="white", high="firebrick2", breaks = c(25,50,75,100)) +
  labs(x = "Subject covariate", y = "Attribute-level", fill = "Imp.") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle=45, vjust = 1, hjust = 1))

ggsave("figures/figure_4.pdf", dpi = 300, width = 10, height = 8)

t_f4 <- Sys.time()

#### Figure 5 ####

trial_data <- imce_sched %>% 
  rename(outcome = `65 years old`) %>% 
  select(c("outcome",
           "country",
           "age","gender" ,
           "ideology" ,
           "ind_inc" ,
           "education",
           "hes_covid_2",
           "wtp_access",
           "wtp_private",
           "int_pol_implem_6")) %>% 
  drop_na() %>% 
  mutate(country = case_when(country == "Australia" ~ "AUS",
                             country == "Brazil" ~ "BRA",
                             country == "Canada" ~ "CAN",
                             country == "Chile" ~ "CHL",
                             country == "China" ~ "CHN",
                             country == "Colombia" ~ "COL",
                             country == "France" ~ "FRA",
                             country == "India" ~ "IND",
                             country == "Italy" ~ "ITA",
                             country == "Spain" ~ "ESP",
                             country == "Uganda" ~ "UGA",
                             TRUE ~ country)) 

for (i in 1:length(names(trial_data))) {
  if (typeof(trial_data[,i]) == "character") {
    trial_data[,i] <- as.factor(trial_data[,i])
  }
}

single_tree <- rpart(outcome ~ ., data = trial_data,
                     method = "anova")

printcp(single_tree) # reports the optimal prunings by complexity parameter
prune_tree <- prune(single_tree, cp = 0.03)

par(xpd = TRUE)
png('figures/figure_5.png', res = 300, width = 18, height = 12, units = "cm", pointsize = 10)
rpart.plot(prune_tree, type = 1, digits = 2, clip.facs = TRUE)
dev.off()

t_f5 <- Sys.time()

#### Figure 6 ####

imce_sched$ideology_cat <- as.factor(imce_sched$ideology)
attribute <- "Lowest 20% income level"
covar = "ideology_cat"

plot_data <- imce_sched[!is.na(imce_sched[[covar]]) & !is.na(imce_sched[[attribute]]),]

plot_data <- plot_data[order(plot_data[[attribute]]),]
plot_data$effect_order <- 1:nrow(plot_data)

plot_data$imce <- plot_data[[attribute]]
plot_data$covar <- plot_data[[covar]]

conf_intvl_ideo <- vacc_imces$imce_lower %>% 
  select(id, `Lowest 20% income level`) %>% 
  
  left_join({vacc_imces$imce_upper %>% select(id, `Lowest 20% income level`)},
            by = "id")

colnames(conf_intvl_ideo) <- c("id","ideo_l","ideo_u")

plot_data <- left_join(plot_data, conf_intvl_ideo, by = "id")

cor.test(plot_data$`Lowest 20% income level`, plot_data$ideology)

effect_line <- ggplot(plot_data, aes(x = effect_order, y = imce)) +
  geom_line() +
  geom_ribbon(aes(ymin = ideo_l, ymax = ideo_u), fill = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = mean(plot_data[[attribute]]), 
             color = "dodgerblue",
             size = 0.2) +
  labs(x = "", y = "IMCE") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(plot_data))) +
  scale_y_continuous(limits = c(-0.025, 0.1))

covar_density <- ggplot(plot_data, aes(x = effect_order, fill = covar)) +
  geom_histogram(binwidth = 60,position="stack") +
  labs(x = "", y = "Density", color = str_to_title(covar), fill = "Ideology") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, nrow(plot_data)))

plot_grid(effect_line, covar_density, ncol = 1, align = "v", rel_heights = c(0.6,0.4))
ggsave("figures/figure_6.pdf", dpi=300, width = 7, height = 4)

t_f6 <- Sys.time()

#### Figure 7 ####

ols_mods <- vacc_data %>% 
  mutate(income = relevel(income, ref = "Average income level"))

# Run LPM for each subject in data
ols_regs <- lapply(unique(ols_mods$id), function (x) {
  
  df_i <- ols_mods[ols_mods$id == x,]
  reg_i <- tidy(lm(select ~ vulnerability + transmission + income + occupation + age_category, data = df_i))
  out_df <- reg_i[reg_i$term == "incomeLowest 20% income level",]
  out_df$id <- x
  out_df$ideology <- df_i$ideology[1]
  return(out_df)
}) %>% 
  do.call("rbind", .)

# Check implausible estimates
sum(is.na(ols_regs$estimate))
sum(abs(ols_regs$estimate) > 1, na.rm = TRUE)
sum(abs(ols_regs$estimate) > 2, na.rm = TRUE)
sum(abs(ols_regs$estimate) > 5, na.rm = TRUE)

ols_regs$implausible <- ifelse(abs(ols_regs$estimate) > 1, TRUE, FALSE)

# No. of IMCEs estimated
sum(!is.na(ols_regs$estimate) & !ols_regs$implausible)

# No. of parametrically estimated standard errors
sum(!is.na(ols_regs$estimate) & !is.na(ols_regs$std.error) & !ols_regs$implausible)

ols_regs_plot <- ols_regs %>% 
  filter(!is.na(ideology),
         !implausible) %>% 
  arrange(estimate) %>% 
  mutate(effect_order = 1:nrow(.))

# Assess correlation coefficient
cor.test(ols_regs_plot$estimate, ols_regs_plot$ideology)

effect_line <- ggplot(ols_regs_plot, aes(x = effect_order, y = estimate)) +
  geom_line() +
  # geom_ribbon(aes(ymin = ideo_l, ymax = ideo_u), fill = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # geom_hline(yintercept = mean(ideology_mod[["estimate"]], na.rm = TRUE), color = "dodgerblue") +
  labs(x = "", y = "IMCE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, nrow(ols_regs_plot)))

covar_density <- ggplot(ols_regs_plot, aes(x = effect_order, fill = as.factor(ideology))) +
  geom_histogram(binwidth = 60,position="stack") +
  labs(x = "", y = "Density", color = str_to_title("Ideology"), fill = "Ideology") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, nrow(ols_regs_plot)))

plot_grid(effect_line, covar_density, ncol = 1, align = "v", rel_heights = c(0.6,0.4))
ggsave("figures/figure_7.pdf", dpi=300, width = 7, height = 4)

t_f7 <- Sys.time()

#### %% Appendix %% ####

#### Table A1 ####

## Manual table generated directly in LaTeX file

#### Tables C1 - C4 ####

## Manual table generated directly in LaTeX file

#### Figure C1 ####

set.seed(89)

# Define a table of simulation parameters (each row is a separate sim. exercise)
sim_params <- data.frame(subjects = 500,
                         rounds = 5,
                         het = c(0.2,0.05,0.2,0.2,0.2,0.2),
                         sigma = c(0.05,0.02, 0.05,0.05,0.05,0.05),
                         sim_opt = c(1,1,2,3,4,5))

iterations <- 100

pred_results <- lapply(1:nrow(sim_params), 
                       function (s) {
                         matrix(nrow = sim_params$subjects[s], 
                                ncol = iterations)
                       })

actual_imces <- list(NA,NA,NA,NA,NA,NA)
for (s in 1:nrow(sim_params)) { 
  
  set.seed(s)
  
  subjects <- sim_params$subjects[s]
  rounds <- sim_params$rounds[s]
  
  imce_schedule <- data.frame(id = 1:sim_params$subjects[s],
                              c1 = runif(sim_params$subjects[s], 0, sim_params$het[s]),
                              c2 = runif(sim_params$subjects[s], 0, sim_params$het[s]),
                              c3 = rbinom(sim_params$subjects[s],1,0.5),
                              c4 = runif(sim_params$subjects[s], 0, sim_params$het[s]))
  
  
  imce_schedule$sigma <- sim_params$sigma[s]
  
  # Determine source of heterogeneity based on simulation
  if (sim_params$sim_opt[s] == 1) {
    
    # Heterogeneity a function of two observed variables
    imce_schedule$mu <- imce_schedule$c1 - imce_schedule$c2
    
  } else if (sim_params$sim_opt[s] == 2) {
    
    # Heterogeneity weakly a function of two observed variables
    imce_schedule$mu <- 0.2*(imce_schedule$c1 - imce_schedule$c2) + 0.8*rnorm(subjects,0,0.125)
    
  } else if (sim_params$sim_opt[s] == 3) {
    
    # Het. a function of a binary variable
    imce_schedule$mu <- ifelse(imce_schedule$c3 == 1, 
                               rnorm(subjects,sim_params$het[s],sim_params$sigma[s]),
                               rnorm(subjects,-sim_params$het[s],sim_params$sigma[s]))
    
  } else if (sim_params$sim_opt[s] == 4) {
    
    # Het. a function of unobserved var., where var. has non-parametric relationship to observed covariate
    imce_schedule$mu <- imce_schedule$c4
    imce_schedule$c1 <- 2*I(imce_schedule$c4 > 0.6*sim_params$het[s]) - rnorm(subjects,0,0.25)
    
  } else if (sim_params$sim_opt[s] == 5) {
    
    # Exponential relationship
    
    imce_schedule$mu <- imce_schedule$c1*2^imce_schedule$c2+imce_schedule$c2
    
  }
  
  imce_schedule$sigma <- sim_params$sigma[s]
  
  actual_imces[[s]] <- imce_schedule
  
  for (b in 1:iterations) {
    
    probs <- as.data.frame(
      do.call("cbind",
              lapply(1:rounds, function (x) rnorm(subjects, imce_schedule$mu, imce_schedule$sigma))
      )
    )
    
    probs$id <- 1:subjects
    
    probs <- tidyr::pivot_longer(probs, -id,
                                 names_to = "round",
                                 values_to = "X1_1")
    
    probs$X2_1 <- 0.1
    
    sched <- data.frame(id = probs$id,
                        X1 = sample(c("a","b"), subjects*rounds, replace = TRUE),
                        X2 = sample(c("c","d"), subjects*rounds, replace = TRUE))
    
    
    sched$y_prob <- 0.5 + I(sched$X1 == "b")*probs$X1_1 + I(sched$X2 == "d")*probs$X2_1
    sched$y_prob <- ifelse(sched$y_prob > 1, 1, ifelse(sched$y_prob < 0, 0, sched$y_prob))
    
    sched$choice <- rbinom(subjects*rounds, 1, sched$y_prob)
    
    sched$y_prob <-  NULL
    
    sched <- merge(sched, imce_schedule[,c("id","c1","c2","c3")], all.x = TRUE)
    
    mod <- cjbart(data = sched, Y = "choice", id = "id", seed = b)
    
    mod_imce <- IMCE(data = sched, model = mod, attribs = c("X1","X2"), ref_levels = c("a","c"),
                     method = "bayes", cores = 8) # was 4
    
    check <- merge(mod_imce$imce[,c("id","b")], imce_schedule[,c("id","mu")], by = "id")
    
    pred_results[[s]][,b] <- mod_imce$imce$b
    
  }
  
}

imce_sim <- sapply(actual_imces, function (s) s$mu)

error_dfs <- lapply(1:6, function (s) {
  apply(pred_results[[s]], 2, function (y) abs(y-imce_sim[,s]))
})

# MAE calcs
maes <- sapply(error_dfs, mean)

compare_preds <- lapply(1:6, function (s) {
  
  comp_df <- data.frame(actual = imce_sim[,s],
                        pred_mean = rowMeans(pred_results[[s]]))
  comp_df$sim <- s
  comp_df$mae <- maes[[s]]
  
  return(comp_df)
  
})

compare_df <- do.call("rbind", compare_preds)
compare_df$Simulation <- paste0("Simulation ",compare_df$sim,"\nMAE = ",round(compare_df$mae,2))
compare_df$ae <- abs(compare_df$actual - compare_df$pred_mean)

ggplot(compare_df, aes(x = pred_mean, y = actual)) +
  facet_wrap(~Simulation, scales = "free") +
  geom_abline(linetype = "dashed", color = "dimgrey") + 
  geom_point(alpha = 0.7, color = "dodgerblue2") +
  theme_bw() +
  labs(y = "Actual IMCE", x = "Predicted IMCE (average)")

ggsave("figures/figure_c1.pdf", width = 6, height = 4)

#### Table C5-C6 ####

## Manually created tables in LaTeX

#### Table C7 ####

set.seed(89)
sim_params <- data.frame(subjects = c(500,500,500,1500,5000,500,500),
                         rounds = c(5,5,10,5,5,5,10),
                         het = c(0.25,0.05, 0.05,0.25,0.25,0.25, 0.25),
                         sigma = c(0.05,0.02,0.02,0.05,0.05,0.05, 0.05),
                         var_het = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE))

sim_res <- matrix(nrow = nrow(sim_params), ncol = 3, 
                  dimnames = list(rep(NULL,5), c("sim","average","bayes")))

iterations <- 500

for (s in 1:nrow(sim_params)) { 
  
  print(s)
  set.seed(s)
  
  subjects <- sim_params$subjects[s]
  rounds <- sim_params$rounds[s]
  
  coverage_results <- list(
    
    bayes = matrix(nrow = subjects, ncol = iterations),
    average = matrix(nrow = subjects, ncol = iterations)
    
  )
  
  imce_schedule <- data.frame(id = 1:subjects,
                              c1 = runif(subjects, 0, sim_params$het[s]),
                              c2 = runif(subjects, 0, sim_params$het[s]))
  
  imce_schedule$mu <- imce_schedule$c1 - imce_schedule$c2
  
  if (sim_params$var_het[s]) {
    imce_schedule$sigma <- runif(subjects, 0.001, sim_params$sigma[s])
  } else {
    imce_schedule$sigma <- sim_params$sigma[s]
  }
  
  for (b in 1:iterations) {
    
    probs <- as.data.frame(
      do.call("cbind",
              lapply(1:rounds, function (x) rnorm(subjects, imce_schedule$mu, imce_schedule$sigma))
      )
    )
    
    probs$id <- 1:subjects
    
    probs <- tidyr::pivot_longer(probs, -id,
                                 names_to = "round",
                                 values_to = "X1_1")
    
    probs$X2_1 <- 0.1
    
    sched <- data.frame(id = probs$id,
                        X1 = sample(c("a","b"), subjects*rounds, replace = TRUE),
                        X2 = sample(c("c","d"), subjects*rounds, replace = TRUE))
    
    
    sched$y_prob <- 0.5 + I(sched$X1 == "b")*probs$X1_1 + I(sched$X2 == "d")*probs$X2_1
    sched$y_prob <- ifelse(sched$y_prob > 1, 1, ifelse(sched$y_prob < 0, 0, sched$y_prob))
    
    sched$y <- rbinom(subjects*rounds, 1, sched$y_prob)
    
    sched$y_prob <- NULL
    
    sched <- merge(sched, imce_schedule[,c("id","c1","c2")], all.x = TRUE)
    
    mod <- cjbart(data = sched, Y = "y", id = "id", seed = b)
    
    for (var_method in c("bayes","average")) {
      
      mod_imce <- IMCE(data = sched, model = mod, attribs = c("X1","X2"), ref_levels = c("a","c"),
                       method = var_method, cores = 8)
      
      check <- cbind(mod_imce$imce[,c("id","b")], "b_lo" = mod_imce$imce_lower[,c("b")])
      check <- cbind(check, "b_hi" = mod_imce$imce_upper[,c("b")])
      check <- merge(check, imce_schedule[,c("id","mu")])
      
      coverage_results[[var_method]][,b] <- ((check$mu < check$b_hi) & (check$mu > check$b_lo))
      
    }
    
  }
  
  sim_res[s,] <- c(
    s,
    sapply(c("average","bayes"), 
           function (x) mean(rowMeans(coverage_results[[x]]))
    )
  )
}

print(xtable(sim_res, digits = 3), 
      include.rownames = FALSE,
      include.colnames = FALSE,
      only.contents = TRUE,
      hline.after = NULL,
      file = "tables/table_c7.tex")

#### Figure C2 ####

set.seed(89)

## Narrowing down on one attribute

rmce <- function(omces) {
  
  omces %>% 
    mutate(round = rep(rep(1:rounds, each = profiles), N)) %>% 
    group_by(i, round) %>% 
    summarise(a2_bar = mean(a2),
              b2_bar = mean(b2),
              c2_bar = mean(c2))
}

B <- 100
N = 250
rounds = 10
profiles = 2

corr_sim <- data.frame(A_bad = rep(NA, B),
                       B_bad = rep(NA, B),
                       C_bad = rep(NA, B),
                       A_good = rep(NA, B),
                       B_good = rep(NA, B),
                       C_good = rep(NA, B))

for (b in 1:B) {
  
  set.seed(b)
  
  base_data <- data.frame(i = rep(1:N, each = rounds*profiles),
                          round = rep(1:rounds, each = profiles),
                          profile = 1:profiles) %>% 
    
    mutate(A = sample(c("a1","a2"), N*rounds*profiles, replace = TRUE),
           B = sample(c("b1","b2"), N*rounds*profiles, replace = TRUE),
           C = sample(c("c1","c2"), N*rounds*profiles, replace = TRUE))
  
  bad_data <- base_data %>% 
    
    mutate(U = rnorm(N*rounds*profiles,0,0.001) + 
             (A == "a2")*0.5*round + 
             (B == "b2")*(0.5-(round-1)*0.1) +
             (C == "c2")*0.5) %>% 
    group_by(i, round) %>% 
    mutate(select = ifelse(U == max(U),1,0)) %>% 
    ungroup() %>% 
    select(-U, -profile)
  
  good_data <- base_data %>% 
    
    mutate(U = rnorm(N*rounds*profiles,0,0.001) + 
             (A == "a2")*1 + 
             (B == "b2")*0.2 +
             (C == "c2")*0.5) %>% 
    group_by(i, round) %>% 
    mutate(select = ifelse(U == max(U),1,0)) %>% 
    ungroup() %>% 
    select(-U, -profile)
  
  bad_mod <- cjbart(bad_data, Y = "select", id = "i", round = "round", cores = 8) # was 4
  good_mod <- cjbart(good_data, Y = "select", id = "i", round = "round", cores = 8) # was 4
  
  bad_pred <- IMCE(bad_data, model = bad_mod, attribs = c("A","B","C"), ref_levels = c("a1","b1","c1"),
                   keep_omce = TRUE)
  good_pred <- IMCE(good_data, model = good_mod, attribs = c("A","B","C"), ref_levels = c("a1","b1","c1"),
                    keep_omce = TRUE)
  
  rmces_bad <- rmce(bad_pred$omce)
  rmces_good <- rmce(good_pred$omce)
  
  corr_sim[b,] <- c(a2_bad = cor.test(rmces_bad$round, rmces_bad$a2_bar)$estimate,
                    b2_bad = cor.test(rmces_bad$round, rmces_bad$b2_bar)$estimate,
                    c2_bad = cor.test(rmces_bad$round, rmces_bad$c2_bar)$estimate,
                    a2_good = cor.test(rmces_good$round, rmces_good$a2_bar)$estimate,
                    b2_good = cor.test(rmces_good$round, rmces_good$b2_bar)$estimate,
                    c2_good = cor.test(rmces_good$round, rmces_good$c2_bar)$estimate)
  
  rm(sim_data, sim_mod, sim_pred, rmces)
  
}

corr_sim %>% 
  mutate(sim = 1:B) %>% 
  pivot_longer(-sim, names_to = "type", values_to = "correlation") %>% 
  mutate(context = ifelse(grepl("bad",type), "Round-effects","No round-effects"),
         attribute = paste0("Attribute ",substr(type,1,1))) %>% 
  
  ggplot(aes(x = correlation, fill = context)) +
  facet_grid(attribute~., scales = "free_y") +
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept=0, linetype = "dashed") +
  xlim(-1,1) +
  scale_fill_manual(values = c("dodgerblue2","firebrick2")) +
  labs(x = "Correlation Coefficient", y= "Density", fill = "DGP") +
  theme(legend.position = "bottom")

ggsave("figures/figure_c2.pdf")

#### Table C8 ####

B <- 100

A1_cor_c1 <- rep(NA, B)
A2_cor_c1 <- rep(NA, B)
A3_cor_c1 <- rep(NA, B)

A1_cor_c2 <- rep(NA, B)
A2_cor_c2 <- rep(NA, B)
A3_cor_c2 <- rep(NA, B)

for (b in 1:B) {
  
  sim_b <- preference_sim(seed = b)
  
  A1_cor_c1[b] <- cor.test(sim_b$imce$`A1: Binary heterogeneity (c1)`,
                           sim_b$imce$c1)$estimate
  A2_cor_c1[b] <- cor.test(sim_b$imce$`A2: Interval heterogeneity (c2)`,
                           sim_b$imce$c1)$estimate
  A3_cor_c1[b] <- cor.test(sim_b$imce$`A3: Random heterogeneity`,
                           sim_b$imce$c1)$estimate
  
  A1_cor_c2[b] <- cor.test(sim_b$imce$`A1: Binary heterogeneity (c1)`,
                           sim_b$imce$c2)$estimate
  A2_cor_c2[b] <- cor.test(sim_b$imce$`A2: Interval heterogeneity (c2)`,
                           sim_b$imce$c2)$estimate
  A3_cor_c2[b] <- cor.test(sim_b$imce$`A3: Random heterogeneity`,
                           sim_b$imce$c2)$estimate
  
}

mean(A1_cor_c1) 
mean(A2_cor_c1)
mean(A3_cor_c1)

mean(A1_cor_c2)
mean(A2_cor_c2)
mean(A3_cor_c2)

corr_table <- data.frame(att = c("A1","A2","A3"),
                         c1= c(mean(A1_cor_c1),mean(A2_cor_c1),mean(A3_cor_c1)),
                         c2= c(mean(A1_cor_c2),mean(A2_cor_c2),mean(A3_cor_c2))) %>% 
  xtable(., digits = 3) %>% 
  print(only.contents = TRUE,
        hline.after = NULL,
        include.rownames = FALSE,
        include.colnames = FALSE,
        file = "tables/table_c8.tex")

#### Figure C3 ####

preference_sim_ols <- function(subjects = 500, rounds = 20, seed = 89) {
  
  set.seed(seed)
  
  utilities <- data.frame(id = 1:subjects,
                          c1 = rbinom(subjects, 1, 0.5),
                          c2 = runif(subjects, -1,1)) %>% 
    mutate(X1_1 = ifelse(c1 == 1, 
                         rnorm(subjects, 1, sd = 1),
                         rnorm(subjects, -1, sd = 1)),
           X2_1 = rnorm(subjects, abs(c2-0.2), sd = 1),
           X3_1 = rnorm(subjects, 0, sd = 0.5))
  
  conjoint_data <- data.frame(id = rep(1:subjects, each = rounds*2),
                              round = rep(1:rounds, each = 2),
                              profile = 1:2,
                              X1 = rbinom(subjects*rounds*2, 1, 0.5),
                              X2 = rbinom(subjects*rounds*2, 1, 0.5),
                              X3 = rbinom(subjects*rounds*2, 1, 0.5)) %>% 
    left_join(utilities, by = "id") %>% 
    mutate(U = X1*X1_1 + X2*X2_1 + X3*X3_1 + rnorm(subjects*rounds*2, 0,0.0005)) %>% 
    mutate(Y = round(7*((U-min(U))/(max(U)-min(U)))))
  
  train_data <- conjoint_data %>% select(id, X1, X2, X3, c1, c2, Y) %>% 
    mutate(X1 = ifelse(X1 == 1, "A1: Binary heterogeneity (c1)","a"),
           X2 = ifelse(X2 == 1, "A2: Interval heterogeneity (c2)","c"),
           X3 = ifelse(X3 == 1, "A3: Random heterogeneity","e")) %>% 
    mutate(X1 = relevel(factor(X1), ref = "a"),
           X2 = relevel(factor(X2), ref = "c"),
           X3 = relevel(factor(X3), ref = "e"))
  
  ols_regs <- lapply(unique(train_data$id), function (x) {
    
    df_i <- train_data[train_data$id == x,]
    reg_i <- tidy(lm(Y ~ X1 + X2 + X3, data = df_i))
    reg_i$id <- x
    reg_i$c1 <- df_i$c1[1]
    reg_i$c2 <- df_i$c2[1]
    return(reg_i)
  }) %>% 
    do.call("rbind", .)
  
  return(list(ols_regs, conjoint_data))
  
}

ols_sim <- preference_sim_ols()

ols_formatted <- ols_sim[[1]] %>% 
  mutate(att = sub("X[0-9]", "", term),
         est_lower = estimate - 1.96*std.error,
         est_upper = estimate + 1.96*std.error,) %>% 
  filter(att != "(Intercept)") %>% 
  group_by(att) %>% 
  arrange(estimate) %>% 
  mutate(effect_order = 1:n(),
         c1 = as.factor(c1)) %>% 
  ungroup()

ggplot(ols_formatted,
       aes(x = effect_order, y = estimate, color = c1)) +
  facet_wrap(~att, ncol = 2) +
  geom_ribbon(aes(ymin = est_lower, ymax = est_upper), fill = "grey", alpha = 0.7, colour = NA) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue2","firebrick2")) +
  labs(x = "", color = expression(c[1])) +
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

ggsave("figures/figure_c3.pdf", width = 7, height = 3)

#### Figure C4 ####

ggplot(ols_formatted,
       aes(x = effect_order, y = estimate, color = c2)) +
  facet_wrap(~att, ncol = 2) +
  geom_ribbon(aes(ymin = est_lower, ymax = est_upper), fill = "grey", alpha = 0.7, colour = NA) +
  geom_point() +
  scale_color_gradient(low = "dodgerblue2", high = "firebrick2") +
  labs(x = "", color = expression(c[2])) +
  guides(color = guide_colorbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

ggsave("figures/figure_c4.pdf", width = 7, height = 3)

#### Table C9 ####

B <- 100

A1_cor_c1_ols <- rep(NA, B)
A2_cor_c1_ols <- rep(NA, B)
A3_cor_c1_ols <- rep(NA, B)

A1_cor_c2_ols <- rep(NA, B)
A2_cor_c2_ols <- rep(NA, B)
A3_cor_c2_ols <- rep(NA, B)

for (b in 1:B) {
  
  sim_b_ols <- preference_sim_ols(seed = b)[[1]] %>% 
    mutate(att = sub("X[0-9]", "", term))
  
  A1_cor_c1_ols[b] <- cor.test(sim_b_ols$estimate[sim_b_ols$att == "A1: Binary heterogeneity (c1)"],
                               sim_b_ols$c1[sim_b_ols$att == "A1: Binary heterogeneity (c1)"])$estimate
  A2_cor_c1_ols[b] <- cor.test(sim_b_ols$estimate[sim_b_ols$att == "A2: Interval heterogeneity (c2)"],
                               sim_b_ols$c1[sim_b_ols$att == "A2: Interval heterogeneity (c2)"])$estimate
  A3_cor_c1_ols[b] <- cor.test(sim_b_ols$estimate[sim_b_ols$att == "A3: Random heterogeneity"],
                               sim_b_ols$c1[sim_b_ols$att == "A3: Random heterogeneity"])$estimate
  
  A1_cor_c2_ols[b] <- cor.test(sim_b_ols$estimate[sim_b_ols$att == "A1: Binary heterogeneity (c1)"],
                               sim_b_ols$c2[sim_b_ols$att == "A1: Binary heterogeneity (c1)"])$estimate
  A2_cor_c2_ols[b] <- cor.test(sim_b_ols$estimate[sim_b_ols$att == "A2: Interval heterogeneity (c2)"],
                               sim_b_ols$c2[sim_b_ols$att == "A2: Interval heterogeneity (c2)"])$estimate
  A3_cor_c2_ols[b] <- cor.test(sim_b_ols$estimate[sim_b_ols$att == "A3: Random heterogeneity"],
                               sim_b_ols$c2[sim_b_ols$att == "A3: Random heterogeneity"])$estimate
  
}

mean(A1_cor_c1_ols) # = 0.689513 
mean(A2_cor_c1_ols) # = 0.00183553 
mean(A3_cor_c1_ols) # = -0.002596537

mean(A1_cor_c2_ols) # = -0.001958549
mean(A2_cor_c2_ols) # = -0.1558983
mean(A3_cor_c2_ols) # = 0.0004389836

# Make table

corr_table_ols <- data.frame(att = c("A1","A2","A3"),
                             c1= c(mean(A1_cor_c1_ols),mean(A2_cor_c1_ols),mean(A3_cor_c1_ols)),
                             c2= c(mean(A1_cor_c2_ols),mean(A2_cor_c2_ols),mean(A3_cor_c2_ols))) %>% 
  xtable(., digits = 3) %>% 
  print(only.contents = TRUE,
        hline.after = NULL,
        include.rownames = FALSE,
        include.colnames = FALSE,
        file = "tables/table_c9.tex")

#### Table D1 ####

comp_table <- comparison %>% 
  select(feature, level, lpm, cjbart) %>% 
  mutate(`Difference (% of Parametric)` = 100*(as.numeric(cjbart)-lpm)/lpm,
         level = factor(level, levels = unlist(immig_model$factor_levels))) %>%
  arrange(level)

for (i in seq(nrow(comp_table),2,-1)) {
  if (comp_table$feature[i] == comp_table$feature[i-1]) {
    comp_table$feature[i] <- ""
  }
}

xtable(comp_table) %>% 
  print(only.contents = TRUE,
        include.rownames = FALSE,
        include.colnames = FALSE,
        hline.after = NULL,
        file = "tables/table_d1.tex")

#### Table D2 ####

comp_unc <- comparison %>% 
  select(feature, level, lpm_intvl, cjbart_intvl) %>% 
  mutate(level = factor(level, levels = unlist(immig_model$factor_levels))) %>%
  arrange(level)

for (i in seq(nrow(comp_unc),2,-1)) {
  if (comp_unc$feature[i] == comp_unc$feature[i-1]) {
    comp_unc$feature[i] <- ""
  }
}

xtable(comp_unc) %>% 
  print(only.contents = TRUE,
        include.rownames = FALSE,
        include.colnames = FALSE,
        hline.after = NULL,
        file = "tables/table_d2.tex")

#### Table D3 ####

vacc_table_print <- vacc_table

for (i in seq(nrow(vacc_table_print),2,-1)) {
  if (vacc_table_print$feature[i] == vacc_table_print$feature[i-1]) {
    vacc_table_print$feature[i] <- ""
  }
}

xtable(vacc_table_print %>% select(-cjbart_intvl, -lpm_intvl,-study)) %>% 
  print(only.contents = TRUE,
        include.rownames = FALSE,
        include.colnames = FALSE,
        hline.after = NULL,
        file = "tables/table_d3.tex")

#### Table D4 ####

xtable(vacc_table_print %>% select(-cjbart, -lpm,-study, -`Difference (% of Parametric)`)) %>% 
  print(only.contents = TRUE,
        include.rownames = FALSE,
        include.colnames = FALSE,
        hline.after = NULL,
        file = "tables/table_d4.tex")

#### Figure E1 ####

preference_sim_cf <- function(subjects = 500, rounds = 5, seed = 89, 
                              tune = FALSE, tune_params = NULL) {
  
  set.seed(seed)
  
  utilities <- data.frame(id = 1:subjects,
                          c1 = rbinom(subjects, 1, 0.5),
                          c2 = runif(subjects, -1,1)) %>% 
    mutate(X1_1 = ifelse(c1 == 1, 
                         rnorm(subjects, 1, sd = 1),
                         rnorm(subjects, -1, sd = 1)),
           X2_1 = rnorm(subjects, abs(c2-0.2), sd = 1),
           X3_1 = rnorm(subjects, 0,sd = 0.5))
  
  conjoint_data <- data.frame(id = rep(1:subjects, each = rounds*2),
                              round = rep(1:rounds, each = 2),
                              profile = 1:2,
                              X1 = rbinom(subjects*rounds*2, 1, 0.5),
                              X2 = rbinom(subjects*rounds*2, 1, 0.5),
                              X3 = rbinom(subjects*rounds*2, 1, 0.5)) %>% 
    left_join(utilities, by = "id") %>% 
    mutate(U = X1*X1_1 + X2*X2_1 + X3*X3_1 + rnorm(subjects*rounds*2, 0,0.0005)) %>% 
    group_by(id, round) %>%
    mutate(Y = ifelse(U == max(U),1,0)) %>% 
    ungroup()
  
  train_data <- conjoint_data %>% select(id, X1, X2, X3, c1, c2, Y)
  
  if (tune) {
    
    tune_results <- grf::causal_forest(
      X = as.data.frame({train_data %>% select(-X1, -Y, -id)}),
      Y = train_data$Y,
      W = train_data$X1,
      cluster = train_data$id,
      tune.parameters = "all"
    )$tuning.output$params
    
    return(as.list(tune_results))
    
  } else {
    
    pref_mod_A1 <- grf::causal_forest(
      X = as.data.frame(train_data %>% select(-X1, -Y, -id)),
      Y = train_data$Y,
      W = train_data$X1,
      cluster = train_data$id,
      sample.fraction = tune_params$sample.fraction,
      mtry = tune_params$mtry,
      min.node.size = tune_params$min.node.size,
      honesty.fraction = tune_params$honesty.fraction,
      honesty.prune.leaves = tune_params$honesty.prune.leaves,
      alpha = tune_params$alpha,
      imbalance.penalty = tune_params$imbalance.penalty
    )
    
    pref_mod_A2 <- grf::causal_forest(
      X = as.data.frame(train_data %>% select(-X2, -Y, -id)),
      Y = train_data$Y,
      W = train_data$X2,
      cluster = train_data$id,
      sample.fraction = tune_params$sample.fraction,
      mtry = tune_params$mtry,
      min.node.size = tune_params$min.node.size,
      honesty.fraction = tune_params$honesty.fraction,
      honesty.prune.leaves = tune_params$honesty.prune.leaves,
      alpha = tune_params$alpha,
      imbalance.penalty = tune_params$imbalance.penalty
    )
    
    pref_mod_A3 <- grf::causal_forest(
      X = as.data.frame(train_data %>% select(-X3, -Y, -id)),
      Y = train_data$Y,
      W = train_data$X3,
      cluster = train_data$id,
      sample.fraction = tune_params$sample.fraction,
      mtry = tune_params$mtry,
      min.node.size = tune_params$min.node.size,
      honesty.fraction = tune_params$honesty.fraction,
      honesty.prune.leaves = tune_params$honesty.prune.leaves,
      alpha = tune_params$alpha,
      imbalance.penalty = tune_params$imbalance.penalty
    )
    
    omces <- train_data %>% select(id, c1, c2) %>% 
      cbind(., A1 = pref_mod_A1$predictions[,1]) %>% 
      cbind(., A2 = pref_mod_A2$predictions[,1]) %>% 
      cbind(., A3 = pref_mod_A3$predictions[,1])
    
    imces <- omces %>% 
      group_by(id) %>% 
      summarise(A1 = mean(A1),
                A2 = mean(A2),
                A3 = mean(A3),
                c1 = unique(c1)[1],
                c2 = unique(c2)[1])
    
    return(imces)
  }
  
}

cf_params <- preference_sim_cf(tune = TRUE, seed = 100)

cf_sim <- preference_sim_cf(tune = FALSE, tune_params = cf_params)

cf_formatted <- cf_sim %>% 
  pivot_longer(cols = starts_with("A"),
               names_to = "att",
               values_to = "imce") %>% 
  mutate(att = case_when(att == "A1" ~ "A1: Binary heterogeneity (c1)",
                         att == "A2" ~ "A2: Interval heterogeneity (c2)",
                         att == "A3" ~ "A3: Random heterogeneity")) %>% 
  group_by(att) %>% 
  arrange(imce) %>% 
  mutate(effect_order = 1:n(),
         c1 = as.factor(c1)) %>% 
  ungroup()

ggplot(cf_formatted,
       aes(x = effect_order, y = imce, color = c1)) +
  facet_wrap(~att, ncol = 2) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue2","firebrick2")) +
  labs(x = "", color = expression(c[1])) +
  guides(color = guide_legend(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

ggsave("figures/figure_e1.pdf", width = 7, height = 3)

#### Figure E2 ####

ggplot(cf_formatted,
       aes(x = effect_order, y = imce, color = c2)) +
  facet_wrap(~att, ncol = 2) +
  geom_point() +
  scale_color_gradient(low = "dodgerblue2", high = "firebrick2") +
  labs(x = "", color = expression(c[2])) +
  guides(color = guide_colorbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

ggsave("figures/figure_e2.pdf", width = 7, height = 3)

#### Table E1 ####

B <- 100

A1_cor_c1_cf <- rep(NA, B)
A2_cor_c1_cf <- rep(NA, B)
A3_cor_c1_cf <- rep(NA, B)

A1_cor_c2_cf <- rep(NA, B)
A2_cor_c2_cf <- rep(NA, B)
A3_cor_c2_cf <- rep(NA, B)

for (b in 1:B) {
  
  sim_b_cf <- preference_sim_cf(seed = b,tune_params = cf_params)
  
  A1_cor_c1_cf[b] <- cor.test(sim_b_cf$A1,
                              sim_b_cf$c1)$estimate
  A2_cor_c1_cf[b] <- cor.test(sim_b_cf$A2,
                              sim_b_cf$c1)$estimate
  A3_cor_c1_cf[b] <- cor.test(sim_b_cf$A3,
                              sim_b_cf$c1)$estimate
  
  A1_cor_c2_cf[b] <- cor.test(sim_b_cf$A1,
                              sim_b_cf$c2)$estimate
  A2_cor_c2_cf[b] <- cor.test(sim_b_cf$A2,
                              sim_b_cf$c2)$estimate
  A3_cor_c2_cf[b] <- cor.test(sim_b_cf$A3,
                              sim_b_cf$c2)$estimate
  
}

mean(A1_cor_c1_cf)
mean(A2_cor_c1_cf) 
mean(A3_cor_c1_cf) 

mean(A1_cor_c2_cf) 
mean(A2_cor_c2_cf)
mean(A3_cor_c2_cf)

# Make table

corr_table_cf <- data.frame(att = c("A1","A2","A3"),
                            c1= c(mean(A1_cor_c1_cf),mean(A2_cor_c1_cf),mean(A3_cor_c1_cf)),
                            c2= c(mean(A1_cor_c2_cf),mean(A2_cor_c2_cf),mean(A3_cor_c2_cf))) %>% 
  xtable(., digits = 3) %>% 
  print(only.contents = TRUE,
        hline.after = NULL,
        include.rownames = FALSE,
        include.colnames = FALSE,
        file = "tables/table_e1.tex")

#### Figure E3 ####

set.seed(89)

vacc_lowinc <- readRDS("data/nbh_clean_conjoint_global.rds") %>%
  select(-all_of(c("person", "ans", "candidate", "weights"))) %>% 
  filter(income != "Highest 20% income level") %>% 
  as.data.frame(.) %>% 
  drop_na(.) %>% 
  mutate(income = ifelse(as.character(income) == "Average income level", 0, 1))


cf_covars <- vacc_lowinc[,!(colnames(vacc_lowinc) %in% c("id","income","select"))] %>% 
  model.matrix(~. , data=.)


cf_mod <- causal_forest(X = cf_covars,
                        Y = vacc_lowinc$select,
                        W = vacc_lowinc$income,
                        tune.parameters = "all")

cf_preds <- predict(cf_mod, estimate.variance = TRUE)

cf_imces <- cbind(cf_preds, id = vacc_lowinc$id) %>% 
  left_join(vacc_lowinc[,c("id","ideology")]) %>% 
  group_by(id) %>% 
  summarise(imce = mean(predictions),
            ideology=  mean(ideology))

cf_plot <- cf_imces[order(cf_imces$imce),]
cf_plot$effect_order <- 1:nrow(cf_plot)

cor.test(cf_plot$imce, cf_plot$ideology)

cf_effect <- ggplot(cf_plot, aes(x = effect_order, y = imce)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = mean(cf_plot$imce), 
             color = "dodgerblue",
             size = 0.2) +
  labs(x = "", y = "IMCE") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(cf_plot)))

cf_density <- ggplot(cf_plot, aes(x = effect_order, fill = as.factor(ideology))) +
  geom_histogram(binwidth = 60,position="stack") +
  labs(x = "", y = "Density", fill = "Ideology") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, nrow(cf_plot)))

cf_ideology <- plot_grid(cf_effect, cf_density, ncol = 1, align = "v", rel_heights = c(0.6,0.4))
ggsave("figures/figure_e3.pdf", dpi=300, width = 7, height = 4)

#### Table E2 ####

covar_lookup <- data.frame(covar_name = c("country","subj_age","gender","ideology",
                                          "ind_inc","education","hes_covid_2",
                                          "wtp_access","wtp_private",
                                          "int_pol_implem_6"),
                           covar_label = c("Country","Age","Gender","Ideology",
                                           "Income","Education","Hesitancy",
                                           "WTP Access","WTP Private",
                                           "Mandatory Vaccination"))

cf_vimp <- data.frame(var_dummy = colnames(cf_covars),
                      vimp = variable_importance(cf_mod)) %>% 
  mutate(var_dummy = ifelse(var_dummy == "age","subj_age",var_dummy)) %>% 
  mutate(covar_name = case_when(str_starts(var_dummy,"country") ~ "country",
                                str_starts(var_dummy,"subj_age") ~ "subj_age",
                                str_starts(var_dummy,"gender") ~ "gender",
                                str_starts(var_dummy,"ideology" ) ~ "ideology",
                                str_starts(var_dummy,"ind_inc" ) ~ "ind_inc",
                                str_starts(var_dummy,"education") ~ "education",
                                str_starts(var_dummy, "hes_covid_2") ~ "hes_covid_2",
                                str_starts(var_dummy,"wtp_access") ~ "wtp_access",
                                str_starts(var_dummy, "wtp_private") ~  "wtp_private",
                                str_starts(var_dummy, "int_pol_implem_6") ~  "int_pol_implem_6")) %>% 
  filter(!is.na(covar_name)) %>% 
  group_by(covar_name) %>% 
  summarise(importance = sum(vimp)) %>% 
  left_join(covar_lookup, by = "covar_name") %>% 
  select(covar_label, importance)

xtable(cf_vimp, digits = 3) %>% 
  print(only.contents = TRUE,
        include.rownames = FALSE,
        include.colnames = FALSE,
        hline.after = NULL,
        file = "tables/table_e2.tex")


#### Figure E4 ####

cf_ri_mod <- causal_forest(X = cf_covars,
                           Y = vacc_lowinc$select,
                           W = vacc_lowinc$income,
                           clusters = as.factor(vacc_lowinc$id),
                           tune.parameters = "all")

cf_ri_preds <- predict(cf_ri_mod, estimate.variance = TRUE)

cf_ri_imces <- cbind(cf_ri_preds, id = vacc_lowinc$id) %>% 
  left_join(vacc_lowinc[,c("id","ideology")]) %>% 
  group_by(id) %>% 
  summarise(imce = mean(predictions),
            ideology=  mean(ideology))

cf_ri_plot <- cf_ri_imces[order(cf_ri_imces$imce),]
cf_ri_plot$effect_order <- 1:nrow(cf_ri_plot)

cor.test(cf_ri_plot$imce, cf_ri_plot$ideology)

cf_ri_effect <- ggplot(cf_ri_plot, aes(x = effect_order, y = imce)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = mean(cf_plot$imce), 
             color = "dodgerblue",
             size = 0.2) +
  labs(x = "", y = "IMCE") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(cf_plot)))

cf_ri_density <- ggplot(cf_ri_plot, aes(x = effect_order, fill = as.factor(ideology))) +
  geom_histogram(binwidth = 60,position="stack") +
  labs(x = "", y = "Density", fill = "Ideology") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, nrow(cf_plot)))

cf_ri_ideology <- plot_grid(cf_ri_effect, cf_ri_density, ncol = 1, align = "v", rel_heights = c(0.6,0.4))
ggsave("figures/figure_e4.png", dpi=300, width = 7, height = 4)

#### Table F1 ####

## Manually created table in LaTeX file

#### Figure F1 ####

set.seed(89)
us_data <- vacc_data[vacc_data$country == "US",]

cvars <- colnames(us_data)[!(colnames(us_data) %in% c("vulnerability","transmission","income","occupation","age_category","select"))]
us_covars <- us_data[,cvars]
us_covars <- us_covars[!duplicated(us_covars),]

vacc_pimces <- pIMCE(covar_data = us_covars,
                     model = het_mod,
                     attribs = c("vulnerability","transmission","income","occupation","age_category"),
                     l = "age_category",
                     l_1 = "65 years old",
                     l_0 = "25 years old",
                     marginals = list("age_category" = c("25 years old" = 0.33,
                                                         "40 years old" = 0.31,
                                                         "65 years old" = 0.22,
                                                         "79 years old" = 0.14),
                                      "vulnerability" = c("Average risk of COVID-19 death" = 0.624,
                                                          "Moderate (Twice the average risk of COVID-19 death)" = 0.188,
                                                          "High (Five times the average risk of COVID-19 death)" = 0.188),
                                      "income" = c("Lowest 20% income level" = 0.2,
                                                   "Average income level" = 0.6,
                                                   "Highest 20% income level" = 0.2)),
                     method = "bayes",
                     cores = 8) # was 2

pimce_wide <- vacc_pimces %>% 
  rename(est_wgtd = pIMCE) %>% 
  left_join(imce_sched[,c("id","65 years old")], by = "id") %>% 
  left_join(vacc_imces$imce_lower[,c("id","65 years old")], by = "id") %>% 
  rename(est_unwgtd = `65 years old.x`,
         imce_lower = `65 years old.y`) %>% 
  left_join(vacc_imces$imce_upper[,c("id","65 years old")], by = "id") %>% 
  rename(imce_upper = `65 years old`)

ggplot(pimce_wide, aes(x = est_wgtd, y = est_unwgtd, color = age)) +
  geom_point() +
  labs(x = "pIMCE", y = "IMCE", color = "Subject's age") +
  theme_minimal()+
  theme(legend.position = "bottom")

ggsave("figures/figure_f1.png", width = 5, height = 5, dpi = 300)


#### Figure F2 ####

pimce_long <- pimce_wide %>% 
  pivot_longer(cols = c(starts_with("est_")),
               names_to = "type",
               names_prefix = "est_",
               values_to = "est") %>% 
  mutate(ci_lower = ifelse(type == "wgtd", pIMCE_lower, imce_lower),
         ci_upper = ifelse(type == "wgtd", pIMCE_upper, imce_upper)) %>% 
  select(id, age, type, est, age, ci_lower, ci_upper) %>% 
  group_by(type) %>% 
  arrange(est) %>% 
  mutate(est_order = 1:nrow(pimce_wide)) %>% 
  ungroup() %>% 
  mutate(age_cat = case_when(age < 25 ~ "<25 years old",
                             age >= 25 & age < 35 ~ "25-34 years old",
                             age >= 35 & age < 45 ~ "35-44 years old",
                             age >= 45 & age < 55 ~ "45-54 years old",
                             age >= 55 & age < 65 ~ "55-64 years old",
                             age >= 65 & age < 75 ~ "65-74 years old",
                             age >= 75 ~ ">75 years old")) %>% 
  mutate(age_cat = factor(age_cat, levels = c("<25 years old",
                                              "25-34 years old",
                                              "35-44 years old",
                                              "45-54 years old",
                                              "55-64 years old",
                                              "65-74 years old",
                                              ">75 years old")),
         label = ifelse(type == "unwgtd", "IMCE","pIMCE"))

pimce_effect_lines <- ggplot(pimce_long, aes(x = est_order, y = est, color = label)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = label), alpha = 0.1, color = NA) +
  scale_fill_discrete(guide = "none") +
  labs(x = "", y = "Estimate", color = "Estimator") +
  theme_minimal()+
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(pimce_long)/2))

pimce_hist <- pimce_long %>% 
  filter(type == "wgtd") %>% 
  ggplot(aes(x = est_order, fill = age_cat)) +
  geom_histogram(binwidth = 30,position="stack") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(y = "pIMCE", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_x_continuous(limits = c(0, nrow(pimce_long)/2))


imce_hist <- pimce_long %>% 
  filter(type == "unwgtd") %>% 
  ggplot(aes(x = est_order, fill = age_cat)) +
  geom_histogram(binwidth = 30,position="stack") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(y = "IMCE", x = "Subject", fill = "Subject's Age") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(pimce_long)/2))

plot_grid(pimce_effect_lines, pimce_hist, imce_hist,
          ncol = 1, align = "HV", rel_heights = c(0.6,0.15,0.25))

ggsave("figures/figure_f2.png",dpi=300, width = 8.5, height = 10)

#### Figure F3 ####

transmission_pimces <- pIMCE(covar_data = us_covars,
                             model = het_mod,
                             attribs = c("vulnerability","transmission","income","occupation","age_category"),
                             l = "transmission",
                             l_1 = "High risk (Five times the average risk of catching and transmitting the COVID-19 virus)",
                             l_0 = "Average risk of catching and transmitting the COVID-19 virus",
                             marginals = list("age_category" = c("25 years old" = 0.33,
                                                                 "40 years old" = 0.31,
                                                                 "65 years old" = 0.22,
                                                                 "79 years old" = 0.14),
                                              "vulnerability" = c("Average risk of COVID-19 death" = 0.624,
                                                                  "Moderate (Twice the average risk of COVID-19 death)" = 0.188,
                                                                  "High (Five times the average risk of COVID-19 death)" = 0.188),
                                              "income" = c("Lowest 20% income level" = 0.2,
                                                           "Average income level" = 0.6,
                                                           "Highest 20% income level" = 0.2)),
                             method = "bayes",
                             cores = 8) # was 2

pimce_wide_trans <- transmission_pimces %>% 
  rename(est_wgtd = pIMCE) %>% 
  left_join(imce_sched[,c("id","High risk (Five times the average risk of catching and transmitting the COVID-19 virus)")], by = "id") %>% 
  left_join(vacc_imces$imce_lower[,c("id","High risk (Five times the average risk of catching and transmitting the COVID-19 virus)")], by = "id") %>% 
  rename(est_unwgtd = `High risk (Five times the average risk of catching and transmitting the COVID-19 virus).x`,
         imce_lower = `High risk (Five times the average risk of catching and transmitting the COVID-19 virus).y`) %>% 
  left_join(vacc_imces$imce_upper[,c("id","High risk (Five times the average risk of catching and transmitting the COVID-19 virus)")], by = "id") %>% 
  rename(imce_upper = `High risk (Five times the average risk of catching and transmitting the COVID-19 virus)`)

ggplot(pimce_wide_trans, aes(x = est_wgtd, y = est_unwgtd, color = age)) +
  geom_point() +
  labs(x = "pIMCE", y = "IMCE", color = "Subject's age") +
  theme_minimal()+
  theme(legend.position = "bottom")

ggsave("figures/figure_f3.png", width = 5, height = 5, dpi = 300)

#### Figure F4 ####

pimce_long_trans <- pimce_wide_trans %>% 
  pivot_longer(cols = starts_with("est_"),
               names_to = "type",
               names_prefix = "est_",
               values_to = "est") %>% 
  mutate(ci_lower = ifelse(type == "wgtd", pIMCE_lower, imce_lower),
         ci_upper = ifelse(type == "wgtd", pIMCE_upper, imce_upper)) %>% 
  select(id, age, type, est, age, ci_lower, ci_upper) %>% 
  group_by(type) %>% 
  arrange(est) %>% 
  mutate(est_order = 1:nrow(pimce_wide)) %>% 
  ungroup() %>% 
  mutate(age_cat = case_when(age < 25 ~ "<25 years old",
                             age >= 25 & age < 35 ~ "25-34 years old",
                             age >= 35 & age < 45 ~ "35-44 years old",
                             age >= 45 & age < 55 ~ "45-54 years old",
                             age >= 55 & age < 65 ~ "55-64 years old",
                             age >= 65 & age < 75 ~ "65-74 years old",
                             age >= 75 ~ ">75 years old")) %>% 
  mutate(age_cat = factor(age_cat, levels = c("<25 years old",
                                              "25-34 years old",
                                              "35-44 years old",
                                              "45-54 years old",
                                              "55-64 years old",
                                              "65-74 years old",
                                              ">75 years old")),
         label = ifelse(type == "unwgtd", "IMCE","pIMCE"))

pimce_effect_trans <- ggplot(pimce_long_trans, aes(x = est_order, y = est, color = label)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = label), alpha = 0.1, color = NA) +
  scale_fill_discrete(guide = "none") +
  labs(x = "", y = "Estimate", color = "Estimator") +
  theme_minimal()+
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(pimce_long)/2))

pimce_hist_trans <- pimce_long_trans %>% 
  filter(type == "wgtd") %>% 
  ggplot(aes(x = est_order, fill = age_cat)) +
  geom_histogram(binwidth = 30,position="stack") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(y = "pIMCE", x = "") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  # guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_x_continuous(limits = c(0, nrow(pimce_long)/2))

imce_hist_trans <- pimce_long_trans %>% 
  filter(type == "unwgtd") %>% 
  ggplot(aes(x = est_order, fill = age_cat)) +
  geom_histogram(binwidth = 30,position="stack") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(y = "IMCE", x = "Subject", fill = "Subject's Age") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(pimce_long)/2))

plot_grid(pimce_effect_trans, pimce_hist_trans, imce_hist_trans,
          ncol = 1, align = "HV", rel_heights = c(0.6,0.15,0.25))

ggsave("figures/figure_f4.png",dpi=300, width = 8.5, height = 10)

#### Figure G1 ####

plot_c2 <- plot(example_sim) +
  aes(color = c2) +
  facet_wrap(~Level, ncol = 2) +
  scale_color_gradient(low = "dodgerblue2", high = "firebrick2") +
  labs(x = "", color = expression(c[2])) +
  guides(color = guide_colorbar(title.position="top", title.hjust = 0.5)) +
  theme(legend.position = c(0.75,0.25),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "darkgray"))

ggsave(plot = plot_c2, "figures/figure_g1.pdf", width = 7, height = 3)

#### Figure G2 ####

gc()

set.seed(89)
batches <- 5
k_batches <- sample(1:5, size = nrow(vacc_data), replace = TRUE)
summary(as.factor(k_batches))

ovf_mods <- list()
ovf_imces <- list()

for (k in 1:5) {
  message(paste0("Running batch ",k,"/",batches))
  ovf_mods[[k]] <- cjbart(vacc_data[k_batches == k,], Y = "select", id  = "id", cores = 8, seed = 89)
  ovf_imces[[k]] <- IMCE(vacc_data, ovf_mods[[k]],
                         attribs = c("income"),
                         ref_levels = c("Average income level"), cores =8)$imce
}

attribute <- "Lowest 20% income level"
covar <- "ideology"

for (k in 1:5) {
  
  ovf_imce <- ovf_imces[[k]]
  ovf_imce$ideology_cat <- as.factor(ovf_imce$ideology)
  plot_data_k <- ovf_imce[!is.na(ovf_imce[["ideology_cat"]]) & !is.na(ovf_imce[[attribute]]),]
  
  plot_data_k$imce <- plot_data_k[[attribute]]
  plot_data_k$covar <- plot_data_k$ideology_cat
  plot_data_k$batch <- k
  
  plot_data_k <- select(plot_data_k, imce, covar, batch, ideology_cat) %>% distinct()
  
  plot_data_k <- plot_data_k[order(plot_data_k$imce),]
  plot_data_k$effect_order <- 1:nrow(plot_data_k)
  
  if (k == 1) {
    ovf_plot <- plot_data_k
  } else {
    ovf_plot <- rbind(ovf_plot, plot_data_k)
  }
  
}

ovf_lines <- ggplot(ovf_plot, aes(x = effect_order, y = imce, color = as.factor(batch))) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "IMCE", color = "Training batch") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, nrow(plot_data_k))) +
  theme(legend.position = "bottom")

ovf_hist <- ggplot(ovf_plot, aes(x = effect_order, fill = ideology_cat)) +
  facet_grid(rows = vars(batch), labeller = labeller(batch = function (x) paste0("Batch ",x))) +
  geom_histogram(binwidth = 60, position="stack") +
  labs(x = "", y = "Density", color = str_to_title(covar), fill = "Ideology") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, nrow(plot_data_k)))

plot_grid(ovf_lines, ovf_hist, ncol = 1, align = "v", rel_heights = c(1,0.75*(5/2)))
ggsave("figures/figure_g2.png", dpi=300, width = 7, height = 8)

#### Figure G3 ####

attribute2 <- "65 years old"

plot_data2 <- imce_sched[!is.na(imce_sched[[covar]]) & !is.na(imce_sched[[attribute2]]),]

plot_data2 <- plot_data2[order(plot_data2[[attribute2]]),]
plot_data2$effect_order <- 1:nrow(plot_data2)

plot_data2$imce <- plot_data2[[attribute2]]
plot_data2$covar <- plot_data2[[covar]]

conf_intvl_ideo2 <- vacc_imces$imce_lower %>% 
  select(id, `65 years old`) %>% 
  
  left_join({vacc_imces$imce_upper %>% select(id, `65 years old`)},
            by = "id")

colnames(conf_intvl_ideo2) <- c("id","ideo_l","ideo_u")

plot_data2 <- left_join(plot_data2, conf_intvl_ideo2, by = "id")

cor.test(plot_data2$`65 years old`, plot_data2$ideology)

effect_line2 <- ggplot(plot_data2, aes(x = effect_order, y = imce)) +
  geom_line() +
  geom_ribbon(aes(ymin = ideo_l, ymax = ideo_u), fill = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = mean(plot_data2[[attribute2]]), 
             color = "dodgerblue",
             size = 0.2) +
  labs(x = "", y = "IMCE") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(0, nrow(plot_data2)))

covar_density2 <- ggplot(plot_data2, aes(x = effect_order, fill = as.factor(covar))) +
  geom_histogram(binwidth = 60,position="stack") +
  labs(x = "", y = "Density", color = str_to_title(covar), fill = "Ideology") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(limits = c(0, nrow(plot_data2)))

plot_grid(effect_line2, covar_density2, ncol = 1, align = "v", rel_heights = c(0.6,0.4))
ggsave("figures/figure_g3.png", dpi=300, width = 7, height = 4)

t_apdx <- Sys.time()

################################################################################
## RUNTIMES                                                                   ##
# Figure 2
t_f2 - t_start
# Figure 3
t_f3 - t_f2
# Figure 4
t_f4 - t_f3
# Figure 5
t_f5 - t_f4
# Figure 6
t_f6 - t_f5
# Figure 7
t_f7 - t_f6
# Appendices
t_apdx - t_f7
################################################################################
## Package requirements

sessionInfo()
