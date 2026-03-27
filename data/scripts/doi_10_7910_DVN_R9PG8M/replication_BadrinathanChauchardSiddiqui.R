# Code for replicating tables and figures in Misinformation and Support for Vigilantism
# Authors: Sumitra Badrinathan & Josiah Gottfried

library(cowplot)
library(dotwhisker)
library(dplyr)
library(fixest)
library(ggalt)
library(ggplot2)
library(mice)
library(multiwayvcov)
library(readr)
library(stargazer)
library(stringi)
library(tidyr)
library(vctrs)
library(rlang)
library(broom)

# If functions below throw errors while creating stargazer tables, here is a quick fix for stargazer
# Uncomment below code if you have not run it before and your R version >= 4.2 (you will only have to do this once)

# ## Quick fix for stargazer <= 5.2.3 is.na() issue with long model names in R >= 4.2
# ## Alexey Knorre: https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53
# # Unload stargazer if loaded
#detach("package:stargazer",unload=T)
# # Delete it
#remove.packages("stargazer")
# # Download the source
#download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# # Unpack
#untar("stargazer_5.2.3.tar.gz")
# # Read the sourcefile with .inside.bracket fun
#stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# # Move the length check 5 lines up so it precedes is.na(.)
#stargazer_src[1990] <- stargazer_src[1995]
#stargazer_src[1995] <- ""
# # Save back
#writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# # Compile and install the patched package
#install.packages("stargazer", repos = NULL, type="source")
#library(stargazer)

#### Data ####

# load data
setwd("~/Dropbox")
all <- read.csv("~/Dropbox/Misinformation in India & Pakistan/APSR/Acceptance/Dataverse/data_final_simplified.csv") 
# the simplified dataset converts don't know / refused values to NAs and defines scales so that higher numbers represent stronger agreement with the question

# split into India and Pakistan subsets
ind <- subset(all, all$country=="india", !endsWith(colnames(all), "_p"))
pak <- subset(all, all$country=="pakistan", !endsWith(colnames(all), "_i"))

#### Functions ####

# function for calculating mean and std errors clustered at respondent level
clustered_mean <- function(data, indep.var = "misinfo_belief", cluster = "respondent_id"){
  # THIS FUNCTION CREATES A TABLE WITH THE MEAN ESTIMATE OF A VARIABLE AND
  # CONFIDENCE MARGIN BASED ON CLUSTERED STANDARD ERRORS
  
  # INPUT
  #   data: A DATA FRAME
  #   indep.var: THE STRING CONTAINING THE NAME OF A COLUMN IN THE DATA FRAME
  #   cluster: A VARIABLE TO CLUSTER BY
  
  # OUTPUT
  #   A REGRESSION TABLE INCLUDING A 95% CONFIDENCE MARGIN
  
  require(broom)
  require(fixest)
  formula <- as.formula(paste(indep.var, "~ 1"))
  regression <- feols(formula, data = data, cluster = cluster)
  out <- cbind(tidy(regression), n = regression$nobs)
  out$margin <- qt(0.975, out$n - 1) * out$std.error
  out
}

# function for analysis of deviance
chisq <- function(variable, indep.var = "any_correction", data) {
  # THIS FUNCTION CONDUCTS A CHI SQUARE TEST FOR A BINARY INDEPENDENT VARIABLE 
  # AND A FACTOR DEPENDENT VARIABLE
  
  # INPUT
  #   variable: A STRING CONTAINING THE NAME OF A FACTOR DEPENDENT VARIABLE
  #   indep.var: A STRING CONTAINING THE NAME OF A BINARY INDEPENDENT VARIABLE
  #   data: A DATA FRAME
  
  # OUTPUT
  #   A DATA FRAME CONTAINING THE RESULTS OF THE TEST
  
  formula <- as.formula(paste(indep.var, variable, sep = "~"))
  outdf <- broom::tidy(anova(glm(formula, data = data, family = "binomial"), test = "Chisq"))
  outdf <- subset(outdf, term != "NULL")
  outdf
}

# function for checking if variable is binary
is.binary <- function(x, na.rm = TRUE) {
  if (is.numeric(x)) {
    max(x, na.rm = na.rm) - min(x, na.rm = na.rm) == 1
  } else if (is.factor(x)) {
    nlevels(x) == 2
  } else return(FALSE)
  
}

# function for modifying Latex output from stargazer function
stargazer2 <- function(stargazer_output){
  # THIS FUNCTION MODIFIES THE OUTPUT OF THE STARGAZER FUNCTION WITH FIND AND REPLACE
  
  # INPUT
  #   stargazer_output: THE LATEX OUTPUT FROM RUNNING THE STARGAZER FUNCTION
  
  # OUTPUT
  #   PRINTS MODIFIED STARGAZER TABLE
  
  # fixed patterns
  
  patt_repl1 <- c("\\begin{tabular}", "\t\\adjustbox{max width=\\textwidth, max totalheight = 0.9\\textheight}{%\n\\begin{tabular}", # the first of each pair is the pattern, the second is the replacement
                  "\\end{tabular}", "\\end{tabular}}",
                  "& India", "& \\textit{India}",
                  "& Pakistan", "& \\textit{Pakistan}",
                  " x ", " $\\times$ ")
  
  intermediate_out <- stri_replace_all_fixed(
    str = stri_paste(stargazer_output, collapse = "\n"),
    pattern = patt_repl1[c(T, F)], # the odd indexes are the patterns
    replacement = patt_repl1[c(F, T)], # the even indexes are the replacements
    vectorize_all = F
  )
  
  # regex patterns
  
  patt_repl2 <- c("\\\\\\\\\\[-1.8ex\\] & \\(1\\) & .*\\)\\\\\\\\ \n", "",
                  "(?<=Positionality 1: Police.{1,100})\\\\\\\\ \n ", "\\\\\\\\\n  Investigates Crime", # insert "Investigates Crime" under "Positionality 1: Police"
                  "(?<=Positionality 2: Police.{1,100})\\\\\\\\ \n ", "\\\\\\\\\n  Prosecutes Vigilantes", # insert "Prosecutes Vigilantes" under "Positionality 2: Police"
                  "(?<=Crime: Tearing Down.{1,100})\\\\\\\\ \n ", "\\\\\\\\\n  Posters") # insert "Posters" under "Crime: Tearing Down"
  
  final_out <- stri_replace_all_regex(
    str = intermediate_out,
    pattern = patt_repl2[c(T, F)], # the odd indexes are the patterns
    replacement = patt_repl2[c(F, T)], # the even indexes are the replacements
    vectorize_all = F
  )
  
  cat(final_out)
}

# helper function for creating main effect tables
effect_table <- function(dep_vars, indep_vars, datalist, title, caption,
                         dep.var.labels, dep.var.labels.include, column.labels, 
                         covariate.labels, omit, p, type) {
  
  # create model and cluster SEs by respondent ID
  mod <- mapply(\(X,Y) lm(as.formula(paste(Y, "~", indep_vars)), data = X), 
                X = rep(datalist, each = length(dep_vars)), 
                Y = rep(dep_vars, length(datalist)), SIMPLIFY = FALSE)
  rse <- lapply(1:length(mod), 
                \(i) sqrt(diag(cluster.vcov(mod[[i]], datalist[[ceiling(i/length(dep_vars))]]$respondent_id))))
  for (i in 1:length(mod)) mod[[i]]$call$formula <- mod[[i]]$terms # workaround to enable separate DV labels for each column
  
  # arguments to pass to stargazer
  stargazer_args <- list(mod, se = rse, digits = 3, 
                         star.cutoffs = c(0.05, 0.01, 0.001), 
                         title = title,
                         dep.var.caption = caption, 
                         dep.var.labels = dep.var.labels,
                         dep.var.labels.include = dep.var.labels.include,
                         column.labels = column.labels,
                         covariate.labels = covariate.labels,
                         omit = omit)
  
  # include adjusted p value if provided
  if (!is.null(p)) {
    p <- p[c(paste0("ind.", dep_vars), paste0("pak.", dep_vars))]
    stargazer_args <- append(stargazer_args, list(p = p, report = "vc*p"))
  }
  
  # if text is requested, print the text version of the Stargazer table
  if (type == "text") {
    do.call(stargazer, append(stargazer_args, list(type = "text")))
    # else print modified latex Stargazer table
  } else {
    do.call(stargazer, stargazer_args) %>%
      capture.output() %>%
      stargazer2()
    cat("\n")
  }
  
}

# function for creating effect tables
# first, define default covariate labels
label.dict = c(
  any_correction = "Correction Treatment",
  any_positionality1 = "Positionality 1: Police",
  any_positionality2 = "Positionality 2: Police",
  vignette_typecow_transport = "Crime: Cow Transport",
  vignette_typelove_jihad = "Crime: Love Jihad",
  vignette_typequran = "Crime: Burning Quran",
  vignette_typereligious_poster = "Crime: Tearing Down",
  support_vigil_self = "Own Support",
  support_vigil_neighbors = "Neighborhood Support",
  punish_mob = "Should the vigilante mob be punished?",
  district = "District",
  respondent_id = "Respondent ID",
  enumerator = "Enumerator ID"
)
default.covariate.labels <- label.dict[1:7]
default.covariate.labels.ind <- label.dict[1:5]
default.covariate.labels.pak <- label.dict[c(1:3, 6:7)]
title_dvs <- c(support_vigil_self = "on Support for Vigilantism",
               support_vigil_neighbors = "on Support for Vigilantism",
               punish_mob = "on Support for Punishing Mob")
captions <- c(support_vigil_self = "Support for vigilantism",
              support_vigil_neighbors = "Support for vigilantism",
              punish_mob = "Should the vigilante mob be punished?",
              misinfo_belief = "Do you believe the claim in the story is accurate?")
dv_labels <- c(support_vigil_self = "Own Support",
               support_vigil_neighbors = "Neighborhood Support",
               punish_mob = "Support for Punishing Mob")
effect_tables <- function(tables = c(1, 2), dep_vars_list = NULL, 
                          indep_vars = "any_correction + any_positionality1 + any_positionality2 + vignette_type", 
                          interaction_var = NULL, fixed_var = NULL, 
                          data_list = list(India = ind, Pakistan = pak),
                          title_start = "Main Effects of Treatments", title_end = NULL,
                          covariate.labels = c(default.covariate.labels, NA),
                          p = NULL, omit = NULL, type = "latex") {
  # THIS FUNCTION CREATES STARGAZER TABLES THAT SHOW THE TREATMENT AND CONDITION
  # EFFECTS
  
  # INPUT
  #   tables: AN INTEGER OR VECTOR REPRESENTING WHICH TABLES TO CREATE (CREATES
  #     BOTH BY DEFAULT)
  #   indep_vars: A STRING CONTAINING THE RHS OF THE FORMULA
  #   interaction_var: A STRING CONTAINING THE NAME OF A VARIABLE TO INTERACT WITH 
  #     THE REDUCED PLAUSIBILITY TREATMENT (OVERIDDEN IF indep_vars IS USED); FIXED
  #     EFFECT APPEARS FIRST AMONG THE VARIABLES AND INTERACTION EFFECT APPEARS LAST
  #   fixed_var: A STRING CONTAINING THE NAME OF A VARIABLE TO INCLUDE IN THE MODEL
  #     AS A FIXED EFFECT (OVERIDDEN IF indep_vars IS USED); APPEARS LAST AMONG
  #     THE VARIABLES
  #   datalist: A LIST CONTAINING THE DATA TO CREATE TABLES FOR (BOTH INDIA AND
  #     PAKISTAN DATA BY DEFAULT)
  #   title: THE TITLE TO APPEAR IN THE STARGAZER OUTPUT
  #   column.labels: THE COLUMN LABELS TO APPEAR IN THE STARGAZER OUTPUT (INDIA
  #     AND PAKISTAN BY DEFAULT)
  #   covariate.labels: THE COVARIATE LABELS TO APPEAR IN THE STARGAZER OUTPUT
  #     (COVARIATE LABELS FOR INDIA AND PAKISTAN BY DEFAULT)
  #   p: A LIST OF NUMERIC VECTORS WITH NAMES IN THE FORMAT country.dv (E.G., 
  #     "ind.support_vigil_self") THAT WILL REPLACE THE DEFAULT P-VALUES FOR EACH
  #     MODEL
  #   omit: A VECTOR THAT SPECIFIES WHICH OF THE EXPLANATORY
  #     VARIABLES SHOULD BE OMITTED FROM THE TABLE
  #   type: THE TYPE OF OUTPUT, "latex" OR "text"
  
  # OUTPUT
  #   PRINTS STARGAZER TABLES
  
  # check inputs
  if(class(data_list) != "list"){
    stop("data_list must be a list")
  }
  
  # define variables for quick access tables
  if (1 %in% tables) {
    dep_vars_list <- append(dep_vars_list, list(c("support_vigil_self", "support_vigil_neighbors")))
  }
  if (2 %in% tables) {
    dep_vars_list <- append(dep_vars_list, list("punish_mob"))
  }
  
  # add interaction and fixed effects to independent variables
  if(!is.null(interaction_var)) interaction_var <- paste(interaction_var, "*") # add interaction effect variable if defined
  if(!is.null(fixed_var)) fixed_var <- paste("+", fixed_var) # add fixed effects variable if defined
  indep_vars <- paste(interaction_var, indep_vars, fixed_var)
  
  for (i in 1:length(dep_vars_list)) {
    
    # create title
    title <- paste(title_start, title_dvs[dep_vars_list[[i]][1]])
    if (!is.null(title_end)) {
      title <- paste(title, title_end)
    }
    
    # create captions
    caption <- captions[dep_vars_list[[i]][1]]
    
    # create dependent variable labels
    if (length(dep_vars_list[[i]]) > 1) {
      dep.var.labels <- rep(dv_labels[dep_vars_list[[i]]], length(data_list))
      dep.var.labels.include <- TRUE
    } else {
      dep.var.labels <- NULL
      dep.var.labels.include <- FALSE
    }
    
    # create column labels
    column.labels = rep(names(data_list), each = length(dep_vars_list[[i]]))
    
    # effect table
    cat(paste("\nTable", i))
    effect_table(dep_vars = dep_vars_list[[i]], indep_vars = indep_vars, datalist = data_list,
                 title = title, caption = caption, dep.var.labels = dep.var.labels,
                 dep.var.labels.include = dep.var.labels.include, column.labels = column.labels,
                 covariate.labels = covariate.labels, omit = omit, p = p, type = type)
  }
  
}




#### Tables Main Paper ####

##### Table 1 #####

effect_tables(tables = 1, type = "latex")


##### Table 2 #####

effect_tables(tables = 2, type = "latex")


##### Table 3 #####

# only include vignettes where elite messaging was used
ind_cow_transport <- subset(ind, vignette_type == "cow_transport")
pak_religious_posters <- subset(pak, vignette_type == "religious_poster")

effect_tables(tables = 1, 
              indep_vars = "any_elite + any_correction + any_positionality1 + any_positionality2", 
              data_list = list(India = ind_cow_transport, Pakistan = pak_religious_posters), 
              title_start = "Main Effect of Elite Messaging",
              type = "latex",
              covariate.labels = c("Elite Message", default.covariate.labels[1:3], NA))


##### Table 4 #####

effect_tables(tables = 1,
              interaction_var = "trust_minority",
              title_start = "Heterogeneous Effect of Trust in Minority",
              type = "latex",
              covariate.labels = c("Trust in Minority", default.covariate.labels, "Correction x Trust in Minority", NA))



#### Figures Main Paper ####
##### Figure 3 #####

# trust in religious majority versus trust in religious minority

# exclude Pakistani non-Muslim respondents from this analysis (all Indian respondents are Hindu)
all_wo_pakminority <- subset(all, country == "india" | religion_p == 1)

# summarize trust in majority and trust in minority variables
trust_maj_tab <- prop.table(table(all_wo_pakminority$trust_majority, all_wo_pakminority$country, useNA = "ifany"), margin = 2)[1:4,]
trust_maj <- as.data.frame(trust_maj_tab); trust_maj$Var3 <- "Majority"
trust_min_tab <- prop.table(table(all_wo_pakminority$trust_minority, all_wo_pakminority$country, useNA = "ifany"), margin = 2)[1:4,]
trust_min <- as.data.frame(trust_min_tab); trust_min$Var3 <- "Minority"

# combine summary data frames and manipulate
trust_df <- rbind(trust_maj, trust_min)
trust_df$Var1 <- as.factor(as.integer(as.integer(trust_df$Var1) < 2.5)) # convert trust level to binary, with 2.5 as the cutoff point
trust_df$Var2 <- as.factor(ifelse(trust_df$Var2 == "india", "India", "Pakistan"))
trust_df <- aggregate(trust_df$Freq, by = list(trust_df$Var1, trust_df$Var2, trust_df$Var3), FUN = sum) # redefine trust_maj so that Var1 has only two levels
names(trust_df) <- c("Var1", "Var2", "Var3", "Freq")
levels(trust_df$Var2) <- rev(levels(trust_df$Var2)) # reverse factor levels so that India appears above Pakistan

# separate data frames
trust_maj <- trust_df[trust_df$Var3 == "Majority",]
trust_min <- trust_df[trust_df$Var3 == "Minority",]

# colors
colors <- c("#115f9a", "#bd981c")

# trust majority plot
p1 <- ggplot(trust_maj, aes(x = Var2, fill = Var1)) +
  geom_bar(data = subset(trust_maj, Var1 == 0), aes(y = Freq), stat = "identity") +
  geom_text(data = subset(trust_maj, Var1 == 0), aes(y = Freq, label = paste0(round(Freq * 100), "%")), stat = "identity", hjust = -0.3, size = 5) +
  geom_bar(data = subset(trust_maj, Var1 == 1), aes(y = -Freq), stat = "identity") +
  geom_text(data = subset(trust_maj, Var1 == 1), aes(y = -Freq, label = paste0(round(Freq * 100), "%")), stat = "identity", hjust = 1.3, size = 5) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(-1.1, 1.1)) +
  coord_flip() +
  theme_minimal() %+replace%
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(),
        panel.grid = element_blank())

# trust minority plot
p2 <- ggplot(trust_min, aes(x = Var2, fill = Var1)) +
  geom_bar(data = subset(trust_min, Var1 == 0), aes(y = Freq), stat = "identity") +
  geom_text(data = subset(trust_min, Var1 == 0), aes(y = Freq, label = paste0(round(Freq * 100), "%")), stat = "identity", hjust = -0.3, size = 5) +
  geom_bar(data = subset(trust_min, Var1 == 1), aes(y = -Freq), stat = "identity") +
  geom_text(data = subset(trust_min, Var1 == 1), aes(y = -Freq, label = paste0(round(Freq * 100), "%")), stat = "identity", hjust = 1.3, size = 5) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(-1.1, 1.1)) +
  coord_flip() +
  theme_minimal() %+replace%
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_blank(),
        panel.grid = element_blank())

# title, legend, and caption
title <- ggdraw() + 
  draw_label(
    "How much trust do you have in the religious majority / minority?",
    x = 0,
    hjust = 0,
    size = 16
  )

legend <- ggdraw() + 
  geom_text(data.frame(x = c(0.38, 0.73), y  = c(0.5, 0.5), lab = c("Not very much / None at all", "Great deal / Quite a lot")),
            mapping = aes(x, y, label = lab),
            hjust = 0.5,
            color = rev(colors),
            size = 5.2,
            fontface = "bold")

caption <- ggdraw() + 
  draw_label(
    "Majority/minority refers to Hindus/Muslims in India and Muslims/non-Muslims in Pakistan",
    x = 1,
    hjust = 1,
    size = 11
  )

# combine plots into a single grid
plot_grid(
  title,
  legend,
  p1 + theme(legend.position = "none"), 
  NULL, # for an extra space between graphs
  p2 + theme(legend.position = "none"),
  caption,
  rel_heights = c(0.2, 0.5, 1, 0.25, 1, 0.3),
  labels = c("", "", "Majority", "", "Minority", "",""),
  label_fontface = "italic",
  label_y = 1.15,
  hjust = -0.25,
  ncol = 1
)


##### Figure 4 #####

# effect of treatment on mean perceived accuracy of misinformation

list.of.tables <- lapply(c(split(ind, paste(ind$any_correction, ind$vignette_type)),
                           split(pak, paste(pak$any_correction, pak$vignette_type))),
                         clustered_mean)
belief_data <- do.call(rbind, list.of.tables)
categories <- do.call(rbind, strsplit(rownames(belief_data), " "))
belief_data$correction = factor(categories[,1], levels = c(1, 0))
belief_data$vignette = factor(categories[,2], levels = c("love_jihad", "corona", "cow_transport",
                                                         "blasphemy", "quran", "religious_poster"))

# plot
ggplot(belief_data, aes(x = vignette, y = estimate, fill = as.factor(correction))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = estimate - margin, ymax = estimate + margin), width = 0.15, position = position_dodge(0.9)) +
  ggtitle("How accurate is the claim in the story?", subtitle = "(1 = Not at all accurate, 4 = Very accurate)\n\n") +
  labs(caption = "\nError bars show 95% confidence level") +
  ylab("Mean Perceieved Accuracy of Misinformation") +
  scale_x_discrete(labels = c("Love\nJihad", "Covid", "Cow\nTransport", "Blasphemy", "Quran", "Religious\nPosters")) +
  scale_fill_discrete(name = "Correction", labels = c("Treatment", "Control")) +
  coord_cartesian(ylim = c(1, 4)) +
  theme_minimal() %+replace%
  theme(plot.title = element_text(size = 16, hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 14, hjust = 0),
        plot.caption = element_text(size = 11, hjust = 1, vjust = 0),
        axis.title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
        legend.position = c(0.5, 1),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.background = element_rect(fill="white", linetype = 0)
  )


##### Figure 5 #####

# feeling thermometer ratings toward politicians

# density plot for Imran Khan feeling thermometer
p1 <- ggplot(pak, aes(x = feelings_khan_p)) +
  geom_density(color = "darkgreen", fill = "darkgreen", alpha = 0.2) +
  ggtitle("Feelings Toward Imran Khan") +
  xlab("Feeling Thermometer") +
  ylab("Density") +
  scale_y_continuous(limits = c(0, 0.047)) +
  theme_minimal() %+replace%
  theme(plot.title = element_text(size = 16, hjust = 0),
        plot.title.position = "plot",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        panel.grid.minor.y = element_blank())

# density plot for Narendra Modi feeling thermometer
p2 <- ggplot(ind, aes(x = feelings_modi_i)) +
  geom_density(color = "darkorange", fill = "darkorange", alpha = 0.2) +
  ggtitle("Feelings Toward Narendra Modi") +
  xlab("Feeling Thermometer") +
  ylab("Density") +
  scale_y_continuous(limits = c(0, 0.047)) +
  theme_minimal() %+replace%
  theme(plot.title = element_text(size = 16, hjust = 0),
        plot.title.position = "plot",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        panel.grid.minor.y = element_blank())

# combine both plots into a single grid
plot_grid(p1, NULL, p2, rel_heights = c(1, 0.1, 1), ncol = 1)




#### Appendix Tables and Figures (posted to CUP online) ####
##### Figure D3 (Appendix) #####

# mean belief in misinformation by vignette

# table for perceived accuracy of misinformation versus vignette type
misinfo <- as.matrix(table(all$misinfo_belief, all$vignette_type))
colnames(misinfo) <- c("pak_vig_1", "ind_vig_2", "ind_vig_3", "ind_vig_1", "pak_vig_2", "pak_vig_3")

# convert table to data frame
mean_belief_mat <- t(t(misinfo[1:4,]) %*% 1:4 / colSums(misinfo[1:4,]))
mean_belief <- data.frame(Vignette = colnames(mean_belief_mat), Mean = t(mean_belief_mat))

# barplot
ggplot(mean_belief, aes(x = Vignette, y = Mean)) +
  geom_bar(stat = "identity") +
  ggtitle("How accurate is the claim in the story?", subtitle = "(1 = Not at all accurate, 4 = Very accurate)\n\n") +
  ylab("Mean Belief in Misinformation") +
  scale_x_discrete(labels = c("Love\nJihad", "Covid", "Cow\nTransport", "Blasphemy", "Quran", "Religious\nPosters")) +
  coord_cartesian(ylim = c(1, 4)) +
  theme_minimal() %+replace%
  theme(plot.title = element_text(size = 16, hjust = 0),
        plot.title.position = "plot",
        axis.title = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 12),
  )



##### Tables E1 and E2 (Appendix) #####

# subset: pakistani sunnis
pak$sunni <- as.integer(pak$sect_p %in% c(2, 3, 4))
sunni <- subset(pak, sunni == 1)

effect_tables(title_end = "Among Sunnis",
              data_list = list(Sunni = sunni),
              covariate.labels = c(default.covariate.labels.pak, NA),
              type = "latex")


##### Tables F3 and F4 (Appendix) #####

# create variable for pre-treatment support for punishing the accused even if guilt is uncertain
ind$accused_judgment_punish <- as.integer(ind$accused_judgment == 1)
pak$accused_judgment_punish <- as.integer(pak$accused_judgment == 1)

# india
main_variables_i <- c("misinfo_belief" = "Belief in Misinformation",
                      "support_vigil_self" = "Support for Vigilantism (Self)",
                      "support_vigil_neighbors" = "Support for Vigilantism (Neighbors)",
                      "punish_mob" = "Support for Punishing Mob",
                      "any_correction" = "Correction Treatment",
                      "any_positionality1" = "Positionality 1",
                      "any_positionality2" = "Positionality 2",
                      "any_elite" = "Elite Messaging",
                      "feelings_modi_i" = "Feelings Toward Modi",
                      "trust_majority" = "Trust in Religious Majority",
                      "trust_minority" = "Trust in Religious Minority",
                      "trust_media" = "Trust in Media",
                      "news_trust_republic_i" = "Trust in Republic TV",
                      "news_trust_aajtak_i" = "Trust in Aaj Tak",
                      "news_trust_jagran_i" = "Trust in Dainik Jagran",
                      "news_trust_zee_news_i" = "Trust in Zee News",
                      "news_trust_ndtv_i" = "Trust in NDTV",
                      "news_trust_amar_ujala_i" = "Trust in Amar Ujala",
                      "news_trust_bhaskar_i" = "Trust in Dainik Bhaskar",
                      "news_trust_hindustan_i" = "Trust in Hindustan Dainik",
                      "news_trust_news18_i" = "Trust in News 18",
                      "accused_judgment_punish" = "Pre-Treat Support for Punishing Accused",
                      "democracy_speech" = "Support for Free Speech",
                      "democracy_due_process" = "Support for Due Process",
                      "democracy_punishment" = "Crim. Should Be Severely Punished",
                      "democracy_majority" = "Laws Should Favor Rel. Maj.",
                      "democracy_army" = "Army Should Overthrow Incomp. Govt.",
                      "democracy_violent_protest" = "Support for Violent Protest")

stargazer(as.data.frame(subset(ind, select = names(main_variables_i))), summary = TRUE, digits = 3,
          title = "Summary of India Variables", covariate.labels = main_variables_i, type = "latex")

# pakistan
main_variables_p <- c("misinfo_belief" = "Belief in Misinformation",
                      "support_vigil_self" = "Support for Vigilantism (Self)",
                      "support_vigil_neighbors" = "Support for Vigilantism (Neighbors)",
                      "punish_mob" = "Support for Punishing Mob",
                      "any_correction" = "Correction Treatment",
                      "any_positionality1" = "Positionality 1",
                      "any_positionality2" = "Positionality 2",
                      "any_elite" = "Elite Messaging",
                      "feelings_khan_p" = "Feelings Toward Khan",
                      "trust_majority" = "Trust in Religious Majority",
                      "trust_minority" = "Trust in Religious Minority",
                      "trust_media" = "Trust in Media",
                      "accused_judgment_punish" = "Pre-Treat Support for Punishing Accused",
                      "democracy_speech" = "Support for Free Speech",
                      "democracy_due_process" = "Support for Due Process",
                      "democracy_punishment" = "Crim. Should Be Severely Punished",
                      "democracy_majority" = "Laws Should Favor Rel. Maj.",
                      "democracy_army" = "Army Should Overthrow Incomp. Govt.",
                      "democracy_violent_protest" = "Support for Violent Protest",
                      "democracy_due_process" = "Support for Due Process",
                      "sunni" = "Sunni")

stargazer(as.data.frame(subset(pak, select = names(main_variables_p))), summary = TRUE, digits = 3,
          title = "Summary of Pakistan Variables", covariate.labels = main_variables_p, type = "latex")


##### Tables G5 and G6 (Appendix) #####

etable(
  list(
    feols(support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | district, vcov = ~respondent_id, data = ind),
    feols(support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | district, vcov = ~respondent_id, data = ind),
    feols(support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | district, vcov = ~respondent_id, data = pak),
    feols(support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | district, vcov = ~respondent_id, data = pak)
  ),
  dict = label.dict,
  title = "Main Effects on Support for Vigilantism with District FEs",
  headers = list("\\textit{India}" = 2, "\\textit{Pakistan}" = 2), 
  digits = 3, 
  adjustbox = "max width=\\textwidth, max totalheight = 0.9\\textheight", 
  tex = TRUE
)

etable(
  list(
    feols(punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | district, vcov = ~respondent_id, data = ind),
    feols(punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | district, vcov = ~respondent_id, data = pak)
  ),
  dict = label.dict,
  title = "Main Effects on Support for Punishing Mob with District FEs",
  headers = list("\\textit{India}" = 1, "\\textit{Pakistan}" = 1), 
  digits = 3, 
  adjustbox = "max width=\\textwidth, max totalheight = 0.9\\textheight", 
  tex = TRUE
)


##### Tables H7 and H8 (Appendix) #####

# convert dependent variables to standard deviation units
dep.vars <- c("Belief in Misinformation" = "misinfo_belief", 
              "Support for Vigilantism (Self)" = "support_vigil_self",
              "Support for Vigilantism (Neighbors)" = "support_vigil_neighbors", 
              "Support for Punishing Mob" = "punish_mob")

ind.stand <- ind
pak.stand <- pak

for(dv in dep.vars){
  ind.stand[[dv]] <- (ind[[dv]] - mean(ind[[dv]], na.rm = T)) / sd(ind[[dv]], na.rm = T)
  pak.stand[[dv]] <- (pak[[dv]] - mean(pak[[dv]], na.rm = T)) / sd(pak[[dv]], na.rm = T)
}

effect_tables(data_list = list(India = ind.stand, Pakistan = pak.stand), 
              title_end = "(SD Units)", 
              type = "latex")


##### Tables I9 and I10 (Appendix) #####

# remove respondents who did not receive the correction
ind_no_correction <- subset(ind, any_correction == 0)
pak_no_correction <- subset(pak, any_correction == 0)

effect_tables(indep_vars = "any_positionality1 + any_positionality2 + vignette_type",
              data_list = list(India = ind_no_correction, Pakistan = pak_no_correction),
              title_end = "(No Correction)",
              covariate.labels = default.covariate.labels[-1],
              type = "latex")

##### Tables J11 to J13 (Appendix) #####

effect_tables(tables = 2,
              interaction_var = "trust_minority",
              title_start = "Heterogeneous Effect of Trust in Minority",
              type = "latex",
              covariate.labels = c("Trust in Minority", default.covariate.labels, 
                                   "Correction x Trust in Minority", NA))

effect_tables(interaction_var = "friend_minority",
              title_start = "Heterogeneous Effect of Comfort with Having Close Minority Friends",
              type = "latex",
              covariate.labels = c("Comfort with Minority Friends", default.covariate.labels, 
                                   "Correction x Comfort with Minority Friends", NA))


##### Tables L14 and L15 (Appendix) #####

# fixed effects for enumerator

# support for vigilantism
etable(
  list(
    feols(support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | enumerator, vcov = ~respondent_id, data = ind),
    feols(support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | enumerator, vcov = ~respondent_id, data = ind),
    feols(support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | enumerator, vcov = ~respondent_id, data = pak),
    feols(support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | enumerator, vcov = ~respondent_id, data = pak)
  ), 
  dict = label.dict,
  title = "Main Effects on Support for Vigilantism with Enumerator FEs",
  headers = list("\\textit{India}" = 2, "\\textit{Pakistan}" = 2), 
  digits = 3, 
  adjustbox = "max width=\\textwidth, max totalheight = 0.9\\textheight", 
  tex = TRUE
)

# support for punishing mob
etable(
  list(
    feols(punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | enumerator, vcov = ~respondent_id, data = ind),
    feols(punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | enumerator, vcov = ~respondent_id, data = pak)
  ), 
  dict = label.dict,
  title = "Main Effects on Support for Punishing Mob with Enumerator FEs",
  headers = list("\\textit{India}" = 1, "\\textit{Pakistan}" = 1), 
  digits = 3, 
  adjustbox = "max width=\\textwidth, max totalheight = 0.9\\textheight", 
  tex = TRUE
)




#### Supplemental Material Tables and Figures (posted to Dataverse) ####

##### Tables A1 to A4 (Supplemental Material) #####
# create variable for corrections in vignettes 1 and 2
ind$correction_in_v1 <- rep(as.integer(ind[1:(nrow(ind)/3), "any_correction"] == 1), 3)
pak$correction_in_v1 <- rep(as.integer(pak[1:(nrow(pak)/3), "any_correction"] == 1), 3)
ind$correction_in_v2 <- rep(as.integer(ind[(nrow(ind)/3+1):(2*nrow(ind)/3), "any_correction"] == 1), 3)
pak$correction_in_v2 <- rep(as.integer(pak[(nrow(pak)/3+1):(2*nrow(pak)/3), "any_correction"] == 1), 3)

# split out data for second and third vignettes
ind_second <- subset(ind, vignette_type == second_vignette)
pak_second <- subset(pak, vignette_type == second_vignette)
ind_third <- subset(ind, vignette_type == third_vignette)
pak_third <- subset(pak, vignette_type == third_vignette)

# test for second vignette only
effect_tables(indep_vars = "correction_in_v1 + any_correction + any_positionality1 + any_positionality2 + vignette_type",
              title_start = "Fixed Effects for Correction in First Vignette",
              title_end = "(Second Vignette Only)",
              data_list = list(India = ind_second, Pakistan = pak_second),
              covariate.labels = c("Correction in First Vignette", default.covariate.labels, NA),
              type = "latex")

# test for third vignette only
effect_tables(indep_vars = "correction_in_v1 + correction_in_v2 + any_correction + any_positionality1 + any_positionality2 + vignette_type",
              title_start = "Fixed Effects for Corrections in First and Second Vignettes",
              title_end = "(Third Vignette Only)",
              data_list = list(India = ind_third, Pakistan = pak_third),
              covariate.labels = c("Correction in First Vignette", "Correction in Second Vignette", 
                                   default.covariate.labels, NA),
              type = "latex")


##### Tables B5 and B6 (Supplemental Material) #####

ind$vignette_type_order <- factor(case_when(ind$vignette_type == ind$first_vignette ~ paste(ind$first_vignette, 1),
                                            ind$vignette_type == ind$second_vignette ~ paste(ind$second_vignette, 2),
                                            ind$vignette_type == ind$third_vignette ~ paste(ind$third_vignette, 3)))
pak$vignette_type_order <- factor(case_when(pak$vignette_type == pak$first_vignette ~ paste(pak$first_vignette, 1),
                                            pak$vignette_type == pak$second_vignette ~ paste(pak$second_vignette, 2),
                                            pak$vignette_type == pak$third_vignette ~ paste(pak$third_vignette, 3)))

effect_tables(tables = NULL,
              dep_vars = list(c("support_vigil_self", "support_vigil_neighbors", "punish_mob")),
              indep_vars = "any_correction + any_positionality1 + any_positionality2 + vignette_type_order", 
              data_list = list(ind),
              title_start = "Main Specification with Profile-Order Effects",
              title_end = "(India)",
              covariate.labels = c(default.covariate.labels[1:3], "Second: Corona", "Third: Corona", 
                                   "First: Cow Transport", "Second: Cow Transport", "Third: Cow Transport", 
                                   "First: Love Jihad", "Second: Love Jihad", "Third: Love Jihad"),
              type = "latex")

effect_tables(tables = NULL,
              dep_vars = list(c("support_vigil_self", "support_vigil_neighbors", "punish_mob")),
              indep_vars = "any_correction + any_positionality1 + any_positionality2 + vignette_type_order", 
              data_list = list(pak),
              title_start = "Main Specification with Profile-Order Effects",
              title_end = "(Pakistan)",
              covariate.labels = c(default.covariate.labels[1:3], "Second: Blashpemy", "Third: Blasphemy", 
                                   "First: Burning Quran", "Second: Burning Quran", "Third: Burning Quran", 
                                   "First: Tearing Down", "Second: Tearing Down", "Third: Tearing Down"),
              type = "latex")


##### Tables C7 and C8 (Supplemental Material) #####

# analysis of deviance across demographic variables in India
demographic_ind <- c("age" = "Age", 
                     "factor(gender)" = "Gender", 
                     "factor(caste_i)" = "Caste",
                     "factor(education)" = "Education",
                     "factor(locality)" = "Locality",
                     "factor(income)" = "Income", 
                     "factor(occupation)" = "Occupation",
                     "factor(land_ownership_i)" = "Land Ownership",
                     "factor(house_type)" = "House Type", 
                     "factor(asset_fan)" = "Asset: Fan",
                     "factor(asset_airconditioner)" = "Asset: Air Conditioning",
                     "factor(asset_cycle)" = "Asset: Cycle", 
                     "factor(asset_fridge)" = "Asset: Fridge", 
                     "factor(asset_toilet)" = "Asset: Toilet",
                     "factor(asset_cooler_i)" = "Asset: Cooler",
                     "factor(asset_computer_i)" = "Asset: Computer",
                     "factor(asset_cellphone_i)" = "Asset: Cell Phone",
                     "factor(asset_car_i)" = "Asset: Car",
                     "factor(asset_washer_i)" = "Asset: Washer", 
                     "factor(asset_tv_i)" = "Asset: TV",
                     "factor(asset_stove_i)" = "Asset: Stove", 
                     "factor(asset_purifier_i)" = "Asset: Purifier",
                     "factor(asset_geyser_i)" = "Asset: Geyser",
                     "factor(asset_generator_i)" = "Asset: Generator", 
                     "factor(asset_account_i)" = "Asset: Bank Account", 
                     "factor(asset_internet_i)" = "Asset: Internet",
                     "factor(asset_card_i)" = "Asset: Banking Card")

randomization_ind <- t(sapply(names(demographic_ind), chisq, data = ind))
randomization_ind[, c("deviance", "p.value")] <- round(as.numeric(randomization_ind[, c("deviance", "p.value")]), 3)
randomization_ind <- randomization_ind[, c("df", "deviance", "p.value")]
colnames(randomization_ind) <- c("DF", "Deviance", "P-value")
rownames(randomization_ind) <- demographic_ind
stargazer(randomization_ind, type = "latex",
          title = "Chi-Square Tests for India Variables")

# analysis of deviance across demographic variables in Pakistan
demographic_pak <- c("age" = "Age", 
                     "factor(gender)" = "Gender", 
                     "factor(mother_tongue_p)" = "Mother Tongue",
                     "factor(sect_p)" = "Sect",
                     "factor(education)" = "Education",
                     "factor(locality)" = "Locality",
                     "factor(income)" = "Income", 
                     "factor(occupation)" = "Occupation",
                     "factor(house_type)" = "House Type", 
                     "factor(rooms_p)" = "Number of Rooms",
                     "factor(asset_fan)" = "Asset: Fan",
                     "factor(asset_airconditioner)" = "Asset: Air Conditioning",
                     "factor(asset_cycle)" = "Asset: Cycle", 
                     "factor(asset_fridge)" = "Asset: Fridge", 
                     "factor(asset_toilet)" = "Asset: Toilet",
                     "factor(asset_pump_p)" = "Asset: Water Pump")

randomization_pak <- t(sapply(names(demographic_pak), chisq, data = pak))
randomization_pak[, c("deviance", "p.value")] <- round(as.numeric(randomization_pak[, c("deviance", "p.value")]), 3)
randomization_pak <- randomization_pak[, c("df", "deviance", "p.value")]
colnames(randomization_pak) <- c("DF", "Deviance", "P-value")
rownames(randomization_pak) <- demographic_pak
stargazer(randomization_pak, type = "latex",
          title = "Chi-Square Tests for Pakistan Variables")

# proportions receiving correction treatment across selected variables
mean(ind$any_correction) # global mean

mean(ind$any_correction[ind$gender == 1]) # male
mean(ind$any_correction[ind$gender == 2]) # female

mean(ind$any_correction[ind$occupation == 1], na.rm = TRUE) # student (n=183)
mean(ind$any_correction[ind$occupation == 2], na.rm = TRUE) # government job (n=132)
mean(ind$any_correction[ind$occupation == 3], na.rm = TRUE) # private job (n=903)
mean(ind$any_correction[ind$occupation == 4], na.rm = TRUE) # agricultural work (n=2433)
mean(ind$any_correction[ind$occupation == 5], na.rm = TRUE) # homemaker (n=1125)
mean(ind$any_correction[ind$occupation == 6], na.rm = TRUE) # don't know / retired (n=324)
mean(ind$any_correction[ind$occupation == 7], na.rm = TRUE) # other (n=402)


##### Tables D9 and D10 (Supplemental Material) #####

# main effects for misinfo belief
effect_tables(tables = NULL, dep_vars = "misinfo_belief", type = "latex")

# interaction effect of vignette type on misinfo belief
effect_tables(tables = NULL,
              dep_vars = "misinfo_belief",
              interaction_var = "vignette_type", 
              title_start = "Heterogeneous Effect of Vignette Type",
              type = "latex",
              covariate.labels = c("Crime: Cow Transport", "Crime: Love Jihad",
                                   "Crime: Burning Quran", "Crime: Tearing Down",
                                   default.covariate.labels[1:3], "Correction x Cow Transport",
                                   "Correction x Love Jihad", "Correction x Burning Quran",
                                   "Correction x Tearing Down"))


##### Tables E11 and E12 (Supplemental Material) #####

# support for vigilantism
etable(
  list(
    feols(support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | respondent_id, data = ind),
    feols(support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | respondent_id, data = ind),
    feols(support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | respondent_id, data = pak),
    feols(support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | respondent_id, data = pak)
  ), 
  dict = label.dict,
  title = "Main Effects on Support for Vigilantism with Respondent FEs",
  headers = list("\\textit{India}" = 2, "\\textit{Pakistan}" = 2), 
  digits = 3, 
  adjustbox = "max width=\\textwidth, max totalheight = 0.9\\textheight", 
  tex = TRUE
)

# support for punishing mob
etable(
  list(
    feols(punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | respondent_id, data = ind),
    feols(punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type | respondent_id, data = pak)
  ), 
  dict = label.dict,
  title = "Main Effects on Support for Punishing Mob with Respondent FEs",
  headers = list("\\textit{India}" = 1, "\\textit{Pakistan}" = 1), 
  digits = 3, 
  adjustbox = "max width=\\textwidth, max totalheight = 0.9\\textheight", 
  tex = TRUE
)


##### Tables F13 to F16 (Supplemental Material) #####

# Benjamini-Hochberg procedure

# get the original p values
modlist <- list(
  ind.support_vigil_self = lm(data = ind, support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type),
  ind.support_vigil_neighbors = lm(data = ind, support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type),
  ind.punish_mob = lm(data = ind, punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type),
  pak.support_vigil_self = lm(data = pak, support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type),
  pak.support_vigil_neighbors = lm(data = pak, support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type),
  pak.punish_mob = lm(data = pak, punish_mob ~ any_correction + any_positionality1 + any_positionality2 + vignette_type)
)

# adjust p values across DVs
p_adj_dvs <- bind_rows(lapply(modlist, \(x) broom::tidy(x)), .id = "country.dv") %>%
  mutate(country = sub("\\..*", "", country.dv),
         term = case_when(stri_detect_fixed(term, "vignette_type") ~ "vignette_type",
                          TRUE ~ term)) %>%
  group_by(country, term) %>%
  mutate(p.value = p.adjust(p.value, method = "BH")) %>%
  split(x = .[["p.value"]], f = .[["country.dv"]])

effect_tables(p = p_adj_dvs, title_end = "(P-Values Adjusted Across DVs)", type = "latex")

# adjust p values across countries
p_adj_countries <- bind_rows(lapply(modlist, \(x) broom::tidy(x)), .id = "country.dv") %>%
  mutate(dv = sub(".*\\.", "", country.dv),
         term = case_when(stri_detect_fixed(term, "vignette_type") ~ "vignette_type",
                          TRUE ~ term)) %>%
  group_by(dv, term) %>%
  mutate(p.value = p.adjust(p.value, method = "BH")) %>%
  split(x = .[["p.value"]], f = .[["country.dv"]])

effect_tables(p = p_adj_countries, title_end = "(P-Values Adjusted Across Countries)", type = "latex")


##### Tables G17 to G20 (Supplemental Material) #####

# multiple imputation

# drop metadata columns, calculated columns, and any columns with more than 2% NAs
cols_to_examine <- ind %>%
  select(-c(X, control, correction_only:elite_only, first_vignette:third_vignette, submission_date_i:vidhansabha_constituency_i, political_party_i, sub_caste_i, fi_observation:respondent_id)) %>%
  summarize(across(everything(), \(x) mean(is.na(x)))) %>%
  pivot_longer(everything()) %>%
  filter(value <= 0.02) %>%
  pull(name)

dat_to_impute <- ind %>%
  select(all_of(c(cols_to_examine, "support_vigil_self", "support_vigil_neighbors"))) %>%
  mutate(across(c(where(is.binary), vignette_type, caste_i, occupation, house_type), as.factor), # convert factors
         education = if_else(education == 7, NA, education)) # treating education as continuous in this analysis, so eliminate 3 "other" responses

# view NA counts
dat_to_impute %>%
  summarize(across(everything(), \(x) sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "num_nas") %>%
  arrange(desc(num_nas))

# prediction matrix
model_variables <- c("support_vigil_self", "support_vigil_neighbors", "any_correction", "any_positionality1", "any_positionality2", "vignette_type")
pred <- quickpred(dat_to_impute, mincor = 0.1, include = model_variables)

# add correlated factor variables to imputation sets for vigilantism variables
dat_to_impute %>%
  select(support_vigil_self, support_vigil_neighbors, caste_i, occupation, house_type) %>%
  mutate(missing_svs = as.integer(is.na(support_vigil_self)),
         missing_svn = as.integer(is.na(support_vigil_neighbors))) %>%
  fastDummies::dummy_cols(ignore_na = TRUE, remove_selected_columns = TRUE) %>%
  cor(use = "pairwise.complete.obs") %>%
  as_tibble(rownames = "variable") %>%
  filter(!(variable %in% c("support_vigil_self", "missing_svs", "support_vigil_neighbors", "missing_svn"))) %>%
  select(variable, support_vigil_self, missing_svs, support_vigil_neighbors, missing_svn) %>%
  filter(apply(.[,-1], 1, \(x) max(abs(x), na.rm = TRUE)) > 0.1)

pred["support_vigil_self", c("occupation", "house_type")] <- 1
pred["support_vigil_neighbors", "occupation"] <- 1

# impute data (or load from rds file)
# imputed_dat <- mice(dat, m = 5, maxit = 20, pred = pred, method = "pmm", seed = 123, print = FALSE)
imputed_dat <- read_rds("imputed_dat.rds")

# check convergence
plot(imputed_dat)

# density plots
densityplot(imputed_dat, ~support_vigil_self, xlab = "Support for Vigilantism (Self)")
densityplot(imputed_dat, ~support_vigil_neighbors, xlab = "Support for Vigilantism (Neighbors)")

# create models
svs_fit <- with(imputed_dat, expr = lm(support_vigil_self ~ any_correction + any_positionality1 + any_positionality2 + vignette_type))
svn_fit <- with(imputed_dat, expr = lm(support_vigil_neighbors ~ any_correction + any_positionality1 + any_positionality2 + vignette_type))

# view pooled results
summary(pool(svs_fit))
summary(pool(svn_fit))

# robustness check: every missing point is 1
ind_na1 <- ind
ind_na1$support_vigil_self[is.na(ind_na1$support_vigil_self)] <- 1
ind_na1$support_vigil_neighbors[is.na(ind_na1$support_vigil_neighbors)] <- 1
effect_tables(tables = 1, 
              data_list = list(India = ind_na1), 
              title_end = "(Every Missing Data Point is 1)",
              type = "latex")

# robustness check: every missing point is 10
ind_na10 <- ind
ind_na10$support_vigil_self[is.na(ind_na10$support_vigil_self)] <- 10
ind_na10$support_vigil_neighbors[is.na(ind_na10$support_vigil_neighbors)] <- 10
effect_tables(tables = 1, 
              data_list = list(India = ind_na10), 
              title_end = "(Every Missing Data Point is 10)",
              type = "latex")

# table for NAs
na_tab <- ind %>%
  group_by(any_correction) %>%
  summarize(svs_na_prop = mean(is.na(support_vigil_self)), 
            svs_na_margin = qt(0.975, df = sqrt(length(support_vigil_self)) - 1,
                               sd(is.na(support_vigil_self)) / sqrt(length(support_vigil_self))),
            svn_na_prop = mean(is.na(support_vigil_neighbors)),
            svn_na_margin = qt(0.975, df = sqrt(length(support_vigil_neighbors)) - 1,
                               sd(is.na(support_vigil_neighbors)) / sqrt(length(support_vigil_neighbors))))

stargazer(na_tab, summarize = FALSE)




##### Tables H21 and H22 (Supplemental Material) #####

# elite messaging x elite evaluations

ind_cow_transport <- subset(ind, vignette_type == "cow_transport")
pak_religious_posters <- subset(pak, vignette_type == "religious_poster")

effect_tables(tables = 1, 
              indep_vars = "feelings_modi_i*any_elite + any_correction + any_positionality1 + any_positionality2", 
              data_list = list(India = ind_cow_transport), 
              title_start = "Elite Messaging x Feelings Toward Modi",
              type = "latex",
              covariate.labels = c("Feelings Toward Modi", "Elite Message", 
                                   default.covariate.labels[1:3], "Elite Message x"))

effect_tables(tables = 1, 
              indep_vars = "feelings_khan_p*any_elite + any_correction + any_positionality1 + any_positionality2", 
              data_list = list(Pakistan = pak_religious_posters), 
              title_start = "Elite Messaging x Feelings Toward Khan",
              type = "latex",
              covariate.labels = c("Feelings Toward Khan", "Elite Message", 
                                   default.covariate.labels[1:3], "Elite Message x"))
