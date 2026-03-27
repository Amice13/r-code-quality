# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Replication script : 'Nonverbal Communication of Partisan Conflict'         #    
# Authors: Frederik Hjorth and Mathias Rask                                   #
# Affiliations: University of Copenhagen and Aarhus University                #
# Date: June 19, 2025                                                         #
# Email: mathiasrask@ps.au.dk                                                 #
# R version: 4.4.0 (2024-04-24) -- "Puppy Cup"                                #
# Platform: Platform: x86_64-pc-linux-gnu                                     #
# OS: Ubuntu 20.04.6 LTS                                                      #
# Packages:                                                                   #
#           - psych_2.5.3,                                                    #
#           - irr_0.84.1                                                      #     
#           - kableExtra_1.3.4                                                #
#           - modelsummary_1.4.1                                              #
#           - fixest_0.12.0                                                   #
#           - data.table_1.16.4                                               #
#           - viridis_0.6.5                                                   #
#           - geomtextpath_0.1.5                                              #
#           - ggtext_0.1.2                                                    #
#           - broom_1.0.6                                                     #
#           - tidyverse_2.0.0                                                 #
#           - lubridate_1.9.4                                                 #
#           - pacman_0.5.1                                                    #  
#                                                                             #
# Datasets:                                                                   #                                                                 
#           - main.rds                                                        #
#           - validation1.rds                                                 # 
#           - validation2.rds                                                 #
#           - votes.rds                                                       #
#           - afinn.rds                                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# SETUP ----

# Clear working environment
rm(list=ls())

# Install package management tool if not already installed
if (!"pacman" %in% rownames(installed.packages())) {
  install.packages('pacman')
} else {
  message("The package is already installed.")
}

# Load (or install prior to loading if not already installed) remotes and
# install old version (1.4.1) of `modelsummary.
# This is necessary to create the tables in the script as it uses
# `kableExtra` to customize the table generated with `modelsummary`.
# Piping the output from `modelsummary` to `kableExtra` is only supported
# in version 1.x of `modelsummary`.
if ("modelsummary" %in% rownames(installed.packages())) {
  if (packageVersion("modelsummary") == "1.4.1") {
    message("The correct version (1.4.1) of modelsummary is installed.")
  } else {
    message("The package is installed, but not the correct version. Version 1.4.1 will be installed.")
    pacman::p_load(remotes)
    remotes::install_version("modelsummary", version = "1.4.1", repos = "http://cran.us.r-project.org")
  }
} else {
  message("The package is not installed. Modelsummary 1.4.1 will be installed.")
  pacman::p_load(remotes)
  remotes::install_version("modelsummary", version = "1.4.1", repos = "http://cran.us.r-project.org")
}

# Load libraries used to produce figures and tables
pacman::p_load(tidyverse,
               ggtext, 
               geomtextpath,
               viridis,
               data.table,
               fixest, 
               lubridate,
               irr,
               psych, 
               broom,
               kableExtra,
               modelsummary)


# Setup folders to avoid errors when running the code
folders <- c("tables", "figures", "figures/main", "figures/appendix")
for (folder in folders) {
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
}

# GLOBALS ----

# Define color scale, 20 colors from the viridis palette
vircols <- scales::viridis_pal()(20)

# Illustration of the scale 
# Uncomment to show
# scales::show_col(vircols)

# Define list of data subsets
subsets <- list(
  NULL,
  ~speakerbloc == 'Left',
  ~speakerbloc == 'Right'
)

subsets <- list(NULL, NULL, NULL)

# TABLE PREP ----

# Regression table specifications to be used in table production
rows <- tribble(~term, ~"(1)", ~"(2)", ~"(3)",~"(4)", ~"(5)",
                'Topic FE', '\\xmark',   '\\xmark', '\\xmark', '\\cmark', '\\cmark')

# Specify position of the rows object above in the table
attr(rows, 'position') <- c(9, 10)

# Regression statistics to be included in the table
gm2 <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2))

# Define model names
n_models <- 5
model_names <- toupper(letters)[1:n_models]
model_list <- vector("list", length = n_models)
names(model_list) <- model_names

# LOAD DATA ----

# Read-in main dataset
adf <- read_rds("data/main.rds")

# Prep data for H3. Only politicians who have spoken in at least 5
# so-called high-profile debates are included in the analysis.
high_profile_n <- adf %>%
  filter(high_profile_debate == 1) %>%
  summarise(nobs = n(), .by=speaker)

adf1 <- adf %>%
  left_join(high_profile_n,by="speaker") %>%
  filter(nobs >= 5)


# FUNCTIONS ----

## AUXILIARY ----

# Melt data from wide to long
w2l <- function(df, vars) {
  d <- melt(df, id.vars=vars[1], variable.name=vars[2], value.name=vars[3])
}

# Labeling axis in ggplot
html.label <- function(df, ...) {
  cols <- quos(...)
  col_names <- sapply(cols, quo_name)
  new_col_names <- paste0(col_names, ".lab")
  map2(cols, new_col_names, ~ {
    df %>%
      mutate(!!.y := paste("<span style='color: ", 
                           ifelse(.data[[.x]] %in% c("ALT", "EL", "RV", "S", "SF"), "Red", "Blue"), 
                           ";'>", 
                           .data[[.x]], 
                           "</span>", 
                           sep = ""))
  }) %>% 
    reduce(full_join, by = names(df))
}

# Heatmap labeling function
label.heatmap <- function(x) { 
  w2l(x, vars=vars) |> 
    html.label(speakerparty, targetparty)
} 

# Intra-class correlation coefficient
icr <- function(data) {
  icc_estimate <- irr::icc(data, model="twoway", type='consistency', unit='single')
  
  result <- data.frame(icc_estimate[7], 
                       icc_estimate[14],
                       icc_estimate[15])
  names(result) <- c("Arousal", "upperconf", "lowerconf")
  return(result)
}


## ESTIMATION ----

# Define model formula
model.formula <- function(v) {
  model_fmls <- list(
    formula(paste("spitch ~", v)),
    formula(paste("spitch ~", v, "+ textsent")),
    formula(paste("spitch ~", v, "+ emotionality")),
    formula(paste("spitch ~", v, "| topictext")),
    formula(paste("spitch ~", v, "+ textsent + emotionality | topictext"))
  )
  return(model_fmls)
}


# Estimate model with fixest::feols()
model.estimate <- function(fmls, data, cluster=NULL) {
  
  # Define a list to store the models
  models <- list()
  
  # Create the models in nested loops
  for (subs in subsets) {
    for (model in fmls) {
      model <- feols(fml = model, data = data, cluster = cluster, subset = subs)
      models[[length(models) + 1]] <- model
    }
  }
  
  # Define subset names
  model_subset <- c(rep('all', length(fmls)), 
                    rep('left', length(fmls)), 
                    rep('right', length(fmls)))
  
  names(models) <- paste(paste0('model', 1:length(fmls)), model_subset, sep = '_')
  
  return(models)
}


# Tidying output from models
model.tidy <- function(m, v) {
  model <- broom::tidy(m)
  
  model$conf.high95 <- model$estimate + 1.96*model$std.error
  model$conf.low95 <- model$estimate - 1.96*model$std.error
  
  model$conf.high90 <- model$estimate + 1.645*model$std.error
  model$conf.low90 <- model$estimate - 1.645*model$std.error
  
  model <- model[model$term == v,]
  
  return(model)
}


# Prepare output from model.tidy() for plots and tables
model.prep <- function(tidy, v, subset=F) {
  
  # Combine into dataframe
  models_tidy <- data.table::rbindlist(tidy)
  
  # Prepare grouping variables for plotting
  models_tidy$group <- rep(c('Baseline', 'Sentiment', 'Emotionality', 'Topics', 'Combined'), 3)
  models_tidy$bloc <- rep(c('All', 'Left', 'Right'), each = length(tidy) / 3)
  models_tidy$blocfac <- fct_relevel(models_tidy$bloc, 'Left', 'All', 'Right')
  models_tidy$groupfac <- fct_relevel(models_tidy$group, 'Baseline', 'Sentiment', 'Emotionality', 'Topics', 'Combined')
  
  if (!subset) {
    models_tidy <- models_tidy %>%
      filter(bloc == 'All')
    return(models_tidy)
  } else {
    return(models_tidy)
  }
}

# Add fixed effect to model formula
model.addfe <- function(feols_formula, fe='dyad') {
  formula_string <- as.character(feols_formula)
  y <- formula_string[2]
  f <- formula_string[1]
  x <- formula_string[3]
  if (length(unlist(str_split(x, '\\|'))) > 1) {
    x <- paste(formula_string[3], '+', fe)
  }
  else {
    x <- paste(formula_string[3], '|', fe)
  }
  f <- formula(paste(formula_string[2], formula_string[1],  x))
  return(f)
}  


## TABLE ----
regtable <- function(models, 
                     coef_map, 
                     table_ix, 
                     gof_map=gm2,
                     fmt=4,
                     output="latex",
                     stars=TRUE,
                     add_rows=NULL,
                     title=NULL,
                     notes="Standard errors clustered at the dyad level.",
                     dependent_variable="Standardized Pitch",
                     folder="tables/") {
  
  if (is.null(title)) {
    title <- paste("Regression table for", table_ix)
  }
  
  
  tab <- msummary(models=models, 
                  stars=stars,
                  coef_map = coef_map,
                  title=title,
                  gof_map = gof_map,
                  fmt=fmt,
                  add_rows = add_rows,
                  label=paste0("tab:", tolower(table_ix)),
                  notes=notes,
                  output = output) %>%
    kableExtra::add_header_above(c(" " = 1,
                                   "Dependent Variable: Standardized Pitch" = 5),
                                 escape = FALSE
    ) %>%
    kable_styling(latex_options = c("hold_position"))
  
  if (!is.null(add_rows)) {
    rownumber <- as.integer(10-nrow(add_rows))
    tab <- tab %>% kableExtra::row_spec(rownumber, extra_latex_after = "\\midrule")
  }
  
  tab %>% 
    cat(., file = paste0(folder, table_ix, '.tex'))
  
  return(tab)
}


## PLOTTING ----

plot_main <- function(model, xlab=NULL,ylab=NULL,ylim=c(0.0, 0.25),legend.position=c(0.2, 0.2)) {
  
  p <- ggplot(model, aes(blocfac, estimate, colour=groupfac)) + 
    
    geom_hline(yintercept=0,linetype="dashed",alpha=.5) +
    
    geom_errorbar(aes(ymin=conf.low90,ymax=conf.high90), 
                  width=0, linewidth=1.75, position = position_dodge(width = 0.5)) + 
    geom_errorbar(aes(ymin=conf.low95,ymax=conf.high95), 
                  width=0,linewidth=1.25, position = position_dodge(width = 0.5)) + 
    
    scale_color_manual(values=c(vircols[1], vircols[5], 
                                vircols[10], vircols[15],
                                vircols[18])) +
    geom_point(position = position_dodge(width = 0.5), size=4) + 
    labs(x=xlab, y=ylab, color=NULL) +
    ylim(ylim[1], ylim[2]) + 
    theme_light() + theme(legend.position = legend.position, 
                          legend.key=element_blank(),
                          legend.background = element_blank(),
                          legend.text=element_text(size=16),
                          axis.text = element_text(size=16),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank()) 
  
  return(p)
}


plot_wordlimit <- function(form, 
                           data, 
                           cluster, 
                           iv, 
                           ylim_coord, 
                           ylim_add1=0.0, 
                           ylim_add2=0.0, 
                           wordlimits=seq(0, 100, 10),
                           legend_pos = "none",
                           text_size=14) {
  
  results <- vector("list", length(wordlimits))
  for (wl in seq_along(wordlimits)) {
    res <- model.estimate(fmls=form, data=subset(data, terms >= word_limits[wl]), cluster=cluster) %>%
      map(., model.tidy, v=iv) %>%
      model.prep()
    
    res <- res %>%
      filter(groupfac == 'Baseline') %>%
      mutate(wordlimit = word_limits[wl])
    results[[wl]] <- res
  }
  
  word <- rbindlist(results)
  word$original <- ifelse(word$wordlimit == 40, 'Original', 'New limit')
  
  p <- ggplot(word, aes(as.factor(wordlimit), estimate, color=original)) + 
    geom_hline(yintercept=0,linetype="dashed",alpha=.5) +
    geom_errorbar(aes(ymin=conf.low90,ymax=conf.high90), 
                  width=0, linewidth=1.75, position = position_dodge(width = 0.5)) + 
    geom_errorbar(aes(ymin=conf.low95,ymax=conf.high95), 
                  width=0,linewidth=1.25, position = position_dodge(width = 0.5)) + 
    scale_color_manual(values=c('grey80', 'grey30')) +
    geom_point(position = position_dodge(width = 0.5), size=3) + 
    xlab('Word Limit') + ylab(NULL) + 
    ylim(ylim_coord[1], ylim_coord[2]) +
    theme_light() + theme(legend.position = legend_pos,
                          legend.key=element_blank(),
                          legend.background = element_blank(),
                          legend.text=element_text(size=text_size),
                          axis.text = element_text(size=text_size),
                          axis.title.x = element_text(size=text_size))
  
  return(p)
  
}


plot_weekdays <- function(form, 
                          data, 
                          cluster, 
                          ylim_coord, 
                          iv, 
                          ylim_add1 = 0.0,
                          ylim_add2 = 0.0,
                          wl=40, 
                          weekdays=wdays,
                          legend_pos = c(0.8, .85),
                          text_size=14,
                          text_col='black') {
  
  
  results <- vector("list", length(weekdays))
  for (wl in seq_along(weekdays)) {
    res <- model.estimate(fmls=form, data=subset(data, terms >= wl & wday==wdays[wl]), cluster=cluster) %>%
      map(., model.tidy, v=iv) %>%
      model.prep()
    
    res <- res %>%
      mutate(wday = wdays[wl],
             wday_ix = wl)
    results[[wl]] <- res
  }
  
  wday_res <- rbindlist(results)
  
  p <- ggplot(wday_res, aes(reorder(wday, wday_ix), estimate, color=groupfac)) + 
    geom_hline(yintercept=0,linetype="dashed",alpha=.5) +
    geom_errorbar(aes(ymin=conf.low90,ymax=conf.high90), 
                  width=0, linewidth=1.75, position = position_dodge(width = 0.75)) + 
    geom_errorbar(aes(ymin=conf.low95,ymax=conf.high95), 
                  width=0,linewidth=1.25, position = position_dodge(width = 0.75)) + 
    geom_point(position = position_dodge(width = 0.75), size=3) + 
    ylim(ylim_coord[1] + ylim_add1, ylim_coord[2] + ylim_add2) + 
    scale_color_manual(values=c(vircols[1], vircols[5], 
                                vircols[10], vircols[15],
                                vircols[18])) +
    labs(x=NULL, y=NULL) + 
    theme_light() + theme(legend.position = legend_pos,
                          legend.key=element_blank(),
                          legend.background = element_blank(),
                          legend.text=element_text(size=text_size),
                          legend.title = element_blank(),
                          axis.text.y = element_text(size=text_size),
                          axis.text.x = element_text(size=text_size, angle = 0),
                          axis.title.x = element_text(size=text_size),
                          axis.ticks = element_blank()) + 
    coord_flip()
  
  return(p)
  
}


plot_gender <- function(form, 
                        data, 
                        cluster, 
                        ylim_coord, 
                        iv, 
                        ylim_add1 = 0.0,
                        ylim_add2 = 0.0,
                        wl=40, 
                        gender_values=gender_vals,
                        legend_pos = c(0.79, 0.85),
                        text_size=12,
                        text_col='black') {
  results <- vector("list", length(gender_values))
  for (wl in seq_along(gender_values)) {
    res <- model.estimate(fmls=form, data=subset(data, terms >= wl & woman == gender_values[wl]), cluster=cluster) %>%
      map(., model.tidy, v=iv) %>%
      model.prep()
    
    res <- res %>%
      mutate(gender = gender_vals[wl],
             gender_ix = wl)
    results[[wl]] <- res
  }
  
  gender_res <- rbindlist(results)
  gender_res <- gender_res %>%
    mutate(gender = ifelse(gender==0, 'Man', 'Woman'))
  
  p <- ggplot(gender_res, aes(reorder(gender, gender_ix), estimate, color=groupfac)) + 
    geom_hline(yintercept=0,linetype="dashed",alpha=.5) +
    geom_errorbar(aes(ymin=conf.low90,ymax=conf.high90), 
                  width=0, linewidth=1.75, position = position_dodge(width = 0.75)) + 
    geom_errorbar(aes(ymin=conf.low95,ymax=conf.high95), 
                  width=0,linewidth=1.25, position = position_dodge(width = 0.75)) + 
    geom_point(position = position_dodge(width = 0.75), size=3) + 
    ylim(ylim_coord[1] + ylim_add1, ylim_coord[2] + ylim_add2) + 
    scale_color_manual(values=c(vircols[1], vircols[5], 
                                vircols[10], vircols[15],
                                vircols[18])) +
    labs(x=NULL, y=NULL) + 
    theme_light() + theme(legend.position = legend_pos,
                          legend.key=element_blank(),
                          legend.background = element_blank(),
                          legend.text=element_text(size=text_size),
                          legend.title = element_blank(),
                          axis.text.y = element_text(size=text_size, color=text_col),
                          axis.text.x = element_text(size=text_size, angle = 0),
                          axis.title.x = element_text(text_size),
                          axis.ticks = element_blank()) 
  
  return(p)
}


generate_and_save_plots <- function(plot_function, param_list, prefix, filenames, width = 6, height = 6) {
  plots <- lapply(param_list, function(params) do.call(plot_function, params))
  
  for (i in seq_along(plots)) {
    ggsave(
      filename = sprintf("figures/appendix/%s.pdf", filenames[i]),
      plot = plots[[i]],
      width = width,
      height = height
    )
  }
  
  return(plots)
}


# MAIN ----

## Figure 2a - Hypothesis 1 (Polarization) ----

predictor.var <- 'outbloc'
cluster.var <- 'dyad'
wordlim <- 40
table.ix <- 'I1'
figure.ix <- '2a'
figure.filename <- file.path('figures/main', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(-.05,.4)
figure.legend.position <- c(0.80, 0.7)

fmls_H1 <- model.formula(v=predictor.var)
results_H1 <- model.estimate(fmls=fmls_H1, 
                             data=subset(adf, terms >= wordlim), 
                             cluster=cluster.var)

fig2a <- plot_main(model=map(.x=results_H1, .f=model.tidy, v=predictor.var) |> 
                     model.prep(), 
          ylim=figure.ylim,
          legend.position=figure.legend.position)

ggsave(filename=figure.filename, 
       width=figure.width, 
       height=figure.height)


## Figure 2b - Hypothesis 2 (Policy) ----

predictor.var <- 'vote_binary_all'
cluster.var <- 'dyad'
wordlim <- 40
table.ix <- 'I2'
figure.ix <- '2b'
figure.filename <- file.path('figures/main', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(-.05,.4)
figure.legend.position <- "none"

fmls_H2 <- model.formula(v=predictor.var)
results_H2 <- model.estimate(fmls=fmls_H2, 
                             data=subset(adf, terms >= wordlim), 
                             cluster=cluster.var)

fig2b <- plot_main(model=map(.x=results_H2, .f=model.tidy, v=predictor.var) |> 
                     model.prep(), 
                   ylim=figure.ylim, 
                   legend.position = figure.legend.position)

ggsave(filename=figure.filename, 
       width=figure.width, 
       height=figure.height)


## Figure 3a - Hypothesis 3 (Debate) ----

predictor.var <- 'high_profile_debate'
cluster.var <- 'dyad'
wordlim <- 40
table.ix <- 'I3'
figure.ix <- '3a'
figure.filename <- file.path('figures/main', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(-.05,.8)
figure.legend.position <- c(0.85, 0.7)

fmls_H3 <- model.formula(v=predictor.var)
results_H3 <- model.estimate(fmls=fmls_H3, 
                             data=subset(adf1, terms >= wordlim), 
                             cluster=cluster.var)

fig3a <- plot_main(model=map(.x=results_H3, .f=model.tidy, v=predictor.var) |> 
                     model.prep(), 
                   ylim=figure.ylim,
                   legend.position=figure.legend.position)

ggsave(filename=figure.filename, 
       width=figure.width, 
       height=figure.height)


## Figure 3b - Hypothesis 4 (Bargaining) ----

predictor.var <- 'meancip'
cluster.var <- 'targetparty'
wordlim <- 40
table.ix <- 'I4'
figure.ix <- '3b'
figure.filename <- file.path('figures/main', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(-.05,.8)
figure.legend.position <- "none"

fmls_H4 <- model.formula(v=predictor.var)
results_H4 <- model.estimate(fmls=fmls_H4, 
                             data=subset(adf, terms >= wordlim), 
                             cluster=cluster.var)

fig3b <- plot_main(model=map(results_H4, model.tidy, v=predictor.var) |> 
                     model.prep(), 
                   ylim=figure.ylim,
                   legend.position=figure.legend.position)

ggsave(filename=figure.filename, 
       width=figure.width, 
       height=figure.height)


# APPENDIX ----

## A - Bargaining Leverage and Extremity ----

# To produce Figure A1
cips <- adf %>%
  distinct(targetparty, latestgovtdate, meancip) %>%
  filter(!is.na(meancip))

### Figure A1 - Bargaining Leverage ----

figure.ix <- 'A1'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 5
figure.height <- 4
figure.legend.position <- "none"

figA1 <- ggplot(cips,aes(x=latestgovtdate,y=meancip,color=targetparty,label=targetparty)) +
  geom_labelline() +
  theme_bw() +
  scale_color_viridis_d(end = .85) +
  labs(x="Government formation date",y="Coalition inclusion probability") +
  theme(legend.position = figure.legend.position) 

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


## C - Gender Effects  ----

h1.figure.ylim <- c(-.05,.4)
h2.figure.ylim <- c(-.05,.4)
h3.figure.ylim <- c(-.05,.8)
h4.figure.ylim <- c(-.05,.8)

gender_vals <- unique(subset(adf, !is.na(woman))$woman)

gender_param_list <- list(
  list(form = fmls_H1, data = adf %>% filter(!is.na(woman)), cluster = 'dyad', iv = 'outbloc', ylim_coord = h1.figure.ylim, ylim_add2 = .2),
  list(form = fmls_H2, data = adf %>% filter(!is.na(woman)), cluster = 'dyad', iv = 'vote_binary_all', ylim_coord = h2.figure.ylim, ylim_add2 = .2, legend_pos = 'none', text_col = 'black'),
  list(form = fmls_H3, data = adf1 %>% filter(!is.na(woman)), cluster = 'dyad', iv = 'high_profile_debate', ylim_coord = h3.figure.ylim, ylim_add1 = .2),
  list(form = fmls_H4, data = adf %>% filter(!is.na(woman)), cluster = 'targetparty', iv = 'meancip', ylim_coord = h4.figure.ylim, legend_pos = 'none', text_col = 'black')
)

gender_filenames <- c("C1a", "C1b", "C2a", "C2b")
gender_plots <- generate_and_save_plots(plot_gender, gender_param_list, "gender", gender_filenames)

## D - Validation ----

# To produce Figure D1
df_binary <- read_rds('data/validation1.rds') %>%
  mutate(agreement=case_when(arousal1 == arousal2 ~ 1,
                             .default = 0))

# To produce Figure D2
df_continuous <- read_rds('data/validation2.rds')

df_valplot <- df_continuous %>%
  pivot_longer(
    cols = starts_with("arousal"),
    names_to = "coder",
    names_prefix = "arousal",
    values_to = "arousal",
    values_drop_na = TRUE
  )

# To produce Table D1
agreement_continuous <- table(df_continuous$agreefac) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(`Coder agreement` = Var1, `Continuous (Exercise 2)` = Freq)
agreement_binary <- table(df_binary$agreefac) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(`Coder agreement` = Var1, `Binary (Exercise 1)` = Freq)

agreepct_continuous <- round(sum(df_continuous$agreement)/nrow(df_continuous), 2)
agreepct_binary <- round(sum(df_binary$agreement)/nrow(df_binary), 2)

krip_continuous <- round(irr::kripp.alpha(x=rbind(df_continuous$arousal1, df_continuous$arousal2), 'interval')[['value']], 2)
krip_binary <- round(irr::kripp.alpha(x=rbind(df_binary$arousal1, df_binary$arousal2), 'nominal')[['value']], 2)

icr_continuous <- round(icr(df_continuous[, grep('arousal', names(df_continuous))])$Arousal, 2)

cohen_binary <- round(psych::cohen.kappa(x=cbind(df_binary$arousal1,df_binary$arousal2))[['kappa']], 2)

agreement_table <- agreement_binary %>%
  left_join(agreement_continuous, by='Coder agreement')

metrics <- c("Agreement pct.", 
             "Krippendorff’s $\\alpha$", 
             "Cohen’s $\\kappa$", 
             "ICC")

binary_vals <- c(agreepct_binary, krip_binary, cohen_binary, "--")
continuous_vals <- c(agreepct_continuous, krip_continuous, "--", icr_continuous)

reliability_table <- data.frame(
  x = metrics,
  y = binary_vals,
  z = continuous_vals) %>%
  rename(`Coder agreement` = x, `Binary (Exercise 1)` = y,
         `Continuous (Exercise 2)` = z)

full_table <- rbind(
  c("\\textit{Coder agreement}", "", ""),
  agreement_table,
  c("\\textit{Intercoder reliability}", "", ""),
  reliability_table
)

# To produce Figure D4
df_res <- df_continuous %>% 
  left_join(adf, by='speech_id')

# Residual dataframe
lm1resids <- lm(zPitch ~ avgarousal, data=df_continuous) %>%
  broom::augment() %>%
  bind_cols(select(df_res, speaker, party, targetparty, speech_id, text))

# Largest positive residual (i.e. pitch is greater than predicted by perceived arousal)
posresid <- lm1resids %>% 
  filter(.resid == max(.resid))

# Largest negative residual (i.e. pitch is lower than predicted by perceived arousal)
negresid <- lm1resids %>% 
  filter(.resid == min(.resid))


### Figure D1 - Binary labeling ----

figure.ix <- 'D1'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4
figure.legend.position <- "none"

figD1 <- ggplot(df_binary,aes(x=agreefac, y=pitch_norm, fill=agreefac)) +
  geom_violin() +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.5,
               colour = "black") +
  labs(x="Binary Emotional Arousal", y="Standardized Pitch") +
  scale_fill_viridis_d(begin = .2,end=.8) +
  scale_y_continuous(limits=c(-3, 3), breaks=seq(-3, 3, 1)) + 
  theme_bw() +
  theme(legend.position = figure.legend.position,
        axis.text = element_text(size=14),
        axis.title = element_text(size=16))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure D2 - Continuous labeling ----

figure.ix <- 'D2'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4
figure.legend.position <- "none"

figD2 <- ggplot(df_valplot, aes(x=arousal, y=zPitch, group=coder, color=coder, fill=coder)) +
  geom_point(aes(x=arousal, y=zPitch, color=coder), position=position_dodge(width=0.4)) +
  geom_smooth(method=lm,  
              se=TRUE,
              fullrange=TRUE)+ 
  scale_linetype_discrete(guide="none") +
  scale_color_manual(values=c(vircols[17], vircols[6]), labels=c('Coder 1', 'Coder 2'), name=NULL) +
  scale_fill_manual(values=c(vircols[17], vircols[6]), labels=c('Coder 1', 'Coder 2'), name=NULL) +
  xlab("Emotional Arousal (0-10)") +
  ylab("Standardized Pitch") +
  scale_y_continuous(limits=c(-3, 3), breaks=seq(-3, 3, 1)) + 
  scale_x_continuous(limits=c(0, 10), breaks=seq(0, 10, 2)) + 
  theme_bw() + 
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.position = figure.legend.position,
        legend.background = element_rect(color = NA)) + 
  annotate('text', x=7, y=2.5, label='Coder 1', color=vircols[6]) + 
  annotate('text', x=7, y=-.5, label='Coder 2', color=vircols[17])

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Table D1 - Reliability ----

table.ix <- 'D1'
table.filename <- file.path('tables', paste0(table.ix, '.tex'))
table.caption <- "Distribution of labels for the binary (exercise 1) and continuous (exercise 2) coding and intercoder reliability metrics. For the distribution of the continuous coding, a speech is classified as activated if arousal is labeled as 5 (the midpoint of the 0--10 scale) or larger. The category `Neither' means that the two agree that the speech is nonactivated, `One coder' means that one of the two coders have labeled a speech as activated, and `Both coders' means that the two coders agree that a speech is activated. Note that the binary coding sums to $99$ and not $100$ because one of the sampled speeches lacked aligned audio. The agreement percentage is computed using the binary representation of the continuous coding also with 5 as the cutoff."

tabD1 <- kbl(
  full_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("", "Binary (Exercise 1)", "Continuous (Exercise 2)"),
  caption = table.caption,
  label = table.ix,
  align = c("l", "c", "c")
) %>%
  add_header_above(c(" " = 1, "Coding" = 2)) %>%
  row_spec(0, bold = FALSE) %>%
  row_spec(c(4), extra_latex_after = "\\midrule") %>%
  kable_styling(latex_options = c("hold_position"),
                position = "center") %>%
  writeLines(table.filename)



### Figure D3 - Sentiment ----

figure.ix <- 'D3'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4
figure.legend.position <- "none"

figD3 <- ggplot(subset(adf, !is.na(targetparty)), aes(x=spitch, y=textsent)) +
  geom_hex(bins=100) +
  scale_fill_gradient(low="gray90", high="black") +
  geom_smooth(color='yellow2', method='lm', linetype='dashed') +
  scale_x_continuous(limits=c(-4, 4), breaks=seq(-4, 4, 1)) + 
  theme_bw() +
  theme(legend.position = figure.legend.position,
        axis.text = element_text(size=14),
        axis.title = element_text(size=16)) +
  labs(x="Standardized Pitch", y="Text Sentiment")

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure D4 - Residuals ----

figure.ix <- 'D4'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4
figure.legend.position <- "none"

figD4 <- df_res %>% 
  mutate(bigresid=ifelse(speech_id %in% c(posresid$speech_id,negresid$speech_id),1,0)) %>%
  ggplot(aes(x=avgarousal, y=zPitch)) +
  geom_smooth(method='lm', color="grey60") +
  geom_point(aes(color=factor(bigresid)), alpha=.85) +
  labs(x='Average perceived arousal', y='Standardized Pitch') +
  scale_color_manual(values=c('darkblue','green3')) +
  annotate("text", x = 5, y = -2, label = "Largest negative residual", color='green3') +
  annotate("text", x = 5.5, y = 2.6, label = "Largest positive residual", color='green3') +
  geom_segment(data=posresid, aes(x=avgarousal, y=.fitted, xend=avgarousal, yend=zPitch), linetype='dashed', color='green3') +
  geom_segment(data=negresid, aes(x=avgarousal, y=.fitted, xend=avgarousal, yend=zPitch), linetype='dashed', color='green3') +
  theme_minimal() +
  theme(legend.position=figure.legend.position)

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

### Table D2 - Residuals ----

table.ix <- 'D2'
table.filename <- file.path('tables', paste0(table.ix, '.tex'))

posresid.text.DA <- sub('\\.\\s\\(Kort bemærkning\\)', '', pull(posresid, text), perl=T) # Positive residual text 
negresid.text.DA <-  sub('\\.\\s\\(Kort bemærkning\\)', '', pull(negresid, text), perl=T)  # Negative residual text
posresid.text.EN <- "If I understand Mr Steen Gade correctly, the question is also about whether a resolution can ensure that agreement can be reached on how the UN should act in the future. All I have to say is that, as far as Kosovo is concerned, no solution was reached in the UN Security Council. Without a UN mandate and with strong warnings from Kofi Annan as Secretary-General, it was decided to take action. It may also be necessary in the future. As the Foreign Minister has already explained today, it is of course important to have a discussion in the UN Security Council about the principles and the things that form the basis for the use of force. We are in complete agreement on that. However, there is still a difference between having a discussion about it in the Security Council and then having to draft a resolution, which we know has no effect on the ground."
negresid.text.EN <- "I would never dream of showing up in the EU and then being made a fool of. And that is why I would say that Morten Østergaard's proposal is completely insane. I would never dream of showing up and making claims about something that I, on behalf of the government, have heard completely rejected many, many times here in the Danish Parliament. I think that European cooperation should be used for serious things and for things where there are concrete issues that can be dealt with, and therefore I have to say that I think that this kind of idea would be another parade show that has no purpose whatsoever other than to throw itself into a lot of claims that cannot be proven at all."

D2_df <- tibble::tibble(
  Residual = c("Positive", "Negative"),
  Original = c(posresid.text.DA, negresid.text.DA),
  Translated = c(posresid.text.EN, negresid.text.EN)
)

D2_tex <- "\\begin{table}[!h]
\\singlespacing
\\caption{Original and translated texts of largest positive and negative residuals.}\\label{apptab:residtexts}
\\begin{tabular}{p{0.10\\linewidth} p{0.40\\linewidth} p{0.40\\linewidth}}
\\hline \\\\
\\textbf{Residual} & \\textbf{Original text} & \\textbf{Translated text} \\\\
\\hline
"

for (i in 1:nrow(D2_df)) {
  D2_tex <- paste0(
    D2_tex,
    D2_df$Residual[i], " & ``", D2_df$Original[i], "'' & ``", D2_df$Translated[i], "'' \\\\\n"
  )
}

D2_tex <- paste0(D2_tex, "\\hline\n\\end{tabular}\n\\end{table}")

writeLines(D2_tex, table.filename)

## E - Word Limits ----

word_limits <- seq(0, 100, by=10)
word_param_list <- list(
  list(form = fmls_H1, data = adf, cluster = 'dyad', iv = 'outbloc', ylim_coord = h1.figure.ylim),
  list(form = fmls_H2, data = adf, cluster = 'dyad', iv = 'vote_binary_all', ylim_coord = h2.figure.ylim),
  list(form = fmls_H3, data = adf1, cluster = 'dyad', iv = 'high_profile_debate', ylim_coord = h3.figure.ylim),
  list(form = fmls_H4, data = adf, cluster = 'targetparty', iv = 'meancip', ylim_coord = h4.figure.ylim)
)

word_filenames <- c("E1a", "E1b", "E2a", "E2b")
word_plots <- generate_and_save_plots(plot_function=plot_wordlimit, 
                                      param_list=word_param_list, 
                                      prefix="wordlimit", 
                                      filenames=word_filenames)


## F - Data Description ---- 

vote_df <- read_rds('data/votes.rds')  

### Figure F1a - Duration ----

figure.ix <- 'F1a'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6

figF1a <- ggplot(adf %>% distinct(debate_id, dur), aes(x=dur)) + 
  geom_histogram(bins=50, fill=vircols[10], color="gray10") + 
  theme_light() + 
  labs(x='Seconds', y=NULL) + 
  theme_light() + 
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=16))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure F1b - Terms ----

figure.ix <- 'F1b'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6

figF1b <- adf %>%
  summarise(mterms = mean(terms), .by=debate_id) %>%
  filter(mterms < 1000) %>%
  ggplot(aes(x=exp(log(mterms)))) + 
  geom_histogram(bins=50, fill=vircols[10], color="gray10") + 
  labs(x='Terms', y=NULL) + 
  theme_light() + 
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size=16))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure F2a - Unstandardized Pitch ----

figure.ix <- 'F2a'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(0, 20000)

figF2a <- ggplot(subset(adf, abs(spitch) <= 4), aes(x=f0_avg)) +
  geom_histogram(bins=75, fill=vircols[10], color="gray10") +
  ylim(figure.ylim) +
  annotate("text", label="Men", color=vircols[10], x=130, y=19000, size=5.5) + 
  annotate("text", label="Women", color=vircols[10], x=215, y=11000, size=5.5)  +
  labs(x="Unstandardized pitch",y="") + 
  theme_light() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.title = element_text(size=16),
        axis.ticks.y = element_blank())

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

### Figure F2b - Standardized Pitch ----

figure.ix <- 'F2b'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(0, 20000)

figF2b <- ggplot(subset(adf, abs(spitch) <= 4), aes(x=spitch)) +
  geom_histogram(bins=75, fill=vircols[10], color="gray10") +
  ylim(figure.ylim) + 
  labs(x="Standardized pitch", y="") + 
  theme_light() + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=14),
        axis.title = element_text(size=16),
        axis.ticks.y = element_blank())

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Table F1 - Summary Statistics ----

table.ix <- 'F1'
table.filename <- file.path('tables', paste0(table.ix, '.tex'))
table.caption <- "Summary table for a selected set of variables in the dataset."
table.fmt <- 2
table.output <- 'latex'

datasum <- adf %>%
  mutate(Sentiment = textsent,
         Emotionality = emotionality,
         `Std. Pitch (average)` = spitch,
         `Pitch (average)` = f0_avg,
         `Pitch (modulation)` = f0_mod,
         `Pitch change (average)` = f0_change_avg, 
         `Loudness (average)`= loudness_avg,
         `Loudness (modulation)`= loudness_mod,
         Outbloc = outbloc,
         `Out-vote`=vote_binary_all,
         `Vote difference`=vote_contested,
         CIP = meancip) %>%
  filter(!is.na(targetparty) & abs(spitch) <=5) %>%
  select(c(Outbloc, `Out-vote`,  `Vote difference`, CIP,
           Sentiment, Emotionality,
           `Std. Pitch (average)`, `Pitch (average)`, `Pitch (modulation)`, `Pitch change (average)`,
           `Loudness (average)`, `Loudness (modulation)`))

datasummary_skim(data=datasum, 
                 fmt=paste0("%.", table.fmt, "f"), 
                 histogram=F, 
                 output=table.output,
                 title=table.caption,
                 label=table.ix) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(., file=table.filename)

### Figure F3a - Contestedness of Legislative Votes ----

figure.ix <- 'F3a'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4

figF3a <- ggplot(vote_df, aes(x=vote_contested)) + 
  geom_histogram(bins=75, fill=vircols[10], color="gray10") +
  labs(x='Raw vote difference') + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(size=16),
        axis.text = element_text(size=14))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure F3b - Contestedness of Legislative Votes (Natural Logarithm) ----

figure.ix <- 'F3b'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4

figF3b <- ggplot(vote_df, aes(x=log(vote_contested + 1))) + 
  geom_density(fill=vircols[10],color="gray10", alpha=.5) +
  theme_bw() + 
  labs(x='Log-transformed vote difference') + 
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(size=16),
        axis.text = element_text(size=16))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


## G - Party Dyads ----

# To produce Figure G1
out_df <- adf %>%
  filter(!is.na(outbloc)) %>%
  mutate(outbloc = case_when(outbloc == 1 ~ 1,
                             outbloc == 0 ~ 0)) %>%
  summarise(N_out = n(), .by=c(outbloc, period)) %>%
  mutate(N_out_total = sum(N_out), .by=period)

period_df <- adf %>%
  summarise(N_total = n(), .by=period) %>%
  filter(N_total >= 9000)

out_df <- out_df %>%
  left_join(period_df, by='period') %>%
  filter(!is.na(N_total)) %>%
  mutate(share_out = N_out/N_total,
         share_out_total = N_out_total / N_total)

# To produce Figure G2a
speech_matrix_prop <- table(adf$party, adf$targetparty) %>% 
  apply(., 1, function(x) x/sum(x)) %>% 
  as.data.frame() %>%
  mutate(speakerparty = rownames(.)) %>%
  setDT()

speech_matrix_freq <- table(adf$party, adf$targetparty) %>% 
  as.data.frame.matrix() %>%
  mutate(speakerparty = rownames(.)) %>% 
  setDT()

vars <- c('speakerparty', 'targetparty', 'n')

speech_matrix_list <-  map(.x=list(speech_matrix_prop, speech_matrix_freq), 
                           .f=label.heatmap) 

# To produce Figure G2b
votedist_df <- adf %>%
  filter(!is.na(vote_binary_all)) %>%
  select(c('speech_procedure_ID','party', 'targetparty', 'vote_binary_all', 'dyad'))

votedist_table <- table(votedist_df$party,
                    votedist_df$targetparty, 
                    votedist_df$vote_binary_all)
votedist_df <- data.frame(votedist_table) %>% 
  as_tibble() %>%
  mutate(dyad = str_glue("{Var1}->{Var2}")) %>%
  mutate(share = Freq/sum(Freq), .by=dyad) %>%
  filter(Var3==0) %>%
  mutate(share = ifelse(!is.na(share), share, 0.0)) %>%
  setDT() %>%
  rename(speakerparty = Var1, targetparty = Var2, freq = Freq) %>%
  inner_join(adf[, c('dyad', 'speakerbloc', 'targetbloc')], by='dyad',
             multiple='first')

votedist_df <- html.label(votedist_df, speakerparty, targetparty)

votedist_df <- votedist_df[order(votedist_df$speakerparty.lab)]

### Figure G1 - Dynamics of Dyadic Speeches ----

figure.ix <- 'G1'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 8
figure.height <- 6
figure.legend.position <- "none"

figG1 <- ggplot(out_df, aes(x=as.factor(period), y=share_out, color=as.factor(outbloc))) +
  geom_point(size=2) + 
  geom_line(aes(group=as.factor(outbloc)), linewidth=.75) + 
  geom_point(aes(x=as.factor(period), 
                 y=share_out_total), 
             color=vircols[10], 
             size=2) + 
  geom_line(aes(x=as.factor(period), 
                y=share_out_total), 
            group=1, 
            color=vircols[10], 
            linetype=2, 
            linewidth=.75) + 
  scale_color_manual(values = c(vircols[5], vircols[15]),
                     labels=c('Inbloc', 'Outbloc')) +
  labs(x=NULL, y='Share') + 
  theme_light() + 
  theme(axis.text.x = element_text(angle=90),
        legend.position = figure.legend.position,
        axis.title = element_text(size=16),
        axis.text = element_text(size=16)) +
  annotate("text",
           label="Outbloc", 
           color=vircols[15], 
           x=5, 
           y=0.335, 
           size=5) + 
  annotate("text", 
           label="Inbloc", 
           color=vircols[5], 
           x=5, 
           y=0.175, 
           size=5) + 
  annotate("text",
           label="Dyadic speeches", 
           color=vircols[10], 
           x=20, 
           y=0.4, 
           size=5)

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure G2a - Bloc Dyads ----

figure.ix <- 'G2a'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4

figG2a <- ggplot() + 
  geom_raster(data = speech_matrix_list[[1]], 
              aes(x= speakerparty.lab,
                  y= targetparty.lab, 
                  fill=n)) + 
  scale_fill_viridis_c(name="Share", begin=0.0, end=1.0, direction=1, limits=c(0, 0.55)) + 
  geom_text(data=speech_matrix_list[[2]], aes(x=speakerparty.lab, y=targetparty.lab, label =n), color='white') +
  theme_bw() + 
  theme(axis.text.x = element_markdown(angle = 90, vjust = 0.5, hjust=1, size=11),
        axis.text.y = element_markdown(angle = 90, vjust = 0.5, hjust=1, size=11),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        legend.key=element_blank(),
        legend.background = element_blank()) + 
  labs(x=NULL, y=NULL) 

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

### Figure G2b - Vote Dyads ----

figure.ix <- 'G2b'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4

figG2b <- ggplot() +
  geom_raster(data = votedist_df, 
              aes(x= speakerparty.lab,
                  y= targetparty.lab, 
                  fill=share)) + 
  scale_fill_viridis_c(name="Share", option='D', end=1.0, limits=c(0, 1.0), breaks=seq(0, 1, 0.20)) +
  geom_text(data=votedist_df, aes(x=speakerparty.lab, y=targetparty.lab, label=round(freq, 2)), 
            color='black', size=4, nudge_y = -0.0, nudge_x = -0.00) + 
  labs(x=NULL, y=NULL) +
  theme_bw() + 
  theme(axis.text.x = element_markdown(angle = 90, vjust = 0.5, hjust=1, size=11),
        axis.text.y = element_markdown(angle = 90, vjust = 0.5, hjust=1, size=11),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

## H - Textual Measures ----

# To produce Figure H2
topic_df <- adf %>%
  filter(!is.na(topictext))
topiclabels <- unique(topic_df$topictext)

binary_columns <- model.matrix(~ topictext - 1, data = topic_df)
topic_df <- cbind(topic_df, binary_columns)

lm_df <- topic_df %>%
  select(spitch, starts_with('topictext'))
lm_df <- lm_df[, c(1, 3:ncol(lm_df))]
cols <- names(lm_df[, c(2:ncol(lm_df))])

results <- vector("list", length(cols))
for (i in 1:length(cols)) {
  e <- tidy(lm(spitch ~ get(cols[i]), data=lm_df), conf.int = T)[2,]
  e$term <- substr(cols[i], 10, nchar(cols[i]))
  results[[i]] <- e
}

topicresult <- data.table::rbindlist(results)

topicresult$conf.low_90 <- topicresult$estimate - (1.645 * topicresult$std.error)
topicresult$conf.high_90 <- topicresult$estimate + (1.645 * topicresult$std.error)
topicresult$conf.low_95 <- topicresult$estimate - (1.96 * topicresult$std.error)
topicresult$conf.high_95 <- topicresult$estimate + (1.96 * topicresult$std.error)

# To produce Table H1
cor_df <- adf %>%
  filter(!is.na(targetparty) & abs(spitch) <= 5 & !is.na(textsent) & !is.na(emotionality))
sentmodel <- lm(spitch ~ scale(textsent), data=cor_df)
emotionmodel <- lm(spitch ~ scale(emotionality), data=cor_df)

# To produce Table H2
topicpitch <- adf %>%
  summarise(meanpitch = mean(spitch, na.rm=T),
            meansent = mean(textsent*-1, na.rm=T),  
            meanemo = mean(emotionality, na.rm=T),.by=topictext) %>%
  filter(!is.na(topictext))

topicpitch <- topicpitch %>%
  mutate(smeanpitch = scale(meanpitch)[,1],
         smeansent = scale(meansent)[,1],
         smeanemo = scale(meanemo)[,1])

topicpitch[paste0(c('pitch', 'sent', 'emotion'), "rank")] <- apply(-topicpitch[,5:7], 2, rank)

topicpitch <- topicpitch[order(topicpitch$pitchrank), c(1,5:10)]

### Figure H1 - AFINN Dictionary ----

figure.ix <- 'H1'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 8
figure.height <- 8
figure.legend.position <- c(0.75, 0.75)
figure.ylim <- c(0, 1750)

figH1 <- ggplot(read_rds('data/afinn.rds'), aes(x=score, fill=wordtype)) +
  geom_bar() + 
  scale_x_continuous(limits=c(-6, 6), breaks=c(-5:5)) + 
  scale_fill_manual(values = c(vircols[1], vircols[5], vircols[15])) +
  ylim(figure.ylim) + 
  labs(x='Valence score', y='') + 
  theme_light() + 
  theme(legend.position = figure.legend.position, 
                        legend.key=element_blank(),
                        legend.background = element_blank(),
                        legend.title = element_blank(),
                        legend.text=element_text(size=22),
                        axis.text = element_text(size=20),
                        axis.title.x = element_text(size=22))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

### Figure H2 - Topics ----

figure.ix <- 'H2'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 4
figure.legend.position <- c(0.15, 0.8)

figH2 <- ggplot(topicresult, aes(x=reorder(term, estimate), y=estimate, color=estimate)) + 
  geom_hline(yintercept = 0, 
             colour = grey(1/2), lty = 2, linewidth=.5) +
  geom_point(aes(x = reorder(term, estimate),
                 y = estimate)) + 
  geom_linerange(aes(x = reorder(term, estimate),
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 lwd = 1) +
  geom_linerange(aes(x = reorder(term, estimate),
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 lwd = 1/2) +
  scale_color_viridis_c() + 
  coord_flip() + 
  theme_light() + 
  theme(axis.text.y = element_text(size=8),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = figure.legend.position,
        legend.text=element_text(size=8),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=9))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Table H1 - Correlation between text and pitch ----

table.ix <- 'H1'
table.filename <- file.path('tables', paste0(table.ix, '.tex'))
table.caption <- "Correlation between textual measures and pitch."

tabH1 <- msummary(models=list("Sentiment"=sentmodel, "Emotionality"=emotionmodel), 
         stars=T,
         coef_map = c(`scale(textsent)`="Estimate",
                      `scale(emotionality)`='Estimate'),
         title=table.caption,
         gof_map = gm2,
         label="pitchcorr",
         output = "latex") %>%
  kable_styling(latex_options = c("hold_position")) %>%
  cat(., file = table.filename)

### Figure H3a - Sentiment ----

figure.ix <- 'H3a'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 8
figure.height <- 8
figure.legend.position <- "none"
figure.ylim <- c(-5, 5)

figH3a <- ggplot(subset(adf, !is.na(targetparty) & abs(spitch) <= 5), 
       aes(x=spitch, y=scale(textsent))) +
  geom_hex(bins=100) + 
  geom_smooth(color='yellow2', method='lm') + 
  scale_fill_gradient(low="gray90",high="black") +
  ylim(figure.ylim) +
  labs(x="z-pitch", y="z-sentiment") + 
  theme_light() +
  theme(legend.position = figure.legend.position,
        axis.text = element_text(size=20),
        axis.title = element_text(size=22))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

### Figure H3b - Emotionality ----

figure.ix <- 'H3b'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 8
figure.height <- 8
figure.legend.position <- "none"
figure.ylim <- c(-5, 5)

figH3b <- ggplot(subset(adf, !is.na(targetparty) & abs(spitch) <= 5),
       aes(x=spitch, y=scale(emotionality))) +
  geom_hex(bins=100) + 
  geom_smooth(color='yellow2', method='lm') + 
  scale_fill_gradient(low="gray90",high="black") +
  ylim(figure.ylim) +
  labs(x="z-pitch", y="z-emotionality") +
  theme_light() +
  theme(legend.position = figure.legend.position,
        axis.text = element_text(size=20),
        axis.title = element_text(size=22)) 

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

### Table H2 - Rhetoric Across Topics ----

table.ix <- 'H2'
table.filename <- file.path('tables', paste0(table.ix, '.tex'))
table.caption <- 'Mean and rank of pitch, sentiment, and emotionality by topic. Values are z-standardized to have zero mean and unit variance. Table is sorted by pitch rank.'

tabH2 <- kable(topicpitch, 
      format='latex', 
      digits=3,
      align=rep('l', 6),
      col.names = c('', 
                    'Pitch', 'Sentiment', 'Emotionality', 
                    'Pitch', 'Sentiment', 'Emotionality'),
      label='tab:rank',
      caption = table.caption,
      booktabs = T,
      linesep='',
      longtable = T) %>%
  kableExtra::add_header_above(c(" " = 1,
                                 "Mean" = 3,
                                 "Rank" = 3),
                               escape = FALSE) %>%
  kable_styling(latex_options=c("scale_down",'striped'), font_size = 10) %>%
  cat(., file = table.filename)


## I - Regression Tables ----

### Table I1 - Hypothesis 1 (Polarization)  ----

table.ix <- 'I1'

for (m in 1:n_models) { model_list[[model_names[m]]] = results_H1[[m]] }

coef_map <- c(`(Intercept)`="Intercept",
              textsent="Text sentiment",
              emotionality='Emotionality',
              outbloc="Outbloc target")

tabI1 <- regtable(models=model_list, 
                  coef_map=coef_map, 
                  table_ix=table.ix, 
                  add_rows=rows)


### Table I2 - Hypothesis 2 (Policy)  ----

table.ix <- 'I2'

for (m in 1:n_models) { model_list[[model_names[m]]] = results_H2[[m]] }

coef_map <- c(`(Intercept)`="Intercept",
              textsent="Text sentiment",
              emotionality='Emotionality', 
              vote_binary_all="Out-vote")

tabI2 <- regtable(models=model_list, 
                  coef_map=coef_map, 
                  table_ix=table.ix, 
                  add_rows=rows)

### Table I3 - Hypothesis 3 (Debate)  ----

table.ix <- 'I3'

for (m in 1:n_models) { model_list[[model_names[m]]] = results_H3[[m]] }

coef_map <- c(`(Intercept)`="Intercept",
              textsent="Text sentiment",
              emotionality='Emotionality', 
              high_profile_debate="High-publicity debate")

tabI3 <- regtable(models=model_list, 
                  coef_map=coef_map, 
                  table_ix=table.ix, 
                  add_rows=rows)

### Table I4 - Hypothesis 4 (Bargaining)  ----

table.ix <- 'I4'

for (m in 1:n_models) { model_list[[model_names[m]]] = results_H4[[m]] }

coef_map <- c(`(Intercept)`="Intercept",
              textsent="Text sentiment",
              emotionality='Emotionality', 
              meancip="Target bargaining")

tabI4 <- regtable(models=model_list, 
                  coef_map=coef_map, 
                  table_ix=table.ix, 
                  add_rows=rows)


## J - Alternative Model Specifications ----

# To produce Figure J1
adf_subset <- adf %>%
  filter(n() >= 9000, .by=period)
periods <- unique(adf_subset$period)

r <- list()
for (p in periods) {
  res <- tidy(feols(as.formula(paste("spitch ~" , "outbloc + textsent + emotionality | topictext")), data=subset(adf_subset, period==p), cluster='dyad'))[1,]
  res$period <- p
  r <- append(r, list(res))
}
rdf <- data.table::rbindlist(r)

rdf$conf.low_90 <- rdf$estimate - (1.645 * rdf$std.error)
rdf$conf.high_90 <- rdf$estimate + (1.645 * rdf$std.error)
rdf$conf.low_95 <- rdf$estimate - (1.96 * rdf$std.error)
rdf$conf.high_95 <- rdf$estimate + (1.96 * rdf$std.error)

# To produce Table J1
fmls_H2_fe <- model.formula(v='vote_binary_all')
fmls_H2_fe <- lapply(fmls_H2_fe, model.addfe, 'dyad')

results_H2_fe <- model.estimate(fmls=fmls_H2_fe, data=adf, cluster='dyad')

rows <- tribble(~term, ~"(1)", ~"(2)", ~"(3)",~"(4)", ~"(5)",
                'Topic FE', '\\xmark',   '\\xmark', '\\xmark', '\\cmark', '\\cmark',
                'Dyad FE', '\\cmark', '\\cmark', '\\cmark', '\\cmark', '\\cmark')

attr(rows, 'position') <- c(7, 8)

### Figure J1 - Dynamics of Polarization ----

figure.ix <- 'J1'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 8
figure.height <- 6
figure.legend.position <- c(0.075, 0.85)
figure.ylim <- c(-.5, .5)

figJ1 <- ggplot(rdf, aes(x=as.factor(period), y=estimate, color=estimate)) + 
  geom_hline(yintercept = 0, 
             colour = grey(1/2), lty = 2, linewidth=.5) +
  geom_point(aes(x = as.factor(period),
                 y = estimate)) + 
  geom_linerange(aes(x = as.factor(period),
                     ymin = conf.low_90,
                     ymax = conf.high_90),
                 linewidth = 1) +
  geom_linerange(aes(x =as.factor(period),
                     ymin = conf.low_95,
                     ymax = conf.high_95),
                 linewidth = 1/2) +
  scale_color_viridis_c() +
  ylim(figure.ylim) + 
  labs(x='Parliamentary Term', y='Estimate') + 
  theme_light() + 
  theme(axis.text.x = element_text(angle=90),
        axis.title = element_text(size=16),
        axis.text = element_text(size=16),
        legend.key=element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = figure.legend.position,
        legend.text = element_text(size=12))

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure J2 - Weekday Effects ----

wdays <- unique(adf$wday)[c(5,3,1,2,4)]

weekday_param_list <- list(
  list(form = fmls_H1, data = adf, cluster = 'dyad', iv = 'outbloc', ylim_coord = h1.figure.ylim, ylim_add2 = .4),
  list(form = fmls_H2, data = adf, cluster = 'dyad', iv = 'vote_binary_all', ylim_coord = h2.figure.ylim, ylim_add2 = .4, legend_pos = 'none', text_col = 'black')
)

weekday_filenames <- c("J2a", "J2b")
weekday_plots <- generate_and_save_plots(plot_weekdays, weekday_param_list, "weekday", weekday_filenames)


### Table J1 - Dyad Fixed Effects ----

table.ix <- "J1"
table.caption <- "Regression table for H2 with dyad FE"
table.fmt <- 4

for (m in 1:n_models) {model_list[[model_names[m]]] = results_H2_fe[[m]]}

coef_map <- c(textsent="Text sentiment",
              emotionality='Emotionality', 
              vote_binary_all="Out-vote")

tabJ1 <- regtable(models=model_list, 
                  coef_map=coef_map, 
                  table_ix=table.ix, 
                  add_rows=rows,
                  title=table.caption,
                  fmt=table.fmt)

### Figure J3 - Dyad Fixed Effects----

figure.ix <- 'J3'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.legend.position <- c(0.85, 0.7)
figure.ylim <- c(-.05,.4)

predictor.var <- 'vote_binary_all'

figJ3 <- plot_main(model=map(.x=results_H2_fe, .f=model.tidy, v=predictor.var) |> model.prep(), 
          ylim=figure.ylim, 
          legend.position = figure.legend.position)

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Table J2 - Without Dyad Fixed Effects ----

table.ix <- 'J2'
table.fmt <- 4
table.caption <- "Regression table for log-transformed vote difference."

predictor.var <- 'log(vote_contested)'
cluster.var <- 'dyad'

vote_fmls <- model.formula(v=predictor.var)
results_contest <- model.estimate(fmls=vote_fmls, 
                                  data=adf, 
                                  cluster=cluster.var)

for (m in 1:n_models) {model_list[[model_names[m]]] = results_contest[[m]]}

coef_map <- c(`(Intercept)`="Intercept",
              textsent="Text sentiment",
              emotionality='Emotionality', 
              `log(vote_contested)`="Vote difference (log)")

tabJ2 <- regtable(models=model_list, 
                  coef_map=coef_map, 
                  table_ix=table.ix, 
                  add_rows=rows,
                  title=table.caption,
                  fmt=table.fmt)

### Table J3 - With Dyad Fixed Effects ----

table.ix <- 'J3'
table.fmt <- 4
table.caption <- "Regression table for log-transformed vote difference without dyad fixed effects."

vote_fmls_fe <- lapply(vote_fmls, model.addfe, 'dyad')
results_contest_fe <- model.estimate(fmls=vote_fmls_fe, 
                                     data=adf, 
                                     cluster=cluster.var)

for (m in 1:n_models) {model_list[[model_names[m]]] = results_contest_fe[[m]]}

tabJ3 <- regtable(models=model_list, 
                  coef_map=coef_map, 
                  table_ix=table.ix, 
                  add_rows=rows,
                  title=table.caption,
                  fmt=table.fmt)

### Figure J4a - Without Dyad Fixed Effects ----

figure.ix <- 'J4a'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(-.2, .05)
figure.legend.position <- c(0.85, 0.68)

figJ4a <- plot_main(model=map(.x=results_contest, .f=model.tidy, v=predictor.var) |> model.prep(), 
                    ylim=figure.ylim, 
                    legend.position=figure.legend.position)

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)


### Figure J4b - With Dyad Fixed Effecs ----

figure.ix <- 'J4b'
figure.filename <- file.path('figures/appendix', paste0(figure.ix, '.pdf'))
figure.width <- 6
figure.height <- 6
figure.ylim <- c(-.2, .05)
figure.legend.position <- "none"

figJ4b <- plot_main(model=map(.x=results_contest_fe, .f=model.tidy, v=predictor.var) |> model.prep(), 
          ylim=figure.ylim, 
          legend.position=figure.legend.position)

ggsave(filename=figure.filename,
       width=figure.width,
       height=figure.height)

