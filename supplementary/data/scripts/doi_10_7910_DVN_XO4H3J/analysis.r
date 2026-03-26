#%% loading packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, stringr, ggplot2, fixest, RColorBrewer, sf,
    texreg, ggrepel, showtext, kableExtra, patchwork, forcats, ggtext, Cairo)




dir.create(file.path("Text/Tables"))
dir.create(file.path("Text/Figures"))

standardize <- function(vec) {
    vec[!is.finite(vec)] <- NA
    vec_mean <- mean(vec, na.rm = T)
    vec_sd <- sd(vec, na.rm = T)
    return( (vec - vec_mean) / vec_sd)
}
winsorize <- function(vec, alpha = 0.05) {
    quantiles <- quantile(vec, c(alpha / 2, 1 - alpha / 2), na.rm = T)
    vec[vec < quantiles[1]] <- quantiles[1]
    vec[vec > quantiles[2]] <- quantiles[2]
    return(vec)
}
get_mean <- function(model) {
    temp_mean <- mean(model$fitted.values + model$residuals, na.rm = T)
    temp_mean <- round(temp_mean, 3)
    return(temp_mean)
}


prep_models <- function(list_of_fixest_models) {
    extracted <- lapply(list_of_fixest_models,
        function(x){texreg::extract(x, include.nobs = TRUE,
            include.groups = FALSE,
            include.rsquared = TRUE,
            include.adjrs = FALSE,
            include.proj.stats = FALSE,
            include.deviance = FALSE,
            include.loglik = FALSE,
            include.pseudors = FALSE)})

    means <- lapply(list_of_fixest_models, get_mean)
    return(list(extracted, means))
}
get_model_means <- function(list_of_fixest_models) {
    means <- lapply(list_of_fixest_models, get_mean)
}



binscatter_unconditional <- function(data, x_var, y_var, weight_var = NULL,
    nbins = 20, xlims = NULL, ylims = NULL) {


    if (length(weight_var) == 0) {
        data$temp_weight <- rep(1, nrow(data))
    } else {
        data$temp_weight <- data[, ..weight_var]
    }

    keep_formula <- as.formula(paste0(y_var, " ~ ", x_var, " + temp_weight"))
    df_bin <- model.frame(keep_formula, data)
    df_bin <- data.table(df_bin)
    colnames(df_bin) <- c("y", "x", "weight")
    df_bin[, y := as.numeric(y)]
    df_bin <- df_bin[order(x)]
    df_bin[, `:=`(cum_weight, cumsum(weight))]
    df_bin[, `:=`(bin, cut(cum_weight, nbins))]
    df_bin <- df_bin[, .(x = weighted.mean(x, weight), y = weighted.mean(y,
        weight)), by = .(bin)]
    ggplot(df_bin, aes(x = x, y = as.numeric(y))) +
        geom_point(size = 0.5) +
        coord_cartesian(ylim = ylims, xlim = xlims)
}

binscatter_controls <- function(data, x_var, y_var, control_formula = NULL,
    weight_var = NULL, nbins = 20, xlims = NULL, ylims = NULL) {
    if (length(weight_var) == 0) {
        data$temp_weight <- rep(1, nrow(data))
    } else {
        data$temp_weight <- data[, ..weight_var]
    }
    
    drop_formula <- as.formula(paste0(y_var, " ~ ", x_var, " + temp_weight"))
    drop_mod <- feols(drop_formula, data)
    if (length(drop_mod$obs_selection$obsRemoved) > 1) {
        data <- data[drop_mod$obs_selection$obsRemoved]
    }
    rm(drop_mod)

    if (length(control_formula) == 0) {
        df_out <- model.frame(drop_formula, data)
        df_out <- data.table(df_out)
        colnames(df_out) <- c("y", "x", "weight")
        rm(data)
    } else {
        y_formula <- as.formula(paste0(y_var, control_formula))
        x_formula <- as.formula(paste0(x_var, control_formula))

        y_mod <- feols(y_formula, data, weights = ~ temp_weight)
        x_mod <- feols(x_formula, data, weights = ~ temp_weight)

        df_out <- data.table(y = y_mod$residuals, x = x_mod$residuals, weight = x_mod$weights)
        rm(y_mod, x_mod, data)
    }

    
    df_out <- df_out[order(x)]
    df_out[, cum_weight := cumsum(weight)]
    df_out[, bin := cut(cum_weight, nbins)]

    # ols slope
    if (length(control_formula) == 0) {
        r_slope <- lm(y ~ x, df_out, weights = df_out$weight)
    } else {
        r_slope <- lm(y ~ x - 1, df_out, weights = df_out$weight)
    }
    

    df_bin <- df_out[, .(x = weighted.mean(x, weight),
        y = weighted.mean(y, weight)), by = .(bin)]
    df_bin[, y := as.numeric(y)]
    df_bin[, x := as.numeric(x)]
    rm(df_out)
    if (length(control_formula) == 0) {
        intercept_val <- r_slope$coefficients[1]
        slope_val <- r_slope$coefficients[2]
    } else {
        intercept_val <- 0
        slope_val <- r_slope$coefficients[1]
    }
    ggplot(df_bin, aes(x = x, y = y)) +
        geom_abline(intercept = intercept_val, slope = slope_val,
            color = brewer.pal(n = 9, name = "Set1")[2], linewidth = 0.5) +
        geom_point(size = 0.5)  +
        coord_cartesian(ylim = ylims, xlim = xlims)
    
}



options(ggplot2.discrete.color = brewer.pal(n = 9, name = "Set1"))
options(ggplot2.discrete.fill = brewer.pal(n = 9, name = "Set1"))


# Uses the Univers font for figures. If you have the Univers font installed
# change the filenames in line 151 to the relevant filenames on your system.
# If not, line 151 can be commented out and the code will run as expected
# but the font will be slightly different
font_add("Univers", regular = "25504584715.ttf", bold = "21178055056.ttf")

showtext_auto()
theme_set(theme_minimal(base_family = "Univers", base_size = 10))


theme_update(legend.position = "bottom", panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(colour="black"),
    legend.text = element_text(color = "black"))



my_blue <- brewer.pal(n = 3, "Set1")[2]

# southern states
confederacy_states <- c(510, 10, 50, 120, 130, 220, 280, 370, 450, 480, 210,
    400, 470)


# load cross-sectional data
df_cs <- fread("Data/_Clean/us_county_cross_section.csv")




#%% load data for main regressions with voting
df_vote <- fread("Data/_Clean/us_voting_pres.csv")
head(df_vote)
df_vote <- merge(df_vote, df_cs, by = c("state_1930", "county_1930"),
    all.x = TRUE)


# create year-specific vote variables
df_vote[, dem2p_1932 := dem2p[election_year == 1932], by = .(state_1930, county_1930)]
df_vote[, dem2p_1920 := dem2p[election_year == 1920], by = .(state_1930, county_1930)]
df_vote[, dem2p_1928 := dem2p[election_year == 1928], by = .(state_1930, county_1930)]
df_vote[, d_dem2p_1920 := dem2p_1932 - dem2p_1920]
df_vote[, d_dem2p := dem2p - dem2p_1932]
df_vote[, d_dem2p_2832 := dem2p_1932 - dem2p_1928]


#%%%% Tables using county-level data

#%% Table 1
rvc1 <- feols(d_dem2p ~ urban_1930 | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc1)
rvc2 <- feols(d_dem2p ~ urban_1930 + agric_1930 + mf_1930 + white_1930 + foreign_born_1930 + union_potential_1939| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc2)

rvc3 <- feols(d_dem2p ~ winsorize(agglomeration_1930) | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc3)
rvc4 <- feols(d_dem2p ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + white_1930 +
    foreign_born_1930 + union_potential_1939 | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc4)


models <- list(rvc1, rvc2, rvc3, rvc4)
prepped <- prep_models(models)
prepped



texreg(prepped[[1]],
    file = "Text/Tables/table_1.tex",
    label = "table_urban_vote",
    caption = "Effects of urbanization on realignment toward the Democrats after 1932",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
    custom.coef.map = list("urban_1930" = "Urban (%)",
        "winsorize(agglomeration_1930)" = "Agglomeration"),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:4),
    custom.gof.rows = list(
        "Controls" = c("", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table shows the results of regressions of
        the county-level change in the Democrats' share of the two-party
        vote between 1932 and 1936 against the urban share of the population in 1930,
        and agglomeration, calculated as the population-weighted average of
        place size raised to the power $\\alpha = 0.078$.  All models include state fixed
        effects. Even-numbered models also control for the shares
        employed in agriculture and manufacturing, the white and immigrant
        shares of the population, and union potential.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )


#%% Table 2

rvc1 <- feols(d_dem2p ~ winsorize(overhead_efficiency) | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc1)
rvc2 <- feols(d_dem2p ~ winsorize(overhead_efficiency) + agric_1930 + mf_1930 + white_1930 +
    foreign_born_1930 + union_potential_1939| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc2)


rvc3 <- feols(d_dem2p ~ urban_1930 + winsorize(overhead_efficiency) | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc3)
rvc4 <- feols(d_dem2p ~ urban_1930 + winsorize(overhead_efficiency) + agric_1930 + mf_1930 +
    white_1930 + foreign_born_1930 + union_potential_1939 | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rvc4)


models <- list(rvc1, rvc2, rvc3, rvc4)
prepped <- prep_models(models)


texreg(prepped[[1]],
    file = "Text/Tables/table_2.tex",
    label = "table_c_inverse_vote",
    caption = "Effects of public sector efficiency on realignment toward the Democrats after 1932",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
    custom.coef.map = list("winsorize(overhead_efficiency)" = "Overhead efficiency",
        "urban_1930" = "Urban (%)"
        ),
    custom.gof.rows = list(
        "Controls" = c("", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:4),
    custom.note = paste0("\\item This table shows the results of regressions of
        the county-level change in the Democrats' share of the two-party
        vote between 1932 and 1936 against
        the overhead efficiency, calculated as total spending on government provision exclusive
        of central administrative costs divided by total spending inclusive of
        administrative costs. All models include state fixed
        effects. Even-numbered models also control for the shares
        employed in agriculture and manufacturing, the white and immigrant
        shares of the population, and union potential.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )

#%% Table A-2

rval1 <- feols(ln_infant_mortality ~ winsorize(overhead_efficiency) | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9
    )
summary(rval1)

rval2 <- feols(ln_infant_mortality ~ winsorize(overhead_efficiency) 
    + agric_1930 + mf_1930
    + foreign_born_1930 + white_1930 + union_potential_1939 | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9
    )
summary(rval2)

rval3 <- feols(ln_infant_mortality ~ winsorize(overhead_efficiency) 
    + agric_1930 + mf_1930
    + foreign_born_1930 + white_1930 + union_potential_1939 + log(spend_percap) | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9
    )
summary(rval3)

rval4 <- feols(ln_children_in_school ~ winsorize(overhead_efficiency) | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9
    )
summary(rval4)

rval5 <- feols(ln_children_in_school ~ winsorize(overhead_efficiency) 
    + agric_1930 + mf_1930
    + foreign_born_1930 + white_1930 + union_potential_1939 | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9
    )
summary(rval5)

rval6 <- feols(ln_children_in_school ~ winsorize(overhead_efficiency) 
    + agric_1930 + mf_1930
    + foreign_born_1930 + white_1930 + union_potential_1939 + log(spend_percap) | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9
    )
summary(rval6)

models <- list(rval1, rval2, rval3, rval4, rval5, rval6)
prepped <- prep_models(models)

texreg(prepped[[1]],
    file = "Text/Tables/table_a2.tex",
    label = "table_oe_validation",
    caption = "Relationship between overhead efficiency, infant mortality and school attendance, for non-Southern counties in 1932",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
    custom.coef.map = list("winsorize(overhead_efficiency)" = "Overhead efficiency",
        "log(spend_percap)" = "Log spending / pop"),
    custom.gof.rows = list("Controls" = c("", "x", "x", "", "x", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("Log infant mortality" = 1:3,
        "Log school attendance" = 4:6
        ),
    custom.note = paste0("\\item This table shows the results of county-level regressions of
        log infant mortality and the log share of children attending school against
        overhead efficiency.
        All models include state fixed effects, models (2), (3), (5), and (6)
        control for the shares in agriculture and manufacturing, the immigrant
        and white population shares, and union potential, (3) and (6) also 
        control for log spending per capita.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )



#%% Table A-3


df_michigan <- fread("Data/_Clean/Michigan_emergency_relief_data.csv")
df_michigan[, county_1930 := as.numeric(county_1930)]
df_michigan <- merge(df_michigan, df_cs, by = c('state_1930', "county_1930"))


rm01 <- feols(admin_cost_ratio ~ winsorize(agglomeration_1930), df_michigan,
    vcov = "hetero")
summary(rm01)

rm02 <- feols(admin_cost_ratio ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939, df_michigan,
    vcov = "hetero")
summary(rm02)

rm1 <- feols(admin_cost_ratio ~ winsorize(overhead_efficiency), df_michigan,
    vcov = "hetero")
summary(rm1)

rm2 <- feols(admin_cost_ratio ~ winsorize(overhead_efficiency) + agric_1930 + mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939, df_michigan,
    vcov = "hetero")
summary(rm2)

rm3 <- feols(admin_cost_ratio ~ winsorize(overhead_efficiency) + urban_1930, df_michigan,
    vcov = "hetero")
summary(rm3)

rm4 <- feols(admin_cost_ratio ~ winsorize(overhead_efficiency) + urban_1930 + agric_1930 + mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939, df_michigan,
    vcov = "hetero")
summary(rm4)

models <- list(rm01, rm02, rm1, rm2, rm3, rm4)
prepped <- prep_models(models)




texreg(prepped[[1]],
    file = "Text/Tables/table_a3.tex",
    label = "table_michigan_costs",
    caption = "Relationship between agglomeration, overhead efficiency, and the cost of emergency relief administration in Michigan",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
    custom.coef.map = list("winsorize(agglomeration_1930)" = "Agglomeration",
        "winsorize(overhead_efficiency)" = "Overhead efficiency",
        "urban_1930" = "Urban (%)"
        ),
    custom.gof.rows = list(
        "Controls" = c("", "x", "", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("Administrative costs / total relief spending " = 1:6),
    custom.note = paste0("\\item This table shows the results of regressions of
        the county-level ratio of emergency relief administrative costs to total
        emergency relief spending, in Michigan, December 1934--February 1935, against
        agglomeration, overhead efficiency and urbanization.
        All models include an intercept. Even-numbered models also control for the shares
        employed in agriculture and manufacturing, the white and immigrant
        shares of the population, and union potential.
        Robust standard errors in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )


#%% Table A-7
rsvc1 <- feols(d_dem2p ~ urban_1930 | state_1930,
    df_vote[election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rsvc1)
rsvc2 <- feols(d_dem2p ~ urban_1930 + agric_1930 + mf_1930 + white_1930 + foreign_born_1930 + union_potential_1939| state_1930,
    df_vote[election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rsvc2)

rsvc3 <- feols(d_dem2p ~ winsorize(agglomeration_1930) | state_1930,
    df_vote[election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rsvc3)
rsvc4 <- feols(d_dem2p ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + white_1930 +
    foreign_born_1930 + union_potential_1939 | state_1930,
    df_vote[election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rsvc4)

models <- list(rsvc1, rsvc2, rsvc3, rsvc4)
prepped <- prep_models(models)
texreg(prepped[[1]],
    file = "Text/Tables/table_a7.tex",
    label = "table_urban_vote_south",
    caption = "Effects of urbanization on realignment towards the Democrats after 1932, including the South",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
    custom.coef.map = list("urban_1930" = "Urban (%)",
        "winsorize(agglomeration_1930)" = "Agglomeration"),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:4),
    custom.gof.rows = list(
        "Controls" = c("", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table shows the results of regressions of
        the county-level change in the Democrats' share of the two-party
        vote between 1932 and 1936 against the urban share of the population in 1930,
        and agglomeration, calculated as the population-weighted average of
        place size raised to the power $\\alpha = 0.078$. Unlike Table \\ref{table_urban_vote},
        these models include counties in the former Confederacy. All models include state fixed
        effects. Even-numbered models also control for the shares
        employed in agriculture and manufacturing, the white and immigrant
        shares of the population, and union potential.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )


#%% Table A-8

df_GA <- fread("Data/_Clean/voting_GA.csv")
df_GA <- merge(df_GA, df_cs, by = c('state_1930', "county_1930"), all.x = TRUE)

df_NC <- fread("Data/_Clean/voting_NC.csv")
df_NC <- merge(df_NC, df_cs, by = c('state_1930', "county_1930"), all.x = TRUE)

df_TN <- fread("Data/_Clean/voting_TN.csv")
df_TN <- merge(df_TN, df_cs, by = c('state_1930', "county_1930"), all.x = TRUE)

df_VA <- fread("Data/_Clean/voting_VA.csv")
df_VA <- merge(df_VA, df_cs, by = c('state_1930', "county_1930"), all.x = TRUE)


r_ga1 <- feols(talmadge_vote_share ~ urban_1930 | event, df_GA,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_ga1)
r_ga2 <- feols(talmadge_vote_share ~ urban_1930 + white_1930 + mf_1930 + agric_1930 + foreign_born_1930 + union_potential_1939 | event, df_GA,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_ga2)
r_nc1 <- feols(shelby_vote_share ~ urban_1930 | event, df_NC,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_nc1)
r_nc2 <- feols(shelby_vote_share ~ urban_1930 + white_1930 + mf_1930 + agric_1930 + foreign_born_1930 + union_potential_1939 | event, df_NC,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_nc2)
r_va1 <- feols(byrd_vote_share ~ urban_1930 | event, df_VA,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_va1)
r_va2 <- feols(byrd_vote_share ~ urban_1930 + white_1930 + mf_1930 + agric_1930 + foreign_born_1930 + union_potential_1939 | event, df_VA,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_va2)
r_tn1 <- feols(crump_vote_share ~ urban_1930 | event, df_TN,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_tn1)
r_tn2 <- feols(crump_vote_share ~ urban_1930 + white_1930 + mf_1930 + agric_1930 + foreign_born_1930 + union_potential_1939 | event, df_TN,
    cluster = ~ state_1930 ^ county_1930, fixef.tol = 1e-9)
summary(r_tn2)


models <- list(r_ga1, r_ga2, r_nc1, r_nc2, r_va1, r_va2, r_tn1, r_tn2)
prepped <- prep_models(models)

texreg(prepped[[1]],
    file = "Text/Tables/table_a8.tex",
    label = "table_southern_primary",
    caption = "Urban-Rural Divides in Voting For the Dominant Faction in Southern Democratic Party Primaries",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)"),
    custom.coef.map = list("urban_1930" = "Urban (%)"),
    custom.gof.rows = list("Machine New Deal stance" = c("Anti", "Anti", "Anti", "Anti", "Anti", "Anti", "Pro", "Pro"),
        "Controls" = c("", "x", "", "x", "", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("Georgia" = 1:2,
        "North Carolina" = 3:4,
        "Virginia" = 5:6,
        "Tennessee" = 7:8
        ),
    custom.note = paste0("\\item This table shows the results of county-level
        regressions of the share of the vote won by the dominant Democratic Party
        faction in Democratic Party gubernatorial primaries in four southern
        states, over the period 1932--1945. The main independent variable is
        the urban share of the population. In Models (1)--(2) 
        the dependent variable is the share of the vote won by the Talmadge faction
        candidate in Georgia, in (3)--(4) voteshare for the Shelby machine candidate in North Carolina,
        in (5)--(6) voteshare for the Byrd organization candidate in Virginia,
        (7)--(8) voteshare for the Crump machine candidate in Tennessee. In
        Georgia, North Carolina, and Virginia, the dominant machine opposed the
        New Deal, in Tennessee it supported it.
        All models include election fixed
        effects. Even-numbered models control for the shares
        employed in agriculture and manufacturing, the white and immigrant
        shares of the population, and union potential.
        Standard errors clustered by county in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    no.margin = TRUE,
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )


#%% Table A-9

rar1 <- feols(d_dem2p ~ urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939
    + sales_pc_1929 + d_sales + deposits_pc_1932 + d_deposits_pc| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar1)
rar2 <- feols(d_dem2p ~ urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 +
    strikes_percap + locals_percap | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar2)

rar3 <- feols(d_dem2p ~ urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930 +
    union_potential_1939 + farms_pc_1930 + ag_output_pc_1930 +  ue_1930 +
    illiteracy_rate_1930| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar3)

rar4 <- feols(d_dem2p ~ urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939
    + d_dem2p_2832| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar4)


rar5 <- feols(d_dem2p ~ urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + sales_pc_1929 + d_sales + deposits_pc_1932 + d_deposits_pc
    + strikes_percap + locals_percap + union_potential_1939 +
    farms_pc_1930 + ag_output_pc_1930 +  ue_1930 +
    illiteracy_rate_1930 + d_dem2p_2832
    | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar5)
rar6 <- feols(d_dem2p ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + white_1930 +
    foreign_born_1930 + union_potential_1939
    + sales_pc_1929 + d_sales + deposits_pc_1932 + d_deposits_pc| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar6)
rar7 <- feols(d_dem2p ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + white_1930 +
    foreign_born_1930 + union_potential_1939
    + strikes_percap + locals_percap + union_potential_1939| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar7)

rar8 <- feols(d_dem2p ~ winsorize(agglomeration_1930)  + agric_1930 + mf_1930 +
    white_1930 + foreign_born_1930 + union_potential_1939
    + farms_pc_1930 + ag_output_pc_1930 +  ue_1930 +
    illiteracy_rate_1930| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar8)

rar9 <- feols(d_dem2p ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 +
    white_1930 + foreign_born_1930 + union_potential_1939
    + d_dem2p_2832| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar9)
rar10 <- feols(d_dem2p ~ winsorize(agglomeration_1930)  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + sales_pc_1929 + d_sales + deposits_pc_1932 + d_deposits_pc
    + strikes_percap + locals_percap + union_potential_1939 +
    farms_pc_1930 + ag_output_pc_1930 +  ue_1930 +
    illiteracy_rate_1930 + d_dem2p_2832| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar10)


models <- list(rar1, rar2, rar3, rar4, rar5, rar6, rar7, rar8, rar9, rar10)
prepped <- prep_models(models)

texreg(prepped[[1]],
    file = "Text/Tables/table_a9.tex",
    label = "table_agglom_extra_robust",
    caption = paste0("Effects of urbanization on voting, controlling for ",
        "income changes, unionization, development, and trends in voting"),
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)"),
    custom.coef.map = list("urban_1930" = "Urban (%)",
        "winsorize(agglomeration_1930)" = "Agglomeration"),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:10),
    custom.gof.rows = list(
        "Income controls" = c("x", "", "", "", "x", "x", "", "", "", "x"),
        "Union controls" = c("", "x", "", "", "x", "", "x", "", "", "x"),
        "Additional controls" = c("", "", "x", "", "x", "", "", "x", "", "x"),
        "$\\Delta$\\% Democrat, 1928--1932" = c("", "", "", "x", "x", "", "", "", "x", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table shows the results of regressions of
        the change in the Democrat share of the two-party vote against urbanization and
        agglomeration. All models include state fixed
        effects and controls for the shares in agriculture and
        manufacturing, the white and immigrant shares, and union potential. Income controls are
        retail sales per capita in 1929, the change in retail sales per capita
        between 1933 to 1935, bank deposits per capita in 1932, and the change in
        deposits per capita between 1932 and 1936. Union controls are the number of
        recorded IWW locals and strikes divided by population.
        Additional controls are farms and agricultural output per capita in 1930,
        and the unemployment and illiteracy rates in 1930.
        Models (4), (5), (9) and (10) control also for the change in the Democrats'
        change of the two-party vote between 1928 and 1932.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    sideways = T,
    no.margin = TRUE,
    use.packages = F
    )



#%% Table A-10

rbm1 <- feols(standardize(d_dem2p) ~ standardize(urban_1930) + standardize(agric_1930) +
    standardize(mf_1930) + standardize(white_1930) +
    standardize(foreign_born_1930) + standardize(union_potential_1939)
     | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rbm1)


rbm2 <- feols(standardize(d_dem2p) ~ winsorize(standardize(agglomeration_1930)) + standardize(agric_1930) +
    standardize(mf_1930) + standardize(white_1930) +
    standardize(foreign_born_1930) + standardize(union_potential_1939)
     | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rbm2)


rbm3 <- feols(standardize(d_dem2p) ~ winsorize(standardize(overhead_efficiency)) +
    + standardize(agric_1930) +
    standardize(mf_1930) + standardize(white_1930) +
    standardize(foreign_born_1930) + standardize(union_potential_1939)
     | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rbm3)


rbm4 <- feols(standardize(d_dem2p) ~ winsorize(standardize(overhead_efficiency)) + (standardize(urban_1930))
    + standardize(agric_1930) +
    standardize(mf_1930) + standardize(white_1930) +
    standardize(foreign_born_1930) + standardize(union_potential_1939)
     | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rbm4)


models <- list(rbm1, rbm2, rbm3, rbm4)
prepped <- prep_models(models)
prepped

coef_std_ag1 <- round(abs(rbm1$coefficients[1] / rbm1$coefficients[2]), 3)
coef_std_ag2 <- round(abs(rbm2$coefficients[1] / rbm2$coefficients[2]), 3)
coef_std_ag3 <- round(abs(rbm3$coefficients[1] / rbm3$coefficients[2]), 3)
coef_std_ag4 <- round(abs(rbm4$coefficients[1] / rbm4$coefficients[3]), 3)

coef_std_mf1 <- round(rbm1$coefficients[1] / rbm1$coefficients[3], 3)
coef_std_mf2 <- round(rbm2$coefficients[1] / rbm2$coefficients[3], 3)
coef_std_mf3 <- round(rbm3$coefficients[1] / rbm3$coefficients[3], 3)
coef_std_mf4 <- round(rbm4$coefficients[1] / rbm4$coefficients[4], 3)


texreg(prepped[[1]],
    file = "Text/Tables/table_a10.tex",
    label = "table_urban_vote_benchmark",
    caption = "Effects of urbanization and efficiency on realignment towards the Democrats after 1932, standardized estimates for benchmarking",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
    custom.coef.map = list("standardize(urban_1930)" = "Urban (%)",
        "winsorize(standardize(agglomeration_1930))" = "Agglomeration",
        "winsorize(standardize(overhead_efficiency))" = "Overhead efficiency",
        "standardize(agric_1930)"= "Agricultural (%)",
        "standardize(mf_1930)" = "Manufacturing (%)",
        "standardize(white_1930)" = "White (%)",
        "standardize(foreign_born_1930)" = "Foreign born (%)",
        "standardize(union_potential_1939)" = "Union potential"
        ),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:4),
    custom.gof.rows = list(
        "Urban/efficiency coefficient\\\\relative to agriculture" = c(coef_std_ag1, coef_std_ag2, coef_std_ag3, coef_std_ag4),
        "relative to manufacturing" = c(coef_std_mf1, coef_std_mf2, coef_std_mf3, coef_std_mf4)
        ),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table reproduces models (2) and (4) of
        Table \\ref{table_urban_vote}, and models (2) and (4) of Table \\ref{table_c_inverse_vote}, standardizing both the independent and
        dependent variables, so that each coefficient can be interpreted as the
        effect of a standard deviation increase in the variable in question in
        terms of standard deviations of the outcome---the change in voteshare for
        the Democrats 1932--1936---holding the other variables fixed. Rows above
        the number of observations report the value of the coefficients on
        \\% urban (1), agglomeration (2), and overhead efficiency scaled by the
        coefficient son \\% agricultural and \\% manufacturing.
        All models include state fixed
        effects. 
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )


#%% Table A-11

rar11 <- feols(d_dem2p ~ winsorize(overhead_efficiency) + urban_1930 + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939
    + sales_pc_1929 + d_sales + deposits_pc_1932 + d_deposits_pc| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar11)
rar12 <- feols(d_dem2p ~ winsorize(overhead_efficiency) + urban_1930 + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939
    + strikes_percap + locals_percap | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar12)

rar13 <- feols(d_dem2p ~ winsorize(overhead_efficiency) + urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930 +
    union_potential_1939 + farms_pc_1930 + ag_output_pc_1930 +  ue_1930 +
    illiteracy_rate_1930| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar13)

rar14 <- feols(d_dem2p ~ winsorize(overhead_efficiency) + urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939
    + d_dem2p_2832| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar14)


rar15 <- feols(d_dem2p ~ winsorize(overhead_efficiency) + urban_1930  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + sales_pc_1929 + d_sales + deposits_pc_1932 + d_deposits_pc
    + strikes_percap + locals_percap + union_potential_1939 +
    farms_pc_1930 + ag_output_pc_1930 +  ue_1930 +
    illiteracy_rate_1930 + d_dem2p_2832
    | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rar15)

models <- list(rar11, rar12, rar13, rar14, rar15)
prepped <- prep_models(models)

texreg(prepped[[1]],
    file = "Text/Tables/table_a11.tex",
    label = "table_c_inverse_extra_robust",
    caption = paste0("Robustness of effects of overhead efficiency on voting, controlling for ",
        "income changes, unionization, development, and trends in voting"),
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:5),
    custom.coef.map = list("winsorize(overhead_efficiency)" = "Overhead efficiency"),
    custom.gof.rows = list(
        "Income controls" = c("x", "", "", "", "x"),
        "Union controls" = c("", "x", "", "", "x"),
        "Additional controls" = c("", "", "x", "", "x"),
        "$\\Delta$\\% Democrat, 1928--1932" = c("", "", "", "x", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table shows the results of regressions of
        the change in the Democrat share of the two-party vote against the overhead efficiency.
        All models include state fixed
        effects and controls for the shares in agriculture and
        manufacturing, the white and immigrant shares, union potential, and the urban share. Income controls are
        retail sales per capita in 1929, the change in retail sales per capita
        between 1933 to 1935, bank deposits per capita in 1932, and the change in
        deposits per capita between 1932 and 1936. Union controls are the number of
        recorded IWW locals and strikes divided by population.
        Additional controls are farms and agricultural output per capita in 1930,
        and the unemployment and illiteracy rates in 1930.
        Models (4) and (5) control also for the change in the Democrats'
        change of the two-party vote between 1928 and 1932.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )

#%% Table A-12

rw1 <- feols(d_dem2p ~ (agglomeration_1930) | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)

rw2 <- feols(d_dem2p ~ (agglomeration_1930) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930 + union_potential_1939| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)

rw3 <- feols(d_dem2p ~ (overhead_efficiency) | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)

rw4 <- feols(d_dem2p ~ (overhead_efficiency) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930 + union_potential_1939 | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)

rw5 <- feols(d_dem2p ~ (overhead_efficiency) + urban_1930| state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)

rw6 <- feols(d_dem2p ~ (overhead_efficiency) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930 + union_potential_1939 + urban_1930 | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936],
    cluster = ~ state_1930, fixef.tol = 1e-9)


models <- list(rw1, rw2, rw3, rw4, rw5, rw6)
prepped <- prep_models(models)
texreg(prepped[[1]],
    file = "Text/Tables/table_a12.tex",
    label = "table_winsorize_robust",
    caption = paste0("Robustness of effects of agglomeration and overhead efficiency on voting, not winsorizing"),
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
    custom.coef.map = list("agglomeration_1930" = "Agglomeration",
        "overhead_efficiency" = "Overhead efficiency",
        "urban_1930" = "Urban (%)"),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:6),
    custom.gof.rows = list(
        "Controls" = c("", "x", "", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table shows the results of regressions of
        the change in the Democrat share of the two-party vote against agglomeration and the overhead efficiency,
        not winsorized.
        All models include state fixed
        effects, even models include controls for the shares in agriculture and
        manufacturing, the white and immigrant shares, and union potential. Models (5) and (6)
        also control for the 1930 urban share.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )

#%% Table A-13


rs1 <- feols(log(spend_nonhighway) ~ (winsorize(overhead_efficiency))
    | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930 , fixef.tol = 1e-9
    )
summary(rs1)

rs2 <- feols(log(spend_nonhighway) ~ (winsorize(overhead_efficiency))
    + agric_1930 + mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939
    | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930 , fixef.tol = 1e-9 
    )
summary(rs2)
rs3 <- feols(log(spend_nonhighway) ~ (winsorize(overhead_efficiency))
    + urban_1930 +
    + agric_1930 + mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939
    | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930 , fixef.tol = 1e-9
    )
summary(rs3)

rs4 <- feols(log(spend_nonhighway) ~ (winsorize(overhead_efficiency)) + log(pop_1930)
    | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930 , fixef.tol = 1e-9 
    )
summary(rs4)

rs5 <- feols(log(spend_nonhighway) ~ (winsorize(overhead_efficiency)) + log(pop_1930)
    + agric_1930 + mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939
    | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930 , fixef.tol = 1e-9
    )
summary(rs5)

rs6 <- feols(log(spend_nonhighway) ~ (winsorize(overhead_efficiency)) + log(pop_1930)
    + urban_1930 +
    + agric_1930 + mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939
    | state_1930,
    df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930 , fixef.tol = 1e-9
    )
summary(rs6)


models <- list(rs1, rs2, rs3, rs4, rs5, rs6)
prepped <- prep_models(models)

texreg(prepped[[1]],
    file = "Text/Tables/table_a13.tex",
    label = "table_spending_percap",
    caption = "Relationship between overhead efficiency and 1932 spending, for non-Southern counties",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
    custom.coef.map = list("winsorize(overhead_efficiency)" = "Overhead efficiency",
        "urban_1930" = "Urban (%)",
        "log(pop_1930)" = "Log population"),
    custom.gof.rows = list("Controls" = c("", "x", "x", "", "x", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("Log spending" = 1:6
        ),
    custom.note = paste0("\\item This table shows the results of county-level regressions of
        log spending against overhead efficiency.
        The dependent variable is log of government spending exclusive
        of highway maintenance in 1932.
        All models include state fixed effects, models (2), (3), (5), and (6)
        control for the shares in agriculture and manufacturing, the immigrant
        and white population shares, and union potential.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )


#%% Table A-14

# calculate police and fire productivity, residual of employment relative to spending
df_police_prod <- df_vote[!(state_1930 %in% confederacy_states) & election_year == 1936]
r_resid <- feols(log(police_fire_percap) ~ log(police_fire_spend_percap), df_police_prod)
df_police_prod[r_resid$obs_selection$obsRemoved, police_resid := r_resid$residuals]
df_police_prod[, exp_police_resid := exp(police_resid)]

rpe1 <- feols(d_dem2p ~ highway_spending_share
    | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rpe1)
rpe2 <- feols(d_dem2p ~ highway_spending_share +
    agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939
    | state_1930,
    df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936], 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rpe2)

rpe3 <- feols(d_dem2p ~
    exp_police_resid
    | state_1930,
    df_police_prod, 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rpe3)
rpe4 <- feols(d_dem2p ~ 
    exp_police_resid  + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 
    | state_1930,
    df_police_prod, 
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rpe4)
models <- list(rpe1, rpe2, rpe3, rpe4)
prepped <- prep_models(models)
texreg(prepped[[1]],
    file = "Text/Tables/table_a14.tex",
    label = "table_alt_prod_voting",
    caption = paste0("Effects of alternative measures of productivity on realignment towards Democrats"),
    custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
    custom.coef.map = list("highway_spending_share" = "Highway budget share",
        "exp_police_resid" = "Police \\& fire productivity"),
    custom.header = list("$\\Delta$ Democrat 1932--1936 (\\%)" = 1:4),
    custom.gof.rows = list(
        "Controls" = c("", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table shows the results of regressions of
        the change in the Democrat share of the two-party vote against alternative measures of productivity.
        The highway budget share is the share of government spending allocated to highway
        maintenance. Police and fire productivity is the exponential of the residual
        from a regression of log police and fire employment per capita against log
        spending per capita on police and fire protection.
        All models include state fixed
        effects, even models include controls for the shares in agriculture and
        manufacturing, the white and immigrant shares, and union potential.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )




#%% Table A-15

r_ab1 <- feols(change_log_deposits_35_36 ~ urban_1930 + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab1)

r_ab2 <- feols(change_log_deposits_35_36 ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab2)


r_ab3 <- feols(change_log_deposits_35_36 ~ winsorize(overhead_efficiency) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab3)

r_ab4 <- feols(change_log_deposits_32_36 ~ urban_1930 + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab4)

r_ab5 <- feols(change_log_deposits_32_36 ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab5)


r_ab6 <- feols(change_log_deposits_32_36 ~ winsorize(overhead_efficiency) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab6)

r_ab7 <- feols(change_log_sales_33_35 ~ urban_1930 + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab7)

r_ab8 <- feols(change_log_sales_33_35 ~ winsorize(agglomeration_1930) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab8)


r_ab9 <- feols(change_log_sales_33_35 ~ winsorize(overhead_efficiency) + agric_1930 + mf_1930 + white_1930 + foreign_born_1930
    + union_potential_1939 | state_1930, df_cs[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(r_ab9)



models <- list(r_ab1, r_ab2, r_ab3, r_ab4, r_ab5, r_ab6, r_ab7, r_ab8, r_ab9)
prepped <- prep_models(models)




texreg(prepped[[1]],
    file = "Text/Tables/table_a15.tex",
    label = "table_urban_growth_orthogonal",
    caption = "Conditional on controls, urbanization, agglomeration and overhead efficiency, efficiency were uncorrelated with short term economic growth",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"),
    custom.coef.map = list(
        "urban_1930" = "Urban (%)",
        "winsorize(agglomeration_1930)" = "Agglomeration",
        "winsorize(overhead_efficiency)" = "Overhead efficiency"
        ),
    custom.gof.rows = list(
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("$\\Delta$ Log deposits, '35--'36" = 1:3,
        "$\\Delta$ Log deposits, '32--'36" = 4:6,
        "$\\Delta$ Log sales, '33--'35" = 7:9),
    custom.note = paste0("\\item This table shows the results of regressions of
        short-run economic growth against urbanization, agglomeration and overhead efficiency.
        In (1)--(3) the dependent variable is the change in the log value of bank
        deposits between 1935 and 1936, in (4)--(6), the change in the log 
        value of bank deposits between 1932 and 1936, in (7)--(9) the change in
        log retail sales per capita between 1933 and 1935.
        All models include state fixed
        effects and controls for the shares
        employed in agriculture and manufacturing, the white and immigrant
        shares of the population, and union potential.
        Standard errors clustered by state in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    no.margin = TRUE,
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )

#%% Table A-5: summary statistics

prep_summary <- function(vec) {
    vec <- vec[!is.na(vec) & is.finite(vec)]
    vec_obs <- length(vec)
    vec_mean <- mean(vec)
    vec_sd <- sd(vec)
    quantiles <- quantile(vec, c(0.05, 0.95))
    vec_q5 <- quantiles[1]
    vec_q95 <- quantiles[2]
    return(c(vec_obs, vec_mean, vec_sd, vec_q5, vec_q95))
}


df_v_temp <- df_vote[!(state_1930 %in% confederacy_states)][election_year == 1936]
df_temp <- df_cs[!(state_1930 %in% confederacy_states)]
vecs <- list(df_temp[, urban_1930],
    df_temp[, (agglomeration_1930)],
    df_temp[, winsorize(agglomeration_1930)],
    df_temp[, (overhead_efficiency)],
    df_temp[, winsorize(overhead_efficiency)],
    df_v_temp[, d_dem2p],
    df_temp[, agric_1930],
    df_temp[, mf_1930],
    df_temp[, white_1930],
    df_temp[, foreign_born_1930],
    df_temp[, union_potential_1939],

    df_temp[, sales_pc_1929],
    df_temp[, d_sales],
    df_temp[, deposits_pc_1932],
    df_temp[, d_deposits_pc],
    df_temp[, strikes_percap * 10000],
    df_temp[, locals_percap * 10000],
    df_temp[, farms_pc_1930],
    df_temp[, ag_output_pc_1930],
    df_temp[, ue_1930],
    df_temp[, illiteracy_rate_1930],
    df_v_temp[, d_dem2p_2832],

    df_temp[, log(agglomeration_1930)],
    df_temp[, log(overhead_efficiency)],
    df_temp[, log(highway_spending_share)],
    df_temp[, log(police_fire_spend_percap)],
    df_temp[, log(police_fire_percap)],

    df_temp[, highway_spending_share],
    df_police_prod[, exp_police_resid]

    )
var_names <- c("\\% urban, 1930",
    "agglomeration, 1930",
    "agglomeration (winsorized), 1930",
    "overhead efficiency, 1932",
    "overhead efficiency (winsorized), 1932",
    "$\\Delta$ Democrat share of two-party vote, 1932--1936",
    "\\% employed in agriculture, 1930",
    "\\% employed in manufacturing, 1930",
    "\\% white, 1930",
    "\\% foreign-born, 1930",
    "union potential, 1930",
    "retail sales per capita, 1929",
    "$\\Delta$ retail sales per capita, 1933--1935",
    "bank deposits per capita, 1932",
    "$\\Delta$ bank deposits per capita, 1932--1936",
    "IWW strikes per capita $\\times$ 10,000",
    "IWW locals per capita $\\times$ 10,000",
    "farms per capita, 1930",
    "agricultural output per capita, 1930",
    "unemployment rate, 1930",
    "illiteracy rate, 1930",
    "$\\Delta$ Democrat share of two-party vote, 1928--1932",
    "log agglomeration, 1930",
    "log overhead efficiency, 1932",
    "log share of spending on highways, 1932",
    "log police and fire spending per capita, 1932",
    "log police and firemen per capita, 1932",
    "share of spending on highways, 1932",
    "police and fire productivity, 1932"

    )

df_sum_stats <- data.table(t(simplify2array(lapply(vecs, prep_summary))))
df_varnames <- data.table(var_names)
df_sum_stats <- cbind(df_varnames, df_sum_stats)

colnames(df_sum_stats) <- c("Variable", "N", "Mean", "SD", "q5", "q95")
df_sum_stats

table_sum_stats <- kable(df_sum_stats, booktabs = TRUE, format = "latex",
    caption = "County-level summary statistics",
    label = "table_sum_stats", digits = c(0, 0, 3, 3, 3, 3), escape = FALSE, format.args = list(big.mark = ",",
    scientific = FALSE),
    linesep = c(""))
table_sum_stats



cat(table_sum_stats, file = "Text/Tables/table_a5.tex")

#%%%% Figures Using County-Level Data


# load long-run voting timeseries data
df_vote_ts <- fread("Data/_Clean/us_voting_pres_long_timeseries.csv")
max(df_vote_ts$year)
print(colnames(df_cs))

# code for Figures 1 and A-9
df_vote_ts <- merge(df_vote_ts, df_cs[, .(state_1930, county_1930, rural_1930)])

df_vote_ts[, `:=`(dem_vote = dem_pres * total_vote,
    rep_vote = rep_pres * total_vote)]

df_vote_ts[, dem2p := dem_pres / (dem_pres + rep_pres)]

#%% Figure 1
rural_mean <- df_cs[, weighted.mean(rural_1930, n_census_1930, na.rm = TRUE)]


df_vote_plot <- df_vote_ts[!(state_1930 %in% confederacy_states) & !is.na(rural_1930),
    .(dem_vote = sum(dem_vote, na.rm = TRUE),
    rep_vote = sum(rep_vote, na.rm = TRUE)),
    by = .(rural = rural_1930 > rural_mean, year)]
df_vote_plot[rural == TRUE, rural_text := "Above average"]
df_vote_plot[rural == FALSE, rural_text := "Below average"]
df_vote_plot[rural == TRUE, urban_text := "Below average urban"]
df_vote_plot[rural == FALSE, urban_text := "Above average urban"]
df_vote_plot[, rural_text := as.factor(rural_text)]


ggplot(df_vote_plot[], aes(x = year, dem_vote / (dem_vote + rep_vote) * 100,
    color = rural_text)) +
    geom_vline(xintercept = 1932, color = "grey") +
    geom_label(aes(x = c(1928), y = c(65), label = c("1932")), family = "Univers",
        color = "black"
        ) +
    geom_line() +
    scale_color_brewer(palette = "Set1") +
    geom_text_repel(data = df_vote_plot[year ==2016],
        aes(x = Inf,
            y = c(43, 64), label = urban_text), family = "Univers", seed = 123) +
    guides(color = "none") +
    labs(x = element_blank(), y = "Democrat (%)\n",
        )
ggsave("Text/Figures/fig_1.pdf", width = 6, height = 4)




#%% Figure 3

# code to regress variable against overhead efficiency
get_coef_overhead_efficiency <- function(dv_string) {
    temp_formula_base <- as.formula(paste0("standardize(", dv_string, ") ~ ",
        "winsorize(standardize(overhead_efficiency)) | state_1930"))

    temp_mod_base <- feols(temp_formula_base, df_cs[!(state_1930 %in% confederacy_states)],
        cluster = ~ state_1930, fixef.tol = 1e-9)
    temp_formula <- as.formula(paste0("standardize(", dv_string, ") ~ ",
        "winsorize(standardize(overhead_efficiency)) + urban_1930 | state_1930"))

    temp_mod <- feols(temp_formula, df_cs[!(state_1930 %in% confederacy_states)],
        cluster = ~ state_1930, fixef.tol = 1e-9)
    df_temp <- data.table(var = rep(dv_string, 2), mod = c("base", "urban"),
        coef = c(temp_mod_base$coefficients[1], temp_mod$coefficients[1]),
        se = c(temp_mod_base$se[1], temp_mod$se[1]))

    return(df_temp)
}
vars <- c("agric_1930", "mf_1930", "white_1930", "foreign_born_1930", "union_potential_1939",
    "sales_pc_1929", "d_sales", "deposits_pc_1932", "d_deposits_pc",
    "strikes_percap", "locals_percap",
    "farms_pc_1930", "ag_output_pc_1930", "ue_1930",
    "illiteracy_rate_1930", "urban_1930", "city_1930", "winsorize(agglomeration_1930)",
    "log_density")
coef_out <- lapply(vars, get_coef_overhead_efficiency) %>% rbindlist()



coef_out
var_names <- c("Agricultural (%)", "Manufacturing (%)", "White (%)", "Foreign-born (%)", "Union potential",
    "Sales per capita 1929",
    "Change in sales per capita 1933-1935", "Deposits per capita 1932", "Change in deposits per capita 1932-1936",
    "IWW strikes per capita", "IWW locals per capita",
    "Farms per capita", "Farm output per capita", "Unemployed 1930 (%)",
    "Illiterate (%)", "Urban (%)", "City (%)", "Agglomeration", "Log density")
coef_out[, var_name := rep(var_names, each = 2)]
coef_out[, var_name := forcats::fct_rev(factor(var_name, levels = unique(var_name)))]
coef_out[mod == "base", specification := "Base specification"]
coef_out[mod == "urban", specification := "Controlling for percentage urban"]
coef_out[1:10, category := "Baseline controls"]
coef_out[11:18, category := "Levels and changes in income"]
coef_out[19:22, category := "Unionization"]
coef_out[23:30, category := "Additional controls"]
coef_out[31:38, category := "Urbanization"]

coef_out[, category := factor(category, levels = unique(category))]

df_categories <- data.table(var_name = rep(c("<b>Baseline controls</b>",
    "<b>Levels and changes in income</b>",
    "<b>Unionization</b>",
    "<b>Additional controls</b>",
    "<b>Urbanization</b>"), each = 2),
    specification = rep(c("Base specification", "Controlling for percentage urban"), 5))
df_categories <- rbind(df_categories, coef_out, fill = TRUE)


vars_order <- c("<b>Baseline controls</b>", "Agricultural (%)", "Manufacturing (%)", "White (%)", "Foreign-born (%)", "Union potential",
    "<b>Levels and changes in income</b>",
    "Sales per capita 1929",
    "Change in sales per capita 1933-1935", "Deposits per capita 1932", "Change in deposits per capita 1932-1936",
    "<b>Unionization</b>",
    "IWW strikes per capita", "IWW locals per capita",
    "<b>Additional controls</b>",
    "Farms per capita", "Farm output per capita", "Unemployed 1930 (%)",
    "Illiterate (%)",
    "<b>Urbanization</b>",
    "Urban (%)", "City (%)", "Agglomeration", "Log density")

df_categories[, var_name := forcats::fct_rev(factor(var_name, levels = vars_order))]


ggplot(df_categories, aes(x = var_name, y = coef, ymin = coef - 1.96 * se, ymax = coef + 1.96 * se,
    color = specification, shape = specification)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_pointrange(size = 0.5, position = position_dodge(width = -0.5)) +
    scale_color_brewer(palette = "Set1") +
    theme(axis.text.y = element_markdown(size = 10, color = "black")) +
    coord_flip() +
    labs(y = "\nStandardized coefficient on overhead efficiency", x = element_blank(),
        color = element_blank(), shape = element_blank(),
        )
ggsave(file = "Text/Figures/fig_3.pdf", width = 9, height = 8)







#%% Figure 4

rc <- feols(I(dem2p) ~ i(election_year, winsorize(overhead_efficiency), 1932)
    | county_1930 ^ state_1930 + state_1930 ^ election_year +
    election_year[mf_1930, agric_1930, white_1930, foreign_born_1930, union_potential_1939, urban_1930],
    df_vote[!(state_1930 %in% confederacy_states)][between(election_year, 1900, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rc)


df_plot <- data.table(iplot(rc)$prm)

ggplot(df_plot[], aes(x = x, y = y, ymin = ci_low, ymax = ci_high)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = 1932, color = "gray") +
    geom_pointrange(size = 0.3, position = position_dodge(width=1)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = element_blank(), color = element_blank(), y = "Coefficient\n", shape = element_blank()
        )
ggsave("Text/Figures/fig_4.pdf", width = 6, height = 4)

#%% Figure A-3

bs_oe1 <- binscatter_controls(data = df_cs[!(state_1930 %in% confederacy_states)], x_var = "winsorize(overhead_efficiency)",
    y_var = "I(general_gov_spend_percap * 1000)") +
    labs(x = "Overhead efficiency", y = "General gov. spending ($/pop)",
        title = "Overhead efficiency and\ncost of general government")



bs_oe2 <- binscatter_controls(data = df_cs[!(state_1930 %in% confederacy_states)], x_var = "winsorize(overhead_efficiency)",
    y_var = "I( (non_general_gov_spend_percap) * 1000)") +
    coord_cartesian(ylim = c(25, 36)) +
    labs(x = "Overhead efficiency", y = "Other gov. spending ($/pop)",
        title = "Overhead efficiency and\nother spending per capita")

ggsave(bs_oe1 | bs_oe2, filename = "Text/Figures/fig_a3.pdf",
    width = 9, height = 3)


#%% Figure A-9

df_vote_ts[, urban_1930 := 1 - rural_1930]

rv <- feols(dem2p ~ i(year, urban_1930) | state_1930 ^ year,
    df_vote_ts[!(state_1930 %in% confederacy_states)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
iplot(rv)
clean_up_iplot <- function(reg, title = NULL, xlab = element_blank(), ylab = "Coefficient") {
    df_plot <- iplot(reg)$prms
    plot_out <- ggplot(df_plot[], aes(x = x, y = y, ymin = ci_low, ymax = ci_high)) +
        geom_hline(yintercept = 0, color = "gray") +
        geom_vline(xintercept = df_plot$x[df_plot$is_ref], color = "gray") +
        geom_pointrange(size = 0.3) +
        labs(x = xlab, y = ylab, title = title)
    return(plot_out)
}
clean_up_iplot(rv, title = "Relationship between 1930 % urban and Democratic Party voting,\nnon-Southern counties, 1880-2020")
ggsave("Text/Figures/fig_a9.pdf", width = 6, height = 4)



#%% Figure A-13


reu <- feols(I(dem2p) ~ i(election_year, (urban_1930), 1932)
    | county_1930 ^ state_1930 +
    state_1930 ^ election_year +
        election_year[agric_1930, mf_1930, white_1930, foreign_born_1930, union_potential_1939],
    df_vote[!(state_1930 %in% confederacy_states)][between(election_year, 1920, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(reu)
iplot(reu)



reagglom <- feols(I(dem2p) ~ i(election_year, (winsorize(agglomeration_1930)), 1932)
    | county_1930 ^ state_1930 +
    state_1930 ^ election_year +
        election_year[agric_1930, mf_1930, white_1930, foreign_born_1930, union_potential_1939],
    df_vote[!(state_1930 %in% confederacy_states)][between(election_year, 1920, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(reagglom)
iplot(reagglom)


make_plot <- function(reg_obj, var_string) {

    df_plot <- iplot(reg_obj)$prms %>% data.table()
    ggplot(df_plot[], aes(x = x, y = y, ymin = ci_low, ymax = ci_high)) +
        geom_hline(yintercept = 0, color = "gray") +
        geom_vline(xintercept = df_plot$x[df_plot$is_ref], color = "gray") +
        geom_pointrange(size = 0.3) +
        labs(x = element_blank(), color = element_blank(), y = "Coefficient", shape = element_blank(),
            title = var_string)

}

plot_urban <- make_plot(reu, "Urban (%)")
plot_agglom <- make_plot(reagglom, "Agglomeration")

ggsave(plot_urban | plot_agglom, file = "Text/Figures/fig_a13.pdf", width = 9, height = 3)


# Figure A-14



rea <- feols(I(dem2p) ~ i(election_year, (agric_1930), 1932)
    | county_1930 ^ state_1930 +
    state_1930 ^ election_year +
        election_year[urban_1930, mf_1930, white_1930, foreign_born_1930, union_potential_1939],
    df_vote[!(state_1930 %in% confederacy_states)][between(election_year, 1920, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rea)
rem <- feols(I(dem2p) ~ i(election_year, (mf_1930), 1932) 
    | county_1930 ^ state_1930 +
    state_1930 ^ election_year +
        election_year[urban_1930, agric_1930, white_1930, foreign_born_1930, union_potential_1939],
    df_vote[!(state_1930 %in% confederacy_states)][between(election_year, 1920, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rem)
iplot(rem)
rew <- feols(I(dem2p) ~ i(election_year, (white_1930), 1932)
    | county_1930 ^ state_1930 +
    state_1930 ^ election_year +
        election_year[urban_1930, agric_1930, mf_1930, foreign_born_1930, union_potential_1939],
    df_vote[!(state_1930 %in% confederacy_states)][
        between(election_year, 1920, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rew)
ref <- feols(I(dem2p) ~ i(election_year, (foreign_born_1930), 1932)
    | county_1930 ^ state_1930 +
    state_1930 ^ election_year +
        election_year[urban_1930, agric_1930, mf_1930, white_1930, union_potential_1939],
    df_vote[!(state_1930 %in% confederacy_states)][
        between(election_year, 1920, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(ref)
iplot(ref)
rel <- feols(I(dem2p) ~ i(election_year, (union_potential_1939), 1932)
    | county_1930 ^ state_1930 +
    state_1930 ^ election_year +
        election_year[urban_1930, agric_1930, mf_1930, white_1930, foreign_born_1930],
    df_vote[!(state_1930 %in% confederacy_states)][
        between(election_year, 1920, 1960)][!is.na(dem2p)],
    cluster = ~ state_1930, fixef.tol = 1e-9)
summary(rel)


plot_agric <- make_plot(rea, "Agricultural (%)")
plot_mf <- make_plot(rem, "Manufacturing (%)")
plot_white <- make_plot(rew, "White (%)")
plot_foreign <- make_plot(ref, "Foreign-born (%)")
plot_union <- make_plot(rel, "Union potential")



ggsave(plot_agric | plot_mf, file = "Text/Figures/fig_a14_ag_mf_ev.pdf", width = 9, height = 3)
ggsave(plot_white | plot_foreign,
    file = "Text/Figures/fig_a14_white_foreign_ev.pdf", width = 9, height = 3)
ggsave(plot_union, file = "Text/Figures/fig_a14_union_ev.pdf", width = 4.5, height = 3)


#%% Figure A-15


sf_map <- st_read("Data/Shapefiles/nhgis0036_shapefile_tl2008_us_county_1930/US_county_1930_conflated.shp")
object.size(sf_map)
sf_map <- st_simplify(sf_map, dTolerance = 1000)
object.size(sf_map)
sf_map$state_1930 <- as.numeric(sf_map$NHGISST)
sf_map$county_1930 <- as.numeric(sf_map$NHGISCTY)
unique(sf_map$STATENAM)
sf_map <- sf_map[!(sf_map$STATENAM %in% c("Hawaii Territory", "Alaska Territory")),]


discretize_variable <- function(var) {
    quantiles <- quantile(var, c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
    var_cut <- cut(var, breaks = quantiles, include.lowest = TRUE)
    var_cut <- as.numeric(var_cut)
    return(var_cut)
}

df_overhead_efficiency_map <- df_cs[!(state_1930 %in% confederacy_states)][, .(state_1930, county_1930, overhead_efficiency,
    urban_1930)]
df_overhead_efficiency_map[, overhead_efficiency_w := winsorize(overhead_efficiency)]
rt <- feols(overhead_efficiency_w ~ urban_1930 | state_1930,
    df_overhead_efficiency_map)
df_overhead_efficiency_map[rt$obs_selection$obsRemoved, overhead_efficiency_resid := rt$residuals]
df_overhead_efficiency_map[, overhead_efficiency_q := discretize_variable(overhead_efficiency_w)]
df_overhead_efficiency_map[, overhead_efficiency_resid_q := discretize_variable(overhead_efficiency_resid)]
df_overhead_efficiency_map[is.na(overhead_efficiency_q) & !is.na(overhead_efficiency_resid_q)]


sf_map_c <- merge(sf_map, df_overhead_efficiency_map,
    by = c("state_1930", "county_1930"), all.x = TRUE)

ggplot(sf_map_c, aes(fill = overhead_efficiency_q)) +
             scale_fill_distiller(palette = "Blues",  guide = "legend", direction = 1) +
             geom_sf(linewidth = 0) +
             labs(fill = "quintile", title = "Overhead efficiency") +
             theme(axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "right")


ggsave(file = "Text/Figures/fig_a15_raw.pdf",
         width = 4.5, height = 3, device = Cairo::CairoPDF)


ggplot(sf_map_c, aes(fill = overhead_efficiency_resid_q)) +
          scale_fill_distiller(palette = "Blues",  guide = "legend", direction = 1) +
          geom_sf(linewidth = 0) +
          labs(fill = "quintile", title = "Overhead efficiency, residualizing out % urban") +
            theme(axis.text.x = element_blank(), axis.text.y = element_blank(), legend.position = "right")

ggsave(file = "Text/Figures/fig_a15_controls.pdf",
         width = 4.5, height = 3, device = Cairo::CairoPDF)



#%% Figure A-16


varstrings <- c("log_nd_fera_pc", "log_nd_cwa_pc", "log_nd_wpa_pc", "log_nd_sspa_pc",
    "log_nd_pwa_pc", "log_nd_pra_pc", "log_nd_pba_pc", "log_nd_aaa_pc", "log_nd_fca_pc",
    "log_nd_fsa_pc", "log_nd_rural_elec_pc", "log_nd_rfc_pc", "log_nd_holc_pc", "log_nd_fha_pc",
    "log_nd_usha_pc", "log_nd_total_pc")


varnames <- c("Federal Emergency Relief Admin.", "Civil Works Admin.", "Works Progress Admin.",
    "Social Security Public Assistance", "Public Works Admin.", "Public Roads Admin.",
    "Public Buildings Admin.", "Agricultural Adjustment Admin.", "Farm Credit Admin.",
    "Farm Security Admin.", "Rural Electrification", "Reconstruction Finance Corp.",
    "Home Owners Loan Corp.", "Farm Housing Admin.", "US Housing Admin.", "Total")

# code to regress variables against urbanization, agglomeration and overhead efficiecny
get_coefs_all <- function(varstring, varname) {
    formula_base <- as.formula(paste0(varstring, " ~ urban_1930 | state_1930"))
    formula_controls <- as.formula(paste0(varstring, " ~ urban_1930  + agric_1930 +
        mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939| state_1930"))
    formula_agg_base <- as.formula(paste0(varstring, " ~ winsorize(agglomeration_1930) | state_1930"))
    formula_agg_controls <- as.formula(paste0(varstring, " ~ winsorize(agglomeration_1930)  + agric_1930 +
        mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939| state_1930"))
    formula_c_base <- as.formula(paste0(varstring, " ~ winsorize(overhead_efficiency) | state_1930"))
    formula_c_controls <- as.formula(paste0(varstring, " ~ winsorize(overhead_efficiency) + agric_1930 +
        mf_1930 + foreign_born_1930 + white_1930 + union_potential_1939| state_1930"))
    
    estim_coefs <- function(formula_obj) {
        mod_base <- feols(formula_obj, df_cs[!(state_1930 %in% confederacy_states)],
            cluster = ~ state_1930, fixef.tol = 1e-9)
        se_vec <- mod_base$coeftable[, 2]
        ci_vec <- confint(mod_base)[1, ]

        df_out <- data.table(coef = mod_base$coefficients[1], se = se_vec[1], ci_l = ci_vec[1], ci_h = ci_vec[2])
    }

    df_res <- lapply(list(formula_base, formula_controls, formula_agg_base, formula_agg_controls,
        formula_c_base, formula_c_controls),
        estim_coefs) %>% rbindlist()
    df_res[, spec := rep(c("State FE", "State FE +\ncontrols"), 3)]
    df_res[, iv := c("Urban (%)", "Urban (%)", "Agglomeration", "Agglomeration", "Overhead efficiency", "Overhead efficiency")]
    df_res[, dv := varname]
    return(df_res)
}


df_coef_all <- lapply(1:length(varstrings), function(x){get_coefs_all(varstrings[x], varnames[x])}) %>% rbindlist()
df_coef_all

df_coef_all[, dv := factor(dv, levels = unique(dv))]
df_coef_all[, dv := forcats::fct_rev(dv)]

ggplot(df_coef_all, aes(x = dv, y = coef, ymin = `ci_l.2.5 %`, ymax = `ci_h.97.5 %`,
    color = spec, shape = spec)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_pointrange(size = 0.3, position = position_dodge(width=-0.5)) +
    coord_flip() +
    facet_grid(cols = vars(iv), scales = "free") +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Dependent variable\n(log spending / population)", y = "Coefficient [95% CI]", color = "Specification", shape = "Specification",
        title = "Urbanization, overhead efficiency, and New Deal spending") +
    theme(legend.position = "right", axis.text.y = element_text(color = "black"))

ggsave("Text/Figures/fig_a16.pdf", width = 9, height = 3)


#%%%% City-level data


df_cities <- fread("Data/_Clean/cities_efficiency.csv")
df_cities_emp <- fread("Data/_Clean/cities_employment.csv")

#%% Table A-1
r1 <- feols(winsorize(log(overhead_efficiency)) ~ log(pop), df_cities, vcov = "hetero")
summary(r1)
r2 <- feols(winsorize(log(overhead_efficiency)) ~ log(pop) | state, df_cities, vcov = "hetero", fixef.tol = 1e-9)
summary(r2)



r3 <- feols(log(highway_spending_share) ~ log(pop), df_cities, vcov = "hetero")
r4 <- feols(log(highway_spending_share) ~ log(pop) | state, df_cities, vcov = "hetero", fixef.tol = 1e-9)

r5 <- feols(log(police_fire_percap) ~ log(pop) + log(police_fire_spend_percap), df_cities_emp, vcov = "hetero")

r6 <- feols(log(police_fire_percap) ~ log(pop) + log(police_fire_spend_percap) | state, df_cities_emp, vcov = "hetero", fixef.tol = 1e-9)




models <- list(r1, r2, r3, r4, r5, r6)
prepped <- prep_models(models)

prepped[[1]]

texreg(prepped[[1]],
    file = "Text/Tables/table_a1.tex",
    label = "table_town_efficiency",
    caption = "Effects of town size on public sector efficiency, 1932",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
    custom.coef.map = list("log(pop)" = "Log population",
        "log(police_fire_spend_percap)" = "Log police \\& fire spend / pop"),
    custom.gof.rows = list(
        "State FE" = c("", "x", "", "x", "", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("Overhead efficiency" = c(1:2), "Highways" = 3:4, "Police \\& fire / pop" = 5:6),
    custom.note = paste0("\\item This table shows the results of regressions
        of government effciency against log town population, for
        non-Southern cities with more than 8,000 residents in 1932, for which
        spending by category is reported. In models (1) and (2) the dependent
        variable is log overhead efficiency, calculated as spending on
        government provision exclusive
        of central administrative costs and highway maintenance divided by total government operating spending
        net of highway maintenance, winsorized. In (3) and (4) the log ratio of
        highway maintenance spending to total government operating spending,
        in (5) and (6) the log number of police and firefighters divided by population.
        Even-numbered models add state fixed effects.
        Robust standard errors in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )


#%% Figure 2
binscatter_unconditional_overlay <- function(data, x_var, y_var, weight_var = NULL,
    nbins = 20, xlims = NULL, ylims = NULL, binpointsize = 0.75, basepointsize = 0.5) {


    if (length(weight_var) == 0) {
        data$temp_weight <- rep(1, nrow(data))
    } else {
        data$temp_weight <- data[, ..weight_var]
    }

    keep_formula <- as.formula(paste0(y_var, " ~ ", x_var, " + temp_weight"))
    df_keep <- model.frame(keep_formula, data)
    df_keep <- data.table(df_keep)
    colnames(df_keep) <- c("y", "x", "weight")
    df_bin <- df_keep[order(x)]
    df_bin[, `:=`(cum_weight, cumsum(weight))]
    df_bin[, `:=`(bin, cut(cum_weight, nbins))]
    df_bin <- df_bin[, .(x = weighted.mean(x, weight), y = weighted.mean(y,
        weight)), by = .(bin)]


    if (length(weight_var) == 0) {
        ggplot() +
            geom_point(data = df_keep, aes(x = x, y = y), size = basepointsize, alpha = 0.3) +
            geom_point(data = df_bin, aes(x = x, y = y), size = binpointsize, color = brewer.pal(n = 9, name = "Set1")[1]) +
            coord_cartesian(ylim = ylims, xlim = xlims)
    } else {
        ggplot() +
            geom_point(data = df_keep, aes(x = x, y = y, size = weight), alpha = 0.3) +
            geom_point(data = df_bin, aes(x = x, y = y), size = binpointsize, color = brewer.pal(n = 9, name = "Set1")[1]) +
            coord_cartesian(ylim = ylims, xlim = xlims)
    }
}

binscatter_unconditional_overlay(df_cities[], x_var = "log(pop)", y_var = "winsorize(log(overhead_efficiency))", nbins = 10) +
    labs(x = "\nLog population", y = "Log overhead efficiency (winsorized)\n",
    )
ggsave(file = "Text/Figures/fig_2.pdf", width = 6, height = 4)


#%%%% UK Data


# UK data 
df_uk <- fread("Data/_Clean/analysis_data_uk.csv")

# Table 3

rb1 <- feols(diff_cons_pc_1900 ~ log(constituency_area) ,
    df_uk[year == 1906], vcov = "hetero")
summary(rb1)
rb2 <- feols(diff_cons_pc_1900 ~ log(constituency_area) +  agric + mf_mining + working_class , 
    df_uk[year == 1906], vcov = "hetero")
summary(rb2)


rb21 <- feols(diff_cons_pc_1918 ~ log(constituency_area) ,
    df_uk[year == 1922], vcov = "hetero")
summary(rb21)
rb22 <- feols(diff_cons_pc_1918 ~ log(constituency_area) + agric + mf_mining + working_class ,
    df_uk[year == 1922], vcov = "hetero")
rb23 <- feols(diff_cons_pc_1918 ~ log(constituency_area) + new_male_electorate_pc + agric + mf_mining + working_class ,
    df_uk[year == 1922], vcov = "hetero")    
summary(rb23)




models <- list(rb1, rb2, rb21, rb22, rb23)

prepped <- prep_models(models)


texreg(prepped[[1]],
    file = "Text/Tables/table_3.tex",
    label = "table_uk_voting",
    caption = "Effects of constituency area on shifts in Conservative voting in the UK",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)"),
    custom.coef.map = list("log(constituency_area)" = "Log area",
        "new_male_electorate_pc" = "New male voters 1918 (%)"),
    custom.gof.rows = list(
        "Controls" = c("", "x", "", "x", "x"),
        "DV mean" = prepped[[2]]),
    custom.gof.names = c("N", "$R^2$"),
    custom.header = list("1900--1906" = 1:2, "1918--1922" = c(3:5)),
    custom.note = paste0("\\item This table shows the results of constituency-level regressions
        of the change in the Conservative vote share between 1900 and 1906 and
        between 1918 and 1922 on the log of constituency area.
        Models (2), (4), and (5) also control for the share employed in agriculture,
        the share employed in manufacturing and mining, and the share in working
        class occupations. (5) controls for the percentage of male voters who were
        recently enfranchised, calculated as the difference between the 1918 
        male parliamentary and local electorates, divided by the 1918 male
        parliamentary electorate.
        Robust standard errors in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(1e-100, 0.01, 0.05, 0.1),
    symbol = "\\dag",
    digits = 3,
    booktabs = T,
    threeparttable = T,
    use.packages = F
    )

#%% Figure 6

r1 <- feols(cons_pc ~ i(year, log(constituency_area)) 
    |   year,
    df_uk, cluster = ~ constituency_id, fixef.tol = 1e-9)
summary(r1)

r2 <- feols(cons_pc ~ i(year, log(constituency_area)) 
    |   year + year[agric, working_class, mf_mining],
    df_uk, cluster = ~ constituency_id, fixef.tol = 1e-9)
summary(r2)
r3 <- feols(cons_pc ~ i(year, log(constituency_area)) 
    |   year + year[agric, working_class, mf_mining, new_male_electorate_pc],
    df_uk[!is.na(new_male_electorate_pc)], cluster = ~ constituency_id, fixef.tol = 1e-9)
summary(r3)




df_plot <- iplot(r1)$prms %>% data.table()
df_plot[, specification := "Base specification"]

df_plot2 <- data.table(iplot(r2)$prms)
df_plot2[, specification := "Controls x Election"]

df_plot3 <- data.table(iplot(r3)$prms)
df_plot3[, specification := "Including franchise extension controls"]


df_plot <- rbindlist(list(df_plot, df_plot2, df_plot3))

ggplot(df_plot[], aes(x = x, y = y, ymin = ci_low, ymax = ci_high,
    color = specification, shape = specification)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_pointrange(size = 0.3, position = position_dodge(width=1)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = element_blank(), color = element_blank(), y = "Coefficient\n", shape = element_blank()
        )
ggsave("Text/Figures/fig_6.pdf", width = 6, height = 4)





#%%%% Figures using idiosyncratic datasets

#%% Figure A-1
df_place_both <- fread("Data/_Clean/emergency_work_social_security_1940_census.csv")

plot_ew1 <- binscatter_unconditional(x_var = "log(n_place)", y_var = "emergency_work",
    df_place_both, nbins = 20) +
    labs(x = "Log place size", y = "% of unemployed on emergency work", title = "Emergency work")


plot_ew2 <- binscatter_unconditional(x_var = "log(n_place)", y_var = "I(emergency_work > 0) * 100",
    df_place_both, nbins = 20) +
    labs(x = "Log place size", y = "% places with non-zero emergency work", title = "Non-zero emergency work")


plot_ew3 <- binscatter_unconditional(x_var = "log(n_place)", y_var = "social_security",
    df_place_both, nbins = 20) +
    labs(x = "Log place size", y = "% of eligible population enrolled", title = "Social Security enrollment")

ggsave(plot_ew1 | plot_ew2 | plot_ew3, file = "Text/Figures/fig_a1.pdf",
    width = 9, height = 3)

# Figure A-2
df_ue <- fread("Data/_Clean/social_security_admin_costs_state.csv")

ggplot(df_ue, aes(x = log_share_urban,
    y = log_admin_ratio,
    label = postal_code)) +
    geom_smooth(method = 'lm', se = FALSE, color = brewer.pal(n = 9, name = "Set1")[2]) +
    geom_text(family = "Univers", size = 3) +
    labs(x = "Log % urban", y = "Log admin expenses / unemployment comp paid",
        title = "Urbanization and ratio of social security employment service\nadministrative expenses to unemployment compensation paid, 1940")
ggsave(file = "Text/Figures/fig_a2.pdf", width = 6, height = 4)


# Figure A-4 through A-6
df_wpa <- fread("Data/_Clean/WPA_cost_of_living.csv")

ggplot(df_wpa, aes(x = log(pop_1930), y = log(total), label = name_clean, color = b_south)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", linewidth = 0.5, se = FALSE) +
    geom_text_repel(size = 2, family = "Univers", max.overlaps = 4, aes(color = NULL), seed = 123) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.box.spacing = unit(0, "mm")) +
    labs(x = "Log population", y = "Log cost of living", title = "City size and cost of living, 1935 WPA study",
        color = element_blank())
ggsave(filename = "Text/Figures/fig_a4_wpa.pdf", width = 4.5, height = 3)


ggplot(df_wpa, aes(x = log(pop_1930), y = log(housing), label = name_clean, color = b_south)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", linewidth = 0.5, se = FALSE) +
    geom_text_repel(size = 2, family = "Univers", max.overlaps = 4, aes(color = NULL), seed = 123) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.box.spacing = unit(0, "mm")) +
    labs(x = "Log population", y = "Log housing cost", title = "City size and housing costs, 1935 WPA study",
        color = element_blank())
ggsave(filename = "Text/Figures/fig_a5_wpa.pdf", width = 4.5, height = 3)


ggplot(df_wpa, aes(x = log(pop_1930), y = log(food), label = name_clean, color = b_south)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", linewidth = 0.5, se = FALSE) +
    geom_text_repel(size = 2, family = "Univers", max.overlaps = 4, aes(color = NULL), seed = 123) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.box.spacing = unit(0, "mm")) +
    labs(x = "Log population", y = "Log food cost", title = "City size and food costs, 1935 WPA study",
        color = element_blank())
ggsave(filename = "Text/Figures/fig_a6_wpa.pdf", width = 4.5, height = 3)


df_ny <- fread("Data/_Clean/NY_cost_of_living.csv")

ggplot(df_ny, aes(x = log(pop), y = log(total_calc), label = town)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", color = my_blue, linewidth = 0.5, se= FALSE) +
    geom_text_repel(size = 2, family = "Univers", max.overlaps = 4, seed = 123) +
    labs(x = "Log population", y = "Log cost of living", title = "City size and cost of living, 1928 NY study")
ggsave(filename = "Text/Figures/fig_a4_ny.pdf", width = 4.5, height = 3)


ggplot(df_ny, aes(x = log(pop), y = log(rent), label = town)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", color = my_blue, linewidth = 0.5, se= FALSE) +
    geom_text_repel(size = 2, family = "Univers", max.overlaps = 4, seed = 123) +
    labs(x = "Log population", y = "Log housing cost", title = "City size and housing costs, 1928 NY study")
ggsave(filename = "Text/Figures/fig_a5_ny.pdf", width = 4.5, height = 3)


ggplot(df_ny, aes(x = log(pop), y = log(food), label = town)) +
    geom_point(size = 0.5) +
    geom_smooth(method = "lm", color = my_blue, linewidth = 0.5, se= FALSE) +
    geom_text_repel(size = 2, family = "Univers", max.overlaps = 4, seed = 123) +
    labs(x = "Log population", y = "Log food cost", title = "City size and food costs, 1928 NY study")
ggsave(filename = "Text/Figures/fig_a6_ny.pdf", width = 4.5, height = 3)



#%% Figure A-7

df_consumption <- fread("Data/_Clean/consumption_data.csv")
df_consumption[, group_f := factor(group, levels = c("village", "small city", "midsize city", "large city", "metropolis"))]

rcons1 <- feols(non_food_share ~ i(group_f), df_consumption, cluster = ~ townname, fixef.tol = 1e-9)
summary(rcons1)

iplot(rcons1)
df1 <- data.table(iplot(rcons1)$prms)
df1[, spec := "No controls"]


rcons2 <- feols(non_food_share ~ i(group_f) + log(total_spend), df_consumption, cluster = ~ townname, fixef.tol = 1e-9)
summary(rcons2)

df2 <- data.table(iplot(rcons2)$prms)
df2[, spec := "Controlling\nfor total spend"]


rcons3 <- feols(non_food_share ~ i(group_f) + log(total_spend) + log(famsize)| state + race, df_consumption, cluster = ~ townname, fixef.tol = 1e-9)
summary(rcons3)

df3 <- data.table(iplot(rcons3)$prms)
df3[, spec := "+ controls for family\nsize, state, and race"]

df_all <- rbindlist(list(df1, df2, df3))
df_all[, spec := factor(spec, levels = c("No controls", "Controlling\nfor total spend", "+ controls for family\nsize, state, and race"))]
ggplot(df_all, aes(x = estimate_names, y = estimate, ymin = ci_low, ymax = ci_high,
    color = spec, shape = spec)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_pointrange(size = 0.5, position = position_dodge(0.5)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = element_blank(), y = "Coefficient relative to village",
        color = element_blank(), shape = element_blank(),
        title = "Town size and non-food share of spending")
ggsave(filename = "Text/Figures/fig_a7_coef.pdf", width = 4.5, height = 3)

df_consumption[, groupf2 := group_f]
df_consumption[group_f == "small city", groupf2 := "small\ncity"]
df_consumption[group_f == "midsize city", groupf2 := "midsize\ncity"]
df_consumption[group_f == "large city", groupf2 := "large\ncity"]
df_consumption[, groupf2 := factor(groupf2, levels = c("village", "small\ncity", "midsize\ncity", "large\ncity", "metropolis"))]


ggplot(df_consumption[!is.na(group_f)], aes(x = log(total_spend), y = non_food_share, color = groupf2)) +
    geom_point(size = 0.2, alpha = 0.5) +
    scale_color_brewer(palette = "RdYlBu") +
    geom_smooth(se = FALSE, method = "loess") +
    theme(legend.spacing = margin(1, 1, 1, 1)) +
    labs(x = "Log total spend", y = "Non-food budget share", color = element_blank(),
        title = "Food Engel curves by town size") 
ggsave(filename = "Text/Figures/fig_a7_scatter.pdf", width = 4.5, height = 3)





#%% Figure A-10
df_spend_long <- fread("Data/_Clean/intergov_transfers_timeseries.csv")

df_spend_long <- melt(df_spend_long, id.vars = "year")
df_spend_long[between(year, 1900, 1960), max_val := max(value), by = .(variable)]
ggplot(df_spend_long[between(year, 1900, 1960)], aes(x = year, y = value)) +
    geom_vline(xintercept = 1932, color = "gray") +
    geom_label(aes(x = c(1929), y = max_val * 0.9, label = c("1932")),
        family = "Univers", color = "black") +
    geom_point() +
    geom_line() +
    facet_wrap(facets = ~ variable, scales = "free") +
    labs(x = element_blank(), y = "% Fed. intergov. transfers",
        title = "Federal intergovernmental transfers as a share of government spending, 1902-1960")

ggsave(filename = "Text/Figures/fig_a10.pdf", width = 9, height = 3)

#%% Figure A-11
df_wallis <- fread("Data/_Clean/federal_spending_timeseries.csv")

ggplot(df_wallis, aes(x = year, y = federal_ex_military_percentage)) +
    geom_vline(xintercept = 1932, color = "gray") +
    geom_label(aes(x = c(1930.5), y = c(56), label = c("1932")),
        family = "Univers", color = "black") +
    geom_line() +
    geom_point() +
    labs(title = "Federal share of non-defense government spending, 1902-1960", y = "Federal % of total",
        x = element_blank())
ggsave("Text/Figures/fig_a11.pdf", width = 6, height = 4)


#%% Figure A-12
df_union_ts <- fread("Data/_Clean/union_membership_timeseries.csv")

ggplot(df_union_ts, aes(x = year, y = bls_union_members)) +
    geom_vline(aes(xintercept = 1932), color = "gray") +
    geom_vline(aes(xintercept = 1936), color = "gray") +
    geom_line() +
    geom_label(aes(x = c(1930), y = c(16), label = c("1932")), family = "Univers"
        ) +
    geom_label(aes(x = c(1938), y = c(16), label = c("1936")), family = "Univers") +
    labs(x = element_blank(), y = "Union members (millions)", title = "Union membership, 1900-1960")
ggsave("Text/Figures/fig_a12.pdf", width = 6, height = 4)


#%% Figure A-18

my_blue <- brewer.pal(3, "Set1")[2]

df_wpid <- fread("Data/_Clean/cross_country_data.csv")
df_wpid_long <- melt(df_wpid,
    id.vars = c("isoname", "rural_urban"))
df_wpid_long
df_wpid_long[variable == "gdppc", `:=`(value = log(value), varname = "Log GDP per capita, 2010")]
df_wpid_long[variable == "gov_rev_pc", `:=`(varname = "Government revenue % of GDP, 2010")]
df_wpid_long[variable == "clientelism", `:=`(varname = "Clientelism, 2010")]
df_wpid_long[, varname := factor(varname, levels = c("Log GDP per capita, 2010",
    "Government revenue % of GDP, 2010", "Clientelism, 2010"))]


ggplot(df_wpid_long, aes(x = value, y = rural_urban, label = isoname)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_smooth(method = lm, color = my_blue, alpha = 0.2, se = FALSE) +
    geom_point(size = 0.5) + 
    geom_text_repel(family = "Univers", size = 2.5, seed = 123, box.padding = 0.15, max.overlaps = 10) +
    facet_wrap(~varname, scales = "free_x") +
    labs(x = element_blank(), y = "Urban-rural left-right gap, 2010-2020",
        title = "The urban-rural left-right divide, GDP, state capacity, and clientelism")
ggsave(filename = "Text/Figures/fig_a18.pdf",
    width = 9, height = 3)



#%% Figure A-19
df_anes_s <- fread("Data/_Clean/ANES_data.csv")

rr1 <- feols(vote_dem ~ i(year, gov_spending_01) | year,
    df_anes_s[!is.na(gov_spending_01) & !is.na(vote_dem)], vcov = "hetero", weights = ~ weight, fixef.tol = 1e-9)

rr2 <- feols(vote_dem ~ i(year, increase_welfare_01) | year,
    df_anes_s[!is.na(increase_welfare_01) & !is.na(vote_dem)], vcov = "hetero", weights = ~ weight, fixef.tol = 1e-9)

rr3 <- feols(vote_dem ~ i(year, lib_ideology_01) | year,
    df_anes_s[!is.na(lib_ideology_01) & !is.na(vote_dem)], vcov = "hetero", weights = ~ weight, fixef.tol = 1e-9)

df1 <- iplot(rr1)$prms
df2 <- iplot(rr2)$prms
df3 <- iplot(rr3)$prms

df1 <- data.table(df1)
df2 <- data.table(df2)
df3 <- data.table(df3)

df1[, var := "Support for government spending"]
df2[, var := "Support for welfare spending"]
df3[, var := "Liberal ideology"]

df_plot <- rbindlist(list(df1, df2, df3))

ggplot(df_plot, aes(x = x, y = y, ymin = ci_low, ymax = ci_high)) +
    facet_wrap(~var) +
    geom_pointrange(size = 0.5) +
    labs(x = element_blank(), y = "Relationship between variable and\nvoting Democrat",
        title = "Relationship between ideology and spending preferences and presidential vote choice")
ggsave(filename = "Text/Figures/fig_a19.pdf",
    width = 9, height = 3)

#%% Figure A-20


df_cces <- fread("Data/_Clean/cces_data.csv")
binscatter_controls_prep <- function(data, x_var, y_var, control_formula = NULL,
    weight_var = NULL, nbins = 20, xlims = NULL, ylims = NULL, case_name = "") {
    if (length(weight_var) == 0) {
        data$temp_weight <- rep(1, nrow(data))
    } else {
        data$temp_weight <- data[, ..weight_var]
    }
    
    # make sure no missingness on x, y, weight
    drop_formula <- as.formula(paste0(y_var, " ~ ", x_var, " + temp_weight"))
    drop_mod <- feols(drop_formula, data)
    if (length(drop_mod$obs_selection$obsRemoved) > 1) {
        data <- data[drop_mod$obs_selection$obsRemoved]
    }
    rm(drop_mod)

    if (length(control_formula) == 0) {
        df_out <- model.frame(drop_formula, data)
        df_out <- data.table(df_out)
        colnames(df_out) <- c("y", "x", "weight")
        rm(data)
    } else {
        y_formula <- as.formula(paste0(y_var, control_formula))
        x_formula <- as.formula(paste0(x_var, control_formula))

        y_mod <- feols(y_formula, data, weights = ~ temp_weight)
        x_mod <- feols(x_formula, data, weights = ~ temp_weight)

        df_out <- data.table(y = y_mod$residuals, x = x_mod$residuals, weight = x_mod$weights)
        rm(y_mod, x_mod, data)
    }

    
    df_out <- df_out[order(x)]
    df_out[, cum_weight := cumsum(weight)]
    df_out[, bin := cut(cum_weight, nbins)]

    # ols slope
    if (length(control_formula) == 0) {
        r_slope <- lm(y ~ x, df_out, weights = df_out$weight)
    } else {
        r_slope <- lm(y ~ x - 1, df_out, weights = df_out$weight)
    }
    

    df_bin <- df_out[, .(x = weighted.mean(x, weight),
        y = weighted.mean(y, weight)), by = .(bin)]
    rm(df_out)
    if (length(control_formula) == 0) {
        intercept_val <- r_slope$coefficients[1]
        slope_val <- r_slope$coefficients[2]
    } else {
        intercept_val <- 0
        slope_val <- r_slope$coefficients[1]
    }
    df_bin[, `:=`(intercept_val = intercept_val,
        slope_val = slope_val,
        case = case_name)]
    return(df_bin)
}

df_police <- binscatter_controls_prep(df_cces, x_var = "share_urban",
    y_var = "police_satisfaction", control_formula = "~ 0 | gender ^ educ ^ race ^ pid3 ^ union",
    weight_var = "commonweight", case_name = "Police")
df_schools <- binscatter_controls_prep(df_cces, x_var = "share_urban",
    y_var = "school_satisfaction", control_formula = "~ 0 | gender ^ educ ^ race ^ pid3 ^ union",
    weight_var = "commonweight", case_name = "Schools")
df_roads <- binscatter_controls_prep(df_cces, x_var = "share_urban",
    y_var = "roads_satisfaction", control_formula = "~ 0 | gender ^ educ ^ race ^ pid3 ^ union",
    weight_var = "commonweight", case_name = "Roads")
df_zoning <- binscatter_controls_prep(df_cces, x_var = "share_urban",
    y_var = "zoning_satisfaction", control_formula = "~ 0 | gender ^ educ ^ race ^ pid3 ^ union",
    weight_var = "commonweight", case_name = "Zoning")
df_mayor <- binscatter_controls_prep(df_cces, x_var = "share_urban",
    y_var = "mayor_satisfaction", control_formula = "~ 0 | gender ^ educ ^ race ^ pid3 ^ union",
    weight_var = "commonweight", case_name = "Mayor")
df_council <- binscatter_controls_prep(df_cces, x_var = "share_urban",
    y_var = "council_satisfaction", control_formula = "~ 0 | gender ^ educ ^ race ^ pid3 ^ union",
    weight_var = "commonweight", case_name = "Town Council")

df_plot <- rbindlist(list(df_police, df_schools, df_roads, df_zoning, df_mayor, df_council))



ggplot(df_plot, aes(x = x, y = y)) +
        geom_abline(aes(intercept = intercept_val, slope = slope_val),
            color = brewer.pal(n = 9, name = "Set1")[2], linewidth = 0.5) +
        geom_point(size = 0.5) +
        facet_wrap(facets = "case", scales = "free_y") +
        labs(x = "Zip code % urban (ex controls for gender x education x race x party ID x union membership)", y = "Satisfaction [0 = poor, 4 = excellent] (ex controls)",
            title = "Zip code urbanization and satisfaction with local government, 2016 Cooperative Election Survey",
            subtitle = 'Thinking now about your local community, how would you grade the following:')


ggsave(filename = "Text/Figures/fig_a20.pdf", width = 9, height = 6)




#%%%% Roper surveys on policy questions
df_roper <- fread("Data/_Clean/roper_surveys_policy.csv")

# functions to estimate regressions on Roper survey data
get_mods <- function(var_name) {
    base_formula <- as.formula(paste0(var_name, "~ urban_b | survey_wave"))
    control_formula <- as.formula(paste0(var_name, "~ urban_b | survey_wave + region + black + OCCUPATION1 + female"))
    r_base <- feols(base_formula, df_roper[], weights = ~ weight,
        vcov = "hetero", fixef.tol = 1e-9)
    r_control <- feols(control_formula, df_roper[], weights = ~ weight,
        vcov = "hetero", fixef.tol = 1e-9)
    return(list(r_base, r_control))
}
get_mods_unweight <- function(var_name) {
    base_formula <- as.formula(paste0(var_name, "~ urban_b | survey_wave"))
    control_formula <- as.formula(paste0(var_name, "~ urban_b | survey_wave + region + black + OCCUPATION1 + female"))
    r_base <- feols(base_formula, df_roper[],
        vcov = "hetero", fixef.tol = 1e-9)
    r_control <- feols(control_formula, df_roper[], 
        vcov = "hetero", fixef.tol = 1e-9)
    return(list(r_base, r_control))
}

get_coef_s_mod <- function(mod) {
    coef_out <- mod$coefficients["urban_b"]
    se_out <- mod$se["urban_b"]
    return(c(coef_out, se_out))
}
get_coef_mod <- function(mod) {
    coef_out <- mod$coefficients["urban_b"]
    se_out <- mod$se["urban_b"]
    coef_out <- round(coef_out, 3)
    se_out <- round(se_out, 3)
    p_val <- coeftable(mod)["urban_b", 4]
    if (p_val < 0.01) {
        coef_out <- paste0(coef_out, "^{**}")
    } else if (p_val < 0.05) {
        coef_out <- paste0(coef_out, "^{*}")
    } else if (p_val < 0.1) {
        coef_out <- paste0(coef_out, "^{\\dag}")
    }

    coef_out <- paste0("$", coef_out, "$")
    se_out <- paste0("$(", se_out, ")$")
    dv_mean_out <- mean(mod$residuals + mod$fitted.values)
    dv_mean_out <- round(dv_mean_out, 3)
    n_out <- format(mod$nobs, big.mark = ",", scientific = FALSE)
    return(c(coef_out, se_out, dv_mean_out, n_out))

}

get_frac_urban_mod <- function(mod) {
    signif(df_roper[mod$obs_selection$obsRemoved, weighted.mean(urban_b,weight, na.rm = TRUE)], 3)
}


get_coefs <- function(var_name, var_descrip) {
    mods <- get_mods(var_name)
    out_base <- get_coef_mod(mods[[1]])
    out_controls <- get_coef_mod(mods[[2]])
    frac_urban_controls <- get_frac_urban_mod(mods[[2]])
    return(c(var_descrip, out_base, out_controls, frac_urban_controls))
}

get_coefs_unweight <- function(var_name, var_descrip) {
    mods <- get_mods_unweight(var_name)
    out_base <- get_coef_mod(mods[[1]])
    out_controls <- get_coef_mod(mods[[2]])
    return(c(var_descrip, out_base, out_controls))
}



get_coef_simp <- function(var_name, var_descrip) {
    mods <- get_mods(var_name)

    base_spec <- get_coef_s_mod(mods[[1]])
    control_spec <- get_coef_s_mod(mods[[2]])
    df_out <- data.table(specification = c("Base specification", "Controls"),
        coef = c(base_spec[1], control_spec[1]),
        se = c(base_spec[2], control_spec[2]))
    df_out[, var := var_descrip]
    return(df_out)
}


get_coef_simp_unweight <- function(var_name, var_descrip) {
    mods <- get_mods_unweight(var_name)

    base_spec <- get_coef_s_mod(mods[[1]])
    control_spec <- get_coef_s_mod(mods[[2]])
    df_out <- data.table(specification = c("Base specification", "Controls"),
        coef = c(base_spec[1], control_spec[1]),
        se = c(base_spec[2], control_spec[2]))
    df_out[, var := var_descrip]
    return(df_out)
}




dvs <- c("increase_spending", "spending_too_little",
    "tax_combined",
    "nd_con", "roosevelt_more_lib",
    "lib_con_party", 
    "increase_ue_relief",
    "second_aaa",
    "favor_old_age_insurance",
    "approve_ss", "approve_ss_tax"
    )
questions <- c("Do you think government spending should be increased to help get business out of its present slump?",
        "In your opinion, is the government spending too little, too much, or the right amount for relief and recovery?",
        "About how much do you think a married man earning $3/5/10,000 a year should pay in the form of income taxes? (converted to % and averaged)",
        "If Roosevelt is not a candidate for reelection in 1940 would you prefer a conservative type of candidate, or a New Dealer",
        "variants of 'Should Roosevelt's administration be more Liberal or Conservative than it currently is' and
            'Is Roosevelt's administration too Liberal or too Conservative'",
        "If there were only two political parties in this country --- one for conservatives and one for liberals --- which would you join?",
       
        "Do you think government expenditures should be increased or decreased on the following: Unemployment relief",
        "Would you like to see the AAA (crop control act) revived?",
        "Do you favor the compulsory old age insurance plan",
        "Approve of Social Security",
        "Approve of tax for Social Security")
descrip <- c("Support increased government spending",
    "Government spending too little on relief",
    "Preferred income tax rate (\\%)",
    "Prefer New Dealer to conservative Democrat", "Want more liberal policies from Roosevelt",
    "Prefer a liberal to a conservative party", 
    "Support increased spending on unemployment relief",
    "Support revival of AAA",
    "Support old age pensions",
    "Approve of Social Security",
    "Approve of tax for Social Security")

#%% Table A-4

df_coefs <- data.table(t(sapply(1:length(dvs), function(x){get_coefs(dvs[x], descrip[x])})))
df_coefs[, c("V4", "V5") := NULL]
colnames(df_coefs) <- c("", "Coef", "S.E.", "Coef", "S.E.", "Mean", "N", "Urban share")


df_coefs_unweight <- data.table(t(sapply(1:length(dvs), function(x){get_coefs_unweight(dvs[x], descrip[x])})))

df_coefs_unweight[, c("V4", "V5") := NULL]
colnames(df_coefs_unweight) <- c("", "Coef", "S.E.", "Coef", "S.E.", "Mean", "N")
df_coefs_unweight




table_out <- kableExtra::kable(df_coefs, booktabs = TRUE, format = "latex",
    caption = "Effect of urban status on Gallup survey responses",
    label = "table_gallup", escape = FALSE, 
    linesep = c("")
    ) %>%
    add_header_above(c(" " = 1, "Base" = 2, "Controls" = 2, " " = 2)) %>%
    column_spec(1, width = c("6cm")) %>%
    kable_styling(font_size = 10) %>%
    pack_rows(index = c("Government spending and taxation" = 3, "Support for New Deal Democrats" = 3,
        "Support for specific programs" = 5)) %>%
    footnote(general = fixed("\\\\scriptsize{This table shows the results of regressions of agreement with
        Gallup poll questions on urban status. Dependent variables are coded so
        that 1 indicates agreement with the statement and 0 indicates disagreement,
        with the exception of the tax question, which is in percentage points.
        The Base specification only includes survey-wave fixed effects. The Controls
        specification includes controls for region, race, occupation, gender,
        and survey-wave. All models are restricted to non-southern respondents
        and weighted using the population weights developed by Berinsky and
        Schickler. Robust standard errors in parentheses.
        $^{**}p<0.01$; $^{*}p<0.05$; $^{\\\\dag}p<0.1$}"
    ), threeparttable = TRUE, footnote_as_chunk = TRUE, general_title = "",
        escape = FALSE)


cat(table_out, file = "Text/Tables/table_a4.tex")

#%% Figure 5

df_coefs <- rbindlist(lapply(1:length(dvs), function(x){get_coef_simp(dvs[x], descrip[x])}))
head(df_coefs)
df_coefs[, var := str_remove_all(var, fixed("\\"))]
df_coefs[, variable := forcats::fct_rev(factor(var, levels = unique(var)))]
df_coefs[, category := NULL]
df_coefs[1:6, category := "Government spending and tax"]
df_coefs[7:12, category := "New Deal Democrats"]
df_coefs[13:nrow(df_coefs), category := "Support for specific programs"]

df_coefs[, category := factor(category, levels = c(unique(category)))]
nrow(df_coefs)

ggplot(df_coefs[], aes(x = variable, y = coef, ymin = coef - 1.96 * se, ymax = coef + 1.96 * se,
    color = specification, shape = specification)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_pointrange(size = 0.5, position = position_dodge(width = -0.5)) +
    scale_color_brewer(palette = "Set1") +
    facet_grid(category ~ ., scales = "free_y", space = "free_y") +
    labs(x = element_blank(), y = "Coefficient on urban status", color = "Specification",
        shape = "Specification", title = "Relationship between urban status and non-Southern Gallup\nsurvey responses, 1936-1938") +
    theme(axis.text.y = element_text(size = 10, color = "black")) +
    coord_flip()



df_heads <- data.table(var = rep(c("<b>Government spending and tax</b>",
    "<b>New Deal Democrats</b>", "<b>Support for specific programs</b>"), each = 2),
        specification = rep(c("Base specification", "Controls"), 3))
df_heads <- rbind(df_coefs, df_heads, fill = TRUE)
df_heads[, variable := factor(var, levels = c(
        "Approve of tax for Social Security", "Approve of Social Security",
        "Support old age pensions", "Support revival of AAA",
        "Support increased spending on unemployment relief",
        "<b>Support for specific programs</b>",
        "Prefer a liberal to a conservative party", 
        "Want more liberal policies from Roosevelt",
        "Prefer New Dealer to conservative Democrat",
        "<b>New Deal Democrats</b>",
        "Preferred income tax rate (%)", "Government spending too little on relief",
        "Support increased government spending", "<b>Government spending and tax</b>")
        )]

ggplot(df_heads[], aes(x = variable, y = coef, ymin = coef - 1.96 * se, ymax = coef + 1.96 * se,
    color = specification, shape = specification)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_pointrange(size = 0.5, position = position_dodge(width = -0.5)) +
    scale_color_brewer(palette = "Set1") +
    labs(x = element_blank(), y = "\nCoefficient on urban status", color = element_blank(),
        shape = element_blank()) +
    theme(axis.text.y = element_markdown(size = 10, color = "black")) +
    coord_flip()
ggsave(file = "Text/Figures/fig_5.pdf", width = 9, height = 6)




#%% Figure A-17

df_coefs_unweight <- rbindlist(lapply(1:length(dvs), function(x){get_coef_simp_unweight(dvs[x], descrip[x])}))

df_coefs_unweight[, var := str_remove_all(var, fixed("\\"))]
df_coefs_unweight[, variable := forcats::fct_rev(factor(var, levels = unique(var)))]
df_coefs_unweight[, category := NULL]
df_coefs_unweight[1:6, category := "Government spending\nand tax"]
df_coefs_unweight[7:12, category := "New Deal Democrats"]
df_coefs_unweight[13:nrow(df_coefs), category := "Support for specific programs"]

df_coefs_unweight[, category := factor(category, levels = c(unique(category)))]
nrow(df_coefs_unweight)

ggplot(df_coefs_unweight[], aes(x = variable, y = coef, ymin = coef - 1.96 * se, ymax = coef + 1.96 * se,
    color = specification, shape = specification)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_pointrange(size = 0.5, position = position_dodge(width = -0.5)) +
    scale_color_brewer(palette = "Set1") +
    facet_grid(category ~ ., scales = "free_y", space = "free_y") +
    labs(x = element_blank(), y = "Coefficient on urban status", color = "Specification",
        shape = "Specification", title = "Relationship between urban status and non-Southern Gallup\nsurvey responses, 1936-1938, without survey weights") +
    theme(axis.text.y = element_text(size = 10, color = "black")) +
    coord_flip()
ggsave(file = "Text/Figures/fig_a17.pdf", width = 9, height = 6)


#%% Table A-6

df_roper_vote <- fread("Data/_Clean/roper_surveys_vote_choice.csv")
r1 <- feols(vote_fdr ~ urban_b  | survey_wave ,
    df_roper_vote[south_b == FALSE], weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r1)

r2 <- feols(vote_fdr ~ city_25  | survey_wave ,
    df_roper_vote[south_b == FALSE], weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r2)

r3 <- feols(vote_fdr ~ occ_recode  | survey_wave ,
    df_roper_vote[south_b == FALSE], weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r3)



r4 <- feols(vote_fdr ~ black  | survey_wave ,
    df_roper_vote[south_b == FALSE], weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r4)

r5 <- feols(vote_fdr ~ female_b  | survey_wave ,
    df_roper_vote[south_b == FALSE], weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r5)

r6 <- feols(vote_fdr ~ urban_b + occ_recode + black + female_b  | survey_wave ,
    df_roper_vote[south_b == FALSE], weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r6)

r7 <- feols(vote_fdr ~ city_25 + occ_recode + black + female_b  | survey_wave ,
    df_roper_vote[south_b == FALSE], weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r7)

r8 <- feols(vote_fdr ~ urban_b + occ_recode + black + female_b + south_b | survey_wave ,
    df_roper_vote, weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r8)

r9 <- feols(vote_fdr ~ city_25 + occ_recode + black + female_b + south_b | survey_wave ,
    df_roper_vote, weights = ~ weight, vcov = "hetero", fixef.tol = 1e-9)
summary(r9)


texreg(list(r1, r2, r3, r4, r5, r6, r7, r8, r9),
    file = "Text/Tables/table_a6.tex",
    label = "table_rev_divide",
    caption = "Urban-rural gaps in voting for Democrats in 1936 as measured in Gallup survey data",
    custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)"),
    custom.coef.map = list("urban_bTRUE" = 'Urban (2,500 cutoff)',
        "city_25TRUE" = "City (25,000 cutoff)",
        "occ_recodeOther = 'Occupation: other",
        "occ_recodeProfessional" = "Occupation:\\\\business/professional",
        "occ_recodeUnemployed" = "Occupation:\\\\unemployed",
        "blackBlack" = "Black",
        "female_bTRUE" = "Female",
        "south_bTRUE" = "South"
        ),
    custom.header = list("Voted for FDR in 1936" = 1:9),
    custom.gof.rows = list(
        "Excluding South" = c(rep("x", 7), "", "")),
    custom.gof.names = c("N", "$R^2$"),
    custom.note = paste0("\\item This table shows the results of regressions of
        whether the individual voted for Roosevelt (1) or the Republican candidate, Landon (0),
        in the 1936 presidential election, using all Gallup surveys with available
        data from the period 1936--1938. In model (1) the independent variable is
        whether the respondent is living in an urban area, defined as having
        more than 2,500 residents, in (2), whether the respondent lives in a
        town of more than 25,000 residents, in (3) the respondent's occupation---the
        base category is labor, in (4) whether the respondent is classified
        as Black, in (5), female. (6)--(9) include all these predictors. (1)--(7)
        exclude respondents from the South, (8) and (9) include them, and include the
        South as an additional predictor. Observations are weighted following Berinsky and
        Schickler's to approximate the total population. Robust standard errors in parentheses.",
        " $^{\\dag}p<0.1; \\: ^{*}p<0.05 ; \\:^{**} p<0.01$"),    
    stars = c(0.01, 0.05),
    symbol = "\\dag",
    no.margin = TRUE,
    digits = 3,
    booktabs = T,
    threeparttable = T,
    fontsize = "footnotesize",
    use.packages = F,
    include.nobs = TRUE,
    include.groups = FALSE,
    include.rsquared = TRUE,
    include.adjrs = FALSE,
    include.proj.stats = FALSE,
    include.deviance = FALSE,
    include.loglik = FALSE,
    include.pseudors = FALSE
    )

#%% Figure A-8
df_agg <- fread("Data/_Clean/roper_state_urban_totals.csv")
ggplot(df_agg[], aes(x = census_urban * 100, y = sample_urban * 100, size = weights / 1000, label = state, color = southern)) +
    geom_point(alpha = 0.5) +
    geom_abline() +
    geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
    scale_color_brewer(palette = "Set1") +
    geom_text_repel(aes(size = NULL, color = NULL), family = "Univers", show.legend = FALSE, size = 3, max.overlaps = 5, seed = 123) +
    labs(x = "Urban (%), 1940 census", y = "Urban (%), Gallup data 1936-1938", size = "Sample size (x1000)",
        color = element_blank(), title = "Comparing urbanization of Gallup sample to 1940 census")
ggsave(filename = "Text/Figures/fig_a8.pdf", width = 6, height = 4)

