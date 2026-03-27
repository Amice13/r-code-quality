# note --------------------------------------------------------------------

# this is one of the two replication scripts for:
#
# umit, r., & schaffer, l. m. (2020). attitudes towards carbon taxes across
# europe: the role of perceived uncertainty and self-interest. energy policy,
# 140, 1-7.
#
# the other replication script is called 01_analysis_stata.do, which needs
# to be run in stata first.
#
# running this script requires eight third-party packages, which can be 
# installed by removing the leading hashes from the lines 21 to 28 below.
#
#
# resul umit, 19 july 2022


# install the required libraries if they are not already installed --------

# if (!require("broom")) install.packages("broom")
# if (!require("countrycode")) install.packages("countrycode")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("gridExtra")) install.packages("gridExtra")
# if (!require("rio")) install.packages("rio")
# if (!require("survey")) install.packages("survey")
# if (!require("viridis")) install.packages("viridis")


# load the required libraries ---------------------------------------------

library(broom)
library(countrycode)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ordinal)
library(rio)
library(survey)
library(viridis)


# solve conflicts on select functions -------------------------------------

select <- dplyr::select
recode <- dplyr::recode


# write a new function for significance stars -----------------------------

star_function <- function(x) {
  symnum(
    x,
    corr = FALSE,
    na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 1),
    symbols = c("***", "**", "*", " ")
  )
}


# import datasets ---------------------------------------------------------

# main dataset, previously downloaded from europeansocialsurvey.org
df_ess <- import("data_ess.dta")

# contextual data
df_context <- import("03_data_context.dta")

# predicted probabilities, linear, previously estimated in 01_analysis_stata.do
df_pl_trust <- read.csv("predictions_linear_trust.csv", skip = 1)
df_pl_efficacy <-
  read.csv("predictions_linear_efficacy.csv", skip = 1)
df_pl_dependence <-
  read.csv("predictions_linear_dependence.csv", skip = 1)
df_pl_rural <- read.csv("predictions_linear_rural.csv", skip = 1)

# predicted probabilities, ordinal, previously estimated in 01_analysis_stata.do
df_po_trust <- read.csv("predictions_ordinal_trust.csv", skip = 1)
df_po_efficacy <-
  read.csv("predictions_ordinal_efficacy.csv", skip = 1)
df_po_dependence <-
  read.csv("predictions_ordinal_dependence.csv", skip = 1)
df_po_rural <- read.csv("predictions_ordinal_rural.csv", skip = 1)


# figure 1 ----------------------------------------------------------------

# prepare dataframe
df_ess1 <- df_ess %>%
  select(cntry, inctxff, pspwght, pweight)

# specify designs
overall_design <-
  svydesign(id = ~ 1 ,
            data = df_ess1,
            weight = ~ pspwght * pweight)
country_design <-
  svydesign(id = ~ 1 ,
            data = df_ess1,
            weight = ~ pspwght)

# calculate and print the mean values of support
overall_mean <-
  svymean(~ inctxff, na.rm = TRUE, design = overall_design)
country_means <-
  svyby(
    ~ inctxff,
    na.rm = TRUE,
    by = ~ cntry,
    design = country_design,
    FUN = svymean
  )

# calculate number of observations
n_observations <- df_ess %>%
  group_by(cntry) %>%
  summarise(n = n())

# get variables from the context dataframe
context_vars <- df_context %>%
  select(cntry, country_name, carbon_tax)

# merge into a new dataframe
df_means <-
  left_join(country_means, n_observations, by = "cntry") %>%
  left_join(., context_vars, by = "cntry") %>%
  select(cntry, country_name, n, inctxff, se, carbon_tax)

# plot
df_means %>%
  ggplot(aes(
    x = reorder(cntry, inctxff),
    y = inctxff,
    colour = as.factor(carbon_tax),
    group = as.factor(carbon_tax)
  )) +
  geom_errorbar(aes(ymin = inctxff - se, ymax = inctxff + se),
                width = .1,
                size = 1.5) +
  geom_point(size = 3) +
  geom_hline(
    yintercept = 3,
    linetype = "dashed",
    colour = "darkgray",
    size = 0.8
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  labs(x = "", y = "\nAttitude towards increasing taxes on fossil fuels") +
  scale_colour_viridis(
    name = "Carbon tax",
    limits = c("1", "0"),
    breaks = c("1", "0"),
    labels = c("with", "without"),
    discrete = TRUE
  ) +
  scale_y_continuous(
    limits = c(1.8, 4.2),
    breaks = c(2, 3, 4),
    labels = c(
      "Somewhat\nagainst",
      "Neither in favour\nnor against",
      "Somewhat\nin favour"
    )
  )


# figure 2 ----------------------------------------------------------------

# prepare dataframe
df_ess2 <- df_ess %>%
  select(cntry,
         inctxff,
         trstpol,
         psppsgva,
         cflsenr,
         domicil,
         pspwght,
         pweight)

# specify design
bycountry_design <-
  svydesign(id = ~ 1 ,
            data = df_ess2,
            weight = ~ pspwght)

# estimate mean values
inctxff_means <-
  svyby(
    ~ inctxff,
    na.rm = TRUE,
    by = ~ cntry,
    design = bycountry_design,
    FUN = svymean
  ) %>%
  select(-se)

trstpol_means <-
  svyby(
    ~ trstpol,
    na.rm = TRUE,
    by = ~ cntry,
    design = bycountry_design,
    FUN = svymean
  ) %>%
  select(-se) %>%
  mutate(iv_name = "trstpol") %>%
  rename(iv = "trstpol")

psppsgva_means <-
  svyby(
    ~ psppsgva,
    na.rm = TRUE,
    by = ~ cntry,
    design = bycountry_design,
    FUN = svymean
  ) %>%
  select(-se) %>%
  mutate(iv_name = "psppsgva") %>%
  rename(iv = "psppsgva")

cflsenr_means <-
  svyby(
    ~ cflsenr,
    na.rm = TRUE,
    by = ~ cntry,
    design = bycountry_design,
    FUN = svymean
  ) %>%
  select(-se) %>%
  mutate(iv_name = "cflsenr") %>%
  rename(iv = "cflsenr")

domicil_means <-
  svyby(
    ~ domicil,
    na.rm = TRUE,
    by = ~ cntry,
    design = bycountry_design,
    FUN = svymean
  ) %>%
  select(-se) %>%
  mutate(iv_name = "domicil") %>%
  rename(iv = "domicil")

# merge all mean values
df_iv_means <-
  rbind(trstpol_means, psppsgva_means, cflsenr_means, domicil_means) %>%
  left_join(., inctxff_means, by = "cntry") %>%
  mutate(
    iv_name = factor(iv_name, levels = c(
      "trstpol", "psppsgva", "cflsenr", "domicil"
    )),
    iv_name = recode(
      iv_name,
      "trstpol" = "Political Trust",
      "psppsgva" = "Political Efficacy",
      "cflsenr" = "Energy Dependence",
      "domicil" = "Rural Area"
    )
  )

# plot components
p1 <-
  ggplot(subset(df_iv_means, iv_name == "Political Trust"),
         aes(x = iv, y = inctxff)) +
  geom_text(aes(label = cntry), size = 3) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  labs(y = "", x = "\nPolitical Trust") +
  scale_x_continuous(
    limits = c(2, 6),
    breaks = c(2, 4, 6),
    labels = c(2, 4, 6)
  )

p2 <-
  ggplot(subset(df_iv_means, iv_name == "Political Efficacy"),
         aes(x = iv, y = inctxff)) +
  geom_text(aes(label = cntry), size = 3) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  labs(y = "", x = "\nPolitical Efficacy") +
  scale_x_continuous(
    limits = c(1.5, 3.5),
    breaks = c(1, 2, 3, 4),
    labels = c(1, 2, 3, 4)
  )

p3 <-
  ggplot(subset(df_iv_means, iv_name == "Energy Dependence"),
         aes(x = iv, y = inctxff)) +
  geom_text(aes(label = cntry), size = 3) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  labs(y = "", x = "\nEnergy Dependence") +
  scale_x_continuous(
    limits = c(2, 6),
    breaks = c(2, 4, 6),
    labels = c(2, 4, 6)
  )

p4 <-
  ggplot(subset(df_iv_means, iv_name == "Rural Area"),
         aes(x = iv, y = inctxff)) +
  geom_text(aes(label = cntry), size = 3) +
  theme_bw() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  labs(y = "", x = "\nRural Area") +
  scale_x_continuous(
    limits = c(1.5, 3.5),
    breaks = c(1, 2, 3, 4),
    labels = c(1, 2, 3, 4)
  )

# put the components together
grid.arrange(
  p1,
  p2,
  p3,
  p4,
  nrow = 2,
  left = textGrob(
    "Attitude towards increasing taxes on fossil fuels",
    rot = 90,
    vjust = 1,
    gp = gpar(fontsize = 12)
  )
)


# figure 3 ----------------------------------------------------------------

# bind the relavant predictions together
rbind(
  # political trust
  df_pl_trust %>% filter(X %in% c("1._at", "11._at")) %>%
    mutate(item = "Political\nTrust",
           value = c("Minimum", "Maximum")),
  
  # political efficacy
  df_pl_efficacy %>% filter(X %in% c("1._at", "5._at")) %>%
    mutate(item = "Political\nEfficacy",
           value = c("Minimum", "Maximum")),
  
  # energy dependence
  df_pl_dependence %>% filter(X %in% c("1._at", "11._at")) %>%
    mutate(item = "Energy\nDependence",
           value = c("Minimum", "Maximum")),
  
  # rural area
  df_pl_rural %>% filter(X %in% c("1._at", "5._at")) %>%
    mutate(item = "Rural\nArea",
           value = c("Minimum", "Maximum"))
) %>%
  # tidy the dataframe
  mutate(item = factor(
    item,
    levels = c(
      "Rural\nArea",
      "Energy\nDependence",
      "Political\nEfficacy",
      "Political\nTrust"
    )
  )) %>%
  # plot
  ggplot(aes(item, y = b, colour = value)) +
  geom_pointrange(aes(ymin = min95 , ymax = max95),
                  size = 0.6) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  labs(x = "", y = "\nPredicted support for\nincreasing taxes on fossil fuels") +
  scale_colour_viridis(name = "Independent\nvariable at ...",
                       discrete = TRUE)


# table s2 ----------------------------------------------------------------

# create the line for total
total_line <-
  list(
    "Total",
    sum(n_observations$n),
    as.data.frame(svymean(
      ~ inctxff, na.rm = TRUE, design = overall_design
    ))[, 1],
    as.data.frame(svymean(
      ~ inctxff, na.rm = TRUE, design = overall_design
    ))[, 2]
  )

# improve the look and table
df_means %>%
  mutate(Country = paste0(country_name, " (", cntry, ")")) %>%
  select(Country,
         Sample = n,
         Mean = inctxff,
         SE = se) %>%
  arrange(Country) %>%
  rbind(total_line)


# figure s1 ---------------------------------------------------------------

# political trust
p5 <- df_po_trust %>%
  filter(X != "N") %>%
  mutate(at = 0:10) %>%
  ggplot(aes(x = at)) +
  geom_line(aes(y = b), size = 1) +
  geom_line(aes(y = max95), linetype = 2, size = 1) +
  geom_line(aes(y = min95), linetype = 2, size = 1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  ) +
  labs(x = "\nPolitical Trust",
       y = "Predicted probability of support\nfor carbon taxes\n") +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  ) +
  scale_y_continuous(
    limits = c(0.2, 0.52),
    breaks = c(0.2, 0.3, 0.4, 0.5),
    labels = c(0.2, 0.3, 0.4, 0.5)
  )

# political efficacy
p6 <- df_po_efficacy %>%
  filter(X != "N") %>%
  mutate(at = 1:5) %>%
  ggplot(aes(x = at)) +
  geom_line(aes(y = b), size = 1) +
  geom_line(aes(y = max95), linetype = 2, size = 1) +
  geom_line(aes(y = min95), linetype = 2, size = 1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  ) +
  labs(x = "\nPolitical Efficacy",
       y = "Predicted probability of support\nfor carbon taxes\n") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c(1, 2, 3, 4, 5)) +
  scale_y_continuous(
    limits = c(0.2, 0.52),
    breaks = c(0.2, 0.3, 0.4, 0.5),
    labels = c(0.2, 0.3, 0.4, 0.5)
  )

# energy dependence
p7 <- df_po_dependence %>%
  filter(X != "N") %>%
  mutate(at = 0:10) %>%
  ggplot(aes(x = at)) +
  geom_line(aes(y = b), size = 1) +
  geom_line(aes(y = max95), linetype = 2, size = 1) +
  geom_line(aes(y = min95), linetype = 2, size = 1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  ) +
  labs(x = "\nEnergy Dependence",
       y = "Predicted probability of support\nfor carbon taxes\n") +
  scale_x_continuous(
    breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  ) +
  scale_y_continuous(
    limits = c(0.2, 0.52),
    breaks = c(0.2, 0.3, 0.4, 0.5),
    labels = c(0.2, 0.3, 0.4, 0.5)
  )

# rural area
p8 <- df_po_rural %>%
  filter(X != "N") %>%
  mutate(at = 1:5) %>%
  ggplot(aes(x = at)) +
  geom_line(aes(y = b), size = 1) +
  geom_line(aes(y = max95), linetype = 2, size = 1) +
  geom_line(aes(y = min95), linetype = 2, size = 1) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16)
  ) +
  labs(x = "\nRural Area",
       y = "Predicted probability of support\nfor carbon taxes\n") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c(1, 2, 3, 4, 5)) +
  scale_y_continuous(
    limits = c(0.2, 0.52),
    breaks = c(0.2, 0.3, 0.4, 0.5),
    labels = c(0.2, 0.3, 0.4, 0.5)
  )

# put the components together
grid.arrange(p5,
             p6,
             p7,
             p8,
             nrow = 2)


# table s7 ----------------------------------------------------------------

# create a list of all countries
list_countries <-
  df_ess %>% select(cntry) %>% unique() %>% as.list() %>% unlist()

# write a loop to estimate results by country
list_results <- list()

for (i in list_countries) {
  models <-
    tidy(clm(
      as.factor(inctxff) ~ trstpol + psppsgva + cflsenr + domicil,
      data = subset(df_ess, cntry == i)
    ))
  
  trstpol_est <-
    round(models[models$term == "trstpol",]$estimate, 3)
  trstpol_se <-
    round(models[models$term == "trstpol",]$std.error, 3)
  trstpol_st <- models[models$term == "trstpol",]$p.value
  
  psppsgva_est <-
    round(models[models$term == "psppsgva",]$estimate, 3)
  psppsgva_se <-
    round(models[models$term == "psppsgva",]$std.error, 3)
  psppsgva_st <- models[models$term == "psppsgva",]$p.value
  
  cflsenr_est <-
    round(models[models$term == "cflsenr",]$estimate, 3)
  cflsenr_se <-
    round(models[models$term == "cflsenr",]$std.error, 3)
  cflsenr_st <- models[models$term == "cflsenr",]$p.value
  
  domicil_est <-
    round(models[models$term == "domicil",]$estimate, 3)
  domicil_se <-
    round(models[models$term == "domicil",]$std.error, 3)
  domicil_st <- models[models$term == "domicil",]$p.value
  
  df_cntrys <-
    data.frame(
      i,
      trstpol_est,
      trstpol_se,
      trstpol_st,
      psppsgva_est,
      psppsgva_se,
      psppsgva_st,
      cflsenr_est,
      cflsenr_se,
      cflsenr_st,
      domicil_est,
      domicil_se,
      domicil_st,
      stringsAsFactors = FALSE
    )
  list_results[[i]] <- df_cntrys
}

# bind the results together
do.call(rbind, list_results) %>%
  mutate(
    country = countrycode(i, "iso2c", "country.name"),
    trstpol_est = paste0(trstpol_est, as.character(star_function(trstpol_st))),
    psppsgva_est = paste0(psppsgva_est, as.character(star_function(psppsgva_st))),
    cflsenr_est = paste0(cflsenr_est, as.character(star_function(cflsenr_st))),
    domicil_est = paste0(domicil_est, as.character(star_function(domicil_st)))
  ) %>%
  select(
    "Country" = country,
    "PTrust_Est" = trstpol_est,
    "PTrust_SE" = trstpol_se,
    "PEfficacy_Est" = psppsgva_est,
    "PEfficacy_SE" = psppsgva_se,
    "EDependence_Est" = cflsenr_est,
    "EDependence_SE" = cflsenr_se,
    "RArea_Est" = domicil_est,
    "RArea_SE" = domicil_se,
  ) %>%
  arrange(Country) %>%
  `rownames<-`(NULL)