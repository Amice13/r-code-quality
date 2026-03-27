## -----------------------------------------------------------------------------
# NOTE: `replication.r` is the R code extracted from `replication.qmd`---it replicates all results but does not save figures in the /figs/ folder as required to render the manuscript.
# If you are not working in an RStudio project, replication.qmd may still render, 
# but replication.r will likely require a line pointing to the file path where you saved the dataverse files 
# (e.g., `here::i_am(path = "Downloads/dataverse_files")` )

testing = F
verification = T
start_time <- Sys.time()

# directory to store model objects 
if (!dir.exists(here::here("models"))) {dir.create(here::here("models"))}

# directory to store figures
if (!dir.exists(here::here("figs"))) {dir.create(here::here("figs"))}




## ----message=FALSE------------------------------------------------------------
#| label: packages

# DUE TO BREAKING CHANGES, THIS CODE REQUIRES AN PRIOR VERSION OF MARGINALEFFECTS
if(packageVersion("marginaleffects") != "0.25.1"){
remove.packages("marginaleffects")
packageurl <- "http://cran.r-project.org/src/contrib/Archive/marginaleffects/marginaleffects_0.25.1.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
}
# as described in the warning below, the uncertainty intervals around predictions do not include uncertainty in the estimates of the fixed effects. Since we are just using predictions to illustrate the model results, not make any claims about particular years, legislators, or agencies (the fixed effects), the uncertainty in the fixed effect estimates is not relevant for our use case (illustrating main effects, for which we do have estimates of uncertainty). However, we wish to alert users of these code that they should not use the predictions to make such claims. 

requires <- c("tidyverse", "magrittr", # data wrangling
              "scales", # plot scales 
              "ggrepel",# plot labels
              "here", # file paths 
              "knitr", # document formatting 
              "kableExtra", # for table formatting 
              "modelsummary", # for regression tables 
              "marginaleffects", # for predictions 
              "fixest", # statistical package for fixed effects estimation 
              "ineq", # to calculate GINI coefficients
              # MAY BE REQUIRED FOR QMD RENDERING
              "base64enc",
              "digest",
              "evaluate",
              "glue",
              "highr",
              "htmltools",
              "jsonlite",
              "markdown",
              "mime",
              "rmarkdown",
              "stringi",
              "stringr",
              "xfun",
              "yaml")

to_install <- c(requires %in% rownames(installed.packages()) == FALSE)

if(sum(to_install>0)){
install.packages( requires[to_install], 
                  repos = "https://cloud.r-project.org/" )
}

library(modelsummary)
library(marginaleffects)
library(fixest)
library(tidyverse)
library(magrittr)
library(ineq)
library(kableExtra)
library(ggrepel)
library(scales)




## ----html-table-formatting----------------------------------------------------
# html table formatting
kablebox <- . %>%  
  head(100) %>%
  knitr::kable() %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::scroll_box(height = "200px")

kablebox_long <- . %>% 
  head(100) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::scroll_box(height = "500px")


## -----------------------------------------------------------------------------
#| label: 2x2-fig
#| fig-width: 3.6
#| fig-height: 3.6
#| fig-show: hold
#| layout-ncol: 2
#| out-width: "50%"


# CAPACITY
capacity <- tibble::tibble(
  priority = .2,
  capacity = c(100, NA, NA, NA, NA, 150)
  ) %>% 
  ggplot() +
  aes(x = capacity*priority,
      y = capacity*(1-priority) ,
      label = str_c(capacity*priority, ", ", capacity*(1-priority))) + 
   geom_segment(x = c(0,0,0,0,0,0),
                 xend = c(100, 110, 120,130,140, 150),
                 y = c(100, 110, 120,130,140, 150),
                 yend = c(0,0,0,0,0,0),
              linetype = 1,
              alpha = c(.1,.2,.4,.6,.8,1)) + 
    geom_point(alpha = c(.5, 1,1,1,1,1) ) +
  geom_segment(x = 100*.2, 
           y = 100*.8,
           xend = 148*.2, 
           yend = 148*.8,
           arrow = arrow(type = "closed", length = unit(.1, "in")),
           color = "blue") + 
  annotate("text", 
          label = "50% increase in capacity, maintaining\na 80:20 ratio of constituency service to\npolicy work (80% constituency service)", 
           x = 150*.2, 
           y = 150*.8,
          hjust = -.01,
          vjust = -.1,
          color = "black",
          size = 3) +
  geom_label(x = 0, 
             y = c(100, 110, 120,130,140, 150), 
             label = c("1x", NA, NA, NA, NA, "1.5x"),
             size = 3,
             hjust = .44 ) + 
    scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  scale_x_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  labs(#title = "",
       x = "Volume of policy work",
       y = "Volume of constituency service") + 
  theme(legend.position = "", 
        panel.grid.minor = element_blank() )




# PRIORITY 
priority <- tibble::tibble(
  priority = c(.2, .4),
  capacity = c(100) 
  ) %>% 
  mutate(
    x = capacity*priority,
      y = capacity*(1-priority)
  ) %>% 
  ggplot() +
  aes(x = x,
      y = y ) + 
        geom_segment(x = c(0),
                 xend = c(100),
                 y = c(100),
                 yend = c(0),
              linetype = 1,
              alpha = c(.1)) +
   stat_function(fun =  ~ 4*.x , geom = "line", linetype = 2, alpha = .1) +
     stat_function(fun =  ~ 3.5*.x , geom = "line", linetype = 2, alpha = .2) +
   stat_function(fun =  ~ 3*.x , geom = "line", linetype = 2, alpha = .4) +
   stat_function(fun =  ~ 2.5*.x , geom = "line", linetype = 2, alpha = .6) +
   stat_function(fun =  ~ 2*.x , geom = "line", linetype = 2, alpha = .8) +
   stat_function(fun =  ~ 1.5*.x , geom = "line", linetype = 2, alpha = 1) +
  geom_point(alpha = c(.5,1) ) +
  geom_segment(x = 100*.2, 
           y = 100*.8,
           xend = 100*.4, 
           yend = 100*.6,
           arrow = arrow(type = "closed", length = unit(.1, "in")),
           color = "blue") + 
  annotate("text", 
          label = "Shifting priority toward policy work\nwithout increasing capacity", 
           x = 100*.4, 
           y = 100*.6,
          hjust = -.01,
          vjust = 1.1,
          color = "black",
          size = 3) +
  geom_label(aes(x = 150*c(.24, .66),
                 y = 150 ),
             label = c("80:20", "60:40"),
          size = 3) + 
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  scale_x_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  #scale_color_gradient(low = "grey", high = "black") +
  labs(#title = "",
       x = "Volume of policy work",
       y = "Volume of constituency service") +
  theme(legend.position = "",         
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )



# BOTH  
both <- tibble::tibble(
  priority = c(.2, .4),
  capacity = c(100, 150) 
  ) %>% 
  mutate(
    x = capacity*priority,
      y = capacity*(1-priority)
  ) %>% 
  ggplot() +
  aes(x = x,
      y = y ) + 
      geom_segment(x = c(0, 0),
                 xend = c(100,150),
                 y = c(100, 150),
                 yend = c(0,0),
              linetype = 1,
              alpha = c(.1, 1)) +
   stat_function(fun =  ~ 4*.x , geom = "line", linetype = 2, alpha = .1) +
   stat_function(fun =  ~ 1.5*.x , geom = "line", linetype = 2, alpha = 1) +
  geom_label(aes(x = 150*c(.24, .66),
                 y = 150 ),
             label = c("80:20", "60:40"),
          size = 3) + 
    geom_point(alpha = c(.5,1) ) +
  geom_segment(x = 100*.2, 
           y = 100*.8,
           xend = 148*.4, 
           yend = 149*.6,
           arrow = arrow(type = "closed", length = unit(.1, "in")),
           color = "blue") + 
  annotate("text", 
          label = "Shifting priority toward policy work\n AND increasing capacity by 50%", 
           x = 150*.4, 
           y = 150*.6,
          hjust = .06,
          vjust = -.5,
          color = "black",
          size = 3) +
    geom_label(x = 0, y = c(100, 150), 
             label = c("1x", "1.5x"),
             size = 3,hjust = .44) + 
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  scale_x_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  #scale_color_gradient(low = "grey", high = "black") +
  labs(#title = "",
       x = "Volume of policy work",
       y = "Volume of constituency service") + 
  theme(legend.position = "",          
        panel.grid.minor = element_blank() )



# Regions   
more_c <- tibble(x = c(0, 20, 37.5,0),
       y = c(100, 80, 150,150) ) %>% 
  mutate(label = "A")

less_c <- tibble(x = c(0, 20, 0,0),
       y = c(0, 80, 80, 0) ) %>% 
  mutate(label = "D")


more_p <- tibble(x = c(19.8, 37.5, 150, 150),
       y = c(79, 150, 150, 79) ) %>% 
  mutate(label = "B")

less_p <- tibble(x = c(100, 21, 150, 150),
       y = c(0, 79, 79, 0) ) %>% 
  mutate(label = "C")


# REGIONS 
regions <- more_p %>%
 full_join( less_p) %>% #full_join(more_c) %>% 
  group_by(label) %>% 
  mutate(mean_x = mean(x),
         mean_y = mean(y),
         name = label %>% 
           str_replace("B", #"c_{i1}p_{i1} > c_{i2}p_{i2}" #FIXME 
                       "The effect of increasing capacity is large\nenough that levels of constituency service\nare maintained or increase, even as\nelected officials prioritize policy work."
                       ) %>% 
           str_replace("C", "Shifting priorities toward policy\ncauses less constituency service"
           #expression("$c_{i1}p_{i1} > c_{i2}p_{i2}$" #FIXME 
           )
         ) %>% 
  ungroup() %>% 
ggplot() +
  aes(x = x,
      y = y,
      fill = label,
      label = name) + 
  geom_polygon( alpha = .5) + 
    geom_segment(x = 0,
                 xend = 100,
                 y = 100,
                 yend = 0,
              linetype = 1,
              alpha = c(.01)) +
     stat_function(fun =  ~ 4*.x , geom = "line", linetype = 2, alpha = .1) +
  geom_point(x = 20, y = 80, alpha = .5, color = "grey") + 
  scale_fill_grey() +
  scale_y_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  scale_x_continuous(breaks = seq(0, 140, by = 20), limits = c(0, 150)) + 
  geom_text(aes(x = mean_x, y = mean_y), 
            check_overlap = T, 
            color = "black",
            #parse = T,
          size = 3,
          vjust =.5) + 
  labs(#title = "",
       x = "Volume of policy work",
       y = "Volume of constituency service") + 
  theme(legend.position = "",          
        panel.grid.minor = element_blank() )


capacity  
priority 
both  
regions


## ----data---------------------------------------------------------------------
load(here::here("data", "corr_counts.Rdata"))
## just in case it was saved as grouped data 
corr_counts %<>% ungroup()

# subset to 2007-2020
corr_counts  %<>% filter(year > 2006, year < 2021)

# New member data 
load(here::here("data", "member_data.Rdata"))

member_data <- member_data |> 
  mutate(
    member = bioname |>
      str_remove(", .*") |>
      str_to_title() |> 
      str_replace("cc", "cC"),
    member_state = paste(member, state_abbrev, sep = " (") |>
      paste0(")"),
    cqlabel = paste0("(", 
                          state_abbrev, 
                          "-", 
                          district_code, 
                          ")") |> 
           str_remove("-0")
    ) 

d <- corr_counts %>% 
  ungroup() %>% 
  mutate(Type = case_when(
    TYPE %in% c(1,2,3) ~ "Constituency\nService", 
    TYPE %in% c(4,5) ~ "Policy"), 
         type = case_when(
            TYPE == 1 ~ "Constituent (individual)",
            TYPE == 2 ~ "Constituent (corporate)",
            TYPE == 3 ~ "Constituent (501c3 or local\n government)",
            TYPE == 4 ~ "Policy (corporate)",
            TYPE == 5 ~ "Policy (general)"
         )) 

type <- d %>% 
  group_by(Type) %>% 
  summarise(n = sum(per_icpsr_chamber_year_agency_type)) %>% 
  ungroup()

percent <- function(x){
  y <- x*100 
  y %<>% round() 
  return(y)}

year_congress<- function(year){
  return(floor((year - 1787)/2))
}

d %<>% mutate(congress = year_congress(year))



dcounts <- d |> left_join(member_data)


## -----------------------------------------------------------------------------
#| label: data_by_type
#| fig-width: 5.5
#| fig-height: 3.5

# paper figure 
type_totals <- d %>% 
  tidyr::drop_na(Type) %>%  
  mutate(total = sum(per_icpsr_chamber_year_agency_type)) %>% 
  group_by(total, Type, type) %>% 
  summarise(nT =  sum(per_icpsr_chamber_year_agency_type)) %>% 
  mutate(percent = paste0(nT, " = ", 100*round(nT/total, 2), "%"),
         type = str_c(type, "\n    ", percent)) %>% 
  ungroup() %>% 
  group_by(Type) %>% 
  mutate(nt = sum(nT),
         percent_t = paste0(100*round(nt/total, 2), "%"))

type_totals %<>% mutate(type = type %>% 
                          # FOR AJPS - remove N and percent 
                          str_remove_all("\n    .*") %>% 
                        # END FOR AJPS 
                          str_replace_all("\\)", "") %>% 
                          str_replace_all(" \\(", ",\n") 
                          
                        ) %>% 
  ungroup()

N <- type_totals$total |> unique()


# horizontal
type_totals %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(alpha = .7,
           aes(x = type, fill = Type, y = nT) ) + 
  scale_y_continuous(breaks = seq.int(0,500000, by = 100000), 
                     limits = c(0,400000), 
                     labels = scales::label_comma() ) + 
  labs(x = "",
       fill = "",
       y = paste("Number of contacts"),
       title = "") +
  #geom_text(aes(label = Type, x = Type,y = nT) ) + 
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.x.top = element_text(),
        legend.position ="none") + 
  ggpubr::geom_bracket(label.size = 3, # FOR AJPS
    xmin = type_totals$type[1], 
    xmax = type_totals$type[3], 
    y.position = 380000,
    label = paste(type_totals$percent_t[1], "Constituency service" ),
    #hjust = 1,
    vjust = -1
  ) + 
  ggpubr::geom_bracket(label.size = 3, # FOR AJPS
    xmin = type_totals$type[4], xmax = type_totals$type[5], 
    y.position = 85000,
    label = paste(type_totals$percent_t[5], "Policy work"),
    #hjust = 1,
    vjust = -1
  )+ 
  scale_fill_viridis_d(option = "cividis", end = .8, direction = -1)


## ----echo=verification, eval=verification-------------------------------------
type_totals |> select(type, percent, percent_t)


## -----------------------------------------------------------------------------
#| label: data_by_type_chair
#| fig-width: 5.5
#| fig-height: 3.5

# extra figure 
type_totals <- d %>% 
  tidyr::drop_na(Type) %>%  
  left_join(member_data |> distinct(icpsr, congress, chair)) %>% 
  group_by(chair) %>% 
  mutate(total = sum(per_icpsr_chamber_year_agency_type)) %>% 
  group_by(total, Type, type, chair) %>% 
  summarise(nT =  sum(per_icpsr_chamber_year_agency_type)) %>% 
  mutate(percent = paste0(nT, " = ", 100*round(nT/total, 2), "%"),
         type = str_c(type, "\n    ", percent)) %>% 
  ungroup() %>% 
  group_by(Type, chair) %>% 
  mutate(nt = sum(nT),
         percent_t = paste0(100*round(nt/total, 2), "%"))

type_totals %<>% mutate(type = type %>% 
                          # FOR AJPS - remove N and percent 
                          str_remove_all("\n    .*") %>% 
                        # END FOR AJPS 
                          str_replace_all("\\)", "") %>% 
                          str_replace_all(" \\(", ",\n") 
                          
                        ) %>% 
  ungroup()


# chair
type_totals %>% 
  ungroup() %>% 
  filter(chair == 1) %>% 
  ggplot() + 
  geom_col(alpha = .7,
           aes(x = type, fill = Type, y = nT) ) + 
  scale_y_continuous(breaks = seq.int(0,500000, by = 100000), 
                     limits = c(0,400000), 
                     labels = scales::label_comma() ) + 
  labs(x = "",
       fill = "",
       y = paste("Number of contacts"),
       title = "Chairs only") +
  #geom_text(aes(label = Type, x = Type,y = nT) ) + 
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.x.top = element_text(),
        legend.position ="none") + 
  ggpubr::geom_bracket(label.size = 3, # FOR AJPS
    xmin = type_totals$type[1], 
    xmax = type_totals$type[3], 
    y.position = 380000,
    label = paste(type_totals$percent_t[1], "Constituency service" ),
    #hjust = 1,
    vjust = -1
  ) + 
  ggpubr::geom_bracket(label.size = 3, # FOR AJPS
    xmin = type_totals$type[4], xmax = type_totals$type[5], 
    y.position = 85000,
    label = paste(type_totals$percent_t[5], "Policy work"),
    #hjust = 1,
    vjust = -1
  )+ 
  scale_fill_viridis_d(option = "cividis", end = .8, direction = -1) 



# non-chair 
type_totals %>% 
  ungroup() %>% 
  filter(chair == 0) %>% 
  ggplot() + 
  geom_col(alpha = .7,
           aes(x = type, fill = Type, y = nT) ) + 
  scale_y_continuous(breaks = seq.int(0,500000, by = 100000), 
                     limits = c(0,400000), 
                     labels = scales::label_comma() ) + 
  labs(x = "",
       fill = "",
       y = paste("Number of contacts"),
       title = "Non-chairs only") +
  #geom_text(aes(label = Type, x = Type,y = nT) ) + 
  theme(panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.x.top = element_text(),
        legend.position ="none") + 
  ggpubr::geom_bracket(label.size = 3, # FOR AJPS
    xmin = type_totals$type[1], 
    xmax = type_totals$type[3], 
    y.position = 380000,
    label = paste(type_totals$percent_t[6], "Constituency service" ),
    #hjust = 1,
    vjust = -1
  ) + 
  ggpubr::geom_bracket(label.size = 3, # FOR AJPS
    xmin = type_totals$type[4], xmax = type_totals$type[5], 
    y.position = 85000,
    label = paste(type_totals$percent_t[9], "Policy work"),
    #hjust = 1,
    vjust = -1
  )+ 
  scale_fill_viridis_d(option = "cividis", end = .8, direction = -1) 


## ----echo=verification, eval=verification-------------------------------------
type_totals |> distinct(chair, Type, percent_t)


## -----------------------------------------------------------------------------
#| label: percentiles
#| fig-height: 3.4
#| fig-width: 6
#| cache: false

percentiles <- dcounts %>% 
  dplyr::filter(year > 2006, year < 2021) %>% 
  dplyr::group_by(member_state, chamber, year) %>% 
  dplyr::summarise(n = sum(per_icpsr_chamber_year_agency_type)) %>%
  dplyr::group_by(member_state, chamber) %>% 
  dplyr::summarise(mean = mean(n)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(chamber) %>% 
  dplyr::mutate(Percentile = dplyr::ntile(mean, 100),
         rank = dplyr::dense_rank(-mean)) %>% 
  dplyr::arrange(-Percentile) %>% 
  dplyr::select(member_state, Percentile, mean, chamber, rank) %>% 
  dplyr::distinct() 


percentiles |>
  arrange(rank) |>
  kablebox()



# percentiles %>%
#     ggplot() +
#     geom_col(aes(x = Percentile, y = mean),
#              color = "grey",
#              width = .000001,
#              fill = "black",
#              position = "dodge") +
#   geom_point(aes(x = Percentile, y = mean), color = "light blue") +
#   geom_text(aes(x = Percentile, y = mean,
#                 label = ifelse(rank <24 | Percentile < 50,
#                                member_state, "")),
#             check_overlap = T,
#                   size = 3,
#                   hjust = "inward") +
#   labs(#title = "Average Legislator Requests per Year by Percentile",
#        x = "Percentile within chamber",
#        y = "Average requests per year") +
#   facet_grid(. ~ chamber , scales = "free_y")


## ----echo=verification, eval=verification-------------------------------------
percentiles |>
  group_by(chamber) |>
  summarise(chamber_average = mean(mean)) 


## ----echo=verification, eval=verification-------------------------------------
percentiles |> arrange(rank) |> head()


## -----------------------------------------------------------------------------
#| label: percentiles-chamber
#| fig-height: 3.4
#| fig-width: 3
#| cache: false
#| 
percentiles %>% 
    filter(chamber == "Senate") %>% 
    ggplot() + 
    geom_col(aes(x = Percentile, y = mean), 
             color = "grey", 
             width = .000001,
             fill = "black",
             position = "dodge") + 
  geom_point(aes(x = Percentile, 
                 y = mean), 
             color = "light blue") + 
  geom_text(aes(x = Percentile, 
                y = mean, 
                label = ifelse(rank <24 | Percentile < 50,
                               member_state, ""
                               )
                ), 
            check_overlap = T, 
                  size = 3,
                  hjust = "inward") + 
  # theme_minimal() + 
  labs(#title = "Average Legislator Requests per Year by Percentile",
       x = "Percentile in the Senate",
       y = "Average requests per year") 

percentiles %>% 
  filter(chamber == "House") %>% 
    ggplot() + 
    geom_col(aes(x = Percentile, y = mean), 
             color = "grey", 
             width = .000001,
             fill = "black",
             position = "dodge") + 
  geom_point(aes(x = Percentile, y = mean), color = "light blue") + 
  geom_text(aes(x = Percentile, y = mean, 
                label = ifelse(rank <24 | Percentile < 50,
                               member_state, "")), 
            check_overlap = T, 
                  size = 3,
                  hjust = "inward") + 
  # theme_minimal() + 
  labs(#title = "Average Legislator Requests per Year by Percentile",
       x = "\nPercentile in the House",
       y = "Average requests per year") 


## ----counts-per-year, fig.height=3, fig.width=5.7, cache=TRUE, eval=TRUE------
#counts-per-year

dcounts_per_legislator <- dcounts  %>% 
  group_by(bioname, year) %>% 
  mutate(per_bioname_year = sum(per_icpsr_chamber_year_agency_type)) %>%
  ungroup() 

schumer2013 <- dcounts_per_legislator %>% filter(bioname == "SCHUMER, Charles Ellis (Chuck)",
                   year == 2013) %>% .$per_bioname_year %>% unique()

cruz2013 <- dcounts_per_legislator %>% filter(bioname == "CRUZ, Rafael Edward (Ted)",
                   year == 2013) %>% .$per_bioname_year %>% unique()

mcconnell2013 <- dcounts_per_legislator %>% filter(bioname == "McCONNELL, Addison Mitchell (Mitch)",
                   year == 2013) %>% .$per_bioname_year %>% unique()

# plot a line for all legislator-year counts in the data (this is a lot of data points and thus takes a long time to render)
p <- dcounts_per_legislator %>% 
  filter(year > 2006, year <2018) %>%
  distinct(bioname, per_bioname_year, year, chamber) %>% 
  filter(per_bioname_year > 0) %>% 
  ggplot() + 
  geom_line(aes(x = year, 
                y = per_bioname_year, 
                group = bioname, 
                color = chamber), 
            alpha = .1) +
  geom_line(aes(x = year, 
                group = bioname, 
                color = chamber,
                y = ifelse(bioname == "SCHUMER, Charles Ellis (Chuck)", per_bioname_year, NA))) + 
  geom_label(x = 2013, 
             y = schumer2013, 
             label = "Chuck Schumer") +
  geom_line(aes(x = year, group = bioname, color = chamber,
                y = ifelse(bioname == "CRUZ, Rafael Edward (Ted)", per_bioname_year, NA))) + 
  geom_label(x = 2013, 
             y = cruz2013, 
             label = "Ted Cruz") +
  geom_line(aes(x = year, group = bioname, color = chamber,
                y = ifelse(bioname == "McCONNELL, Addison Mitchell (Mitch)", per_bioname_year, NA))) + 
  geom_label(x = 2013, 
             y = mcconnell2013, 
             label = "Mitch McConnell") +
  labs(x = "",
       color = " ",
       y = paste("Requests per legislator per year"),
       title = "") +
  scale_x_continuous(breaks = unique(dcounts$year)) + 
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis", direction = -1) 

# Non-AJPS formatting 
# p + 
  # theme_minimal() + 
  # theme(axis.text.x = element_text(angle = 30, vjust = 0) ) 


# FOR AJPS 
p +
  theme(panel.background = element_blank(), 
        legend.position = "inside",
        #FIXME NOTE: legend justification requires ggplot2 v3.5 or higher https://stackoverflow.com/questions/78212838/error-when-running-code-examples-from-usmap-r-package
        legend.justification.inside = c(1, 1), 
        #legend.position.inside = c(1, 1),
        axis.ticks = element_blank()
        ) 


## ----comestock----------------------------------------------------------------
dcounts %>% 
  filter(bioname == "COMSTOCK, Barbara J.", year %in% c(2015, 2016)) |> 
  group_by(year) %>% 
  summarise(n = sum(per_icpsr_chamber_year_agency_type))


## ----case-study-function------------------------------------------------------
case_study <- function(cq){

  CHAMBER <- filter(member_data, cqlabel == cq) %>% 
    .$chamber %>% .[1]

dcounts %>% 
  group_by(member_state, year, chamber, cqlabel) %>% summarise(n = sum(per_icpsr_chamber_year_agency_type)) %>% 
  ungroup() %>% 
  group_by(chamber) %>% 
  mutate(mean_per_chamber = mean(n) %>% round(0)) %>% 
  filter(cqlabel == cq, year > 2007, year <2017) %>% 
  ggplot() +
  aes(x = year, 
      y = n, 
      fill = member_state %>% str_replace("-.*", "\\)")) + 
  geom_col(alpha = .5, position = "dodge") +
  geom_hline(aes(yintercept = mean_per_chamber), linetype = 2) + 
  geom_text(aes(y = mean_per_chamber), 
            x = 2007.5, 
            label = "Chamber average", check_overlap = T,
            size = 3, vjust = 1.3, hjust = 0) + 
  scale_x_continuous(breaks = seq(2008, 2016, 1)) +
  labs(x = "",
       y = "Number of requests", 
       fill = "") +  # str_c(CHAMBER, "-", cq) %>% str_remove_all("\\(|\\)|NA") ) +
  theme(panel.grid.major.x =  element_blank(),         
        legend.position = "bottom",
        axis.title.x = element_text(margin = unit(c(-5, 0, 0, 0), "mm")),
        # # FOR AJPS
        # axis.text.x = element_text(angle = 45), 
        # # END AJPS 
        panel.grid.minor.x =  element_blank() )+
    scale_fill_viridis_d(begin = 0, end = .6, option = "cividis", direction = -1) 
  
} 


## ----cases--------------------------------------------------------------------
#| fig-width: 5.7
#| fig-height: 2.5
#| out-width: "100%"


# plot for WI for paper
case_study("(WI)")

case_study("(WI-7)")

# Wolf --> Comstock example 
case_study("(VA-10)")


# 
# case_study("(NY-07)")
# 
# case_study("(NY-14)")

# plot all 
# cases <- unique(member_data$cqlabel) #%>% head()

# map(cases, case_study )



## ----echo=verification, eval=verification-------------------------------------


## ----pop, fig.width=6, fig.height=4-------------------------------------------
dcounts %>% 
  filter(chamber == "Senate") %>% 
    group_by(member_state, year, pop2010) %>% 
  summarise(n = sum(per_icpsr_chamber_year_agency_type)) %>% 
    group_by(member_state, pop2010) %>%
  summarise(mean = mean(n)) %>% ungroup() %>% 
    ggplot() + 
    geom_point(aes(x = pop2010, y = mean), color = "light blue") + 
    geom_smooth(aes(x = pop2010, y = mean)) + 
  geom_text_repel(
    aes(
    x = pop2010,
    y = mean, 
    label = ifelse(mean > 390 | log(pop2010) > 17.0 | mean < 100,# & (log(pop2010) > 27 | log(pop2010) < 14), 
                   member_state  ,
                   "")), 
            seed = 44,
            check_overlap = T, 
            min.segment.length = 0,
            size = 3) + 
  labs(#title = "",
       x = "State population (log scale)",
       y = "Average number of requests per year") +
  scale_x_log10(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma, limits = c(0,500) ) 


## ----means--------------------------------------------------------------------
##creating the member-level aggregate count variables

# TOTAL counts 
d <- corr_counts |>
  group_by(agency, icpsr, chamber, year) |>
  summarise(perYear = sum(per_icpsr_chamber_year_agency_type)) |> 
  ungroup()

# Constituent counts 
d_con <- corr_counts |>
  subset(TYPE %in% c(1, 2, 3)) |> 
  group_by(agency, icpsr, chamber, year) |>
  summarise(perYear_con = sum(per_icpsr_chamber_year_agency_type))

# Policy counts 
d_policy <- corr_counts |>
  subset(TYPE %in% c(4, 5)) |> 
  group_by(agency, icpsr, chamber, year) |>
  summarise(perYear_pol = sum(per_icpsr_chamber_year_agency_type))

d$perYear_con <- d_con$perYear_con
d$perYear_policy<- d_policy$perYear_pol


## Means for paper
mean_total <- mean(d$perYear)
mean_con <- mean(d$perYear_con)
mean_pol <- mean(d$perYear_policy)

## Ns
n2007_2020 <- d$perYear |> sum()
n2007_2020coded <- sum(d$perYear_con, d$perYear_policy)
n_agency <- d$agency |> unique() |> length()



# Regression table formatting for AJPS 
modelsummary_AJPS <- function(models, notes = "", center_rows = 1, ...){
  modelsummary::modelsummary(models,  
             # Custom sig stars for AJPS 
             stars = c('†' = .1, '*' = .05, '**' = .01), 
             # Align coefficients by decimal for AJPS 
             align = paste0("l", paste0(rep("d", length(models)), collapse = "")), 
             add_rows = rows,               
             coef_map = cm, 
             gof_map = gm, 
             output = "tinytable",
             notes = notes) |>
    # bold header, hline bottom, aligned center
    tinytable::style_tt(i = 0:1, bold = T, line = "b",  align = "c") |>
    # stats aligned center 
    tinytable::style_tt(i = center_rows, align = "c") |> 
    # row labels left
    tinytable::style_tt(j = 1, align = "l")
} 

modelsummary <- modelsummary_AJPS

save(mean_total, mean_con, mean_pol, 
     n_agency, n2007_2020, n2007_2020coded, 
     modelsummary_AJPS,
     file = here::here("data", "means.Rdata"))


## ----echo=verification, eval=verification-------------------------------------
n_agency
n2007_2020
n2007_2020coded


## ----drop-chamber-switchers---------------------------------------------------
# make icpsr year variable 
d %<>% mutate( icpsr_year = paste(icpsr, year, sep='_') )

## chamber switchers  in original data (party switchers are not an issue because they net new ICPSR ids)
chamber_switchers  <- d |> 
  distinct(icpsr_year, agency, chamber) |>
  add_count(icpsr_year, agency,  #party, 
            name = "n") |>
  filter(n > 1) |> 
  distinct(icpsr_year, chamber) |> 
  arrange(icpsr_year)

# DROP CHAMBER SWITCHERS
d %<>% filter(!icpsr_year %in% chamber_switchers$icpsr_year)

############################
# add covariates for models #
############################

# congress year converters 
congress_years<- function(congress){
  years<- c(congress*2 + 1787, congress*2 + 1788 )
  return(years)
}

year_congress<- function(year){
  return(floor((year - 1787)/2))
}

# make congress to merge with member data 
d$congress<- year_congress(d$year)

# inspect duplicates 
d <- d |> 
  ungroup() |>
  distinct() |>
  add_count(icpsr, year, agency,  #party, 
            name = "n") 

if(testing){
d |>filter(n > 1) |> distinct(icpsr, year) 
} 

# LEFT join in member data 
d <- d |>  
  ungroup() |> 
  left_join(member_data, 
            by = c('congress', 'icpsr', "chamber") ) 


## count or repeated values (chamber and party switchers)
# note that in the full data, we use the dates of letters to attribute them to the proper party or chamber at the time
# but in yearly counts, we lose this level of detail, leading to undercounts for switchers

# # inspect for duplicates 
if(testing){
d <- d |> 
  ungroup() |>
  distinct() |>
  add_count(icpsr, year, agency,  #party, 
            name = "n") 
  
d |>filter(n > 1) |> distinct(icpsr, year) 
}




## ----data-transformations-----------------------------------------------------
# This chunk takes the minimal count data and creates legislator-level and district-level yearly count and ratio data
# These are then used to estimate the models
# the key DVs are
# - perYear = counts per year (member level and district level)
# - ratio = ratio of policy work to constituency service


####################
# TRANSFORMATIONS #  
###################

# tenure in office for experience tests 
d <- d |> 
  group_by(icpsr) |>
  mutate(
    tenure = year - first_year,
    first = ifelse(tenure==0, 1, 0),
    second = ifelse(tenure==1, 1, 0),
    third = ifelse(tenure==2, 1, 0),
    fourth = ifelse(tenure==3, 1, 0),
    fifth = ifelse(tenure==4, 1, 0),
    sixth = ifelse(tenure==5, 1, 0),
    max_year = max(tenure)
  ) 

# two-way fixed effects 
d <- d |> 
  ungroup() |> 
  mutate(
    icpsr_agency = paste(agency, icpsr, sep='_'),
    agency_year = paste(agency, year, sep='_')
  )

# indicator for whether they survived their first election 
d <- d |> 
  mutate(survive = ifelse(
    chamber =='House' & max_year>1 | chamber=='Senate' & max_year>5,
    1, 0)
  )

dcounts_tenure <- d

#########
# RATIO # 
#########  

##  the ratio variable - at the legislator-year level 
## (not the legislator-year-agency level like above)
dcounts_ratio  <- d |>
  ungroup() |> 
  # variables for ratio models 
  group_by(year, icpsr, chamber,
           chair, ranking_minority,  
           majority, presidents_party,
           first, second, third, fourth, fifth, sixth)  |>
  summarise(perCon = sum(perYear_con), 
            perPol = sum(perYear_policy)) |>
  mutate(ratio = perCon/(perCon + perPol))

#### PER DISTRICT COUNTS ######################
d %<>% mutate(decade = case_when(
  year < 2011 ~ '0', 
  year > 2010 ~ '1'))


d %<>% mutate(state_dist = case_when(
  chamber=='Senate'~ paste(state,district_code,  sep='_' ),
  chamber =='House'~ paste(paste(state, district_code, sep='_'), decade, sep='_')
))


dcounts_per_district<- d |>
  group_by(year, state_dist, icpsr, chamber, 
           tenure, first, second, third, fourth, fifth, sixth,
           state) |>
  summarise(perYear = sum(perYear),
            perYear_con = sum(perYear_con),
            perYear_policy = sum(perYear_policy)) |>
  distinct()

# rename first year as "new_member" 
dcounts_per_district %<>% 
  mutate(new_member = first)


## ----prestige-----------------------------------------------------------------
# THE BETTER DEFINITION WE USE IN THE FINAL PAPER 
prestige_house <- c(
  "APPROPRIATIONS", #
  # "ARMED SERVICES", #
  #"BUDGET", # 
  "COMMERCE", #
  "ENERGY", #
  "FINANCIAL SERVICES", # 
  "BANKING", # 
  #"FOREIGN"
    #"INTERNATIONAL RELATIONS", # 
  # "JUDICIARY", # 
    "FINANCIAL SERVICES", # 
    "RULES", #
  "WAYS"
  ) %>% paste(collapse  = "|")# 

prestige_senate <- c(
   #"AGRICULTURE", 
   "APPROPRIATIONS", #
  "ARMED SERVICES", #
  #  "BANKING", # 
  #"BUDGET", # 
  #"COMMERCE", #
  #"ENERGY", #
  #  "NATURAL RESOURCES", # 
  #  "ENVIRONMENT", # 
   "FINANCE", #
  "FOREIGN RELATIONS") %>% paste(collapse  = "|")
  #"HOMELAND SECURITY", #
  #"JUDICIARY", # 
  # "INTELLIGENCE",
  #"RULES") #

prestige_house |> str_split("\\|") |> kable(col.names = "House `Exclusive` Prestige Committees (CRS 2022, pg 3)")
prestige_senate |> str_split("\\|") |> kable(col.names = "Senate `Super A` Prestige Committees  (CRS 2022, pg 3)")

member_data %<>% ungroup() %>% 
  mutate(prestige = ifelse(
    (chamber == "House" & str_detect(committees, prestige_house) ) | (chamber == "Senate" & str_detect(committees, prestige_senate)), 1, 0)
                    )

# INSPECT 
member_data |> drop_na(prestige) |> count(prestige) |> kable()

# Join prestige coding into main data 
dcounts_tenure %<>% left_join(member_data %>% distinct(icpsr, congress, prestige))

dcounts_ratio %<>% left_join(member_data %>% distinct(icpsr, congress, prestige))


## ----count-means--------------------------------------------------------------
# check N
dcounts_tenure %>% tally(perYear, name = "Member-Level Observations") |> kable()
# dcounts_ratio %>% tally(perCon) |> kable()
# dcounts_ratio %>% tally(perPol) |> kable()
dcounts_per_district %>% ungroup %>%  tally(perYear, name = "District-Level Observations") |> kable()

# check n per year 
dcounts_tenure %>% filter(year > 2006) %>%  group_by(year) %>% tally(perYear) |> kablebox_long()

# average per member
dcounts_tenure %>% left_join(member_data %>% distinct(bioname, icpsr)) %>%
  group_by(bioname, year) %>% 
  summarise(perYear = sum(perYear)) %>% 
  group_by(bioname) %>% 
  summarise(average_per_year = mean(perYear), average_per_year) |> 
  arrange(-average_per_year) |> 
  kablebox()

# average per member per agency 
dcounts_tenure %>% left_join(member_data %>% select(bioname, icpsr)) %>%
  group_by(bioname) %>% 
  summarise(average_per_year_per_agency = mean(perYear)) |> arrange(-average_per_year_per_agency) |> 
  kablebox()


# per member per agency 
dcounts_tenure %>% left_join(member_data %>% distinct(bioname, icpsr)) %>%
  group_by(bioname, agency) %>% 
  summarise(per_year_per_agency = perYear) |> arrange(-per_year_per_agency) |> 
  kablebox()



# mean 
perYear_mean1 <- dcounts_tenure$perYear |> mean()
perYear_mean_total <- dcounts_tenure |> 
  group_by(icpsr, year) |> 
  transmute(perYearTotal = sum(perYear))
perYear_mean_total <- mean(perYear_mean_total$perYearTotal) |> round()

perYear_mean_policy <- dcounts_tenure |> 
  group_by(icpsr, year) |> 
  transmute(perYear = sum(perYear_policy))
perYear_mean_policy <- mean(perYear_mean_policy$perYear) |> round()

perYear_mean_con <- dcounts_tenure |> 
  group_by(icpsr, year) |> 
  transmute(perYear = sum(perYear_con))
perYear_mean_con <- mean(perYear_mean_con$perYear) |> round()

# Better variable names for tables
dcounts_tenure %<>% 
  mutate(Legislator = icpsr,
         Legislator_x_Agency = icpsr_agency,
         Year_x_Agency = agency_year)

# total per agency
dcounts_tenure %>% group_by(agency) %>% summarise(n = perYear %>% sum()) %>% arrange(-n) %>% kablebox()

# total per legislator-agency pair 
dcounts_tenure %>% group_by(Legislator_x_Agency) %>% summarise(n = perYear %>% sum()) %>% arrange(-n) %>% kablebox()


## ----modal-agencies-----------------------------------------------------------
 dcounts_tenure %>% 
 group_by(agency, year) %>% 
  summarise(perAgency = sum(perYear)) %>% 
  ungroup() %>% 
  mutate(median = median(perAgency),
         diff = abs(perAgency - median)) %>% 
  arrange(diff) %>% 
  kablebox()


## ----modal-legislators--------------------------------------------------------
modal <- dcounts_tenure %>% 
  group_by(icpsr) %>% 
  filter(first_year > 2006, first_year < 2013,  max_year > 6) %>% 
  mutate(Chair = sum(chair, na.rm = T) >1) %>% 
  group_by(icpsr, year) %>%
  mutate(perLeg = sum(perYear)) %>% 
  filter(perYear == 1, perLeg == perYear_mean_total  ) %>% 
  select(perAgency = perYear, perLeg, icpsr, icpsr_agency, agency_year, first_year, max_year, Chair) %>% 
  distinct() %>% 
  ungroup() %>% left_join(member_data %>% select(bioname, icpsr))


modal %>%
  mutate(average = perLeg) %>% 
  count(average, bioname, first_year, max_year, Chair, sort = T, name = "agency_year_count == 1") %>% 
  distinct() %>%
  kablebox()


## ----values, fig.width=5, fig.height=3----------------------------------------

modal %<>% filter(Chair) %>% distinct()

modal %>%
  kablebox()

dcounts_tenure %>% 
  group_by(icpsr, year) %>% 
  mutate(perYearTotal = sum(perYear)) %>%
  #mutate (perLegislator = sum(perYear)) %>% 
  ungroup() %>% 
  mutate(meanLeg = mean(perYearTotal),
         meanYear = mean(perYear)) %>%
  filter( abs(perYearTotal - perYear_mean_total) < 10,   
          perYear < 10 )  %>% 
  ggplot() +
  aes(x = perYear, 
      y = perYearTotal) + 
  geom_jitter(alpha = .5) + 
  geom_vline(aes(xintercept = meanYear), color = "blue") + 
  geom_hline(aes(yintercept = meanLeg), color = "blue") + 
  geom_text_repel(
    aes( label = ifelse(icpsr_agency %in% modal$icpsr_agency & agency_year %in% modal$agency_year , 
                     icpsr_agency, 
                     NA)  ),
    color = "red", 
    box.padding = 0.5, 
    max.overlaps = Inf
    ) + 
  labs(x = "Contacts per Legislator per Agency per year",
       y = "Contacts per Legislator per year") + 
  scale_x_continuous(breaks = seq.int(0, 9, length.out =  10))

modal %>%
  distinct(perAgency, bioname, icpsr_agency, agency_year) %>% 
  kablebox()

agency = "DHS" 
agency = "DOI"
agency = "ETA"
agency = "ED"
agency = "NLRB"
agency = "NPS"
icpsr = 40705
year = 2017

# by chair and President's party
values <- tidyr::expand(dcounts_tenure,
                        agency =  "Treasury_IRS",
                        chair = chair, 
                        ranking_minority	= FALSE, 
                        prestige = TRUE, 
                        first = FALSE,
                        second = FALSE,
                        third = FALSE,
                        fourth = FALSE,
                        fifth = FALSE, 
                        sixth = FALSE,
                        Legislator_x_Agency = "Treasury_IRS_20947",
                        Year_x_Agency = "Treasury_IRS_2015",
                        Legislator =  "20947",
                        Year =  "2015",
                        majority = TRUE,
                        presidents_party = presidents_party
                   ) %>% 
  drop_na(majority, chair)

# by year 
values_tenure <- tidyr::expand(dcounts_tenure,
                        agency =  "Treasury_IRS",
                        chair = chair, 
                        ranking_minority	= FALSE, 
                        prestige = TRUE, 
                        first = first,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator_x_Agency =  "Treasury_IRS_20947",
                        Year_x_Agency =  "Treasury_IRS_2015",
                        Legislator =  "20947",
                        Year =  "2015",
                        majority = TRUE,
                        presidents_party = TRUE
                   ) %>% 
  drop_na(chair)


## ----gini---------------------------------------------------------------------
gini <- dcounts_tenure |> 
  ungroup() |> 
  group_by(year, icpsr) |> 
  summarize(perYear = sum(perYear)) |> 
  ungroup() |> 
group_by(year) |> 
  summarise(gini = round(ineq::Gini(perYear),2)) |>
  arrange(- year) |> 
  filter(year > 2006) 

gini|> 
  kablebox()

gini |> 
  ggplot() +
  aes( x = year, y = gini) + 
  geom_point() + 
  geom_smooth(method = "lm")

gini_min <- min(gini$gini)
gini_max <- max(gini$gini)


gini2 <- dcounts_tenure |> 
  ungroup() |> 
  group_by(year, chamber, icpsr) |> 
  summarize(perYear = sum(perYear)) |> 
  ungroup() |> 
group_by(year, chamber) |> 
  summarise(gini = round(ineq::Gini(perYear),2)) |>
  arrange(- year) |> 
  filter(year > 2006) 

gini2|> 
  kablebox()

gini2 |> 
  ggplot() +
  aes( x = year, y = gini, color = chamber) + 
  geom_point() + 
  geom_smooth(method = "lm")




## ----echo=verification, eval=verification-------------------------------------
gini %<>% filter(year > 2017)

min(gini$gini)
max(gini$gini)


## -----------------------------------------------------------------------------
# helper functions to plot predicted values
ggchair <- function(predicted = predicted) {
  
  predicted$chair %<>% str_replace("0", " Not\nchair") %>% str_replace("1", "Chair")
  
  predicted$presidents_party %<>% str_replace("0", "\nNot president's\nparty") %>% str_replace("1", "\nPresident's\nparty")

predicted %>%  
  ggplot() + 
  aes(x = presidents_party, 
      y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high, #predicted + 1.96*std.error,
      fill = chair,
      shape = chair,
      color = chair,
      label = chair,
      ) + 
      geom_text_repel(direction = "y", size = 3, 
                      hjust= -.25, 
                      min.segment.length = Inf, 
                      force = 2,
                      vjust = 0, 
                      check_overlap = T) + 
  geom_pointrange(position =  position_dodge(width = -.1)  )  + 
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") +
  theme(legend.position = "none",
        axis.ticks.x = element_blank())
}


ggtenure <- function(predicted = predicted) {
  
predicted %<>%
  # drop estimates from impossible values
      group_by(rowid, 
               #predicted, std.error, 
               estimate, conf.low, conf.high,
               chair) %>% 
  mutate(sum = sum(first, second, third, fourth, fifth, sixth),
         more = ifelse(sum == 0, 1,0)) %>% 
    filter(sum < 2) %>% 
    pivot_longer(cols = c("first", "second", "third", "fourth", "fifth", "sixth", "more")) %>% 
    select(name, value) %>% 
    filter(value == 1) %>% 
    # clean up for presentation
    mutate(year_in_congress = name %>% 
             str_replace("more", "7\nor more") %>% 
             str_replace("sixth", "6") %>% 
             str_replace("fifth", "5") %>% 
             str_replace("fourth", "4") %>% 
             str_replace("third", "3") %>% 
             str_replace("second", "2") %>% 
             str_replace("first", "1") 
             ) %>% 
    ungroup()
  
# Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values 
# predicted %<>% full_join(dcounts_tenure2 %>% mutate(predicted = NA))
  
predicted %<>% filter(chair == 0 | name %in% c("sixth", "more"))

  predicted$chair %<>% str_replace("0", " Not\nchair") %>% str_replace("1", "Chair")
  

predicted %>%  
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_congress, 
            y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high, #predicted + 1.96*std.error,
      shape = chair,
      color = chair,
      label = ifelse(year_in_congress == "6", chair, NA) ) + 
    geom_text_repel(direction = "y", size = 3,
                    min.segment.length = Inf,
                    hjust= -.15, 
                    check_overlap = T) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis")  +
  theme(legend.position = "none")
}



## ----cm-----------------------------------------------------------------------
# Coef Map
cm = c("chair" = "Committee Chair",
       "ranking_minority" = "Ranking Member",
       "prestigeR2" = "Prestige Committee (R2)",
       "prestigeOLD" = "Prestige Committee (Old)",
       "prestige" = "Prestige Committee",
       "new_member" = "New Member", 
       "new_senator" = " New Senator in Delegation",
       "new_one" = " New Member in Delegation",
       "new_proportion" = " New Proportion in Delegation",
       "new_member:same_party" = " New Member x Same Party", 
       "first" = "First Year",
       "second" = "Second Year",
       "third" = "Third Year",
       "fourth" = "Fourth Year",
       "fifth" = "Fifth Year",
       "sixth" = "Sixth Year",
       "same_party:second" = "Second Year x Same Party",
       "same_party:third" = "Third Year x Same Party",
       "same_party:fourth" = "Fourth Year x Same Party",
       "same_party:fifth" = "Fifth Year x Same Party",
       "same_party:sixth" = "Sixth Year x Same Party",
       "same_party" = "Same Party",
       "majority" = "Majority",
       "presidents_party" = "President's party",
       "Legislator" = "Legislator", 
       "Agency" = "Agency",
       "Num.Obs." = "Observations"
       )

# FORMATTING FOR AJPS 
cmAJPS <- cm |> str_to_sentence()
names(cmAJPS) <- names(cm)
cm <- cmAJPS
# END FORMATTING FOR AJPS 

# set fixed effects mapping 
setFixest_dict(cm)

format_n <- function(x) format(round(x, 3), big.mark=",") # this works
f <- function(x) stringr::str_replace(x, "[A-z]", "✓") #FIXME not sure why this is not working

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = format_n),
    list("raw" = "FE: Year_x_Agency", "clean" = "Year x agency fixed effects", "fmt" = f),
      list("raw" = "FE: Legislator_x_Agency", "clean" = "Legislator x agency fixed effects", "fmt" = f),
  list("raw" = "FE: icpsr_agency", "clean" = "Legislator-agency fixed effects", "fmt" = f),
       list("raw" = "FE: District", "clean" = "District fixed effects", "fmt" = f),
       list("raw" = "FE: Year", "clean" = "Year fixed effects", "fmt" = f),
       list("raw" = "FE: Legislator.*x.*Agency", "clean" = "Legislator x agency fixed effects", "fmt" = f),
        list("raw" = "FE: Year.*x.*Agency", "clean" = "Year x agency fixed effects", "fmt" = f),
       list("raw" = "FE: Legislator", "clean" = "Legislator fixed effects", "fmt" = f)
  )


## -----------------------------------------------------------------------------
#| label: "m-total"
#| fig-cap: "m-total-[1:4].png"
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"

# paper table 2
# Model 1 
# cross-sectional 
m_total_cross <-feglm (perYear ~ 
                         chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency,
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_total_cross)}

coefplot(m_total_cross, horiz = T, drop = "(Intercept)") 



# Model 2
m_total_dnd <-feglm (perYear ~ 
                       chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator",
                    # ALT vcov = hetero ~ ssc(cluster.adj = TRUE),
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_total_dnd)}

coefplot(m_total_dnd, horiz = T) 


# 3
m_total_2nd <-feglm (perYear ~ 
                       chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure %>% filter(survive == 1))

if(testing){modelsummary::modelsummary(m_total_2nd)}
coefplot(m_total_2nd, horiz = T) 


# 4
m_logtotal_dnd <-feglm (log(perYear + 1) ~ 
                          chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_logtotal_dnd)}

coefplot(m_logtotal_dnd, horiz = T) 




## ----models_total-------------------------------------------------------------
models_total <- list(
  "(1)" = m_total_cross,
  "(2)" = m_total_dnd,
  "(3)" =  m_total_2nd,
  "(4)" = m_logtotal_dnd
)

rows <- tibble(
  term = c("Dependent variable", 
           "Majority",
           "President's party",
           "All legislators", 
           "Served at least 2nd term"),
  `(1)` = c("Count", "✓", "✓", "✓", ""),
  `(2)` =c("Count", "✓", "✓", "✓", ""),
  `(3)` = c("Count", "✓","✓","", "✓"),
  `(4)` = c("Log(Count+1)","✓","✓", "✓", "") 
)

rows <- tibble(
  term = c("Dependent variable", 
           #"Majority",
           #"President's party",
           "All legislators", 
           "Served at least 2nd term"),
  `(1)` = c("Count", "✓", ""),
  `(2)` =c("Count", "✓", ""),
  `(3)` = c("Count","", "✓"),
  `(4)` = c("Log(Count+1)", "✓", "") 
)

attr(rows, 'position') <- c(0, 20,21,22,23)

attr(rows, 'position') <- c(0, 24,25)


# HTML
modelsummary(models_total,
             notes = list("Robust standard errors in parentheses, clustered by legislator.",
                          "This table shows estimates of the effect of institutional power on levels of constituency service. All coefficients represent the average additional requests per year per agency; per legislator, per year effects are simply these coefficients times the number of agencies in the data.")) 

beta <- m_total_dnd$coefficients %>%
  round(3) %>% 
  as_tibble(rownames = "beta") %>% 
  pivot_wider(names_from = beta)

se <- m_total_dnd$se %>%
  round(3) %>% as_tibble(rownames = "se") %>%
  pivot_wider(names_from = se)

save(models_total, rows, cm, gm,  beta, se,
     file = here::here("models", "models_total-manuscript-table-2.Rdata"))


if(testing){
rows <- tibble(
  term = c("Dependent variable"),
  `(1)` = c("Count"),
  `(2)` =c("Count"),
  `(3)` = c("Count"),
  `(4)` = c("Log(Count+1)") 
)

attr(rows, 'position') <- c(0)


modelsummary::modelsummary(models_total,  
             # Custom sig stars for AJPS 
             #stars = c('†' = .1, '*' = .05, '**' = .01), 
             # Align coefficients by decimal for AJPS 
             #align = paste0("l", paste0(rep("d", length(models)), collapse = "")), 
             add_rows = rows,               
             #coef_map = cm, 
             #gof_map = gm, 
             #output = "tinytable",
             notes = "") 
}


## ----echo=verification, eval=verification-------------------------------------
models_total


## ----echo=verification, eval=verification-------------------------------------
beta$chair
beta$chair - se$chair*1.96
beta$chair + se$chair*1.96
n_agency
(beta$chair*n_agency) %>% round(0)
(beta$chair*n_agency)/perYear_mean_total  %>% round(2)*100


## -----------------------------------------------------------------------------
#| label: "m-total-predicted"
#| fig-cap: "m-total-predicted"
#| layout-ncol: 2
#| fig-height: 3
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"


predicted <- predictions(m_total_cross,
                         newdata = values)


# Cross-sectional predictions
predicted %>%
    mutate(estimate = estimate*n_agency, 
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>%  
  ggchair() + 
  labs(#title =  "", # "Predicted total letters per year\n(cross-sectional)",
       x = "",
       y = "Predicted total letters per year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_total_cross,
                         newdata = values_tenure)

predicted %>%
    mutate(estimate = estimate*n_agency, 
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>% 
ggtenure() +
  labs(#title =  "", # "Predicted total letters per year\n(cross-sectional)",
       x = "Years serving in Congress",
       y = "      Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 


# diff in diff 
predicted <- predictions(m_total_dnd,
                         newdata = values)


# Predictions by Chair and President's Party
predicted %>%
    mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>% # std.error = std.error*n_agency)) %>%
  ggchair() + 
  labs(#title =  "", # "Predicted total letters per year\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted2 <- predictions(m_total_dnd,
                         newdata = values_tenure)

predicted2 %>%
    mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>%  # std.error = std.error*n_agency) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1) %>% str_replace("N", "1")))) + 
  labs(#title =  "", # "Predicted total letters per year\nDifference in Differences (Within-Legislator)",
       x = "Years serving in Congress",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 


## ----echo=verification, eval=verification-------------------------------------
predicted |> distinct( presidents_party, estimate, chair) |> as_tibble() |> 
  mutate(estimate*n_agency)


## -----------------------------------------------------------------------------
#| label: "m-con"
#| fig-cap: "figs/m-con-[1:4].png"
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"


m_con_cross <-feglm (perYear_con ~ 
                       chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency,
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_con_cross)}

coefplot(m_con_cross, horiz = T, drop = "(Intercept)") 



# 2
m_con_dnd <-feglm (perYear_con ~ 
                     chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator",
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_con_dnd)}
coefplot(m_con_dnd, horiz = T) 


# 3
m_con_2nd <-feglm (perYear_con ~ 
                     chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure %>% filter(survive == 1))

if(testing){modelsummary::modelsummary(m_con_2nd)}

coefplot(m_con_2nd, horiz = T) 


# 4
m_logcon_dnd <-feglm (log(perYear_con + 1) ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_logcon_dnd)}

coefplot(m_logcon_dnd, horiz = T) 


## ----models_con---------------------------------------------------------------
models_con <- list(
  "(1)" = m_con_cross,
  "(2)" = m_con_dnd,
  "(3)" =  m_con_2nd,
  "(4)" = m_logcon_dnd
)

beta <- m_con_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

se <- m_con_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)

save(models_con, rows, cm, gm,  beta, se,
     file = here::here("models", "models_con.Rdata"))


## ----echo=verification, eval=verification-------------------------------------
beta$chair
beta$chair + se$chair*1.96
beta$chair - se$chair*1.96


## -----------------------------------------------------------------------------
#| label: "m-con-predicted"
#| fig-cap: "m-con-predicted"
#| layout-ncol: 2
#| fig-height: 3
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"


# Cross-sectional predictions:
predicted <- predictions(m_con_cross,
                         newdata = values)


# A plot by chair & presidents' party
predicted %>%
  mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>% # std.error = std.error*n_agency)) %>%
  ggchair() + 
  labs(#title =  "", # "Predicted Constituency Service Letters per year\n(cross-sectional)",
       x = "",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_con_cross,
                         newdata = values_tenure)

predicted %>%
    mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>%  # std.error = std.error*n_agency) %>%
ggtenure() +
   # geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(#title =  "", # "Predicted Constituency Service Letters Per year\n(Cross Sectional)",
       x = "Years serving in Congress",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 



# Diff in diff predictions 
predicted <- predictions(m_con_dnd,
                         newdata = values)


# diff in diff plot
predicted %>%
  mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>% # std.error = std.error*n_agency)) %>%
  ggchair() + 
  labs(#title =  "", # "Predicted Constituency Service Letters per year\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_con_dnd,
                         newdata = values_tenure)

predicted %>%
    mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>%  # std.error = std.error*n_agency) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(#title =  "", # "Predicted Constituency Service Letters Per year\nDifference in Differences (Within-Legislator)",
       x = "Years serving in Congress",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 


## -----------------------------------------------------------------------------
#| label: "m-policy"
#| fig-cap: "figs/m-policy-[1:4].png"
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"

m_policy_cross <-feglm (perYear_policy ~ 
                          chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority +  presidents_party | Year_x_Agency,
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_policy_cross)}

coefplot(m_policy_cross, horiz = T, drop = "(Intercept)") 



# 2
m_policy_dnd <-feglm (perYear_policy ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator",
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_policy_dnd)}
coefplot(m_policy_dnd, horiz = T) 


# 3
m_policy_2nd <-feglm (perYear_policy ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure %>% filter(survive == 1))

if(testing){modelsummary::modelsummary(m_tenure_2nd)}
coefplot(m_policy_2nd, horiz = T) 


# 4
m_logcon_dnd <-feglm (log(perYear_policy + 1) ~ 
                        chair + ranking_minority + prestige + 
                    first + second + third + fourth + fifth + sixth + 
                    majority + presidents_party | Year_x_Agency + Legislator_x_Agency, 
                    cluster = "Legislator", 
           data = dcounts_tenure)

if(testing){modelsummary::modelsummary(m_logcon_dnd)}

coefplot(m_logcon_dnd, horiz = T) 


## ----models_policy------------------------------------------------------------
models_policy <- list(
  "(1)" = m_policy_cross,
  "(2)" = m_policy_dnd,
  "(3)" =  m_policy_2nd,
  "(4)" = m_logcon_dnd
)

modelsummary(models_policy)

beta <- m_policy_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

se <- m_policy_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)

save(models_policy, rows, cm, gm,  beta, se,
     file = here::here("models", "models_policy.Rdata"))


## -----------------------------------------------------------------------------
#| label: "m-policy-predicted"
#| fig-cap: "m-policy-predicted"
#| layout-ncol: 2
#| fig-height: 3
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"

predicted <- predictions(m_policy_cross,
                         newdata = values)

# Cross-sectional predictions
predicted %>%
  mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>% # std.error = std.error*n_agency)) %>%
  ggchair() +
  labs(#title =  "", # "Predicted Policy Letters per year\n(cross-sectional)",
       x = "",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 

# by tenure
predicted <- predictions(m_policy_cross,
                         newdata = values_tenure)

predicted %>%
    mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>%  # std.error = std.error*n_agency) %>%
ggtenure() +
  labs(#title =  "", # "Predicted Policy Letters Per year\n(Cross Sectional)",
       x = "Years serving in Congress",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 


# Predictions for diff in diff plot 
predicted <- predictions(m_policy_dnd,
                         newdata = values)


# diff in diff plot
predicted %>%
  mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>% # std.error = std.error*n_agency)) %>%
  ggchair() +
  labs(#title =  "", # "Predicted Policy Letters per year\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "")  

# by tenure
predicted <- predictions(m_policy_dnd,
                         newdata = values_tenure)

predicted %>%
    mutate(estimate = estimate*n_agency, #predicted*n_agency,
         conf.high = conf.high*n_agency, conf.low = conf.low*n_agency) %>%  # std.error = std.error*n_agency) %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(#title =  "", # "Predicted Policy Letters Per year\nDifference in Differences (Within-Legislator)",
       x = "Years serving in Congress",
       y = "  Predicted letters per year",
       fill = "",
       color = "",
       shape = "") 




## ----models_total_con---------------------------------------------------------
models_total_con <- list(
  "(1)" = m_total_cross,
  "(2)" = m_total_dnd,
  "(3)" =  m_con_cross,
  "(4)" = m_con_dnd
)


rows <- tibble(
  term = c("Dependent variable"),
  `(1)` = c("Total Count"),
  `(2)` =c("Total Count"),
  `(3)` = c("Constituent Service"),
  `(4)` = c("Constituent Service") 
)

attr(rows, 'position') <- c(0)

modelsummary(models_total_con)

beta_total <- m_total_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

beta_con <- m_con_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

se_total <- m_total_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se) |> mutate_all(as.numeric)

se_con <- m_con_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)|> mutate_all(as.numeric)

save(models_total_con, rows, cm, gm, beta_total, se_total, beta_con, se_con,
     file = here::here("models", "models_total_con.Rdata"))


## -----------------------------------------------------------------------------
#| label: "m-ratio"
#| fig-cap: "figs/m-ratio-[1:4].png"
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"

dcounts_ratio %<>% 
  mutate(Legislator = icpsr,
         Year = year) %>% 
  filter(year >2006 & year < 2019)

m_ratio_cross <- feglm(ratio ~
  chair + ranking_minority + prestige + majority +  presidents_party + 
  first + second + third + fourth + fifth + sixth | Year,
cluster = "Legislator",
data = dcounts_ratio)

if(testing){modelsummary::modelsummary(m_ratio_cross)}

coefplot(m_ratio_cross, horiz = T, drop = "(Intercept)") 



# Diff in diff
m_ratio_dnd <- feglm(ratio ~
  chair + ranking_minority + prestige + majority + presidents_party + 
  first + second + third + fourth + fifth + sixth | 
    Legislator + Year,
cluster = "Legislator",
data = dcounts_ratio)

if(testing){modelsummary::modelsummary(m_ratio_dnd)}

coefplot(m_ratio_dnd, horiz = T) 


## ----models_ratio-------------------------------------------------------------
models_ratio <- list(
  "(1)" = m_ratio_cross,
  "(2)" = m_ratio_dnd
)

rows <- tibble(
  term = c("Dependent variable", 
           "Majority", 
           "President's party"),
  `(1)` = c("Ratio", 
            "✓", 
            "✓"),
  `(2)` =c("Ratio", 
           "✓", 
           "✓")
)

# all coef
rows <- tibble(
  term = c("Dependent variable"),
  `(1)` = c("Ratio"),
  `(2)` =c("Ratio")
)

# Check marks for controls 
attr(rows, 'position') <- c(0, 20,21)

# all coef
attr(rows, 'position') <- c(0)


modelsummary(models_ratio,
             notes = list("Robust standard errors in parentheses, clustered by legislator.") )

beta <- m_ratio_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

se <- m_ratio_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)

save(models_ratio, rows, cm, gm,  beta, se,
     file = here::here("models", "models_ratio-manuscript-table-4.Rdata"))


## ----echo=verification, eval=verification-------------------------------------
-beta$chair
beta$chair + se$chair*1.96
beta$chair - se$chair*1.96
-beta$ranking_minority
-beta$ranking_minority + se$ranking_minority*1.96
-beta$ranking_minority - se$ranking_minority*1.96


## -----------------------------------------------------------------------------
#| label: "m-ratio-predicted"
#| fig-cap: "m-ratio-predicted"
#| layout-ncol: 2
#| fig-height: 3
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| fig-show: "hold"

predicted <- predictions(m_ratio_cross,
                         newdata = values)



# Cross-sectional predictions
predicted %>%
ggchair() +
  labs(#title = "", # Predicted ratio of constituent to policy work\n(cross-sectional)",
       x = "",
       y = "Predicted ratio of\nconstituent to policy work",
       fill = "",
       color = "",
       shape = "")

# by tenure
predicted <- predictions(m_ratio_cross,
                         newdata = values_tenure)

predicted %>%
ggtenure() +
  labs(#title = "", # "Predicted Ratio of Constituent to Policy Work\n(cross-sectional)",
       x = "Years serving in Congress",
       y = "Predicted ratio of\nconstituent to policy work",
       fill = "",
       color = "",
       shape = "")


# Predictions for diff in diff plot 
predicted <- predictions(m_ratio_dnd,
                         newdata = values)


# diff in diff plot
predicted %>%
  ggchair() + 
  labs(#title = "", # "Predicted Ratio of Constituent to Policy Work\nDifference in Differences (Within-Legislator)",
       x = "",
       y = "Predicted ratio of\nconstituent to policy work",
       fill = "",
       color = "",
       shape = "")

# by tenure
predicted <- predictions(m_ratio_dnd,
                         newdata = values_tenure)

predicted %>%
ggtenure() +
    geom_line(aes(x = as.numeric(year_in_congress %>% str_sub(1,1)))) + 
  labs(#title = "", # "Predicted Ratio of Constituent to Policy Work\nDifference in Differences (Within-Legislator)",
       x = "Years serving in Congress",
       y = "Predicted ratio of\nconstituent to policy work",
       fill = "",
       color = "",
       shape = "")


## ----crosswalk----------------------------------------------------------------
# year congress crosswalk 
year_congress <- dcounts_tenure %>% distinct(year, congress)


# Add Congress variable 
dcounts_per_district %<>% 
  ungroup() %>% 
  # Add Congress variable 
  left_join(year_congress)

member_data %<>% left_join(year_congress) %>% arrange(icpsr, year)

duplicates <-distinct(dcounts_per_district, icpsr, chamber, congress, state, state_dist, new_member)  %>% count(icpsr, chamber, congress, state_dist) %>% filter(n > 1) 

# Because members of the house are new one year and not the other, we can't do "new" at the Congress level 
# inner_join(duplicates, dcounts_per_district)

# make a variable for the prior seat holder party 
party_crosswalk <- member_data  %>% 
  ungroup() %>% 
  # add new member and state_dist (accounting for redistricting)
  left_join(distinct(dcounts_per_district, icpsr, chamber, congress, state, state_dist, new_member, year)) %>% 
  # Begin at 2000 census redistricting 
  filter(year > 2001) %>% 
  # Make additional new member and state_dist to fill in NAs with "0" (i.e., 2002-2012 redistricting cycle )
  mutate(state_dist2 = paste(state, district_code, "0", sep = "_") |> 
           str_replace("0_0", "0"),
         state_dist = coalesce(state_dist, state_dist2)) %>% 
  group_by(state_dist, chamber, year) %>% 
  # combine ICPSR IDS for senators to get the senate delegation to know if it changed 
  arrange(icpsr) |> # Make sure they are in the same order 
  mutate(icpsr = unique(icpsr) |> paste(collapse = ";")) %>% # collapse senate ICPSR
  ungroup() %>% 
  group_by(state_dist, chamber) |> 
  arrange(year) |> 
  mutate(
         lag_icpsr = dplyr::lag(icpsr),
         new_member2 = icpsr != lag_icpsr,
         new_member =  coalesce(new_member, as.numeric(new_member2))) %>% 
  distinct(year, congress, state_dist, state, district_code, party, new_member #, new_member2, icpsr, lag_icpsr        
           ) %>% #FIXME DROPPING OBS HERE
  # # for Senate, capture mixed delegations 
  group_by(state_dist, year) %>% 
  arrange(party) %>% 
  mutate(state_dist_party = unique(party) |> paste(collapse = ";")) %>% 
  ungroup() %>% 
  # state_dist is missing for some obs in member_data, so here is a backup that does not account for redistricting. If we end up needing this, we should correct it to the districts accounting for redistricting
  mutate(state_dist2 = paste(state, district_code, sep = "_")) %>% 
  distinct(congress, state_dist,  state_dist_party, new_member, year #, new_member2, icpsr, lag_icpsr
  ) %>% 
  arrange(state_dist, year) %>% 
  # group by icpsr 
  group_by(state_dist) %>% 
  # create lag party var and fill it in for that member's tenure 
  mutate(lag_p = dplyr::lag(state_dist_party), 
         same_party = state_dist_party == lag_p,
         same_party = ifelse(new_member, 
                same_party,
                             NA )) %>% 
  tidyr::fill(same_party, .direction = "down")  %>% 
  ungroup()

# LOOK FOR MISSING OBS BY CONGRESS 
#member_data |> count(congress)
#party_crosswalk |> ungroup() |> count(congress, is.na(state_dist))
#party_crosswalk |> ungroup() |> filter(is.na(state_dist), congress > 109)


party_crosswalk  |>  
  select(year, state_dist, state_dist_party, lag_p, same_party, new_member)  |> 
  kablebox_long()



## ----add-crosswalk------------------------------------------------------------

# Add same_party variable to district counts data 
dcounts_per_district %<>% 
  left_join(party_crosswalk ) %>% 
  arrange(state_dist, congress) %>% 
  ungroup()
  
dcounts_per_district |> 
  count(same_party) |> 
  kablebox()
# 


# dcounts_per_district |> filter(chamber == "House") |>   select(icpsr, state_dist, year, state_dist_party, lag_p, same_party, new_member)   |> kablebox()

dcounts_per_district %<>% mutate(same_party = as.numeric(same_party))


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-cap: "figs/m-district-[1:4].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district"
#| fig-show: "hold"

# modify var names for presentation 
dcounts_per_district %<>% 
  mutate(same_party = as.numeric(same_party),
         Legislator = icpsr,
         District = state_dist,
         Year = year)

m_district_cross <- feglm(perYear ~
  new_member + 
  second + third + fourth + fifth + sixth | Year,
cluster = "District",
data = dcounts_per_district)

if(testing){modelsummary::modelsummary(m_district_cross)}

coefplot(m_district_cross, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 


m_district_cross_party <- feglm(perYear ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | Year,
cluster = "District",
data = dcounts_per_district)

if(testing){modelsummary::modelsummary(m_district_cross)}

coefplot(m_district_cross_party, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 


# Diff in diff
m_district_dnd <- feglm(perYear ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district )

coefplot(m_district_dnd, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 

# Diff in diff
m_district_dnd_party <- feglm(perYear ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district )

coefplot(m_district_dnd_party, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 



# Diff in diff
m_district_dnd_house <- feglm(perYear ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "House"))


# Diff in diff + party change var 
m_district_dnd_house_party <- feglm(perYear ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "House"))

# Diff in diff
m_district_dnd_senate <- feglm(perYear ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "Senate"))


# Diff in diff + party change var 
m_district_dnd_senate_party <- feglm(perYear ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "Senate"))



## ----models_district----------------------------------------------------------
models_district <- list(
  "(1)" = m_district_cross,
  "(2)" = m_district_dnd,
  "(3)" = m_district_dnd_house,
  "(4)" = m_district_dnd_senate
)


rows <- tibble(
  term = c("Dependent variable", 
           "All districts",
           "House only",
           "Senate only"),
  `(1)` = c("Per year", 
            "✓", 
            "", 
            ""),
  `(2)` =c("Per year", 
           "✓", 
            "", 
            ""),
    `(3)` = c("Per year", 
            "", 
            "✓", 
            ""),
  `(4)` =c("Per year", 
           #"✓", 
           "", 
            "", 
            "✓")
)



# Check marks for controls 
attr(rows, 'position') <- c(0, 14:16)



modelsummary(models_district,
             notes = list("Robust standard errors in parentheses, clustered by district.") )

beta <- m_district_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

beta_cross <- m_district_cross$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)


se <- m_district_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)

save(models_district, rows, cm, gm,  beta, beta_cross, se,
     file = here::here("models", "models_district-manuscript-table-3.Rdata"))


## ----echo=verification, eval=verification-------------------------------------
# cross-sectional average differences
beta_cross

# diff in diff
beta

# all results
models_district


## ----models_district_party----------------------------------------------------

models_district_party <- list(
  "(1)" = m_district_cross_party,
  "(2)" = m_district_dnd_party,
  "(3)" = m_district_dnd_house_party,
  "(4)" = m_district_dnd_senate_party
)


# Check marks for controls 
attr(rows, 'position') <- c(0, 28:31)



modelsummary(models_district_party, 
             notes = list("Robust standard errors in parentheses, clustered by district.") )

save(models_district_party, rows, cm, gm,  beta, se,
     file = here::here("models", "models_district_party.Rdata"))


## ----echo=verification, eval=verification-------------------------------------
beta$new_member
(beta$new_member/perYear_mean_total)*100
beta$new_member + se$new_member*1.96
beta$new_member - se$new_member*1.96


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-predicted-[1:4].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-predicted"
#| fig-show: "hold"


ggdistrict <- function(predicted = predicted, party = FALSE) {
  
predicted %<>%
  # drop estimates from impossible values
      group_by(rowid, 
               #predicted, std.error, 
               estimate, conf.low, conf.high,
               same_party) %>% 
  mutate(sum = sum(new_member, second, third, fourth, fifth, sixth),
         more = ifelse(sum == 0, 1,0)) %>% 
    filter(sum < 2) %>% 
    pivot_longer(cols = c("new_member", "second", "third", "fourth", "fifth", "sixth", "more")) %>% 
    select(name, value) %>% 
    filter(value == 1) %>% 
    # clean up for presentation
    mutate(year_in_congress = name %>% 
             str_replace("more", "7\nor more") %>% 
             str_replace("sixth", "6") %>% 
             str_replace("fifth", "5") %>% 
             str_replace("fourth", "4") %>% 
             str_replace("third", "3") %>% 
             str_replace("second", "2") %>% 
             str_replace("new_member", " New\nmember") 
             ) %>% 
    ungroup()
  
# Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values 
# predicted %<>% full_join(dcounts_tenure2 %>% mutate(predicted = NA))
  



p <- predicted %>%  
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_congress, 
            y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  # geom_line() + # requires numeric...look to see how I did this on the other ones
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") 

if(party){
p <- predicted %>%  
  mutate(same_party = as.logical(same_party)) %>% 
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_congress, 
            y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high, #predicted + 1.96*std.error,
      shape = same_party,
      color = same_party) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") 
}

return(p)
  
}


# dcounts_per_district %>% filter(District == "tennessee_1_1") |> View()
# dcounts_per_district %>% filter(District == "alabama_1_1") |> View()


# by year 
values <- tidyr::expand(dcounts_per_district,
                        new_member = new_member,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator = "21376",
                        Year = "2016",
                        District = "alabama_1_1",
                        same_party = same_party
                   ) %>% 
  drop_na(same_party)

predicted <- predictions(m_district_cross,
                         newdata = values) 



# Cross-sectional predictions
predicted %>%
ggdistrict() +
  labs(#title = "Letters\nper district\n(cross-sectional)",
       x = "Years serving in Congress",
       y = "   Predicted letters\nper year per district\n(cross-sectional)",
       fill = "",
       color = "",
       shape = "") 



# Predictions with the party of the prior representative

predicted <- predictions(m_district_cross_party,
                         newdata = values) 

predicted %>%
  ggdistrict(party = TRUE) + 
  labs(# title = "",
       x = "Years serving in Congress",
       y = "   Predicted letters\nper year per district\n(cross-sectional)",
       fill = "",
       color = "Same party",
       shape = "Same party") 




# diff in diff plot

predicted <- predictions(m_district_dnd,
                         newdata = values) 


predicted %>%
ggdistrict() +
      geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper district\n(difference-in-differences)",
       x = "Years serving in Congress",
       y = "    Predicted letters\nper year per district\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  

predicted <- predictions(m_district_dnd_party,
                         newdata = values) 


# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters Per District\n(difference-in-differences)",
       x = "Years serving in Congress",
       y = "   Predicted letters\nper year per district\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-predicted-house-[1:2].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-predicted-house"
#| fig-show: "hold"




# diff in diff plot

predicted <- predictions(m_district_dnd_house,
                         newdata = values) 


predicted %>%
ggdistrict() +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper House district\n(difference-in-differences)",
       x = "Years serving in Congress",
       y = "    Predicted letters per year\nper House district\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  

predicted <- predictions(m_district_dnd_house_party,
                         newdata = values)

# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper House district\n(difference-in-differences)",
       x = "Years serving in Congress",
       y = "   Predicted letters per year\nper House districte district\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-predicted-senate-[1:2].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-predicted-senate"
#| fig-show: "hold"


# Values for Senate seats 
values <- tidyr::expand(dcounts_per_district,
                        new_member = new_member,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator = "49700",
                        Year = "2017",
                        District = "alabama_0",
                        same_party = same_party
                   ) %>% 
  drop_na(same_party)

# diff in diff plot

predicted <- predictions(m_district_dnd_senate,
                         newdata = values) 


predicted %>%
ggdistrict() +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters per senator\n(difference-in-differences)",
       x = "Years serving in Congress",
       y = "   Predicted letters per year\nper Senator\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  

predicted <- predictions(m_district_dnd_senate_party,
                         newdata = values) 


# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters per senator\n(difference-in-differences)",
       x = "Years serving in Congress",
       y = "   Predicted letters per year\nper Senator\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-cap: "figs/m-district-con-[1:4].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-con"
#| fig-show: "hold"



m_district_con_cross <- feglm(perYear_con ~
  new_member + 
  second + third + fourth + fifth + sixth | Year,
cluster = "District",
data = dcounts_per_district)

if(testing){modelsummary::modelsummary(m_district_con_cross)}

coefplot(m_district_con_cross, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 


m_district_con_cross_party <- feglm(perYear_con ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | Year,
cluster = "District",
data = dcounts_per_district)

if(testing){modelsummary::modelsummary(m_district_con_cross)}

coefplot(m_district_con_cross_party, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 


# Diff in diff
m_district_con_dnd <- feglm(perYear_con ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district )

coefplot(m_district_con_dnd, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 

# Diff in diff
m_district_con_dnd_party <- feglm(perYear_con ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district )

coefplot(m_district_con_dnd_party, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 



# Diff in diff
m_district_con_dnd_house <- feglm(perYear_con ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "House"))


# Diff in diff + party change var 
m_district_con_dnd_house_party <- feglm(perYear_con ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "House"))

# Diff in diff
m_district_con_dnd_senate <- feglm(perYear_con ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "Senate"))


# Diff in diff + party change var 
m_district_con_dnd_senate_party <- feglm(perYear_con ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "Senate"))



## ----models_district_con------------------------------------------------------
models_district_con <- list(
  "(1)" = m_district_con_cross,
  "(2)" = m_district_con_dnd,
  "(3)" = m_district_con_dnd_house,
  "(4)" = m_district_con_dnd_senate
)


rows <- tibble(
  term = c("Dependent variable", 
           "All districts",
           "House only",
           "Senate only"),
  `(1)` = c("Per year", 
            "✓", 
            "", 
            ""),
  `(2)` =c("Per year", 
           "✓", 
            "", 
            ""),
    `(3)` = c("Per year", 
            "", 
            "✓", 
            ""),
  `(4)` =c("Per year", 
           "", 
            "", 
            "✓")
)



# Check marks for controls 
attr(rows, 'position') <- c(0, 14:16)



modelsummary(models_district_con, 
             notes = list("Robust standard errors in parentheses, clustered by district.") ) 

beta <- m_district_con_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

beta_cross <- m_district_con_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)


se <- m_district_con_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)

save(models_district_con, rows, cm, gm,  beta, se,
     file = here::here("models", "models_district_con.Rdata"))


## ----echo=verification, eval=verification-------------------------------------
beta$new_member
beta$new_member + se$new_member*1.96
beta$new_member - se$new_member*1.96


## ----models_district_con_party------------------------------------------------

models_district_con_party <- list(
  "(1)" = m_district_con_cross_party,
  "(2)" = m_district_con_dnd_party,
  "(3)" = m_district_con_dnd_house_party,
  "(4)" = m_district_con_dnd_senate_party
)


# Check marks for controls 
attr(rows, 'position') <- c(0, 28:31)



modelsummary(models_district_con_party, 
             notes = list("Robust standard errors in parentheses, clustered by district.") ) 

save(models_district_con_party, rows, cm, gm,  beta, se,
     file = here::here("models", "models_district_con_party.Rdata"))


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-con-predicted-[1:4].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-con-predicted"
#| fig-show: "hold"


ggdistrict <- function(predicted = predicted, party = FALSE) {
  
predicted %<>%
  # drop estimates from impossible values
      group_by(rowid, 
               #predicted, std.error, 
               estimate, conf.low, conf.high,
               same_party) %>% 
  mutate(sum = sum(new_member, second, third, fourth, fifth, sixth),
         more = ifelse(sum == 0, 1,0)) %>% 
    filter(sum < 2) %>% 
    pivot_longer(cols = c("new_member", "second", "third", "fourth", "fifth", "sixth", "more")) %>% 
    select(name, value) %>% 
    filter(value == 1) %>% 
    # clean up for presentation
    mutate(year_in_congress = name %>% 
             str_replace("more", "7\nor more") %>% 
             str_replace("sixth", "6") %>% 
             str_replace("fifth", "5") %>% 
             str_replace("fourth", "4") %>% 
             str_replace("third", "3") %>% 
             str_replace("second", "2") %>% 
             str_replace("new_member", " New\nmember") 
             ) %>% 
    ungroup()
  
# Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values 
# predicted %<>% full_join(dcounts_tenure2 %>% mutate(predicted = NA))
  



p <- predicted %>%  
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_congress, 
            y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  # geom_line() + # requires numeric...look to see how I did this on the other ones
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") 

if(party){
p <- predicted %>%  
  mutate(same_party = as.logical(same_party)) %>% 
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_congress, 
            y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high, #predicted + 1.96*std.error,
      shape = same_party,
      color = same_party) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") 
}

return(p)
  
}


# dcounts_per_district %>% filter(District == "tennessee_1_1") |> View()
# dcounts_per_district %>% filter(District == "alabama_1_1") |> View()


# by year 
values <- tidyr::expand(dcounts_per_district,
                        new_member = new_member,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator = "21376",
                        Year = "2016",
                        District = "alabama_1_1",
                        same_party = same_party
                   ) %>% 
  drop_na(same_party)

predicted <- predictions(m_district_con_cross,
                         newdata = values) 



# Cross-sectional predictions
predicted %>%
ggdistrict() +
  labs(#title = "Letters\nper district\n(cross-sectional)",
       x = "Years serving in Congress",
       y = "   Predicted letters per year\n(constituency service only)\nper district\n(cross-sectional)",
       fill = "",
       color = "",
       shape = "") 



# Predictions with the party of the prior representative

predicted <- predictions(m_district_con_cross_party,
                         newdata = values) 

predicted %>%
  ggdistrict(party = TRUE) + 
  labs(#title = "Letters\nper district\n(cross-sectional)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(constituency service only)\nper district\n(cross-sectional)",
       fill = "",
       color = "Same party",
       shape = "Same party") 




# diff in diff plot

predicted <- predictions(m_district_con_dnd,
                         newdata = values) 


predicted %>%
ggdistrict() +
      geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper district\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(constituency service only)\nper district\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  

predicted <- predictions(m_district_con_dnd_party,
                         newdata = values) 

# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters Per District\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(constituency service only)\nper district\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-con-predicted-house-[1:2].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-con-predicted-house"
#| fig-show: "hold"




# diff in diff plot

predicted <- predictions(m_district_con_dnd_house,
                         newdata = values) 


predicted %>%
ggdistrict() +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper House district\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(constituency service only)\nper House district\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  

predicted <- predictions(m_district_con_dnd_house_party,
                         newdata = values) 

# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper House district\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(constituency service only)\nper House district\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-con-predicted-senate-[1:2].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-con-predicted-senate"
#| fig-show: "hold"


# Values for Senate seats 
values <- tidyr::expand(dcounts_per_district,
                        new_member = new_member,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator = "49700",
                        Year = "2017",
                        District = "alabama_0",
                        same_party = same_party
                   ) %>% 
  drop_na(same_party)

# diff in diff plot

predicted <- predictions(m_district_con_dnd_senate,
                         newdata = values) 


predicted %>%
ggdistrict() +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters per senator\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(constituency service only)\nper senator\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  

predicted <- predictions(m_district_con_dnd_senate_party,
                         newdata = values) 
# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_congress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters per senator\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(constituency service only)\nper senator\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-cap: "figs/m-district-policy-[1:4].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-policy"
#| fig-show: "hold"



m_district_policy_cross <- feglm(perYear_policy ~
  new_member + 
  second + third + fourth + fifth + sixth | Year,
cluster = "District",
data = dcounts_per_district)

if(testing){modelsummary::modelsummary(m_district_policy_cross)}

coefplot(m_district_policy_cross, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 


m_district_policy_cross_party <- feglm(perYear_policy ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | Year,
cluster = "District",
data = dcounts_per_district)

if(testing){modelsummary::modelsummary(m_district_policy_cross)}

coefplot(m_district_policy_cross_party, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 


# Diff in diff
m_district_policy_dnd <- feglm(perYear_policy ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district )

coefplot(m_district_policy_dnd, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 

# Diff in diff
m_district_policy_dnd_party <- feglm(perYear_policy ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district )

coefplot(m_district_policy_dnd_party, horiz = T, drop = "(Intercept)", sub = "Reference = A member serving >6 years") 



# Diff in diff
m_district_policy_dnd_house <- feglm(perYear_policy ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "House"))


# Diff in diff + party change var 
m_district_policy_dnd_house_party <- feglm(perYear_policy ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "House"))

# Diff in diff
m_district_policy_dnd_senate <- feglm(perYear_policy ~
  new_member + #same_party + 
  second + third + fourth + fifth + sixth | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "Senate"))


# Diff in diff + party change var 
m_district_policy_dnd_senate_party <- feglm(perYear_policy ~
  new_member*same_party + 
  second*same_party + third*same_party + fourth*same_party + fifth*same_party + sixth*same_party | 
    District + Year,
cluster = "District",
data = dcounts_per_district |> filter(chamber == "Senate"))



## ----models_district_policy---------------------------------------------------
models_district_policy <- list(
  "(1)" = m_district_policy_cross,
  "(2)" = m_district_policy_dnd,
  "(3)" = m_district_policy_dnd_house,
  "(4)" = m_district_policy_dnd_senate
)


rows <- tibble(
  term = c("Dependent variable", 
           "All districts",
           "House only",
           "Senate only"),
  `(1)` = c("Per year", 
            "✓", 
            "", 
            ""),
  `(2)` =c("Per year", 
           "✓", 
            "", 
            ""),
    `(3)` = c("Per year", 
            "", 
            "✓", 
            ""),
  `(4)` =c("Per year", 
           #"✓", 
           "", 
            "", 
            "✓")
)



# Check marks for controls 
attr(rows, 'position') <- c(0, 14:16)



modelsummary(models_district_policy, 
             notes = list("Robust standard errors in parentheses, clustered by district.") ) 

beta <- m_district_policy_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)

beta_cross <- m_district_policy_dnd$coefficients %>%round(3) %>% as_tibble(rownames = "beta") %>% pivot_wider(names_from = beta)


se <- m_district_policy_dnd$se %>%round(3) %>% as_tibble(rownames = "se") %>% pivot_wider(names_from = se)

save(models_district_policy, rows, cm, gm,  beta, se,
     file = here::here("models", "models_district_policy.Rdata"))


## ----models_district_policy_party---------------------------------------------

models_district_policy_party <- list(
  "(1)" = m_district_policy_cross_party,
  "(2)" = m_district_policy_dnd_party,
  "(3)" = m_district_policy_dnd_house_party,
  "(4)" = m_district_policy_dnd_senate_party
)

# Check marks for controls 
attr(rows, 'position') <- c(0, 28:31)

modelsummary(models_district_policy_party, 
             notes = list("Robust standard errors in parentheses, clustered by district.") ) 

save(models_district_policy_party, rows, cm, gm,  beta, se,
     file = here::here("models", "models_district_policy_party.Rdata"))


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-policy-predicted-[1:4].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-policy-predicted"
#| fig-show: "hold"


ggdistrict <- function(predicted = predicted, party = FALSE) {
  
predicted %<>%
  # drop estimates from impossible values
      group_by(rowid, 
               #predicted, std.error, 
               estimate, conf.low, conf.high,
               same_party) %>% 
  mutate(sum = sum(new_member, second, third, fourth, fifth, sixth),
         more = ifelse(sum == 0, 1,0)) %>% 
    filter(sum < 2) %>% 
    pivot_longer(cols = c("new_member", "second", "third", "fourth", "fifth", "sixth", "more")) %>% 
    select(name, value) %>% 
    filter(value == 1) %>% 
    # clean up for presentation
    mutate(year_in_policygress = name %>% 
             str_replace("more", "7\nor more") %>% 
             str_replace("sixth", "6") %>% 
             str_replace("fifth", "5") %>% 
             str_replace("fourth", "4") %>% 
             str_replace("third", "3") %>% 
             str_replace("second", "2") %>% 
             str_replace("new_member", " New\nmember") 
             ) %>% 
    ungroup()
  
# Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values 
p <- predicted %>%  
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_policygress, 
            y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  # geom_line() + # requires numeric...look to see how I did this on the other ones
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") 

if(party){
p <- predicted %>%  
  mutate(same_party = as.logical(same_party)) %>% 
  ungroup() %>% 
  ggplot() + 
  aes(x = year_in_policygress, 
            y = estimate, # predicted, 
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high, #predicted + 1.96*std.error,
      shape = same_party,
      color = same_party) + 
  geom_pointrange(position =  position_dodge(width = -.3)  )  + 
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis") 
}

return(p)
  
}


# dcounts_per_district %>% filter(District == "tennessee_1_1") |> View()
# dcounts_per_district %>% filter(District == "alabama_1_1") |> View()


# by year 
values <- tidyr::expand(dcounts_per_district,
                        new_member = new_member,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator = "21376",
                        Year = "2016",
                        District = "alabama_1_1",
                        same_party = same_party
                   ) %>% 
  drop_na(same_party)

predicted <- predictions(m_district_policy_cross,
                         newdata = values) 


# Cross-sectional predictions
predicted %>%
ggdistrict() +
  labs(#title = "Letters\nper district\n(cross-sectional)",
       x = "Years serving in Congress",
       y = "   Predicted letters per year\n(policy only)\nper district\n(cross-sectional)",
       fill = "",
       color = "",
       shape = "") 



# Predictions with the party of the prior representative
predicted <- predictions(m_district_policy_cross_party,
                         newdata = values) 

predicted %>%
  ggdistrict(party = TRUE) + 
  labs(#title = "Letters\nper district\n(cross-sectional)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(policy only)\nper district\n(cross-sectional)",
       fill = "",
       color = "Same party",
       shape = "Same party") 




# diff in diff plot
predicted <- predictions(m_district_policy_dnd,
                         newdata = values) 


predicted %>%
ggdistrict() +
      geom_line(aes(x = as.numeric(year_in_policygress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper district\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(policy only)\nper district\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  
predicted <- predictions(m_district_policy_dnd_party,
                         newdata = values)

# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_policygress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters Per District\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(policy only)\nper district\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-policy-predicted-house-[1:2].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-policy-predicted-house"
#| fig-show: "hold"




# diff in diff plot
predicted <- predictions(m_district_policy_dnd_house,
                         newdata = values)


predicted %>%
ggdistrict() +
        geom_line(aes(x = as.numeric(year_in_policygress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper House district\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(policy only)\nper House district\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  
predicted <- predictions(m_district_policy_dnd_house_party,
                         newdata = values)
# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_policygress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters\nper House district\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(policy only)\nper House district\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 3
#| fig-cap: "figs/m-district-policy-predicted-senate-[1:2].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#|   - "4"
#| out-width: "100%"
#| label: "m-district-policy-predicted-senate"
#| fig-show: "hold"


# Values for Senate seats 
values <- tidyr::expand(dcounts_per_district,
                        new_member = new_member,
                        second = second,
                        third = third,
                        fourth = fourth,
                        fifth = fifth, 
                        sixth = sixth,
                        Legislator = "49700",
                        Year = "2017",
                        District = "alabama_0",
                        same_party = same_party
                   ) %>% 
  drop_na(same_party)

# diff in diff plot
predicted <- predictions(m_district_policy_dnd_senate,
                         newdata = values) 


predicted %>%
ggdistrict() +
        geom_line(aes(x = as.numeric(year_in_policygress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters per senator\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(policy only)\nper senator\n(difference-in-differneces)",
       fill = "",
       color = "",
       shape = "")



# Predictions with the Same party  
predicted <- predictions(m_district_policy_dnd_senate_party,
                         newdata = values) 

# diff in diff plot
predicted %>%
  ggdistrict(party = TRUE) +
        geom_line(aes(x = as.numeric(year_in_policygress %>% str_replace(" N", "1") %>% str_sub(1,1) ))) + 
  labs(#title = "Letters per senator\n(difference-in-differences)",
       x = "Years serving in Congress",
              y = "   Predicted letters per year\n(policy only)\nper senator\n(difference-in-differences)",
       fill = "",
       color = "Same party",
       shape = "Same party")


## ----spillover-data-----------------------------------------------------------

# new IVs: new_senator, new_proportion, new_one

dcounts_spillover <- dcounts_per_district %>% 
  # FIRST, identify districts with new members 
  group_by(state, Year) %>% 
  # size of the delegation 
  add_count(name = "n_delegation") %>% 
  mutate(
    # new IVs:
    new_senator = ifelse(chamber == "Senate" & new_member == 1, 1, NA), 
    new_proportion = sum(new_member)/n_delegation, 
    new_one = ifelse(new_member == 1, 1, NA)
  ) %>% 
  fill(new_senator, .direction = "updown") %>% 
  fill(new_one, .direction = "updown") %>% 
  mutate(new_senator =   replace_na(new_senator, 0),
         new_one = replace_na(new_one, 0) 
  ) %>% 
  group_by(state, Year, chamber) %>% 
  arrange(year, state) 

  # THEN WE DROP ALL OF THE NEW MEMBERS 
dcounts_spillover %<>%   filter(new_member == 0)  


# # inspect data 
# dcounts_spillover |> select(Year, state, chamber, new_member, new_one, new_senator, new_proportion, perYear) |> ungroup() |>
#   #filter(new_senator ==1) |>  
#   #count(new_senator, new_member) |> 
#   kablebox_long()
#     



## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-cap: "figs/m-spillover-[1:3].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#| out-width: "100%"
#| label: "m-spillover"
#| fig-show: "hold"

m_spillover <- feglm(perYear ~
  new_senator  | Year + District,
cluster = "District",
data = dcounts_spillover)

# coefplot(m_spillover, horiz = T, drop = "(Intercept)", sub = "") 

m_spillover_proportion <- feglm(perYear ~
  new_proportion  | Year + District,
cluster = "District",
data = dcounts_spillover |> filter(chamber == "Senate"))

# coefplot(m_spillover_proportion, horiz = T, drop = "(Intercept)", sub = "") 

m_spillover_new_one <- feglm(perYear ~
  new_one  | Year + District,
cluster = "District",
data = dcounts_spillover |> filter(chamber == "Senate"))

# coefplot(m_spillover_new_one, horiz = T, drop = "(Intercept)", sub = "") 



## ----models_spillover---------------------------------------------------------
models_spillover <- list(
  "(1)" = m_spillover,
  "(2)" = m_spillover_proportion,
  "(3)" = m_spillover_new_one
)


rows <- tibble(
  term = c("Dependent variable", 
           "All districts",
           "Senate only"),
  `(1)` = c("Per year", 
            "✓", 
            ""),
  `(2)` =c("Per year", 
            "", 
           "✓"),
    `(3)` = c("Per year", 
            "", 
            "✓")
)



# Check marks for controls 
attr(rows, 'position') <- c(0, 9:11)


modelsummary(models_spillover,
             notes = list("Robust standard errors in parentheses, clustered by district.") ) 

save(models_spillover, rows, cm, gm,  #beta, se,
     file = here::here("models", "models_spillover.Rdata"))


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-cap: "figs/m-spillover-con-[1:3].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#| out-width: "100%"
#| label: "m-spillover-con"
#| fig-show: "hold"

m_spillover_con <- feglm(perYear_con ~
  new_senator  | Year + District,
cluster = "District",
data = dcounts_spillover)

# coefplot(m_spillover, horiz = T, drop = "(Intercept)", sub = "") 

m_spillover_con_proportion <- feglm(perYear_con ~
  new_proportion  | Year + District,
cluster = "District",
data = dcounts_spillover |> filter(chamber == "Senate"))

# coefplot(m_spillover_proportion, horiz = T, drop = "(Intercept)", sub = "") 

m_spillover_con_new_one <- feglm(perYear_con ~
  new_one  | Year + District,
cluster = "District",
data = dcounts_spillover |> filter(chamber == "Senate"))

# coefplot(m_spillover_new_one, horiz = T, drop = "(Intercept)", sub = "") 



## ----models_spillover_con-----------------------------------------------------
models_spillover_con <- list(
  "(1)" = m_spillover_con,
  "(2)" = m_spillover_con_proportion,
  "(3)" = m_spillover_con_new_one
)


rows <- tibble(
  term = c("Dependent variable", 
           "All districts",
           "Senate only"),
  `(1)` = c("Per year (CS-only)", 
            "✓", 
            ""),
  `(2)` =c("Per year (CS-only)", 
            "", 
           "✓"),
    `(3)` = c("Per year (CS-only)", 
            "", 
            "✓")
)



# Check marks for controls 
attr(rows, 'position') <- c(0, 9:11)


modelsummary(models_spillover_con,
             notes = list("Robust standard errors in parentheses, clustered by district.") ) 

save(models_spillover_con, rows, cm, gm,  #beta, se,
     file = here::here("models", "models_spillover_con-manuscript-table-5.Rdata"))


## ----echo=verification, eval=verification-------------------------------------
models_spillover_con


## -----------------------------------------------------------------------------
#| layout-ncol: 2
#| fig-height: 4.5
#| fig-cap: "figs/m-spillover-policy-[1:3].png"
#| fig-subcap: 
#|   - "1"
#|   - "2"
#|   - "3"
#| out-width: "100%"
#| label: "m-spillover-policy"
#| fig-show: "hold"

m_spillover_policy <- feglm(perYear_policy ~
  new_senator  | Year + District,
cluster = "District",
data = dcounts_spillover)

# coefplot(m_spillover, horiz = T, drop = "(Intercept)", sub = "") 

m_spillover_policy_proportion <- feglm(perYear_policy ~
  new_proportion  | Year + District,
cluster = "District",
data = dcounts_spillover |> filter(chamber == "Senate"))

# coefplot(m_spillover_proportion, horiz = T, drop = "(Intercept)", sub = "") 

m_spillover_policy_new_one <- feglm(perYear_policy ~
  new_one  | Year + District,
cluster = "District",
data = dcounts_spillover |> filter(chamber == "Senate"))

# coefplot(m_spillover_new_one, horiz = T, drop = "(Intercept)", sub = "") 



## ----models_spillover_policy--------------------------------------------------
models_spillover_policy <- list(
  "(1)" = m_spillover_policy,
  "(2)" = m_spillover_policy_proportion,
  "(3)" = m_spillover_policy_new_one
)


rows <- tibble(
  term = c("Dependent variable", 
           "All districts",
           "Senate only"),
  `(1)` = c("Per year (Policy-only)", 
            "✓", 
            ""),
  `(2)` =c("Per year (Policy-only)", 
            "", 
           "✓"),
    `(3)` = c("Per year (Policy-only)", 
            "", 
            "✓")
)



# Check marks for controls 
attr(rows, 'position') <- c(0, 9:11)


modelsummary(models_spillover_policy,
             notes = list("Robust standard errors in parentheses, clustered by district.") ) 

save(models_spillover_policy, rows, cm, gm,  #beta, se,
     file = here::here("models", "models_spillover_policy.Rdata"))


## ----echo=TRUE----------------------------------------------------------------
sessionInfo <- sessionInfo()
save(sessionInfo, file = here::here("data", "sessionInfo.rda"))


## ----echo=TRUE----------------------------------------------------------------
time = Sys.time() - start_time
save(time, file = here::here("data", "time.rda"))

