### R code from vignette source 'chatbot_SI_jop.Rnw'

###################################################
### code chunk number 1: setup
###################################################
#| eval = TRUE,
#| echo = FALSE,
#| results = 'hide',
#| message = FALSE

require(knitr, quietly = TRUE)

options(modelsummary_factory_latex = 'kableExtra')

options(width = 110, knitr.kable.NA = '')

knit_hooks$set(inline = function(x) {
  prettyNum(x, big.mark=',')
})



###################################################
### code chunk number 2: load
###################################################
#| eval = TRUE, 
#| echo=FALSE, warning=FALSE, message=FALSE,
#| results = 'hide'
# load functions
source('utils.R')

### read in evaluation data

df_eval <- readRDS('clean_evaluation_data.rds') 
eval_n <- nrow(df_eval)

### read in concerns data
df_concerns <- readRDS('clean_learning_concerns_data.rds') 
concerns_n <- nrow(df_concerns)
learn_n <- length(unique(df_concerns$user_id))

df_nigeria <- df_eval[df_eval$country1 == 'nigeria', ]
df_kenya <- df_eval[df_eval$country1 == 'kenya', ]

# sample size in each country
nigeria_n <- nrow(df_nigeria)
kenya_n <- nrow(df_kenya)

# load colors 
cbPalette <- c('#999999', '#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', 
               '#D55E00', '#CC79A7')


## ggplot theme
vcf_theme <- function(){
  theme(panel.background = element_rect(fill = 'white', 
                                        colour = NA), 
        panel.border = element_rect(fill = NA, 
                                    colour = 'grey20'), 
        panel.grid = element_line(colour = 'grey92'), 
        panel.grid.minor = element_line(size = rel(0.5)), 
        strip.background = element_rect(fill = 'grey85', 
                                        colour = 'grey20'), 
        legend.title = element_text(size = 14),#element_blank(),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 14),
        # panel.border = element_rect(colour = 'gray50', fill=NA, size=.11),
        legend.position = 'bottom',
        axis.text = element_text(size=12),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 12),
        plot.caption = element_text(size = 12)
  )
}


# just pretest willingness_2s
covariate_list_short <- c('willingness_f', 'get_vaccinated_f')

covariate_list_full <- c(
  # pre-test response
  'willingness_f', 'get_vaccinated_f', 
  # pre-registered covariates
  'is_male',
  'age', # no need for flag
  'education', 'education_flag',
  'is_urban',
  'religion_christian',
  'religion_muslim',
  'religion_pentecostal',
  'religiosity', 'religiosity_flag',
  'digital_index',
  'fb_post', 'fb_post_flag',
  'fb_msg', 'fb_msg_flag',
  'assets_index', 'assets_index_flag',
  'has_job',
  'hhold', 'hhold_flag',
  'party_aligned',
  'health_access',
  'cov_know',
  'cov_worried', 'cov_worried_flag',
  'cov_govt'
  #'cov_govt_flag'
)
# check distributions
# sapply(covariate_list_full, function(x){table(df_eval[,x], useNA = 'ifany') })

covariate_list <- covariate_list_full



###################################################
### code chunk number 3: balance
###################################################
#| eval = TRUE, 
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| strip.white=TRUE, 
#| results='asis'

covars <- c( 
  #demographics
  'age', 'is_female', 'religion_christian', 'religiosity', 'party_aligned',
  #SES
  'is_urban', 'completed_secondary_school', 'hhold20', 'assets_index', 'has_job',
  #healthcare access, knowledge, attitudes
  'health_access', 
  'cov_know', 
  'know_wheretoget', 
  'adult_vax',
  'getvax_easy',
  'trust_index')

df_treat <- df_eval |> 
  mutate(is_female = 1*(is_male==0),
         hhold20 = case_when(hhold<20 ~hhold, 
                             TRUE ~NA)) |> 
  drop_na(treatment_group) |> 
  group_by(treatment_group) |> 
  summarise(across(all_of(covars),
                   .fns = list(mean = ~ sprintf('%.3f', mean(., na.rm = TRUE)),
                               se = ~ sprintf('(%.3f)', 
                                              sd(., na.rm = TRUE)/sqrt(sum(!is.na(.))))),
                   .names = '{.fn}_{.col}'
  )) |> t()

row.names(df_treat)[grep('se_', row.names(df_treat))] <- ''

label_rows <- c('mean_age' = 'Age',
                'mean_is_female' = '% Non-male',
                'mean_health_access' = 'Hours to nearest health facility',
                'mean_party_aligned' = '% Party aligned',
                'mean_cov_know' = 'COVID knowledge index (-5:5)', 
                'mean_religion_christian' = '% Christian', 
                'mean_religiosity' = 'Religiosity (1:6)', 
                'mean_completed_secondary_school' = '% Completed secondary school', 
                'mean_is_urban' = '% Urban', 
                'mean_hhold20' = 'Household size', 
                'mean_assets_index' = 'Assets index (0:6)', 
                'mean_has_job' = '% Employed', 
                'mean_trust_index' = 'Trust index (-14:14)', 
                'mean_adult_vax' = '% Any prior vaccination', 
                'mean_know_wheretoget' = '% Know where to get COVID vaccine', 
                'mean_getvax_easy' = 'Ease of getting COVID vaccine (-2:2)')

rownames(df_treat)[match(names(label_rows), 
                         rownames(df_treat))] <- label_rows

colnames(df_treat) <- c("Control","PSA","Concerns")

kbl(df_treat[-1,],
    format = 'latex',
    caption= 'Balance table: covariate means and standard errors by intervention group',
    align = 'c', booktabs = TRUE,
    linesep = '',
    toprule = '\\vspace{-2.5em} \\\\ \\toprule') |>
  kable_styling(latex_options = c('HOLD_position')) |> 
  pack_rows('Demographics', 2, 3) |>
  pack_rows(NULL, 4, 5, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 6, 7, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 8, 9, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 10, 11, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 12, 13, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows('Socioeconomic status', 14, 15) |> 
  pack_rows(NULL, 16, 17, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 18, 19, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 20, 21, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 22, 23, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows('Healthcare access/knowledge/attitudes', 24, 25) |> 
  pack_rows(NULL, 26, 27, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 28, 29, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 30, 31, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 32, 33, bold = FALSE, latex_gap_space = '-1.85\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |> 
  footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage, $n = $ ',
                            prettyNum(eval_n, big.mark = ','),
                            ". Estimates are of means grouped by treatment condition, produced from sample means and standard error of the sample mean."),
           escape = FALSE,
           threeparttable = TRUE,
           general_title = '')


###################################################
### code chunk number 4: resp_map
###################################################
#| eval = TRUE, 
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| fig.align='center', 
#| fig.height=9, 
#| fig.cap=
#| paste0('\\textbf{Distribution of respondents across counties in Kenya and states in Nigeria.}'),
#| prefix.string = 'fig',
#| results='asis'


# get count of respondents by state (nigeria) / county (kenya)
count_nigeria_states <- as.data.frame(table(df_eval[df_eval$country1=="nigeria",]$region))
count_kenya_counties <- as.data.frame(table(df_eval[df_eval$country1=="kenya",]$region))

names(count_nigeria_states) <- names(count_kenya_counties) <- c("admin","count")

# get spatial data for nigeria and kenya
# downloaded from: https://data.humdata.org/dataset/cod-ab-nga?
nigeria_map <- read_sf("nga_adm_osgof_20190417/nga_admbnda_adm1_osgof_20190417.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)

# load kenya county shapefile (natural earth only has province level)
# downloaded from: https://gadm.org/download_country_v3.html#google_vignette
kenya_map <- read_sf("gadm36_KEN_shp/gadm36_KEN_1.shp", quiet = TRUE, stringsAsFactors = FALSE,as_tibble = TRUE)

# clean kenya county punctutation
#sum(unique(kenya_map$NAME_1) %in% count_kenya_counties$admin)
#unique(kenya_map$NAME_1)[!unique(kenya_map$NAME_1) %in% count_kenya_counties$admin]

count_kenya_counties$admin <- sub("/"," ",as.character(count_kenya_counties$admin))
kenya_map$NAME_1 <- sub("'","",as.character(kenya_map$NAME_1))
kenya_map$NAME_1 <- sub("-"," ",as.character(kenya_map$NAME_1))

# change FCT to abuja
nigeria_map$ADM1_EN <- ifelse(nigeria_map$ADM1_EN == "Federal Capital Territory", "Abuja", nigeria_map$ADM1_EN)

# merge spatial and count data
nigeria_map <- merge(nigeria_map, count_nigeria_states, by.x="ADM1_EN", by.y="admin")
kenya_map <- merge(kenya_map, count_kenya_counties, by.x="NAME_1", by.y="admin")


# Plotting the heatmaps

# create threshold for white text labels
dark_threshold <- 1200 

# Nigeria:
# Compute the centroids of each state for labeling
centroids <- st_centroid(st_geometry(nigeria_map))
nigeria_map$lon <- st_coordinates(centroids)[, 1]
nigeria_map$lat <- st_coordinates(centroids)[, 2]

nigeria_map$text_color <- ifelse(nigeria_map$count > dark_threshold, "white", "black")

nigeria_plot <- ggplot(data = nigeria_map) + 
  geom_sf(aes(fill = count)) +
  geom_text_repel(aes(x = lon, y = lat, label = ADM1_EN, color = text_color), size = 1.5, box.padding = 0.1, max.overlaps = Inf, point.size = NA) +
  #geom_text(aes(x = lon, y = lat, label = name, color = text_color), size = 3, check_overlap = TRUE) +
  scale_color_identity() +  # This ensures colors are taken directly from the text_color column
  scale_fill_viridis_c(direction = -1, option = "mako", name = "") +
  theme_void() +
  labs(title = "Nigeria") +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",  # Position legend at the bottom
    legend.key.size = unit(1, "cm"),       # Increase legend key size
    legend.text = element_text(size = 10, angle = 45),  # Reduce font size and rotate
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# Kenya:
# Compute the centroids of each state for labeling
centroids <- st_centroid(st_geometry(kenya_map))
kenya_map$lon <- st_coordinates(centroids)[, 1]
kenya_map$lat <- st_coordinates(centroids)[, 2]

kenya_map$text_color <- ifelse(kenya_map$count > dark_threshold, "white", "black")

kenya_plot <- ggplot(data = kenya_map) + 
  geom_sf(aes(fill = count)) +
  geom_text_repel(aes(x = lon, y = lat, label = NAME_1, color = text_color), size = 1.5, box.padding = 0.1, max.overlaps = Inf, point.size = NA) +
  #geom_text(aes(x = lon, y = lat, label = NAME_1, color = text_color), size = 3, check_overlap = FALSE) +
  scale_color_identity() +
  scale_fill_viridis_c(
    direction = -1, 
    option = "mako", 
    name = "")+
  #breaks = seq(0, max(kenya_map$count), by = 100)) + # Adjust "by" based on your data range) +
  theme_void() +  # Use theme_void() for a cleaner look
  labs(
    title = "Kenya",
    #caption = "Source: Your Data Source"  # Adjust as needed
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",  # Position legend at the bottom
    legend.key.size = unit(1, "cm"),       # Increase legend key size
    legend.text = element_text(size = 10, angle = 45),  # Reduce font size and rotate
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


# plot kenya and nigeria on top of each other
kenya_plot / nigeria_plot



###################################################
### code chunk number 5: kenya_ab_table
###################################################
#| eval = TRUE, 
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| strip.white=TRUE, 
#| results='asis'
df_afro_kenya_2019 <- read_sav("afrobarometer_kenya_2019.sav")  

df_afro_kenya <- df_afro_kenya_2019 |> 
  mutate(
    age = ifelse(Q1!=998|999|-1, Q1, NA),
    is_male = ifelse(Q101==1, 1, 0),
    is_christian = case_when(Q98A == 1|
                               Q98A == 2|
                               Q98A == 3|
                               Q98A == 4|
                               Q98A == 5|
                               Q98A == 6|
                               Q98A == 7|
                               Q98A == 8|
                               Q98A == 9|
                               Q98A == 10|
                               Q98A == 11|
                               Q98A == 12|
                               Q98A == 13|
                               Q98A == 14|
                               Q98A == 15|
                               Q98A == 16|
                               Q98A == 17|
                               Q98A == 30|
                               Q98A == 31|
                               Q98A == 32|
                               Q98A == 33 ~ 1,
                             Q98A == 0|
                               Q98A == 18|
                               Q98A == 19|
                               Q98A == 20|
                               Q98A == 21|
                               Q98A == 22|
                               Q98A == 23|
                               Q98A == 24|
                               Q98A == 25|
                               Q98A == 26|
                               Q98A == 27|
                               Q98A == 28|
                               Q98A == 29|
                               Q98A == 34|
                               Q98A == 300|
                               Q98A == 9995 ~ 0,
                             TRUE ~ NA_real_),
    is_urban = case_when(URBRUR == 1 ~ 1,
                         URBRUR == 2 ~ 0,
                         TRUE ~ NA_real_),
    completed_secondary_school = case_when(Q97 == 0|
                                             Q97 == 1|
                                             Q97 == 2|
                                             Q97 == 3|
                                             Q97 == 4 ~ 0,
                                           Q97 == 5|
                                             Q97 == 6|
                                             Q97 == 7|
                                             Q97 == 8|
                                             Q97 == 9 ~ 1,
                                           TRUE ~ NA_real_),
    # assets_index = 
    #      # our assets: radio, tv, vehicle, computer, bank, phone (0-6) A-F in AB
    assets_index =  
      case_when(Q92A == 0 ~ 0,
                Q92A == 1|
                  Q92A == 2 ~ 1,
                TRUE ~ 0) +  
      case_when(Q92B == 0 ~ 0,
                Q92B == 1|
                  Q92B == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92C == 0 ~ 0,
                Q92C == 1|
                  Q92C == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92D == 0 ~ 0,
                Q92D == 1|
                  Q92D == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92E == 0 ~ 0,
                Q92E == 1|
                  Q92E == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92F == 0 ~ 0,
                Q92F == 1|
                  Q92F == 2 ~ 1,
                TRUE ~ 0),
    has_job = case_when(Q95A == 0|
                          Q95A == 1 ~ 0,
                        Q95A == 2|
                          Q95A == 3 ~ 1,
                        TRUE ~ NA_real_),
    voted = case_when(Q13 == 0 ~ 0,
                      Q13 == 3 ~ 1,
                      TRUE ~ NA_real_),
    party_aligned = case_when(Q91B == 305 ~ 1,
                              Q91B != 305 ~ 0,
                              TRUE ~ NA_real_)) |>
  summarise(
    avg_age = as.numeric(sub("0+$", "", mean(age, na.rm=TRUE))),
    se_age = sd(age, na.rm = TRUE)/sqrt(sum(!is.na(age))),
    
    pct_female = 1 - mean(is_male, na.rm=TRUE), 
    se_female = sd(is_male, na.rm = TRUE)/sqrt(sum(!is.na(is_male))),
    
    pct_christian = mean(is_christian, na.rm=TRUE), 
    se_christian = sd(is_christian, na.rm = TRUE)/sqrt(sum(!is.na(is_christian))),
    
    pct_urban  = mean(is_urban, na.rm=TRUE), 
    se_urban  = sd(is_urban, na.rm = TRUE)/sqrt(sum(!is.na(is_urban))),
    
    pct_completed_secondary_school = mean(completed_secondary_school, na.rm=TRUE), 
    se_completed_secondary_school = sd(completed_secondary_school, na.rm = TRUE)/sqrt(sum(!is.na(completed_secondary_school))),
    
    avg_assets_index = mean(assets_index, na.rm = TRUE),
    se_assets_index = sd(assets_index, na.rm = TRUE)/sqrt(sum(!is.na(assets_index))),
    
    pct_has_job = mean(has_job, na.rm=TRUE), 
    se_has_job = sd(has_job, na.rm = TRUE)/sqrt(sum(!is.na(has_job))),
    
    pct_voted = mean(voted, na.rm=TRUE),
    se_voted  = sd(voted, na.rm = TRUE)/sqrt(sum(!is.na(voted))),
    
    pct_party_aligned = mean(party_aligned, na.rm=TRUE),
    se_party_aligned  = sd(party_aligned, na.rm = TRUE)/sqrt(sum(!is.na(party_aligned)))
  ) |> t()


label_rows <- c('avg_age' = 'Age',
                'pct_female' = '% Non-male',
                'pct_christian' = '% Christian', 
                'pct_urban' = '% Urban', 
                'pct_completed_secondary_school' = '% Completed secondary school', 
                'avg_assets_index' = 'Assets index (0:6)', 
                'pct_has_job' = '% Employed', 
                'pct_voted' = '% Voted last election',
                'pct_party_aligned' = '% Party aligned')

rownames(df_afro_kenya)[match(names(label_rows), 
                              rownames(df_afro_kenya))] <- label_rows

# round to 3 digits
df_afro_kenya <- round(df_afro_kenya,3)

# add ( ) to se rows
df_afro_kenya[,1] <- ifelse(startsWith(rownames(df_afro_kenya), "se_"), paste0("(", round(df_afro_kenya,3), ")"), df_afro_kenya)

row.names(df_afro_kenya)[grep('se_', row.names(df_afro_kenya))] <- ''


## survey data - kenya ##

ab_covars <- c( 
  #demographics
  'age', 'is_female', 'religion_christian', 
  #SES
  'is_urban', 'completed_secondary_school', 'assets_index', 
  'has_job',
  'voted','party_aligned')

df_dems <- df_eval |> 
  filter(country1 == "kenya") |>
  mutate(is_female = 1*(is_male==0)) |>
  summarise(across(all_of(ab_covars),
                   .fns = list(mean = ~ sprintf('%.3f', mean(., na.rm = TRUE)),
                               se = ~ sprintf('(%.3f)', 
                                              sd(., na.rm = TRUE)/sqrt(sum(!is.na(.))))),
                   .names = '{.fn}_{.col}'
  )) |> t()

# remove "se" from names
row.names(df_dems)[grep('se_', row.names(df_dems))] <- ''

label_rows <- c('mean_age' = 'Age',
                'mean_is_female' = '% Non-male',
                'mean_religion_christian' = '% Christian', 
                'mean_is_urban' = '% Urban', 
                'mean_completed_secondary_school' = '% Completed secondary school', 
                'mean_assets_index' = 'Assets index (0:6)', 
                'mean_has_job' = '% Employed', 
                'mean_voted' = '% Voted last election',
                'mean_party_aligned' = '% Party aligned')

rownames(df_dems)[match(names(label_rows), 
                        rownames(df_dems))] <- label_rows


## combine data sets ##
kenya_fbab <- cbind(df_dems,df_afro_kenya)

colnames(kenya_fbab) <- c("Study sample","Afrobarometer (2019)")

## kenya table ##

kbl(kenya_fbab,
    format = 'latex',
    caption= 'Kenya sample characteristics compared to Afrobarometer',
    align = 'c', booktabs = TRUE,
    linesep = '',
    toprule = '\\vspace{-2.5em} \\\\ \\toprule') |>
  kable_styling(latex_options = c('HOLD_position'))  |>
  pack_rows(NULL, 2, 3, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 4, 5, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 6, 7, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 8, 9, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 10, 11, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 12, 13, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |> 
  pack_rows(NULL, 14, 15, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 16, 17, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |> 
  pack_rows(NULL, 18, 19, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  footnote(general = paste0('\\\\footnotesize The study sample is users in the evaluation stage, $n = $ ',
                            prettyNum(eval_n, big.mark = ','),
                            ". Estimates are of means, produced from sample means and standard error of the sample mean."),
           escape = FALSE,
           threeparttable = TRUE,
           general_title = '')



###################################################
### code chunk number 6: nigeria_ab_table
###################################################
#| eval = TRUE, 
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| strip.white=TRUE, 
#| results='asis'
#| 
## nigeria ##

# load afrobarometer data
df_afro_nigeria_2021 <- read_sav("afrobarometer_nigeria_2021.sav")  



df_afro_nigeria <- df_afro_nigeria_2021 |> 
  mutate(
    age = ifelse(Q1!=998|999|-1, Q1, NA),
    is_male = ifelse(Q101==1, 1, 0),
    is_christian = case_when(Q98A == 1|
                               Q98A == 2|
                               Q98A == 3|
                               Q98A == 4|
                               Q98A == 5|
                               Q98A == 6|
                               Q98A == 7|
                               Q98A == 8|
                               Q98A == 9|
                               Q98A == 10|
                               Q98A == 11|
                               Q98A == 12|
                               Q98A == 13|
                               Q98A == 14|
                               Q98A == 15|
                               Q98A == 16|
                               Q98A == 17|
                               Q98A == 30|
                               Q98A == 31|
                               Q98A == 32|
                               Q98A == 33 ~ 1,
                             Q98A == 0|
                               Q98A == 18|
                               Q98A == 19|
                               Q98A == 20|
                               Q98A == 21|
                               Q98A == 22|
                               Q98A == 23|
                               Q98A == 24|
                               Q98A == 25|
                               Q98A == 26|
                               Q98A == 27|
                               Q98A == 28|
                               Q98A == 29|
                               Q98A == 34|
                               Q98A == 300|
                               Q98A == 9995 ~ 0,
                             TRUE ~ NA_real_),
    is_urban = case_when(URBRUR == 1 ~ 1,
                         URBRUR == 2 ~ 0,
                         TRUE ~ NA_real_),
    completed_secondary_school = case_when(Q97 == 0|
                                             Q97 == 1|
                                             Q97 == 2|
                                             Q97 == 3|
                                             Q97 == 4 ~ 0,
                                           Q97 == 5|
                                             Q97 == 6|
                                             Q97 == 7|
                                             Q97 == 8|
                                             Q97 == 9 ~ 1,
                                           TRUE ~ NA_real_),
    assets_index =
      # our assets: radio, tv, vehicle, computer, bank, phone, **bike not asked in nigeria survey**
      case_when(Q92A == 0 ~ 0,
                Q92A == 1|
                  Q92A == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92B == 0 ~ 0,
                Q92B == 1|
                  Q92B == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92C == 0 ~ 0,
                Q92C == 1|
                  Q92C == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92D == 0 ~ 0,
                Q92D == 1|
                  Q92D == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92E == 0 ~ 0,
                Q92E == 1|
                  Q92E == 2 ~ 1,
                TRUE ~ 0) +
      case_when(Q92F == 0 ~ 0,
                Q92F == 1|
                  Q92F == 2 ~ 1,
                TRUE ~ 0),
    has_job = case_when(Q95A == 0|
                          Q95A == 1 ~ 0,
                        Q95A == 2|
                          Q95A == 3 ~ 1,
                        TRUE ~ NA_real_),
    voted = case_when(Q13 == 0 ~ 0,
                      Q13 == 3 ~ 1,
                      TRUE ~ NA_real_),
    party_aligned = case_when(Q91B == 621 ~ 1,
                              Q91B != 621 ~ 0,
                              TRUE ~ NA_real_))|>
  summarise(
    avg_age = as.numeric(sub("0+$", "", mean(age, na.rm=TRUE))),
    se_age = sd(age, na.rm = TRUE)/sqrt(sum(!is.na(age))),
    
    pct_female = 1 - mean(is_male, na.rm=TRUE), 
    se_female = sd(is_male, na.rm = TRUE)/sqrt(sum(!is.na(is_male))),
    
    pct_christian = mean(is_christian, na.rm=TRUE), 
    se_christian = sd(is_christian, na.rm = TRUE)/sqrt(sum(!is.na(is_christian))),
    
    pct_urban  = mean(is_urban, na.rm=TRUE), 
    se_urban  = sd(is_urban, na.rm = TRUE)/sqrt(sum(!is.na(is_urban))),
    
    pct_completed_secondary_school = mean(completed_secondary_school, na.rm=TRUE), 
    se_completed_secondary_school = sd(completed_secondary_school, na.rm = TRUE)/sqrt(sum(!is.na(completed_secondary_school))),
    
    avg_assets_index = mean(assets_index, na.rm=TRUE),
    se_assets_index = sd(assets_index, na.rm = TRUE)/sqrt(sum(!is.na(assets_index))),
    
    pct_has_job = mean(has_job, na.rm=TRUE), 
    se_has_job = sd(has_job, na.rm = TRUE)/sqrt(sum(!is.na(has_job))),
    
    pct_voted = mean(voted, na.rm=TRUE),
    se_voted  = sd(voted, na.rm = TRUE)/sqrt(sum(!is.na(voted))),
    
    pct_party_aligned = mean(party_aligned, na.rm=TRUE),
    se_party_aligned  = sd(party_aligned, na.rm = TRUE)/sqrt(sum(!is.na(party_aligned)))
  ) |> t()


label_rows <- c('avg_age' = 'Age',
                'pct_female' = '% Non-male',
                'pct_christian' = '% Christian', 
                'pct_urban' = '% Urban', 
                'pct_completed_secondary_school' = '% Completed secondary school', 
                'avg_assets_index' = 'Assets index (0:6)', 
                'pct_has_job' = '% Employed', 
                'pct_voted' = '% Voted last election',
                'pct_party_aligned' = '% Party aligned')

rownames(df_afro_nigeria)[match(names(label_rows), 
                                rownames(df_afro_nigeria))] <- label_rows

# round to 3 digits
df_afro_nigeria <- round(df_afro_nigeria,3)

# add ( ) to se rows
df_afro_nigeria[,1] <- ifelse(startsWith(rownames(df_afro_nigeria), "se_"), paste0("(", round(df_afro_nigeria,3), ")"), df_afro_nigeria)

row.names(df_afro_nigeria)[grep('se_', row.names(df_afro_nigeria))] <- ''


## survey data - nigeria ##

ab_covars <- c( 
  #demographics
  'age', 'is_female', 'religion_christian', 
  #SES
  'is_urban', 'completed_secondary_school', 'assets_index', 
  'has_job',
  'voted','party_aligned')

df_dems <- df_eval |> 
  filter(country1 == "nigeria") |>
  mutate(is_female = 1*(is_male==0)) |>
  summarise(across(all_of(ab_covars),
                   .fns = list(mean = ~ sprintf('%.3f', mean(., na.rm = TRUE)),
                               se = ~ sprintf('(%.3f)', 
                                              sd(., na.rm = TRUE)/sqrt(sum(!is.na(.))))),
                   .names = '{.fn}_{.col}'
  )) |> t()

# remove "se" from names
row.names(df_dems)[grep('se_', row.names(df_dems))] <- ''

label_rows <- c('mean_age' = 'Age',
                'mean_is_female' = '% Non-male',
                'mean_religion_christian' = '% Christian', 
                'mean_is_urban' = '% Urban', 
                'mean_completed_secondary_school' = '% Completed secondary school', 
                'mean_assets_index' = 'Assets index (0:6)', 
                'mean_has_job' = '% Employed', 
                'mean_voted' = '% Voted last election',
                'mean_party_aligned' = '% Party aligned')

rownames(df_dems)[match(names(label_rows), 
                        rownames(df_dems))] <- label_rows


## combine data sets ##
nigeria_fbab <- cbind(df_dems,df_afro_nigeria)

colnames(nigeria_fbab) <- c("Study sample","Afrobarometer (2021)")

## nigeria table ##

kbl(nigeria_fbab,
    format = 'latex',
    caption= 'Kenya sample characteristics compared to Afrobarometer',
    align = 'c', booktabs = TRUE,
    linesep = '',
    toprule = '\\vspace{-2.5em} \\\\ \\toprule') |>
  kable_styling(latex_options = c('HOLD_position'))  |>
  pack_rows(NULL, 2, 3, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 4, 5, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 6, 7, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 8, 9, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 10, 11, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 12, 13, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |> 
  pack_rows(NULL, 14, 15, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 16, 17, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |> 
  pack_rows(NULL, 18, 19, bold = FALSE, latex_gap_space = '-1.65\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  footnote(general = paste0('\\\\footnotesize The study sample is users in the evaluation stage, $n = $ ',
                            prettyNum(eval_n, big.mark = ','),
                            ". Estimates are of means, produced from sample means and standard error of the sample mean."),
           escape = FALSE,
           threeparttable = TRUE,
           general_title = '')



###################################################
### code chunk number 7: intake_vaccines_calcs
###################################################
#| eval = TRUE, 
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| reults = 'asis'

# load data

# create dataframes for figures
df <- read.csv("clean_vaxintake_data.csv")

vaccinations_kenya <- df |> 
  filter(country1 == "kenya") |> 
  drop_na(vax_status_numeric) |> 
  mutate(count = 1,
         total_count = n()) |> 
  group_by(vax_status_numeric) |> 
  summarise(percent = round(sum(count / total_count) * 100, digits=1),
            count = paste(paste(" (n=", prettyNum(sum(count), big.mark = ','),sep=""),")",sep=""),sep="") |> 
  mutate(vax_status = paste(as.character(vax_status_numeric),count,sep="")) |> 
  select(percent, vax_status)

vaccinations_nigeria <- df |> 
  filter(country1 == "nigeria") |> 
  drop_na(vax_status_numeric) |> 
  mutate(count = 1,
         total_count = n()) |> 
  group_by(vax_status_numeric) |> 
  summarise(percent = round(sum(count / total_count) * 100, digits=1),
            count = paste(paste(" (n=", prettyNum(sum(count), big.mark = ','),sep=""),")",sep=""),sep="") |> 
  mutate(vax_status = paste(as.character(vax_status_numeric),count,sep="")) |> 
  select(percent, vax_status)


###################################################
### code chunk number 8: intake_vaccines
###################################################
#| eval = TRUE, 
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| fig.align='center', 
#| fig.height=5.5, 
#| fig.cap=
#| paste0('\\textbf{Adaptive assignment in the learning stage.} ', 
#| 'The algorithm updates separately within each concern category. ',
#| 'The sample is concerns expressed by users in the learning stage, $n = $ ',
#| prettyNum(nrow(df_concerns), big.mark = ','),
#| '; up to three concerns are accounted for from each of the ',
#| prettyNum(learn_n, big.mark = ','),
#| ' users in the learning stage.'),
#| 
#| fig.cap=
#| paste0('\\textbf{Vaccine doses for all respondents at intake survey. }', 
#| 'The sample is all users who completed the intake survey, $n = $ ',
#| prettyNum(sum(table(df$vax_status_numeric)), big.mark = ','),
#| '.'),
#| prefix.string = 'fig',
#| results='asis'

# kenya plot

in.kenya <- ggplot(vaccinations_kenya, aes(x=vax_status, y=percent)) + 
  geom_bar(color="000000", fill="steelblue", stat="identity") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label=paste0(percent,"%")), vjust=-.5, size=2.5) +
  xlab("Number of Doses") +
  ylab("Percent of respodnents") +
  ggtitle("COVID-19 Vaccination among intake respondents: Kenya") +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(t = 10, r = 60, b = 10, l = 10),
    plot.title = element_text(size = 10, hjust = .3),
    text = element_text(size = 10))


# nigeria plot

in.nigeria <- ggplot(vaccinations_nigeria, aes(x=vax_status, y=percent)) + 
  geom_bar(color="000000", fill="steelblue", stat="identity") +
  scale_y_continuous(limits = c(0, 100)) +
  geom_text(aes(label=paste0(percent,"%")), vjust=-.5, size=2.5) +
  xlab("Number of Doses") +
  ylab("Percent of respondents") +
  ggtitle("COVID-19 Vaccination among intake respondents: Nigeria") +
  theme(
    panel.background = element_blank(),
    plot.margin = margin(t = 10, r = 60, b = 10, l = 10),
    plot.title = element_text(size = 10, hjust = .3),
    text = element_text(size = 10))

in.kenya/in.nigeria


###################################################
### code chunk number 9: joint_concerns_figure
###################################################
#| eval = TRUE, 
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| fig.align='center', 
#| fig.width=12, fig.height=8,
#| prefix.string = 'fig',
#| fig.cap=
#| paste0('\\textbf{Adaptive assignment in the learning stage.} ', 
#| 'The algorithm updates separately within each concern category. ',
#| 'The sample is concerns expressed by users in the learning stage, $n = $ ',
#| prettyNum(nrow(df_concerns), big.mark = ','),
#| '; up to three concerns are accounted for from each of the ',
#| prettyNum(learn_n, big.mark = ','),
#| ' users in the learning stage.'),
#| strip.white=TRUE,
#| results='asis'
messages <- c(`1` = 'Risks',
              `2` = 'Benefits',
              `3` = 'Elite cues\n(political leaders)',
              `4` = 'Elite cues\n(WHO)',
              `5` = 'Elite cues\n(healthcare)',
              `6` = 'Elite cues\n(religious leaders)',
              `7` = 'Misinfo\n(Facebook tips)',
              `8` = 'Misinfo\n(AfricaCheck video)',
              `9` = 'Pledge',
              `10` = 'Misinfo\n(accuracy prime)',
              `11` = 'Misinfo\n(7 types)',
              `12` = 'Social considerations',
              `13` = 'How vaccines work',
              `14` = 'Vaccine\ndevelopment/approvals',
              `15` = 'Vaccine\nsafety/effectiveness',
              `16` = 'Vaccine side effects\n(mild)',
              `17` = 'Vaccine side effects\n(debunk)')

concernsl <- list(`1` = 'Side effects',
                  `2` = c('Vaccine does', 'not work'),
                  `3` = c('COVID is', 'not real'),
                  `4` = c('Protected', 'by God'),
                  `5` = c('Do not trust', 'healthcare', 'workers'),
                  `6` = c('Do not trust', 'government'),
                  `7` = c('Not sure what', 'to believe'))

concerns <- c(`1` = 'Side effects',
              `2` = 'Vaccine does not work',
              `3` = 'COVID is not real',
              `4` = 'Protected by God',
              `5` = 'Do not trust healthcare workers',
              `6` = 'Do not trust government',
              `7` = 'Not sure what to believe')

df_concerns$count <- ave(rep(1,nrow(df_concerns)), df_concerns$treatment_id,
                         df_concerns$concern_id, FUN = cumsum) # for graph
out <- list()
for(cid in sort(unique(df_concerns$concern_id)) ){
  ddfh <- df_concerns[which(df_concerns$concern_id == cid),]
  ddfh$time <- 1:nrow(ddfh)
  out[[cid]] <- ggplot(ddfh, aes(x = time, y = count, color = as.factor(treatment_id))) +
    geom_line() +
    facet_grid(~concern_id,
               labeller = labeller(concern_id = concerns)) + 
    scale_color_manual(name = 'Message id', labels = as_labeller(messages),
                       values = cbPalette[-c(1, 5)]) + 
    # ggtitle(concerns[cid]) + 
    coord_cartesian(ylim = c(0, ceiling(max(df_concerns$count)/100)*100), 
                    xlim = c(0, 1800)) +
    vcf_theme() +
    theme(legend.position = c(0.28,0.7),
          legend.background=element_blank(),
          legend.title=element_blank(), 
          legend.key = element_rect(fill = NA),
          legend.text=element_text(size=8.5),
          axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          plot.background = element_rect(fill='transparent', 
                                         color = 'transparent'),
          plot.margin = margin(0, -10, 0, 0, "pt"))
  
  if(cid < 5){
    out[[cid]] <- out[[cid]] + 
      theme(axis.text.x=element_text(colour = '#FFFFFF00'),
            axis.ticks.x=element_line(colour = '#FFFFFF00'))
  }
  
  if(cid %in% c(2,3,5,6)){
    out[[cid]] <- out[[cid]] + 
      theme(axis.text.y=element_text(colour = "#FFFFFF00"),
            axis.ticks.y=element_line(colour = "#FFFFFF00"))
  }
}

bottom_text <- grid::textGrob('Time', gp = grid::gpar(fontsize = 18))
left_text <- grid::textGrob('Cumulative assignment', gp = grid::gpar(fontsize = 18), rot = 90, vjust = -0.4)

g <- gridExtra::grid.arrange(grobs = out, 
                             left = left_text, 
                             bottom = bottom_text, 
                             vp=grid::viewport(width=0.95, height=0.95))

grid::grid.draw(g)


###################################################
### code chunk number 10: learning
###################################################
#| eval = TRUE, 
#| echo=FALSE, warning=FALSE, message=FALSE, 
#| strip.white=TRUE,
#| results='asis'
# Response estimates
hj_out <- by(df_concerns[, c('success', 'prob', 'treatment_id', 'concern_id')], 
             list(treatment.concern = 
                    interaction(df_concerns$treatment_id, 
                                df_concerns$concern_id, drop = TRUE)), 
             function(dd) {
               y <- dd$success
               probs <- dd$prob
               numer <- y/probs
               denom <- sum(1/probs)
               c(hj = sum(numer/denom), 
                 sm = mean(y, na.rm = TRUE),
                 count = length(y), message = mean(dd$treatment_id), concern = mean(dd$concern_id))
             },
             simplify = FALSE)
hj_out <- data.frame(do.call(rbind, hj_out))
hj_out <- hj_out[order(hj_out[,'concern'], -hj_out[,'hj']),]

# primary result estimates for each treatment x concern consideration, hajek and sample mean

results_df <- data.frame(
  Messaging = messages[hj_out[,'message']],
  Concern = hj_out[,'concern'],
  LS = hj_out[,'count'], # learning sample
  SM = hj_out[,'sm'], # sample mean
  HJ = hj_out[,'hj'], # hajek estimate
  # 'inclusion error', decision rule for including second best arm,
  IE = sqrt(hj_out[,'hj']*(1-hj_out[,'hj']))*0.05, 
  check.names = FALSE
)

results_df <- results_df |> 
  group_by(Concern) |> 
  arrange(-HJ, .by_group = TRUE) |> 
  mutate(diff = HJ - first(HJ),
         group_id = seq_along(Concern),
         Selected = case_when((abs(diff) < first(IE) &
                                 group_id<=2) ~ 'Yes',
                              TRUE ~ 'No'),
         IE = case_when(group_id ==1 ~ IE, 
                        TRUE ~ NA),
         # Allow concern names to fill multiple rows
         Concern = c(concernsl[[first(Concern)]], 
                     rep('', length(Concern)-
                           length(concernsl[[first(Concern)]]))) ) |> 
  ungroup() |> 
  select(Concern, Messaging, LS, SM, HJ, 
         IE, Selected)

kbl(results_df, 
    caption= 'Learning from adaptive assignment.', 
    col.names = linebreak(c('Concern', 'Messaging', 'Leaning\nsample', 
                            'Sample\nmean', 'Hàjek\nestimate', 
                            'Inclusion\nerror', 'Selected'), 
                          align = c('l', 'r', 'c', 'c', 'c', 'c', 'c')),
    format = 'latex',
    digits = c(NA, NA, 0, 3,3,3,NA),
    toprule = '\\vspace{-3em} \\\\ \\toprule',
    align = 'lrrcccc', booktabs = TRUE,
    linesep = c( c( rep('', 3),  '\\addlinespace'),
                 c( rep('', 3),  '\\addlinespace'),
                 c( rep('', 3),  '\\addlinespace'),
                 c( rep('', 2),  '\\addlinespace'),
                 c( rep('', 4),  '\\addlinespace'),
                 c( rep('', 4),  '\\addlinespace'),
                 c( rep('', 4),  '\\addlinespace')),
    escape = FALSE)|> 
  kable_styling(full_width = FALSE, 
                latex_options = c('HOLD_position')) |> 
  footnote(general = paste0('\\\\scriptsize The sample is concerns expressed by users in the learning stage, $n = $ ',
                            prettyNum(nrow(df_concerns), big.mark = ','),'; up to three concerns are accounted for from each of the ',prettyNum(learn_n, big.mark = ','),' users in the learning stage. ' ,"The ``Learning sample'' column represents the number of concerns addressed by a given messaging condition for a given concern, as assigned by the adaptive algorithm. The ``Sample mean'' and ``Hàjek estimate'' columns are estimates of mean response under counterfactual messaging, produced from sample means and a stabilized inverse probability weighted estimator; response is a binary measure of whether the respondent affirmed that the given messaging addressed their stated concern. The ``Inclusion error'' column is $0.05\\\\times \\\\sqrt{\\\\hat p ( 1- \\\\hat p)}$, where $\\\\hat p$ is the Hàjek estimate of response. When the Hàjek estimate for the second best messaging is within the inclusion error of the best messaging, both options were selected to be randomly assigned in the evaluation phase, as pre-registered and reported in the ``Selected'' column."
  ),
  escape = FALSE,
  threeparttable = TRUE,
  general_title = '')


###################################################
### code chunk number 11: main_estimation
###################################################
#| eval = TRUE,
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

covariate_lin <- as.formula(paste('~' , paste0(covariate_list,
                                               collapse = ' + ')))
# Comparison to PSA
df_eval_alt <- df_eval |>
  mutate(treatment_group = relevel(treatment_group, ref = 'PSA'))
df_kenya_alt <- df_kenya |>
  mutate(treatment_group = relevel(treatment_group, ref = 'PSA'))
df_nigeria_alt <- df_nigeria |>
  mutate(treatment_group = relevel(treatment_group, ref = 'PSA'))

# Response function General
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_vax <- estimatr::lm_robust(response ~ treatment_group, data = df_eval)
est_DM_vax_alt <- estimatr::lm_robust(response ~ treatment_group,
                                      data = df_eval_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_vax <- estimatr::lm_robust(response_imputed ~ treatment_group,
                                   data = df_eval)
est_IMP_vax_alt <- estimatr::lm_robust(response_imputed ~ treatment_group,
                                       data = df_eval_alt)


## C) Generalized random forest
est_GRF_vax <- mcf_estimate(df_eval, 'response', covariates = covariate_list)
est_GRF_vax_alt <- mcf_estimate(df_eval_alt, 'response',
                                covariates = covariate_list)

## D) IP Weighting
est_IP_vax <- IP_estimate(df_eval, 'response',
                          covariates = covariate_list)[['ATE']]
est_IP_vax_alt <- IP_estimate(df_eval_alt, 'response',
                              covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_vax <- estimatr::lm_lin(response ~ treatment_group, data = df_eval,
                                covariates = covariate_lin)
est_Lin_vax_alt <- estimatr::lm_lin(response ~ treatment_group,
                                    data = df_eval_alt,
                                    covariates = covariate_lin)


# Response function Kenya
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_kenya <- estimatr::lm_robust(response ~ treatment_group, data = df_kenya)
est_DM_kenya_alt <- estimatr::lm_robust(response ~ treatment_group,
                                        data = df_kenya_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_kenya <- estimatr::lm_robust(response_imputed ~ treatment_group,
                                     data = df_kenya)
est_IMP_kenya_alt <- estimatr::lm_robust(response_imputed ~ treatment_group,
                                         data = df_kenya_alt)

## C) Generalized random forest
est_GRF_kenya <- mcf_estimate(df_kenya, 'response', covariates = covariate_list)
est_GRF_kenya_alt <- mcf_estimate(df_kenya_alt, 'response',
                                  covariates = covariate_list)

## D) IP Weighting
est_IP_kenya <- IP_estimate(df_kenya, 'response',
                            covariates = covariate_list)[['ATE']]
est_IP_kenya_alt <- IP_estimate(df_kenya_alt, 'response',
                                covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_kenya <- estimatr::lm_lin(response ~ treatment_group, data = df_kenya,
                                  covariates = covariate_lin)
est_Lin_kenya_alt <- estimatr::lm_lin(response ~ treatment_group,
                                      data = df_kenya_alt,
                                      covariates = covariate_lin)


# Response function Nigeria
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_nigeria <- estimatr::lm_robust(response ~ treatment_group, data = df_nigeria)
est_DM_nigeria_alt <- estimatr::lm_robust(response ~ treatment_group,
                                          data = df_nigeria_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_nigeria <- estimatr::lm_robust(response_imputed ~ treatment_group,
                                       data = df_nigeria)
est_IMP_nigeria_alt <- estimatr::lm_robust(response_imputed ~ treatment_group,
                                           data = df_nigeria_alt)

## C) Generalized random forest
est_GRF_nigeria <- mcf_estimate(df_nigeria, 'response', covariates = covariate_list)
est_GRF_nigeria_alt <- mcf_estimate(df_nigeria_alt, 'response',
                                    covariates = covariate_list)

## D) IP Weighting
est_IP_nigeria <- IP_estimate(df_nigeria, 'response',
                              covariates = covariate_list)[['ATE']]
est_IP_nigeria_alt <- IP_estimate(df_nigeria_alt, 'response',
                                  covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_nigeria <- estimatr::lm_lin(response ~ treatment_group, data = df_nigeria,
                                    covariates = covariate_lin)
est_Lin_nigeria_alt <- estimatr::lm_lin(response ~ treatment_group,
                                        data = df_nigeria_alt,
                                        covariates = covariate_lin)


###################################################
### code chunk number 12: intention_estimation
###################################################
#| eval = TRUE,
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

# Response function General
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_vax_i <- estimatr::lm_robust(get_vaccinated_2 ~ treatment_group, data = df_eval)
est_DM_vax_alt_i <- estimatr::lm_robust(get_vaccinated_2 ~ treatment_group,
                                        data = df_eval_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_vax_i <- estimatr::lm_robust(get_vaccinated_2_imputed ~ treatment_group,
                                     data = df_eval)
est_IMP_vax_alt_i <- estimatr::lm_robust(get_vaccinated_2_imputed ~ treatment_group,
                                         data = df_eval_alt)


## C) Generalized random forest
est_GRF_vax_i <- mcf_estimate(df_eval, 'get_vaccinated_2', covariates = covariate_list)
est_GRF_vax_alt_i <- mcf_estimate(df_eval_alt, 'get_vaccinated_2',
                                  covariates = covariate_list)

## D) IP Weighting
est_IP_vax_i <- IP_estimate(df_eval, 'get_vaccinated_2',
                            covariates = covariate_list)[['ATE']]
est_IP_vax_alt_i <- IP_estimate(df_eval_alt, 'get_vaccinated_2',
                                covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_vax_i <- estimatr::lm_lin(get_vaccinated_2 ~ treatment_group, data = df_eval,
                                  covariates = covariate_lin)
est_Lin_vax_alt_i <- estimatr::lm_lin(get_vaccinated_2 ~ treatment_group,
                                      data = df_eval_alt,
                                      covariates = covariate_lin)


# Response function Kenya
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_kenya_i <- estimatr::lm_robust(get_vaccinated_2 ~ treatment_group, data = df_kenya)
est_DM_kenya_alt_i <- estimatr::lm_robust(get_vaccinated_2 ~ treatment_group,
                                          data = df_kenya_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_kenya_i <- estimatr::lm_robust(get_vaccinated_2_imputed ~ treatment_group,
                                       data = df_kenya)
est_IMP_kenya_alt_i <- estimatr::lm_robust(get_vaccinated_2_imputed ~ treatment_group,
                                           data = df_kenya_alt)

## C) Generalized random forest
est_GRF_kenya_i <- mcf_estimate(df_kenya, 'get_vaccinated_2', covariates = covariate_list)
est_GRF_kenya_alt_i <- mcf_estimate(df_kenya_alt, 'get_vaccinated_2',
                                    covariates = covariate_list)

## D) IP Weighting
est_IP_kenya_i <- IP_estimate(df_kenya, 'get_vaccinated_2',
                              covariates = covariate_list)[['ATE']]
est_IP_kenya_alt_i <- IP_estimate(df_kenya_alt, 'get_vaccinated_2',
                                  covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_kenya_i <- estimatr::lm_lin(get_vaccinated_2 ~ treatment_group, data = df_kenya,
                                    covariates = covariate_lin)
est_Lin_kenya_alt_i <- estimatr::lm_lin(get_vaccinated_2 ~ treatment_group,
                                        data = df_kenya_alt,
                                        covariates = covariate_lin)


# Response function Nigeria
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_nigeria_i <- estimatr::lm_robust(get_vaccinated_2 ~ treatment_group, data = df_nigeria)
est_DM_nigeria_alt_i <- estimatr::lm_robust(get_vaccinated_2 ~ treatment_group,
                                            data = df_nigeria_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_nigeria_i <- estimatr::lm_robust(get_vaccinated_2_imputed ~ treatment_group,
                                         data = df_nigeria)
est_IMP_nigeria_alt_i <- estimatr::lm_robust(get_vaccinated_2_imputed ~ treatment_group,
                                             data = df_nigeria_alt)

## C) Generalized random forest
est_GRF_nigeria_i <- mcf_estimate(df_nigeria, 'get_vaccinated_2', covariates = covariate_list)
est_GRF_nigeria_alt_i <- mcf_estimate(df_nigeria_alt, 'get_vaccinated_2',
                                      covariates = covariate_list)

## D) IP Weighting
est_IP_nigeria_i <- IP_estimate(df_nigeria, 'get_vaccinated_2',
                                covariates = covariate_list)[['ATE']]
est_IP_nigeria_alt_i <- IP_estimate(df_nigeria_alt, 'get_vaccinated_2',
                                    covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_nigeria_i <- estimatr::lm_lin(get_vaccinated_2 ~ treatment_group, data = df_nigeria,
                                      covariates = covariate_lin)
est_Lin_nigeria_alt_i <- estimatr::lm_lin(get_vaccinated_2 ~ treatment_group,
                                          data = df_nigeria_alt,
                                          covariates = covariate_lin)


###################################################
### code chunk number 13: willing_estimation
###################################################
#| eval = TRUE,
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

# Response function General
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_vax_w <- estimatr::lm_robust(willingness_2 ~ treatment_group, data = df_eval)
est_DM_vax_alt_w <- estimatr::lm_robust(willingness_2 ~ treatment_group,
                                        data = df_eval_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_vax_w <- estimatr::lm_robust(willingness_2_imputed ~ treatment_group,
                                     data = df_eval)
est_IMP_vax_alt_w <- estimatr::lm_robust(willingness_2_imputed ~ treatment_group,
                                         data = df_eval_alt)


## C) Generalized random forest
est_GRF_vax_w <- mcf_estimate(df_eval, 'willingness_2', covariates = covariate_list)
est_GRF_vax_alt_w <- mcf_estimate(df_eval_alt, 'willingness_2',
                                  covariates = covariate_list)

## D) IP Weighting
est_IP_vax_w <- IP_estimate(df_eval, 'willingness_2',
                            covariates = covariate_list)[['ATE']]
est_IP_vax_alt_w <- IP_estimate(df_eval_alt, 'willingness_2',
                                covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_vax_w <- estimatr::lm_lin(willingness_2 ~ treatment_group, data = df_eval,
                                  covariates = covariate_lin)
est_Lin_vax_alt_w <- estimatr::lm_lin(willingness_2 ~ treatment_group,
                                      data = df_eval_alt,
                                      covariates = covariate_lin)


# Response function Kenya
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_kenya_w <- estimatr::lm_robust(willingness_2 ~ treatment_group, data = df_kenya)
est_DM_kenya_alt_w <- estimatr::lm_robust(willingness_2 ~ treatment_group,
                                          data = df_kenya_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_kenya_w <- estimatr::lm_robust(willingness_2_imputed ~ treatment_group,
                                       data = df_kenya)
est_IMP_kenya_alt_w <- estimatr::lm_robust(willingness_2_imputed ~ treatment_group,
                                           data = df_kenya_alt)

## C) Generalized random forest
est_GRF_kenya_w <- mcf_estimate(df_kenya, 'willingness_2', covariates = covariate_list)
est_GRF_kenya_alt_w <- mcf_estimate(df_kenya_alt, 'willingness_2',
                                    covariates = covariate_list)

## D) IP Weighting
est_IP_kenya_w <- IP_estimate(df_kenya, 'willingness_2',
                              covariates = covariate_list)[['ATE']]
est_IP_kenya_alt_w <- IP_estimate(df_kenya_alt, 'willingness_2',
                                  covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_kenya_w <- estimatr::lm_lin(willingness_2 ~ treatment_group, data = df_kenya,
                                    covariates = covariate_lin)
est_Lin_kenya_alt_w <- estimatr::lm_lin(willingness_2 ~ treatment_group,
                                        data = df_kenya_alt,
                                        covariates = covariate_lin)


# Response function Nigeria
## A) Difference-in-means estimate (i.e, treating missing data as random)
est_DM_nigeria_w <- estimatr::lm_robust(willingness_2 ~ treatment_group, data = df_nigeria)
est_DM_nigeria_alt_w <- estimatr::lm_robust(willingness_2 ~ treatment_group,
                                            data = df_nigeria_alt)

## B) Difference-in-means estimate , imputing pre-test as post-test
est_IMP_nigeria_w <- estimatr::lm_robust(willingness_2_imputed ~ treatment_group,
                                         data = df_nigeria)
est_IMP_nigeria_alt_w <- estimatr::lm_robust(willingness_2_imputed ~ treatment_group,
                                             data = df_nigeria_alt)

## C) Generalized random forest
est_GRF_nigeria_w <- mcf_estimate(df_nigeria, 'willingness_2', covariates = covariate_list)
est_GRF_nigeria_alt_w <- mcf_estimate(df_nigeria_alt, 'willingness_2',
                                      covariates = covariate_list)

## D) IP Weighting
est_IP_nigeria_w <- IP_estimate(df_nigeria, 'willingness_2',
                                covariates = covariate_list)[['ATE']]
est_IP_nigeria_alt_w <- IP_estimate(df_nigeria_alt, 'willingness_2',
                                    covariates = covariate_list)[['ATE']]

## E) Lin estimator
est_Lin_nigeria_w <- estimatr::lm_lin(willingness_2 ~ treatment_group, data = df_nigeria,
                                      covariates = covariate_lin)
est_Lin_nigeria_alt_w <- estimatr::lm_lin(willingness_2 ~ treatment_group,
                                          data = df_nigeria_alt,
                                          covariates = covariate_lin)


###################################################
### code chunk number 14: main_results_intention
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

# Combine model objects
out_list_i <- list(
  DM = est_DM_vax_i,
  IMP = est_IMP_vax_i,
  Lin = est_Lin_vax_i,
  IP = est_IP_vax_i
)

out_list_kenya_i <- list(
  DM = est_DM_kenya_i,
  IMP = est_IMP_kenya_i,
  Lin = est_Lin_kenya_i,
  IP = est_IP_kenya_i
)

out_list_nigeria_i <- list(
  DM = est_DM_nigeria_i,
  IMP = est_IMP_nigeria_i,
  Lin = est_Lin_nigeria_i,
  IP = est_IP_nigeria_i
)

out_list_i <- modelsummary(out_list_i,
                           output = 'modelsummary_list')
out_list_kenya_i <- modelsummary(out_list_kenya_i,
                                 output = 'modelsummary_list')
out_list_nigeria_i <- modelsummary(out_list_nigeria_i,
                                   output = 'modelsummary_list')

for(x in names(out_list_i)){
  # add on Concerns - PSA coefficients
  out_list_i[[x]]$tidy <- rbind(out_list_i[[x]]$tidy |>
                                  select(term, estimate, std.error, statistic,
                                         p.value),
                                broom::tidy(get(paste0('est_', x, '_vax_alt_i'))) |>
                                  filter(term == 'treatment_groupConcerns') |>
                                  mutate(term = 'treatment_groupConcerns2') |>
                                  select(term, estimate, std.error, statistic,
                                         p.value))
  
  out_list_kenya_i[[x]]$tidy <- rbind(out_list_kenya_i[[x]]$tidy |>
                                        select(term, estimate, std.error, statistic,
                                               p.value),
                                      broom::tidy(get(paste0('est_', x, '_kenya_alt_i'))) |>
                                        filter(term == 'treatment_groupConcerns') |>
                                        mutate(term = 'treatment_groupConcerns2') |>
                                        select(term, estimate, std.error, statistic,
                                               p.value))
  
  out_list_nigeria_i[[x]]$tidy <- rbind(out_list_nigeria_i[[x]]$tidy |>
                                          select(term, estimate, std.error, statistic,
                                                 p.value),
                                        broom::tidy(get(paste0('est_', x, '_nigeria_alt_i'))) |>
                                          filter(term == 'treatment_groupConcerns') |>
                                          mutate(term = 'treatment_groupConcerns2') |>
                                          select(term, estimate, std.error, statistic,
                                                 p.value))
  
  # drop p-values for control condition so no stars
  out_list_i[[x]]$tidy[which(out_list_i[[x]]$tidy$term == '(Intercept)'),
                       c('p.value', 'statistic')] <- NA
  out_list_kenya_i[[x]]$tidy[which(out_list_kenya_i[[x]]$tidy$term == '(Intercept)'),
                             c('p.value', 'statistic')] <- NA
  out_list_nigeria_i[[x]]$tidy[which(out_list_nigeria_i[[x]]$tidy$term == '(Intercept)'),
                               c('p.value', 'statistic')] <- NA
}

# Add GRF; different format
out_list_i[['GRF']] <- format_grf_summary(est_GRF_vax_i)
out_list_i[['GRF']]$tidy <- rbind(out_list_i[['GRF']]$tidy,
                                  format_grf_summary(est_GRF_vax_alt_i)$tidy |>
                                    filter(term == 'treatment_groupConcerns') |>
                                    mutate(term = 'treatment_groupConcerns2'))
out_list_i[['GRF']]$tidy[which(out_list_i[['GRF']]$tidy$term == '(Intercept)'),
                         c('p.value', 'statistic')] <- NA


out_list_kenya_i[['GRF']] <- format_grf_summary(est_GRF_kenya_i)
out_list_kenya_i[['GRF']]$tidy <- rbind(out_list_kenya_i[['GRF']]$tidy,
                                        format_grf_summary(est_GRF_kenya_alt_i)$tidy |>
                                          filter(term == 'treatment_groupConcerns') |>
                                          mutate(term = 'treatment_groupConcerns2'))
out_list_kenya_i[['GRF']]$tidy[which(out_list_kenya_i[['GRF']]$tidy$term == '(Intercept)'),
                               c('p.value', 'statistic')] <- NA

out_list_nigeria_i[['GRF']] <- format_grf_summary(est_GRF_nigeria_i)
out_list_nigeria_i[['GRF']]$tidy <- rbind(out_list_nigeria_i[['GRF']]$tidy,
                                          format_grf_summary(est_GRF_nigeria_alt_i)$tidy |>
                                            filter(term == 'treatment_groupConcerns') |>
                                            mutate(term = 'treatment_groupConcerns2'))
out_list_nigeria_i[['GRF']]$tidy[which(out_list_nigeria_i[['GRF']]$tidy$term == '(Intercept)'),
                                 c('p.value', 'statistic')] <- NA


modelsummary(list(Combined = out_list_i,
                  Kenya = out_list_kenya_i,
                  Nigeria = out_list_nigeria_i),
             shape= 'rbind',
             output = 'latex',
             coef_map = c('treatment_groupPSA' = 'PSA - Control',
                          'treatment_groupConcerns' = 'Concerns - Control',
                          'treatment_groupConcerns2' = 'Concerns - PSA',
                          '(Intercept)' = 'Control mean'),
             stars = TRUE ,
             gof_map = list(list('raw' = 'nobs',
                                 'clean' = 'n',
                                 'fmt' = f1)),
             escape = FALSE,
             title= 'Treatment effect estimates and response under alternative approaches to estimation, intention measure.') |>
  kable_styling(latex_options = 'HOLD_position') |>
  footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage. The response measure is the vaccine intention measure. Estimates are average treatment effects, and control mean. Estimating procedures are discussed in the text. Statistical significance is reported only for treatment effect estimates, not for control means.'
  ),
  escape = FALSE,
  threeparttable = TRUE,
  general_title = '')


###################################################
### code chunk number 15: main_results_willingness
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

# Combine model objects
out_list_w <- list(
  DM = est_DM_vax_w,
  IMP = est_IMP_vax_w,
  Lin = est_Lin_vax_w,
  IP = est_IP_vax_w
)

out_list_kenya_w <- list(
  DM = est_DM_kenya_w,
  IMP = est_IMP_kenya_w,
  Lin = est_Lin_kenya_w,
  IP = est_IP_kenya_w
)

out_list_nigeria_w <- list(
  DM = est_DM_nigeria_w,
  IMP = est_IMP_nigeria_w,
  Lin = est_Lin_nigeria_w,
  IP = est_IP_nigeria_w
)

out_list_w <- modelsummary(out_list_w,
                           output = 'modelsummary_list')
out_list_kenya_w <- modelsummary(out_list_kenya_w,
                                 output = 'modelsummary_list')
out_list_nigeria_w <- modelsummary(out_list_nigeria_w,
                                   output = 'modelsummary_list')

for(x in names(out_list_w)){
  # add on Concerns - PSA coefficients
  out_list_w[[x]]$tidy <- rbind(out_list_w[[x]]$tidy |>
                                  select(term, estimate, std.error, statistic,
                                         p.value),
                                broom::tidy(get(paste0('est_', x, '_vax_alt_w'))) |>
                                  filter(term == 'treatment_groupConcerns') |>
                                  mutate(term = 'treatment_groupConcerns2') |>
                                  select(term, estimate, std.error, statistic,
                                         p.value))
  
  out_list_kenya_w[[x]]$tidy <- rbind(out_list_kenya_w[[x]]$tidy |>
                                        select(term, estimate, std.error, statistic,
                                               p.value),
                                      broom::tidy(get(paste0('est_', x, '_kenya_alt_w'))) |>
                                        filter(term == 'treatment_groupConcerns') |>
                                        mutate(term = 'treatment_groupConcerns2') |>
                                        select(term, estimate, std.error, statistic,
                                               p.value))
  
  out_list_nigeria_w[[x]]$tidy <- rbind(out_list_nigeria_w[[x]]$tidy |>
                                          select(term, estimate, std.error, statistic,
                                                 p.value),
                                        broom::tidy(get(paste0('est_', x, '_nigeria_alt_w'))) |>
                                          filter(term == 'treatment_groupConcerns') |>
                                          mutate(term = 'treatment_groupConcerns2') |>
                                          select(term, estimate, std.error, statistic,
                                                 p.value))
  
  # drop p-values for control condition so no stars
  out_list_w[[x]]$tidy[which(out_list_w[[x]]$tidy$term == '(Intercept)'),
                       c('p.value', 'statistic')] <- NA
  out_list_kenya_w[[x]]$tidy[which(out_list_kenya_w[[x]]$tidy$term == '(Intercept)'),
                             c('p.value', 'statistic')] <- NA
  out_list_nigeria_w[[x]]$tidy[which(out_list_nigeria_w[[x]]$tidy$term == '(Intercept)'),
                               c('p.value', 'statistic')] <- NA
}

# Add GRF; different format
out_list_w[['GRF']] <- format_grf_summary(est_GRF_vax_w)
out_list_w[['GRF']]$tidy <- rbind(out_list_w[['GRF']]$tidy,
                                  format_grf_summary(est_GRF_vax_alt_w)$tidy |>
                                    filter(term == 'treatment_groupConcerns') |>
                                    mutate(term = 'treatment_groupConcerns2'))
out_list_w[['GRF']]$tidy[which(out_list_w[['GRF']]$tidy$term == '(Intercept)'),
                         c('p.value', 'statistic')] <- NA


out_list_kenya_w[['GRF']] <- format_grf_summary(est_GRF_kenya_w)
out_list_kenya_w[['GRF']]$tidy <- rbind(out_list_kenya_w[['GRF']]$tidy,
                                        format_grf_summary(est_GRF_kenya_alt_w)$tidy |>
                                          filter(term == 'treatment_groupConcerns') |>
                                          mutate(term = 'treatment_groupConcerns2'))
out_list_kenya_w[['GRF']]$tidy[which(out_list_kenya_w[['GRF']]$tidy$term == '(Intercept)'),
                               c('p.value', 'statistic')] <- NA

out_list_nigeria_w[['GRF']] <- format_grf_summary(est_GRF_nigeria_w)
out_list_nigeria_w[['GRF']]$tidy <- rbind(out_list_nigeria_w[['GRF']]$tidy,
                                          format_grf_summary(est_GRF_nigeria_alt_w)$tidy |>
                                            filter(term == 'treatment_groupConcerns') |>
                                            mutate(term = 'treatment_groupConcerns2'))
out_list_nigeria_w[['GRF']]$tidy[which(out_list_nigeria_w[['GRF']]$tidy$term == '(Intercept)'),
                                 c('p.value', 'statistic')] <- NA


modelsummary(list(Combined = out_list_w,
                  Kenya = out_list_kenya_w,
                  Nigeria = out_list_nigeria_w),
             shape= 'rbind',
             output = 'latex',
             coef_map = c('treatment_groupPSA' = 'PSA - Control',
                          'treatment_groupConcerns' = 'Concerns - Control',
                          'treatment_groupConcerns2' = 'Concerns - PSA',
                          '(Intercept)' = 'Control mean'),
             stars = TRUE ,
             gof_map = list(list('raw' = 'nobs',
                                 'clean' = 'n',
                                 'fmt' = f1)),
             escape = FALSE,
             title= 'Treatment effect estimates and response under alternative approaches to estimation, willingness measure.') |>
  kable_styling(latex_options = c('HOLD_position')) |>
  footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage. The response measure is the vaccine willingness measure. Estimates are average treatment effects, and control mean. Estimating procedures are discussed in the text. Statistical significance is reported only for treatment effect estimates, not for control means.'
  ),
  escape = FALSE,
  threeparttable = TRUE,
  general_title = '')


###################################################
### code chunk number 16: main_results_index
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

# Combine model objects
out_list <- list(
  DM = est_DM_vax,
  IMP = est_IMP_vax,
  Lin = est_Lin_vax,
  IP = est_IP_vax
)

out_list_kenya <- list(
  DM = est_DM_kenya,
  IMP = est_IMP_kenya,
  Lin = est_Lin_kenya,
  IP = est_IP_kenya
)

out_list_nigeria <- list(
  DM = est_DM_nigeria,
  IMP = est_IMP_nigeria,
  Lin = est_Lin_nigeria,
  IP = est_IP_nigeria
)

out_list <- modelsummary(out_list,
                         output = 'modelsummary_list')
out_list_kenya <- modelsummary(out_list_kenya,
                               output = 'modelsummary_list')
out_list_nigeria <- modelsummary(out_list_nigeria,
                                 output = 'modelsummary_list')

for(x in names(out_list)){
  # add on Concerns - PSA coefficients
  out_list[[x]]$tidy <- rbind(out_list[[x]]$tidy |>
                                select(term, estimate, std.error, statistic,
                                       p.value),
                              broom::tidy(get(paste0('est_', x, '_vax_alt'))) |>
                                filter(term == 'treatment_groupConcerns') |>
                                mutate(term = 'treatment_groupConcerns2') |>
                                select(term, estimate, std.error, statistic,
                                       p.value))
  
  out_list_kenya[[x]]$tidy <- rbind(out_list_kenya[[x]]$tidy |>
                                      select(term, estimate, std.error, statistic,
                                             p.value),
                                    broom::tidy(get(paste0('est_', x, '_kenya_alt'))) |>
                                      filter(term == 'treatment_groupConcerns') |>
                                      mutate(term = 'treatment_groupConcerns2') |>
                                      select(term, estimate, std.error, statistic,
                                             p.value))
  
  out_list_nigeria[[x]]$tidy <- rbind(out_list_nigeria[[x]]$tidy |>
                                        select(term, estimate, std.error, statistic,
                                               p.value),
                                      broom::tidy(get(paste0('est_', x, '_nigeria_alt'))) |>
                                        filter(term == 'treatment_groupConcerns') |>
                                        mutate(term = 'treatment_groupConcerns2') |>
                                        select(term, estimate, std.error, statistic,
                                               p.value))
  
  # drop p-values for control condition so no stars
  out_list[[x]]$tidy[which(out_list[[x]]$tidy$term == '(Intercept)'),
                     c('p.value', 'statistic')] <- NA
  out_list_kenya[[x]]$tidy[which(out_list_kenya[[x]]$tidy$term == '(Intercept)'),
                           c('p.value', 'statistic')] <- NA
  out_list_nigeria[[x]]$tidy[which(out_list_nigeria[[x]]$tidy$term == '(Intercept)'),
                             c('p.value', 'statistic')] <- NA
}

# Add GRF; different format
out_list[['GRF']] <- format_grf_summary(est_GRF_vax)
out_list[['GRF']]$tidy <- rbind(out_list[['GRF']]$tidy,
                                format_grf_summary(est_GRF_vax_alt)$tidy |>
                                  filter(term == 'treatment_groupConcerns') |>
                                  mutate(term = 'treatment_groupConcerns2'))
out_list[['GRF']]$tidy[which(out_list[['GRF']]$tidy$term == '(Intercept)'),
                       c('p.value', 'statistic')] <- NA


out_list_kenya[['GRF']] <- format_grf_summary(est_GRF_kenya)
out_list_kenya[['GRF']]$tidy <- rbind(out_list_kenya[['GRF']]$tidy,
                                      format_grf_summary(est_GRF_kenya_alt)$tidy |>
                                        filter(term == 'treatment_groupConcerns') |>
                                        mutate(term = 'treatment_groupConcerns2'))
out_list_kenya[['GRF']]$tidy[which(out_list_kenya[['GRF']]$tidy$term == '(Intercept)'),
                             c('p.value', 'statistic')] <- NA

out_list_nigeria[['GRF']] <- format_grf_summary(est_GRF_nigeria)
out_list_nigeria[['GRF']]$tidy <- rbind(out_list_nigeria[['GRF']]$tidy,
                                        format_grf_summary(est_GRF_nigeria_alt)$tidy |>
                                          filter(term == 'treatment_groupConcerns') |>
                                          mutate(term = 'treatment_groupConcerns2'))
out_list_nigeria[['GRF']]$tidy[which(out_list_nigeria[['GRF']]$tidy$term == '(Intercept)'),
                               c('p.value', 'statistic')] <- NA


modelsummary(list(Combined = out_list,
                  Kenya = out_list_kenya,
                  Nigeria = out_list_nigeria),
             shape= 'rbind',
             output = 'latex',
             coef_map = c('treatment_groupPSA' = 'PSA - Control',
                          'treatment_groupConcerns' = 'Concerns - Control',
                          'treatment_groupConcerns2' = 'Concerns - PSA',
                          '(Intercept)' = 'Control mean'),
             stars = TRUE ,
             gof_map = list(list('raw' = 'nobs',
                                 'clean' = 'n',
                                 'fmt' = f1)),
             escape = FALSE,
             title= 'Treatment effect estimates and response under alternative approaches to estimation, combined response function.') |>
  kable_styling(latex_options = c('HOLD_position')) |>
  footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage. The response measure is the combined index. Estimates are average treatment effects, and control mean. Estimating procedures are discussed in the text. Statistical significance is reported only for treatment effect estimates, not for control means.'
  ),
  escape = FALSE,
  threeparttable = TRUE,
  general_title = '')


###################################################
### code chunk number 17: secondary_estimation
###################################################
#| eval = TRUE,
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

## outcomes of interest ##
## 1. sharing: (0,.5,1) - proportion of stimuli wanted to share (messenger and timeline)
# true: "share_true_scaled"
# false: "share_false_scaled"
## 2. Information seeking
# "pre_register_imputed"
df_eval$pre_register <- df_eval$pre_register_imputed
## 3. encouraging others - post a WHO pro-vaccine msg on their timeline
# "share_post_numeric"
## 4. vaccination status (from follow up survey)
# "vaxed_f_numeric" / "vaxed_f_binary"

# Function for each model:

run_DM <- function(outcome, data, data_alt) {
  est_reg <- estimatr::lm_robust(as.formula(paste0(outcome, " ~ treatment_group")), data = data)
  est_reg_alt <- estimatr::lm_robust(as.formula(paste0(outcome, " ~ treatment_group")), data = data_alt)
  return(list(est_reg, est_reg_alt))
}

run_GRF <- function(outcome, data, data_alt, covariates) {
  if(outcome == 'vaxed_f_binary') {
    covariates <- c(covariates, 'response_imputed')
  }
  
  est_reg <- mcf_estimate(data, outcome, covariates = covariates)
  est_reg_alt <- mcf_estimate(data_alt, outcome, covariates = covariates)
  return(list(est_reg, est_reg_alt))
}

run_IP <- function(outcome, data, data_alt, covariates) {
  if(outcome == 'vaxed_f_binary') {
    covariates <- c(covariates, 'response_imputed')
  }
  est_reg <- IP_estimate(data, outcome, covariates = covariates)[['ATE']]
  est_reg_alt <- IP_estimate(data_alt, outcome, covariates = covariates)[['ATE']]
  return(list(est_reg, est_reg_alt))
}

run_Lin <- function(outcome, data, data_alt, covariates_lin) {
  if(outcome == 'vaxed_f_binary') {
    covariates_lin <- as.formula(paste('~' , paste0(
      c('willingness', 'get_vaccinated'),
      collapse = ' + ')))
  }
  
  est_reg <- estimatr::lm_lin(as.formula(paste0(outcome, " ~ treatment_group")),
                              data = data, covariates = covariates_lin)
  est_reg_alt <- estimatr::lm_lin(as.formula(paste0(outcome, " ~ treatment_group")),
                                  data = data_alt, covariates = covariates_lin)
  return(list(est_reg, est_reg_alt))
}


# Main function:
# Attrition / missing data:
# - For information seeking and encouragement, we impute zeros, i.e., that
# respondents did not click through for more information or share posts with
# peers, which is in line with their actual behavior.
# - For sharing behavior, we report both unadjusted, and re-weighted doubly
# robust estimates, where the weighting accounts for both treatment propensity
# and missingness propensity, under the assumption that missingness is ignorable
# conditioning on measured covariates and treatment.
# - In the follow-up survey where we elicit vaccination status, we report both
# unadjusted, and re-weighted doubly robust estimates, where the weighting
# accounts for both treatment propensity and missingness propensity, under the
# assumption that missingness is ignorable conditioning on measured covariates,
# treatment, and **primary response**.


run_all_models <- function(data, data_alt, covariate_list, covariate_lin) {
  outcomes <- c("share_true_scaled", # sharing behavior
                "share_false_scaled", # sharing behavior
                "pre_register_imputed", # information seeking
                "share_post_numeric_imputed", # encouragement
                "vaxed_f_binary" # vaccination status
  )
  results <- list()
  
  for (outcome in outcomes) {
    results[[outcome]] <- list(
      DM = run_DM(outcome, data, data_alt),
      GRF = run_GRF(outcome, data, data_alt, covariate_list),
      IP = run_IP(outcome, data, data_alt, covariate_list),
      Lin = run_Lin(outcome, data, data_alt, covariate_lin)
    )
    
  }
  return(results)
}

# run function for all 3 datasets:

results <- run_all_models(df_eval, df_eval_alt, covariate_list, covariate_lin)

results_kenya <- run_all_models(df_kenya, df_kenya_alt, covariate_list, covariate_lin)

results_nigeria <- run_all_models(df_nigeria, df_nigeria_alt, covariate_list, covariate_lin)



# code to extract coefficients, se's and n's

extract_coefficients_and_se <- function(results) {
  coefficients <- list()
  
  # Specifying the coefficient mapping
  coef_map <- list(
    'treatment_groupPSA' = 'PSA - Control',
    'treatment_groupConcerns' = c('Concerns - Control', 'Concerns - PSA'),
    '(Intercept)' = 'Control mean'
  )
  
  for (outcome in names(results)) {
    coefficients[[outcome]] <- list()
    
    for (model_type in names(results[[outcome]])) {
      # Create new lists to store the specific coefficients and standard errors
      specific_coefs <- list()
      specific_se <- list()
      specific_nobs <- list()
      
      if (model_type != "GRF") {
        # Extracting coefficients and standard errors for regular data
        est_reg <- results[[outcome]][[model_type]][[1]]
        est_reg_alt <- results[[outcome]][[model_type]][[2]]
        
        # Extract nobs
        specific_nobs$main <- est_reg$nobs
        specific_nobs$alt <- est_reg_alt$nobs
        
        # Loop through the coef_map and extract relevant coefficients and standard errors
        for (coef_name in names(coef_map)) {
          if (coef_name %in% names(est_reg$coefficients)) {
            specific_coefs[[coef_map[[coef_name]][1]]] <- est_reg$coefficients[[coef_name]]
            specific_se[[coef_map[[coef_name]][1]]] <- est_reg$std.error[[coef_name]]
          }
          
          if (length(coef_map[[coef_name]]) > 1 && coef_name %in% names(est_reg_alt$coefficients)) {
            specific_coefs[[coef_map[[coef_name]][2]]] <- est_reg_alt$coefficients[[coef_name]]
            specific_se[[coef_map[[coef_name]][2]]] <- est_reg_alt$std.error[[coef_name]]
          }
        }
      } else {
        # For GRF, use the format_grf_summary function
        grf_summary <- format_grf_summary(results[[outcome]][[model_type]][[1]])$tidy
        grf_summary_alt <- format_grf_summary(results[[outcome]][[model_type]][[2]])$tidy
        
        specific_nobs$main <- results[[outcome]][[model_type]][[1]]$nobs[1]
        specific_nobs$alt <- results[[outcome]][[model_type]][[2]]$nobs[1]
        
        # Extracting coefficients and standard errors for GRF. Adjust if the structure is different.
        for (coef_name in names(coef_map)) {
          if (coef_name %in% grf_summary$term) {
            specific_coefs[[coef_map[[coef_name]][1]]] <- grf_summary[which(grf_summary$term == coef_name),]$estimate
            specific_se[[coef_map[[coef_name]][1]]] <- grf_summary[which(grf_summary$term == coef_name),]$std.error
          }
          
          if (length(coef_map[[coef_name]]) > 1 && coef_name %in% grf_summary_alt$term) {
            specific_coefs[[coef_map[[coef_name]][2]]] <- grf_summary_alt[which(grf_summary_alt$term == coef_name),]$estimate
            specific_se[[coef_map[[coef_name]][2]]] <- grf_summary_alt[which(grf_summary_alt$term == coef_name),]$std.error
          }
        }
      }
      
      coefficients[[outcome]][[model_type]] <- list(
        coef = specific_coefs,
        se = specific_se,
        nobs = specific_nobs
      )
    }
  }
  
  return(coefficients)
}



###################################################
### code chunk number 18: extract_results
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
coefficients <- extract_coefficients_and_se(results)
coefficients_kenya <- extract_coefficients_and_se(results_kenya)
coefficients_nigeria <- extract_coefficients_and_se(results_nigeria)


###################################################
### code chunk number 19: secondary_results_all_new
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

generate_kable_table_per_outcome <- function(outcome_coefficients) {
  # Dataset to label mapping
  dataset_map <- list(
    'coefficients' = '\\textit{Combined}', # Italicize the sample name
    'coefficients_kenya' = '\\textit{Kenya}',  # Italicize the sample name
    'coefficients_nigeria' = '\\textit{Nigeria}'  # Italicize the sample name
  )
  
  outcome_map <- list(
    'true' = 'Sharing True Post',
    'false' = 'Sharing False Post',
    'register' = 'Informating Seeking',
    'encourage' = 'Encourage Others',
    'vaxed' = 'Vaccination status (follow up)'
  )
  
  rows_list <- list()
  
  for (dataset in names(outcome_coefficients)) {
    coefficients <- outcome_coefficients[[dataset]]
    
    dataset_rows <- data.frame(
      Coefficient = c('PSA - Control', '', 'Concerns - Control', '', 'Concerns - PSA', '', 'Control mean', '', 'n'),
      DM = NA,
      Lin = NA,
      IP = NA,
      GRF = NA,
      stringsAsFactors = FALSE
    )
    
    for (coef_name in c('PSA - Control', 'Concerns - Control', 'Concerns - PSA', 'Control mean')) {
      for (model_type in c("DM", "Lin", "IP", "GRF")) {
        coef_val <- coefficients[[model_type]]$coef[[coef_name]]
        se_val <- coefficients[[model_type]]$se[[coef_name]]
        p_val <- 2 * (1 - pnorm(abs(coef_val / se_val)))  # Two-tailed p-value
        
        # Determine the significance symbol
        sig_symbol <- ""
        if (coef_name != 'Control mean') {
          if (p_val < 0.001) sig_symbol <- "***"
          else if (p_val < 0.01) sig_symbol <- "**"
          else if (p_val < 0.05) sig_symbol <- "*"
          else if (p_val < 0.1) sig_symbol <- "+"
        }
        
        coef_row_index <- which(dataset_rows$Coefficient == coef_name)
        se_row_index <- coef_row_index + 1  # SE row is right after coef row
        
        dataset_rows[coef_row_index, model_type] <- sprintf("%.3f%s", coef_val, sig_symbol)
        dataset_rows[se_row_index, model_type] <- sprintf("(%.3f)", se_val)
      }
    }
    
    # Add the nobs to the dataset rows and format it with commas
    for (model_type in c("DM", "Lin", "IP", "GRF")) {
      dataset_rows[dataset_rows$Coefficient == 'n', model_type] <- formatC(coefficients[[model_type]]$nobs$main, format="d", big.mark=",")
    }
    
    # Add dataset label to the top of each section
    dataset_label_row <- data.frame(Coefficient = dataset_map[[dataset]],
                                    DM = NA,
                                    Lin = NA, IP = NA, GRF = NA,
                                    stringsAsFactors = FALSE)
    rows_list[[dataset]] <- rbind(dataset_label_row, dataset_rows)
  }
  
  # Combine all datasets into one data frame
  full_table <- do.call(rbind, rows_list)
  
  # Define the caption and footnote for the table based on the outcome
  input_name <- deparse(substitute(outcome_coefficients))
  outcome_name <- sub("_.*", "", input_name)
  
  caption_text <- sprintf("Treatment effect estimates and response under alternative approaches to estimation, %s.", outcome_map[[outcome_name]])
  
  footnote_text <- "\\multicolumn{5}{l}{\\rule{0pt}{1em}+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001}"
  
  # Use kable to format the table
  kable_output <- kable(full_table,
                        format = "latex",
                        booktabs = TRUE,
                        linesep = "",
                        align = c("l", "c", "c", "c", "c"),
                        row.names = FALSE,
                        col.names = c("", "DM",
                                      "Lin", "IP", "GRF"),
                        caption = caption_text,
                        escape = FALSE) |>
    kable_styling(latex_options = c('HOLD_position')) |>
    footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage. The response measure is ', outcome_map[[outcome_name]], '. Estimates are average treatment effects, and control mean. Estimating procedures are discussed in the text. Statistical significance is reported only for treatment effect estimates, and differences, not for baseline control means.'
    ),
    escape = FALSE,
    threeparttable = TRUE,
    general_title = '') |>
    # Add lines after the dataset names
    row_spec(1, extra_latex_after = "\\midrule") |>
    row_spec(11, extra_latex_after = "\\midrule") |>
    row_spec(21, extra_latex_after = "\\midrule") |>
    row_spec(30, extra_latex_after = footnote_text)
  
  return(kable_output)
}



###################################################
### code chunk number 20: secondary_results_true
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

true_coefficients <- list(
  coefficients = coefficients[["share_true_scaled"]],
  coefficients_kenya = coefficients_kenya[["share_true_scaled"]],
  coefficients_nigeria = coefficients_nigeria[["share_true_scaled"]]
)

generate_kable_table_per_outcome(true_coefficients)



###################################################
### code chunk number 21: secondary_results_false
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

false_coefficients <- list(
  coefficients = coefficients[["share_false_scaled"]],
  coefficients_kenya = coefficients_kenya[["share_false_scaled"]],
  coefficients_nigeria = coefficients_nigeria[["share_false_scaled"]]
)

print(generate_kable_table_per_outcome(false_coefficients))



###################################################
### code chunk number 22: secondary_results_register
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

register_coefficients <- list(
  coefficients = coefficients[["pre_register_imputed"]],
  coefficients_kenya = coefficients_kenya[["pre_register_imputed"]],
  coefficients_nigeria = coefficients_nigeria[["pre_register_imputed"]]
)

print(generate_kable_table_per_outcome(register_coefficients))



###################################################
### code chunk number 23: secondary_results_encourage
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

encourage_coefficients <- list(
  coefficients = coefficients[["share_post_numeric_imputed"]],
  coefficients_kenya = coefficients_kenya[["share_post_numeric_imputed"]],
  coefficients_nigeria = coefficients_nigeria[["share_post_numeric_imputed"]]
)

print(generate_kable_table_per_outcome(encourage_coefficients))



###################################################
### code chunk number 24: secondary_results_vaxed
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

vaxed_coefficients <- list(
  coefficients = coefficients[["vaxed_f_binary"]],
  coefficients_kenya = coefficients_kenya[["vaxed_f_binary"]],
  coefficients_nigeria = coefficients_nigeria[["vaxed_f_binary"]]
)

print(generate_kable_table_per_outcome(vaxed_coefficients))



###################################################
### code chunk number 25: predict_type
###################################################
#| eval = TRUE,
#| echo=FALSE,
#| warning=FALSE,
#| message=FALSE,
#| strip.white=TRUE,
#| results='asis'

summary_variables <- c(
  #demographics
  'age', 'is_female', 'religion_christian', 'religiosity', 'party_aligned',
  #SES
  'is_urban', 'completed_secondary_school', 'hhold20', 'assets_index', 'has_job',
  #healthcare access, knowledge, attitudes
  'health_access',
  'cov_know',
  'know_wheretoget',
  'adult_vax',
  'getvax_easy',
  'trust_index')

# create groups based on two pre-treatment measures of vaccine willingness/intention (1=Hesitant, 2/3 = Undecided, 4=Eager)
df_eval <- df_eval |>
  mutate(willingness_group =
           factor(case_when(willingness > 2 ~ willingness-1,
                            willingness <= 2 ~ willingness,
                            TRUE ~ 0),
                  levels = c(0,1,2,3),
                  labels = c('Missing', 'Hesitant','Undecided','Eager')),
         willingness_group_post =
           factor(case_when(willingness_2 > 2 ~ willingness_2-1,
                            willingness_2 <= 2 ~ willingness_2,
                            TRUE ~ 0),
                  levels = c(0,1,2,3),
                  labels = c('Missing', 'Hesitant','Undecided','Eager')),
         get_vaccinated_group =
           factor(case_when(get_vaccinated > 2 ~ get_vaccinated-1,
                            get_vaccinated <= 2 ~ get_vaccinated,
                            TRUE ~ 0),
                  levels = c(0,1,2,3),
                  labels = c('Missing', 'Hesitant','Undecided','Eager')),
         get_vaccinated_group_post =
           factor(case_when(get_vaccinated_2 > 2 ~ get_vaccinated_2-1,
                            get_vaccinated_2 <= 2 ~ get_vaccinated_2,
                            TRUE ~ 0),
                  levels = c(0,1,2,3),
                  labels = c('Missing', 'Hesitant','Undecided','Eager')))

df_willingness <- df_eval |>
  mutate(is_female = 1*(is_male==0),
         hhold20 = case_when(hhold<20 ~hhold,
                             TRUE ~NA)) |>
  drop_na(willingness_group) |>
  group_by(willingness_group) |>
  summarise(across(all_of(summary_variables),
                   .fns = list(mean = ~ sprintf('%.3f', mean(., na.rm = TRUE)),
                               se = ~ sprintf('(%.3f)',
                                              sd(., na.rm = TRUE)/sqrt(sum(!is.na(.))))),
                   .names = '{.fn}_{.col}'
  )) |> t()


colnames(df_willingness) <- df_willingness['willingness_group',]
row.names(df_willingness)[grep('se_', row.names(df_willingness))] <- ''

label_rows <- c('mean_age' = 'Age',
                'mean_is_female' = '% Non-male',
                'mean_health_access' = 'Hours to nearest health facility',
                'mean_party_aligned' = '% Party aligned',
                'mean_cov_know' = 'COVID knowledge index (-5:5)',
                'mean_religion_christian' = '% Christian',
                'mean_religiosity' = 'Religiosity (1:6)',
                'mean_completed_secondary_school' = '% Completed secondary school',
                'mean_is_urban' = '% Urban',
                'mean_hhold20' = 'Household size',
                'mean_assets_index' = 'Assets index (-5:5)',
                'mean_has_job' = '% Employed',
                'mean_trust_index' = 'Trust index (-14:14)',
                'mean_adult_vax' = '% Any prior vaccination',
                'mean_know_wheretoget' = '% Know where to get COVID vaccine',
                'mean_getvax_easy' = 'Ease of getting COVID vaccine (-2:2)')

rownames(df_willingness)[match(names(label_rows),
                               rownames(df_willingness))] <- label_rows

kbl(df_willingness[-1,],
    format = 'latex',
    caption= 'Composition of baseline vaccine attitude types.',
    align = 'c', booktabs = TRUE,
    toprule = '\\vspace{-3em} \\\\ \\toprule') |>
  kable_styling(latex_options = c('HOLD_position')) |>
  pack_rows('Demographics', 2, 3) |>
  pack_rows(NULL, 4, 5, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 6, 7, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 8, 9, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 10, 11, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 12, 13, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows('Socioeconomic status', 14, 15) |>
  pack_rows(NULL, 16, 17, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 18, 19, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 20, 21, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 22, 23, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows('Healthcare access/knowledge/attitudes', 24, 25) |>
  pack_rows(NULL, 26, 27, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 28, 29, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 30, 31, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  pack_rows(NULL, 32, 33, bold = FALSE, latex_gap_space = '-2\\\\defaultaddspace',
            escape = FALSE, colnum = 0) |>
  footnote(general = paste0('\\\\scriptsize The sample is users in the evaluation stage, $n = $ ',
                            prettyNum(eval_n, big.mark = ','),
                            ". Estimates are of subgroup means, produced from sample means and standard error of the sample mean. Subgroups are defined on pre-treatment response to the vaccine willingness question; \\\\textit{hesitant} respondents reported that they wanted to get a vaccine ``not at all;'' \\\\textit{undecided} respondents wanted to get a vaccine ``a little'' or ``moderately;'' \\\\textit{eager} respondents wanted to get a vaccine ``very much.''"
  ),
  escape = FALSE,
  threeparttable = TRUE,
  general_title = '')


###################################################
### code chunk number 26: hte_concern_calcs
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'

concerns <- c(`1` = 'Side effects',
              `2` = 'Vaccine does not work',
              `3` = 'COVID is not real',
              `4` = 'Protected by God',
              `5` = 'Do not trust healthcare workers',
              `6` = 'Do not trust government',
              `7` = 'Not sure what to believe')

# create data frames for figures
response_kenya <- df_kenya |>
  mutate(concern = concerns[concern_id1]) |>
  drop_na(concern, willingness, willingness_2, get_vaccinated, get_vaccinated_2) |>
  group_by(concern) |>
  summarise(willing_mean = mean(willingness_2, na.rm = TRUE),
            willing_sd = sd(willingness_2, na.rm = TRUE) / sqrt(sum(!is.na(willingness_2))),
            diff_willingness_mean = mean(willingness_2-willingness, na.rm = TRUE),
            diff_willingness_sd = sd(willingness_2-willingness, na.rm = TRUE) / sqrt(sum(!is.na(willingness_2-willingness))),
            intend_mean = mean(get_vaccinated_2, na.rm = TRUE),
            intend_sd = sd(get_vaccinated_2, na.rm = TRUE) / sqrt(sum(!is.na(get_vaccinated_2))),
            diff_intend_mean = mean(get_vaccinated_2-get_vaccinated, na.rm = TRUE),
            diff_intend_sd = sd(get_vaccinated_2-get_vaccinated, na.rm = TRUE) / sqrt(sum(!is.na(get_vaccinated_2-get_vaccinated))),
            n = n()) |>
  mutate(country = 'Kenya',
         pre_post = 'post',
         statistic_willing = diff_willingness_mean/diff_willingness_sd,
         statistic_intend = diff_intend_mean/diff_intend_sd) |>
  select(willing_mean, willing_sd, intend_mean, intend_sd, pre_post, concern, n, country,
         statistic_willing, statistic_intend)

response_pre_kenya <- df_kenya |>
  mutate(concern = concerns[concern_id1]) |>
  drop_na(concern, willingness, willingness_2, get_vaccinated, get_vaccinated_2) |>
  group_by(concern) |>
  summarise(willing_mean = mean(willingness, na.rm = TRUE),
            willing_sd = sd(willingness, na.rm = TRUE) /  sqrt(sum(!is.na(willingness))),
            intend_mean = mean(get_vaccinated, na.rm = TRUE),
            intend_sd = sd(get_vaccinated, na.rm = TRUE) /  sqrt(sum(!is.na(get_vaccinated))),
            n = n()) |>
  mutate(country = 'Kenya', pre_post = 'pre',
         statistic_willing = NA,
         statistic_intend = NA) |>
  select(willing_mean, willing_sd, intend_mean, intend_sd, pre_post, concern, n, country,
         statistic_willing, statistic_intend)


# nigeria dataset
response_nigeria <- df_nigeria |>
  mutate(concern = concerns[concern_id1]) |>
  drop_na(concern, willingness, willingness_2, get_vaccinated, get_vaccinated_2) |>
  group_by(concern) |>
  summarise(willing_mean = mean(willingness_2, na.rm = TRUE),
            willing_sd = sd(willingness_2, na.rm = TRUE) / sqrt(sum(!is.na(willingness_2))),
            diff_willingness_mean = mean(willingness_2-willingness, na.rm = TRUE),
            diff_willingness_sd = sd(willingness_2-willingness, na.rm = TRUE) / sqrt(sum(!is.na(willingness_2-willingness))),
            intend_mean = mean(get_vaccinated_2, na.rm = TRUE),
            intend_sd = sd(get_vaccinated_2, na.rm = TRUE) / sqrt(sum(!is.na(get_vaccinated_2))),
            diff_intend_mean = mean(get_vaccinated_2-get_vaccinated, na.rm = TRUE),
            diff_intend_sd = sd(get_vaccinated_2-get_vaccinated, na.rm = TRUE) / sqrt(sum(!is.na(get_vaccinated_2-get_vaccinated))),
            n = n()) |>
  mutate(country = 'Nigeria',
         pre_post = 'post',
         statistic_willing = diff_willingness_mean/diff_willingness_sd,
         statistic_intend = diff_intend_mean/diff_intend_sd) |>
  select(willing_mean, willing_sd, intend_mean, intend_sd, pre_post, concern, n, country,
         statistic_willing, statistic_intend)

response_pre_nigeria <- df_nigeria |>
  mutate(concern = concerns[concern_id1]) |>
  drop_na(concern, willingness, willingness_2, get_vaccinated, get_vaccinated_2) |>
  group_by(concern) |>
  summarise(willing_mean = mean(willingness),
            willing_sd = sd(willingness, na.rm = TRUE) /  sqrt(sum(!is.na(willingness))),
            intend_mean = mean(get_vaccinated, na.rm = TRUE),
            intend_sd = sd(get_vaccinated, na.rm = TRUE) /  sqrt(sum(!is.na(get_vaccinated))),
            n = n()) |>
  mutate(country = 'Nigeria', pre_post = 'pre',
         statistic_willing = NA,
         statistic_intend = NA) |>
  select(willing_mean, willing_sd, intend_mean, intend_sd, pre_post, concern, n, country,
         statistic_willing, statistic_intend)


# combine datasets #
response_pre_post_both <- rbind(response_pre_kenya, response_kenya,
                                response_pre_nigeria, response_nigeria)


## reformat so intention and willingness are variable indicators for mean and sd cols
reshaped_data <- response_pre_post_both |>
  pivot_longer(cols = c(willing_mean, intend_mean, willing_sd, intend_sd),
               names_to = c('measure', '.value'),
               names_pattern = '(willing|intend)_(mean|sd)') |>
  mutate(pre_post = factor(pre_post, levels = c('pre','post')),
         concern = factor(concern, levels = concerns),
         country_time = factor(interaction(country, pre_post),
                               levels = c('Kenya.pre',
                                          'Kenya.post',
                                          'Nigeria.pre',
                                          'Nigeria.post')))

# add a column of stars indicating diffs that are stat. sig
reshaped_data <- reshaped_data |>
  mutate(significance = case_when(
    measure == "willing" & abs(statistic_willing) > 3.29 ~ '***', # 0.1% level
    measure == "willing" & abs(statistic_willing) > 2.58 ~ '**',  # 1% level
    measure == "willing" & abs(statistic_willing) > 1.96 ~ '*',   # 5% level
    
    measure == "intend" & abs(statistic_intend) > 3.29 ~ '***', # 0.1% level
    measure == "intend" & abs(statistic_intend) > 2.58 ~ '**',  # 1% level
    measure == "intend" & abs(statistic_intend) > 1.96 ~ '*',   # 5% level
    
    TRUE ~ NA_character_
  ))

# great var for height of highest confidence interval (to put * above)
reshaped_data <- reshaped_data |>
  group_by(country, concern, measure) |>
  mutate(ymax_star = max(mean + 1.96*sd)) |>
  ungroup()

# Filter for post data
post_data <- subset(reshaped_data,
                    pre_post == 'post' & measure == 'intend')

# Calculate a fixed y-position for the sample size labels based on the global min and max of the mean values
label_position <- min(reshaped_data$mean) - 0.5 * (max(reshaped_data$mean) - min(reshaped_data$mean))


# Plot
g <- ggplot(reshaped_data, aes(x = concern, y = mean,
                               color = country_time,
                               shape = country_time)) +
  stat_gradientinterval(aes(x = concern,
                            ydist = distributional::dist_normal(mean, sd),
                            fill = country_time),
                        width = 1,
                        position = position_dodge(0.75),
                        linewidth = 0,
                        point_size = 0,
                        point_alpha = 0,
                        interval_alpha = 0,
                        show.legend = FALSE,
                        fill_type = 'segments') +
  geom_point(position = position_dodge(0.75), size = 3) +  # Dodge position so points don't overlap
  geom_errorbar(aes(ymin = mean - 1.96*sd, ymax = mean + 1.96*sd),
                width = 0.3, position = position_dodge(0.75)) +
  geom_text(data = post_data, aes(label = prettyNum(n, big.mark = ','),
                                  y = label_position + .3,
                                  group = country,
                                  color =  country_time),
            position = position_dodge(.95), size = 3.5,
            show.legend = FALSE) +  # Display sample size for 'post'
  geom_text(data = post_data,
            aes(label = 'n:', x = .35, y = label_position + .3),
            color = 'black',
            size = 4,
            show.legend = FALSE) +
  facet_wrap(~ measure, ncol = 2, scales = 'fixed',
             labeller = labeller(measure = c(intend = 'Intention', willing = 'Willingness'))) +  # Fixed scale to ensure same y-axis range for both plots
  
  # add stars for significant diffs
  geom_text(aes(label = significance, y = ymax_star,
                group = country,
                color = country_time),
            position = position_dodge(0.75),
            vjust = -0.5,
            size = 4,
            show.legend = FALSE) +
  
  scale_shape_manual(name = 'Measure',
                     breaks = c('Kenya.pre','Kenya.post','Nigeria.pre','Nigeria.post'),
                     labels = c('Kenya-pre','Kenya-post','Nigeria-pre','Nigeria-post'),
                     values = c(2,17, 0, 15)) +  # Different shapes for country and pre/post
  scale_color_manual(name = 'Measure',
                     breaks = c('Kenya.pre','Kenya.post','Nigeria.pre','Nigeria.post'),
                     labels = c('Kenya-pre','Kenya-post','Nigeria-pre','Nigeria-post'),
                     values = c(cbPalette[7],cbPalette[7], cbPalette[4], cbPalette[4])) +
  scale_fill_manual(name = 'Measure',
                    breaks = c('Kenya.pre','Kenya.post','Nigeria.pre','Nigeria.post'),
                    labels = c('Kenya-pre','Kenya-post','Nigeria-pre','Nigeria-post'),
                    values = c(cbPalette[7],cbPalette[7], cbPalette[4], cbPalette[4]), guide = 'none') +
  
  scale_x_discrete(expand = c(0.15, 0)) +
  scale_y_continuous(breaks = seq(1.8, 4, by = .5)) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.line.x = NULL,
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 20, face = 'bold'),
    legend.position = 'bottom'
  ) +
  labs(y = 'Estimate', x = 'Concern category', fill = 'Country', shape = 'Meaure') +
  theme(panel.grid.major.x = element_blank()) +
  vcf_theme()



###################################################
### code chunk number 27: hte_concern
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| fig.align='center',
#| fig.width=11,
#| fig.height = 7,
#| prefix.string = 'fig',
#| fig.pos = '!ht',
#| fig.cap=
#| paste0('\\textbf{Among respondents in concern-addressing group, pre- and
#|  post-treatment COVID-19 vaccine intention and willingness.} ',
#| 'The sample is users in the concern-addressing condition in the evaluation
#|  stage who have complete pre- and post-treatment response measures and who
#|  provided information on their concerns, $n = $ ',
#|   prettyNum(sum(df_eval$treatment_group=='Concerns' &
#|                 !is.na(df_eval$willingness_2) &
#|                 !is.na(df_eval$concern_id1)), big.mark = ','),
#| '. Samples within each concern category by country are presented at the
#|  bottom of the Intention plot, color coded by country. ',
#| "Estimates are of subgroup means, produced from sample means and standard error of the sample mean.
#|  Subgroups are defined on users' first expressed concern.
#|  Error bars represent symmetric 95\\% confidence intervals.
#|  Stars represent statistical significance of the p-value associated with the
#|  difference in pre- and post-treatment mean response by primary concern
#|  category and country (*p < 0.05, **p < 0.01, ***p < 0.001)."),
#| strip.white=TRUE,
#| results='asis'
g


###################################################
### code chunk number 28: heterogeneity_estimation
###################################################
#| eval = TRUE,
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'


hetero_covariates_list<-c("is_male", "age_median","cov_know_median",
                          "health_access_median","party_aligned",
                          "religiosity_median","digital_index_median")


## response function (man/above median)
for (covariate in hetero_covariates_list){
  ## A) Difference-in-means estimate (i.e, treating missing data as random)
  est_DM <- paste0("est_DM_", covariate)
  est_formula <- paste0(est_DM, " <- estimatr::lm_robust(response ~ treatment_group, data = df_eval[which(df_eval$", covariate, " == 1),])")
  eval(parse(text = est_formula))
  
  est_DM_alt <- paste0("est_DM_", covariate, "_alt")
  est_formula_alt <- paste0(est_DM_alt, " <- estimatr::lm_robust(response ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " == 1),])")
  eval(parse(text = est_formula_alt))
  
  ## B) Imputing pre-test as post-test
  est_IMP <- paste0("est_IMP_", covariate)
  est_formula_IMP <- paste0(est_IMP, " <- estimatr::lm_robust(response_imputed ~ treatment_group, data = df_eval[which(df_eval$", covariate, " == 1),])")
  eval(parse(text = est_formula_IMP))
  
  est_IMP_alt <- paste0("est_IMP_", covariate, "_alt")
  est_formula_IMP_alt <- paste0(est_IMP_alt, " <- estimatr::lm_robust(response_imputed ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " == 1),])")
  eval(parse(text = est_formula_IMP_alt))
  
  ## C) Generalized random forest
  est_GRF <- paste0("est_GRF_", covariate)
  est_formula_GRF <- paste0(est_GRF, " <- mcf_estimate(df_eval[which(df_eval$", covariate, " == 1),], 'response', covariates = covariate_list)")
  eval(parse(text = est_formula_GRF))
  
  est_GRF_alt <- paste0("est_GRF_", covariate, "_alt")
  est_formula_GRF_alt <- paste0(est_GRF_alt, " <- mcf_estimate(df_eval_alt[which(df_eval_alt$", covariate, " == 1),], 'response', covariates = covariate_list)")
  eval(parse(text = est_formula_GRF_alt))
  
  ## D) IP Weighting
  est_IP <- paste0("est_IP_", covariate)
  est_formula_IP <- paste0(est_IP, " <- IP_estimate(df_eval[which(df_eval$", covariate, " == 1),], covariates = covariate_list)[['ATE']]")
  eval(parse(text = est_formula_IP))
  
  est_IP_alt <- paste0("est_IP_", covariate, "_alt")
  est_formula_IP_alt <- paste0(est_IP_alt, " <- IP_estimate(df_eval_alt[which(df_eval_alt$", covariate, " == 1),], covariates = covariate_list)[['ATE']]")
  eval(parse(text = est_formula_IP_alt))
  
  ## E) Lin estimator
  est_Lin <- paste0("est_Lin_", covariate)
  if (covariate %in% c("health_access_median","party_aligned","age_median")){
    covariate_lin_temp <- as.formula(paste('~', paste0(
      c('willingness_imputed', 'get_vaccinated_imputed'),
      collapse = ' + ')))
  }else {
    covariate_lin_temp<-covariate_lin
  }
  
  est_formula_Lin <- paste0(est_Lin, " <- estimatr::lm_lin(response ~ treatment_group, data = df_eval[which(df_eval$", covariate, " == 1),], covariates = covariate_lin_temp)")
  eval(parse(text = est_formula_Lin))
  
  est_Lin_alt <- paste0("est_Lin_", covariate, "_alt")
  est_formula_Lin_alt <- paste0(est_Lin_alt, " <- estimatr::lm_lin(response ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " == 1),], covariates = covariate_lin_temp)")
  eval(parse(text = est_formula_Lin_alt))
}


## response function (no man/below median)
for (covariate in hetero_covariates_list){
  
  ## A) Difference-in-means estimate (i.e, treating missing data as random)
  est_DM <- paste0("est_DM_no", covariate)
  est_formula <- paste0(est_DM, " <- estimatr::lm_robust(response ~ treatment_group, data = df_eval[which(df_eval$", covariate, " != 1),])")
  eval(parse(text = est_formula))
  
  est_DM_alt <- paste0("est_DM_no", covariate, "_alt")
  est_formula_alt <- paste0(est_DM_alt, " <- estimatr::lm_robust(response ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " != 1),])")
  eval(parse(text = est_formula_alt))
  
  ## B) Imputing pre-test as post-test
  est_IMP <- paste0("est_IMP_no", covariate)
  est_formula_IMP <- paste0(est_IMP, " <- estimatr::lm_robust(response_imputed ~ treatment_group, data = df_eval[which(df_eval$", covariate, " != 1),])")
  eval(parse(text = est_formula_IMP))
  
  est_IMP_alt <- paste0("est_IMP_no", covariate, "_alt")
  est_formula_IMP_alt <- paste0(est_IMP_alt, " <- estimatr::lm_robust(response_imputed ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " != 1),])")
  eval(parse(text = est_formula_IMP_alt))
  
  ## C) Generalized random forest
  est_GRF <- paste0("est_GRF_no", covariate)
  est_formula_GRF <- paste0(est_GRF, " <- mcf_estimate(df_eval[which(df_eval$", covariate, " != 1),], 'response', covariates = covariate_list)")
  eval(parse(text = est_formula_GRF))
  
  est_GRF_alt <- paste0("est_GRF_no", covariate, "_alt")
  est_formula_GRF_alt <- paste0(est_GRF_alt, " <- mcf_estimate(df_eval_alt[which(df_eval_alt$", covariate, " != 1),], 'response', covariates = covariate_list)")
  eval(parse(text = est_formula_GRF_alt))
  
  ## D) IP Weighting
  est_IP <- paste0("est_IP_no", covariate)
  est_formula_IP <- paste0(est_IP, " <- IP_estimate(df_eval[which(df_eval$", covariate, " != 1),], covariates = covariate_list)[['ATE']]")
  eval(parse(text = est_formula_IP))
  
  est_IP_alt <- paste0("est_IP_no", covariate, "_alt")
  est_formula_IP_alt <- paste0(est_IP_alt, " <- IP_estimate(df_eval_alt[which(df_eval_alt$", covariate, " != 1),], covariates = covariate_list)[['ATE']]")
  eval(parse(text = est_formula_IP_alt))
  
  
  ## E) Lin estimator
  est_Lin <- paste0("est_Lin_no", covariate)
  est_formula_Lin <- paste0(est_Lin, " <- estimatr::lm_lin(response ~ treatment_group, data = df_eval[which(df_eval$", covariate, " != 1),], covariates = covariate_lin)")
  eval(parse(text = est_formula_Lin))
  
  est_Lin_alt <- paste0("est_Lin_no", covariate, "_alt")
  est_formula_Lin_alt <- paste0(est_Lin_alt, " <- estimatr::lm_lin(response ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " != 1),], covariates = covariate_lin)")
  eval(parse(text = est_formula_Lin_alt))
}



###################################################
### code chunk number 29: heterogeneity_results
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'


hetero_covariates_list<-c("is_male", "age_median","cov_know_median",
                          "health_access_median","party_aligned",
                          "religiosity_median","digital_index_median")

covariate_terms <- list(
  is_male = c("Male", "Non-male"),
  age_median = c("Age above median", "Age below median"),
  cov_know_median = c("Scientific knowledge above median", "Scientific knowledge below median"),
  health_access_median = c("Health access above median", "Health access below median"),
  party_aligned = c("Aligned with governing party", "Not aligned with governing party"),
  religiosity_median = c("Religiosity above median", "Religiosity below median"),
  digital_index_median = c("Digital index score above median", "Digital index score below median")
)

table_terms <- list(
  is_male = c("gender"),
  age_median = c("age"),
  cov_know_median = c("scientific knowledge"),
  health_access_median = c("health access"),
  party_aligned = c("alignment with governing party"),
  religiosity_median = c("religiosity"),
  digital_index_median = c("digital index score")
)


for (covariate in hetero_covariates_list) {
  
  temp_list <- list(
    DM = get(paste0("est_DM_", covariate)),
    IMP = get(paste0("est_IMP_", covariate)),
    Lin = get(paste0("est_Lin_", covariate)),
    IP = get(paste0("est_IP_", covariate))
  )
  assign(paste0("out_list_", covariate), temp_list)
  
  temp_list <- list(
    DM = get(paste0("est_DM_no", covariate)),
    IMP = get(paste0("est_IMP_no", covariate)),
    Lin = get(paste0("est_Lin_no", covariate)),
    IP = get(paste0("est_IP_no", covariate))
  )
  
  assign(paste0("out_list_no", covariate), temp_list)
  
  
  temp_list <- list(
    DM = compare_models(get(paste0("est_DM_no", covariate)), get(paste0("est_DM_", covariate)))$Difference,
    IMP = compare_models(get(paste0("est_IMP_no", covariate)), get(paste0("est_IMP_", covariate)))$Difference,
    Lin = compare_models(get(paste0("est_Lin_no", covariate)), get(paste0("est_Lin_", covariate)))$Difference,
    IP = compare_models(get(paste0("est_IP_no", covariate)), get(paste0("est_IP_", covariate)))$Difference
  )
  assign(paste0("out_list_diff_", covariate), temp_list)
  
  temp <- list(
    DM = compare_models(get(paste0("est_DM_no", covariate, "_alt")), get(paste0("est_DM_", covariate, "_alt")))$Difference,
    IMP = compare_models(get(paste0("est_IMP_no", covariate, "_alt")), get(paste0("est_IMP_", covariate, "_alt")))$Difference,
    Lin = compare_models(get(paste0("est_Lin_no", covariate, "_alt")), get(paste0("est_Lin_", covariate, "_alt")))$Difference,
    IP = compare_models(get(paste0("est_IP_no", covariate, "_alt")), get(paste0("est_IP_", covariate, "_alt")))$Difference
  )
  assign(paste0("out_list_diff_alt_", covariate), temp_list)
  
  out_list_covariate <- modelsummary(get(paste0('out_list_', covariate)),
                                     output = 'modelsummary_list')
  out_list_no_covariate <- modelsummary(get(paste0('out_list_no', covariate)),
                                        output = 'modelsummary_list')
  
  out_list_diff<-get(paste0('out_list_diff_', covariate))
  out_list_diff_alt<-get(paste0('out_list_diff_alt_', covariate))
  
  for(x in names(out_list_covariate)){
    # add on Concerns - PSA coefficients
    
    out_list_covariate[[x]]$tidy <- rbind(out_list_covariate[[x]]$tidy |>
                                            select(term, estimate, std.error,
                                                   statistic, p.value),
                                          broom::tidy(get(paste0('est_', x, '_', covariate, '_alt')))[3,] |>
                                            mutate(term = 'treatment_groupConcerns2') |>
                                            select(term, estimate, std.error, statistic,p.value))
    
    out_list_no_covariate[[x]]$tidy <- rbind(out_list_no_covariate[[x]]$tidy |>
                                               select(term, estimate, std.error,
                                                      statistic, p.value),
                                             broom::tidy(get(paste0('est_', x, '_no', covariate, '_alt')))[3,] |>
                                               mutate(term = 'treatment_groupConcerns2') |>
                                               select(term, estimate, std.error, statistic, p.value))
    
    out_list_diff[[x]]$tidy <- rbind(out_list_diff[[x]]$tidy |>
                                       select(term, estimate, std.error,
                                              statistic,p.value),
                                     out_list_diff_alt[[x]]$tidy[which(out_list_diff_alt[[x]]$tidy$term == 'treatment_groupConcerns'),] |>
                                       mutate(term = 'treatment_groupConcerns2') |>
                                       select(term, estimate, std.error,
                                              statistic, p.value))
    
    # drop p-values for control condition so no stars
    out_list_covariate[[x]]$tidy[which(out_list_covariate[[x]]$tidy$term == '(Intercept)'), c('p.value', 'statistic')] <- NA
    out_list_no_covariate[[x]]$tidy[which(out_list_no_covariate[[x]]$tidy$term == '(Intercept)'), c('p.value', 'statistic')] <- NA
  }
  
  # Add GRF; different format
  
  out_list_covariate[['GRF']] <- format_grf_summary(get(paste0('est_GRF_', covariate)))
  out_list_covariate[['GRF']]$tidy <- rbind(out_list_covariate[['GRF']]$tidy,format_grf_summary(get(paste0('est_GRF_', covariate, '_alt')))$tidy[3,] |>
                                              mutate(term = 'treatment_groupConcerns2'))
  out_list_covariate[['GRF']]$tidy[which(out_list_covariate[['GRF']]$tidy$term == '(Intercept)'),
                                   c('p.value', 'statistic')] <- NA
  
  # Models without the covariate
  out_list_no_covariate[['GRF']] <- format_grf_summary(get(paste0('est_GRF_no', covariate)))
  out_list_no_covariate[['GRF']]$tidy <- rbind(out_list_no_covariate[['GRF']]$tidy,format_grf_summary(get(paste0('est_GRF_no', covariate, '_alt')))$tidy[3,] |>
                                                 mutate(term = 'treatment_groupConcerns2'))
  out_list_no_covariate[['GRF']]$tidy[which(out_list_no_covariate[['GRF']]$tidy$term == '(Intercept)'),
                                      c('p.value', 'statistic')] <- NA
  
  # Difference between with and without the covariate
  out_list_diff[['GRF']] <- compare_models(format_grf_summary(get(paste0('est_GRF_no', covariate))), format_grf_summary(get(paste0('est_GRF_', covariate))))$Difference
  
  out_list_diff[['GRF']]$tidy <- rbind(out_list_diff[['GRF']]$tidy,
                                       compare_models(format_grf_summary(get(paste0('est_GRF_no', covariate, '_alt'))),
                                                      format_grf_summary(get(paste0('est_GRF_', covariate, '_alt'))))$Difference$tidy |>
                                         filter(term == 'treatment_groupConcerns') |>
                                         mutate(term = 'treatment_groupConcerns2'))
  
  named_list <- list()
  named_list[[covariate_terms[[covariate]][1]]] <- out_list_covariate
  named_list[[covariate_terms[[covariate]][2]]] <- out_list_no_covariate
  named_list[["Difference"]] <- out_list_diff
  
  
  print(modelsummary(named_list,
                     shape= 'rbind',
                     output = 'latex',
                     coef_map = c('treatment_groupPSA' = 'PSA - Control',
                                  'treatment_groupConcerns' = 'Concerns - Control',
                                  'treatment_groupConcerns2' = 'Concerns - PSA',
                                  '(Intercept)' = 'Control mean'),
                     stars = TRUE ,
                     gof_map = list(list('raw' = 'nobs',
                                         'clean' = 'n',
                                         'fmt' = f1)),
                     escape = FALSE,
                     caption= paste0("Treatment effect estimates and response under alternative approaches to estimation, combined response measure, by ", table_terms[[covariate]], ".")) |>
          kable_styling(latex_options = c('HOLD_position')) |>
          footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage. The response measure is the combined index. Estimates are average treatment effects, and control mean. Estimating procedures are discussed in the text. Statistical significance is reported only for treatment effect estimates, and differences, not for baseline control means.'
          ),
          escape = FALSE,
          threeparttable = TRUE,
          general_title = '')
  )
}



###################################################
### code chunk number 30: heterogeneity_estimation_religion
###################################################
#| eval = TRUE,
#| cache = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'


hetero_covariates_list<-c("religion_christian","religion_muslim")

## response function (man/above median)
for (covariate in hetero_covariates_list){
  ## A) Difference-in-means estimate (i.e, treating missing data as random)
  est_DM <- paste0("est_DM_", covariate)
  est_formula <- paste0(est_DM, " <- estimatr::lm_robust(response ~ treatment_group, data = df_eval[which(df_eval$", covariate, " == 1),])")
  eval(parse(text = est_formula))
  
  est_DM_alt <- paste0("est_DM_", covariate, "_alt")
  est_formula_alt <- paste0(est_DM_alt, " <- estimatr::lm_robust(response ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " == 1),])")
  eval(parse(text = est_formula_alt))
  
  ## B) Imputing pre-test as post-test
  est_IMP <- paste0("est_IMP_", covariate)
  est_formula_IMP <- paste0(est_IMP, " <- estimatr::lm_robust(response_imputed ~ treatment_group, data = df_eval[which(df_eval$", covariate, " == 1),])")
  eval(parse(text = est_formula_IMP))
  
  est_IMP_alt <- paste0("est_IMP_", covariate, "_alt")
  est_formula_IMP_alt <- paste0(est_IMP_alt, " <- estimatr::lm_robust(response_imputed ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " == 1),])")
  eval(parse(text = est_formula_IMP_alt))
  
  ## C) Generalized random forest
  est_GRF <- paste0("est_GRF_", covariate)
  est_formula_GRF <- paste0(est_GRF, " <- mcf_estimate(df_eval[which(df_eval$", covariate, " == 1),], 'response', covariates = covariate_list)")
  eval(parse(text = est_formula_GRF))
  
  est_GRF_alt <- paste0("est_GRF_", covariate, "_alt")
  est_formula_GRF_alt <- paste0(est_GRF_alt, " <- mcf_estimate(df_eval_alt[which(df_eval_alt$", covariate, " == 1),], 'response', covariates = covariate_list)")
  eval(parse(text = est_formula_GRF_alt))
  
  ## D) IP Weighting
  est_IP <- paste0("est_IP_", covariate)
  est_formula_IP <- paste0(est_IP, " <- IP_estimate(df_eval[which(df_eval$", covariate, " == 1),], covariates = covariate_list)[['ATE']]")
  eval(parse(text = est_formula_IP))
  
  est_IP_alt <- paste0("est_IP_", covariate, "_alt")
  est_formula_IP_alt <- paste0(est_IP_alt, " <- IP_estimate(df_eval_alt[which(df_eval_alt$", covariate, " == 1),], covariates = covariate_list)[['ATE']]")
  eval(parse(text = est_formula_IP_alt))
  
  ## E) Lin estimator
  est_Lin <- paste0("est_Lin_", covariate)
  if (covariate %in% c("religion_christian","religion_muslim")){
    covariate_lin_temp <- as.formula(paste('~', paste0(
      c('willingness_imputed', 'get_vaccinated_imputed'),
      collapse = ' + ')))
  }else {
    covariate_lin_temp<-covariate_lin
  }
  
  est_formula_Lin <- paste0(est_Lin, " <- estimatr::lm_lin(response ~ treatment_group, data = df_eval[which(df_eval$", covariate, " == 1),], covariates = covariate_lin_temp)")
  eval(parse(text = est_formula_Lin))
  
  est_Lin_alt <- paste0("est_Lin_", covariate, "_alt")
  est_formula_Lin_alt <- paste0(est_Lin_alt, " <- estimatr::lm_lin(response ~ treatment_group, data = df_eval_alt[which(df_eval_alt$", covariate, " == 1),], covariates = covariate_lin_temp)")
  eval(parse(text = est_formula_Lin_alt))
}



###################################################
### code chunk number 31: heterogeneity_results_religion
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'


# Combine model objects
out_list_christian <- list(
  DM = est_DM_religion_christian,
  IMP = est_IMP_religion_christian,
  Lin = est_Lin_religion_christian,
  IP = est_IP_religion_christian
)

out_list_muslim <- list(
  DM = est_DM_religion_muslim,
  IMP = est_IMP_religion_muslim,
  Lin = est_Lin_religion_muslim,
  IP = est_IP_religion_muslim
)
#
out_list_diff_religion <- list(
  DM = compare_models(est_DM_religion_muslim, est_DM_religion_christian)$Difference,
  IMP = compare_models(est_IMP_religion_muslim, est_IMP_religion_christian)$Difference,
  Lin = compare_models(est_Lin_religion_muslim, est_Lin_religion_christian)$Difference,
  IP = compare_models(est_IP_religion_muslim, est_IP_religion_christian)$Difference
)

out_list_diff_religion_alt <- list(
  DM = compare_models(est_DM_religion_christian_alt, est_DM_religion_muslim_alt)$Difference,
  IMP = compare_models(est_IMP_religion_christian_alt, est_IMP_religion_muslim_alt)$Difference,
  Lin = compare_models(est_Lin_religion_christian_alt, est_Lin_religion_muslim_alt)$Difference,
  IP = compare_models(est_IP_religion_christian_alt, est_IP_religion_muslim_alt)$Difference
)

#
out_list_christian <- modelsummary(out_list_christian,
                                   output = 'modelsummary_list')
out_list_muslim<- modelsummary(out_list_muslim,
                               output = 'modelsummary_list')
#
for(x in names(out_list_christian)){
  # add on Concerns - PSA coefficients
  out_list_christian[[x]]$tidy <- rbind(out_list_christian[[x]]$tidy |>
                                          select(term, estimate, std.error, statistic,
                                                 p.value),
                                        broom::tidy(get(paste0('est_', x, '_religion_christian_alt')))[3,] |>
                                          mutate(term = 'treatment_groupConcerns2') |>
                                          select(term, estimate, std.error, statistic,
                                                 p.value))
  
  out_list_muslim[[x]]$tidy <- rbind(out_list_muslim[[x]]$tidy |>
                                       select(term, estimate, std.error, statistic,
                                              p.value),
                                     broom::tidy(get(paste0('est_', x, '_religion_muslim_alt')))[3,] |>
                                       mutate(term = 'treatment_groupConcerns2') |>
                                       select(term, estimate, std.error, statistic,
                                              p.value))
  
  out_list_diff_religion[[x]]$tidy <- rbind(out_list_diff_religion[[x]]$tidy |>
                                              select(term, estimate, std.error, statistic,
                                                     p.value),
                                            out_list_diff_religion_alt[[x]]$tidy[
                                              which(out_list_diff_religion_alt[[x]]$tidy$term == 'treatment_groupConcerns'),] |>
                                              mutate(term = 'treatment_groupConcerns2') |>
                                              select(term, estimate, std.error, statistic,
                                                     p.value))
  
  # drop p-values for control condition so no stars
  out_list_christian[[x]]$tidy[which(out_list_christian[[x]]$tidy$term == '(Intercept)'),
                               c('p.value', 'statistic')] <- NA
  out_list_muslim[[x]]$tidy[which(out_list_muslim[[x]]$tidy$term == '(Intercept)'),
                            c('p.value', 'statistic')] <- NA
}

#
# # Add GRF; different format
out_list_christian[['GRF']] <- format_grf_summary(est_GRF_religion_christian)
out_list_christian[['GRF']]$tidy <- rbind(out_list_christian[['GRF']]$tidy,
                                          format_grf_summary(est_GRF_religion_christian_alt)$tidy[3,] |>
                                            mutate(term = 'treatment_groupConcerns2'))
out_list_christian[['GRF']]$tidy[which(out_list_christian[['GRF']]$tidy$term == '(Intercept)'),
                                 c('p.value', 'statistic')] <- NA

out_list_muslim[['GRF']] <- format_grf_summary(est_GRF_religion_muslim)
out_list_muslim[['GRF']]$tidy <- rbind(out_list_muslim[['GRF']]$tidy,
                                       format_grf_summary(est_GRF_religion_muslim_alt)$tidy[3,] |>
                                         mutate(term = 'treatment_groupConcerns2'))
out_list_muslim[['GRF']]$tidy[which(out_list_muslim[['GRF']]$tidy$term == '(Intercept)'),
                              c('p.value', 'statistic')] <- NA

out_list_diff_religion[['GRF']] <- compare_models(format_grf_summary(est_GRF_religion_muslim),
                                                  format_grf_summary(est_GRF_religion_christian))$Difference
out_list_diff_religion[['GRF']]$tidy <- rbind(out_list_diff_religion[['GRF']]$tidy,
                                              compare_models(format_grf_summary(est_GRF_religion_muslim_alt),
                                                             format_grf_summary(est_GRF_religion_christian_alt))$Difference$tidy |>
                                                filter(term == 'treatment_groupConcerns') |>
                                                mutate(term = 'treatment_groupConcerns2'))


modelsummary(list(`Religion: Christian` = out_list_christian,
                  `Religion: Muslim` = out_list_muslim,
                  Difference = out_list_diff_religion),
             shape= 'rbind',
             output = 'latex',
             coef_map = c('treatment_groupPSA' = 'PSA - Control',
                          'treatment_groupConcerns' = 'Concerns - Control',
                          'treatment_groupConcerns2' = 'Concerns - PSA',
                          '(Intercept)' = 'Control mean'),
             stars = TRUE ,
             gof_map = list(list('raw' = 'nobs',
                                 'clean' = 'n',
                                 'fmt' = f1)),
             escape = FALSE,
             caption= 'Treatment effect estimates and response under alternative approaches to estimation.') |>
  kable_styling(latex_options = c('HOLD_position')) |>
  footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage. The response measure is the combined index. Estimates are average treatment effects, and control mean. Estimating procedures are discussed in the text. Statistical significance is reported only for treatment effect estimates, and differences, not for baseline control means.'
  ),
  escape = FALSE,
  threeparttable = TRUE,
  general_title = '')





###################################################
### code chunk number 32: cost
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| strip.white=TRUE,
#| results='asis'
total_ad <- 8824.20
eval_ad <- 8095.75
nigeria_ad <- 3137.8
kenya_ad <- 4957.95

concern_n <- sum(df_eval$treatment_group == 'Concerns')
concern_nigeria_n <- sum(df_nigeria$treatment_group == 'Concerns')
concern_kenya_n <- sum(df_kenya$treatment_group == 'Concerns')

concern_ad <- eval_ad * concern_n/eval_n
concern_ad_nigeria <- nigeria_ad * concern_nigeria_n/nigeria_n
concern_ad_kenya <- kenya_ad * concern_kenya_n/kenya_n

concern_ad_inc <- concern_ad + 0.75*concern_n
concern_ad_inc_nigeria <- concern_ad_nigeria + 0.75*concern_nigeria_n
concern_ad_inc_kenya <- concern_ad_kenya + 0.75*concern_kenya_n

eval_all <- eval_ad + 0.75*eval_n
pp_all <- eval_ad/eval_n + 0.75# total pp cost (ads + incentives)



###################################################
### code chunk number 33: cost_table
###################################################
#| eval = TRUE,
#| echo=FALSE,
#| warning=FALSE,
#| message=FALSE,
#| strip.white=TRUE,
#| results='asis'
df_eval <- df_eval |>
  mutate(get_vaccinated_bin = 1 * (get_vaccinated_2>2),
         willingness_bin = 1 * (willingness_2>2),
         either_bin = pmax(get_vaccinated_bin, willingness_bin, na.rm = TRUE))

df_kenya <- df_kenya |>
  mutate(get_vaccinated_bin = 1 * (get_vaccinated_2>2),
         willingness_bin = 1 * (willingness_2>2),
         either_bin = pmax(get_vaccinated_bin, willingness_bin, na.rm = TRUE))

df_nigeria <- df_nigeria |>
  mutate(get_vaccinated_bin = 1 * (get_vaccinated_2>2),
         willingness_bin = 1 * (willingness_2>2),
         either_bin = pmax(get_vaccinated_bin, willingness_bin, na.rm = TRUE))

est_bin_intention <- lm_lin(get_vaccinated_bin ~ treatment_group, data = df_eval,
                            covariates = covariate_lin)

est_bin_willingness <- lm_lin(willingness_bin ~ treatment_group, data = df_eval,
                              covariates = covariate_lin)

est_bin_either <-  lm_lin(either_bin ~ treatment_group, data = df_eval,
                          covariates = covariate_lin)

est_bin_intention_kenya <- lm_lin(get_vaccinated_bin ~ treatment_group,
                                  data = df_kenya,
                                  covariates = covariate_lin)
est_bin_intention_nigeria <- lm_lin(get_vaccinated_bin ~ treatment_group,
                                    data = df_nigeria,
                                    covariates = covariate_lin)

est_bin_willingness_kenya <- lm_lin(willingness_bin ~ treatment_group,
                                    data = df_kenya,
                                    covariates = covariate_lin)
est_bin_willingness_nigeria <- lm_lin(willingness_bin ~ treatment_group,
                                      data = df_nigeria,
                                      covariates = covariate_lin)

est_bin_either_kenya <-  lm_lin(either_bin ~ treatment_group, data = df_kenya,
                                covariates = covariate_lin)

est_bin_either_nigeria <-  lm_lin(either_bin ~ treatment_group, data = df_nigeria,
                                  covariates = covariate_lin)

out_list <- modelsummary(list(Intention = est_bin_intention,
                              Willingness = est_bin_willingness,
                              Either = est_bin_either),
                         output = 'modelsummary_list')
out_list_kenya <- modelsummary(list(Intention = est_bin_intention_kenya,
                                    Willingness = est_bin_willingness_kenya,
                                    Either = est_bin_either_kenya),
                               output = 'modelsummary_list')
out_list_nigeria <- modelsummary(list(Intention = est_bin_intention_nigeria,
                                      Willingness = est_bin_willingness_nigeria,
                                      Either = est_bin_either_nigeria),
                                 output = 'modelsummary_list')

for(x in names(out_list)){
  # drop p-values for control condition so no stars
  out_list[[x]]$tidy[which(out_list[[x]]$tidy$term == '(Intercept)'),
                     c('p.value', 'statistic')] <- NA
  out_list_kenya[[x]]$tidy[which(out_list_kenya[[x]]$tidy$term == '(Intercept)'),
                           c('p.value', 'statistic')] <- NA
  out_list_nigeria[[x]]$tidy[which(out_list_nigeria[[x]]$tidy$term == '(Intercept)'),
                             c('p.value', 'statistic')] <- NA
  
  # Number of people influenced
  out_list[[x]]$glance['influenced'] <-
    out_list[[x]]$tidy[
      which(out_list[[x]]$tidy$term == 'treatment_groupConcerns'),
      'estimate'] *
    sum(df_eval$treatment_group == 'Concerns')
  
  out_list_kenya[[x]]$glance['influenced'] <-
    out_list_kenya[[x]]$tidy[
      which(out_list_kenya[[x]]$tidy$term == 'treatment_groupConcerns'),
      'estimate'] *
    sum(df_kenya$treatment_group == 'Concerns')
  
  out_list_nigeria[[x]]$glance['influenced'] <-
    out_list_nigeria[[x]]$tidy[
      which(out_list_nigeria[[x]]$tidy$term == 'treatment_groupConcerns'),
      'estimate'] *
    sum(df_nigeria$treatment_group == 'Concerns')
  
  # Cost per influenced person
  out_list[[x]]$glance['cost'] <- concern_ad/out_list[[x]]$glance['influenced']
  
  out_list_kenya[[x]]$glance['cost'] <- concern_ad_kenya/out_list_kenya[[x]]$glance['influenced']
  
  out_list_nigeria[[x]]$glance['cost'] <-  concern_ad_nigeria/out_list_nigeria[[x]]$glance['influenced']
  
  # Cost per influenced person including incentives
  out_list[[x]]$glance['cost_inc'] <- concern_ad_inc/out_list[[x]]$glance['influenced']
  
  out_list_kenya[[x]]$glance['cost_inc'] <- concern_ad_inc_kenya/out_list_kenya[[x]]$glance['influenced']
  
  out_list_nigeria[[x]]$glance['cost_inc'] <-  concern_ad_inc_nigeria/out_list_nigeria[[x]]$glance['influenced']
}


modelsummary(list(Combined = out_list,
                  Kenya = out_list_kenya,
                  Nigeria = out_list_nigeria),
             shape = 'rbind',
             output = 'latex',
             coef_map = c('treatment_groupConcerns' = 'Concerns - Control',
                          '(Intercept)' = 'Control mean'),
             stars = TRUE ,
             gof_map = list(
               list('raw' = 'cost',
                    'clean' = 'Ad cost per influenced person',
                    'fmt' = fd),
               list('raw' = 'cost_inc',
                    'clean' = 'Total cost per influenced person',
                    'fmt' = fd)),
             escape = FALSE,
             title= 'Estimates on cost per influenced person.') |>
  kable_styling(latex_options = c('HOLD_position')) |>
  footnote(general = paste0('\\\\footnotesize The sample is users in the evaluation stage, $n = $ ',
                            prettyNum(eval_n, big.mark = ','),' overall, $n = $ ',
                            prettyNum(kenya_n, big.mark = ','),' in Kenya, and $n = $ ',
                            prettyNum(nigeria_n, big.mark = ','),' in Nigeria. The response measures are binary transformation of the outcome for the \\\\textit{Intention} and \\\\textit{Willingness} measures, and for the \\\\textit{Either} measure, a variable that takes the value one if either of the other binary transformations is a one, and zero otherwise. Estimates are average treatment effects, and control means. Estimates are produced from a linear estimator, controlling for pre-test response (Lin, 2013). Statistical significance is reported only for treatment effect estimates, not for control means.'
  ),
  escape = FALSE,
  threeparttable = TRUE,
  general_title = '')

min_cost <- min(unlist(lapply(lapply(out_list, `[[`, 2), `[[`, 'cost')))
max_cost <- max(unlist(lapply(lapply(out_list, `[[`, 2), `[[`, 'cost')))

min_total_cost <- min(unlist(lapply(lapply(out_list, `[[`, 2), `[[`, 'cost_inc')))
max_total_cost <- max(unlist(lapply(lapply(out_list, `[[`, 2), `[[`, 'cost_inc')))


###################################################
### code chunk number 34: learning_outcomes
###################################################
#| eval = TRUE,
#| echo=FALSE, warning=FALSE, message=FALSE,
#| fig.align='center',
#| fig.width=12, fig.height=8,
#| prefix.string = 'fig',
#| fig.cap=
#| paste0('\\textbf{Vaccine intention and willingness response estimates in the
#|  learning stage.} ',
#| 'We estimate response separately across concern categories. ',
#| 'The sample is users who expressed only one concern in the learning stage,
#|  $n = $ ',
#| prettyNum(sum(! (duplicated(df_concerns$user_id) |
#| duplicated(df_concerns$user_id, fromLast=TRUE))), big.mark = ','),
#| '. We only consider these users in this analysis, to ensure there is no
#|  spillover across messaging. Estimates are of mean response, produced from
#|  the non-contextual stabilized variance weighting scheme discussed in
#|  Hadad et al. 2013. Error bars represent symmetric 95\\%
#|  confidence intervals.'),
#| strip.white=TRUE,
#| results='asis'
out <- list()
df_concerns1 <- df_concerns[! (duplicated(df_concerns$user_id) |
                                 duplicated(df_concerns$user_id, fromLast=TRUE)) ,]
for(cid in sort(unique(df_concerns$concern_id)) ){
  ddfh <- df_concerns1[which(df_concerns1$concern_id == cid),]
  
  df_probs <- as.matrix(ddfh[,paste0('prob_c', cid, '_m', sort(unique(ddfh$treatment_id) ))])
  
  balwts <- calculate_balwts(as.numeric(as.factor(ddfh$treatment_id)), df_probs)
  
  A <- nrow(ddfh)
  K <- length(unique(as.factor(ddfh$treatment_id)))
  
  aipw_scores_g <- banditsCI::aw_scores(
    ws = as.numeric(as.factor(ddfh$treatment_id)),
    yobs = ddfh$get_vaccinated_2_imputed,
    K = length(unique(as.factor(ddfh$treatment_id))),
    balwts = balwts
  )
  aipw_scores_w <- aw_scores(
    ws = as.numeric(as.factor(ddfh$treatment_id)),
    yobs = ddfh$willingness_2_imputed,
    K = length(unique(as.factor(ddfh$treatment_id))),
    balwts = balwts
  )
  
  policy1 <- lapply(1:K, function(x) {
    pol_mat <- matrix(0, nrow = A, ncol = K)
    pol_mat[,x] <- 1
    pol_mat
  }
  )
  
  df_probs <- df_probs/rowSums(df_probs)
  
  out_full_g <- banditsCI::output_estimates(
    policy1 = policy1,
    gammahat = aipw_scores_g,
    probs_array = df_probs,
    non_contextual_minvar = FALSE,
    non_contextual_stablevar = TRUE,
    non_contextual_twopoint = FALSE,
    contextual_minvar = FALSE,
    contextual_stablevar = FALSE,
    uniform = FALSE)
  
  out_full_w <- banditsCI::output_estimates(
    policy1 = policy1,
    gammahat = aipw_scores_w,
    probs_array = df_probs,
    non_contextual_minvar = FALSE,
    non_contextual_stablevar = TRUE,
    non_contextual_twopoint = FALSE,
    contextual_minvar = FALSE,
    contextual_stablevar = FALSE,
    uniform = FALSE)
  
  
  # Combine the data frames into a single data frame
  combinedMatrix_g <- do.call(rbind,
                              lapply(out_full_g, `[`, 'non_contextual_stablevar',))
  combinedMatrix_w <- do.call(rbind,
                              lapply(out_full_w, `[`, 'non_contextual_stablevar',))
  
  gg_data <- data.frame(
    estimate = c(combinedMatrix_g[,'estimate'], combinedMatrix_w[,'estimate']),
    std.error = c(combinedMatrix_w[,'std.error'], combinedMatrix_w[,'std.error']),
    measure = factor(rep(c('Intent', 'Willingness'), each = K)),
    treatment_id = factor(rep(sort(unique(ddfh$treatment_id)), times = 2)),
    concern_id = cid)
  
  gg_labels <- gg_data |>
    group_by(treatment_id) |>
    summarize(min.est = min(estimate),
              max.est = max(estimate),
              estimate = mean(estimate),
              std.error = max(std.error),
              concern_id = mean(concern_id),
              measure = 'Intent') |>
    mutate(estimate = ((seq_along(estimate))%%2) * (-2.5*std.error + min.est-0.9) + ((seq_along(estimate) + 1)%%2)*(min(2.5*std.error,4) + max.est+2) ) |>
    ungroup() |>
    mutate(label = gsub('\n', '\n\ ', messages[sort(unique(ddfh$treatment_id))]))
  
  # Create the plot
  out[[cid]] <- ggplot(gg_data,
                       aes(y = estimate,
                           x = treatment_id,
                           color = treatment_id,
                           fill = treatment_id,
                           group = measure,
                           shape = measure)) +
    geom_point(aes(y = estimate),
               size = 2,
               position = position_dodge(width=0.75)) +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error), width = 0.05,
                  position = position_dodge(width=0.75), show.legend=FALSE) +
    stat_gradientinterval(aes(x = treatment_id,
                              ydist = distributional::dist_normal(estimate, std.error),
                              fill = treatment_id, group = measure),
                          width = 1,
                          position = position_dodge(0.75),
                          linewidth = 0,
                          point_size = 0,
                          point_alpha = 0,
                          interval_alpha = 0,
                          show.legend = FALSE,
                          fill_type = 'segments') +
    geom_text(data = gg_labels,
              aes(label=label),
              size = 3.6, color = 'grey40',
              hjust = 0.5, lineheight = .8) +
    facet_grid(~concern_id,
               labeller = labeller(concern_id = concerns)) +
    scale_color_manual(labels = as_labeller(messages),
                       values = cbPalette[-c(1, 5)]) +
    scale_fill_manual(labels = as_labeller(messages),
                      values = cbPalette[-c(1, 5)]) +
    vcf_theme() +
    scale_y_continuous(breaks = 1:4) +
    scale_x_discrete(expand = c(.25,.25)) +
    theme(#legend.position = 'None',
      legend.background=element_blank(),
      legend.title.align=0.5, legend.box.just = 'center',
      legend.key = element_rect(fill = NA),
      legend.text=element_text(size=16),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      plot.background = element_rect(fill='transparent',
                                     color = 'transparent'),
      plot.margin = margin(0, -10, 0, 0, 'pt')) +
    coord_cartesian(ylim = c(-1.2,8)) +
    guides(colour = 'none', fill = 'none',
           shape = guide_legend(title='Measure',
                                title.position = 'top'))
  
  if(cid %in% c(2,3,5,6)){
    out[[cid]] <- out[[cid]] +
      theme(axis.text.y=element_text(colour = '#FFFFFF00'),
            axis.ticks.y=element_line(colour = '#FFFFFF00'))
  }
}
legend <- cowplot::get_plot_component(out[[1]], "guide-box", return_all = TRUE)[[3]]
out <- lapply(out, function(x) x + theme(legend.position = 'None'))
out[[8]] <- legend

bottom_text <- grid::textGrob('Messaging', gp = grid::gpar(fontsize = 18))
left_text <- grid::textGrob('Response estimates', gp = grid::gpar(fontsize = 18), rot = 90, vjust = -0.4)

g <- gridExtra::grid.arrange(grobs = out,
                             left = left_text,
                             bottom = bottom_text,
                             vp=grid::viewport(width=0.95, height=0.95))

grid::grid.draw(g)



