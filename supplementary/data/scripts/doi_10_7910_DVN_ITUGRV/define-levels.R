## define levels
library(scales)

## for plots
color_mapping_practice <- c('black', brewer_pal(type = 'qual', palette = 'Set2')(4)[c(1,2,4)])
response_rate_limits <- c(0, 1)
response_rate_breaks = seq(0, 1, by = 0.2)
lower_year <- 2005

## fix numbers in figure
pretty_number <- function(number, digits = 3) {
  prettyNum(signif(number, digits),
            big.mark = ',', decimal.mark = '.',
            scientific = FALSE)
}

# for cleaning main df
qcut <- function(x, n) {
  cut(x, quantile(x, seq(0, 1, length = n + 1), na.rm = TRUE), labels = seq_len(n),
      include.lowest = TRUE)
}

## all types of research
combined_string_career_stage = 'Both'
combined_string_practice <- 'Any'
combined_string_discipline <- 'All'
combined_string_research_type <- 'All'


## factor levels
career_stage_levels = c('PhD Student',
                        'Published Author',
                        combined_string_career_stage
)

practice_levels <- c(combined_string_practice,
                     "Posting data or code online",
                     "Posting study instruments online",
                     "Pre-registering hypotheses or analyses")

research_type_levels = c(combined_string_research_type,
                         'Experimental',
                         'Quantitative non-experimental',
                         'Qualitative or Theoretical'
)

discipline_levels = c('Sociology',
                      'Economics',
                      'Political Science',
                      'Psychology',
                      combined_string_discipline
)

opinion_levels <- as.character(1:5)
favor_levels <- c(`1` = 'Not at all in favor',
                  `2` = 'Moderately not in favor',
                  `3` = 'Neither in favor nor against',
                  `4` = 'Moderately in favor',
                  `5` = 'Very much in favor')
names(opinion_levels) <- favor_levels

## define econ subfields
combined_string_subfield <- 'All'
subfield_levels = c(  'Agricultural Economics',
                      'Behavioral Economics',
                      'Development Economics',
                      'Econometrics',
                      'Economic History',
                      'Economics of Education',
                      'Environmental Economics',
                      'Finance',
                      'Health Economics',
                      'Industrial Organization',
                      'Industrial Relations',
                      'International Economics',
                      'Labor Economics',
                      'Macro Economics',
                      'Others',
                      'Political Economics',
                      'Public Economics',
                      'Theory',
                      'Urban Economics',
                      combined_string_subfield
)

##
publicly_posting_recode_proper <- 'Posting data or \n code online'
analysis_pre_registration_recode_proper <-  'Pre-registering \n hypotheses or analyses'
