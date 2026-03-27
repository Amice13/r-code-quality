require(dplyr); require(stringr); require(xtable)
require(readxl)
# Create two miscellaneous tables for the SI
# First, Tables A.1 and A.2 report the most common suffixes
# across the entire time period.

top_fifty <- readRDS('final_data/top_50.RDS')
top_fifty <- top_fifty %>%
  mutate(fmt_speaker = recode_factor(speaker.type,
    'bureaucrat' = 'Bureaucrat',
    'pm' = 'PM',
    'minister' = 'Minister',
    'junior.minister' = 'Junior Min.',
    'viceminister' = 'Vice Min.',
    'parliamentarysec' = 'Parl. Sec',
    'committee.chair' = 'Chair',
    'ordinarymember' = 'MP',
    'other' = 'Other'
  ))
top_fifty <- top_fifty %>%
  mutate(suffix = recode(suffix, 'imputed.OM' = "\u541B")) %>%
  mutate(suffix = ifelse(is.na(suffix), 'Unclear', suffix))
top_fifty <- top_fifty %>% rename(Suffix = suffix)

top_fifty %>% 
  filter(Suffix == "政府委員") %>%
  group_by(chamber, speaker.type) %>% summarize(sum = sum(n)) %>%
  group_by(chamber) %>% mutate(ratio = sum/sum(sum))

suffix_hor <- top_fifty %>% filter(chamber == 'syugiin') %>%
  reshape2::dcast(Suffix + total_n ~ fmt_speaker, value.var = 'n', fill = '') %>%
  select(-matches('total_n'), Total = total_n) %>%
  arrange(desc(Total))

suffix_hoc <- top_fifty %>% filter(chamber == 'sangiin') %>%
  reshape2::dcast(Suffix + total_n ~ fmt_speaker, value.var = 'n', fill = '') %>%
  select(-matches('total_n'), Total = total_n) %>%
  arrange(desc(Total))

suffix_hor <- print.xtable(xtable(suffix_hor), include.rownames = FALSE)
suffix_hor <- str_split(suffix_hor, pattern='\n')[[1]]

suffix_hor[grep(suffix_hor, pattern='begin.table')] <- paste0(suffix_hor[grep(suffix_hor, pattern='begin.table')], 
  '\n\\caption{Distribution of Suffixes in the Diet Committee Record (House of Representatives)}\n\\resizebox{\\textwidth}{!}{')

suffix_hor[grep(suffix_hor, pattern='end.tabular')] <-
  paste0(
    suffix_hor[grep(suffix_hor, pattern='end.tabular')] ,
    '}\n\\label{tab:suffixhr}\n',
    '\\caption*{\\footnotesize Note: Table lists the 50 most frequent suffixes following speaker names in the House of Representatives committee records, and how each was categorized into different speaker groups for the analysis.}'
  )



suffix_hoc <- print.xtable(xtable(suffix_hoc), include.rownames = FALSE)
suffix_hoc <- str_split(suffix_hoc, pattern='\n')[[1]]

suffix_hoc[grep(suffix_hoc, pattern='begin.table')] <- paste0(suffix_hoc[grep(suffix_hoc, pattern='begin.table')], 
  '\n\\caption{Distribution of Suffixes in the Diet Committee Record (House of Councillors)}\n\\resizebox{\\textwidth}{!}{')

suffix_hoc[grep(suffix_hoc, pattern='end.tabular')] <-
  paste0(
    suffix_hoc[grep(suffix_hoc, pattern='end.tabular')] ,
    '}\n\\label{tab:suffixhc}\n',
    '\\caption*{\\footnotesize Note: Table lists the 50 most frequent suffixes following speaker names in the House of Councillors committee records, and how each was categorized into different speaker groups for the analysis.}'
  )

writeLines(suffix_hor, 'figures/tab_a1_suffix_hor.tex', useBytes = T)
writeLines(suffix_hoc, 'figures/tab_a2_suffix_hoc.tex', useBytes = T)

# Second, Tables A.3 and A.4 show the mapping used to ensure
# the standing committees are consistent across renamings

standing_table_hor <- read_excel('final_data/Standing_committees.xlsx', 
  sheet = 'House of Representatives')

standing_table_hoc <- read_excel('final_data/Standing_committees.xlsx', 
  sheet = 'House of Councillors')

names(standing_table_hoc)[1] <- names(standing_table_hor)[1] <- 'Jurisdiction'
standing_table_hoc <- standing_table_hoc %>% select(Jurisdiction, matches('19[56789]|20'))
standing_table_hor <- standing_table_hor %>% select(Jurisdiction, matches('19[56789]|20'))

standing_table_hoc <- standing_table_hoc[-1:-2,]
standing_table_hor <- standing_table_hor[-1:-2,]

standing_table_hor <- standing_table_hor[rowMeans(is.na(standing_table_hor[,-1] %>% data.frame)) != 1,]
standing_table_hoc <- standing_table_hoc[rowMeans(is.na(standing_table_hoc[,-1] %>% data.frame)) != 1,]

names(standing_table_hor) <- gsub(gsub(names(standing_table_hor), pattern='[月年]', replacement = '-'),
  pattern='日', replacement = '')
names(standing_table_hoc) <- gsub(gsub(names(standing_table_hoc), pattern='[月年]', replacement = '-'),
  pattern='日', replacement = '')

# May not work on some Windows machines if "sourced"

standing_table_hoc <- standing_table_hoc %>% mutate(across(everything(), 
   ~ gsub(., pattern='委員会$', replacement = '')))
standing_table_hor <- standing_table_hor %>% mutate(across(everything(), 
   ~ gsub(., pattern='委員会$', replacement = '')))

# Swap out katakana middle dot for a slightly different middle dot
standing_table_hoc <- standing_table_hoc %>% mutate(across(everything(), 
   ~ gsub(., pattern='\u30fb', perl = T, replacement = '·')))
standing_table_hor <- standing_table_hor %>% mutate(across(everything(), 
   ~ gsub(., pattern='\u30fb', perl = T, replacement = '·')))

#Remove parentheticals
remove_paren <- '(?<=\\()Foreign Affairs |.Education, Science and Technology.|.Health and Labor.|.Agriculture, Forestry and Fisheries.'

standing_table_hoc$Jurisdiction <- gsub(standing_table_hoc$Jurisdiction,
  pattern=remove_paren, replacement = '', perl = T)
standing_table_hor$Jurisdiction <- gsub(standing_table_hor$Jurisdiction,
  pattern=remove_paren, replacement = '', perl = T)



standing_table_hoc <- xtable(standing_table_hoc)
standing_table_hor <- xtable(standing_table_hor)


standing_table_hor <- print(standing_table_hor, include.rownames = FALSE)
standing_table_hoc <- print(standing_table_hoc, include.rownames = FALSE)

# standing_table_hor <- gsub(standing_table_hor, pattern='(?<=tabular\\}\\{).', replacement = 'p{2cm}', perl = T)
# standing_table_hoc <- gsub(standing_table_hoc, pattern='(?<=tabular\\}\\{).', replacement = 'p{2cm}', perl = T)


standing_table_hor <- str_split(standing_table_hor, pattern='\n')[[1]]
standing_table_hor[grep(standing_table_hor, pattern='begin.table')] <- paste0(standing_table_hor[grep(standing_table_hor, pattern='begin.table')], 
                                                              '\n\\caption{Standing Committees of the House of Representatives}\n\\resizebox{\\textwidth}{!}{')

standing_table_hor[grep(standing_table_hor, pattern='end.tabular')] <-
  paste0(
    standing_table_hor[grep(standing_table_hor, pattern='end.tabular')] ,
    '}\n\\label{tab:standinghr}\n',
    '\\caption*{\\footnotesize Note: Table lists the names (in Japanese) of the standing committees in the House of Representatives over time, and how each is grouped (left column label) for the purposes of including committee fixed effects in the analyses.}'
  )

writeLines(standing_table_hor, 'figures/tab_a3_standing_hor.tex', useBytes = T)


standing_table_hoc <- str_split(standing_table_hoc, pattern='\n')[[1]]
standing_table_hoc[grep(standing_table_hoc, pattern='begin.table')] <- paste0(standing_table_hoc[grep(standing_table_hoc, pattern='begin.table')], 
                                                                              '\n\\caption{Standing Committees of the House of Councillors}\n\\resizebox{\\textwidth}{!}{')

standing_table_hoc[grep(standing_table_hoc, pattern='end.tabular')] <-
  paste0(
    standing_table_hoc[grep(standing_table_hoc, pattern='end.tabular')] ,
    '}\n\\label{tab:standinghc}\n',
    '\\caption*{\\footnotesize Note: Table lists the names (in Japanese) of the standing committees in the House of Councillors over time, and how each is grouped (left column label) for the purposes of including committee fixed effects in the analyses.}'
  )

writeLines(standing_table_hoc, 'figures/tab_a4_standing_hoc.tex', useBytes = T)
