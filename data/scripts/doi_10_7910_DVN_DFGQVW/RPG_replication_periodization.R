rm(list=setdiff(ls(), 'theme.plot'))

source('RPG_replication_aux_functions.R')

# Loop over schemes/periodization
# Takes a while to run but shouldn't be too intensive
# on a modern laptop
store.markov <- readRDS('./final_data/markov_output.RDS')

spkrs <- setdiff(unique(store.markov$first_speaker), 
                 c('other', 'unclear'))
print(spkrs)

for (scheme.id in 1:5){
  message(scheme.id)
  scheme.markov <- collapse.data.by.scheme(data = store.markov, outcome = count,
                                           scheme.id = scheme.id, limit.scheme = TRUE)
  
  s.activity.data  <- scheme.markov %>% group_by(chamber, committee, cabinet, cabinet_number,  
                                                 budget, period, committee.type, first_speaker) %>% 
    #Get total number of speeches by first_speaker.
    summarize(value = sum(value), total.committee = unique(total.committee)) %>%
    #Get proportion of activity by type.
    mutate(pr.activity = value/total.committee)
  s.activity.data <- s.activity.data %>% filter(first_speaker %in% spkrs)
  s.activity.regressions <- s.activity.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker) %>% 
    do(
      fmt.regression(
        plm(formula = pr.activity ~ period, data = ., index = 'committee'), 
        se.type = 'robust')
    )
  
  s.discursive.data  <- scheme.markov %>% filter(first_speaker %in% spkrs & second_speaker %in% spkrs)
  s.discursive.data <- s.discursive.data %>% mutate(pr.discursive = value/total)
  
  s.discursive.regressions <- s.discursive.data %>% filter(period != 'Pre-1955') %>% group_by(chamber, first_speaker, second_speaker) %>% 
    do(
      fmt.regression(
        plm(formula = pr.discursive ~ (period), data = ., index = 'committee'), 
        se.type = 'robust')
    )
  s.activity.regressions <- clean.columns(s.activity.regressions)
  s.discursive.regressions <- clean.columns(s.discursive.regressions)
  
  pdf(glue('figures/fig_a9_scheme_{scheme.id}_FE_reg_pr.pdf'))
  print(
    ggplot(s.activity.regressions, 
           aes(x=fmt.name,y=est,ymin=est-1.96*se,ymax=est+1.96*se, 
               linetype = fmt.chamber, pch = fmt.chamber,
               group=fmt.chamber)
    ) + theme.plot + facet_wrap(. ~ fmt_first_speaker, nrow = 2) +
      ylim(c(-0.20, .20))
    
  )
  dev.off()
  
  pdf(glue('figures/fig_a10_scheme_{scheme.id}_FE_da_reg_pr.pdf'))
  print(
    ggplot(s.discursive.regressions %>% filter(first_speaker == 'opposition'  & !grepl(fmt.name, pattern='Intercept')),
           aes(x = fmt.name, y= est, ymin = est-1.96*se, ymax=est+1.96*se, group=fmt.chamber,
               linetype = fmt.chamber, pch = fmt.chamber)) +
      theme.plot + facet_wrap(. ~ fmt_second_speaker, nrow = 2) +
      ylim(c(-0.35, 0.35))
    
  )
  dev.off()
}
