
# FILE:     UnmaskingReplication.R
# AUTHOR:   Austin Hart & J. Scott Matthews
# DATE:     July 7, 2021
# SUBJECT:  REPLICATION FILES, JOURNAL OF POLITICS
#           Main Analysis for Exps 1 - 5

# Notes for replication:
#   Required packages: tidyverse, knitr, broom
#   User-written functions & graphical theme defined below


# SETUP ---------------------------------------------------

# load packages
  library(tidyverse)
  library(knitr);library(broom)

# directory (set directory below if you choose)
  # setwd("your.path.here")

# Data for main text, experiments 1-5    
  load(url("https://www.dropbox.com/s/c9safb3z4bib6wb/HartMatthewsJOP.RData?dl=1"))

# CUSTOM FUNCTIONS ---------------------------------------
  
# FctClean() - function to relabel + reorder factors 
  FctClean <- function(facvar, ...){
    fct_recode(fct_relevel(facvar, ...), ...)
  }  
  
# Custom theme for visuals
  mytheme = theme_bw() + theme(
    plot.title = element_text(hjust = 0, size = 10, face = 'bold'),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"), size = 10),
    axis.text.y = element_text(size = 10), axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = 'gray80',linetype = 'solid',size = 0.35),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    panel.spacing = unit(2, "lines"),
    plot.margin=unit(c(.2,.2,.2,.2),"cm"),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, face = 'bold')
  )    
  

# RETENTION FUNCTIONS -------------------------------------

# Filter for Table 4
  df =
    dfmain %>%
    select(VoteB,AvgPre,AvgPost,Study,Design)
  
# Table 4: estimated performance vote by study
  df %>%
    split( f = list(.$Study,.$Design), drop = TRUE ) %>%
    map_df(
      ~ broom::tidy( lm(VoteB ~ AvgPre + AvgPost, data = .x) ),
      .id = 'Study'
    ) %>%
    select( -std.error, -statistic ) %>%
    filter( term != '(Intercept)' ) %>%
    pivot_wider(
      values_from = c( estimate, p.value ),
      names_from = c( term )
    ) %>%
    mutate_at( # scale performance in 100s of units
      vars(starts_with( 'estimate' )),
      ~ . * 100
    ) %>%
    mutate_at( # 1-sided tests
      vars(starts_with( 'p.' )),
      ~ . * 0.5
    ) %>%
    arrange(Study) %>%
    .[, c(1, 2, 4, 3, 5)] # reorder columns


# summary: baseline retention rate
  df %>% # by study
    group_by( Study ) %>%
    summarise( Reappointed = 100 * mean(VoteB) )
  
  mean(100 * df$VoteB) # overall
  
# remove objects  
  rm(df)  
  
  
# STUDY 1 ---------------------------------------
  
# Isolate data
  df1 = 
    dfmain %>%
    filter(Study == 1)
  
# Table 5: Unmasking incumbent performance
  # Estimates shown
    df1 %>%
      lm(VoteB ~ 0 + NoiseS1 + I(AvgAB/100):NoiseS1 + I(AvgA/100):NoiseS1, data = .) %>%
      summary()
  
  # Tests: differences/interaction effect 
    df1 %>%
      mutate(NoiseS1 = relevel(as.factor(NoiseS1), ref = 'Low (100)')) %>%
      lm(VoteB ~ I(AvgAB/100)*NoiseS1 + I(AvgA/100)*NoiseS1, data = .) %>%
      summary() # divide p-values by 2 for 1-tailed tests    
    
  rm(df1)  

  
# STUDY 4 ---------------------------------------
  
# Isolate Study 4 data
  df4 =
    dfmain %>%
    filter(Study == 4) %>%
    select(Design,Report1.S4,Report2.S4)
  
# Figure 1: a preference for benchmarking
  # Name and mutate
    df4 =
      df4 %>%
      mutate_all(as.character) %>%
      rename(
        Rep1 = Report1.S4,
        Rep2 = Report2.S4
      )
    
  # Tabulate report selections 
    iso =
      df4 %>%
      pivot_longer(
        -Design,
        names_to = 'name',
        values_to = 'Choice'
      ) %>%
      group_by(Design,Choice) %>%
      summarise(n = n()) %>%
      mutate(
        prop = 100 * n/sum(n),
        grp = 'Individual selections'
      )
    
  # Identify & tabulate choice combinations    
    com =
      df4 %>%
      mutate(
        Choice = case_when(
          Rep1 == 'B' & Rep2 != 'AvB' ~ 'Per',
          c(Rep1 %in% 'A' & Rep2 %in% 'B') | c(Rep1 %in% 'AvB' | Rep2 %in% 'AvB') ~ 'Ben',
          TRUE ~ 'Irr'
        )
      ) %>%
      group_by(Design,Choice) %>%
      summarize(n = n()) %>%
      mutate(
        prop = 100 * n/sum(n),
        grp = 'Combination'
      ) %>%
      ungroup()
    
  # Append and factor
    com =
      com %>%
      bind_rows(iso) 
    
    com$Choice = as_factor(com$Choice)
    
    com =
      com %>%
      mutate(
        Choice = FctClean(
          Choice,
          'Incumbent avg' = 'B',
          'Comparator avg' = 'A',
          'Contrast' = 'AvB',
          'Bonus to date' = 'Y',
          '(none)' = 'None',
          'Inc-centered' = 'Per',
          'Benchmarked' = 'Ben',
          'Irrelevant' = 'Irr'
        )
      )
    
  # Visualize
    f1 = 
      com %>%
      ggplot( aes(x = Choice, y = prop )) +
      facet_grid( ~ fct_rev(grp), scales = 'free', space = 'free' ) +
      geom_point(aes(shape = fct_rev(Design)), 
                 color = 'black', size = 2.75, position = position_dodge(width = 0.25)) +
      labs(
        x = NULL,
        y = 'Percent who chose the report',
        color = 'Task design',
        shape = 'Task design'
      ) +
      scale_y_continuous(expand = c(0,0),
                         breaks = seq(0, 100, 50),
                         labels = c('0','50','100%')) +
      coord_cartesian(ylim = c(0,103)) +
      scale_shape_manual(values = c(24,19)) +
      mytheme +
      theme(
        axis.text.x = element_text(angle = 90,vjust = 0.4,hjust=1)
      )
    
  # Export  
    ggsave(filename = "fg1.png", plot = f1, 
           width = 6, height = 3, dpi = 1200)  
   
    
# model estimates by report chosen
  # Isoalte Data and Vars from main
    df4 = 
      dfmain %>%
      select(Design,Report1.S4,Report2.S4,VoteB,AvgPre,AvgPost) %>%
      mutate_at(c('Report1.S4','Report2.S4'),as.character)
    
    df4 =
      df4 %>%
      mutate(
        Choice = case_when(
          Report1.S4 == 'B' & Report2.S4 != 'AvB' ~ 'Per',
          c(Report1.S4 %in% 'A' & Report2.S4 %in% 'B') | c(Report1.S4 %in% 'AvB' | Report2.S4 %in% 'AvB') ~ 'Ben',
          TRUE ~ 'Irr'
        ),
        choiceComp = if_else(Choice == 'Ben','Benchmarker','Else')
      )
    
    df4 %>%
      split(list(.$Design,.$choiceComp)) %>%
      map_df(
        ~ tidy(lm(VoteB ~ AvgPre + AvgPost, data = .x)),
        .id = 'Arm.Choice'
      ) %>%
      select(-std.error,-statistic) %>%
      filter(term != '(Intercept)') %>%
      pivot_wider(
        values_from = c(estimate,p.value),
        names_from = c(term)
      ) %>%
      mutate_at(
        vars(starts_with('estimate')),
        ~ 100 * .
      ) %>%
      arrange(Arm.Choice) %>%
      .[, c(1, 2, 4, 3, 5)] # reorder columns  
    
# clean enviro   
  rm(f1,com,iso,df4)
    
  
    
# STUDY 5 ---------------------------------------
  
# Isolate data
  df5 = 
    dfmain %>%
    filter(Study == 5) %>%
    select(Study,Design,VoteB,AvgPost,AvgPre,ReportS5)
  
# Figure 2: can targeted cues mitigate benchmarking?  
  # Estimate models
    me = # extraction game
      df5 %>%
      filter(Design == 'Unmasking') %>%
      lm(VoteB ~ 0 + ReportS5 + I(AvgPost/100):ReportS5 + I(AvgPre/100):ReportS5, data = .) 
    
    mr = # recognition game
      df5 %>%
      filter(Design == 'Recognition') %>%
      lm(VoteB ~ 0 + ReportS5 + I(AvgPost/100):ReportS5 + I(AvgPre/100):ReportS5, data = .) 
    
  # Store estiamtes/ses
    emods =
      tibble(
        game = c(rep('Unmasking',6),rep('Recognition',6)),
        report = rep(c('Incumbent Avg','Pre-Benchmarked','Bonus (control)'),4),
        vars = c(rep('Incumbent Tenure',3),rep('Pre-Incumbent',3),
                 rep('Incumbent Tenure',3),rep('Pre-Incumbent',3)),
        coefs = c(me$coefficients[4:9],mr$coefficients[4:9]),
        ses = c(sqrt(diag(vcov(me)))[4:9],sqrt(diag(vcov(mr)))[4:9])
      ) %>%
      mutate(
        game = fct_rev(game),
        report = factor(report, 
                        levels = c('Incumbent Avg','Pre-Benchmarked','Bonus (control)'))
      )
    
  # Plot
    f2 =
      emods %>%
      ggplot(aes(x = fct_rev(report), y = coefs, 
                 fill = fct_rev(vars), shape = fct_rev(vars))) +
      facet_grid(game ~., scales = 'free_y') +
      geom_linerange(aes(ymin = coefs - 1.96*ses, ymax = coefs + 1.96*ses),
                     size = 1) +
      geom_point(size = 3, color = 'black') +
      coord_flip() +
      geom_hline(yintercept = 0) +
      mytheme +
      scale_fill_manual(values = c('white','black')) +
      scale_shape_manual(values = c(21,22,24)) +
      labs(x = NULL,
           y = 'Effect of performance\non Incumbent retention',
           color = NULL,
           shape = NULL,
           fill = NULL) +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color = 'gray80'),
            legend.position = 'right')
    
    ggsave(filename = 'fg2.png', plot = f2, height = 3, width = 5.5, dpi=1200)

  # Estimated performance vote by design & by treatment arm     
    df5 %>%
      mutate(Report = relevel(ReportS5, ref = 'Bonus to date')) %>%
      lm(VoteB ~ I(AvgPost/100)*ReportS5 + I(AvgPre/100)*ReportS5, data = .) %>%
      summary()
    
    df5 %>%
      mutate(Report = relevel(ReportS5, ref = 'Bonus to date')) %>%
      lm(VoteB ~ I(AvgPost/100)*ReportS5 + I(AvgPre/100)*ReportS5, data = .) %>%
      summary()
    
# clean objects
  rm(f2,emods,me,mr,df5)
  
  