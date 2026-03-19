
# FILE:     UnmaskingReplicationSI.R
# AUTHOR:   Austin Hart & J. Scott Matthews
# DATE:     July 14, 2021
# SUBJECT:  REPLICATION FILES, JOURNAL OF POLITICS
#           Supplemental Info/Online Appendix

# Notes for replication:
#   Required packages: tidyverse, knitr, broom, cowplot, lemon, gtable
#   Custom functions & graphical theme defined below


# SETUP ---------------------------------------------------

# load packages
  library(tidyverse)
  library(knitr);library(broom)
  library(cowplot);library(lemon);library(gtable)

# directory (set directory below if you choose)
  # setwd("your.path.here")

# Data for SI
  # main analysis
    load(url("https://www.dropbox.com/s/c9safb3z4bib6wb/HartMatthewsJOP.RData?dl=1"))
  # rand/balance checks
    load(url("https://www.dropbox.com/s/fsq4irn9dfwhlc3/HartMatthewsJOPSI.RData?dl=1"))

    
# CUSTOM FUNCTIONS ---------------------------------------
  
# FctClean() - function to relabel + reorder factors 
  FctClean <- function(facvar, ...){
    fct_recode(fct_relevel(facvar, ...), ...)
  }  
  
# FctCaseWhen() - function to create ordered factor from case_when())    
  FctCaseWhen = function(...) {
    args = as.list(match.call())
    levels = sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
    levels = levels[!is.na(levels)]
    factor(dplyr::case_when(...), levels = levels)
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
  
# shiftLegend - function to move legend within faceted plot  
  shiftLegend = function(p) {
    pnls = cowplot::plot_to_gtable(p) %>% 
      gtable::gtable_filter("panel") %>%
      with(setNames(grobs, layout$name)) %>% 
      purrr::keep(~identical(.x,zeroGrob()))
    if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
    lemon::reposition_legend( p, "center", panel=names(pnls) )
  } 

  
# PARTICIPANTS ----------------------------------
  
# Attention
  fct_count(as_factor(dfsi$Attention[!is.na(dfsi$Female)]), prop = T)
  
# Duration
  summary(dfsi$DurationSec/60)
  
# Demographics
  fct_count(dfsi$Female, prop = T)
  fct_count(dfsi$RaceEth, prop = T)
  fct_count(dfsi$Age, prop = T)
  fct_count(dfsi$Edu, prop = T)
  

# COMPREHENSION ---------------------------------
  
# Overall
  dfsi %>% 
    filter(!is.na(RulesTotal)) %>%
    count(RulesTotal) %>%
    mutate(prop = 100 * n/sum(n))
  
  chisq.test(table(dfsi$RulesTotal))
  
# By study
  dfsi %>%
    filter(!is.na(RulesTotal)) %>%
    group_by(Study, RulesTotal) %>%
    summarise(n = n()) %>%
    mutate(perc = 100 * n/sum(n)) %>%
    select(-n)%>%
    pivot_wider(
      names_from = Study, 
      values_from = perc
    ) %>%
    knitr::kable(digits = 1, format = 'latex')
  
  
# RANDOMIZATION ---------------------------------
  
# worker type
  # Isolate types and stack
    type = 
      dfsi %>%
      select(TypeA,TypeB) %>%
      pivot_longer(
        cols = c(TypeA,TypeB),
        names_to = 'Worker',
        values_to = 'Competence'
      )
    
    summary(type$Competence)
  
  # Plot
    pt = 
      type %>%
      ggplot(aes(x = Competence, ..density..)) +
      geom_histogram(color = "gray20", fill = "gray", binwidth = 26) +
      scale_y_continuous(limits = c(0,0.003), expand = c(0,0)) +
      scale_x_continuous(limits = c(900,1500)) +
      labs(title = expression("(a) " ~ mu[w] %~% U(950,1450)), 
           y = "Density", x = "Worker Type (units/wk)") +
      mytheme
    
    rm(type)
  
  # Check average output by assigned type
    out =
      bind_rows(
        dfsi %>% select(type = TypeA, avg = AvgA),
        dfsi %>% select(type = TypeB, avg = AvgB)
      )
  
    rvl = 
      out %>%
      ggplot(aes(x = type, y = avg)) +
      geom_bin2d(bins = 20, show.legend = F) +
      geom_abline(slope=1, intercept=0) +
      geom_smooth(fill = "cornflowerblue", color = 'white', method = 'loess', se = T) +
      coord_cartesian(ylim = c(850,1550),
                      xlim = c(920,1480)) +
      scale_x_continuous(breaks = c(1000,1200,1400)) +
      scale_fill_gradient2(low="gray70", high = "gray10") +
      labs(title = expression("(b) " ~ E(Y[w]) == mu[w]),
           y = "Output (8 week average)", x = "Assigned worker type") +
      mytheme
  
    summary(lm(avg ~ type, data = out))    
  
    rm(out)
  
  # Merge plots and save
    chx = gridExtra::grid.arrange(pt, rvl, ncol = 2)
  
    ggsave(filename = "mu_checks.pdf",
           plot = chx,
           width = 6,
           height = 2.5,
           dpi = 1000) 

      
# BALANCE ---------------------------------------
    
# Study 1
  b1 =
    dfsi %>%
    filter(Study == 1) %>%
    aov(NoiseS1 == "Low (100)" ~ 
          Age + Edu + RaceEth + Female + as.factor(RulesTotal), data = .) %>%
    broom::tidy() %>%
    .[1:5,c(1,5,6)]

# Study 4        
  b4 =
    dfsi %>%
    filter(Study == 4) %>%
    aov(Design == 'Unmasking' ~ 
          Age + Edu + RaceEth + Female + as.factor(RulesTotal), data = .) %>%
    broom::tidy() %>%
    .[1:5,c(1,5,6)]
  
# Study 5  
  b5 =
    dfsi %>%
    filter(Study == 5) %>%
    aov(Design == 'Unmasking' ~ 
          Age + Edu + RaceEth + Female + as.factor(RulesTotal), data = .) %>%
    broom::tidy() %>%
    .[1:5,c(1,5,6)]
  
# Combine
  left_join(b1,b4, by = 'term', suffix = c('.1','.2')) %>%
    left_join(b5, by = 'term')
    
  rm(b1,b4,b5)
  
  
# RETENTION -------------------------------------
 
# Figure 2, SI: baseline response to production
  # Stack and clean for graphing retention functions
    df =
      dfmain %>%
      pivot_longer(
        cols = c(AvgPre,AvgPost),
        names_to = 'Period',
        values_to = 'Production'
      ) %>%
      transmute(
        VoteB,
        Production,
        Period = FctClean(
          Period,
          '"B"^"+"' = "AvgPre",
          '"B"["post"]' = "AvgPost"
        ),
        Sfac = FctCaseWhen(
          Study == 1 ~ '1. Unmasking (n = 849)',
          Study == 2 ~ '2. Recognition (n = 368)',
          Study == 3 ~ '3. Unmasking (n = 550)',
          Study == 4 ~ '4. Unm/Rec (n = 720)',
          Study == 5 ~ '5. Unm/Rec (n = 1376)'
        )
      ) %>%
      group_by(Sfac,Period) %>%
      mutate(
        ProdZ = (Production - mean(Production))/sd(Production)
      ) %>%
      ungroup()
    
    glabs = parse(text = c('"Pre-Incumbent, F"["pre"]',
                           '"Incumbent tenure, F"["post"]'))
    
  # Plot retention functions    
    lp = 
      df %>%
      ggplot( aes(y = VoteB, x = Production) ) +
      facet_wrap( vars(Sfac), nrow = 2 ) +
      stat_smooth(
        aes( linetype = Period, color = Period ),
        method = 'loess', span = .94, se = FALSE
      ) +
      scale_color_manual( values = c( 'black','gray39' ), labels = glabs ) +
      scale_linetype_manual( values = c( 'dotted','solid' ), labels = glabs ) +
      coord_cartesian( xlim = c(700,1700), ylim = c(0,1)) +
      scale_x_continuous( breaks = c(950,1200,1450) ) +
      scale_y_continuous( limits = c(-1,2), expand = c(0,.01),
                          breaks = c(0,0.5,1) ) +
      labs(
        y = 'Proportion retaining incumbent',
        x = 'Factory output (units/worker/week)',
        linetype = 'Production Period',
        color = 'Production Period'
      ) +
      mytheme 
    
  # Shift legend to open facet  
    lpf = shiftLegend(lp)
    
  # Save final figure    
    ggsave(filename = "baseline.png", plot = lpf, width = 6, height = 3)
  
    rm(lp,lpf,glabs,df)
    

  
# MODEL ESTIMATES -------------------------------
  
# Full Equation 1 estimates (Table 3, SI)
  # Isolate data
    df =
      dfmain %>%
      select(VoteB,AvgPre,AvgPost,Study,Design)
  
  # Estimated performance vote by study
    Base = 
      df %>%
      split( f = list(.$Study,.$Design), drop = TRUE ) %>%
      map_df(
        ~ tidy(lm(VoteB ~ AvgPre + AvgPost, data = .x)),
        .id = 'Study'
      ) 
    
  # Primary estimates  
    Parms =
      Base %>%
      select(-std.error,-statistic) %>%
      filter(term != '(Intercept)') %>%
      pivot_wider(
        values_from = c(estimate,p.value),
        names_from = c(term)
      ) %>%
      mutate_at(
        vars(starts_with('estimate')),
        ~.*100
      ) %>%
      mutate_at(
        vars(starts_with('p.value')),
        ~.*0.5 # 1-tailed test
      ) %>%
      arrange(Study) %>%
      .[, c(1, 2, 4, 3, 5)] # reorder columns
    
  # Intercept
    Int = 
      Base %>%
      filter(term == '(Intercept)') %>%
      select(Study,estimate,std.error) %>%
      arrange(Study) %>%
      rename(Intercept = estimate)
    
  # Join
    left_join(Parms,Int) %>%
      .[c(1,2,3,5,4,7,6),]
    
    rm(df,Base,Parms,Int)
  
# Alternate, using assigned types   
  # Isolate data
    df =
      dfmain %>%
      select(VoteB,TypeA,TypeB,Study,Design)
    
  # Estimated performance vote by study
    Base = 
      df %>%
      split( f = list(.$Study,.$Design), drop = TRUE ) %>%
      map_df(
        ~ tidy(lm(VoteB ~ TypeA + TypeB, data = .x)),
        .id = 'Study'
      ) 
    
  # Primary estimates  
    Parms =
      Base %>%
      select(-std.error,-statistic) %>%
      filter(term != '(Intercept)') %>%
      pivot_wider(
        values_from = c(estimate,p.value),
        names_from = c(term)
      ) %>%
      mutate_at(
        vars(starts_with('estimate')),
        ~.*100
      ) %>%
      mutate_at(
        vars(starts_with('p.value')),
        ~.*0.5 # 1-tailed test
      ) %>%
      arrange(Study) %>%
      .[, c(1, 2, 4, 3, 5)] # reorder columns
    
  # Intercept
    Int = 
      Base %>%
      filter(term == '(Intercept)') %>%
      select(Study,estimate,std.error) %>%
      arrange(Study) %>%
      rename(Intercept = estimate)
    
  # Join
    left_join(Parms,Int) %>%
      .[c(1,2,3,5,4,7,6),]
    
    rm(df,Base,Parms,Int)

        