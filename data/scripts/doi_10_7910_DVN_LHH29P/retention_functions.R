
get_weekly_retention = function(retention_df, period, path, n_weeks_20){
  # Get pathway and time-period cut
  df <- retention_df %>%
    filter(!!sym(glue("g_pathway_{path}")) == 1,
           !!sym(glue("t_period_{period}")) == 1)

  # Get
  df <- df %>%
    group_by(UL_id_unique) %>%
    arrange(person_week) %>%
    mutate(total_weeks = max(person_week), # how many weeks could you have been paid
           worked_week = any_work * person_week,
           last_week = max(worked_week, na.rm = T), # final week you were paid
           survived = as.numeric(person_week <= last_week),
           # get cumulative attendance pct for each week that someone has survived (n_showed_up / n_weeks)
           cumulative_attendance = cumsum(any_work),
           cml = case_when(survived == 1 ~ cumulative_attendance/person_week)) %>%
    # keep only if we observe you for 3+ more weeks
    filter(person_week <= total_weeks -3 & person_week <= n_weeks_20) %>%
    ungroup()

  df %>%
    mutate(attend = as.numeric(survived == 1 & any_work == 1)) %>%
    group_by(person_week) %>%
    summarise(n = n(),
              survived =  sum(survived) / n,
              cml = mean(cml, na.rm = T)) %>%
    mutate(path = path,
           period = period)

}



# Figures used in the paper - combining the pathway and total plots into 1
plot_retention = function(weekly_retention_df, period, max_break = 80, max_week = 78, combined = T){

  stopifnot(period %in% c("precovid", "all"))

  if(period == "precovid"){ #primary figure

    df_all <- weekly_retention_df %>%
      filter(period_cut == "precovid",
             path_cut == "all") %>%
      mutate(type = "All READI Participants") %>%
      select(-path_cut)

    df_pathway <- weekly_retention_df %>%
      filter(period_cut == "precovid",
             path_cut %in% c("cr", "ul")) %>%  #primary fig excludes re due to small N
      mutate(type = "By Pathway") %>%
      select(-label)


    df_combined <- bind_rows(df_all, df_pathway)

  } else if(period == "all"){ #appendix figure

    df_all <- weekly_retention_df %>%
      filter(period_cut == "all",
             path_cut == "all") %>%
      mutate(type = "All READI Participants") %>%
      select(-path_cut)

    df_pathway <- weekly_retention_df %>%
      filter(period_cut == "all",
             path_cut %in% c("re","cr", "ul")) %>% #all paths
      mutate(type = "By Pathway") %>%
      select(-label)

    df_combined <- bind_rows(df_all, df_pathway)
  }

  df_combined <- df_combined %>%
    mutate(pathway = case_when(path_cut == 'ul' ~ "Algorithm",
                               path_cut == 'cr' ~ "Outreach",
                               path_cut == 're' ~ "Re-entry"))


  label_weeks <- df_combined %>%
    filter(!is.na(period)) %>%
    pull(person_week)

  if(combined == T){
    plot_out <- df_combined %>%
      ggplot(aes(person_week, value, color = path_cut, linetype=stat)) +
      geom_line(size= 1.5) +
      geom_label_repel(aes(label = pathway),
                       size = 6.5,
                       data = df_combined %>% filter(person_week == (max_week - 10)),
                       nudge_x = 1,
                       family = "Times New Roman",
                       na.rm = TRUE) +
      geom_label(data= df_combined %>% filter(person_week %in% c(1, label_weeks), stat == "survived"),
                 aes(x=person_week*0.97, y=.05, label= label),
                 size=6.5,
                 color = "black", family = "Times") +
      scale_color_manual("",
                         breaks=c("ul", "cr", "re"),
                         values=c('#bf212f', '#264b96', '#27b376'),
                         labels=c("Algorithm", "Outreach", "Re-entry")) +
      scale_linetype_manual("",
                            breaks = c("survived", "cml"),
                            values = c("solid", "dotted"),
                            labels = c("Fraction still working", "Cumulative attendance among those still working")) +

      facet_wrap(~type, ncol = 1, scales = "free_x") +
      scale_x_continuous(breaks=seq(0, max_break, 10),
                         limits= c(0, max_week)) +
      scale_y_continuous(limits=c(0,1)) +
      labs(x="Week of employment",
           y="Fraction of eligible participants") +
      theme_classic() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_blank(),
            axis.title= element_text(size=config$axis_label_size),
            axis.text= element_text(size=config$axis_size),
            axis.title.y = element_text(vjust = +4),
            plot.margin = margin(1,1,1,1, "cm"),
            legend.position="bottom",
            legend.box="vertical",
            legend.text = element_text(size=config$axis_size),
            strip.text.x = element_text(size=config$title_size),
            title = element_text(size=config$title_size),
            text = element_text(family = "Times")) +
      guides(colour="none")

    return(plot_out)

  } else{
    plot_all <- df_all %>%
      ggplot(aes(person_week, value, linetype=stat)) +
      geom_line(size= 1.5) +
      geom_label(data= df_all %>% filter(person_week %in% c(1, label_weeks), stat == "survived"),
                 aes(x=person_week, y=.05, label= label),
                 size=6.5,
                 color = "black", family = "Times") +
      scale_color_manual("",
                         breaks=c("ul", "cr", "re"),
                         values=c('#bf212f', '#264b96', '#27b376'),
                         labels=c("Algorithm", "Outreach", "Re-entry")) +
      scale_linetype_manual("",
                            breaks = c("survived", "cml"),
                            values = c("solid", "dotted"),
                            labels = c("Fraction still working", "Cumulative attendance among those still working")) +

      scale_x_continuous(breaks=seq(0, max_break, 10),
                         limits= c(0, max_week)) +
      scale_y_continuous(limits=c(0,1)) +
      labs(x="Week of employment",
           y="Fraction of eligible participants") +
      theme_classic() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_blank(),
            axis.title= element_text(size=config$axis_label_size),
            axis.text= element_text(size=config$axis_size),
            axis.title.y = element_text(vjust = +4),
            plot.margin = margin(1,1,1.5,1, "cm"),
            legend.position="bottom",
            legend.box="vertical",
            legend.text = element_text(size=config$axis_size),
            strip.text.x = element_text(size = config$title_size),
            title = element_text(size=config$title_size),
            text = element_text(family = "Times"))


    plot_path <- df_pathway %>%
      ggplot(aes(person_week, value, color = path_cut, linetype=stat)) +
      geom_line(size= 1.5) +
      scale_color_manual("",
                         breaks=c("ul", "cr", "re"),
                         values=c('#bf212f', '#264b96', '#27b376'),
                         labels=c("Algorithm", "Outreach", "Re-entry")) +
      scale_linetype_manual("",
                            breaks = c("survived", "cml"),
                            values = c("solid", "dotted"),
                            labels = c("Fraction still working", "Cumulative attendance among those still working")) +
      scale_x_continuous(breaks=seq(0, max_break, 10),
                         limits= c(0, max_week)) +
      scale_y_continuous(limits=c(0,1)) +
      labs(x="Week of employment",
           y="Fraction of eligible participants") +
      theme_classic() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            strip.background = element_blank(),
            axis.title= element_text(size=config$axis_label_size),
            axis.text= element_text(size=config$axis_size),
            axis.title.y = element_text(vjust = +4),
            plot.margin = margin(1,1,1,1, "cm"),
            legend.position="bottom",
            legend.box="vertical",
            legend.text = element_text(size=config$axis_size),
            strip.text.x = element_text(size = config$title_size),
            title = element_text(size = config$title_size),
            text = element_text(family = "Times"))

    return(list(plot_all, plot_path))
  }



}

