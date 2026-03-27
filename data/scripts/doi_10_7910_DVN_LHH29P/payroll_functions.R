get_payroll_20 = function(payroll_df, roster_df, days_per_month){
  payroll_df %>%
    inner_join(roster_df %>% select(UL_id_unique,pathway,ran_date,treatment,t_headcount,neighborhood)) %>%
    mutate(ran_date = ymd(ran_date),
           end_date = ran_date + days(20 * days_per_month)) %>%
    filter(pay_date <= end_date, pay_date >= ran_date)
}

# Gets the average total earnings and hours worked across each path and earnings type
#' @param type the earnings/hours "type" to use in the payroll data
payroll_avgs <- function(path, payroll_df, type = "regular", covid_cut, standby_cut) {

  if (path == "all") {
    df <- payroll_df
  } else {
    df <- payroll_df %>%
      filter(pathway == path)
  }

  if (type == "regular") {
    hours_col <- "t_payroll_work_hours"
    earnings_col <- "t_payroll_work_earnings"
  } else if (type == "total") {
    hours_col <- "t_payroll_total_hours"
    earnings_col <- "t_payroll_total_earnings"
  } else if (type == "training") {
    hours_col <- "t_payroll_training_hours"
    earnings_col <- "t_payroll_training_earnings"
  } else {error("invalid type")}

  #total period N (this is our denominator regardless of type)
  n_total <- df %>% filter(t_payroll_total_hours > 0) %>% group_by(UL_id_unique) %>% n_groups
  n_personweeks_total <- df %>% filter(t_payroll_total_hours > 0) %>% group_by(UL_id_unique,pay_week) %>% n_groups

  total_earnings <- df %>% pull(earnings_col) %>% sum(na.rm = T)
  total_hours <- df %>% pull(hours_col) %>% sum(na.rm = T)

  avg_earnings_total <- total_earnings/n_total
  avg_hours_total <- total_hours/n_total

  #pre covid
  df_pre <- df %>% filter(pay_date < covid_cut)
  total_earnings_pre <- df_pre %>% pull(earnings_col) %>% sum(na.rm = T)
  total_hours_pre <- df_pre %>% pull(hours_col) %>% sum(na.rm = T)
  avg_earnings_pre <- total_earnings_pre/n_total
  avg_hours_pre <- total_hours_pre/n_total

  #standby period
  df_standby <- df %>% filter(pay_date >= covid_cut, pay_date < standby_cut)
  total_earnings_standby <- df_standby %>% pull(earnings_col) %>% sum(na.rm = T)
  total_hours_standby <- df_standby %>% pull(hours_col) %>% sum(na.rm = T)
  avg_earnings_standby <- total_earnings_standby/n_total
  avg_hours_standby <- total_hours_standby/n_total

  #post covid
  df_post <- df %>% filter(pay_date >= standby_cut)
  total_earnings_post <- df_post %>% pull(earnings_col) %>% sum(na.rm = T)
  total_hours_post <- df_post %>% pull(hours_col) %>% sum(na.rm = T)
  avg_earnings_post <- total_earnings_post/n_total
  avg_hours_post <- total_hours_post/n_total

  tibble(path, type, n_total,
         avg_earnings_total,   avg_hours_total,
         avg_earnings_pre,     avg_hours_pre,
         avg_earnings_standby, avg_hours_standby,
         avg_earnings_post,    avg_hours_post)

}


calc_payroll_percentiles = function(payroll_df, roster_df, var, path, period, ntiles = c(.25, .5, .75)){

  payroll_takers <- payroll_df %>%
    group_by(UL_id_unique) %>%
    filter(sum(t_payroll_total_hours, na.rm = T) > 0)

  if (period == "precovid") {
    payroll_takers <- payroll_takers %>%
      filter(pay_date <= ymd(covid_cut))

  } else if (period == "standby") {
    payroll_takers <- payroll_takers %>%
      filter(pay_date > covid_cut, pay_date < standby_cut)

  } else if (period == "postcovid") {
    payroll_takers <- payroll_takers %>%
      filter(pay_date >= standby_cut)
  } else if (period != "all") {
    stop("Invalid period")
  }

  payroll_takers <- payroll_takers %>%
    summarise(across(c(!!sym(var)), ~sum(.x, na.rm = T))) %>%
    left_join(roster_df %>% select(UL_id_unique, contains("g_pathway")), "UL_id_unique") %>%
    mutate(g_pathway_all = 1) %>%
    filter(!!sym(glue("g_pathway_{path}")) == 1)

  pctiles <- map_dfc(ntiles, function(ntile) {
    var_name <- paste0("qnt_", ntile*100)
    payroll_takers %>%
      summarise(!!sym(var_name) :=  quantile(!!sym(var), probs= ntile))
  })

  payroll_takers %>%
    summarise(mean = mean(!!sym(var))) %>%
    mutate(type = var) %>%
    cbind(pctiles) %>%
    pivot_longer(-c(type), names_to = "measure", values_to =  "n") %>%
    mutate(pathway = path, period = period)

}

donut_plot <- function(value, title, font, color, shade) {
  
  df <- tibble(x = 1, y = round(value, 2)) %>% 
    mutate(y_negative = 1 - y) %>% 
    pivot_longer(cols = -x) 
  
  number <- percent(value)
  
  donut <- ggplot(df,
                  aes(x = x,
                      y = value,
                      fill = name)) +
    geom_col(show.legend = FALSE) +
    coord_polar(theta = "y", direction = -1) +
    xlim(c(-2, 3.5)) +
    scale_fill_manual(values = c(color, shade)) +
    theme_void() +
    annotate("text", label = paste0(as.character(round(value, 2)*100),"%"),
             family = font, fontface = "bold", color = color, size = 17, x = -2, y = 0) + 
    labs(caption=title) + 
    theme(plot.caption = element_text(hjust=0.5, vjust=20, size=16, face="bold", family="Times"),
          plot.margin = margin(t = -50, r = -70, b = -50, l = -70))
  return(donut)
}


gen_donut_series <- function(df, color_scheme) {
  
  donuts <- vector('list', nrow(df))
  
  for(i in 1:nrow(df)) {
  donuts[[i]] <- donut_plot(df[[i,"takeup_rate"]], df[[i,"pathway"]], "Times", color_scheme[i], "#eceeef")
  }
  
  return(donuts)
  }