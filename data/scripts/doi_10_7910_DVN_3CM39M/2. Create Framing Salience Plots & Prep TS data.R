# Load packages
if (!require("pacman")) install.packages("pacman")
p_load(vars, tseries, tsm, TSstudio, forecast, mFilter, tidyverse, readxl, writexl, cowplot, quanteda,
       janitor, readtext, irr, bit64, igraph, ggraph, ggplot2, tscount,
       beepr, pbapply, rtweet, lmtest, vrtest, pracma, lubridate, quanteda.textplots, tscount, bruceR)

# disable scientific notation of numbers
options(scipen=999)

# In this script I create the frames and salience plots and the inside lobbying plot. I also create the time series datasets from the
# Frame data.

######################### 1. Build all the functions ############################
# create a function that creates a plot displaying the relative frequency of frames before/after specific events
plot_frames = function(data, title, labels, phase1, phase2, phase3, phase4 = NULL, legend = "none"){
  
  data$datetime = as.Date(data$datetime)
  
  #create a variable containing info on the phase
  data$phase = ifelse(data$datetime < as.Date(phase1), 0, 1)
  data$phase = ifelse(data$datetime >= as.Date(phase2), 2, data$phase)
  data$phase = ifelse(data$datetime >= as.Date(phase3), 3, data$phase)
  if(!is.null(phase4)){
    data$phase = ifelse(data$datetime >= as.Date(phase4), 4, data$phase)
    values = c(15,6,17,18,13)
  } else{
    values = c(15,6,17,18)
  }
  
  # group frames into aggregate categories
  data$ag_censor_filter = ifelse(data$censor > 0 | data$overly_broad > 0 | data$uploadfilter > 0, 1, 0)
  data$ag_break_internet = data$break_internet
  data$ag_fair_creators = ifelse(data$fair_pay > 0 | data$culture_creator > 0, 1, 0)
  data$ag_criminal_safety = ifelse(data$criminal > 0 | data$public_safety > 0, 1, 0)
  data$ag_astro = data$astroturf
  data$ag_lobby = data$lobby
  data$ag_platform_power = ifelse(data$bad_platforms > 0 | data$market_power > 0, 1, 0)
  data$ag_econ_inno = ifelse(data$economy > 0 | data$inno > 0, 1, 0)
  data$ag_user_rights = ifelse(data$consumers_users > 0 | data$privacy > 0, 1, 0)
  data$ag_misinfo = data$misinfo
  data$ag_diversity = data$diversity_freepress
  data$ag_reach = data$reach
  
  #aggregate by article
  data.grouped = data %>%
    group_by(article_id) %>% 
    summarise(
      ag_censor_filter = sum(ag_censor_filter),
      ag_break_internet = sum(ag_break_internet),
      ag_fair_creators = sum(ag_fair_creators),
      ag_criminal_safety = sum(ag_criminal_safety),
      ag_astro = sum(ag_astro),
      ag_lobby = sum(ag_lobby),
      ag_platform_power = sum(ag_platform_power),
      ag_econ_inno = sum(ag_econ_inno),
      ag_user_rights = sum(ag_user_rights),
      ag_misinfo = sum(ag_misinfo),
      ag_diversity = sum(ag_diversity),
      ag_reach = sum(ag_reach),
      no_paragraphs = length(paragraph_id),
      phase = mean(phase)
    )
  
  #divide paragraphs that use a frame by total number of paragraphs in article
  data.grouped$ag_censor_filter = (data.grouped$ag_censor_filter/data.grouped$no_paragraphs)
  data.grouped$ag_break_internet = (data.grouped$ag_break_internet/data.grouped$no_paragraphs)
  data.grouped$ag_fair_creators = (data.grouped$ag_fair_creators/data.grouped$no_paragraphs)
  data.grouped$ag_criminal_safety = (data.grouped$ag_criminal_safety/data.grouped$no_paragraphs)
  data.grouped$ag_astro = (data.grouped$ag_astro/data.grouped$no_paragraphs)
  data.grouped$ag_lobby = (data.grouped$ag_lobby/data.grouped$no_paragraphs)
  data.grouped$ag_platform_power = (data.grouped$ag_platform_power/data.grouped$no_paragraphs)
  data.grouped$ag_econ_inno = (data.grouped$ag_econ_inno/data.grouped$no_paragraphs)
  data.grouped$ag_user_rights = (data.grouped$ag_user_rights/data.grouped$no_paragraphs)
  data.grouped$ag_misinfo = (data.grouped$ag_misinfo/data.grouped$no_paragraphs)
  data.grouped$ag_diversity = (data.grouped$ag_diversity/data.grouped$no_paragraphs)
  data.grouped$ag_reach = (data.grouped$ag_reach/data.grouped$no_paragraphs)
  
  #aggregate by phase
  data.grouped = data.grouped %>%
    group_by(phase) %>% 
    summarise(
      ag_censor_filter = mean(ag_censor_filter),
      ag_break_internet = mean(ag_break_internet),
      ag_fair_creators = mean(ag_fair_creators),
      ag_criminal_safety = mean(ag_criminal_safety),
      ag_astro = mean(ag_astro),
      ag_lobby = mean(ag_lobby),
      ag_platform_power = mean(ag_platform_power),
      ag_econ_inno = mean(ag_econ_inno),
      ag_user_rights = mean(ag_user_rights),
      ag_misinfo = mean(ag_misinfo),
      ag_diversity = mean(ag_diversity),
      ag_reach = mean(ag_reach),
      no_paragraphs = mean(no_paragraphs),
      no_articles = length(article_id))
  
  #drop variables that wont be plotted
  data.grouped = select(data.grouped, c(-no_paragraphs, -no_articles))
  
  #convert from wide to long
  data.plot = data.grouped %>%
    pivot_longer(-phase, names_to = "frame", values_to = "freq_par")
  
  #plot
  plot_frames = ggplot(data.plot, aes(x = reorder(frame, freq_par), ymin = 0, ymax = freq_par, shape = as.factor(phase))) +
    geom_linerange(position = position_dodge(width=0.66), size =1, color = "black") +
    geom_point(aes(y=freq_par, shape = as.factor(phase)), color = "black", position = position_dodge(width=0.66), size=3) +
    labs(title = title,
         y = "Relative Frequency",
         x = NULL,
         shape = "Phase") +
    scale_shape_manual(values = values, labels = labels) +
    scale_x_discrete(labels = c(ag_censor_filter = "Censorship & Uploadfilter",
                                ag_break_internet = "Breaking the Internet",
                                ag_fair_creators = "Fair Pay & Creators",
                                ag_criminal_safety = "Criminality & Public Safety",
                                ag_astro = "Astroturfing",
                                ag_lobby = "Lobbying",
                                ag_platform_power = "Bad Platforms & Market Power",
                                ag_econ_inno = "Economy & Innovation",
                                ag_user_rights = "User Rights & Cybersecurity",
                                ag_misinfo = "Misinformation",
                                ag_diversity = "Diversity & Free Press",
                                ag_reach = "Reach"
    )) + 
    coord_flip() + 
    theme_classic() +
    theme(legend.position = legend)
  
  return(plot_frames)
}

# Create a function that creates a plot of the salience of the laws on a weekly basis
plot_salience = function(data, start_date, no_weeks, vline1, vline2, vline3, event_names, event_pos_x, event_pos_y, xbreaks, max_y, seq_y, ylab,  title){
  
  data$datetime = as.Date(data$datetime, format = "%Y-%m-%d")
  
  data$date_week = format(data$datetime, "%Y-%V")
  
  data = data %>% 
    group_by(date_week) %>%
    summarise(no_articles = length(unique(article_id)))
  
  # fill missing weeks
  date_week = seq(as.Date(start_date), by = "week", length.out = no_weeks)
  date_week = format(date_week, "%Y-%V")
  
  no_articles = seq(0, by = 0, length.out = no_weeks)
  
  empty_data = cbind(as.data.frame(date_week), as.data.frame(no_articles))
  
  data = rbind(data, empty_data)
  
  data = data %>% 
    group_by(date_week) %>%
    summarise(no_articles = sum(no_articles))
  
  plot_sal = ggplot(data, aes(x = date_week, y = no_articles, group = 1)) + 
    geom_vline(xintercept = vline1, color = "grey") +
    geom_vline(xintercept = vline2, color = "grey") +
    geom_vline(xintercept = vline3, color = "grey") +
    geom_line() + 
    scale_x_discrete(breaks = xbreaks) +
    scale_y_continuous(breaks = seq(0, max_y, by = seq_y), limits = c(0, max_y)) + 
    annotate("text", 
             x = event_pos_x, 
             y = event_pos_y,
             label = event_names,
             color = "grey",
             size = 3.5,
             angle = 90,
             fontface = "bold") + 
    labs(x = "Week",
         y = ylab,
         title = title) +
    theme_classic()
  
  plot_sal
  
}

######################## 2. Plot the frames and salience ##########################
############# a) Newspaper ############# 
###### i) SOPA ######
# load frames data
sopa_news_dict = read.csv("SOPA/SOPA_FRAMES_Newspapers.csv")

sopa_news_dict$datetime = sopa_news_dict$datetime.x

# Create a plot displaying the relative frequency of frames
sopa_news_frames_plot = plot_frames(data = sopa_news_dict, title = "Frames in US Newspaper Articles on Online Copyright",
                                    phase1 = "2011-11-14", phase2 = "2012-01-16", phase3 = "2012-02-01",
                                    labels = c("Pre-ACD", "Post-ACD", "Post-IB", "Feb-Dec 12"),
                                    legend = "right")

# create a plot displaying the salience over time 
sopa_news_salience_plot = plot_salience(data = sopa_news_dict, start_date = "2010-01-01", no_weeks = 156,
                                        vline1 = "2011-45", vline2 = "2012-02", vline3 = NULL,
                                        event_names = c("American Censorship Day", "Internet Blackout"),
                                        event_pos_x = c("2011-42", "2011-52"), event_pos_y = c(40, 40),
                                        xbreaks = c("2010-01", "2010-26","2011-01", "2011-26","2012-01", "2012-26", "2013-01"),
                                        max_y = 50, seq_y = 10,
                                        ylab = "Number of Articles",  title = "Salience of Online Copyright in US Newspapers")

# merge frames and salience plots
sopa_news_plot = plot_grid(sopa_news_salience_plot, sopa_news_frames_plot, rel_widths = c(.8,1))

#save plot
ggsave("SOPA/SOPA_Frames in Newspaper Articles.png", sopa_news_plot, width = 12, height = 6, dpi = 640)

###### ii) EUCD ######
eu_news_dict = read.csv("EUCD/EUCD_FRAMES_Newspapers.csv")

# create subsets of data for each language
en_news_dict = subset(eu_news_dict, language == "Englisch")
de_news_dict = subset(eu_news_dict, language == "Deutsch")
fr_news_dict = subset(eu_news_dict, language == "Französisch")
es_news_dict = subset(eu_news_dict, language == "Spanisch")
it_news_dict = subset(eu_news_dict, language == "Italienisch")
pl_news_dict = subset(eu_news_dict, language == "Polnisch")
pt_news_dict = subset(eu_news_dict, language == "Portugiesisch")
nl_news_dict = subset(eu_news_dict, language == "Niederländisch")

# Create a plot displaying the relative frequency of frames in newspaper articles
en_news_frames_plot = plot_frames(data = en_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

de_news_frames_plot = plot_frames(data = de_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

fr_news_frames_plot = plot_frames(data = fr_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

es_news_frames_plot = plot_frames(data = es_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

it_news_frames_plot = plot_frames(data = it_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

pl_news_frames_plot = plot_frames(data = pl_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

pt_news_frames_plot = plot_frames(data = pt_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

nl_news_frames_plot = plot_frames(data = nl_news_dict, title = "",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

eu_news_frames_plot = plot_frames(data = eu_news_dict, title = "Frames in EU Newspaper Articles on Online Copyright",
                                  phase1 = "2018-07-03", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                  labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                  legend = "right")

# create a plot displaying the salience over time 
en_news_salience_plot = plot_salience(data = en_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "English")

de_news_salience_plot = plot_salience(data = de_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "German")

fr_news_salience_plot = plot_salience(data = fr_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "French")

es_news_salience_plot = plot_salience(data = es_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "Spanish")

it_news_salience_plot = plot_salience(data = it_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "Italian")

pl_news_salience_plot = plot_salience(data = pl_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "Polish")

pt_news_salience_plot = plot_salience(data = pt_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "Portuguese")

nl_news_salience_plot = plot_salience(data = nl_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "Dutch")

eu_news_salience_plot = plot_salience(data = eu_news_dict, start_date = "2016-01-01", no_weeks = 210,
                                      vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                      event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                      event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(100, 100, 100),
                                      xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                      max_y = 125, seq_y = 25,
                                      ylab = "Number of Articles",  title = "Salience of Online Copyright in All EU Newspapers")

# merge frames and salience plots
en_news_plot = plot_grid(en_news_salience_plot, en_news_frames_plot, rel_widths = c(.7,1))
de_news_plot = plot_grid(de_news_salience_plot, de_news_frames_plot, rel_widths = c(.7,1))
fr_news_plot = plot_grid(fr_news_salience_plot, fr_news_frames_plot, rel_widths = c(.7,1))
es_news_plot = plot_grid(es_news_salience_plot, es_news_frames_plot, rel_widths = c(.7,1))
it_news_plot = plot_grid(it_news_salience_plot, it_news_frames_plot, rel_widths = c(.7,1))
pl_news_plot = plot_grid(pl_news_salience_plot, pl_news_frames_plot, rel_widths = c(.7,1))
pt_news_plot = plot_grid(pt_news_salience_plot, pt_news_frames_plot, rel_widths = c(.7,1))
nl_news_plot = plot_grid(nl_news_salience_plot, nl_news_frames_plot, rel_widths = c(.7,1))
eu_news_plot = plot_grid(eu_news_salience_plot, eu_news_frames_plot, rel_widths = c(.7,1))

# save plots
ggsave("EUCD/Plots/EN_Frames in Newspaper Articles.png", en_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/DE_Frames in Newspaper Articles.png", de_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/FR_Frames in Newspaper Articles.png", fr_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ES_Frames in Newspaper Articles.png", es_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/IT_Frames in Newspaper Articles.png", it_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PL_Frames in Newspaper Articles.png", pl_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PT_Frames in Newspaper Articles.png", pt_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/NL_Frames in Newspaper Articles.png", nl_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ALL_Frames in Newspaper Articles.png", eu_news_plot, width = 15, height = 7.5, dpi = 640)

ggsave("EUCD/Plots/EN_Frames in Newspaper Articles.pdf", en_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/DE_Frames in Newspaper Articles.pdf", de_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/FR_Frames in Newspaper Articles.pdf", fr_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ES_Frames in Newspaper Articles.pdf", es_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/IT_Frames in Newspaper Articles.pdf", it_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PL_Frames in Newspaper Articles.pdf", pl_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PT_Frames in Newspaper Articles.pdf", pt_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/NL_Frames in Newspaper Articles.pdf", nl_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ALL_Frames in Newspaper Articles.pdf", eu_news_plot, width = 15, height = 7.5, dpi = 640)

combo_news_plot = plot_grid(de_news_plot, pl_news_plot, fr_news_plot, en_news_plot, it_news_plot,
                            pt_news_plot, nl_news_plot, es_news_plot, 
                            ncol = 2)

combo_news_plot = plot_grid(eu_news_plot, combo_news_plot, nrow = 2, rel_heights = c(1,3))

#save plot
ggsave("EUCD/Plots/Combo_Frames in Newspaper Articles.png", combo_news_plot, width = 20, height = 30, dpi = 640)
ggsave("EUCD/Plots/Combo_Frames in Newspaper Articles.pdf", combo_news_plot, width = 20, height = 30, dpi = 640)

# only all, french, and german
red_tweets_plot = plot_grid(eu_news_plot, de_news_plot, fr_news_plot, nrow = 3)
ggsave("EUCD/Plots/Red_Frames in Newspaper Articles.pdf", red_tweets_plot, width = 15, height = 22.5, dpi = 640)
ggsave("EUCD/Plots/Red_Frames in Newspaper Articles.png", red_tweets_plot, width = 15, height = 22.5, dpi = 640)

############# b) All Tweets #############
##### i) SOPA #####
# load data
sopa_tweets_dict = read.csv("SOPA/SOPA_FRAMES_Tweets_Tweetids.csv")

# Draw the plots
sopa_tweets_dict$article_id = sopa_tweets_dict$X
sopa_tweets_dict$paragraph_id = sopa_tweets_dict$X
sopa_tweets_dict$datetime = as.Date(sopa_tweets_dict$date)

# Create a plot displaying the relative frequency of frames
sopa_tweets_frames_plot = plot_frames(data = sopa_tweets_dict, title = "Frames in Tweets on SOPA, PIPA, COICA",
                                      phase1 = "2011-11-14", phase2 = "2012-01-16", phase3 = "2012-02-01",
                                      labels = c("Pre-ACD", "Post-ACD", "Post-IB", "Feb-Dec 12"),
                                      legend = "right")

# create a plot displaying the salience over time 
sopa_tweets_salience_plot = plot_salience(data = sopa_tweets_dict, start_date = "2010-01-01", no_weeks = 156,
                                          vline1 = "2011-45", vline2 = "2012-02", vline3 = NULL,
                                          event_names = c("American Censorship Day", "Internet Blackout"),
                                          event_pos_x = c("2011-42", "2011-52"), event_pos_y = c(750000, 750000),
                                          xbreaks = c("2010-01", "2010-26","2011-01", "2011-26","2012-01", "2012-26", "2013-01"),
                                          max_y = 900000, seq_y = 100000,
                                          ylab = "Number of Tweets",  title = "Salience of SOPA, PIPA, COICA in Tweets")

# merge frames and salience plots
sopa_tweets_plot = plot_grid(sopa_tweets_salience_plot, sopa_tweets_frames_plot, rel_widths = c(.8,1))

#save plot
ggsave("SOPA/SOPA_Frames in Tweets.png", sopa_tweets_plot, width = 12, height = 6, dpi = 640)

##### ii) EUCD #####
# load data
eucd_tweets_dict = read.csv("EUCD/EUCD_FRAMES_Twitter_Tweetids.csv")

# create some variables for plotting
eucd_tweets_dict$article_id = eucd_tweets_dict$X
eucd_tweets_dict$paragraph_id = eucd_tweets_dict$X
eucd_tweets_dict$datetime = as.Date(eucd_tweets_dict$date)

# drop data before 2016
eucd_tweets_dict = subset(eucd_tweets_dict, datetime >= "2016-01-01")

# create subsets of data by language
en_tweets_dict = subset(eucd_tweets_dict, language == "en")
de_tweets_dict = subset(eucd_tweets_dict, language == "de")
fr_tweets_dict = subset(eucd_tweets_dict, language == "fr")
nl_tweets_dict = subset(eucd_tweets_dict, language == "nl")
pt_tweets_dict = subset(eucd_tweets_dict, language == "pt")
pl_tweets_dict = subset(eucd_tweets_dict, language == "pl")
es_tweets_dict = subset(eucd_tweets_dict, language == "es")
it_tweets_dict = subset(eucd_tweets_dict, language == "it")

# Create a plot displaying the relative frequency of frames in tweets
en_tweets_frames_plot = plot_frames(data = en_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

de_tweets_frames_plot = plot_frames(data = de_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

fr_tweets_frames_plot = plot_frames(data = fr_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

es_tweets_frames_plot = plot_frames(data = es_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

it_tweets_frames_plot = plot_frames(data = it_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

pl_tweets_frames_plot = plot_frames(data = pl_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

pt_tweets_frames_plot = plot_frames(data = pt_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

nl_tweets_frames_plot = plot_frames(data = nl_tweets_dict, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

eu_tweets_frames_plot = plot_frames(data = eucd_tweets_dict, title = "Frames in Tweets on EU Copyright Directive",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

# create a plot displaying the salience over time 
en_tweets_salience_plot = plot_salience(data = en_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "English")

de_tweets_salience_plot = plot_salience(data = de_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "German")

fr_tweets_salience_plot = plot_salience(data = fr_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "French")

es_tweets_salience_plot = plot_salience(data = es_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "Spanish")

it_tweets_salience_plot = plot_salience(data = it_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "Italian")

pl_tweets_salience_plot = plot_salience(data = pl_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "Polish")

pt_tweets_salience_plot = plot_salience(data = pt_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "Portuguese")

nl_tweets_salience_plot = plot_salience(data = nl_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "Dutch")

eu_tweets_salience_plot = plot_salience(data = eucd_tweets_dict, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600000, 600000, 600000),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 700000, seq_y = 50000,
                                        ylab = "Number of Articles",  title = "Salience of EU Copyright Directive on Twitter")

# merge frames and salience plots
en_tweets_plot = plot_grid(en_tweets_salience_plot, en_tweets_frames_plot, rel_widths = c(.7,1))
de_tweets_plot = plot_grid(de_tweets_salience_plot, de_tweets_frames_plot, rel_widths = c(.7,1))
fr_tweets_plot = plot_grid(fr_tweets_salience_plot, fr_tweets_frames_plot, rel_widths = c(.7,1))
es_tweets_plot = plot_grid(es_tweets_salience_plot, es_tweets_frames_plot, rel_widths = c(.7,1))
it_tweets_plot = plot_grid(it_tweets_salience_plot, it_tweets_frames_plot, rel_widths = c(.7,1))
pl_tweets_plot = plot_grid(pl_tweets_salience_plot, pl_tweets_frames_plot, rel_widths = c(.7,1))
pt_tweets_plot = plot_grid(pt_tweets_salience_plot, pt_tweets_frames_plot, rel_widths = c(.7,1))
nl_tweets_plot = plot_grid(nl_tweets_salience_plot, nl_tweets_frames_plot, rel_widths = c(.7,1))
eu_tweets_plot = plot_grid(eu_tweets_salience_plot, eu_tweets_frames_plot, rel_widths = c(.7,1))

# save plots
ggsave("EUCD/Plots/EN_Frames in Tweets.png", en_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/DE_Frames in Tweets.png", de_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/FR_Frames in Tweets.png", fr_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ES_Frames in Tweets.png", es_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/IT_Frames in Tweets.png", it_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PL_Frames in Tweets.png", pl_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PT_Frames in Tweets.png", pt_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/NL_Frames in Tweets.png", nl_tweets_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ALL_Frames in Tweets.png", eu_tweets_plot, width = 15, height = 7.5, dpi = 640)

ggsave("EUCD/Plots/EN_Frames in Tweets.pdf", en_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/DE_Frames in Tweets.pdf", de_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/FR_Frames in Tweets.pdf", fr_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ES_Frames in Tweets.pdf", es_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/IT_Frames in Tweets.pdf", it_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PL_Frames in Tweets.pdf", pl_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/PT_Frames in Tweets.pdf", pt_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/NL_Frames in Tweets.pdf", nl_news_plot, width = 15, height = 7.5, dpi = 640)
ggsave("EUCD/Plots/ALL_Frames in Tweets.pdf", eu_news_plot, width = 15, height = 7.5, dpi = 640)

combo_tweets_plot = plot_grid(de_tweets_plot, pl_tweets_plot, fr_tweets_plot, en_tweets_plot, it_tweets_plot,
                              pt_tweets_plot, nl_tweets_plot, es_tweets_plot, 
                              ncol = 2)

combo_tweets_plot = plot_grid(eu_tweets_plot, combo_tweets_plot, nrow = 2, rel_heights = c(1,3))

#save plot
ggsave("EUCD/Plots/Combo_Frames in tweets.png", combo_tweets_plot, width = 20, height = 30, dpi = 640)

# only all, french, and german
red_tweets_plot = plot_grid(eu_tweets_plot, de_tweets_plot, fr_tweets_plot, nrow = 3)
ggsave("EUCD/Plots/Red_Frames in tweets.pdf", red_tweets_plot, width = 15, height = 22.5, dpi = 640)
ggsave("EUCD/Plots/Red_Frames in tweets.png", red_tweets_plot, width = 15, height = 22.5, dpi = 640)

############# c) Parliamentarians #############
###### i) MEPs ######
# load data
eucd_tweets_meps = read.csv("EUCD/EUCD_FRAMES_MEPs_Tweetids.csv")
eucd_tweets_meps = subset(eucd_tweets_meps, tweetid != "")

eucd_tweets_meps$article_id = eucd_tweets_meps$X
eucd_tweets_meps$paragraph_id = eucd_tweets_meps$X

# descriptive statistics
length(unique(eucd_tweets_meps$name)) # 261 of 860 meps tweeted about the directive
max(table(eucd_tweets_meps$name)) # Felix Reda (@Senficon) wrote 2634 tweets on the directive
median(table(eucd_tweets_meps$name)) # the median MEP who tweeted on the directive wrote 3 tweets
mean(table(eucd_tweets_meps$name)) # of those MEPs who tweeted each wrote 27 tweets on average
nrow(eucd_tweets_meps)/860 # of all MEPs each wrote 8 tweets on average
table(eucd_tweets_meps$language)
table(eucd_tweets_meps$country)

# create subsets of data by language
en_tweets_meps = subset(eucd_tweets_meps, language == "en")
de_tweets_meps = subset(eucd_tweets_meps, language == "de")
fr_tweets_meps = subset(eucd_tweets_meps, language == "fr")
nl_tweets_meps = subset(eucd_tweets_meps, language == "nl")
pt_tweets_meps = subset(eucd_tweets_meps, language == "pt")
pl_tweets_meps = subset(eucd_tweets_meps, language == "pl")
es_tweets_meps = subset(eucd_tweets_meps, language == "es")
it_tweets_meps = subset(eucd_tweets_meps, language == "it")

# Create a plot displaying the relative frequency of frames in newspaper articles
en_tweets_frames_plot = plot_frames(data = en_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

de_tweets_frames_plot = plot_frames(data = de_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

fr_tweets_frames_plot = plot_frames(data = fr_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

es_tweets_frames_plot = plot_frames(data = es_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

it_tweets_frames_plot = plot_frames(data = it_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

pl_tweets_frames_plot = plot_frames(data = pl_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

pt_tweets_frames_plot = plot_frames(data = pt_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

nl_tweets_frames_plot = plot_frames(data = nl_tweets_meps, title = "",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"))

eu_tweets_frames_plot = plot_frames(data = eucd_tweets_meps, title = "Frames in MEPs Tweets on EU Copyright Directive",
                                    phase1 = "2018-07-04", phase2 ="2018-10-22", phase3 ="2019-03-21", phase4 ="2019-04-30",
                                    labels = c("Pre-Wiki Blackout", "Post-Wiki Blackout", "Post-YT action", "Post-Blackout", "After April"),
                                    legend = "right")

# create a plot displaying the salience over time 
en_tweets_salience_plot = plot_salience(data = en_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "English")

de_tweets_salience_plot = plot_salience(data = de_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "German")

fr_tweets_salience_plot = plot_salience(data = fr_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "French")

es_tweets_salience_plot = plot_salience(data = es_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "Spanish")

it_tweets_salience_plot = plot_salience(data = it_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "Italian")

pl_tweets_salience_plot = plot_salience(data = pl_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "Polish")

pt_tweets_salience_plot = plot_salience(data = pt_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "Portuguese")

nl_tweets_salience_plot = plot_salience(data = nl_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "Dutch")

eu_tweets_salience_plot = plot_salience(data = eucd_tweets_meps, start_date = "2016-01-01", no_weeks = 210,
                                        vline1 = "2018-27", vline2 = "2018-43", vline3 = "2019-12",
                                        event_names = c("Wiki Blackouts\nReddit Mobilization", "YT #saveyourinternet", "Multiple Blackouts"),
                                        event_pos_x = c("2018-20", "2018-40", "2019-09"), event_pos_y = c(600, 600, 600),
                                        xbreaks = c("2016-01", "2016-26","2017-01", "2017-26","2018-01", "2018-26", "2019-01", "2019-26"),
                                        max_y = 750, seq_y = 250,
                                        ylab = "Number of Articles",  title = "Frequency of MEP Tweets on EU Copyright Directive")

# merge frames and salience plots
en_tweets_plot = plot_grid(en_tweets_salience_plot, en_tweets_frames_plot, rel_widths = c(.9,1))
de_tweets_plot = plot_grid(de_tweets_salience_plot, de_tweets_frames_plot, rel_widths = c(.9,1))
fr_tweets_plot = plot_grid(fr_tweets_salience_plot, fr_tweets_frames_plot, rel_widths = c(.9,1))
es_tweets_plot = plot_grid(es_tweets_salience_plot, es_tweets_frames_plot, rel_widths = c(.9,1))
it_tweets_plot = plot_grid(it_tweets_salience_plot, it_tweets_frames_plot, rel_widths = c(.9,1))
pl_tweets_plot = plot_grid(pl_tweets_salience_plot, pl_tweets_frames_plot, rel_widths = c(.9,1))
pt_tweets_plot = plot_grid(pt_tweets_salience_plot, pt_tweets_frames_plot, rel_widths = c(.9,1))
nl_tweets_plot = plot_grid(nl_tweets_salience_plot, nl_tweets_frames_plot, rel_widths = c(.9,1))
eu_tweets_plot = plot_grid(eu_tweets_salience_plot, eu_tweets_frames_plot, rel_widths = c(.7,1))

combo_tweets_plot = plot_grid(de_tweets_plot, pl_tweets_plot, fr_tweets_plot, en_tweets_plot, it_tweets_plot,
                              pt_tweets_plot, nl_tweets_plot, es_tweets_plot, 
                              ncol = 2)

combo_tweets_plot = plot_grid(eu_tweets_plot, combo_tweets_plot, nrow = 2, rel_heights = c(1,3))

#save plot
ggsave("EUCD/Plots/Combo_Frames in MEPs tweets.png", combo_tweets_plot, width = 20, height = 30, dpi = 640)
ggsave("EUCD/Plots/Combo_Frames in MEPs tweets.pdf", combo_tweets_plot, width = 20, height = 30, dpi = 640)

###### ii) US Congresspeople ######
## Twitter
congress_twitter = read.csv("SOPA/SOPA_FRAMES_Congress_Tweetids.csv")

congress_twitter$article_id = congress_twitter$X
congress_twitter$paragraph_id = congress_twitter$X

# Create a plot displaying the relative frequency of frames
sopa_reps_frames_plot = plot_frames(data = congress_twitter, title = "Frames in US Congresspeoples Tweets",
                                    phase1 = "2011-11-14", phase2 = "2012-01-16", phase3 = "2012-02-01",
                                    labels = c("Pre-ACD", "Post-ACD", "Post-IB", "Feb-Dec 12"),
                                    legend = "right")

# create a plot displaying the salience over time 
sopa_reps_salience_plot = plot_salience(data = congress_twitter, start_date = "2010-01-01", no_weeks = 156,
                                        vline1 = "2011-45", vline2 = "2012-02", vline3 = NULL,
                                        event_names = c("American Censorship Day", "Internet Blackout"),
                                        event_pos_x = c("2011-42", "2011-52"), event_pos_y = c(220, 220),
                                        xbreaks = c("2010-01", "2010-26","2011-01", "2011-26","2012-01", "2012-26", "2013-01"),
                                        max_y = 300, seq_y = 50,
                                        ylab = "Number of Tweets",  title = "Salience in US Congresspeoples Tweets")

# merge frames and salience plots
sopa_reps_plot = plot_grid(sopa_reps_salience_plot, sopa_reps_frames_plot, rel_widths = c(.8,1))

#save plot
ggsave("SOPA/SOPA_Frames in US Congresspeople Tweets.png", sopa_reps_plot, width = 16, height = 8, dpi = 640)

######################### 3. Inside Lobbying Data ############################
##### i) Define Functions
lobby_by_IGtype_inner = function(data, subtype_list, var, eucd = T){
  
  
  if(eucd == T){
    output = rbind(sum(subset(data, Subtype %in% subtype_list & year == 2013)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2014)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2015)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2016)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2017)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2018)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2019)[var], na.rm = T))
  } else {
    output = rbind(sum(subset(data, Subtype %in% subtype_list & year == 2010)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2011)[var], na.rm = T),
                   sum(subset(data, Subtype %in% subtype_list & year == 2012)[var], na.rm = T))
  }
  
  
  output = as.data.frame(output)
  
  colnames(output)[1] = var
  
  return(output)
  
}

eu_lobby_by_IGtype = function(data, subtype_list, IGtype){
  
  lobby_exp_low = lobby_by_IGtype_inner(data, subtype_list, var = "lobby_exp_low")
  lobby_exp_high = lobby_by_IGtype_inner(data, subtype_list, var = "lobby_exp_high")
  ec_meet = lobby_by_IGtype_inner(data, subtype_list, var = "ec_meet")
  lobbyists = lobby_by_IGtype_inner(data, subtype_list, var = "lobbyists_fte")
  ep_acc = lobby_by_IGtype_inner(data, subtype_list, var = "ep_acc")
  
  output = cbind(lobby_exp_low, lobby_exp_high, ec_meet, lobbyists, ep_acc)
  
  output$year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019)
  
  output$IGtype = IGtype
  
  return(output)
}

sopa_lobby_by_IGtype = function(data, subtype_list, IGtype){
  
  lobby_exp = lobby_by_IGtype_inner(data, subtype_list, var = "lobby_exp", eucd = F)
  lobbyists = lobby_by_IGtype_inner(data, subtype_list, var = "lobbyists", eucd = F)
  revolvers = lobby_by_IGtype_inner(data, subtype_list, var = "revolvers", eucd = F)
  
  output = cbind(lobby_exp, lobbyists, revolvers)
  
  output$year = c(2010, 2011, 2012)
  output$IGtype = IGtype
  
  return(output)
}

lobby_plot = function(data, ymin = 0, ymax, ylab, title, rangeplot = F){
  
  if(rangeplot == F){
    ymin_point = ymax
  } else {
    ymin_point = ymin
  }
  
  plot = ggplot(data, aes(ymin = ymin, x = reorder(IGtype, ymax), 
                          ymax = ymax,
                          shape = reorder(IGtype, ymax))) +
    geom_linerange(position = position_dodge(width=0.75), size = 1) +
    geom_point(aes(y = ymin_point), position = position_dodge(width=0.75), size = 3) +
    geom_point(aes(y = ymax), position = position_dodge(width=0.75), size = 3) +
    coord_flip() +
    ylab(ylab) +
    xlab(NULL) +
    ggtitle(title) +
    labs(shape = "Interest Group Type") + 
    scale_shape_manual(values = c("Civil Society Coalition" = 0,
                                  "Rightsholder" = 15,
                                  "Other" = 16,
                                  "Platforms" = 17,
                                  "Platform & ICT & Media" = 18,
                                  "Lobbying Agencies" = 6,
                                  "Creatives" = 13)) +
    theme_classic() + 
    theme(plot.title = element_text(face = "bold"))
  
  return(plot)
}

##### ii) SOPA #####
# read data
SOPA_IG_info = read_xlsx("SOPA/OpenSecrets_IGs_lobbying_on_Copyright.xlsx")

sum(subset(SOPA_IG_info, Oppose == 1)$lobby_exp_12, na.rm = T)
sum(subset(SOPA_IG_info, Support == 1)$lobby_exp_12, na.rm = T)

subtypes_rightsholder = c("Telecom", "Telecom & Media", "Education", "Manufacturer", 
                          "Manufacturers", "ICT & Consulting", "Electronics & ICT", 
                          "Manufacturing", "Rightsholder")

subtypes_plat = c("Platform", "Platforms and User Rights")

subtypes_civ_soc = c("User/Consumer Rights", "Library", 
                     "Open Source", "Open Software", 
                     "Cybersecurity", "Human Rights")

subtypes_plat_rh_mix = c("Domains", "Platform & ICT")

subtypes_creatives = c("Creatives")

subtypes_lobbyists = c("Law Firm", "Lobbying Agency")

subtypes_others = c("Other", "Seniors Advocacy", "Trade", "Trade Union", "Transport",
                    "Political Party", "Tax", "Security", "Real Estate", "Government",
                    "Political Action Committee", "Plumbing", "Police", "Firefighter", "Energy",
                    "Gambling", "Environment", "Finance",
                    "Hospitality", "Individual", "Individual Rights", "Science", "Marketing", "Retail Sales",
                    "Crypto", "Construction", "Chamber of Commerce")

sopa_IG_lobby_exp = select(SOPA_IG_info, c(Name, Subtype, Support, Oppose, lobby_exp_10, lobby_exp_11, lobby_exp_12))
sopa_IG_lobbyists = select(SOPA_IG_info, c(lobbyists_10, lobbyists_11, lobbyists_12))
sopa_IG_revolvers = select(SOPA_IG_info, c(revolvers_10, revolvers_11, revolvers_12))

sopa_IG_lobby_exp = sopa_IG_lobby_exp %>% 
  gather(key = "year", value = lobby_exp, lobby_exp_10:lobby_exp_12) 

sopa_IG_lobby_exp$year = as.numeric(gsub("lobby_exp_", "20", sopa_IG_lobby_exp$year))

sopa_IG_lobbyists = sopa_IG_lobbyists %>% 
  gather(key = "year", value = lobbyists, lobbyists_10:lobbyists_12) 

sopa_IG_lobbyists$year = as.numeric(gsub("lobbyists_", "20", sopa_IG_lobbyists$year))

sopa_IG_revolvers = sopa_IG_revolvers %>% 
  gather(key = "year", value = revolvers, revolvers_10:revolvers_12) 

sopa_IG_revolvers$year = as.numeric(gsub("revolvers_", "20", sopa_IG_revolvers$year))

SOPA_IG_lobby = cbind(sopa_IG_lobby_exp, sopa_IG_lobbyists, sopa_IG_revolvers)

SOPA_IG_lobby = as.data.frame(rbind(sopa_lobby_by_IGtype(SOPA_IG_lobby, subtypes_others, IGtype = "Other"),
                                    sopa_lobby_by_IGtype(SOPA_IG_lobby, subtypes_creatives, IGtype = "Creatives"),
                                    sopa_lobby_by_IGtype(SOPA_IG_lobby, subtypes_lobbyists, IGtype = "Lobbying Agencies"),
                                    sopa_lobby_by_IGtype(SOPA_IG_lobby, subtypes_plat, IGtype = "Platforms"),
                                    sopa_lobby_by_IGtype(SOPA_IG_lobby, subtypes_civ_soc, IGtype = "Civil Society Coalition"),
                                    sopa_lobby_by_IGtype(SOPA_IG_lobby, subtypes_plat_rh_mix, IGtype = "Platform & ICT & Media"),
                                    sopa_lobby_by_IGtype(SOPA_IG_lobby, subtypes_rightsholder, IGtype = "Rightsholder")))

SOPA_IG_lobby$lobby_exp_mio = SOPA_IG_lobby$lobby_exp/1000000

SOPA_IG_lobby = SOPA_IG_lobby %>%
  group_by(IGtype) %>%
  summarize(lobby_exp_mio = sum(lobby_exp_mio),
            revolvers = mean(revolvers),
            lobbyists = mean(lobbyists))

sopa_lobby_exp = lobby_plot(SOPA_IG_lobby, rangeplot = F, 
                            ymax = SOPA_IG_lobby$lobby_exp_mio,
                            ylab = "Total Lobbying Expenditure (Mio. $) (2010-12)",
                            title = "Lobbying Expenditure") + 
  guides(shape = "none")

sopa_lobbyists = lobby_plot(SOPA_IG_lobby, rangeplot = F, 
                            ymax = SOPA_IG_lobby$lobbyists,
                            ylab = "Average Number of Lobbyists (2010-12)",
                            title = "Lobbyists") + 
  guides(shape = "none")

sopa_revolvers = lobby_plot(SOPA_IG_lobby, rangeplot = F, 
                            ymax = SOPA_IG_lobby$revolvers,
                            ylab = "Average Number of Revolvers (2010-12)",
                            title = "Revolvers") + 
  guides(shape = "none")

sopa_lobby = plot_grid(sopa_lobby_exp, sopa_lobbyists, sopa_revolvers, ncol = 3, rel_widths = (c(1,1,1)))

##### iii) EUCD #####
EUCD_IG_info = read_xlsx("EUCD/EU Copyright_IGs_Klassifizierung.xlsx")
eucd_IG_lobby = read.csv("EUCD/EU Copyright_IGs_Tables_utf8.csv", encoding = "utf8")

eucd_IG_lobby = full_join(EUCD_IG_info, eucd_IG_lobby, by = c("regid", "website"))

eucd_IG_lobby$year = gsub("Financial year: [0-9]{2}/[0-9]{2}/[0-9]{4} - [0-9]{2}/[0-9]{2}/", "", eucd_IG_lobby$year)
eucd_IG_lobby$year = gsub(" ", "", eucd_IG_lobby$year)
eucd_IG_lobby$year = as.numeric(eucd_IG_lobby$year)

subtypes_rightsholder = c("Media Retail", "Data Analytics", "Furniture", "Television",
                          "Press Stock", "Electronics", "ICT", "Copyright Management", 
                          "Industry", "Education", "Rightsholder", "Telecom & Media",
                          "ICT, Media, Telecom", "Telecom & ICT", "Telecom")

subtypes_plat = c("Platform", "Platforms", "Platforms and User Rights", "Platform-dependent entrepreneur", 
                  "Tech & Platforms")

subtypes_civ_soc = c("Consumer Rights, User Rights", "Archives, Libraries, Museum, Theatre",
                     "Open Knowledge", "Open Source", "Open Access", "Open Source Software",
                     "Human Rights", "Blind Rights", "ICT Refurbishing")

subtypes_plat_rh_mix = c("Platform & ICT", "Media & ICT & Platforms", "ICT & Platform", "Platform & Media",
                         "Tech & Start ups")

subtypes_creatives = c("Artists", "Creative Sector", "Theater", "Translators")

subtypes_lobbyists = c("Law Firm", "Lobbying Agency", "Consultancy", "Consultant")

subtypes_others = c("Anti-Corruption", "Blockchain", "Development", "Health Policy", "Language",
                    "Political Party", "Start ups", "Transpareny", "Anti-HateSpeech", "Betting",
                    "Energy", "Geography", "Hospitality", "Individual", "Market Research",
                    "Regional Organization", "Tech & Digital Research", "Uploadfilter",
                    "Electoral Support", "Finance", "Health",  "Marketing", "Retail",
                    "Standardization", "Trade Union", "White collar workers", "Think Tank", 
                    "Science", "Computer Science", "Private-sponsored Science")

eucd_IG_lobby = as.data.frame(rbind(eu_lobby_by_IGtype(eucd_IG_lobby, subtypes_others, IGtype = "Other"),
                                    eu_lobby_by_IGtype(eucd_IG_lobby, subtypes_creatives, IGtype = "Creatives"),
                                    eu_lobby_by_IGtype(eucd_IG_lobby, subtypes_lobbyists, IGtype = "Lobbying Agencies"),
                                    eu_lobby_by_IGtype(eucd_IG_lobby, subtypes_plat, IGtype = "Platforms"),
                                    eu_lobby_by_IGtype(eucd_IG_lobby, subtypes_civ_soc, IGtype = "Civil Society Coalition"),
                                    eu_lobby_by_IGtype(eucd_IG_lobby, subtypes_plat_rh_mix, IGtype = "Platform & ICT & Media"),
                                    eu_lobby_by_IGtype(eucd_IG_lobby, subtypes_rightsholder, IGtype = "Rightsholder")))

eucd_IG_lobby$lobby_exp_low_mio = eucd_IG_lobby$lobby_exp_low/1000000
eucd_IG_lobby$lobby_exp_high_mio = eucd_IG_lobby$lobby_exp_high/1000000

eucd_IG_lobby = subset(eucd_IG_lobby, year > 2015)

eucd_IG_lobby = eucd_IG_lobby %>%
  group_by(IGtype) %>%
  summarize(lobby_exp_low_mio = sum(lobby_exp_low_mio, na.rm = T),
            lobby_exp_high_mio = sum(lobby_exp_high_mio, na.rm = T),
            lobbyists = mean(lobbyists_fte, na.rm = T),
            ep_acc = sum(ep_acc, na.rm = T)
            )

lobby_exp_eucd = lobby_plot(eucd_IG_lobby, rangeplot = T, 
                            ymin = eucd_IG_lobby$lobby_exp_low_mio, ymax = eucd_IG_lobby$lobby_exp_high_mio,
                            ylab = "Total Lobbying Expenditure (Mio. €) (2016-19)",
                            title = "Lobbying Expenditure") +
  guides(shape = "none")

#ec_meet_eucd = lobby_plot(eucd_IG_lobby, rangeplot = F, 
#                          ymin = 0, ymax = eucd_IG_lobby$ec_meet,
#                          ylab = "Total Number of Meetings with European Commission",
#                          title = "Meetings with European Commission") +
#  guides(shape = "none")

lobbyists_eucd = lobby_plot(eucd_IG_lobby, rangeplot = F, 
                            ymin = 0, ymax = eucd_IG_lobby$lobbyists,
                            ylab = "Average Number of Lobbyists (FTE) (2016-19)", 
                            title = "Lobbyists (FTE)") +
  guides(shape = "none")

ep_acc_eucd = lobby_plot(eucd_IG_lobby, rangeplot = F, 
                         ymin = 0, ymax = eucd_IG_lobby$ep_acc,
                         ylab = "Total EP Access Passes (2016-19)", 
                         title = "European Parliament Access Passes") +
  guides(shape = "none")

eucd_lobby = plot_grid(lobby_exp_eucd, lobbyists_eucd, ep_acc_eucd, nrow = 1, 
                       rel_widths = c(1,1,1))

sopa_eucd_lobby = plot_grid(sopa_lobby, eucd_lobby, nrow = 2, labels = c("SOPA", "EUCD"), label_size = 18,
                            label_fontface = "italic")

ggsave("Lobbyplot.png", sopa_eucd_lobby, width = 14, height = 8, dpi = 640)
ggsave("Lobbyplot.pdf", sopa_eucd_lobby, width = 14, height = 8, dpi = 640)

######################### 4. Prepare Time Series Data ############################
# function to aggregate by day
aggregate_day = function(data, type, start_date, end_date, no_days, ma = F, ma_order = NULL, week = F){
  
  data = select(data, c(datetime, censor, overly_broad, break_internet, 
                                        uploadfilter, economy, inno, lobby, astroturf, diversity_freepress, 
                                        privacy, consumers_users, public_safety, criminal, fair_pay, 
                                        culture_creator, bad_platforms, market_power, misinfo, reach))
  
  data$type = type
  data$datetime = as.Date(data$datetime)
  
  data = subset(data, data$datetime <= end_date & data$datetime >= start_date)
  
  # create dates for each day
  date_day = seq(as.Date(start_date), by = "day", length.out = no_days)
  date_day = format(date_day, "%Y-%m-%d")
  
  # create dataframe with each date as a row and each frame as a variable (cells are zero)
  censor = seq(0, by = 0, length.out = no_days)
  overly_broad = seq(0, by = 0, length.out = no_days)
  break_internet = seq(0, by = 0, length.out = no_days)
  uploadfilter = seq(0, by = 0, length.out = no_days)
  economy = seq(0, by = 0, length.out = no_days)
  inno = seq(0, by = 0, length.out = no_days)
  lobby = seq(0, by = 0, length.out = no_days)
  astroturf = seq(0, by = 0, length.out = no_days)
  diversity_freepress = seq(0, by = 0, length.out = no_days)
  privacy = seq(0, by = 0, length.out = no_days)
  consumers_users = seq(0, by = 0, length.out = no_days)
  public_safety = seq(0, by = 0, length.out = no_days)
  criminal = seq(0, by = 0, length.out = no_days)
  fair_pay = seq(0, by = 0, length.out = no_days)
  culture_creator = seq(0, by = 0, length.out = no_days)
  bad_platforms = seq(0, by = 0, length.out = no_days)
  market_power = seq(0, by = 0, length.out = no_days)
  misinfo = seq(0, by = 0, length.out = no_days)
  reach = seq(0, by = 0, length.out = no_days)
  count = seq(0, by = 0, length.out = no_days)
  
  empty_data = cbind(as.data.frame(date_day), as.data.frame(censor), as.data.frame(overly_broad),
                     as.data.frame(break_internet), as.data.frame(uploadfilter), as.data.frame(economy),
                     as.data.frame(inno), as.data.frame(lobby), as.data.frame(astroturf),
                     as.data.frame(diversity_freepress), as.data.frame(privacy), as.data.frame(consumers_users),
                     as.data.frame(public_safety), as.data.frame(criminal), as.data.frame(fair_pay),
                     as.data.frame(culture_creator), as.data.frame(bad_platforms), as.data.frame(market_power),
                     as.data.frame(misinfo), as.data.frame(reach), as.data.frame(count))
  
  # add type variable
  empty_data$type = type
  
  # rename to datetime
  names(empty_data)[names(empty_data) == "date_day"] = "datetime"
  
  #create a count
  data$count = 1
  
  # bind data and empty dataframe together
  data = rbind(data, empty_data)
  
  # group frames into aggregate categories (as cells in empty_dataframe are zero, they don't change the count)
  data$ag_censor_filter = ifelse(data$censor > 0 | data$overly_broad > 0 | data$uploadfilter > 0, 1, 0)
  data$ag_break_internet = data$break_internet
  data$ag_fair_creators = ifelse(data$fair_pay > 0 | data$culture_creator > 0, 1, 0)
  data$ag_criminal_safety = ifelse(data$criminal > 0 | data$public_safety > 0, 1, 0)
  data$ag_astro = data$astroturf
  data$ag_lobby = data$lobby
  data$ag_platform_power = ifelse(data$bad_platforms > 0 | data$market_power > 0, 1, 0)
  data$ag_econ_inno = ifelse(data$economy > 0 | data$inno > 0, 1, 0)
  data$ag_user_rights = ifelse(data$consumers_users > 0 | data$privacy > 0, 1, 0)
  data$ag_misinfo = data$misinfo
  data$ag_diversity = data$diversity_freepress
  data$ag_reach = data$reach
  
  if(week == T){
    data$time_var = format(data$datetime, "%Y-%U")
  } else {
    data$time_var = data$datetime
  }
  
  # summarize the number of frames issued by day
  data = data %>% 
    group_by(time_var) %>%
    summarise(ag_censor_filter = sum(ag_censor_filter),
              ag_break_internet = sum(ag_break_internet),
              ag_fair_creators = sum(ag_fair_creators),
              ag_criminal_safety = sum(ag_criminal_safety),
              ag_astro = sum(ag_astro),
              ag_lobby = sum(ag_lobby),
              ag_platform_power = sum(ag_platform_power),
              ag_econ_inno = sum(ag_econ_inno),
              ag_user_rights = sum(ag_user_rights),
              ag_misinfo = sum(ag_misinfo),
              ag_diversity = sum(ag_diversity),
              ag_reach = sum(ag_reach),
              count = sum(count))
  
  if(ma == T){
    #compute moving average
    data$ma_cf = as.numeric(movavg(data$ag_censor_filter, n = ma_order, type = "e"))
    data$ma_bi = as.numeric(movavg(data$ag_break_internet, n = ma_order, type = "e"))
    data$ma_fc = as.numeric(movavg(data$ag_fair_creators, n = ma_order, type = "e"))
    data$ma_cs = as.numeric(movavg(data$ag_criminal_safety, n = ma_order, type = "e"))
    data$ma_as = as.numeric(movavg(data$ag_astro, n = ma_order, type = "e"))
    data$ma_lo = as.numeric(movavg(data$ag_lobby, n = ma_order, type = "e"))
    data$ma_pp = as.numeric(movavg(data$ag_platform_power, n = ma_order, type = "e"))
    data$ma_ei = as.numeric(movavg(data$ag_econ_inno, n = ma_order, type = "e"))
    data$ma_ur = as.numeric(movavg(data$ag_user_rights, n = ma_order, type = "e"))
    data$ma_mi = as.numeric(movavg(data$ag_misinfo, n = ma_order, type = "e"))
    data$ma_di = as.numeric(movavg(data$ag_diversity, n = ma_order, type = "e"))
    data$ma_re = as.numeric(movavg(data$ag_reach, n = ma_order, type = "e"))
    
    data$ma_count = as.numeric(movavg(data$count, n = ma_order, type = "e"))
    
    # rename colums
    names(data)[names(data) == "ma_cf"] = paste("ma_cf", type, sep = "_")
    names(data)[names(data) == "ma_bi"] = paste("ma_bi", type, sep = "_")
    names(data)[names(data) == "ma_fc"] = paste("ma_fc", type, sep = "_")
    names(data)[names(data) == "ma_cs"] = paste("ma_cs", type, sep = "_")
    names(data)[names(data) == "ma_as"] = paste("ma_as", type, sep = "_")
    names(data)[names(data) == "ma_lo"] = paste("ma_lo", type, sep = "_")
    names(data)[names(data) == "ma_pp"] = paste("ma_pp", type, sep = "_")
    names(data)[names(data) == "ma_ei"] = paste("ma_ei", type, sep = "_")
    names(data)[names(data) == "ma_ur"] = paste("ma_ur", type, sep = "_")
    names(data)[names(data) == "ma_mi"] = paste("ma_mi", type, sep = "_")
    names(data)[names(data) == "ma_di"] = paste("ma_di", type, sep = "_")
    names(data)[names(data) == "ma_re"] = paste("ma_re", type, sep = "_")
    
    names(data)[names(data) == "ma_count"] = paste("ma_count", type, sep = "_")
  }
    
  # rename colums
  names(data)[names(data) == "ag_censor_filter"] = paste("censor_filter", type, sep = "_")
  names(data)[names(data) == "ag_break_internet"] = paste("break_internet", type, sep = "_")
  names(data)[names(data) == "ag_fair_creators"] = paste("fair_creators", type, sep = "_")
  names(data)[names(data) == "ag_criminal_safety"] = paste("criminal_safety", type, sep = "_")
  names(data)[names(data) == "ag_astro"] = paste("astro", type, sep = "_")
  names(data)[names(data) == "ag_lobby"] = paste("lobby", type, sep = "_")
  names(data)[names(data) == "ag_platform_power"] = paste("platform_power", type, sep = "_")
  names(data)[names(data) == "ag_econ_inno"] = paste("econ_inno", type, sep = "_")
  names(data)[names(data) == "ag_user_rights"] = paste("user_rights", type, sep = "_")
  names(data)[names(data) == "ag_misinfo"] = paste("misinfo", type, sep = "_")
  names(data)[names(data) == "ag_diversity"] = paste("diversity", type, sep = "_")
  names(data)[names(data) == "ag_reach"] = paste("reach", type, sep = "_")
  names(data)[names(data) == "count"] = paste("count", type, sep = "_")
  
  if(week == F){
    data$days = 1:nrow(data)
    data$weekday = lubridate::wday(data$time_var, week_start=1)
  } else if(week == T){
    data$weeks = 1:nrow(data)
  }

  return(data)
}

aggregate_day_EUCD = function(data, type, 
                              end_date = "2019-08-31", start_date = "2018-01-01", no_days = 608, 
                              ma = T, ma_order = 7, week = F){
  
  data$language[data$language == "Englisch"] = "en"
  data$language[data$language == "Deutsch"] = "de"
  data$language[data$language == "Niederländisch"] = "nl"
  data$language[data$language == "Polnisch"] = "pl"
  data$language[data$language == "Portugiesisch"] = "pt"
  data$language[data$language == "Spanisch"] = "es"
  data$language[data$language == "Französisch"] = "fr"
  data$language[data$language == "Italienisch"] = "it"
  
  sub_all = data
  sub_en = subset(data, language == "en")
  sub_de = subset(data, language == "de")
  sub_nl = subset(data, language == "nl")
  sub_fr = subset(data, language == "fr")
  sub_it = subset(data, language == "it")
  sub_pl = subset(data, language == "pl")
  sub_pt = subset(data, language == "pt")
  sub_es = subset(data, language == "es")
  sub_und = subset(data, language != "de" & language != "en" & language != "es" & 
                     language != "fr" & language != "it" & language != "nl" & language != "pl" &
                     language != "pt")
  
  sub_all = aggregate_day(data = sub_all, type = type,
                         end_date = end_date, start_date = start_date, no_days = no_days,
                         ma = ma, ma_order = ma_order, week = week)
  sub_en = aggregate_day(data = sub_en, type = type,
                         end_date = end_date, start_date = start_date, no_days = no_days,
                         ma = ma, ma_order = ma_order, week = week)
  sub_de = aggregate_day(data = sub_de, type = type,
                         end_date = end_date, start_date = start_date, no_days = no_days,
                         ma = ma, ma_order = ma_order, week = week)
  sub_nl = aggregate_day(data = sub_nl, type = type,
                         end_date = end_date, start_date = start_date, no_days = no_days,
                         ma = ma, ma_order = ma_order, week = week)
  
  sub_en$lang = "en"
  sub_de$lang = "de"
  sub_nl$lang = "nl"
  sub_all$lang = "all"
  
  data = rbind(sub_all, sub_en, sub_de, sub_nl)
  
  if(type != "IG_web"){
    sub_fr = aggregate_day(data = sub_fr, type = type,
                           end_date = end_date, start_date = start_date, no_days = no_days,
                           ma = ma, ma_order = ma_order, week = week)
    sub_it = aggregate_day(data = sub_it, type = type,
                           end_date = end_date, start_date = start_date, no_days = no_days,
                           ma = ma, ma_order = ma_order, week = week)
    sub_pl = aggregate_day(data = sub_pl, type = type,
                           end_date = end_date, start_date = start_date, no_days = no_days,
                           ma = ma, ma_order = ma_order, week = week)
    sub_pt = aggregate_day(data = sub_pt, type = type,
                           end_date = end_date, start_date = start_date, no_days = no_days,
                           ma = ma, ma_order = ma_order, week = week)
    sub_es = aggregate_day(data = sub_es, type = type,
                           end_date = end_date, start_date = start_date, no_days = no_days,
                           ma = ma, ma_order = ma_order, week = week)
    sub_fr$lang = "fr"
    sub_it$lang = "it"
    sub_pl$lang = "pl"
    sub_pt$lang = "pt"
    sub_es$lang = "es"
    
    
    data = rbind(data, sub_fr, sub_it, 
                 sub_pl, sub_pt, sub_es)
  }

  if(nrow(sub_und)>0){
    
    sub_und = aggregate_day(data = sub_und, type = type,
                            end_date = end_date, start_date = start_date, no_days = no_days,
                            ma = ma, ma_order = ma_order, week = week)
    
    sub_und$lang = "und"
    data = rbind(data, sub_und)
  }
  
  return(data)
}

compute_percents = function(data, type, norm_col = 2, ma = F, ma_col = NULL){
  
  count_col = norm_col + 12
  
  data$cf_perc = (as.vector(data[, norm_col])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$bi_perc = (as.vector(data[, (norm_col+1)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$fc_perc = (as.vector(data[, (norm_col+2)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$cs_perc = (as.vector(data[, (norm_col+3)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$as_perc = (as.vector(data[, (norm_col+4)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$lo_perc = (as.vector(data[, (norm_col+5)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$pp_perc = (as.vector(data[, (norm_col+6)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$ei_perc = (as.vector(data[, (norm_col+7)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$ur_perc = (as.vector(data[, (norm_col+8)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$mi_perc = (as.vector(data[, (norm_col+9)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$di_perc = (as.vector(data[, (norm_col+10)])[[1]]/as.vector(data[, count_col])[[1]])*100
  data$re_perc = (as.vector(data[, (norm_col+11)])[[1]]/as.vector(data[, count_col])[[1]])*100
  
  data$cf_perc[is.nan(data$cf_perc)] = 0
  data$bi_perc[is.nan(data$bi_perc)] = 0
  data$fc_perc[is.nan(data$fc_perc)] = 0
  data$cs_perc[is.nan(data$cs_perc)] = 0
  data$as_perc[is.nan(data$as_perc)] = 0
  data$lo_perc[is.nan(data$lo_perc)] = 0
  data$pp_perc[is.nan(data$pp_perc)] = 0
  data$ei_perc[is.nan(data$ei_perc)] = 0
  data$ur_perc[is.nan(data$ur_perc)] = 0
  data$mi_perc[is.nan(data$mi_perc)] = 0
  data$di_perc[is.nan(data$di_perc)] = 0
  data$re_perc[is.nan(data$re_perc)] = 0
  
  names(data)[names(data) == "cf_perc"] = paste("cf_perc", type, sep = "_")
  names(data)[names(data) == "bi_perc"] = paste("bi_perc", type, sep = "_")
  names(data)[names(data) == "fc_perc"] = paste("fc_perc", type, sep = "_")
  names(data)[names(data) == "cs_perc"] = paste("cs_perc", type, sep = "_")
  names(data)[names(data) == "as_perc"] = paste("as_perc", type, sep = "_")
  names(data)[names(data) == "lo_perc"] = paste("lo_perc", type, sep = "_")
  names(data)[names(data) == "pp_perc"] = paste("pp_perc", type, sep = "_")
  names(data)[names(data) == "ei_perc"] = paste("ei_perc", type, sep = "_")
  names(data)[names(data) == "ur_perc"] = paste("ur_perc", type, sep = "_")
  names(data)[names(data) == "mi_perc"] = paste("mi_perc", type, sep = "_")
  names(data)[names(data) == "di_perc"] = paste("di_perc", type, sep = "_")
  names(data)[names(data) == "re_perc"] = paste("re_perc", type, sep = "_")
  
  if(ma == T){
    data$ma_cf_perc = (as.vector(data[, (ma_col)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_bi_perc = (as.vector(data[, (ma_col+1)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_fc_perc = (as.vector(data[, (ma_col+2)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_cs_perc = (as.vector(data[, (ma_col+3)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_as_perc = (as.vector(data[, (ma_col+4)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_lo_perc = (as.vector(data[, (ma_col+5)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_pp_perc = (as.vector(data[, (ma_col+6)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_ei_perc = (as.vector(data[, (ma_col+7)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_ur_perc = (as.vector(data[, (ma_col+8)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_mi_perc = (as.vector(data[, (ma_col+9)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_di_perc = (as.vector(data[, (ma_col+10)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    data$ma_re_perc = (as.vector(data[, (ma_col+11)])[[1]]/as.vector(data[, ma_col+12])[[1]])*100
    
    data$ma_cf_perc[is.nan(data$ma_cf_perc)] = 0
    data$ma_bi_perc[is.nan(data$ma_bi_perc)] = 0
    data$ma_fc_perc[is.nan(data$ma_fc_perc)] = 0
    data$ma_cs_perc[is.nan(data$ma_cs_perc)] = 0
    data$ma_as_perc[is.nan(data$ma_as_perc)] = 0
    data$ma_lo_perc[is.nan(data$ma_lo_perc)] = 0
    data$ma_pp_perc[is.nan(data$ma_pp_perc)] = 0
    data$ma_ei_perc[is.nan(data$ma_ei_perc)] = 0
    data$ma_ur_perc[is.nan(data$ma_ur_perc)] = 0
    data$ma_mi_perc[is.nan(data$ma_mi_perc)] = 0
    data$ma_di_perc[is.nan(data$ma_di_perc)] = 0
    data$ma_re_perc[is.nan(data$ma_re_perc)] = 0
    
    names(data)[names(data) == "ma_cf_perc"] = paste("ma_cf_perc", type, sep = "_")
    names(data)[names(data) == "ma_bi_perc"] = paste("ma_bi_perc", type, sep = "_")
    names(data)[names(data) == "ma_fc_perc"] = paste("ma_fc_perc", type, sep = "_")
    names(data)[names(data) == "ma_cs_perc"] = paste("ma_cs_perc", type, sep = "_")
    names(data)[names(data) == "ma_as_perc"] = paste("ma_as_perc", type, sep = "_")
    names(data)[names(data) == "ma_lo_perc"] = paste("ma_lo_perc", type, sep = "_")
    names(data)[names(data) == "ma_pp_perc"] = paste("ma_pp_perc", type, sep = "_")
    names(data)[names(data) == "ma_ei_perc"] = paste("ma_ei_perc", type, sep = "_")
    names(data)[names(data) == "ma_ur_perc"] = paste("ma_ur_perc", type, sep = "_")
    names(data)[names(data) == "ma_mi_perc"] = paste("ma_mi_perc", type, sep = "_")
    names(data)[names(data) == "ma_di_perc"] = paste("ma_di_perc", type, sep = "_")
    names(data)[names(data) == "ma_re_perc"] = paste("ma_re_perc", type, sep = "_")
    
  }
  
  return(data)
}

effects_plot = function(model, y_list, ci = "95", sopa = T, xlim = 150, xlim_pos = F, xlim_pos_val = NULL, xbreaks = 25, lang = NULL, HC_se = F){
  
  if(xlim_pos == F){
    xlim_pos_val = xlim
    }
  
  sum_mod = summary(model)
  
  coeff_list = vector(mode = "list")
  plot_list = vector(mode = "list")
  
  for(i in 1:length(y_list)){
    coeff_list[[y_list[i]]] = sum_mod[["varresult"]][[y_list[i]]][["coefficients"]]
    
    coeff_list[[y_list[i]]] = data.frame(coeff_list[[y_list[i]]])
    
    coeff_list[[y_list[i]]]$var = rownames(coeff_list[[y_list[i]]])
    
    if(HC_se == T){
      ## Robust standard errors
      se_hc = diag(sqrt(vcovHC(model1)))
      
      covariates_length = (length(se_hc)/length(y_list))
      
      coeff_list[[y_list[i]]]$se = se_hc[((i-1)*covariates_length + 1):(i*covariates_length)]
    } else {
      coeff_list[[y_list[i]]]$se = coeff_list[[y_list[i]]][["Std..Error"]]
    }
    
    if(ci == "95"){
      coeff_list[[y_list[i]]][["ci_lo"]] = coeff_list[[y_list[i]]][["Estimate"]] - (1.96*coeff_list[[y_list[i]]]$se)
      coeff_list[[y_list[i]]][["ci_up"]] = coeff_list[[y_list[i]]][["Estimate"]] + (1.96*coeff_list[[y_list[i]]]$se)
    } else if (ci == "90"){
      coeff_list[[y_list[i]]][["ci_lo"]] = coeff_list[[y_list[i]]][["Estimate"]] - (1.64*coeff_list[[y_list[i]]]$se)
      coeff_list[[y_list[i]]][["ci_up"]] = coeff_list[[y_list[i]]][["Estimate"]] + (1.64*coeff_list[[y_list[i]]]$se)
    }
    
    coeff_list[[y_list[i]]]$r2 = sum_mod[["varresult"]][[y_list[i]]][["r.squared"]]
    
    if(sopa == T){
      
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..Internet.Blackout"] = "PLAT: Internet Blackout"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..American.Censorship.Day...PARL..SOPA.Hearings"] = "PLAT: American Censorship Day\n& PARL: SOPA Hearings"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..SOPA.introduced"] = "PARL: SOPA introduced"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..PIPA.proceeds"] = "PARL: PIPA proceeds"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..PIPA.introduced"] = "PARL: PIPA introduced"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..PIPA...COICA.hearings"] = "PARL: PIPA & COICA hearings"
      
      coeff_list[[y_list[i]]] = subset(coeff_list[[y_list[i]]],
                                       var == "PLAT: American Censorship Day\n& PARL: SOPA Hearings" | 
                                         var == "PLAT: Internet Blackout" | 
                                         var == "PARL: PIPA introduced" | 
                                         var == "PARL: PIPA & COICA hearings" | 
                                         var == "PARL: SOPA introduced" | 
                                         var == "PARL: PIPA proceeds")
      
      level_order = c("PLAT: American Censorship Day\n& PARL: SOPA Hearings", 
                      "PLAT: Internet Blackout", "PARL: PIPA introduced", 
                      "PARL: PIPA & COICA hearings", "PARL: SOPA introduced", 
                      "PARL: PIPA proceeds")
      
      title = NULL
      
      y_text = "PLAT: American Censorship Day\n& PARL: SOPA Hearings"
      
    } else {
      
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..Multiple.Platforms.Blackout"] = "PLAT: Multiple Platforms Blackout"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..Wikipedia.Blackouts...PARL..EP.re.opens.debate"] = "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PLAT..YouTube.Ads"] = "PLAT: YouTube Ads"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..EP.proceeds.to.trilogue"] = "PARL: EP proceeds to trilogue"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..EP.passes.EUCD"] = "PARL: EP passes EUCD"
      coeff_list[[y_list[i]]]$var[coeff_list[[y_list[i]]]$var == "PARL..Council.approves.EUCD"] = "PARL: Council approves EUCD"
      
      coeff_list[[y_list[i]]] = subset(coeff_list[[y_list[i]]],
                                       var == "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate" | 
                                         var == "PLAT: YouTube Ads" |
                                         var == "PLAT: Multiple Platforms Blackout" |
                                         var == "PARL: EP proceeds to trilogue" |
                                         var == "PARL: EP passes EUCD" |
                                         var == "PARL: Council approves EUCD")
      
      level_order = c("PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate",
                      "PLAT: YouTube Ads", "PLAT: Multiple Platforms Blackout",
                      "PARL: EP proceeds to trilogue", "PARL: EP passes EUCD",
                      "PARL: Council approves EUCD")
      
      title = as.character(lang)
      
      y_text = "PLAT: Wikipedia Blackouts &\nPARL: EP re-opens debate"
      
    }
    
    plot_list[[i]] = ggplot(data = coeff_list[[i]], aes(y = factor(var, level = level_order))) + 
      geom_vline(xintercept = 0, size = 0.25) + 
      geom_segment(aes(yend = factor(var, level = level_order), x = ci_lo, xend = ci_up), 
                   size = 1, color = "grey") +
      geom_point(aes(x = Estimate), size = 3) +
      scale_x_continuous(limits = c(-xlim,xlim_pos_val), breaks = c(seq(-xlim, xlim_pos_val, by = xbreaks))) +
      ylab("") +
      xlab("Estimate") +
      ggtitle(title, subtitle = paste("Dependent Variable:", y_list[[i]], sep = " ")) +
      theme_classic() +
      annotate("text", x = (xlim_pos_val-xbreaks), y = y_text,
               label = paste0("R2:\n", as.character(round(coeff_list[[i]]$r2[1],
                                                                   digits = 3))))
    
  }
  
  for(i in 2:length(y_list)){
    
    plot_list[[i]] = plot_list[[i]] + theme(axis.text.y = element_blank())
    
  }
  
  return(plot_list)
}

tab_mod_inner = function(model, var){
  # get model summary
  sum_mod = summary(model)
  
  #extract coefficients
  results = data.frame(sum_mod[["varresult"]][[var]][["coefficients"]])
  # rename
  names(results)[2] = "se"
  names(results)[3] = "p"
  # rounding
  results = round(results, 3)
  # stars for p values
  results$p_star = ifelse(results$p < 0.1, ".", "") 
  results$p_star = ifelse(results$p < 0.05, "*", results$p_star) 
  results$p_star = ifelse(results$p < 0.01, "**", results$p_star) 
  # merge estimate, standard errors, and stars into a single column
  results$output = paste0(results$Estimate, results$p_star, " (", results$se, ")")
  # delete all other columns
  results = select(results, output)
  
  # extract r2
  r2 = data.frame(sum_mod[["varresult"]][[var]][["r.squared"]])
  # rename and round
  names(r2)[1] = "output"
  rownames(r2) = "r2"
  r2 = round(r2, 3)
  
  # extract f
  f = t(data.frame(sum_mod[["varresult"]][[var]][["fstatistic"]]))
  # merge f statistics and degrees of freedom
  f = data.frame(paste0(round(f[1],2), "(df=",round(f[2]),";",round(f[3]),")"))
  # rename and round
  names(f)[1] = "output"
  rownames(f) = "F-Statistic"
  
  # extract residual standard error
  resse = data.frame(sum_mod[["varresult"]][[var]][["sigma"]])
  # rename and round
  names(resse)[1] = "output"
  rownames(resse) = "Resid. Std. Error"
  resse = round(resse, 3)
  # create output
  resse$output = paste0(resse$output, "(df=",sum_mod[["varresult"]][["diff_news"]][["df"]][2],")")
  
  #Log likelihood (full mod)
  loglik = data.frame(sum_mod[["logLik"]])
  # rename and round
  names(loglik)[1] = "output"
  rownames(loglik) = "Log Likelihood"
  loglik = round(loglik, 3)
  
  # Observations
  obs = data.frame(sum_mod[["obs"]])
  # rename and round
  names(obs)[1] = "output"
  rownames(obs) = "Observations"
  
  # bind 
  results = rbind(results, obs, r2, resse, f, loglik)
  
  names(results)[1] = var
  
  return(results)
}

tab_mod = function(model, sopa = F){
  
  var1 = tab_mod_inner(model = model, var = "diff_tweets")
  var2 = tab_mod_inner(model = model, var = "diff_news")
  var3 = tab_mod_inner(model = model, var = "diff_IG_plat")
  var4 = tab_mod_inner(model = model, var = "diff_IG_rh_cr")
  
  if(sopa == T){
    var5 = tab_mod_inner(model = model, var = "diff_congress")
  } else {
    var5 = tab_mod_inner(model = model, var = "diff_meps")
  }
  
  output = cbind(var1, var2, var3, var4, var5)
  
  output$names = rownames(output)
  
  return(output)
}

############# a) SOPA #############
###### I) SOPA Newspapers ######
sopa_news_dict = aggregate_day(data = sopa_news_dict, type = "news", 
                               end_date = "2012-05-31", start_date = "2010-09-20", no_days = 620,
                               ma = T, ma_order = 7, week = F)

sopa_news_dict = compute_percents(data = sopa_news_dict, type = "news", norm_col = 2, 
                                ma = T, ma_col = 15)

###### II) SOPA Tweets ######
sopa_tweets_dict = aggregate_day(data = sopa_tweets_dict, type = "tweets", 
                                 end_date = "2012-05-31", start_date = "2010-09-20", no_days = 620,
                                 ma = T, ma_order = 7, week = F)

sopa_tweets_dict = compute_percents(data = sopa_tweets_dict, type = "tweets", norm_col = 2, 
                                  ma = T, ma_col = 15)

###### III) SOPA IG Rightsholder ######
sopa_IG_dict = read.csv("SOPA/SOPA_FRAMES_Interest Groups_Websites&Tweetids.csv")

sopa_IG_rh_cr = subset(sopa_IG_dict, Subtype %in% subtypes_rightsholder | Subtype %in% subtypes_creatives)

sopa_IG_rh_cr = aggregate_day(data = sopa_IG_rh_cr, type = "IG_rh_cr", 
                               end_date = "2012-05-31", start_date = "2010-09-20", no_days = 620,
                               ma = T, ma_order = 7, week = F)

sopa_IG_rh_cr = compute_percents(data = sopa_IG_rh_cr, type = "IG_rh_cr", norm_col = 2, 
                                    ma = T, ma_col = 15)

###### IV) SOPA IG Platforms ######
sopa_IG_plat = subset(sopa_IG_dict, Subtype %in% subtypes_plat | Subtype %in% subtypes_civ_soc)

sopa_IG_plat = aggregate_day(data = sopa_IG_plat, type = "IG_plat", 
                            end_date = "2012-05-31", start_date = "2010-09-20", no_days = 620,
                            ma = T, ma_order = 7, week = F)

sopa_IG_plat = compute_percents(data = sopa_IG_plat, type = "IG_plat", norm_col = 2, 
                                    ma = T, ma_col = 15)

###### V) SOPA IG Other ######
sopa_IG_oth = subset(sopa_IG_dict, !(Subtype %in% subtypes_plat | 
                                  Subtype %in% subtypes_rightsholder | 
                                  Subtype %in% subtypes_creatives |
                                  Subtype %in% subtypes_civ_soc))

sopa_IG_oth = aggregate_day(data = sopa_IG_oth, type = "IG_oth", 
                            end_date = "2012-05-31", start_date = "2010-09-20", no_days = 620,
                            ma = T, ma_order = 7, week = F)

sopa_IG_oth = compute_percents(data = sopa_IG_oth, type = "IG_oth", norm_col = 2, 
                               ma = T, ma_col = 15)

###### VI) SOPA Congresspeople ######
sopa_congress = aggregate_day(data = congress_twitter, type = "congress", 
                          end_date = "2012-05-31", start_date = "2010-09-20", no_days = 620,
                          ma = T, ma_order = 7, week = F)

sopa_congress = compute_percents(data = sopa_congress, type = "congress", norm_col = 2, 
                               ma = T, ma_col = 15)

# combine datasets
sopa_ts = cbind(sopa_IG_plat, sopa_IG_rh_cr, sopa_IG_oth, sopa_tweets_dict, sopa_news_dict, sopa_congress)

###### VII) Aggregate Frames ######
# as the same paragraph can include break internet and censorship frames, percentages can be above 100)
# pro platform frames
sopa_ts$pro_plat_IG_plat = sopa_ts$censor_filter_IG_plat + sopa_ts$break_internet_IG_plat + sopa_ts$user_rights_IG_plat
sopa_ts$pro_plat_news = sopa_ts$censor_filter_news + sopa_ts$break_internet_news + sopa_ts$user_rights_news
sopa_ts$pro_plat_congress = sopa_ts$censor_filter_congress + sopa_ts$break_internet_congress + sopa_ts$user_rights_congress
sopa_ts$pro_plat_IG_rh_cr = sopa_ts$censor_filter_IG_rh_cr + sopa_ts$break_internet_IG_rh_cr + sopa_ts$user_rights_IG_rh_cr
sopa_ts$pro_plat_tweets = sopa_ts$censor_filter_tweets + sopa_ts$break_internet_tweets + sopa_ts$user_rights_tweets
sopa_ts$pro_plat_IG_oth = sopa_ts$censor_filter_IG_oth + sopa_ts$break_internet_IG_oth + sopa_ts$user_rights_IG_oth

sopa_ts$pro_plat_perc_IG_plat = sopa_ts$cf_perc_IG_plat + sopa_ts$bi_perc_IG_plat + sopa_ts$ur_perc_IG_plat
sopa_ts$pro_plat_perc_news = sopa_ts$cf_perc_news + sopa_ts$bi_perc_news + sopa_ts$ur_perc_news
sopa_ts$pro_plat_perc_congress = sopa_ts$cf_perc_congress + sopa_ts$bi_perc_congress + sopa_ts$ur_perc_congress
sopa_ts$pro_plat_perc_IG_rh_cr = sopa_ts$cf_perc_IG_rh_cr + sopa_ts$bi_perc_IG_rh_cr + sopa_ts$ur_perc_IG_rh_cr
sopa_ts$pro_plat_perc_tweets = sopa_ts$cf_perc_tweets + sopa_ts$bi_perc_tweets + sopa_ts$ur_perc_tweets
sopa_ts$pro_plat_perc_IG_oth = sopa_ts$cf_perc_IG_oth + sopa_ts$bi_perc_IG_oth + sopa_ts$ur_perc_IG_oth

sopa_ts$pro_plat_ma_IG_plat = sopa_ts$ma_cf_IG_plat + sopa_ts$ma_bi_IG_plat + sopa_ts$ma_ur_IG_plat
sopa_ts$pro_plat_ma_news = sopa_ts$ma_cf_news + sopa_ts$ma_bi_news + sopa_ts$ma_ur_news
sopa_ts$pro_plat_ma_congress = sopa_ts$ma_cf_congress + sopa_ts$ma_bi_congress + sopa_ts$ma_ur_congress
sopa_ts$pro_plat_ma_IG_rh_cr = sopa_ts$ma_cf_IG_rh_cr + sopa_ts$ma_bi_IG_rh_cr + sopa_ts$ma_ur_IG_rh_cr
sopa_ts$pro_plat_ma_tweets = sopa_ts$ma_cf_tweets + sopa_ts$ma_bi_tweets + sopa_ts$ma_ur_tweets
sopa_ts$pro_plat_ma_IG_oth = sopa_ts$ma_cf_IG_oth + sopa_ts$ma_bi_IG_oth + sopa_ts$ma_ur_IG_oth

sopa_ts$pro_plat_ma_perc_IG_plat = sopa_ts$ma_cf_perc_IG_plat + sopa_ts$ma_bi_perc_IG_plat + sopa_ts$ma_ur_perc_IG_plat
sopa_ts$pro_plat_ma_perc_news = sopa_ts$ma_cf_perc_news + sopa_ts$ma_bi_perc_news + sopa_ts$ma_ur_perc_news
sopa_ts$pro_plat_ma_perc_congress = sopa_ts$ma_cf_perc_congress + sopa_ts$ma_bi_perc_congress + sopa_ts$ma_ur_perc_congress
sopa_ts$pro_plat_ma_perc_IG_rh_cr = sopa_ts$ma_cf_perc_IG_rh_cr + sopa_ts$ma_bi_perc_IG_rh_cr + sopa_ts$ma_ur_perc_IG_rh_cr
sopa_ts$pro_plat_ma_perc_tweets = sopa_ts$ma_cf_perc_tweets + sopa_ts$ma_bi_perc_tweets + sopa_ts$ma_ur_perc_tweets
sopa_ts$pro_plat_ma_perc_IG_oth = sopa_ts$ma_cf_perc_IG_oth + sopa_ts$ma_bi_perc_IG_oth + sopa_ts$ma_ur_perc_IG_oth

# anti platform frames
sopa_ts$anti_plat_IG_plat = sopa_ts$fair_creators_IG_plat + sopa_ts$criminal_safety_IG_plat + sopa_ts$platform_power_IG_plat
sopa_ts$anti_plat_news = sopa_ts$fair_creators_news + sopa_ts$criminal_safety_news + sopa_ts$platform_power_news
sopa_ts$anti_plat_congress = sopa_ts$fair_creators_congress + sopa_ts$criminal_safety_congress + sopa_ts$platform_power_congress
sopa_ts$anti_plat_IG_rh_cr = sopa_ts$fair_creators_IG_rh_cr + sopa_ts$criminal_safety_IG_rh_cr + sopa_ts$platform_power_IG_rh_cr
sopa_ts$anti_plat_tweets = sopa_ts$fair_creators_tweets + sopa_ts$criminal_safety_tweets + sopa_ts$platform_power_tweets
sopa_ts$anti_plat_IG_oth = sopa_ts$fair_creators_IG_oth + sopa_ts$criminal_safety_IG_oth + sopa_ts$platform_power_IG_oth

sopa_ts$anti_plat_perc_IG_plat = sopa_ts$fc_perc_IG_plat + sopa_ts$cs_perc_IG_plat + sopa_ts$pp_perc_IG_plat
sopa_ts$anti_plat_perc_news = sopa_ts$fc_perc_news + sopa_ts$cs_perc_news + sopa_ts$pp_perc_news
sopa_ts$anti_plat_perc_congress = sopa_ts$fc_perc_congress + sopa_ts$cs_perc_congress + sopa_ts$pp_perc_congress
sopa_ts$anti_plat_perc_IG_rh_cr = sopa_ts$fc_perc_IG_rh_cr + sopa_ts$cs_perc_IG_rh_cr + sopa_ts$pp_perc_IG_rh_cr
sopa_ts$anti_plat_perc_tweets = sopa_ts$fc_perc_tweets + sopa_ts$cs_perc_tweets + sopa_ts$pp_perc_tweets
sopa_ts$anti_plat_perc_IG_oth = sopa_ts$fc_perc_IG_oth + sopa_ts$cs_perc_IG_oth + sopa_ts$pp_perc_IG_oth

sopa_ts$anti_plat_ma_IG_plat = sopa_ts$ma_fc_IG_plat + sopa_ts$ma_cs_IG_plat + sopa_ts$ma_pp_IG_plat
sopa_ts$anti_plat_ma_news = sopa_ts$ma_fc_news + sopa_ts$ma_cs_news + sopa_ts$ma_pp_news
sopa_ts$anti_plat_ma_congress = sopa_ts$ma_fc_congress + sopa_ts$ma_cs_congress + sopa_ts$ma_pp_congress
sopa_ts$anti_plat_ma_IG_rh_cr = sopa_ts$ma_fc_IG_rh_cr + sopa_ts$ma_cs_IG_rh_cr + sopa_ts$ma_pp_IG_rh_cr
sopa_ts$anti_plat_ma_tweets = sopa_ts$ma_fc_tweets + sopa_ts$ma_cs_tweets + sopa_ts$ma_pp_tweets
sopa_ts$anti_plat_ma_IG_oth = sopa_ts$ma_fc_IG_oth + sopa_ts$ma_cs_IG_oth + sopa_ts$ma_pp_IG_oth

sopa_ts$anti_plat_ma_perc_IG_plat = sopa_ts$ma_fc_perc_IG_plat + sopa_ts$ma_cs_perc_IG_plat + sopa_ts$ma_pp_perc_IG_plat
sopa_ts$anti_plat_ma_perc_news = sopa_ts$ma_fc_perc_news + sopa_ts$ma_cs_perc_news + sopa_ts$ma_pp_perc_news
sopa_ts$anti_plat_ma_perc_congress = sopa_ts$ma_fc_perc_congress + sopa_ts$ma_cs_perc_congress + sopa_ts$ma_pp_perc_congress
sopa_ts$anti_plat_ma_perc_IG_rh_cr = sopa_ts$ma_fc_perc_IG_rh_cr + sopa_ts$ma_cs_perc_IG_rh_cr + sopa_ts$ma_pp_perc_IG_rh_cr
sopa_ts$anti_plat_ma_perc_tweets = sopa_ts$ma_fc_perc_tweets + sopa_ts$ma_cs_perc_tweets + sopa_ts$ma_pp_perc_tweets
sopa_ts$anti_plat_ma_perc_IG_oth = sopa_ts$ma_fc_perc_IG_oth + sopa_ts$ma_cs_perc_IG_oth + sopa_ts$ma_pp_perc_IG_oth

# neutral frames
sopa_ts$neutral_IG_plat = sopa_ts$econ_inno_IG_plat + sopa_ts$diversity_IG_plat + sopa_ts$reach_IG_plat + sopa_ts$lobby_IG_plat + sopa_ts$misinfo_IG_plat + sopa_ts$astro_IG_plat
sopa_ts$neutral_news = sopa_ts$econ_inno_news + sopa_ts$diversity_news + sopa_ts$reach_news + sopa_ts$lobby_news + sopa_ts$misinfo_news + sopa_ts$astro_news
sopa_ts$neutral_congress = sopa_ts$econ_inno_congress + sopa_ts$diversity_congress + sopa_ts$reach_congress + sopa_ts$lobby_congress + sopa_ts$misinfo_congress + sopa_ts$astro_congress
sopa_ts$neutral_IG_rh_cr = sopa_ts$econ_inno_IG_rh_cr + sopa_ts$diversity_IG_rh_cr + sopa_ts$reach_IG_rh_cr + sopa_ts$lobby_IG_rh_cr + sopa_ts$misinfo_IG_rh_cr + sopa_ts$astro_IG_rh_cr
sopa_ts$neutral_tweets = sopa_ts$econ_inno_tweets + sopa_ts$diversity_tweets + sopa_ts$reach_tweets + sopa_ts$lobby_tweets + sopa_ts$misinfo_tweets + sopa_ts$astro_tweets
sopa_ts$neutral_IG_oth = sopa_ts$econ_inno_IG_oth + sopa_ts$diversity_IG_oth + sopa_ts$reach_IG_oth + sopa_ts$lobby_IG_oth + sopa_ts$misinfo_IG_oth + sopa_ts$astro_IG_oth

sopa_ts$neutral_perc_IG_plat = sopa_ts$ei_perc_IG_plat + sopa_ts$di_perc_IG_plat + sopa_ts$re_perc_IG_plat + sopa_ts$lo_perc_IG_plat + sopa_ts$mi_perc_IG_plat + sopa_ts$as_perc_IG_plat
sopa_ts$neutral_perc_news = sopa_ts$ei_perc_news + sopa_ts$di_perc_news + sopa_ts$re_perc_news + sopa_ts$lo_perc_news + sopa_ts$mi_perc_news + sopa_ts$as_perc_news
sopa_ts$neutral_perc_congress = sopa_ts$ei_perc_congress + sopa_ts$di_perc_congress + sopa_ts$re_perc_congress + sopa_ts$lo_perc_congress + sopa_ts$mi_perc_congress + sopa_ts$as_perc_congress
sopa_ts$neutral_perc_IG_rh_cr = sopa_ts$ei_perc_IG_rh_cr + sopa_ts$di_perc_IG_rh_cr + sopa_ts$re_perc_IG_rh_cr + sopa_ts$lo_perc_IG_rh_cr + sopa_ts$mi_perc_IG_rh_cr + sopa_ts$as_perc_IG_rh_cr
sopa_ts$neutral_perc_tweets = sopa_ts$ei_perc_tweets + sopa_ts$di_perc_tweets + sopa_ts$re_perc_tweets + sopa_ts$lo_perc_tweets + sopa_ts$mi_perc_tweets + sopa_ts$as_perc_tweets
sopa_ts$neutral_perc_IG_oth = sopa_ts$ei_perc_IG_oth + sopa_ts$di_perc_IG_oth + sopa_ts$re_perc_IG_oth + sopa_ts$lo_perc_IG_oth + sopa_ts$mi_perc_IG_oth + sopa_ts$as_perc_IG_oth

sopa_ts$neutral_ma_IG_plat = sopa_ts$ma_ei_IG_plat + sopa_ts$ma_di_IG_plat + sopa_ts$ma_re_IG_plat + sopa_ts$ma_lo_IG_plat + sopa_ts$ma_mi_IG_plat + sopa_ts$ma_as_IG_plat
sopa_ts$neutral_ma_news = sopa_ts$ma_ei_news + sopa_ts$ma_di_news + sopa_ts$ma_re_news + sopa_ts$ma_lo_news + sopa_ts$ma_mi_news + sopa_ts$ma_as_news
sopa_ts$neutral_ma_congress = sopa_ts$ma_ei_congress + sopa_ts$ma_di_congress + sopa_ts$ma_re_congress + sopa_ts$ma_lo_congress + sopa_ts$ma_mi_congress + sopa_ts$ma_as_congress
sopa_ts$neutral_ma_IG_rh_cr = sopa_ts$ma_ei_IG_rh_cr + sopa_ts$ma_di_IG_rh_cr + sopa_ts$ma_re_IG_rh_cr + sopa_ts$ma_lo_IG_rh_cr + sopa_ts$ma_mi_IG_rh_cr + sopa_ts$ma_as_IG_rh_cr
sopa_ts$neutral_ma_tweets = sopa_ts$ma_ei_tweets + sopa_ts$ma_di_tweets + sopa_ts$ma_re_tweets + sopa_ts$ma_lo_tweets + sopa_ts$ma_mi_tweets + sopa_ts$ma_as_tweets
sopa_ts$neutral_perc_IG_oth = sopa_ts$ei_perc_IG_oth + sopa_ts$di_perc_IG_oth + sopa_ts$re_perc_IG_oth + sopa_ts$lo_perc_IG_oth + sopa_ts$mi_perc_IG_oth + sopa_ts$as_perc_IG_oth

sopa_ts$neutral_ma_perc_IG_plat = sopa_ts$ma_ei_perc_IG_plat + sopa_ts$ma_di_perc_IG_plat + sopa_ts$ma_re_perc_IG_plat + sopa_ts$ma_lo_perc_IG_plat + sopa_ts$ma_mi_perc_IG_plat + sopa_ts$ma_as_perc_IG_plat
sopa_ts$neutral_ma_perc_news = sopa_ts$ma_ei_perc_news + sopa_ts$ma_di_perc_news + sopa_ts$ma_re_perc_news + sopa_ts$ma_lo_perc_news + sopa_ts$ma_mi_perc_news + sopa_ts$ma_as_perc_news
sopa_ts$neutral_ma_perc_congress = sopa_ts$ma_ei_perc_congress + sopa_ts$ma_di_perc_congress + sopa_ts$ma_re_perc_congress + sopa_ts$ma_lo_perc_congress + sopa_ts$ma_mi_perc_congress + sopa_ts$ma_as_perc_congress
sopa_ts$neutral_ma_perc_IG_rh_cr = sopa_ts$ma_ei_perc_IG_rh_cr + sopa_ts$ma_di_perc_IG_rh_cr + sopa_ts$ma_re_perc_IG_rh_cr + sopa_ts$ma_lo_perc_IG_rh_cr + sopa_ts$ma_mi_perc_IG_rh_cr + sopa_ts$ma_as_perc_IG_rh_cr
sopa_ts$neutral_ma_perc_tweets = sopa_ts$ma_ei_perc_tweets + sopa_ts$ma_di_perc_tweets + sopa_ts$ma_re_perc_tweets + sopa_ts$ma_lo_perc_tweets + sopa_ts$ma_mi_perc_tweets + sopa_ts$ma_as_perc_tweets
sopa_ts$neutral_ma_perc_IG_oth = sopa_ts$ma_ei_perc_IG_oth + sopa_ts$ma_di_perc_IG_oth + sopa_ts$ma_re_perc_IG_oth + sopa_ts$ma_lo_perc_IG_oth + sopa_ts$ma_mi_perc_IG_oth + sopa_ts$ma_as_perc_IG_oth

write.csv(sopa_ts, "SOPA/SOPA_Time-Series_Data_Days.csv")
#write.csv(sopa_ts, "SOPA/SOPA_Time-Series_Data_Weeks.csv")
#sopa_ts = read.csv("SOPA/SOPA_Time-Series_Data_Days.csv")

############# b) EUCD #############
##### I) EU Newspapers #####
eu_news_dict = aggregate_day_EUCD(data = eu_news_dict, type = "news", week = T, ma_order = 4)

# compute percentages
eu_news_dict = compute_percents(data = eu_news_dict, type = "news", norm_col = 2, 
                                ma = T, ma_col = 15)

##### II) EU Tweets #####
eucd_tweets_dict = aggregate_day_EUCD(data = eucd_tweets_dict, type = "tweets", week = T, ma_order = 4)

# compute percentages
eucd_tweets_dict = compute_percents(data = eucd_tweets_dict, type = "tweets", norm_col = 2,
                                    ma = T, ma_col = 15)

##### III) EU Interest Group Tweets #####
eucd_IG_dict = read.csv("EUCD/EUCD_FRAMES_Interest Groups_Websites&Tweetids.csv")

eucd_IG_plat = subset(eucd_IG_dict, Subtype %in% subtypes_plat | Subtype %in% subtypes_civ_soc)

eucd_IG_plat = aggregate_day_EUCD(data = eucd_IG_plat, type = "IG_plat", week = T, ma_order = 4)

# compute percentages
eucd_IG_plat = compute_percents(data = eucd_IG_plat, type = "IG_plat", norm_col = 2,
                                ma = T, ma_col = 15)

##### IV) EU Interest Group Websites #####
eucd_IG_rh_cr = subset(eucd_IG_dict, Subtype %in% subtypes_rightsholder | Subtype %in% subtypes_creatives)

eucd_IG_rh_cr = aggregate_day_EUCD(data = eucd_IG_rh_cr, type = "IG_rh_cr", week = T, ma_order = 4)

eucd_IG_rh_cr = compute_percents(data = eucd_IG_rh_cr, type = "IG_rh_cr", 
                                 norm_col = 2, ma = T, ma_col = 15)

##### V) EU Interest Group Websites #####
eucd_IG_oth = subset(eucd_IG_dict, !(Subtype %in% subtypes_plat | 
                                       Subtype %in% subtypes_rightsholder | 
                                       Subtype %in% subtypes_creatives | 
                                       Subtype %in% subtypes_civ_soc))

eucd_IG_oth = aggregate_day_EUCD(data = eucd_IG_oth, type = "IG_oth", week = T, ma_order = 4)

eucd_IG_oth = compute_percents(data = eucd_IG_oth, type = "IG_oth", 
                               norm_col = 2, ma = T, ma_col = 15)

##### VI) EU MEPS #####
eucd_tweets_meps = aggregate_day_EUCD(data = eucd_tweets_meps, type = "meps", week = T, ma_order = 4)

eucd_tweets_meps = compute_percents(data = eucd_tweets_meps, type = "meps", 
                                    norm_col = 2, ma = T, ma_col = 15)

# days
#eucd_ts = full_join(eucd_tweets_meps, eucd_tweets_dict, by = c("time_var", "days", "lang"))
#eucd_ts = full_join(eucd_ts, eucd_IG_plat, by = c("time_var", "days", "lang"))
#eucd_ts = full_join(eucd_ts, eu_news_dict, by = c("time_var", "days", "lang"))
#eucd_ts = full_join(eucd_ts, eucd_IG_rh_cr, by = c("time_var", "days", "lang"))
#eucd_ts = full_join(eucd_ts, eucd_IG_oth, by = c("time_var", "days", "lang"))

# weeks
eucd_ts = full_join(eucd_tweets_meps, eucd_tweets_dict, by = c("time_var", "weeks", "lang"))
eucd_ts = full_join(eucd_ts, eucd_IG_plat, by = c("time_var", "weeks", "lang"))
eucd_ts = full_join(eucd_ts, eu_news_dict, by = c("time_var", "weeks", "lang"))
eucd_ts = full_join(eucd_ts, eucd_IG_rh_cr, by = c("time_var", "weeks", "lang"))
eucd_ts = full_join(eucd_ts, eucd_IG_oth, by = c("time_var", "weeks", "lang"))

###### VII) Aggregate Frames ######
# as the same paragraph can include break internet and censorship frames, percentages can be above 100)
# pro platform frames
eucd_ts$pro_plat_IG_rh_cr = eucd_ts$censor_filter_IG_rh_cr + eucd_ts$break_internet_IG_rh_cr + eucd_ts$user_rights_IG_rh_cr
eucd_ts$pro_plat_news = eucd_ts$censor_filter_news + eucd_ts$break_internet_news + eucd_ts$user_rights_news
eucd_ts$pro_plat_meps = eucd_ts$censor_filter_meps + eucd_ts$break_internet_meps + eucd_ts$user_rights_meps
eucd_ts$pro_plat_IG_plat = eucd_ts$censor_filter_IG_plat + eucd_ts$break_internet_IG_plat + eucd_ts$user_rights_IG_plat
eucd_ts$pro_plat_tweets = eucd_ts$censor_filter_tweets + eucd_ts$break_internet_tweets + eucd_ts$user_rights_tweets
eucd_ts$pro_plat_IG_oth = eucd_ts$censor_filter_IG_oth + eucd_ts$break_internet_IG_oth + eucd_ts$user_rights_IG_oth

eucd_ts$pro_plat_perc_IG_rh_cr = eucd_ts$cf_perc_IG_rh_cr + eucd_ts$bi_perc_IG_rh_cr + eucd_ts$ur_perc_IG_rh_cr
eucd_ts$pro_plat_perc_news = eucd_ts$cf_perc_news + eucd_ts$bi_perc_news + eucd_ts$ur_perc_news
eucd_ts$pro_plat_perc_meps = eucd_ts$cf_perc_meps + eucd_ts$bi_perc_meps + eucd_ts$ur_perc_meps
eucd_ts$pro_plat_perc_IG_plat = eucd_ts$cf_perc_IG_plat + eucd_ts$bi_perc_IG_plat + eucd_ts$ur_perc_IG_plat
eucd_ts$pro_plat_perc_tweets = eucd_ts$cf_perc_tweets + eucd_ts$bi_perc_tweets + eucd_ts$ur_perc_tweets
eucd_ts$pro_plat_perc_IG_oth = eucd_ts$cf_perc_IG_oth + eucd_ts$bi_perc_IG_oth + eucd_ts$ur_perc_IG_oth

eucd_ts$pro_plat_ma_IG_rh_cr = eucd_ts$ma_cf_IG_rh_cr + eucd_ts$ma_bi_IG_rh_cr + eucd_ts$ma_ur_IG_rh_cr
eucd_ts$pro_plat_ma_news = eucd_ts$ma_cf_news + eucd_ts$ma_bi_news + eucd_ts$ma_ur_news
eucd_ts$pro_plat_ma_meps = eucd_ts$ma_cf_meps + eucd_ts$ma_bi_meps + eucd_ts$ma_ur_meps
eucd_ts$pro_plat_ma_IG_plat = eucd_ts$ma_cf_IG_plat + eucd_ts$ma_bi_IG_plat + eucd_ts$ma_ur_IG_plat
eucd_ts$pro_plat_ma_tweets = eucd_ts$ma_cf_tweets + eucd_ts$ma_bi_tweets + eucd_ts$ma_ur_tweets
eucd_ts$pro_plat_ma_IG_oth = eucd_ts$ma_cf_IG_oth + eucd_ts$ma_bi_IG_oth + eucd_ts$ma_ur_IG_oth

eucd_ts$pro_plat_ma_perc_IG_rh_cr = eucd_ts$ma_cf_perc_IG_rh_cr + eucd_ts$ma_bi_perc_IG_rh_cr + eucd_ts$ma_ur_perc_IG_rh_cr
eucd_ts$pro_plat_ma_perc_news = eucd_ts$ma_cf_perc_news + eucd_ts$ma_bi_perc_news + eucd_ts$ma_ur_perc_news
eucd_ts$pro_plat_ma_perc_meps = eucd_ts$ma_cf_perc_meps + eucd_ts$ma_bi_perc_meps + eucd_ts$ma_ur_perc_meps
eucd_ts$pro_plat_ma_perc_IG_plat = eucd_ts$ma_cf_perc_IG_plat + eucd_ts$ma_bi_perc_IG_plat + eucd_ts$ma_ur_perc_IG_plat
eucd_ts$pro_plat_ma_perc_tweets = eucd_ts$ma_cf_perc_tweets + eucd_ts$ma_bi_perc_tweets + eucd_ts$ma_ur_perc_tweets
eucd_ts$pro_plat_ma_perc_IG_oth = eucd_ts$ma_cf_perc_IG_oth + eucd_ts$ma_bi_perc_IG_oth + eucd_ts$ma_ur_perc_IG_oth

# anti platform frames
eucd_ts$anti_plat_IG_rh_cr = eucd_ts$fair_creators_IG_rh_cr + eucd_ts$criminal_safety_IG_rh_cr + eucd_ts$platform_power_IG_rh_cr
eucd_ts$anti_plat_news = eucd_ts$fair_creators_news + eucd_ts$criminal_safety_news + eucd_ts$platform_power_news
eucd_ts$anti_plat_meps = eucd_ts$fair_creators_meps + eucd_ts$criminal_safety_meps + eucd_ts$platform_power_meps
eucd_ts$anti_plat_IG_plat = eucd_ts$fair_creators_IG_plat + eucd_ts$criminal_safety_IG_plat + eucd_ts$platform_power_IG_plat
eucd_ts$anti_plat_tweets = eucd_ts$fair_creators_tweets + eucd_ts$criminal_safety_tweets + eucd_ts$platform_power_tweets
eucd_ts$anti_plat_IG_oth = eucd_ts$fair_creators_IG_oth + eucd_ts$criminal_safety_IG_oth + eucd_ts$platform_power_IG_oth

eucd_ts$anti_plat_perc_IG_rh_cr = eucd_ts$fc_perc_IG_rh_cr + eucd_ts$cs_perc_IG_rh_cr + eucd_ts$pp_perc_IG_rh_cr
eucd_ts$anti_plat_perc_news = eucd_ts$fc_perc_news + eucd_ts$cs_perc_news + eucd_ts$pp_perc_news
eucd_ts$anti_plat_perc_meps = eucd_ts$fc_perc_meps + eucd_ts$cs_perc_meps + eucd_ts$pp_perc_meps
eucd_ts$anti_plat_perc_IG_plat = eucd_ts$fc_perc_IG_plat + eucd_ts$cs_perc_IG_plat + eucd_ts$pp_perc_IG_plat
eucd_ts$anti_plat_perc_tweets = eucd_ts$fc_perc_tweets + eucd_ts$cs_perc_tweets + eucd_ts$pp_perc_tweets
eucd_ts$anti_plat_perc_IG_oth = eucd_ts$fc_perc_IG_oth + eucd_ts$cs_perc_IG_oth + eucd_ts$pp_perc_IG_oth

eucd_ts$anti_plat_ma_IG_rh_cr = eucd_ts$ma_fc_IG_rh_cr + eucd_ts$ma_cs_IG_rh_cr + eucd_ts$ma_pp_IG_rh_cr
eucd_ts$anti_plat_ma_news = eucd_ts$ma_fc_news + eucd_ts$ma_cs_news + eucd_ts$ma_pp_news
eucd_ts$anti_plat_ma_meps = eucd_ts$ma_fc_meps + eucd_ts$ma_cs_meps + eucd_ts$ma_pp_meps
eucd_ts$anti_plat_ma_IG_plat = eucd_ts$ma_fc_IG_plat + eucd_ts$ma_cs_IG_plat + eucd_ts$ma_pp_IG_plat
eucd_ts$anti_plat_ma_tweets = eucd_ts$ma_fc_tweets + eucd_ts$ma_cs_tweets + eucd_ts$ma_pp_tweets
eucd_ts$anti_plat_ma_IG_oth = eucd_ts$ma_fc_IG_oth + eucd_ts$ma_cs_IG_oth + eucd_ts$ma_pp_IG_oth

eucd_ts$anti_plat_ma_perc_IG_rh_cr = eucd_ts$ma_fc_perc_IG_rh_cr + eucd_ts$ma_cs_perc_IG_rh_cr + eucd_ts$ma_pp_perc_IG_rh_cr
eucd_ts$anti_plat_ma_perc_news = eucd_ts$ma_fc_perc_news + eucd_ts$ma_cs_perc_news + eucd_ts$ma_pp_perc_news
eucd_ts$anti_plat_ma_perc_meps = eucd_ts$ma_fc_perc_meps + eucd_ts$ma_cs_perc_meps + eucd_ts$ma_pp_perc_meps
eucd_ts$anti_plat_ma_perc_IG_plat = eucd_ts$ma_fc_perc_IG_plat + eucd_ts$ma_cs_perc_IG_plat + eucd_ts$ma_pp_perc_IG_plat
eucd_ts$anti_plat_ma_perc_tweets = eucd_ts$ma_fc_perc_tweets + eucd_ts$ma_cs_perc_tweets + eucd_ts$ma_pp_perc_tweets
eucd_ts$anti_plat_ma_perc_IG_oth = eucd_ts$ma_fc_perc_IG_oth + eucd_ts$ma_cs_perc_IG_oth + eucd_ts$ma_pp_perc_IG_oth

# neutral frames
eucd_ts$neutral_IG_rh_cr = eucd_ts$econ_inno_IG_rh_cr + eucd_ts$diversity_IG_rh_cr + eucd_ts$reach_IG_rh_cr + eucd_ts$lobby_IG_rh_cr + eucd_ts$misinfo_IG_rh_cr + eucd_ts$astro_IG_rh_cr
eucd_ts$neutral_news = eucd_ts$econ_inno_news + eucd_ts$diversity_news + eucd_ts$reach_news + eucd_ts$lobby_news + eucd_ts$misinfo_news + eucd_ts$astro_news
eucd_ts$neutral_meps = eucd_ts$econ_inno_meps + eucd_ts$diversity_meps + eucd_ts$reach_meps + eucd_ts$lobby_meps + eucd_ts$misinfo_meps + eucd_ts$astro_meps
eucd_ts$neutral_IG_plat = eucd_ts$econ_inno_IG_plat + eucd_ts$diversity_IG_plat + eucd_ts$reach_IG_plat + eucd_ts$lobby_IG_plat + eucd_ts$misinfo_IG_plat + eucd_ts$astro_IG_plat
eucd_ts$neutral_tweets = eucd_ts$econ_inno_tweets + eucd_ts$diversity_tweets + eucd_ts$reach_tweets + eucd_ts$lobby_tweets + eucd_ts$misinfo_tweets + eucd_ts$astro_tweets
eucd_ts$neutral_IG_oth = eucd_ts$econ_inno_IG_oth + eucd_ts$diversity_IG_oth + eucd_ts$reach_IG_oth + eucd_ts$lobby_IG_oth + eucd_ts$misinfo_IG_oth + eucd_ts$astro_IG_oth

eucd_ts$neutral_perc_IG_rh_cr = eucd_ts$ei_perc_IG_rh_cr + eucd_ts$di_perc_IG_rh_cr + eucd_ts$re_perc_IG_rh_cr + eucd_ts$lo_perc_IG_rh_cr + eucd_ts$mi_perc_IG_rh_cr + eucd_ts$as_perc_IG_rh_cr
eucd_ts$neutral_perc_news = eucd_ts$ei_perc_news + eucd_ts$di_perc_news + eucd_ts$re_perc_news + eucd_ts$lo_perc_news + eucd_ts$mi_perc_news + eucd_ts$as_perc_news
eucd_ts$neutral_perc_meps = eucd_ts$ei_perc_meps + eucd_ts$di_perc_meps + eucd_ts$re_perc_meps + eucd_ts$lo_perc_meps + eucd_ts$mi_perc_meps + eucd_ts$as_perc_meps
eucd_ts$neutral_perc_IG_plat = eucd_ts$ei_perc_IG_plat + eucd_ts$di_perc_IG_plat + eucd_ts$re_perc_IG_plat + eucd_ts$lo_perc_IG_plat + eucd_ts$mi_perc_IG_plat + eucd_ts$as_perc_IG_plat
eucd_ts$neutral_perc_tweets = eucd_ts$ei_perc_tweets + eucd_ts$di_perc_tweets + eucd_ts$re_perc_tweets + eucd_ts$lo_perc_tweets + eucd_ts$mi_perc_tweets + eucd_ts$as_perc_tweets
eucd_ts$neutral_perc_IG_oth = eucd_ts$ei_perc_IG_oth + eucd_ts$di_perc_IG_oth + eucd_ts$re_perc_IG_oth + eucd_ts$lo_perc_IG_oth + eucd_ts$mi_perc_IG_oth + eucd_ts$as_perc_IG_oth

eucd_ts$neutral_ma_IG_rh_cr = eucd_ts$ma_ei_IG_rh_cr + eucd_ts$ma_di_IG_rh_cr + eucd_ts$ma_re_IG_rh_cr + eucd_ts$ma_lo_IG_rh_cr + eucd_ts$ma_mi_IG_rh_cr + eucd_ts$ma_as_IG_rh_cr
eucd_ts$neutral_ma_news = eucd_ts$ma_ei_news + eucd_ts$ma_di_news + eucd_ts$ma_re_news + eucd_ts$ma_lo_news + eucd_ts$ma_mi_news + eucd_ts$ma_as_news
eucd_ts$neutral_ma_meps = eucd_ts$ma_ei_meps + eucd_ts$ma_di_meps + eucd_ts$ma_re_meps + eucd_ts$ma_lo_meps + eucd_ts$ma_mi_meps + eucd_ts$ma_as_meps
eucd_ts$neutral_ma_IG_plat = eucd_ts$ma_ei_IG_plat + eucd_ts$ma_di_IG_plat + eucd_ts$ma_re_IG_plat + eucd_ts$ma_lo_IG_plat + eucd_ts$ma_mi_IG_plat + eucd_ts$ma_as_IG_plat
eucd_ts$neutral_ma_tweets = eucd_ts$ma_ei_tweets + eucd_ts$ma_di_tweets + eucd_ts$ma_re_tweets + eucd_ts$ma_lo_tweets + eucd_ts$ma_mi_tweets + eucd_ts$ma_as_tweets
eucd_ts$neutral_ma_IG_oth = eucd_ts$ma_ei_IG_oth + eucd_ts$ma_di_IG_oth + eucd_ts$ma_re_IG_oth + eucd_ts$ma_lo_IG_oth + eucd_ts$ma_mi_IG_oth + eucd_ts$ma_as_IG_oth

eucd_ts$neutral_ma_perc_IG_rh_cr = eucd_ts$ma_ei_perc_IG_rh_cr + eucd_ts$ma_di_perc_IG_rh_cr + eucd_ts$ma_re_perc_IG_rh_cr + eucd_ts$ma_lo_perc_IG_rh_cr + eucd_ts$ma_mi_perc_IG_rh_cr + eucd_ts$ma_as_perc_IG_rh_cr
eucd_ts$neutral_ma_perc_news = eucd_ts$ma_ei_perc_news + eucd_ts$ma_di_perc_news + eucd_ts$ma_re_perc_news + eucd_ts$ma_lo_perc_news + eucd_ts$ma_mi_perc_news + eucd_ts$ma_as_perc_news
eucd_ts$neutral_ma_perc_meps = eucd_ts$ma_ei_perc_meps + eucd_ts$ma_di_perc_meps + eucd_ts$ma_re_perc_meps + eucd_ts$ma_lo_perc_meps + eucd_ts$ma_mi_perc_meps + eucd_ts$ma_as_perc_meps
eucd_ts$neutral_ma_perc_IG_plat = eucd_ts$ma_ei_perc_IG_plat + eucd_ts$ma_di_perc_IG_plat + eucd_ts$ma_re_perc_IG_plat + eucd_ts$ma_lo_perc_IG_plat + eucd_ts$ma_mi_perc_IG_plat + eucd_ts$ma_as_perc_IG_plat
eucd_ts$neutral_ma_perc_tweets = eucd_ts$ma_ei_perc_tweets + eucd_ts$ma_di_perc_tweets + eucd_ts$ma_re_perc_tweets + eucd_ts$ma_lo_perc_tweets + eucd_ts$ma_mi_perc_tweets + eucd_ts$ma_as_perc_tweets
eucd_ts$neutral_ma_perc_IG_oth = eucd_ts$ma_ei_perc_IG_oth + eucd_ts$ma_di_perc_IG_oth + eucd_ts$ma_re_perc_IG_oth + eucd_ts$ma_lo_perc_IG_oth + eucd_ts$ma_mi_perc_IG_oth + eucd_ts$ma_as_perc_IG_oth

#write.csv(eucd_ts, "EUCD/EUCD_Time-Series_Data_Days.csv")
write.csv(eucd_ts, "EUCD/EUCD_Time-Series_Data_Weeks.csv")

eucd_days = read.csv("EUCD/EUCD_Time-Series_Data_Days.csv")

eucd_days$anti_plat_perc_news


#############################
scatterzzz = function(data, var){

    data$time_var = as.Date(data$time_var)
    phase1 = "2018-07-03"
    phase2 = "2018-10-21"
    phase3 = "2019-03-20"
    phase4 = "2019-04-30"
    
    #create a variable containing info on the phase
    data$phase = ifelse(data$time_var < as.Date(phase1), 0, 1)
    data$phase = ifelse(data$time_var >= as.Date(phase2), 2, data$phase)
    data$phase = ifelse(data$time_var >= as.Date(phase3), 3, data$phase)
    data$phase = ifelse(data$time_var >= as.Date(phase4), 4, data$phase)
    
  if(var == "tweets"){
    data = select(data, c("anti_plat_perc_tweets", "pro_plat_perc_tweets", "count_tweets", "lang", "phase", "time_var"))
    data$rel_frames = data$anti_plat_perc_tweets - data$pro_plat_perc_tweets
    data$count = data$count_tweets
    ylab = "Tweets per Day"
  } else if (var == "news"){
    data = select(data, c("anti_plat_perc_news", "pro_plat_perc_news", "count_news", "lang", "phase", "time_var"))
    data$rel_frames = data$anti_plat_perc_news - data$pro_plat_perc_news
    data$count = data$count_news
    ylab = "Articles per Day"
  } else if (var == "meps"){
    data = select(data, c("anti_plat_perc_meps", "pro_plat_perc_meps", "count_meps", "lang", "phase", "time_var"))
    data$rel_frames = data$anti_plat_perc_meps - data$pro_plat_perc_meps
    data$count = data$count_meps
    ylab = "Tweets per Day"
  }
  
  data = subset(data, lang != "all" & lang != "und" & phase != 4)
  
  data = data %>%
  group_by(phase, lang) %>%
  summarize(rel_frames = sum(rel_frames, na.rm = T),
            count = mean(count, na.rm = T))
  
  data$phase[data$phase == 0] = "1: Pre-Wikipedia Blackout"
  data$phase[data$phase == 1] = "2: Post-Wikipedia Blackout"
  data$phase[data$phase == 2] = "3: Post-Youtube Ads"
  data$phase[data$phase == 3] = "4: Post-March Blackout"
  
  plot = ggplot(data, aes(x = rel_frames, y = count)) +
    geom_vline(xintercept = 0, color = "grey") +
    geom_hline(yintercept = mean(data$count), color = "grey") +
    geom_text(
      label=data$lang, 
      nudge_x = 0, nudge_y = 0, 
      check_overlap = F
    ) + facet_wrap(~phase) +
    ylab(ylab) +
    xlab("Relative Framing") + 
    theme_classic()
  
  return(plot)
}

tweets_scatter = scatterzzz(eucd_days, var = "tweets")
news_scatter = scatterzzz(eucd_days, var = "news")
meps_scatter = scatterzzz(eucd_days, var = "meps")

ggsave("EUCD_Tweets_Scatter.png", tweets_scatter, width = 6, height = 6, dpi = 640)
ggsave("EUCD_Tweets_Scatter.pdf", tweets_scatter, width = 6, height = 6, dpi = 640)
ggsave("EUCD_News_Scatter.png", news_scatter, width = 6, height = 6, dpi = 640)
ggsave("EUCD_News_Scatter.pdf", news_scatter, width = 6, height = 6, dpi = 640)
ggsave("EUCD_MEPs_Scatter.png", meps_scatter, width = 6, height = 6, dpi = 640)
ggsave("EUCD_MEPs_Scatter.pdf", meps_scatter, width = 6, height = 6, dpi = 640)
