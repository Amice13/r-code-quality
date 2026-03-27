library(tidyverse)
library(ggpubr)
library(ggrepel)
library(scales)
source("economics/define-levels.R")
time_trend_subfield <- read_rds("economics/output/time_trend_subfield.rds")
# set theme -----------------------------------
theme_set(theme_bw() +
            theme(strip.background = element_blank(),
                  strip.text = element_text(color = "black", hjust = 0),
                  axis.title.x = element_blank(),
                  plot.title = element_text(size = 11),
                  legend.text = element_text(size = 10),
                  legend.title = element_blank(),
                  legend.position = "bottom",
                  legend.key.width = unit(8, 'char')))


# create figure template -----------------------------------
p <- (ggplot(data = NULL,
             aes(x = Year,
                 y = total_adopters_share,
                 label = paste0(pretty_number(
                   total_adopters_share * 100, digits = 2),'% '))) +
        labs(y = element_blank()) +
        geom_line(aes(color = Practice,
                      size = thicker_line,
                      linetype = Practice)) +
        scale_x_continuous(limits = c(NA, 2018),
                           breaks = seq(lower_year, 2017, by = 3),
                           minor_breaks = seq(lower_year, 2017, by = 1.5),
                           expand = c(0, 0.5, 0, .95)) + # distance from right side
        scale_y_continuous(labels = function(x) paste0(pretty_number(x * 100, digits = 3), '%'),
                           limits = response_rate_limits,
                           breaks = response_rate_breaks,
                           minor_breaks = seq(0, 1, by = 0.1)) +
        scale_size_manual(values = c('TRUE' = 1.3, 'FALSE' = 1.1),
                          guide = 'none') +
        scale_linetype_manual(values = c('Any' = 'solid',
                                         'Posting data or code online' = 'longdash',
                                         'Posting study instruments online'='dotdash',
                                         'Pre-registering hypotheses or analyses'='dotted')) +
        scale_color_manual(values = color_mapping_practice) +
        guides(color = "legend",
               guide_legend(nrow = 2, byrow = TRUE)) +
        guides(linetype = guide_legend(nrow=2, byrow=TRUE,
                                       override.aes = list(size = 2))) # legend size
)



## Economics Discipline Figure -----------------------------------

econ_all <-
  p %+% (time_trend_subfield %>%
           filter(Year != Inf,
                  Year >= lower_year,
                  Year <= 2017,
                  econ_subfield == "All",
                  Discipline == 'Economics',
                  `Research type` == combined_string_research_type)) +
  facet_wrap(~econ_subfield) + ## to keep panel at the top
  geom_text_repel(data = time_trend_subfield %>%
                    filter(Year == 2017,
                           econ_subfield == "All"), # add this here because subfield need to be specified
                  nudge_x = 0.5,
                  min.segment.length = 5,
                  nudge_y = 0,
                  size = 3,
                  hjust = 0,
                  direction='y') +
  labs(tag = LETTERS[1]) +
  scale_x_continuous(limits = c(NA, 2017),
                     breaks = seq(lower_year, 2017, by = 3),
                     minor_breaks = seq(lower_year, 2017, by = 1.5),
                     expand = c(0, 0.5, 0, .8))

econ_all



# developmental economics -----------------------------------
dev_econ <-
  p %+% (time_trend_subfield %>%
           filter(Year != Inf,
                  Year >= lower_year,
                  Year <= 2017,
                  econ_subfield == "Development Economics",
                  Discipline == 'Economics',
                  `Research type` == combined_string_research_type)) +
  facet_wrap(~econ_subfield) + ## to keep panels at the top
  geom_text_repel(data = time_trend_subfield %>% # add this here because subfield need to be specified
                    filter(Year == 2017,
                           econ_subfield == "Development Economics",
                           Practice != "Posting data or code online"),
                  nudge_x = 0.5,
                  min.segment.length = 5,
                  nudge_y = 0,
                  size = 3,
                  hjust = 0,
                  direction='y') +
  labs(tag = LETTERS[2]) +
  scale_x_continuous(limits = c(NA, 2018),
                     breaks = seq(lower_year, 2017, by = 3),
                     minor_breaks = seq(lower_year, 2017, by = 1.5),
                     expand = c(0, 0.5, 0, 1.5))


dev_econ

# other econ subfield figures -----------------------------------
tt_subfields <- time_trend_subfield %>%
  filter(Year == 2017) %>%
  distinct(econ_subfield, .keep_all = T) %>%
  arrange(desc(total_adopters_share)) %>%
  pull(econ_subfield) %>%
  .[c(-1, -4)]
tt_plots <- list()

for(i in 1:length(tt_subfields)){
  if (tt_subfields[i] == "All"){
    upper
  }
  tt_plots[[i]] <-
    p %+% (time_trend_subfield %>%
             filter(Year != Inf,
                    Year >= lower_year,
                    Year <= 2017,
                    econ_subfield == tt_subfields[i],
                    Discipline == 'Economics',
                    `Research type` == combined_string_research_type)) +
    facet_wrap(~econ_subfield) +
    geom_text_repel(data = time_trend_subfield %>%
                      filter(Year == 2017,
                             econ_subfield == tt_subfields[i]),
                    nudge_x = 0.5,
                    min.segment.length = 5,
                    nudge_y = 0,
                    size = 3,
                    hjust = 0,
                    direction='y') +
    labs(tag = LETTERS[i + 2]) ## edit here to change tags

}

# combine figures -----------------------------------
ggpubr::ggarrange(econ_all,
                  ggarrange(
                    dev_econ,
                    tt_plots[[1]],
                    tt_plots[[2]],
                    tt_plots[[3]],
                    legend = "none"),
                  nrow = 2,
                  common.legend = TRUE,
                  heights = c(1.1, 1.5), # porportion between the two rows
                  legend = "bottom")

# export -----------------------------------
ggsave("economics/output/time_trend_econ_subfields.png", height = 7.5, width = 7.5, units = "in")

# https://ggplot2.tidyverse.org/reference/ggsave.html
# ggsave("time_trend_econ_subfields.eps", height = 7.5, width = 7.5,
#        units = "in",
#        device = "eps")

