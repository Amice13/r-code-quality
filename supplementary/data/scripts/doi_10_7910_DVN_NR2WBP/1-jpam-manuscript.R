set.seed(123)
pacman::p_load(here, emmeans, tidyverse, estimatr, janitor, ggrepel, directlabels)

##########################################################################################
####################################### Load Data ########################################
##########################################################################################
df <- read_csv(here::here("data/workbc-cleaned.csv"))

# Factor condition
df <- df %>% mutate(condition = factor(condition, levels = c('Control (no email)', 'Social Norms', 'Checklist'))) 

##########################################################################################
################################# Descriptive Statistics #################################
##########################################################################################
# Total sample size
df %>% tally()

# Total sample size by condition
df %>% group_by(condition) %>% tally()

# Total sample size by sex
df %>% group_by(gender) %>% tally()

# Total sample size by education
df %>% group_by(education_cat) %>% tally()

# Total sample size by education
df %>% group_by(time_since_ei_application) %>% tally()

###########################################################################################
############################# Enrollment Treatment Effects ################################
###########################################################################################
# H1
lm_1 <- lm_robust(enrolled_1mo ~ condition, weights = invwt, data = df, se_type = 'stata')

# Linear comparison of checklist vs social norms
lm1_em <- emmeans(lm_1, "condition")
pairs(lm1_em, adjust="holm", infer=T) 

###########################################################################################
####################################### Figure 5 ##########################################
###########################################################################################
df %>%
  group_by(condition) %>%
  do(tidy(lm_robust(enrolled_1mo ~ 1, weights = invwt, data = ., se_type = 'stata'))) %>% 
  mutate(Y = estimate) %>%
  ungroup %>%
  mutate(condition = case_when(condition=='Checklist' ~ 'Checklist\nemail nudge\n(N=4619)',
                               condition=='Social Norms' ~ 'Social norms\nemail nudge\n(N=4624)',
                               TRUE ~ 'Control\n(N=4765)')) %>%
  mutate(condition = factor(condition, levels = c('Control\n(N=4765)', 'Social norms\nemail nudge\n(N=4624)', 'Checklist\nemail nudge\n(N=4619)'))) %>%
  ggplot(., aes((condition), Y)) +
  geom_col(aes(fill=condition), color='black', width = 0.35) + # Plot points
  scale_fill_manual(values = c("white", "#d4d5d7", "#828282")) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.15, colour=c("black","black","black")) +
  geom_label_repel(aes(label=paste0(sprintf("%0.2f", round((100*estimate), digits = 3)), '%')),force = 0, nudge_y = .004, fontface = 'bold', segment.color = NA, label.size = NA, fill='white') +
  ylab(NULL) +
  xlab(NULL) +
  theme_classic() +
  labs(title="",
       subtitle="",
       x="",
       # y="% enrolled after 30 days",
       caption = "Note: 95% confidence intervals shown using robust standard errors") +
  theme(text = element_text(size=14),
        plot.caption = element_text(hjust = 0.5),
        axis.line = element_blank(),
        # axis.title.y = element_text(angle = 0),
        legend.position="none") +
  scale_y_continuous(breaks=c(0, .01), limits = c(0,.0125), labels = scales::percent_format(accuracy = 1))
ggsave(here::here('output/manuscript','figure-5.png'), width=6, height=5)

###########################################################################################
############################# Engagement Treatment Effects ################################
###########################################################################################
df %>%
  filter(condition != 'Control (no email)') %>%
  dplyr::select(event_opened, event_clicked, event_submitted, condition, invwt) %>%
  pivot_longer(-c(condition, invwt)) %>%
  nest(data = -c(name)) %>%
  mutate(
    fit = map(data, ~ lm_robust(value ~ condition, data = .x, weights = invwt, se_type='stata')),
    tidied = map(fit, tidy)
  ) %>%
  unnest(tidied) %>%
  mutate(p_adj = p.adjust(p.value)) %>%
  dplyr::select(name, term, estimate, std.error, p_adj, conf.low, conf.high, df)

###########################################################################################
####################################### Figure 6 ##########################################
###########################################################################################
df %>%
  filter(condition != 'Control (no email)') %>%
  dplyr::select(event_opened, event_clicked, event_submitted, condition) %>%
  pivot_longer(-condition) %>%
  nest(data = -c(name, condition)) %>%
  mutate(
    fit = map(data, ~ lm_robust(value ~ 1, data = .x, se_type='stata')),
    tidied = map(fit, tidy)
  ) %>%
  unnest(tidied) %>%
  mutate(Y = estimate) %>%
  mutate(condition = case_when(condition=='Checklist' ~ 'Checklist\nemail nudge\n(N=4619)',
                               condition=='Social Norms' ~ 'Social norms\nemail nudge\n(N=4624)')) %>%
  mutate(condition = factor(condition, levels = c('Social norms\nemail nudge\n(N=4624)', 'Checklist\nemail nudge\n(N=4619)'))) %>%
  mutate(variable = case_when(name == 'event_opened' ~ 'Email Opens',
                              name == 'event_clicked' ~ 'Click-Throughs',
                              name == 'event_submitted' ~ 'Form Submissions')) %>%
  mutate(variable = factor(variable, levels = c('Email Opens', 'Click-Throughs', 'Form Submissions'))) %>%
  ggplot(., aes(x=condition, y=estimate, group=variable)) +
  geom_col(aes(fill=condition), color='black', width = 0.5) + # Plot points
  scale_fill_manual(values = c("#d4d5d7", "#828282")) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.15, colour='black') +
  geom_label_repel(aes(label=paste0(sprintf("%0.2f", round((100*estimate), digits = 3)), '%')),force = 0, nudge_y = .05, fontface = 'bold', segment.color = NA, label.size = NA, fill='white') +
  ylab(NULL) +
  xlab(NULL) +
  theme_classic() +
  facet_wrap(~ variable, strip.position="bottom") +
  labs(title="",
       subtitle="",
       x="",
       y="",
       caption = "Note: 95% confidence intervals shown using robust standard errors") +
  theme(text = element_text(size=14),
        plot.caption = element_text(hjust = 0.5),
        legend.spacing.y = unit(0.5, 'cm'),
        legend.box.margin = margin(12, 12, 12, 12),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_rect(colour=NA, fill=NA),
        panel.border = element_rect(fill = NA, color = NA),
        axis.text.x=element_blank(),
        legend.title = element_blank()) +
  guides(fill = guide_legend(byrow = TRUE)) +
  scale_y_continuous(breaks=c(0, .2, .4), limits = c(0,.4), labels = scales::percent_format(accuracy = 1)) # Turn Y-axis to percent
ggsave(here::here('output/manuscript','figure-6.png'), width=8, height=5)

###########################################################################################
######################################### LATE ############################################
###########################################################################################
# checklist
late_checklist <- estimatr::iv_robust(enrolled_1mo ~ event_opened | condition, weights = invwt, data = filter(df, condition!='Social Norms'), diagnostics = T) # without fixed effects
summary(late_checklist)

# social norms
late_social_norms <- estimatr::iv_robust(enrolled_1mo ~ event_opened | condition, weights = invwt, data = filter(df, condition!='Checklist'), diagnostics = T) # without fixed effects
summary(late_social_norms)

