library(tidyverse)
library(haven)

box <- read_dta("position_effects_box.dta") %>%
  select(prediction = `_margin`,
         lower = `_ci_lb`,
         upper = `_ci_ub`,
         position = `_m1`) %>%
  mutate(Context = "Election Day")

post <- read_dta("position_effects_post.dta") %>%
  select(prediction = `_margin`,
         lower = `_ci_lb`,
         upper = `_ci_ub`,
         position = `_m1`) %>%
  mutate(Context = "Postal Voting")

diff <- read_dta("post_box_effect.dta") %>%
  select(prediction = `_margin`,
         lower = `_ci_lb`,
         upper = `_ci_ub`,
         position = `_m1`)

diff_age <- read_dta("int_age_diff.dta") %>%
  select(prediction = `_margin`,
         lower = `_ci_lb`,
         upper = `_ci_ub`,
         position = `_m1`,
         age = `_at2`)

df <- bind_rows(box, post)

ggplot(df, aes(x = position,
               y = prediction,
               ymin = lower,
               ymax = upper, 
               color = Context,
               shape = Context)) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = .8) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = 1:10) +
  ylab("Predicted Vote Share") +
  xlab("Ballot Position") +
  scale_shape_manual("Context", values = c(16,15)) +
  scale_color_manual("Context", values = c("orange", "lightblue"))

ggsave(file = "ballot_position_pred.pdf", width = 10, height = 6)

ggplot(diff, aes(x = position,
               y = prediction,
               ymin = lower,
               ymax = upper)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0),
            fill = "orange",
            alpha = 0.05) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf),
            fill = "lightblue",
            alpha = 0.05) +
  geom_pointrange(position = position_dodge(width = 0.5),
                  size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = 1:10) +
  ylab(expression("Postal Voting - Election Day Voting")) +
  xlab("Ballot Position") +
  ylim(c(-0.04,0.025)) +
  annotate("text", x = 1, y = 0.021, hjust = 0, vjust = 0, size = 6,
           label = "Candidate performs better among postal voters") +
  annotate("text", x = 1, y = -0.039, hjust = 0, vjust = 0, size = 6,
           label = "Candidate performs better among election day voters")

ggsave(file = "ballot_position_diff_pred.pdf", width = 10, height = 6)

diff_age <- filter(diff_age, position == 1 | position == 5 | position == 10)

diff_age$age <- as.numeric(diff_age$age)

diff_age$position <- factor(paste0("Ballot Position = ", diff_age$position),
                            levels = paste0("Ballot Position = ", c(1,5,10)))

ggplot(diff_age, aes(x = age,
                 y = prediction,
                 ymin = lower,
                 ymax = upper)) +
  geom_hline(aes(yintercept = 0), lty = 2) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0),
            fill = "orange",
            alpha = 0.05) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf),
            fill = "lightblue",
            alpha = 0.05) +
  geom_ribbon(position = position_dodge(width = 0.5),
                  size = 1.5, 
              fill = "grey50", alpha = 0.5) +
  geom_line() +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  ylab(expression("Postal Voting - Election Day Voting")) +
  xlab("Age of Candidate") +
  ylim(c(-0.1,0.035)) +
  annotate("text", x = 20, y = 0.03, hjust = 0, vjust = 0, size = 3,
           label = "Candidate performs better among postal voters") +
  annotate("text", x = 20, y = -0.1, hjust = 0, vjust = 0, size = 3,
           label = "Candidate performs better among election day voters") +
  facet_wrap(~ position)

ggsave(file = "ballot_position_diff_pred_age.pdf", width = 12, height = 6)

#############################################
# Balance
#############################################

vars <- c("pol_int", "hh_important", "age", "female",
          "spd", "cdu", "linke", "gruene", "fdp", "afd")

vars_name <- c("Political Interest", "Interest in local issues",
               "Age (> 60 years)", "Female", "SPD",
               "CDU", "Left Party", "Green Party",
               "FDP", "AfD")

diff_in_means <- pmap_df(list(vars, vars_name), function(x,y){

  read_dta(paste0(x,".dta")) %>%
    select(prediction = `_margin`,
           lower = `_ci_lb`,
           upper = `_ci_ub`) %>%
    mutate(Variable = y)
  
})

diff_in_means$panel <- ifelse(grepl("SPD|CDU|Left|AfD|FDP|Green", diff_in_means$Variable),
                              "Evaluation of Parties", "Attitudes and Demographics")

ggplot(diff_in_means, aes(y = prediction,
                          ymin = lower, 
                          ymax = upper,
                          x = Variable)) +
  geom_pointrange() +
  geom_hline(aes(yintercept = 0), lty = 2) +
  facet_wrap(~ panel, scales = "free") +
  coord_flip() +
  ylab("Difference in Means") +
  xlab("") +
  theme_bw(base_size = 14)

ggsave(file = "diff_in_means.pdf", width = 10, height = 4.5)





