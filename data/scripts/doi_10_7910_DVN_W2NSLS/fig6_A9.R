############################################################################################
#### Fig 6: Increases in Twitter Followers China vs Hong Kong By Category (Regression Estimate)
#### Fig A9: Increases in Twitter Followers from China versus Others (Regression Estimate)
#### data: twitter/data/fig6_daily_follower_china_hk.csv
####       twitter/data/figA9_daily_follower_china_tw.csv
####       twitter/data/figA9_daily_follower_china_us.csv
#### outputs: twitter/figs/Fig6_summary_china_vs_hk_zh.pdf
####          twitter/figs/FigA9_summary_china_vs_others_zh.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("data.table"); library("here")

ReorderAccountType = function(df) {
  df = df %>%
    mutate(type_complete = fct_relevel(type_complete,
                                       "International News Agencies",
                                       "Citizen Journalists / Political Bloggers",
                                       "Activists or US / Taiwan / Hong Kong Politics",
                                       "Pornography Accounts",
                                       "State Media or Chinese Officials",
                                       "Non-Political Bloggers or Entertainment Accounts"))
  return(df)
}

df_type = read_csv(here("twitter/data/fig6_daily_follower_china_hk.csv")) %>%
  ReorderAccountType(.)

df_type_list = list(
  read_csv(here("twitter/data/fig6_daily_follower_china_hk.csv")) %>% ReorderAccountType(.),
  read_csv(here("twitter/data/figA9_daily_follower_china_tw.csv")) %>% ReorderAccountType(.),
  read_csv(here("twitter/data/figA9_daily_follower_china_us.csv")) %>% ReorderAccountType(.)
  )
names(df_type_list) = c("Hong Kong", "Taiwan", "US")

## Below is vestigial from when aggregation happened here.
# 
# files = list.files('Data/top_followers_fastCrawl_aggs', full.names=TRUE) %>%
#   .[grep('followers_count_zh', .)]
# files = files[files!="Data/top_followers_fastCrawl/followers_count_by_location_day.csv"]
# pageids = str_split_fixed(files, "_|/", 7)[, 6]
# df_by_day = lapply(files, function(x) {read_csv(x, col_types = "cDcd")}) %>%
#   data.table::rbindlist(.) %>%
#   complete(user_id, follow_date, location_classified, fill = list(followers = 0)) %>%
#   filter(follow_date <= as.Date("2020-04-30"))
# accounts = read_csv("Data/CN_popular_account_list.csv", col_types = "cnccccncccncnncc")
# length(unique(accounts$id_str))
# length(unique(df_by_day$user_id))
# 
# df_by_day = df_by_day %>%
#   inner_join(accounts, by = c("user_id" = "id_str"))
# 
# df_type_list = sapply(c("Hong Kong", "Taiwan", "US"), 
#                       simplify = FALSE, USE.NAMES = TRUE, 
#                       function(x) {
#                         df_by_day %>%
#                           filter(follow_date >= "2019-12-01" & follow_date < "2020-04-01") %>%
#                           filter(zh == 1) %>%
#                           mutate(type_complete = fct_relevel(type_complete,
#                                                              "International News Agencies",
#                                                              "Citizen Journalists / Political Bloggers",
#                                                              "Activists or US / Taiwan / Hong Kong Politics",
#                                                              "Pornography Accounts",
#                                                              "State Media or Chinese Officials",
#                                                              "Non-Political Bloggers or Entertainment Accounts")) %>%
#                           group_by(location_classified, follow_date, type_complete) %>%
#                           summarize(followers=sum(followers)) %>%
#                           ungroup() %>%
#                           complete(type_complete, follow_date, location_classified, fill = list(followers = 0)) %>%
#                           group_by(location_classified, type_complete) %>%
#                           filter(location_classified==x | location_classified=="China") %>%
#                           mutate(co_period=ifelse(follow_date>="2019-12-01" & follow_date<="2019-12-31", 1, 0),
#                                  tr_period=ifelse(follow_date>="2020-01-23" & follow_date<="2020-03-13", 1, 0),
#                                  china=ifelse(location_classified=="China", 1, 0)) %>%
#                           filter(tr_period==1 | co_period==1)
#                       })
# 
# write_csv(df_type_list[[1]], here("twitter/data/fig6_daily_follower_china_hk.csv"))
# write_csv(df_type_list[[2]], here("twitter/data/figA9_daily_follower_china_tw.csv"))
# write_csv(df_type_list[[3]], here("twitter/data/figA9_daily_follower_china_us.csv"))


### Fig 6


models_nb = plyr::dlply(df_type, "type_complete", function(df) {
  MASS::glm.nb(followers ~ I(tr_period==1)*I(china==1), data = df)})

GetSummary = function(models_list, parameter, 
                      conf_int = TRUE, model_name=NULL) {
  group = names(models_list)
  mean = sapply(models_list, function(model) {
    exp(coef(model)[parameter])})
  summary = data.frame(
    group = group,
    mean = mean,
    row.names = NULL)
  if (conf_int) {
    ci_lower = sapply(models_list, function(model) {
      exp(confint(model)[parameter, 1])})
    ci_upper = sapply(models_list, function(model) {
      exp(confint(model)[parameter, 2])})
    summary$ci_lower = ci_lower
    summary$ci_upper = ci_upper
  }
  if (!is.null(model_name)) {
    summary$model_name = model_name
  }
  return(summary)
}

summary = GetSummary(models_list=models_nb, 
                     parameter="I(tr_period == 1)TRUE:I(china == 1)TRUE",
                     model_name="Negative Binomial") %>%
  mutate(group = fct_relevel(group,
                             "International News Agencies",
                             "Citizen Journalists / Political Bloggers",
                             "Activists or US / Taiwan / Hong Kong Politics",
                             "Pornography Accounts",
                             "State Media or Chinese Officials",
                             "Non-Political Bloggers or Entertainment Accounts"))

levels(summary$group) = c("International\nNews Agencies",
                          "Citizen Journalists /\nPolitical Bloggers",
                          "Activists or US / Taiwan /\nHong Kong Politics",
                          "Pornography\nAccounts",
                          "State Media or\nChinese Officials",
                          "Non-Political Bloggers or\nEntertainment Accounts")

p = ggplot(summary,
           aes(x = group, y = mean, ymin = ci_lower, ymax = ci_upper)) +
  geom_pointrange(fatten = 1.5, size = .75) +
  geom_hline(aes(yintercept = 1), linetype="dashed") +
  #labs(title="Relative Size of New Followers, China / Hong Kong") + 
  xlab("") +
  ylab("Mean and 95% Confidence Interval") + 
  coord_flip() + theme_minimal() + 
  scale_x_discrete(limits = rev(levels(summary$group))) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=11, color="black")) +
  scale_y_continuous(breaks=c(0.75, 1, 1.25, 1.5, 1.75), 
                     labels=c("0.75x", "1x", "1.25x", "1.5x", "1.75x"))


pdf(here("twitter/figs/Fig6_summary_china_vs_hk_zh.pdf"), width = 6, height = 4)
gridExtra::grid.arrange(grid::textGrob("Relative Size of New Followers, China / Hong Kong", 
                                       gp = grid::gpar(fontsize = 14)), 
                        p, heights = c(0.1, 1))
dev.off()


### Fig A9

models_list = sapply(c("Hong Kong", "Taiwan", "US"),
                     simplify = FALSE, USE.NAMES = TRUE, 
                     function(x) {
                       plyr::dlply(df_type_list[[x]], "type_complete", 
                                   function(df) {
                                     MASS::glm.nb(followers ~ I(tr_period==1)*I(china==1), data = df)
                                   })})


summary_list = sapply(c("Hong Kong", "Taiwan", "US"),
                      simplify = FALSE, USE.NAMES = TRUE, 
                      function(x) {
                        summary = GetSummary(models_list=models_list[[x]], 
                                             parameter="I(tr_period == 1)TRUE:I(china == 1)TRUE",
                                             model_name="Negative Binomial") %>%
                          mutate(group = fct_relevel(group,
                                                     "International News Agencies",
                                                     "Citizen Journalists / Political Bloggers",
                                                     "Activists or US / Taiwan / Hong Kong Politics",
                                                     "Pornography Accounts",
                                                     "State Media or Chinese Officials",
                                                     "Non-Political Bloggers or Entertainment Accounts"))
                        levels(summary$group) = c("International\nNews Agencies",
                                                  "Citizen Journalists /\nPolitical Bloggers",
                                                  "Activists or US / Taiwan /\nHong Kong Politics",
                                                  "Pornography\nAccounts",
                                                  "State Media or\nChinese Officials",
                                                  "Non-Political Bloggers or\nEntertainment Accounts")
                        return(summary)
                      })


summary = bind_rows(summary_list, .id = 'country')
summary$country = as.factor(summary$country)
levels(summary$country) = c("Hong Kong", "Taiwan", "US")

p = ggplot(summary,
           aes(x = group, y = mean, ymin = ci_lower, ymax = ci_upper,
               color = country, shape = country)) +
  geom_pointrange(fatten = 1.5, size = .75, 
                  position = position_dodge(width = -1/2)) +
  geom_hline(aes(yintercept = 1), linetype="dashed") +
  scale_colour_manual(name = "Control Group",
                      values = c("#e41a1c", "#4daf4a", "#377eb8")) +
  scale_shape_discrete(name = "Control Group") +
  xlab("") +
  ylab("Mean and 95% Confidence Interval") + 
  coord_flip() + theme_minimal() + 
  scale_x_discrete(limits = rev(levels(summary$group))) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=11, color="black"),
        legend.text = element_text(size=10, color="black")) +
  scale_y_continuous(breaks=c(0.75, 1, 1.25, 1.5, 1.75), 
                     labels=c("0.75x", "1x", "1.25x", "1.5x", "1.75x"))


pdf(here("twitter/figs/FigA9_summary_china_vs_others_zh.pdf"), width = 6, height = 5)
gridExtra::grid.arrange(grid::textGrob("Relative Size of New Followers, China / Control Group", 
                                       gp = grid::gpar(fontsize = 14)), 
                        p, heights = c(0.1, 1))
dev.off()



