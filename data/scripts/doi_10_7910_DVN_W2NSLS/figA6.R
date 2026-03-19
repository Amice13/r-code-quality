############################################################################################
#### Fig A6: Increases in Twitter Followers from mainland China versus Hong Kong by Week
#### data: twitter/data/fig6_follower_china_hk_weekly_placebo.Rds
#### outputs: twitter/figs/FigA6_placebo_china_vs_hk_zh.pdf
############################################################################################

library("tidyverse"); library("RColorBrewer"); library("data.table"); library("here")

df_type_list = readRDS(here("twitter/data/fig6_follower_china_hk_weekly_placebo.Rds"))

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
# weeks = as.character(c(-3:9))
# 
# df_type_list = sapply(weeks, 
#                       simplify = FALSE, USE.NAMES = TRUE, 
#                       function(x) {
#                         start_date = as.Date("2020-01-23") + 7*as.numeric(x)
#                         end_date = as.Date("2020-01-29") + 7*as.numeric(x)
#                         df_by_day %>%
#                           filter(follow_date >= "2019-12-01" & follow_date < "2020-04-01") %>%
#                           filter(zh == 1) %>%
#                           filter(location_classified!="Unknown") %>%
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
#                           filter(location_classified=="Hong Kong" | location_classified=="China") %>%
#                           mutate(co_period=ifelse(follow_date>="2019-12-01" & follow_date<="2019-12-31", 1, 0),
#                                  tr_period=ifelse(follow_date>=start_date & follow_date<=end_date, 1, 0),
#                                  china=ifelse(location_classified=="China", 1, 0)) %>%
#                           filter(tr_period==1 | co_period==1)
#                       })
# 
# saveRDS(df_type_list, here("twitter/data/fig6_follower_china_hk_weekly_placebo.Rds"))

weeks = as.character(c(-3:9))

models_list = sapply(weeks,
                     simplify = FALSE, USE.NAMES = TRUE, 
                     function(x) {
                       plyr::dlply(df_type_list[[x]], "type_complete", 
                                   function(df) {
                                     MASS::glm.nb(followers ~ I(tr_period==1)*I(china==1), data = df)
                                   })})

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


summary_list = sapply(weeks,
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
                        levels(summary$group) = c("International News Agencies",
                                                  "Citizen Journalists Political Bloggers",
                                                  "Activists or US / TW / HK Politics",
                                                  "Pornography Accounts",
                                                  "State Media or Chinese Officials",
                                                  "Non-Polical Bloggers or Entertainments")
                        return(summary)
                      })


summary = bind_rows(summary_list, .id = 'week')
summary$week = as.factor(summary$week)
levels(summary$week) = as.character(c(-3:9))

p = ggplot(summary,
           aes(x = week, y = mean, ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(aes(yintercept = 1), linetype="dashed") +
  geom_vline(aes(xintercept = "0"), linetype="dashed", color="red") +
  geom_pointrange(fatten = 1.5, size = .75) +
  scale_color_discrete(name = "Twitter Account Type") +
  scale_shape_discrete(name = "Twitter Account Type") +
  #labs(title="Relative Size of New Followers, China / Hong Kong") + 
  xlab("Weeks into Lockdown") +
  ylab("Mean and 95% Confidence Interval") + 
  facet_wrap(~group, ncol=3, scales = "free_x") +
  theme_bw() + 
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(size=10, color="black"),
        axis.text.y = element_text(size=10, color="black"),
        legend.text = element_text(size=10, color="black"),
        strip.text = element_text(size=12, color="black")) +
  scale_y_continuous(breaks=c(0.5, 1, 1.5), 
                     labels=c("0.5x", "1x", "1.5x")) +
  coord_cartesian(ylim=c(0.5, 2))


pdf(here("twitter/figs/FigA6_placebo_china_vs_hk_zh.pdf"), width = 10, height = 6)
gridExtra::grid.arrange(grid::textGrob("Relative Size of New Followers by Week, China / Hong Kong", 
                                       gp = grid::gpar(fontsize = 16)), 
                        p, heights = c(0.1, 1))
dev.off()


