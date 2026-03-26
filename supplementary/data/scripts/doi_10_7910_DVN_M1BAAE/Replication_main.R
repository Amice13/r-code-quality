library(tidyverse)

# Figure 1
df_city <- read.csv("town2023.csv", fileEncoding = "CP932")

pair_mapping_df <- tribble(
  ~Name, ~Pair,
  "山手町", 1, "西芦屋町", 1,
  "岩園町", 2, "平田町", 2,
  "三条南町", 3, "南浜町", 3,
  "陽光町", 4, "朝日ヶ丘町", 4,
  "前田町", 5, "海洋町", 5,
  "松浜町", 6, "翠ヶ丘町", 6,
  "浜芦屋町", 7, "親王塚町", 7,
  "楠町", 8, "浜町", 8,
  "川西町", 9, "打出小槌町", 9,
  "西山町", 10, "奥池南町", 10,
  "南宮町", 11, "春日町", 11,
  "大原町", 12, "大東町", 12,
  "呉川町", 13, "大桝町", 13,
  "茶屋之町", 14, "潮見町", 14,
  "緑町", 15, "浜風町", 15,
  "平田北町", 16, "公光町", 16,
  "打出町", 17, "船戸町", 17,
  "東芦屋町", 18, "若葉町", 18,
  "精道町", 19, "松ノ内町", 19,
  "竹園町", 20, "伊勢町", 20,
  "宮塚町", 21, "若宮町", 21,
  "津知町", 22, "月若町", 22,
  "西蔵町", 23, "新浜町", 23,
  "奥山町", 24, "清水町", 24,
  "三条町", 25, "業平町", 25
)

treat_0_names <- c(
  "山手町", "岩園町", "三条南町", "陽光町", "前田町", "松浜町",
  "浜芦屋町", "楠町", "川西町", "西山町", "南宮町", "大原町",
  "呉川町", "茶屋之町", "緑町", "平田北町", "打出町", "東芦屋町",
  "精道町", "竹園町", "宮塚町", "津知町", "西蔵町", "奥山町",
  "三条町"
)

treat_1_names <- c(
  "西芦屋町", "平田町", "南浜町", "朝日ヶ丘町", "海洋町", "翠ヶ丘町",
  "親王塚町", "浜町", "打出小槌町", "奥池南町", "春日町", "大東町",
  "大桝町", "潮見町", "浜風町", "公光町", "船戸町", "若葉町",
  "松ノ内町", "伊勢町", "若宮町", "月若町", "新浜町", "清水町",
  "業平町"
)

district_mapping_df <- tribble(
  ~Name, ~District,
  "奥池町", 1, "奥池南町", 1,
  "朝日ヶ丘町", 2, "剣谷町", 2,
  "六麓荘町", 3, "岩園町", 3, "東山町", 3,
  "奥山町", 4, "山手町", 4, "東芦屋町", 4,
  "山芦屋町", 5, "西山町", 5, "三条町", 5,
  "翠ヶ丘町", 6, "親王塚町", 6,
  "大原町", 7, "船戸町", 7,
  "松ノ内町", 8, "月若町", 8, "西芦屋町", 8,
  "三条南町", 8, "業平町", 8, "前田町", 8,
  "清水町", 8,
  "春日町", 9, "打出町", 9,
  "楠町", 10, "打出小槌町", 10,
  "上宮川町", 11, "宮塚町", 11, "若宮町", 11, "宮川町", 11,
  "茶屋之町", 12, "大桝町", 12, "公光町", 12, "精道町", 12,
  "川西町", 13, "津知町", 13, "平田北町", 13,
  "南宮町", 14, "大東町", 14,
  "浜町", 16, "西蔵町", 16,
  "竹園町", 17, "呉川町", 17, "伊勢町", 17,
  "浜芦屋町", 18, "松浜町", 18, "平田町", 18,
  "新浜町", 19, "浜風町", 19, "高浜町", 19,
  "若葉町", 20, "緑町", 20, "潮見町", 20,
  "陽光町", 21, "海洋町", 21, "南浜町", 21, "涼風町", 21
)

townlevel_df_23city <-
  df_city |>
  group_by(Name) |> 
  summarize(
    N = sum(Electrate_n),
    Early = sum(Early),
    Day = sum(Day),
    Abs = sum(Absentee),
    .groups = "drop" 
  ) |>
  as_tibble() |>
  mutate(
    Vote_2023city = ((Early + Day) / N) * 100,
  ) |>
  left_join(pair_mapping_df, by = "Name") |>
  mutate(
    Treat = case_when(
      Name %in% treat_0_names ~ 0,
      Name %in% treat_1_names ~ 1,
      TRUE ~ NA_real_ 
    )
  ) |>
  left_join(district_mapping_df, by = "Name") |>
  mutate(Election = "City")

paired_df <- townlevel_df_23city |>
  filter(!is.na(Pair), !is.na(Treat))
J <- nrow(paired_df)/2

sim_df <- tibble(rand = numeric(2000))

for (i in 1:2000) {
  dif_vec <- numeric(25)  
  
  for (j in 1:25) {
    treat <- paired_df |> 
      filter(Treat == 1) |> 
      sample_n(size = 1, replace = TRUE)
    
    placebo <- paired_df |> 
      filter(Treat == 0) |> 
      sample_n(size = 1, replace = TRUE)
    
    dif <- treat$Vote_2023city - placebo$Vote_2023city
    dif_vec[j] <- dif
  }
  
  sim_df$rand[i] <- mean(dif_vec)
}

for (i in 1:2000) {
  dif_vec <- numeric(25)  
  
  for (j in 1:25) {
    group <- sample(1:25, 1, replace = T)
    treat <- paired_df |> 
      filter(Pair == group & Treat == 1) |> 
      sample_n(size = 1, replace = TRUE)
    
    placebo <- paired_df |> 
      filter(Pair == group & Treat == 0) |> 
      sample_n(size = 1, replace = TRUE)
    
    dif <- treat$Vote_2023city - placebo$Vote_2023city
    dif_vec[j] <- dif
  }
  
  sim_df$pair[i] <- mean(dif_vec)
}

result_townlevel_df <- tibble(
  tau = c(mean(sim_df$rand), mean(sim_df$pair)),
  se = c(sd(sim_df$rand), sd(sim_df$pair)),
  label = c("Not considering matched pairs", "Considering matched pairs")
)

Fig1 <-
  result_townlevel_df |>
  ggplot(aes(x = label)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_segment(aes(x = label, xend = label, y = tau - 1.96 * se, yend = tau + 1.96 * se),
               size = 1.25) +
  geom_segment(aes(x = label, xend = label, y = tau - 2.58 * se, yend = tau + 2.58 * se),
               size = 0.35) +
  geom_point(aes(y = tau), size = 3) +
  coord_flip() +
  labs(x = "", y = "Average treatment effect of election closeness on turnout rate") +
  theme_classic()

## To save and output the Figure 1
## ggsave(plot=Fig1, filename = "Figures/Fig1.jpeg", dpi = 1200, height = 2.8, width = 6.5)

# Table 1
library(lme4)
library(sjPlot)

df_individual <- read.csv("individual2023.csv", fileEncoding = "CP932") |>
  filter(Treat != "Control") |>
  mutate(Treat = if_else(Treat == "Placebo", 0, 1)
  )

model1 <- lmer(Vote_city ~ Treat +  Sex + Age + Vote_pref + (1 | Town), data = df_individual)
summary(fit1)

model2 <- lmer(Vote_city ~ Treat*Vote_pref +  Sex + Age + Vote_pref + (1 | Town), data = df_individual)
summary(fit2)

## To save and output the Table 1
## tab_model(model1, model2,
## file = "model_output.tex",
## digits = 3,
## dv.labels = c("Model 1 (without interaction)", " Model 2 (with interaction)"),
## show.intercept = F,
## show.se = T,
## show.ci = F,
## show.re.var = F,
## show.icc = F, 
## show.r2 = F,
## show.ngroups = F)

# Figure 2
df_survey <- read.csv("survey.csv", fileEncoding = "CP932") |>
  mutate(Group = case_when(Treat == "Iv" | Treat == "Cr" ~ 0,
                           Treat == "Mo" | Treat == "Wa" ~ 1,
                           TRUE ~ 2))
Recognition <-
  df_survey |> 
  mutate(Option = if_else(Recog == 1, "Remember", "Not remember/NA")) |>
  dplyr::select(Group, Option) |>
  group_by(Group, Option) |>
  summarize(n = n()) |>
  mutate(N = case_when(Group == 0 ~ 297, Group == 1 ~ 311, TRUE ~ 94),
         Per = n/N,
         SE = sqrt((Per * (1-Per)/N)),
         Group = case_when(Group == 0 ~ "Placebo", Group == 1 ~ "Treatment", TRUE ~ "Control"),
         Group = factor(Group, levels = c("Treatment","Placebo","Control")),
         Option = factor(Option, levels = c("Remember","Not remember/NA")))

Fig2 <-
  Recognition |>
  ggplot(aes(x = Option, y = Per*100)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  facet_wrap(~Group) +
  labs(x = "Degree of the postcard recognition", y = "%") +
  geom_text(aes(label = round(Per*100,1),x = Option, y = Per*100+5)) +
  theme_classic()

## To save and output the Figure 2
## ggsave(plot=Fig2, filename = "Figures/Fig2.jpeg", dpi = 900, width = 7, height = 3.2)