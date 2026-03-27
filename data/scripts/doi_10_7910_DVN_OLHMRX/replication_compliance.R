library(tidyverse)
library(patchwork)

full <- read_csv("trust_compliance.csv")
JP <- full %>% 
  filter(country == "JP")

UK <- full %>% 
  filter(country == "UK")

# Fig 1 & 2
mask_JP <- JP %>% 
  group_by(wave, com_mask) %>% 
  tally()

wash_JP <- JP %>% 
  group_by(wave, com_wash) %>% 
  tally()


mask_UK <- UK %>% 
  group_by(wave, com_mask) %>% 
  tally()


wash_UK <- UK %>% 
  group_by(wave, com_wash) %>% 
  tally()

a <- wash_JP %>% 
  drop_na() %>% 
  mutate(mask = factor(com_wash, labels = c("Never","Almost never","Sometimes","Most of the time","All the time"))) %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot() +
  aes(x = wave, y = n, fill = mask) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Japan: Thinking about the **past week**, how frequently did you do the following?", 
       x = "",
       y = "", 
       fill = "Washed hands for 20 seconds or longer") +
  guides(fill = FALSE) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

b <- wash_UK %>% 
  drop_na() %>% 
  mutate(mask = factor(com_wash, labels = c("Never","Almost never","Sometimes","Most of the time","All the time"))) %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot() +
  aes(x = wave, y = n, fill = mask) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "UK: Thinking about the **past week**, how frequently did you do the following?", 
       x = "",
       y = "", 
       fill = "Washed hands for 20 seconds or longer") +
  theme(legend.position = "bottom")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

fig1 <- a + b +
  plot_layout(ncol =1)

ggsave("fig1.jpg", dpi = 300)

c <- mask_JP %>% 
  drop_na() %>% 
  mutate(mask = factor(com_mask, labels = c("Never","Almost never","Sometimes","Most of the time","All the time"))) %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot() +
  aes(x = wave, y = n, fill = mask) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Japan: Thinking about the **past week**, how frequently did you do the following?", 
       x = "",
       y = "", 
       fill = "Wore a mask while inside public transportation") +
  guides(fill = FALSE) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

d <- mask_UK %>% 
  drop_na() %>% 
  mutate(mask = factor(com_mask, labels = c("Never","Almost never","Sometimes","Most of the time","All the time"))) %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot() +
  aes(x = wave, y = n, fill = mask) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "UK: Thinking about the **past week**, how frequently did you do the following?", 
       x = "",
       y = "", 
       fill = "Wore a mask while inside public transportation") +
  theme(legend.position = "bottom")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

fig2 <- c + d +
  plot_layout(ncol =1)

ggsave("fig2.jpg", dpi = 300)

# Figure 3
trust <- full %>% 
  group_by(country, wave) %>% 
  summarise(Westminster = mean(trust_core_1_1, na.rm = TRUE),
            PM = mean(trust_core_1_2, na.rm = TRUE),
            Parliament = mean(trust_core_1_3, na.rm = TRUE),
            Local_MP = mean(trust_core_1_4, na.rm = TRUE),
            Med_system = mean(trust_core_1_5, na.rm = TRUE),
            Police = mean(trust_core_1_6, na.rm = TRUE),
            Courts = mean(trust_core_1_7, na.rm = TRUE),
            Trad_media = mean(trust_core_1_8, na.rm = TRUE),
            SNS_news = mean(trust_core_1_9, na.rm = TRUE),
            Gen_public = mean(trust_core_2, na.rm = TRUE))

fig3 <- trust %>% 
  pivot_longer(c(5,7,12), names_to = "trust", values_to = "value") %>% 
  ggplot() +
  geom_line(aes(x = wave, y = value, color = trust)) +
  geom_point(aes(x = wave, y = value, shape = trust)) +
  facet_grid(~country)+
  theme(legend.position = "bottom")

ggsave("fig3.jpg", dpi = 300)

# Base Models
Nested_data_JP <- JP %>% 
  group_nest(wave)
Nested_data_UK <- UK %>% 
  group_nest(wave)


# Figure 4
Nested_JP_1_M <- Nested_data_JP %>% 
  mutate(lm_test = map(data, ~lm(com_mask ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test2 = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test2) %>% 
  unnest(cols = lm_test2) %>% 
  filter(term == c("threat"))  %>% 
  mutate(country = "JP")

Nested_JP_1_W <- Nested_data_JP %>% 
  mutate(lm_test = map(data, ~lm(com_wash ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test2 = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test2) %>% 
  unnest(cols = lm_test2) %>% 
  filter(term == c("threat"))  %>% 
  mutate(country = "JP")

Nested_UK_1_M <- Nested_data_UK %>% 
  mutate(lm_test = map(data, ~lm(com_mask ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test2 = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test2) %>% 
  unnest(cols = lm_test2) %>% 
  filter(term == c("threat"))  %>% 
  mutate(country = "UK")

Nested_UK_1_W <- Nested_data_UK %>% 
  mutate(lm_test = map(data, ~lm(com_wash ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test2 = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test2) %>% 
  unnest(cols = lm_test2) %>% 
  filter(term == c("threat"))  %>% 
  mutate(country = "UK")

Threat_M <- bind_rows(Nested_JP_1_M, Nested_UK_1_M)

e <- Threat_M %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot(aes(x = wave, y = estimate, color = country, shape = country)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = guide_legend(title = NULL), color = FALSE) +
  scale_color_discrete(labels = c("Japan","UK")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "Mask wearing")

Threat_W <- bind_rows(Nested_JP_1_W, Nested_UK_1_W)

f <- Threat_W %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot(aes(x = wave, y = estimate, color = country, shape = country)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = guide_legend(title = NULL), color= FALSE) +
  scale_color_discrete(labels = c("Japan","UK")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "Wash hands")

fig4 <- e + f +
  plot_layout(ncol =1)

ggsave("fig4.jpg", dpi = 300)




# Figure 5 Wash hands
Nested_JP_2_W <- Nested_data_JP %>% 
  mutate(lm_test = map(data, ~lm(com_wash ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test) %>% 
  unnest(cols = lm_test) %>% 
  filter(term == c("trust_core_1_5","trust_core_2","trust_core_1_3"))  

g <- Nested_JP_2_W %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot(aes(x = wave, y = estimate, color = term, shape = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = FALSE, color = FALSE) +
  scale_shape_discrete(labels = c("Parliament","Medical System","General Public")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "Japan / Wash hands")+
  theme(legend.position = "bottom")


Nested_UK_2_W <- Nested_data_UK %>% 
  mutate(lm_test = map(data, ~lm(com_wash ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test) %>% 
  unnest(cols = lm_test) %>% 
  filter(term == c("trust_core_1_5","trust_core_2","trust_core_1_3"))  

h <- Nested_UK_2_W %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot(aes(x = wave, y = estimate, color = term, shape = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = guide_legend(title = NULL), color = FALSE) +
  scale_shape_discrete(labels = c("Parliament","Medical System","General Public")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "UK / Wash hands")+
  theme(legend.position = "bottom")

fig5 <- g + h +
  plot_layout(ncol = 1)

ggsave("fig5.jpg", dpi = 300)

## Figure 6 Wear a Mask
Nested_JP_2_M <- Nested_data_JP %>% 
  mutate(lm_test = map(data, ~lm(com_mask ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test) %>% 
  unnest(cols = lm_test) %>% 
  filter(term == c("trust_core_1_5","trust_core_2","trust_core_1_3"))  

i <- Nested_JP_2_M %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot(aes(x = wave, y = estimate, color = term, shape = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = FALSE, color = FALSE) +
  scale_shape_discrete(labels = c("Parliament","Medical System","General Public")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "Japan / Mask wearing")+
  theme(legend.position = "bottom")


Nested_UK_2_M <- Nested_data_UK %>% 
  mutate(lm_test = map(data, ~lm(com_mask ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test) %>% 
  unnest(cols = lm_test) %>% 
  filter(term == c("trust_core_1_5","trust_core_2","trust_core_1_3"))  

j <- Nested_UK_2_M %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>%   
  ggplot(aes(x = wave, y = estimate, color = term, shape = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = guide_legend(title = NULL), color = FALSE) +
  scale_shape_discrete(labels = c("Parliament","Medical System","General Public")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "UK / Mask wearing")+
  theme(legend.position = "bottom")

fig6 <- i + j +
  plot_layout(ncol = 1)

ggsave("fig6.jpg", dpi = 300)

## social distance
Nested_JP_2_W <- Nested_data_JP %>% 
  mutate(lm_test = map(data, ~lm(com_sd ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test) %>% 
  unnest(cols = lm_test) %>% 
  filter(term == c("trust_core_1_5","trust_core_2","trust_core_1_3"))  

g <- Nested_JP_2_W %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot(aes(x = wave, y = estimate, color = term, shape = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = FALSE, color = FALSE) +
  scale_shape_discrete(labels = c("Parliament","Medical System","General Public")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "Japan / Social Distance")+
  theme(legend.position = "bottom")


Nested_UK_2_W <- Nested_data_UK %>% 
  mutate(lm_test = map(data, ~lm(com_sd ~ trust_core_2 + trust_core_1_3 + trust_core_1_5 + vax_count + k6 + threat + age + women, data = .x)),
         lm_test = map(lm_test, broom::tidy, conf.int = TRUE)) %>% 
  dplyr::select(wave, lm_test) %>% 
  unnest(cols = lm_test) %>% 
  filter(term == c("trust_core_1_5","trust_core_2","trust_core_1_3"))  

h <- Nested_UK_2_W %>% 
  mutate(wave = factor(wave, levels = 1:20, labels = c("2022Jul","2022Aug","2022Sep","2022Oct","2022Nov","2022Dec","2023Jan","2023Feb","2023Mar","2023Apr","2023May","2023Jun","2023Jul","2023Aug","2023Sep","2023Oct","2023Nov","2023Dec","2024Jan","2024Feb"))) %>% 
  ggplot(aes(x = wave, y = estimate, color = term, shape = term)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(width = 0.2)) +
  guides(shape = guide_legend(title = NULL), color = FALSE) +
  scale_shape_discrete(labels = c("Parliament","Medical System","General Public")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y ="", title = "UK / Social Distance")+
  theme(legend.position = "bottom")

g + h +
  plot_layout(ncol = 1)

