library(tidyverse)
library(ggthemes)

my_df <- as_tibble(list(est = c(3.09394, 3.662697, 3.19605, 3.919497, 3.453157, 3.735151, 3.79633, 3.651598, 3.530695, 3.556992, 3.782804, 3.519695),
                        lb = c(2.915402, 3.565984, 2.931648, 3.759452, 3.350838, 3.551991, 3.636964, 3.388332, 3.439758, 3.291307, 3.628776, 3.245771),
                        ub = c(3.272478, 3.75941, 3.460453, 4.079542, 3.555475, 3.91831, 3.955697, 3.914863, 3.621632, 3.822677, 3.936832, 3.793619),
                        group = rep(c(rep("No Kids", 2), rep("Has Kids", 2)), 3),
                        ev = c("LGB Contact", "LGB Contact", "LGB Contact", "LGB Contact", "Transgender Contact", "Transgender Contact","Transgender Contact","Transgender Contact","Knowing Parents of Transgender Kids", "Knowing Parents of Transgender Kids","Knowing Parents of Transgender Kids","Knowing Parents of Transgender Kids"),
                        cond = rep(c("No", "Yes"),6)))

my_df <- my_df %>%
  mutate(cond = factor(cond, levels = c("No", "Yes")),
         ev = factor(ev, levels = c("LGB Contact", "Transgender Contact", "Knowing Parents of Transgender Kids")),
         group = factor(group, levels = c("No Kids","Has Kids")))

p <- ggplot(my_df, aes(x = cond, y=est, ymin = lb, ymax = ub, color = group, fill = group))

p + geom_pointrange(position = position_dodge(width = 0.5), size = 1.5) +
  theme_clean() +
  scale_color_manual(values = c("black","gray")) + 
  theme(legend.background = element_blank()) +
  labs(x = NULL,
       y = "Religious Refusals",
       fill = NULL,
       color = NULL) +
  facet_wrap(~ev, ncol = 3)

my_df <- as_tibble(list(est = c(0.4717188, 0.6065317, 0.4067615, 0.6331207, 0.5602578, 0.6224448, 0.5504941, 0.6499182, 0.5625836, 0.6990293, 0.5951078, 0.4608292,0.1714499, 0.1811204, 0.279257, 0.1537444, 0.1873674, 0.1635914, 0.1692538, 0.1999585, 0.1828916, 0.1549541, 0.1837994, 0.1365968, 0.3568313, 0.212348, 0.3139815, 0.2131349, 0.2523749, 0.2139638, 0.2802521, 0.1501232, 0.2545248, 0.1460166, 0.2210928, 0.402574),
                        lb = c(0.3961485, 0.5672642, 0.2869453, 0.5657313, 0.5159054, 0.549478, 0.4804354, 0.543789, 0.5241892, 0.5944021, 0.527811, 0.3055442, 0.1223943, 0.1502414, 0.1665462, 0.110704, 0.15436, 0.1076484, 0.1196703, 0.1279291, 0.1544793, 0.0726015, 0.137583, 0.0540747, 0.2851184, 0.1797142, 0.1975108, 0.1500154, 0.2113257, 0.1514765, 0.2155572, 0.0496098, 0.220332, 0.0683032, 0.1572297, 0.228373),
                        ub = c(0.5472891,0.6457991,0.5265776,0.7005101,0.6046101,0.6954148,0.6205528,0.7560474,0.600978,0.8036565,0.6624046,0.6161142,0.2205055,0.2119993,0.3919678,0.1967848,0.2203748,0.2195344,0.2188374,0.271988,0.211304,0.2373066,0.2300157,0.2191188,0.4285443,0.2449817,0.4304522,0.2762544,0.293424,0.2764511,0.3449469,0.2506367,0.2887175,0.22373,0.2849559,0.5767751),
                        group = rep(c(rep("No Kids", 2), rep("Has Kids", 2)), 9),
                        ev = rep(c("LGB Contact", "LGB Contact", "LGB Contact", "LGB Contact", "Transgender Contact", "Transgender Contact","Transgender Contact","Transgender Contact","Knowing Parents of Transgender Kids", "Knowing Parents of Transgender Kids","Knowing Parents of Transgender Kids","Knowing Parents of Transgender Kids"),3),
                        cond = rep(c("No", "Yes"),18),
                        dv = c(rep("Illegal", 12),rep("Legal", 12), rep("Don't Know", 12))))

my_df <- my_df %>%
  mutate(cond = factor(cond, levels = c("No", "Yes")),
         ev = factor(ev, levels = c("LGB Contact", "Transgender Contact", "Knowing Parents of Transgender Kids")),
         group = factor(group, levels = c("No Kids","Has Kids")),
         dv = factor(dv, levels = c("Illegal", "Legal", "Don't Know")))

p <- ggplot(my_df, aes(x = cond, y=est, ymin = lb, ymax = ub, color = group, fill = group))

p + geom_pointrange(position = position_dodge(width = 0.5), size = 0.5) +
  theme_clean() +
  scale_color_manual(values = c("black","gray")) + 
  theme(legend.background = element_blank()) +
  labs(x = NULL,
       y = "Probability",
       fill = NULL,
       color = NULL) +
  facet_wrap(dv~ev, ncol = 3)

#si figures
my_df <- as_tibble(list(est = c(3.053047, 3.665315, 3.236866, 3.860894, 
                                3.446842, 3.743522, 3.797473, 3.626084, 
                                3.522367, 3.608143, 3.781777, 3.392027),
                        lb = c(2.875892, 3.568321, 2.969368, 3.69985,
                               3.345583, 3.564644, 3.637083, 3.385845, 
                               3.43183, 3.291307, 3.622949, 3.143068),
                        ub = c(3.230201, 3.762309, 3.504363, 4.021938,
                               3.548101, 3.9224, 3.957862, 3.866324, 
                               3.612904, 3.869597, 3.940606, 3.640985),
                        group = rep(c(rep("No Kids", 2), rep("Has Kids", 2)), 3),
                        ev = c("LGB Contact", "LGB Contact", "LGB Contact", "LGB Contact", "Transgender Contact", "Transgender Contact","Transgender Contact","Transgender Contact","Parents of Transgender Kids", "Parents of Transgender Kids","Parents of Transgender Kids","Parents of Transgender Kids"),
                        cond = rep(c("No", "Yes"),6)))

my_df <- my_df %>%
  mutate(cond = factor(cond, levels = c("No", "Yes")),
         ev = factor(ev, levels = c("LGB Contact", "Transgender Contact", "Parents of Transgender Kids")),
         group = factor(group, levels = c("No Kids","Has Kids")))

p <- ggplot(my_df, aes(x = cond, y=est, ymin = lb, ymax = ub, color = group, fill = group))

p + geom_pointrange(position = position_dodge(width = 0.5), size = 1.5) +
  theme_clean() +
  scale_color_manual(values = c("black","gray")) + 
  theme(legend.background = element_blank()) +
  labs(x = NULL,
       y = "Religious Refusals",
       fill = NULL,
       color = NULL) +
  facet_wrap(~ev, ncol = 3)

my_df <- as_tibble(list(est = c(0.4545324, 0.5883314, 0.3921291, 0.604321, 
                                0.5371783, 0.6096668, 0.5394119, 0.6099023,
                                0.5471788, 0.6703983, 0.5748252, 0.4792656,
                                0.1767524, 0.1849308, 0.2687807, 0.1623644, 
                                0.1862279, 0.1728276, 0.1791784, 0.1906287, 
                                0.1857354, 0.1593456, 0.186204, 0.1486263,
                                0.3687152, 0.2267377, 0.3390902, 0.2333146,
                                0.2765938, 0.2175056, 0.2814097, 0.199469,
                                0.2670859, 0.1702561, 0.2389708, 0.3721081),
                        lb = c(0.384554, 0.5501364, 0.2788492, 0.5406897, 
                               0.4966196, 0.539108, 0.4734752, 0.5087107, 
                               0.5113447, 0.567818, 0.514828, 0.3482983, 
                               0.1287144, 0.1532455, 0.1637106, 0.1215692, 
                               0.1537475, 0.1166376, 0.1300751, 0.1251051, 
                               0.1570318, 0.079214, 0.1428569, 0.0667844,
                               0.3027539, 0.1928946, 0.2249214, 0.1722268,
                               0.2383736, 0.1548101, 0.2189853, 0.0966802,
                               0.2351681, 0.0858174, 0.1817888, 0.2276823),
                        ub = c(0.5245093,0.6265265,0.5054091, 0.6679524, 
                               0.577737,0.6802256,0.6053485,0.7110938,
                               0.5830128,0.7729785,0.6348225,0.6102329,
                               0.2247904,0.21166162,0.3738508,0.2031596,
                               0.2187084,0.2290176,0.2282818,0.2561523,
                               0.214439,0.2394772,0.2295511,0.2304683,
                               0.4346766,0.2605808,0.4532589,0.2944023,
                               0.3148139,0.2802012,0.3438341,0.3022579,
                               0.2990036,0.2546949,0.2961528,0.5165339),
                        group = rep(c(rep("No Kids", 2), rep("Has Kids", 2)), 9),
                        ev = rep(c("LGB Contact", "LGB Contact", "LGB Contact", "LGB Contact", "Transgender Contact", "Transgender Contact","Transgender Contact","Transgender Contact","Parents of Transgender Kids", "Parents of Transgender Kids","Parents of Transgender Kids","Parents of Transgender Kids"),3),
                        cond = rep(c("No", "Yes"),18),
                        dv = c(rep("Illegal", 12),rep("Legal", 12), rep("Don't Know", 12))))

my_df <- my_df %>%
  mutate(cond = factor(cond, levels = c("No", "Yes")),
         ev = factor(ev, levels = c("LGB Contact", "Transgender Contact", "Parents of Transgender Kids")),
         group = factor(group, levels = c("No Kids","Has Kids")),
         dv = factor(dv, levels = c("Illegal", "Legal", "Don't Know")))

p <- ggplot(my_df, aes(x = cond, y=est, ymin = lb, ymax = ub, color = group, fill = group))

p + geom_pointrange(position = position_dodge(width = 0.5), size = 0.5) +
  theme_clean() +
  scale_color_manual(values = c("black","gray")) + 
  theme(legend.background = element_blank()) +
  labs(x = NULL,
       y = "Probability",
       fill = NULL,
       color = NULL) +
  facet_wrap(dv~ev, ncol = 3)
