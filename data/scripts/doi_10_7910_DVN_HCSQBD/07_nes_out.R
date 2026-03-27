rm(list = ls())

library("ltm")
library("pryr")
library("dplyr")
library("tidyr")
library("splines")
library("ggplot2")
library("hIRT")
load("nes_out_pca.RData")
load("nes_out.RData")

start_time <- Sys.time()

colors <- rep("grey40", 6)

econ2_plot <- econ2_plot %>% lapply(as.vector) %>% tbl_df() %>%
  mutate(mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se)

civil2_plot <- civil2_plot %>% lapply(as.vector) %>% tbl_df() %>%
  mutate(mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se)

moral2_plot <- moral2_plot %>% lapply(as.vector) %>% tbl_df() %>%
  mutate(mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se)

foreign2_plot <- foreign2_plot %>% lapply(as.vector) %>% tbl_df() %>%
  mutate(mean_upper = mean_est + 1.96 * mean_se,
         mean_lower = mean_est - 1.96 * mean_se)

# table 1

write.csv(table1, file = "table1.csv", row.names = FALSE)

# figures 2-3

out1a <- rbind(cbind(econ_plot, `Issue Domain` = "Economics"),
             cbind(moral_plot, `Issue Domain` = "Morality"),
             cbind(civil_plot, `Issue Domain` = "Civil Rights"),
             cbind(foreign_plot, `Issue Domain` = "Foreign Policy")) %>%
  lapply(as.vector) %>% tbl_df() %>% dplyr::select(-party) %>%
  mutate(fitted_upper = fitted + 1.96*fitted_se, fitted_lower = fitted - 1.96*fitted_se) %>%
  mutate(`Issue Domain` = factor(`Issue Domain`, levels = c("Economics", "Civil Rights", "Morality", "Foreign Policy")),
         educ = factor(educ, levels = c("No College", "College"),
                       labels = c("High School or Less", "Some College or Above")))

out1b <- rbind(cbind(econ_pca1, `Issue Domain` = "Economics"),
              cbind(moral_pca1, `Issue Domain` = "Morality"),
              cbind(civil_pca1, `Issue Domain` = "Civil Rights"),
              cbind(foreign_pca1, `Issue Domain` = "Foreign Policy")) %>%
  lapply(as.vector) %>% tbl_df() %>% dplyr::select(-party) %>%
  mutate(`Issue Domain` = factor(`Issue Domain`, levels = c("Economics", "Civil Rights", "Morality", "Foreign Policy")),
         educ = factor(educ, levels = c("No College", "College"),
                       labels = c("High School or Less", "Some College or Above")))

ggplot(out1a, aes(x = year)) +
  geom_line(aes(y = fitted, linetype = `Party ID`, color = `Party ID`), size = 1.1) +
  geom_ribbon(aes(ymin = fitted_lower, ymax = fitted_upper, fill = `Party ID`), alpha=0.2) + 
  facet_grid(`Issue Domain`~educ, switch = "y", scales = "free_y") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(-1.7, 1.7)) +
  scale_x_continuous(limits = c(1972, 2016)) +
  labs(x = "Year", y = "Conservatism") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom") # 750*1000

ggsave("figure2.tiff", width = 7.5, height = 10)

ggplot(out1b, aes(x = year)) +
  geom_line(aes(y = fitted, linetype = `Party ID`, color = `Party ID`), size = 1.1) +
  geom_ribbon(aes(ymin = fitted_lower, ymax = fitted_upper, fill = `Party ID`), alpha=0.2) + 
  facet_grid(`Issue Domain`~educ, switch = "y", scales = "free_y") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(limits = c(-1.7, 1.7)) +
  scale_x_continuous(limits = c(1972, 2012)) +
  labs(x = "Year", y = "Conservatism") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom") # 750*1000

ggsave("figure3.tiff", width = 7.5, height = 10)
                       
# figure 4

out2a <- rbind(cbind(econ2_plot, `Issue Domain` = "Economics"),
              cbind(moral2_plot, `Issue Domain` = "Morality"),
              cbind(civil2_plot, `Issue Domain` = "Civil Rights"),
              cbind(foreign2_plot, `Issue Domain` = "Foreign Policy")) %>%
  lapply(as.vector) %>% tbl_df() %>% dplyr::select(-mean_se) %>%
  gather(measure, value, mean_est:var_lower) %>%
  separate(measure, c("measure", "stat")) %>%
  spread(stat, value) %>%
  mutate(measure = factor(measure, levels = c("mean", "var"), labels = c("Mean", "Variance")),
         `Issue Domain` = factor(`Issue Domain`, levels = c("Economics", "Civil Rights", "Morality", "Foreign Policy")))

ggplot(out2a, aes(x = year))+
  geom_line(aes(y = est, color = `Issue Domain`), size = 1.2) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = `Issue Domain`), alpha=0.2) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  facet_grid(measure~`Issue Domain`, scale = "free", switch = "y") +
  labs(x = "Year", y = "Conservatism") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom") # 1000*750

ggsave("figure4.tiff", width = 10, height = 7.5)

# figure 5

out3a <- nes_align_out %>% mutate_at(vars(econ3_score:moral3_score), funs(as.vector)) %>%
  group_by(year) %>%
  summarise(
  `Sample Size` = n(),
  econ_sigma = sd(econ3_score),
  civil_sigma = sd(civil3_score),
  moral_sigma = sd(moral3_score),
  econ.civil_rho = cor(econ3_score, civil3_score),
  econ.moral_rho = cor(econ3_score, moral3_score),
  civil.moral_rho = cor(civil3_score, moral3_score)) %>%
  gather(measure, value, econ_sigma:civil.moral_rho) %>%
  separate(measure, c("measure", "stat"), sep="_") %>%
  filter(stat=="rho") %>% dplyr::select(-stat) %>%
  mutate(measure = factor(measure, levels = c("econ.civil", "econ.moral", "civil.moral"),
                          labels = c("Economics - Civil Rights", "Economics - Morality", "Civil Rights - Morality")))

ggplot(out3a, aes(x = year, y = value)) +
  geom_point(aes(size = `Sample Size`), col = "grey40") +
  geom_line(col = "grey40", size = 1) +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010)) +
  labs(x = "Year", y = "Correlation Coefficient") +
  scale_y_continuous(limits = c(0, 0.7)) +
  facet_wrap( ~ measure, scale = "free_x")+
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom") # 1000*750

ggsave("figure5.tiff", width = 12, height = 9)

# figure A2 in Supplementary Material

outA2 <- list(hgrm_econ, hgrm_civil, hgrm_moral, hgrm_foreign) %>%
  lapply(coef_item) %>% unlist(recursive = FALSE) %>%
  sapply(function(x) x["Dscrmn", 1:2]) %>% t() %>% data.frame() %>%
  lapply(unlist) %>% data.frame() %>%
  mutate(item = factor(rownames(.), levels = rev(rownames(.)))) %>%
  `names<-`(c("estimate", "se", "item")) %>%
  mutate(`Issue Domain` = factor(c(rep("Economics", 15), rep("Civil Rights", 17),
                                   rep("Morality", 10), rep("Foreign Policy", 4)),
                                 levels = c("Economics", "Civil Rights", "Morality", "Foreign Policy"))) 


ggplot(outA2) +
  geom_point(aes(x = estimate, y = item, shape = `Issue Domain`, color = `Issue Domain`), size=2.5)+
  geom_errorbarh(aes(x = estimate, xmin = estimate-1.96*se, xmax = estimate+1.96*se, y = item, color = `Issue Domain`))+
  scale_color_manual(values = colors) +
  xlab("Parameter Estimates")+
  ylab("Item")+
  geom_vline(xintercept = 0, linetype = 2, color = "darkgray", size = 1) +
  theme_minimal(base_size = 17)+
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title="Issue Domain", nrow = 2, byrow = TRUE),
         shape = guide_legend(title="Issue Domain", nrow = 2, byrow = TRUE))

ggsave("figureS2.tiff", width = 7.5, height = 10)

end_time <- Sys.time()

nes_out_time <- end_time - start_time

save.image(file = "nes_final.RData")

