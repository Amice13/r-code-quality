####figure S14####
figure_s14 <- main_group
figure_s14 <- unique(figure_s14[c("cowcode","fips","year","adm_abs","sa_territory_t","sa_revenue","sa_expenditure")])
figure_s14 <- figure_s14[complete.cases(figure_s14),]
figure_s14 <- figure_s14 %>% group_by(cowcode,year) %>% mutate(sa_territory_t = sum(adm_abs * sa_territory_t) / sum(adm_abs))#calculating the average, size-weighted territorial autonomy across all units
figure_s14 <- unique(figure_s14[c("cowcode","year","sa_territory_t","sa_revenue","sa_expenditure")])
cor(figure_s14[c("sa_territory_t","sa_revenue","sa_expenditure")], use ="pairwise.complete.obs")
figure_s14_1 <- ggplot(aes(y = sa_territory_t, x = sa_revenue), data = figure_s14) + geom_point(alpha=0.2, size=1) + 
  geom_smooth(method = "lm") +
  ylab("avg. territorial autonomy\n(country)") + xlab("regional gov. revenue") +
  theme_bw() + theme(legend.position = "bottom", text=element_text(family="Times")) + 
  guides(shape=guide_legend(title="a) revenue"))
figure_s14_1
figure_s14_2 <- ggplot(aes(y = sa_territory_t, x = sa_expenditure), data = figure_s14) + geom_point(alpha=0.2, size=1) + 
  geom_smooth(method = "lm") +
  ylab("avg. territorial autonomy\n(country)") + xlab("regional gov. expenditure") +
  theme_bw() + theme(legend.position = "bottom", text=element_text(family="Times")) + 
  guides(shape=guide_legend(title="b) expenditure"))
figure_s14_2
figure_s14_combined <- grid.arrange(figure_s14_1, figure_s14_2, nrow = 1, ncol =2)
ggsave(figure_s14_combined, file='../figures/figures14.pdf', width = 17, height = 6, units="cm",dpi=1000)

####table S5####
##part 1: all units ("all")
table_s5_1 <- unique(main_group[c("cowcode","fips","year","sa_political_territory_t","sa_territory_t","sa_revenue","sa_expenditure","rai_autonomy")])
table_s5_part1 <- cor(table_s5_1[c("sa_territory_t","sa_revenue","sa_expenditure","rai_autonomy")], use ="pairwise.complete.obs")
write.csv(table_s5_part1, file="../tables/tables5_part1.csv")
##part 2: units with autonomous governing organ ("autonomous")
table_s5_2 <- subset(table_s5_1, sa_political_territory_t > 0)
table_s5_part2 <- cor(table_s5_2[c("sa_territory_t","sa_revenue","sa_expenditure","rai_autonomy")], use ="pairwise.complete.obs")
write.csv(table_s5_part2, file="../tables/tables5_part2.csv")

####figure S15####
figure_s15 <- main_group
figure_s15 <- unique(figure_s15[c("cowcode","fips","year","sa_territory_t","rai_autonomy")])
figure_s15 <- figure_s15[complete.cases(figure_s15),]
cor(figure_s15[c("sa_territory_t","rai_autonomy")], use ="pairwise.complete.obs")
figure_s15_plot <- ggplot(aes(y = sa_territory_t, x = rai_autonomy), data = figure_s15) + geom_point(alpha=0.003, size=1) + 
  geom_smooth(method = "lm") +
  ylab("territorial autonomy") + xlab("RAI self-rule") +
  theme_bw() + theme(legend.position = "bottom", text=element_text(family="Times"))
figure_s15_plot
ggsave(figure_s15_plot, file='../figures/figures15.pdf', width = 8.5, height = 6, units="cm",dpi=1000)

####figure S16####
figure_s16 <- main_group
figure_s16 <- unique(figure_s16[c("cowcode","gwgroupid","state_control","fips","year","int_grp_rel","adm_abs","size","sa_territory_t","autonomy")])
figure_s16 <- figure_s16[complete.cases(figure_s16),]
figure_s16 <- figure_s16 %>% group_by(gwgroupid,year) %>% mutate(sa_territory_t = sum((int_grp_rel * adm_abs) * sa_territory_t) / sum(int_grp_rel * adm_abs))
figure_s16 <- unique(figure_s16[c("gwgroupid","year","sa_territory_t","autonomy")])
figure_s16 <- figure_s16[complete.cases(figure_s16),]
cor(figure_s16[c("sa_territory_t","autonomy")], use ="pairwise.complete.obs")
figure_s16_plot <- ggplot(aes(y = sa_territory_t, x = autonomy), data = figure_s16) + geom_point(alpha=0.003, size=1) + 
  geom_smooth(method = "lm") +
  ylab("avg. territorial autonomy\n(group)") + xlab("de-facto autonomy (EPR)") +
  theme_bw() + theme(legend.position = "bottom", text=element_text(family="Times")) + 
  guides(shape=guide_legend(title="b) expenditure"))
figure_s16_plot
ggsave(figure_s16_plot, file='../figures/figures16.pdf', width = 8.5, height = 6, units="cm",dpi=1000)