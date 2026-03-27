##a) civil violence
me_main_m1g_t1_sc <- margins_summary(main_m1g_t1_sc, variables = "sa_territory_t", vcov = cluster.vcov(main_m1g_t1_sc, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 1)$cowcode)))
me_main_m1g_t1_sc$term <- "second-order maj."
me_main_m1g_t1_sf <- margins_summary(main_m1g_t1_sf, variables = "sa_territory_t", vcov = cluster.vcov(main_m1g_t1_sf, as.integer(subset(main_group, subset_analysis == 1 & int_grp_rel_dominant_g == 0)$cowcode)))
me_main_m1g_t1_sf$term <- "second-order min."
me_main_m1g <- rbind.fill(me_main_m1g_t1_sc, me_main_m1g_t1_sf)
me_main_m1g$term <- as.factor(me_main_m1g$term)
me_main_m1g$term = factor(me_main_m1g$term,levels(me_main_m1g$term)[c(2,1)])
me_main_m1g$model <- 1
me_main_m1g$estimate <- me_main_m1g$AME
me_main_m1g$conf.low <- me_main_m1g$lower
me_main_m1g$conf.high <- me_main_m1g$upper
me_main_m1g_plot <- dwplot(me_main_m1g, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(shape = 16, colour = "black"), whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") + geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") + scale_color_manual(name = "Coefficient for:", values = c("#1b9e77","#d95f02","#7570b3"))+ 
  scale_linetype_manual(name = "Coefficient for:", values = c(3,2,1)) + scale_shape_manual(name = "Coefficient for:", values = c(17,18,20)) + 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("group type")+
  guides(shape = guide_legend(nrow=2,"model",reverse=TRUE), colour = guide_legend(nrow=2,"model",reverse=TRUE), linetype = guide_legend(nrow=2,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format(), breaks = c(0, -0.01, -0.02))
##b) communal violence
me_main_m1d_t1_sb <- margins_summary(main_m1d_t1_sb, variables = "sa_territory_t", vcov = cluster.vcov(main_m1d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_main_m1d_t1_sb$term <- "all"
me_main_m2d_t1_sb <- margins_summary(main_m2d_t1_sb, variables = "sa_territory_t", at = list(included_excluded = c(0,1)), vcov = cluster.vcov(main_m2d_t1_sb, as.integer(subset(main_dyad,  subset_analysis == 1 & int_grp_rel_dominant == 1)$cowcode)))
me_main_m2d_t1_sb$term <- ifelse(me_main_m2d_t1_sb$included_excluded == 1, "included/excluded", "other")
me_main_m13d <- rbind.fill(me_main_m1d_t1_sb, me_main_m2d_t1_sb)
me_main_m13d$model <- 1
me_main_m13d <- subset(me_main_m13d, term != "other")
me_main_m13d$term <- as.factor(me_main_m13d$term)
me_main_m13d$estimate <- me_main_m13d$AME
me_main_m13d$conf.low <- me_main_m13d$lower
me_main_m13d$conf.high <- me_main_m13d$upper
me_main_m13d_plot <- dwplot(me_main_m13d,
                            vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                            dot_args = list(shape = 16, colour = "black"), 
                            whisker_args = list(linetype = 1, colour = "black")) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type") +
  scale_x_continuous(labels=scales::percent_format(), breaks = c(0, 0.0025, 0.005, 0.0075))
##c) combined
figure4 <- grid.arrange(me_main_m1g_plot, me_main_m13d_plot, ncol=2, nrow=1)
ggsave(figure4, file='../figures/figure4.pdf', width = 14, height = 3.5, units="cm",dpi=1000)