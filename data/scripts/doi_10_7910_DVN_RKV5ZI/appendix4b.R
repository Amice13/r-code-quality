####Table A4####
#see Stata file for estimation of all marginal effects and underlying IV models

####Figure 5####
#ME estimates based on margins results from Stata file
##a) uncorrected
me_ivg2_end <- data.frame(factor = "sa_territory_t", model = "uncorrected", term = "second-order maj.", estimate = .0009268, conf.low =-.0036664, conf.high = .0055199)#table A6, model 5
me_ivg1_end <- data.frame(factor = "sa_territory_t", model = "uncorrected", term = "second-order min.", estimate = -.0054098, conf.low = -.0093911, conf.high = -.0014284)#table A6, model 7
me_ivd1_end <- data.frame(factor = "sa_territory_t", model = "uncorrected", term = "all", estimate = -.0004906, conf.low = -.0014017, conf.high = .0004205)#table A7, model 9
me_ivd2_end <- data.frame(factor = "sa_territory_t", model = "uncorrected", term = "included/excluded", estimate = .002628, conf.low = .0012852, conf.high = .0039709)#table A7, model 11
##b) instrumented
me_ivg2_ex <- data.frame(factor = "sa_territory_t", model = "IV", term = "second-order maj.", estimate = -.0172106, conf.low = -.0606086, conf.high = .0261874)#table A6, model 6
me_ivg1_ex <- data.frame(factor = "sa_territory_t", model = "IV", term = "second-order min.", estimate = -.0233837, conf.low =  -.0491156, conf.high = .0023482)#table A6, model 8
me_ivd1_ex <- data.frame(factor = "sa_territory_t", model = "IV", term = "all", estimate = .006599, conf.low = -.0011226, conf.high = .0143207)#table A7, model 10
me_ivd2_ex <- data.frame(factor = "sa_territory_t", model = "IV", term = "included/excluded", estimate = .016036, conf.low = .0020314, conf.high = .0300406)#table A7, model 12
##c) combined (civil)
me_ivg <- rbind(me_ivg2_end, me_ivg1_end, me_ivg2_ex, me_ivg1_ex)
me_ivg_plot <- dwplot(me_ivg, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), dot_args = list(aes(colour = as.factor(model), shape = as.factor(model))),  whisker_args = list(linetype = 1), aes(colour = as.factor(model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of civil violence") + ylab("dyad type") +
  scale_color_manual(name = "Model", values = c("#1b9e77","#7570b3"))+ scale_shape_manual(name = "Model", values = c(17,20))+ scale_x_continuous(labels=scales::percent_format()) +
  guides(shape = guide_legend(nrow=1,"model",reverse=TRUE), colour = guide_legend(nrow=1,"model",reverse=TRUE))
##d) combined (communal)
me_ivd <- rbind(me_ivd1_end, me_ivd2_end, me_ivd1_ex, me_ivd2_ex)
me_ivd_plot <- dwplot(me_ivd,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(aes(colour = as.factor(model), shape = as.factor(model))), whisker_args = list(linetype = 1), aes(colour = as.factor(model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\n of communal violence") + ylab("dyad type") +
  scale_color_manual(name = "Model", values = c("#1b9e77","#7570b3"))+ scale_shape_manual(name = "Model", values = c(17,20))+ 
  scale_x_continuous(labels=scales::percent_format()) +
  guides(shape = guide_legend(nrow=1,"model",reverse=TRUE), colour = guide_legend(nrow=1,"model",reverse=TRUE))
##e) combined
figure5 <- grid_arrange_shared_legend(me_ivg_plot, me_ivd_plot, ncol=2,nrow=1)
ggsave(figure5, file='../figures/figure5.pdf', width = 14, height = 5, units="cm",dpi=1000)

####2.3: Figure A21
iv_graph <- subset(grid_group, main_sample_gid == 1 & sample_colonies_all == 1 & gidl_g_largest_g == 0 & irrelevant == 0 & state_control == 0)
iv_graph <- unique(iv_graph[c("cowcode","fips","year","former_fr","gidl_g_size_abs","sa_territory_t")])
figurea21 <- ggplot(aes(y = sa_territory_t, x = log(gidl_g_size_abs+0.00000001), color = factor(former_fr), fill = factor(former_fr)), data = iv_graph) + geom_point(alpha=0.05, size=0.5) + 
  geom_smooth(method = "lm") +
  ylab("territorial autonomy") + xlab("max. abs. size (grid cell, logged)") +
  theme_bw() + theme(legend.position = "bottom", axis.text = element_text(size=7), text=element_text(family="Times")) + 
  guides(shape=guide_legend(title="a) autonomy")) +
  scale_color_manual(name = "French colony", values = c("#7fc97f","#fdc086")) +
  scale_fill_manual(name = "French colony", values = c("#7fc97f","#fdc086")) + coord_cartesian(ylim = c(0,1))
ggsave(figurea21, file='../figures/figurea21.pdf', width = 17, height = 8, units="cm",dpi=500)

####Figure A22####
#see Stata file

####Figure A23####
#ME estimates based on margins results from Stata file
##a) robustness checks (civil)
me_iv_r1a <- data.frame(model = "land-use controls",term="second-order maj.", estimate =  -.0195918, conf.low = -.0682545, conf.high = .0290709)#table X39, model A141
me_iv_r1b <- data.frame(model = "land-use controls",term="second-order min.", estimate = -.0222317, conf.low = -.051153, conf.high = .0066896)#table X39, model A142
me_iv_r2a <- data.frame(model = "gross cell product control",term="second-order maj.", estimate = -.0123007, conf.low = -.0553406 , conf.high = .0307392)#table X40, model A143
me_iv_r2b <- data.frame(model = "gross cell product control",term="second-order min.", estimate = -.0207895, conf.low = -.0459941, conf.high = .0044151)#table X40, model A144
me_iv_r3a <- data.frame(model = "cell nightlights control",term="second-order maj.", estimate = -.0168785, conf.low = -.0622786, conf.high = .0285217)#table X41, model A145
me_iv_r3b <- data.frame(model = "cell nightlights control",term="second-order min.", estimate = -.0232247, conf.low = -.0491409, conf.high = .0026916)#table X41, model A146
me_iv_r4a <- data.frame(model = "agricultural production x included elites",term="second-order maj.", estimate = -.0195432, conf.low = -.0692929, conf.high = .0302064)#table X42, model A147
me_iv_r4b <- data.frame(model = "agricultural production x included elites",term="second-order min.", estimate = -.0227306, conf.low = -.0501123, conf.high = .0046512)#table X42, model A148
me_iv_r5a <- data.frame(model = "absolute size at independence",term="second-order maj.", estimate = -.01951, conf.low = -.0690471, conf.high = .0300272)#table X43, model A149
me_iv_r5b <- data.frame(model = "absolute size at independence",term="second-order min.", estimate = -.0224804, conf.low = -.0457309, conf.high = .00077)#table X43, model A150
##b) robustness checks (communal)
me_ivd_r1a <- data.frame(model = "land-use controls",term="all", estimate = .0078165, conf.low = -.0018326, conf.high = .0174656)#table X44, model A151
me_ivd_r1b <- data.frame(model = "land-use controls",term="included/excluded", estimate =.0211882, conf.low = .0047059, conf.high = .0376705)#table X44, model A152
me_ivd_r2a <- data.frame(model = "gross cell product control",term="all", estimate = .0071643, conf.low = -.0011579 , conf.high = .0154865)#table X45, model A153
me_ivd_r2b <- data.frame(model = "gross cell product control",term="included/excluded", estimate = .0176473, conf.low = .0024292, conf.high = .0328653)#table X45, model A154
me_ivd_r3a <- data.frame(model = "cell nightlights control",term="all", estimate = .0068561, conf.low = -.0010354, conf.high = .0147477)#table X46, model A155
me_ivd_r3b <- data.frame(model = "cell nightlights control",term="included/excluded", estimate = .0165353, conf.low = .0023742, conf.high = .0306964)#table X46, model A156
me_ivd_r4a <- data.frame(model = "agricultural production x included elites",term="all", estimate = .0080126, conf.low = -.0015529, conf.high = .0175781)#table X47, model A157
me_ivd_r4b <- data.frame(model = "agricultural production x included elites",term="included/excluded", estimate = .0212764, conf.low = .0039694, conf.high = .0385834)#table X47, model A158
me_ivd_r5a <- data.frame(model = "absolute size at independence",term="all", estimate = .007154, conf.low =  -.000168, conf.high = .0144761)#table X48, model A159
me_ivd_r5b <- data.frame(model = "absolute size at independence",term="included/excluded", estimate = .0173151, conf.low = .0043841, conf.high = .0302462)#table X48, model A160
##c) combined (civil)
iv_r_plot_data_g <- rbind.fill(me_iv_r1a,me_iv_r1b,me_iv_r4a,me_iv_r4b, me_iv_r2a,me_iv_r2b,me_iv_r3a,me_iv_r3b,me_iv_r5a,me_iv_r5b)
iv_r_plot_data_g$model <- as.factor(iv_r_plot_data_g$model)
iv_r_plot_g <- dwplot(iv_r_plot_data_g,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("a) civil violence") +
  scale_color_manual(name = "model", values = c("#7fc97f","#beaed4","#fdc086","#386cb0","#f0027f"))+ scale_linetype_manual(name = "model", values = c(5,4,3,2,1))+ scale_shape_manual(name = "model", values = c(15,16,17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\nof civil violence") + ylab("group type")+
  guides(shape = guide_legend(nrow=3,"model",reverse=TRUE), colour = guide_legend(nrow=3,"model",reverse=TRUE),linetype = guide_legend(nrow=3,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
##d) combined (communal)
ivd_r_plot_data_g <- rbind.fill(me_ivd_r1a,me_ivd_r1b,me_ivd_r4a,me_ivd_r4b, me_ivd_r2a,me_ivd_r2b,me_ivd_r3a,me_ivd_r3b,me_ivd_r5a,me_ivd_r5b)
ivd_r_plot_data_g$model <- as.factor(iv_r_plot_data_g$model)
ivd_r_plot_g <- dwplot(ivd_r_plot_data_g,vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),dot_args = list(aes(colour = model,shape =model)), whisker_args = list(linetype = 1, aes(colour = model))) +
  theme_bw() + theme(plot.title = element_text(size=12)) + xlab("Coefficient estimate") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
  ggtitle("b) communal violence maj./min. dyad") +
  scale_color_manual(name = "model", values = c("#7fc97f","#beaed4","#fdc086","#386cb0","#f0027f"))+ scale_linetype_manual(name = "model", values = c(5,4,3,2,1))+ scale_shape_manual(name = "model", values = c(15,16,17,18,20))+ 
  theme(plot.title.position = "plot",legend.position = "bottom", text=element_text(family="Times"), axis.title=element_text(size=10), legend.text=element_text(size=10)) + 
  xlab("change in predicted prob.\nof communal violence") + ylab("dyad type")+
  guides(shape = guide_legend(nrow=3,"model",reverse=TRUE), colour = guide_legend(nrow=3,"model",reverse=TRUE),linetype = guide_legend(nrow=3,"model",reverse=TRUE)) +
  scale_x_continuous(labels=scales::percent_format())
##e) combined
figurea23 <- grid_arrange_shared_legend(iv_r_plot_g, ivd_r_plot_g, ncol=2, nrow=1)
ggsave(figurea23, file='../figures/figurea23.pdf', width = 14, height = 7, units="cm",dpi=1000)