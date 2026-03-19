
####APC-Analysis Diffuse Support#####



datCompleteNew <- datComplete %>% 
  gather(dv_name, dv_value, c(idea_democracy, satisfaction_democracy, libdem_opposition:libdem_generalwill))


#####Rerun analysis with effect coding and OVA for predictions########

#### ** GAM####

set.seed(1234)

###Change reference category for the categorical variables

datCompleteNew <- datCompleteNew %>% 
  mutate(age_grps = if_else(age_grp1 == 1, 1,
                            if_else(age_grp2 == 1, 2, 3)),
         age_grps = factor(age_grps))

model_GAM_Ost = datCompleteNew %>% 
  filter(surveyyear > 1990) %>% 
  filter(east == 1) %>% 
  group_by(dv_name) %>%
  drop_na(dv_value) %>% 
  droplevels() %>%
  do(fitModel = bam(dv_value ~ age_grps + period.f + s(born_adult), data = ., subset = east == 1 , weights = weight, contrasts = list(age_grps = contr.sum, period.f = contr.sum))
  )

model_GAM_West = datCompleteNew %>% 
  filter(east == 0) %>% 
  group_by(dv_name) %>%
  drop_na(dv_value) %>% 
  droplevels() %>%
  do(fitModel = bam(dv_value ~ age_grps + period.f + s(born_adult), data = ., subset = east == 0 , weights = weight, contrasts = list(age_grps = contr.sum, period.f = contr.sum))
  )

name_flag <- model_GAM_Ost$dv_name


datBindOst <- list()
datBindWest <- list()

for(i in 1:length(name_flag)){
  datBindOst[[i]] <- datCompleteNew %>% 
    filter(surveyyear > 1990) %>% 
    filter(east == 1) %>% 
    drop_na(age_grps, period.f, born_adult, weight, dv_value) %>% 
    select(dv_name, dv_value, period.f, born_adult, age_grps) %>% 
    filter(dv_name == name_flag[i]) %>% 
    droplevels() %>% 
    select(period.f, born_adult, age_grps)
  
  datBindWest[[i]] <- datCompleteNew %>% 
    filter(east == 0) %>% 
    drop_na(age_grps, period.f, born_adult, weight, dv_value) %>% 
    select(dv_name, dv_value, period.f, born_adult, age_grps) %>% 
    filter(dv_name == name_flag[i]) %>% 
    droplevels() %>% 
    select(period.f, born_adult, age_grps)
}

predList_Ost <- list()
predList_West <- list()

for(i in 1:length(name_flag)){
  predList_Ost[[i]] <- predict(model_GAM_Ost$fitModel[[i]], datBindOst[[i]],  type = "link", se.fit = TRUE)
  predList_West[[i]] <- predict(model_GAM_West$fitModel[[i]], datBindWest[[i]], type = "link", se.fit = TRUE)
}

preds_Ost <- list()
preds_West <- list()

for(i in 1:length(name_flag)){
  preds_Ost[[i]] <- transform(cbind(data.frame(predList_Ost[[i]]), datBindOst[[i]]))
  preds_West[[i]] <- transform(cbind(data.frame(predList_West[[i]]), datBindWest[[i]]))
}




rmvn <- function(n, mu, sig) { ## MVN random deviates
  L <- mroot(sig)
  m <- ncol(L)
  t(mu + L %*% matrix(rnorm(m*n), m, n))
}


critOst <- list()
critWest <- list()

for(i in 1:length(name_flag)){
  
  Vb <- vcov(model_GAM_Ost$fitModel[[i]])
  
  pred <- predict(model_GAM_Ost$fitModel[[i]], se.fit = TRUE)
  
  se.fit <- pred$se.fit
  
  set.seed(42)
  N <- 1000
  
  BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
  
  Cg <- predict(model_GAM_Ost$fitModel[[i]], type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  
  masd <- apply(absDev, 2L, max)
  
  critOst[[i]] <- quantile(masd, prob = 0.95, type = 8)
}


for(i in 1:length(name_flag)){
  
  Vb <- vcov(model_GAM_West$fitModel[[i]])
  
  pred <- predict(model_GAM_West$fitModel[[i]], se.fit = TRUE)
  
  se.fit <- pred$se.fit
  
  set.seed(42)
  N <- 1000
  
  BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
  
  Cg <- predict(model_GAM_West$fitModel[[i]], type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  
  masd <- apply(absDev, 2L, max)
  
  critWest[[i]] <- quantile(masd, prob = 0.95, type = 8)
}


predBornAdult_Ost <- list()
predBornAdult_West <- list()

for(i in 1:length(name_flag)){
  
  predBornAdult_Ost[[i]] <- preds_Ost[[i]] %>% 
    group_by(born_adult) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critOst[[i]] * se.fit,
           LL = fit - critOst[[i]] * se.fit,
           east = 1) %>% 
    select(born_adult, fit, UL, LL, east)
  
  predBornAdult_West[[i]] <- preds_West[[i]] %>% 
    group_by(born_adult) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critWest[[i]] * se.fit,
           LL = fit - critWest[[i]] * se.fit,
           east = 0) %>% 
    select(born_adult, fit, UL, LL, east)
}


predPeriod_Ost <- list()
predPeriod_West <- list()

for(i in 1:length(name_flag)){
  predPeriod_Ost[[i]] <- preds_Ost[[i]] %>% 
    group_by(period.f) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critOst[[i]] * se.fit,
           LL = fit - critOst[[i]] * se.fit,
           east = 1) %>% 
    select(period.f, fit, UL, LL, east)
  
  predPeriod_West[[i]] <- preds_West[[i]] %>% 
    group_by(period.f) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critWest[[i]] * se.fit,
           LL = fit - critWest[[i]] * se.fit,
           east = 0) %>% 
    select(period.f, fit, UL, LL, east)
  
}

names(predBornAdult_Ost) <- name_flag
names(predBornAdult_West) <- name_flag

predBornAdultOst <- bind_rows(predBornAdult_Ost, .id = "dv_name")
predBornAdultWest <- bind_rows(predBornAdult_West, .id = "dv_name")

predBornAdult <- bind_rows(predBornAdultOst, predBornAdultWest)


names(predPeriod_Ost) <- name_flag
names(predPeriod_West) <- name_flag


predPeriodOst <- bind_rows(predPeriod_Ost, .id = "dv_name")
predPeriodWest <- bind_rows(predPeriod_West, .id = "dv_name")

predPeriod <- bind_rows(predPeriodOst, predPeriodWest)



#### ** Plotting results#####


###Cohort


pg_list_cohort <- list()

name_flag_plots <- c("Support for the idea of democracy", "Pluralist differences of interest groups", "Pluralist differences of parties", "Freedom of expression", "Prioritizing individual interests", "Political opposition", "Multi-party competition", "Evaluation of regime performance")


for(i in 1:length(name_flag)){
  predPlotData <- predBornAdult[predBornAdult$dv_name==name_flag[i],]
  
  pg_list_cohort[[i]] <- ggplot(predPlotData, aes(x=born_adult, 
                                                           y=fit,
                                                           fill = as.factor(east),
                                                           linetype = as.factor(east),
                                                           color = as.factor(east))) + 
    geom_errorbar(aes(ymin = LL, ymax = UL), alpha = 0.2) +
    geom_point(alpha = 0.1, size = 1) + 
    geom_smooth(aes(y = fit), se = F, alpha = 0.5) + 
    geom_smooth(aes(y = LL), se = F, alpha = 0.5, linetype = "solid", size = 0.5) + 
    geom_smooth(aes(y = UL), se = F, alpha = 0.5, linetype = "solid", size = 0.5) + 
    ggtitle("")
  
  pg_list_cohort[[i]] <- pg_list_cohort[[i]] + 
    scale_x_continuous(name = "Year in which respondents turned 18", 
                       breaks = c(1925, 1950, 1975, 2000, 2025)) + 
    scale_y_continuous(name = "Mean attitudes across cohorts", 
                       limits = c(-0.05,1.05)) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          text = element_text(size=9), 
          axis.text = element_text(size = 10), 
          legend.position = "none") + 
    geom_vline(xintercept = 1990, 
               linetype = "dashed", 
               color = "red", 
               size = 0.5)
  
  
}


###Period


pg_list_period <- list()

name_flag_plots <- c("Support for the idea of democracy", "Pluralist differences of interest groups", "Pluralist differences of parties", "Freedom of expression", "Prioritizing individual interests", "Political opposition", "Multi-party competition", "Evaluation of regime performance")



for(i in 1:length(name_flag)){
  
  predPlotData <- predPeriod[predPeriod$dv_name==name_flag[i],]
  
  pg_list_period[[i]] <- ggplot(predPlotData, aes(x=as.Date(period.f, format="%Y"),
                                    y=fit, 
                                    group = east,
                                    colour = as.factor(east), 
                                    linetype = as.factor(east))) + 
    geom_errorbar(aes(ymin = LL, ymax = UL)) +
    geom_point() + 
    geom_smooth(se = F, 
                method = "gam", formula = y ~ s(x, k = 3),
                size = 1) + 
    ggtitle(paste(name_flag_plots[i]))
  
  pg_list_period[[i]] <- pg_list_period[[i]] + 
    scale_x_date(name = "Year in which survey was conducted", 
                 limits = c(as.Date("1980-01-01", format="%Y"), c(as.Date("2020-01-01", format="%Y")))) + 
    scale_y_continuous(name = "Mean attitudes across periods", 
                       limits = c(-0.05,1.05)) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          text = element_text(size=9), 
          axis.text = element_text(size = 10), 
          legend.position = "none")
  
}

pg_list_period[[1]] <- pg_list_period[[1]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[1]] <- pg_list_cohort[[1]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))

pg_list_period[[5]] <- pg_list_period[[5]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[5]] <- pg_list_cohort[[5]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))

pg_list_period[[2]] <- pg_list_period[[2]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[2]] <- pg_list_cohort[[2]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))

pg_list_period[[6]] <- pg_list_period[[6]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[6]] <- pg_list_cohort[[6]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))


pg_list_cohort[[8]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("West", "East"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("West", "East"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))

pg_list_cohort[[8]] <- pg_list_cohort[[8]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("West", "East"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("West", "East"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))

pg_list_cohort[[4]] <- pg_list_cohort[[4]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("West", "East"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("West", "East"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))

pg_list_cohort[[3]] <- pg_list_cohort[[3]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("West", "East"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("West", "East"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))

pg_list_cohort[[7]] <- pg_list_cohort[[7]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("West", "East"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("West", "East"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))



#Figure 1
tiff(file=paste0("plots/figure1_satisfaction_idea_OVA.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[8]],  pg_list_period[[1]], pg_list_cohort[[8]],  pg_list_cohort[[1]], nrow = 2))

dev.off()

#Figure 2
tiff(file=paste0("plots/figure2_libDem_expre_prerogative_OVA.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[4]],  pg_list_period[[5]],  pg_list_cohort[[4]],  pg_list_cohort[[5]], nrow = 2))

dev.off()

#Figure 3
tiff(file=paste0("plots/figure3_libDem_pluralism_OVA.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[3]],  pg_list_period[[2]],  pg_list_cohort[[3]],  pg_list_cohort[[2]], nrow = 2))

dev.off()

#Figure 4
tiff(file=paste0("plots/figure4_libDem_competition_OVA.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[7]],  pg_list_period[[6]],  pg_list_cohort[[7]],  pg_list_cohort[[6]], nrow = 2))

dev.off()


name_flag_tables <- c("Support for the idea of democracy", "Pluralist differences of interest groups", "Pluralist differences of parties", "Freedom of expression", "Prioritizing individual interests", "Political opposition", "Multi-party competition", "Evaluation of regime performance")

####Regression tables GAM models
htmlreg(list(model_GAM_West$fitModel[[8]], model_GAM_West$fitModel[[1]], model_GAM_West$fitModel[[4]], model_GAM_West$fitModel[[5]], model_GAM_West$fitModel[[3]], model_GAM_West$fitModel[[2]], model_GAM_West$fitModel[[7]], model_GAM_West$fitModel[[6]]), file = "tables/gam_west_diffuse_OVA.doc", custom.model.names = name_flag_tables, caption = "", single.row = T)

htmlreg(list(model_GAM_Ost$fitModel[[8]], model_GAM_Ost$fitModel[[1]], model_GAM_Ost$fitModel[[4]], model_GAM_Ost$fitModel[[5]], model_GAM_Ost$fitModel[[3]], model_GAM_Ost$fitModel[[2]], model_GAM_Ost$fitModel[[7]], model_GAM_Ost$fitModel[[6]]), file = "tables/gam_east_diffuse_OVA.doc", custom.model.names = name_flag_tables, caption = "", single.row = T)


#####Appendix#########



######Separate analysis by Left and Right#######

###Change reference category for the categorical variables

datCompleteNew <- datCompleteNew %>% 
  mutate(age_grps = if_else(age_grp1 == 1, 1,
                            if_else(age_grp2 == 1, 2, 3)),
         age_grps = factor(age_grps))

model_GAM_Left = datCompleteNew %>% 
  filter(partyvote_category == 0) %>% 
  group_by(dv_name) %>%
  drop_na(dv_value) %>% 
  droplevels() %>%
  do(fitModel = bam(dv_value ~ age_grps + period.f + s(born_adult), data = ., weights = weight, contrasts = list(age_grps = contr.sum, period.f = contr.sum))
  )

model_GAM_Right = datCompleteNew %>% 
  filter(partyvote_category == 1) %>% 
  group_by(dv_name) %>%
  drop_na(dv_value) %>% 
  droplevels() %>%
  do(fitModel = bam(dv_value ~ age_grps + period.f + s(born_adult), data = ., weights = weight, contrasts = list(age_grps = contr.sum, period.f = contr.sum))
  )

name_flag <- model_GAM_Left$dv_name


datBindLeft <- list()
datBindRight <- list()

for(i in 1:length(name_flag)){
  datBindLeft[[i]] <- datCompleteNew %>% 
    filter(partyvote_category == 0) %>% 
    drop_na(age_grps, period.f, born_adult, weight, dv_value) %>% 
    select(dv_name, dv_value, period.f, born_adult, age_grps) %>% 
    filter(dv_name == name_flag[i]) %>% 
    droplevels() %>% 
    select(period.f, born_adult, age_grps)
  
  datBindRight[[i]] <- datCompleteNew %>% 
    filter(partyvote_category == 1) %>% 
    drop_na(age_grps, period.f, born_adult, weight, dv_value) %>% 
    select(dv_name, dv_value, period.f, born_adult, age_grps) %>% 
    filter(dv_name == name_flag[i]) %>% 
    droplevels() %>% 
    select(period.f, born_adult, age_grps)
}

predList_Left <- list()
predList_Right <- list()

for(i in 1:length(name_flag)){
  predList_Left[[i]] <- predict(model_GAM_Left$fitModel[[i]], datBindLeft[[i]],  type = "link", se.fit = TRUE)
  predList_Right[[i]] <- predict(model_GAM_Right$fitModel[[i]], datBindRight[[i]], type = "link", se.fit = TRUE)
}

preds_Left <- list()
preds_Right <- list()

for(i in 1:length(name_flag)){
  preds_Left[[i]] <- transform(cbind(data.frame(predList_Left[[i]]), datBindLeft[[i]]))
  preds_Right[[i]] <- transform(cbind(data.frame(predList_Right[[i]]), datBindRight[[i]]))
}




rmvn <- function(n, mu, sig) { ## MVN random deviates
  L <- mroot(sig)
  m <- ncol(L)
  t(mu + L %*% matrix(rnorm(m*n), m, n))
}


critLeft <- list()
critRight <- list()

for(i in 1:length(name_flag)){
  
  Vb <- vcov(model_GAM_Left$fitModel[[i]])
  
  pred <- predict(model_GAM_Left$fitModel[[i]], se.fit = TRUE)
  
  se.fit <- pred$se.fit
  
  set.seed(42)
  N <- 1000
  
  BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
  
  Cg <- predict(model_GAM_Left$fitModel[[i]], type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  
  masd <- apply(absDev, 2L, max)
  
  critLeft[[i]] <- quantile(masd, prob = 0.95, type = 8)
}


for(i in 1:length(name_flag)){
  
  Vb <- vcov(model_GAM_Right$fitModel[[i]])
  
  pred <- predict(model_GAM_Right$fitModel[[i]], se.fit = TRUE)
  
  se.fit <- pred$se.fit
  
  set.seed(42)
  N <- 1000
  
  BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
  
  Cg <- predict(model_GAM_Right$fitModel[[i]], type = "lpmatrix")
  simDev <- Cg %*% t(BUdiff)
  
  absDev <- abs(sweep(simDev, 1, se.fit, FUN = "/"))
  
  masd <- apply(absDev, 2L, max)
  
  critRight[[i]] <- quantile(masd, prob = 0.95, type = 8)
}


predBornAdult_Left <- list()
predBornAdult_Right <- list()

for(i in 1:length(name_flag)){
  
  predBornAdult_Left[[i]] <- preds_Left[[i]] %>% 
    group_by(born_adult) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critLeft[[i]] * se.fit,
           LL = fit - critLeft[[i]] * se.fit,
           left = 1) %>% 
    select(born_adult, fit, UL, LL, left)
  
  predBornAdult_Right[[i]] <- preds_Right[[i]] %>% 
    group_by(born_adult) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critRight[[i]] * se.fit,
           LL = fit - critRight[[i]] * se.fit,
           left = 0) %>% 
    select(born_adult, fit, UL, LL, left)
}


predPeriod_Left <- list()
predPeriod_Right <- list()

for(i in 1:length(name_flag)){
  predPeriod_Left[[i]] <- preds_Left[[i]] %>% 
    group_by(period.f) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critLeft[[i]] * se.fit,
           LL = fit - critLeft[[i]] * se.fit,
           left = 1) %>% 
    select(period.f, fit, UL, LL, left)
  
  predPeriod_Right[[i]] <- preds_Right[[i]] %>% 
    group_by(period.f) %>% 
    mutate(m = n(),
           mean_fit = mean(fit),
           S_sq = (fit - mean_fit)^2/(m - 1)) %>%
    replace_na(list(S_sq = 0)) %>% 
    summarize(fit = mean(fit),
              S_sq = mean(S_sq),
              se.fit = mean(se.fit^2) + S_sq,
              se.fit = sqrt(se.fit)) %>% 
    ungroup() %>% 
    mutate(UL = fit + critRight[[i]] * se.fit,
           LL = fit - critRight[[i]] * se.fit,
           left = 0) %>% 
    select(period.f, fit, UL, LL, left)
  
}

names(predBornAdult_Left) <- name_flag
names(predBornAdult_Right) <- name_flag

predBornAdultLeft <- bind_rows(predBornAdult_Left, .id = "dv_name")
predBornAdultRight <- bind_rows(predBornAdult_Right, .id = "dv_name")

predBornAdult <- bind_rows(predBornAdultLeft, predBornAdultRight)


names(predPeriod_Left) <- name_flag
names(predPeriod_Right) <- name_flag


predPeriodLeft <- bind_rows(predPeriod_Left, .id = "dv_name")
predPeriodRight <- bind_rows(predPeriod_Right, .id = "dv_name")

predPeriod <- bind_rows(predPeriodLeft, predPeriodRight)



#### ** Plotting results#####


###Cohort


pg_list_cohort <- list()

name_flag_plots <- c("Support for the idea of democracy", "Pluralist differences of interest groups", "Pluralist differences of parties", "Freedom of expression", "Prioritizing individual interests", "Political opposition", "Multi-party competition", "Evaluation of regime performance")


for(i in 1:length(name_flag)){
  predPlotData <- predBornAdult[predBornAdult$dv_name==name_flag[i],]
  
  pg_list_cohort[[i]] <- ggplot(predPlotData, aes(x=born_adult, 
                                                  y=fit,
                                                  fill = as.factor(left),
                                                  linetype = as.factor(left),
                                                  color = as.factor(left))) + 
    geom_errorbar(aes(ymin = LL, ymax = UL), alpha = 0.2) +
    geom_point(alpha = 0.1, size = 1) + 
    geom_smooth(aes(y = fit), se = F, alpha = 0.5) + 
    geom_smooth(aes(y = LL), se = F, alpha = 0.5, linetype = "solid", size = 0.5) + 
    geom_smooth(aes(y = UL), se = F, alpha = 0.5, linetype = "solid", size = 0.5) + 
    ggtitle("")
  
  pg_list_cohort[[i]] <- pg_list_cohort[[i]] + 
    scale_x_continuous(name = "Year in which respondents turned 18", 
                       breaks = c(1925, 1950, 1975, 2000, 2025)) + 
    scale_y_continuous(name = "Mean attitudes across cohorts", 
                       limits = c(-0.05,1.05)) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          text = element_text(size=9), 
          axis.text = element_text(size = 10), 
          legend.position = "none") + 
    geom_vline(xintercept = 1990, 
               linetype = "dashed", 
               color = "red", 
               size = 0.5)
  
  
}


###Period


pg_list_period <- list()

name_flag_plots <- c("Support for the idea of democracy", "Pluralist differences of interest groups", "Pluralist differences of parties", "Freedom of expression", "Prioritizing individual interests", "Political opposition", "Multi-party competition", "Evaluation of regime performance")



for(i in 1:length(name_flag)){
  
  predPlotData <- predPeriod[predPeriod$dv_name==name_flag[i],]
  
  pg_list_period[[i]] <- ggplot(predPlotData, aes(x=as.Date(period.f, format="%Y"),
                                                  y=fit, 
                                                  group = left,
                                                  colour = as.factor(left), 
                                                  linetype = as.factor(left))) + 
    geom_errorbar(aes(ymin = LL, ymax = UL)) +
    geom_point() + 
    geom_smooth(se = F, 
                method = "gam", formula = y ~ s(x, k = 3),
                size = 1) + 
    ggtitle(paste(name_flag_plots[i]))
  
  pg_list_period[[i]] <- pg_list_period[[i]] + 
    scale_x_date(name = "Year in which survey was conducted", 
                 limits = c(as.Date("1980-01-01", format="%Y"), c(as.Date("2020-01-01", format="%Y")))) + 
    scale_y_continuous(name = "Mean attitudes across periods", 
                       limits = c(-0.05,1.05)) + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          text = element_text(size=9), 
          axis.text = element_text(size = 10), 
          legend.position = "none")
  
}

pg_list_period[[1]] <- pg_list_period[[1]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[1]] <- pg_list_cohort[[1]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))

pg_list_period[[5]] <- pg_list_period[[5]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[5]] <- pg_list_cohort[[5]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))

pg_list_period[[2]] <- pg_list_period[[2]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[2]] <- pg_list_cohort[[2]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))

pg_list_period[[6]] <- pg_list_period[[6]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))
pg_list_cohort[[6]] <- pg_list_cohort[[6]] + scale_y_continuous(name = "", limits = c(-0.05,1.05))


pg_list_cohort[[8]] <- pg_list_cohort[[8]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("Right", "Left"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("Right", "Left"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))

pg_list_cohort[[4]] <- pg_list_cohort[[4]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("Right", "Left"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("Right", "Left"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))

pg_list_cohort[[3]] <- pg_list_cohort[[3]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("Right", "Left"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("Right", "Left"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))

pg_list_cohort[[7]] <- pg_list_cohort[[7]] + theme(legend.position = c(0.2,0.2), legend.key = element_rect(fill = NA)) + scale_linetype_manual(values = c("solid", "dashed"),labels = c("Right", "Left"), name = "") + scale_color_manual(values = c("#F8766D", "#00BFC4"),labels = c("Right", "Left"), name = "") + guides(fill = F, color = F, linetype = guide_legend(override.aes = list(color = c("#F8766D", "#00BFC4"))))



#Figure 1
tiff(file=paste0("plots/Appendix/figure1_satisfaction_idea_OVA_LR.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[8]],  pg_list_period[[1]], pg_list_cohort[[8]],  pg_list_cohort[[1]], nrow = 2))

dev.off()

#Figure 2
tiff(file=paste0("plots/Appendix/figure2_libDem_expre_prerogative_OVA_LR.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[4]],  pg_list_period[[5]],  pg_list_cohort[[4]],  pg_list_cohort[[5]], nrow = 2))

dev.off()

#Figure 3
tiff(file=paste0("plots/Appendix/figure3_libDem_pluralism_OVA_LR.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[3]],  pg_list_period[[2]],  pg_list_cohort[[3]],  pg_list_cohort[[2]], nrow = 2))

dev.off()

#Figure 4
tiff(file=paste0("plots/Appendix/figure4_libDem_competition_OVA_LR.tiff"), width = 2800, height = 2800, res = 400)

grid.arrange(arrangeGrob(pg_list_period[[7]],  pg_list_period[[6]],  pg_list_cohort[[7]],  pg_list_cohort[[6]], nrow = 2))

dev.off()


name_flag_tables <- c("Support for the idea of democracy", "Pluralist differences of interest groups", "Pluralist differences of parties", "Freedom of expression", "Prioritizing individual interests", "Political opposition", "Multi-party competition", "Evaluation of regime performance")

####Regression tables GAM models
htmlreg(list(model_GAM_Right$fitModel[[8]], model_GAM_Right$fitModel[[1]], model_GAM_Right$fitModel[[4]], model_GAM_Right$fitModel[[5]], model_GAM_Right$fitModel[[3]], model_GAM_Right$fitModel[[2]], model_GAM_Right$fitModel[[7]], model_GAM_Right$fitModel[[6]]), file = "tables/gam_Right_diffuse_OVA.doc", custom.model.names = name_flag_tables, caption = "", single.row = T)

htmlreg(list(model_GAM_Left$fitModel[[8]], model_GAM_Left$fitModel[[1]], model_GAM_Left$fitModel[[4]], model_GAM_Left$fitModel[[5]], model_GAM_Left$fitModel[[3]], model_GAM_Left$fitModel[[2]], model_GAM_Left$fitModel[[7]], model_GAM_Left$fitModel[[6]]), file = "tables/gam_Left_diffuse_OVA.doc", custom.model.names = name_flag_tables, caption = "", single.row = T)








###Plot effect sizes (+ cohort effect)#####

datWest <- list()
datOst <- list()

bWest <- list()
bOst <- list()

for(i in 1:length(name_flag)){
  datWest[[i]] <- model_GAM_West$fitModel[[i]]
  datOst[[i]] <- model_GAM_Ost$fitModel[[i]]
  
  bWest[[i]] <- getViz(datWest[[i]])
  bOst[[i]] <- getViz(datOst[[i]])
  

}

for(i in 1:length(name_flag)){
  png(file=paste0("plots/Appendix/figureAppendix_cohort_effect_west_",name_flag[i],".png"))
  print(plot(bWest[[i]]) + labs(title = name_flag_plots[i], x = "Year the respondent turned 18", y = "Smoothed cohort effect"))
  dev.off()
}

for(i in 1:length(name_flag)){
png(file=paste0("plots/Appendix/figureAppendix_cohort_effect_east_",name_flag[i],".png"))
print(plot(bOst[[i]]) + labs(title = name_flag_plots[i], x = "Year the respondent turned 18", y = "Smoothed cohort effect"))
dev.off()
}





#### EFA ####

#### ** 1- Factor EFA (as reported in Appendix)** ####

## We conduct an EFA to assess whether the factor structur is stable over time in both regions of Germany

#Select Data
dataEFA <- datComplete %>% 
  select(east, period.f, libdem_freeopinion, libdem_generalwill, libdem_criticism, libdem_conflict, libdem_partyopportunity, libdem_opposition) %>% 
  drop_na()

##Dataset without East, Period Variables
dataEFA_Full <- dataEFA %>% 
  select(-c(east, period.f))


# Factor Analysis for full dataset across Germany
## Eigenvalue > 1 only for Factor = 1
## What is reported in the appendix
parallel <- fa.parallel(dataEFA_Full, fm = 'minres', fa = 'fa')
fa.parallel(dataEFA_Full, fm = 'minres', fa = 'fa')


##Factor Analysis in Both Regions of Germany, Factor = 1
commonFactor <- fa(dataEFA_Full,nfactors = 1,rotate = "oblimin",fm="minres")
print(commonFactor)
print(commonFactor$loadings)




# Faktorstruktur getrennt nach Ost/West und Jahr
#1 - Factor Solution
commonFactorSlice <- dataEFA %>% 
  arrange(east, period.f) %>% 
  group_by(east, period.f) %>% 
  group_map(~ fa(.,nfactors = 1,rotate = "oblimin",fm="minres"))

names(commonFactorSlice) <- dataEFA %>% 
  arrange(east, period.f) %>% 
  group_by(east, period.f) %>% 
  slice(1) %>% 
  select(east, period.f) %>% 
  ungroup() %>% 
  mutate(east = if_else(east == 0, "West", "East")) %>% 
  mutate(ePeriod = paste(east, period.f, sep = "_")) %>% 
  pull(ePeriod)

for(i in 1:10){
  print(names(commonFactorSlice)[i])
  print(commonFactorSlice[[i]]$loadings)
}




