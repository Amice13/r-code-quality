#prelims
figpath <- "results/"
figname <- paste0(figpath, "ab_figA19.pdf")

#workings
thresh <- 0.5
ab$mrd.cen.high.lang <- (ab$mrdk_precol_centr_mean_lang >=  quantile(ab$mrdk_precol_centr_mean_lang , thresh, na.rm =T))
ab$pop23.high.lang <- (ab$pops.23_lang >= quantile(ab$pops.23_lang ,thresh, na.rm = T))
ab$area.high.lang <- (ab$area_sqkm_mean_lang >= quantile(ab$area_sqkm_mean_lang , thresh, na.rm = T))

stats1 <- ab %>% group_by(mrd.cen.high.lang ) %>%
    summarise(m = mean(eth.stayer, na.rm = T), stdv = sd(eth.stayer, na.rm =T),
    n =  n())
stats1$sizeindicator <- "Murdock Centralisation"
stats1 <- subset(stats1, !is.na(mrd.cen.high.lang))
names(stats1) <- c("Large", "m", "stdv", "n", "sizeindicator")
stats1$mlow <- stats1$m - 1.96/sqrt(stats1$n)
stats1$mhigh <- stats1$m + 1.96/sqrt(stats1$n)

stats2 <- ab %>% group_by(pop23.high.lang ) %>%
    summarise(m = mean(eth.stayer, na.rm = T), stdv = sd(eth.stayer, na.rm =T), n = n())
stats2$sizeindicator <- "Pop Size 1923"
stats2 <- subset(stats2, !is.na(pop23.high.lang))
names(stats2) <- c("Large", "m", "stdv", "n", "sizeindicator")
stats2$mlow <- stats2$m - 1.96/sqrt(stats2$n)
stats2$mhigh <- stats2$m + 1.96/sqrt(stats2$n)

stats3 <- ab %>% group_by(area.high.lang ) %>%
    summarise(m = mean(eth.stayer, na.rm = T), stdv = sd(eth.stayer, na.rm =T),
    n =  n())
stats3$sizeindicator <- "Polygon Area"
stats3 <- subset(stats3, !is.na(area.high.lang))
names(stats3) <- c("Large", "m", "stdv", "n", "sizeindicator")
stats3$mlow <- stats3$m - 1.96/sqrt(stats3$n)
stats3$mhigh <- stats3$m + 1.96/sqrt(stats3$n)

full <- rbind(stats1, stats2, stats3)
full$order <- c(1,2, 4,5, 7,8)
full$label <- c(paste("Murdock Centralisation", c("Low", "High")),
      paste("Pop size 1923", c("Low", "High")),
      paste("Polygon Area", c("Low", "High")) )


testdf1 <-  subset(ab, !is.na(mrd.cen.high.lang) & !is.na(eth.stayer) , select = c(mrd.cen.high.lang, eth.stayer ))
t.test(eth.stayer ~ mrd.cen.high.lang, data = testdf1)

testdf2 <-  subset(ab, !is.na(pop23.high.lang) & !is.na(eth.stayer) , select = c(pop23.high.lang, eth.stayer ))
t.test(eth.stayer ~ pop23.high.lang, data = testdf2)

testdf3 <-  subset(ab, !is.na(area.high.lang) & !is.na(eth.stayer) , select = c(area.high.lang, eth.stayer ))
t.test(eth.stayer ~ area.high.lang, data = testdf3)


dataplot <- subset(full, sizeindicator != "Pop Size 1923")
dataplot$order <- c(1,2,4,5)

cols <- wes_palette("Royal2", 5)[c(3,5)]
plot1 <- ggplot(dataplot, aes(y = sizeindicator, x=m , fill = Large)) +
    geom_bar(stat="identity", color="white",
             position=position_dodge(), aes(  ), width = 0.7 ) +
    geom_errorbar(aes(xmin=mlow, xmax=mhigh), width=.2,
                  position=position_dodge(.7)) +
  scale_fill_manual(values=cols, name="Large Group") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  labs(x = "Share of Ethnic Stayers", y = "",
       title="Share of Ethnic Stayers by Historical Population Size",
       caption = "pval<0.001 for t-test of mean equality for both Murdock centralisation and Polygon area.") +
 ggsave(figname , height = 12 , width = 9)
rm(dataplot)
