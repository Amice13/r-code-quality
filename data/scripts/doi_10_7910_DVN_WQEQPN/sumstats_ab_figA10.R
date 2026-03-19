col1 <- wes_palette(n=5, name="FantasticFox1")[3]
col2 <- wes_palette(n=5, name="FantasticFox1")[4]
col3 <- wes_palette(n=5, name="Rushmore1")[2]

outpath <- "results/"
Rout <- function(name, path = outpath) {
 paste0(path,name)
}

### prepare models
yvars<- c("I(100*epr_link_narrow)","I(100*preg_link)")
cropvar <- "log(1+289000*hance_crops5_sum/area_sqkm)"
pubvar <- "in.soas.23"
##
controls.geo <-c( "TSI_CRU_mean_1901_1920", "malaria_suit_max", "elevation_mean",
                   "precipitation_fao", "temperature_fao","coast_log" ,
                 "agric_suit", "dist_river_nav" )
controls.hist <- c("explorers_log", "dist_jedwab_city_1900", "dist_capital" )
controls.ind <- c("urban" , "sex", "age", "educ","econ_food" )
base.x <-  c(controls.geo,controls.hist, controls.ind)

##sumstats
var.labs <- c(
            TSI_CRU_mean_1901_1920 = "Average TSI",
              malaria_suit_max = "Malaria suitability",
              elevation_mean = "Elevation" ,
              precipitation_fao = "Precipitation" ,
              temperature_fao = "Temperature" ,
              coast_log = "Distance to coast" ,
              agric_suit = "Agricultural suitability" ,
              dist_river_nav =  "Distance to River" ,
              explorers_log = "Distance to explorer route" ,
              dist_jedwab_city_1900 = "Distance to colonial city",
              dist_capital = "Distance to Capital",
              urban = "Urban Area",
             sex = "Sex (individual)",
             age = "Age (individual)" ,
             educ = "Education indicator (individual)",
             econ_food = "Poverty indicator (individual)"
           )


df1 <- subset(ab ,
  select = c( base.x, "in.soas.23_lang", "hance_crops5_dummy_max_lang") ) %>%
  drop_na()
set.cobalt.options(binary = "std")
balt1 <- bal.tab( df1[ , c(base.x)] ,
 treatment = df1$in.soas.23_lang)

balt2 <- bal.tab( df1[ , c(base.x)] ,
 treatment = df1$hance_crops5_dummy_max_lang )
## db
ab_ss_lang <- data.frame(names = var.labs, diff = balt1[[1]]$Diff.Un , Treatment= "In Bibliographies")
ab_ss_lang <- rbind(ab_ss_lang,
      data.frame(names = var.labs, diff = balt2[[1]]$Diff.Un ,
         Treatment = "Cash Crops"))
ab_ss_lang$names2 <- factor(ab_ss_lang$names , levels = var.labs)

ss_plot <- ggplot(data=ab_ss_lang, aes(x= names2 , y=diff, fill = Treatment )) +
  geom_dotplot(binaxis='y', stackdir='center',
            stackratio=1.5, dotsize=1.3, colour = NA) +
  scale_fill_manual(values=c(col1,col2), name = "Treatment")  +
  geom_hline(yintercept=0.1, linetype="dashed", color = col3) +
  geom_hline(yintercept=-0.1, linetype="dashed", color = col3) +
  labs(title = "AB Standardized Mean Differences") +
  ylab("Standardized Mean Differences Treatment - Control") + xlab("") +
  coord_flip() + theme_bw() +
  ggsave(Rout("ab_figA10b.pdf") , width = 8, height = 8)


## location

df2 <- subset(ab ,
  select = c( base.x, "in.soas.23_poly", "hance_crops5_dummy_max_poly") ) %>%
  drop_na()
set.cobalt.options(binary = "std")
balt1 <- bal.tab( df2[ , c(base.x)] ,
 treatment = df2$in.soas.23_poly)

balt2 <- bal.tab( df2[ , c(base.x)] ,
 treatment = df2$hance_crops5_dummy_max_poly )
## db
ab_ss_poly <- data.frame(names = var.labs, diff = balt1[[1]]$Diff.Un , Treatment= "In Bibliographies")
ab_ss_poly <- rbind(ab_ss_poly,
      data.frame(names = var.labs, diff = balt2[[1]]$Diff.Un ,
         Treatment = "Cash Crops"))

ab_ss_poly$names2 <- factor(ab_ss_poly$names , levels = var.labs)

ss_plot <- ggplot(data=ab_ss_poly, aes(x= names2 , y=diff, fill = Treatment )) +
  geom_dotplot(binaxis='y', stackdir='center',
            stackratio=1.5, dotsize=1.3, colour = NA) +
  scale_fill_manual(values=c(col1,col2), name = "Treatment")  +
  geom_hline(yintercept=0.1, linetype="dashed", color = col3) +
  geom_hline(yintercept=-0.1, linetype="dashed", color = col3) +
  labs(title = "AB Standardized Mean Differences") +
  ylab("Standardized Mean Differences Treatment - Control") + xlab("") +
  coord_flip() + theme_bw() +
  ggsave(Rout("ab_figA10a.pdf"), width = 8, height = 8)


# ### Outcome response AB
#
# ## ethnic
# ab$ethnicity_clean <- tolower(ab$ethnicity_clean)
# ab$ethnicity_clean <- ifelse(
#   grepl("regional origin", ab$ethnicity_clean),
#   "related to region",
#   ab$ethnicity_clean
# )
# eth.sal <- subset(ab, select = c("ethnicity_clean" , "subj_ethid_o" ) , subset = !is.na(subj_ethid_o) )  %>%
#   group_by(ethnicity_clean , subj_ethid_o ) %>%
#   summarize( count.sal = n())
#
# eth <-  subset(ab, select = c("ethnicity_clean"   ),  subset = !is.na(subj_ethid_o) )  %>%
#   group_by(ethnicity_clean  ) %>%
#   summarize( count.eth = n())
#
# df <- left_join(eth.sal, eth, by = "ethnicity_clean"  ) %>%
#   subset(, ethnicity_clean != "")
#
# df$shares <- df$count.sal/df$count.eth
# df <- drop_na(df)
#
# df$id <- factor(df$subj_ethid_o,
#   labels = c("Ethnic only", "More Ethnic", "Equal", "More Nation", "Nation only"))
#
# # Reorder
# df <- df %>% arrange(id, desc(shares))
# df$facteth1 <- ordered(df$ethnicity_clean, levels =unique(df$ethnicity_clean))
#
#
# ## country
# iso.sal <- subset(ab, select = c("country_name" , "subj_ethid_o" ),  subset = !is.na(subj_ethid_o) )  %>%
#   group_by(country_name , subj_ethid_o ) %>%
#   summarize( count.sal = n())
#
# iso <-  subset(ab, select = c("country_name"   ), subset = !is.na(subj_ethid_o) )  %>%
#   group_by(country_name  ) %>%
#   summarize( count.iso = n())
#
# iso <- left_join(iso.sal, iso, by = "country_name"  ) %>%
#   drop_na()
#
# iso$shares <- iso$count.sal/iso$count.iso
# iso$id <- factor(iso$subj_ethid_o,
#   labels = c("Ethnic only", "More Ethnic", "Equal", "More Nation", "Nation only"))
#
# #### plots
#
# wescols <- wes_palette(n=5, name="Zissou1")[1:5]
#
#
#
# k<- 1
# df <- arrange(df,ethnicity_clean)
# ethnic <- unique(df$ethnicity_clean)
# N <- length(ethnic)
#
# for (i in 1:10 ){
# j <-  192*i
# df.sub <- df %>% subset( ethnicity_clean %in% ethnic[k:j])
# plot <- ggplot(df.sub, aes(fill=id, y=shares, x=ethnicity_clean) ) +
#     geom_bar(position="fill", stat="identity" )  +
#     scale_fill_manual(values = wescols[1:5], name = "Indentity choice") +
#     theme_bw() + theme(text = element_text(size=9),
#    legend.position = "bottom" ,
#    legend.text = element_text(size=11) ) +
#     xlab("Ethnicity") + ylab("Shares of identity choices") +
#     coord_flip() + ggsave(Rout(paste0("sumstats_ab_outcome", i, ".pdf")), width = 8, height =13)
# k <- 1+j
# }
#
# plot <- ggplot(iso, aes(fill=id, y=shares, x=country_name) ) +
#     geom_bar(position="fill", stat="identity" )  +
#     scale_fill_manual(values = wescols[1:5], name = "Identity choice") +
#     theme(
#     legend.position = "bottom" ,
#     legend.text = element_text(size=11),
#     axis.text.x=element_text(size = 10 , angle = 25),
#     axis.ticks.x=element_blank() ,
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank()) +
#     xlab("Country") + ylab("Shares of identity choices") +
#      ggsave(Rout(paste0("sumstats_ab_outcome", "_iso", ".pdf")), width = 15, height =7.5)
#
# plot <- ggplot(df, aes(fill=id, y=shares, x=facteth1) ) +
#     geom_bar(position="fill", stat="identity" )  +
#     scale_fill_manual(values = wescols[1:5], name = "Identity choice") +
#     theme(
#     legend.position = "bottom" ,
#     legend.text = element_text(size=11),
#     axis.text.x=element_blank(),
#     axis.ticks.x=element_blank()) +
#     xlab("Ethnicity") + ylab("Shares of identity choices") +
#      ggsave(Rout(paste0("sumstats_ab_outcome", "_all", ".pdf")), width = 15, height =7.5)
