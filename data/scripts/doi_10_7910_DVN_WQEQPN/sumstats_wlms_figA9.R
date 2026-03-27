col1 <- wes_palette(n=5, name="Rushmore1")[5]
col2 <- wes_palette(n=5, name="Rushmore1")[4]
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

outcomes <- c("epr_link","epr_link_narrow", "preg_link", "preg_link_narrow")

treat <- c("in.chr23", "hance_crops5_dummy")

controls.geo <-c( "TSI_CRU_mean_1901_1920", "malaria_suit_max", "elevation_mean",
                  "dist_river_nav", "precipitation_fao", "temperature_fao","coast_log" ,
                 "agric_suit" )
controls.hist <- c("explorers_log", "dist_jedwab_city_1900", "dist_capital" )

base.x <-  c(controls.geo,controls.hist)

##sumstats
var.labs <- c(epr_link = "Linked in EPR",
              epr_link_narrow = "Linked in EPR, excl.",
              preg_link = "Linked in PREG",
              preg_link_narrow = "Linked in PREG, excl.",
              TSI_CRU_mean_1901_1920 = "Average TSI",
              malaria_suit_max = "Malaria suitability",
              elevation_mean = "Elevation" ,
              dist_river_nav =  "Distance to River" ,
              precipitation_fao = "Precipitation" ,
              temperature_fao = "Temperature" ,
              coast_log = "Distance to coast" ,
              agric_suit = "Agricultural suitability" ,
              explorers_log = "Distance to explorer route" ,
              dist_jedwab_city_1900 = "Distance to colonial city",
              dist_capital = "Distance to Capital")

### Data for ss
df1 <- subset(wlms_data ,
  select = c( outcomes, treat, base.x, "hance_country", "epr_country", "unmatched") ,
  hance_country==1 & unmatched==0 & (epr_country==1 | preg_country ==1) ) %>%
  drop_na()

n1<- dim(df1)[1]

## compute ss
set.cobalt.options(binary = "std")

balt1 <- bal.tab( df[ , c(outcomes, base.x)] ,
 treatment = df1$in.chr23)
balt2 <- bal.tab( df[ , c(outcomes, base.x)] ,
  treatment = df1$hance_crops5_dummy)

## DF of ss
df.ss<- data.frame(names = var.labs, diff = balt1[[1]]$Diff.Un , treatment = "In Bibliographies")
df.ss <- rbind(df.ss,
      data.frame(names = var.labs, diff = balt2[[1]]$Diff.Un ,
         treatment = "Cash Crops"))


ss_plot <- ggplot(data=df.ss, aes(x= names , y=diff, fill = treatment )) +
  geom_dotplot(binaxis='y', stackdir='center',
            stackratio=1.5, dotsize=1.2, colour = NA) +
  scale_fill_manual(values=c(col1,col2), name = "Treatment")  +
  geom_hline(yintercept=0.1, linetype="dashed", color = col3) +
  geom_hline(yintercept=-0.1, linetype="dashed", color = col3) +
  labs(title = "Group Mean Differences (Treat-Control)" ) +
  ylab("Standardized Mean Differences") + xlab("") +
  coord_flip() + theme_bw() + ggsave(Rout("wlms_figA9.pdf"), width = 8 , height = 8)
