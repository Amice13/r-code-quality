## Functions
source("regression_functions.R")
tab.path <- "results/"
outpath <- "results/"
Rout <- function(name, path = outpath) {
 paste0(path,name)
}
#####


crops <- c("hance_crops5_dummy_max_lang", "crop_lang")
pubvar <- c("pubspc.23_lang", "pubspc.soas_lang", "in.soas.23_lang", "in.chr23_lang")
controls.ind <- c("sex", "age", "urban", "high_educ", "pov" , "eth.stayer"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
"TSI_CRU_mean_1901_1920" , "temperature_fao" ,  "coast_log"  , "rivers_log" ,
"latitude", "agric_suit", "latitude")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log" , "dist.cath_log"  )
nolog <- c("dist_capital_col" , "dist_capital" , "dist_jedwab_city_1900" , "dist_coast"  ,
 "dist_river_nav" , "dist.prot", "dist.cath","dist_expl_route" )
base.x <-  c(controls.ind, controls.geo,controls.hist, nolog)

tots <- c("country_iso3c", "round", "pres_party_id" , "language_clean" ,
   crops, pubvar, controls.ind, controls.geo, controls.hist, nolog)
ab.bloc <- subset(ab[, c( tots) ] , !is.na(pres_party_id))


ab.vote <- ab.bloc %>% group_by(round, country_iso3c,  language_clean, pres_party_id) %>%
  tally( name = "tot_votes") %>% group_by(round, country_iso3c , language_clean) %>% mutate(tots.eth = sum(tot_votes)) %>%
  mutate(shares = tot_votes/tots.eth , shares2 = (tot_votes/tots.eth)^2) %>% group_by(round , country_iso3c, language_clean) %>%
  mutate(HH = sum(shares2) , sanity = sum(shares)) %>% mutate(F = 1- HH)

ab.vote$ones <- 1
ab.norm <- subset(ab.vote, select=c("round", "country_iso3c", "pres_party_id", "ones")) %>%
group_by(round, country_iso3c, pres_party_id) %>% summarise_at("ones", min) %>%
group_by(round, country_iso3c) %>% tally(name = "tot_parties")

ab.vote <- left_join(ab.vote, ab.norm , by = c("round", "country_iso3c"))
ab.vote$HH.norm <- (ab.vote$HH - (1/ab.vote$tot_parties))/(1-(1/ab.vote$tot_parties))

ab.vote <- ab.vote[,c("round" , "country_iso3c","language_clean","HH", "HH.norm", "tot_parties" , "tots.eth")] %>%
group_by(round, country_iso3c, language_clean) %>%
summarise_at(c("HH", "HH.norm", "tot_parties", "tots.eth") , mean , na.rm = T)

ab.vote <- subset(ab.vote, tots.eth > 10 )

ab.xs <- ab.bloc[, c("round", "country_iso3c", "language_clean" ,
   crops, pubvar, controls.ind, controls.geo, controls.hist, nolog)] %>%
   group_by(round, country_iso3c, language_clean) %>%
   summarise_all(mean, na.rm = T)

ab.bloc <- left_join(ab.vote, ab.xs , by = c("round", "country_iso3c", "language_clean"))

# ### partial correlation plots
### plots
colcrop <- wes_palette(n=5, name="Darjeeling1")[5]
collang <- wes_palette(n=5, name="Darjeeling1")[4]
colline <-  wes_palette(n=5, name="Darjeeling2")[1]

## all sample
my.crop <- lm(data = ab.bloc, HH.norm ~ pubspc.23_lang + tots.eth + tot_parties  +
  sex + age + high_educ + pov + eth.stayer +
  ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
  dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route +
  factor(country_iso3c)  + factor(round) ,
na.action=na.exclude )
predHH.crop.eps <-   ab.bloc$HH.norm -predict(my.crop)

mx.crop <- lm(data = ab.bloc, crop_lang ~ pubspc.23_lang  + tots.eth + tot_parties  +
  sex + age + high_educ + pov + eth.stayer +
  ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
  dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route + dist_river_nav +
  factor(country_iso3c)  + factor(round)  ,
na.action=na.exclude )
pred.crop.eps <-   ab.bloc$crop_lang -predict(mx.crop)

my.lang <- lm(data = ab.bloc, HH.norm ~ crop_lang  + tots.eth + tot_parties  +
   sex + age + high_educ + pov + eth.stayer +
 ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
 dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route + dist_river_nav +
  factor(country_iso3c)  + factor(round) ,
na.action=na.exclude )
predHH.lang.eps <-   ab.bloc$HH.norm -predict(my.lang)

mx.lang <- lm(data = ab.bloc,  pubspc.23_lang ~ crop_lang  + tots.eth + tot_parties  +
  sex + age + high_educ + pov + eth.stayer +
  ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
  dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route + dist_river_nav +
  factor(country_iso3c)  + factor(round) ,
na.action=na.exclude )
pred.lang.eps <-   ab.bloc$pubspc.23_lang -predict(mx.lang)

preds <- data.frame(pred.crop.eps, pred.lang.eps, predHH.crop.eps, predHH.lang.eps)


plot1 <- ggplot(data = subset(preds,   pred.lang.eps > -0.5  ) ,
 aes(y = predHH.lang.eps , x = pred.lang.eps)) +
 geom_point( color = collang , alpha = 0.7, stroke =0   ) +
 geom_smooth(method='lm' , colour =colline , alpha =0.3 , size =0.6) +
 theme_bw() + theme(panel.grid.minor = element_blank()) +labs(
   x = "Publications pth (1923) - Residuals" ,
   y = "Bloc Voting - HH Index - Residuals ",
   title = "Bloc Voting Partial Residuals Plot",
   caption = "95% CI"
 ) + ggsave(Rout("ab_figA40b.pdf") , height = 5 , width = 5)

 plot2 <- ggplot(data = subset(preds, pred.crop.eps > - 5*10^5  & pred.crop.eps < 6*10^5  ) ,
  aes(y = predHH.crop.eps , x = pred.crop.eps)) +
  geom_point( color = colcrop , alpha = 0.7, stroke =0   ) +
  geom_smooth(method='lm'  , colour =colline , alpha =0.3 , size =0.7) +
  theme_bw() + theme(panel.grid.minor = element_blank()) +labs(
    x = "Cash Crop 1000 USD - Residuals" ,
    y = "Bloc Voting - HH Index - Residuals ",
    title = "Bloc Voting Partial Residuals Plot",
    caption = "95% CI"
  ) + ggsave(Rout("ab_figA40a.pdf") , height = 5 , width = 5)



## In bibliographies
ab.bloc2 <- subset(ab.bloc, in.soas.23_lang == 1)


my.crop <- lm(data = ab.bloc2, HH.norm ~ pubspc.23_lang + tots.eth + tot_parties  +
  sex + age + high_educ + pov + eth.stayer +
  ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
  dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route +
  factor(country_iso3c)  + factor(round) ,
na.action=na.exclude )
predHH.crop.eps <-   ab.bloc$HH.norm -predict(my.crop)

mx.crop <- lm(data = ab.bloc2, crop_lang ~ pubspc.23_lang  + tots.eth + tot_parties  +
  sex + age + high_educ + pov + eth.stayer +
  ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
  dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route + dist_river_nav +
  factor(country_iso3c)  + factor(round)  ,
na.action=na.exclude )
pred.crop.eps <-   ab.bloc$crop_lang -predict(mx.crop)

my.lang <- lm(data = ab.bloc2, HH.norm ~ crop_lang  + tots.eth + tot_parties  +
   sex + age + high_educ + pov + eth.stayer +
 ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
 dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route + dist_river_nav +
  factor(country_iso3c)  + factor(round) ,
na.action=na.exclude )
predHH.lang.eps <-   ab.bloc$HH.norm -predict(my.lang)

mx.lang <- lm(data = ab.bloc2,  pubspc.23_lang ~ crop_lang  + tots.eth + tot_parties  +
  sex + age + high_educ + pov + eth.stayer +
  ruggedness_nunn_puga + elevation_mean +  malaria_suit_max + TSI_CRU_mean_1901_1920 + temperature_fao + latitude +
  dist_coast + dist_capital_col + dist.prot + dist.cath + dist_expl_route + dist_river_nav +
  factor(country_iso3c)  + factor(round) ,
na.action=na.exclude )
pred.lang.eps <-   ab.bloc$pubspc.23_lang -predict(mx.lang)

preds <- data.frame(pred.crop.eps, pred.lang.eps, predHH.crop.eps, predHH.lang.eps)

plot3 <- ggplot(data = subset(preds,   pred.lang.eps > -0.5 ) ,
 aes(y = predHH.lang.eps , x = pred.lang.eps)) +
 geom_point( color = collang , alpha = 0.7, stroke =0   ) +
 geom_smooth(method='lm' , colour =colline , alpha =0.3 , size =0.6) +
 theme_bw() + theme(panel.grid.minor = element_blank()) +labs(
   x = "Publications pth (1923) - Residuals" ,
   y = "Bloc Voting - HH Index - Residuals ",
   title = "Bloc Voting Partial Residuals Plot",
   caption = "In Bibliogrpahies, 95% CI"
 ) + ggsave(Rout("ab_figA40d.pdf") , height = 5 , width = 5)

 plot4 <- ggplot(data = subset(preds, pred.crop.eps > - 5*10^5  & pred.crop.eps < 6*10^5  ) ,
  aes(y = predHH.crop.eps , x = pred.crop.eps)) +
  geom_point( color = colcrop , alpha = 0.7, stroke =0   ) +
  geom_smooth(method='lm'  , colour =colline , alpha =0.3 , size =0.7) +
  theme_bw() + theme(panel.grid.minor = element_blank()) +labs(
    x = "Cash Crop 1000 USD - Residuals" ,
    y = "Bloc Voting - HH Index - Residuals ",
    title = "Bloc Voting Partial Residuals Plot",
    caption = "In Bibliographies, 95% CI"
  ) + ggsave(Rout("ab_figA40c.pdf") , height = 5 , width = 5)
