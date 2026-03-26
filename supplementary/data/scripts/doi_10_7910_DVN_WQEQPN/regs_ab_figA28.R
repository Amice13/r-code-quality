## prelims
source("regression_functions.R")
tab.path <- "results/"
fig.path <- "results/"
figname <- paste0(fig.path, "ab_figA28.pdf")

#####
#####

yvars1 <- c( "identity_more_ethnic")
yvars <- paste0(yvars1, "_sc")

## above median | treat > 0
ab$highpub_poly <- ifelse(  ab$pubspc.23_poly > quantile(ab$pubspc.23_poly[ab$pubspc.23_poly > 0], na.rm = T, 0.5), 1 , 0 )
ab$highpub_lang <- ifelse(  ab$pubspc.23_lang > quantile(ab$pubspc.23_lang[ab$pubspc.23_lang > 0], na.rm = T, 0.5), 1 , 0 )
ab$highcrop_poly <- ifelse(  ab$crop_poly > quantile(ab$crop_poly[ab$crop_poly >0], na.rm = T, 0.5), 1 , 0 )
ab$highcrop_lang  <- ifelse( ab$crop_lang > quantile(ab$crop_lang[ab$crop_lang > 0] , na.rm = T, 0.5), 1 , 0 )

## treat == 0
ab$zeropub_poly <- ifelse(  ab$pubspc.23_poly == 0 , 1 , 0 )
ab$zeropub_lang <- ifelse(  ab$pubspc.23_lang  == 0 , 1 , 0 )
ab$zerocrop_poly <- ifelse(  ab$crop_poly == 0 , 1 , 0 )
ab$zerocrop_lang  <- ifelse( ab$crop_lang == 0 , 1 , 0 )

## below median | treat < 0 & above 0
ab$lowpub_poly <- ifelse(  ab$pubspc.23_poly < quantile(ab$pubspc.23_poly[ab$pubspc.23_poly > 0], na.rm = T, 0.5), 1-ab$zeropub_poly , 0 )
ab$lowpub_lang <- ifelse(  ab$pubspc.23_lang < quantile(ab$pubspc.23_lang[ab$pubspc.23_lang > 0], na.rm = T, 0.5), 1- ab$zeropub_lang , 0 )
ab$lowcrop_poly <- ifelse(  ab$crop_poly < quantile(ab$crop_poly[ab$crop_poly >0], na.rm = T, 0.5), 1 - ab$zerocrop_poly , 0 )
ab$lowcrop_lang  <- ifelse( ab$crop_lang < quantile(ab$crop_lang[ab$crop_lang > 0] , na.rm = T, 0.5), 1 - ab$zerocrop_lang , 0 )

ab$int.high.pub.lang <- ab$pubspc.23_lang * ab$highcrop_lang
ab$int.low.pub.lang <- ab$pubspc.23_lang * ab$lowcrop_lang
ab$int.zero.pub.lang <- ab$pubspc.23_lang * ab$zerocrop_lang

ab$int.high.pub.poly <- ab$pubspc.23_poly * ab$highcrop_poly
ab$int.low.pub.poly <- ab$pubspc.23_poly * ab$lowcrop_poly
ab$int.zero.pub.poly <- ab$pubspc.23_poly * ab$zerocrop_poly

ab$int.high.crop.lang <- ab$crop_lang * ab$highpub_lang
ab$int.low.crop.lang <- ab$crop_lang * ab$lowpub_lang
ab$int.zero.crop.lang <- ab$crop_lang * ab$zeropub_lang

ab$int.high.crop.poly <- ab$crop_poly * ab$highpub_poly
ab$int.low.crop.poly <- ab$crop_poly * ab$lowpub_poly
ab$int.zero.crop.poly <- ab$crop_poly * ab$zeropub_poly


treat.pub.lang <- c("int.high.pub.lang", "int.low.pub.lang", "int.zero.pub.lang", "highcrop_lang", "lowcrop_lang", "zerocrop_lang", "crop_lang")
treat.pub.poly <- c("int.high.pub.poly", "int.low.pub.poly", "int.zero.pub.poly", "highcrop_poly", "lowcrop_poly", "zerocrop_poly", "crop_poly")
treat.crop.lang <- c("int.high.crop.lang", "int.low.crop.lang", "int.zero.crop.lang", "highpub_lang", "lowpub_lang", "zeropub_lang", "pubspc.23_lang")
treat.crop.poly <- c("int.high.crop.poly", "int.low.crop.poly", "int.zero.crop.poly", "highpub_poly", "lowpub_poly", "zeropub_poly", "pubspc.23_poly")

lab.pub <- c(paste("Publications x",  c("1.High Crops", "1.Low Crops", "1.Zero Crops")), c("1.High Crops", "1.Low Crops", "1.Zero Crops"), "Crops")
lab.crop <- c(paste("Crops x",  c("1.High Publications", "1.Low Publications", "1.Zero Publications")), c("1.High Publications", "1.Low Publications", "1.Zero Publications"), "Publications")

## Still need to scale them

controls.ind <- c("sex", "age", "age2" , "f.educ", "f.water",
                  "f.food", "f.income", "f.health",  "ea_sex", "ea_age"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
                  "TSI_CRU_mean_1901_1920" , "temperature_fao" ,  "coast_log"  ,
                  "latitude", "agric_suit", "latitude")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log"  )

i <- 1
base.x1 <- controls.ind
base.x2 <- c(controls.ind, controls.geo, controls.hist)

m1 <- felm(as.formula(RegFor( y = yvars[i] , x = c( treat.crop.poly, base.x2) ,
                              FE = "country_survey_round" , IV="0", clust = "loc.id" )),
           data=ab  )

m2 <- felm(as.formula(RegFor( y = yvars[i] , x = c( treat.pub.poly, base.x2) ,
                               FE = "country_survey_round" , IV="0", clust = "loc.id" )),
            data=ab  )

m3 <- felm(as.formula(RegFor( y = yvars[i] , x = c( treat.crop.lang, base.x1) ,
                              FE = "loc.id" , IV="0", clust = "loc.id" )),
           data=ab  )

m4<- felm(as.formula(RegFor( y = yvars[i] , x = c( treat.pub.lang, base.x1) ,
                             FE = "loc.id" , IV="0", clust = "loc.id" )),
          data=ab  )

## coefs
out1 <-tibble(coef(m1)[treat.crop.poly], m1$se[treat.crop.poly])
out1$names <- treat.crop.poly
names(out1) <- c("beta","se","variable")
out1$betalo <- out1$beta - 1.65*out1$se
out1$betahi <- out1$beta + 1.65*out1$se
out1$lab <- lab.crop
out1$spec <- "Geographic Level"
out1$mgal <- "Interacted Marginal Effect of Cash Crops"

out2 <-tibble(coef(m2)[treat.pub.poly], m2$se[treat.pub.poly])
out2$names <- treat.pub.poly
names(out2) <- c("beta","se","variable")
out2$betalo <- out2$beta - 1.65*out2$se
out2$betahi <- out2$beta + 1.65*out2$se
out2$lab <- lab.pub
out2$spec <- "Geographic Level"
out2$mgal <- "Interacted Marginal Effect of Publications"

out3 <-tibble(coef(m3)[treat.crop.lang], m3$se[treat.crop.lang])
out3$names <- treat.crop.lang
names(out3) <- c("beta","se","variable")
out3$betalo <- out3$beta - 1.65*out3$se
out3$betahi <- out3$beta + 1.65*out3$se
out3$lab <- lab.crop
out3$spec <- "Ethnic Level"
out3$mgal <- "Interacted Marginal Effect of Cash Crops"

out4 <-tibble(coef(m4)[treat.pub.lang], m4$se[treat.pub.lang])
out4$names <- treat.crop.poly
names(out4) <- c("beta","se","variable")
out4$betalo <- out4$beta - 1.65*out4$se
out4$betahi <- out4$beta + 1.65*out4$se
out4$lab <- lab.pub
out4$spec <- "Ethnic Level"
out4$mgal <- "Interacted Marginal Effect of Publications"

df.geo <- rbind(out1, out2)
df.eth <- rbind(out3, out4)

df<- rbind(df.geo, df.eth)
df$reg <- paste( df$spec, "-", df$mgal)

df$order <- (-1)* (cumsum(grepl(" x " , df$lab))/10 + c(rep(0,7), rep(0.02,7), rep(0.04,7), rep(0.06,7)))

forplot <- subset(df, grepl(" x " , df$lab))
###
col <- c(wes_palette("GrandBudapest1")[2:3], wes_palette("Cavalcanti1")[2:3])
plot <- ggplot(forplot, aes(x =beta, y = order, col = reg)) +
  geom_pointrange(aes(xmin=betalo, xmax = betahi, shape = reg) , size = 1) +
  geom_vline(xintercept=0,linetype="dotted", size=0.2) +
  scale_color_manual(values = col, name = "Specification") +
  scale_shape_manual(values = c(0,5,4,8), name = "Specification") +
  scale_y_continuous(breaks=forplot$order, labels=forplot$lab)  +
  labs(x = "Coefficient, 95% Confidence Interval", y = "  ",
       title="AB regressions - Ethnic vs National ID",
       subtitle="Interactions with group size among ethnic leavers") +
  theme_minimal(base_size = 16 )+
  theme(axis.text.y = element_text(size=16),  legend.position = "bottom")

plot <- plot + guides(color=guide_legend(ncol=1,nrow=4,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=4,byrow=TRUE),
           shape=guide_legend(ncol=1,nrow=4,byrow=TRUE))
plot <- plot + ggsave(figname , height = 8 , width = 10)
