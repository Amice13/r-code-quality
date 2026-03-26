## prelims
source("regression_functions.R")
tab.path <- "results/"
fig.path <- "results/"
figname <- paste0(fig.path, "ab_figA31a.pdf")
figname2 <- paste0(fig.path, "ab_figA31b.pdf")

#####
#####

yvars1 <- c( "identity_more_ethnic")
yvars <- paste0(yvars1, "_sc")

ab$int1 <- ab$crop_poly_sc * ab$isfrcol
ab$int2 <- ab$crop_poly_sc * ab$isukcol
ab$int3 <- ab$crop_poly_sc * (1-ab$isukcol - ab$isfrcol)
ab$int4 <- ab$pubspc.23_poly_sc * ab$isfrcol
ab$int5 <- ab$pubspc.23_poly_sc * ab$isukcol
ab$int6 <- ab$pubspc.23_poly_sc * (1-ab$isfrcol - ab$isukcol)

ab$int1.lang <- ab$crop_lang_sc * ab$isfrcol
ab$int2.lang <- ab$crop_lang_sc * ab$isukcol
ab$int3.lang <- ab$crop_lang_sc * (1-ab$isukcol - ab$isfrcol)
ab$int4.lang <- ab$pubspc.23_lang_sc * ab$isfrcol
ab$int5.lang <- ab$pubspc.23_lang_sc * ab$isukcol
ab$int6.lang <- ab$pubspc.23_lang_sc * (1-ab$isfrcol - ab$isukcol)


ints <- paste0(c("int"),1:6)
ints.lang <- paste0(c("int"),1:6, ".lang")

controls.ind <- c("sex", "age", "age2", "f.educ",  "ea_sex", "ea_age" , "ea_school", "ea_clinic"  )
controls.geo <- c("ruggedness_nunn_puga" ,  "elevation_mean"  ,  "malaria_suit_max",
"TSI_CRU_mean_1901_1920" , "temperature_fao" ,  "coast_log"  ,
"latitude", "agric_suit", "latitude")
controls.hist <- c("explorers_log", "cities_log", "capital_log" , "dist.prot_log" , "dist.print_log"  )

i <- 1

base.x1 <- c(controls.ind, controls.geo, controls.hist)
base.x2 <- controls.ind
# controls.geo,controls.hist)



m1 <- felm(as.formula(RegFor( y = yvars[i] , x = c( ints, base.x1) ,
          FE = "country_survey_round" , IV="0", clust = "loc.id" )),
          data=ab  )
m2 <- felm(as.formula(RegFor( y = yvars[i] , x = c( ints.lang, base.x2) ,
          FE = "loc.id" , IV="0", clust = "loc.id" )),
          data=ab  )
## coefs
out <-tibble(coef(m1)[ints], m1$se[ints])
names(out) <- c("beta","se")
out$names <- c("crop_fr", "crop_uk", "crop_others", "pubs_fr", "pubs_uk", "pubs_others")
out$treatment <- c(rep("Cash Crops",3), rep("Publications",3) )
out$interaction <- rep(c("France", "UK", "Others"),2)
out$label <- paste(out$treatment, "x", out$interaction )
out$betalo <- out$beta - 1.96*out$se
out$betahi <- out$beta + 1.96*out$se

out$order <- (-1)*(1:6/10 + c(rep(0,3), rep(0.02,3)) )

## out.language
## coefs
out.lang <-tibble(coef(m2)[ints.lang], m2$se[ints.lang])
names(out.lang) <- c("beta","se")
out.lang$names <- c("crop_fr", "crop_uk", "crop_others", "pubs_fr", "pubs_uk", "pubs_others")
out.lang$treatment <- c(rep("Cash Crops",3), rep("Publications",3) )
out.lang$interaction <- rep(c("France", "UK", "Others"),2)
out.lang$label <- paste(out$treatment, "x", out$interaction )
out.lang$betalo <- out.lang$beta - 1.96*out.lang$se
out.lang$betahi <- out.lang$beta + 1.96*out.lang$se

out.lang$order <- (-1)*(1:6/10 + c(rep(0,3), rep(0.02,3)) )

###

col <- wes_palette("BottleRocket2")[c(2,1,3)]
plot <- ggplot(out, aes(x =beta, y = order, col = interaction)) +
  geom_pointrange(aes(xmin=betalo, xmax = betahi, shape = interaction) , size = 1 ) +
  geom_vline(xintercept=0,linetype="dotted", size=0.2) +
  scale_color_manual(values = col, name = "Interaction") +
  scale_shape_manual(values = c(0,5,4), name = "Interaction") +
  scale_y_continuous(breaks=out$order, labels=out$label) +
  labs(x = "Coefficient, 95% Confidence Interval", y = "Treatment Variable",
       title="Interactive Effects?",
       subtitle="Location-level AB effects on Ethnic vs National ID") +
  theme_minimal(base_size = 16 )+
  theme(axis.text.y = element_text(size=16),  legend.position = "bottom", legend.box = "vertical")
plot <- plot + guides(color=guide_legend(ncol=,nrow=1,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE),
           shape=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
           ggsave(figname , height = 10 , width = 10)

plot <- ggplot(out.lang, aes(x =beta, y = order, col = interaction)) +
  geom_pointrange(aes(xmin=betalo, xmax = betahi, shape = interaction) , size = 1 ) +
  geom_vline(xintercept=0,linetype="dotted", size=0.2) +
  scale_color_manual(values = col, name = "Interaction") +
  scale_shape_manual(values = c(0,5,4), name = "Interaction") +
  scale_y_continuous(breaks=out.lang$order, labels=out.lang$label) +
  labs(x = "Coefficient, 95% Confidence Interval", y = "Treatment Variable",
       title="Interactive Effects?",
       subtitle="Ethnic-level AB effects on Ethnic vs National ID") +
  theme_minimal(base_size = 16 )+
  theme(axis.text.y = element_text(size=16),  legend.position = "bottom", legend.box = "vertical")
plot <- plot + guides(color=guide_legend(ncol=,nrow=1,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE),
           shape=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
           ggsave(figname2 , height = 10 , width = 10)
