## prelims
source("regression_functions.R")
tab.path <- "results/"
fig.path <- "results/"
figname <- paste0(figpath, "ab_figA20.pdf")

#####
#####
yvars1 <- "identity_more_ethnic"
yvars <- paste0(yvars1, "_sc")
cropvar <- c( "crop_lang_sc")
croplab <- c("Cash crops USD pkm2")
pubvar <- c("pubspc.23_lang_sc")
publab <- c("Pubs pth pop (1923)")

controls.ind <- c("sex", "age", "age2" , "f.educ", "f.water",
 "f.food", "f.income", "f.health",  "ea_sex", "ea_age"  )


base.x1 <- c(controls.ind, "lpopc_hyde_meanprecol_lang", "larea_sqkm_mean_lang")


model <- felm(as.formula(RegFor( y = yvars , x = c(cropvar, pubvar, base.x1) ,
          FE = "loc.id" , IV="0", clust = "loc.id" )),
          data=ab , subset =  eth.stayer == FALSE   )

coef.lab <- c("crops_lang", "pubs23_lang", "larea_km2")
coef.beta <- coefficients(model)[c(cropvar, pubvar,"larea_sqkm_mean_lang" )]
coef.se <- model$se[c(cropvar, pubvar,"larea_sqkm_mean_lang" )]

out <- data.frame(name = coef.lab, beta = coef.beta, se =coef.se)
out$order <- c(0.1,0.2,0.3)
out$betalo <- out$beta - 1.96*out$se
out$betahi <- out$beta + 1.96*out$se
out$label <- c("Cash crops USD pkm2", "Pubs pth pop (1923)" , "Polygon size km2")

plot <- ggplot(out, aes(x =beta, y = order)) +
  geom_pointrange(aes(xmin=betalo, xmax = betahi) , size = 1) +
  geom_vline(xintercept=0,linetype="dotted", size=0.2) +
  scale_y_continuous(breaks=out$order, labels=out$label)  +
  labs(x = "Coefficient, 95% Confidence Interval", y = "  ",
      title = "AB - Ethnic vs National ID" ,
       subtitle="Ethnic level- Ethnic leavers - Includes polygon size ") +
  theme_minimal(base_size = 16 )+
  theme(axis.text.y = element_text(size=16),  legend.position = "bottom")

plot <- plot + guides(color=guide_legend(ncol=1,nrow=4,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=4,byrow=TRUE),
           shape=guide_legend(ncol=1,nrow=4,byrow=TRUE))

plot <- plot + ggsave(figname , height = 5 , width = 10)
