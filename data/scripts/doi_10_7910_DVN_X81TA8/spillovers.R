cuts <- seq(0,250,25)

data.g25$dist_crop_bins_25 <- 0

for (i in c(1:(length(cuts)-1))){
  data.g25$dist_crop_bins_25 <- ifelse(data.g25$dist_hance_crop >= cuts[i] & 
                                              data.g25$dist_hance_crop < cuts[i + 1],
                                            i,data.g25$dist_crop_bins_25)
}



data.g25$dist_mineral_bins_25 <- 0

for (i in c(1:(length(cuts)-1))){
  data.g25$dist_mineral_bins_25 <- ifelse(data.g25$dist_hance_mineral >= cuts[i] & 
                                                 data.g25$dist_hance_mineral < cuts[i + 1],
                                               i,data.g25$dist_mineral_bins_25)
}




out.vars <- c("road_dens_2015_log","urb_dens_2015_log","nl_2015_pc_log","hh_wealth")

out.ls <- lapply(out.vars,function(y){
  out <- mdlr_dummy_plus(y=y, x="factor(dist_crop_bins_25) + factor(dist_mineral_bins_25)", 
                         cntr=controls.std, fe="iso3c_col", iv=NULL, cl="iso3c_col", dat = data.g25, listw=NULL)
})

coef(out.ls[[1]])

lfe::waldtest(out.ls[[1]],grep("crop",names(coef(out.ls[[1]]))))
lfe::waldtest(out.ls[[2]],grep("crop",names(coef(out.ls[[1]]))))


sum.roads <- deltaMethod(out.ls[[1]],"`factor(dist_crop_bins_25)1` + `factor(dist_crop_bins_25)2` + 
              `factor(dist_crop_bins_25)3` + `factor(dist_crop_bins_25)4` + 
              `factor(dist_crop_bins_25)5` + `factor(dist_crop_bins_25)6` + 
              `factor(dist_crop_bins_25)7` + `factor(dist_crop_bins_25)8` + 
              `factor(dist_crop_bins_25)9` + `factor(dist_crop_bins_25)10`")
sum.cities <- deltaMethod(out.ls[[2]],"`factor(dist_crop_bins_25)1` + `factor(dist_crop_bins_25)2` + 
              `factor(dist_crop_bins_25)3` + `factor(dist_crop_bins_25)4` + 
              `factor(dist_crop_bins_25)5` + `factor(dist_crop_bins_25)6` + 
              `factor(dist_crop_bins_25)7` + `factor(dist_crop_bins_25)8` + 
              `factor(dist_crop_bins_25)9` + `factor(dist_crop_bins_25)10`")
sum.nl <- deltaMethod(out.ls[[3]],"`factor(dist_crop_bins_25)1` + `factor(dist_crop_bins_25)2` + 
              `factor(dist_crop_bins_25)3` + `factor(dist_crop_bins_25)4` + 
              `factor(dist_crop_bins_25)5` + `factor(dist_crop_bins_25)6` + 
              `factor(dist_crop_bins_25)7` + `factor(dist_crop_bins_25)8` + 
              `factor(dist_crop_bins_25)9` + `factor(dist_crop_bins_25)10`")
sum.wealth <- deltaMethod(out.ls[[4]],"`factor(dist_crop_bins_25)1` + `factor(dist_crop_bins_25)2` + 
              `factor(dist_crop_bins_25)3` + `factor(dist_crop_bins_25)4` + 
              `factor(dist_crop_bins_25)5` + `factor(dist_crop_bins_25)6` + 
              `factor(dist_crop_bins_25)7` + `factor(dist_crop_bins_25)8` + 
              `factor(dist_crop_bins_25)9` + `factor(dist_crop_bins_25)10`")


sum.df <- rbind(sum.roads,sum.cities,sum.nl,sum.wealth)
sum.df$t.val <- sum.df$Estimate/sum.df$SE
sum.df$p.val <- NA
sum.df$p.val[1] <- 2*pt(-abs(sum.df$t.val[1]),df=out.ls[[1]]$df)
sum.df$p.val[2] <- 2*pt(-abs(sum.df$t.val[2]),df=out.ls[[2]]$df)
sum.df$p.val[3] <- 2*pt(-abs(sum.df$t.val[3]),df=out.ls[[3]]$df)
sum.df$p.val[4] <- 2*pt(-abs(sum.df$t.val[4]),df=out.ls[[4]]$df)
sum.df$p.val <- round(sum.df$p.val,3)


plot.ls <- lapply(out.ls,function(mod){
  out <- get_plot_data(mod,grep("bins",names(coef(mod))))
})


plot.df <- do.call(rbind,plot.ls)
out.labs <- c("Road Density 1998 (log)","Urb. Pop. Dens. 2015 (log)","Night Lights 2015 pc (log)","DHS Household Wealth")
plot.df$outcome  <- rep(out.labs,each=20) 
plot.df$outcome <- factor(plot.df$outcome,levels=out.labs)
seq1 <- seq(0,225,25)
seq2 <-seq(25,250,25)
crop_bins <- paste0("Cash Crops (",seq1,"-",seq2," km)")
mineral_bins <- paste0("Minerals (",seq1,"-",seq2," km)")
plot.df$what <- rep(c(crop_bins,mineral_bins),4)

plot.df.crops <- plot.df[grepl("Crops",plot.df$what),] 
plot.df.minerals <- plot.df[grepl("Minerals",plot.df$what),] 

plot.df.crops$order <- rep(c(10:1),4)
plot.df.minerals$order <- rep(c(10:1),4)

p <- ggplot(plot.df.crops)
p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(10:1),minor_breaks = NULL, labels=plot.df.crops$what[c(1:10)],limits = c(0.8,10.1)) + 
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Distance Bins",
       title="Spillover Analysis: Cash Crops",
       subtitle="Distance to Colonial Resources & Contemporary Outcomes") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10)) 
p
p + ggsave(paste0("output/tables_wd/","spillovers_crops.pdf"),width=16,height=5.5)


out.labs2 <- c("Road Density 1998 (log)","Urban Population Density 2015 (log)","Night Lights per capita 2015 (log)","DHS Household Wealth")
i <- 4
p.crops.ls <- lapply(c(1:4),function(i){
  p <- ggplot(plot.df.crops[plot.df.crops$outcome==out.labs[i],])
  p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
    geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
    geom_vline(xintercept=0,linetype="dotted", size=0.6) +
    scale_y_continuous(breaks=c(10:1),minor_breaks = NULL, labels=plot.df.crops$what[c(1:10)],limits = c(0.8,10.1)) + 
    labs(x = "Coefficients and 95% Confidence Intervals", y = "Distance Bins",
         title=out.labs2[i],
         subtitle=paste("Coefficient Sum:",round(sum.df$Estimate[i],3), " p-value:", round(sum.df$p.val[i],3)))  +
    theme_minimal(base_size=10) +  
    theme(axis.text.y = element_text(size=8)) 
  return(p)
})

p.out <- p.crops.ls[[1]] | p.crops.ls[[2]] | p.crops.ls[[3]] | p.crops.ls[[4]]
p.out <- p.out + plot_annotation(title="Spillover Analysis",
                        subtitle="Distance to Colonial Cash Crops & Contemporary Outcomes",
                        theme = theme(plot.title = element_text(size = 20),
                                      plot.subtitle = element_text(size = 16)))

p.out + ggsave(paste0("output/tables_wd/","spillovers_crops_new.pdf"),width=20,height=5.5)





sum.roads <- deltaMethod(out.ls[[1]],"`factor(dist_mineral_bins_25)1` + `factor(dist_mineral_bins_25)2` + 
              `factor(dist_mineral_bins_25)3` + `factor(dist_mineral_bins_25)4` + 
              `factor(dist_mineral_bins_25)5` + `factor(dist_mineral_bins_25)6` + 
              `factor(dist_mineral_bins_25)7` + `factor(dist_mineral_bins_25)8` + 
              `factor(dist_mineral_bins_25)9` + `factor(dist_mineral_bins_25)10`")
sum.cities <- deltaMethod(out.ls[[2]],"`factor(dist_mineral_bins_25)1` + `factor(dist_mineral_bins_25)2` + 
              `factor(dist_mineral_bins_25)3` + `factor(dist_mineral_bins_25)4` + 
              `factor(dist_mineral_bins_25)5` + `factor(dist_mineral_bins_25)6` + 
              `factor(dist_mineral_bins_25)7` + `factor(dist_mineral_bins_25)8` + 
              `factor(dist_mineral_bins_25)9` + `factor(dist_mineral_bins_25)10`")
sum.nl <- deltaMethod(out.ls[[3]],"`factor(dist_mineral_bins_25)1` + `factor(dist_mineral_bins_25)2` + 
              `factor(dist_mineral_bins_25)3` + `factor(dist_mineral_bins_25)4` + 
              `factor(dist_mineral_bins_25)5` + `factor(dist_mineral_bins_25)6` + 
              `factor(dist_mineral_bins_25)7` + `factor(dist_mineral_bins_25)8` + 
              `factor(dist_mineral_bins_25)9` + `factor(dist_mineral_bins_25)10`")
sum.wealth <- deltaMethod(out.ls[[4]],"`factor(dist_mineral_bins_25)1` + `factor(dist_mineral_bins_25)2` + 
              `factor(dist_mineral_bins_25)3` + `factor(dist_mineral_bins_25)4` + 
              `factor(dist_mineral_bins_25)5` + `factor(dist_mineral_bins_25)6` + 
              `factor(dist_mineral_bins_25)7` + `factor(dist_mineral_bins_25)8` + 
              `factor(dist_mineral_bins_25)9` + `factor(dist_mineral_bins_25)10`")


sum.df <- rbind(sum.roads,sum.cities,sum.nl,sum.wealth)
sum.df$t.val <- sum.df$Estimate/sum.df$SE
sum.df$p.val <- NA
sum.df$p.val[1] <- 2*pt(-abs(sum.df$t.val[1]),df=out.ls[[1]]$df)
sum.df$p.val[2] <- 2*pt(-abs(sum.df$t.val[2]),df=out.ls[[2]]$df)
sum.df$p.val[3] <- 2*pt(-abs(sum.df$t.val[3]),df=out.ls[[3]]$df)
sum.df$p.val[4] <- 2*pt(-abs(sum.df$t.val[4]),df=out.ls[[4]]$df)
sum.df$p.val <- round(sum.df$p.val,3)



out.labs2 <- c("Road Density 1998 (log)","Urban Population Density 2015 (log)","Night Lights per capita 2015 (log)","DHS Household Wealth")
i <- 4
p.minerals.ls <- lapply(c(1:4),function(i){
  p <- ggplot(plot.df.minerals[plot.df.minerals$outcome==out.labs[i],])
  p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
    geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
    geom_vline(xintercept=0,linetype="dotted", size=0.6) +
    scale_y_continuous(breaks=c(10:1),minor_breaks = NULL, labels=plot.df.minerals$what[c(1:10)],limits = c(0.8,10.1)) + 
    labs(x = "Coefficients and 95% Confidence Intervals", y = "Distance Bins",
         title=out.labs2[i],
         subtitle=paste("Coefficient Sum:",round(sum.df$Estimate[i],3), " p-value:", round(sum.df$p.val[i],3)))  +
    theme_minimal(base_size=10) +  
    theme(axis.text.y = element_text(size=8)) 
  return(p)
})

p.out <- p.minerals.ls[[1]] | p.minerals.ls[[2]] | p.minerals.ls[[3]] | p.minerals.ls[[4]]
p.out <- p.out + plot_annotation(title="Spillover Analysis",
                                 subtitle="Distance to Colonial Minerals & Contemporary Outcomes",
                                 theme = theme(plot.title = element_text(size = 20),
                                               plot.subtitle = element_text(size = 16)))

p.out + ggsave(paste0("output/tables_wd/","spillovers_minerals_new.pdf"),width=20,height=5.5)



p <- ggplot(plot.df.minerals)
p <- p + geom_point(size=2.75,aes(x=beta,y=order),shape=19) + 
  geom_errorbarh(aes(x=beta,y=order, xmin=lb, xmax=ub), size=1, height=0.0) +
  geom_vline(xintercept=0,linetype="dotted", size=0.6) +
  scale_y_continuous(breaks=c(10:1),minor_breaks = NULL, labels=plot.df.minerals$what[c(1:10)],limits = c(0.8,10.1)) + 
  labs(x = "Coefficients and 95% Confidence Intervals", y = "Distance Bins",
       title="Spillover Analysis: Minerals",
       subtitle="Distance to Colonial Resources & Contemporary Outcomes") +
  facet_wrap(~ outcome,nrow=1,scales="free_x")  +
  theme_minimal(base_size=16) +  
  theme(axis.text.y = element_text(size=10)) 
p
p + ggsave(paste0("output/tables_wd/","spillovers_minerals.pdf"),width=16,height=5.5)
