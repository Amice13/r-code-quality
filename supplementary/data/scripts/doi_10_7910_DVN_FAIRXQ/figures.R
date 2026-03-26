##################################
##### Figures for TSMO Paper #####
##################################


# Time Trend of Polyarchy, Participatory Democracy, and Number of TSMO Memberships

trendplotdata <- working.data.all.tsmos %>% 
  select(ccode,year,polyarchy,partip,num.tsmo.mems,num.igo.mems) %>% 
  group_by(year) %>%
  summarize(tsmo.mems = median(num.tsmo.mems,na.rm = T),
            igo.mems = median(num.igo.mems,na.rm = T),
            polyarchy = mean(polyarchy,na.rm = T),
            partip = mean(partip,na.rm = T)) %>%
  mutate(tsmo.mems = tsmo.mems/400) %>%  # Re-scale for plotting
  gather(key = var,value = stat,-year) %>%
  filter(var != "igo.mems")

trendplot <- ggplot(trendplotdata,aes(x = year)) +
  geom_line(aes(y = stat,group = var,linetype = var)) +
  scale_y_continuous(name = "Mean Democracy Score",
                     limits = c(0,0.6),
                     sec.axis = sec_axis(~.*400,name = "Median TSMO Memberships")) +
  scale_x_continuous(name = "Year",
                     limits = c(1953,2013),
                     breaks = c(1953,1973,1993,2013)) +
  scale_linetype_manual(name = "",
                        values = c("polyarchy" = 1, "partip" = 2, "tsmo.mems" = 3),
                        labels = c("Participartory Dem.","Polyarchy","TSMO Memberships")) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("tables and figures/trendplot.jpg",dpi = 500)



# Marginal Effects Plot #

 # Polyarchy
 
 model <- wtd.lead.mods[[1]]
 
 figdata <- working.data.all.tsmos %>% 
   summarize_if(is.numeric, mean,na.rm = T) %>%
   slice(rep(1, each = 500)) %>%
   mutate(tsmo.wtd.polyarchy = seq(from = 0, to = 1, length.out = 500),
          ccode = as.numeric(gsub(".*)","",names(model$coefficients[
            which.min(abs(model$coefficients - median(model$coefficients[
              str_subset(names(model$coefficients),pattern="ccode")])))])))
   ) # Country intercept closest to the medan for prediction purposes.
 
 figdata <- as.data.frame(cbind(figdata,predict(model,newdata = figdata, interval = "confidence"))) %>% 
   mutate(tsmo.wtd.polyarchy = (tsmo.wtd.polyarchy - mean(working.data.all.tsmos$tsmo.wtd.polyarchy))/sd(working.data.all.tsmos$tsmo.wtd.polyarchy))
 
 predplot.polyarchy <- ggplot(figdata,aes(x = tsmo.wtd.polyarchy,y = fit)) + 
   geom_ribbon(aes(ymin = lwr,ymax = upr), fill = "gray") + 
   geom_line() +
   labs(x = 'TSMO Diffusion (Standardized)', y = 'Pred. Polyarchy Score', title = "Polyarchy") +
   scale_x_continuous(limits = c(-2,2)) +
   scale_y_continuous(limits = c(0,0.7)) +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5))
 predplot.polyarchy
 
 
 
# Participatory Democracy
 
model <- wtd.lead.mods[[3]] # Participatory Democracy 
 
figdata <- working.data.all.tsmos %>% 
   summarize_if(is.numeric, mean,na.rm = T) %>%
   slice(rep(1, each = 500)) %>%
   mutate(tsmo.wtd.partip = seq(from = 0, to = 1, length.out = 500),
          ccode = as.numeric(gsub(".*)","",names(model$coefficients[
            which.min(abs(model$coefficients - median(model$coefficients[
              str_subset(names(model$coefficients),pattern="ccode")])))])))
   ) # Country intercept closest to the medan for prediction purposes.
 
figdata <- as.data.frame(cbind(figdata,predict(model,newdata = figdata, interval = "confidence"))) %>% 
   mutate(tsmo.wtd.partip = (tsmo.wtd.partip - mean(working.data.all.tsmos$tsmo.wtd.partip))/sd(working.data.all.tsmos$tsmo.wtd.partip))
 
predplot.partip <- ggplot(figdata,aes(x = tsmo.wtd.partip,y = fit)) + 
   geom_ribbon(aes(ymin = lwr,ymax = upr), fill = "gray") + 
   geom_line() +
   labs(x = 'TSMO Diffusion (Standardized)', y = 'Pred. Participatory Dem. Score', title = "Participatory Democracy") +
   scale_x_continuous(limits = c(-2,2)) +
   scale_y_continuous(limits = c(0,0.7)) +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5))
predplot.partip


# Freedom of Association

model <- wtd.lead.mods[[8]] # Freedom of Association

figdata <- working.data.all.tsmos %>% 
   summarize_if(is.numeric, mean,na.rm = T) %>%
   slice(rep(1, each = 500)) %>%
   mutate(tsmo.wtd.fr.assoc = seq(from = 0, to = 1, length.out = 500),
          ccode = as.numeric(gsub(".*)","",names(model$coefficients[
             which.min(abs(model$coefficients - median(model$coefficients[
                str_subset(names(model$coefficients),pattern="ccode")])))])))
   ) # Country intercept closest to the medan for prediction purposes.

figdata <- as.data.frame(cbind(figdata,predict(model,newdata = figdata, interval = "confidence"))) %>% 
   mutate(tsmo.wtd.fr.assoc = (tsmo.wtd.fr.assoc - mean(working.data.all.tsmos$tsmo.wtd.fr.assoc))/sd(working.data.all.tsmos$tsmo.wtd.fr.assoc))

predplot.fr.assoc <- ggplot(figdata,aes(x = tsmo.wtd.fr.assoc,y = fit)) + 
   geom_ribbon(aes(ymin = lwr,ymax = upr), fill = "gray") + 
   geom_line() +
   labs(x = 'TSMO Diffusion (Standardized)', y = 'Pred. Free Association Score', title = "Free Association") +
   scale_x_continuous(limits = c(-2,2)) +
   scale_y_continuous(limits = c(0,1)) +
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5))
predplot.fr.assoc
 
# Combined Marginal Effect Plot

png("tables and figures/margin_plot.png",width = 8,height = 4, units = "in", res = 500)
multiplot(predplot.partip,predplot.fr.assoc,cols = 2)
dev.off()

# Imbens Plots

png("tables and figures/imbens_plot.png",width = 5,height = 9, units = "in", res = 500)
multiplot(partip.imbens.plot,fre.exp.imbens.plot,fr.assoc.imbens.plot)
dev.off()