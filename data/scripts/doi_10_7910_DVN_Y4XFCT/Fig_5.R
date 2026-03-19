##########################################################################
# Figure 5: The Effect of Campaign Spending Limits on the Presence of UCKG and
# AG Branches
##########################################################################

rm(list=ls())

library(ggplot2)
library(rdrobust)
library(ggthemes)
library(ggplot2)

electoral <- readRDS(here("data","mayors_municipal_level.rds"))


# ANALYSIS

party_test <- data.frame(conventional_coef=as.numeric(),
                           robust_coef = as.numeric(),
                           conventional_upper = as.numeric(),
                           conventional_lower = as.numeric(),
                           robust_upper = as.numeric(),
                           robust_lower = as.numeric(),
                           n=as.numeric(),
                           group=as.character(),
                           dv=as.character(),
                           stringsAsFactors=FALSE)

years <- c(2013:2020)

for(i in years){
  temp_results <- with(electoral, rdrobust(y = get(paste0('iurd_',i)) - iurd_2012, inverted_margin, all=TRUE))
  temp <- data.frame(conventional_coef = temp_results$coef[[1]],
                     robust_coef = temp_results$coef[[3]],
                     conventional_upper = temp_results$ci[[4]],
                     conventional_lower = temp_results$ci[[1]],
                     robust_upper = temp_results$ci[[6]],
                     robust_lower = temp_results$ci[[3]],
                     n = temp_results$N_h[[1]] + temp_results$N_h[[2]],
                     group = i,
                     dv = 'Number of IURDs')
  
  party_test = rbind(party_test,temp)
}

party_test$treat <- ifelse(party_test$group > 2015 , 'After Spending Caps', 'Before Spending Caps')

party_test$treat <- factor(party_test$treat, levels = c('Before Spending Caps','After Spending Caps'))

# Plot ----
plot_iurds <- ggplot(party_test[party_test$group != 2016,], aes(x = group, y = robust_coef, color = as.factor(treat))) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=.25,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 2016, color = "red",linetype = 'dashed',alpha = .5) +
  #theme(legend.title=element_blank()) +
  xlab("") +
  coord_cartesian(ylim = c(-.5,.5)) +
  ylab('Number of New UCKG Churches since 2012') +
  NULL

#ggsave(here('img','Fig_5_left.pdf'), plot = plot_iurds, device = 'pdf',height = 13, width = 13, units = 'cm') 


# Assembleia de Deus ----

# ANALYSIS

party_test <- data.frame(conventional_coef=as.numeric(),
                         robust_coef = as.numeric(),
                         conventional_upper = as.numeric(),
                         conventional_lower = as.numeric(),
                         robust_upper = as.numeric(),
                         robust_lower = as.numeric(),
                         n=as.numeric(),
                         group=as.character(),
                         dv=as.character(),
                         stringsAsFactors=FALSE)

years <- c(2013:2020)

for(i in years){
  temp_results <- with(electoral, rdrobust(y = get(paste0('assembleia_',i)) - assembleia_2012, inverted_margin, all=TRUE))
  temp <- data.frame(conventional_coef = temp_results$coef[[1]],
                     robust_coef = temp_results$coef[[3]],
                     conventional_upper = temp_results$ci[[4]],
                     conventional_lower = temp_results$ci[[1]],
                     robust_upper = temp_results$ci[[6]],
                     robust_lower = temp_results$ci[[3]],
                     n = temp_results$N_h[[1]] + temp_results$N_h[[2]],
                     group = i,
                     dv = 'Number of assembleias')
  
  party_test = rbind(party_test,temp)
}

party_test$treat <- ifelse(party_test$group > 2015 , 'After Spending Caps', 'Before Spending Caps')

party_test$treat <- factor(party_test$treat, levels = c('Before Spending Caps','After Spending Caps'))

# Plot ----
plot_assembleias <- ggplot(party_test[party_test$group != 2016,], aes(x = group, y = robust_coef, color = as.factor(treat))) +
  geom_errorbar(aes(ymax = (robust_upper), ymin = (robust_lower)), width=.25,size=.75,position=position_dodge(width=0.5)) +
  geom_point(alpha=.9,size=2.5 ,position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, linetype="dotted",colour='gray20') +
  theme_minimal(base_size = 12) +
  scale_color_manual(values=c("grey", "black")) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 2016, color = "red",linetype = 'dashed',alpha = .5) +
  #theme(legend.title=element_blank()) +
  xlab("") +
  coord_cartesian(ylim = c(-.5,.5)) +
  ylab('Number of New Assembleia de Deus Churches since 2012') +
  NULL

#ggsave(here('img','Fig_5_right.pdf'), plot = plot_assembleias, device = 'pdf',height = 13, width = 13, units = 'cm') 

