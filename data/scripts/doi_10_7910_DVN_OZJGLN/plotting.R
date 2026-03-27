library(ggplot2)

plot_helper <- function(data, var, weights) {
  data = data
  party_color = '#6C6C6C'
  data$ego = 'ego'
  ylab_text = ifelse(var=='fairb', 'Percent equitable', 'Contribution')
  multiplier = ifelse(var=='fairb', 100, 1)
  keep_cols = c('arm_num','ego', 'belief_type_part', var, weights)
  belief_remove = c('non_copart_sig_copart_bel', 'ND_sig_copart_bel')
  thelim = ifelse(var=='fairb', 75, 0.75)
  tmp_mega = subset(data, (arm_num==1) | (arm_num==2 & !belief_type_part %in% belief_remove) | (arm_num==3 & belief_type_part!=belief_remove[1]),
                    select=keep_cols )
  
  tmp_mega$arm_num = as.factor(tmp_mega$arm_num)
  levels(tmp_mega$arm_num) = c('Baseline', 'Non-disclosure', 'Falsification')
  tmp_mega = droplevels(tmp_mega)
  tmp_mega = tmp_mega %>%
    dplyr::group_by(ego, belief_type_part, arm_num) %>%
    dplyr::summarise(mean = wtd.mean(eval(as.name(paste(var))),eval(as.name(paste(weights)))) * multiplier, 
                     se = sqrt((wtd.mean(eval(as.name(paste(var))),eval(as.name(paste(weights)))) * (1 - wtd.mean(eval(as.name(paste(var))),eval(as.name(paste(weights))))))/sum(eval(as.name(paste(weights))))) * multiplier)

  
  tmp_mega = as.data.frame(tmp_mega)
  tmp_mega$belief_type_part = as.factor(tmp_mega$belief_type_part)
  tmp_mega$bel_order = rep(1, dim(tmp_mega)[1])
  tmp_mega$bel_order[which(tmp_mega$belief_type_part=='copart_sig_copart_bel')] = 1
  tmp_mega$bel_order[which(tmp_mega$belief_type_part=='non_copart_sig_non_copart_bel')] = 2
  tmp_mega$bel_order[which(tmp_mega$belief_type_part=='ND_sig_non_copart_bel')] = 3
  tmp_mega$bel_order[which(tmp_mega$belief_type_part=='copart_sig_non_copart_bel')] = 3
  tmp_mega$bel_order[which(tmp_mega$belief_type_part=='non_copart_sig_non_copart_bel')] = 2
  
  tmp_mega$belief_type_part = as.character(tmp_mega$belief_type_part)
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='copart_sig_copart_bel' & tmp_mega$arm_num=='Baseline')] = 'B_copart_sig_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='copart_sig_copart_bel' & tmp_mega$arm_num=='Non-disclosure')] = 'ND_copart_sig_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='copart_sig_copart_bel' & tmp_mega$arm_num=='Falsification')] = 'FLS_copart_sig_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='copart_sig_non_copart_bel' & tmp_mega$arm_num=='Baseline')] = 'B_copart_sig_non_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='copart_sig_non_copart_bel' & tmp_mega$arm_num=='Non-disclosure')] = 'ND_copart_sig_non_copart_bel'
  
  
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='copart_sig_non_copart_bel' & tmp_mega$arm_num=='Falsification')] = 'FLS_copart_sig_non_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='ND_sig_non_copart_bel' & tmp_mega$arm_num=='Non-disclosure')] = 'ND_ND_sig_non_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='non_copart_sig_non_copart_bel' & tmp_mega$arm_num=='Baseline')] = 'B_non_copart_sig_non_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='non_copart_sig_non_copart_bel' & tmp_mega$arm_num=='Non-disclosure')] = 'ND_non_copart_sig_non_copart_bel'
  tmp_mega$belief_type_part[which(tmp_mega$belief_type_part=='non_copart_sig_non_copart_bel' & tmp_mega$arm_num=='Falsification')] = 'FLS_non_copart_sig_non_copart_bel'
  
  p1<- ggplot(tmp_mega, aes(x=belief_type_part, y=mean, fill=reorder(arm_num, bel_order), group=arm_num)) + 
    geom_bar(stat="identity", 
             position=position_dodge(width = 1), width=0.45) +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0,
                  size=0.75,position=position_dodge(1))+
    ylim(c(0, thelim)) + 
    ylab(ylab_text)+
    xlab('')+
    theme_minimal() +
    theme(legend.position="none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          aspect.ratio = 1/1.25)+
    scale_fill_manual(values = rep(party_color, 10)) +
    scale_x_discrete(labels = c("Copartisan\nCopartisan","Non-copartisan\nNon-copartisan", 'Copartisan\nNon-copartisan',
                                'Copartisan\nCopartisan', 'Non-copartisan\nNon-copartisan', 'Copartisan\nNon-copartisan',
                                'Copartisan\nCopartisan','Non-copartisan\nNon-copartisan', 'Withheld\nNon-copartisan','Copartisan\nNon-copartisan'),
                     limits = c("B_copart_sig_copart_bel","B_non_copart_sig_non_copart_bel", 'B_copart_sig_non_copart_bel',
                                'FLS_copart_sig_copart_bel','FLS_non_copart_sig_non_copart_bel', 'FLS_copart_sig_non_copart_bel',
                                'ND_copart_sig_copart_bel','ND_non_copart_sig_non_copart_bel', 'ND_ND_sig_non_copart_bel', 'ND_copart_sig_non_copart_bel'))
  
  return(p1)
}

