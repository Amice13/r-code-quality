#Set working directory to replication folder
#-----------------------------
#Packages necessary to run analyses
#library(tidyverse)
#library(estimatr)
#library(latex2exp)

#-----------------------------------------------------------------
#Set parameters for plots
title.size <- 20
x.axis.tick.size <- 14
y.axis.tick.size <- 14
x.axis.label.size <- 18
y.axis.label.size <- 18
facet.text <- 15


#-----------------------------------------------------------------
#Define LOESS for binomial data
binomial_smooth <- function(...) {#for loess on binary DV
  
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
  
}


#-----------------------------------------------------------------
#Construct model 


#Which disposition does the court prefer
preference <- function(posterior,position,n,diffs){
  liberal = sum(posterior[(position+1):(n+1)]*diffs[(position+1):(n+1)])
  conservative = sum(posterior[1:position]*diffs[1:position])
  decision = ifelse(liberal>=conservative,1,0)
  return(decision)
}

#Update beliefs based on previous decision
update_beliefs <- function(prior,p,d,n){
  posterior= prior
  for (i in 1:nrow(d)){
    if(d[i,1]==1){
      posterior[(d[i,2]+1):(n+1)] =  posterior[(d[i,2]+1):(n+1)]*p
      posterior[1:d[i,2]] =  posterior[1:d[i,2]]*(1-p)
    }
    else{
      posterior[(d[i,2]+1):(n+1)] =  posterior[(d[i,2]+1):(n+1)]*(1-p)
      posterior[1:d[i,2]] =  posterior[1:d[i,2]]*p
    }
  }
  return(posterior)
}

#Determine if the signal impacts which disposition the court will choose
informative_check <- function(prior,p,d,n,position,diffs,first){
  if (first){
    lib_signal = matrix(c(1,position), ncol = 2)
    con_signal = matrix(c(0,position), ncol = 2)
  }
  else{
    lib_signal = rbind(d,c(1,position))
    con_signal = rbind(d,c(0,position))
  }
  informative = ifelse(preference(update_beliefs(prior,p,lib_signal,n),position,n,diffs)==
                         preference(update_beliefs(prior,p,con_signal,n),position,n,diffs),0,1)
  return(informative)
}

#Which decision will the court choose
court_decision <- function(yi,x,w,prior,p,d,n,position,diffs,first){
  informative = informative_check(prior,p,d,n,position,diffs, first)
  if (informative==1){
    if (yi<=x+w){
      decision = rbinom(1,1,p)
    }
    else{
      decision = 1-rbinom(1,1,p)
    }
  }
  else{
    if (first){
      decision = preference(prior,position,n,diffs)
    }
    else{
      decision = preference(update_beliefs(prior,p,d,n),position,n,diffs)
    }
  }
  return(c(decision,informative))
}


issue_sequence <- function(n,p){
  no_learn <- TRUE
  all_decisions <- c()
  y <- runif(n, 1/3,2/3)
  x <- runif(1, 0,1)
  w <- runif(1,-1/3,1/3)
  prior <- rep(1/(n+1),n+1)
  courts <-data.frame(ideal_point=y,sequence =seq(1,n))
  courts <- courts[order(courts[,1]),]
  courts$ranking <- seq(1,n)
  courts <- courts[order(courts$sequence),]
  courts <- as.matrix(courts)
  temp = sort(y)-x
  temp1 = matrix(nrow=n+1,ncol=1)
  temp2 = matrix(nrow=n+1,ncol=1)
  temp1[n+1] =1/3
  temp2[1] = -1/3
  temp1[1:n]=temp
  temp2[2:(n+1)]=temp
  diff = temp1-temp2
  
  for (i in 1:n){
    yi = courts[i,1]
    pos = courts[i,3]
    
    if (no_learn){
      decision = court_decision(yi,x,w,prior,p,matrix(),n,pos,diff,TRUE)
      if (decision[2]==1){
        no_learn<- FALSE
        inf_decisions = matrix(c(decision[1],pos),ncol=2)
      }
      all_decisions <- c(all_decisions,decision[1])
    }
    else{
      decision = court_decision(yi,x,w,prior,p,inf_decisions,n,pos,diff,FALSE)
      if (decision[2]==1){
        inf_decisions = rbind(inf_decisions,c(decision[1],pos))
      }
      all_decisions <- c(all_decisions,decision[1])
    }
  }
  return(all_decisions)
}

no_learn_sequence <- function(n,p){
  no_learn <- TRUE
  all_decisions <- c()
  y <- runif(n, 1/3,2/3)
  x <- runif(1, 0,1)
  w <- runif(1,-1/3,1/3)
  prior <- rep(1/(n+1),n+1)
  courts <-data.frame(ideal_point=y,sequence =seq(1,n))
  courts <- courts[order(courts[,1]),]
  courts$ranking <- seq(1,n)
  courts <- courts[order(courts$sequence),]
  courts <- as.matrix(courts)
  temp = sort(y)-x
  temp1 = matrix(nrow=n+1,ncol=1)
  temp2 = matrix(nrow=n+1,ncol=1)
  temp1[n+1] =1/3
  temp2[1] = -1/3
  temp1[1:n]=temp
  temp2[2:(n+1)]=temp
  diff = temp1-temp2
  
  for (i in 1:n){
    yi = courts[i,1]
    pos = courts[i,3]
    
    decision = court_decision(yi,x,w,prior,p,matrix(),n,pos,diff,TRUE)
    all_decisions <- c(all_decisions,decision[1])
  }
  
  return(all_decisions)
}

#-----------------------------------------------------------------
#Simulations


#---------------------
#p=0.70



#Simulate model with learning
set.seed(08540)
results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  results[j,1:n] <- issue_sequence(n[[1]],0.7)
}

dyadic_results = data.frame(results)
colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
dyadic_results$issue <- seq(1,50000)
dyadic_results <- dyadic_results %>% pivot_longer(!issue)
colnames(dyadic_results)<- c("Issue","Period","Disposition")

minority_positions <- dyadic_results %>%
  drop_na() %>%
  group_by(Issue) %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

#Simulate model without learning
set.seed(08540)
no_learn_results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  no_learn_results[j,1:n] <- no_learn_sequence(n[[1]],0.7)
}

no_learn_results = data.frame(no_learn_results)
colnames(no_learn_results) <- c(1,2,3,4,5,6,7,8,9,10)
no_learn_results$issue <- seq(1,50000)
no_learn_results <- no_learn_results %>% pivot_longer(!issue)
colnames(no_learn_results)<- c("Issue","Period","Disposition")

no_learn_results <- no_learn_results %>%
  group_by(Issue) %>%
  drop_na() %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

no_learn_results$l = 0
minority_positions$l = 1

simulations <- bind_rows(minority_positions,no_learn_results)

ggplot(simulations,aes(x = Period,y = n, fill=as.factor(l)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(l)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("stripe", "none"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "black", "1" = "darkgrey"))+
  scale_x_continuous(name="Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 10, y = 0.2, label = TeX("p=0.70"), color = "black", cex=6) +
  annotate("text", x = 6.5, y = 0.14, label = TeX("Independent decisions"), color = "black", cex=8) +
  annotate("text", x = 4.75, y = 0.12, label = TeX("$\\downarrow$"), color = "black", cex=8) +
  annotate("text", x = 3.75, y = 0.2, label = TeX("$\\leftarrow$Learning from "), color ="black",cex=8 )+
  annotate("text", x = 4.25, y = 0.18, label = TeX("previous decisions"), color ="black",cex=8 )
ggsave('Plots/Prop_Minority_Decisions_70.pdf',width=7, height=5)

#---------------------
#p=0.75



#Simulate model with learning
set.seed(08540)
results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  results[j,1:n] <- issue_sequence(n[[1]],0.75)
}

dyadic_results = data.frame(results)
colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
dyadic_results$issue <- seq(1,50000)
dyadic_results <- dyadic_results %>% pivot_longer(!issue)
colnames(dyadic_results)<- c("Issue","Period","Disposition")

minority_positions <- dyadic_results %>%
  drop_na() %>%
  group_by(Issue) %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))


#Simulate model without learning
set.seed(08540)
no_learn_results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  no_learn_results[j,1:n] <- no_learn_sequence(n[[1]],0.75)
}

no_learn_results = data.frame(no_learn_results)
colnames(no_learn_results) <- c(1,2,3,4,5,6,7,8,9,10)
no_learn_results$issue <- seq(1,50000)
no_learn_results <- no_learn_results %>% pivot_longer(!issue)
colnames(no_learn_results)<- c("Issue","Period","Disposition")

no_learn_results <- no_learn_results %>%
  group_by(Issue) %>%
  drop_na() %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

no_learn_results$l = 0
minority_positions$l = 1

simulations <- bind_rows(minority_positions,no_learn_results)

ggplot(simulations,aes(x = Period,y = n, fill=as.factor(l)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(l)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("stripe", "none"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "black", "1" = "darkgrey"))+
  scale_x_continuous(name="Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 10, y = 0.2, label = TeX("p=0.75"), color = "black", cex=6) +
  annotate("text", x = 6.5, y = 0.14, label = TeX("Independent decisions"), color = "black", cex=8) +
  annotate("text", x = 4.75, y = 0.12, label = TeX("$\\downarrow$"), color = "black", cex=8) +
  annotate("text", x = 3.75, y = 0.2, label = TeX("$\\leftarrow$Learning from "), color ="black",cex=8 )+
  annotate("text", x = 4.25, y = 0.18, label = TeX("previous decisions"), color ="black",cex=8 )
ggsave('Plots/Prop_Minority_Decisions_75.pdf',width=7, height=5)

#---------------------
#p=0.80
#Simulate model with learning
set.seed(08540)
results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  results[j,1:n] <- issue_sequence(n[[1]],0.8)
}

dyadic_results = data.frame(results)
colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
dyadic_results$issue <- seq(1,50000)
dyadic_results <- dyadic_results %>% pivot_longer(!issue)
colnames(dyadic_results)<- c("Issue","Period","Disposition")

minority_positions <- dyadic_results %>%
  drop_na() %>%
  group_by(Issue) %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

#Simulate model without learning
set.seed(08540)
no_learn_results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  no_learn_results[j,1:n] <- no_learn_sequence(n[[1]],0.8)
}

no_learn_results = data.frame(no_learn_results)
colnames(no_learn_results) <- c(1,2,3,4,5,6,7,8,9,10)
no_learn_results$issue <- seq(1,50000)
no_learn_results <- no_learn_results %>% pivot_longer(!issue)
colnames(no_learn_results)<- c("Issue","Period","Disposition")

no_learn_results <- no_learn_results %>%
  group_by(Issue) %>%
  drop_na() %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

no_learn_results$l = 0
minority_positions$l = 1

simulations <- bind_rows(minority_positions,no_learn_results)

ggplot(simulations,aes(x = Period,y = n, fill=as.factor(l)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(l)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("stripe", "none"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "black", "1" = "darkgrey"))+
  scale_x_continuous(name="Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 10, y = 0.2, label = TeX("p=0.80"), color = "black", cex=6) +
  annotate("text", x = 6.5, y = 0.14, label = TeX("Independent decisions"), color = "black", cex=8) +
  annotate("text", x = 4.75, y = 0.12, label = TeX("$\\downarrow$"), color = "black", cex=8) +
  annotate("text", x = 3.75, y = 0.2, label = TeX("$\\leftarrow$Learning from "), color ="black",cex=8 )+
  annotate("text", x = 4.25, y = 0.18, label = TeX("previous decisions"), color ="black",cex=8 )
ggsave('Plots/Prop_Minority_Decisions_80.pdf',width=7, height=5)

#---------------------
#p=0.85

#Simulate model with learning
set.seed(08540)
results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  results[j,1:n] <- issue_sequence(n[[1]],0.85)
}

dyadic_results = data.frame(results)
colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
dyadic_results$issue <- seq(1,50000)
dyadic_results <- dyadic_results %>% pivot_longer(!issue)
colnames(dyadic_results)<- c("Issue","Period","Disposition")

minority_positions <- dyadic_results %>%
  drop_na() %>%
  group_by(Issue) %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

#Simulate model without learning
set.seed(08540)
no_learn_results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  no_learn_results[j,1:n] <- no_learn_sequence(n[[1]],0.85)
}

no_learn_results = data.frame(no_learn_results)
colnames(no_learn_results) <- c(1,2,3,4,5,6,7,8,9,10)
no_learn_results$issue <- seq(1,50000)
no_learn_results <- no_learn_results %>% pivot_longer(!issue)
colnames(no_learn_results)<- c("Issue","Period","Disposition")

no_learn_results <- no_learn_results %>%
  group_by(Issue) %>%
  drop_na() %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

no_learn_results$l = 0
minority_positions$l = 1

simulations <- bind_rows(minority_positions,no_learn_results)

ggplot(simulations,aes(x = Period,y = n, fill=as.factor(l)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(l)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("stripe", "none"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "black", "1" = "darkgrey"))+
  scale_x_continuous(name="Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 10, y = 0.2, label = TeX("p=0.85"), color = "black", cex=6) +
  annotate("text", x = 6.5, y = 0.14, label = TeX("Independent decisions"), color = "black", cex=8) +
  annotate("text", x = 4.75, y = 0.12, label = TeX("$\\downarrow$"), color = "black", cex=8) +
  annotate("text", x = 3.75, y = 0.2, label = TeX("$\\leftarrow$Learning from "), color ="black",cex=8 )+
  annotate("text", x = 4.25, y = 0.18, label = TeX("previous decisions"), color ="black",cex=8 )
ggsave('Plots/Prop_Minority_Decisions_85.pdf',width=7, height=5)

#---------------------
#p=0.90



#Simulate model with learning
set.seed(08540)
results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  results[j,1:n] <- issue_sequence(n[[1]],0.9)
}

dyadic_results = data.frame(results)
colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
dyadic_results$issue <- seq(1,50000)
dyadic_results <- dyadic_results %>% pivot_longer(!issue)
colnames(dyadic_results)<- c("Issue","Period","Disposition")

minority_positions <- dyadic_results %>%
  drop_na() %>%
  group_by(Issue) %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

#Simulate model without learning
set.seed(08540)
no_learn_results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  n <- 10
  no_learn_results[j,1:n] <- no_learn_sequence(n[[1]],0.9)
}

no_learn_results = data.frame(no_learn_results)
colnames(no_learn_results) <- c(1,2,3,4,5,6,7,8,9,10)
no_learn_results$issue <- seq(1,50000)
no_learn_results <- no_learn_results %>% pivot_longer(!issue)
colnames(no_learn_results)<- c("Issue","Period","Disposition")

no_learn_results <- no_learn_results %>%
  group_by(Issue) %>%
  drop_na() %>%
  mutate(m = min(sum(Disposition),n()-sum(Disposition))) %>%
  filter(m==1) %>%
  mutate(maj_position = ifelse(sum(Disposition)>2,1,0),
         minority = ifelse(Disposition!=maj_position,1,0)) %>%
  ungroup() %>%
  filter(minority==1)%>%
  group_by(Period) %>%
  summarise(n = n()) %>%
  mutate(n = n/sum(n), Period = as.numeric(Period))

no_learn_results$l = 0
minority_positions$l = 1

simulations <- bind_rows(minority_positions,no_learn_results)

ggplot(simulations,aes(x = Period,y = n, fill=as.factor(l)))+
  geom_bar_pattern(stat='identity',
                   aes(pattern = as.factor(l)),
                   colour = "black",
                   pattern_fill = "white",
                   pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.05,
                   position = position_dodge())+
  scale_pattern_manual(values = c("stripe", "none"),
                       guide = guide_legend(override.aes = list(fill = "white")) # <- make lighter
  ) +
  scale_fill_manual("legend", values = c("0" = "black", "1" = "darkgrey"))+
  scale_x_continuous(name="Period", breaks = seq(1,12,1)) +
  scale_y_continuous(name="Proportion", expand = expansion(mult = c(0, .05))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 10, y = 0.2, label = TeX("p=0.90"), color = "black", cex=6) +
  annotate("text", x = 6.5, y = 0.14, label = TeX("Independent decisions"), color = "black", cex=8) +
  annotate("text", x = 4.75, y = 0.12, label = TeX("$\\downarrow$"), color = "black", cex=8) +
  annotate("text", x = 3.75, y = 0.2, label = TeX("$\\leftarrow$Learning from "), color ="black",cex=8 )+
  annotate("text", x = 4.25, y = 0.18, label = TeX("previous decisions"), color ="black",cex=8 )
ggsave('Plots/Prop_Minority_Decisions_90.pdf',width=7, height=5)

