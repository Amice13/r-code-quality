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

#-----------------------------------------------------------------
#Simulations for Figure A.1

#------------------------
#p=0.7
set.seed(08540)
results <- matrix(ncol=10,nrow=50000)
for (j in 1:50000){
  results[j,] <- issue_sequence(10,0.7)
  }

dyadic_results = data.frame(results)
colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
dyadic_results$issue <- seq(1,50000)
dyadic_results <- dyadic_results %>% pivot_longer(!issue)
colnames(dyadic_results)<- c("Issue","Period","Disposition")
dyadic_agreement <- data.frame('Issue'=as.integer(),'Former'=as.integer(),
                               'Latter'=as.integer(),'Agreement'=as.integer())
for (i in 1:10){
  for (j in i:10){
    if (i!=j){
      former = filter(dyadic_results, Period==as.double(i))
      latter = filter(dyadic_results, Period==as.double(j))
      agreement <- ifelse(former$Disposition==latter$Disposition,1,0)
      temp <- data.frame('Issue'=former$Issue,'Former'=former$Period,
                         'Latter' = latter$Period, 'Agreement'=agreement)
      dyadic_agreement <- rbind(dyadic_agreement,temp)
    }
  }
}

ggplot(dyadic_agreement, aes(x = as.double(Former), y =Agreement))+
  binomial_smooth(formula = y ~ splines::ns(x, 3), se=F)+
  geom_hline(yintercept = mean(dyadic_agreement$Agreement), color = 'black',
             linetype = 'dashed',size = 1)+
  scale_x_continuous(name=TeX("Period of Court, t"), breaks = seq(0,9,1)) +
  scale_y_continuous(name=TeX("Probability of Subsequent Agreement"), expand = expansion(mult = c(0.05, 0))) +
  theme_bw()+
  theme(legend.position="none",#suppress legend
        axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
        axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
  annotate("text", x = 2, y = 1, label = TeX(""), cex=8) +
  annotate("text", x =6.5, y = .863, label = TeX("$\\uparrow$ Average Probability"), color = "black", cex=8)+ 
  annotate("text", x = 6.7, y = .853, label = TeX("of Agreement"), color = "black", cex=8) +
  annotate("text", x = 8, y = .99, label = TeX("$p=0.7$"), cex=8) +
  annotate("text", x = 2, y = 1, label = TeX(""), cex=8) 
  ggsave('Plots/Prob_Subsequent_Agreement_70.pdf',width=20, height=15, units='cm')
  
  #------------------------
  #p=0.75
  set.seed(08540)
  results <- matrix(ncol=10,nrow=50000)
  for (j in 1:50000){
    results[j,] <- issue_sequence(10,0.75)
  }
  
  dyadic_results = data.frame(results)
  colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
  dyadic_results$issue <- seq(1,50000)
  dyadic_results <- dyadic_results %>% pivot_longer(!issue)
  colnames(dyadic_results)<- c("Issue","Period","Disposition")
  dyadic_agreement <- data.frame('Issue'=as.integer(),'Former'=as.integer(),
                                 'Latter'=as.integer(),'Agreement'=as.integer())
  for (i in 1:10){
    for (j in i:10){
      if (i!=j){
        former = filter(dyadic_results, Period==as.double(i))
        latter = filter(dyadic_results, Period==as.double(j))
        agreement <- ifelse(former$Disposition==latter$Disposition,1,0)
        temp <- data.frame('Issue'=former$Issue,'Former'=former$Period,
                           'Latter' = latter$Period, 'Agreement'=agreement)
        dyadic_agreement <- rbind(dyadic_agreement,temp)
      }
    }
  }
  
  ggplot(dyadic_agreement, aes(x = as.double(Former), y =Agreement))+
    binomial_smooth(formula = y ~ splines::ns(x, 3), se=F)+
    geom_hline(yintercept = mean(dyadic_agreement$Agreement), color = 'black',
               linetype = 'dashed',size = 1)+
    scale_x_continuous(name=TeX("Period of Court, t"), breaks = seq(0,9,1)) +
    scale_y_continuous(name=TeX("Probability of Subsequent Agreement"), expand = expansion(mult = c(0.05, 0))) +
    theme_bw()+
    theme(legend.position="none",#suppress legend
          axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
          axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) +
    annotate("text", x =6.5, y = .863, label = TeX("$\\uparrow$ Average Probability"), color = "black", cex=8)+ 
    annotate("text", x = 6.7, y = .853, label = TeX("of Agreement"), color = "black", cex=8) +
    annotate("text", x = 8, y = .99, label = TeX("$p=0.75$"), cex=8) +
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) 
  ggsave('Plots/Prob_Subsequent_Agreement_75.pdf',width=20, height=15, units='cm')
  
  
  
  
  #------------------------
  #p=0.80
  set.seed(08540)
  results <- matrix(ncol=10,nrow=50000)
  for (j in 1:50000){
    results[j,] <- issue_sequence(10,0.8)
  }
  
  dyadic_results = data.frame(results)
  colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
  dyadic_results$issue <- seq(1,50000)
  dyadic_results <- dyadic_results %>% pivot_longer(!issue)
  colnames(dyadic_results)<- c("Issue","Period","Disposition")
  dyadic_agreement <- data.frame('Issue'=as.integer(),'Former'=as.integer(),
                                 'Latter'=as.integer(),'Agreement'=as.integer())
  for (i in 1:10){
    for (j in i:10){
      if (i!=j){
        former = filter(dyadic_results, Period==as.double(i))
        latter = filter(dyadic_results, Period==as.double(j))
        agreement <- ifelse(former$Disposition==latter$Disposition,1,0)
        temp <- data.frame('Issue'=former$Issue,'Former'=former$Period,
                           'Latter' = latter$Period, 'Agreement'=agreement)
        dyadic_agreement <- rbind(dyadic_agreement,temp)
      }
    }
  }
  
  ggplot(dyadic_agreement, aes(x = as.double(Former), y =Agreement))+
    binomial_smooth(formula = y ~ splines::ns(x, 3), se=F)+
    geom_hline(yintercept = mean(dyadic_agreement$Agreement), color = 'black',
               linetype = 'dashed',size = 1)+
    scale_x_continuous(name=TeX("Period of Court, t"), breaks = seq(0,9,1)) +
    scale_y_continuous(name=TeX("Probability of Subsequent Agreement"), expand = expansion(mult = c(0.05, 0))) +
    theme_bw()+
    theme(legend.position="none",#suppress legend
          axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
          axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) +
    annotate("text", x =6.5, y = .863, label = TeX("$\\uparrow$ Average Probability"), color = "black", cex=8)+ 
    annotate("text", x = 6.7, y = .853, label = TeX("of Agreement"), color = "black", cex=8) +
    annotate("text", x = 8, y = .99, label = TeX("$p=0.80$"), cex=8) +
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) 
  ggsave('Plots/Prob_Subsequent_Agreement_80.pdf',width=20, height=15, units='cm')
  
  
  
  
  #------------------------
  #p=0.85
  set.seed(08540)
  results <- matrix(ncol=10,nrow=50000)
  for (j in 1:50000){
    results[j,] <- issue_sequence(10,0.85)
  }
  
  dyadic_results = data.frame(results)
  colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
  dyadic_results$issue <- seq(1,50000)
  dyadic_results <- dyadic_results %>% pivot_longer(!issue)
  colnames(dyadic_results)<- c("Issue","Period","Disposition")
  dyadic_agreement <- data.frame('Issue'=as.integer(),'Former'=as.integer(),
                                 'Latter'=as.integer(),'Agreement'=as.integer())
  for (i in 1:10){
    for (j in i:10){
      if (i!=j){
        former = filter(dyadic_results, Period==as.double(i))
        latter = filter(dyadic_results, Period==as.double(j))
        agreement <- ifelse(former$Disposition==latter$Disposition,1,0)
        temp <- data.frame('Issue'=former$Issue,'Former'=former$Period,
                           'Latter' = latter$Period, 'Agreement'=agreement)
        dyadic_agreement <- rbind(dyadic_agreement,temp)
      }
    }
  }
  
  ggplot(dyadic_agreement, aes(x = as.double(Former), y =Agreement))+
    binomial_smooth(formula = y ~ splines::ns(x, 3), se=F)+
    geom_hline(yintercept = mean(dyadic_agreement$Agreement), color = 'black',
               linetype = 'dashed',size = 1)+
    scale_x_continuous(name=TeX("Period of Court, t"), breaks = seq(0,9,1)) +
    scale_y_continuous(name=TeX("Probability of Subsequent Agreement"), expand = expansion(mult = c(0.05, 0))) +
    theme_bw()+
    theme(legend.position="none",#suppress legend
          axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
          axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) +
    annotate("text", x =6.5, y = .863, label = TeX("$\\uparrow$ Average Probability"), color = "black", cex=8)+ 
    annotate("text", x = 6.7, y = .853, label = TeX("of Agreement"), color = "black", cex=8) +
    annotate("text", x = 8, y = .99, label = TeX("$p=0.85$"), cex=8) +
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) 
  ggsave('Plots/Prob_Subsequent_Agreement_85.pdf',width=20, height=15, units='cm')
  
  
  
  
  #------------------------
  #p=0.90
  set.seed(08540)
  results <- matrix(ncol=10,nrow=50000)
  for (j in 1:50000){
    results[j,] <- issue_sequence(10,0.90)
  }
  
  dyadic_results = data.frame(results)
  colnames(dyadic_results) <- c(1,2,3,4,5,6,7,8,9,10)
  dyadic_results$issue <- seq(1,50000)
  dyadic_results <- dyadic_results %>% pivot_longer(!issue)
  colnames(dyadic_results)<- c("Issue","Period","Disposition")
  dyadic_agreement <- data.frame('Issue'=as.integer(),'Former'=as.integer(),
                                 'Latter'=as.integer(),'Agreement'=as.integer())
  for (i in 1:10){
    for (j in i:10){
      if (i!=j){
        former = filter(dyadic_results, Period==as.double(i))
        latter = filter(dyadic_results, Period==as.double(j))
        agreement <- ifelse(former$Disposition==latter$Disposition,1,0)
        temp <- data.frame('Issue'=former$Issue,'Former'=former$Period,
                           'Latter' = latter$Period, 'Agreement'=agreement)
        dyadic_agreement <- rbind(dyadic_agreement,temp)
      }
    }
  }
  
  ggplot(dyadic_agreement, aes(x = as.double(Former), y =Agreement))+
    binomial_smooth(formula = y ~ splines::ns(x, 3), se=F)+
    geom_hline(yintercept = mean(dyadic_agreement$Agreement), color = 'black',
               linetype = 'dashed',size = 1)+
    scale_x_continuous(name=TeX("Period of Court, t"), breaks = seq(0,9,1)) +
    scale_y_continuous(name=TeX("Probability of Subsequent Agreement"), expand = expansion(mult = c(0.05, 0))) +
    theme_bw()+
    theme(legend.position="none",#suppress legend
          axis.text.x = element_text(size=18), axis.title.x = element_text(size=x.axis.label.size),
          axis.text.y = element_text(size=18), axis.title.y = element_text(size=y.axis.label.size))+
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) +
    annotate("text", x =6.5, y = .863, label = TeX("$\\uparrow$ Average Probability"), color = "black", cex=8)+ 
    annotate("text", x = 6.7, y = .853, label = TeX("of Agreement"), color = "black", cex=8) +
    annotate("text", x = 8, y = .99, label = TeX("$p=0.90$"), cex=8) +
    annotate("text", x = 2, y = 1, label = TeX(""), cex=8) 
  ggsave('Plots/Prob_Subsequent_Agreement_90.pdf',width=20, height=15, units='cm')
  
  

  
  


