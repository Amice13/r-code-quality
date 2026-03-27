#setwd("data/friends")
li<-list.files("data/friends")
li<-paste0("data/friends/",li)
sets<-lapply(li,readRDS)

#install.packages("emIRT")

adj<-do.call(rbind,sets)

vocab<-unique(adj$user_id)

adjmat<-matrix(NA,nrow=length(sets),ncol=length(vocab))
usernames<-rep(NA,nrow(adjmat))
i<-1

for(i in 1:length(sets)){
  x<-sets[[i]]
  line<-vocab%in%x$user_id
  usernames[i]<-x$user[1]
  adjmat[i,]<-line
}

adjdf<-as.data.frame(adjmat)

rownames(adjdf)<-usernames
save(adjdf,file="data/adjmat_friends.rdata")