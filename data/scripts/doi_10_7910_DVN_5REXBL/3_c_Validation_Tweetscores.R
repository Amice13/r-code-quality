source("tools/toolbox.R")

load("data/adjmat_friends.rdata")

count<-colSums(adjdf)

adjdf<-adjdf[,count>5,]

adjdf<-adjdf[rowSums(adjdf)>0,]

v1<-vegan::cca(adjdf)

positions<-as.data.frame(v1$CA$u)
positions$user_id<-rownames(adjdf)


load("./data/individual_diff.rdata")
### 
full$user_id

t1<-merge(full,positions,by="user_id")

t1$dim1<-t1$nominate
t1$CA1<-t1$CA1*-1

t1$party<-as.factor(t1$party)
t2<-t1[!is.na(t1$dim1),]


pdf("appendix/figures/figure11.pdf",width = 15,height=10)
plot_rc(m6 = t2,var = "CA1",var3="theta",lab1="Twitter Friends",lab2="Twitter Text",pcols=c("light blue","coral"))
dev.off()



pdf("appendix/figures/figure12.pdf",width = 10,height=20)
par(mfrow=c(3,1))
plot_rc(m6 = t2,var = "CA1",var3="nominate",lab1="Twitter Friends")
plot_rc(m6 = t2,var = "CA1",var3="theta",lab1="Twitter Friends",lab2="Twitter Text")
plot_rc(m6 = t2,var = "theta",var3="nominate")
dev.off()

