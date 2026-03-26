library(data.table)
library(tidyverse)
library(dplyr)
library(lazyeval)
library(zoo)
library(tidytext)
library(quanteda)
library(lubridate)
library(text2vec)
library(parallel)
library(ggplot2)
library(data.table)
library(vars)
library(vegan)
library(ggforce)
library(tidyr)
library(xtable)
library(lubridate)
library(stringr)


#Collecting Data Fles
ff        <- list.files("data_new3/x_label/")




print("Begin Data Creation for Figures 7 and 8")

# Figure





 data.hold.fn <-list()
 idx<-1
 for(f in ff){
   data.hold.fn[[idx]] <- read.csv(paste0("data_new3/x_label/",f))
   idx              <- idx+1
 }

 data.plot<-rbindlist(data.hold.fn)
 rm(data.hold.fn)



 data.plot2<-data.plot %>%
   mutate(date=as.Date(date,format = "%a %b %d %H:%M:%S %z %Y")) %>%
   group_by(date)%>%dplyr::select(starts_with("X")) %>%
   mutate(n=n(),
          X.fake.news=case_when(X1>0.9|X2>0.9|X9>0.9|X10>0.9|X11>0.9|X14>0.9|X18>0.9|X26>0.9|X27>0.9~1,
                                TRUE ~0),
          X.real.news = case_when(X4>0.9|X5>0.9|X16>0.9|X13>0.9|X20>0.9 |X21>0.9 ~1,
                                  TRUE ~0),
          X.legal  = case_when(X0>0.9|X6>0.9|X8>0.9|X23>0.9|X24>0.9|X27>0.9|X28>0.9 ~1,
                               TRUE ~0),) %>%
   summarise_all(mean)


 data.plot.max.topic <- data.plot2 %>%
   group_by(date) %>%
   mutate(total.tweet = sum(n))%>%
   summarise_at(vars(starts_with("X")), ~weighted.mean(.,w=n))%>%
   filter(date>as.Date("2020-09-01"))



 n.top<-data.plot2 %>%
   group_by(date) %>%
   mutate(total.tweet = sum(n))


### Figure 7: Tweets Per Day
print("Plot Figure 7: Tweets per Day")

 g<-ggplot(data=n.top %>%
          filter(date>as.Date("2020-09-01")),
        aes(x=as.Date(date),y=total.tweet/1000000))+
   ylab("Millions of Tweets")+
   xlab("")+
   scale_x_date(date_labels = "%B %Y",
                date_breaks = "1 month")+
   scale_y_continuous(n.breaks = 10) +
   geom_vline(xintercept = as.Date("2020-11-07"),color="red",lty=2)+
   geom_line(size=1) +
   geom_vline(xintercept = as.Date("2021-01-10"),color="red",lty=2)+
   theme_bw()+
   annotate("text",x=as.Date("2020-11-17"),y=3,label="Election Day",size=7)+
   annotate("text",x=as.Date("2021-01-01"),y=3,label="January 6th",size=7) +
   theme(text = element_text(size=20))



 pdf("Plots/tweets_per_day.pdf",width=16,height = 9)
 plot(g)
 dev.off()

 ## Figure 8

print("Begin Figure 8: Topic Composition of Election Fraud")

g<-ggplot(data=data.plot.max.topic %>%
          filter(date>as.Date("2020-10-01")),
        aes(x=as.Date(date),y=X.fake.news*100))+
   ylab("Percent (%) of Daily Tweets")+
   xlab("")+
   scale_x_date(date_labels = "%B %Y",
                date_breaks = "1 month")+
   scale_y_continuous(n.breaks = 10,limits = c(0,30)) +
   geom_line(size=1) +
   geom_line(data=data.plot.max.topic %>%
               filter(date>as.Date("2020-10-01")),
             aes(x=as.Date(date),y=X.real.news*100)) +
   geom_line(data=data.plot.max.topic %>%
               filter(date>as.Date("2020-10-01")),
             aes(x=as.Date(date),y=X.legal*100),lty=2)+
   geom_vline(xintercept = as.Date("2021-01-06"),color="red")+
   geom_vline(xintercept = as.Date("2020-11-07"),color="red")+
   theme_bw()+
   annotate("text",x=as.Date("2020-11-15"),y=29,label="Election Day",size=7)+
   annotate("text",x=as.Date("2020-12-29"),y=29,label="January 6th",size=7)+
   annotate("text",x=as.Date("2020-10-10"),y=29.2,label="Discrediting the Election",size=7)+
   annotate("text",x=as.Date("2020-10-15"),y=11,label="Verifying the Election",size=7)+
   annotate("text",x=as.Date("2020-12-01"),y=21,label="Legal Challenges",size=7)+
  theme(text=element_text(size=20))

plot(g)

pdf("Plots/topical_composition.pdf",width=16,height = 9)
plot(g)
dev.off()

######
