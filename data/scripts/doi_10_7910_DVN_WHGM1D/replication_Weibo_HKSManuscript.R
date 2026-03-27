


###### Conspiracy and debunking narratives about COVID-19 origins on Chinese social media: #####
                      ########### how it started and who is to blame #############


## Author: Kaiping Chen, Anfan Chen, Jingwen Zhang, Jingbo Meng, Cuihua Shen
## Date: November 15, 2020
## Harvard Kennedy School Misinformation Review
## How to cite: Chen, K., Chen, A., Zhang, J., Meng, J., & Shen, C. (2020). Conspiracy and debunking narratives about COVID-19 origins on Chinese social media: How it started and who is to blame (forthcoming). Harvard Kennedy School Misinformation Review. 



##install packages
library(data.table)
library(lubridate)
library(ggplot2)
library(sjPlot)
library(MASS)
library(ggeffects)
library(stargazer)
# Operating system: iMac, R: 4.0.2

setwd("~/OneDrive - UW-Madison/WeiboCOVID19") # please adjust this code line to your working directory
d <- fread("Conspiracy_COVID-19_Weibo_dataset_20200917.csv")



################### Replicate Figure 1 ###################

conspiracy <- d[信息类型!=2] #n=1516 conspiracy-related posts: conspiracy posts or debunking posts
consp.dt <- cbind(conspiracy[, 1], conspiracy[, 43], conspiracy[, 53:60], conspiracy[, 45:51])
  #select columns needed for Figure 1: origination type, debunk or not, responsibility attribution entities

setnames(consp.dt, c("信息类型","中国","美国", "自然形成","人工合成","基因改造","生物武器","五G","电子烟","转基因作物"), 
         c("debunk_or_not", "China","US","Nature/unknown origin","Human-synthesis","Lab-edited","Bio weapon","5G","ECigarette","GMO"))

cols <- c("China","US", "日本","塞尔维亚","欧洲国家","Bill_Gates","其他国家或主体","无明确","Nature/unknown origin","Human-synthesis","Lab-edited","Bio weapon","5G","ECigarette","GMO")
consp.dt[, (cols):=lapply(.SD, as.numeric), .SDcols=cols]

consp.dt[, debunk_or_not := factor(debunk_or_not, levels = c(0,1),labels=c("conspiracy", "debunk"))]

consp.dt <- melt(consp.dt, id.vars = 1:10, variable.name = "conspiracy_type")[value > 0]
consp.dt[, value := NULL]
consp.dt[, ID := NULL]
att.dt <- consp.dt[, lapply(.SD, sum), by = c("conspiracy_type","debunk_or_not")]

# totals
tot.dt <- consp.dt[, .N, by = c("conspiracy_type","debunk_or_not")]
# merge together
att.dt <- merge(att.dt, tot.dt, by = c("conspiracy_type","debunk_or_not"))

att.dt[, Others:=塞尔维亚 + 日本 +欧洲国家 + Bill_Gates + 其他国家或主体 + 无明确]


plot_dt <- melt(att.dt, id.vars = c("conspiracy_type","debunk_or_not"), measure.vars=c("China","US","Others"), variable.name = "Responsibility", value.name ="count")

figure1 <- ggplot(plot_dt, aes(debunk_or_not, count, fill=Responsibility, label=count)) +
  geom_col() +geom_text(size=2.0, position=position_stack(vjust=0.5)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_text(size=15)) +
  scale_fill_brewer(palette="Set1") +  ylim(0, 400)

figure1 <- figure1 + facet_wrap(.~conspiracy_type,ncol=3) + 
  ylab("Number of Conspiracy and Debunking Posts on Weibo") +
  theme(strip.text.x = element_text(size = 15), legend.title=element_text(size=15), legend.text=element_text(size=15))

ggsave("figure1.png", plot=figure1, width=8.75, height=6.5, dpi=300)




################### Replicate Figure 2 #####################

# text box labels were added manually using PPT

conspiracy <- d[信息类型!=2] #n=1516

conspiracy[, dates :=as.Date(parse_date_time(created_at,"ymd_HM"))]

ChinaCOVID19 <- fread("Newly Comfirmed Case Num.csv")
ChinaCOVID19[, dates :=as.Date(parse_date_time(Date,"m/d/y"))]
setnames(ChinaCOVID19, "Newly Confirmed Case Number", "China_Cases")

  #Evolution of Conspiracy and Debunking Posts
d2 <- conspiracy[, c("dates", "信息类型")]
setnames(d2,"信息类型", "debunk_or_not")

d2[, debunk_or_not := factor(debunk_or_not, levels = c(0,1),labels=c("Conspiracy posts", "Debunking posts"))]

tmp <- d2[, .(total_posts=.N), .(dates,debunk_or_not)][order(dates)]

a <- merge(tmp, ChinaCOVID19, by="dates",all.x=TRUE)

  #Evolution of Responsibility Attribution

d3 <- conspiracy[, c("ID","dates", "中国", "美国")]
setnames(d3,c("中国", "美国"), c("China","US"))

d3 <- melt(d3, id.vars = c("ID","dates"), variable.name = "responsibility_attribution")[value > 0]

d3[, value := NULL]
d3[, ID := NULL]

tmp <- d3[, .(total_posts=.N), .(dates,responsibility_attribution)][order(dates)]

b <- merge(tmp, ChinaCOVID19, by="dates",all.x=TRUE)

  # Putting a and b in one plot

setnames(a,"debunk_or_not","countryOrattitude_value")
setnames(b,"responsibility_attribution","countryOrattitude_value")

a[, country_attitude:="Conspiracy vs Debunking Posts"]
b[, country_attitude:="Responsbility Attribution"]

d4 <- rbind(a,b)

coeff=300
figure2 <- ggplot(d4, aes(x=dates)) +
  geom_col( aes(y=China_Cases / coeff),fill="lightgrey") + # Divide by 100 to get the same range than the total posts
  geom_line( aes(y=total_posts,col = countryOrattitude_value)) + 
  scale_y_continuous(
    name = "Total Posts",
    sec.axis = sec_axis(~.*coeff, name="Total China COVID19 Cases")
  )

figure2 <- figure2 + facet_grid(vars(country_attitude)) +
  #labs(colour = "countryOrattitude_value") +
  scale_color_discrete(l=50) +
  #ggtitle(label = "Evolution of Responsibilty, Conspiracy and Counteractive narrative", subtitle="with China COVID19 Cases at Background") +
  #geom_text(x=as.numeric(ymd("2020-02-02")), y=20, label="Trump Closed U.S Border") +
  #geom_text(x=as.numeric(ymd("2020-02-15")), y=100, label="Hubei Changed Counting of Diagonses\nCases Surged") +
  #geom_text(x=as.numeric(ymd("2020-03-17")), y=70, label="Trump Called China Virus\nChinese Top Officials Blamed US Soldiers for Virus\nChina Expelled US Journalists") +
  #geom_text(x=as.numeric(ymd("2020-03-24")), y=50, label="Trump Tweeted \nHe Would Stop Use Term China Virus") +
  #geom_text(x=as.numeric(ymd("2020-04-15")), y=40, label="Trump Green Card Ban") +
  #geom_text(x=as.numeric(ymd("2020-04-28")), y=20, label="US 5G Clean\nPath on Huawei") +
  geom_vline(xintercept=as.numeric(c(ymd("2020-02-15"), ymd("2020-03-15"), ymd("2020-03-24"), ymd("2020-04-15"),ymd("2020-04-29"))),
             linetype=4, colour="black") + 
  theme(panel.grid=element_blank(),legend.title=element_blank(), 
        axis.title.y=element_text(size=13), axis.text.x=element_text(size=13), axis.title.x=element_blank(),
        strip.text.y=element_text(size=13)) +
  theme(panel.background = element_rect(fill="aliceblue"))



################### Replicate Figure 4 #####################

setwd("~/OneDrive - UW-Madison/WeiboCOVID19")
d <- fread("Conspiracy_COVID-19_Weibo_dataset_20200917.csv")
conspiracy <- d[信息类型!=2] #n=1516

  ## Preparation: merge variables/create new dummies

# merge several types under 阴谋论类型
conspiracy[, conspiracy_type_others:=ifelse(五G == 1|电子烟==1|转基因作物==1, 1, 0)]
# create a new variable: deliberately-made or not under 阴谋论类型. Deliberately-made means as long as any of the non自然形成 type is mentioned in a post, we coded that this post contains deliberately-made conspiracy
conspiracy[, deliberately_made:=ifelse(人工合成 == 1|基因改造==1|生物武器==1|五G==1| 转基因作物==1, 1, 0)]

# merge several types under 归责对象
conspiracy[, responsibility_other_entities:=ifelse(塞尔维亚 == 1|日本==1|欧洲国家==1|Bill_Gates==1|其他国家或主体==1, 1, 0)]

# create several new variables for 归责对象
conspiracy[, has_responsibility:=ifelse(无明确== 1, 0, 1)]

conspiracy[, china_responsibility_only:=ifelse(responsibility_other_entities == 1|美国==1|无明确==1, 0, 1)]
conspiracy[, US_responsibility_only:=ifelse(responsibility_other_entities == 1|中国==1|无明确==1, 0, 1)]

conspiracy[, responsibility_ordinal:=ifelse(china_responsibility_only == 1, "China_responsbile_only", 
                                            ifelse(US_responsibility_only==1,"US_responsbile_only", "AllOtherSituations_responsible"))]

conspiracy[, responsibility_ordinal:=as.factor(responsibility_ordinal)]
conspiracy[, responsibility_ordinal:=relevel(responsibility_ordinal,"AllOtherSituations_responsible")]

# create a new variable for 信源. "cited_source" is an ordinal variable: has source and mention scientist, has source but not mention scientist, no source
conspiracy[, cited_source:=ifelse(科学研究人员或学者==1, "cite_scientists",
                                           ifelse(无信源==1,"no_source_cite","only_cite_nonscientist_source"))]
conspiracy[,cited_source:=as.factor(cited_source)]
conspiracy[, cited_source:=relevel(cited_source,"no_source_cite")]

conspiracy[, cited_scientist:=ifelse(科学研究人员或学者==1, 1,0)]

conspiracy[, has_source:=ifelse(无信源== 1, 0, 1)]

# manually checked whether above codings are correct: write.xlsx(conspiracy, "test.xlsx")

# create participation, mobilization as dependent variables (in addition to info cascade)
conspiracy[,participation := rowSums(.SD), .SDcols=c("like_num", "repost_num","comment_num")]
conspiracy[,participation := log(participation+1)]
conspiracy[,mobilization := rowSums(.SD), .SDcols=c("@ count","# count")]
conspiracy[,mobilization :=log(mobilization+1)]

# create the time variable. Time means the difference between when the data is collected - the post created date
conspiracy[, dates :=as.Date(parse_date_time(created_at,"ymd_HM"))]
conspiracy[, collection_date:=as.Date(parse_date_time("2020-04-30","ymd"))]
conspiracy[, time_since_posted:=collection_date-dates]

# create province as a dummy for user: from Hubei or not
conspiracy[, hubei:=ifelse(province=="湖北",1,0)]

# rename some variables
setnames(conspiracy, "Emotion Polarity(0=negative,1=postive)", "emotion_polarity")
setnames(conspiracy, "emotion score", "emotion_score")
setnames(conspiracy, "Length of Text", "post_length")
setnames(conspiracy, "number of posts", "user_total_posts")
setnames(conspiracy, "Verification Status(0=ordinary user, 1= influencer, 2= organizational user)", "verification_status")
setnames(conspiracy, "信息类型", "DebunkOrConspiracy")

# relabel the values for gender to English
conspiracy[, gender := factor(gender, levels = c("女","男"),labels=c("Female", "Male"))]

# relabel the values for debunking to English
conspiracy[, DebunkOrConspiracy := factor(DebunkOrConspiracy, levels = c(0,1),labels=c("Conspiracy Posts", "Debunking Posts"))]

# turn some variables into factor variable and assign baseline
cols <- c("自然形成", "人工合成", "基因改造", "生物武器", "conspiracy_type_others","verification_status","vip_level")
conspiracy[,(cols):= lapply(.SD, as.factor), .SDcols = cols]
conspiracy[, vip_level:=relevel(vip_level,"未开通")] #baseline 未开通

# check the distribution of post length and fans number
#hist(conspiracy$post_length)
#hist(conspiracy$fans_num)
conspiracy[,post_length:=log(post_length)]
conspiracy[,fans_num:=log(fans_num)]

# Regression - Full Model
f1 <- lm (participation ~ DebunkOrConspiracy*cited_source + 
            DebunkOrConspiracy *自然形成 + DebunkOrConspiracy *人工合成 + DebunkOrConspiracy *基因改造 + DebunkOrConspiracy *生物武器 + DebunkOrConspiracy *conspiracy_type_others +
            china_responsibility_only + US_responsibility_only +
            DebunkOrConspiracy *gender + verification_status + DebunkOrConspiracy*fans_num + hubei + user_total_posts +
            emotion_score + emotion_polarity + Anger + Anx + Sad +
            post_length + as.numeric(time_since_posted), data=conspiracy)

f2 <- lm (mobilization ~ DebunkOrConspiracy*cited_source + 
            DebunkOrConspiracy *自然形成 + DebunkOrConspiracy *人工合成 + DebunkOrConspiracy *基因改造 + DebunkOrConspiracy *生物武器 + DebunkOrConspiracy *conspiracy_type_others +
            china_responsibility_only + US_responsibility_only +
            DebunkOrConspiracy*gender + verification_status + DebunkOrConspiracy*fans_num + hubei + user_total_posts +
            emotion_score + emotion_polarity + Anger + Anx + Sad +
            post_length + as.numeric(time_since_posted), data=conspiracy)


#### Visualize the interaction effect of debunking * gender on participation [Panel A in Figure 4]
  # both are categorical variables
mydf1 <- ggpredict(f1, terms = c("DebunkOrConspiracy", "gender"))
mydf1
exp(1.04-1.50) #0.6312836, for debunking posts, male is associated with 36.87% less participation compared to women
exp(0.98-1.13) #0.860708, for conspiracy posts, male is associated with 13.93% less participation compared to women

interact1 <- plot(mydf1) +
  ylab("Log Participation") + ggtitle("Panel A") +
  theme(text = element_text(size = 10), legend.title=element_blank(),legend.text = element_text(size = 7), legend.position="bottom", axis.title.x=element_blank())
ggsave("figure4_panelA.png", plot=interact1, width=4.5, height=4.5, dpi=300)

#### Visualize the interaction effect of debunking * Number of followers on participation [Panel B in Figure 4]
  # debunk is a dummy variable, number of followers are continuous variable
mydf2 <- ggpredict(f1, terms = c("fans_num", "DebunkOrConspiracy"))
mydf2

  # within debunking posts, fans increased by 10% (e.g., from 10000 -> 11000)
p1 <- ggpredict(f1, terms = c("fans_num [9.21034]", "DebunkOrConspiracy[Debunking Posts]"),)
p2 <- ggpredict(f1, terms = c("fans_num [9.305651]", "DebunkOrConspiracy[Debunking Posts]"),)
exp(2.56-2.52) # when number of fans increased by 10%, participation increased by around 4.08%

  # within conspiracy posts, fans increased by 10% (e.g., from 10000 -> 11000)
p3 <- ggpredict(f1, terms = c("fans_num [9.21034]", "DebunkOrConspiracy[Conspiracy Posts]"),)
p4 <- ggpredict(f1, terms = c("fans_num [9.305651]", "DebunkOrConspiracy[Conspiracy Posts]"),)
exp(1.95-1.92) # when number of fans increased by 10%, participation increased by 3.05% for conspiracy posts.


interact2 <- plot(mydf2) + ggtitle("Panel B") +
  ylab("Log Participation") + xlab("Log Number of Followers") +
  theme(text = element_text(size = 10), legend.title=element_blank(), legend.text = element_text(size = 7), legend.position="bottom")
ggsave("figure4_panelB.png", plot=interact2, width=6.5, height=4.5, dpi=300)

  # intercept for Panel B
intercept <- ggpredict(f1, terms = c("fans_num [0]", "DebunkOrConspiracy"),)

#### Visualize the interaction effect debunking * scientists on mobilization [Panel C in Figure 4]
  # both are categorical variables
mydf3 <- ggpredict(f2, terms = c("DebunkOrConspiracy","cited_source"))
mydf3
  # For debunking posts, compare citing scientists vs no source cited
exp(0.54-0.35) #1.20925, citing scientists is associated with around 20.93% increase in mobilization
  # For conspiracy posts, compare citing scientists vs no source cited
exp(0.33-0.37) #0.9607894, citing scientists is associated with around 4% decrease in mobilization

interact3 <- plot(mydf3) + ggtitle("Panel C") +
  ylab("Log Mobilization") + scale_color_discrete(breaks = c("no_source_cite","cite_scientists","only_cite_nonscientist_source"), labels = c("No Source Cited", "Cite Scientists", "Cite Nonscientist Sources"))+
  theme(text = element_text(size = 10), axis.title.x=element_blank(), legend.title=element_blank(), legend.text = element_text(size = 7), legend.position="bottom")
ggsave("figure4_panelC.png", plot=interact3, width=6.5, height=4.5)



#################### Replicate Appendix D. Regression Table Output for the Baseline Model ###########################

m0_0 <- lm(participation ~ DebunkOrConspiracy + 
             gender + verification_status + fans_num + hubei + user_total_posts +
             emotion_score + emotion_polarity + Anger + Anx + Sad +
             post_length + time_since_posted, data=conspiracy)
summary(m0_0)
exp(-0.106) #debunking decreased participation by (1-0.89942) = 10.06%

m0_1 <- lm(mobilization ~ DebunkOrConspiracy + 
             gender + verification_status + fans_num + hubei + user_total_posts +
             emotion_score + emotion_polarity + Anger + Anx + Sad +
             post_length + time_since_posted, data=conspiracy)
summary(m0_1)
exp(0.113) #debunking increased mobilization by 11.96%

stargazer(m0_0,m0_1, style = "apsr")


#################### Replicate Appendix E. Regression Table Output for Figure 4 ############################

stargazer(f1,f2, stype="apsr")


############## Replicate Appendix C: Descriptive Table for Each Hand-Coded Conspiracy Variables  #############

# Origination Types
conspiracy[自然形成==1, .N]
conspiracy[人工合成==1, .N]
conspiracy[基因改造==1, .N]
conspiracy[生物武器==1, .N]
conspiracy[五G==1, .N]
conspiracy[电子烟==1, .N]
conspiracy[转基因作物==1, .N]

# Responsibility Attribution
conspiracy[中国==1, .N]
conspiracy[塞尔维亚==1, .N]
conspiracy[美国==1, .N]
conspiracy[日本==1, .N]
conspiracy[欧洲国家==1, .N]
conspiracy[Bill_Gates==1, .N]
conspiracy[其他国家或主体==1, .N]
conspiracy[无明确==1, .N]

# Source Cited
conspiracy[中国政府==1, .N]
conspiracy[美国政府==1, .N]
conspiracy[其他国家政府==1, .N]
conspiracy[科学研究人员或学者==1, .N]
conspiracy[名人==1, .N]
conspiracy[普通民众==1, .N]
conspiracy[国外媒体==1, .N]
conspiracy[国内媒体==1, .N]
conspiracy[产业企业==1, .N]
conspiracy[非政府组织==1, .N]
conspiracy[其他==1, .N]
conspiracy[无信源==1, .N]



############## Replicate Appendix F: Distribution Table of Narratives #############

d <- fread("Conspiracy_COVID-19_Weibo_dataset_20200917.csv")
conspiracy <- d[信息类型!=2] #n=1516

# Coding rule for 归责对象 (i.e., responsibility attribution) column: 
#0=China, 1=Serbia, 2=The United States, 3=Japan, 4=Other European countries, 5= Bill Gates
#6=Other countries (outside of the above countries), 7=No clear responsibility attribution

nature <- conspiracy[自然形成==1] # 350 posts
table(nature$归责对象)

human_made <- conspiracy[人工合成==1] # 563
table(human_made$归责对象)

geneEdited <- conspiracy[基因改造==1] #636
table(geneEdited$归责对象)

bio_weapon <- conspiracy[生物武器==1] #188
table(bio_weapon$归责对象)

fiveG <- conspiracy[五G==1] #75
table(fiveG$归责对象)

GMO <- conspiracy[转基因作物==1] #7
table(GMO$归责对象)


#########  Replicate the statistics described under Finding 1 the main manuscript part ##########

## chi-square test: debunking or not and responsibility attribution
c1 <- conspiracy[信息类型==0&中国==1,.N]
c2 <- conspiracy[信息类型==0&美国==1,.N]
c3 <- conspiracy[信息类型==1&中国==1,.N]
c4 <- conspiracy[信息类型==1&美国==1,.N]
t4 <- matrix(c(c1,c2,c3,c4),ncol=2,byrow=TRUE)
colnames(t4) <- c("China","US")
rownames(t4) <- c("Not Debunk","Debunk")
t4 <- as.table(t4)
chisq.test(t4,correct=FALSE)

## origination and responsibility attribution for conspiracy posts

# Nature/Unknown origin
nature_conspiracy_only <- nature[信息类型==0] #207 posts
c1 <- nature_conspiracy_only [中国==1,.N]
c2 <- nature_conspiracy_only [美国==1,.N]
c1/(c1+c2) #31.07%

# Human-synthesis
human_conspiracy_only <- human_made[信息类型==0] #377 posts
c1 <- human_conspiracy_only[中国==1,.N]
c2 <- human_conspiracy_only[美国==1,.N]
c1/(c1+c2) #15.36%

# Bioweapon
bio_weapon_conspiracy_only <- bio_weapon[信息类型==0] #153 posts
c1 <- bio_weapon_conspiracy_only[中国==1,.N]
c2 <- bio_weapon_conspiracy_only[美国==1,.N]
c1/(c1+c2) #4.20%
