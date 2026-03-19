
#############
# appendix  #
#############

library(psych)
library(xtable)

###------load data

cdl<-read.csv("covid19_normal_lucid.csv")
attach(cdl)


cd<-read.csv("covid19_normal.csv")
attach(cd)

############Simple descriptive statistics: Table 1 and 2###############


###------setting variables: Yahoo


#age
cd$age<-as.numeric(cd$Q2.2)
cd$age[cd$age==444]<-NA
cd$age[cd$age==19750618]<-NA

#income
cd$income<-as.numeric(cd$Q4.1_1)


#gender
cd$genderd<-as.numeric(cd$Q2.1)
cd$genderd[cd$gender==2]<-0
cd$genderd[cd$gender==3]<-NA
cd$genderd[cd$gender==4]<-NA

#education
cd$educationd<-cd$Q2.4
cd$educationd[cd$Q2.4==1]<-0
cd$educationd[cd$Q2.4==2]<-0
cd$educationd[cd$Q2.4==3]<-0
cd$educationd[cd$Q2.4==4]<-1
cd$educationd[cd$Q2.4==5]<-1
cd$educationd[cd$Q2.4==6]<-NA
cd$educationd[cd$Q2.4==7]<-NA

des_yahoo<-data.frame(cd$age,cd$genderd,cd$educationd,cd$income)

xtable(psych::describe(des_yahoo))

###------setting variables: Lucid
#age
cdl$agel<-as.numeric(cdl$Q2.2)
cdl$agel[cdl$agel==3690306]<-NA
cdl$agel[cdl$agel==5770818]<-NA
cdl$agel[cdl$agel==97]<-NA
cdl$agel[cdl$agel==100]<-NA
cdl$agel[cdl$agel==2]<-NA
cdl$agel[cdl$agel==3]<-NA
cdl$agel[cdl$agel==5]<-NA
cdl$agel[cdl$agel==11]<-NA
cdl$agel[cdl$agel==12]<-NA
cdl$agel[cdl$agel==13]<-NA
cdl$agel[cdl$agel==14]<-NA
cdl$agel[cdl$agel==15]<-NA
cdl$agel[cdl$agel==16]<-NA
cdl$agel[cdl$agel==17]<-NA



#income
cdl$incomel<-as.numeric(cdl$Q4.1_1)



#gender
cdl$genderdl<-as.numeric(cdl$Q2.1)
cdl$genderdl[cdl$genderl==3]<-NA
cdl$genderdl[cdl$genderl==4]<-NA
cdl$genderdl[cdl$genderl==2]<-0

#education
cdl$educationdl<-cdl$Q2.4
cdl$educationdl[cdl$Q2.4==1]<-0
cdl$educationdl[cdl$Q2.4==2]<-0
cdl$educationdl[cdl$Q2.4==3]<-0
cdl$educationdl[cdl$Q2.4==4]<-1
cdl$educationdl[cdl$Q2.4==5]<-1
cdl$educationdl[cdl$Q2.4==6]<-NA
cdl$educationdl[cdl$Q2.4==7]<-NA


des_luc<-data.frame(cdl$agel,cdl$genderdl,cdl$educationdl,cdl$incomel)

xtable(psych::describe(des_luc))



############comparison with census: Table3 and 4#######################

###------setting variables: Yahoo


#age
cd$age<-as.numeric(cd$Q2.2)
length(na.omit(cd$age))

#income
cd$income<-as.numeric(cd$Q4.1_1)
length(na.omit(cd$income))


#gender
cd$gender<-as.numeric(cd$Q2.1)
cd$gender[cd$gender==3]<-NA
cd$gender[cd$gender==4]<-NA
length(na.omit(cd$gender))

#education
cd$education<-cd$Q2.4
cd$education[cd$Q2.4==1]<-1
cd$education[cd$Q2.4==2]<-2
cd$education[cd$Q2.4==3]<-3
cd$education[cd$Q2.4==4]<-4
cd$education[cd$Q2.4==5]<-4
cd$education[cd$Q2.4==6]<-NA
cd$education[cd$Q2.4==7]<-NA


###------setting variables: Lucid
#age
cdl$agel<-as.numeric(cdl$Q2.2)
length(na.omit(cdl$agel))

#income
cdl$incomel<-as.numeric(cdl$Q4.1_1)
length(na.omit(cdl$incomel))


#gender
cdl$genderl<-as.numeric(cdl$Q2.1)
cdl$genderl[cdl$genderl==3]<-NA
cdl$genderl[cdl$genderl==4]<-NA
length(na.omit(cdl$genderl))

#education
cdl$educationl<-cdl$Q2.4
cdl$educationl[cdl$Q2.4==1]<-1
cdl$educationl[cdl$Q2.4==2]<-2
cdl$educationl[cdl$Q2.4==3]<-3
cdl$educationl[cdl$Q2.4==4]<-4
cdl$educationl[cdl$Q2.4==5]<-4
cdl$educationl[cdl$Q2.4==6]<-NA
cdl$educationl[cdl$Q2.4==7]<-NA



###population check###

###age

#Lucid
tbagel<-table(cut(x=cdl$agel,breaks=c(20,25,30,35,40,45,50,55,60,65,70,75),right=FALSE))
tb_proage_lu<-prop.table(tbagel)

#yahoo
tbage<-table(cut(x=cd$age,breaks=c(20,25,30,35,40,45,50,55,60,65,70,75),right=FALSE))
tb_proage<-prop.table(tbage)


#country
country_age<-c(639,	630,	651,	735,	826,
               975,	879,	791,	738,	803,954)
tb_country_proage<-country_age/sum(country_age)


t.test(tb_proage_lu,tb_country_proage)
t.test(tb_proage,tb_country_proage)

tb_agey<-cbind(tb_country_proage,tb_proage_lu)
tb_agel<-cbind(tb_country_proage,tb_proage)

###income

#country
country_income<-c(6.6, 	12.7, 	13.9, 	13.3, 	10, 	8.9,
                  7.1, 	6.2, 	5.1, 	3.9, 	5.1, 	3, 	5.1, 0.9,0.4,1.2)

#lucid
incomecutl<-prop.table(table(cut(cdl$incomel,breaks=c(0,100,200,300,400,500,
                               600,700,800,900,1000,1100,1400,1600,1800,2000,3000))))


#yahoo
incomecut<-prop.table(table(cut(cd$income,breaks=c(0,100,200,300,400,500,
                                                   600,700,800,900,1000,1100,1400,1600,
                                                   1800,2000,3000))))

t.test(country_income/100,incomecutl)
t.test(country_income/100,incomecut)

tb_incomey<-cbind(country_income/100,incomecut)
tb_incomel<-cbind(country_income/100,incomecutl)



###gender

#country
country_gender<-c(6446/12548,6103/12548)

#lucid
genderl<-prop.table(table(cdl$genderl))

#yahoo
gender<-prop.table(table(cd$gender))


t.test(country_gender,genderl)
t.test(country_gender,gender)

tb_gendery<-cbind(country_gender,gender)
tb_genderl<-cbind(country_gender,genderl)

###educationl
#country
country_education<-c(0.188,0.465,0.148,0.199)

#lucid
pro_educationl<-prop.table(table(cdl$educationl))

#yahoo
pro_education<-prop.table(table(cd$education))

t.test(country_education,pro_educationl)
t.test(country_education,pro_education)


tb_educationy<-cbind(country_education,pro_education)
tb_educationl<-cbind(country_education,pro_educationl)


###make table

#lucid
tb_lucid<-rbind(tb_agel,tb_genderl,tb_educationl,tb_incomel)

#yahoo
tb_ycs<-rbind(tb_agey,tb_gendery,tb_educationy,tb_incomey)

xtable(tb_lucid)
xtable(tb_ycs)
