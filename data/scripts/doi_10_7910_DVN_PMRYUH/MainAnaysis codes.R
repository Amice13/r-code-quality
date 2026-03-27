library(stddiff)
library(grf)
#Upload Data
DATA=read.csv("Calculus1.csv")
dim(DATA)
#Levels and Subgrouops.
summary(factor(DATA$Location))
Nablus=as.numeric(as.factor(DATA$Location==0))-1
DATA$Nablus=Nablus
North=as.numeric(as.factor(DATA$Location==1))-1
DATA$North=North
South=as.numeric(as.factor(DATA$Location==2))-1
DATA$South=South
Pal48=as.numeric(as.factor(DATA$Location==3))-1
DATA$Pal48=Pal48
summary(factor(DATA$Pal48))

summary(factor(DATA$Teacher))
Teacher0=as.numeric(as.factor(DATA$Teacher==0))-1
DATA$Teacher0=Teacher0
Teacher1=as.numeric(as.factor(DATA$Teacher==1))-1
DATA$Teacher1=Teacher1
Teacher2=as.numeric(as.factor(DATA$Teacher==2))-1
DATA$Teacher2=Teacher2
Teacher3=as.numeric(as.factor(DATA$Teacher==3))-1
DATA$Teacher3=Teacher3
Teacher4=as.numeric(as.factor(DATA$Teacher==4))-1
DATA$Teacher4=Teacher4
Teacher5=as.numeric(as.factor(DATA$Teacher==5))-1
DATA$Teacher5=Teacher5
Teacher6=as.numeric(as.factor(DATA$Teacher==6))-1
DATA$Teacher6=Teacher6
Teacher7=as.numeric(as.factor(DATA$Teacher==7))-1
DATA$Teacher7=Teacher7
Teacher8=as.numeric(as.factor(DATA$Teacher==8))-1
DATA$Teacher8=Teacher8
Teacher9=as.numeric(as.factor(DATA$Teacher==9))-1
DATA$Teacher9=Teacher9
Teacher10=as.numeric(as.factor(DATA$Teacher==10))-1
DATA$Teacher10=Teacher10
Teacher11=as.numeric(as.factor(DATA$Teacher==11))-1
DATA$Teacher11=Teacher11
Teacher12=as.numeric(as.factor(DATA$Teacher==12))-1
DATA$Teacher12=Teacher12
Teacher13=as.numeric(as.factor(DATA$Teacher==13))-1
DATA$Teacher13=Teacher13
summary(factor(DATA$Teacher5))

summary(factor(DATA$Department))
Civ.Eng=as.numeric(as.factor(DATA$Department==0))-1
DATA$Civ.Eng=Civ.Eng
Comp.Eng=as.numeric(as.factor(DATA$Department==1))-1
DATA$Comp.Eng=Comp.Eng
Industrial=as.numeric(as.factor(DATA$Department==2))-1
DATA$Industrial=Industrial
Physics=as.numeric(as.factor(DATA$Department==3))-1
DATA$Physics=Physics
IT=as.numeric(as.factor(DATA$Department==4))-1
DATA$IT=IT
MATH=as.numeric(as.factor(DATA$Department==5))-1
DATA$MATH=MATH
Other=as.numeric(as.factor(DATA$Department==6))-1
DATA$Other=Other

summary(factor(DATA$Year))
FirstYear=as.numeric(as.factor(DATA$Year==1))-1
DATA$FirstYear=FirstYear
SecondYear=as.numeric(as.factor(DATA$Year==2))-1
DATA$SecondYear=SecondYear
ThirdYear=as.numeric(as.factor(DATA$Year==3))-1
DATA$ThirdYear=ThirdYear

dim(DATA)
write.csv(DATA,"modifiedCalculus.csv")
DATA$Gender=DATA$Gender-1
summary(DATA$Absence)
DATA$Absence[is.na(DATA$Absence)]<-0
#Descriptive Statistics # Table 1
DATA=subset(DATA,DATA$FirstYear==1)
dim(DATA)
Control=subset(DATA,DATA$Treatment==0)
dim(Control)
Treated=subset(DATA,DATA$Treatment==1)
dim(Treated)
BASELINE=matrix(,nrow = 39,ncol = 5)
BASELINE=as.data.frame(BASELINE)
colnames(BASELINE)=c("Covariate","Face-to-Face","Online","P-value","SD Test")
BASELINE[1:3,1]=c("Gender","Male","Female")
A=t.test(Control$Gender==0,Treated$Gender==0)
B=t.test(Control$Gender==1,Treated$Gender==1)
C=stddiff.binary(data = DATA,"Treatment","Gender")
BASELINE[2,2:5]=c(A$estimate[[1]],A$estimate[[2]],A$p.value,C[[5]]) ; BASELINE[3,2:5]=c(B$estimate[[1]],B$estimate[[2]],B$p.value,C[[5]])
BASELINE[4:7,1]=c("Year","First","Second","Third")
A2=t.test(Control$FirstYear==1,Treated$FirstYear==1)
C2=stddiff.binary(data = DATA,"Treatment","FirstYear")
BASELINE[5,2:5]=c(A2$estimate[[1]],A2$estimate[[2]],A2$p.value,C2[[5]])
A3=t.test(Control$SecondYear,Treated$SecondYear)
C3=stddiff.binary(data = DATA,"Treatment","SecondYear")
BASELINE[6,2:5]=c(A3$estimate[[1]],A3$estimate[[2]],A3$p.value,C3[[5]])
A4=t.test(Control$ThirdYear,Treated$ThirdYear)
C4=stddiff.binary(data = DATA,"Treatment","ThirdYear")
BASELINE[7,2:5]=c(A4$estimate[[1]],A4$estimate[[2]],A4$p.value,C4[[5]])

BASELINE[8:12,1]=c("Location","Nablus","North","South","Palestine1948")
A5=t.test(Control$Nablus==1,Treated$Nablus==1)
C5=stddiff.binary(data = DATA,"Treatment","Nablus")
BASELINE[9,2:5]=c(A5$estimate[[1]],A5$estimate[[2]],A5$p.value,C5[[5]])

A6=t.test(Control$North,Treated$North)
C6=stddiff.binary(data = DATA,"Treatment","North")
BASELINE[10,2:5]=c(A6$estimate[[1]],A6$estimate[[2]],A6$p.value,C6[[5]])

A7=t.test(Control$South,Treated$South)
C7=stddiff.binary(data = DATA,"Treatment","South")
BASELINE[11,2:5]=c(A7$estimate[[1]],A7$estimate[[2]],A7$p.value,C7[[5]])

A8=t.test(Control$Pal48,Treated$Pal48)
C8=stddiff.binary(data = DATA,"Treatment","Pal48")
BASELINE[12,2:5]=c(A8$estimate[[1]],A8$estimate[[2]],A8$p.value,C8[[5]])

BASELINE[13,1]=c("Incomplete Final Exam")
A9=t.test(Control$Incomplete==1,Treated$Incomplete==1)
C9=stddiff.binary(data = DATA,"Treatment","Incomplete")
BASELINE[13,2:5]=c(A9$estimate[[1]],A9$estimate[[2]],A9$p.value,C9[[5]])

BASELINE[14:28,1]=c("Teacher","Teacher1","Teacher2","Teacher3","Teacher4","Teacher5",
                    "Teacher6","Teacher7","Teacher8","Teacher9","Teacher10","Teacher11",
                    "Teacher12","Teacher13","Teacher14")
A10=t.test(Control$Teacher0==1,Treated$Teacher0==1)
C10=stddiff.binary(data = DATA,"Treatment","Teacher0")
BASELINE[15,2:5]=c(A10$estimate[[1]],A10$estimate[[2]],A10$p.value,C10[[5]])

A11=t.test(Control$Teacher1==1,Treated$Teacher1==1)
C11=stddiff.binary(data = DATA,"Treatment","Teacher1")
BASELINE[16,2:5]=c(A11$estimate[[1]],A11$estimate[[2]],A11$p.value,C11[[5]])
A12=t.test(Control$Teacher2==1,Treated$Teacher2==1)
C12=stddiff.binary(data = DATA,"Treatment","Teacher2")
BASELINE[17,2:5]=c(A12$estimate[[1]],A12$estimate[[2]],A12$p.value,C12[[5]])
A13=t.test(Control$Teacher3==1,Treated$Teacher3==1)
C13=stddiff.binary(data = DATA,"Treatment","Teacher3")
BASELINE[18,2:5]=c(A13$estimate[[1]],A13$estimate[[2]],A13$p.value,C13[[5]])
A14=t.test(Control$Teacher4==1,Treated$Teacher4==1)
C14=stddiff.binary(data = DATA,"Treatment","Teacher4")
BASELINE[19,2:5]=c(A14$estimate[[1]],A14$estimate[[2]],A14$p.value,C14[[5]])
A15=t.test(Control$Teacher5==1,Treated$Teacher5==1)
C15=stddiff.binary(data = DATA,"Treatment","Teacher5")
BASELINE[20,2:5]=c(A15$estimate[[1]],A15$estimate[[2]],A15$p.value,C15[[5]])

A16=t.test(Control$Teacher6==1,Treated$Teacher6==1)
C16=stddiff.binary(data = DATA,"Treatment","Teacher6")
BASELINE[21,2:5]=c(A16$estimate[[1]],A16$estimate[[2]],A16$p.value,C16[[5]])


A17=t.test(Control$Teacher7==1,Treated$Teacher7==1)
C17=stddiff.binary(data = DATA,"Treatment","Teacher7")
BASELINE[22,2:5]=c(A17$estimate[[1]],A17$estimate[[2]],A17$p.value,C17[[5]])

A18=t.test(Control$Teacher8==1,Treated$Teacher8==1)
C18=stddiff.binary(data = DATA,"Treatment","Teacher8")
BASELINE[23,2:5]=c(A18$estimate[[1]],A18$estimate[[2]],A18$p.value,C18[[5]])

A19=t.test(Control$Teacher9==1,Treated$Teacher9==1)
C19=stddiff.binary(data = DATA,"Treatment","Teacher9")
BASELINE[24,2:5]=c(A19$estimate[[1]],A19$estimate[[2]],A19$p.value,C19[[5]])


A20=t.test(Control$Teacher10==1,Treated$Teacher10==1)
C20=stddiff.binary(data = DATA,"Treatment","Teacher10")
BASELINE[25,2:5]=c(A20$estimate[[1]],A20$estimate[[2]],A20$p.value,C20[[5]])

A21=t.test(Control$Teacher11==1,Treated$Teacher11==1)
C21=stddiff.binary(data = DATA,"Treatment","Teacher11")
BASELINE[26,2:5]=c(A21$estimate[[1]],A21$estimate[[2]],A21$p.value,C21[[5]])

A22=t.test(Control$Teacher12==1,Treated$Teacher12==1)
C22=stddiff.binary(data = DATA,"Treatment","Teacher12")
BASELINE[27,2:5]=c(A22$estimate[[1]],A22$estimate[[2]],A22$p.value,C22[[5]])

A23=t.test(Control$Teacher13==1,Treated$Teacher13==1)
C23=stddiff.binary(data = DATA,"Treatment","Teacher13")
BASELINE[28,2:5]=c(A23$estimate[[1]],A23$estimate[[2]],A23$p.value,C23[[5]])

BASELINE[29:36,1]=c("Department","Civil/Arch","Computer Engineering","Industrial Eng","Physics","Information Tech",
                    "Mathematics","Other")

A24=t.test(Control$Civ.Eng==1,Treated$Civ.Eng==1)
C24=stddiff.binary(data = DATA,"Treatment","Civ.Eng")
BASELINE[30,2:5]=c(A24$estimate[[1]],A24$estimate[[2]],A24$p.value,C24[[5]])
A25=t.test(Control$Comp.Eng==1,Treated$Comp.Eng==1)
C25=stddiff.binary(data = DATA,"Treatment","Comp.Eng")
BASELINE[31,2:5]=c(A25$estimate[[1]],A25$estimate[[2]],A25$p.value,C25[[5]])
A26=t.test(Control$Industrial==1,Treated$Industrial==1)
C26=stddiff.binary(data = DATA,"Treatment","Industrial")
BASELINE[32,2:5]=c(A26$estimate[[1]],A26$estimate[[2]],A26$p.value,C26[[5]])

A27=t.test(Control$Physics==1,Treated$Physics==1)
C27=stddiff.binary(data = DATA,"Treatment","Physics")
BASELINE[33,2:5]=c(A27$estimate[[1]],A27$estimate[[2]],A27$p.value,C27[[5]])

A28=t.test(Control$IT==1,Treated$IT==1)
C28=stddiff.binary(data = DATA,"Treatment","IT")
BASELINE[34,2:5]=c(A28$estimate[[1]],A28$estimate[[2]],A28$p.value,C28[[5]])

A29=t.test(Control$MATH==1,Treated$MATH==1)
C29=stddiff.binary(data = DATA,"Treatment","MATH")
BASELINE[35,2:5]=c(A29$estimate[[1]],A29$estimate[[2]],A29$p.value,C29[[5]])

A30=t.test(Control$Other==1,Treated$Other==1)
C30=stddiff.binary(data = DATA,"Treatment","Other")
BASELINE[36,2:5]=c(A30$estimate[[1]],A30$estimate[[2]],A30$p.value,C30[[5]])

BASELINE[37:38,1]=c("Failed %","High School Score")
A31=t.test(Control$Failed==1,Treated$Failed==1)
C31=stddiff.binary(data = DATA,"Treatment","Failed")
BASELINE[37,2:5]=c(A31$estimate[[1]],A31$estimate[[2]],A31$p.value,C31[[5]])

A32=t.test(Control$HighSchool,Treated$HighSchool)
C32=stddiff.numeric(data = DATA,"Treatment","HighSchool")
BASELINE[38,2:5]=c(A32$estimate[[1]],A32$estimate[[2]],A32$p.value,C32[[7]])

A33=t.test(Control$Absence,Treated$Absence)
C33=stddiff.numeric(data = DATA,"Treatment","Absence")
BASELINE[39,2:5]=c(A33$estimate[[1]],A33$estimate[[2]],A33$p.value,C33[[7]])

write.csv(BASELINE,"Table129Nov.csv")

# Average Effects 
#1- Simple T-test
t.test(Treated$Y,Control$Y)

#2-Simple Linear Regression
LR1=lm(DATA$Y~DATA$Treatment+DATA$Gender+DATA$Nablus+DATA$North+DATA$South+DATA$Pal48+DATA$Incomplete+
         DATA$Teacher0+DATA$Teacher1+DATA$Teacher2+DATA$Teacher3+DATA$Teacher4+DATA$Teacher5+DATA$Teacher6+
         DATA$Teacher7+DATA$Teacher8+DATA$Teacher9+DATA$Teacher10+DATA$Teacher11+DATA$Teacher12+DATA$Teacher13+
         DATA$Civ.Eng+DATA$Comp.Eng+DATA$Industrial+DATA$Physics+DATA$IT+DATA$MATH+DATA$Other+
         DATA$Failed+DATA$HighSchool+DATA$Absence)
summary(LR1)

LR2=lm(DATA$Y~DATA$Treatment+DATA$Gender+DATA$Nablus+DATA$North+DATA$South+DATA$Incomplete+
         DATA$Teacher0+DATA$Teacher1+DATA$Teacher2+DATA$Teacher3+DATA$Teacher4+DATA$Teacher5+DATA$Teacher6+
         DATA$Teacher7+DATA$Teacher8+DATA$Teacher9+DATA$Teacher10+DATA$Teacher11+DATA$Teacher12+
         DATA$Civ.Eng+DATA$Comp.Eng+DATA$Industrial+DATA$Physics+DATA$IT+DATA$MATH+
         DATA$Failed+DATA$HighSchool+DATA$Absence)
summary(LR2)

#3-Causal Forest
set.seed(777)
Xs=cbind(DATA$Gender,DATA$Nablus,DATA$North,
         DATA$South,DATA$Pal48,DATA$Incomplete,DATA$Teacher0,DATA$Teacher1,DATA$Teacher2,DATA$Teacher3,DATA$Teacher4
         ,DATA$Teacher5,DATA$Teacher6,DATA$Teacher7,DATA$Teacher8,DATA$Teacher9,DATA$Teacher10,DATA$Teacher11,DATA$Teacher12
         ,DATA$Teacher13,DATA$Civ.Eng,DATA$Comp.Eng,DATA$Industrial,DATA$Physics,DATA$IT,DATA$MATH,DATA$Other,
         DATA$Failed,DATA$HighSchool,DATA$Absence)
dim(Xs)
Outcome=DATA$Y
Treatment=DATA$Treatment
w.hat=regression_forest(Xs,Treatment,num.trees = 20000)
propensity_scores=pmax(pmin(w.hat$predictions, 0.99), 0.01)
hist(propensity_scores)
CF1=causal_forest(Xs,Outcome,Treatment,W.hat=propensity_scores,num.trees = 20000,tune.parameters="all")
average_treatment_effect(CF1)
average_treatment_effect(CF1)[[1]]-(1.96)*average_treatment_effect(CF1)[[2]];average_treatment_effect(CF1)[[1]]+(1.96)*average_treatment_effect(CF1)[[2]]
test_calibration(CF1)

T1M=(0.97-1)/0.32
T1H=(1.12-1)/0.13



Impo=variable_importance(CF1)
round(Impo,digits = 3)

set.seed(777)
XsMost=cbind(DATA$Gender,DATA$Nablus,DATA$North,
         DATA$South,DATA$Pal48,DATA$Teacher0,DATA$Teacher1,DATA$Teacher2,DATA$Teacher3,DATA$Teacher4
         ,DATA$Teacher5,DATA$Teacher7,DATA$Teacher8,DATA$Teacher11,DATA$Teacher12
         ,DATA$Civ.Eng,DATA$Comp.Eng,DATA$Industrial,DATA$Physics,DATA$IT,DATA$MATH,DATA$Other,
         DATA$Failed,DATA$HighSchool,DATA$Absence)
dim(XsMost)
Outcome=DATA$Y
Treatment=DATA$Treatment
w.hat=regression_forest(XsMost,Treatment,num.trees = 20000)
propensity_scores=pmax(pmin(w.hat$predictions, 0.99), 0.01)
hist(propensity_scores)
CF2=causal_forest(XsMost,Outcome,Treatment,W.hat=propensity_scores,num.trees = 20000,tune.parameters="all")
average_treatment_effect(CF2)
average_treatment_effect(CF2)[[1]]-(1.96)*average_treatment_effect(CF2)[[2]];average_treatment_effect(CF2)[[1]]+(1.96)*average_treatment_effect(CF2)[[2]]
test_calibration(CF2)

T1M=(0.99-1)/0.27
T1H=(1.16-1)/0.12



Impo2=variable_importance(CF2)
round(Impo2,digits = 3)



hist(CF2$predictions)

#####Catterpillar Plot
library(ggplot2)
library(metafor)
ITEs=predict(CF2,estimate.variance = TRUE)
range(ITEs[,1])
response=rma(ITEs[,1],ITEs[,2])
response
head(ITEs)
par(mfrow=c(1,2))
forest(response,xlim = c(-15,20),order ="obs",slab = NA,annotate = FALSE,efac = 2.56,pch = 19,col = "black",colout = "gray",
       psize = 2,cex.lab = 1,cex.axis = 1,lty=c("solid","blank"),xlab = "Individualized Effects on the final score",main="Personalized Effects")


lines(sort(ITEs[,1]),2210:1,pch=19,cex=0.2,col="black")
points(sort(ITEs[,1]),2210:1,pch=19,cex=0.2,col="black")
addpoly(response,mlab = "",cex=1,col = "red")


#Subgroups
HSS=DATA$HighSchool
HSS1=cut(HSS,c(59.99,69.99,79.99,89.99,99.99))
summary(HSS1)
DATA$TawjihiSub=HSS1
SubgroupData=DATA[c("Gender","Location","Incomplete",
                    "Teacher","Department",
                    "Failed","TawjihiSub")]


dim(SubgroupData)
head(SubgroupData)
#Subgroups
cates <- data.frame(var = rep(NA, 36), level = NA, cate = NA, lb=NA,ub=NA)
counter <- 1
for (i in seq_along(SubgroupData[1,])) {
  for (j in levels(factor(SubgroupData[,i]))) {
    tmp <- average_treatment_effect(
      CF2, 
      subset = SubgroupData[,i] == j
    )
    cates$var[[counter]] <- i
    cates$level[[counter]] <- j
    cates$cate[[counter]] <- tmp[[1]]
    cates$lb[[counter]] <- tmp[[1]]-1.96*tmp[[2]]
    cates$ub[[counter]] <- tmp[[1]]+1.96*tmp[[2]]
    counter <- counter + 1
  }
  
}

names(SubgroupData)


dim(cates)
cates=data.matrix(cates)
cates=as.data.frame(cates)

CATES=cates[1:35,]

CATES$var=c("Gender","Gender","Location","Location","Location","Location","Incomplete",
            "Incomplete","Teacher","Teacher","Teacher","Teacher","Teacher","Teacher","Teacher","Teacher","Teacher","Teacher",
            "Teacher","Teacher","Teacher","Teacher","Department","Department","Department","Department","Department","Department",
            "Department","Failed before","Failed before","High School Score","High School Score","High School Score","High School Score"
)


CATES$level=c("Male","Female","Nablus","North","South","Palestine1948","No",
              "Yes","Teacher1","Teacher2","Teacher3","Teacher4","Teacher5","Teacher6","Teacher7","Teacher8","Teacher9","Teacher10",
              "Teacher11","Teacher12","Teacher13","Teacher14","Civil Engineering","Computer Engineering","Industrial Engineering",
              "Physics","Information Technology","Mathematics","Other","No","Yes","60-69.99","70-79.99","80-89.99","90-99.99")


#Add Average
#Avedd=matrix(,nrow = 1,ncol = 5)
#Avedd=as.data.frame(Avedd)
#colnames(Avedd)=colnames(CATES)
#Avedd[1,]=c("Population","Overall",2.56,1.23,3.9)
#CATES=rbind(Avedd,CATES)
#dim(CATES)
write.csv(CATES,"Subgroups6December.csv")
summary(factor(SubgroupData$Gender))
summary(factor(SubgroupData$Location))
summary(factor(SubgroupData$Incomplete))
summary(factor(SubgroupData$Teacher))
summary(factor(SubgroupData$Department))
summary(factor(SubgroupData$Failed))
summary(factor(SubgroupData$TawjihiSub))

# Create a combined label for variable and level
CATES$label <- paste(CATES$var, CATES$level, sep = ": ")
CATES$label<-c("..Gender: Male","Female","..Location: Nablus","North West Bank","South West Bank","Palestine1948",
               "..No Incomplete Exam","Did Incomplete exam","..Teacher:1","Teacher:2","Teacher:3","Teacher:4","Teacher:5",
               "Teacher:6","Teacher:7","Teacher:8","Teacher:9","Teacher:10","Teacher:11","Teacher:12","Teacher:13","Teacher:14",
               "..Department: Civil Engineering","Computer Engineering","Industrial Engineering","Physics",
               "Information Technology","Mathematics","Other","..Did not fail before","Failed before","..High School Score: 60-69.99"
               ,"70-79.99","80-89.99","90-99.99")
# Reverse the order for plotting
CATES$label <- factor(CATES$label, levels = rev(CATES$label))
head(CATES)


library(ggplot2)
forest_plot <- ggplot(CATES, aes(x = cate, y = label)) +
  geom_point(size = 3) +  # Add points for the effect estimates
  geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.2) +  # Add error bars
  theme_minimal() +  # Use a clean theme
  labs(
    x = "Effect Size (95% CI)", 
    y = "",
    title = "Subgroup Analysis"
  ) +
  geom_vline(xintercept = 2.56, linetype = "dashed", color = "red") +  # Add vertical line at ATE
  geom_rect(aes(xmin = 1.23, xmax = 3.90, ymin = -Inf, ymax = Inf),  # Add shaded CI region
            fill = "blue", alpha = 0.01) +  # Transparent blue shading
  # Add only point estimates and CIs in the margin
  geom_text(aes(x = min(lb) - 0.9, label = sprintf("%.2f (%.2f, %.2f)", cate, lb, ub)), 
            hjust = 0.5, size = 3, color = "black") +  # Margin text for estimates
  theme(
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    plot.title = element_text(hjust = 0.3)  # Center title
  )

# Print the plot
print(forest_plot)

##############
