#3-Causal Forest-CLUSTERED
set.seed(777)
Xs=cbind(DATA$Gender,DATA$Nablus,DATA$North,
         DATA$South,DATA$Pal48,DATA$Incomplete,DATA$Teacher0,DATA$Teacher1,DATA$Teacher2,DATA$Teacher3,DATA$Teacher4
         ,DATA$Teacher5,DATA$Teacher6,DATA$Teacher7,DATA$Teacher8,DATA$Teacher9,DATA$Teacher10,DATA$Teacher11,DATA$Teacher12
         ,DATA$Teacher13,DATA$Civ.Eng,DATA$Comp.Eng,DATA$Industrial,DATA$Physics,DATA$IT,DATA$MATH,DATA$Other,
         DATA$Failed,DATA$HighSchool,DATA$Absence)
dim(Xs)
Outcome=DATA$Y
Treatment=DATA$Treatment
w.hat=regression_forest(Xs,Treatment,num.trees = 20000,clusters = DATA$Teacher)
propensity_scores=pmax(pmin(w.hat$predictions, 0.99), 0.01)
hist(propensity_scores)
CF1=causal_forest(Xs,Outcome,Treatment,W.hat=propensity_scores,num.trees = 20000,tune.parameters="all",clusters = 
                    DATA$Teacher)
average_treatment_effect(CF1)
average_treatment_effect(CF1)[[1]]-(1.96)*average_treatment_effect(CF1)[[2]];average_treatment_effect(CF1)[[1]]+(1.96)*average_treatment_effect(CF1)[[2]]
test_calibration(CF1)

T1M=(0.97-1)/0.32
T1H=(1.12-1)/0.13



Impo=variable_importance(CF1)
round(Impo,digits = 3)
0.20*mean(Impo)
set.seed(777)
XsMost=cbind(DATA$Gender,DATA$Nablus,DATA$North,
             DATA$Teacher1,DATA$Teacher7,DATA$Teacher11
             ,DATA$Civ.Eng,DATA$Comp.Eng,DATA$IT,
             DATA$Failed,DATA$HighSchool,DATA$Absence)
dim(XsMost)
Outcome=DATA$Y
Treatment=DATA$Treatment
w.hat=regression_forest(XsMost,Treatment,num.trees = 20000,clusters = DATA$Teacher)
propensity_scores=pmax(pmin(w.hat$predictions, 0.99), 0.01)
hist(propensity_scores)
CF2=causal_forest(XsMost,Outcome,Treatment,W.hat=propensity_scores,num.trees = 20000,tune.parameters="all",
                  clusters = DATA$Teacher)
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
par(mfrow=c(1,1))
forest(response,xlim = c(-15,20),order ="obs",slab = NA,annotate = FALSE,efac = 2.56,pch = 19,col = "black",colout = "gray",
       psize = 2,cex.lab = 1,cex.axis = 1,lty=c("solid","blank"),xlab = "Individualized Effects on the final score",main="Personalized Effects")


lines(sort(ITEs[,1]),2210:1,pch=19,cex=0.2,col="black")
points(sort(ITEs[,1]),2210:1,pch=19,cex=0.2,col="black")
addpoly(response,mlab = "",cex=1,col = "red")


#Subgroups
HSS=DATA$HighSchool
HSS1=cut(HSS,c(59.99,69.99,79.99,89.99,99.99))
summary(HSS1)
length(DATA$TawjihiSub)
dim(DATA)
DATA$TawjihiSub=HSS1
SubgroupData=DATA[c("Gender","Location","Incomplete",
                    "Department",
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

CATES=cates[1:21,]

CATES$var=c("Gender","Gender","Location","Location","Location","Location","Incomplete",
            "Incomplete","Department","Department","Department","Department","Department","Department",
            "Department","Failed before","Failed before","High School Score","High School Score","High School Score","High School Score"
)


CATES$level=c("Male","Female","Nablus","North","South","Palestine1948","No",
              "Yes","Civil Engineering","Computer Engineering","Industrial Engineering",
              "Physics","Information Technology","Mathematics","Other","No","Yes","60-69.99","70-79.99","80-89.99","90-99.99")


#Add Average
#Avedd=matrix(,nrow = 1,ncol = 5)
#Avedd=as.data.frame(Avedd)
#colnames(Avedd)=colnames(CATES)
#Avedd[1,]=c("Population","Overall",2.56,1.23,3.9)
#CATES=rbind(Avedd,CATES)
#dim(CATES)
write.csv(CATES,"Subgroups2025.csv")
summary(factor(SubgroupData$Gender))
summary(factor(SubgroupData$Location))
summary(factor(SubgroupData$Incomplete))
summary(factor(SubgroupData$Teacher))
summary(factor(SubgroupData$Department))
summary(factor(SubgroupData$Failed))
summary(factor(SubgroupData$TawjihiSub))

# Create a combined label for variable and level
CATES$label <- paste(CATES$var, CATES$level, sep = ": ")
CATES$label<-c("..Gender: Male","Female","..Location: Center","North ","South ","External",
               "..No Incomplete Exam","Did Incomplete exam",
               "..Department: Civil Engineering","Computer Engineering","Industrial Engineering","Physics",
               "Information Technology","Mathematics","Other","..Did not fail before","Failed before","..High School Score: 60-69.99"
               ,"70-79.99","80-89.99","90-99.99")
# Reverse the order for plotting
CATES$label <- factor(CATES$label, levels = rev(CATES$label))
head(CATES)
class(CATES)

library(ggplot2)
forest_plot <- ggplot(CATES, aes(x = cate, y = label)) +
  geom_point(size = 3) +  # Add points for the effect estimates
  geom_errorbarh(aes(xmin = lb, xmax = ub), height = 0.2) +  # Add error bars
  theme_minimal() +  # Use a clean theme
  labs(
    x = "Effect Size (95% CI)", 
    y = "",
    title = "Subgroup Analysis - with clusters"
  ) +
  geom_vline(xintercept = 2.58, linetype = "dashed", color = "red") +  # Add vertical line at ATE
  geom_rect(aes(xmin = 0.033, xmax = 5.134, ymin = -Inf, ymax = Inf),  # Add shaded CI region
            fill = "blue", alpha = 0.01) +  # Transparent blue shading
  # Add only point estimates and CIs in the margin
  geom_text(aes(x = min(lb) - 0.9, label = sprintf("%.2f (%.2f, %.2f)", cate, lb, ub)), 
            hjust = 0.4, size = 3, color = "black") +  # Margin text for estimates
  theme(
    axis.text.y = element_text(size = 9),  # Adjust y-axis text size
    plot.title = element_text(hjust = 0.2)  # Center title
  )

# Print the plot
print(forest_plot)
