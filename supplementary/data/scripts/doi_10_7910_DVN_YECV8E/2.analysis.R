# from RawData.rds to RawData2.rds
# panoramic view and variable organization for English publication

# se recomeçar da planilha, campos a mais do database

folderout <- "analysis"
filename <- "Anaysis"
filermd <- file.path(folderout,filename)
out.format.exec <- "html_document"

fileout <- eiras::CreateMarkdown(filermd,
                                 title="VERAS, first analysis",
                                 author="Paulo S. P. Silveira, Jose O. Siqueira")
# recupera o RDS
data <- readRDS("RawData2.rds")
data$Group.num <- as.numeric(NA)
data$Group.num[data$Group=="V"] <- 1
data$Group.num[data$Group=="R"] <- 0

txt <- '
```{r echo=FALSE, fig.width=9, fig.height=20}
grf <- eiras::Missing(subset(data,select=Sex:WHOQOL),x.for.labels=110,col.width=50,plot=TRUE,cex=0.8)
```
'
eiras::Add2Markdown(fileout,txt,"text")

data$SchoolType <- factor(data$SchoolType)
data$CityLocation <- factor(data$CityLocation)
data$State <- factor(data$State)
data$CourseYear <- factor(data$CourseYear)
data$GovernAid[is.na(data$GovernAid)] <- "No"
data$GovernAid <- factor(data$GovernAid, levels=c("Yes","No"))

o.data <- data

for (i in 1:8)
{
  data <- o.data
  if(i==1)
  {
    selected.tit <- "Global scores"
    selected.short <- "Global scores"
    selected.var <- c(
      "Group",
      "Group.num",
      "GQoL","CQoL", 
      "WHOQOL",
      "DREEM",
      "VERASQ",
      "IRI",
      "RESILIENCE",
      "BURNOUT",
      "PSQI",
      "STAI_state",          
      "STAI_trace",
      "BDI",
      "EPWORTH"
    )
  }
  if (i==2)
  {
    selected.tit <- "World Heath Organization Quality of Life Questionnaire: short form"
    selected.short <- "WHOQOL"
    selected.var <- c(
      "Group",
      "Group.num",
      "WHOQOL_QoL","WHOQOL_health","WHOQOL_phys","WHOQOL_psy","WHOQOL_soc","WHOQOL_env"
      )
  }
  if (i==3)
  {
    selected.tit <- "Dundee Ready Educational Environment Measure"
    selected.short <- "DREEM"
    selected.var <- c(
      "Group",
      "Group.num",
      "DREEM_learn","DREEM_teacher","DREEM_self","DREEM_env","DREEM_soc" 
    )
  }
  if (i==4)
  {
    selected.tit <- "Student Quality of Life Questionnaire"
    selected.short <- "VERASQ"
    selected.var <- c(
      "Group",
      "Group.num",
      "VERASQ_time","VERASQ_psy","VERASQ_phys","VERASQ_env" 
    )
  }
  if (i==5)
  {
    selected.tit <- "Davis Interpersonal Reactivity Index"
    selected.short <- "IRI"
    selected.var <- c(
      "Group",
      "Group.num",
      "IRI_empathy","IRI_perspec","IRI_distress"
    )
  }
  if (i==6)
  {
    selected.tit <- "Wagnild and Young's Resilience Scale (RS-14)"
    selected.short <- "RESILIENCE"
    selected.var <- c(
      "Group",
      "Group.num",
      "RESILIENCE_selfreliance","RESILIENCE_meaningful","RESILIENCE_equaninity","RESILIENCE_perseverance","RESILIENCE_aloneness"  
    )
  }
  if (i==7)
  {
    selected.tit <- "Maslach Burnout Inventory"
    selected.short <- "BURNOUT"
    selected.var <- c(
      "Group",
      "Group.num",
      "BURNOUT_exhaut","BURNOUT_deperson","BURNOUT_accomp"
    )
  }
  if (i==8)
  {
    selected.tit <- "Pittsburgh Sleep Quality Index (start time)"
    selected.short <- "PSQI"
    selected.var <- c(
      "Group",
      "Group.num",
      "PSQI_quality","PSQI_latency","PSQI_duration","PSQI_efficiency",
      "PSQI_disturbance","PSQI_medication","PSQI_dysfunction"
    )
  }
  for (i2 in 3:length(selected.var))
  {
    mt_tmp <- eiras::SimpleSummary(data[data$Group=="R",selected.var[i2]])
    mt_tmp <- cbind(selected.var[i2],"R",mt_tmp)
    if(!exists("mt_summary"))
    {
      mt_summary <- mt_tmp
    } else
    {
      mt_summary <- rbind(mt_summary,mt_tmp)
    }
    mt_tmp <- eiras::SimpleSummary(data[data$Group=="V",selected.var[i2]])
    mt_tmp <- cbind(selected.var[i2],"V",mt_tmp)
    mt_summary <- rbind(mt_summary,mt_tmp)
  }
  # controles
  selected.var2 <- paste(selected.var[3:length(selected.var)],collapse = " + ")
  control.var <- c(
    "Sex",
    "Age",
    "BMI",
    "Acronym",
    "SchoolType",
    "CityPopulation",
    "CityLocation",
    "State",
    "CourseYear",
    "GovernAid",
    "Companionship",
    "ActivityDuration",
    "METsEstimate",
    "SchoolFee",
    "SchoolAge",
    "Region"
  )
  lst <- list()
  for (i3 in 1:length(control.var))
  {
    mt_tmp <- eiras::SimpleSummary(data[data$Group=="R",control.var[i3]])
    r.aux <- rownames(mt_tmp)
    mt_tmp <- cbind(control.var[i3],"R",mt_tmp)
    if(r.aux=="Nominal" | r.aux=="Ordinal")
    {lst[[paste0(control.var[i3],"_R")]] <- mt_tmp}else
    {mt_summary <- rbind(mt_summary,mt_tmp)} 
    mt_tmp <- eiras::SimpleSummary(data[data$Group=="V",control.var[i3]])
    r.aux <- rownames(mt_tmp)
    mt_tmp <- cbind(control.var[i3],"V",mt_tmp)
    if(r.aux=="Nominal" | r.aux=="Ordinal")
    {lst[[paste0(control.var[i3],"_V")]] <- mt_tmp}else
    {mt_summary <- rbind(mt_summary,mt_tmp)} 
  }  
  # salva os que usarei para analise
  openxlsx::write.xlsx(data.frame(mt_summary),file.path(folderout,"SimpleSummary.xlsx"))
  # control subgroups
  # "Sex" "Age" "BMI"
  control.var.bio <- control.var[c(1,2,3)] 
  control.var.bio2 <- paste(control.var.bio,collapse = " + ")
  # Health ("Live_with" "PhysAct_weekhours" "PhysAtvKcal")
  # control.var.circ <- control.var[c(9,11,12,13)]
  control.var.circ <- control.var[c(9,12,13)] 
  control.var.circ2 <- paste(control.var.circ,collapse = " + ")
  # School_type (pub, prv), Course year(9), GovernAid (10)
  # control.var.sch <- control.var[c(5,14,15)] 
  control.var.sch <- control.var[c(5)] 
  control.var.sch2 <- paste(control.var.sch,collapse = " + ")
  # inhabitants, cap/country, UF (8)
  # control.var.city <- control.var[c(6,7,8,16)] 
  control.var.city <- control.var[c(6,7)] 
  control.var.city2 <- paste(control.var.city,collapse = " + ")
  
  # salva os que usarei para analise
  for (i2 in c(control.var.bio,
              control.var.circ,
              control.var.sch,
              control.var.city))
  {
    r.aux <- grep(i2,names(lst))
    if(length(r.aux)>0)
    {
      p1 <- data.frame(lst[r.aux[1]])
      colnames(p1) <- gsub(paste0(i2,"_R."),"",colnames(p1))
      p2 <- data.frame(lst[r.aux[2]])
      colnames(p2) <- gsub(paste0(i2,"_V."),"",colnames(p2))
      pp <- rbind(p1,p2)
      openxlsx::write.xlsx(pp,file.path(folderout,paste0(i2,".xlsx")))
    }
  }
  # preserve selected and transformed data
  data <- data[,c(selected.var,control.var)]
  # saveRDS(data,"Analysis01.rds")
  
  eiras::Add2Markdown(fileout,
                      selected.tit,
                      "title")
  
  eiras::Add2Markdown(fileout,
                      paste0(selected.short,": GLM without controls"),
                      "title",2)
  VIs <- selected.var2
  formula <- paste0("Group.num ~ ", VIs)
  model <- lm(formula, data=data)
  
  eiras::Add2Markdown(fileout,formula,"text")
  sink("tmp.txt")
  anv <- car::Anova(model)
  anv$`Pr(>F)`[1:(length(selected.var)-2)] <- anv$`Pr(>F)`[1:(length(selected.var)-2)]*(length(selected.var)-2)
  anv <- car::Anova(model)
  eta2 <- effectsize::eta_squared(anv, alternative="two.sided", partial=FALSE, ci=1-0.05/length(VIs))
  eta2$Interpretation <- effectsize::interpret_eta_squared(eta2$Eta2)
  print(eta2)
  sink()
  tmp <- file("tmp.txt","r")
  dt_tmp <- readLines(tmp)
  close(tmp)
  eiras::Add2Markdown(fileout,dt_tmp,"pre")
  # store in HD
  VI.aux <- rownames(anv)[1:(nrow(anv)-1)]
  namesstat <- c("Variable","F","df","p","sig")
  dt_statistic <- data.frame(matrix(nrow=length(VI.aux), 
                                    ncol=length(namesstat)))
  names(dt_statistic) <- namesstat
  dt_statistic$Variable <- VI.aux
  for (r.anv in 1:(nrow(anv)-1))
  {
    r.stat <- which(dt_statistic$Variable == rownames(anv)[r.anv])
    dt_statistic$F[r.stat] <- anv$`F value`[r.anv]
    dt_statistic$p[r.stat] <- anv$`Pr(>F)`[r.anv]
    dt_statistic$df[r.stat] <- anv$Df[r.anv]
  }
  dt_statistic$F <- format(round(dt_statistic$F,2), nsmall = 2)
  dt_statistic$p[1:(length(selected.var)-2)] <- dt_statistic$p[1:(length(selected.var)-2)]*(length(selected.var)-2)
  dt_statistic$txt <- ""
  dt_statistic$txt <- format(round(dt_statistic$p,4), nsmall = 4)
  dt_statistic$sig[dt_statistic$p<0.05 & !is.na(dt_statistic$p)] <- "*"
  dt_statistic$txt[dt_statistic$p>1] <- "> 0.9999"
  dt_statistic$txt[dt_statistic$p<1e-4] <- "< 0.0001"
  dt_statistic$p <- dt_statistic$txt
  dt_statistic$txt <- NULL
  # df denominator
  dt_statistic <- rbind(dt_statistic,dt_statistic[1,])
  dt_statistic[nrow(dt_statistic),] <- rep("",ncol(dt_statistic))
  dt_statistic$df[nrow(dt_statistic)] <- paste0("/ ",anv$Df[nrow(anv)])
  openxlsx::write.xlsx(dt_statistic,file.path(folderout,paste0("Result_",selected.short,".xlsx")))
  eiras::Add2Markdown(fileout,
                      paste0(selected.short,": GLM with control variables"),
                      "title",2)
  VIs <- paste0(selected.var2," + ",
                control.var.bio2," + ",
                control.var.circ2," + ",
                control.var.sch2," + ",
                control.var.city2)
  formula <- paste0("Group.num ~ ", VIs)
  model <- lm(formula, data=data)
  eiras::Add2Markdown(fileout,formula,"text")
  sink("tmp.txt")
  print(table(data$Group,data$CityLocation))
  anv <- car::Anova(model)
  anv$`Pr(>F)`[1:(length(selected.var)-2)] <- anv$`Pr(>F)`[1:(length(selected.var)-2)]*(length(selected.var)-2)
  anv$`Pr(>F)`[anv$`Pr(>F)`>1] <- 1
  print(anv)
  anv <- car::Anova(model)
  eta2 <- effectsize::eta_squared(anv, alternative="two.sided", partial=FALSE, ci=1-0.05/length(VIs))
  eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
  print(eta2)
  sink()
  tmp <- file("tmp.txt","r")
  dt_tmp <- readLines(tmp)
  close(tmp)
  eiras::Add2Markdown(fileout,dt_tmp,"pre")
  # store in HD
  VI.aux <- rownames(anv)[1:(nrow(anv)-1)]
  namesstat <- c("Variable","F","df","p","sig")
  dt_statistic <- data.frame(matrix(nrow=length(VI.aux), 
                                    ncol=length(namesstat)))
  names(dt_statistic) <- namesstat
  dt_statistic$Variable <- VI.aux
  for (r.anv in 1:(nrow(anv)-1))
  {
    r.stat <- which(dt_statistic$Variable == rownames(anv)[r.anv])
    dt_statistic$F[r.stat] <- anv$`F value`[r.anv]
    dt_statistic$p[r.stat] <- anv$`Pr(>F)`[r.anv]
    dt_statistic$df[r.stat] <- anv$Df[r.anv]
  }
  dt_statistic$F <- format(round(dt_statistic$F,2), nsmall = 2)
  dt_statistic$p[1:(length(selected.var)-2)] <- dt_statistic$p[1:(length(selected.var)-2)]*(length(selected.var)-2)
  dt_statistic$txt <- ""
  dt_statistic$txt <- format(round(dt_statistic$p,4), nsmall = 4)
  dt_statistic$sig[dt_statistic$p<0.05 & !is.na(dt_statistic$p)] <- "*"
  dt_statistic$txt[dt_statistic$p>1] <- "> 0.9999"
  dt_statistic$txt[dt_statistic$p<1e-4] <- "< 0.0001"
  dt_statistic$p <- dt_statistic$txt
  dt_statistic$txt <- NULL
  # df denominator
  dt_statistic <- rbind(dt_statistic,dt_statistic[1,])
  dt_statistic[nrow(dt_statistic),] <- rep("",ncol(dt_statistic))
  dt_statistic$df[nrow(dt_statistic)] <- paste0("/ ",anv$Df[nrow(anv)])
  openxlsx::write.xlsx(dt_statistic,file.path(folderout,paste0("ResultCTRL_",selected.short,".xlsx")))
}

data <- o.data

data$tmp <- ""
data$tmp[data$CityPopulation< 500000] <- "<500t"
data$tmp[data$CityPopulation>=500000 & data$CityPopulation<1000000] <- "500t,1mi"
data$tmp[data$CityPopulation>=1000000 & data$CityPopulation<5000000] <- "1mi,5mi"
data$tmp[data$CityPopulation>= 5000000] <- ">5mi"
data$tmp <- factor(data$tmp,levels=c("<500t","500t,1mi","1mi,5mi",">5mi"))
data$CityPopulation <- data$tmp
data$tmp <- NULL

eiras::Add2Markdown(fileout,"Checking control variables","title")
selected.var <- c(
  "Sex",
  "CourseYear",
  "SchoolType",
  "CityPopulation",
  "CityLocation"
)
VIs <- paste(selected.var,collapse = " + ")
formula <- paste0("Group.num ~ ", VIs)
model <- lm(formula, data=data)
eiras::Add2Markdown(fileout,formula,"text")
sink("tmp.txt")
anv <- car::Anova(model)
anv$`Pr(>F)`[1:length(selected.var)] <- anv$`Pr(>F)`[1:length(selected.var)]*length(selected.var)
anv$`Pr(>F)`[anv$`Pr(>F)`>1] <- 1
print(anv)
anv <- car::Anova(model)
eta2 <- effectsize::eta_squared(anv, alternative="two.sided", partial=FALSE, ci=1-0.05/length(VIs))
eta2$interpret <- effectsize::interpret_eta_squared(eta2$Eta2)
print(eta2)
sink()
tmp <- file("tmp.txt","r")
dt_tmp <- readLines(tmp)
close(tmp)
eiras::Add2Markdown(fileout,dt_tmp,"pre")

# store in HD
VI.aux <- rownames(anv)[1:(nrow(anv)-1)]
namesstat <- c("Variable","F","df","p","sig")
dt_statistic <- data.frame(matrix(nrow=length(VI.aux), 
                                  ncol=length(namesstat)))
names(dt_statistic) <- namesstat
dt_statistic$Variable <- VI.aux
for (r.anv in 1:(nrow(anv)-1))
{
  r.stat <- which(dt_statistic$Variable == rownames(anv)[r.anv])
  dt_statistic$F[r.stat] <- anv$`F value`[r.anv]
  dt_statistic$p[r.stat] <- anv$`Pr(>F)`[r.anv]
  dt_statistic$df[r.stat] <- anv$Df[r.anv]
}
dt_statistic$F <- format(round(dt_statistic$F,2), nsmall = 2)
dt_statistic$p[1:(length(selected.var)-2)] <- dt_statistic$p[1:(length(selected.var)-2)]*(length(selected.var)-2)
dt_statistic$txt <- ""
dt_statistic$txt <- format(round(dt_statistic$p,4), nsmall = 4)
dt_statistic$sig[dt_statistic$p<0.05 & !is.na(dt_statistic$p)] <- "*"
dt_statistic$txt[dt_statistic$p>1] <- "> 0.9999"
dt_statistic$txt[dt_statistic$p<1e-4] <- "< 0.0001"
dt_statistic$p <- dt_statistic$txt
dt_statistic$txt <- NULL
# df denominator
dt_statistic <- rbind(dt_statistic,dt_statistic[1,])
dt_statistic[nrow(dt_statistic),] <- rep("",ncol(dt_statistic))
dt_statistic$df[nrow(dt_statistic)] <- paste0("/ ",anv$Df[nrow(anv)])
openxlsx::write.xlsx(dt_statistic,file.path(folderout,paste0("Result_Posthoc.xlsx")))


for (i in selected.var)
{
  if(i == selected.var[1]){emm <- emmeans::emmeans(object=model, specs=pairwise~Sex, adjust="bonf")}
  if(i == selected.var[2]){emm <- emmeans::emmeans(object=model, specs=pairwise~CourseYear, adjust="bonf")}
  if(i == selected.var[3]){emm <- emmeans::emmeans(object=model, specs=pairwise~SchoolType, adjust="bonf")}
  if(i == selected.var[4]){emm <- emmeans::emmeans(object=model, specs=pairwise~CityPopulation, adjust="bonf")}
  if(i == selected.var[5]){emm <- emmeans::emmeans(object=model, specs=pairwise~CityLocation, adjust="bonf")}

  eiras::Add2Markdown(fileout,i,"title",2)
  # print(table(data$Group,data$CityLocation))
  
  sink("tmp.txt")
  print(emm)
  dtcld <- multcomp::cld(object=emm$emmeans,
                      adjust="bonf",
                      Letters=letters,
                      alpha=0.05)
  dtcld <- dtcld[order(dtcld[,1],decreasing=TRUE),]
  print(dtcld)
  sink()
  tmp <- file("tmp.txt","r")
  dt_tmp <- readLines(tmp)
  close(tmp)
  eiras::Add2Markdown(fileout,dt_tmp,"pre")
  
  filegraph <- paste("emm",i,".png",sep="")
  eiras::OpenFig(file.path(folderout,filegraph),width=500,height=400)
  print(plot(emm$emmeans, colors="black"))
  dev.off()
  eiras::Add2Markdown(fileout,filegraph,"png")
  
  filegraph <- paste("cnt",i,".png",sep="")
  eiras::OpenFig(file.path(folderout,filegraph),width=500,height=400)
  print(plot(emm$contrasts, colors="black"))
  dev.off()
  eiras::Add2Markdown(fileout,filegraph,"png")
}

# generate html
rmarkdown::render(fileout, output_format = out.format.exec)
txt <- eiras::BarTitle(paste("Report available in ",fileout,sep=""))
cat(txt)
