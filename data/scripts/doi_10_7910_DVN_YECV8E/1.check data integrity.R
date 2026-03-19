# from RawData.rds to RawData2.rds
# panoramic view and variable organization for English publication
# 
# Requirement:
# eirasdata package available at Harvard Dataverse
# https://doi.org/10.7910/DVN/DLQTPH
# eiras package version 1.0.2 available at Harvard Dataverse
# https://doi.org/10.7910/DVN/TBBAVU

# recover RDS
data <- readRDS("RawData.rds")
# add a dictionary
dt_dictionary <- data.frame(readxl::read_excel("Dictionary.xlsx"))

folderout <- "analysis"
filename <- "Organize"
filermd <- file.path(folderout,filename)
out.format.exec <- "html_document"

try(dir.create(folderout),silent = TRUE)
fileout <- eiras::CreateMarkdown(filermd,
                                 title="VERAS, data organizing",
                                 author="Paulo S. P. Silveira, Jose O. Siqueira")

eiras::Add2Markdown(fileout,"Analysis of Raw Data","title")

eiras::Add2Markdown(fileout,"Variable names","title",level=2)

txt <- paste0("Many variable names are in Portuguese, It is required to translate and add a data dictionary for them.")
eiras::Add2Markdown(fileout,txt,"text")

dnames <- names(data)
ordernames <- rep(0,length(dnames))
for (d in 1:length(dnames)) 
{
  rowdic <- which(dt_dictionary$Original==dnames[d])
  if(length(rowdic)>0)
  {
    dnames[d] <- dt_dictionary$New.name[rowdic]
    ordernames[d] <- rowdic
  }
}
names(data) <- dnames
dt_tmp <- data.frame(dnames,ordernames)
dt_tmp <- dt_tmp[order(dt_tmp$ordernames),]
dnames <- dt_tmp$dnames
data <- data[,dnames]

# adiciona WHOQOL global
data$WHOQOL <- 0
data$WHOQOL <- (data$WHOQOL_phys+data$WHOQOL_psy+data$WHOQOL_soc+data$WHOQOL_env)/4


sink("tmp.txt")
cat("\n")
for (d in 1:nrow(dt_dictionary))
{
  txt <- paste0("\n* <b>",dt_dictionary$New.name[d],"</b>: ",dt_dictionary$Description[d])
  cat(txt)
}
cat("\n")
sink()
tmp <- file("tmp.txt","r")
dt_tmp <- readLines(tmp)
close(tmp)
eiras::Add2Markdown(fileout,dt_tmp,"text")


##################################################################
# descritiva
fields <- skimr::skim(data)
eiras::Add2Markdown(fileout,knitr::knit_print(fields),"text")

eiras::Add2Markdown(fileout,"Character variables","title",level=2)
for (v.aux in 1:nrow(fields))
{
  if (fields$skim_type[v.aux]=="character" | fields$skim_type[v.aux]=="factor")
  {
    variable <- fields$skim_variable[v.aux]
    eiras::Add2Markdown(fileout,paste0("<br><big><b>",variable,"</b></big>"),"text")
    uniques <- fields$character.n_unique[v.aux]
    txt <- paste0(" has ", uniques," unique character sequences.<br>")
    eiras::Add2Markdown(fileout,txt,"text")
    if(uniques <= 100)
    {
      txt <- paste0(
            "```{r echo=FALSE}\n
      tbl <- gtsummary::tbl_summary(data[\"",variable,"\"])\n
      knitr::knit_print(tbl)\n
      ```"    )
    } else
    {
      txt <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                    "(too many; no table available).<br>")
    }
    eiras::Add2Markdown(fileout,txt,"text")
  }
}

eiras::Add2Markdown(fileout,"Descriptive Statistics","title")

eiras::Add2Markdown(fileout,"Factors","title",2)

txt <- paste0("Most of the character variables must be converted to factor and translated to English:")
eiras::Add2Markdown(fileout,txt,"text")

# factors and replacements
variables <- c("Group",
               "Sex",
               "CoursePhase",
               "GovernAid",
               "Companionship",
               "ActivityDuration",
               "METsCategory",
               "PhysAct_interruption",
               "Sleep_lessthanideal"
)

data$Group <- factor(data$Group)
data$Sex <- factor(data$Sex, 
                   levels=c("Feminino","Masculino"),
                   labels=c("Female","Male")
)
data$CoursePhase <- factor(data$CoursePhase, 
                   levels=c("1o e 2o ano","3o e 4o ano","5o e 6o ano"),
                   labels=c("basic (years 1 and 2)",
                            "clinic (years 3 and 4)",
                            "internship (years 5 and 6)"),
                   ordered = TRUE
)
data$GovernAid <- factor(data$GovernAid, 
                         levels=c("No","Yes"),
                         labels=c("No","Yes")
)
data$Companionship <- factor(data$Companionship, 
                         levels=c("alone","other students","relatives","partner"),
                         labels=c("alone","other students","relatives","partner")
)
data$ActivityDuration[data$ActivityDuration=="0"] <- NA
data$ActivityDuration <- factor(data$ActivityDuration, 
                         levels=c(
                           "Nao pratico esporte",                           
                           "Menos de 1 hora por semana",
                           "De 1 a 2 horas por semana",
                           "De 2 a 3 horas por semana",
                           "De 3 a 4 horas por semana",
                           "Mais de 4 horas por semana"
                         ),
                         labels=c(
                           "no sports",                           
                           "less than 1 hour per week",
                           "1 to 2 hours per week",
                           "2 to 3 hours per week",
                           "3 to 4 hours per week",
                           "more than 4 hours per week"
                         ),
                         ordered = TRUE
)
data$PhysAct_howlong[data$PhysAct_howlong=="0"] <- NA
data$PhysAct_howlong <- factor(data$PhysAct_howlong, 
                                 levels=c(
                                   "Nao pratico",                           
                                   "3 meses",
                                   "6 meses",
                                   "1 ano",
                                   "> 1 ano"
                                 ),
                                 labels=c(
                                   "no sports",                           
                                   "3 months",
                                   "6 months",
                                   "1 year",
                                   "more than 1 year"
                                 ),
                                 ordered = TRUE
)
data$PhysAct_interruption[data$PhysAct_interruption=="0"] <- NA
data$PhysAct_interruption <- factor(data$PhysAct_interruption, 
                               levels=c(
                                 "Nunca pratiquei atividade fisica regular",                           
                                 "3 meses",
                                 "6 meses",
                                 "1 ano",
                                 "> 1 ano",
                                 "Nao se aplica (estou praticando esportes atualmente)"
                               ),
                               labels=c(
                                 "never engaged",                           
                                 "3 months",
                                 "6 months",
                                 "1 year",
                                 "more than 1 year",
                                 "currently engaged"
                               ),
                               ordered = TRUE
)
data$METsCategory <- factor(data$METsCategory,
                                    levels=c(
                                      "NoPA",
                                      "LowPA",
                                      "ModPA",
                                      "HighPA"
                                    ),
                                    labels=c(
                                      "NoPA",
                                      "LowPA",
                                      "ModPA",
                                      "HighPA"
                                    ),
                                    ordered = TRUE
)
data$Sleep_lessthanideal <- factor(data$Sleep_lessthanideal, 
                        levels=c("Nao","Sim"),
                        labels=c("No","Yes")
)

data.factors <- data[,variables]
fields <- skimr::skim(data.factors)
eiras::Add2Markdown(fileout,knitr::knit_print(fields),"text")

c.grp <- which(names(data)=="Group")
for (v in variables)
{
  sink("tmp.txt")  
  cat(eiras::BarTitle(v))
  c.aux <- which(names(data)==v)
  print(eiras::SimpleSummary(data[,c.aux]))
  sink()
  tmp <- file("tmp.txt","r")
  dt_tmp <- readLines(tmp)
  close(tmp)
  eiras::Add2Markdown(fileout,dt_tmp,"pre")
  
  if (v != "Group")
  {
    filegraph <- paste("mosaic_",v,".png",sep="")
    eiras::OpenFig(file.path(folderout,filegraph))
    data.mosaic <- data[,c(c.grp,c.aux)]
    names(data.mosaic) <- c("Group","variable")
    fnc_replace <- function(data,old,new)
    {
      data$variable <- as.character(data$variable)
      for (i in 1:length(old))
      {
        data$variable[data$variable==old[i]] <- new[i]
      }
      data$variable <- factor(data$variable,ordered=TRUE,
                              levels=new)
      return(data)
    }
    if(v=="Course_phase")
    {
      old <- c("basic (years 1 and 2)","clinic (years 3 and 4)","internship (years 5 and 6)")
      new <- c("basic","clinic","internship")
      data.mosaic <- fnc_replace(data.mosaic,old,new)
    }
    if(v=="ActivityDuration")
    {
      old <- c("no sports","less than 1 hour per week","1 to 2 hours per week","2 to 3 hours per week","3 to 4 hours per week","more than 4 hours per week")
      new <- c("no","<1","1-2","2-3","3-4",">4")
      data.mosaic <- fnc_replace(data.mosaic,old,new)
    }
    if(v=="PhysAct_howlong")
    {
      old <- c("no sports","3 months","6 months","1 year","more than 1 year")
      new <- c("no","3m","6m","1y",">1y")
      data.mosaic <- fnc_replace(data.mosaic,old,new)
    }
    if(v=="PhysAtvKcalCat")
    {
      old <- c("NoPA","LowPA","ModPA","HighPA")
      new <- c("-","Low","Mod","High")
      data.mosaic <- fnc_replace(data.mosaic,old,new)
    }
    
    if(v=="PhysAct_interruption")
    {
      old <- c("never engaged","3 months","6 months","1 year","more than 1 year","currently engaged")
      new <- c("-","3m","6m","1y",">1y","practicing")
      data.mosaic <- fnc_replace(data.mosaic,old,new)
    }
    grf <- mosaicplot(~Group+variable, data=data.mosaic,
                      cex.axis=2,
                      color=TRUE, shade=TRUE)
    dev.off()
    eiras::Add2Markdown(fileout,filegraph,"png")
  }
  
}



eiras::Add2Markdown(fileout,"Missing data analysis","title",2)

txt <- '
```{r echo=FALSE}
n.total <- nrow(data)
n.completo <- nrow(na.omit(data))
n.incompleto <- n.total - n.completo
cat("Numero de casos total = ", n.total, "\n", sep="")
cat("Numero de casos completos = ", n.completo, 
    " (",round(100*n.completo/n.total,2),"%)\n", sep="")
cat("Numero de casos incompletos = ", n.incompleto, 
    " (",round(100*n.incompleto/n.total,2),"%)\n", sep="")
obs.falt <- sum(is.na(data))
obs.valid <- sum(!is.na(data))
obs.tot <- obs.falt + obs.valid
cat("Numero de observacoes validas = ", obs.valid, 
    " (",round(100*obs.valid/obs.tot,2),"%)\n", sep="")
cat("Numero de observacoes faltantes = ", obs.falt, 
    " (",round(100*obs.falt/obs.tot,2),"%)\n", sep="")
```
'
eiras::Add2Markdown(fileout,txt,"text")
txt <- '
```{r echo=FALSE, fig.width=9, fig.height=20}
grf <- eiras::Missing(subset(data,select=-ID),x.for.labels=110,col.width=50,plot=TRUE,cex=0.8)
```
'
eiras::Add2Markdown(fileout,txt,"text")

# saving results
saveRDS(data, file="RawData2.rds")

# generate html
rmarkdown::render(fileout, output_format = out.format.exec)
txt <- eiras::BarTitle(paste("Report available in ",fileout,sep=""))
cat(txt)
