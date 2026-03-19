################################################### Replication of Table (1) in the Main Text ###########################################

library(stargazer)
data <- read.csv("MainData.csv", stringsAsFactors = F)

############ 
#Table (1)
############

media <- lm(media_score ~ conspiracy+scandal, data = data)
article <- lm(article_accuracy ~ conspiracy+scandal, data = data)
poltrust <- lm(poltrust_score ~ conspiracy+scandal, data = data)

media1 <- lm(media_score ~ conspiracy+scandal + scandal*conspiracy, data = data)
article1 <- lm(article_accuracy ~ conspiracy+scandal + scandal*conspiracy, data = data)
poltrust1 <- lm(poltrust_score ~ conspiracy+scandal + scandal*conspiracy, data = data)


stargazer(media, article, poltrust, media1, article1, poltrust1, align=TRUE,  no.space=TRUE, 
          covariate.labels = c("Conspiracy", "Scandal", "Scandal x Conspiracy", "Control"), 
          keep.stat="n", dep.var.labels = c("\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Info. Providers}", 
                                            "\\multirow{2}{3 cm}{\\centering Article\\ Accuracy}",
                                            "\\multirow{2}{3 cm}{ \\centering Trust\\ Instit.}"), 
          dep.var.caption="Outcome", digits=2, column.sep.width = "-35pt", header=F)

