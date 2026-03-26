install.packages("sirt")
install.packages("readxl")
library(sirt)
library(readxl)


# load data(item response patterns, domain of items)
itemDat <- read.table("Dataset_1.csv", sep = ",", header = T)
itemDomainDat <- read_xlsx("Dataset_2_item_domain.xlsx")

# assign domain information of items
iteminfo <- itemDomainDat

# estimate Rasch model
mod1 <- sirt::rasch.mml2(itemDat[, -1])

# estimate theta using WLEs
wle1 <- sirt::wle.rasch(itemDat[, -1], b = mod1$item$b)$theta

# DETECT for content domains
detect1 <- sirt::conf.detect(data = itemDat[, -1], score = wle1, itemcluster = iteminfo$Category)
