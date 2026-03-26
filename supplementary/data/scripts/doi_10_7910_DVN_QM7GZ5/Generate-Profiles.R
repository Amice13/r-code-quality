rm(list=ls())
library(data.table)
library(gridExtra)
library(grid)
library(gtable)
library(ggplot2)

# NOTE: Adjust "./" to your directory

profiles <- fread("./Data-Extract/Profiles.csv")
profiles[movie=="I Dont Like Movies Always Fall Asleep From Boredom", movie := "None"]
profiles[movie=="All Of The Twilight, Harry Potter, And Lord Of The Rings Movies", movie := "Twilight, Harry Potter, Lord Of The Rings"]
setkey(profiles, qid)
profiles[, id := 1:.N]
correspondence <- profiles[, .(id, mid, qid, score)]
fwrite(correspondence, file="./Data-Extract/ID-Correspondence.csv")
profiles <- profiles[, .(id, country, gender, age, subject, sport, color, movie, drink)]
names(profiles) <- c("id", "Country", "Gender", "Age", "Favorite High School Subject",
                     "Favorite Sport", "Favorite Color", "Favorite Movie", "Prefers Coffee/Tea")
profiles <- melt(profiles, id.vars="id")
profiles[, variable := as.character(variable)]
profiles[, variable := factor(variable, levels=c("Country", "Gender", "Age", "Favorite High School Subject",
                                                 "Favorite Sport", "Favorite Color", "Favorite Movie", "Prefers Coffee/Tea"))]
setorder(profiles, id, variable)

fg_fun <- function (label, parse = FALSE, col = "black", fontsize = 9, 
                    cex = 0.8, fontfamily = "", fontface = 1, lineheight = 1.2, 
                    alpha = 1, rot = 0, check.overlap = FALSE, name = NULL, vp = NULL, 
                    just = "left", hjust = 0, vjust = 0.5, x = 0.01, y = 0.5, 
                    default.units = "npc") 
{
  if (parse) {
    label <- tryCatch(parse(text = label), error = function(e) label)
  }
  textGrob(label = label, x = x, y = y, just = just, hjust = hjust, 
           vjust = vjust, rot = rot, check.overlap = check.overlap, 
           default.units = default.units, name = name, vp = vp, 
           gp = gpar(col = col, cex = cex, fontfamily = fontfamily, 
                     fontface = fontface, fontsize = fontsize, lineheight = lineheight, 
                     alpha = alpha))
}
set.seed(8)

for(j in 1:nrow(profiles)){
  if(j%%20==0){print(j)}
  png(paste0("./Data-Extract/Profile-Pictures/", j, ".png"), 660, 330, res=150, pointsize=9)
  use <- as.matrix(profiles[id==j, -c("id", "variable")])
  rownames(use) <- paste0(c("Country", "Gender", "Age", 
                            "Favorite High School Subject",
                            "Favorite Sport", "Favorite Color", 
                            "Favorite Movie", "Prefers Coffee/Tea"), ":")
  t2 <- ttheme_default(core=list(fg_fun=fg_fun, bg_params=list(col="grey60", fill=blues9[c(1)])), 
                       rowhead=list(fg_fun=fg_fun, fg_params=list(fontface=1L)))
  g <- tableGrob(use, cols=NULL, theme=t2)
  grid.newpage()
  grid.draw(g)
  dev.off()
}


