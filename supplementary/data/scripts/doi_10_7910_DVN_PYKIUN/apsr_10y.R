library(data.table)
library(ggplot2)


n_articles = 804

dat = read.csv("apsr_10y.csv")
setDT(dat)
dat[, "X" := NULL]
dat[, where := trimws(where)]

tmp = dat[, .(bib = any(where == "bibliography")), by = "article"][
          , table(bib)]

datplot = data.frame(
    y = c("Bibliography", "Text but not bibliography", "No reference"),
    x = c(tmp[["TRUE"]], tmp[["FALSE"]], 804 - sum(tmp)))
datplot$y = factor(datplot$y, rev(datplot$y))

p <- ggplot(datplot, aes(x, y)) +
    geom_bar(stat = "identity") +
    labs(x = "", y = "") +
    theme_classic()
ggsave(p, filename = "apsr_10y.png", width = 7, height = 1.7)
