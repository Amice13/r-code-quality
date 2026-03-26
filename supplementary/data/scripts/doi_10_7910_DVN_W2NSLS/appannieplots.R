library("here")

plotCustom <- function(..., text1 = 140, text2 = 60, theby = 400, bylim = 1200, no_axis=F) {
  plot(...,
       pch=16,
       ## ylim=c(1400,0),
       cex.lab=1.5,
       cex.main=1.3,
       cex.axis=1.3,
       bty="n",
       yaxt="n", cex=1.5
  )
  lines(c(as.Date("2020/01/24"), as.Date("2020/01/24")), c(1500,0), lty=2)
  text(as.Date("2020/01/24"), text1, "Wuhan \n Lockdown", cex = 1)
  if (!no_axis) {
    axis(2, at=seq(0, bylim, theby), labels=seq(0, bylim, theby), las=1, cex.axis=1.3, hadj=0.9)
  }
}


#App Annie Time Trends Scripts


#VPN Script, standardized rank
vpn <- read.csv(here("appannie/data/vpn.csv"))

pdf(here("appannie/figures/VPNAppAnnie.pdf"), height=4, width=6)
plotCustom(
  as.Date(vpn$date), vpn$rank,
  xlab="Date (2020)",
  ylab="Rank",
  ylim=c(120,0),
  yaxt="n",
  main="VPN Ranking",
  no_axis=TRUE
)
dev.off()

 #Twitter
twitter <- read.csv(here("appannie/data/twitter.csv"))

pdf(here("appannie/figures/TwitterAppAnnie.pdf"), height=4, width=6)
plotCustom(
  as.Date(twitter$date),
  twitter$rank,
  xlab="Date (2020)",
  ylab="Rank",
  ylim=c(700,0),
  main="Twitter, \n App Ranking",
  bylim = 600, theby = 200,
  text1 = 100,
  text2 = 40,
  col = rgb(29, 161, 242, maxColorValue=255)
)
dev.off()

#Facebook
facebook <- read.csv(here("appannie/data/facebook.csv"))

pdf(here("appannie/figures/FacebookAppAnnie.pdf"), height=4, width=6)
plotCustom(
  as.Date(facebook$date),
  facebook$rank,
  xlab="Date (2020)",
  ylab="Rank",
  ylim=c(700,0),
  main="Facebook, \n App Ranking",
  bylim = 600, theby = 200,
  col = rgb(63, 92, 154, maxColorValue=255)
)
dev.off()


#Wikipedia
wiki <- read.csv(here("appannie/data/wiki.csv"))

pdf(here("appannie/figures/WikipediaAppAnnie.pdf"), height=4, width=6)
plot(
  as.Date(wiki$date),
  round(wiki$rank),
  xlab="Date (2020)",
  ylab="Rank",
  ylim=c(120,0),
  main="Wikipedia, \n Reference App Ranking",
  pch=16,
  ## ylim=c(1400,0),
  cex.lab=1.5,
  cex.main=1.3,
  cex.axis=1.3,
  bty="n",
  yaxt="n", cex=1.5
)
lines(c(as.Date("2020/01/24"), as.Date("2020/01/24")), c(1500,30), lty=2)
text(as.Date("2020/01/24"), 15, "Wuhan \n Lockdown")
axis(2, at=seq(0, 100, 20), labels=seq(0, 100, 20), las=1, cex.axis=1.3, hadj=0.9)
dev.off()
