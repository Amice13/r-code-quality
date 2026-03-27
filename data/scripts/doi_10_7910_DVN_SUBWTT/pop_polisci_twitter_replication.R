rm(list = ls())
require(stargazer)
require(lme4)
require(lfe)
require(tidytext)
require(igraph)
require(stringr)
require(tidyverse)
require(data.table)
require(chorddiag)
require(circlize)
require(tinter)
# ################################################################### Loading Data
load("./pop_polisci_twitter_replication.RData")

# Custom function
jb_alluv <- function(dat,freq, col = "gray", border = 0, layer, hide = FALSE, 
                     border.lty = 2,
                     alpha = 0.5, gap.width = 0.05, xw = 0.1, cw = 0.1, blocks = TRUE, 
                     ordering = NULL, axis_labels = NULL, cex = par("cex"),cex.labels = 1,
                     cex.axis = par("cex.axis"),show.vals = TRUE,shouts = NULL,shins = NULL,
                     block.col = NULL,block.border = NA,block.lwd = 1,block.lty = 1) 
{
  p <- data.frame(dat, freq = freq, col, alpha, border, hide, border.lty,
                  stringsAsFactors = FALSE)
  np <- ncol(p) - 6
  if (!is.null(ordering)) {
    stopifnot(is.list(ordering))
    if (length(ordering) != np) 
      stop("'ordering' argument should have ", np, 
           " components, has ", length(ordering))
  }
  n <- nrow(p)
  if (missing(layer)) {
    layer <- 1:n
  }
  p$layer <- layer
  d <- p[, 1:np, drop = FALSE]
  p <- p[, -c(1:np), drop = FALSE]
  p$freq <- with(p, freq/sum(freq))
  col <- col2rgb(p$col, alpha = TRUE)
  if (!identical(alpha, FALSE)) {
    col["alpha", ] <- p$alpha * 256
  }
  p$col <- apply(col, 2, function(x) do.call(rgb, c(as.list(x), 
                                                    maxColorValue = 256)))
  isch <- sapply(d, is.character)
  d[isch] <- lapply(d[isch], as.factor)
  if (length(blocks) == 1) {
    blocks <- if (!is.na(as.logical(blocks))) {
      rep(blocks, np)
    }
    else if (blocks == "bookends") {
      c(TRUE, rep(FALSE, np - 2), TRUE)
    }
  }
  if (is.null(axis_labels)) {
    axis_labels <- names(d)
  } else {
    if (length(axis_labels) != ncol(d)) 
      stop("`axis_labels` should have length ", names(d), 
           ", has ", length(axis_labels))
  }
  getp <- function(i, d, f, w = gap.width) {
    a <- c(i, (1:ncol(d))[-i])
    if (is.null(ordering[[i]])) {
      o <- do.call(order, d[a])
    }
    else {
      d2 <- d
      d2[1] <- ordering[[i]]
      o <- ordering[[i]] #do.call(order, d2[a])
    }
    x <- c(0, cumsum(f[o])) * (1 - w)
    x <- cbind(x[-length(x)], x[-1])
    gap <- cumsum(c(0L, diff(as.numeric(d[o, i])) != 0))
    mx <- max(gap)
    if (mx == 0) 
      mx <- 1
    gap <- gap/mx * w
    (x + gap)[order(o), ]
  }
  
  dd <- lapply(seq_along(d), getp, d = d, f = p$freq)
  rval <- list(endpoints = dd)
  op <- par(mar = c(2, 1, 1, 1))
  plot(NULL, type = "n", xlim = c(1 - cw, np + cw), ylim = c(0, 
                                                             1), xaxt = "n", yaxt = "n", xaxs = "i", 
       yaxs = "i", xlab = "", ylab = "", frame = FALSE)
  # axis(4,at = dd[[2]],labels = round(dd[[2]],3),las = 2,line = -8,cex.axis = .8)
  ind <- which(!p$hide)[rev(order(p[!p$hide, ]$layer))]
  for (i in ind) {
    for (j in 1:(np - 1)) {
      xspline(c(j, j, j + xw, j + 1 - xw, j + 1, j + 1, j + 1 - xw, j + xw, j) + rep(c(cw, -cw, cw), c(3, 4, 2)), 
              c(dd[[j]][i, c(1, 2, 2)], rev(dd[[j + 1]][i, c(1, 1, 2, 2)]), dd[[j]][i, c(1, 1)]), 
              shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0), open = FALSE, 
              col = p$col[i], border = p$border[i],lty = p$border.lty[i])
      if(diff(dd[[j]][i,])/2 > .01 & show.vals) {
        y.left <- dd[[j]][i,1] + diff(dd[[j]][i,])/2
        text(j+xw,y.left,paste0(round(shouts[i]*100,0),"%"),pos = 4,cex = cex.labels)
      }
      if(diff(dd[[j+1]][i,])/2 > .01 & show.vals) {
        y.right <- dd[[j+1]][i,1] + diff(dd[[j+1]][i,])/2
        text(j+1-xw,y.right,paste0(round(shins[i]*100,0),"%"),pos = 2,cex = cex.labels)
      }
    }
  }
  
  for (j in seq_along(dd)) {
    ax <- lapply(split(dd[[j]], d[, j]), range)
    if (blocks[j]) {
      counter <- 1
      for (k in seq_along(ax)) {
        rect(j - cw, ax[[k]][1], j + cw, ax[[k]][2],col = block.col[counter],border = block.border[counter],lwd = block.lwd[counter],lty = block.lty[counter])
        counter <- ifelse(counter < max(length(block.col),length(block.border)),counter + 1,1)
      }
    }
    else {
      for (i in ind) {
        x <- j + c(-1, 1) * cw
        y <- t(dd[[j]][c(i, i), ])
        w <- xw * (x[2] - x[1])
        xspline(x = c(x[1], x[1], x[1] + w, x[2] - w, 
                      x[2], x[2], x[2] - w, x[1] + w, x[1]), y = c(y[c(1, 
                                                                       2, 2), 1], y[c(2, 2, 1, 1), 2], y[c(1, 1), 
                                                                                                         1]), shape = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 0), 
                open = FALSE, col = p$col[i], border = p$border[i],lty = p$border.lty[i])
      }
    }
    for (k in seq_along(ax)) {
      text(j, mean(ax[[k]]), labels = names(ax)[k], cex = cex)
    }
  }
  axis(1, at = rep(c(-cw, cw), ncol(d)) + rep(seq_along(d), 
                                              each = 2), line = 0.5, col = "white", col.ticks = "black", 
       labels = FALSE)
  axis(1, at = seq_along(d), tick = FALSE, labels = axis_labels, 
       cex.axis = cex.axis)
  par(op)
}






# ################################################################### Command Line Arguments
# ideo is either "pablo" or "smapp" for which ideology score to use. ak is a logical for whether to subset to users who use the also know hashtags
args <- c("pablo",FALSE)
# args <- commandArgs(trailingOnly = T)
ideo <- as.character(args[1])
ak <- as.logical(args[2])


# Simplifying the replication by choice of ideology score
if(ideo == "_smapp") {
  tweeters$ideology.raw <- tweeters$smapp.ideo # For the histograms
  tweeters$ideosq <- tweeters$ideology.raw^2 # For the initial regressions
  tweeters$ideobin <- tweeters$ideobin.smapp # For the ERGMs
  dyad.dat$ideocol <- dyad.dat$ideocol.smapp
  dyad.dat$ideorow <- dyad.dat$ideorow.smapp
  dyad.dat$libcoldum <- dyad.dat$libcoldum.smapp
  dyad.dat$librowdum <- dyad.dat$librowdum.smapp
  dyad.dat$conscoldum <- dyad.dat$conscoldum.smapp
  dyad.dat$consrowdum <- dyad.dat$consrowdum.smapp
  dyad.dat$ideo.dist <- dyad.dat$ideo.dist.smapp
}


if(ak) {
  tweeters <- tweeters %>% filter(wak > 0)
}





############################################## Figure 1
allmentions <- NULL
for(i in names(out$raw)) {
  if(length(out$raw[[i]]) == 1) { next }
  vect <- unlist(str_split(gsub("NA","",out$raw[[i]]$mention_screen_name),"\\|"))
  vect <- vect[which(vect != "")]
  allmentions <- c(allmentions,vect)
}

toplot <- table(allmentions)
top50 <- toplot[order(-toplot)] %>% head(50)
pdf("./Figures/figure1.pdf",width = 8,height = 8)
par(mar = c(4,10,4,1))
barplot(rev(top50),horiz = TRUE,las = 2,cex.names = .6,cex.axis = .8,
        border = "white",col = ifelse(tolower(rev(names(top50))) %in% tolower(names(out$raw)),
                                      "forestgreen",rgb(0,0,0,.2)),main = "Top 50 Most Mentioned Entities Overall")
legend('bottomright',legend = c("#polisci","Others"),fill = c("forestgreen","gray70"),border = "white")
dev.off()







############################################# Figure 2
pdf(paste0("./Figures/figure2.pdf"))
phd.univ %>% filter(Gender != "L") %>% 
  mutate(tenured = ifelse(tenured == "tenure-track","TT",
                          ifelse(tenured == "dir-dean-chair-prov","Leadership",
                                 ifelse(tenured == "tenured","Tenured","Adjunct/Lecturer")))) %>%
  group_by(Gender,tenured) %>% 
  summarise(tw = mean(has_tw,na.rm=T),
            sd = sd(has_tw,na.rm = T),
            n = n()) %>%
  mutate(se = sd/sqrt(n)) %>%
  ggplot(aes(x = reorder(tenured,-tw),y = tw,fill = Gender)) + 
  geom_bar(position = position_dodge(),stat = "identity") +
  geom_errorbar(aes(ymin = tw-2*se,ymax = tw+2*se),
                width = .2,position = position_dodge(.9)) + 
  scale_fill_manual("Gender",values = c("F" = "skyblue","M" = "forestgreen")) +
  theme(axis.text.x=NULL,
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  labs(y = "% on Twitter",title = "Academics on Twitter",subtitle = "By Gender and Position",x = "Position")
dev.off()










###################################################### Figure 3
hashes <- NULL
for(i in names(out$raw)) {
  if(length(out$raw[[i]]) == 1) { next }
  hashes <- bind_rows(hashes,data_frame(hashes = length(which(grepl("#",out$raw[[i]]$text[which(out$raw[[i]]$is_rt == "Original Content")]))),
                                        screen_name = tolower(i)))
}
tweeters <- tweeters %>% left_join(hashes)
tweeters$hashdum <- (tweeters$hashes > 0)+0
tweeters$logwak <- log(tweeters$wak+1)
tweeters$wakdum <- (tweeters$wak>0)+0

tweeters$verif[which(is.na(tweeters$verif))] <- FALSE
tweeters$verif <- tweeters$verif+0
res.behav <- list()
for(y in c("logged_tweets","logged_followers","logged_friends","hashes","logwak","wak","wakdum","hashdum",
           "verif","avg.rt.byothers","avg.fav.byothers","pct.oc.no_rt","pct.oc.no_fav")) {
  tweeters$y <- tweeters[[y]]
  res.behav[[y]] <- lmer(scale(y) ~ 
                           Gender + factor(tenurebin) + 
                           scale(ideosq) +
                           scale(Rank) + 
                           scale(Tuition.and.fees) + 
                           scale(GRFTF17) + scale(UGTENR17) + 
                           (1|school),tweeters)
}

y <- "pct.oc.no_fav"
toplot <- summary(res.behav[[y]])$coefficients
toplot <- toplot[-1,]
toplot <- toplot[nrow(toplot):1,]
labs <- c("Male","TT","Ideo (sq)","School Rank","Tuition","Grad Enroll","UG Enroll")
pdf(paste0("./Figures/figure3.pdf"),width = 8,height = 6)
nf <- layout(matrix(1:5, 1, 5, byrow = TRUE), 
             widths=c(1,2,2,2,1))
par(mar = c(4,1,4,1))
plot(0,0,type = 'n',bty = 'n',axes = F,xlab = "",ylab = "",ylim = c(.5,7.5))
axis(2,at = 1:nrow(toplot),labels = rev(labs),las = 2,line = -6)

plot(0,0,yaxt='n',ylim = c(.5,7.5),type = 'n',
     xlab = "Estimate",ylab = "",xlim = c(-.6,.6),
     main = "Profile Attributes")
abline(v = 0,lty = 3)
nudge <- .15
bgs <- c("black","white","gray70")
pchs <- c(21,22,23)
counter <- 1
for(y in c("verif","logged_followers","logged_friends")) {
  toplot <- summary(res.behav[[y]])$coefficients
  toplot <- toplot[-1,]
  toplot <- toplot[nrow(toplot):1,]
  
  segments(toplot[,1] - 2*toplot[,2],
           (1:nrow(toplot))+nudge,toplot[,1] + 2*toplot[,2])
  points(toplot[,1],(1:nrow(toplot))+nudge,pch = pchs[counter],bg = bgs[counter])
  nudge <- nudge - .15
  counter <- counter + 1
}
legend("topleft",legend = c("Verified","Followers","Friends"),pch = 21:23,
       pt.bg = c("black","white","gray70"),cex = .8)

plot(0,0,yaxt='n',ylim = c(.5,7.5),type = 'n',
     xlab = "Estimate",ylab = "",xlim = c(-.6,.6),
     main = "Engagement by Others")
abline(v = 0,lty = 3)
nudge <- .15
counter <- 1
for(y in c("avg.rt.byothers","avg.fav.byothers","pct.oc.no_fav")) {
  toplot <- summary(res.behav[[y]])$coefficients
  toplot <- toplot[-1,]
  toplot <- toplot[nrow(toplot):1,]
  
  segments(toplot[,1] - 2*toplot[,2],
           (1:nrow(toplot))+nudge,toplot[,1] + 2*toplot[,2])
  points(toplot[,1],(1:nrow(toplot))+nudge,pch = pchs[counter],bg = bgs[counter])
  nudge <- nudge - .15
  counter <- counter + 1
}
legend("topleft",legend = c("RTs","Favs","% No Fav"),pch = 21:23,
       pt.bg = bgs,cex = .8)


plot(0,0,yaxt='n',ylim = c(.5,7.5),type = 'n',
     xlab = "Estimate",ylab = "",xlim = c(-.6,.6),
     main = "Behavior")
abline(v = 0,lty = 3)
nudge <- .15
counter <- 1
for(y in c("logged_tweets","hashdum","wakdum")) {
  toplot <- summary(res.behav[[y]])$coefficients
  toplot <- toplot[-1,]
  toplot <- toplot[nrow(toplot):1,]
  
  segments(toplot[,1] - 2*toplot[,2],
           (1:nrow(toplot))+nudge,toplot[,1] + 2*toplot[,2])
  points(toplot[,1],(1:nrow(toplot))+nudge,pch = pchs[counter],bg = bgs[counter])
  nudge <- nudge - .15
  counter <- counter + 1
}
legend("topleft",legend = c("Tweets","Any #","#alsoknow"),pch = 21:23,
       pt.bg = bgs,cex = .8)
plot(0,0,type = 'n',bty = 'n',axes = F,xlab = "",ylab = "",ylim = c(.5,7.5))
segments(-1,.75,-.5,.75)
segments(-1,4.25,-.5,4.25)
segments(-.5,.75,-.5,4.25)
text(-.1,2.5,"School Covariates",srt = -90)
segments(-1,4.75,-.5,4.75)
segments(-1,7.25,-.5,7.25)
segments(-.5,4.75,-.5,7.25)
text(-.1,6,"Individual Covariates",srt = -90)
dev.off()






###################################################### Figure 4
names(graph.list$all.mat) <- "Mention"
pdf(paste0("./Figures/figure4.pdf"),width = 8,height = 6,pointsize = 9)
set.seed(1021)
cluster_bar(graph = graph.list$all.mat,clust.method = "cluster_label_prop",steps = 5,
            remove = c("political","politics","author","views","scientist","fan",
                       "science","prof","https","tco","assistant","professor","johns","jhu",
                       "sais","hopkins","food","director","syracuse","ut","tulane","umass",
                       "temple","stanford","diego","san","houston","0qjs3aq",
                       "university","associate","assoc","african","tweets",
                       "carolina","brook","supoliscu","dad","maxwell","miamiuniversity",
                       "alum","uchicago","york","austin","princeton","harvard",
                       "california","wustlpolisci","destruct","miami",
                       "studying","deep",
                       "coloradostateu","bdsspsu",
                       "phd","book","fellow","editor","polisci","asst","study"),
            var = "Gender",label.ref = "Female",group.size = 20,varlab = "Gender",
            weights = E(graph.list$all.mat)$weight) 
dev.off()









###################################################### Figure 5
res.cent <- list()
colnames(tweeters %>% select(starts_with("cent_")))
for(y in colnames(tweeters %>% select(starts_with("cent_")))) {
  if(grepl("follna",y)) { next }
  tweeters$y <- tweeters[[y]]
  res.cent[[y]] <- lmer(scale(y) ~ 
                          Gender + factor(tenurebin) + 
                          scale(ideosq) +
                          scale(Rank) + 
                          scale(Tuition.and.fees) + 
                          scale(GRFTF17) + scale(UGTENR17) + 
                          (1|school),tweeters %>% filter(tenured != "other"))
}

toplot <- summary(res.cent[[y]])$coefficients
toplot <- toplot[-1,]
toplot <- toplot[nrow(toplot):1,]
labs <- c("Male","TT","Ideo (sq)","School Rank","Tuition","Grad Enroll","UG Enroll")


pdf(paste0("./Figures/figure5.pdf"),width = 8,height = 6)
nf <- layout(matrix(1:5, 1, 5, byrow = TRUE), 
             widths=c(1,2,2,2,1))
par(mar = c(4,1,4,1))
plot(0,0,type = 'n',bty = 'n',axes = F,xlab = "",ylab = "",ylim = c(.5,7.5))
axis(2,at = 1:nrow(toplot),labels = rev(labs),las = 2,line = -6)

plot(0,0,yaxt='n',ylim = c(.5,7.5),type = 'n',
     xlab = "Estimate",ylab = "",xlim = c(-.6,.6),
     main = "All Ties")
abline(v = 0,lty = 3)
nudge <- .15
bgs <- c("black","white","gray40","gray80")
pchs <- c(21:24)
counter <- 1
for(y in c("cent_degree_in_all","cent_degree_out_all","cent_bt_all","cent_eigvect_all")) {
  toplot <- summary(res.cent[[y]])$coefficients
  toplot <- toplot[-1,]
  toplot <- toplot[nrow(toplot):1,]
  
  segments(toplot[,1] - 2*toplot[,2],
           (1:nrow(toplot))+nudge,toplot[,1] + 2*toplot[,2])
  points(toplot[,1],(1:nrow(toplot))+nudge,pch = pchs[counter],bg = bgs[counter])
  nudge <- nudge - .1
  counter <- counter + 1
}
legend("topleft",legend = c("In-Deg","Out-Deg","Between","Eigen"),pch = 21:24,
       pt.bg = c("black","white","gray40","gray80"),cex = .8)

plot(0,0,yaxt='n',ylim = c(.5,7.5),type = 'n',
     xlab = "Estimate",ylab = "",xlim = c(-.6,.6),
     main = "Retweet Ties")
abline(v = 0,lty = 3)
nudge <- .15
counter <- 1
for(y in c("cent_degree_in_rt","cent_degree_out_rt","cent_bt_rt","cent_eigvect_rt")) {
  toplot <- summary(res.cent[[y]])$coefficients
  toplot <- toplot[-1,]
  toplot <- toplot[nrow(toplot):1,]
  
  segments(toplot[,1] - 2*toplot[,2],
           (1:nrow(toplot))+nudge,toplot[,1] + 2*toplot[,2])
  points(toplot[,1],(1:nrow(toplot))+nudge,pch = pchs[counter],bg = bgs[counter])
  nudge <- nudge - .1
  counter <- counter + 1
}

plot(0,0,yaxt='n',ylim = c(.5,7.5),type = 'n',
     xlab = "Estimate",ylab = "",xlim = c(-.6,.6),
     main = "Follower Ties")
abline(v = 0,lty = 3)
nudge <- .15
counter <- 1
for(y in c("cent_degree_in_foll","cent_degree_out_foll","cent_bt_foll","cent_eigvect_foll")) {
  toplot <- summary(res.cent[[y]])$coefficients
  toplot <- toplot[-1,]
  toplot <- toplot[nrow(toplot):1,]
  
  segments(toplot[,1] - 2*toplot[,2],
           (1:nrow(toplot))+nudge,toplot[,1] + 2*toplot[,2])
  points(toplot[,1],(1:nrow(toplot))+nudge,pch = pchs[counter],bg = bgs[counter])
  nudge <- nudge - .1
  counter <- counter + 1
}

plot(0,0,type = 'n',bty = 'n',axes = F,xlab = "",ylab = "",ylim = c(.5,7.5))
segments(-1,.75,-.5,.75)
segments(-1,4.25,-.5,4.25)
segments(-.5,.75,-.5,4.25)
text(-.1,2.5,"School Covariates",srt = -90)
segments(-1,4.75,-.5,4.75)
segments(-1,7.25,-.5,7.25)
segments(-.5,4.75,-.5,7.25)
text(-.1,6,"Individual Covariates",srt = -90)
dev.off()










################################################################ Figure 6
col.ideos2 <- as.character(tweeters$ideobin[match(colnames(adj.mat$all.mat),tweeters$screen_name)])
for(links in names(adj.mat)[3]) {
  mains <- ifelse(grepl("url",links),"URL shares",
                  ifelse(grepl("res",links),"research shares",
                         ifelse(grepl("all",links),"mentions",
                                ifelse(grepl("rt",links),"retweets",
                                       ifelse(grepl("foll",links),"follows","")))))
  pdf(paste0("./Figures/figure6.pdf"),width = 7,height = 7,pointsize = 12)
  barplot.wrapper(vect = col.ideos2,mat = adj.mat[[links]],
                  mains = paste0("% ",mains," of ",c("libs","mods","conservs")," by..."),
                  labs = c("...liberals","...moderates","...conservatives"),
                  cols = rep("#c3c3c3",9))
  dev.off()
}







################################################################ Figure 7
prep <- chord.prep(covariates = c("tenurebin","Gender"))
cols <- c("#60A145","#82B788",
          "#2A7E64","#4C94A7",
          "#B1C05C","#D2D69F",
          "#7B9E7B","#9CB4BE",
          "#A78426","#C99A69",
          "#716245","#937888")
cols <- rep(c("skyblue","forestgreen"),2)

chordplot <- covariate_chord(mat = prep[[1]],plot = T,lims = 1,
                             cexes = 1,cexlabs = .8,
                             track.labs = list(c("Women","Men"),
                                               c("Tenure Track","Tenured")),
                             track.cols = list(c("forestgreen","skyblue"),
                                               c("gold","gray60")),
                             pval.test = T,
                             cols = cols)


alluv <- expand.grid(rownames(chordplot),colnames(chordplot))
alluv$freq <- as.vector(chordplot)

alluv <- alluv %>% group_by(Var2) %>% mutate(totin = sum(freq)) %>%
  mutate(shin = freq / totin) %>% 
  ungroup() %>% group_by(Var1) %>% mutate(totout = sum(freq)) %>%
  mutate(shout = freq / totout) %>% 
  arrange(Var1,freq)

shouts = alluv$shout
shins = alluv$shin



pdf(paste0("./Figures/figure7.pdf"),width = 8,height = 6)
jb_alluv(dat = alluv[,1:2],freq = alluv$freq,col = c(rep("#58508d",4),
                                                     rep("#bc5090",4),
                                                     rep("#ff6361",4),
                                                     rep("#ffa600",4)),
         alpha = .2,
         ordering = list(order(alluv$Var1, alluv$freq),
                         order(alluv$Var2, alluv$freq)),
         border = c(rep("grey60",16)),border.lty = c(1,1,1,1,
                                                     2,2,2,2,
                                                     3,3,3,3,
                                                     4,4,4,4),
         gap.width = .2,show.vals = T,shins = alluv$shin,shouts = alluv$shout,
         block.col = bayou::makeTransparent(c("#58508d","#bc5090","#ff6361","#ffa600"),40),
         axis_labels = c("Mentioning","Mentioned"),
         block.border = c("grey60","grey60","grey60","grey60"),block.lwd =c(1,1,
                                                                            2,2,
                                                                            3,3,
                                                                            4,4),
         block.lty = c(1,2,3,4),
         cex = .8,cex.labels = .7)
dev.off()






################################################################ Figure 8
require(dyadRobust)
# int.res <- list()
# 
# for(y in c("mentiondum","research.shares.dum")) {
#   dyad.dat$y <- dyad.dat[[y]]
#   m.nofoll <- lm(scale(y) ~ 
#                    scale(rank.dist) + scale(ugenr.dist) + scale(grenr.dist) + 
#                    tenrowdum*tencoldum +
#                    malerowdum * malecoldum + 
#                    # consrowdum * conscoldum + 
#                    librowdum * libcoldum,
#                  dyad.dat)
#   
#   m.foll <- lm(scale(y) ~ 
#                  scale(rank.dist) + scale(ugenr.dist) + scale(grenr.dist) + 
#                  follows + tenrowdum*tencoldum +
#                  malerowdum * malecoldum + 
#                  # consrowdum * conscoldum + 
#                  librowdum * libcoldum,
#                dyad.dat)
#   
#   m.nofoll.rob <- dyadRobust(fit = m.nofoll,dat = dyad.dat,dyadid = "name",egoid = "namerow",alterid = "namecol")
#   m.foll.rob <- dyadRobust(fit = m.foll,dat = dyad.dat,dyadid = "name",egoid = "namerow",alterid = "namecol")
#   
#   
#   male.int.nofoll <- interaction_plot_continuous(model = m.nofoll,varcov = m.nofoll.rob$Vhat,effect = "malerowdum",pointsplot = T,
#                                                  moderator = "malecoldum",plot = F,num_points = 2,show_est = T,
#                                                  xlabel = c("Female","Male"),colr = "black",alph = 255)
#   tenure.int.nofoll <- interaction_plot_continuous(model = m.nofoll,varcov = m.nofoll.rob$Vhat,effect = "tenrowdum",pointsplot = T,
#                                                    moderator = "tencoldum",plot = F,num_points = 2)
#   # cons.int.nofoll <- interaction_plot_continuous(model = m.nofoll,varcov = m.nofoll.rob$Vhat,effect = "consrowdum",pointsplot = T,
#   #                                                moderator = "conscoldum",plot = F,num_points = 2)
#   lib.int.nofoll <- interaction_plot_continuous(model = m.nofoll,varcov = m.nofoll.rob$Vhat,effect = "librowdum",pointsplot = T,
#                                                 moderator = "libcoldum",plot = F,num_points = 2)
#   
#   male.int.foll <- interaction_plot_continuous(model = m.foll,varcov = m.foll.rob$Vhat,effect = "malerowdum",pointsplot = T,
#                                                moderator = "malecoldum",plot = F,num_points = 2,show_est = T,
#                                                xlabel = c("Female","Male"),colr = "black",alph = 255)
#   tenure.int.foll <- interaction_plot_continuous(model = m.foll,varcov = m.foll.rob$Vhat,effect = "tenrowdum",pointsplot = T,
#                                                  moderator = "tencoldum",plot = F,num_points = 2)
#   # cons.int.foll <- interaction_plot_continuous(model = m.foll,varcov = m.foll.rob$Vhat,effect = "consrowdum",pointsplot = T,
#   #                                              moderator = "conscoldum",plot = F,num_points = 2)
#   lib.int.foll <- interaction_plot_continuous(model = m.foll,varcov = m.foll.rob$Vhat,effect = "librowdum",pointsplot = T,
#                                               moderator = "libcoldum",plot = F,num_points = 2)
#   int.res[[y]] <- list(m.foll = m.foll,m.nofoll = m.nofoll,m.foll.rob = m.foll.rob,
#                        m.nofoll.rob = m.nofoll.rob,
#                        male.int.foll = male.int.foll,
#                        male.int.nofoll =male.int.nofoll,
#                        tenure.int.foll = tenure.int.foll,
#                        tenure.int.nofoll = tenure.int.nofoll,
#                        # cons.int.foll = cons.int.foll,
#                        # cons.int.nofoll = cons.int.nofoll,
#                        lib.int.foll = lib.int.foll,
#                        lib.int.nofoll = lib.int.nofoll)
# }
# save(int.res,file = paste0("./dyad_interactions",ideo,".RData"))



load(paste0("./dyad_interactions",ideo,".RData"))
inter_plot <- function(results = int.res$mentiondum,type = "foll",xlims = NULL) {
  nf <- layout(matrix(1:3, 1, 3, byrow = TRUE), 
               widths=c(.5,2,1))
  par(mar = c(4,0,4,2))
  plot(0,0,ylim = c(.75,9),axes = F,bty = 'n',type = 'n',xlab = "",ylab = "")
  axis(2,at = c(1:2,4:5,7:8),labels = c("Women","Men","TT","Tenured",
                                        "Not Liberal","Liberal"),las = 2,line = -7)
  if(is.null(xlims)) {
    ranges <- c(min(sapply(c("male","tenure","lib"),function(x) min(results[[paste0(x,".int",".",type)]]$lb))),
                max(sapply(c("male","tenure","lib"),function(x) max(results[[paste0(x,".int",".",type)]]$ub))))
  } else {
    ranges <- xlims[[1]]
  }
  plot(0,0,xlim = ranges,ylim = c(.75,9),yaxt='n',ylab = "",xlab = "Estimate",main = "Marginal Effects")
  abline(v = 0,lty = 2)
  rect(-1,2.5,1,3.5,border = NA,col = "grey80")
  rect(-1,5.5,1,6.5,border = NA,col = "grey80")
  rect(-1,8.5,1,9.5,border = NA,col = "grey80")
  text(par('usr')[1],9,pos = 4,labels = "IDEO: MFX of Liberal",cex = 1.1,col = "white")
  text(par('usr')[1],6,pos = 4,labels = "POSITION: MFX of Tenured",cex = 1.1,col = "white")
  text(par('usr')[1],3,pos = 4,labels = "GENDER: MFX of Male",cex = 1.1,col = "white")
  counter <- 1
  for(mod in c("male","tenure","lib")) {
    segments(results[[paste0(mod,".int",".",type)]]$lb,counter:(counter+1),
             results[[paste0(mod,".int",".",type)]]$ub)
    points(results[[paste0(mod,".int",".",type)]]$delta_1,counter:(counter+1),pch = 19)
    counter <- counter + 3
  }
  int.inds <- which(grepl(":",names(results[[paste0("m.",type,".rob")]]$bhat)))
  if(is.null(xlims)) {
    ranges <- c(min(-.01,results[[paste0("m.",type,".rob")]]$bhat[int.inds] - 3*results[[paste0("m.",type,".rob")]]$sehat[int.inds]),
                max(results[[paste0("m.",type,".rob")]]$bhat[int.inds] + 3*results[[paste0("m.",type,".rob")]]$sehat[int.inds]))
  } else {
    ranges <- xlims[[2]]
  }
  plot(0,0,xlim = ranges,ylim = c(.75,9),yaxt='n',ylab = "",xlab = "Estimate",main = "Interaction Terms")
  abline(v = 0,lty = 2)
  rect(-1,2.5,1,3.5,border = NA,col = "grey80")
  rect(-1,5.5,1,6.5,border = NA,col = "grey80")
  rect(-1,8.5,1,9.5,border = NA,col = "grey80")
  text(par('usr')[1],9,pos = 4,labels = "IDEO: Liberal Homophily",cex = 1.1,col = "white")
  text(par('usr')[1],6,pos = 4,labels = "POSITION: Tenured Homophily",cex = 1.1,col = "white")
  text(par('usr')[1],3,pos = 4,labels = "GENDER: Male Homophily",cex = 1.1,col = "white")
  counter <- 1
  for(mod in c("male","ten","lib")) {
    ints <- c(results[[paste0("m.",type,".rob")]]$bhat[paste0(mod,"rowdum:",mod,"coldum")],
              results[[paste0("m.",type,".rob")]]$sehat[paste0(mod,"rowdum:",mod,"coldum")])
    segments(ints[1] - 2*ints[2],counter+.5,ints[1]+2*ints[2])
    points(ints[1],counter+.5,pch = 19)
    counter <- counter + 3
  }  
}


pdf(paste0("./Figures/figure8.pdf"),width = 8,height = 6)
inter_plot(results = int.res$mentiondum,type = "nofoll",xlims = list(c(-.15,.15),c(-.01,.25)))
dev.off()








################################################### Figure 9
euc.dist <- dist(adj.mat$foll.mat,method = "euclidean")
euc.dist <- as.matrix(euc.dist)
hist(euc.dist)
quantile(euc.dist)
zz <- Sys.time()
set.seed(1021)
# full.res <- list()
# for(ys in c("mentiondum","retweets.dum","research.shares.dum","research.shares.na.dum")) {
#   dyad.dat$y <- dyad.dat[[ys]]
#   
#   bs.res <- mfx.res.gender <- mfx.res.position <- mfx.res.ideo.cons <- mfx.res.ideo.lib <- NULL
#   for(i in sample(1:nrow(euc.dist),size = nrow(euc.dist),replace = T)) {
#     # if(sum(follower.adjmat[i,],na.rm=T) < 30) { next }
#     closest <- euc.dist[i,which(euc.dist[i,] < 6.86)]
#     if(length(closest) < 10) { next }
#     tmp.dat <- dyad.dat %>% filter(namerow %in% names(closest)) %>% select(y,rank.dist,ugenr.dist,grenr.dist,matches("(row|col)dum")) %>% filter(complete.cases(.))
#     if(any(unlist(lapply(tmp.dat,function(x) length(unique(x)))) < 2)) {next}
#     if(nrow(tmp.dat) < 1102) { next }
#     bs.res <- rbind(bs.res,summary(tmp <- lm(scale(y) ~ 
#                                                scale(rank.dist) + scale(ugenr.dist) + scale(grenr.dist) + 
#                                                # follows + 
#                                                scale(tenrowdum)*scale(tencoldum) +
#                                                scale(malerowdum) * scale(malecoldum) + 
#                                                # scale(consrowdum) * scale(conscoldum) + 
#                                                scale(librowdum) * scale(libcoldum),
#                                              tmp.dat))$coefficients[,1])
#     mfx.res.gender <- rbind(mfx.res.gender,interaction_plot_continuous(model = tmp,effect = "scale(malerowdum)",moderator = "scale(malecoldum)",num_points = 2)$delta_1)
#     mfx.res.position <- rbind(mfx.res.position,interaction_plot_continuous(model = tmp,effect = "scale(tenrowdum)",moderator = "scale(tencoldum)",num_points = 2)$delta_1)
#     # mfx.res.ideo.cons <- rbind(mfx.res.ideo.cons,interaction_plot_continuous(model = tmp,effect = "scale(consrowdum)",moderator = "scale(conscoldum)",num_points = 2)$delta_1)
#     mfx.res.ideo.lib <- rbind(mfx.res.ideo.lib,interaction_plot_continuous(model = tmp,effect = "scale(librowdum)",moderator = "scale(libcoldum)",num_points = 2)$delta_1)
#   }
#   cat(ys,"done in",Sys.time() - zz,"\n")
#   zz <- Sys.time()
#   full.res[[ys]] <- list(bs.res = bs.res,mfx.res = list(gender = mfx.res.gender,position = mfx.res.position,lib = mfx.res.ideo.lib))
# }
# save(full.res,file = paste0("./mk_ultra",ideo,".RData"))


load(paste0("./mk_ultra",ideo,".RData"))
extract.pval <- function(vect) {
  ev <- round(mean(vect,na.rm=T),3)
  med <- round(median(vect,na.rm=T),3)
  pvals <- ifelse(ev < 0,round(1-ecdf(vect)(0),3),round(ecdf(vect)(0),3))
  stars <- ifelse(pvals < .001,"***",
                  ifelse(pvals < .01,"**",
                         ifelse(pvals < .05,"*","")))
  return(list(pval = pvals,ev = ev,median = med,stars = stars))
}

require(vioplot)
require(scales)
pdf(paste0("./Figures/figure9.pdf"),width = 8,height = 6)
par(mfrow = c(2,3))
for(outs in names(full.res)[-c(3:4)]) {
  for(cov in names(full.res[[outs]]$mfx.res)) {
    ylabs <- ""
    if(cov == "gender") {
      names <- c("Female","Male")
      bs <- "scale(malerowdum):scale(malecoldum)"
      mains <- "Gender"
      ylabs <- ifelse(outs == "mentiondum","Mentions",
                      ifelse(outs == "retweets.dum","Retweets","Research Engagement"))
    } else if(cov == "position") {
      names <- c("Tenure Track","Tenured")
      bs <- "scale(tenrowdum):scale(tencoldum)"
      mains <- "Position"
    } else if(cov == "cons") {
      names <- c("Liberal","Conservative")
      bs <- "scale(consrowdum):scale(conscoldum)"
      mains <- "Ideology"
    } else {
      names <- c("Conservative","Liberal")
      bs <- "scale(librowdum):scale(libcoldum)"
      mains <- "Ideology"
    }
    lims <- quantile(unlist(lapply(full.res, function(x) unlist(lapply(x, function(y) quantile(unlist(y),c(.025,.975)))))),c(0,1))
    
    plot(NULL,xlim = c(.75,2.25),ylim = lims + c(-.02,.02),xaxt='n',
         ylab = ylabs,main = ifelse(outs == "mentiondum",paste0(mains,":\nEffect of ",names[2]," Ego"),""),xlab = paste0("Alter ",mains))
    axis(side = 1,at = c(1,2),labels = names)
    for(i in 1:ncol(full.res[[outs]]$mfx.res[[cov]])) {
      dens <- density(rnorm(1000,mean = mean(full.res[[outs]]$mfx.res[[cov]][,i]),sd(full.res[[outs]]$mfx.res[[cov]][,i])))
      dens <- density(full.res[[outs]]$mfx.res[[cov]][,i])
      dens$y <- rescale(dens$y,c(0,.25))  
      polygon(c(dens$y,rev(dens$y*-1))+i,c(dens$x,rev(dens$x)),border = NA,col = rgb(0,0,0,.2))
      segments(i,quantile(full.res[[outs]]$mfx.res[[cov]][,i],c(.025)),i,quantile(full.res[[outs]]$mfx.res[[cov]][,i],c(.975)))
      points(rep(i,2),c(median(full.res[[outs]]$mfx.res[[cov]][,i]),mean(full.res[[outs]]$mfx.res[[cov]][,i])),pch = 21,bg = c("black","white"))
      infs <- extract.pval(full.res[[outs]]$mfx.res[[cov]][,i])
      text(i,par('usr')[4],paste0("MFX:\n",infs[2],"\n",infs[1],infs[4]),pos = 1)
    }
    infs <- extract.pval(full.res[[outs]]$bs.res[,bs])
    text(1.5,par('usr')[3] + diff(par('usr')[3:4])/2,paste0("Interaction:\n",infs[2],"\n",infs[1],infs[4]),pos = 1)
    abline(h = 0,lty = 3)
  }
}
legend("bottomright",legend = c("Mean","Median"),pch = 21,pt.bg = c("white","black"))
dev.off()











################################################################ Figure 10
gender.foll <- adj.mat$foll.mat
colnames(gender.foll) <- tweeters$Gender
pct.female <- NULL
for(ego in rownames(gender.foll)) {
  pct.female <- c(pct.female,sum(gender.foll[ego,which(colnames(gender.foll) == "Male")],na.rm=T) / sum(gender.foll[ego,],na.rm=T))
}
tweeters$pct.femalebreaks <- cut(pct.female,breaks = quantile(pct.female,c(0,.25,.5,.75,1),na.rm=T),include.lowest = T)

count.labs <- NULL
for(i in unique(tweeters$pct.femalebreaks)) {
  if(is.na(i)) {
    tmp <- tweeters %>% filter(is.na(pct.femalebreaks)) %>%
      group_by(Gender) %>% summarise(n = n())
  } else {
    tmp <- tweeters %>% filter(pct.femalebreaks == i) %>%
      group_by(Gender) %>% summarise(n = n())
  }
  count.labs <- c(count.labs,paste(sapply(1:2,function(x) paste(substr(tmp[x,1],1,1),tmp[x,2],sep = "=")),collapse = "; "))
}

names(count.labs) <- unique(tweeters$pct.femalebreaks)

# int.res2 <- list()
# for(y in c("mentiondum","research.shares.dum")) {
#   dyad.dat$y <- dyad.dat[[y]]
#   for(brk in unique(tweeters$pct.femalebreaks)) {
#     if(is.na(brk)) {
#       tmp.dat <- dyad.dat %>% filter(namerow %in% tweeters$screen_name[which(is.na(tweeters$pct.femalebreaks))])
#     } else {
#       tmp.dat <- dyad.dat %>% filter(namerow %in% tweeters$screen_name[which(tweeters$pct.femalebreaks == brk)])
#     }
#     if(paste0(y,brk) %in% names(int.res2)) { next }
#     m.nofoll <- lm(scale(y) ~
#                      scale(rank.dist) + scale(ugenr.dist) + scale(grenr.dist) +
#                      tenrowdum*tencoldum +
#                      malerowdum * malecoldum +
#                      # consrowdum * conscoldum +
#                      librowdum * libcoldum,
#                    tmp.dat)
#     
#     m.foll <- lm(scale(y) ~
#                    scale(rank.dist) + scale(ugenr.dist) + scale(grenr.dist) +
#                    follows + tenrowdum*tencoldum +
#                    malerowdum * malecoldum +
#                    # consrowdum * conscoldum +
#                    librowdum * libcoldum,
#                  tmp.dat)
#     
#     m.nofoll.rob <- dyadRobust(fit = m.nofoll,dat = tmp.dat,dyadid = "name",egoid = "namerow",alterid = "namecol")
#     m.foll.rob <- dyadRobust(fit = m.foll,dat = tmp.dat,dyadid = "name",egoid = "namerow",alterid = "namecol")
#     
#     
#     male.int.nofoll <- interaction_plot_continuous(model = m.nofoll,varcov = m.nofoll.rob$Vhat,effect = "malerowdum",pointsplot = T,
#                                                    moderator = "malecoldum",plot = F,num_points = 2,show_est = T,
#                                                    xlabel = c("Female","Male"),colr = "black",alph = 255)
#     male.int.foll <- interaction_plot_continuous(model = m.foll,varcov = m.foll.rob$Vhat,effect = "malerowdum",pointsplot = T,
#                                                  moderator = "malecoldum",plot = F,num_points = 2,show_est = T,
#                                                  xlabel = c("Female","Male"),colr = "black",alph = 255)
#     int.res2[[paste0(y,brk)]] <- list(m.foll = m.foll,m.nofoll = m.nofoll,m.foll.rob = m.foll.rob,
#                                       m.nofoll.rob = m.nofoll.rob,
#                                       male.int.foll = male.int.foll,
#                                       male.int.nofoll =male.int.nofoll)
#   }
# }
# save(int.res2,file = "./quartile_interaction.RData")
load("./quartile_interaction.RData")


pdf(paste0("./Figures/figure10.pdf"))
plot(0,0,type = 'n',xlim = c(.5,5.5),ylim = c(-.3,.3),xaxt='n',xlab = "",ylab = "MFX of being Male on Mentioning",main = "Mentions by Gender:\nMarginal Effect on Male Egos by Gender of Alters")
pcts <- 1-unique(as.numeric(unlist(strsplit(gsub("[[:alpha:]]|\\(|\\]|\\.{2,}","",names(int.res2)),","))))
pcts <- cbind(round(pcts[order(pcts)]*100,0)[-5],round(pcts[order(pcts)]*100,0)[-1])
pcts[which(is.na(pcts))] <- 1
labs <- sapply(1:4,function(x) paste(paste0(pcts[x,],"%"),collapse=" to "))
axis(1,at = 1:5,labels = paste(c("Bottom Quartile","2nd Quartile","3rd Quartile","Top Quartile","Don't Follow"),
                               c(labs,"#polisci Scholars"),
                               count.labs[c(3,2,4,5,1)],sep = "\n"),padj = .5,tck =.005,cex.axis = .8)
abline(h = 0,lty = 3)
counter <- 0
for(m in names(int.res2)[which(grepl("mentiondum",names(int.res2)))][c(3,2,4,5,1)]) {
  segments(c(.9,1.1)+counter,int.res2[[m]]$male.int.foll$lb,c(.9,1.1)+counter,int.res2[[m]]$male.int.foll$ub)
  points(c(.9,1.1)+counter,int.res2[[m]]$male.int.foll$delta_1,pch = 21,bg = c("white","black"))
  text(1+counter,par('usr')[4]*.8,label = gsub("Interaction: ","",gsub("\\(","\n\\(",int.res2[[m]]$male.int.foll$est)),pos = 1)
  counter <- counter + 1
}
mtext("Interaction Coefficients:",side = 3,line = -1)
mtext("Share of Women in Total Following",side = 1,line = 4)
legend("bottomright",legend = c("Women","Men"),pch = 21,pt.bg = c("white","black"))
dev.off()









################################################################ Figure 11
labs <- c("Male","TT","Ideo (sq)","School Rank","Tuition","Grad Enroll","UG Enroll")
pdf(paste0("./Figures/figure11.pdf"))
nf <- layout(matrix(1:3, 1, 3, byrow = TRUE), 
             widths=c(1,4,1))
par(mar = c(4,1,4,1))
plot(0,0,type = 'n',bty = 'n',axes = F,xlab = "",ylab = "",ylim = c(.5,7.5))
axis(2,at = 1:nrow(toplot),labels = rev(labs),las = 2,line = -6)

plot(0,0,yaxt='n',ylim = c(.5,7.5),type = 'n',
     xlab = "Estimate",ylab = "",xlim = c(-.45,.45),
     main = "Research Ties")
abline(v = 0,lty = 3)
nudge <- .15
bgs <- c("black","white","gray40","gray80")
pchs <- c(21:24)
counter <- 1
for(y in c("cent_degree_in_res","cent_degree_out_res",
           "cent_bt_res","cent_eigvect_res")) {
  toplot <- summary(res.cent[[y]])$coefficients
  toplot <- toplot[-1,]
  toplot <- toplot[nrow(toplot):1,]
  
  segments(toplot[,1] - 2*toplot[,2],
           (1:nrow(toplot))+nudge,toplot[,1] + 2*toplot[,2])
  points(toplot[,1],(1:nrow(toplot))+nudge,pch = pchs[counter],bg = bgs[counter])
  nudge <- nudge - .1
  counter <- counter + 1
}
legend("topleft",legend = c("In-Deg","Out-Deg","Between","Eigen"),pch = 21:24,
       pt.bg = c("black","white","gray40","gray80"),cex = .8)

plot(0,0,type = 'n',bty = 'n',axes = F,xlab = "",ylab = "",ylim = c(.5,7.5))
segments(-1,.75,-.5,.75)
segments(-1,4.25,-.5,4.25)
segments(-.5,.75,-.5,4.25)
text(-.1,2.5,"School Covariates",srt = -90)
segments(-1,4.75,-.5,4.75)
segments(-1,7.25,-.5,7.25)
segments(-.5,4.75,-.5,7.25)
text(-.1,6,"Individual Covariates",srt = -90)
dev.off()








################################################################ Figure 12
pdf(paste0("./Figures/figure12.pdf"),width = 8,height = 6)
inter_plot(results = int.res$research.shares.dum,type = "foll",xlims = list(c(-.15,.15),c(-.01,.25)))
dev.off()








################################################################# Table 2
for(y in c("research_sh","research_rts","research_favs","research_eng",
           "research_foll","rt_sh","fav_sh","research_cent")) {
  tweeters[[paste0("log_",y)]] <- log((tweeters[[y]]*100)+1)
}


diss.res <- diss.res.smapp <- list()
for(y in c("log_research_sh","log_research_rts","log_research_favs","log_research_eng",
           "log_research_foll","log_rt_sh","log_fav_sh","log_research_cent")) {
  tweeters$y <- tweeters[[y]]
  diss.res[[y]] <- lmer(y ~ 
                          Gender + factor(tenurebin) + 
                          factor(ideobin) + 
                          scale(logged_tweets) + 
                          scale(logged_followers) + 
                          scale(years_old) + 
                          scale(Rank) + 
                          factor(R1) +
                          factor(state) +
                          scale(GRFTF17) + scale(UGTENR17) + 
                          (1|school),tweeters)
}

stargazer(diss.res$log_research_sh,diss.res$log_research_rts,diss.res$log_research_favs,
          diss.res$log_research_eng,diss.res$log_research_foll,
          keep = "GenderMale|tenurebin|ideobin|years|Rank|\\(R1\\)|state|GRFTF17",
          covariate.labels = c("Male","Tenure Track","Moderate","Conservative","Years Online",
                               "School Rank","R1 Inst.","State School","Grad Enroll"),
          keep.stat = c("n","ll"),out = paste0("./Tables/table2.tex"),star.cutoffs = c(.05,.01,.001))
