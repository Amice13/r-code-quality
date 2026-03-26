

#### PART 2. If yes, how may this impact measurement values? ####


## distribution of ideology for individuals in each top scoring topic
    for (i in 1:N.topics){#print(mean(d$leftright[d$max.left==i], na.rm=TRUE))
                          }
    for (i in 1:N.topics){#print(mean(d$leftright[d$max.right==i], na.rm=TRUE))
                          }

  # PLOT MEANS FOR DIFFERENT SMALL CATEGORIES
    # LEFT
      names <- names.left
      means <- NULL; lowerci <- NULL; upperci <- NULL; means0 <- NULL; lowerci0 <- NULL; upperci0 <- NULL
      for (i in 1:N.topics){
        meani <- mean(d$leftright[d$max.left==i], na.rm=TRUE)
        means <- c(means,meani)
        n <- sum(!is.na(d$leftright[d$max.left==i]))
        sdev <- sd(d$leftright[d$max.left==i], na.rm=TRUE)
        lower <-     meani - qt(0.975,df=n-1)*sdev/sqrt(n-1)
        lowerci <- c(lowerci,lower)
        upper <-     meani + qt(0.975,df=n-1)*sdev/sqrt(n-1)
        upperci <- c(upperci,upper)
      }
      graphdata <- data.frame(rbind(upperci, means, lowerci))
      names(graphdata) <- names
      # GRAPH TO PLOT MEANS FOR TOPICS ON LEFT-RIGHT SCALE
      pdf(file="plots/means_left.pdf", width=6, height=2)
      plot.new()
      par(mar=c(0,0,0,0),oma=c(0,0,0,0))
      plot.window(xlim=c(0,11), ylim=c(-1, N.topics+1))
      lines(c(1,10), c(0,0))
      for (i in 1:10){segments(i,-.4, i,.4)}
      text(0, 0, "left")
      text(11, 0, "right")
      y.positions <- seq(1,N.topics,1)
      points(graphdata[2,], y.positions, pch=19)
      for (i in 1:N.topics){segments(graphdata[3,i],i,graphdata[1,i],i)}
      for (i in 1:N.topics){text(7.5,i, names(graphdata)[i], cex=.7)}
      text(2,N.topics, "Associations  \n with left", cex=.7)
      for(i in 1:10){text(i,-1, as.character(i), cex=.6)}
      segments(mean(d$leftright, na.rm=TRUE),0,mean(d$leftright, na.rm=TRUE),4, lty=2)
      dev.off()
    # RIGHT
      names <- names.right
      means <- NULL; lowerci <- NULL; upperci <- NULL; means0 <- NULL; lowerci0 <- NULL; upperci0 <- NULL
      for (i in 1:N.topics){
        meani <- mean(d$leftright[d$max.right==i], na.rm=TRUE)
        means <- c(means,meani)
        n <- sum(!is.na(d$leftright[d$max.right==i]))
        sdev <- sd(d$leftright[d$max.right==i], na.rm=TRUE)
        lower <-     meani - qt(0.975,df=n-1)*sdev/sqrt(n-1)
        lowerci <- c(lowerci,lower)
        upper <-     meani + qt(0.975,df=n-1)*sdev/sqrt(n-1)
        upperci <- c(upperci,upper)
      }
      graphdata <- data.frame(rbind(upperci, means, lowerci))
      names(graphdata) <- names
      # GRAPH TO PLOT MEANS FOR TOPICS ON LEFT-RIGHT SCALE
      pdf(file="plots/means_right.pdf", width=6, height=2)
      # windows(6,2)
      plot.new()
      par(mar=c(0,0,0,0),oma=c(0,0,0,0))
      plot.window(xlim=c(0,11), ylim=c(-1, N.topics+1))
      lines(c(1,10), c(0,0))
      for (i in 1:10){segments(i,-.4, i,.4)}
      text(0, 0, "left")
      text(11, 0, "right")
      y.positions <- seq(1,N.topics,1)
      points(graphdata[2,], y.positions, pch=19)
      for (i in 1:N.topics){segments(graphdata[3,i],i,graphdata[1,i],i)}
      for (i in 1:N.topics){text(7.5,i, names(graphdata)[i], cex=.7)}
      text(2,N.topics, "Associations  \n with right", cex=.7)
      for(i in 1:10){text(i,-1, as.character(i), cex=.6)}
      segments(mean(d$leftright, na.rm=TRUE),0,mean(d$leftright, na.rm=TRUE),4, lty=2)
      dev.off()

