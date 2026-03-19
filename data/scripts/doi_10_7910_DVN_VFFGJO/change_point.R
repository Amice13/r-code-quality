change_point <- function(avgdf, mcmc_n = 50000, p = 0.2, w = 0.01,
                         outfile, margin_t = c(3, 3, 0, 4.75), margin_b = c(-2, 3, 3, 2.42),
                         axis.title.x.size = 15, axis.title.y.size = 15, axis.text.size = 12, axis.text.x.size = 11, axis.text.y.size = 12,
                         x.angle = NULL, x.hjust = NULL, 
                         xtitle = 'Congress/Year', xlabs = paste(conglabs, yrlabs, sep='\n')){
  
  coalsize <- as.matrix(avgdf)
  bcp.out  <- bcp(coalsize, p0 = p, w0 = w, mcmc = mcmc_n)
  bp       <- breakpoints(coalsize ~ 1)
  ci       <- confint(bp, level = 0.90)
  
  df        <- data.frame(bcp.out$data, bcp.out$posterior.mean, bcp.out$posterior.prob)
  names(df) <- c('x', 'pt', 'mean', 'prob')
  df$prob   <- ifelse(is.na(df$prob), 0, df$prob)
  cong      <- round(rescale(congrow, 1, nrow(df)))
  
  df$cong   <- NA
  df$yr     <- NA
  for (i in 1:length(cong)){
    df$cong[cong[i]] <- conglabs[i]
    df$yr[cong[i]]   <- yrlabs[i]
  }
  
  plist <- list()
  plist[['coalsize']] <- coalsize
  plist[['bcp.out']]  <- bcp.out
  plist[['bp']]       <- bp
  plist[['ci']]       <- ci
  
  plist[['plt1']]     <- ggplot(data = df, aes(x = x)) +
    geom_vline(xintercept = ci$confint, lty = c(3,2,3), color = 'black') +
    geom_point(aes(y = pt), color = 'gray30') +
    geom_line(aes(y = mean)) + 
    labs(y = 'Posterior Mean') +
    theme_bw() +
    theme(axis.title.x     = element_blank(),
          axis.text.x      = element_blank(),
          axis.title.y     = element_text(size = axis.title.y.size, color = "black"),
          axis.text        = element_text(size = axis.text.size,    color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  plist[['plt2']]     <- ggplot(data = df, aes(x = x, y = prob)) +
    geom_vline(xintercept = ci$confint, lty = c(3,2,3), color = 'black') +
    geom_line() + 
    labs(y = 'Posterior Probability', x = xtitle) +
    ylim(0, ceiling(max(df$prob, na.rm=T)*100)/100) + 
    scale_x_continuous(breaks = cong, labels = xlabs) +
    theme_bw() +
    theme(axis.text.x  = element_text(size = axis.text.x.size, angle = x.angle, hjust = x.hjust, color="black"),
          axis.text.y  = element_text(size = axis.text.y.size, color="black"),
          axis.title.x = element_text(size = axis.title.y.size),
          axis.title.y = element_text(size = axis.title.x.size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  pdf(file = outfile, width = 5, height = 5)
  grid.arrange(arrangeGrob(plist[['plt1']] + theme(plot.margin = unit(margin_t,  "mm"))),
               arrangeGrob(plist[['plt2']] + theme(plot.margin = unit(margin_b,  "mm"))),
               ncol = 1, nrow = 2, heights = c(5,5))
  dev.off() 
  return(plist)
}