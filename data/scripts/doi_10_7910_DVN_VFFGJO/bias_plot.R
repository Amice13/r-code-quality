bias_plot <- function(bias_posterior, ck_data, title = NULL) {
  quants <- apply(bias_posterior, 2, quantile, 
                  probs = c(0.025, .25, 0.5, 0.75, 0.975))
  congress_data <- dplyr::summarize(dplyr::group_by(ck_data, congress), 
                                    majority = mean(demmaj))
  Majority <- factor(congress_data$majority, 
                     labels = c("Republican", "Democrat"))
  df <- data.frame(Congress = congress_data$congress, Majority, 
                   LB   = quants[1,], LB50 = quants[2,], 
                   Bias = quants[3, ], 
                   UB50 = quants[4,], UB = quants[5,])
  congress <- data.frame(number = 46:106)
  congress <- dplyr::mutate(congress, 
                            start = 1878 + seq(1,122,2), 
                            end = start + 2,
                            pre_Reed = (end <= 1890),
                            post_Czar = (end %in% 1890:1910),
                            post_Cannon = (end %in% 1912:1961),
                            post_Packing = (end %in% 1962:2001)
  )
  landmarks <- c(1890, 1910, 1961)
  ll        <- length(landmarks)
  xmin      <- congress$number[1]
  xmax      <- congress$number[nrow(congress)]
  for(i in 1:ll) {
    xmin <- c(xmin, congress$number[which.max(which(congress$end <= landmarks[i]))])
  }
  xmax <- c(xmin[-1], xmax)
  landmark_names  <- cbind(c("pre-", rep("post-",3)), c("Reed", "Reed", "Cannon", "Packing"))
  dem_color       <- "#4589c3"
  rep_color       <- "#c34589"
  landmark_color  <- 'gray50'
  dark_gray_color <- "#222222"
  axis_line_color <- dark_gray_color
  
  my_theme <- theme(
    axis.line.x = element_line(size = 0.5, color = axis_line_color),
    axis.line.y = element_line(size = 0.5, color = axis_line_color),
    axis.text   = element_text(size=13, color="black"),
    axis.title  = element_text(size=15),
    legend.position = "bottom",
    legend.text  = element_text(size = 15), 
    legend.title = element_text(size = 15), 
    legend.box   = "vertical",
    panel.grid.major = element_line(colour = 'gray90', size = 0.5),
    panel.grid.minor = element_line(colour = 'gray90', size = 0.2)
  )
  
  period_lab <- "Period hypothesized by Cox & Katz to show bias toward majority"
  my_clrs    <- scale_color_manual(values = c('gray10', 'gray55'), name = "Majority Party")
  my_lty     <- scale_linetype_manual(values = 'dashed', labels = period_lab, name = "")
  my_guides  <- guides(color   = guide_legend(order = 1), 
                      linetype = guide_legend(order = 2, keywidth=3, override.aes = list(size=1.3)), 
                      shape    = guide_legend(order = 1))
  
  x_scale <- scale_x_continuous(breaks = c(46, seq(50,100,10), 106), 
                                labels = c("46 \n 1879   ", "50 \n 1887 ", "60 \n 1907  ", "70 \n 1927  ", "80 \n 1947  ", "90 \n 1967  ", "100 \n 1987 ", "106 \n 1999 "))
  y_scale <- scale_y_continuous(limits = c(-.2, .3))
  
  rects1 <- annotate("rect", xmin=xmin, xmax=xmax, ymin=-0.175, ymax=-0.125, fill = c("black","gray25","black","gray25"))
  rects2 <- annotate("rect", xmin=xmin[c(2,4)], xmax=xmax[c(2,4)], ymin=-0.130, ymax=-0.125, fill = landmark_color)
  rects3 <- annotate("rect", xmin=c(seq(50,58,2),seq(86,104,2)), xmax=c(seq(51,59,2),seq(87,105,2)), ymin=-0.130, ymax=-0.125, fill = 'white')
  
  txts1 <- annotate("text", x = (xmin + xmax)/2, y = -0.1375, label = landmark_names[,1], size = 4, color = "white")
  txts2 <- annotate("text", x = (xmin + xmax)/2, y = -0.16,   label = landmark_names[,2], size = 4, color = "white")
  
  base  <- ggplot(df, aes(x = Congress, y = Bias, ymin = LB, ymax = UB))
  graph <- base + my_lty + my_clrs + x_scale + y_scale + ylab("Bias toward majority") +
    geom_hline(yintercept = 0, color = "black", linetype = 3) +
    geom_linerange(aes(color = Majority), size = 1.75, alpha = .75) +
    geom_point(color='black', aes(shape = Majority), size = 2) +
    geom_segment(aes(linetype=""), x=xmin[2], xend=xmax[2], y=-.1725, yend=-0.1725, size=1, color=landmark_color) +
    rects1 + rects2 + txts1 + txts2 + rects3  +
    scale_shape_manual(name = "Majority Party", values = c(17,19)) +
    my_guides 
  
  graph <- graph + theme_bw() %+replace% my_theme
  
  return(graph)
}

my_theme <- theme(
  legend.position = 'bottom',
  plot.title  = element_text(hjust = 0.5, face  = "plain", size = 12),
  axis.line.x = element_line(size = 0.5,  color = axis_line_color),
  axis.line.y = element_line(size = 0.5,  color = axis_line_color),
  axis.text   = element_text(size = 13,   color = "black"),
  axis.title  = element_text(size = 15),
  panel.grid.major = element_line(colour = 'gray90', size = 0.5),
  panel.grid.minor = element_line(colour = 'gray90', size = 0.2))
