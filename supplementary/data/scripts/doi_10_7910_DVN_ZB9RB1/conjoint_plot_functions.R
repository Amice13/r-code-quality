plot_fnct_mmdiff <- function(DATA){
  plot(DATA,
       size = 1.5,
       vline = 0, 
       xlim = c(-0.3,0.3), 
       xlab = "Estimated Difference in Marginal Means"
  ) +
    ggplot2::geom_text(
      aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
      colour = "black", 
      size = 2.5,
      position = position_nudge(y = .5)
    ) +
    ggplot2::theme(legend.position = "none")+
    theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
    theme(panel.grid.major = element_line())
}

plot_fnct_amcediff<- function(DATA){
  plot(DATA,
       size = 1.5,
       vline = 0, 
       xlab = "Estimated Difference in AMCE"
  ) +
    ggplot2::geom_text(
      aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
      colour = "black", 
      size = 2.5,
      position = position_nudge(y = .5)
    ) +
    ggplot2::theme(legend.position = "none") + 
    theme(axis.text.y = element_text(face = c('plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain','bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold', 'plain', 'bold')))+
    theme(panel.grid.major = element_line())
}

plot_fnct_amce <- function(DATA) {
  plot(DATA, 
     group = "BY",
     size = 1.5,
     vline = 0)+
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
    theme(legend.title = element_blank())
}

plot_fnct_mm <- function(DATA){
  plot(DATA, 
       group = "BY",
       size = 1.5,
       vline = 0)+
    theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
    theme(panel.grid.major = element_line())+
    theme(legend.title = element_blank())
}

