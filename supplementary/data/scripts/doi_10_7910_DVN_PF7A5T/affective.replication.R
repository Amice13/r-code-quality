#Affective (In)Attention Replication Script

load("df.Rdata")
load("part.df.Rdata")

library(modelsummary)
library(effects)

##Figure 1
rdf <- df[df$rsp.vidF=="056-7" & !is.na(df$rsp.vidF),]
rdf$time <- c(1:59)

{
  png("figure.lost.056.png", width = 7, height = 5, units = "in", bg = "white", res = 300)
  plot(rdf$time, rdf$gsl, type = "n", ann = F, axes = F, ylim = c(0, 3))
  abline(h = 0, lty = 2)
  abline(v = 2, lty = 2)
  abline(v = 12, lty = 2)
  text(x = 7, y = 2, labels = "bv", cex = .8)
  polygon(c(2, 2, 12, 12), c(0, 3.5, 3.5, 0), col = rgb(0.8, 0.8, 0.8, alpha = 0.3), border = NA)
  abline(v = 45, lty = 2)
  text(x = 51, y = 2, labels = "ev", cex = .8)
  polygon(c(45, 45, 55, 55), c(0, 3.5, 3.5, 0), col = rgb(0.8, 0.8, 0.8, alpha = 0.3), border = NA)
  abline(v = 5, lty = 3)
  text(x = 6, y = 0.5, labels = "man screaming", cex = .8, srt = -90)
  abline(v = 42, lty = 3)
  text(x = 43, y = 1.5, labels = "bloody leg", cex = .8, srt = -90)
  abline(v = 49, lty = 3)
  text(x = 50, y = 0.6, labels = "screaming woman", cex = .8, srt = -90)
  abline(v = 55, col = "red" , lty = 2)
  lines(rdf$time, rdf$gsl, type = "l", col = "black", lwd = 3)
  axis(side = 2, las = 1, cex.axis = 0.8, col = "black")
  mtext(side = 2, expression(paste("Normalized Skin Conductance")), cex = 0.8, line = 3)
  axis(side = 1, las = 1, cex.axis = 0.8, col = "white", padj = 0.8, at = seq(1, 130, by = 10))
  mtext(side = 1, "seconds", cex = 1, line = 3)
  text(1, 3, "Lost, Respondent 056", pos = 4, cex = 1.5)
  dev.off()
}

##Figure 2
rdf2 <- df[df$rsp.vidF=="032-13" & !is.na(df$rsp.vidF),]
rdf2$time <- c(1:68)

{
  png("figure.carpool 032.png", width = 7, height = 5, units = "in", bg = "white", res = 300)
  plot(rdf2$time, rdf2$gsl, type = "n", ann = F, axes = F, ylim = c(0, 2.5))
  abline(v = 2, lty = 2)
  abline(v = 12, lty = 2)
  text(x = 7, y = 0.5, labels = "bv", cex = .8)
  polygon(c(2, 2, 12, 12), c(0, 3, 3, 0), col = rgb(0.8, 0.8, 0.8, alpha = 0.3), border = NA)
  abline(v = 54, lty = 2)
  text(x = 59, y = 0.5, labels = "ev", cex = .8)
  abline(h = 0, lty = 2)
  abline(v = 64, col = "red" , lty = 2)
  polygon(c(54, 54, 64, 64), c(0, 3, 3, 0), col = rgb(0.8, 0.8, 0.8, alpha = 0.3), border = NA)
  lines(rdf2$time, rdf2$gsl, type = "l", col = "black", lwd = 3)
  axis(side = 2, las = 1, cex.axis = 0.8, col = "black")
  mtext(side = 2, expression(paste("Normalized Skin Conductance")), cex = 0.8, line = 3)
  axis(side = 1, las = 1, cex.axis = 0.8, col = "white", padj = 0.8, at = seq(1, 70, by = 10))
  mtext(side = 1, "seconds", cex = 1, line = 3)
  text(1, 2.5, "Carpool Karaoke, Respondent 032", pos = 4, cex = 1.5)
  dev.off()
}

##Hypotheses tests

##Figure 3 GSL
sum(part.df$A.gsl4 >0)
sum(part.df$A.gsl4 <0)
sum(part.df$A.gsl4 == 0)

mean(part.df$A.gsl4)
max(part.df$A.gsl4)
min(part.df$A.gsl4)

library(ggplot2)
A.scl.h1a <- ggplot(part.df, aes(x = A.gsl4)) +
  geom_histogram(
    binwidth = 0.25,    
    color = "black", 
    fill = "slategray1", 
    alpha = 0.7, 
    boundary = 0      
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(part.df$A.gsl4) / 0.25) * 0.25, 
                 ceiling(max(part.df$A.gsl4) / 0.25) * 0.25, 
                 by = 0.25),  
    labels = scales::number_format(accuracy = 0.01)  
  ) +
  scale_y_continuous(expand = c(0, 0)) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "black", size = .5), 
    axis.line.y = element_line(color = "black", size = .5)
  ) +
  labs(
    title = "Frequency of A (Skin Conductance), 10 second BV and EV", 
    x = "A (SCL)", 
    y = "Count"
  ) +
  geom_vline(
    xintercept = 0, 
    linetype = "longdash", 
    color = "red",        
    size = 1
  )

##Figure 3 HR
sum(part.df$A.hr4 >0)
sum(part.df$A.hr4 <0)
sum(part.df$A.hr4 == 0)

mean(part.df$A.hr4)
max(part.df$A.hr4)
min(part.df$A.hr4)

A.hr.3a <- ggplot(part.df, aes(x = A.hr4)) +
  geom_histogram(
    binwidth = 4, 
    color = "black", 
    fill = "slategray1", 
    alpha = 0.7, 
    boundary = 0  
  ) +
  scale_x_continuous(
    limits = c(-36, 20),  
    breaks = seq(-36, 20, by = 4),  
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5)
  ) +
  labs(
    title = "Frequency of A (Heart Rate), 10 second BV and EV", 
    x = "A (HR)", 
    y = "Count"
  ) +
  geom_vline(
    xintercept = 0,  
    linetype = "longdash", 
    color = "red",        
    size = 1
  )

library(gridExtra)
combined_plot <- grid.arrange(A.scl.h1a, A.hr.3a, nrow = 2)
ggsave("combined_plot.png", plot = combined_plot, width = 8, height = 10, dpi = 300, bg="white")


#Figure 4 News v Entertainment
sum(part.df$A.gsl4[part.df$type == "news"] >0)
sum(part.df$A.gsl4[part.df$type == "news"] <0)
sum(part.df$A.gsl4[part.df$type == "news"] == 0)

mean(part.df$A.gsl4[part.df$type == "news"])
max(part.df$A.gsl4[part.df$type == "news"])
min(part.df$A.gsl4[part.df$type == "news"])

sum(part.df$A.gsl4[part.df$type == "ent"] >0)
sum(part.df$A.gsl4[part.df$type == "ent"] <0)
sum(part.df$A.gsl4[part.df$type == "ent"] == 0)

mean(part.df$A.gsl4[part.df$type == "ent"])
max(part.df$A.gsl4[part.df$type == "ent"])
min(part.df$A.gsl4[part.df$type == "ent"])

x_limits <- c(floor(min(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
              ceiling(max(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25)

plot_news <- ggplot(subset(part.df, type == "news"), aes(x = A.gsl4, fill = type)) +   
  geom_histogram(
    binwidth = .25,
    position = "dodge",
    color = "black",
    alpha = 0.7,
    boundary = 0
  ) +   
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(
      floor(min(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      ceiling(max(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      by = 0.25
    ),
    labels = scales::number_format(accuracy = 0.01)
  ) +   
  scale_y_continuous(expand = c(0, 0)) +   
  scale_fill_manual(values = c("news" = "slategray1")) +   
  theme_minimal() +   
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) +   
  labs(
    title = "Frequency of A (Skin Conductance), News",
    x = "A (SCL)",
    y = "Count",
    fill = "type"
  ) +   
  geom_vline(
    xintercept = 0,
    linetype = "longdash",
    color = "red",
    size = 1
  )

plot_ent <- ggplot(subset(part.df, type == "ent"), aes(x = A.gsl4, fill = type)) +   
  geom_histogram(
    binwidth = .25,
    position = "dodge",
    color = "black",
    alpha = 0.7,
    boundary = 0
  ) +   
  scale_x_continuous(
    limits = x_limits, 
    breaks = seq(
      floor(min(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      ceiling(max(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      by = 0.25
    ),
    labels = scales::number_format(accuracy = 0.01)
  ) +   
  scale_y_continuous(expand = c(0, 0)) +   
  scale_fill_manual(values = c("ent" = "dodgerblue")) +   
  theme_minimal() +   
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) +   
  labs(
    title = "Frequency of A (Skin Conductance), Entertainment",
    x = "A (SCL)",
    y = "Count",
    fill = "type"
  ) +   
  geom_vline(
    xintercept = 0,
    linetype = "longdash",
    color = "red",
    size = 1
  )


plot_newshr <- ggplot(subset(part.df, type == "news"), aes(x = A.hr4, fill = type)) +  
  geom_histogram(
    binwidth = 4,  
    position = "dodge",  
    color = "black",  
    alpha = 0.7,      
    boundary = 0 
  ) + 
  scale_x_continuous(
    limits = c(-32, 32),  
    breaks = seq(-32, 32, by = 4),  
    labels = scales::number_format(accuracy = 1)  
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("news" = "slategray1")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) + 
  labs(
    title = "Frequency of A (Heart Rate), News", 
    x = "A (HR)", 
    y = "Count",
    fill = "type"
  ) +
  geom_vline(
    xintercept = 0, 
    linetype = "longdash", 
    color = "red",        
    size = 1
  )

plot_enthr <- ggplot(subset(part.df, type == "ent"), aes(x = A.hr4, fill = type)) +  
  geom_histogram(
    binwidth = 4, 
    position = "dodge", 
    color = "black",
    alpha = 0.7,      
    boundary = 0 
  ) + 
  scale_x_continuous(
    limits = c(-32, 32),  
    breaks = seq(-32, 32, by = 4),  
    labels = scales::number_format(accuracy = 1)  
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("ent" = "dodgerblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) + 
  labs(
    title = "Frequency of A (Heart Rate), Entertainment", 
    x = "A (HR)", 
    y = "Count",
    fill = "type"
  ) +
  geom_vline(
    xintercept = 0, 
    linetype = "longdash", 
    color = "red",        
    size = 1
  )

library(gridExtra)
combined_plot <- grid.arrange(plot_news, plot_newshr, plot_ent, plot_enthr, ncol = 2, nrow = 2)
ggsave("A.news.png",  plot = combined_plot, width = 9, height=6.5, bg="white")


#Figure 5 Positive v Negative
x_limits <- c(floor(min(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
              ceiling(max(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25)


plot_neg <- ggplot(subset(part.df, valence == "neg"), aes(x = A.gsl4, fill = valence)) +   
  geom_histogram(
    binwidth = .25,
    position = "dodge",
    color = "black",
    alpha = 0.7,
    boundary = 0
  ) +   
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(
      floor(min(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      ceiling(max(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      by = 0.25
    ),
    labels = scales::number_format(accuracy = 0.01)
  ) +   
  scale_y_continuous(expand = c(0, 0)) +   
  scale_fill_manual(values = c("neg" = "slategray1")) +   
  theme_minimal() +   
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) +   
  labs(
    title = "Frequency of A (Skin Conductance), Negative Stimuli",
    x = "A (SCL)",
    y = "Count",
    fill = "valence"
  ) +   
  geom_vline(
    xintercept = 0,
    linetype = "longdash",
    color = "red",
    size = 1
  )

plot_pos <- ggplot(subset(part.df, valence == "pos"), aes(x = A.gsl4, fill = valence, legend = FALSE)) +   
  geom_histogram(
    binwidth = .25,
    position = "dodge",
    color = "black",
    alpha = 0.7,
    boundary = 0
  ) +   
  scale_x_continuous(
    limits = x_limits, 
    breaks = seq(
      floor(min(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      ceiling(max(part.df$A.gsl4, na.rm = TRUE) / 0.25) * 0.25,
      by = 0.25
    ),
    labels = scales::number_format(accuracy = 0.01)
  ) +   
  scale_y_continuous(expand = c(0, 0)) +   
  scale_fill_manual(values = c("pos" = "dodgerblue")) +   
  theme_minimal() +   
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) +   
  labs(
    title = "Frequency of A (Skin Conductance), Positive Stimuli",
    x = "A (SCL)",
    y = "Count",
    fill = "valence"
  ) +   
  geom_vline(
    xintercept = 0,
    linetype = "longdash",
    color = "red",
    size = 1
  )


plot_hrneg <- ggplot(subset(part.df, valence == "neg"), aes(x = A.hr4, fill = valence)) +  
  geom_histogram(
    binwidth = 4,  
    position = "dodge",  
    color = "black", 
    alpha = 0.7,     
    boundary = 0 
  ) + 
  scale_x_continuous(
    limits = c(-32, 32),  
    breaks = seq(-32, 30, by = 4),  
    labels = scales::number_format(accuracy = 1)  
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("neg" = "slategray1")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) + 
  labs(
    title = "Frequency of A (Heart Rate), Negative Stimuli", 
    x = "A (HR)", 
    y = "Count",
    fill = "valence"
  ) +
  geom_vline(
    xintercept = 0, 
    linetype = "longdash", 
    color = "red",        
    size = 1
  )

plot_hrpos <- ggplot(subset(part.df, valence == "pos"), aes(x = A.hr4, fill = valence)) +  
  geom_histogram(
    binwidth = 4,  
    position = "dodge",  
    color = "black",  
    alpha = 0.7,      
    boundary = 0 
  ) + 
  scale_x_continuous(
    limits = c(-32, 32),  
    breaks = seq(-32, 32, by = 4),  
    labels = scales::number_format(accuracy = 1)  
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("pos" = "dodgerblue")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black", size = .5),
    axis.line.y = element_line(color = "black", size = .5),
    legend.position = "none"
  ) + 
  labs(
    title = "Frequency of A (Heart Rate), Positive Stimuli", 
    x = "A (HR)", 
    y = "Count",
    fill = "valence"
  ) +
  geom_vline(
    xintercept = 0, 
    linetype = "longdash", 
    color = "red",        
    size = 1
  )
valence_plot <- grid.arrange(plot_neg, plot_hrneg, plot_pos, plot_hrpos, ncol = 2, nrow = 2)
ggsave("A.valence.png",  plot = valence_plot, width = 9, height=6.5, bg="white")
