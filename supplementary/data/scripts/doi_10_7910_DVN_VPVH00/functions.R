
# Load figure functions ---------------------------------------------------

#Set color scheme
display.brewer.all()
display.brewer.pal(9,"Set1")
brewer.pal(9,"Set1")

CreatePalette <- function(base){
  fc <- colorRampPalette(c(base, "black"))
  pal1 <- fc(9)[3]
  
  
  fc <- colorRampPalette(c(base, "white"))
  pal2 <- fc(5)[1:2]
  pal3 <- fc(9)[6]
  
  plot(rep(1, 4),col = c(pal1, pal2, pal3), pch = 19, cex = 7)
  c(pal1, pal2, pal3)
}

reds <- CreatePalette(brewer.pal(9,"Set1")[1])
blues <- CreatePalette(brewer.pal(9,"Set1")[2])
greens <- CreatePalette(brewer.pal(9,"Set1")[3])
purples <- CreatePalette(brewer.pal(9,"Set1")[4])
oranges <- CreatePalette(brewer.pal(9,"Set1")[5])

# Stacked bar function ----------------------------------------------------

fills <- c("Correct" = blues[4], "Correct after mistakes" = blues[2])

StackedBar <- function(data, ylab = "Proportion of children", facet = TRUE,
  x_labels = NULL, xlab = "Transaction", legend.pos = "right", text.size = 3, 
  remove.left = FALSE, remove.right = FALSE){   
  p <- ggplot(data, aes(y = manycorr, ymin = low, ymax = hig, x = factor(bar))) + 
    geom_bar(stat = "identity", aes(fill = "Correct after mistakes"), col = "black", size = 0.25) +
    geom_bar(aes(y = mcorr, fill = "Correct"), stat = "identity", col = "black", size = 0.25) + 
    geom_errorbar(width = 0.1) + 
    labs(x = xlab, y = ylab, fill = "") + 
    scale_fill_manual(values = fills) + 
    theme_bw() + 
    theme(legend.position = legend.pos) + 
    ylim(c(0,1.02)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + 
    geom_text(aes(y = 0.05, label = paste0("(N = ", n, ")")), size = text.size) + 
    geom_text(aes(y = 0.13, label = format(round(manycorr,2), nsmall =2)), size = text.size) + 
    theme(strip.background=element_rect(fill="grey96"))
  
  if (facet == TRUE){
    p <- p + facet_wrap(vars(figure))  
  }
  
  if (remove.left == TRUE){
    p <- p + theme(axis.text.y = element_blank(),  axis.ticks.y = element_blank())+ 
      theme(plot.margin = unit(c(5.5,5.5,16.5,0), "pt"))
  }
  
  if (remove.right == TRUE){
    p <- p + theme(plot.margin = unit(c(5.5,0.75,5.5,5.5), "pt"))
  }
  
  
  if (!is.null(x_labels)){
    p <- p + scale_x_discrete(breaks = 1:max(data$bar), labels = x_labels) 
  } 
  
  p
} 


# ASER bar function -------------------------------------------------------

AserBar <- function(data, facet = FALSE, ylim = 1, ylab =  "Proportion of children", 
  xvar = level, yvar = math, annotate = TRUE, annotate.pos = 0.05, annotate.gap = 0.07, remove.left= FALSE, remove.right = FALSE,
  x_labels = c("1-digit \n recognition", "2-digit \n recognition", "Subtraction",  "Division"), text.size = 3){
  
  xvar <- enquo(xvar)
  yvar <- enquo(yvar)
  n_level = annotate.pos*ylim
  b_level = (annotate.pos + annotate.gap)*ylim  
  
  p <- ggplot(data, aes(y = !! yvar , ymin = low, ymax = hig, x = !! xvar)) + 
    geom_bar(stat = "identity", aes(fill = !! xvar, col = !! xvar), size = 0.5)+
    geom_errorbar(width = 0.1) + 
    labs(x = "", y = ylab, fill = "") + 
    scale_fill_manual(values = c(reds[4], blues[4], greens[4], purples[4], oranges[4]), guide = "none" ) + 
    scale_color_manual(values = c("black", "black", "black", "black", "black"), guide = "none") + 
    theme_bw() + 
    ylim(c(0,ylim)) + 
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + 
    scale_x_discrete(labels = x_labels) + 
    theme(legend.position = "none")+ 
    theme(strip.background=element_rect(fill="grey96"))
  
  if (facet == TRUE){
    p <- p + facet_wrap(vars(figure))  
  }
  
  if (remove.left == TRUE){
    p <- p + theme(axis.text.y = element_blank(),  axis.ticks.y = element_blank())+ 
      theme(plot.margin = unit(c(5.5,5.5,16.5,0), "pt"))
  }
  
  if (remove.right == TRUE){
    p <- p + theme(plot.margin = unit(c(5.5,0.75,5.5,5.5), "pt"))
  }
  
  
  if (annotate == TRUE){
    p <- p + geom_text(aes(y = n_level, label = paste0("(N = ", n, ")")), size = text.size) + 
      geom_text(aes(y = b_level, label = format(round(!! yvar ,2), nsmall =2)), size = text.size) 
  }
  
  p
}


# Bar Plot Maker  ---------------------------------------------------------


# BarPlotFacet ------------------------------------------------------------
#Barplot maker that incorporates facetting
#Notes must be clearner way to create the function labels 
#Notes: Would be nice to have control over the p-value regression (obv possible)
# -------------------------------------------------------------------------


BarPlotFacet <- function(data, treat, outcome, filename, title = "", height = 12, width = 12, 
  legend.pos = "right",  ylim.min = 0, ylim.max = 1, ylab = "", xlab = "", 
  fill.palette = NULL, color.palette = NULL, p.display = FALSE, x.label = TRUE, annotate = TRUE,
  col.bw = FALSE, facet.wrap = NULL, wrap.row = 1, facet.grid1 = NULL, annotate.pos = 0.05, 
  facet.grid2 = NULL, drop.facet = FALSE, lab.wrap.len = 12, remove.grid = FALSE, remove.legend = FALSE, 
  plot.margin = c(5.5, 5.5, 5.5, 5.5)){
  
  
  
  label_N <- function(x){
    y =  annotate.pos  
    ymin= y 
    ymax = y 
    label = paste0("(N = ", sum(!is.na(x)), ")") 
    return(data.frame(y = y, ymin = ymin, ymax = ymax, label = label))
  }
  
  label_val <- function(x){
    y =  annotate.pos + 0.05  
    ymin= y 
    ymax = y 
    label = round(sum(x, na.rm = TRUE)/sum(!is.na(x)),2)
    return(data.frame(y = y, ymin = ymin, ymax = ymax, label = label))
  }
  
  treat <- enquo(treat)
  outcome <- enquo(outcome)
  
  
  if (!is.null(fill.palette)){
    p <- ggplot(data, aes(x = !! treat, y = !! outcome, fill = !! treat, col = !! treat)) + stat_summary(fun.data = mean_se, geom = "bar")
  } else {
    p <- ggplot(data, aes(x = !! treat, y = !! outcome))  + stat_summary(fun.data = mean_se, geom = "bar", col = "black", alpha = 0.8, fill = "grey80")
  }
  
  p <- p+  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1, col = "black", fun.args = list(mult = 1.96)) +
    stat_summary(fun.data = label_N, geom = "text", vjust = 0, col = "black") +
    stat_summary(fun.data = label_val, geom = "text", vjust = 0, col = "black") +
    coord_cartesian(ylim = c(ylim.min, ylim.max)) + 
    ylab(ylab) + 
    xlab(xlab)  +
    ggtitle(title) +
    theme_bw() + 
    theme(plot.title = element_text(hjust =0.5)) +
    theme(legend.position = legend.pos) + 
    theme(axis.text.x = element_text(size = 12)) + 
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = lab.wrap.len)) + 
    theme(plot.margin = unit(plot.margin, "pt")) + 
    theme(strip.background=element_rect(fill="grey96"))
  
  #Display p- values. Stat_compare_means is uesful for faceting purposes. 
  if (p.display == TRUE){
   #p <- p + stat_compare_means(method = "t.test", aes(label = paste0("p-value = ", format(round(as.numeric(str_replace(..p.format.., "<2e-16", "0")),3), nsmall = 3))),
      #label.x = 1.25, label.y = 0.95*ylim.max, size = 3.88)
    #p <- p + stat_compare_means(method = "t.test", aes(label = paste0("p-value = ", format(as.numeric(..p.format..), nsmall = 3))), label.x = 1.25, label.y = 0.95*ylim.max, size = 3.88)
    p <- p + stat_compare_means(method = "t.test", aes(label = paste0("p-value = ", after_stat(p.format))), label.x = 1.25, label.y = 0.95*ylim.max, size = 3.88)
    #p <- p + stat_compare_means(method = "t.test", aes(label = paste0("p-value = ", str_replace(after_stat(p.format), "<2e-16", "0"))), label.x = 1.25, label.y = 0.95*ylim.max, size = 3.88)
  }

  #Remove x axis label option 
  if (x.label == FALSE){
    p <- p + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
  }
  
  #Remove gridlines option  
  if (remove.grid == TRUE){
    p <- p + theme(panel.grid.minor.x  = element_blank(), panel.grid.major.x = element_blank(),
      panel.grid.minor.y  = element_blank(), panel.grid.major.y = element_blank())
  }
  
  
  #Facet wrap option
  if (!is.null(substitute(facet.wrap))){
    facet.wrap <- enquo(facet.wrap)
    p <- p + facet_wrap(vars(!! facet.wrap), nrow = wrap.row, drop = drop.facet)
  }
  
  #Facet grid optoin 
  if (!is.null(substitute(facet.grid1))){
    facet.grid1 <- enquo(facet.grid1)
    facet.grid2 <- enquo(facet.grid2)
    p <- p + facet_grid(rows = vars(!! facet.grid1), col = vars(!! facet.grid2), drop = drop.facet)
  }
  
  #Add fill colours and remove legend if needed
  if (!is.null(fill.palette)){
    if (remove.legend == FALSE){
      p <- p + scale_fill_manual("Group", values  = fill.palette) + scale_color_manual("Group", values  = color.palette)
    } else {
      p <- p + scale_fill_manual("Group", values  = fill.palette, guide = "none") + scale_color_manual("Group", values  = color.palette, guide = "none")
    }
  } else {
    if (remove.legend == FALSE){
      p <- p + scale_fill_manual("Group") + scale_color_manual("Group")
    } else {
      p <- p + scale_fill_manual("Group", guide = "none") + scale_color_manual("Group", guide = "none")
    }
  }
  
  #Remove legend for plot_layout

  
  #Save and output figure 
  ggsave(paste0("Graphs/",filename, ".pdf"), height = height, width = width, 
    units = "cm")
  
  p
} 


# Hints Section Summary Figure  -------------------------------------------

section_summary <- function(data, section_name, title = "", filename = ""){
  
  p <- data %>% 
    filter(question_stem %in% paste0(section_name, c("_p1", "_p2", "_p3", "_p4", "_p5")), question_part_s != "") %>%
    mutate(question_number = case_when(question_number == "Question 3" ~ "Queston 3 \n (Hint second \n attempt)",
      question_number == "Question 4" ~ "Queston 4 \n (Hint first \n attempt)",
      TRUE ~ question_number)) %>%
    mutate(question_number = factor(question_number, levels = c("Question 1", "Question 2", 
      "Queston 3 \n (Hint second \n attempt)", "Queston 4 \n (Hint first \n attempt)", 
      "Question 5"))) %>%
    mutate(shape = case_when(question_number == "Queston 3 \n (Hint second \n attempt)"& question_part_s == "After second attempt"~ "Hint provided",
      question_number == "Queston 4 \n (Hint first \n attempt)"& question_part_s == "After first attempt"~ "Hint provided",
      TRUE ~ "No hint")) %>%
    mutate(title = title) %>%
    ggplot(aes(x = question_number, y = correct, col = question_part_s, shape = shape)) +
    stat_summary(fun = "mean", geom = "point", size = 2.5, position = position_dodge(0.2)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.1, fun.args=list(mult=1.96), position = position_dodge(0.2)) +  # Adding 95% CI error bars
    labs(x = "", y = "Proportion of correct answers") + 
    theme_bw() +
    coord_cartesian(ylim = c(0.45, 0.75)) + 
    theme(panel.grid.major.x = element_blank()) + 
    theme(panel.grid.minor.x = element_blank()) + 
    guides(color = guide_legend(title = NULL)) + 
    scale_color_manual(values = c(reds[2], blues[2])) + 
    scale_shape_manual("", values = c(4, 19)) + 
    facet_wrap(vars(title)) + 
    theme(plot.title = element_text(hjust = 0.5))+ 
    theme(strip.background=element_rect(fill="grey96"))
  ggsave(paste0("Graphs/", filename, ".pdf"), height = 12, width = 22, units = "cm")
  
  p
}
