#-----------------------------------------------------------------------------------------------------
# Author: Kailash Rajah
#-----------------------------------------------------------------------------------------------------

# Prepare workspace and load datasets -----------------------------------------------------------------

  rm(list = ls())
  library("readxl")
  library("tidyr")
  library("dplyr")
  library("haven")
  library("ggplot2")
  library("gridExtra")
  library("sandwich")
  library("lmtest")
  library("broom")
  library("mice")
  library("miceadds")
  library("plotrix")
  library("stringr")
  library("rlang")
  library("ggpubr")
  library("patchwork")
  library("RColorBrewer")
  library("png")
  library("figpatch")
  library("magick")
  setwd("C:/Users/kaila/Dropbox (MIT)/Math Projects/Market_Math/7_Dataverse/Code & Output")
  source("Analysis/functions.R")

# Study 1: Kolkata figures  --------------------------------------------------------
  
  #Create base plots 
  s1_market_ylab                    <- as_factor(read_dta("Graphs/market_data.dta")) %>% filter(group == 1) %>% StackedBar()
  s1_market_ylab_legbottom          <- as_factor(read_dta("Graphs/market_data.dta")) %>% filter(group == 1) %>% StackedBar(legend.pos = "bottom")
  s1_hypothetical_noylab            <- as_factor(read_dta("Graphs/hypothetical_data_kolkata.dta")) %>% filter(group == 1, bar != 3) %>% StackedBar(ylab = "") 
  s1_hypothetical_ylab_legbottom    <- as_factor(read_dta("Graphs/hypothetical_data_kolkata.dta")) %>% filter(group == 1, bar != 3) %>% StackedBar(legend.pos = "bottom") 
  s1_hypothetical_ylab              <- as_factor(read_dta("Graphs/hypothetical_data_kolkata.dta")) %>% filter(group == 1, bar != 3) %>% StackedBar() 
  s1_aser                           <- as_factor(read_dta("Graphs/aser_data.dta")) %>% filter(group == 1) %>% mutate(level = factor(level)) %>%  AserBar()
  s1_aser_facet                     <- as_factor(read_dta("Graphs/aser_data.dta")) %>% filter(group == 1) %>% mutate(level = factor(level)) %>%  AserBar(facet = TRUE)
  s1_absanc                         <- as_factor(read_dta("Graphs/abstract_anchored_data.dta")) %>% filter(group == 1) %>% StackedBar(facet = FALSE, x_labels = c("Anchored", "Abstract"), xlab = "", legend.pos = "bottom")
  s1_absanc_facet_noylab            <- as_factor(read_dta("Graphs/abstract_anchored_data.dta")) %>% filter(group == 1) %>% StackedBar(x_labels = c("Anchored", "Abstract"), xlab = "", ylab = "", legend.pos = "bottom")
  s2_calc_method                    <- readPNG("Graphs/aser_example.png", native = TRUE)  
  
  #Combine plots
  design <- "
   1111111111111122222222222222
   3333333333333344444444444444
   5555555555555555555555555555
   "    
  fig1 <-  s1_market_ylab + s1_hypothetical_noylab + s1_aser_facet  + s2_calc_method + guide_area() + plot_layout(heights = c(1,1,0.1), design=design, guides = "collect") &
    theme(legend.position = 'bottom') &  plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')') &  theme(plot.tag.position  = c(0.02, 1)) 
  fig1 

  #Output plots 
  ggsave("Graphs/fig1.pdf",         fig1,                            height = 18, width = 20,   units = "cm")

    
# Study 2: Delhi figures  --------------------------------------------------------
  
  #Create base plots 
  s2a_market_ylab                   <- as_factor(read_dta("Graphs/market_data.dta")) %>% filter(group == 2) %>% StackedBar()
  s2a_market_facet_ylab             <- as_factor(read_dta("Graphs/market_data.dta")) %>% filter(group == 2) %>% mutate(figure = "Working children") %>%  StackedBar(remove.right = TRUE, xlab = "")
  s2a_market_ylab_legbottom         <- as_factor(read_dta("Graphs/market_data.dta")) %>% filter(group == 2) %>% StackedBar(legend.pos = "bottom")
  s2b_market_facet_ylab             <- as_factor(read_dta("Graphs/market_data.dta")) %>% filter(group == 4) %>% mutate(figure = "Non-working children") %>% StackedBar()
  s2b_market_facet_noy              <- as_factor(read_dta("Graphs/market_data.dta")) %>% filter(group == 4) %>% mutate(figure = "Non-working children") %>% StackedBar(ylab = "", xlab = "", remove.left = TRUE)
  s2a_hypothetical_noylab           <- as_factor(read_dta("Graphs/hypothetical_data_delhi.dta")) %>% filter(group == 2, bar != 6) %>% StackedBar(ylab = "")
  s2a_hypothetical_ylab_legbottom   <- as_factor(read_dta("Graphs/hypothetical_data_delhi.dta")) %>% filter(group == 2, bar != 6) %>% StackedBar(legend.pos = "bottom")
  s2a_hypothetical_ylab             <- as_factor(read_dta("Graphs/hypothetical_data_delhi.dta")) %>% filter(group == 2) %>% mutate(figure = "Working children")     %>% StackedBar(x_labels = c("1", "2", "3", "4", "5", "Average"), xlab = "", text.size = 2.3, remove.right = TRUE)
  s2b_hypothetical_ylab             <- as_factor(read_dta("Graphs/hypothetical_data_delhi.dta")) %>% filter(group == 4) %>% mutate(figure = "Non-working children") %>% StackedBar(x_labels = c("1", "2", "3", "4", "5", "Average"), text.size = 2.3)
  s2b_hypothetical_noy              <- as_factor(read_dta("Graphs/hypothetical_data_delhi.dta")) %>% filter(group == 4) %>% mutate(figure = "Non-working children") %>% StackedBar(x_labels = c("1", "2", "3", "4", "5", "Average"), xlab = "", text.size = 2.3, remove.left = TRUE, ylab  = "" )
  s2a_aser                          <- as_factor(read_dta("Graphs/aser_data.dta")) %>% filter(group == 2) %>% mutate(level = factor(level)) %>%  AserBar(facet = TRUE)
  s2a_aser_facet                    <- as_factor(read_dta("Graphs/aser_data.dta")) %>% filter(group == 2) %>% mutate(level = factor(level)) %>%  mutate(figure = "Working children") %>% AserBar(facet = TRUE, ylim = 1.01, remove.right = TRUE)
  s2b_aser_facet                    <- as_factor(read_dta("Graphs/aser_data.dta")) %>% filter(group == 4) %>% mutate(level = factor(level)) %>%  mutate(figure = "Non-working children") %>% AserBar(facet = TRUE, ylim = 1.01, ylab = "", remove.left = TRUE)
  s2a_ancabs                        <- as_factor(read_dta("Graphs/abstract_anchored_data.dta")) %>% filter(group == 2) %>% StackedBar(facet = TRUE, x_labels = c("Anchored", "Abstract"), xlab = "")
  pres_fig6                         <- as_factor(read_dta("Graphs/roundable_data.dta")) %>% filter(group == 2, bar %in% c(1,2)) %>% StackedBar(x_labels = c("Non-Roundable", "Roundable"), xlab = "", legend.pos = "bottom")
  s2a_roundable                     <- as_factor(read_dta("Graphs/roundable_data.dta")) %>% filter(group == 2, bar %in% c(1,2)) %>% mutate(figure = "Working children") %>% StackedBar(facet = TRUE, x_labels = c("Non-Roundable", "Roundable"), xlab = "")
  s2a_roundable                     <- as_factor(read_dta("Graphs/roundable_data.dta")) %>% filter(group == 4, bar %in% c(1,2)) %>% mutate(figure = "Non-working children") %>% StackedBar(facet = TRUE, x_labels = c("Non-Roundable", "Roundable"), xlab = "")
  s2a_solutions                     <- as_factor(read_dta("Graphs/solutions_data.dta")) %>% filter(group == 2) %>% mutate(figure = "Working children", type = factor(type)) %>% AserBar(facet = TRUE, xvar = type, yvar = num, ylim = 130,  x_labels = c("Numbers", "Operations"), text.size = 3.5, annotate.pos = 0.06, annotate.gap = 0.05, ylab = "Number of times written on paper", remove.right = TRUE)
  s2b_solutions                     <- as_factor(read_dta("Graphs/solutions_data.dta")) %>% filter(group == 4) %>% mutate(figure = "Non-working children", type = factor(type)) %>% AserBar(facet = TRUE, xvar = type, yvar = num, ylim = 130, x_labels = c("Numbers", "Operations"),text.size = 3.5,  annotate.pos = 0.05, ylab = "", remove.left = TRUE)
  s2a_pen                           <- as_factor(read_dta("Graphs/pen_data.dta")) %>% filter(group == 2) %>% mutate(figure = "Working children", type = factor(type)) %>% AserBar(facet = TRUE, xvar = type, yvar = usepen,  x_labels = c("Market", "Hypothetical", "Anchored\nroundable", "Abstract\nroundable", "Written"),    text.size = 3.5, ylab = "Proportion using pen and paper", remove.right = TRUE)
  s2b_pen                           <- as_factor(read_dta("Graphs/pen_data.dta")) %>% filter(group == 4) %>% mutate(figure = "Non-working children", type = factor(type)) %>% AserBar(facet = TRUE, xvar = type, yvar = usepen, x_labels = c("Market", "Hypothetical", "Anchored\nroundable", "Abstract\nroundable", "Written"), text.size = 3.5, ylab = "", remove.left = TRUE)
  s2_calc_method                    <- fig("Graphs/calc_method.png")  
  s2_calc_method1                   <- fig("Graphs/calc_method1.png")  
  s2_calc_method2                   <- fig("Graphs/calc_method2.png")  
  s2_calc_method3                   <- fig("Graphs/calc_method3.png")  
  s2_calc_method_half               <- fig("Graphs/calc_method_half.png")  
  
    
  #Combine plots 
  ext1                              <- (s2_calc_method1) / (s2_calc_method2) / (s2_calc_method3) + plot_layout(heights = c(1,1,1.5))  & plot_annotation(tag_levels = list(c("(a)", "", "(b)", "", "(c)"))) &  theme(plot.tag.position  = c(0.02, 1)) 
  
  #Output plots
  fig2                              <- (s2a_market_facet_ylab + s2b_market_facet_noy + plot_spacer() + plot_spacer() + s2a_hypothetical_ylab +  s2b_hypothetical_noy + plot_spacer() + plot_spacer() + 
                                        s2a_aser_facet +  s2b_aser_facet) + plot_spacer() + plot_spacer() +
                                        plot_layout(guides = "collect", ncol = 2, heights = c(15, -3.5, 15 , -3.5, 15, -3)) & theme(legend.position = 'right') & 
                                        plot_annotation(tag_levels = list(c("(a)", "", "(b)", "", "(c)", ""))) &  
                                        theme(plot.tag.position  = c(0.02, 1))  
  ggsave("Graphs/fig2.pdf", fig2, height = 21.25, width = 22.5, units = "cm")
  
  
  design <-
    "
    AAAABBBBEEEEE
    FFFFFFFFEEEEE
    CCCCDDDDEEEEE
    "
    fig3 <- list(s2a_pen, s2b_pen, s2a_solutions,  s2b_solutions, s2_calc_method_half, plot_spacer()) |> wrap_plots(design = design, heights = c(5, -0.9, 5)) &
            plot_annotation(tag_levels = list(c("(a)", "", "(b)", "", "(c)"))) &  theme(plot.tag.position  = c(0.02, 1)) & theme(legend.position = 'right')
  ggsave("Graphs/fig3.pdf", fig3, height = 19.5, width = 32.5,units = "cm")
  
  
  
# Study 3 -----------------------------------------------------------------
  
  #Figure 4: Vishal vs abstract comparison
  df <- as_factor(read_dta("../../3_Data_processing/7_Combined (Study 3)/clean/01_appended_data.dta"))
  p1 <- df %>% filter(no_market_selling == 1 | market == "Working child") %>%  
    filter(append_question %in% c( "Market word problem", "Market word problem with hint", "Oral abstract division", "Oral abstract subtraction")) %>%
    mutate(append_question = recode(append_question,  "Oral abstract division" = "Abstract", "Oral abstract subtraction"  = "Abstract")) %>% 
    mutate(append_question = factor(append_question, levels = c("Abstract", "Market word problem", "Market word problem with hint"))) %>%
    BarPlotFacet(
      filename      = "fig4", 
      treat         = school, 
      outcome       = correct, 
      facet.wrap    = append_question, 
      width         = 21, 
      height        = 14, 
      ylim.max      = 1, 
      ylab          = "Proportion of correct answers", 
      legend.pos    = "none", 
      fill.palette  = c(reds[4], blues[4]), 
      color.palette = c("black", "black"), 
      p.display     = FALSE, 
      remove.grid   = TRUE, 
      annotate.pos  = 0.11)
  
  ann_text <- data.frame(school = "Working child", correct = 0.95, lab = c("p-value < 0.001"), append_question = factor(c("Abstract", "Market word problem", "Market word problem with hint"), levels = c("Abstract", "Market word problem", "Market word problem with hint")))
  p1 <- p1 + geom_text(data = ann_text, aes(label = lab), hjust = 0)
  ggsave("Graphs/fig4.pdf", p1, width = 21, height = 14, units = "cm") 
  
  
  #Figure 5: Abstract anchored and roundable non-roundable 
  #Anchored plot 
  df <- as_factor(read_dta("../../3_Data_processing/7_Combined (Study 3)/clean/01_appended_wave12_data.dta"))
  p1 <- df %>%
        filter(no_market_selling == 1 | market == "Working child") %>%
        filter(append_question %in% c( "Oral abstract addition", "Oral abstract multiplication", "Oral anchored addition", "Oral anchored multiplication")) %>%
        mutate(append_question = recode(append_question,  "Oral abstract addition" = "Abstract", "Oral abstract multiplication"  = "Abstract", "Oral anchored addition" = "Anchored", "Oral anchored multiplication" = "Anchored")) %>%
        mutate(append_question = factor(append_question, levels = c("Abstract","Anchored"))) %>%
        BarPlotFacet(
          filename = "fig5_absanc_add_mult",
          treat = append_question, 
          outcome = correct,
          facet.wrap =  school,
          width = 20, 
          height = 14,  
          ylim.max = 1, 
          legend.pos = "none", 
          ylab = "Proportion of correct answers",
          fill.palette = c(reds[4], blues[4]), 
          color.palette = c("black", "black"), 
          p.display = FALSE, 
          remove.grid = TRUE)
  
    ann_text <- data.frame(append_question = "Abstract", correct = 0.95, lab = c("p-value = 0.001", "p-value = 0.028"), school = factor(c("Working child", "Non-working child"), levels = c("Working child", "Non-working child")))
    p1 <- p1 + geom_text(data = ann_text, aes(label = lab), hjust = -0.25)
  
  
  p2 <- df %>% filter(no_market_selling == 1 | market == "Working child") %>%  
        filter(append_question %in% c("Oral abstract addition",     "Oral abstract subtraction",   "Oral abstract multiplication", "Oral anchored addition",    "Oral anchored multiplication")) %>%
        BarPlotFacet(
          filename      = "fig5_roudable_all_comparable", 
          treat         = roundable, 
          outcome       = correct, 
          facet.wrap    = school, 
          width         = 21, 
          height        = 14, 
          ylim.max      = 1, 
          ylab          = "Proportion of correct answers", 
          legend.pos    = "none", 
          fill.palette = c(reds[4], blues[4]), 
          color.palette = c("black", "black"), 
          p.display     = FALSE, 
          remove.grid   = TRUE)
  
   ann_text <- data.frame(roundable = "Non-roundable", correct = 0.95, lab = c("p-value = 0.083", "p-value = 0.870"), school = factor(c("Working child", "Non-working child"), levels = c("Working child", "Non-working child")))
   p2 <- p2 + geom_text(data = ann_text, aes(label = lab), hjust = -0.25)
  
  fig5 <- p1 + plot_spacer() + p2 + plot_layout(ncol= 1, heights = c(10,-1.2,10)) & plot_annotation(tag_levels = list(c("(a)", "(b)", "(b)"))) &  theme(plot.tag.position  = c(0.02, 1))
  ggsave("Graphs/fig5.pdf", fig5, width = 22.5, height = 21.25, units = "cm") 
  
  # Figure 6: Hints 
  df <- as_factor(read_dta("../../3_Data_processing/5_Market children (Study 3a - Wave 2)/clean/3_long_data.dta"))
  p1 <- section_summary(df, section = "d", filename = "figs3_anchoring", title = "Anchoring hints")
  p2 <- section_summary(df, section = "e", filename = "figs3_rounding", title = "Rounding hints")
  p3 <- df %>% filter(!is.na(e_vishal_hint), subsection == "vi_aft" ) %>% mutate(title = "Process hints") %>%
        BarPlotFacet(
          filename = "figs3_e_vishal_hint", 
          outcome = correct,
          treat = e_vishal_hint, 
          legend.pos = "none",
          ylab = "Proportion of correct answers",
          plot.margin = c(5.5, 20, 5.5, 20), 
          facet.wrap = title,
          fill.palette = c(reds[4], blues[4]), 
          color.palette = c("black", "black"), 
          p.display = TRUE,
          remove.grid = TRUE, 
          remove.legend = TRUE)
  
  design <-
    "
    AAAAAAA
    BBBBBBB
    #CCCCC#
    "
  figs3 <- list(p1, p2, p3) |> wrap_plots(design = design, heights = c(1,1,1.5)) & plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)"))) &  theme(plot.tag.position  = c(0.02, 1)) & theme(legend.position = 'right') 
  ggsave("Graphs/figs3.pdf", figs3, width = 20, height = 30, units = "cm") 
  



