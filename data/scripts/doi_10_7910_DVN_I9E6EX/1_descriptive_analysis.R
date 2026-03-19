##############################
#
# Replication file for the descriptive analysis in:
#
# Consolidating Progress:
# The Selection of Female Ministers in Autocracies and Democracies
#
# For publication in the the American Political Science Review
#
# Jacob Nyrup, Hikaru Yamagishi, & Stuart Bramwell
# 
##################

###################
## Load packages ##
###################

pacman::p_load(tidyverse,scales)

###############
## Load data ##
###############

df_cross <- read.csv("../1_data/df_consolidatingprogress_V1.csv") %>% 
            mutate(share_female = share_female/100,
            importance_weight = importance_weight/100)

################
### Figure 1 ###
################

# Plot 1.1 - Autocracy v. democracy

df_figure_1 <- df_cross %>% 
                      mutate(democracy_bmr = as.factor(democracy_bmr)) %>% 
                      filter(!is.na(democracy_bmr)) %>%
                      group_by(year,democracy_bmr) %>% 
                      summarize(share_female = mean(share_female,na.rm=TRUE)
                      )

figure1_1 <- ggplot(data=df_figure_1, aes(x=year, y=value, group=democracy_bmr)) +
                 geom_line(aes(y=share_female, color=democracy_bmr),size=1.1,show.legend=TRUE) +
                 theme_bw() + 
                 theme(panel.border = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.line = element_line(colour = "black"),
                       legend.position = "none",
                       legend.title = element_blank(),
                       plot.title = element_text(size=9),
                       axis.title.x = element_text(size=10),
                       axis.title.y = element_text(size=10),
                       axis.text.x=element_text(colour="black"),
                       axis.text.y=element_text(colour="black")) +
                 scale_x_continuous(breaks=seq(1970,2021,10)) + 
                 scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits = c(0,0.30)) +
                 ylab("") +
                 xlab("") +
                 annotate("text", x = 2018, y = 0.29, label = "Democracies",size=3) + 
                 annotate("text", x = 2019, y = 0.18, label = "Autocracies",size=3) +
                 labs(title = "% women in cabinet") +
                 scale_color_manual(values=c("#CB2314","#273046"),guide = FALSE)
  
# Plot 1.2 - Difference between democracy and autocracy

df_figure_2 <- df_figure_1 %>% 
                    pivot_wider(names_from=democracy_bmr,values_from=c(share_female)) %>% 
                    mutate(share_female_dif = `1`-`0`)

figure1_2 <- ggplot(data=df_figure_2, aes(x=year, y=value)) +
           geom_line(aes(y=share_female_dif, linetype="solid"),size=1.1,show.legend=TRUE, color = "#273046") +
           theme_bw() + 
           theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                legend.position = "",
                legend.title = element_blank(),
                plot.title = element_text(size=9),
                axis.title.x = element_text(size=10),
                axis.title.y = element_text(size=10),
                axis.text.x=element_text(colour="black"),
                axis.text.y=element_text(colour="black")) +
           scale_x_continuous(breaks=seq(1970,2021,10)) + 
           scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits = c(0,0.15)) +
           ylab("") +
           xlab("") +
           labs(title = "Difference in % women in cabinet (democracies - autocracies)")

# Plot 1.3 - Different types of autocracy

df_figure1_3 <- df_cross %>% 
                   filter(!is.na(electoral)) %>% 
                   mutate(electoral = case_when(electoral %in% c(0, 1) ~ "Closed Autocracy",
                               electoral == 2 ~ "Electoral Autocracy",
                               electoral == 3 ~ "Electoral Democracy"),
                   electoral = fct_inorder(electoral)) %>% 
                   group_by(electoral, year) %>% 
                   summarise(share_female = mean(share_female, na.rm = TRUE)) %>% 
                   ungroup() %>% 
                   mutate(electoral = fct_reorder(electoral, share_female))

figure1_3 <- ggplot(df_figure1_3,aes(x = year, y = share_female, colour = electoral)) +
          geom_line(size = 1) +
          theme_bw() + 
          theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "",
                legend.title = element_blank(),
                plot.title = element_text(size=9),
                axis.title.x = element_text(size=10),
                axis.title.y = element_text(size=10),
                axis.text.x=element_text(colour="black"),
                axis.text.y=element_text(colour="black")) +
          scale_x_continuous(breaks=seq(1970,2021,10)) + 
          scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits = c(0,0.30)) +
          scale_color_manual(values=c("#CB2314","#273046","#354823"),
                             guide = guide_legend(reverse = TRUE)) +
          labs(title = "% women in cabinet", x = "", y = NULL) +
          annotate("text", x = 2020, y = 0.29, label = "Electoral\ndemocracy",size=3) + 
          annotate("text", x = 2020.5, y = 0.14, label = "Electoral\nautocracy",size=3) +
          annotate("text", x = 2020, y = 0.07, label = "Closed\nautocracy",size=3) 

# Print figure ---
ggsave(
  "../3_output/figure1.pdf",
  gridExtra::grid.arrange(figure1_1, figure1_2, figure1_3),
  width = 6,
  height = 9,
  dpi = 1200
)

################
### Figure 2 ###
################

iso_vector <- c("ARG","BGR","BRA","CHL","ESP",
                "GHA","IDN","KEN","KOR","MEX",
                "MNG","MWI","NPL","PHL","POL",
                "PRT","ROU","SEN","TUN","TWN")

df_figure2 <- df_cross %>% 
            filter(country_isocode %in% iso_vector) %>% 
            select(country_name, year, share_female, poly, democracy_stock_poly_95) %>% 
            pivot_longer(-c(country_name, year)) %>% 
            mutate(name = case_when(name == "share_female" ~ "Proportion of women in cabinet",
                          name == "poly" ~ "Polyarchy rating",
                          TRUE ~ "Stock of Polyarchy (95%)"),
            name = fct_relevel(name,c("Proportion of women in cabinet","Polyarchy rating","Stock of Polyarchy (95%)")))


figure2 <- df_figure2 %>%
            ggplot(aes(x = year, y = value, colour = name, linetype = name)) +
            geom_line(size = 1) +
            theme_minimal() +
            theme(panel.border = element_blank(), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black"),
            legend.title = element_blank(),
            legend.position = "bottom",
            plot.title = element_text(size=12),
            plot.subtitle = element_text(size=10),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            axis.text.x=element_text(colour="black"),
            axis.text.y=element_text(colour="black")
            ) +
            facet_wrap(. ~ country_name, ncol = 4) +
            scale_x_continuous(breaks = c(1970, 1990, 2010)) +
            scale_y_continuous(breaks=c(0,0.5,1),limits = c(0,1)) +
            scale_color_manual(values=c("#273046","#FAD510","#CB2314")) +
            scale_linetype_manual(values=c(4,2,1)) +
            labs(x = "", y = "Proportion/Polyarchy score", colour = "", linetype = "")

# Print figure ---

ggsave(
       "../3_output/figure2.pdf",
       figure2,
       width = 7,
       height = 6,
       dpi = 1200)

