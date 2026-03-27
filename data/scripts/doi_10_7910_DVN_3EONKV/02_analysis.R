########################################
#### CONJOINT ANALYSIS                 #
########################################


#This code replicates the figures included in the article. 

#INCLUDE YOUR PATH HERE
rm(list=ls())

library("rio")
library("cjoint")
library("ggthemes")
library("plyr")
library("dplyr")

#If necessary, set encoding to avoid errors
Sys.setlocale(locale = "en_US.ISO8859-1")

#Open the dataset
conj <- rio::import("conjoint-data-stacked.rds")

# FIGURE 1 -------------------------------------------------------

#FIGURE 1: The effect of different policies on the fairness of redistribution

#We implement a few changes so that the plot looks good
#we first change the names to reorder the labels
names(conj)[names(conj)=="wealth"] <- "awealth"
names(conj)[names(conj)=="wealthiest"] <- "bwealthiest"
names(conj)[names(conj)=="poorest"] <- "cpoorest"
names(conj)[names(conj)=="mobility"] <- "dmobility"
names(conj)[names(conj)=="origin"] <- "eorigin"


#we run the model
m_fair <- amce(fair ~ awealth + eorigin  + bwealthiest + cpoorest +
                 dmobility, 
               data = conj,
               respondent.id = "ID")

#we change the labels
names_at <- c("The country's wealth...", 
              "The wealthiest...", 
              "The poorest...", 
              "Social mobility",
              "People's wealth would still come from...")


#change the original function to make the plot preetier. 
text.size=11
text.color = "black"
theme_bw1 <- function(base_size = text.size, base_family = "") {
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.text.x = element_text(size = base_size*.9, colour = text.color, 
                                     hjust = .5 , vjust=1),
          axis.text.y = element_text(size = base_size, 
                                     colour = text.color, hjust = 0 , vjust=.5 ), 
          axis.ticks = element_blank(),
          axis.title.y =  element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
          plot.title = element_text(face = "bold"),legend.position = "none")}

#we plot it - Figure 1
x <- plot(m_fair, 
          plot.theme =  theme_bw1()  ,  
          attribute.names = names_at)



# FIGURE 2 --------------------------------------------------------

#The effect of different policies on the fairness of redistribution across party affiliation
#Marginal means by party identification
library("cregg")

mm_by <- cj(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
              dmobility, id = ~ID, estimate = "mm", by = ~PID,
            feature_labels = list(awealth = "The country's wealth...",
                                  eorigin = "People's wealth would still come from...",
                                  bwealthiest = "The wealthiest...",
                                  cpoorest = "The poorest...", 
                                  dmobility = "Social mobility"))

plot(mm_by, group = "PID") +
   ggplot2::geom_vline(xintercept = mean(conj$fair), linetype="dashed", size=0.3, colour="grey") +
  ggplot2::scale_colour_manual(breaks = c("Democrat", "Independent","Republican"),
                             values = c("blue", "orange", "red")) +
  ggplot2::theme(text = element_text(size=15))



# FIGURE 3 ----------------------------------------------------------

#The effect of diFFerent policies on the fairness of redistribution across income groups

#Marginal means by socioeconomic status
conj$Income <- conj$Income_group
mm_by_income <- cj(conj, fair ~ awealth + eorigin  + bwealthiest + cpoorest +
              dmobility, id = ~ID, estimate = "mm", by = ~Income,
            feature_labels = list(awealth = "The country's wealth...",
                                  eorigin = "People's wealth would still come from...",
                                  bwealthiest = "The wealthiest...",
                                  cpoorest = "The poorest...", 
                                  dmobility = "Social mobility"))

plot(mm_by_income, group = "Income") +
  geom_vline(xintercept = mean(conj$fair), linetype="dashed", size=0.3, colour="grey") +
  ggplot2::theme(text = element_text(size=15))




# FIGURE 4 ----------------------------------------------------------

#The effect of different policies on the fairness of redistribution between low- and high-income individuals

#we create two datasets: one with low-income individuals and another one with high-income individuals
conj_low <- conj[ which(conj$Income_group == "Low"),]
conj_high <- conj[ which(conj$Income_group == "High"),]

#Run the model
library("margins")
gen_model <- lm(fair ~ awealth + eorigin  + bwealthiest + cpoorest + dmobility, 
                data = conj_low)

#Run the different simulations
#1) WEALTHIEST GET WEALTHIER, POOREST GET RICHER, GENERAL WEALTH DOES NOT CHANGE
wwpw_df <- data.frame(awealth="Would stay the same",  
                      eorigin="People\'s talent", 
                      bwealthiest="Would be wealthier",
                      cpoorest="Would be wealthier ",
                      dmobility="No social mobility") 

wwpw_df <- predict(gen_model, wwpw_df,  interval = 'confidence')
wwpw_df <- data.frame(wwpw_df)
wwpw_df$sim <- c("Wealthiest would be wealthier,\npoorest would be wealthier\nand general wealth would stay the same")

#2) WEALTHIEST GET WEALTHIER, POOREST GET POORER, GENERAL WEALTH STAYS THE SAME
wwpp_df <- data.frame(awealth="Would stay the same",  
                      eorigin="People\'s talent", 
                      bwealthiest="Would be wealthier",
                      cpoorest="Would be poorer ",
                      dmobility="No social mobility") 
wwpp_df <- predict(gen_model, wwpp_df,  interval = 'confidence')
wwpp_df <- data.frame(wwpp_df)
wwpp_df$sim <- c("Wealthiest would be wealthier\npoorest would be poorer\nand general wealth would stay the same")


#3) POOREST GET RICHER, WEALTH DECREASES AND WEALTHIEST GET POORER
wwppwd_df <- data.frame(awealth="Would decrease",  
                        eorigin="People\'s talent", 
                        bwealthiest="Would be poorer",
                        cpoorest="Would be wealthier ",
                        dmobility="No social mobility") 
wwppwd_df <- predict(gen_model, wwppwd_df,  interval = 'confidence')
wwppwd_df <- data.frame(wwppwd_df)
wwppwd_df$sim <- c("Wealthiest would be poorer,\npoorest would be wealthier and\ngeneral wealth would decrease")

#4) POOREST GET RICHER, WEALTHIEST GET POORER, GENERAL WEALTH INCREASES
pwwpgwi_df <- data.frame(awealth="Would increase",  
                         eorigin="People\'s talent", 
                         bwealthiest="Would be poorer",
                         cpoorest="Would be wealthier ",
                         dmobility="No social mobility") 
pwwpgwi_df <- predict(gen_model, pwwpgwi_df,  interval = 'confidence')
pwwpgwi_df <- data.frame(pwwpgwi_df)
pwwpgwi_df$sim <- c("Wealthiest would be poorer,\npoorest would be wealthier and\ngeneral wealth would increase")


#5) WEALTHIEST GET POORER, POOREST STAY THE SAME, GENERAL WEALTH INCREASES
wppsgwi_df <- data.frame(awealth="Would increase",  
                         eorigin="People\'s talent", 
                         bwealthiest="Would be poorer",
                         cpoorest="Would keep their status ",
                         dmobility="No social mobility") 
wppsgwi_df <- predict(gen_model, wppsgwi_df,  interval = 'confidence')
wppsgwi_df <- data.frame(wppsgwi_df)
wppsgwi_df$sim <- c("Wealthiest keep their status,\npoorest would be wealthier and\ngeneral wealth would increase")


df_poor <- rbind(wwpp_df,wwppwd_df,wwpw_df,pwwpgwi_df,wppsgwi_df)

gen_model_high <- lm(fair ~ awealth + eorigin  + bwealthiest + cpoorest + dmobility, 
                     data = conj_high)

#1) WEALTHIEST GET WEALTHIER, POOREST GET RICHER, GENERAL WEALTH DOES NOT CHANGE
wwpw_df <- data.frame(awealth="Would stay the same",  
                      eorigin="People\'s talent", 
                      bwealthiest="Would be wealthier",
                      cpoorest="Would be wealthier ",
                      dmobility="No social mobility") 

wwpw_df <- predict(gen_model_high, wwpw_df,  interval = 'confidence')
wwpw_df <- data.frame(wwpw_df)
wwpw_df$sim <- c("Wealthiest would be wealthier,\npoorest would be wealthier\nand general wealth would stay the same")

#2) WEALTHIEST GET WEALTHIER, POOREST GET POORER, GENERAL WEALTH STAYS THE SAME
wwpp_df <- data.frame(awealth="Would stay the same",  
                      eorigin="People\'s talent", 
                      bwealthiest="Would be wealthier",
                      cpoorest="Would be poorer ",
                      dmobility="No social mobility") 
wwpp_df <- predict(gen_model_high, wwpp_df,  interval = 'confidence')
wwpp_df <- data.frame(wwpp_df)
wwpp_df$sim <- c("Wealthiest would be wealthier\npoorest would be poorer\nand general wealth would stay the same")


#3) POOREST GET RICHER, WEALTH DECREASES AND WEALTHIEST GET POORER
wwppwd_df <- data.frame(awealth="Would decrease",  
                        eorigin="People\'s talent", 
                        bwealthiest="Would be poorer",
                        cpoorest="Would be wealthier ",
                        dmobility="No social mobility") 
wwppwd_df <- predict(gen_model_high, wwppwd_df,  interval = 'confidence')
wwppwd_df <- data.frame(wwppwd_df)
wwppwd_df$sim <- c("Wealthiest would be poorer,\npoorest would be wealthier and\ngeneral wealth would decrease")

#4) POOREST GET RICHER, WEALTHIEST GET POORER, GENERAL WEALTH INCREASES
pwwpgwi_df <- data.frame(awealth="Would increase",  
                         eorigin="People\'s talent", 
                         bwealthiest="Would be poorer",
                         cpoorest="Would be wealthier ",
                         dmobility="No social mobility") 
pwwpgwi_df <- predict(gen_model_high, pwwpgwi_df,  interval = 'confidence')
pwwpgwi_df <- data.frame(pwwpgwi_df)
pwwpgwi_df$sim <- c("Wealthiest would be poorer,\npoorest would be wealthier and\ngeneral wealth would increase")

#5) WEALTHIEST GET POORER, POOREST STAY THE SAME, GENERAL WEALTH INCREASES
wppsgwi_df <- data.frame(awealth="Would increase",  
                         eorigin="People\'s talent", 
                         bwealthiest="Would be poorer",
                         cpoorest="Would keep their status ",
                         dmobility="No social mobility") 
wppsgwi_df <- predict(gen_model_high, wppsgwi_df,  interval = 'confidence')
wppsgwi_df <- data.frame(wppsgwi_df)
wppsgwi_df$sim <- c("Wealthiest keep their status,\npoorest would be wealthier and\ngeneral wealth would increase")

df_rich <- rbind(wwpp_df,wwppwd_df,wwpw_df,pwwpgwi_df,wppsgwi_df)

#Bring datasets together
df_poor$income <- c("Low")
df_rich$income <- c("High")

df_plot <- rbind(df_poor, df_rich)

#Plot
df_plot$sim <- as.factor(df_plot$sim)


df_plot$sim2 <- factor(df_plot$sim, 
                       levels = c("Wealthiest would be poorer,\npoorest would be wealthier and\ngeneral wealth would increase",
                                  "Wealthiest would be wealthier,\npoorest would be wealthier\nand general wealth would stay the same", 
                                  "Wealthiest would be poorer,\npoorest would be wealthier and\ngeneral wealth would decrease", 
                                  "Wealthiest keep their status,\npoorest would be wealthier and\ngeneral wealth would increase", 
                                  "Wealthiest would be wealthier\npoorest would be poorer\nand general wealth would stay the same",
                                  ordered=TRUE))



ggplot(df_plot) +
  geom_pointrange(aes(x = sim2, y = fit, ymin = lwr, ymax = upr, group=income,
                      linetype=income),  
                  lwd = 1/2, position = position_dodge(.3),
                  shape = 21, fill = "WHITE", stat = "identity") +
  ylab(("Coefficient + 95% CI \n Perceived fairness" )) +
  coord_flip() + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        axis.text=element_text(size=11),
        axis.title=element_text(size=12)) +
  xlab("") +
  annotate("text", x = 1.2, y = 7.2, label = "Low-income") +
  annotate("text", x = 1.1, y = 6, label = "High-income") 


