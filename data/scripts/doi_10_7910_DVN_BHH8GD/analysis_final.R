#This is the analysis file to produce all results in the main text and SI
rm(list=ls())
library(tidyverse)
library(rstanarm)
library(brms)
library(ggrepel)
library(sf)
library(ggmap)
library(grid)
library(gt)
library(gtExtras)
library(ggpubr)

setwd("") #set WD here
options(mc.cores = 4)
upper <- function(x){quantile(x, .9)}
lower <- function(x){quantile(x, .1)}

#Prep data----
df <- readRDS("Data/state_year_shootings_cleaned.rds") %>% 
  filter(!is.na(shall_issue)) %>% #remove DC 
  transmute(state_abb, year, race_eth = factor(race_eth, levels = c("White", "Black", "Hispanic")), n_shootings, pop = round(pop), State, hfr) 

pop_size <- df %>% group_by(race_eth) %>% summarise(pop = sum(pop)/6)

#Fit Models----
##M1: Varying slope----
M1 <- brm(n_shootings ~ race_eth + (1 + race_eth | state_abb) + (1 | year) + 
           offset(log(pop)),
         data = df, sample_prior = T,
         family = negbinomial(link = "log", link_shape = "log"),
         seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
         prior = c(prior_string("normal(0,5)", class = "b"),
                     prior_string("normal(0,10)", class = "Intercept"),
                   prior_string("exponential(.02)", class = "shape")))
loo_M1 <- loo(M1)
#launch_shinystan(M1)

##M2: Add Firearm Ownership----
M2 <- brm(n_shootings ~ race_eth*hfr + (1 + race_eth | state_abb) + (1 | year) + 
            offset(log(pop)),
          data = df, sample_prior = T,
          family = negbinomial(link = "log", link_shape = "log"),
          seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
          prior = c(prior_string("normal(0,5)", class = "b"),
                    prior_string("normal(0,10)", class = "Intercept"),
                    prior_string("exponential(.02)", class = "shape")))
loo_M2 <- loo(M2)
#launch_shinystan(M2)

#Table 1----
t1 <- df %>% group_by(race_eth) %>% summarise(shootings = sum(n_shootings), 
                                             pop = sum(pop)/6, shoot_rate = sum(n_shootings)/pop*100000)

t1$rd <- round(t1$shoot_rate-t1$shoot_rate[t1$race_eth=="White"],2)
t1$rr <- round(t1$shoot_rate/t1$shoot_rate[t1$race_eth=="White"],2)
t1$shoot_rate <- round(t1$shoot_rate,2)
t1 <- t1 %>% select(-pop, -shootings)

write_csv(t1, file = "Output/table1.csv")

#Table S3----
#M1:
y_pred <- t(posterior_predict(M1, draws=5000, seed=3233)) 
y_bar_w <- colSums(y_pred[df$race_eth== "White",])
y_bar_b <- colSums(y_pred[df$race_eth== "Black",])
y_bar_h <- colSums(y_pred[df$race_eth== "Hispanic",])

format_nums <- function(x, lb, ub) {paste0(round(x, 2), " (", round(lb, 2), ", ", round(ub, 2), ")")}
w <- median(y_bar_w)/pop_size$pop[pop_size$race_eth=="White"]*100000
w_lb <- lower(y_bar_w)/pop_size$pop[pop_size$race_eth=="White"]*100000
w_ub <- upper(y_bar_w)/pop_size$pop[pop_size$race_eth=="White"]*100000
w2 <- format_nums(w, w_lb, w_ub)

b <- median(y_bar_b)/pop_size$pop[pop_size$race_eth=="Black"]*100000
b_lb <- lower(y_bar_b)/pop_size$pop[pop_size$race_eth=="Black"]*100000
b_ub <- upper(y_bar_b)/pop_size$pop[pop_size$race_eth=="Black"]*100000
b2 <- format_nums(b, b_lb, b_ub)

h <- median(y_bar_h)/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000
h_lb <- lower(y_bar_h)/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000
h_ub <- upper(y_bar_h)/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000
h2 <- format_nums(h, h_lb, h_ub)

bw_rd <- median((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rd_lb <- lower((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rd_ub <- upper((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rd2 <- format_nums(bw_rd, bw_rd_lb, bw_rd_ub)

hw_rd <- median((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rd_lb <- lower((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rd_ub <- upper((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rd2 <- format_nums(hw_rd, hw_rd_lb, hw_rd_ub)

bw_rr <- median((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rr_lb <- lower((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rr_ub <- upper((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rr2 <- format_nums(bw_rr, bw_rr_lb, bw_rr_ub)

hw_rr <- median((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rr_lb <- lower((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rr_ub <- upper((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rr2 <- format_nums(hw_rr, hw_rr_lb, hw_rr_ub)

tab_dat_M1 <- data.frame(race_eth = c("White", "Black", "Hispanic"), 
                         shoot_rate = c(w2, b2, h2),
                         rd = c(NA, bw_rd2, hw_rd2),
                         rr = c(NA, bw_rr2, hw_rr2))

#M2:
y_pred <- t(posterior_predict(M2, draws=5000, seed=3233)) 
y_bar_w <- colSums(y_pred[df$race_eth== "White",])
y_bar_b <- colSums(y_pred[df$race_eth== "Black",])
y_bar_h <- colSums(y_pred[df$race_eth== "Hispanic",])

w <- median(y_bar_w)/pop_size$pop[pop_size$race_eth=="White"]*100000
w_lb <- lower(y_bar_w)/pop_size$pop[pop_size$race_eth=="White"]*100000
w_ub <- upper(y_bar_w)/pop_size$pop[pop_size$race_eth=="White"]*100000
w2 <- format_nums(w, w_lb, w_ub)

b <- median(y_bar_b)/pop_size$pop[pop_size$race_eth=="Black"]*100000
b_lb <- lower(y_bar_b)/pop_size$pop[pop_size$race_eth=="Black"]*100000
b_ub <- upper(y_bar_b)/pop_size$pop[pop_size$race_eth=="Black"]*100000
b2 <- format_nums(b, b_lb, b_ub)

h <- median(y_bar_h)/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000
h_lb <- lower(y_bar_h)/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000
h_ub <- upper(y_bar_h)/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000
h2 <- format_nums(h, h_lb, h_ub)

bw_rd <- median((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rd_lb <- lower((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rd_ub <- upper((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rd2 <- format_nums(bw_rd, bw_rd_lb, bw_rd_ub)

hw_rd <- median((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rd_lb <- lower((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rd_ub <- upper((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) - (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rd2 <- format_nums(hw_rd, hw_rd_lb, hw_rd_ub)

bw_rr <- median((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rr_lb <- lower((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rr_ub <- upper((y_bar_b/pop_size$pop[pop_size$race_eth=="Black"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
bw_rr2 <- format_nums(bw_rr, bw_rr_lb, bw_rr_ub)

hw_rr <- median((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rr_lb <- lower((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rr_ub <- upper((y_bar_h/pop_size$pop[pop_size$race_eth=="Hispanic"]*100000) / (y_bar_w/pop_size$pop[pop_size$race_eth=="White"]*100000))
hw_rr2 <- format_nums(hw_rr, hw_rr_lb, hw_rr_ub)

tab_dat_M2 <- data.frame(race_eth = c("White", "Black", "Hispanic"), 
                         shoot_rate = c(w2, b2, h2),
                         rd = c(NA, bw_rd2, hw_rd2),
                         rr = c(NA, bw_rr2, hw_rr2))

tab_dat_M1$Model <- "Model 1"
tab_dat_M2$Model <- "Model 2"

tab_dat <- rbind(tab_dat_M1, tab_dat_M2)

write_csv(tab_dat, file = "Output/table_S3.csv")

#Figs 2 & 3----
M1sims <- data.frame(M1)

#RD:
pd <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
pd <- pd + as.vector(r)
pd2 <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r2 <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
sr <- M1sims %>% select(dplyr::ends_with("ethBlack.") & starts_with("r_state_abb"))
br <- M1sims %>% select(b_race_ethBlack)
pd2 <- pd2 + sr + as.vector(r2) + as.vector(br)
pd <- exp(pd2)-exp(pd)
store1 <- pd
pd_df <- data.frame(qoi = apply(pd, 2, median), ub = apply(pd, 2, FUN = upper), 
                    lb = apply(pd, 2, FUN = lower))
pd_df$State <- substr(rownames(pd_df),13,14)
rownames(pd_df) <- NULL
pd_df$Race <- "Black"
pd_df$State <- factor(pd_df$State, levels = unique(pd_df$State[order(pd_df$qoi)]), ordered=TRUE)
pd_df$qoi <- pd_df$qoi*6*100000
pd_df$lb <- pd_df$lb*6*100000
pd_df$ub <- pd_df$ub*6*100000
pb <- ggplot(aes(y = State, x = qoi), data=pd_df) +geom_vline(xintercept = 0, size = 1, color="green") +
  geom_point() + geom_errorbar(aes(xmin=lb, xmax=ub)) +  theme_bw() + xlab("Black-White Rate Difference") +
  geom_vline(color="red", linetype="dashed", xintercept = mean(pd_df$qoi), size = 1);pb
b_qoi <- pd_df %>% transmute(State = as.character(State), b = qoi)

pd <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
pd <- pd + as.vector(r)
pd2 <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r2 <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
sr <- M1sims %>% select(dplyr::ends_with("ethHispanic.") & starts_with("r_state_abb"))
br <- M1sims %>% select(b_race_ethHispanic)
pd2 <- pd2 + sr + as.vector(r2) + as.vector(br)
pd <- exp(pd2)-exp(pd)
store2 <- pd
pd_df <- data.frame(qoi = apply(pd, 2, median), ub = apply(pd, 2, FUN = upper), 
                    lb = apply(pd, 2, FUN = lower))
pd_df$State <- substr(rownames(pd_df),13,14)
rownames(pd_df) <- NULL
pd_df$Race <- "Hispanic"
pd_df$State <- factor(pd_df$State, levels = unique(pd_df$State[order(pd_df$qoi)]), ordered=TRUE)
pd_df$qoi <- pd_df$qoi*6*100000
pd_df$lb <- pd_df$lb*6*100000
pd_df$ub <- pd_df$ub*6*100000
ph <- ggplot(aes(y = State, x = qoi), data=pd_df) + geom_vline(xintercept = 0, size = 1,color="green") +
  geom_point() + geom_errorbar(aes(xmin=lb, xmax=ub)) +  theme_bw() + xlab("Hispanic-White Rate Difference")+
  geom_vline(color="red", linetype="dashed", xintercept = mean(pd_df$qoi), size = 1);ph
mh <- mean(pd_df$qoi)

h_qoi <- pd_df %>% transmute(State = as.character(State), h = qoi)

pd <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
pd <- pd + as.vector(r)
pd_df <- data.frame(qoi = exp(apply(pd, 2, median)), ub = exp(apply(pd, 2, FUN = upper)), 
                    lb = exp(apply(pd, 2, FUN = lower)))
pd_df$State <- substr(rownames(pd_df),13,14)
rownames(pd_df) <- NULL
pd_df$Race <- "White Rates"
pd_df$State <- factor(pd_df$State, levels = unique(pd_df$State[order(pd_df$qoi)]), ordered=TRUE)
pd_df$qoi <- pd_df$qoi*100000
pd_df$lb <- pd_df$lb*100000
pd_df$ub <- pd_df$ub*100000
o_qoi <- pd_df %>% transmute(State = as.character(State), o = qoi)

#RR:
pd <- M1sims %>% select(dplyr::ends_with("ethBlack.") & starts_with("r_state_abb")) #this is simpler way to calculate IRRs and gives same answers to dividing race/ethnic groups after bringing in intercepts
r <- M1sims %>% select(b_race_ethBlack)
pd <- pd + as.vector(r)
store3 <- pd
pd_df <- data.frame(qoi = exp(apply(pd, 2, median)), ub = exp(apply(pd, 2, FUN = upper)), 
                    lb = exp(apply(pd, 2, FUN = lower)))
pd_df$State <- substr(rownames(pd_df),13,14)
rownames(pd_df) <- NULL
pd_df$Race <- "Black"
pd_df$State <- factor(pd_df$State, levels = unique(pd_df$State[order(pd_df$qoi)]), ordered=TRUE)
pb2 <- ggplot(aes(y = State, x = qoi), data=pd_df) + geom_vline(xintercept = 1, size = 1, color="green") +
  geom_point() + geom_errorbar(aes(xmin=lb, xmax=ub)) +  theme_bw() + xlab("Black-White Rate Ratio") +
  geom_vline(color="red", linetype="dashed", xintercept = exp(median(M1sims$b_race_ethBlack)), size=1) + 
  scale_x_continuous(breaks = c(1,3,5,7,9))

b_qoi2 <- pd_df %>% transmute(State = as.character(State), b = qoi)

pd <- M1sims %>% select(dplyr::ends_with("ethHispanic.") & starts_with("r_state_abb"))
r <- M1sims %>% select(b_race_ethHispanic)
pd <- pd + as.vector(r)
store4 <- pd
pd_df <- data.frame(qoi = exp(apply(pd, 2, median)), ub = exp(apply(pd, 2, FUN = upper)), 
                    lb = exp(apply(pd, 2, FUN = lower)))
pd_df$State <- substr(rownames(pd_df),13,14)
rownames(pd_df) <- NULL
pd_df$Race <- "Hispanic"
pd_df$State <- factor(pd_df$State, levels = unique(pd_df$State[order(pd_df$qoi)]), ordered=TRUE)
ph2 <- ggplot(aes(y = State, x = qoi), data=pd_df) + geom_vline(xintercept = 1, size = 1, color="green") +
  geom_point() + geom_errorbar(aes(xmin=lb, xmax=ub)) +  theme_bw() + xlab("Hispanic-White Rate Ratio") +
  geom_vline(color="red", linetype="dashed", xintercept = exp(median(M1sims$b_race_ethHispanic)), size=1)  +
  scale_x_continuous(breaks = c(0,1,2)) 

h_qoi2 <- pd_df %>% transmute(State = as.character(State), h = qoi)

pd <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
pd <- pd + as.vector(r)
store5 <- pd
pd_df <- data.frame(qoi = exp(apply(pd, 2, median)), ub = exp(apply(pd, 2, FUN = upper)), 
                    lb = exp(apply(pd, 2, FUN = lower)))
pd_df$State <- substr(rownames(pd_df),13,14)
rownames(pd_df) <- NULL
pd_df$Race <- "White Rates"
pd_df$State <- factor(pd_df$State, levels = unique(pd_df$State[order(pd_df$qoi)]), ordered=TRUE)
pd_df$qoi <- pd_df$qoi*100000
pd_df$lb <- pd_df$lb*100000
pd_df$ub <- pd_df$ub*100000
o_qoi2 <- pd_df %>% transmute(State = as.character(State), o = qoi)

combo_plot <- ggarrange(pb + geom_vline(xintercept = t1$rd[t1$race_eth=="Black"], color="blue", linetype="dotted"), 
                        pb2 + geom_vline(xintercept = t1$rd[t1$race_eth=="Black"], color="blue", linetype="dotted"),
                        labels = "AUTO", nrow = 1)
ggexport(combo_plot, filename = "Output/Fig2.pdf", width = 8, height = 8)

combo_plot <- ggarrange(ph + geom_vline(xintercept = t1$rd[t1$race_eth=="Hispanic"], color="blue", linetype="dotted"),
                        ph2 + geom_vline(xintercept = t1$rr[t1$race_eth=="Hispanic"], color="blue", linetype="dotted"),
                        labels = "AUTO", nrow = 1)
ggexport(combo_plot, filename = "Output/Fig3.pdf", width = 8, height = 8)

#Make scatterplot matrix: RD
o_qoi$o <- o_qoi$o*6
pd <- left_join(b_qoi, h_qoi, by = "State") %>% left_join(., o_qoi, by = "State")

#correlation of medians
correlation_value <- round(cor(pd[,-1]),2) 

#median & CIs of correlations:
#rescale
store1b <- store1*6*100000
store2b <- store2*6*100000
store3b <- exp(store3)
store4b <- exp(store4)
store5b <- exp(store5)*6*100000

#get correlations across draws
row_cor <- function(x, y) {
  xm <- x - rowMeans(x)
  ym <- y - rowMeans(y)
  rowSums(xm * ym) / sqrt(rowSums(xm^2) * rowSums(ym^2))
}

#B RR vs RD
out <- row_cor(store1b, store3b)
round(median(out),2);round(lower(out),2);round(upper(out),2)

#H RR vs RD
out <- row_cor(store2b, store4b)
round(median(out),2);round(lower(out),2);round(upper(out),2)

#B RD vs W rates
out <- row_cor(store1b, store5b)
round(median(out),2); round(lower(out),2); round(upper(out),2)
cor1 <- round(median(out),2)

#B RR vs W rates
out <- row_cor(store3b, store5b)
round(median(out),2); round(lower(out),2); round(upper(out),2)
cor2 <- round(median(out),2)

#H RD vs W rates
out <- row_cor(store2b, store5b)
round(median(out),2); round(lower(out),2); round(upper(out),2)
cor3 <- round(median(out),2)

#H RR vs W rates
out <- row_cor(store4b, store5b)
round(median(out),2); round(lower(out),2); round(upper(out),2)
cor4 <- round(median(out),2)

#B  vs H RD
out <- row_cor(store1b, store2b)
round(median(out),2); round(lower(out),2); round(upper(out),2)
cor5 <- round(median(out),2)

#B  vs H RR
out <- row_cor(store3b, store4b)
round(median(out),2); round(lower(out),2); round(upper(out),2)
cor6 <- round(median(out),2)


#Fig S1----
a <- ggplot(aes(x=b, y = h), data= pd) + 
  geom_point() + geom_label_repel(aes(label = State)) +
  theme_bw() + xlab("Black-White Rate Difference") + ylab("Hispanic-White Rate Difference") + 
  annotate("text", x = Inf, y = -Inf, label = paste("r =", cor5), 
           hjust = 1.5, vjust = -14, size = 7, color = "red"); a

b <- ggplot(aes(x=b, y = o), data= pd) +
  geom_point() + geom_label_repel(aes(label = State)) +
  theme_bw() + xlab("Black-White Rate Difference") + ylab("White Fatal Shooting Rate") + 
  annotate("text", x = Inf, y = -Inf, label = paste("r =", cor1), 
           hjust = 1.5, vjust = -15.5, size = 7, color = "red"); b

c <- ggplot(aes(x=h, y = o), data= pd) + 
  geom_point() + geom_label_repel(aes(label = State)) +
  theme_bw() + xlab("Hispanic-White Rate Difference") + ylab("White Fatal Shooting Rate") + 
  annotate("text", x = Inf, y = -Inf, label = paste("r =", cor3), 
           hjust = 1.5, vjust = -15.5, size = 7, color = "red"); c

combo_plot <- ggarrange(b,c,a, labels = "AUTO", ncol = 2, nrow=2)
ggexport(combo_plot, filename = "Output/FigS1.pdf", width = 8, height = 8)

#Make scatterplot matrix: RR
o_qoi2$o <- o_qoi2$o*6
pd <- left_join(b_qoi2, h_qoi2, by = "State") %>% left_join(., o_qoi2, by = "State")
correlation_value <- round(cor(pd[,-1]),2) 

#Fig S2----
a <- ggplot(aes(x=b, y = h), data= pd) + 
  geom_point() + geom_label_repel(aes(label = State)) +
  theme_bw() + xlab("Black-White Rate Ratio") + ylab("Hispanic-White Rate Ratio") + 
  annotate("text", x = Inf, y = -Inf, label = paste("r =", cor6), 
           hjust = 1.5, vjust = -15.5, size = 7, color = "red"); a

b <- ggplot(aes(x=b, y = o), data= pd) +
  geom_point() + geom_label_repel(aes(label = State)) +
  theme_bw() + xlab("Black-White Rate Ratio") + ylab("White Fatal Shooting Rate") + 
  annotate("text", x = Inf, y = -Inf, label = paste("r =", cor2), 
           hjust = 1.5, vjust = -15.5, size = 7, color = "red"); b

c <- ggplot(aes(x=h, y = o), data= pd) + 
  geom_point() + geom_label_repel(aes(label = State)) +
  theme_bw() + xlab("Hispanic-White Rate Ratio") + ylab("White Fatal Shooting Rate") + 
  annotate("text", x = Inf, y = -Inf, label = paste("r =", cor4), 
           hjust = 1.5, vjust = -15.5, size = 7, color = "red"); c

combo_plot <- ggarrange(b,c,a, labels = "AUTO", ncol = 2, nrow=2)
ggexport(combo_plot, filename = "Output/FigS2.pdf", width = 8, height = 8)


#Fig 1----
state_estimates <- left_join(o_qoi, b_qoi, by = "State") %>% left_join(., h_qoi, by = "State")
state_estimates$h <- state_estimates$h + state_estimates$o #from RD to H rate
state_estimates$b <- state_estimates$b + state_estimates$o #from RD to B rate
state_estimates$STUSPS <- state_estimates$State
state_estimates$STUSPS[state_estimates$STUSPS=="NB"] <- "NE"
states <- read_sf("Data/cb_2016_us_state_20m/") 
states <- states %>% filter(STUSPS != "PR", GEOID != 11)
states <- left_join(states, state_estimates, by = "STUSPS")

HI_state <- states %>% filter(STUSPS=="HI")
AK_state <- states %>% filter(STUSPS=="AK")
states2 <- states %>% filter(! STUSPS %in% c("AK", "HI"))

AK_state <- st_crop(AK_state, y = c(xmin = -179.2, ymin = 51.21985, xmax = -128, ymax = 71.35257))#trims some Aleutians

#White
map <- ggplot(data=states2) + theme_void() + #change to states2 to make it easier to see
  geom_sf(aes(fill = o)) +
  scale_fill_gradient(limits=c(0,4.54), low="white", high="blue")+
  guides(fill=guide_legend(title="Rate\nper 100k")) + theme(legend.position = c(.92,.3)); map

map_AK <- ggplot(data=AK_state) + geom_sf() +  theme_void() +
  geom_sf(aes(fill = o)) +
  scale_fill_gradient(limits=c(0,4.54), low="white", high="blue")+
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=1)) 

map_HI <- ggplot(data=HI_state) + geom_sf() +  theme_void() +
  geom_sf(aes(fill = o)) +
  scale_fill_gradient(limits=c(0,4.54), low="white", high="blue")+
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=1)) 

map_combo_w <- map + 
  inset(ggplotGrob(map_HI), xmin = -115, xmax = -100, ymin = 23, ymax = 28) +
  inset(ggplotGrob(map_AK), xmin = -127, xmax = -112, ymin = 23, ymax = 31.5); map_combo_w

#Black
map <- ggplot(data=states2) + theme_void() + #change to states2 to make it easier to see
  geom_sf(aes(fill = b)) +
  scale_fill_gradient(limits=c(1.5,10.5), low="white", high="red")+
  guides(fill=guide_legend(title="Rate\nper 100k")) + theme(legend.position = c(.92,.3)); map

map_AK <- ggplot(data=AK_state) + geom_sf() +  theme_void() +
  geom_sf(aes(fill = b)) +
  scale_fill_gradient(limits=c(1.5,10.5), low="white", high="red")+
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=1)) 

map_HI <- ggplot(data=HI_state) + geom_sf() +  theme_void() +
  geom_sf(aes(fill = b)) +
  scale_fill_gradient(limits=c(1.5,10.5), low="white", high="red")+
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=1)) 

map_combo_b <- map + 
  inset(ggplotGrob(map_HI), xmin = -115, xmax = -100, ymin = 23, ymax = 28) +
  inset(ggplotGrob(map_AK), xmin = -127, xmax = -112, ymin = 23, ymax = 31.5); map_combo_b

#Hispanic
map <- ggplot(data=states2) + theme_void() + #change to states2 to make it easier to see
  geom_sf(aes(fill = h)) +
  scale_fill_gradient(limits=c(0,6.5), low="white", high="green")+
  guides(fill=guide_legend(title="Rate\nper 100k")) + theme(legend.position = c(.92,.3)); map

map_AK <- ggplot(data=AK_state) + geom_sf() +  theme_void() +
  geom_sf(aes(fill = h)) +
  scale_fill_gradient(limits=c(0,6.5), low="white", high="green")+
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=1)) 

map_HI <- ggplot(data=HI_state) + geom_sf() +  theme_void() +
  geom_sf(aes(fill = h)) +
  scale_fill_gradient(limits=c(0,6.5), low="white", high="green")+
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, size=1)) 

map_combo_h <- map + 
  inset(ggplotGrob(map_HI), xmin = -115, xmax = -100, ymin = 23, ymax = 28) +
  inset(ggplotGrob(map_AK), xmin = -127, xmax = -112, ymin = 23, ymax = 31.5); map_combo_h

map_combo <- ggarrange(map_combo_w, map_combo_b, map_combo_h, ncol = 1, labels= c("(A) White", "(B) Black", "(C) Hispanic"))
ggexport(filename = "Output/Fig1.pdf", map_combo, width=6, height=12)  

#numbers reported in the text
M1sims <- data.frame(M1)
pd <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
pd <- pd + as.vector(r)
pd_df <- data.frame(qoi = exp(apply(pd, 2, median)), ub = exp(apply(pd, 2, FUN = upper)), 
                    lb = exp(apply(pd, 2, FUN = lower)))
pd_df$State <- substr(rownames(pd_df),13,14)
pd_df$qoi <- pd_df$qoi*100000*6
pd_df$lb <- pd_df$lb*100000*6
pd_df$ub <- pd_df$ub*100000*6
pd_df[which.min(pd_df$qoi),]
pd_df[which.max(pd_df$qoi),]

M1sims <- data.frame(M1)
pd <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
sr <- M1sims %>% select(dplyr::ends_with("ethBlack.") & starts_with("r_state_abb"))
br <- M1sims %>% select(b_race_ethBlack)
pd <- pd + sr+ as.vector(r) + as.vector(br)
pd_df <- data.frame(qoi = exp(apply(pd, 2, median)), ub = exp(apply(pd, 2, FUN = upper)), 
                    lb = exp(apply(pd, 2, FUN = lower)))
pd_df$State <- substr(rownames(pd_df),13,14)
pd_df$qoi <- pd_df$qoi*100000*6
pd_df$lb <- pd_df$lb*100000*6
pd_df$ub <- pd_df$ub*100000*6
pd_df[which.min(pd_df$qoi),]
pd_df[which.max(pd_df$qoi),]

M1sims <- data.frame(M1)
pd <- M1sims %>% select(dplyr::ends_with("Intercept.") & starts_with("r_state_abb"))
r <- M1sims %>% select(dplyr::starts_with("b_Intercept"))
sr <- M1sims %>% select(dplyr::ends_with("ethHispanic.") & starts_with("r_state_abb"))
br <- M1sims %>% select(b_race_ethHispanic)
pd <- pd + sr+ as.vector(r) + as.vector(br)
pd_df <- data.frame(qoi = exp(apply(pd, 2, median)), ub = exp(apply(pd, 2, FUN = upper)), 
                    lb = exp(apply(pd, 2, FUN = lower)))
pd_df$State <- substr(rownames(pd_df),13,14)
pd_df$qoi <- pd_df$qoi*100000*6
pd_df$lb <- pd_df$lb*100000*6
pd_df$ub <- pd_df$ub*100000*6
pd_df[which.min(pd_df$qoi),]
pd_df[which.max(pd_df$qoi),]

#Fig 4----
M2sims <- as.matrix(M2)
M2sims <- M2sims[, 1:6]

new.dat <- data.frame(`(Intercept)` = 1, race_ethBlack = c(0,1,0), race_ethHispanic = c(0,0,1), hfr = rep(seq(from = .1, to =.6, by=.1), each=3))
new.dat$`race_ethBlack:hfr` <- new.dat$race_ethBlack*new.dat$hfr
new.dat$`race_ethHispanic:hfr` <- new.dat$race_ethHispanic*new.dat$hfr

out <- M2sims %*% t(new.dat)
out_med <- apply(out, 2, median)
out_lb <- apply(out, 2, function(x) quantile(x, 0.1))
out_ub <- apply(out, 2, function(x) quantile(x, 0.9))
out2 <- data.frame(new.dat, estimate = exp(out_med)*100000*6, lb = exp(out_lb)*100000*6, ub = exp(out_ub)*100000*6)
out2$Race <- c("White", "Black", "Hispanic")

pc <- ggplot(aes(x= hfr, y= estimate, color = Race, linetype = Race), data = out2)  + theme_bw() +
  ylab("Fatal Police Shooting Rate") +geom_line(size=1.5) + xlab("Household Firearm Ownership Rate") +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.width = unit(1.5,"cm")) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = Race, x =hfr), alpha=.3, inherit.aes = F, show.legend = F) +
  scale_color_manual(values = c("red","green","blue")) +scale_fill_manual(values = c("red","green","blue"))

ggsave("Output/Fig4.pdf", pc, width=4, height=4.5)

#Calculate point estimates and precision for QOIs from this figure
M2sims <- as.matrix(M2)
M2sims <- M2sims[, 1:6]

df$hfr_decile <- cut(df$hfr, breaks=c(quantile(df$hfr, probs = seq(0, 1, by = 0.1))), 
                     labels = 1:10,
                     include.lowest = T)
bottom_decile_avg <- mean(df$hfr[df$hfr_decile==1]) #12.0
top_decile_avg <- mean(df$hfr[df$hfr_decile==10]) #59.5

#RD among low
new.dat <- data.frame(`(Intercept)` = 1, race_ethBlack = c(0,1,0), race_ethHispanic = c(0,0,1), hfr = bottom_decile_avg)
new.dat$`race_ethBlack:hfr` <- new.dat$race_ethBlack*new.dat$hfr
new.dat$`race_ethHispanic:hfr` <- new.dat$race_ethHispanic*new.dat$hfr
outa <- M2sims %*% t(new.dat)
mean((exp(outa[,2])*100000*6) - (exp(outa[,1])*100000*6)); lower((exp(outa[,2])*100000*6) - (exp(outa[,1])*100000*6)); 
upper((exp(outa[,2])*100000*6) - (exp(outa[,1])*100000*6)) #B-W
mean((exp(outa[,3])*100000*6) - (exp(outa[,1])*100000*6)); lower((exp(outa[,3])*100000*6) - (exp(outa[,1])*100000*6));
upper((exp(outa[,3])*100000*6) - (exp(outa[,1])*100000*6)) #H-W

#RD among high
new.dat <- data.frame(`(Intercept)` = 1, race_ethBlack = c(0,1,0), race_ethHispanic = c(0,0,1), hfr = top_decile_avg)
new.dat$`race_ethBlack:hfr` <- new.dat$race_ethBlack*new.dat$hfr
new.dat$`race_ethHispanic:hfr` <- new.dat$race_ethHispanic*new.dat$hfr
outb <- M2sims %*% t(new.dat)
mean((exp(outb[,2])*100000*6) - (exp(outb[,1])*100000*6)); lower((exp(outb[,2])*100000*6) - (exp(outb[,1])*100000*6)); 
upper((exp(outb[,2])*100000*6) - (exp(outb[,1])*100000*6)) #B-W
mean((exp(outb[,3])*100000*6) - (exp(outb[,1])*100000*6)); lower((exp(outb[,3])*100000*6) - (exp(outb[,1])*100000*6)); 
upper((exp(outb[,3])*100000*6) - (exp(outb[,1])*100000*6)) #H-W

#Diffs between low and high RD
t <- ((exp(outa[,3])*100000*6) - (exp(outa[,1])*100000*6)) - ((exp(outb[,3])*100000*6) - (exp(outb[,1])*100000*6))
mean(t); lower(t); upper(t)#H-W

t <-  ((exp(outb[,2])*100000*6) - (exp(outb[,1])*100000*6)) - ((exp(outa[,2])*100000*6) - (exp(outa[,1])*100000*6))
mean(t); lower(t); upper(t)#B-W

#RR among low
new.dat <- data.frame(`(Intercept)` = 1, race_ethBlack = c(0,1,0), race_ethHispanic = c(0,0,1), hfr = bottom_decile_avg)
new.dat$`race_ethBlack:hfr` <- new.dat$race_ethBlack*new.dat$hfr
new.dat$`race_ethHispanic:hfr` <- new.dat$race_ethHispanic*new.dat$hfr
outa <- M2sims %*% t(new.dat)
mean((exp(outa[,2])*100000*6) / (exp(outa[,1])*100000*6)); lower((exp(outa[,2])*100000*6) / (exp(outa[,1])*100000*6)); 
upper((exp(outa[,2])*100000*6) / (exp(outa[,1])*100000*6)) #B-W
mean((exp(outa[,3])*100000*6) / (exp(outa[,1])*100000*6)); lower((exp(outa[,3])*100000*6) / (exp(outa[,1])*100000*6));
upper((exp(outa[,3])*100000*6) / (exp(outa[,1])*100000*6)) #H-W

#RR among high
new.dat <- data.frame(`(Intercept)` = 1, race_ethBlack = c(0,1,0), race_ethHispanic = c(0,0,1), hfr = top_decile_avg)
new.dat$`race_ethBlack:hfr` <- new.dat$race_ethBlack*new.dat$hfr
new.dat$`race_ethHispanic:hfr` <- new.dat$race_ethHispanic*new.dat$hfr
outb <- M2sims %*% t(new.dat)
mean((exp(outb[,2])*100000*6) / (exp(outb[,1])*100000*6)); lower((exp(outb[,2])*100000*6) / (exp(outb[,1])*100000*6)); 
upper((exp(outb[,2])*100000*6) / (exp(outb[,1])*100000*6)) #B-W
mean((exp(outb[,3])*100000*6) / (exp(outb[,1])*100000*6)); lower((exp(outb[,3])*100000*6) / (exp(outb[,1])*100000*6));
upper((exp(outb[,3])*100000*6) / (exp(outb[,1])*100000*6)) #H-W

#Diffs between low and high RR
t <- ((exp(outa[,3])*100000*6) / (exp(outa[,1])*100000*6)) - ((exp(outb[,3])*100000*6) / (exp(outb[,1])*100000*6))
mean(t); lower(t); upper(t)#H-W

t <-  ((exp(outb[,2])*100000*6) / (exp(outb[,1])*100000*6)) - ((exp(outa[,2])*100000*6) / (exp(outa[,1])*100000*6))
mean(t); lower(t); upper(t)#B-W

#Fig S4----
M2sims <- as.matrix(M2)
M2sims <- M2sims[, 1:6]

new.dat <- data.frame(`(Intercept)` = 1, race_ethBlack = c(0,1,0), race_ethHispanic = c(0,0,1), hfr = rep(seq(from = .1, to =.6, by=.1), each=3))
new.dat$`race_ethBlack:hfr` <- new.dat$race_ethBlack*new.dat$hfr
new.dat$`race_ethHispanic:hfr` <- new.dat$race_ethHispanic*new.dat$hfr

out <- M2sims %*% t(new.dat)

out_w <- exp(out[,which(new.dat$race_ethBlack==0 & new.dat$race_ethHispanic==0)])*6*100000
out_b <- exp(out[,which(new.dat$race_ethBlack==1 & new.dat$race_ethHispanic==0)])*6*100000
out_h <- exp(out[,which(new.dat$race_ethBlack==0 & new.dat$race_ethHispanic==1)])*6*100000

out_bw_rd <- out_b-out_w
out_med <- apply(out_bw_rd, 2, mean)
out_lb <- apply(out_bw_rd, 2, function(x) quantile(x, 0.1))
out_ub <- apply(out_bw_rd, 2, function(x) quantile(x, 0.9))
out_1 <- data.frame(hfr = rep(seq(from = .1, to =.6, by=.1)), estimate = out_med, lb = out_lb, ub = out_ub, 
                    comparison = "Black-White", metric = "Rate Difference")

out_hw_rd <- out_h-out_w
out_med <- apply(out_hw_rd, 2, mean)
out_lb <- apply(out_hw_rd, 2, function(x) quantile(x, 0.1))
out_ub <- apply(out_hw_rd, 2, function(x) quantile(x, 0.9))
out_2 <- data.frame(hfr = rep(seq(from = .1, to =.6, by=.1)), estimate = out_med, lb = out_lb, ub = out_ub, 
                    comparison = "Hispanic-White", metric = "Rate Difference")

out_bw_rr <- out_b/out_w
out_med <- apply(out_bw_rr, 2, mean)
out_lb <- apply(out_bw_rr, 2, function(x) quantile(x, 0.1))
out_ub <- apply(out_bw_rr, 2, function(x) quantile(x, 0.9))
out_3 <- data.frame(hfr = rep(seq(from = .1, to =.6, by=.1)), estimate = out_med, lb = out_lb, ub = out_ub, 
                    comparison = "Black-White", metric = "Rate Ratio")

out_hw_rr <- out_h/out_w
out_med <- apply(out_hw_rr, 2, mean)
out_lb <- apply(out_hw_rr, 2, function(x) quantile(x, 0.1))
out_ub <- apply(out_hw_rr, 2, function(x) quantile(x, 0.9))
out_4 <- data.frame(hfr = rep(seq(from = .1, to =.6, by=.1)), estimate = out_med, lb = out_lb, ub = out_ub, 
                    comparison = "Hispanic-White", metric = "Rate Ratio")

out_pd <- rbind(out_1, out_2, out_3, out_4)
p <- ggplot(aes(x=hfr, y=estimate, ymin=lb, ymax=ub), data= out_pd) + geom_line(color= "skyblue",size=1.5) + 
  geom_ribbon(alpha=.25, fill="skyblue") +
  theme_bw() + facet_grid(metric~comparison, scale="free_y") +
  ylab("Disparity Metric Value") + xlab("Household Firearm Ownership Rate")

ggsave("Output/FigS4.pdf", p, width=6, height=6)

#Fig S3----
df$hfr2 <- poly(df$hfr,2, raw = T)[,2]
M2b <-  brm(n_shootings ~ race_eth*hfr + race_eth*hfr2 + (1 + race_eth | state_abb) + (1 | year) + 
              offset(log(pop)),
            data = df, sample_prior = T,
            family = negbinomial(link = "log", link_shape = "log"),
            seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
            prior = c(prior_string("normal(0,5)", class = "b"),
                      prior_string("normal(0,10)", class = "Intercept"),
                      prior_string("exponential(.02)", class = "shape")))#this lets it explore all reasonable space

M2bsims <- as.matrix(M2b)
M2bsims <- M2bsims[, 1:9]

new.dat <- data.frame(`(Intercept)` = 1, race_ethBlack = c(0,1,0), race_ethHispanic = c(0,0,1), hfr = rep(seq(from = .1, to =.6, by=.1), each=3))
new.dat$hfr2 <- new.dat$hfr^2
new.dat$`race_ethBlack:hfr` <- new.dat$race_ethBlack*new.dat$hfr
new.dat$`race_ethHispanic:hfr` <- new.dat$race_ethHispanic*new.dat$hfr
new.dat$`race_ethBlack:hfr2` <- new.dat$race_ethBlack*new.dat$hfr2
new.dat$`race_ethHispanic:hfr2` <- new.dat$race_ethHispanic*new.dat$hfr2

out <- M2bsims %*% t(new.dat)
out_med <- apply(out, 2, median)
out_lb <- apply(out, 2, function(x) quantile(x, 0.1))
out_ub <- apply(out, 2, function(x) quantile(x, 0.9))
out2 <- data.frame(new.dat, estimate = exp(out_med)*100000*6, lb = exp(out_lb)*100000*6, ub = exp(out_ub)*100000*6)
out2$Race <- c("White", "Black", "Hispanic")

p <- ggplot(aes(x= hfr, y= estimate, color = Race, linetype=Race), data = out2)  + theme_bw() +
  ylab("Fatal Police Shooting Rate") +geom_line(size=1.5) + xlab("Household Firearm Ownership Rate") +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.width = unit(1.5,"cm")) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = Race, x =hfr), alpha=.3, inherit.aes = F, show.legend = F) +
  scale_color_manual(values = c("red","green","blue")) +scale_fill_manual(values = c("red","green","blue")); p

ggsave("Output/FigS3.pdf", p, width=4, height=4.5)

#Table S2-----
tab_output <- NULL
for(i in 1:2){
  mod <- paste0("M",i)
  yrep1 <- posterior_predict(get(mod)) 
  
  t <- apply(yrep1, 1, mean)
  obs <- mean(df$n_shootings)
  range_x <- range(c(t,obs))
  y_mean <- mean(t >= obs)
  
  t <- apply(yrep1, 1, max)
  obs <- max(df$n_shootings)
  range_x <- range(c(t,obs))
  y_max <- mean(t >= obs)
  
  t <- apply(yrep1, 1, function(f){sum(f==0)})
  obs <- sum(df$n_shootings==0)
  range_x <- range(c(t,obs))
  prop_zero <- mean(t >= obs)
  
  t <- apply(yrep1, 1, var)
  obs <- var(df$n_shootings)
  range_x <- range(c(t,obs))
  var_mod <- mean(t >= obs) 
  
  temp <- data.frame(Model = i, y_mean, y_max, prop_zero, var_mod)
  tab_output <- rbind(tab_output, temp)
}

tabout <- round(tab_output,2)
write_csv(tabout, "Output/table_S2.csv")

#Table S4----
M1sims <- as.matrix(M1)
M1_coef <- round(apply(as.matrix(M1), 2, median)[1:3], 3)
M1_lb <- round(apply(as.matrix(M1), 2, lower)[1:3], 3)
M1_ub <- round(apply(as.matrix(M1), 2, upper)[1:3], 3)

M2sims <- as.matrix(M2)
M2_coef <- round(apply(as.matrix(M2), 2, median)[1:6], 3)
M2_lb <- round(apply(as.matrix(M2), 2, lower)[1:6], 3)
M2_ub <- round(apply(as.matrix(M2), 2, upper)[1:6], 3)

M1_coefs <- data.frame(var = names(M1_coef), M1 = paste0(M1_coef, " (", M1_lb, ", ", M1_ub,")"))
M2_coefs <- data.frame(var = names(M2_coef), M2 = paste0(M2_coef, " (", M2_lb, ", ", M2_ub,")"))

reg_out <- full_join(M1_coefs, M2_coefs, by = "var") 

loo_combo <- data.frame(var = "LOOIC", M1 = loo_M1$estimates[3,1], M2 = loo_M2$estimates[3,1])
loo_combo[2:3] <- round(loo_combo[2:3],1)

reg_out <- rbind(reg_out, loo_combo)
reg_out <- sapply(reg_out, as.character)
reg_out[is.na(reg_out)] <- ""
reg_out <- as.data.frame(reg_out)

gtsave(gt(reg_out), "Output/table_S4.html")

#disparities without CA, AR, NM----
t1b <- df %>% filter(!state_abb %in% c("CA", "AZ", "NM")) %>% group_by(race_eth) %>% summarise(shootings = sum(n_shootings), 
                                                                               pop = sum(pop)/6, shoot_rate = sum(n_shootings)/sum(pop)*100000)

t1b$rd <- round(t1b$shoot_rate-t1b$shoot_rate[t1b$race_eth=="White"],2)
t1b$rr <- round(t1b$shoot_rate/t1b$shoot_rate[t1b$race_eth=="White"],2)
t1b$shoot_rate <- round(t1b$shoot_rate,2)
t1b %>% select(-pop, -shootings)

#Table S1----
df2 <- readRDS("Data/state_year_shootings_cleaned_imputed.rds")
df2 <- df2 %>% 
  filter(!is.na(shall_issue)) %>% #get rid of DC 
  transmute(state_abb, year, race_eth = factor(race_eth, levels = c("White", "Black", "Hispanic")), n_shootings, pop = round(pop), State, hfr) 

M1_imp <- brm(n_shootings ~ race_eth + (1 + race_eth | state_abb) + (1 | year) + 
            offset(log(pop)),
          data = df2, sample_prior = T,
          family = negbinomial(link = "log", link_shape = "log"),
          seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
          prior = c(prior_string("normal(0,5)", class = "b"),
                    prior_string("normal(0,10)", class = "Intercept"),
                    prior_string("exponential(.02)", class = "shape")))#this is in effect quite wide since can be huge variance added or none

M2_imp <- brm(n_shootings ~ race_eth*hfr + (1 + race_eth | state_abb) + (1 | year) + 
            offset(log(pop)),
          data = df2, sample_prior = T,
          family = negbinomial(link = "log", link_shape = "log"),
          seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
          prior = c(prior_string("normal(0,5)", class = "b"),
                    prior_string("normal(0,10)", class = "Intercept"),
                    prior_string("exponential(.02)", class = "shape")))

loo_M1_imp <- loo(M1_imp)
loo_M2_imp <- loo(M2_imp)

M1_imp_coef <- round(apply(as.matrix(M1_imp), 2, median)[1:3], 3)
M1_imp_lb <- round(apply(as.matrix(M1_imp), 2, lower)[1:3], 3)
M1_imp_ub <- round(apply(as.matrix(M1_imp), 2, upper)[1:3], 3)

M2_imp_coef <- round(apply(as.matrix(M2_imp), 2, median)[1:6], 3)
M2_imp_lb <- round(apply(as.matrix(M2_imp), 2, lower)[1:6], 3)
M2_imp_ub <- round(apply(as.matrix(M2_imp), 2, upper)[1:6], 3)

M1_imp_coefs <- data.frame(var = names(M1_imp_coef), M1_imp = paste0(M1_imp_coef, " (", M1_imp_lb, ", ", M1_imp_ub,")"))
M2_imp_coefs <- data.frame(var = names(M2_imp_coef), M2_imp = paste0(M2_imp_coef, " (", M2_imp_lb, ", ", M2_imp_ub,")"))

reg_out <- full_join(M1_imp_coefs, M2_imp_coefs, by = "var") 

loo_combo <- data.frame(var = "LOOIC", M1_imp = loo_M1_imp$estimates[3,1], M2_imp = loo_M2_imp$estimates[3,1])
loo_combo[2:3] <- round(loo_combo[2:3],1)

reg_out <- rbind(reg_out, loo_combo)
reg_out <- sapply(reg_out, as.character)
reg_out[is.na(reg_out)] <- ""
reg_out <- as.data.frame(reg_out)

gtsave(gt(reg_out), "Output/table_S1.html")


#Table S5----
M1_pois <- brm(n_shootings ~ race_eth + (1 + race_eth | state_abb) + (1 | year) + 
            offset(log(pop)),
          data = df, sample_prior = T,
          family = poisson(),
          seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
          prior = c(prior_string("normal(0,5)", class = "b"),
                    prior_string("normal(0,10)", class = "Intercept")))#this is in effect quite wide since can be huge variance added or none
loo_M1_pois <- loo(M1_pois)

M2_pois <- brm(n_shootings ~ race_eth*hfr + (1 + race_eth | state_abb) + (1 | year) + 
            offset(log(pop)),
          data = df, sample_prior = T,
          family = poisson(),
          seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
          prior = c(prior_string("normal(0,5)", class = "b"),
                    prior_string("normal(0,10)", class = "Intercept")))
loo_M2_pois <- loo(M2_pois)

M1_coef <- round(apply(as.matrix(M1_pois), 2, median)[1:3], 3)
M1_lb <- round(apply(as.matrix(M1_pois), 2, lower)[1:3], 3)
M1_ub <- round(apply(as.matrix(M1_pois), 2, upper)[1:3], 3)

M2_coef <- round(apply(as.matrix(M2_pois), 2, median)[1:6], 3)
M2_lb <- round(apply(as.matrix(M2_pois), 2, lower)[1:6], 3)
M2_ub <- round(apply(as.matrix(M2_pois), 2, upper)[1:6], 3)

M1_coefs <- data.frame(var = names(M1_coef), M1 = paste0(M1_coef, " (", M1_lb, ", ", M1_ub,")"))
M2_coefs <- data.frame(var = names(M2_coef), M2 = paste0(M2_coef, " (", M2_lb, ", ", M2_ub,")"))

reg_out <- full_join(M1_coefs, M2_coefs, by = "var") 

loo_combo <- data.frame(var = "LOOIC", M1 = loo_M1_pois$estimates[3,1], M2 = loo_M2_pois$estimates[3,1])
loo_combo[2:3] <- round(loo_combo[2:3],1)

reg_out <- rbind(reg_out, loo_combo)
reg_out <- sapply(reg_out, as.character)
reg_out[is.na(reg_out)] <- ""
reg_out <- as.data.frame(reg_out)

gtsave(gt(reg_out), "Output/table_S5.html")

#Fig S5----
#Race-specific numbers
df2 <- df %>% mutate(hfr_backup = hfr)
df2$race_eth <- ifelse(df2$race_eth == "White", "White", "Black/Hispanic")
df2 <- df2 %>% group_by(race_eth,state_abb, State, year) %>% summarise(n_shootings = sum(n_shootings), pop = sum(pop))

dat <- read_csv("Data/hfr-estimates_racespecific.csv") %>% 
  group_by(State, race_eth) %>% 
  summarise(hfr = mean(Mean_HFR))

w1 <- dat %>% filter(race_eth=="White") %>% arrange(State)
bh1 <- dat %>% filter(race_eth=="Black/Hispanic") %>% arrange(State)
cor(w1$hfr, bh1$hfr) ;plot(w1$hfr, bh1$hfr);abline(a=0, b=1)

df2 <- left_join(df2, dat, by =c("State", "race_eth"))

M2_alt <- brm(n_shootings ~ race_eth*hfr + (1 + race_eth | state_abb) + (1 | year) + 
            offset(log(pop)),
          data = df2, sample_prior = T,
          family = negbinomial(link = "log", link_shape = "log"),
          seed = 68473, iter = 4000, warmup = 2000, control = list(adapt_delta = 0.99),
          prior = c(prior_string("normal(0,5)", class = "b"),
                    prior_string("normal(0,10)", class = "Intercept"),
                    prior_string("exponential(.02)", class = "shape")))

M2sims <- as.matrix(M2_alt)
M2sims <- M2sims[, 1:4]

new.dat <- data.frame(`(Intercept)` = 1, race_ethWhite = c(0,1), hfr = rep(seq(from = .1, to =.6, by=.1), each=2))
new.dat$`race_ethWhite:hfr` <- new.dat$race_ethWhite*new.dat$hfr

out <- M2sims %*% t(new.dat)
out_med <- apply(out, 2, median)
out_lb <- apply(out, 2, function(x) quantile(x, 0.1))
out_ub <- apply(out, 2, function(x) quantile(x, 0.9))
out2 <- data.frame(new.dat, estimate = exp(out_med)*100000*6, lb = exp(out_lb)*100000*6, ub = exp(out_ub)*100000*6)
out2$Race <- c("Black/Hispanic", "White")

pc <- ggplot(aes(x= hfr, y= estimate, color = Race, linetype=Race), data = out2)  + theme_bw() +
  ylab("Fatal Police Shooting Rate") +geom_line(size=1.5) + xlab("Household Firearm Ownership Rate") +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.key.width = unit(1.5,"cm")) +
  geom_ribbon(aes(ymin = lb, ymax = ub, fill = Race, x =hfr), alpha=.3, inherit.aes = F, show.legend = F)
pc
ggsave("Output/FigS5.pdf", pc, width=4, height=4.5)


