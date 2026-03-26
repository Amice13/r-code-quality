# Loads, cleans and append Stata output tables
install.packages(c("haven", "dplyr", "ggplot2", "readr"))
library(haven)
library(dplyr)
library(ggplot2)
library(readr)

###################################################################
# Create a combined data frame for figures 1, 2, SI1, SI2, Si3, SI4
file_list5 <- c("tmp/est_fig1_dta.dta", "tmp/est_figSI1_dta.dta", 
                "tmp/est_figSI2_dta.dta", "tmp/est_figSI3_dta.dta", 
                "tmp/est_figSI4_dta.dta")
file_list10 <-c("tmp/est_fig2_women_dta.dta", "tmp/est_fig2_men_dta.dta")
result_list <- list()
combined_results <- data.frame()
file_mapping <- c("fig1_dta"       = "figuur1",
                  "fig2_women_dta" = "figuur2_women",
                  "fig2_men_dta"   = "figuur2_men",
                  "figSI1_dta"     = "figuur_si1",
                  "figSI2_dta"     = "figuur_si2",
                  "figSI3_dta"     = "figuur_si3",
                  "figSI4_dta"     = "figuur_si4")

for (file in file_list5) {
  
  result_name <- sub(".dta$", "", sub("tmp/est_", "", file))
  
  data <- read_dta(file)
  data <- data[-c(1:4), ]
  data <- data[-nrow(data), ] 
  result_list[[result_name]] <- data %>%
    mutate(var = as.numeric(gsub("\\D*(\\d+).*", "\\1", v1)),
           id = ifelse(grepl("1\\.(int)?East", v1), "East",
                       ifelse(grepl("0bn\\.(int)?East", v1), "West", NA)),
           year = 1905 + (var - 1) * 5)  %>%
    select(-v1) %>%
    rename(coef = v2, lower = v3, upper = v4)
}

for (file in file_list10) {
  
  result_name <- sub(".dta$", "", sub("tmp/est_", "", file))
  data <- read_dta(file)
  data <- data[-c(1:4), ]
  data <- data[-nrow(data), ] 
  result_list[[result_name]] <- data %>%
    mutate(var = as.numeric(gsub("\\D*(\\d+).*", "\\1", v1)),
           id = ifelse(grepl("1\\.East", v1), "East",
                       ifelse(grepl("0bn\\.East", v1), "West", NA)),
           year = 1905 + (var - 1) * 10)  %>%
    select(-v1) %>%
    rename(coef = v2, lower = v3, upper = v4)
}

for (result_name in names(result_list)) {
  
  result_data <- result_list[[result_name]]
  
  result_data$fig <- file_mapping[result_name]
  
  combined_results <- bind_rows(combined_results, result_data)
}

# Add fig 3 to the combined data frame
fig3 <- read_dta("tmp/est_fig3_dta.dta") 
fig3 <- fig3[-c(1:4), ]
data <- data[-nrow(data), ] 
fig3 <- mutate(fig3, var = as.numeric(gsub("\\D*(\\d+).*", "\\1", v1)),
               id = ifelse(grepl("1\\.(int)?East", v1), "East",
                           ifelse(grepl("0bn\\.(int)?East", v1), "West", NA))) %>%
  select(-v1) %>%
  rename(coef = v2, lower = v3, upper = v4)
fig3$fig <- "figuur3"

combined_results <- bind_rows(combined_results, fig3)

write.csv(combined_results, "tmp/models.csv", row.names = FALSE)

###################################################################
# Create figures
#Data
rm(list=ls())
d <- read_dta("ALLBUS90-2018_recoded.dta") 
models <- read_csv("tmp/models.csv") 

# Figure 1 
fig1 <- filter(models, fig == "figuur1") 
tmp <- data.frame(table(d$yob))
tmp$Var1 <- as.numeric(as.character(tmp$Var1))

fig <- ggplot(data = fig1) +
  geom_ribbon(aes(x=year, ymin=lower, ymax=upper, group=id),fill = "gray95") + 
  geom_line(aes(x=year, y=coef, linetype =id, group=id)) + 
  geom_line(aes(x=year,y=upper, group=id), colour = "gray90") + 
  geom_line(aes(x=year,y=lower, group=id), colour = "gray90") + 
  labs(y="Gender Traditional Attitude Scale (1 - 19)", x="Birth Year") + 
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "bottom") +
  geom_bar(data = tmp, aes(x = Var1, y = Freq/400), stat = "identity",
           fill="white", colour = "gray80") 
ggsave("Tables & Figures/figure1.png", width=7.75, height=4.75, dpi=900,fig) 

# Figure 2 
fig2 <- filter(models, fig == "figuur2_women"| fig == "figuur2_men") %>% 
  mutate(fig=recode(fig, 
                    `figuur2_women`="Women",
                    `figuur2_men`="Men"))
tmp <- data.frame(table(d$yob))
tmp$Var1 <- as.numeric(as.character(tmp$Var1))

fig<- ggplot(data = fig2) + 
  geom_ribbon(aes(x=year, ymin=lower, ymax=upper, group=id),fill = "gray95") +
  geom_line(aes(x=year, y=coef, linetype =id, group=id)) +
  geom_line(aes(x=year,y=upper, group=id), colour = "gray90") + 
  geom_line(aes(x=year,y=lower, group=id), colour = "gray90") + 
  facet_wrap(.~fig, scales="free_y") + 
  labs(y="Gender Traditional Attitude Scale (1 - 19)", x="Birth Year") + 
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "bottom") +
  geom_bar(data = tmp, aes(x=Var1, y=Freq/400), stat = "identity",
           fill="white", colour = "gray80")

ggsave("Tables & Figures/figure2.png", width=7.75, height=4.75, dpi=900,fig) 

# Figure 3
fig3 <- filter(models, fig == "figuur3") 
tmp <- data.frame(table(d$length))
tmp$Var1 <- as.numeric(as.character(tmp$Var1))

fig <- ggplot(data = fig3) + 
  geom_ribbon(aes(x=var, ymin=lower, ymax=upper, group=id),fill = "gray95") +
  geom_line(aes(x=var, y=coef, linetype =id, group=id)) + 
  geom_line(aes(x=var,y=upper, group=id), colour = "gray90") + 
  geom_line(aes(x=var,y=lower, group=id), colour = "gray90") + 
  labs(y="Gender Traditional Attitude Scale (1 - 19)", x="Length") + 
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "bottom")
ggsave("Tables & Figures/figure3.png", width=7.75, height=4.75, dpi=900,fig)

# Figure SI.1
figSI1 <- filter(models, fig == "figuur_si1") 
tmp <- data.frame(table(d$yob))
tmp$Var1 <- as.numeric(as.character(tmp$Var1))

fig <- ggplot(data = figSI1) + 
  geom_ribbon(aes(x=year, ymin=lower, ymax=upper, group=id),fill = "gray95") +
  geom_line(aes(x=year, y=coef, linetype =id, group=id)) + 
  geom_line(aes(x=year,y=upper, group=id), colour = "gray90") + 
  geom_line(aes(x=year,y=lower, group=id), colour = "gray90") + 
  labs(y="Gender Traditional Attitude Scale (1 - 19)", x="Birth Year") + 
  theme_minimal() +
  theme(legend.title=element_blank(), 
        legend.position = "bottom") +
  geom_bar(data = tmp, aes(x = Var1, y = Freq/400), stat = "identity",
           fill="white", colour = "gray80") 
ggsave("Tables & Figures/figureSI1.png", width=7.75, height=4.75, dpi=900,fig) 

# Figure SI.2 
figSI2 <- filter(models, fig == "figuur_si2") 
tmp <- data.frame(table(d$yob))
tmp$Var1 <- as.numeric(as.character(tmp$Var1))

fig <- ggplot(data = figSI2) +
  geom_ribbon(aes(x=year, ymin=lower, ymax=upper, group=id),fill = "gray95") +
  geom_line(aes(x=year, y=coef, linetype =id, group=id)) + 
  geom_line(aes(x=year,y=upper, group=id), colour = "gray90") + 
  geom_line(aes(x=year,y=lower, group=id), colour = "gray90") +
  labs(y="Gender Traditional Attitude Scale (1 - 19)", x="Birth Year") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        legend.position = "bottom") +
  geom_bar(data = tmp, aes(x = Var1, y = Freq/400), stat = "identity",
           fill="white", colour = "gray80") 
ggsave("Tables & Figures/figureSI2.png", width=7.75, height=4.75, dpi=900,fig) 

# Figure SI.3 
figSI3 <- filter(models, fig == "figuur_si3") 
tmp <- data.frame(table(d$yob))
tmp$Var1 <- as.numeric(as.character(tmp$Var1))

fig <- ggplot(data = figSI3) + 
  geom_ribbon(aes(x=year, ymin=lower, ymax=upper, group=id),fill = "gray95") + 
  geom_line(aes(x=year, y=coef, linetype =id, group=id)) +  
  geom_line(aes(x=year,y=upper, group=id), colour = "gray90") +
  geom_line(aes(x=year,y=lower, group=id), colour = "gray90") +
  labs(y="Gender Traditional Attitude Scale (1 - 19)", x="Birth Year") + 
  theme_minimal() +
  theme(legend.title=element_blank(), 
        legend.position = "bottom") +
  geom_bar(data = tmp, aes(x = Var1, y = Freq/400), stat = "identity",
           fill="white", colour = "gray80") 
ggsave("Tables & Figures/figureSI3.png", width=7.75, height=4.75, dpi=900,fig) 

# Figure SI4
figSI4 <- filter(models, fig == "figuur_si4") 
tmp <- data.frame(table(d$yob))
tmp$Var1 <- as.numeric(as.character(tmp$Var1))

fig <- ggplot(data = figSI4) + 
  geom_ribbon(aes(x=year, ymin=lower, ymax=upper, group=id),fill = "gray95") + 
  geom_line(aes(x=year, y=coef, linetype =id, group=id)) + 
  geom_line(aes(x=year,y=upper, group=id), colour = "gray90") + 
  geom_line(aes(x=year,y=lower, group=id), colour = "gray90") +
  labs(y="Gender Traditional Attitude Scale (1 - 19)", x="Birth Year") + 
  theme_minimal() +
  theme(legend.title=element_blank(), 
        legend.position = "bottom") +
  geom_bar(data = tmp, aes(x = Var1, y = Freq/400), stat = "identity",
           fill="white", colour = "gray80") 
ggsave("Tables & Figures/figureSI4.png", width=7.75, height=4.75, dpi=900,fig)
