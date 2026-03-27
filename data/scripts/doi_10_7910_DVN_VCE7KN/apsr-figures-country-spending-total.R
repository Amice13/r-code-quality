

# Read csv file
spend.combined <- read.csv(here("Data", "spending-overseas-20191205.csv"))

# Creates customized palette to accomodate larger number of groups
colourCount = length(unique(spend.combined$country))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Line chart by country
# Note I needed a custom Brewer palette here because there were too many countries
p1 <- ggplot(data = spend.combined, aes(x = year, y = total, group = as.factor(country), colour = as.factor(country), linetype = as.factor(country))) +
  geom_line(size = .75)+
  theme_bw() +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "",
       y = "Millions of current dollars",
       x = "Year",
       color = "Country",
       linetype = "Country") +
  scale_colour_manual(values = getPalette(colourCount)) 

ggsave(here("Figures", "apsr-figure-spending-country.pdf"), width = 8, height = 5, units = "in")


p1.title <- p1 + 
  labs(title = "Total cost of maintaining US military presence") +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("Figures", "figure-spending-country-title.pdf"), width = 8, height = 5, units = "in")
ggsave(here("Figures", "figure-spending-country-title.png"), width = 8, height = 5, units = "in")
