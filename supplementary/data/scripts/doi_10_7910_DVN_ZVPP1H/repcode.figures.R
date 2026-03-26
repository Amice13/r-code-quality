# Load ggplot2 package for graphics
require(ggplot2)

# Read data into R for Figure 2 (fig2data.csv) and Appendix Figure (figappdata.csv)
fig2data <- read.csv("fig2data.csv")
figappdata <- read.csv("figappdata.csv")

# Rescale turnout-proportion data to be comparable to turnout-percentage data
fig2data[fig2data$model == "Column IV Model", c("est", "stderr", "lb", "ub")] <- 100*fig2data[fig2data$model == "Column IV Model", c("est", "stderr", "lb", "ub")]

# Create plot window corresponding to Figure 2 size
dev.new(width = 7, height = 4)

# Generate plot for Figure 2
ggplot(fig2data, aes(x = factor(agegroup, labels = c("Under 25", "25-34", "35-49", "50-61", "62 & Over")), y = est, ymin = lb, ymax = ub, shape = as.factor(timezone), colour = as.factor(timezone))) + geom_pointrange(position = position_dodge(width = -0.3)) + facet_grid(. ~ model) + theme_minimal() + theme(legend.position = "none") + scale_shape_manual(values = c(1, 16)) + scale_color_hue(h = c(60, 300), c = 200) + xlab("Age Group") + ylab("Estimated Percent Turnout") + annotate("text", x = 2.4, y = 46.2, label = "Central", colour = "magenta") + annotate("text", x = 3.65, y = 45.7, label = "Eastern", colour = "orange")

# Create index to rows of output looking at a specific age-party group across time zones
figappdata$include <- substr(figappdata$code1, 4, 6) == substr(figappdata$code2, 4, 6)

# Exclude other rows from data
figappdata <- figappdata[figappdata$include, ]

# Extract information on age and party from output codes
figappdata$age <- as.numeric(substr(figappdata$code1, 4, 4))
figappdata$party <- substr(figappdata$code1, 6, 6)

# Create plot window corresponding to Appendix Figure size
dev.new(width = 7.5, height = 5.25)

# Generate plot for Appendix Figure
ggplot(figappdata, aes(x = factor(age, labels = c("Under 25", "25-34", "35-49", "50-61", "62 & Over")), y = est, ymin = lb, ymax = ub, shape = party, linetype = party, color = party)) + geom_pointrange(position = position_dodge(width = 0.3)) + theme_minimal() + theme(legend.position = "none") + scale_color_hue(h = c(240, 0), l = 50) + xlab("Age Group") + ylab("Estimated Percent Higher Turnout in Central Time Zone") + annotate("text", x = 2.6, y = 0.8, label = "Democrat", colour = "blue3") + annotate("text", x = 3.45, y = 0.8, label = "Republican", colour = "red3") + annotate("text", x = 3.2, y = -1.8, label = "Other", colour = "green4")