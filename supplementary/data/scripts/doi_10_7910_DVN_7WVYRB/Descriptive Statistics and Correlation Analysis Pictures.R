# 加载必要的库
library(ggplot2)
library(dplyr)
library(extrafont)  # 加载字体包

# 读取数据
# 假设 data.csv 的列名为 "水平组"、"图类型"、"得分"、"总注视时间"、"图区注视时长"、"图区注视次数"
data <- read.csv("data/data.csv", stringsAsFactors = TRUE)

# 箱线图 - 展示得分在不同水平组和图类型下的分布
ggplot(data, aes(x = 图类型, y = 得分, fill = 水平组)) +
  geom_boxplot(outlier.size = 1, outlier.colour = "red") +
  labs(
    title = "不同水平组和图类型下的得分分布",
    x = "图类型",
    y = "得分",
    fill = "水平组"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", family = "SimSun"),  # 设置宋体字，12号字体
    axis.text = element_text(size = 12, family = "SimSun"),  # 设置宋体字，12号字体
    axis.title = element_text(size = 12, family = "SimSun"),  # 设置宋体字，12号字体
    legend.title = element_text(size = 12, family = "SimSun"),  # 设置宋体字，12号字体
    legend.text = element_text(size = 12, family = "SimSun")  # 设置宋体字，12号字体
  ) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

# 箱线图 - 展示总注视时间在不同水平组和图类型下的分布
ggplot(data, aes(x = 图类型, y = 总注视时间, fill = 水平组)) +
  geom_boxplot(outlier.size = 1, outlier.colour = "red") +
  labs(
    title = "不同水平组和图类型下的总注视时间分布",
    x = "图类型",
    y = "总注视时间（秒）",
    fill = "水平组"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", family = "SimSun"),
    axis.text = element_text(size = 12, family = "SimSun"),
    axis.title = element_text(size = 12, family = "SimSun"),
    legend.title = element_text(size = 12, family = "SimSun"),
    legend.text = element_text(size = 12, family = "SimSun")
  ) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

# 箱线图 - 展示图区注视时长在不同水平组和图类型下的分布
ggplot(data, aes(x = 图类型, y = 图区注视时长, fill = 水平组)) +
  geom_boxplot(outlier.size = 1, outlier.colour = "red") +
  labs(
    title = "不同水平组和图类型下的图区注视时长分布",
    x = "图类型",
    y = "图区注视时长（秒）",
    fill = "水平组"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", family = "SimSun"),
    axis.text = element_text(size = 12, family = "SimSun"),
    axis.title = element_text(size = 12, family = "SimSun"),
    legend.title = element_text(size = 12, family = "SimSun"),
    legend.text = element_text(size = 12, family = "SimSun")
  ) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))

# 箱线图 - 展示图区注视次数在不同水平组和图类型下的分布
ggplot(data, aes(x = 图类型, y = 图区注视次数, fill = 水平组)) +
  geom_boxplot(outlier.size = 1, outlier.colour = "red") +
  labs(
    title = "不同水平组和图类型下的图区注视次数分布",
    x = "图类型",
    y = "图区注视次数",
    fill = "水平组"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold", family = "SimSun"),
    axis.text = element_text(size = 12, family = "SimSun"),
    axis.title = element_text(size = 12, family = "SimSun"),
    legend.title = element_text(size = 12, family = "SimSun"),
    legend.text = element_text(size = 12, family = "SimSun")
  ) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))
