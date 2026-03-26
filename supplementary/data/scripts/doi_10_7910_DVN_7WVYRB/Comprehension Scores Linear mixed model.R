# -------------- 加载必要的库 -----------------
library(lme4)
library(lmerTest)
library(dplyr)
library(ggplot2)

# -------------- 读取数据 -----------------
data <- read.csv("data/data_en1.csv", skip = 1, header = FALSE, stringsAsFactors = TRUE)

# 手动指定列名
colnames(data) <- c("Subject", "Proficiency_Group", "Illustration_Type", "Score",
                    "total_fixation_duration", "Illustration_Area_Duration",
                    "V7", "V8", "Illustration_Area_Fixation_Count",
                    "V10", "V11", "V12", "V13")

# 因子化并固定顺序
target_order <- c("Representational", "Explanatory", "Transformational", "Organizational")
data$Proficiency_Group <- factor(data$Proficiency_Group,
                                 levels = c("Low_Proficiency_Group", "High_Proficiency_Group"))
data$Illustration_Type <- factor(data$Illustration_Type, levels = target_order)

# -------------- 模型拟合 -----------------
model <- lmer(Score ~ Proficiency_Group * Illustration_Type + (1 | Subject), data = data)
summary(model)

# -------------- 提取结果并保存 -----------------
fixed_effects <- summary(model)$coefficients
random_effects  <- VarCorr(model)

write.csv(as.data.frame(fixed_effects), "fixed_effects_understanding_score.csv", row.names = TRUE)

# -------------- 模型诊断（简单） -----------------
par(mfrow = c(1, 2))
plot(model,  main = "Residuals vs Fitted")
qqnorm(residuals(model), main = "Q-Q Plot")
qqline(residuals(model))

# -------------- 分组柱状图 & 保存 -----------------
if (!dir.exists("plots")) dir.create("plots")

grouped_data <- data %>%
  group_by(Illustration_Type, Proficiency_Group) %>%
  summarise(
    平均得分 = mean(Score, na.rm = TRUE),
    标准误   = sd(Score, na.rm = TRUE) / sqrt(n()),
    .groups  = "drop"
  )

p <- ggplot(grouped_data,
            aes(x = Illustration_Type, y = 平均得分, fill = Proficiency_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = 平均得分 - 标准误, ymax = 平均得分 + 标准误),
                position = position_dodge(0.7), width = 0.25) +
  labs(x = "Illustration Type",
       y = "Average Score",
       fill = "Proficiency Group") +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  ) +
  scale_fill_manual(values = c("#1f77b4","#ff7f0e"))   # Low=橙, High=蓝

# 显示图形
print(p)

# 保存图形
ggsave("plots/Grouped_Bar_Plot_Score.png", plot = p, width = 8, height = 6, dpi = 300)