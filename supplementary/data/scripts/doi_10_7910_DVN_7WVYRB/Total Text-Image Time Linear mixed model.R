# -------------- 加载必要的库 -----------------
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)

# 创建 plots 文件夹
if (!dir.exists("plots")) dir.create("plots")

# -------------- 读取数据 -----------------
data <- read.csv("data/data_en1.csv", skip = 1, header = FALSE, stringsAsFactors = TRUE)

colnames(data) <- c("Subject", "Proficiency_Group", "Illustration_Type", "Score",
                    "total_fixation_duration", "Illustration_Area_Duration",
                    "V7", "V8", "Illustration_Area_Fixation_Count",
                    "V10", "V11", "V12", "V13")

# 因子化并固定顺序
data$Proficiency_Group <- factor(data$Proficiency_Group,
                                 levels = c("Low_Proficiency_Group",
                                            "High_Proficiency_Group"))  # 低→左，高→右
data$Illustration_Type <- factor(data$Illustration_Type,
                                 levels = c("Representational",
                                            "Explanatory",
                                            "Transformational",
                                            "Organizational"))

# -------------- 线性混合模型 -----------------
model_total_fixation <- lmer(
  total_fixation_duration ~ Proficiency_Group * Illustration_Type + (1 | Subject),
  data = data
)
summary(model_total_fixation)

# -------------- 保存结果 -----------------
fixed_effects <- summary(model_total_fixation)$coefficients
random_effects <- VarCorr(model_total_fixation)

write.csv(as.data.frame(fixed_effects),
          "fixed_effects_total_fixation_time.csv", row.names = TRUE)
write.csv(as.data.frame(random_effects),
          "random_effects_total_fixation_time.csv", row.names = TRUE)

# -------------- 模型诊断图 -----------------
png("plots/Residuals_vs_Fitted_total_fixation_duration.png", width = 800, height = 600)
plot(fitted(model_total_fixation), residuals(model_total_fixation),
     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "#1f77b4")
abline(h = 0, lty = 2, col = "red")
dev.off()

png("plots/QQ_Plot_total_fixation_duration.png", width = 800, height = 800)
qqnorm(residuals(model_total_fixation), main = "Q-Q Plot of Residuals",
       pch = 19, col = "#ff7f0e")
qqline(residuals(model_total_fixation), col = "red", lwd = 2)
dev.off()

# -------------- 计算均值与标准误 -----------------
grouped_data <- data %>%
  group_by(Illustration_Type, Proficiency_Group) %>%
  summarise(
    average_total_fixation_duration = mean(total_fixation_duration, na.rm = TRUE),
    标准误 = sd(total_fixation_duration, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# -------------- 绘制并保存柱状图 -----------------
grouped_bar_plot <- ggplot(
  grouped_data,
  aes(x = Illustration_Type, y = average_total_fixation_duration, fill = Proficiency_Group)
) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(
    aes(ymin = average_total_fixation_duration - 标准误,
        ymax = average_total_fixation_duration + 标准误),
    position = position_dodge(0.7), width = 0.25
  ) +
  labs(
    x    = "Illustration Type",
    y    = "Average Total Fixation Duration (seconds)",
    fill = "Proficiency Group"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text    = element_text(size = 12),
    axis.title   = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  ) +
  scale_fill_manual(values = c("#1f77b4","#ff7f0e"))   # Low→橙（左），High→蓝（右）

print(grouped_bar_plot)
ggsave("plots/Grouped_Bar_Plot_total_fixation_duration.png",
       plot = grouped_bar_plot, width = 8, height = 6, dpi = 300)