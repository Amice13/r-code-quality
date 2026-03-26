# 加载必要的库
library(dplyr)

# 读取数据
# 假设文件名为 "data.csv"，列名为 "水平组"、"插图类型"、"得分"、"总注视时间"、"插图区注视时长"、"插图区注视次数"
data <- read.csv("data/data.csv", stringsAsFactors = TRUE)

# 分组计算描述性统计量
descriptive_stats <- data %>%
  group_by(水平组, 插图类型) %>%
  summarise(
    理解得分 = paste0(round(mean(得分, na.rm = TRUE), 2), " ± ", round(sd(得分, na.rm = TRUE), 2)),
    总注视时间 = paste0(round(mean(总注视时间, na.rm = TRUE), 2), " ± ", round(sd(总注视时间, na.rm = TRUE), 2)),
    插图区注视时长 = paste0(round(mean(插图区注视时长, na.rm = TRUE), 2), " ± ", round(sd(插图区注视时长, na.rm = TRUE), 2)),
    插图区注视次数 = paste0(round(mean(插图区注视次数, na.rm = TRUE), 2), " ± ", round(sd(插图区注视次数, na.rm = TRUE), 2))
  )

# 打印结果为表格形式
print(descriptive_stats)

# 如果需要将结果保存为 CSV 文件
write.csv(descriptive_stats, "descriptive_stats_table.csv", row.names = FALSE)
