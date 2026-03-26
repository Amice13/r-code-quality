# 加载必要的库
library(dplyr)

# 读取数据，确保文件路径正确，并且指定UTF-8编码
data <- read.csv("data/data.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# 查看数据集的基本结构和统计信息
str(data)
summary(data)

# 查看每个变量的缺失值数量
sapply(data, function(x) sum(is.na(x)))

# 数据清洗
# 去除注视时间小于80毫秒的数据点（假设单位是毫秒）
# 去除缺失值
data_cleaned <- data %>%
  filter(`插图区每个点平均注视时长` >= 80) %>%
  filter(complete.cases(`插图区每个点平均注视时长`, `插图区注视次数`))

# 检查清洗后的数据集是否为空
if(nrow(data_cleaned) == 0) {
  stop("清洗后的数据集为空，请检查数据和清洗条件。")
}

# 数据标准化
# 标准化注视时间和注视次数
mean_fixation_time <- mean(data_cleaned$`插图区每个点平均注视时长`, na.rm = TRUE)
sd_fixation_time <- sd(data_cleaned$`插图区每个点平均注视时长`, na.rm = TRUE)
mean_fixation_count <- mean(data_cleaned$`插图区注视次数`, na.rm = TRUE)
sd_fixation_count <- sd(data_cleaned$`插图区注视次数`, na.rm = TRUE)

data_normalized <- data_cleaned %>%
  mutate(`标准化插图区每个点平均注视时长` = (`插图区每个点平均注视时长` - mean_fixation_time) / sd_fixation_time,
         `标准化插图区注视次数` = (`插图区注视次数` - mean_fixation_count) / sd_fixation_count)

# 查看预处理后的数据
head(data_normalized)