# -
r语言折线图代码
# 加载所需包
library(readxl)      # 读取Excel文件
library(ggplot2)     # 绘图
library(tidyr)       # 数据转换（长格式）
library(dplyr)       # 数据汇总

# ---------- 读取数据 ----------
# 请将工作目录设置为包含Excel文件的文件夹，或使用完整路径
control <- read_excel("对照组血钾水平.xlsx", sheet = 1, col_names = FALSE)
experiment <- read_excel("实验组血钾水平.xlsx", sheet = 1, col_names = FALSE)

# 提取第7~10列（对应0、1、3、6月），并命名
control_data <- control[, 1:4]
experiment_data <- experiment[, 1:4]
names(control_data) <- c("0", "1", "3", "6")
names(experiment_data) <- c("0", "1", "3", "6")

# ---------- 转换为长格式 ----------
control_long <- pivot_longer(control_data, cols = everything(), 
                             names_to = "Time", values_to = "K")
experiment_long <- pivot_longer(experiment_data, cols = everything(), 
                                names_to = "Time", values_to = "K")

# 添加组别标识
control_long$Group <- "Control"
experiment_long$Group <- "Experiment"

# 合并两组数据
all_data <- rbind(control_long, experiment_long)
all_data$Time <- factor(all_data$Time, levels = c("0", "1", "3", "6"))  # 保持顺序

# ---------- 计算均值和标准误 ----------
summary_stats <- all_data %>%
  group_by(Group, Time) %>%
  summarise(
    Mean = mean(K, na.rm = TRUE),
    SD = sd(K, na.rm = TRUE),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

# 查看汇总表（可输出到控制台）
print(summary_stats)

# ---------- 绘制折线图 ----------
p <- ggplot(summary_stats, aes(x = Time, y = Mean, 
                               group = Group, color = Group, shape = Group)) +
  geom_line(linewidth = 0.4) +                      # 用 linewidth 替代 size
  geom_point(size = 1) +                            # 点大小保持不变（size仍适用于点）
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.2, linewidth = 0.4) +     # 误差棒线宽用 linewidth
  labs(
    x = "Time (months)",
    y = "Serum Potassium (mmol/L)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),
    axis.line = element_line(linewidth = 0.5),      # 坐标轴线宽用 linewidth
    panel.grid = element_blank()
  ) +
  scale_color_manual(values = c("Control" = "#0072B2", "Experiment" = "#D55E00"))  # 更学术的颜色

# 显示图形
print(p)
# ---------- 保存为高分辨率图片（出版质量） ----------
ggsave("Potassium_trend.png", p, dpi = 300, width = 6, height = 4, units = "in")
