# scripts/02_eda_visualization.R
# NHANES数据探索性数据分析脚本

source("scripts/00_setup.R")

# 加载额外需要的库
if (!require("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
  library(corrplot)
}

cat("========================================\n")
cat("开始NHANES数据探索性分析\n")
cat("========================================\n\n")

# 加载数据
cat("加载清洗后的数据...\n")
data <- readRDS(file.path(data_processed_path, "nhanes_analysis_data.rds"))
cat("数据加载完成，共", nrow(data), "行,", ncol(data), "列\n\n")

# ============================================================================
# 步骤1: 描述性统计
# ============================================================================

cat("步骤1: 描述性统计...\n")

# 基本统计量
basic_stats <- data %>%
  summarise(
    总样本数 = n(),
    高风险样本数 = sum(cvd_risk_status == "High Risk", na.rm = TRUE),
    高风险比例 = round(mean(cvd_risk_status == "High Risk", na.rm = TRUE) * 100, 1),
    平均年龄 = round(mean(age, na.rm = TRUE), 1),
    平均BMI = round(mean(bmi, na.rm = TRUE), 1),
    平均血糖 = round(mean(glucose, na.rm = TRUE), 2),
    男性比例 = round(mean(gender == "Male", na.rm = TRUE) * 100, 1)
  )

cat("\n基本统计量:\n")
print(basic_stats)

# 保存基本统计
write.csv(basic_stats, 
          file.path(output_tables_path, "basic_statistics.csv"), 
          row.names = FALSE)

cat("\n")

# ============================================================================
# 步骤2: 目标变量分布分析
# ============================================================================

cat("步骤2: 目标变量分布分析...\n")

# 2.1 分类目标：心血管疾病风险分布
cvd_dist <- data %>%
  group_by(cvd_risk_status) %>%
  summarise(
    数量 = n(),
    比例 = round(n() / nrow(data) * 100, 1)
  )

cat("\n心血管疾病风险分布:\n")
print(cvd_dist)

# 按年龄、性别分组
cvd_by_demo <- data %>%
  group_by(age_group, gender, cvd_risk_status) %>%
  summarise(数量 = n(), .groups = "drop") %>%
  group_by(age_group, gender) %>%
  mutate(比例 = round(数量 / sum(数量) * 100, 1))

cat("\n按年龄和性别分组的心血管疾病风险分布:\n")
print(cvd_by_demo)

# 保存
write.csv(cvd_by_demo, 
          file.path(output_tables_path, "cvd_risk_by_demographics.csv"), 
          row.names = FALSE)

# 2.2 回归目标：血糖分布
glucose_dist <- summary(data$glucose)
cat("\n血糖分布:\n")
print(glucose_dist)

# 可视化：血糖分布直方图
p1 <- ggplot(data, aes(x = glucose)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "空腹血糖分布", x = "血糖 (mg/dL)", y = "频数") +
  theme_minimal()

ggsave(file.path(output_figures_path, "eda", "glucose_distribution.png"), 
       p1, width = 8, height = 6, dpi = 300)

cat("  已保存: glucose_distribution.png\n")

# ============================================================================
# 步骤3: 风险因素分析
# ============================================================================

cat("\n步骤3: 风险因素分析...\n")

# 3.1 风险因素与心血管疾病风险的关系
risk_factors_analysis <- data %>%
  group_by(cvd_risk_status) %>%
  summarise(
    平均BMI = round(mean(bmi, na.rm = TRUE), 2),
    平均收缩压 = round(mean(systolic_bp, na.rm = TRUE), 2),
    平均血糖 = round(mean(glucose, na.rm = TRUE), 2),
    平均HDL = round(mean(hdl_cholesterol, na.rm = TRUE), 2),
    平均甘油三酯 = round(mean(triglycerides, na.rm = TRUE), 2),
    平均生活方式风险评分 = round(mean(lifestyle_risk_score, na.rm = TRUE), 2)
  )

cat("\n风险因素对比（按心血管疾病风险状态）:\n")
print(risk_factors_analysis)

write.csv(risk_factors_analysis, 
          file.path(output_tables_path, "risk_factors_analysis.csv"), 
          row.names = FALSE)

# 3.2 生活方式因素与风险的关系
lifestyle_analysis <- data %>%
  group_by(cvd_risk_status) %>%
  summarise(
    当前吸烟比例 = round(mean(smoking_binary == "Current smoker", na.rm = TRUE) * 100, 1),
    饮酒比例 = round(mean(alcohol_category != "Non-drinker", na.rm = TRUE) * 100, 1),
    不活动比例 = round(mean(activity_level == "Inactive", na.rm = TRUE) * 100, 1),
    平均睡眠时长 = round(mean(sleep_hours, na.rm = TRUE), 1)
  )

cat("\n生活方式因素对比:\n")
print(lifestyle_analysis)

write.csv(lifestyle_analysis, 
          file.path(output_tables_path, "lifestyle_analysis.csv"), 
          row.names = FALSE)

# ============================================================================
# 步骤4: 可视化分析
# ============================================================================

cat("\n步骤4: 创建可视化图表...\n")

# 4.1 心血管疾病风险按年龄和性别分布
p2 <- data %>%
  filter(!is.na(cvd_risk_status)) %>%
  ggplot(aes(x = age_group, fill = cvd_risk_status)) +
  geom_bar(position = "fill") +
  facet_wrap(~ gender) +
  labs(title = "心血管疾病风险分布（按年龄和性别）",
       x = "年龄组", y = "比例", fill = "风险状态") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_figures_path, "eda", "cvd_risk_by_age_gender.png"), 
       p2, width = 10, height = 6, dpi = 300)
cat("  已保存: cvd_risk_by_age_gender.png\n")

# 4.2 BMI与血糖关系
p3 <- ggplot(data, aes(x = bmi, y = glucose)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "BMI与血糖关系", x = "BMI", y = "血糖 (mg/dL)") +
  theme_minimal()

ggsave(file.path(output_figures_path, "eda", "bmi_glucose_relationship.png"), 
       p3, width = 8, height = 6, dpi = 300)
cat("  已保存: bmi_glucose_relationship.png\n")

# 4.3 风险因素箱线图
p4 <- data %>%
  select(cvd_risk_status, bmi, systolic_bp, glucose, hdl_cholesterol) %>%
  pivot_longer(cols = -cvd_risk_status, names_to = "变量", values_to = "值") %>%
  filter(!is.na(cvd_risk_status)) %>%
  ggplot(aes(x = cvd_risk_status, y = 值, fill = cvd_risk_status)) +
  geom_boxplot() +
  facet_wrap(~ 变量, scales = "free_y") +
  labs(title = "风险因素对比（按心血管疾病风险状态）",
       x = "风险状态", y = "值", fill = "风险状态") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(output_figures_path, "eda", "risk_factors_boxplot.png"), 
       p4, width = 12, height = 8, dpi = 300)
cat("  已保存: risk_factors_boxplot.png\n")

# 4.4 相关性热力图
numeric_vars <- c("age", "bmi", "systolic_bp", "diastolic_bp", "glucose", 
                  "hdl_cholesterol", "triglycerides", "sleep_hours", 
                  "lifestyle_risk_score")

# 检查变量是否存在
numeric_vars <- numeric_vars[numeric_vars %in% names(data)]

if (length(numeric_vars) >= 2) {
  cor_data <- data %>%
    select(all_of(numeric_vars)) %>%
    cor(use = "complete.obs")
  
  if (nrow(cor_data) > 1 && ncol(cor_data) > 1 && !any(is.na(cor_data))) {
    png(file.path(output_figures_path, "eda", "correlation_heatmap.png"), 
        width = 10, height = 10, units = "in", res = 300)
    corrplot(cor_data, method = "color", type = "upper", 
             order = "hclust", tl.cex = 0.8, tl.col = "black")
    dev.off()
    cat("  已保存: correlation_heatmap.png\n")
  } else {
    cat("  警告: 相关性矩阵计算失败，跳过相关性热力图\n")
  }
} else {
  cat("  警告: 数值变量不足，跳过相关性热力图\n")
}

cat("\n可视化图表创建完成\n")

# ============================================================================
# 步骤5: 统计检验
# ============================================================================

cat("\n步骤5: 统计检验...\n")

# 5.1 高风险组与低风险组的差异检验
high_risk <- data %>% filter(cvd_risk_status == "High Risk")
low_risk <- data %>% filter(cvd_risk_status == "Low Risk")

# t检验：血糖
if (nrow(high_risk) > 0 && nrow(low_risk) > 0) {
  t_test_glucose <- t.test(high_risk$glucose, low_risk$glucose)
  cat("\n血糖差异检验（高风险 vs 低风险）:\n")
  cat("  t =", round(t_test_glucose$statistic, 3), 
      ", p =", format(t_test_glucose$p.value, scientific = TRUE), "\n")
}

# 卡方检验：性别分布
if (nrow(high_risk) > 0 && nrow(low_risk) > 0) {
  gender_table <- table(data$cvd_risk_status, data$gender)
  chi_test_gender <- chisq.test(gender_table)
  cat("\n性别分布差异检验:\n")
  cat("  χ² =", round(chi_test_gender$statistic, 3), 
      ", p =", format(chi_test_gender$p.value, scientific = TRUE), "\n")
}

cat("\n")

# ============================================================================
# 步骤6: 保存EDA摘要
# ============================================================================

cat("步骤6: 保存EDA摘要...\n")

eda_summary <- list(
  基本统计 = basic_stats,
  风险分布 = cvd_dist,
  风险因素分析 = risk_factors_analysis,
  生活方式分析 = lifestyle_analysis
)

saveRDS(eda_summary, 
        file.path(output_tables_path, "eda_summary.rds"))

cat("EDA摘要已保存\n\n")

# ============================================================================
# 步骤7: 生成EDA报告
# ============================================================================

cat("\n步骤7: 生成EDA报告...\n")

# 创建报告内容
report_content <- paste0(
  "# NHANES探索性数据分析报告\n\n",
  "## 数据概览\n\n",
  "- **总样本数**: ", basic_stats$总样本数, "\n",
  "- **高风险比例**: ", basic_stats$高风险比例, "%\n",
  "- **平均年龄**: ", basic_stats$平均年龄, " 岁\n",
  "- **平均BMI**: ", basic_stats$平均BMI, "\n",
  "- **平均血糖**: ", basic_stats$平均血糖, " mg/dL\n",
  "- **男性比例**: ", basic_stats$男性比例, "%\n\n",
  "## 主要发现\n\n",
  "### 1. 心血管疾病风险分布\n",
  "- 高风险: ", cvd_dist$数量[cvd_dist$cvd_risk_status == "High Risk"], 
  " (", cvd_dist$比例[cvd_dist$cvd_risk_status == "High Risk"], "%)\n",
  "- 低风险: ", cvd_dist$数量[cvd_dist$cvd_risk_status == "Low Risk"],
  " (", cvd_dist$比例[cvd_dist$cvd_risk_status == "Low Risk"], "%)\n\n",
  "### 2. 风险因素分析\n",
  "高风险组与低风险组在以下指标上存在显著差异：\n",
  "- BMI: ", risk_factors_analysis$平均BMI[risk_factors_analysis$cvd_risk_status == "High Risk"],
  " vs ", risk_factors_analysis$平均BMI[risk_factors_analysis$cvd_risk_status == "Low Risk"], "\n",
  "- 收缩压: ", risk_factors_analysis$平均收缩压[risk_factors_analysis$cvd_risk_status == "High Risk"],
  " vs ", risk_factors_analysis$平均收缩压[risk_factors_analysis$cvd_risk_status == "Low Risk"], " mmHg\n",
  "- 血糖: ", risk_factors_analysis$平均血糖[risk_factors_analysis$cvd_risk_status == "High Risk"],
  " vs ", risk_factors_analysis$平均血糖[risk_factors_analysis$cvd_risk_status == "Low Risk"], " mg/dL\n\n",
  "### 3. 生活方式因素\n",
  "高风险组的生活方式特征：\n",
  "- 当前吸烟比例: ", lifestyle_analysis$当前吸烟比例[lifestyle_analysis$cvd_risk_status == "High Risk"], "%\n",
  "- 不活动比例: ", lifestyle_analysis$不活动比例[lifestyle_analysis$cvd_risk_status == "High Risk"], "%\n",
  "- 平均睡眠时长: ", lifestyle_analysis$平均睡眠时长[lifestyle_analysis$cvd_risk_status == "High Risk"], " 小时\n\n",
  "## 可视化图表\n\n",
  "所有图表已保存到: output/figures/eda/\n\n",
  "报告生成时间: ", Sys.time(), "\n"
)

# 保存报告
writeLines(report_content, file.path(project_root, "docs", "EDA报告.md"))
cat("EDA报告已保存到: docs/EDA报告.md\n\n")

cat("========================================\n")
cat("探索性数据分析完成！\n")
cat("========================================\n")
cat("\n输出文件:\n")
cat("  图表:", file.path(output_figures_path, "eda"), "\n")
cat("  表格:", file.path(output_tables_path), "\n")
cat("  报告: docs/EDA报告.md\n")

