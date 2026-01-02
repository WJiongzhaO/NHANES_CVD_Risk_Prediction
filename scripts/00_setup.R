# scripts/00_setup.R
# NHANES项目环境设置脚本

# 安装必要的包
required_packages <- c(
  "haven", "dplyr", "tidyr", "ggplot2", 
  "summarytools", "naniar", "corrr", "ggraph",
  "mgcv", "randomForest", "caret", "pROC",
  "rpart", "rpart.plot", "glmnet", "xgboost",
  "smotefamily", "iml", "SHAPforxgboost", "e1071"
)

# 检查并安装缺失的包
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, dependencies = TRUE)
}

# 加载所有包
suppressPackageStartupMessages({
  library(haven)        # 读取SAS数据
  library(dplyr)        # 数据操作
  library(tidyr)       # 数据整理
  library(ggplot2)     # 数据可视化
  library(summarytools) # 描述性统计
  library(naniar)      # 缺失值分析
  library(corrr)       # 相关性分析
  library(ggraph)      # 网络图
  library(mgcv)        # 广义加性模型
  library(randomForest) # 随机森林
  library(caret)       # 分类回归训练
  library(pROC)        # ROC曲线
  library(rpart)       # 决策树
  library(rpart.plot)  # 决策树可视化
  library(glmnet)      # 正则化回归
  library(xgboost)     # 梯度提升
})

# 设置图形主题
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                  axis.title = element_text(face = "bold"),
                  legend.position = "bottom"))

# 设置文件路径
project_root <- "F:/NHANES_Metabolic_Syndrome_Analysis"
# 检查项目根目录是否存在
if (!dir.exists(project_root)) {
  # 如果绝对路径不存在，尝试当前工作目录
  project_root <- getwd()
  cat("警告: 使用当前工作目录作为项目根目录:", project_root, "\n")
}
data_raw_path <- file.path(project_root, "data", "raw","")
data_processed_path <- file.path(project_root, "data", "processed","")
output_figures_path <- file.path(project_root, "output", "figures","")
output_tables_path <- file.path(project_root, "output", "tables","")
output_models_path <- file.path(project_root, "output", "models","")

# 创建必要的目录
dir.create(data_raw_path, recursive = TRUE, showWarnings = FALSE)
dir.create(data_processed_path, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figures_path, recursive = TRUE, showWarnings = FALSE)
dir.create(output_tables_path, recursive = TRUE, showWarnings = FALSE)
dir.create(output_models_path, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_models_path, "classification"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_models_path, "regression"), recursive = TRUE, showWarnings = FALSE)

# 设置随机种子
set.seed(123)

cat("NHANES项目环境设置完成！\n")

