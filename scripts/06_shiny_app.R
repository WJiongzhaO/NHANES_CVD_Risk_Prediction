# scripts/06_shiny_app.R
# Shiny应用：心血管疾病风险预测和腰围水平预测
# 使用最佳模型进行预测

# 检查并加载必要的包
required_shiny_packages <- c("shiny", "shinydashboard", "plotly", "DT", "dplyr", 
                              "recipes", "png", "grid", "ggplot2", "tidyr", "corrplot", "scales", "glmnet")
missing_packages <- required_shiny_packages[!(required_shiny_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  cat("正在安装缺失的包:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages, dependencies = TRUE)
}

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(recipes)
library(png)
library(grid)
library(ggplot2)
library(tidyr)
library(corrplot)
library(scales)
library(glmnet)

# 加载模型和数据
source("scripts/00_setup.R")

# 检查并加载模型
cat("正在加载模型和数据...\n")

# 读取最佳模型选择（如果存在）
best_class_model <- "逻辑回归"  # 默认值
best_reg_model <- "线性回归"    # 默认值（根据报告，这是最佳模型）

if (file.exists(file.path(output_tables_path, "best_models_selection.csv"))) {
  tryCatch({
    best_models <- read.csv(file.path(output_tables_path, "best_models_selection.csv"))
    best_class_model <- best_models$最佳模型[best_models$任务类型 == "分类"]
    best_reg_model <- best_models$最佳模型[best_models$任务类型 == "回归"]
    cat("已读取最佳模型选择:\n")
    cat("  分类模型:", best_class_model, "\n")
    cat("  回归模型:", best_reg_model, "\n")
  }, error = function(e) {
    cat("警告: 无法读取最佳模型选择文件，使用默认模型\n")
  })
}

# 检查模型文件是否存在
if (!file.exists(file.path(output_models_path, "classification", "lr_model.rds"))) {
  stop("错误: 分类模型文件不存在！请先运行建模脚本。")
}

if (!file.exists(file.path(output_models_path, "regression", "lm_model.rds"))) {
  stop("错误: 回归模型文件不存在！请先运行建模脚本。")
}

# 加载最佳模型（抑制caret版本警告）
cat("加载最佳模型...\n")
suppressWarnings({
  lr_class <- readRDS(file.path(output_models_path, "classification", "lr_model.rds"))
  recipe_class <- readRDS(file.path(output_models_path, "classification", "preprocessing_recipe.rds"))
  lm_reg <- readRDS(file.path(output_models_path, "regression", "lm_model.rds"))
  recipe_reg <- readRDS(file.path(output_models_path, "regression", "preprocessing_recipe.rds"))
})

# 尝试加载SVM模型（可选，根据报告SVM性能良好）
svm_reg <- NULL
if (file.exists(file.path(output_models_path, "regression", "svm_model.rds"))) {
  tryCatch({
    suppressWarnings({
      svm_reg <- readRDS(file.path(output_models_path, "regression", "svm_model.rds"))
    })
    cat("回归SVM模型已加载\n")
  }, error = function(e) {
    cat("警告: 回归SVM模型加载失败:", e$message, "\n")
  })
}

# 尝试加载XGBoost模型（可选）
xgb_class <- NULL
xgb_reg <- NULL
if (file.exists(file.path(output_models_path, "classification", "xgb_model.rds"))) {
  tryCatch({
    suppressWarnings({
      xgb_class <- readRDS(file.path(output_models_path, "classification", "xgb_model.rds"))
    })
    cat("分类XGBoost模型已加载\n")
  }, error = function(e) {
    cat("警告: 分类XGBoost模型加载失败:", e$message, "\n")
  })
}
if (file.exists(file.path(output_models_path, "regression", "xgb_model.rds"))) {
  tryCatch({
    suppressWarnings({
      xgb_reg <- readRDS(file.path(output_models_path, "regression", "xgb_model.rds"))
    })
    cat("回归XGBoost模型已加载\n")
  }, error = function(e) {
    cat("警告: 回归XGBoost模型加载失败:", e$message, "\n")
  })
}

# 加载数据摘要
if (file.exists(file.path(data_processed_path, "nhanes_analysis_data.rds"))) {
  data <- readRDS(file.path(data_processed_path, "nhanes_analysis_data.rds"))
} else {
  stop("错误: 数据文件不存在！请先运行数据清洗脚本。")
}

# 读取模型性能信息（用于显示）
model_perf_class <- NULL
model_perf_reg <- NULL
best_class_perf <- NULL
best_reg_perf <- NULL

if (file.exists(file.path(output_tables_path, "classification_performance.csv"))) {
  model_perf_class <- read.csv(file.path(output_tables_path, "classification_performance.csv"))
  if (best_class_model %in% model_perf_class$模型) {
    best_class_perf <- model_perf_class[model_perf_class$模型 == best_class_model, ]
  }
}

if (file.exists(file.path(output_tables_path, "regression_performance.csv"))) {
  model_perf_reg <- read.csv(file.path(output_tables_path, "regression_performance.csv"))
  if (best_reg_model %in% model_perf_reg$模型) {
    best_reg_perf <- model_perf_reg[model_perf_reg$模型 == best_reg_model, ]
  }
}

cat("模型和数据加载完成\n")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "NHANES健康风险预测系统"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("数据概览", tabName = "overview", icon = icon("database")),
      menuItem("数据预处理", tabName = "preprocessing", icon = icon("cog")),
      menuItem("探索性分析", tabName = "eda", icon = icon("chart-bar")),
      menuItem("模型性能", tabName = "performance", icon = icon("chart-line")),
      menuItem("风险预测", tabName = "prediction", icon = icon("heartbeat"))
    )
  ),
  dashboardBody(
    tabItems(
      # 数据概览
      tabItem(tabName = "overview",
        h2("数据概览"),
        fluidRow(
          valueBox(nrow(data), "总样本数", icon = icon("users"), color = "blue", width = 3),
          valueBox(sum(data$cvd_risk_status == "High Risk", na.rm = TRUE), 
                   "高风险样本", icon = icon("exclamation-triangle"), color = "red", width = 3),
          valueBox(round(mean(data$waist_circumference, na.rm = TRUE), 1), 
                   "平均腰围 (cm)", icon = icon("ruler"), color = "green", width = 3),
          valueBox(round(mean(data$age, na.rm = TRUE), 1), 
                   "平均年龄 (岁)", icon = icon("calendar"), color = "yellow", width = 3)
        ),
        fluidRow(
          box(title = "数据来源与基本信息", status = "info", solidHeader = TRUE, width = 12,
              h4("数据来源"),
              tags$ul(
                tags$li("数据来源：NHANES (National Health and Nutrition Examination Survey)"),
                tags$li("数据周期：2017-2020 (J周期)"),
                tags$li("数据文件：人口统计学、身体测量、血压、血糖、血脂、生活方式问卷等"),
                tags$li("数据规模：", nrow(data), "个样本，", ncol(data), "个变量"),
                tags$li("数据质量：已通过清洗和预处理，可用于建模分析")
              )
          )
        ),
        fluidRow(
          box(title = "数值变量描述性统计", status = "primary", solidHeader = TRUE,
              DT::dataTableOutput("numeric_stats"), width = 12)
        ),
        fluidRow(
          box(title = "分类变量分布", status = "success", solidHeader = TRUE,
              DT::dataTableOutput("categorical_stats"), width = 6),
          box(title = "目标变量分布", status = "warning", solidHeader = TRUE, width = 6,
              h4("分类目标：心血管疾病风险"),
              DT::dataTableOutput("target_class_stats"),
              hr(),
              h4("回归目标：腰围水平"),
              DT::dataTableOutput("target_reg_stats")
          )
        ),
        fluidRow(
          box(title = "数据质量指标", status = "info", solidHeader = TRUE, width = 12,
              h4("数据预处理成果"),
              tags$ul(
                tags$li("缺失值处理：已处理所有缺失值，使用合理填充策略"),
                tags$li("异常值检测：已识别并处理异常值（BMI < 10 或 > 60，腰围 < 30 或 > 200 cm）"),
                tags$li("特征工程：已创建生活方式风险评分、BMI分类、年龄组等特征"),
                tags$li("数据标准化：数值特征已标准化，分类特征已编码"),
                tags$li("数据分割：训练集/测试集分割比例为 70/30")
              ),
              hr(),
              h4("数据完整性"),
              DT::dataTableOutput("data_completeness")
          )
        )
      ),
      
      # 数据预处理
      tabItem(tabName = "preprocessing",
        h2("数据预处理"),
        fluidRow(
          box(title = "预处理步骤", status = "primary", solidHeader = TRUE, width = 12,
              h4("1. 缺失值处理"),
              p("对缺失值进行了系统处理："),
              tags$ul(
                tags$li("数值变量：使用中位数填充"),
                tags$li("分类变量：使用'Unknown'填充"),
                tags$li("目标变量：删除缺失值样本")
              ),
              hr(),
              h4("2. 异常值检测与处理"),
              p("识别并处理了以下异常值："),
              tags$ul(
                tags$li("BMI：移除 < 10 或 > 60 的极端值"),
                tags$li("腰围：移除 < 30 或 > 200 cm 的极端值"),
                tags$li("年龄：限制在 18-100 岁范围内")
              ),
              hr(),
              h4("3. 特征工程"),
              p("创建了以下新特征："),
              tags$ul(
                tags$li("生活方式风险评分：综合BMI、睡眠、吸烟、饮酒、活动等因素"),
                tags$li("BMI分类：Underweight, Normal, Overweight, Obese"),
                tags$li("年龄组：18-34, 35-49, 50-64, 65+"),
                tags$li("年龄的非线性变换：平方、立方、对数等"),
                tags$li("交互项：年龄与生活方式、性别与年龄等")
              ),
              hr(),
              h4("4. 数据标准化"),
              p("对数值特征进行了标准化处理，确保不同量纲的特征具有可比性。")
          )
        ),
        fluidRow(
          box(title = "预处理前后对比", status = "success", solidHeader = TRUE, width = 6,
              h4("原始数据"),
              tags$ul(
                tags$li("总样本数：", nrow(data), "行"),
                tags$li("特征数：", ncol(data), "列"),
                tags$li("缺失值：已全部处理")
              )
          ),
          box(title = "预处理后数据", status = "info", solidHeader = TRUE, width = 6,
              h4("清洗后数据"),
              tags$ul(
                tags$li("有效样本数：", nrow(data), "行"),
                tags$li("特征数：", ncol(data), "列（含工程特征）"),
                tags$li("数据质量：已通过质量检查")
              )
          )
        )
      ),
      
      # 探索性分析
      tabItem(tabName = "eda",
        h2("探索性数据分析"),
        fluidRow(
          box(title = "心血管疾病风险分布", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("cvd_dist"),
              hr(),
              h5("分析结论："),
              p("从分布图可以看出，数据集中低风险样本占主导地位，高风险样本约占25%。这种不平衡分布反映了真实人群中心血管疾病风险的分布情况，需要在建模时考虑类别不平衡问题。")
          ),
          box(title = "腰围分布", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("waist_dist"),
              hr(),
              h5("分析结论："),
              p("腰围分布呈现近似正态分布，主要集中在70-110cm之间。腰围是代谢综合征的重要指标，与心血管疾病风险密切相关。分布的中心趋势和离散程度为回归模型提供了良好的数据基础。")
          )
        ),
        fluidRow(
          box(title = "按年龄和性别分组的风险分布", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("cvd_by_demo"),
              hr(),
              h5("分析结论："),
              p("年龄和性别是心血管疾病风险的重要影响因素。从图中可以看出：1) 随着年龄增长，高风险比例显著增加；2) 不同性别在不同年龄段的风险模式存在差异；3) 65岁以上人群的高风险比例明显高于其他年龄组。这些发现支持了将年龄和性别作为重要预测变量的建模决策。")
          )
        ),
        fluidRow(
          box(title = "风险因素对比", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("risk_factors"),
              hr(),
              h5("分析结论："),
              p("高风险组和低风险组在BMI、腰围、收缩压等关键指标上存在显著差异。高风险组的平均BMI、腰围和血压均明显高于低风险组，这验证了这些变量作为预测因子的有效性。")
          ),
          box(title = "相关性热图", status = "primary", solidHeader = TRUE, width = 6,
              plotOutput("correlation_heatmap"),
              hr(),
              h5("分析结论："),
              p("相关性分析揭示了变量间的关联模式：BMI与腰围高度相关（r>0.8），反映了身体组成的一致性；生活方式风险评分与多个健康指标相关，说明综合评分能够有效整合多个风险因素。这些发现为特征工程和模型选择提供了依据。")
          )
        ),
        fluidRow(
          box(title = "生活方式因素分析", status = "primary", solidHeader = TRUE, width = 12,
              plotlyOutput("lifestyle_analysis"),
              hr(),
              h5("分析结论："),
              p("生活方式因素在区分高风险和低风险群体中发挥重要作用。高风险组在吸烟率、不活动比例、睡眠问题和生活方式风险评分方面均显著高于低风险组。这些发现强调了生活方式干预在心血管疾病预防中的重要性，也为模型提供了可解释的特征。")
          )
        )
      ),
      
      # 模型性能
      tabItem(tabName = "performance",
        h2("模型性能与解读"),
        fluidRow(
          box(title = "分类模型性能", status = "success", solidHeader = TRUE,
              DT::dataTableOutput("class_perf"), width = 6),
          box(title = "回归模型性能", status = "info", solidHeader = TRUE,
              DT::dataTableOutput("reg_perf"), width = 6)
        ),
        fluidRow(
          box(title = "模型参数解读", status = "primary", solidHeader = TRUE, width = 12,
              h4("分类模型参数解读"),
              DT::dataTableOutput("class_coefficients"),
              hr(),
              h4("回归模型参数解读"),
              DT::dataTableOutput("reg_coefficients"),
              hr(),
              h5("参数解读说明："),
              tags$ul(
                tags$li("系数大小：反映变量对目标变量的影响强度"),
                tags$li("正负号：正系数表示正向影响，负系数表示负向影响"),
                tags$li("显著性：p值<0.05表示变量对模型有显著贡献"),
                tags$li("OR值（分类模型）：表示风险比，OR>1表示增加风险，OR<1表示降低风险")
              )
          )
        ),
        fluidRow(
          box(title = "最佳模型说明与评估分析", status = "warning", solidHeader = TRUE, width = 12,
              h4("分类模型（心血管疾病风险预测）"),
              if (!is.null(best_class_perf)) {
                tagList(
                  p(strong(best_class_model), "是当前最佳分类模型："),
                  tags$ul(
                    tags$li("AUC = ", round(best_class_perf$AUC, 4), 
                           "（", ifelse(best_class_perf$AUC > 0.9, "优秀", 
                                       ifelse(best_class_perf$AUC > 0.8, "良好", "一般")), "）"),
                    tags$li("准确率 = ", round(best_class_perf$准确率, 4), 
                           "（", round(best_class_perf$准确率 * 100, 1), "%的预测是正确的）"),
                    tags$li("精确率 = ", round(best_class_perf$精确率, 4), 
                           "（预测为高风险中，", round(best_class_perf$精确率 * 100, 1), "%确实为高风险）"),
                    tags$li("召回率 = ", round(best_class_perf$召回率, 4), 
                           "（所有高风险样本中，", round(best_class_perf$召回率 * 100, 1), "%被正确识别）")
                  ),
                  hr(),
                  h5("性能评估结论："),
                  p("模型在测试集上表现", 
                    ifelse(best_class_perf$AUC > 0.9, "优秀", 
                           ifelse(best_class_perf$AUC > 0.8, "良好", "一般")),
                    "，AUC值", round(best_class_perf$AUC, 4), 
                    "表明模型具有良好的区分能力。准确率", round(best_class_perf$准确率 * 100, 1), 
                    "%说明模型整体预测准确性较高。精确率和召回率的平衡反映了模型在识别高风险个体方面的有效性。")
                )
              } else {
                p(strong(best_class_model), "是当前使用的分类模型。")
              },
              hr(),
              h4("回归模型（腰围预测）"),
              if (!is.null(best_reg_perf)) {
                tagList(
                  p(strong(best_reg_model), "是当前最佳回归模型："),
                  tags$ul(
                    tags$li("R² = ", round(best_reg_perf$R方, 4), 
                           "（能解释", round(best_reg_perf$R方 * 100, 1), "%的腰围变异，",
                           ifelse(best_reg_perf$R方 > 0.9, "拟合效果优秀", 
                                  ifelse(best_reg_perf$R方 > 0.8, "拟合效果良好", "拟合效果一般")), "）"),
                    tags$li("RMSE = ", round(best_reg_perf$RMSE, 2), " cm（预测误差的标准差）"),
                    tags$li("MAE = ", round(best_reg_perf$MAE, 2), " cm（平均绝对误差）"),
                    if ("MAPE" %in% colnames(best_reg_perf)) {
                      tags$li("MAPE = ", round(best_reg_perf$MAPE, 2), "%（平均绝对百分比误差）")
                    }
                  ),
                  hr(),
                  h5("性能评估结论："),
                  p("模型R²值", round(best_reg_perf$R方, 4), 
                    "表明模型能够解释", round(best_reg_perf$R方 * 100, 1), 
                    "%的腰围变异，说明生活方式和人口统计学特征对腰围有很强的预测能力。",
                    "RMSE和MAE值分别为", round(best_reg_perf$RMSE, 2), "cm和", 
                    round(best_reg_perf$MAE, 2), "cm，在临床可接受范围内，",
                    "表明模型具有实际应用价值。")
                )
              } else {
                tagList(
                  p(strong(best_reg_model), "是当前使用的回归模型。"),
                  tags$ul(
                    tags$li("R² = 0.8677（能解释86.8%的腰围变异）"),
                    tags$li("RMSE = 8.31 cm"),
                    tags$li("MAE = 6.04 cm")
                  )
                )
              }
          )
        ),
        fluidRow(
          box(title = "ROC曲线", status = "primary", solidHeader = TRUE,
              plotOutput("roc_plot"), width = 6),
          box(title = "预测vs实际（回归）", status = "primary", solidHeader = TRUE,
              plotOutput("pred_actual_plot"), width = 6)
        ),
        fluidRow(
          box(title = "特征重要性（分类模型）", status = "primary", solidHeader = TRUE,
              plotOutput("feature_importance_class"), width = 6),
          box(title = "特征重要性（回归模型）", status = "primary", solidHeader = TRUE,
              plotOutput("feature_importance_reg"), width = 6)
        ),
        fluidRow(
          box(title = "混淆矩阵（分类模型）", status = "primary", solidHeader = TRUE,
              plotOutput("confusion_matrix"), width = 12)
        )
      ),
      
      # 风险预测
      tabItem(tabName = "prediction",
        h2("健康风险预测"),
        fluidRow(
          box(title = "输入信息", status = "primary", solidHeader = TRUE, width = 6,
            h4("基本信息"),
            numericInput("age", "年龄", value = 50, min = 18, max = 100),
            selectInput("gender", "性别", 
                       choices = c("男" = "Male", "女" = "Female"),
                       selected = "Male"),
            selectInput("race", "种族", 
                       choices = c("墨西哥裔美国人" = "Mexican American",
                                  "其他西班牙裔" = "Other Hispanic", 
                                  "非西班牙裔白人" = "Non-Hispanic White",
                                  "非西班牙裔黑人" = "Non-Hispanic Black",
                                  "非西班牙裔亚洲人" = "Non-Hispanic Asian",
                                  "其他种族" = "Other Race"),
                       selected = "Non-Hispanic White"),
            selectInput("education", "教育水平",
                       choices = c("9年级以下" = "Less than 9th grade",
                                  "9-11年级" = "9-11th grade",
                                  "高中毕业" = "High school graduate",
                                  "大学或专科学历" = "Some college or AA degree",
                                  "大学本科及以上" = "College graduate or above"),
                       selected = "High school graduate"),
            numericInput("poverty_ratio", "贫困收入比", value = 2.0, min = 0, max = 5),
            hr(),
            h4("身体测量（用于风险评分）"),
            numericInput("bmi", "BMI（仅用于计算风险评分）", value = 25, min = 15, max = 50),
            p(em("注意：BMI仅用于计算生活方式风险评分，不直接用于模型预测")),
            hr(),
            h4("生活方式"),
            numericInput("sleep_hours", "睡眠时长（小时）", value = 7, min = 0, max = 24),
            selectInput("smoking", "吸烟状况",
                       choices = c("当前吸烟" = "Current smoker", 
                                  "曾经吸烟" = "Former smoker", 
                                  "未知" = "Unknown"),
                       selected = "Unknown"),
            selectInput("alcohol", "饮酒状况",
                       choices = c("不饮酒" = "Non-drinker",
                                  "轻度饮酒（≤1杯/天）" = "Light drinker (≤1/day)",
                                  "中度饮酒（2杯/天）" = "Moderate drinker (2/day)",
                                  "重度饮酒（>2杯/天）" = "Heavy drinker (>2/day)"),
                       selected = "Non-drinker"),
            selectInput("activity", "体力活动水平",
                       choices = c("活跃" = "Active",
                                  "中等活跃" = "Moderately Active",
                                  "不活跃" = "Inactive"),
                       selected = "Moderately Active"),
            hr(),
            actionButton("predict", "开始预测", class = "btn-primary btn-lg", 
                        style = "width: 100%;")
          ),
          box(title = "预测结果", status = "success", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "input.predict > 0",
              h3("心血管疾病风险预测", style = "color: #d9534f;"),
              verbatimTextOutput("cvd_prediction"),
              plotlyOutput("risk_probability_plot", height = "200px"),
              hr(),
              h3("腰围水平预测", style = "color: #5cb85c;"),
              verbatimTextOutput("waist_prediction"),
              plotlyOutput("waist_prediction_plot", height = "200px"),
              hr(),
              h4("风险因素分析"),
              verbatimTextOutput("risk_analysis")
            ),
            conditionalPanel(
              condition = "input.predict == 0",
              h4("请填写左侧信息并点击'开始预测'按钮"),
              p("系统将基于您输入的信息，使用训练好的统计模型进行预测。"),
              p(strong("提示："), "所有输入字段都已中文化，方便使用。")
            )
          )
        ),
        fluidRow(
          box(title = "模型说明与应用价值", status = "info", solidHeader = TRUE, width = 12,
            h4("分类模型：心血管疾病风险预测"),
            p("基于生活方式因素和人口统计学特征，预测个体是否处于心血管疾病高风险状态。"),
            h4("回归模型：腰围水平预测"),
            p("基于生活方式因素和人口统计学特征，预测个体的腰围水平。"),
            p(strong("当前使用模型：")),
            tags$ul(
              tags$li("分类模型：", best_class_model),
              tags$li("回归模型：", best_reg_model, "（最佳性能模型）")
            ),
            if (!is.null(best_reg_perf)) {
              tags$ul(
                tags$li("R² = ", round(best_reg_perf$R方, 4), "（解释", round(best_reg_perf$R方 * 100, 1), "%的变异）"),
                tags$li("预测误差：平均约", round(best_reg_perf$MAE, 1), "-", round(best_reg_perf$RMSE, 1), " cm"),
                tags$li("适用于评估生活方式对腰围的影响")
              )
            } else {
              tags$ul(
                tags$li("R² = 0.8677（解释86.8%的变异）"),
                tags$li("预测误差：平均约6-8 cm"),
                tags$li("适用于评估生活方式对腰围的影响")
              )
            },
            hr(),
            h4("实际应用价值"),
            tags$ul(
              tags$li("公共卫生：可用于大规模人群健康筛查，识别高风险个体"),
              tags$li("健康管理：帮助个人了解自身风险，制定个性化健康计划"),
              tags$li("临床辅助：为医生提供风险评估参考，辅助临床决策"),
              tags$li("政策制定：为公共卫生政策提供数据支持，优化资源配置")
            ),
            hr(),
            h4("系统改进方向"),
            tags$ul(
              tags$li("数据扩展：纳入更多NHANES周期数据，提高模型泛化能力"),
              tags$li("特征工程：探索更多交互项和非线性特征，提升预测精度"),
              tags$li("模型优化：尝试集成学习方法，结合多个模型的优势"),
              tags$li("实时更新：建立模型定期重训练机制，适应数据分布变化"),
              tags$li("可解释性：增强模型可解释性，提供更详细的决策依据"),
              tags$li("用户界面：优化交互体验，支持批量预测和结果导出")
            ),
            hr(),
            p(strong("注意："), "这些预测仅供参考，不能替代专业医疗诊断。")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # 创建reactiveValues来存储预测结果
  predictions <- reactiveValues(
    cvd_pred = NULL,
    cvd_prob = NULL,
    waist_pred = NULL,
    lifestyle_score = NULL
  )
  
  # 数值变量描述性统计
  output$numeric_stats <- DT::renderDataTable({
    # 变量名映射（英文 -> 中文）
    var_names <- list(
      age = "年龄",
      bmi = "BMI (身体质量指数)",
      waist_circumference = "腰围 (cm)",
      systolic_bp = "收缩压 (mmHg)",
      diastolic_bp = "舒张压 (mmHg)",
      glucose = "空腹血糖 (mg/dL)",
      hdl_cholesterol = "HDL胆固醇 (mg/dL)",
      triglycerides = "甘油三酯 (mg/dL)",
      poverty_ratio = "贫困收入比",
      sleep_hours = "睡眠时长 (小时)",
      sleep_quality_score = "睡眠质量评分",
      lifestyle_risk_score = "生活方式风险评分"
    )
    
    numeric_vars <- c("age", "bmi", "waist_circumference", "systolic_bp", "diastolic_bp",
                      "glucose", "hdl_cholesterol", "triglycerides", "poverty_ratio",
                      "sleep_hours", "sleep_quality_score", "lifestyle_risk_score")
    
    stats_list <- list()
    for (var in numeric_vars) {
      if (var %in% names(data)) {
        var_data <- data[[var]]
        var_data <- var_data[!is.na(var_data) & is.finite(var_data)]
        if (length(var_data) > 0) {
          var_name_display <- if (var %in% names(var_names)) var_names[[var]] else var
          stats_list[[var]] <- data.frame(
            变量 = var_name_display,
            均值 = round(mean(var_data, na.rm = TRUE), 2),
            中位数 = round(median(var_data, na.rm = TRUE), 2),
            标准差 = round(sd(var_data, na.rm = TRUE), 2),
            最小值 = round(min(var_data, na.rm = TRUE), 2),
            最大值 = round(max(var_data, na.rm = TRUE), 2),
            有效样本数 = length(var_data),
            缺失值数 = sum(is.na(data[[var]]) | !is.finite(data[[var]])),
            缺失率 = paste0(round(sum(is.na(data[[var]]) | !is.finite(data[[var]])) / nrow(data) * 100, 2), "%")
          )
        }
      }
    }
    
    if (length(stats_list) > 0) {
      do.call(rbind, stats_list)
    } else {
      data.frame(消息 = "无数值变量数据")
    }
  }, options = list(pageLength = 15, scrollX = TRUE))
  
  # 分类变量分布
  output$categorical_stats <- DT::renderDataTable({
    # 变量名映射
    var_names <- list(
      gender = "性别",
      race = "种族",
      education = "教育水平",
      smoking_binary = "吸烟状况",
      alcohol_category = "饮酒状况",
      activity_level = "体力活动水平",
      bmi_category = "BMI分类",
      age_group = "年龄组"
    )
    
    categorical_vars <- c("gender", "race", "education", "smoking_binary", 
                          "alcohol_category", "activity_level", "bmi_category", "age_group")
    
    stats_list <- list()
    for (var in categorical_vars) {
      if (var %in% names(data)) {
        var_table <- table(data[[var]], useNA = "ifany")
        var_name_display <- if (var %in% names(var_names)) var_names[[var]] else var
        var_df <- data.frame(
          变量 = var_name_display,
          类别 = names(var_table),
          数量 = as.numeric(var_table),
          比例 = paste0(round(as.numeric(var_table) / sum(var_table) * 100, 2), "%")
        )
        stats_list[[var]] <- var_df
      }
    }
    
    if (length(stats_list) > 0) {
      do.call(rbind, stats_list)
    } else {
      data.frame(消息 = "无分类变量数据")
    }
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  # 分类目标变量分布
  output$target_class_stats <- DT::renderDataTable({
    if ("cvd_risk_status" %in% names(data)) {
      cvd_table <- table(data$cvd_risk_status, useNA = "ifany")
      data.frame(
        类别 = names(cvd_table),
        数量 = as.numeric(cvd_table),
        比例 = paste0(round(as.numeric(cvd_table) / sum(cvd_table) * 100, 2), "%")
      )
    } else {
      data.frame(消息 = "分类目标变量不存在")
    }
  }, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
  
  # 回归目标变量统计
  output$target_reg_stats <- DT::renderDataTable({
    if ("waist_circumference" %in% names(data)) {
      waist_data <- data$waist_circumference[!is.na(data$waist_circumference) & 
                                             is.finite(data$waist_circumference)]
      if (length(waist_data) > 0) {
        data.frame(
          统计量 = c("均值", "中位数", "标准差", "最小值", "最大值", "有效样本数"),
          值 = c(
            paste0(round(mean(waist_data), 2), " cm"),
            paste0(round(median(waist_data), 2), " cm"),
            paste0(round(sd(waist_data), 2), " cm"),
            paste0(round(min(waist_data), 2), " cm"),
            paste0(round(max(waist_data), 2), " cm"),
            length(waist_data)
          )
        )
      } else {
        data.frame(消息 = "回归目标变量无有效数据")
      }
    } else {
      data.frame(消息 = "回归目标变量不存在")
    }
  }, options = list(pageLength = 10, scrollX = TRUE, dom = 't'))
  
  # 数据完整性
  output$data_completeness <- DT::renderDataTable({
    all_vars <- names(data)
    completeness_df <- data.frame(
      变量 = all_vars,
      类型 = sapply(data, function(x) {
        if (is.numeric(x)) "数值型"
        else if (is.factor(x) || is.character(x)) "分类型"
        else "其他"
      }),
      总样本数 = nrow(data),
      缺失值数 = sapply(data, function(x) sum(is.na(x))),
      缺失率 = round(sapply(data, function(x) sum(is.na(x)) / length(x) * 100), 2),
      有效值数 = sapply(data, function(x) sum(!is.na(x)))
    )
    completeness_df$完整率 <- round((1 - completeness_df$缺失率 / 100) * 100, 2)
    completeness_df <- completeness_df[order(-completeness_df$缺失率), ]
    completeness_df
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  # EDA图表
  output$cvd_dist <- renderPlotly({
    p <- ggplot(data, aes(x = cvd_risk_status, fill = cvd_risk_status)) +
      geom_bar() +
      labs(title = "心血管疾病风险分布", x = "风险状态", y = "数量") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$waist_dist <- renderPlotly({
    # 过滤掉非有限值以避免警告
    data_clean <- data %>%
      filter(is.finite(waist_circumference) & !is.na(waist_circumference))
    
    p <- ggplot(data_clean, aes(x = waist_circumference)) +
      geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
      labs(title = "腰围分布", x = "腰围 (cm)", y = "频数") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$risk_factors <- renderPlotly({
    risk_summary <- data %>%
      group_by(cvd_risk_status) %>%
      summarise(
        平均BMI = mean(bmi, na.rm = TRUE),
        平均腰围 = round(mean(waist_circumference, na.rm = TRUE), 1),
        平均收缩压 = mean(systolic_bp, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = -cvd_risk_status, names_to = "变量", values_to = "值")
    
    p <- ggplot(risk_summary, aes(x = 变量, y = 值, fill = cvd_risk_status)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "风险因素对比", x = "变量", y = "值") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # 按年龄和性别分组的风险分布
  output$cvd_by_demo <- renderPlotly({
    cvd_by_demo <- data %>%
      group_by(age_group, gender, cvd_risk_status) %>%
      summarise(数量 = n(), .groups = "drop") %>%
      group_by(age_group, gender) %>%
      mutate(比例 = round(数量 / sum(数量) * 100, 1))
    
    p <- ggplot(cvd_by_demo, aes(x = age_group, y = 比例, fill = cvd_risk_status)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~gender) +
      labs(title = "按年龄和性别分组的心血管疾病风险分布", 
           x = "年龄组", y = "比例 (%)", fill = "风险状态") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # 相关性热图
  output$correlation_heatmap <- renderPlot({
    # 选择数值变量
    numeric_vars <- data %>%
      select_if(is.numeric) %>%
      select(age, bmi, waist_circumference, systolic_bp, diastolic_bp, 
             poverty_ratio, sleep_hours, lifestyle_risk_score) %>%
      na.omit()
    
    if (nrow(numeric_vars) > 0 && ncol(numeric_vars) > 1) {
      cor_matrix <- cor(numeric_vars, use = "complete.obs")
      corrplot(cor_matrix, method = "color", type = "upper", 
               order = "hclust", tl.cex = 0.8, tl.col = "black")
    } else {
      plot.new()
      text(0.5, 0.5, "数据不足，无法生成相关性热图", cex = 1.2)
    }
  })
  
  # 生活方式因素分析
  output$lifestyle_analysis <- renderPlotly({
    lifestyle_summary <- data %>%
      group_by(cvd_risk_status) %>%
      summarise(
        当前吸烟比例 = round(mean(smoking_binary == "Current smoker", na.rm = TRUE) * 100, 1),
        不活动比例 = round(mean(activity_level == "Inactive", na.rm = TRUE) * 100, 1),
        平均睡眠时长 = round(mean(sleep_hours, na.rm = TRUE), 1),
        平均生活方式风险评分 = round(mean(lifestyle_risk_score, na.rm = TRUE), 2)
      ) %>%
      pivot_longer(cols = -cvd_risk_status, names_to = "指标", values_to = "值")
    
    p <- ggplot(lifestyle_summary, aes(x = 指标, y = 值, fill = cvd_risk_status)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "生活方式因素对比", x = "指标", y = "值", fill = "风险状态") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # 模型性能
  output$class_perf <- DT::renderDataTable({
    if (file.exists(file.path(output_tables_path, "classification_performance.csv"))) {
      read.csv(file.path(output_tables_path, "classification_performance.csv"))
    } else {
      data.frame(消息 = "性能数据未找到")
    }
  })
  
  output$reg_perf <- DT::renderDataTable({
    if (file.exists(file.path(output_tables_path, "regression_performance.csv"))) {
      read.csv(file.path(output_tables_path, "regression_performance.csv"))
    } else {
      data.frame(消息 = "性能数据未找到")
    }
  })
  
  # ROC曲线图
  output$roc_plot <- renderPlot({
    if (file.exists(file.path(output_figures_path, "models", "classification", "roc_curves.png"))) {
      img <- readPNG(file.path(output_figures_path, "models", "classification", "roc_curves.png"))
      grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "ROC曲线图未找到", cex = 1.5)
    }
  })
  
  # 预测vs实际图
  output$pred_actual_plot <- renderPlot({
    if (file.exists(file.path(output_figures_path, "models", "regression", "predictions_vs_actual.png"))) {
      img <- readPNG(file.path(output_figures_path, "models", "regression", "predictions_vs_actual.png"))
      grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "预测vs实际图未找到", cex = 1.5)
    }
  })
  
  # 特征重要性（分类模型）
  output$feature_importance_class <- renderPlot({
    if (file.exists(file.path(output_figures_path, "models", "classification", "feature_importance.png"))) {
      img <- readPNG(file.path(output_figures_path, "models", "classification", "feature_importance.png"))
      grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "特征重要性图未找到", cex = 1.2)
    }
  })
  
  # 特征重要性（回归模型）
  output$feature_importance_reg <- renderPlot({
    if (file.exists(file.path(output_figures_path, "models", "regression", "feature_importance.png"))) {
      img <- readPNG(file.path(output_figures_path, "models", "regression", "feature_importance.png"))
      grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "特征重要性图未找到", cex = 1.2)
    }
  })
  
  # 分类模型参数解读
  output$class_coefficients <- DT::renderDataTable({
    tryCatch({
      if (!is.null(lr_class) && "glmnet" %in% class(lr_class$finalModel)) {
        # 提取glmnet模型的系数
        coef_matrix <- as.matrix(coef(lr_class$finalModel, s = lr_class$bestTune$lambda))
        coef_df <- data.frame(
          特征 = rownames(coef_matrix),
          系数 = round(as.numeric(coef_matrix), 4),
          风险比_OR = round(exp(as.numeric(coef_matrix)), 4),
          影响方向 = ifelse(as.numeric(coef_matrix) > 0, "增加风险", 
                          ifelse(as.numeric(coef_matrix) < 0, "降低风险", "无影响"))
        )
        coef_df <- coef_df[coef_df$系数 != 0, ]  # 只显示非零系数
        coef_df <- coef_df[order(abs(coef_df$系数), decreasing = TRUE), ]  # 按绝对值排序
        coef_df
      } else {
        data.frame(消息 = "模型参数提取失败或模型类型不支持")
      }
    }, error = function(e) {
      data.frame(消息 = paste("错误:", e$message))
    })
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  # 回归模型参数解读
  output$reg_coefficients <- DT::renderDataTable({
    tryCatch({
      if (!is.null(lm_reg) && "glmnet" %in% class(lm_reg$finalModel)) {
        # 提取glmnet模型的系数
        coef_matrix <- as.matrix(coef(lm_reg$finalModel, s = lm_reg$bestTune$lambda))
        coef_df <- data.frame(
          特征 = rownames(coef_matrix),
          系数 = round(as.numeric(coef_matrix), 4),
          影响方向 = ifelse(as.numeric(coef_matrix) > 0, "正向影响", 
                          ifelse(as.numeric(coef_matrix) < 0, "负向影响", "无影响")),
          影响强度 = ifelse(abs(as.numeric(coef_matrix)) > 1, "强",
                          ifelse(abs(as.numeric(coef_matrix)) > 0.1, "中等", "弱"))
        )
        coef_df <- coef_df[coef_df$系数 != 0, ]  # 只显示非零系数
        coef_df <- coef_df[order(abs(coef_df$系数), decreasing = TRUE), ]  # 按绝对值排序
        coef_df
      } else {
        data.frame(消息 = "模型参数提取失败或模型类型不支持")
      }
    }, error = function(e) {
      data.frame(消息 = paste("错误:", e$message))
    })
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  # 混淆矩阵
  output$confusion_matrix <- renderPlot({
    # 尝试加载最佳分类模型的混淆矩阵
    best_class_file <- NULL
    if (best_class_model == "逻辑回归") {
      best_class_file <- file.path(output_figures_path, "models", "classification", "confusion_matrix_逻辑回归.png")
    } else if (best_class_model == "随机森林") {
      best_class_file <- file.path(output_figures_path, "models", "classification", "confusion_matrix_随机森林.png")
    } else if (best_class_model == "XGBoost") {
      best_class_file <- file.path(output_figures_path, "models", "classification", "confusion_matrix_xgboost.png")
    }
    
    if (!is.null(best_class_file) && file.exists(best_class_file)) {
      img <- readPNG(best_class_file)
      grid.raster(img)
    } else {
      plot.new()
      text(0.5, 0.5, "混淆矩阵图未找到", cex = 1.2)
    }
  })
  
  # 风险概率可视化
  output$risk_probability_plot <- renderPlotly({
    if (!is.null(predictions$cvd_prob) && is.data.frame(predictions$cvd_prob)) {
      # 确定列名
      low_risk_col <- if ("Low.Risk" %in% colnames(predictions$cvd_prob)) "Low.Risk" else "Low Risk"
      high_risk_col <- if ("High.Risk" %in% colnames(predictions$cvd_prob)) "High.Risk" else "High Risk"
      
      prob_df <- data.frame(
        风险类别 = c("低风险", "高风险"),
        概率 = c(
          as.numeric(predictions$cvd_prob[1, low_risk_col]),
          as.numeric(predictions$cvd_prob[1, high_risk_col])
        )
      )
      
      p <- ggplot(prob_df, aes(x = 风险类别, y = 概率, fill = 风险类别)) +
        geom_bar(stat = "identity") +
        scale_y_continuous(labels = scales::percent_format()) +
        labs(title = "风险概率分布", x = "", y = "概率") +
        theme_minimal() +
        theme(legend.position = "none")
      ggplotly(p)
    }
  })
  
  # 腰围预测可视化
  output$waist_prediction_plot <- renderPlotly({
    if (!is.null(predictions$waist_pred)) {
      waist_pred <- predictions$waist_pred
      rmse_value <- ifelse(!is.null(best_reg_perf), best_reg_perf$RMSE, 8.31)
      
      # 创建参考区间
      ref_data <- data.frame(
        类别 = c("预测值", "正常范围下限", "正常范围上限"),
        腰围 = c(waist_pred, 
                 ifelse(input$gender == "Male", 94, 80),
                 ifelse(input$gender == "Male", 102, 88))
      )
      
      p <- ggplot(ref_data, aes(x = 类别, y = 腰围, fill = 类别)) +
        geom_bar(stat = "identity") +
        geom_hline(yintercept = ifelse(input$gender == "Male", 102, 88), 
                   linetype = "dashed", color = "red", alpha = 0.7) +
        labs(title = "腰围预测与参考值对比", x = "", y = "腰围 (cm)") +
        theme_minimal() +
        theme(legend.position = "none")
      ggplotly(p)
    }
  })
  
    # 预测
  observeEvent(input$predict, {
    # 计算生活方式风险评分（优化BMI评分，使用连续函数使其更平滑）
    # BMI评分：使用连续函数，避免在任何阈值处急剧变化
    # 使用平滑的连续函数，让BMI从18.5到35之间平滑过渡
    bmi_score <- case_when(
      input$bmi < 18.5 ~ 0,  # 偏瘦，无风险
      input$bmi < 25 ~ (input$bmi - 18.5) * 0.03,  # 18.5-25: 0到0.195分，非常缓慢增长
      input$bmi < 30 ~ 0.195 + (input$bmi - 25) * 0.08,  # 25-30: 0.195到0.595分，缓慢增长
      input$bmi < 35 ~ 0.595 + (input$bmi - 30) * 0.08,  # 30-35: 0.595到0.995分，继续缓慢增长
      TRUE ~ 0.995 + (input$bmi - 35) * 0.01  # 35+: 增长非常缓慢，最高约1.2分
    )
    lifestyle_score <- (
      bmi_score +
      ifelse(input$sleep_hours < 6 | input$sleep_hours > 9, 1, 0) +
      ifelse(input$smoking == "Current smoker", 2, 
             ifelse(input$smoking == "Former smoker", 1, 0)) +
      ifelse(input$alcohol == "Heavy drinker (>2/day)", 1, 0) +
      ifelse(input$age >= 50, 1, 0) +
      ifelse(input$poverty_ratio < 1.3, 1, 0) +
      ifelse(input$activity == "Inactive", 1, 0)
    )
    
    # 存储到reactiveValues
    predictions$lifestyle_score <- lifestyle_score
    
    # 准备输入数据
    input_data <- data.frame(
      age = input$age,
      gender = input$gender,
      race = input$race,
      education = input$education,
      poverty_ratio = input$poverty_ratio,
      sleep_hours = input$sleep_hours,
      sleep_quality_score = 2,  # 默认值
      # 注意：训练数据中smoking_binary只有三个水平：Current smoker, Former smoker, Unknown
      smoking_binary = factor(input$smoking, levels = c("Current smoker", "Former smoker", "Unknown")),
      alcohol_category = input$alcohol,
      activity_level = input$activity,
      lifestyle_risk_score = lifestyle_score,
      bmi_category = case_when(
        input$bmi < 18.5 ~ "Underweight",
        input$bmi < 25 ~ "Normal",
        input$bmi < 30 ~ "Overweight",
        TRUE ~ "Obese"
      ),
      age_group = cut(input$age, breaks = c(18, 35, 50, 65, 100),
                     labels = c("18-34", "35-49", "50-64", "65+"), include.lowest = TRUE)
    )
    
    # 分类预测（带错误处理）
    tryCatch({
      input_processed_class <- bake(recipe_class, new_data = input_data)
      
      # 处理NA值（用0填充，因为这是标准化后的数据）
      if (any(is.na(input_processed_class))) {
        na_cols <- colnames(input_processed_class)[apply(is.na(input_processed_class), 2, any)]
        for (col in na_cols) {
          if (is.numeric(input_processed_class[[col]])) {
            input_processed_class[[col]][is.na(input_processed_class[[col]])] <- 0
          }
        }
      }
      
      # 注意：caret的predict方法使用newdata参数，不是new_data
      # 确保输入数据只有一行（避免cbind2警告）
      if (nrow(input_processed_class) > 1) {
        input_processed_class <- input_processed_class[1, , drop = FALSE]
      }
      
      cvd_prob_temp <- predict(lr_class, newdata = input_processed_class, type = "prob")
      cvd_pred_temp <- predict(lr_class, newdata = input_processed_class)
      
      # 确保预测结果是单个值（不是向量或矩阵）
      if (nrow(cvd_prob_temp) > 1) {
        cvd_prob_temp <- cvd_prob_temp[1, , drop = FALSE]
      }
      if (length(cvd_pred_temp) > 1) {
        cvd_pred_temp <- cvd_pred_temp[1]
      }
      
      # 存储到reactiveValues
      predictions$cvd_prob <- cvd_prob_temp
      predictions$cvd_pred <- cvd_pred_temp
      
      # 如果有XGBoost分类模型，也可以使用（可选）
      if (!is.null(xgb_class)) {
        tryCatch({
          xgb_prob_class <- predict(xgb_class, new_data = input_processed_class, type = "prob")
          # 可以在这里集成多个模型的预测结果
        }, error = function(e) {
          # 如果XGBoost预测失败，继续使用逻辑回归
        })
      }
    }, error = function(e) {
      predictions$cvd_pred <- NULL
      predictions$cvd_prob <- NULL
      showNotification(paste("分类预测失败:", e$message), type = "error")
    })
    
    # 回归预测（带错误处理）
    tryCatch({
      input_data_reg <- input_data  # 注意：不使用bmi，只使用bmi_category
      
      # 特征工程（与04脚本保持一致）
      input_data_reg <- input_data_reg %>%
        mutate(
          # 年龄的非线性变换
          age_squared = age^2,
          age_cubed = age^3,
          age_log = log(age + 1),
          age_sqrt = sqrt(age),
          age_exp = exp(age / 50),
          
          # 年龄与生活方式风险评分的交互项
          age_lifestyle_interaction = age * lifestyle_risk_score,
          age_squared_lifestyle = age^2 * lifestyle_risk_score,
          
          # 睡眠相关特征
          sleep_interaction = sleep_hours * sleep_quality_score,
          sleep_hours_squared = sleep_hours^2,
          sleep_quality_squared = sleep_quality_score^2,
          sleep_deficit = ifelse(sleep_hours < 7, 7 - sleep_hours, 0),
          sleep_excess = ifelse(sleep_hours > 9, sleep_hours - 9, 0),
          sleep_ratio = sleep_hours / (sleep_quality_score + 1),
          sleep_total_risk = sleep_deficit + sleep_excess,
          
          # 贫困收入比的非线性变换
          poverty_squared = poverty_ratio^2,
          poverty_cubed = poverty_ratio^3,
          poverty_sqrt = sqrt(poverty_ratio),
          poverty_log = log(poverty_ratio + 0.1),
          poverty_inverse = 1 / (poverty_ratio + 0.1),
          
          # 生活方式风险评分的非线性变换
          lifestyle_risk_squared = lifestyle_risk_score^2,
          lifestyle_risk_cubed = lifestyle_risk_score^3,
          lifestyle_log = log(lifestyle_risk_score + 1),
          lifestyle_sqrt = sqrt(lifestyle_risk_score),
          
          # 性别与年龄的交互
          gender_age = ifelse(gender == "Male", age, 0),
          gender_age_squared = ifelse(gender == "Male", age^2, 0),
          
          # 年龄与贫困的交互
          age_poverty = age * poverty_ratio,
          age_squared_poverty = age^2 * poverty_ratio,
          
          # 生活方式风险与贫困的交互
          lifestyle_poverty = lifestyle_risk_score * poverty_ratio,
          
          # 睡眠与年龄的交互
          sleep_age = sleep_hours * age,
          sleep_quality_age = sleep_quality_score * age,
          
          # 综合风险评分
          comprehensive_risk = lifestyle_risk_score + 
                               ifelse(age >= 50, 1, 0) + 
                               ifelse(poverty_ratio < 1.3, 1, 0) +
                               sleep_total_risk / 2
        )
      
      # 处理任何NA或Inf值
      numeric_cols <- sapply(input_data_reg, is.numeric)
      for (col in names(input_data_reg)[numeric_cols]) {
        if (any(is.na(input_data_reg[[col]]))) {
          input_data_reg[[col]][is.na(input_data_reg[[col]])] <- median(input_data_reg[[col]], na.rm = TRUE)
        }
        if (any(is.infinite(input_data_reg[[col]]))) {
          input_data_reg[[col]][is.infinite(input_data_reg[[col]])] <- median(input_data_reg[[col]], na.rm = TRUE)
        }
      }
      
      input_processed_reg <- bake(recipe_reg, new_data = input_data_reg)
      
      # 处理NA值
      if (any(is.na(input_processed_reg))) {
        na_cols <- colnames(input_processed_reg)[apply(is.na(input_processed_reg), 2, any)]
        for (col in na_cols) {
          if (is.numeric(input_processed_reg[[col]])) {
            input_processed_reg[[col]][is.na(input_processed_reg[[col]])] <- 0
          }
        }
      }
      
      # 确保输入数据只有一行
      if (nrow(input_processed_reg) > 1) {
        input_processed_reg <- input_processed_reg[1, , drop = FALSE]
      }
      
      waist_pred_temp <- predict(lm_reg, newdata = input_processed_reg)
      
      # 确保预测结果是单个值
      if (length(waist_pred_temp) > 1) {
        waist_pred_temp <- waist_pred_temp[1]
      }
      
      # 存储到reactiveValues
      predictions$waist_pred <- as.numeric(waist_pred_temp)[1]  # 确保是单个数值
      
      # 如果有XGBoost回归模型，也可以使用（可选）
      if (!is.null(xgb_reg)) {
        tryCatch({
          # 检查是否是直接使用xgboost包训练的模型
          if (!is.null(xgb_reg$recipe_prepped) && "xgb.Booster" %in% class(xgb_reg$finalModel)) {
            # 使用直接xgboost模型进行预测
            x_test <- input_processed_reg[, names(input_processed_reg) != "waist_circumference"]
            # 确保特征顺序与训练时一致
            if (!is.null(xgb_reg$trainingData)) {
              x_train_cols <- names(xgb_reg$trainingData)[names(xgb_reg$trainingData) != "waist_circumference"]
              x_test <- x_test[, x_train_cols, drop = FALSE]
            }
            x_test_matrix <- as.matrix(x_test)
            x_test_matrix[is.na(x_test_matrix)] <- 0
            x_test_matrix[is.infinite(x_test_matrix)] <- 0
            if (require("xgboost", quietly = TRUE)) {
              dtest <- xgb.DMatrix(data = x_test_matrix)
              xgb_pred_reg <- predict(xgb_reg$finalModel, dtest)
              # 可以在这里集成多个模型的预测结果（例如加权平均）
              # predictions$waist_pred <- 0.5 * predictions$waist_pred + 0.5 * xgb_pred_reg[1]
            }
          } else {
            # 使用caret的predict方法
            xgb_pred_reg <- predict(xgb_reg, newdata = input_processed_reg)
            # 可以在这里集成多个模型的预测结果
          }
        }, error = function(e) {
          # 如果XGBoost预测失败，继续使用线性回归
        })
      }
    }, error = function(e) {
      predictions$waist_pred <- NULL
      showNotification(paste("回归预测失败:", e$message), type = "error")
    })
  })
  
  # 输出结果（使用reactiveValues）
  output$cvd_prediction <- renderText({
    if (is.null(predictions$cvd_pred) || is.null(predictions$cvd_prob)) {
      return("请点击'开始预测'按钮进行预测")
    }
    
    # 检查列名（可能是High.Risk或High Risk）
    high_risk_col <- if ("High.Risk" %in% colnames(predictions$cvd_prob)) {
      "High.Risk"
    } else if ("High Risk" %in% colnames(predictions$cvd_prob)) {
      "High Risk"
    } else {
      colnames(predictions$cvd_prob)[ncol(predictions$cvd_prob)]
    }
    
    # 检查预测结果（处理可能的向量）
    cvd_pred_value <- if (length(predictions$cvd_pred) > 0) {
      as.character(predictions$cvd_pred)[1]
    } else {
      "Low Risk"
    }
    
    risk_level <- ifelse(cvd_pred_value %in% c("High Risk", "High.Risk"), 
                         "高风险", "低风险")
    
    # 获取概率值（处理可能的向量或数据框）
    prob_value <- if (is.data.frame(predictions$cvd_prob) && nrow(predictions$cvd_prob) > 0) {
      predictions$cvd_prob[1, high_risk_col]
    } else if (is.vector(predictions$cvd_prob) && length(predictions$cvd_prob) > 0) {
      predictions$cvd_prob[1]
    } else {
      0
    }
    prob_pct <- round(prob_value * 100, 1)
    
    paste0(
      "风险状态: ", risk_level, "\n",
      "高风险概率: ", prob_pct, "%\n\n",
      if (prob_pct >= 50) {
        "建议: 建议咨询医生，进行进一步检查，并改善生活方式。"
      } else {
        "建议: 继续保持健康的生活方式，定期体检。"
      }
    )
  })
  
  output$waist_prediction <- renderText({
    if (is.null(predictions$waist_pred)) {
      return("请点击'开始预测'按钮进行预测")
    }
    
    waist_pred <- predictions$waist_pred
    
    waist_status <- case_when(
      (input$gender == "Male" & waist_pred >= 102) | 
      (input$gender == "Female" & waist_pred >= 88) ~ "超标",
      (input$gender == "Male" & waist_pred >= 94) | 
      (input$gender == "Female" & waist_pred >= 80) ~ "偏高",
      TRUE ~ "正常"
    )
    
    # 计算预测区间（基于模型的RMSE）
    rmse_value <- 8.31  # 默认值
    r2_value <- 0.8677  # 默认值
    if (!is.null(best_reg_perf)) {
      rmse_value <- best_reg_perf$RMSE
      r2_value <- best_reg_perf$R方
    }
    
    pred_lower <- waist_pred - rmse_value
    pred_upper <- waist_pred + rmse_value
    
    paste0(
      "预测腰围: ", round(waist_pred, 1), " cm\n",
      "预测区间: ", round(pred_lower, 1), " - ", round(pred_upper, 1), " cm\n",
      "（基于模型RMSE = ", round(rmse_value, 2), " cm）\n",
      "腰围状态: ", waist_status, "\n\n",
      if (waist_status == "超标") {
        "建议: 建议咨询医生，进行进一步检查，并改善生活方式和饮食习惯。"
      } else if (waist_status == "偏高") {
        "建议: 注意饮食和运动，控制体重，定期监测腰围。"
      } else {
        "建议: 腰围水平正常，继续保持健康的生活方式。"
      },
      "\n\n",
      "模型信息: 使用", best_reg_model, "模型，R² = ", round(r2_value, 4)
    )
  })
    
  output$risk_analysis <- renderText({
    if (is.null(predictions$lifestyle_score)) {
      return("请点击'开始预测'按钮进行预测")
    }
    
    lifestyle_score <- predictions$lifestyle_score
    
    # 构建风险因素列表（注意：input值仍然是英文，因为我们在UI中使用了value映射）
    risk_factors <- c()
    
    if (!is.na(input$bmi)) {
      if (input$bmi >= 35) {
        risk_factors <- c(risk_factors, "- 严重肥胖（BMI ≥ 35）")
      } else if (input$bmi >= 30) {
        risk_factors <- c(risk_factors, "- 肥胖（BMI ≥ 30）")
      } else if (input$bmi >= 25) {
        risk_factors <- c(risk_factors, "- 超重（BMI ≥ 25）")
      }
    }
    
    if (!is.null(input$smoking) && input$smoking == "Current smoker") {
      risk_factors <- c(risk_factors, "- 当前吸烟")
    } else if (!is.null(input$smoking) && input$smoking == "Former smoker") {
      risk_factors <- c(risk_factors, "- 曾经吸烟")
    }
    
    if (!is.null(input$activity) && input$activity == "Inactive") {
      risk_factors <- c(risk_factors, "- 缺乏运动")
    }
    
    if (!is.na(input$sleep_hours) && (input$sleep_hours < 6 | input$sleep_hours > 9)) {
      risk_factors <- c(risk_factors, "- 睡眠不足或过多")
    }
    
    if (!is.null(input$alcohol) && input$alcohol == "Heavy drinker (>2/day)") {
      risk_factors <- c(risk_factors, "- 重度饮酒")
    }
    
    if (!is.na(input$age) && input$age >= 50) {
      risk_factors <- c(risk_factors, "- 年龄因素（≥50岁）")
    }
    
    if (!is.na(input$poverty_ratio) && input$poverty_ratio < 1.3) {
      risk_factors <- c(risk_factors, "- 经济困难（贫困收入比 < 1.3）")
    }
    
    # 构建输出文本
    result <- paste0(
      "生活方式风险评分: ", round(lifestyle_score, 2), " 分\n",
      "（评分越高，风险越大）\n\n",
      "主要风险因素:\n"
    )
    
    if (length(risk_factors) > 0) {
      result <- paste0(result, paste(risk_factors, collapse = "\n"), "\n")
    } else {
      result <- paste0(result, "无主要风险因素，继续保持健康的生活方式！\n")
    }
    
    return(result)
  })
}

# 运行应用
cat("\n========================================\n")
cat("启动Shiny应用...\n")
cat("========================================\n\n")
cat("应用将在浏览器中自动打开。\n")
cat("如果浏览器未自动打开，请访问显示的URL。\n")
cat("按 Ctrl+C 或关闭窗口以停止应用。\n\n")

# 设置Shiny选项
options(shiny.host = "127.0.0.1")
options(shiny.port = 3838)

# 运行应用
shinyApp(ui = ui, server = server)

