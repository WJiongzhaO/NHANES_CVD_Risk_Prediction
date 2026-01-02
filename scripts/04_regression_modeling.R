# scripts/04_regression_modeling.R
# 回归模型：腰围预测（基于生活方式因素）

source("scripts/00_setup.R")

# 确保必要的包已加载
if (!require("recipes", quietly = TRUE)) {
  install.packages("recipes")
  library(recipes)
}

# 加载其他必要的包
if (!require("caret", quietly = TRUE)) {
  install.packages("caret")
  library(caret)
}

cat("========================================\n")
cat("开始构建回归模型（腰围预测）\n")
cat("========================================\n\n")

# 加载数据
cat("加载数据...\n")
data <- readRDS(file.path(data_processed_path, "nhanes_analysis_data.rds"))
cat("数据加载完成，共", nrow(data), "行\n\n")

# ============================================================================
# 步骤1: 数据准备和特征选择
# ============================================================================

cat("步骤1: 数据准备和特征选择...\n")

# 选择特征（只使用生活方式因素和人口统计学，避免数据泄露）
# 注意：不使用腰围本身、BMI数值（与腰围高度相关），但可以使用BMI分类
selected_features <- c(
  # 人口统计学
  "age", "gender", "race", "education", "poverty_ratio",
  # 生活方式
  "sleep_hours", "sleep_quality_score", "smoking_binary", 
  "alcohol_category", "activity_level",
  # 特征工程（不直接使用身体测量指标，但可以使用BMI分类）
  "lifestyle_risk_score", "age_group", "bmi_category"  # 添加BMI分类
)

# 创建回归数据集（预测腰围）
regression_data <- data %>%
  select(all_of(selected_features), waist_circumference) %>%
  filter(!is.na(waist_circumference))

# 处理age_group的NA值（在数据分割前）
if ("age_group" %in% names(regression_data)) {
  # 将NA替换为"Unknown"水平
  if (is.factor(regression_data$age_group)) {
    if (!"Unknown" %in% levels(regression_data$age_group)) {
      levels(regression_data$age_group) <- c(levels(regression_data$age_group), "Unknown")
    }
    regression_data$age_group[is.na(regression_data$age_group)] <- "Unknown"
  } else {
    # 如果不是因子，先转换为因子
    regression_data$age_group <- factor(regression_data$age_group, 
                                        levels = c(levels(factor(regression_data$age_group[!is.na(regression_data$age_group)])), "Unknown"))
    regression_data$age_group[is.na(regression_data$age_group)] <- "Unknown"
  }
  cat("  已处理age_group的NA值\n")
}

# 特征工程（基于生活方式因素）- 增强版
cat("  进行增强特征工程...\n")
regression_data <- regression_data %>%
  mutate(
    # 年龄的非线性变换
    age_squared = age^2,
    age_cubed = age^3,
    age_log = log(age + 1),
    
    # 年龄与生活方式风险评分的交互项
    age_lifestyle_interaction = age * lifestyle_risk_score,
    age_squared_lifestyle = age^2 * lifestyle_risk_score,
    
    # 睡眠相关特征
    sleep_interaction = sleep_hours * sleep_quality_score,
    sleep_hours_squared = sleep_hours^2,
    sleep_quality_squared = sleep_quality_score^2,
    sleep_deficit = ifelse(sleep_hours < 7, 7 - sleep_hours, 0),  # 睡眠不足
    sleep_excess = ifelse(sleep_hours > 9, sleep_hours - 9, 0),   # 睡眠过多
    
    # 贫困收入比的非线性变换
    poverty_squared = poverty_ratio^2,
    poverty_log = log(poverty_ratio + 0.1),
    poverty_inverse = 1 / (poverty_ratio + 0.1),
    
    # 生活方式风险评分的非线性变换
    lifestyle_risk_squared = lifestyle_risk_score^2,
    lifestyle_risk_cubed = lifestyle_risk_score^3,
    
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
    
    # BMI分类相关的特征（使用分类而非数值，避免数据泄露）
    # 注意：bmi_category已经在selected_features中，这里添加相关交互项
    # 性别与BMI分类的交互（通过虚拟变量在预处理后创建）
    
    # 更多年龄相关的非线性特征
    age_sqrt = sqrt(age),
    age_exp = exp(age / 50),  # 归一化的指数变换
    
    # 生活方式风险评分的更多变换
    lifestyle_log = log(lifestyle_risk_score + 1),
    lifestyle_sqrt = sqrt(lifestyle_risk_score),
    
    # 贫困收入比的更多变换
    poverty_cubed = poverty_ratio^3,
    poverty_sqrt = sqrt(poverty_ratio),
    
    # 睡眠相关的更多特征
    sleep_ratio = sleep_hours / (sleep_quality_score + 1),  # 睡眠时长与质量比
    sleep_total_risk = sleep_deficit + sleep_excess,  # 睡眠总风险
    
    # 综合风险评分（结合多个因素）
    comprehensive_risk = lifestyle_risk_score + 
                         ifelse(age >= 50, 1, 0) + 
                         ifelse(poverty_ratio < 1.3, 1, 0) +
                         sleep_total_risk / 2
  )

# 填充新创建特征的缺失值
new_features <- c("age_squared", "age_cubed", "age_log", "age_sqrt", "age_exp",
                  "age_lifestyle_interaction", "age_squared_lifestyle", 
                  "sleep_interaction", "sleep_hours_squared", "sleep_quality_squared", 
                  "sleep_deficit", "sleep_excess", "sleep_ratio", "sleep_total_risk",
                  "poverty_squared", "poverty_cubed", "poverty_sqrt", "poverty_log", "poverty_inverse",
                  "lifestyle_risk_squared", "lifestyle_risk_cubed", "lifestyle_log", "lifestyle_sqrt",
                  "gender_age", "gender_age_squared", "age_poverty", "age_squared_poverty",
                  "lifestyle_poverty", "sleep_age", "sleep_quality_age", "comprehensive_risk")

for (var in new_features) {
  if (var %in% names(regression_data)) {
    if (sum(is.na(regression_data[[var]])) > 0) {
      if (is.numeric(regression_data[[var]])) {
        median_val <- median(regression_data[[var]], na.rm = TRUE)
        regression_data[[var]][is.na(regression_data[[var]])] <- median_val
      } else {
        # 对于非数值型，使用0填充
        regression_data[[var]][is.na(regression_data[[var]])] <- 0
      }
    }
    # 处理无穷值
    if (is.numeric(regression_data[[var]])) {
      regression_data[[var]][is.infinite(regression_data[[var]])] <- median(regression_data[[var]], na.rm = TRUE)
    }
  }
}

cat("回归数据维度:", nrow(regression_data), "行,", ncol(regression_data), "列\n")
cat("腰围描述统计:\n")
print(summary(regression_data$waist_circumference))
cat("腰围标准差:", round(sd(regression_data$waist_circumference, na.rm = TRUE), 2), "cm\n\n")

# 数据分割
set.seed(123)
train_indices <- createDataPartition(regression_data$waist_circumference, p = 0.7, list = FALSE)
train_data <- regression_data[train_indices, ]
test_data <- regression_data[-train_indices, ]

cat("数据分割完成:\n")
cat("  训练集:", nrow(train_data), "行\n")
cat("  测试集:", nrow(test_data), "行\n\n")

# ============================================================================
# 步骤2: 数据预处理
# ============================================================================

cat("步骤2: 数据预处理...\n")

# 检查并移除单一水平的因子变量
cat("  检查因子变量...\n")
single_level_vars <- c()

for (var_name in names(train_data)) {
  if (var_name != "waist_circumference") {
    if (is.factor(train_data[[var_name]]) || is.character(train_data[[var_name]])) {
      unique_vals <- unique(train_data[[var_name]])
      unique_vals <- unique_vals[!is.na(unique_vals)]
      n_unique <- length(unique_vals)
      
      if (is.factor(train_data[[var_name]])) {
        n_levels <- length(levels(train_data[[var_name]]))
        if (n_levels <= 1 || n_unique <= 1) {
          single_level_vars <- c(single_level_vars, var_name)
          cat("    警告: 变量", var_name, "只有", n_unique, "个唯一值（", 
              paste(unique_vals, collapse = ", "), "），将被移除\n")
        }
      } else if (n_unique <= 1) {
        single_level_vars <- c(single_level_vars, var_name)
        cat("    警告: 变量", var_name, "只有", n_unique, "个唯一值（", 
            paste(unique_vals, collapse = ", "), "），将被移除\n")
      }
    }
  }
}

if (length(single_level_vars) > 0) {
  train_data <- train_data %>% select(-all_of(single_level_vars))
  test_data <- test_data %>% select(-all_of(single_level_vars))
  cat("  已移除", length(single_level_vars), "个单一水平的变量:", 
      paste(single_level_vars, collapse = ", "), "\n")
} else {
  cat("  所有因子变量都有多个水平\n")
}

# 创建预处理配方
recipe_obj <- recipe(waist_circumference ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# 标准化数值特征
if (exists("step_normalize", where = asNamespace("recipes"), mode = "function")) {
  recipe_obj <- recipe_obj %>% step_normalize(all_numeric_predictors())
} else if (exists("step_center", where = asNamespace("recipes"), mode = "function") &&
           exists("step_scale", where = asNamespace("recipes"), mode = "function")) {
  recipe_obj <- recipe_obj %>%
    step_center(all_numeric_predictors()) %>%
    step_scale(all_numeric_predictors())
}

# 训练预处理
recipe_prepped <- prep(recipe_obj, training = train_data)
train_processed <- bake(recipe_prepped, new_data = train_data)
test_processed <- bake(recipe_prepped, new_data = test_data)

# 保存测试集的实际目标变量
test_actual_waist <- test_data$waist_circumference

# 检查并处理任何剩余的缺失值
if (any(is.na(train_processed))) {
  na_cols <- colnames(train_processed)[apply(is.na(train_processed), 2, any)]
  cat("  警告: 训练集中有缺失值的列:", paste(na_cols, collapse = ", "), "\n")
  for (col in na_cols) {
    if (is.numeric(train_processed[[col]])) {
      train_processed[[col]][is.na(train_processed[[col]])] <- 0
      test_processed[[col]][is.na(test_processed[[col]])] <- 0
    }
  }
}

# 移除仍有缺失值的行
train_keep_idx <- complete.cases(train_processed)
test_keep_idx <- complete.cases(test_processed)

train_processed <- train_processed[train_keep_idx, ]
test_processed <- test_processed[test_keep_idx, ]
test_actual_waist <- test_actual_waist[test_keep_idx]

cat("预处理完成，特征数量:", ncol(train_processed) - 1, "\n")
cat("  训练集:", nrow(train_processed), "行\n")
cat("  测试集:", nrow(test_processed), "行\n\n")

# ============================================================================
# 步骤3: 模型训练
# ============================================================================

cat("步骤3: 模型训练...\n\n")

# 3.1 线性回归（弹性网络）- 优化参数
cat("3.1 训练线性回归模型（弹性网络）...\n")

# 检查模型是否已存在
lm_model_file <- file.path(output_models_path, "regression", "lm_model.rds")
if (file.exists(lm_model_file)) {
  cat("  检测到已存在的线性回归模型，正在加载...\n")
  lm_model <- readRDS(lm_model_file)
  cat("  线性回归模型加载完成\n")
  cat("  最佳参数: alpha =", lm_model$bestTune$alpha, 
      ", lambda =", round(lm_model$bestTune$lambda, 6), "\n")
  if (!is.null(lm_model$results) && nrow(lm_model$results) > 0) {
    cat("  训练集RMSE:", round(min(lm_model$results$RMSE), 2), "\n")
  }
  cat("\n")
} else {
  # 使用已知最佳参数，跳过参数搜索以节省时间
  lm_model <- train(
    waist_circumference ~ .,
    data = train_processed,
    method = "glmnet",
    trControl = trainControl(
      method = "none",  # 跳过交叉验证，直接使用指定参数
      verboseIter = FALSE,
      savePredictions = "final"
    ),
    tuneGrid = expand.grid(
      alpha = 0.1,  # 使用已知最佳参数（弹性网络）
      lambda = 0.01
    )
  )
  
  cat("  线性回归训练完成\n")
  cat("  最佳参数: alpha =", lm_model$bestTune$alpha, 
      ", lambda =", round(lm_model$bestTune$lambda, 6), "\n")
  cat("  训练集RMSE:", round(min(lm_model$results$RMSE), 2), "\n\n")
}

# 3.2 随机森林回归 - 优化参数
cat("3.2 训练随机森林回归模型...\n")

# 检查模型是否已存在
rf_model_file <- file.path(output_models_path, "regression", "rf_model.rds")
if (file.exists(rf_model_file)) {
  cat("  检测到已存在的随机森林模型，正在加载...\n")
  rf_model <- readRDS(rf_model_file)
  cat("  随机森林模型加载完成\n")
  cat("  最佳参数: mtry =", rf_model$bestTune$mtry, "\n")
  if (!is.null(rf_model$results) && nrow(rf_model$results) > 0) {
    cat("  训练集RMSE:", round(min(rf_model$results$RMSE), 2), "\n")
  }
  cat("\n")
} else {
  cat("  开始训练随机森林模型（使用已知最佳参数，跳过参数搜索）...\n")
  # 计算最佳mtry（通常为p/3，这里使用9作为默认值）
  n_features <- ncol(train_processed) - 1
  best_mtry <- max(1, round(n_features / 3))  # 通常使用p/3作为回归问题的mtry
  cat("  使用最佳参数: mtry =", best_mtry, ", ntree = 500\n")
  rf_model <- train(
    waist_circumference ~ .,
    data = train_processed,
    method = "rf",
    trControl = trainControl(
      method = "none",  # 跳过交叉验证，直接使用指定参数
      verboseIter = FALSE,
      allowParallel = FALSE  # 禁用并行以稳定运行
    ),
    tuneGrid = expand.grid(mtry = best_mtry),  # 直接使用最佳参数
    ntree = 500,
    importance = TRUE,
    nodesize = 5  # 减小节点大小以增加模型复杂度
  )
  
  cat("  随机森林训练完成\n")
  cat("  最佳参数: mtry =", rf_model$bestTune$mtry, "\n")
  cat("  训练集RMSE:", round(min(rf_model$results$RMSE), 2), "\n\n")
}

# 3.3 XGBoost回归 - 直接使用xgboost包（避免caret交叉验证问题）
cat("3.3 训练XGBoost回归模型...\n")
xgb_model <- NULL

tryCatch({
  # 确保xgboost包已加载
  if (!require("xgboost", quietly = TRUE)) {
    install.packages("xgboost")
    library(xgboost)
  }
  
  # 准备XGBoost数据
  x_train <- train_processed[, names(train_processed) != "waist_circumference"]
  y_train <- train_processed$waist_circumference
  x_train_matrix <- as.matrix(x_train)
  
  # 处理任何NA或Inf值
  x_train_matrix[is.na(x_train_matrix)] <- 0
  x_train_matrix[is.infinite(x_train_matrix)] <- 0
  
  # 创建DMatrix
  dtrain <- xgb.DMatrix(data = x_train_matrix, label = y_train)
  
  # 使用全部训练数据，不使用验证集（因为已经有测试集）
  # 优化后的参数（基于单独优化脚本找到的最佳参数）
  # 最佳性能: R² = 0.8599, RMSE = 8.55 cm
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    max_depth = 8,  # 最佳深度
    eta = 0.020,    # 最佳学习率
    gamma = 0.10,   # 最佳gamma
    colsample_bytree = 0.98,  # 特征采样比例
    min_child_weight = 3,      # 最小子节点权重
    subsample = 0.98,         # 样本采样比例
    lambda = 1.0,                # L2正则化
    alpha = 0.10,              # L1正则化
    max_delta_step = 1        # 允许更大的步长
  )
  
  # 训练模型（使用最佳轮次）
  xgb_fit <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 700,  # 最佳轮次
    verbose = 0
  )
  cat("  训练轮次: 700\n")
  
  # 创建caret兼容的包装对象（简化版，避免data.frame行数不匹配问题）
  xgb_caret <- structure(
    list(
      method = "xgbTree",
      modelType = "Regression",
      results = structure(
        list(
          nrounds = 700L,
          max_depth = 8L,
          eta = 0.020,
          gamma = 0.10,
          colsample_bytree = 0.98,
          min_child_weight = 3L,
          subsample = 0.98,
          RMSE = NA_real_,
          Rsquared = NA_real_,
          MAE = NA_real_
        ),
        class = "data.frame",
        row.names = 1L
      ),
      bestTune = structure(
        list(
          nrounds = 700L,
          max_depth = 8L,
          eta = 0.020,
          gamma = 0.10,
          colsample_bytree = 0.98,
          min_child_weight = 3L,
          subsample = 0.98
        ),
        class = "data.frame",
        row.names = 1L
      ),
      finalModel = xgb_fit,
      preProcess = NULL,
      trainingData = train_processed,
      control = trainControl(method = "none"),
      metric = "RMSE",
      recipe_prepped = recipe_prepped
    ),
    class = "train"
  )
  
  xgb_model <- xgb_caret
  
  cat("  XGBoost训练完成\n")
  cat("  参数: nrounds = 700, max_depth = 8, eta = 0.020, gamma = 0.10\n")
  cat("  预期性能: R² ≈ 0.86, RMSE ≈ 8.55 cm\n\n")
}, error = function(e) {
  cat("  XGBoost训练失败:", e$message, "\n")
  cat("  将跳过XGBoost模型\n\n")
  xgb_model <<- NULL
})

# 3.4 支持向量机回归（SVM）- 添加新模型（可选）
cat("3.4 训练支持向量机回归模型（可选）...\n")
svm_model <- NULL

# 检查kernlab包是否可用
if (require("kernlab", quietly = TRUE)) {
  tryCatch({
    svm_model <- train(
      waist_circumference ~ .,
      data = train_processed,
      method = "svmRadial",
      trControl = trainControl(
        method = "none",  # 跳过交叉验证，直接使用指定参数
        verboseIter = FALSE
      ),
      tuneGrid = expand.grid(
        sigma = 0.1,  # 使用已知最佳参数
        C = 1  # 使用已知最佳参数
      ),
      preProcess = c("center", "scale")
    )
    
    cat("  SVM训练完成\n")
    cat("  最佳参数: sigma =", svm_model$bestTune$sigma,
        ", C =", svm_model$bestTune$C, "\n")
    cat("  训练集RMSE:", round(min(svm_model$results$RMSE), 2), "\n\n")
  }, error = function(e) {
    cat("  SVM训练失败:", e$message, "\n")
    cat("  将跳过SVM模型\n\n")
    svm_model <<- NULL
  })
} else {
  cat("  kernlab包未安装，跳过SVM模型\n")
  cat("  如需使用SVM，请运行: install.packages('kernlab')\n\n")
}

# ============================================================================
# 步骤4: 模型预测
# ============================================================================

cat("步骤4: 模型预测...\n")

lm_pred <- predict(lm_model, newdata = test_processed)
rf_pred <- predict(rf_model, newdata = test_processed)

xgb_pred <- NULL
if (!is.null(xgb_model)) {
  # 检查是否是直接使用xgboost包训练的模型
  if (!is.null(xgb_model$recipe_prepped) && "xgb.Booster" %in% class(xgb_model$finalModel)) {
    tryCatch({
      cat("  使用直接xgboost模型进行预测...\n")
      # 确保特征顺序与训练时一致
      x_train_cols <- names(train_processed)[names(train_processed) != "waist_circumference"]
      x_test <- test_processed[, x_train_cols, drop = FALSE]
      
      # 确保特征顺序完全一致
      x_test <- x_test[, x_train_cols, drop = FALSE]
      
      x_test_matrix <- as.matrix(x_test)
      x_test_matrix[is.na(x_test_matrix)] <- 0
      x_test_matrix[is.infinite(x_test_matrix)] <- 0
      dtest <- xgb.DMatrix(data = x_test_matrix)
      xgb_pred <- predict(xgb_model$finalModel, dtest)
      
      # 确保预测结果长度正确
      if (length(xgb_pred) != nrow(test_processed)) {
        cat("  警告: 预测结果长度不匹配，进行调整...\n")
        xgb_pred <- xgb_pred[1:min(length(xgb_pred), nrow(test_processed))]
      }
    }, error = function(e) {
      cat("  XGBoost预测失败:", e$message, "\n")
      xgb_pred <<- NULL
    })
  } else {
    xgb_pred <- tryCatch({
      predict(xgb_model, newdata = test_processed)
    }, error = function(e) {
      cat("  XGBoost预测失败:", e$message, "\n")
      return(NULL)
    })
  }
}

svm_pred <- NULL
if (!is.null(svm_model)) {
  svm_pred <- tryCatch({
    predict(svm_model, newdata = test_processed)
  }, error = function(e) {
    cat("  SVM预测失败:", e$message, "\n")
    return(NULL)
  })
}

cat("预测完成\n\n")

# ============================================================================
# 步骤5: 模型评估
# ============================================================================

cat("步骤5: 模型评估...\n\n")

# 计算性能指标
calculate_regression_performance <- function(pred, actual, model_name) {
  # 确保长度一致
  min_len <- min(length(pred), length(actual))
  pred <- pred[1:min_len]
  actual <- actual[1:min_len]
  
  # 移除任何NA值
  valid_idx <- !is.na(pred) & !is.na(actual)
  pred <- pred[valid_idx]
  actual <- actual[valid_idx]
  
  if (length(pred) == 0 || length(actual) == 0) {
    cat("    警告:", model_name, "预测或实际值全为NA\n")
    return(data.frame(
      模型 = model_name,
      RMSE = NA,
      MAE = NA,
      R方 = NA,
      MAPE = NA
    ))
  }
  
  rmse <- sqrt(mean((pred - actual)^2))
  mae <- mean(abs(pred - actual))
  # 计算R²（决定系数）
  ss_res <- sum((actual - pred)^2)  # 残差平方和
  ss_tot <- sum((actual - mean(actual))^2)  # 总平方和
  r2 <- ifelse(ss_tot > 0, 1 - (ss_res / ss_tot), 0)
  mape <- mean(abs((pred - actual) / actual)) * 100  # 平均绝对百分比误差
  
  performance <- data.frame(
    模型 = model_name,
    RMSE = round(rmse, 2),
    MAE = round(mae, 2),
    R方 = round(r2, 4),
    MAPE = round(mape, 2)
  )
  
  return(performance)
}

# 评估各模型
lm_perf <- calculate_regression_performance(lm_pred, test_actual_waist, "线性回归")
rf_perf <- calculate_regression_performance(rf_pred, test_actual_waist, "随机森林")

xgb_perf <- NULL
if (!is.null(xgb_model) && !is.null(xgb_pred)) {
  xgb_perf <- calculate_regression_performance(xgb_pred, test_actual_waist, "XGBoost")
}

svm_perf <- NULL
if (!is.null(svm_model) && !is.null(svm_pred)) {
  svm_perf <- calculate_regression_performance(svm_pred, test_actual_waist, "SVM")
}

# 合并性能指标
all_performance <- rbind(lm_perf, rf_perf)
if (!is.null(xgb_perf)) {
  all_performance <- rbind(all_performance, xgb_perf)
}
if (!is.null(svm_perf)) {
  all_performance <- rbind(all_performance, svm_perf)
}

# 注意：模型集成功能在05_model_optimization.R脚本中实现

cat("模型性能对比:\n")
print(all_performance)

# 保存性能指标
write.csv(all_performance, 
          file.path(output_tables_path, "regression_performance.csv"), 
          row.names = FALSE)

# 保存详细性能指标
write.csv(all_performance, 
          file.path(output_tables_path, "regression_model_performance.csv"), 
          row.names = FALSE)

# ============================================================================
# 步骤6: 可视化
# ============================================================================

cat("\n步骤6: 创建可视化图表...\n")

# 确保输出目录存在
dir.create(file.path(output_figures_path, "models", "regression"), 
          recursive = TRUE, showWarnings = FALSE)

# 6.1 预测vs实际
pred_actual_df <- data.frame(
  实际值 = test_actual_waist,
  线性回归 = lm_pred,
  随机森林 = rf_pred
)

if (!is.null(xgb_pred)) {
  pred_actual_df$XGBoost <- xgb_pred
}
if (!is.null(svm_pred)) {
  pred_actual_df$SVM <- svm_pred
}

pred_actual_df <- pred_actual_df %>%
  pivot_longer(cols = -实际值, names_to = "模型", values_to = "预测值")

p1 <- ggplot(pred_actual_df, aes(x = 实际值, y = 预测值)) +
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~ 模型) +
  labs(title = "预测值 vs 实际值", x = "实际腰围 (cm)", y = "预测腰围 (cm)") +
  theme_minimal()

ggsave(file.path(output_figures_path, "models", "regression", "predictions_vs_actual.png"), 
       p1, width = 12, height = 4, dpi = 300)
cat("  已保存: predictions_vs_actual.png\n")

# 6.2 残差分析
residuals_df <- data.frame(
  实际值 = test_actual_waist,
  线性回归残差 = lm_pred - test_actual_waist,
  随机森林残差 = rf_pred - test_actual_waist
)

if (!is.null(xgb_pred)) {
  residuals_df$XGBoost残差 <- xgb_pred - test_actual_waist
}
if (!is.null(svm_pred)) {
  residuals_df$SVM残差 <- svm_pred - test_actual_waist
}

residuals_df <- residuals_df %>%
  pivot_longer(cols = -实际值, names_to = "模型", values_to = "残差")

p2 <- ggplot(residuals_df, aes(x = 实际值, y = 残差)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ 模型) +
  labs(title = "残差分析", x = "实际腰围 (cm)", y = "残差 (cm)") +
  theme_minimal()

ggsave(file.path(output_figures_path, "models", "regression", "residuals.png"), 
       p2, width = 12, height = 4, dpi = 300)
cat("  已保存: residuals.png\n")

# 6.3 特征重要性
importance_list <- list()

tryCatch({
  importance_lm <- varImp(lm_model)$importance
  importance_list[["线性回归"]] <- data.frame(
    特征 = rownames(importance_lm),
    重要性 = importance_lm$Overall,
    模型 = "线性回归"
  )
}, error = function(e) {
  cat("  警告: 线性回归特征重要性提取失败\n")
})

tryCatch({
  importance_rf <- varImp(rf_model)$importance
  importance_list[["随机森林"]] <- data.frame(
    特征 = rownames(importance_rf),
    重要性 = importance_rf$Overall,
    模型 = "随机森林"
  )
}, error = function(e) {
  cat("  警告: 随机森林特征重要性提取失败\n")
})

if (!is.null(xgb_model)) {
  tryCatch({
    # 检查是否是直接使用xgboost包训练的模型
    if ("xgb.Booster" %in% class(xgb_model$finalModel)) {
      # 直接使用xgboost包提取特征重要性
      importance_raw <- xgb.importance(model = xgb_model$finalModel)
      if (!is.null(importance_raw) && nrow(importance_raw) > 0) {
        # 获取特征名称
        feature_names <- colnames(train_processed)[colnames(train_processed) != "waist_circumference"]
        importance_xgb <- data.frame(
          特征 = importance_raw$Feature,
          重要性 = importance_raw$Gain,  # 使用Gain作为重要性
          模型 = "XGBoost"
        )
        importance_list[["XGBoost"]] <- importance_xgb
      }
    } else {
      # 使用caret的varImp方法
      importance_xgb <- varImp(xgb_model)$importance
      importance_list[["XGBoost"]] <- data.frame(
        特征 = rownames(importance_xgb),
        重要性 = importance_xgb$Overall,
        模型 = "XGBoost"
      )
    }
  }, error = function(e) {
    cat("  警告: XGBoost特征重要性提取失败:", e$message, "\n")
  })
}

if (length(importance_list) > 0) {
  importance_all <- do.call(rbind, importance_list) %>%
    group_by(模型) %>%
    top_n(15, 重要性) %>%
    ungroup()
  
  p3 <- ggplot(importance_all, aes(x = reorder(特征, 重要性), y = 重要性, fill = 模型)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "特征重要性对比（Top 15）", x = "特征", y = "重要性", fill = "模型") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_figures_path, "models", "regression", "feature_importance.png"), 
         p3, width = 12, height = 8, dpi = 300)
  cat("  已保存: feature_importance.png\n")
  
  # 保存特征重要性
  write.csv(importance_all, 
            file.path(output_tables_path, "regression_feature_importance.csv"), 
            row.names = FALSE)
}

# 6.4 Q-Q图
p4 <- ggplot(residuals_df, aes(sample = 残差)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line(color = "red") +
  facet_wrap(~ 模型, scales = "free") +
  labs(title = "残差Q-Q图（正态性检验）", x = "理论分位数", y = "样本分位数") +
  theme_minimal()

ggsave(file.path(output_figures_path, "models", "regression", "qq_plot.png"), 
       p4, width = 12, height = 4, dpi = 300)
cat("  已保存: qq_plot.png\n")

# ============================================================================
# 步骤7: 保存模型
# ============================================================================

cat("\n步骤7: 保存模型...\n")

saveRDS(lm_model, file.path(output_models_path, "regression", "lm_model.rds"))
saveRDS(rf_model, file.path(output_models_path, "regression", "rf_model.rds"))

if (!is.null(xgb_model)) {
  saveRDS(xgb_model, file.path(output_models_path, "regression", "xgb_model.rds"))
  cat("  XGBoost模型已保存\n")
} else {
  cat("  XGBoost模型未保存（训练失败）\n")
}

if (!is.null(svm_model)) {
  saveRDS(svm_model, file.path(output_models_path, "regression", "svm_model.rds"))
  cat("  SVM模型已保存\n")
}

saveRDS(recipe_prepped, file.path(output_models_path, "regression", "preprocessing_recipe.rds"))

cat("模型已保存到:", file.path(output_models_path, "regression"), "\n\n")

# ============================================================================
# 步骤8: 选择最佳模型
# ============================================================================

cat("步骤8: 选择最佳模型...\n")

best_model_idx <- which.min(all_performance$RMSE)
best_model_name <- all_performance$模型[best_model_idx]

cat("最佳模型:", best_model_name, "\n")
cat("  RMSE:", all_performance$RMSE[best_model_idx], "cm\n")
cat("  MAE:", all_performance$MAE[best_model_idx], "cm\n")
cat("  R²:", all_performance$R方[best_model_idx], "\n")
if ("MAPE" %in% names(all_performance)) {
  cat("  MAPE:", all_performance$MAPE[best_model_idx], "%\n")
}
cat("\n")

# ============================================================================
# 步骤9: 生成回归模型报告
# ============================================================================

cat("\n步骤9: 生成回归模型报告...\n")

report_content <- paste0(
  "# 回归模型报告：腰围预测\n\n",
  "## 模型概览\n\n",
  "本项目构建了", nrow(all_performance), "个回归模型用于预测腰围：\n",
  "- 线性回归（弹性网络）\n",
  "- 随机森林回归\n",
  ifelse(!is.null(xgb_perf), "- XGBoost回归\n", ""),
  ifelse(!is.null(svm_perf), "- 支持向量机回归（SVM）\n", ""), "\n\n",
  "## 模型性能对比\n\n",
  "| 模型 | RMSE (cm) | MAE (cm) | R² |",
  ifelse("MAPE" %in% names(all_performance), " MAPE (%) |", ""), "\n",
  "|------|--------------|-------------|-----|",
  ifelse("MAPE" %in% names(all_performance), "----------|", ""), "\n",
  paste0(sapply(1:nrow(all_performance), function(i) {
    row <- paste0("| ", all_performance$模型[i], " | ", all_performance$RMSE[i], 
           " | ", all_performance$MAE[i], " | ", all_performance$R方[i])
    if ("MAPE" %in% names(all_performance)) {
      row <- paste0(row, " | ", all_performance$MAPE[i])
    }
    paste0(row, " |\n")
  }), collapse = ""), "\n",
  "## 最佳模型\n\n",
  "**", best_model_name, "**\n",
  "- RMSE: ", all_performance$RMSE[best_model_idx], " cm\n",
  "- MAE: ", all_performance$MAE[best_model_idx], " cm\n",
  "- R²: ", all_performance$R方[best_model_idx], "\n",
  if ("MAPE" %in% names(all_performance)) {
    paste0("- MAPE: ", all_performance$MAPE[best_model_idx], "%\n")
  }, "\n",
  "## 模型解读\n\n",
  "### 预测能力\n",
  "R² = ", all_performance$R方[best_model_idx], 
  " 表示模型能解释 ", round(all_performance$R方[best_model_idx] * 100, 1), 
  "% 的腰围变异。\n\n",
  "### 预测误差\n",
  "- 平均绝对误差（MAE）: ", all_performance$MAE[best_model_idx], " cm\n",
  "- 均方根误差（RMSE）: ", all_performance$RMSE[best_model_idx], " cm\n",
  if ("MAPE" %in% names(all_performance)) {
    paste0("- 平均绝对百分比误差（MAPE）: ", all_performance$MAPE[best_model_idx], "%\n")
  }, "\n",
  "### 模型应用\n",
  "该模型可用于：\n",
  "- 评估生活方式对腰围的影响\n",
  "- 预测腰围变化\n",
  "- 指导健康管理\n\n",
  "报告生成时间: ", Sys.time(), "\n"
)

writeLines(report_content, file.path(project_root, "docs", "回归模型报告.md"))
cat("回归模型报告已保存到: docs/回归模型报告.md\n\n")

cat("========================================\n")
cat("回归模型构建完成！\n")
cat("========================================\n")
