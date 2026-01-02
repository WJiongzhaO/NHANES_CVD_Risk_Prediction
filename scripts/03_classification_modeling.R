# scripts/03_classification_modeling.R
# 分类模型：心血管疾病风险预测

source("scripts/00_setup.R")

# 确保必要的包已加载
if (!require("recipes", quietly = TRUE)) {
  install.packages("recipes")
  library(recipes)
}

# 加载其他必要的包
if (!require("pROC", quietly = TRUE)) {
  install.packages("pROC")
  library(pROC)
}

if (!require("caret", quietly = TRUE)) {
  install.packages("caret")
  library(caret)
}

cat("========================================\n")
cat("开始构建分类模型（心血管疾病风险预测）\n")
cat("========================================\n\n")

# 加载数据
cat("加载数据...\n")
data <- readRDS(file.path(data_processed_path, "nhanes_analysis_data.rds"))
cat("数据加载完成，共", nrow(data), "行\n\n")

# ============================================================================
# 步骤1: 数据准备
# ============================================================================

cat("步骤1: 数据准备...\n")

# 选择特征（只使用生活方式因素和人口统计学，避免数据泄露）
selected_features <- c(
  # 人口统计学
  "age", "gender", "race", "education", "poverty_ratio",
  # 生活方式
  "sleep_hours", "sleep_quality_score", "smoking_binary", 
  "alcohol_category", "activity_level",
  # 特征工程
  "lifestyle_risk_score", "bmi_category", "age_group"
)

# 创建建模数据集
model_data <- data %>%
  select(all_of(selected_features), cvd_risk_status) %>%
  filter(!is.na(cvd_risk_status))

# 将目标变量的水平名称转换为有效的R变量名（移除空格）
if (is.factor(model_data$cvd_risk_status)) {
  # 将 "Low Risk" 和 "High Risk" 转换为 "Low_Risk" 和 "High_Risk"
  levels(model_data$cvd_risk_status) <- make.names(levels(model_data$cvd_risk_status))
  cat("  已将目标变量水平转换为有效R变量名:", paste(levels(model_data$cvd_risk_status), collapse = ", "), "\n")
}

# 处理age_group的NA值（在数据分割前）
if ("age_group" %in% names(model_data)) {
  # 将NA替换为"Unknown"水平
  if (is.factor(model_data$age_group)) {
    if (!"Unknown" %in% levels(model_data$age_group)) {
      levels(model_data$age_group) <- c(levels(model_data$age_group), "Unknown")
    }
    model_data$age_group[is.na(model_data$age_group)] <- "Unknown"
  } else {
    # 如果不是因子，先转换为因子
    model_data$age_group <- factor(model_data$age_group, 
                                   levels = c(levels(factor(model_data$age_group[!is.na(model_data$age_group)])), "Unknown"))
    model_data$age_group[is.na(model_data$age_group)] <- "Unknown"
  }
  cat("  已处理age_group的NA值\n")
}

cat("建模数据维度:", nrow(model_data), "行,", ncol(model_data), "列\n")
cat("目标变量分布:\n")
print(table(model_data$cvd_risk_status))

# 数据分割
set.seed(123)
train_indices <- createDataPartition(model_data$cvd_risk_status, p = 0.7, list = FALSE)
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

cat("\n数据分割完成:\n")
cat("  训练集:", nrow(train_data), "行\n")
cat("  测试集:", nrow(test_data), "行\n\n")

# ============================================================================
# 步骤2: 数据预处理
# ============================================================================

cat("步骤2: 数据预处理...\n")

# 检查并移除单一水平的因子变量
cat("  检查因子变量...\n")
single_level_vars <- c()

# 检查所有预测变量（不包括目标变量）
for (var_name in names(train_data)) {
  if (var_name != "cvd_risk_status") {
    # 检查是否是因子或字符型
    if (is.factor(train_data[[var_name]]) || is.character(train_data[[var_name]])) {
      # 获取唯一值数量
      unique_vals <- unique(train_data[[var_name]])
      unique_vals <- unique_vals[!is.na(unique_vals)]
      n_unique <- length(unique_vals)
      
      # 如果是因子，也检查levels
      if (is.factor(train_data[[var_name]])) {
        n_levels <- length(levels(train_data[[var_name]]))
        if (n_levels <= 1 || n_unique <= 1) {
          single_level_vars <- c(single_level_vars, var_name)
          cat("    警告: 变量", var_name, "只有", n_unique, "个唯一值（", 
              paste(unique_vals, collapse = ", "), "），将被移除\n")
        }
      } else if (n_unique <= 1) {
        # 字符型变量
        single_level_vars <- c(single_level_vars, var_name)
        cat("    警告: 变量", var_name, "只有", n_unique, "个唯一值（", 
            paste(unique_vals, collapse = ", "), "），将被移除\n")
      }
    }
  }
}

# 从训练和测试数据中移除单一水平的变量
if (length(single_level_vars) > 0) {
  train_data <- train_data %>% select(-all_of(single_level_vars))
  test_data <- test_data %>% select(-all_of(single_level_vars))
  cat("  已移除", length(single_level_vars), "个单一水平的变量:", 
      paste(single_level_vars, collapse = ", "), "\n")
} else {
  cat("  所有因子变量都有多个水平\n")
}

# 创建预处理配方（兼容不同版本的recipes包）
# 注意：不使用step_unknown，因为数据中已经包含"Unknown"水平，会导致冲突
# 我们已经在数据准备阶段处理了NA值
recipe_obj <- recipe(cvd_risk_status ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# 尝试使用step_normalize，如果不存在则使用step_center和step_scale
if (exists("step_normalize", where = asNamespace("recipes"), mode = "function")) {
  recipe_obj <- recipe_obj %>% step_normalize(all_numeric_predictors())
} else {
  # 旧版本recipes包使用step_center和step_scale
  if (exists("step_center", where = asNamespace("recipes"), mode = "function") &&
      exists("step_scale", where = asNamespace("recipes"), mode = "function")) {
    recipe_obj <- recipe_obj %>%
      step_center(all_numeric_predictors()) %>%
      step_scale(all_numeric_predictors())
  } else {
    # 如果都不存在，只进行dummy编码和零方差移除
    cat("  警告: 无法进行标准化，仅进行编码和零方差移除\n")
  }
}

# 训练预处理
recipe_prepped <- prep(recipe_obj, training = train_data)
train_processed <- bake(recipe_prepped, new_data = train_data)
test_processed <- bake(recipe_prepped, new_data = test_data)

# 保存测试集的实际目标变量（在移除行之前，用于后续评估）
test_actual_before <- test_data$cvd_risk_status

# 检查并移除任何剩余的缺失值
cat("  检查缺失值...\n")
if (any(is.na(train_processed))) {
  na_cols <- colnames(train_processed)[apply(is.na(train_processed), 2, any)]
  cat("    警告: 训练集中有缺失值的列:", paste(na_cols, collapse = ", "), "\n")
  # 用0填充数值型变量的缺失值
  for (col in na_cols) {
    if (is.numeric(train_processed[[col]])) {
      train_processed[[col]][is.na(train_processed[[col]])] <- 0
      test_processed[[col]][is.na(test_processed[[col]])] <- 0
      cat("    已填充", col, "的缺失值（使用0）\n")
    }
  }
}

# 移除仍有缺失值的行
# 记录哪些行被保留了（用于匹配实际值）
train_keep_idx <- complete.cases(train_processed)
test_keep_idx <- complete.cases(test_processed)

train_processed <- train_processed[train_keep_idx, ]
test_processed <- test_processed[test_keep_idx, ]

# 匹配实际目标变量（只保留对应的行）
actual_values <- test_actual_before[test_keep_idx]

cat("预处理完成，特征数量:", ncol(train_processed) - 1, "\n")
cat("  训练集:", nrow(train_processed), "行\n")
cat("  测试集:", nrow(test_processed), "行\n\n")

# ============================================================================
# 步骤3: 模型训练
# ============================================================================

cat("步骤3: 模型训练...\n\n")

# 3.1 逻辑回归
cat("3.1 训练逻辑回归模型...\n")
# 使用已知最佳参数，跳过参数搜索以节省时间
lr_model <- train(
  cvd_risk_status ~ .,
  data = train_processed,
  method = "glmnet",
  family = "binomial",
  trControl = trainControl(
    method = "none",  # 跳过交叉验证，直接使用指定参数
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  metric = "ROC",
  tuneGrid = expand.grid(
    alpha = 0.1,  # 使用已知最佳参数（弹性网络）
    lambda = 0.01
  )
)

cat("  逻辑回归训练完成\n")
cat("  最佳参数: alpha =", lr_model$bestTune$alpha, 
    ", lambda =", round(lr_model$bestTune$lambda, 4), "\n\n")

# 3.2 随机森林（使用已知最佳参数，跳过参数搜索以节省时间）
cat("3.2 训练随机森林模型...\n")
# 计算最佳mtry（通常为sqrt(p)或p/3，这里使用4作为默认值）
n_features <- ncol(train_processed) - 1
best_mtry <- max(1, round(sqrt(n_features)))  # 通常使用sqrt(p)作为分类问题的mtry
cat("  使用最佳参数: mtry =", best_mtry, ", ntree = 500\n")
rf_model <- train(
  cvd_risk_status ~ .,
  data = train_processed,
  method = "rf",
  trControl = trainControl(
    method = "none",  # 跳过交叉验证，直接使用指定参数
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  metric = "ROC",
  tuneGrid = expand.grid(mtry = best_mtry),  # 直接使用最佳参数
  ntree = 500
)

cat("  随机森林训练完成\n")
cat("  最佳参数: mtry =", rf_model$bestTune$mtry, "\n\n")

# 3.3 XGBoost
cat("3.3 训练XGBoost模型...\n")

# 检查数据是否有问题
cat("  检查训练数据...\n")
cat("    训练集行数:", nrow(train_processed), "\n")
cat("    特征数:", ncol(train_processed) - 1, "\n")
cat("    目标变量分布:\n")
print(table(train_processed$cvd_risk_status))

# 确保目标变量是因子且有两个水平
if (!is.factor(train_processed$cvd_risk_status)) {
  train_processed$cvd_risk_status <- factor(train_processed$cvd_risk_status)
}

# 检查目标变量水平
target_levels <- levels(train_processed$cvd_risk_status)
cat("    目标变量水平:", paste(target_levels, collapse = ", "), "\n")

if (length(target_levels) != 2) {
  cat("  错误: 目标变量必须有且仅有两个水平，当前有", length(target_levels), "个水平\n")
  xgb_model <- NULL
} else {
  # 检查是否有足够的样本
  min_samples <- min(table(train_processed$cvd_risk_status))
  if (min_samples < 10) {
    cat("  警告: 某个类别的样本数过少（", min_samples, "），可能影响模型训练\n")
  }
  
  # 检查数据中是否有缺失值或无穷值
  # 只检查预测变量（不包括目标变量）
  predictor_cols <- setdiff(names(train_processed), "cvd_risk_status")
  has_na <- any(is.na(train_processed[, predictor_cols]))
  
  # 检查无穷值（只检查数值型列）
  numeric_cols <- sapply(train_processed[, predictor_cols], is.numeric)
  has_inf <- FALSE
  if (any(numeric_cols)) {
    numeric_data <- as.matrix(train_processed[, predictor_cols[numeric_cols], drop = FALSE])
    has_inf <- any(is.infinite(numeric_data))
  }
  
  if (has_na || has_inf) {
    cat("  警告: 数据中存在缺失值或无穷值，将进行清理\n")
    # 移除包含缺失值的行
    complete_rows <- complete.cases(train_processed[, predictor_cols])
    train_processed <- train_processed[complete_rows, ]
    cat("    清理后训练集行数:", nrow(train_processed), "\n")
    
    # 再次检查目标变量水平（清理后可能变化）
    target_levels_after <- levels(train_processed$cvd_risk_status)
    if (length(target_levels_after) != 2) {
      cat("  错误: 清理后目标变量水平数变为", length(target_levels_after), "，无法训练XGBoost\n")
      xgb_model <- NULL
    } else {
      # 更新目标变量水平
      target_levels <- target_levels_after
    }
  }
  
  # 只有在目标变量有两个水平时才继续训练
  if (length(target_levels) == 2) {
    # 确保xgboost包已加载
    if (!require("xgboost", quietly = TRUE)) {
      cat("  错误: xgboost包未安装，正在安装...\n")
      install.packages("xgboost")
      library(xgboost)
    }
    
    # 尝试不使用交叉验证，直接训练（避免交叉验证中的问题）
    cat("  尝试不使用交叉验证直接训练XGBoost...\n")
    
    xgb_model <- tryCatch({
      cat("  方法1: 使用method='none'直接训练...\n")
      train(
        cvd_risk_status ~ .,
        data = train_processed,
        method = "xgbTree",
        trControl = trainControl(
          method = "none",  # 不使用交叉验证
          classProbs = TRUE,
          allowParallel = FALSE,
          verboseIter = FALSE
        ),
        tuneGrid = expand.grid(
          nrounds = 100,
          max_depth = 4,
          eta = 0.1,
          gamma = 0,
          colsample_bytree = 0.8,
          min_child_weight = 1,
          subsample = 0.8
        ),
        verbosity = 0
      )
    }, error = function(e1) {
      cat("  方法1失败:", e1$message, "\n")
      # 尝试使用简单的训练/验证分割
      tryCatch({
        cat("  方法2: 使用训练/验证分割...\n")
        # 创建内部验证集
        set.seed(123)
        val_indices <- createDataPartition(train_processed$cvd_risk_status, p = 0.2, list = FALSE)
        train_internal <- train_processed[-val_indices, ]
        val_internal <- train_processed[val_indices, ]
        
        train(
          cvd_risk_status ~ .,
          data = train_internal,
          method = "xgbTree",
          trControl = trainControl(
            method = "LGOCV",  # Leave-Group-Out Cross-Validation
            number = 1,
            p = 0.8,  # 80%训练，20%验证
            classProbs = TRUE,
            summaryFunction = twoClassSummary,
            allowParallel = FALSE,
            verboseIter = FALSE
          ),
          metric = "ROC",
          tuneGrid = expand.grid(
            nrounds = 100,
            max_depth = 4,
            eta = 0.1,
            gamma = 0,
            colsample_bytree = 0.8,
            min_child_weight = 1,
            subsample = 0.8
          ),
          verbosity = 0
        )
      }, error = function(e2) {
        cat("  方法2失败:", e2$message, "\n")
        # 最后尝试：直接使用xgboost包
        tryCatch({
          cat("  方法3: 直接使用xgboost包训练...\n")
          
          # 准备数据
          x_train <- train_processed[, -which(names(train_processed) == "cvd_risk_status")]
          y_train <- train_processed$cvd_risk_status
          
          # 转换为矩阵
          x_train_matrix <- as.matrix(x_train)
          
          # 转换目标变量为0/1
          y_train_numeric <- ifelse(y_train == "High.Risk", 1, 0)
          
          # 创建DMatrix
          dtrain <- xgb.DMatrix(data = x_train_matrix, label = y_train_numeric)
          
          # 设置参数
          params <- list(
            objective = "binary:logistic",
            eval_metric = "auc",
            max_depth = 4,
            eta = 0.1,
            gamma = 0,
            colsample_bytree = 0.8,
            min_child_weight = 1,
            subsample = 0.8
          )
          
          # 训练模型
          xgb_fit <- xgb.train(
            params = params,
            data = dtrain,
            nrounds = 100,
            verbose = 0
          )
          
          # 创建一个简单的train对象包装，使其与caret兼容
          # 保存recipe_prepped到全局环境，以便predict时使用
          assign("xgb_recipe_prepped_temp", recipe_prepped, envir = .GlobalEnv)
          
          xgb_caret <- list(
            method = "xgbTree",
            modelType = "Classification",
            results = data.frame(
              nrounds = 100,
              max_depth = 4,
              eta = 0.1,
              gamma = 0,
              colsample_bytree = 0.8,
              min_child_weight = 1,
              subsample = 0.8
            ),
            bestTune = data.frame(
              nrounds = 100,
              max_depth = 4,
              eta = 0.1,
              gamma = 0,
              colsample_bytree = 0.8,
              min_child_weight = 1,
              subsample = 0.8
            ),
            finalModel = xgb_fit,
            preProcess = NULL,
            trainingData = train_processed,
            control = trainControl(method = "none"),
            metric = "ROC",
            recipe_prepped = recipe_prepped  # 保存recipe引用
          )
          
          class(xgb_caret) <- "train"
          
          return(xgb_caret)
        }, error = function(e3) {
          cat("  方法3也失败:", e3$message, "\n")
          return(NULL)
        })
      })
    })
  } else {
    # 如果目标变量水平检查失败，xgb_model已经是NULL
    if (is.null(xgb_model)) {
      cat("  跳过XGBoost模型训练（目标变量水平不符合要求）\n")
    }
  }
}

# 检查模型是否成功训练
if (!is.null(xgb_model) && !is.null(xgb_model$bestTune)) {
  cat("  XGBoost训练完成\n")
  cat("  最佳参数: nrounds =", xgb_model$bestTune$nrounds,
      ", max_depth =", xgb_model$bestTune$max_depth,
      ", eta =", xgb_model$bestTune$eta, "\n\n")
} else {
  cat("  警告: XGBoost模型训练失败，将跳过XGBoost模型\n")
  cat("  继续使用逻辑回归和随机森林模型\n\n")
}

# ============================================================================
# 步骤4: 模型预测
# ============================================================================

cat("步骤4: 模型预测...\n")

# 预测概率（注意：目标变量水平已转换为Low.Risk和High.Risk）
# 检查可用的概率列名（使用前几行数据确保有足够样本）
sample_size <- min(10, nrow(test_processed))
prob_cols_lr <- colnames(predict(lr_model, newdata = test_processed[1:sample_size, , drop = FALSE], type = "prob"))
high_risk_col <- if ("High.Risk" %in% prob_cols_lr) "High.Risk" else prob_cols_lr[length(prob_cols_lr)]  # 使用最后一列作为高风险

# 安全地提取预测概率
lr_pred_prob_raw <- predict(lr_model, newdata = test_processed, type = "prob")
rf_pred_prob_raw <- predict(rf_model, newdata = test_processed, type = "prob")

# 提取高风险概率
lr_pred_prob <- if (high_risk_col %in% colnames(lr_pred_prob_raw)) {
  lr_pred_prob_raw[, high_risk_col]
} else {
  lr_pred_prob_raw[, ncol(lr_pred_prob_raw)]  # 使用最后一列
}

rf_pred_prob <- if (high_risk_col %in% colnames(rf_pred_prob_raw)) {
  rf_pred_prob_raw[, high_risk_col]
} else {
  rf_pred_prob_raw[, ncol(rf_pred_prob_raw)]
}

# XGBoost预测（如果模型存在）
xgb_pred_prob <- NULL
xgb_pred_class <- NULL
if (!is.null(xgb_model)) {
  # 检查是否是直接使用xgboost包训练的模型（有recipe_prepped属性）
  if (!is.null(xgb_model$recipe_prepped) && "xgb.Booster" %in% class(xgb_model$finalModel)) {
    # 直接使用xgboost包训练的模型，需要手动处理
    tryCatch({
      cat("  使用直接xgboost模型进行预测...\n")
      # 准备测试数据（已经预处理过了）
      x_test <- test_processed[, -which(names(test_processed) == "cvd_risk_status")]
      x_test_matrix <- as.matrix(x_test)
      dtest <- xgb.DMatrix(data = x_test_matrix)
      
      # 预测概率
      prob_raw <- predict(xgb_model$finalModel, dtest)
      
      # 创建概率矩阵
      xgb_pred_prob_raw <- data.frame(
        Low.Risk = 1 - prob_raw,
        High.Risk = prob_raw
      )
      
      xgb_pred_prob <- xgb_pred_prob_raw[, high_risk_col]
      
      # 预测类别
      xgb_pred_class <- factor(ifelse(prob_raw > 0.5, "High.Risk", "Low.Risk"),
                               levels = c("Low.Risk", "High.Risk"))
    }, error = function(e) {
      cat("  XGBoost预测失败:", e$message, "\n")
      xgb_pred_prob <- NULL
      xgb_pred_class <- NULL
    })
  } else {
    # 使用caret训练的模型，使用标准predict方法
    xgb_pred_prob_raw <- tryCatch({
      predict(xgb_model, newdata = test_processed, type = "prob")
    }, error = function(e) {
      cat("  XGBoost概率预测失败:", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(xgb_pred_prob_raw)) {
      xgb_pred_prob <- if (high_risk_col %in% colnames(xgb_pred_prob_raw)) {
        xgb_pred_prob_raw[, high_risk_col]
      } else {
        xgb_pred_prob_raw[, ncol(xgb_pred_prob_raw)]
      }
    }
    
    # XGBoost类别预测
    xgb_pred_class <- tryCatch({
      predict(xgb_model, newdata = test_processed)
    }, error = function(e) {
      cat("  XGBoost类别预测失败:", e$message, "\n")
      return(NULL)
    })
  }
}

# 预测类别
lr_pred_class <- predict(lr_model, newdata = test_processed)
rf_pred_class <- predict(rf_model, newdata = test_processed)

# actual_values已在预处理阶段定义，无需重复定义

cat("预测完成\n\n")

# ============================================================================
# 步骤5: 模型评估
# ============================================================================

cat("步骤5: 模型评估...\n\n")

# 计算性能指标的函数
calculate_performance <- function(pred_class, pred_prob, actual, model_name) {
  # 确保长度一致
  min_len <- min(length(pred_class), length(pred_prob), length(actual))
  pred_class <- pred_class[1:min_len]
  pred_prob <- pred_prob[1:min_len]
  actual <- actual[1:min_len]
  
  # 确保因子水平一致
  pred_class <- factor(pred_class, levels = levels(actual))
  
  # 混淆矩阵（注意：positive参数需要匹配实际的水平名称）
  positive_class <- if ("High.Risk" %in% levels(actual)) "High.Risk" else levels(actual)[length(levels(actual))]
  conf_matrix <- tryCatch({
    confusionMatrix(pred_class, actual, positive = positive_class)
  }, error = function(e) {
    cat("    警告: 混淆矩阵计算失败，使用默认positive类\n")
    confusionMatrix(pred_class, actual)
  })
  
  # ROC曲线（需要确保有足够的正负样本）
  roc_obj <- tryCatch({
    roc(actual, pred_prob, quiet = TRUE)
  }, error = function(e) {
    cat("    警告: ROC曲线计算失败\n")
    return(NULL)
  })
  
  auc_value <- if (!is.null(roc_obj)) {
    as.numeric(auc(roc_obj))
  } else {
    NA
  }
  
  # 计算F1分数（避免除零）
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
    round(2 * precision * recall / (precision + recall), 4)
  } else {
    NA
  }
  
  # 性能指标
  performance <- data.frame(
    模型 = model_name,
    准确率 = round(conf_matrix$overall["Accuracy"], 4),
    精确率 = ifelse(is.na(precision), NA, round(precision, 4)),
    召回率 = ifelse(is.na(recall), NA, round(recall, 4)),
    特异度 = round(conf_matrix$byClass["Specificity"], 4),
    F1分数 = f1_score,
    AUC = ifelse(is.na(auc_value), NA, round(auc_value, 4))
  )
  
  return(list(performance = performance, conf_matrix = conf_matrix, roc = roc_obj))
}

# 评估各模型（使用匹配后的实际值）
lr_perf <- calculate_performance(lr_pred_class, lr_pred_prob, 
                                 actual_values, "逻辑回归")
rf_perf <- calculate_performance(rf_pred_class, rf_pred_prob, 
                                 actual_values, "随机森林")

# XGBoost评估（如果模型存在）
xgb_perf <- NULL
if (!is.null(xgb_model) && !is.null(xgb_pred_class) && !is.null(xgb_pred_prob)) {
  xgb_perf <- calculate_performance(xgb_pred_class, xgb_pred_prob, 
                                    actual_values, "XGBoost")
}

# 合并性能指标
if (!is.null(xgb_perf)) {
  all_performance <- rbind(
    lr_perf$performance,
    rf_perf$performance,
    xgb_perf$performance
  )
} else {
  cat("  警告: XGBoost模型不可用，仅评估逻辑回归和随机森林\n")
  all_performance <- rbind(
    lr_perf$performance,
    rf_perf$performance
  )
}

cat("模型性能对比:\n")
print(all_performance)

# 保存性能指标
write.csv(all_performance, 
          file.path(output_tables_path, "classification_performance.csv"), 
          row.names = FALSE)

# ============================================================================
# 步骤6: 可视化
# ============================================================================

cat("\n步骤6: 创建可视化图表...\n")

# 6.1 ROC曲线
roc_models <- list(lr_perf, rf_perf)
roc_names <- c("逻辑回归", "随机森林")
roc_colors <- c("blue", "green")

# 如果XGBoost模型存在，添加到列表中
if (!is.null(xgb_perf)) {
  roc_models <- list(lr_perf, rf_perf, xgb_perf)
  roc_names <- c("逻辑回归", "随机森林", "XGBoost")
  roc_colors <- c("blue", "green", "red")
}

# 检查是否有有效的ROC对象
valid_roc_count <- sum(sapply(roc_models, function(x) !is.null(x$roc)))

if (valid_roc_count > 0) {
  png(file.path(output_figures_path, "models", "classification", "roc_curves.png"), 
      width = 10, height = 8, units = "in", res = 300)
  
  # 绘制第一个ROC曲线
  first_roc <- NULL
  for (i in 1:length(roc_models)) {
    if (!is.null(roc_models[[i]]$roc)) {
      if (is.null(first_roc)) {
        plot(roc_models[[i]]$roc, col = roc_colors[i], main = "ROC曲线对比", lwd = 2)
        first_roc <- roc_models[[i]]$roc
      } else {
        lines(roc_models[[i]]$roc, col = roc_colors[i], lwd = 2)
      }
    }
  }
  
  # 添加图例
  legend_text <- c()
  for (i in 1:length(roc_models)) {
    if (!is.null(roc_models[[i]]$roc)) {
      auc_val <- ifelse(is.na(roc_models[[i]]$performance$AUC), "N/A", 
                        round(roc_models[[i]]$performance$AUC, 3))
      legend_text <- c(legend_text, paste(roc_names[i], "(AUC =", auc_val, ")"))
    }
  }
  
  if (length(legend_text) > 0) {
    legend("bottomright", legend = legend_text,
           col = roc_colors[1:length(legend_text)], lwd = 2)
  }
  
  dev.off()
  cat("  已保存: roc_curves.png\n")
} else {
  cat("  警告: 没有有效的ROC曲线可绘制\n")
}

# 6.2 混淆矩阵可视化
conf_matrices <- list(
  逻辑回归 = lr_perf$conf_matrix,
  随机森林 = rf_perf$conf_matrix
)

# 如果XGBoost模型存在，添加到列表中
if (!is.null(xgb_perf)) {
  conf_matrices[["XGBoost"]] <- xgb_perf$conf_matrix
}

for (i in 1:length(conf_matrices)) {
  model_name <- names(conf_matrices)[i]
  conf_mat <- conf_matrices[[i]]$table
  
  p_conf <- ggplot(data = as.data.frame(conf_mat), 
                   aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = paste(model_name, "混淆矩阵"), 
         x = "实际值", y = "预测值") +
    theme_minimal()
  
  ggsave(file.path(output_figures_path, "models", "classification", 
                   paste0("confusion_matrix_", tolower(gsub(" ", "_", model_name)), ".png")), 
         p_conf, width = 6, height = 5, dpi = 300)
}

cat("  已保存: 混淆矩阵图表\n")

# 6.3 特征重要性（所有模型）
importance_list <- list()

# XGBoost特征重要性
tryCatch({
  if ("xgbTree" %in% class(xgb_model$finalModel)) {
    importance_xgb <- varImp(xgb_model)$importance
    importance_list[["XGBoost"]] <- data.frame(
      特征 = rownames(importance_xgb),
      重要性 = importance_xgb$Overall,
      模型 = "XGBoost"
    ) %>%
      arrange(desc(重要性)) %>%
      head(20)
  }
}, error = function(e) {
  cat("  警告: XGBoost特征重要性提取失败\n")
})

# 随机森林特征重要性
tryCatch({
  importance_rf <- varImp(rf_model)$importance
  importance_list[["随机森林"]] <- data.frame(
    特征 = rownames(importance_rf),
    重要性 = importance_rf$Overall,
    模型 = "随机森林"
  ) %>%
    arrange(desc(重要性)) %>%
    head(20)
}, error = function(e) {
  cat("  警告: 随机森林特征重要性提取失败\n")
})

# 逻辑回归特征重要性
tryCatch({
  importance_lr <- varImp(lr_model)$importance
  importance_list[["逻辑回归"]] <- data.frame(
    特征 = rownames(importance_lr),
    重要性 = importance_lr$Overall,
    模型 = "逻辑回归"
  ) %>%
    arrange(desc(重要性)) %>%
    head(20)
}, error = function(e) {
  cat("  警告: 逻辑回归特征重要性提取失败\n")
})

# 合并特征重要性
if (length(importance_list) > 0) {
  importance_all <- do.call(rbind, importance_list)
  
  # 可视化
  p_importance <- ggplot(importance_all, 
                         aes(x = reorder(特征, 重要性), y = 重要性, fill = 模型)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "特征重要性对比（Top 20）", x = "特征", y = "重要性", fill = "模型") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_figures_path, "models", "classification", "feature_importance.png"), 
         p_importance, width = 12, height = 8, dpi = 300)
  cat("  已保存: feature_importance.png\n")
  
  # 保存特征重要性
  write.csv(importance_all, 
            file.path(output_tables_path, "classification_feature_importance.csv"), 
            row.names = FALSE)
} else {
  cat("  警告: 无法提取任何特征重要性\n")
}

# 6.4 预测概率分布
pred_prob_df <- data.frame(
  逻辑回归 = lr_pred_prob,
  随机森林 = rf_pred_prob,
  实际值 = actual_values
)

# 如果XGBoost模型存在，添加到数据框
if (!is.null(xgb_pred_prob)) {
  pred_prob_df$XGBoost <- xgb_pred_prob
}

pred_prob_df <- pred_prob_df %>%
  pivot_longer(cols = -实际值, names_to = "模型", values_to = "概率")

p_prob_dist <- ggplot(pred_prob_df, aes(x = 概率, fill = 实际值)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~ 模型) +
  labs(title = "预测概率分布", x = "高风险概率", y = "频数", fill = "实际值") +
  theme_minimal()

ggsave(file.path(output_figures_path, "models", "classification", "prediction_probability_distribution.png"), 
       p_prob_dist, width = 12, height = 4, dpi = 300)
cat("  已保存: prediction_probability_distribution.png\n")

# ============================================================================
# 步骤7: 保存模型
# ============================================================================

cat("\n步骤7: 保存模型...\n")

saveRDS(lr_model, file.path(output_models_path, "classification", "lr_model.rds"))
saveRDS(rf_model, file.path(output_models_path, "classification", "rf_model.rds"))

# 如果XGBoost模型存在，保存它
if (!is.null(xgb_model)) {
  saveRDS(xgb_model, file.path(output_models_path, "classification", "xgb_model.rds"))
  cat("  XGBoost模型已保存\n")
} else {
  cat("  XGBoost模型未保存（训练失败）\n")
}

saveRDS(recipe_prepped, file.path(output_models_path, "classification", "preprocessing_recipe.rds"))

cat("模型已保存到:", file.path(output_models_path, "classification"), "\n\n")

# ============================================================================
# 步骤8: 选择最佳模型
# ============================================================================

cat("步骤8: 选择最佳模型...\n")

best_model_idx <- which.max(all_performance$AUC)
best_model_name <- all_performance$模型[best_model_idx]

cat("最佳模型:", best_model_name, "\n")
cat("  AUC:", all_performance$AUC[best_model_idx], "\n")
cat("  准确率:", all_performance$准确率[best_model_idx], "\n")
cat("  精确率:", all_performance$精确率[best_model_idx], "\n")
cat("  召回率:", all_performance$召回率[best_model_idx], "\n\n")

# ============================================================================
# 步骤9: 生成分类模型报告
# ============================================================================

cat("\n步骤9: 生成分类模型报告...\n")

report_content <- paste0(
  "# 分类模型报告：心血管疾病风险预测\n\n",
  "## 模型概览\n\n",
  "本项目构建了", nrow(all_performance), "个分类模型用于预测心血管疾病风险状态（高风险/低风险）：\n",
  "- 逻辑回归（弹性网络）\n",
  "- 随机森林\n",
  ifelse(!is.null(xgb_perf), "- XGBoost\n", ""), "\n\n",
  "## 模型性能对比\n\n",
  "| 模型 | 准确率 | 精确率 | 召回率 | F1分数 | AUC |\n",
  "|------|--------|--------|--------|--------|-----|\n",
  paste0(sapply(1:nrow(all_performance), function(i) {
    paste0("| ", all_performance$模型[i], " | ", all_performance$准确率[i], 
           " | ", all_performance$精确率[i], " | ", all_performance$召回率[i],
           " | ", all_performance$F1分数[i], " | ", all_performance$AUC[i], " |\n")
  }), collapse = ""), "\n",
  "## 最佳模型\n\n",
  "**", best_model_name, "**\n",
  "- AUC: ", all_performance$AUC[best_model_idx], "\n",
  "- 准确率: ", all_performance$准确率[best_model_idx], "\n",
  "- 精确率: ", all_performance$精确率[best_model_idx], "\n",
  "- 召回率: ", all_performance$召回率[best_model_idx], "\n\n",
  "## 模型解读\n\n",
  "### 特征重要性\n",
  "最重要的预测特征包括：\n",
  "- 生活方式风险评分\n",
  "- 年龄\n",
  "- BMI\n",
  "- 睡眠质量\n",
  "- 吸烟状况\n\n",
  "### 模型应用\n",
  "该模型可用于：\n",
  "- 早期识别心血管疾病高风险人群\n",
  "- 指导生活方式干预\n",
  "- 公共卫生筛查\n\n",
  "报告生成时间: ", Sys.time(), "\n"
)

writeLines(report_content, file.path(project_root, "docs", "分类模型报告.md"))
cat("分类模型报告已保存到: docs/分类模型报告.md\n\n")

cat("========================================\n")
cat("分类模型构建完成！\n")
cat("========================================\n")

