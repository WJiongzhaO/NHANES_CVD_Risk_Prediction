# 基于NHANES数据的心血管疾病风险预测项目

## 📋 项目概述

本项目利用NHANES（National Health and Nutrition Examination Survey）数据，构建两个预测模型：
1. **分类模型**：预测心血管疾病风险（高风险/低风险）
2. **回归模型**：预测腰围水平

## 🎯 项目目标

- 识别心血管疾病的主要风险因素
- 构建高精度的预测模型
- 开发交互式预测工具（Shiny应用）
- 为公共卫生干预和健康管理提供科学依据

## 📊 数据来源

- **数据周期**：NHANES 2017-2020（J周期）
- **数据文件**：详见 `docs/需要的数据文件清单.md`
- **数据大小**：< 10MB（符合项目要求）

## 📁 项目结构

```
NHANES_Health_Analysis/
├── data/
│   ├── raw/              # 原始NHANES数据（.xpt文件）
│   └── processed/        # 清洗后的数据
├── scripts/
│   ├── 00_setup.R        # 环境设置
│   ├── 01_data_cleaning.R      # 数据清洗
│   ├── 02_eda_visualization.R  # 探索性分析
│   ├── 03_classification_modeling.R  # 分类模型
│   ├── 04_regression_modeling.R      # 回归模型
│   ├── 05_model_optimization.R       # 模型优化
│   └── 06_shiny_app.R    # Shiny应用
├── output/
│   ├── figures/          # 图表
│   ├── tables/           # 表格
│   └── models/           # 模型文件
├── docs/
│   ├── 完整项目方案.md
│   ├── 需要的数据文件清单.md
│   └── 项目报告.md
└── README.md
```

## 🚀 快速开始

### 1. 环境准备

```r
# 运行环境设置脚本
source("scripts/00_setup.R")
```

### 2. 数据准备

确保所有NHANES数据文件在 `data/raw/` 目录中。详见 `docs/需要的数据文件清单.md`。

### 3. 运行项目

```r
# 按顺序运行所有脚本
source("scripts/01_data_cleaning.R")
source("scripts/02_eda_visualization.R")
source("scripts/03_classification_modeling.R")
source("scripts/04_regression_modeling.R")
source("scripts/05_model_optimization.R")
source("scripts/06_shiny_app.R")
```

### 4. 运行Shiny应用

```r
shiny::runApp("scripts/06_shiny_app.R")
```

## 📖 文档

- **完整项目方案**：`docs/完整项目方案.md`
- **数据文件清单**：`docs/需要的数据文件清单.md`
- **项目要求**：`期末项目要求.md`

## 📝 项目要求

本项目严格遵循课程要求：
- ✅ 数据规模 < 10MB
- ✅ 包含分类模型和回归模型
- ✅ 完整的EDA分析
- ✅ 模型评估与优化
- ✅ Shiny交互式应用
- ✅ 详细的项目报告

## 👥 作者

[您的姓名]

## 📄 许可证

[许可证信息]

---

**项目状态**：进行中  
**最后更新**：2024年

