# scripts/12_adv_descriptive_analyze.R
# 目标：使用 tinytable 生成可直接用于 Typst 的描述性统计表
# 输入：data/adv_output/CEPS_prepared.rds
# 输出：data/adv_output/descriptive_table.typ

suppressPackageStartupMessages({
  library(tidyverse)
  library(bruceR)
  library(tinytable)
  library(here)
})

output_dir <- here("data", "adv_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ceps <- import(here(output_dir, "CEPS_prepared.rds"), as = "tbl") |>
  select(-ids, -clsids, -schids, -is_random_class)

cli::cli_alert_warning("尚未实现，等待模型分析完成后再补充必要的描述性统计表。")

# 第一类是重新编码后的因变量：
#
# - 是否参与课外班
# - 是否有正的课外班金钱投入
# - 正值样本中的课外班金钱投入
# - 是否有正的补习时间
# - 正值样本中的补习时间
# - 家校联系的有序变量
#
# 第二类是推进后的核心自变量：
#
# - 班级平均投入氛围
# - 班级右尾压力指标
# - 班级极化/离散指标
# - 学校平均氛围
# - 班级相对学校平均偏离
#
# 第三类是新增关键控制变量：
#
# - 是否认识班内其他家长
# - 基线学业表现
#
# 这张表的目的是告诉读者推进分析到底在用什么新变量，这些变量的分布大概是什么样子。

# 添加两张统计图：
# 图 1：零值与正值分布图，展示将“课外班金钱投入”和“补习时间”拆成发生与强度的必要性
# 图 2：班级氛围的右尾/极化分布图，比如
#   - 班级右尾压力的直方图；
#   - 班级极化指标的箱线图；
#   - 或者班级均值与极化程度的散点图。