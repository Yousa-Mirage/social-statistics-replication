# scripts/14_adv_structure_decomp_model.R
# 目标：拟合推进部分第二阶段的结构分解模型，并输出 1 张 Typst 回归表。
# 输入：data/adv_output/CEPS_prepared.rds
# 输出：
#   data/adv_output/adv_structure_decomp_table.typ

suppressPackageStartupMessages({
  library(tidyverse)
  library(bruceR)
  library(here)
  library(glmmTMB)
  library(modelsummary)
  library(tinytable)
})

output_dir <- here("data", "adv_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

optimal_cores <- max(1, parallel::detectCores(logical = FALSE) - 1)
smart_ctrl <- glmmTMBControl(parallel = list(n = optimal_cores, autopar = TRUE))

ceps <- import(here(output_dir, "CEPS_prepared.rds"), as = "tbl") |>
  mutate(know_other_par_num = as.numeric(know_other_par) - 1)

# %% 模型公式准备 ------------------------------------------------

# fmt: skip
ctrl_vars <- c(
  # 家长层面
  "is_father", "parage", "SES", "know_other_par_num",
  # 学生层面
  "baseline_perf_z", "cog3pl", "stsex", "ethgro", "jisu", "sibling", "juzhu",
  # 班级层面
  "clgrade", "smoke", "game",
  # 学校层面
  "sch_loc", "sch_rank", "sch_par_edu", "sch_par_income", "sch_teacher_edu",
  # 基期教育期望
  "w1_qiwang"
)


build_formula <- function(dv, sch_mean_iv, cls_dev_iv, lagged_dv = NULL) {
  rhs_terms <- c("w1_qiwang_atomos_z", sch_mean_iv, cls_dev_iv, lagged_dv, ctrl_vars)

  str_glue("{dv} ~ {str_c(rhs_terms, collapse = ' + ')} + (1 | schids/clsids)") |>
    as.formula()
}

# %% 建模样本 ------------------------------------------------

ceps_entry <- ceps |>
  filter(w1_buxi == 0) |>
  mutate(entry_buxi = if_else(w2_buxi == 1, 1, 0, missing = NA_real_))

ceps_money <- ceps |>
  filter(w2_buxi_money_pos == 1, !is.na(w2_buxi_money_pos_log))

ceps_time <- ceps |>
  filter(w2_buxi_time_pos == 1, !is.na(w2_buxi_time_pos_log))

# %% 模型拟合 ------------------------------------------------

cli::cli_alert_info("正在拟合结构分解模型：进入课外班竞争")

model_entry <- glmmTMB(
  build_formula(
    "entry_buxi",
    "sch_mean_w1_buxi_rate_atomos_z",
    "cls_dev_from_sch_w1_buxi_rate_atomos_z"
  ),
  data = ceps_entry,
  family = binomial(link = "logit"),
  control = smart_ctrl
)

cli::cli_alert_info("正在拟合结构分解模型：金钱投入强度")

model_money <- glmmTMB(
  build_formula(
    "w2_buxi_money_pos_log",
    "sch_mean_w1_buxi_money_atomos_z",
    "cls_dev_from_sch_w1_buxi_money_atomos_z",
    "w1_buxi_money_log"
  ),
  data = ceps_money,
  family = gaussian(),
  REML = TRUE,
  control = smart_ctrl
)

cli::cli_alert_info("正在拟合结构分解模型：补习时间强度")

model_time <- glmmTMB(
  build_formula(
    "w2_buxi_time_pos_log",
    "sch_mean_w1_buxi_time_atomos_z",
    "cls_dev_from_sch_w1_buxi_time_atomos_z",
    "w1_buxi_time_log"
  ),
  data = ceps_time,
  family = gaussian(),
  REML = TRUE,
  control = smart_ctrl
)

models_list <- list(
  "是否参加课外班" = model_entry,
  "金钱投入强度" = model_money,
  "补习时间强度" = model_time
)

# %% 结果整理 ------------------------------------------------

build_sample_gof <- function(models) {
  map_dfc(models, function(m) {
    model_data <- model.frame(m)
    c(
      as.character(nobs(m)),
      as.character(n_distinct(model_data$schids)),
      as.character(n_distinct(model_data$clsids))
    )
  }) |>
    set_names(names(models)) |>
    add_column(
      term = c("样本量", "学校数量", "班级数量"),
      .before = 1
    )
}

coef_map <- c(
  "sch_mean_w1_buxi_rate_atomos_z" = "学校平均氛围",
  "cls_dev_from_sch_w1_buxi_rate_atomos_z" = "班级相对学校平均偏离",
  "sch_mean_w1_buxi_money_atomos_z" = "学校平均氛围",
  "cls_dev_from_sch_w1_buxi_money_atomos_z" = "班级相对学校平均偏离",
  "sch_mean_w1_buxi_time_atomos_z" = "学校平均氛围",
  "cls_dev_from_sch_w1_buxi_time_atomos_z" = "班级相对学校平均偏离"
)

tab_decomp <- modelsummary(
  models_list,
  output = "tinytable",
  coef_map = coef_map,
  gof_omit = ".*",
  add_rows = build_sample_gof(models_list),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  width = c(0.25, rep(0.17, length(models_list))),
  notes = c(
    "注：表中仅报告两项核心分解变量的估计结果。表 1 的因变量为是否参加课外班的二元变量，样本限制为基期未参加课外班的家长。",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  )
) |>
  theme_empty() |>
  style_tt(i = 0, line = "t", line_width = 0.1) |>
  style_tt(i = 0, line = "b", line_width = 0.05) |>
  style_tt(i = 5, line = "t", line_width = 0.05) |>
  style_tt(i = 7, line = "b", line_width = 0.1) |>
  style_tt(i = 0, bold = TRUE) |>
  style_tt(j = 2:4, align = "c") |>
  format_tt(j = 2:4, escape = TRUE) |>
  format_tt("notes", escape = TRUE)

save_tt(tab_decomp, here(output_dir, "adv_structure_decomp_table.typ"), overwrite = TRUE)

cli::cli_alert_success("推进部分第二阶段的模型拟合完成。")
cli::cli_alert_info("模型表格输出: {.file {here(output_dir, 'adv_structure_decomp_table.typ')}}")
