# scripts/13_adv_regression_model.R
# 目标：拟合推进部分第一阶段的 12 个模型，并输出 3 张 Typst 回归表。
# 输入：data/adv_output/CEPS_prepared.rds
# 输出：
#   data/adv_output/adv_regression_table_1.typ,
#   data/adv_output/adv_regression_table_2.typ,
#   data/adv_output/adv_regression_table_3.typ

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
  # 作为控制变量时按 0/1/2 的线性有序变量处理，避免 ordered factor 默认多项式对比。
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

build_formula <- function(dv, mean_iv, lagged_dv = NULL, extra_iv = NULL) {
  rhs_terms <- c("w1_qiwang_atomos_z", mean_iv, extra_iv, lagged_dv, ctrl_vars)

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

# %% 表 1：进入课外班竞争 ------------------------------------------------

cli::cli_alert_info("正在拟合模型 1：进入课外班竞争的 Logit 模型")

table1_models <- list(
  "A" = glmmTMB(
    build_formula("entry_buxi", "w1_buxi_rate_atomos_z"),
    data = ceps_entry,
    family = binomial(link = "logit"),
    control = smart_ctrl
  ),
  "B" = glmmTMB(
    build_formula(
      "entry_buxi",
      "w1_buxi_rate_atomos_z",
      extra_iv = c("w1_buxi_money_atomos_top20_z", "w1_buxi_time_atomos_top20_z")
    ),
    data = ceps_entry,
    family = binomial(link = "logit"),
    control = smart_ctrl
  ),
  "C" = glmmTMB(
    build_formula(
      "entry_buxi",
      "w1_buxi_rate_atomos_z",
      extra_iv = c("w1_buxi_money_atomos_cv_z", "w1_buxi_time_atomos_cv_z")
    ),
    data = ceps_entry,
    family = binomial(link = "logit"),
    control = smart_ctrl
  ),
  "D" = glmmTMB(
    build_formula(
      "entry_buxi",
      "w1_buxi_rate_atomos_z",
      extra_iv = c(
        "w1_buxi_money_atomos_top20_z",
        "w1_buxi_time_atomos_top20_z",
        "w1_buxi_money_atomos_cv_z",
        "w1_buxi_time_atomos_cv_z"
      )
    ),
    data = ceps_entry,
    family = binomial(link = "logit"),
    control = smart_ctrl
  )
)

# %% 表 2：金钱投入强度 ------------------------------------------------

cli::cli_alert_info("正在拟合模型 2：金钱投入强度线性混合模型")

table2_models <- list(
  "A" = glmmTMB(
    build_formula("w2_buxi_money_pos_log", "w1_buxi_money_atomos_z", "w1_buxi_money_log"),
    data = ceps_money,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  ),
  "B" = glmmTMB(
    build_formula(
      "w2_buxi_money_pos_log",
      "w1_buxi_money_atomos_z",
      "w1_buxi_money_log",
      "w1_buxi_money_atomos_top20_z"
    ),
    data = ceps_money,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  ),
  "C" = glmmTMB(
    build_formula(
      "w2_buxi_money_pos_log",
      "w1_buxi_money_atomos_z",
      "w1_buxi_money_log",
      "w1_buxi_money_atomos_cv_z"
    ),
    data = ceps_money,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  ),
  "D" = glmmTMB(
    build_formula(
      "w2_buxi_money_pos_log",
      "w1_buxi_money_atomos_z",
      "w1_buxi_money_log",
      c("w1_buxi_money_atomos_top20_z", "w1_buxi_money_atomos_cv_z")
    ),
    data = ceps_money,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  )
)

# %% 表 3：补习时间强度 ------------------------------------------------

cli::cli_alert_info("正在拟合模型 3：补习时间强度线性混合模型")

table3_models <- list(
  "A" = glmmTMB(
    build_formula("w2_buxi_time_pos_log", "w1_buxi_time_atomos_z", "w1_buxi_time_log"),
    data = ceps_time,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  ),
  "B" = glmmTMB(
    build_formula(
      "w2_buxi_time_pos_log",
      "w1_buxi_time_atomos_z",
      "w1_buxi_time_log",
      "w1_buxi_time_atomos_top20_z"
    ),
    data = ceps_time,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  ),
  "C" = glmmTMB(
    build_formula(
      "w2_buxi_time_pos_log",
      "w1_buxi_time_atomos_z",
      "w1_buxi_time_log",
      "w1_buxi_time_atomos_cv_z"
    ),
    data = ceps_time,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  ),
  "D" = glmmTMB(
    build_formula(
      "w2_buxi_time_pos_log",
      "w1_buxi_time_atomos_z",
      "w1_buxi_time_log",
      c("w1_buxi_time_atomos_top20_z", "w1_buxi_time_atomos_cv_z")
    ),
    data = ceps_time,
    family = gaussian(),
    REML = TRUE,
    control = smart_ctrl
  )
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

shared_coef_map <- c(
  "w1_qiwang_atomos_z" = "班级家长期望氛围",
  "know_other_par_num" = "认识班内其他家长",
  "baseline_perf_z" = "基期学业表现",
  "w1_qiwang" = "基期教育期望"
)

coef_map_table1 <- c(
  "w1_buxi_rate_atomos_z" = "班级课外班参与氛围",
  "w1_buxi_money_atomos_top20_z" = "班级补习支出右尾压力",
  "w1_buxi_time_atomos_top20_z" = "班级补习时间右尾压力",
  "w1_buxi_money_atomos_cv_z" = "班级补习支出离散程度",
  "w1_buxi_time_atomos_cv_z" = "班级补习时间离散程度",
  shared_coef_map
)

coef_map_table2 <- c(
  "w1_buxi_money_atomos_z" = "班级补习支出均值氛围",
  "w1_buxi_money_atomos_top20_z" = "班级补习支出右尾压力",
  "w1_buxi_money_atomos_cv_z" = "班级补习支出离散程度",
  "w1_buxi_money_log" = "基期补习支出",
  shared_coef_map
)

coef_map_table3 <- c(
  "w1_buxi_time_atomos_z" = "班级补习时间均值氛围",
  "w1_buxi_time_atomos_top20_z" = "班级补习时间右尾压力",
  "w1_buxi_time_atomos_cv_z" = "班级补习时间离散程度",
  "w1_buxi_time_log" = "基期补习时间",
  shared_coef_map
)

build_table <- function(models, coef_map, notes) {
  modelsummary(
    models,
    output = "tinytable",
    coef_map = coef_map,
    gof_omit = ".*",
    add_rows = build_sample_gof(models),
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = TRUE,
    notes = notes,
    width = c(0.25, rep(0.12, length(models)))
  ) |>
    theme_empty() |>
    style_tt(i = 0, line = "t", line_width = 0.1) |>
    style_tt(i = 0, line = "b", line_width = 0.05) |>
    style_tt(i = length(coef_map) * 2 + 1, line = "t", line_width = 0.05) |>
    style_tt(i = length(coef_map) * 2 + 3, line = "b", line_width = 0.1) |>
    style_tt(i = 0, bold = TRUE) |>
    style_tt(j = 2:(length(models) + 1), align = "c") |>
    format_tt(j = 2:(length(models) + 1), escape = TRUE) |>
    format_tt("notes", escape = TRUE)
}

tab_reg_1 <- build_table(
  table1_models,
  coef_map_table1,
  c(
    "注：因变量为是否参加课外班的二元变量，样本限制为基期未参加课外班的家长。",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  )
) |>
  style_tt(i = 10, line = "b", line_width = 0.05)

tab_reg_2 <- build_table(
  table2_models,
  coef_map_table2,
  c(
    "注：因变量为正值样本中的补习支出对数强度，仅保留第二期有正支出的家庭。",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  )
) |>
  style_tt(i = 6, line = "b", line_width = 0.05)

tab_reg_3 <- build_table(
  table3_models,
  coef_map_table3,
  c(
    "注：因变量为正值样本中的补习时间对数强度，仅保留第二期有正补习时间的家庭。",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  )
) |>
  style_tt(i = 6, line = "b", line_width = 0.05)

save_tt(tab_reg_1, here(output_dir, "adv_regression_table_1.typ"), overwrite = TRUE)
save_tt(tab_reg_2, here(output_dir, "adv_regression_table_2.typ"), overwrite = TRUE)
save_tt(tab_reg_3, here(output_dir, "adv_regression_table_3.typ"), overwrite = TRUE)

cli::cli_alert_success("推进部分第一阶段的模型拟合完成。")
cli::cli_alert_info("表 1 输出: {.file {here(output_dir, 'adv_regression_table_1.typ')}}")
cli::cli_alert_info("表 2 输出: {.file {here(output_dir, 'adv_regression_table_2.typ')}}")
cli::cli_alert_info("表 3 输出: {.file {here(output_dir, 'adv_regression_table_3.typ')}}")
