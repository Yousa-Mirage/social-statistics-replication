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
  library(marginaleffects)
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

compute_ame <- function(model, variable, label) {
  suppressWarnings(
    avg_slopes(
      model,
      variables = variable,
      type = "response",
      re.form = NA
    )
  ) |>
    as_tibble() |>
    transmute(
      label,
      ame = estimate,
      std_error = std.error,
      statistic,
      p_value = p.value,
      conf_low = .data[["conf.low"]],
      conf_high = .data[["conf.high"]]
    )
}

compute_probability_shift <- function(model, variable, label) {
  model_data <- model.frame(model)
  q <- quantile(model_data[[variable]], c(0.25, 0.75), na.rm = TRUE)

  nd_q25 <- model_data
  nd_q75 <- model_data
  nd_q25[[variable]] <- unname(q[[1]])
  nd_q75[[variable]] <- unname(q[[2]])

  pred_q25 <- suppressWarnings(
    predictions(model, newdata = nd_q25, type = "response", re.form = NA)
  ) |>
    summarise(prob_q25 = mean(estimate, na.rm = TRUE))

  pred_q75 <- suppressWarnings(
    predictions(model, newdata = nd_q75, type = "response", re.form = NA)
  ) |>
    summarise(prob_q75 = mean(estimate, na.rm = TRUE))

  tibble(
    label,
    q25 = unname(q[[1]]),
    q75 = unname(q[[2]]),
    prob_q25 = pred_q25$prob_q25,
    prob_q75 = pred_q75$prob_q75,
    prob_change = pred_q75$prob_q75 - pred_q25$prob_q25
  )
}

build_prediction_df <- function(model, sample_data, focal_var, n_points = 100) {
  x_seq <- seq(
    min(sample_data[[focal_var]], na.rm = TRUE),
    max(sample_data[[focal_var]], na.rm = TRUE),
    length.out = n_points
  )

  grid_args <- list(model = model, newdata = sample_data, grid_type = "mean_or_mode")
  grid_args[[focal_var]] <- x_seq
  newdata <- do.call(datagrid, grid_args)

  suppressWarnings(
    predictions(model, newdata = newdata, type = "response", re.form = NA)
  ) |>
    as_tibble() |>
    mutate(x = x_seq)
}

plot_prediction_curve <- function(pred_df, x_label, q25, q75) {
  ggplot(pred_df, aes(x = x, y = estimate)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.18, fill = "#4C78A8") +
    geom_vline(xintercept = c(q25, q75), linetype = "dashed", linewidth = 0.6, color = "#7A7A7A") +
    annotate(
      "text",
      x = q25,
      y = 1.08,
      label = "P25",
      family = "Microsoft YaHei",
      size = 3.6,
      color = "#5A5A5A"
    ) +
    annotate(
      "text",
      x = q75,
      y = 1.08,
      label = "P75",
      family = "Microsoft YaHei",
      size = 3.6,
      color = "#5A5A5A"
    ) +
    geom_line(linewidth = 1.1, color = "#1F4E79") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    coord_cartesian(ylim = c(0, 1), clip = "off") +
    labs(
      x = x_label,
      y = "预测概率",
      caption = "注：其余变量固定在样本均值或参照组，图中展示的是固定效应层面的预测概率及 95% 置信区间。"
    ) +
    theme_bw(base_size = 13, base_family = "Microsoft YaHei") +
    theme(
      text = element_text(family = "Microsoft YaHei"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", family = "Microsoft YaHei"),
      plot.caption = element_text(hjust = 0, size = 10.5, margin = margin(t = 8)),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )
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

ame_participation <- compute_ame(
  table1_models$A,
  "w1_buxi_rate_atomos_z",
  "班级平均参与氛围"
)

shift_participation <- compute_probability_shift(
  table1_models$A,
  "w1_buxi_rate_atomos_z",
  "班级平均参与氛围"
)

pred_plot_1 <- build_prediction_df(table1_models$A, ceps_entry, "w1_buxi_rate_atomos_z")
fig_1 <- plot_prediction_curve(
  pred_plot_1,
  "班级平均参与氛围（标准化）",
  shift_participation$q25,
  shift_participation$q75
)

ggsave(
  filename = here(output_dir, "adv_pred_prob_plot_1.svg"),
  plot = fig_1,
  device = function(...) grDevices::svg(..., family = "Microsoft YaHei"),
  width = 8,
  height = 5,
  bg = "white"
)

cli::cli_alert_success("推进部分第一阶段的模型拟合完成。")
cli::cli_alert_info("表 1 输出: {.file {here(output_dir, 'adv_regression_table_1.typ')}}")
cli::cli_alert_info("表 2 输出: {.file {here(output_dir, 'adv_regression_table_2.typ')}}")
cli::cli_alert_info("表 3 输出: {.file {here(output_dir, 'adv_regression_table_3.typ')}}")
cli::cli_alert_info("图 1 输出: {.file {here(output_dir, 'adv_pred_prob_plot_1.svg')}}")

cat("\n=== 表1A 平均边际效应（概率尺度）===\n")
print(
  ame_participation |>
    mutate(across(where(is.numeric), ~ round(.x, 4))),
  n = Inf
)

cat("\n=== 表1A 从 P25 到 P75 的平均预测概率变化 ===\n")
print(
  shift_participation |>
    mutate(across(where(is.numeric), ~ round(.x, 4))),
  n = Inf
)
