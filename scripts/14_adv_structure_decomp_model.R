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
  library(marginaleffects)
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
  "是否进入课外班竞争" = model_entry,
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
  width = c(0.25, 0.21, 0.17, 0.17),
  notes = c(
    "注：表中仅报告两项核心分解变量的估计结果，其他控制变量与表 5–7 保持一致。第一列因变量为第二期是否参加课外班，样本限制为基期未参加课外班的家长；第二、三列分别以正值样本中的课外班金钱投入对数和补习时间对数为因变量。学校平均氛围和班级相对学校平均偏离均由班级层整体均值分解得到。",
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

ame_decomp <- bind_rows(
  compute_ame(model_entry, "sch_mean_w1_buxi_rate_atomos_z", "学校平均氛围"),
  compute_ame(model_entry, "cls_dev_from_sch_w1_buxi_rate_atomos_z", "班级相对学校平均偏离")
)

shift_decomp <- bind_rows(
  compute_probability_shift(model_entry, "sch_mean_w1_buxi_rate_atomos_z", "学校平均氛围"),
  compute_probability_shift(
    model_entry,
    "cls_dev_from_sch_w1_buxi_rate_atomos_z",
    "班级相对学校平均偏离"
  )
)

pred_plot_2 <- build_prediction_df(model_entry, ceps_entry, "sch_mean_w1_buxi_rate_atomos_z")
fig_2 <- plot_prediction_curve(
  pred_plot_2,
  "学校平均参与氛围（标准化）",
  shift_decomp$q25[shift_decomp$label == "学校平均氛围"],
  shift_decomp$q75[shift_decomp$label == "学校平均氛围"]
)

ggsave(
  filename = here(output_dir, "adv_pred_prob_plot_2.svg"),
  plot = fig_2,
  device = function(...) grDevices::svg(..., family = "Microsoft YaHei"),
  width = 8,
  height = 5,
  bg = "white"
)

cli::cli_alert_success("推进部分第二阶段的模型拟合完成。")
cli::cli_alert_info("模型表格输出: {.file {here(output_dir, 'adv_structure_decomp_table.typ')}}")
cli::cli_alert_info("图 2 输出: {.file {here(output_dir, 'adv_pred_prob_plot_2.svg')}}")

cat("\n=== 结构分解模型平均边际效应（概率尺度）===\n")
print(
  ame_decomp |>
    mutate(across(where(is.numeric), ~ round(.x, 4))),
  n = Inf
)

cat("\n=== 结构分解模型从 P25 到 P75 的平均预测概率变化 ===\n")
print(
  shift_decomp |>
    mutate(across(where(is.numeric), ~ round(.x, 4))),
  n = Inf
)
