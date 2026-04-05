# scripts/04_rep_robust_hetero.R
# 目标：复刻论文异质性分析和稳健性检验，输出 Typst 表格
# 输入：data/rep_output/CEPS_prepared.rds
# 输出：data/rep_output/robust_table.typ, data/rep_output/hetero_table.typ

suppressPackageStartupMessages({
  library(tidyverse)
  library(bruceR)
  library(here)
  library(glmmTMB)
  library(modelsummary)
  library(tinytable)
})

output_dir <- here("data", "rep_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 通用控制变量列表
# fmt: skip
ctrl_formula <- str_c(
  c(
    # 家长层面
    "is_father", "parage", "SES",
    # 学生层面
    "cog3pl", "stsex", "ethgro", "jisu", "sibling", "juzhu",
    # 班级层面
    "clgrade", "smoke", "game",
    # 学校层面
    "sch_loc", "sch_rank", "sch_par_edu", "sch_par_income", "sch_teacher_edu",
    # 基期教育期望
    "w1_qiwang"
  ),
  collapse = " + "
)

# %% 稳健性检验 - 随机分班
cli::cli_alert_info("正在进行稳健性检验 - 随机分班...")

ceps_random <- import(here(output_dir, "CEPS_prepared.rds"), as = "tbl") |>
  filter(is_random_class == 1)

# 构建模型拟合公式辅助函数
build_formula <- function(dv, iv_touru, lagged_dv) {
  str_glue(
    "{dv} ~ w1_qiwang_atomos_z + {iv_touru} + {lagged_dv} + {ctrl_formula} + (1 | schids/clsids)"
  ) |>
    as.formula()
}

robust_models_list <- list(
  "学业辅导" = glmmTMB(
    build_formula("w2_jiandu", "w1_jiandu_atomos_z", "w1_jiandu"),
    data = ceps_random,
    family = gaussian(),
    REML = TRUE
  ),
  "教养方式" = glmmTMB(
    build_formula("w2_jiaoyang", "w1_jiaoyang_atomos_z", "w1_jiaoyang"),
    data = ceps_random,
    family = gaussian(),
    REML = TRUE
  ),
  "课外班参与" = glmmTMB(
    build_formula("w2_buxi", "w1_buxi_rate_atomos_z", "w1_buxi"),
    data = ceps_random,
    family = gaussian(),
    REML = TRUE
  ),
  "金钱投入" = glmmTMB(
    build_formula("w2_buxi_money_log", "w1_buxi_money_atomos_z", "w1_buxi_money_log"),
    data = ceps_random,
    family = gaussian(),
    REML = TRUE
  ),
  "补习时间" = glmmTMB(
    build_formula("w2_buxi_time_log", "w1_buxi_time_atomos_z", "w1_buxi_time_log"),
    data = ceps_random,
    family = gaussian(),
    REML = TRUE
  ),
  "家校联系" = glmmTMB(
    build_formula("w2_contact_z", "w1_contact_atomos_z", "w1_contact_z"),
    data = ceps_random,
    family = gaussian(),
    REML = TRUE
  )
)

# 精简系数映射字典
coef_map_robust <- c(
  "w1_qiwang_atomos_z" = "班级家长期望氛围",
  "w1_jiandu_atomos_z" = "班级家长投入氛围",
  "w1_jiaoyang_atomos_z" = "班级家长投入氛围",
  "w1_buxi_rate_atomos_z" = "班级家长投入氛围",
  "w1_buxi_money_atomos_z" = "班级家长投入氛围",
  "w1_buxi_time_atomos_z" = "班级家长投入氛围",
  "w1_contact_atomos_z" = "班级家长投入氛围"
)

# 稳健性检验 GOF 指标提取函数
build_robust_gof <- function(models) {
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

# 生成稳健性检验结果表格
tab_robust <- modelsummary(
  robust_models_list,
  output = "tinytable",
  coef_map = coef_map_robust,
  gof_omit = ".*",
  add_rows = build_robust_gof(robust_models_list),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = TRUE,
  notes = c(
    "注：因篇幅有限，本表中的控制变量和基期因变量的结果未报告，控制变量与表 2 相同。",
    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  )
) |>
  theme_empty() |>
  # 手动添加表格线
  style_tt(i = 0, line = "t", line_width = 0.1) |>
  style_tt(i = 0, line = "b", line_width = 0.05) |>
  style_tt(i = 4, line = "b", line_width = 0.05) |>
  style_tt(i = 7, line = "b", line_width = 0.1) |>
  # 文本对齐方式设置
  style_tt(i = 0, bold = TRUE, align = "c") |>
  style_tt(j = 2:7, align = "c") |>
  format_tt(j = 2:7, escape = TRUE) |>
  format_tt("notes", escape = TRUE)

save_tt(tab_robust, here(output_dir, "robust_table.typ"), overwrite = TRUE)

cli::cli_alert_success("Robustness Check Models Fitted and Table Generated!")
cli::cli_alert_info("Output: {.file {here(output_dir, 'robust_table.typ')}}")

# %% 异质性分析
cli::cli_alert_info("正在进行异质性分析...")

ceps <- import(here(output_dir, "CEPS_prepared.rds"), as = "tbl") |>
  mutate(know_other_par = as.numeric(know_other_par) - 1)

# 辅助函数：生成异质性分析模型公式
build_hetero_formula <- function(dv, iv_touru, lagged_dv, moderator) {
  str_glue(
    "{dv} ~ w1_qiwang_atomos_z + {iv_touru} * {moderator} + {lagged_dv} + {ctrl_formula} + (1 | schids/clsids)"
  ) |>
    as.formula()
}

# 六个模型的变量映射列表
# fmt: skip
dv_list <- c(
  "w2_jiandu", "w2_jiaoyang", "w2_buxi",
  "w2_buxi_money_log", "w2_buxi_time_log", "w2_contact_z"
)
# fmt: skip
iv_list <- c(
  "w1_jiandu_atomos_z", "w1_jiaoyang_atomos_z", "w1_buxi_rate_atomos_z",
  "w1_buxi_money_atomos_z", "w1_buxi_time_atomos_z", "w1_contact_atomos_z"
)
# fmt: skip
lag_list <- c(
  "w1_jiandu", "w1_jiaoyang", "w1_buxi",
  "w1_buxi_money_log", "w1_buxi_time_log", "w1_contact_z"
)
# fmt: skip
model_names <- c(
  "学业辅导", "教养方式", "课外班参与",
  "金钱投入", "补习时间", "家校联系"
)

# 拟合特定调节变量的 6 个模型
fit_hetero_models <- function(moderator) {
  models <- list()
  for (i in seq_along(dv_list)) {
    f <- build_hetero_formula(dv_list[i], iv_list[i], lag_list[i], moderator)
    models[[model_names[i]]] <- glmmTMB(f, data = ceps, family = gaussian(), REML = TRUE)
  }
  models
}

# 拟合三组异质性模型: 家长网络融入度、家长性别、家庭社会经济地位
models_net <- fit_hetero_models("know_other_par")
models_sex <- fit_hetero_models("is_father")
models_ses <- fit_hetero_models("SES")

# 指标函数
build_hetero_coef_map <- function(moderator, mod_label) {
  cmap <- c()
  for (iv in iv_list) {
    cmap[iv] <- "班级家长投入氛围"
  }
  cmap[moderator] <- mod_label
  for (iv in iv_list) {
    cmap[paste0(iv, ":", moderator)] <- paste0("班级家长投入氛围 × ", mod_label)
    cmap[paste0(moderator, ":", iv)] <- paste0("班级家长投入氛围 × ", mod_label)
  }
  cmap
}

# 提取单块数据框
get_hetero_df <- function(models, coef_map) {
  modelsummary(
    models,
    output = "data.frame",
    coef_map = coef_map,
    gof_map = list(list(raw = "nobs", clean = "样本量", fmt = 0)),
    stars = TRUE
  ) |>
    select(-part, -statistic) |>
    mutate(term = replace_na(term, ""))
}

coef_map_net <- build_hetero_coef_map("know_other_par", "认识班内其他家长")
coef_map_sex <- build_hetero_coef_map("is_father", "父亲")
coef_map_ses <- build_hetero_coef_map("SES", "家长社会经济地位")

df_net <- get_hetero_df(models_net, coef_map_net)
df_sex <- get_hetero_df(models_sex, coef_map_sex)
df_ses <- get_hetero_df(models_ses, coef_map_ses)

final_hetero_df <- bind_rows(df_net, df_sex, df_ses)
colnames(final_hetero_df)[1] <- " " # 将变量名称列的表头留空

# 计算插入组标题行在拼接后的确切索引位置
idx_net <- 1
idx_sex <- nrow(df_net) + 1
idx_ses <- nrow(df_net) + nrow(df_sex) + 1

# 计算标准误所在的行号
se_rows <- c(
  idx_net + 1,
  idx_net + 3,
  idx_net + 5,
  idx_sex + 1,
  idx_sex + 3,
  idx_sex + 5,
  idx_ses + 1,
  idx_ses + 3,
  idx_ses + 5
)
final_hetero_df[[1]][se_rows] <- "" # 将标准误所在行的变量名称列置空

group_list <- list(
  "家长网络融入度的异质性" = idx_net,
  "家长性别的异质性" = idx_sex,
  "阶层的异质性" = idx_ses
)

tab_hetero <- final_hetero_df |>
  tt(
    notes = c(
      "注：因篇幅有限，本表中的控制变量和基期因变量的结果未报告，控制变量与表 2 相同。",
      "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
    )
  ) |>
  # 插入分组标题
  group_tt(i = group_list) |>
  # 划定表格线条
  style_tt(i = 0, line = "t", line_width = 0.1) |>
  style_tt(i = 0, line = "b", line_width = 0.05) |>
  style_tt(i = idx_sex, line = "b", line_width = 0.05) |>
  style_tt(i = 1 + idx_ses, line = "b", line_width = 0.05) |>
  style_tt(i = nrow(final_hetero_df) + 3, line = "b", line_width = 0.1) |>
  # 文本样式
  style_tt(i = 0, bold = TRUE, align = "c") |>
  style_tt(j = 2:7, align = "c") |>
  style_tt(i = c(idx_net, idx_sex + 1, idx_ses + 2), align = "c", bold = TRUE) |>
  # 转义字符
  format_tt(j = 2:7, escape = TRUE) |>
  format_tt("notes", escape = TRUE)

save_tt(tab_hetero, here(output_dir, "hetero_table.typ"), overwrite = TRUE)

cli::cli_alert_success("Heterogeneity Models Fitted and Table Generated!")
cli::cli_alert_info("Output: {.file {here(output_dir, 'hetero_table.typ')}}")
