# scripts/2_regress_model.R
# 目标：复刻论文模型拟合，输出 Typst 表格
# 输入：data/CEPS_prepared.rds
# 输出：data/output/regression_table.typ

suppressPackageStartupMessages({
  library(tidyverse)
  library(bruceR)
  library(here)
  library(modelsummary)
  library(tinytable)
})

output_dir <- here("data", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ceps <- import(here("data", "CEPS_prepared.rds"), as = "tbl")

# %% 模型公式准备
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

# 构建模型拟合公式辅助函数
build_formula <- function(dv, iv_touru, lagged_dv) {
  # {dv}: 因变量
  # w1_qiwang_atomos_z: 核心自变量-班级家长期望氛围
  # {iv_touru}: 核心自变量-相应班级投入氛围
  # {lagged_dv}: 相应的滞后因变量
  # {ctrl_formula}: 通用控制变量列表
  # (1 | schids/clsids): 随机效应-学校/班级
  str_glue(
    "{dv} ~ w1_qiwang_atomos_z + {iv_touru} + {lagged_dv} + {ctrl_formula} + (1 | schids/clsids)"
  ) |>
    as.formula()
}

# %% 模型拟合
models_list <- list(
  "学业辅导" = lmer(
    build_formula("w2_jiandu", "w1_jiandu_atomos_z", "w1_jiandu"),
    data = ceps
  ),
  "教养方式" = lmer(
    build_formula("w2_jiaoyang", "w1_jiaoyang_atomos_z", "w1_jiaoyang"),
    data = ceps
  ),
  "课外班参与" = lmer(
    build_formula("w2_buxi", "w1_buxi_rate_atomos_z", "w1_buxi"),
    data = ceps
  ),
  "金钱投入" = lmer(
    build_formula("w2_buxi_money_log", "w1_buxi_money_atomos_z", "w1_buxi_money_log"),
    data = ceps
  ),
  "补习时间" = lmer(
    build_formula("w2_buxi_time_log", "w1_buxi_time_atomos_z", "w1_buxi_time_log"),
    data = ceps
  ),
  "家校联系" = lmer(
    build_formula("w2_contact_z", "w1_contact_atomos_z", "w1_contact_z"),
    data = ceps
  )
)

# %% 模型结果整理
if (!exists("models_list")) {
  models_list <- import(here("data", "output", "original_models.rds"))
}

# 自定义 GOF 指标计算函数
build_custom_gof <- function(models) {
  map_dfc(models, function(m) {
    model_data <- m@frame
    is_glmer <- inherits(m, "glmerMod")

    # 1. 样本量与群组数
    n_obs <- nobs(m)
    n_sch <- n_distinct(model_data$schids)
    n_cls <- n_distinct(model_data$clsids)

    # 2. 提取方差分量计算 ICC
    vc <- as_tibble(lme4::VarCorr(m))
    var_sch <- vc |> filter(grp == "schids") |> pull(vcov)
    var_cls <- vc |> filter(grepl("clsids", grp)) |> pull(vcov)
    var_res <- if (is_glmer) {
      (pi^2) / 3
    } else {
      vc |> filter(grp == "Residual") |> pull(vcov)
    }

    total_var <- var_sch + var_cls + var_res
    icc_sch <- var_sch / total_var
    icc_cls <- (var_sch + var_cls) / total_var

    # 3. 似然比检验 (LRT)
    f_fixed <- reformulas::nobars(formula(m))

    if (is_glmer) {
      m_null <- glm(f_fixed, data = model_data, family = family(m))
      ll_full <- logLik(m)
    } else {
      m_null <- lm(f_fixed, data = model_data)
      ll_full <- logLik(update(m, REML = FALSE))
    }
    ll_null <- logLik(m_null)

    lrt_stat <- max(2 * (as.numeric(ll_full) - as.numeric(ll_null)), 0)
    df_diff <- attr(ll_full, "df") - attr(ll_null, "df")
    p_val <- pchisq(lrt_stat, df = df_diff, lower.tail = FALSE)

    p_str <- case_when(
      is.na(p_val) ~ "",
      p_val < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", p_val)
    )

    # 4. 返回字符向量
    c(
      sprintf("%.3f", icc_cls),
      sprintf("%.3f", icc_sch),
      p_str,
      as.character(n_obs),
      as.character(n_sch),
      as.character(n_cls)
    )
  }) |>
    set_names(names(models)) |>
    add_column(
      term = c("ICC(班级 | 学校)", "ICC(学校)", "似然比检验($p$ 值)", "样本量", "学校数量", "班级数量"),
      .before = 1
    )
}

# 系数名称映射
coef_map <- c(
  "w1_qiwang_atomos_z" = "班级家长期望氛围",

  # 核心自变量对齐
  "w1_jiandu_atomos_z" = "班级家长投入氛围",
  "w1_jiaoyang_atomos_z" = "班级家长投入氛围",
  "w1_buxi_rate_atomos_z" = "班级家长投入氛围",
  "w1_buxi_money_atomos_z" = "班级家长投入氛围",
  "w1_buxi_time_atomos_z" = "班级家长投入氛围",
  "w1_contact_atomos_z" = "班级家长投入氛围",

  # 家长与个体层面
  "is_father" = "父亲",
  "parage" = "家长年龄",
  "SES" = "家庭社会经济地位",
  "cog3pl" = "认知能力得分",
  "stsex" = "男生",
  "ethgro" = "汉族",
  "jisu" = "平时寄宿",
  "sibling" = "兄弟姐妹数量",

  # 居住状况
  "juzhu父母都在家" = "父母都在家",
  "juzhu只有母亲在家" = "只有母亲在家",
  "juzhu只有父亲在家" = "只有父亲在家",

  # 班级层面
  "clgrade" = "校内班级排名",
  "smoke" = "班内抽烟喝酒学生人数",
  "game" = "班内去网吧游戏厅学生人数",

  # 学校层面
  "sch_loc市/县的中心城区" = "中心城区",
  "sch_loc市/县的边缘城区" = "边缘城区",
  "sch_loc市/县的城乡结合部" = "城乡结合部",
  "sch_loc市/县外的镇" = "市/县外的镇",

  "sch_rank" = "区县内学校排名",
  "sch_par_edu" = "学校家长平均受教育程度",
  "sch_par_income" = "学校家长平均职业收入",
  "sch_teacher_edu" = "学校教师平均受教育年限",

  # 滞后项
  "w1_qiwang" = "基期教育期望",
  "w1_jiandu" = "基期因变量",
  "w1_jiaoyang" = "基期因变量",
  "w1_buxi" = "基期因变量",
  "w1_buxi_money_log" = "基期因变量",
  "w1_buxi_time_log" = "基期因变量",
  "w1_contact_z" = "基期因变量"
)

# 生成回归结果表格
tab_reg <- modelsummary(
  models_list,
  output = "tinytable",
  coef_map = coef_map,
  gof_omit = ".*",
  add_rows = build_custom_gof(models_list),
  stars = TRUE,
) |>
  theme_empty() |>
  group_tt(
    i = list(
      "居住状况 (父母都不在家 $= 0$)" = 21,
      "学校地理位置 (农村 $= 0$)" = 33
    )
  ) |>
  # 手动添加表格线
  style_tt(i = 0, line = "t", line_width = 0.1) |>
  style_tt(i = 0, line = "b", line_width = 0.05) |>
  style_tt(i = 4, line = "b", line_width = 0.05) |>
  style_tt(i = 51, line = "t", line_width = 0.05) |>
  style_tt(i = 55, line = "t", line_width = 0.05) |>
  style_tt(i = 60, line = "b", line_width = 0.1) |>
  # 表格内部样式
  style_tt(i = 0, bold = TRUE) |>
  style_tt(i = c(21, 34), align = "l", bold = FALSE) |>
  style_tt(j = 2:7, align = "c") |>
  style_tt(i = 22:27, j = 1, indent = 1) |>
  style_tt(i = 35:42, j = 1, indent = 1) |>
  # 转义特殊字符
  format_tt(j = 2:7, escape = TRUE) |>
  format_tt("notes", escape = TRUE)

save_tt(tab_reg, here(output_dir, "regression_table.typ"), overwrite = TRUE)

cli::cli_alert_success("Models Fitted and Table Generated!")
cli::cli_alert_info("Output: {.file {here(output_dir, 'regression_table.typ')}}")
