# scripts/02_rep_descriptive_analyze.R
# 目标：使用 tinytable 生成可直接用于 Typst 的描述性统计表
# 输入：data/rep_output/CEPS_prepared.rds
# 输出：data/rep_output/descriptive_table.typ

suppressPackageStartupMessages({
  library(tidyverse)
  library(bruceR)
  library(tinytable)
  library(here)
})

output_dir <- here("data", "rep_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ceps <- import(here(output_dir, "CEPS_prepared.rds"), as = "tbl") |>
  select(-ids, -clsids, -schids, -is_random_class)

# 变量标签
var_labels <- list(
  # --- 因变量 ---
  w2_jiandu = "家长每周监督辅导功课频率(标准分)",
  w2_jiaoyang = "教养方式严格程度(标准分)",
  w2_buxi = "是否参与课外班",
  w2_buxi_money_log = "课外班金钱投入对数",
  w2_buxi_time_log = "学业辅导班每周课时对数",
  w2_contact_z = "是否主动与老师联络(标准分)",

  # --- 自变量：班级氛围 ---
  w1_qiwang_atomos_z = "班级家长平均教育期望(标准分)",
  w1_jiandu_atomos_z = "监督辅导班级均值(标准分)",
  w1_jiaoyang_atomos_z = "教养严格性班级均值(标准分)",
  w1_buxi_rate_atomos_z = "班级课外班参与率(标准分)",
  w1_buxi_money_atomos_z = "课外班金钱投入班级均值(标准分)",
  w1_buxi_time_atomos_z = "补习时间班级均值(标准分)",
  w1_contact_atomos_z = "家校联系班级均值(标准分)",

  # --- 滞后项 (基期个体特征) ---
  w1_qiwang = "家长期望(基期滞后项)",
  w1_jiandu = "家长监督辅导(基期滞后项)",
  w1_jiaoyang = "教养方式严格程度(基期滞后项)",
  w1_buxi = "是否参与课外班(基期滞后项)",
  w1_buxi_money_log = "课外班金钱投入(基期滞后项)",
  w1_buxi_time_log = "补习时间(基期滞后项)",
  w1_contact_z = "家校联系频率(基期滞后项)",

  # --- 控制变量：家长层面 ---
  is_father = "父亲",
  parage = "家长年龄",
  parhk = "非农/居民户籍",
  parjob_3c = "职业",
  edu = "受教育程度",
  eduy = "受教育年限",
  wealth = "自评家庭经济状况",
  party = "党员身份",
  SES = "家长社会经济地位(标准分)",
  know_other_par = "是否认识班内其他家长",

  # --- 控制变量：子女层面 ---
  stsex = "男生",
  onechild = "独生子女",
  sibling = "兄弟姐妹数量",
  ethgro = "汉族",
  cog3pl = "认知能力得分",
  jisu = "平时寄宿",
  juzhu = "父母在家中居住状况",

  # --- 控制变量：班级层面 ---
  clgrade = "校内班级排名",
  smoke = "班内抽烟喝酒学生人数",
  game = "班内去网吧游戏厅学生人数",

  # --- 控制变量：学校层面 ---
  sch_rank = "区县内学校排名",
  sch_par_edu = "学校家长平均受教育程度",
  sch_par_income = "学校家长平均职业收入",
  sch_teacher_edu = "学校教师平均受教育年限",
  sch_loc = "学校所在地理位置"
)

# 分组及顺序
var_groups <- list(
  "因变量：家长教育投入（第二期）" = c(
    "w2_jiandu",
    "w2_jiaoyang",
    "w2_buxi",
    "w2_buxi_money_log",
    "w2_buxi_time_log",
    "w2_contact_z"
  ),
  "自变量：班级家长氛围（基期）" = c(
    "w1_qiwang_atomos_z",
    "w1_jiandu_atomos_z",
    "w1_jiaoyang_atomos_z",
    "w1_buxi_rate_atomos_z",
    "w1_buxi_money_atomos_z",
    "w1_buxi_time_atomos_z",
    "w1_contact_atomos_z"
  ),
  "滞后项：个体基期变量（第一期）" = c(
    "w1_qiwang",
    "w1_jiandu",
    "w1_jiaoyang",
    "w1_buxi",
    "w1_buxi_money_log",
    "w1_buxi_time_log",
    "w1_contact_z"
  ),
  "控制变量：家长层面" = c(
    "is_father",
    "parage",
    "parhk",
    "parjob_3c",
    "edu",
    "eduy",
    "wealth",
    "party",
    "SES",
    "know_other_par"
  ),
  "控制变量：子女层面" = c(
    "stsex",
    "onechild",
    "sibling",
    "ethgro",
    "cog3pl",
    "jisu",
    "juzhu"
  ),
  "控制变量：班级层面" = c("clgrade", "smoke", "game"),
  "控制变量：学校层面" = c(
    "sch_rank",
    "sch_par_edu",
    "sch_par_income",
    "sch_teacher_edu",
    "sch_loc"
  )
)

# 格式化函数
fmt_n <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

fmt_num <- function(x) {
  formatC(x, format = "f", digits = 2, big.mark = ",")
}

# 统计单变量的描述性信息
summarise_one_var <- function(data, var) {
  x <- data[[var]]
  label <- var_labels[[var]]

  # 分类变量：返回 1 行主标题 + N 行子类别
  if (is.factor(x) || is.character(x)) {
    valid <- x[!is.na(x)]
    if (length(valid) == 0) {
      return(tibble(row_type = "cat_label", 变量 = label))
    }

    tab <- table(valid, useNA = "no")
    pct <- as.numeric(tab) / sum(tab) * 100

    bind_rows(
      tibble(row_type = "cat_label", 变量 = label),
      tibble(
        row_type = "level",
        变量 = names(tab),
        `样本量/频数(%)` = sprintf("%s (%.1f%%)", fmt_n(as.integer(tab)), pct)
      )
    )
  } else {
    # 连续变量：返回 1 行统计结果
    x_num <- as.numeric(x)
    n_val <- sum(!is.na(x_num))

    tibble(
      row_type = "numeric",
      变量 = label,
      `样本量/频数(%)` = fmt_n(n_val),
      均值 = if (n_val > 0) fmt_num(mean(x_num, na.rm = TRUE)) else "",
      标准差 = if (n_val > 1) fmt_num(sd(x_num, na.rm = TRUE)) else "",
      最小值 = if (n_val > 0) fmt_num(min(x_num, na.rm = TRUE)) else "",
      最大值 = if (n_val > 0) fmt_num(max(x_num, na.rm = TRUE)) else ""
    )
  }
}

# 生成统计数据
desc_df <- imap_dfr(var_groups, function(vars, grp) {
  map_dfr(vars, ~ summarise_one_var(ceps, .x)) |>
    mutate(group = grp, .before = 1)
}) |>
  mutate(across(everything(), ~ replace_na(.x, "")))

# 计算分组行号
group_index <- desc_df |>
  mutate(orig_id = row_number()) |>
  group_by(group) |>
  summarise(start = min(orig_id), .groups = "drop") |>
  arrange(start)

group_spec <- setNames(as.list(group_index$start), group_index$group)

desc_df <- desc_df |>
  mutate(
    orig_id = row_number(),
    shift = map_int(orig_id, ~ sum(group_index$start <= .x)),
    new_id = orig_id + shift
  )

idx_group <- group_index$start + seq_len(nrow(group_index)) - 1L
idx_group_lines <- idx_group[-1]
idx_var <- desc_df |>
  filter(row_type %in% c("numeric", "cat_label")) |>
  pull(new_id)
idx_level <- desc_df |>
  filter(row_type == "level") |>
  pull(new_id)

# 生成 tinytable 并输出 Typst
ttbl <- desc_df |>
  select(变量, `样本量/频数(%)`, 均值, 标准差, 最小值, 最大值) |>
  tt() |>
  style_tt(i = 0, bold = TRUE) |>
  style_tt(j = 2:6, align = "c") |>
  group_tt(i = group_spec) |>
  style_tt(i = idx_group, bold = TRUE) |>
  style_tt(i = idx_group_lines, line = "t", line_width = 0.05) |>
  style_tt(i = idx_var, j = 1, indent = 1) |>
  style_tt(i = idx_level, j = 1, indent = 2)

save_tt(ttbl, here(output_dir, "descriptive_table.typ"), overwrite = TRUE)

cli::cli_alert_success("Descriptive tinytable completed.")
cli::cli_alert_info("Output: {.file {here(output_dir, 'descriptive_table.typ')}}")
