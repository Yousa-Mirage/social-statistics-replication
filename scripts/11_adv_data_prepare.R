# scripts/11_adv_data_prepare.R
# 目标：完成数据合并、清洗与变量准备。
# 输入：data/CEPSw1/*.dta, data/CEPSw2/*.dta
# 输出：data/adv_output/CEPS_prepared.rds

suppressPackageStartupMessages({
  library(tidyverse)
  library(bruceR)
  library(here)
})

output_dir <- here("data", "adv_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# %% 1 读取并合并 W1/W2 原始数据 ------------------------------------------------

read_ceps_dta <- function(path) {
  miss_codes <- c(-1, -2, -3, -4, -5, -6, -7, -8, -9, -88, -99, -888, -999, -9999, -99999) # nolint: object_usage_linter

  import(path, as = "tbl") |>
    mutate(
      across(everything(), ~ haven::zap_missing(haven::zap_labels(.x))),
      across(
        where(is.numeric),
        ~ if_else(.x %in% miss_codes, NA_real_, as.numeric(.x))
      ),
    ) |>
    mutate(across(where(is.character), ~ na_if(trimws(.x), "")))
}

w1_student <- read_ceps_dta(here("data", "CEPSw1", "学生数据.dta"))
w1_parent <- read_ceps_dta(here("data", "CEPSw1", "家长数据.dta")) |>
  select(ids, !any_of(names(w1_student)))
w1_school <- read_ceps_dta(here("data", "CEPSw1", "学校数据.dta")) |>
  select(schids, !any_of(names(w1_student)))
w1_class <- read_ceps_dta(here("data", "CEPSw1", "班级数据.dta")) |>
  select(clsids, !any_of(names(w1_student)))

w2_student <- read_ceps_dta(here("data", "CEPSw2", "学生数据.dta")) |>
  select(ids, !any_of(names(w1_student)))
w2_parent <- read_ceps_dta(here("data", "CEPSw2", "家长数据.dta")) |>
  select(ids, !any_of(names(w1_student)))

# 合并数据，筛选样本
ceps <- w1_student |>
  left_join(w2_student, by = "ids") |>
  left_join(w1_parent, by = "ids") |>
  left_join(w2_parent, by = "ids") |>
  left_join(w1_class, by = "clsids") |>
  left_join(w1_school, by = "schids") |>
  filter(
    grade9 == 0,
    ba01 %in% c(1, 2),
    w2ba01 %in% c(1, 2),
    ba01 == w2ba01
  )

# %% 2 控制变量准备 ------------------------------------------------
## %% 子女层面变量 ------------------------------------------------
ceps <- ceps |>
  mutate(
    # 性别：男为 1，女为 0
    stsex = stsex,
    # 独生子女：独生子女为 1，非独生子女为 0
    onechild = case_when(b01 == 1 ~ 1, b01 == 2 ~ 0, TRUE ~ NA_real_),
    # 兄弟姐妹数量
    sibling = rowSums(pick(num_range("b020", 1:4)), na.rm = TRUE),
    # 民族：汉族为 1，其他民族为 0
    ethgro = case_when(
      a03 == 1 ~ 1,
      a03 %in% 2:8 ~ 0,
      TRUE ~ NA_real_
    ),
    # 认知能力得分
    cog3pl = cog3pl,
    # 基线学业表现：三科标准化期中成绩至少有两科非缺失时取平均
    baseline_perf = if_else(
      rowSums(!is.na(pick(c(stdchn, stdmat, stdeng)))) >= 2,
      rowMeans(pick(c(stdchn, stdmat, stdeng)), na.rm = TRUE),
      NA_real_
    ),
    baseline_perf_z = as.numeric(scale(baseline_perf)),
    # 寄宿：W1 缺失时用 W2(w2b15) 回填 c18
    jisu = case_when(
      coalesce(c18, w2b15) == 1 ~ 1,
      coalesce(c18, w2b15) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # 父母居住情况
    juzhu = factor(
      stlb_4c,
      levels = c(4, 1, 2, 3),
      labels = c("父母都不在家", "父母都在家", "只有母亲在家", "只有父亲在家")
    ),
  )

ceps |>
  select(stsex, onechild, sibling, ethgro, cog3pl, baseline_perf, baseline_perf_z, jisu) |>
  Describe()
Freq(ceps$juzhu)

## %% 家长层面变量 ------------------------------------------------
ceps <- ceps |>
  mutate(
    # 身份：父亲为 1，母亲为 2
    shenfen = ba01,
    is_father = as.integer(ba01 == 1),

    # 年龄
    parage = case_when(
      !is.na(be01) ~ 2013 - be01,
      shenfen == 1 ~ 2013 - w2be01,
      shenfen == 2 ~ 2013 - w2be10,
      TRUE ~ NA_real_
    ),
    parage = if_else(between(parage, 18, 80), parage, NA_real_),

    # 户籍类型：农村为 0，城市为 1
    w2_hk_raw = case_when(
      ba01 == 1 ~ w2be04,
      ba01 == 2 ~ w2be13,
      TRUE ~ NA_real_
    ),
    parhk = coalesce(be04, w2_hk_raw),
    parhk = case_when(
      parhk == 1 ~ 0,
      parhk %in% c(2, 3) ~ 1,
      TRUE ~ NA_real_
    ),

    # 职业
    parjob_w1_coded = case_when(
      be08 %in% 1:3 ~ 1, # 管理类与技术类
      be08 %in% 4:8 | be08 == 10 ~ 2, # 一般职工与农民
      be08 == 9 ~ 3, # 无业、失业、下岗
      TRUE ~ NA_real_
    ),
    raw_w2_val = case_when(
      ba01 == 1 ~ w2be09,
      ba01 == 2 ~ w2be18,
      TRUE ~ NA_real_
    ),
    parjob_w2_coded = case_when(
      raw_w2_val %in% 1:7 ~ 1, # 管理类与技术类
      raw_w2_val %in% 8:12 ~ 2, # 一般职工与农民
      raw_w2_val == 13 ~ 3, # 无业、失业、下岗
      TRUE ~ NA_real_
    ),
    parjob_3c = factor(
      coalesce(parjob_w1_coded, parjob_w2_coded),
      levels = 1:3,
      labels = c("管理类与技术类", "一般职工与农民", "无业、失业、下岗")
    ),

    # 教育年限（WARN：原来的Stata代码好像没有转为年限！？）
    # （需要分别算一下edu和eduy的平均值）
    w2_edu_raw = case_when(
      ba01 == 1 ~ w2be08,
      ba01 == 2 ~ w2be17,
      TRUE ~ NA_real_
    ),
    edu = coalesce(as.numeric(be07), as.numeric(w2_edu_raw)),
    eduy = case_when(
      edu == 1 ~ 0,
      edu == 2 ~ 6,
      edu == 3 ~ 9,
      edu %in% 4:6 ~ 12,
      edu == 7 ~ 15,
      edu == 8 ~ 16,
      edu == 9 ~ 19,
      TRUE ~ NA_real_
    ),

    # 经济状况：W1 缺失时使用 W2 回填
    wealth = coalesce(be19, w2be23),

    # 党员身份：W1 缺失时按父/母身份使用 W2 回填
    w2_party_raw = case_when(
      ba01 == 1 ~ w2be07,
      ba01 == 2 ~ w2be16,
      TRUE ~ NA_real_
    ),
    party_raw = coalesce(be06, w2_party_raw),
    party = case_when(
      party_raw == 1 ~ 1,
      party_raw %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    # 是否认识班内其他家长：推进分析只使用 W1，避免控制变量引入后续时期信息
    know_other_par = ordered(
      ba12,
      levels = 0:2,
      labels = c("不认识", "认识一部分", "全都认识")
    )
  )

# 家长社会经济地位（因子）
# WARN: 论文中说用受教育年限，但它受教育年限实际用的是序数变量受教育程度
ceps_ses <- ceps |>
  select(ids, parhk, parjob_3c, party, wealth, eduy) |>
  drop_na()
ses_pca_result <- PCA(
  data = ceps_ses,
  vars = c("parhk", "parjob_3c", "party", "wealth", "eduy"),
  nfactors = 1,
  rotation = "none",
  rev = c("parjob_3c"),
  plot.scree = FALSE
)
ceps <- ceps |>
  left_join(
    tibble(
      ids = ceps_ses$ids,
      SES = ses_pca_result$result$scores[, 1]
    ),
    by = "ids"
  )

ceps |>
  select(is_father, parage, parhk, parjob_3c, edu, eduy, wealth, party, SES, know_other_par) |>
  Describe()
for (v in c("parjob_3c", "know_other_par")) {
  cat("\nVariable:", v, "\n")
  Freq(ceps, v)
}

## %% 班级层面变量 ------------------------------------------------
ceps <- ceps |>
  mutate(
    # 校内班级排名
    clgrade = hra04,
    # 班级抽烟喝酒
    # WARN: 论文中这里似乎是0~3，但数据里是1~4.我认为这里没有必要转换，因为论文中游戏变量也没有转换
    smoke = hrb06,
    # 网吧游戏厅
    game = hrb07,
  )

ceps |>
  select(clgrade, smoke, game) |>
  Describe()

## %% 学校层面变量 ------------------------------------------------
ceps <- ceps |>
  mutate(
    # 区县内学校排名
    sch_rank = schrank_5c,
    # 学校地理位置
    sch_loc = factor(
      schloc_5c,
      levels = c(5, 1, 2, 3, 4),
      labels = c(
        "农村",
        "市/县的中心城区",
        "市/县的边缘城区",
        "市/县的城乡结合部",
        "市/县外的镇"
      )
    ),
    # 学校数据中已给出“本校家长平均教育程度/职业收入”
    sch_par_edu = plb08,
    sch_par_income = plb09,
    # 学校教师平均受教育年限
    # WARN: 如果缺失表示“学校没填”，那会把学校教师平均教育年限系统性压低
    across(
      c(plc0401a, plc0402a, plc0403a, plc0404a, plc0405a),
      ~ replace_na(as.numeric(.x), 0)
    ),
    total_teachers = plc0401a + plc0402a + plc0403a + plc0404a + plc0405a,
    sch_teacher_edu = if_else(
      total_teachers > 10,
      (plc0401a * 9 + plc0402a * 12 + plc0403a * 15 + plc0404a * 16 + plc0405a * 19) / total_teachers,
      NA_real_
    )
  )

ceps |>
  select(sch_rank, sch_par_edu, sch_par_income, sch_teacher_edu) |>
  Describe()
Freq(ceps$sch_loc)

# %% 3 因变量、自变量、滞后变量准备 ------------------------------------------------
## %% 滞后变量 ------------------------------------------------

ceps <- ceps |>
  mutate(
    # 是否参与补习
    w1_buxi = case_when(
      ba03 > 0 | ba02 == 1 ~ 1, # 有花费或自报参加
      ba02 == 2 ~ 0, # 没花费且自报不参加
      TRUE ~ NA_real_
    ),
    # 补习金钱投入
    w1_buxi_money = case_when(
      w1_buxi == 0 ~ 0, # 不参加者，金额为 0
      w1_buxi == 1 & is.na(ba03) ~ NA_real_, # 参加但没填金额，视为缺失
      TRUE ~ ba03 # 其他情况取原值
    ),
    w1_buxi_money_log = log1p(w1_buxi_money),
    w1_buxi_money_pos = case_when(
      is.na(w1_buxi_money) ~ NA_real_,
      w1_buxi_money > 0 ~ 1,
      TRUE ~ 0
    ),
    w1_buxi_money_pos_log = if_else(
      w1_buxi_money > 0,
      log(w1_buxi_money),
      NA_real_,
      missing = NA_real_
    ),
    # 每周补习时间（小时）
    across(c(b15b1, b15b2, b16b1, b16b2), ~ replace_na(., 0)),
    w1_buxi_time = (b15b1 + b15b2 / 60) * 5 + (b16b1 + b16b2 / 60) * 2,
    w1_buxi_time = case_when(
      w1_buxi == 0 ~ 0,
      w1_buxi == 1 & w1_buxi_time == 0 ~ NA_real_,
      TRUE ~ w1_buxi_time
    ),
    w1_buxi_time_log = log1p(w1_buxi_time),
    w1_buxi_time_pos = case_when(
      is.na(w1_buxi_time) ~ NA_real_,
      w1_buxi_time > 0 ~ 1,
      TRUE ~ 0
    ),
    w1_buxi_time_pos_log = if_else(
      w1_buxi_time > 0,
      log(w1_buxi_time),
      NA_real_,
      missing = NA_real_
    ),
  )

ceps |>
  select(
    w1_buxi,
    w1_buxi_money_log,
    w1_buxi_money_pos,
    w1_buxi_money_pos_log,
    w1_buxi_time_log,
    w1_buxi_time_pos,
    w1_buxi_time_pos_log
  ) |>
  Describe()

## %% 自变量 ------------------------------------------------

# 班级自变量代表了一期数据中“我的同学的家长们对他们孩子的平均期望/投入”。

# nolint
get_atomos <- function(x) {
  n_valid <- sum(!is.na(x)) # nolint: object_usage_linter
  sum_x <- sum(x, na.rm = TRUE) # nolint: object_usage_linter

  case_when(
    n_valid < 2 ~ NA_real_, # 班级有效人数不足2人，无法计算同伴效应
    is.na(x) ~ sum_x / n_valid, # 自身缺失，直接取班级其他人均值
    TRUE ~ (sum_x - x) / (n_valid - 1) # 正常情况：(总分 - 己分) / (人数 - 1)
  )
}

get_leave_one_top_mean <- function(x, top_prop) {
  vapply(
    seq_along(x),
    function(i) {
      others <- x[-i]
      others <- others[!is.na(others)]

      if (length(others) == 0) {
        return(NA_real_)
      }

      top_n <- ceiling(length(others) * top_prop)
      mean(sort(others, decreasing = TRUE)[seq_len(top_n)])
    },
    numeric(1)
  )
}

get_leave_one_sd <- function(x) {
  vapply(
    seq_along(x),
    function(i) {
      others <- x[-i]
      others <- others[!is.na(others)]

      if (length(others) < 2) {
        return(NA_real_)
      }

      sd(others)
    },
    numeric(1)
  )
}

# CV 在均值为 0 的班级上没有定义，因此仅保留为备选指标；
# 推进部分主分析使用更稳健的 leave-one-out 标准差（SD）。
get_leave_one_cv <- function(x) {
  vapply(
    seq_along(x),
    function(i) {
      others <- x[-i]
      others <- others[!is.na(others)]

      if (length(others) < 2) {
        return(NA_real_)
      }

      mean_others <- mean(others)
      if (is.na(mean_others) || dplyr::near(mean_others, 0)) {
        return(NA_real_)
      }

      # 对可能跨 0 的因子分数/标准分，用绝对均值保证 CV 表示离散程度大小。
      sd(others) / abs(mean_others)
    },
    numeric(1)
  )
}

# 班级家长期望氛围
qiwang_data <- ceps |>
  mutate(
    expe = case_when(
      ba18 == 1 ~ 7,
      ba18 == 2 ~ 9,
      ba18 %in% 3:5 ~ 12,
      ba18 == 6 ~ 15,
      ba18 == 7 ~ 16,
      ba18 == 8 ~ 19,
      ba18 == 9 ~ 23,
      TRUE ~ NA_real_
    ),
    grade = bc11,
  ) |>
  select(ids, expe, grade) |>
  drop_na()
qiwang_pca_result <- PCA(
  data = qiwang_data,
  vars = c("expe", "grade"),
  nfactors = 1,
  rotation = "none",
  rev = c("grade"),
  plot.scree = FALSE
)

ceps <- ceps |>
  left_join(
    tibble(
      ids = qiwang_data$ids,
      w1_qiwang = qiwang_pca_result$result$scores[, 1]
    ),
    by = "ids"
  ) |>
  mutate(
    # 班级家长期望氛围
    w1_qiwang_atomos = get_atomos(w1_qiwang),
    # 班级家长投入氛围：推进部分仅保留课外班参与、金钱投入、补习时间
    w1_buxi_rate_atomos = get_atomos(w1_buxi),
    w1_buxi_rate_atomos_top10 = get_leave_one_top_mean(w1_buxi, 0.10),
    w1_buxi_rate_atomos_top20 = get_leave_one_top_mean(w1_buxi, 0.20),
    w1_buxi_rate_atomos_sd = get_leave_one_sd(w1_buxi),
    w1_buxi_rate_atomos_cv = get_leave_one_cv(w1_buxi),
    w1_buxi_money_atomos = get_atomos(w1_buxi_money_log),
    w1_buxi_money_atomos_top10 = get_leave_one_top_mean(w1_buxi_money_log, 0.10),
    w1_buxi_money_atomos_top20 = get_leave_one_top_mean(w1_buxi_money_log, 0.20),
    w1_buxi_money_atomos_sd = get_leave_one_sd(w1_buxi_money_log),
    w1_buxi_money_atomos_cv = get_leave_one_cv(w1_buxi_money_log),
    w1_buxi_time_atomos = get_atomos(w1_buxi_time_log),
    w1_buxi_time_atomos_top10 = get_leave_one_top_mean(w1_buxi_time_log, 0.10),
    w1_buxi_time_atomos_top20 = get_leave_one_top_mean(w1_buxi_time_log, 0.20),
    w1_buxi_time_atomos_sd = get_leave_one_sd(w1_buxi_time_log),
    w1_buxi_time_atomos_cv = get_leave_one_cv(w1_buxi_time_log),

    .by = clsids
  ) |>
  mutate(
    # 对班级自变量进行标准化
    across(
      matches("_atomos($|_top10$|_top20$|_sd$|_cv$)"),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    )
  )

ceps |>
  select(matches("_atomos(_top10|_top20|_sd|_cv)?_z$")) |>
  Describe()

# 结构分解：将班级氛围拆成学校平均氛围与班级相对学校平均偏离。
# 这里使用班级层整体氛围（班均值）进行分解，便于后续学校层随机斜率和跨层交互解释。
class_climate_decomp <- ceps |>
  summarise(
    cls_mean_w1_buxi_rate_atomos = mean(w1_buxi, na.rm = TRUE),
    cls_mean_w1_buxi_money_atomos = mean(w1_buxi_money_log, na.rm = TRUE),
    cls_mean_w1_buxi_time_atomos = mean(w1_buxi_time_log, na.rm = TRUE),
    .by = c(schids, clsids)
  ) |>
  mutate(
    sch_mean_w1_buxi_rate_atomos = mean(cls_mean_w1_buxi_rate_atomos, na.rm = TRUE),
    sch_mean_w1_buxi_money_atomos = mean(cls_mean_w1_buxi_money_atomos, na.rm = TRUE),
    sch_mean_w1_buxi_time_atomos = mean(cls_mean_w1_buxi_time_atomos, na.rm = TRUE),
    .by = schids
  ) |>
  mutate(
    cls_dev_from_sch_w1_buxi_rate_atomos = cls_mean_w1_buxi_rate_atomos - sch_mean_w1_buxi_rate_atomos,
    cls_dev_from_sch_w1_buxi_money_atomos = cls_mean_w1_buxi_money_atomos -
      sch_mean_w1_buxi_money_atomos,
    cls_dev_from_sch_w1_buxi_time_atomos = cls_mean_w1_buxi_time_atomos - sch_mean_w1_buxi_time_atomos
  ) |>
  mutate(
    across(
      c(starts_with("sch_mean_"), starts_with("cls_dev_from_sch_")),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    )
  ) |>
  select(
    schids,
    clsids,
    starts_with("sch_mean_"),
    starts_with("cls_dev_from_sch_")
  )

ceps <- ceps |>
  left_join(class_climate_decomp, by = c("schids", "clsids"))

## %% 因变量 ------------------------------------------------

# 是否补习：w2ba21
# 补习费用：w2ba22
# 补习时间：周中：w2b07c 周末：w2b08c

ceps <- ceps |>
  mutate(
    # 是否参与补习（W2）
    w2_buxi = case_when(
      w2ba22 > 0 | w2ba21 == 1 ~ 1,
      w2ba21 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # 补习金钱投入（W2）：保留复刻用的 log1p，同时新增两部分模型变量
    w2_buxi_money_raw = case_when(
      w2_buxi == 0 ~ 0,
      w2_buxi == 1 & is.na(w2ba22) ~ NA_real_,
      TRUE ~ w2ba22
    ),
    w2_buxi_money_log = log1p(w2_buxi_money_raw),
    w2_buxi_money_pos = case_when(
      is.na(w2_buxi_money_raw) ~ NA_real_,
      w2_buxi_money_raw > 0 ~ 1,
      TRUE ~ 0
    ),
    w2_buxi_money_pos_log = if_else(
      w2_buxi_money_raw > 0,
      log(w2_buxi_money_raw),
      NA_real_,
      missing = NA_real_
    ),
    # 每周补习时间（W2，单位：小时）
    w2_buxi_weekday_hour = case_when(
      w2b07c == 1 ~ 0,
      w2b07c == 2 ~ 0.5,
      w2b07c == 3 ~ 1.5,
      w2b07c == 4 ~ 2.5,
      w2b07c == 5 ~ 3.5,
      w2b07c == 6 ~ 5,
      TRUE ~ NA_real_
    ),
    w2_buxi_weekend_hour = case_when(
      w2b08c == 1 ~ 0,
      w2b08c == 2 ~ 1,
      w2b08c == 3 ~ 3,
      w2b08c == 4 ~ 5,
      w2b08c == 5 ~ 7,
      w2b08c == 6 ~ 9,
      TRUE ~ NA_real_
    ),
    w2_buxi_time_raw = w2_buxi_weekday_hour * 5 + w2_buxi_weekend_hour * 2,
    w2_buxi_time_raw = case_when(
      w2_buxi == 0 ~ 0,
      w2_buxi == 1 & w2_buxi_time_raw == 0 ~ NA_real_,
      TRUE ~ w2_buxi_time_raw
    ),
    w2_buxi_time_log = log1p(w2_buxi_time_raw),
    w2_buxi_time_pos = case_when(
      is.na(w2_buxi_time_raw) ~ NA_real_,
      w2_buxi_time_raw > 0 ~ 1,
      TRUE ~ 0
    ),
    w2_buxi_time_pos_log = if_else(
      w2_buxi_time_raw > 0,
      log(w2_buxi_time_raw),
      NA_real_,
      missing = NA_real_
    )
  )

ceps |>
  select(
    w2_buxi,
    w2_buxi_money_raw,
    w2_buxi_money_log,
    w2_buxi_money_pos,
    w2_buxi_money_pos_log,
    w2_buxi_time_raw,
    w2_buxi_time_log,
    w2_buxi_time_pos,
    w2_buxi_time_pos_log
  ) |>
  Describe()


# %% 4 缺失值插补 ------------------------------------------------

# 众数辅助函数
calc_mode <- function(x) {
  ux <- unique(na.omit(x))
  if (length(ux) == 0) {
    return(NA)
  }
  ux[which.max(tabulate(match(x, ux)))]
}

# 学校层面变量的缺失值使用全部学校的均值插补
school_mean_imputes <- ceps |>
  summarise(
    sch_par_edu = mean(sch_par_edu, na.rm = TRUE),
    sch_par_income = mean(sch_par_income, na.rm = TRUE)
  )

ceps <- ceps |>
  mutate(
    sch_par_edu = if_else(is.na(sch_par_edu), school_mean_imputes$sch_par_edu, sch_par_edu),
    sch_par_income = if_else(is.na(sch_par_income), school_mean_imputes$sch_par_income, sch_par_income)
  ) |>
  mutate(
    # 连续/定序变量：使用班级均值
    SES = if_else(is.na(SES), mean(SES, na.rm = TRUE), SES),
    parage = if_else(is.na(parage), mean(parage, na.rm = TRUE), parage),
    wealth = if_else(is.na(wealth), mean(wealth, na.rm = TRUE), wealth),

    # 分类/虚拟变量：使用班级众数
    party = if_else(is.na(party), calc_mode(party), party),
    parhk = if_else(is.na(parhk), calc_mode(parhk), parhk),
    jisu = if_else(is.na(jisu), calc_mode(jisu), jisu),
    w1_buxi = if_else(is.na(w1_buxi), calc_mode(w1_buxi), w1_buxi),

    .by = clsids
  )

# 学校层连续变量 grand-mean centering：便于后续随机斜率和跨层交互解释
ceps <- ceps |>
  grand_mean_center(
    vars = c("sch_rank", "sch_par_edu", "sch_par_income", "sch_teacher_edu"),
    add.suffix = "_c"
  )

# %% 5 保存 ------------------------------------------------

# fmt: skip
final_vars <- c(
  # 标识变量
  "ids", "clsids", "schids",
  # 因变量（W2）
  "w2_buxi",
  "w2_buxi_money_raw", "w2_buxi_money_log", "w2_buxi_money_pos", "w2_buxi_money_pos_log",
  "w2_buxi_time_raw", "w2_buxi_time_log", "w2_buxi_time_pos", "w2_buxi_time_pos_log",
  # 核心自变量（班级氛围）
  "w1_qiwang_atomos_z",
  "w1_buxi_rate_atomos_z", "w1_buxi_money_atomos_z", "w1_buxi_time_atomos_z",
  # 班级氛围的极化与右尾特征（主分析使用 SD）
  "w1_buxi_rate_atomos_top10_z", "w1_buxi_rate_atomos_top20_z", "w1_buxi_rate_atomos_sd_z", "w1_buxi_rate_atomos_cv_z",
  "w1_buxi_money_atomos_top10_z", "w1_buxi_money_atomos_top20_z", "w1_buxi_money_atomos_sd_z", "w1_buxi_money_atomos_cv_z",
  "w1_buxi_time_atomos_top10_z", "w1_buxi_time_atomos_top20_z", "w1_buxi_time_atomos_sd_z", "w1_buxi_time_atomos_cv_z",
  # 班级氛围的结构分解变量
  "sch_mean_w1_buxi_rate_atomos", "sch_mean_w1_buxi_money_atomos", "sch_mean_w1_buxi_time_atomos",
  "cls_dev_from_sch_w1_buxi_rate_atomos", "cls_dev_from_sch_w1_buxi_money_atomos", "cls_dev_from_sch_w1_buxi_time_atomos",
  "sch_mean_w1_buxi_rate_atomos_z", "sch_mean_w1_buxi_money_atomos_z", "sch_mean_w1_buxi_time_atomos_z",
  "cls_dev_from_sch_w1_buxi_rate_atomos_z", "cls_dev_from_sch_w1_buxi_money_atomos_z", "cls_dev_from_sch_w1_buxi_time_atomos_z",
  # 滞后项（W1）
  "w1_qiwang", "w1_buxi",
  "w1_buxi_money_log", "w1_buxi_money_pos", "w1_buxi_money_pos_log",
  "w1_buxi_time_log", "w1_buxi_time_pos", "w1_buxi_time_pos_log",
  # 控制变量：子女层面
  "stsex", "onechild", "sibling", "ethgro", "cog3pl", "baseline_perf", "baseline_perf_z", "jisu", "juzhu",
  # 控制变量：家长层面
  "shenfen", "is_father", "parage", "parhk", "parjob_3c", "edu", "eduy",
  "wealth", "party", "SES", "know_other_par",
  # 控制变量：班级层面
  "clgrade", "smoke", "game",
  # 控制变量：学校层面
  "sch_rank", "sch_loc", "sch_par_edu", "sch_par_income", "sch_teacher_edu",
  "sch_rank_c", "sch_par_edu_c", "sch_par_income_c", "sch_teacher_edu_c",
  # 随机分班指示变量(稳健性检验)
  "is_random_class"
)

ceps |>
  # 计算随机分班指示变量，用于稳健性检验
  mutate(is_random_class = if_else(hra05 == 2 & ple1503 == 1, 1, 0)) |>
  select(all_of(final_vars)) |>
  export(here(output_dir, "CEPS_prepared.rds"))

cli::cli_alert_success("Data preparation completed.")
cli::cli_alert_info("Output: {.file {here(output_dir, 'CEPS_prepared.rds')}}")
