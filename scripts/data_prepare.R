# scripts/data_prepare.R
# 目标：完成数据合并、清洗与变量准备。
# 输入：data/CEPSw1/*.dta, data/CEPSw2/*.dta
# 输出：data/CEPS_prepared.rds

suppressPackageStartupMessages({
  library(dplyr)
  library(bruceR)
  library(here)
})

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
    # 寄宿：寄宿为 1，走读为 0 (WARN: 可进行插补)
    jisu = case_when(c18 == 1 ~ 1, c18 == 2 ~ 0, TRUE ~ NA_real_),
    # 父母居住情况
    juzhu = factor(
      stlb_4c,
      levels = 1:4,
      labels = c("父母都在家", "只有目前在家", "只有父亲在家", "父母都不在家")
    ),
  )

ceps |>
  select(stsex, onechild, sibling, ethgro, cog3pl, jisu) |>
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

    # 经济状况（WARN：可选 w2be23 填补）
    wealth = be19,

    # 党员身份：共产党员为 1，其余为 0
    party = case_when(
      be06 == 1 ~ 1,
      be06 %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    # 是否认识班内其他家长（因子变量）
    know_other_par = factor(
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
      levels = 1:5,
      labels = c(
        "市/县城的中心城区",
        "市/县城的边缘城区",
        "市/县城的城乡结合部",
        "市/县城区外的镇",
        "农村"
      )
    ),
    # 学校数据中已给出“本校家长平均教育程度/职业收入”
    sch_par_edu = plb08,
    sch_par_income = plb09,
    # 学校教师平均受教育年限
    across(c(plc0401a, plc0402a, plc0403a, plc0404a, plc0405a), ~ as.numeric(.x)),
    total_teachers = plc0401a + plc0402a + plc0403a + plc0404a + plc0405a,
    sch_teacher_edu = if_else(
      total_teachers > 0,
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

# 只有家校联系频率需要标准化，两个因子已经是标准化得分了，金钱和时间进行了对数化，是否参与补习是二元变量不需要处理。

# # 教养方式严格程度
jiaoyang_data <- ceps |>
  select(ids, starts_with("ba080")) |>
  mutate(across(
    starts_with("ba080"),
    as.numeric
  )) |>
  drop_na()
jiaoyang_pca_result <- PCA(
  data = jiaoyang_data,
  var = "ba080",
  items = 1:8,
  nfactors = 1,
  rotation = "none",
  plot.scree = FALSE
)

# 家长每周监督辅导
jiandu_data <- ceps |>
  select(ids, b2201, b2202) |>
  mutate(across(
    c(b2201, b2202),
    as.numeric
  )) |>
  drop_na()
jiandu_pca_result <- PCA(
  data = jiandu_data,
  vars = c("b2201", "b2202"),
  nfactors = 1,
  rotation = "none",
  plot.scree = FALSE
)

ceps <- ceps |>
  left_join(
    # 家长每周监督辅导
    tibble(
      ids = jiandu_data$ids,
      w1_jiandu = jiandu_pca_result$result$scores[, 1]
    ),
    by = "ids"
  ) |>
  left_join(
    # 教养方式严格程度
    tibble(
      ids = jiaoyang_data$ids,
      w1_jiaoyang = jiaoyang_pca_result$result$scores[, 1]
    ),
    by = "ids"
  ) |>
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
    # 每周补习时间（小时）
    across(c(b15b1, b15b2, b16b1, b16b2), ~ replace_na(., 0)),
    w1_buxi_time = (b15b1 + b15b2 / 60) * 5 + (b16b1 + b16b2 / 60) * 2,
    w1_buxi_time = case_when(
      w1_buxi == 0 ~ 0,
      w1_buxi == 1 & w1_buxi_time == 0 ~ NA_real_,
      TRUE ~ w1_buxi_time
    ),
    w1_buxi_time_log = log1p(w1_buxi_time),
    # 家校联系频率
    w1_contact = as.numeric(bb02),
    w1_contact_z = as.numeric(scale(w1_contact))
  )

ceps |>
  select(w1_jiandu, w1_jiaoyang, w1_buxi, w1_buxi_money_log, w1_buxi_time_log, w1_contact_z) |>
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
    # 班级家长投入氛围
    w1_jiandu_atomos = get_atomos(w1_jiandu),
    w1_jiaoyang_atomos = get_atomos(w1_jiaoyang),
    w1_buxi_rate_atomos = get_atomos(w1_buxi),
    w1_buxi_money_atomos = get_atomos(w1_buxi_money_log),
    w1_buxi_time_atomos = get_atomos(w1_buxi_time_log),
    w1_contact_atomos = get_atomos(w1_contact),

    .by = clsids
  ) |>
  mutate(
    # 对班级自变量进行标准化
    across(
      ends_with("_atomos"),
      ~ as.numeric(scale(.x)),
      .names = "{.col}_z"
    )
  )

# 计算班级投入氛围因子分数
touru_data <- ceps |>
  select(ids, ends_with("_atomos_z"), -w1_qiwang_atomos_z) |>
  drop_na()
touru_pca <- PCA(
  data = touru_data,
  vars = c(
    "w1_jiandu_atomos_z",
    "w1_jiaoyang_atomos_z",
    "w1_buxi_rate_atomos_z",
    "w1_buxi_money_atomos_z",
    "w1_buxi_time_atomos_z",
    "w1_contact_atomos_z"
  ),
  nfactors = 1,
  rotation = "none",
  plot.scree = FALSE
)
ceps <- ceps |>
  left_join(
    tibble(
      ids = touru_data$ids,
      w1_touru_atomos_z = touru_pca$result$scores[, 1]
    ),
    by = "ids"
  )

ceps |>
  select(ends_with("_atomos_z")) |>
  Describe()

## %% 因变量 ------------------------------------------------

# 监督频率：w2a18, w2a19
# 教养方式：w2ba170,1:6
# 是否补习：w2ba21
# 补习费用：w2ba22
# 补习时间：周中：w2b07c 周末：w2b08c
# 主动联络：w2bb02

# 教养方式严格程度（W2）
w2_jiaoyang_data <- ceps |>
  select(ids, starts_with("w2ba170")) |>
  drop_na()
w2_jiaoyang_pca_result <- PCA(
  data = w2_jiaoyang_data,
  var = "w2ba170",
  items = 1:6,
  nfactors = 1,
  rotation = "none",
  plot.scree = FALSE
)

# 家长每周监督辅导（W2）
w2_jiandu_data <- ceps |>
  select(ids, w2a18, w2a19) |>
  drop_na()
w2_jiandu_pca_result <- PCA(
  data = w2_jiandu_data,
  vars = c("w2a18", "w2a19"),
  nfactors = 1,
  rotation = "none",
  plot.scree = FALSE
)

ceps <- ceps |>
  left_join(
    tibble(
      ids = w2_jiandu_data$ids,
      w2_jiandu = w2_jiandu_pca_result$result$scores[, 1]
    ),
    by = "ids"
  ) |>
  left_join(
    tibble(
      ids = w2_jiaoyang_data$ids,
      w2_jiaoyang = w2_jiaoyang_pca_result$result$scores[, 1]
    ),
    by = "ids"
  ) |>
  mutate(
    # 是否参与补习（W2）
    w2_buxi = case_when(
      w2ba22 > 0 | w2ba21 == 1 ~ 1,
      w2ba21 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    # 补习金钱投入（W2）
    w2_buxi_money = case_when(
      w2_buxi == 0 ~ 0,
      w2_buxi == 1 & is.na(w2ba22) ~ NA_real_,
      TRUE ~ w2ba22
    ),
    w2_buxi_money_log = log1p(w2_buxi_money),
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
    w2_buxi_time = w2_buxi_weekday_hour * 5 + w2_buxi_weekend_hour * 2,
    w2_buxi_time = case_when(
      w2_buxi == 0 ~ 0,
      w2_buxi == 1 & w2_buxi_time == 0 ~ NA_real_,
      TRUE ~ w2_buxi_time
    ),
    w2_buxi_time_log = log1p(w2_buxi_time),
    # 主动联络老师频率（W2）
    w2_contact = w2bb02,
    w2_contact_z = as.numeric(scale(w2_contact))
  )

ceps |>
  select(w2_jiandu, w2_jiaoyang, w2_buxi, w2_buxi_money_log, w2_buxi_time_log, w2_contact_z) |>
  Describe()

# %% 4 保存 ------------------------------------------------

# fmt: skip
final_vars <- c(
  # 标识变量
  "ids", "clsids", "schids",
  # 因变量（W2）
  "w2_jiandu", "w2_jiaoyang", "w2_buxi",
  "w2_buxi_money_log", "w2_buxi_time_log", "w2_contact_z",
  # 核心自变量（班级氛围）
  "w1_qiwang_atomos_z", "w1_touru_atomos_z",
  "w1_jiandu_atomos_z", "w1_jiaoyang_atomos_z", "w1_buxi_rate_atomos_z",
  "w1_buxi_money_atomos_z", "w1_buxi_time_atomos_z", "w1_contact_atomos_z",
  # 滞后项（W1）
  "w1_qiwang", "w1_jiandu", "w1_jiaoyang", "w1_buxi",
  "w1_buxi_money_log", "w1_buxi_time_log", "w1_contact_z",
  # 控制变量：子女层面
  "stsex", "onechild", "sibling", "ethgro", "cog3pl", "jisu", "juzhu",
  # 控制变量：家长层面
  "shenfen", "is_father", "parage", "parhk", "parjob_3c", "edu", "eduy",
  "wealth", "party", "SES", "know_other_par",
  # 控制变量：班级层面
  "clgrade", "smoke", "game",
  # 控制变量：学校层面
  "sch_rank", "sch_loc", "sch_par_edu", "sch_par_income", "sch_teacher_edu"
)

ceps |>
  select(all_of(final_vars)) |>
  export(here("data/CEPS_prepared.rds"))

cli::cli_alert_success("Data preparation completed.")
cli::cli_alert_info("Output: {.file {here('data/CEPS_prepared.rds')}}")
