# 《社会统计学（中级）》课程复刻作业

[本仓库](https://github.com/Yousa-Mirage/social-statistics-replication)是 2026 年春季学期《社会统计学（中级）》课程作业，具体包含对肖志文 2024 年发表在《青年研究》期刊上的《中学生家长内卷：期望与投入氛围的影响》一文的复刻与推进代码。

> 肖志文.中学生家长内卷：期望与投入氛围的影响[J].青年研究,2024,(05):56-69+95-96.

## 复现流程

此作业的统计部分使用 R 完成，输出表格和作业报告使用 Typst 完成，确保可复现性。复现流程如下：

1. 环境准备：
  - [R 4.5.0+](https://www.r-project.org)
  - [Typst 0.14.0+](https://github.com/typst/typst)
  - [just](https://github.com/casey/just)
2. 下载并转码两期 [CEPS 数据](http://ceps.ruc.edu.cn/)，将其分别放在 `data/CEPSw1/` 和 `data/CEPSw2/` 目录下；
3. 在项目根路径下运行 `just env`，恢复 R 环境依赖；
4. 运行 `just replicate`，执行复刻部分代码并输出结果；
5. 运行 `just advance`，执行推进部分代码并输出结果；
6. （如有 `typ` 作业源文件）运行 `just typst`，编译生成 Typst 报告。

## 项目结构

```plaintext
.
├── data/
│   ├── CEPSw1/               # CEPS 第一轮原始数据目录
│   ├── CEPSw2/               # CEPS 第二轮原始数据目录
│   ├── rep_output/           # 复刻部分输出的中间数据、表格与图形
│   └── adv_output/           # 推进部分输出的中间数据、表格与图形
├── papers/                   # 课程作业 Typst 源文件
│   └── lib.typ               # Typst 报告模板
├── scripts/
│   ├── 01_rep_data_prepare.R             # 复刻：数据清理、合并与变量构造
│   ├── 02_rep_descriptive_analyze.R      # 复刻：描述统计表
│   ├── 03_rep_regression_model.R         # 复刻：基准回归与结果表
│   ├── 04_rep_robust_hetero.R            # 复刻：稳健性与异质性分析
│   ├── 11_adv_data_prepare.R             # 推进：新增变量与分析样本构造
│   ├── 12_adv_descriptive_analyze.R      # 推进：描述统计分析
│   ├── 13_adv_regression_model.R         # 推进：两部分模型、右尾压力与离散指标分析
│   └── 14_adv_structure_decomp_model.R   # 推进：学校均值/班级偏离的结构分解模型
├── renv/                    # renv 虚拟环境
├── .Rprofile                # 自动激活 renv
├── air.toml                 # air 格式化配置
├── justfile                 # just 任务入口
├── renv.lock                # 项目依赖锁文件
└── README.md
```
