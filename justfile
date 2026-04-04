set windows-shell := ["powershell", "-c"]
set dotenv-load := true

MY_NAME := env("MY_NAME", "")

# 列出所有可用的命令
default:
    just --list

# 执行复现部分的全流程脚本
replicate:
    Rscript ./scripts/01_rep_data_prepare.R
    Rscript ./scripts/02_rep_descriptive_analyze.R
    Rscript ./scripts/03_rep_regression_model.R
    Rscript ./scripts/04_rep_robust_hetero.R

# 执行推进部分的全流程脚本
advance:
    Rscript ./scripts/11_adv_data_prepare.R

# 编译 Typst 文档
typst:
    typst compile ./papers/1-选题说明.typ {{ "./papers/" + MY_NAME + "_选题说明.pdf" }} --root .
    typst compile ./papers/2-论文初稿.typ {{ "./papers/" + MY_NAME + "_论文初稿.pdf" }} --root .

# 运行 Air 格式化
format:
    @air format .
    @jarl check .

alias rep := replicate
alias adv := advance
alias fmt := format
