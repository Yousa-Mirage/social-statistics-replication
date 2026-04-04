# AGENTS.md
## Purpose
This file is for coding agents working in this repository.
Prefer the repository-specific guidance here over generic defaults.

## Project Overview
- Language: R
- Environment manager: `renv`
- Task runner: `just`
- Formatter/checker: `air` and `jarl`
- Main outputs: `.rds` data objects and `.typ` Typst tables
- Architecture: standalone analysis scripts, not an R package

## Other Instruction Files
- No `.cursorrules`, `.cursor/rules/`, or `.github/copilot-instructions.md` files are present.
- If any of those files are added later, treat them as additional constraints.

## Repository Map
- `scripts/01_rep_data_prepare.R`: replication data preparation
- `scripts/02_rep_descriptive_analyze.R`: descriptive table generation
- `scripts/03_rep_regression_model.R`: regression models and table output
- `scripts/04_rep_robust_hetero.R`: robustness and heterogeneity analysis
- `scripts/11_adv_data_prepare.R`: advanced-analysis data preparation stub
- `data/CEPSw1/`, `data/CEPSw2/`: raw data
- `data/rep_output/`, `data/adv_output/`: generated artifacts
- `papers/`: Typst source files
- `justfile`, `air.toml`, `.Rprofile`, `renv.lock`: workflow/config files

## Environment Setup
Work from the repo root.
The project auto-activates `renv` through `.Rprofile`, but explicit restore/status commands are still useful.

```bash
Rscript -e "renv::restore(prompt = FALSE)"
Rscript -e "renv::status()"
just --list
```

Notes:
- `.Rprofile` sources `renv/activate.R`.
- `renv.lock` targets R 4.5.x.
- In this workspace, `renv::status()` reports some base/recommended packages out of sync.
- Do not edit `renv.lock` or `renv/` unless the task is explicitly about dependencies.

## Build, Lint, and Run
There is no package build step and there is no checked-in automated unit-test suite.
Use `just` for full workflows and direct `Rscript` calls for narrow validation.
- Full replication: `just rep`
- Advanced branch: `just adv`
- Typst compile: `just typst`
- Format/check: `just format` or `just fmt`

`just rep` runs:

```bash
Rscript .\scripts\01_rep_data_prepare.R
Rscript .\scripts\02_rep_descriptive_analyze.R
Rscript .\scripts\03_rep_regression_model.R
Rscript .\scripts\04_rep_robust_hetero.R
```

`just format` runs:

```bash
air format .
jarl check .
```

`air.toml` sets `line-width = 105`.

## Single-Test Equivalent
Because this is a script-first analysis repo, the practical “single test” is rerunning the smallest affected script.

```bash
Rscript .\scripts\01_rep_data_prepare.R
Rscript .\scripts\02_rep_descriptive_analyze.R
Rscript .\scripts\03_rep_regression_model.R
Rscript .\scripts\04_rep_robust_hetero.R
Rscript .\scripts\11_adv_data_prepare.R
```

Use this mapping:
- Data import, cleaning, or schema changes: `01_rep_data_prepare.R`
- Descriptive table changes: `02_rep_descriptive_analyze.R`
- Model, formula, or coefficient-table changes: `03_rep_regression_model.R`
- Robustness or heterogeneity changes: `04_rep_robust_hetero.R`
- Advanced-analysis work: `11_adv_data_prepare.R`
- If you change `CEPS_prepared.rds` fields, rerun every downstream script that consumes them.

## Validation Expectations
- Prefer the narrowest rerun that actually exercises the change.
- Treat `data/rep_output/` and `data/adv_output/` as build artifacts unless the user asks to inspect diffs.
- If validation is blocked by environment or data issues, state the exact blocker.

## Code Style
### General Structure
- Keep the existing standalone-script architecture; do not invent package scaffolding unless asked.
- Preserve the header convention at the top of each script:
  `# scripts/<name>.R`
  `# 目标：...`
  `# 输入：...`
  `# 输出：...`
- Keep helpers small and close to first use.
- Keep transformations in the script that owns the output.
- Match the repo's current mix of English identifiers and Chinese explanatory comments.

### Imports and Paths
- Load packages at the top inside `suppressPackageStartupMessages({ ... })`.
- Follow the existing package set unless a task clearly needs a new dependency.
- Common attached packages are `tidyverse`, `bruceR`, `here`, `tinytable`, and `modelsummary`.
- Use `pkg::fun()` for selectively used functions or when clarity matters.
- Existing scripts rely on namespaced calls like `haven::zap_missing()`, `lme4::VarCorr()`, `reformulas::nobars()`, and `cli::cli_alert_*()`.
- Build paths with `here()`; do not hardcode absolute paths.
- Create output directories with `dir.create(..., recursive = TRUE, showWarnings = FALSE)` when needed.

### Formatting and Naming
- Use the base pipe `|>`, not `%>%`.
- Keep lines within 105 characters unless a longer line is clearly better.
- Use trailing commas in multi-line calls when they improve readability and diffs.
- Use `snake_case` for objects and helper functions.
- Reuse established suffixes when they fit: `_raw`, `_coded`, `_data`, `_result`, `_list`, `_z`, `_log`, `_3c`.
- Use `is_` prefixes for binary indicators, for example `is_father` and `is_random_class`.
- Use `# fmt: skip` only for visually aligned formula/vector blocks.
- Use `# nolint` sparingly and target specific false positives when possible.

### Types, Missing Values, and Factors
- Be explicit about result types inside `case_when()` and `if_else()`.
- Use typed missing values such as `NA_real_` for numeric outputs.
- Normalize imported Stata-labelled values before heavy manipulation.
- The current import pattern uses `haven::zap_labels()` and `haven::zap_missing()`.
- Prefer `coalesce()` for fallback logic.
- Use `drop_na()` only when complete cases are actually required.
- When creating factors, set both `levels` and `labels` explicitly.
- Preserve the intended reference category via factor order.

### Data Manipulation
- Prefer readable `mutate()`, `summarise()`, `across()`, `pick()`, `case_when()`, and `if_else()` pipelines.
- Prefer `.by = ...` for local grouped operations instead of a longer `group_by()` / `ungroup()` cycle.
- Always use explicit join keys.
- Use `all_of()` or `any_of()` when selecting from maintained variable lists.
- Comment statistical intent, paper-vs-data discrepancies, or imputation logic; do not comment obvious mechanics.

### Modeling and Tables
- Build complex formulas as strings with `str_glue()` or `str_c()`, then convert with `as.formula()`.
- Keep shared formula fragments in named objects like `ctrl_formula`.
- Keep coefficient maps explicit and readable, even if repetitive.
- Prefer deterministic table styling and file output.
- Existing table code writes with `save_tt(..., overwrite = TRUE)`.

### Error Handling and Logging
- Fail fast on missing files, missing columns, or impossible values.
- Do not silently swallow data problems.
- For new validations, keep checks close to the input boundary and make failures actionable.
- Use `cli::cli_alert_info()` for stage markers and `cli::cli_alert_success()` for successful completion of long scripts.

## Agent Working Rules
- Prefer minimal edits that preserve the current analysis flow.
- Do not rename established variables unless the task requires it.
- Do not hide core script logic behind new abstractions unless reuse clearly pays off.
- Do not introduce package-style tests or scaffolding unless the user explicitly asks for them.
- When changing upstream variables or schema, check all downstream scripts that import `CEPS_prepared.rds`.
- Optimize for reproducibility and readable analytical intent.
