# Risk Information Measure

[![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![WRDS](https://img.shields.io/badge/Data-WRDS-003366)](https://wrds-www.wharton.upenn.edu/)

An R implementation of the risk information measure from Smith & So (2022, JAR), adapted for **Analyst/Investor Day** events instead of earnings announcements.

## Overview

Replication of Smith & So (2022, JAR) applied to **Analyst/Investor Day** events (2010–2025). The risk information measure decomposes the change in option-implied variance around an event into a level shift and a short-horizon slope. Data are pulled from WRDS (Compustat, Capital IQ, CRSP CIZ v2, OptionMetrics, Fama-French) and the output is written to `data/smithsoJAR.csv`.

## Project Structure

```
riskinfo/
├── data/
│   └── smithsoJAR.csv         # panel output: event-firm observations
├── R/
│   ├── wd-diff.R              # weekday distance helper (SAS intck equivalent)
│   ├── vol-to.R               # vol_to(), vol_to_liq()
│   └── run-ff.R               # run_ff() — Fama-French 4-factor OLS
├── refs/
│   └── Smith & So (2022, JAR)
├── load.R                     # entry point: packages, connection, config
└── riskinfo.R                 # main pipeline (sources R/, runs all steps)
```

## Usage

Set your WRDS credentials as environment variables (e.g. in `.Renviron`):

```
WRDS_USER=your_username
WRDS_PASSWORD=your_password
```

Then open `load.R`, adjust the configuration if needed, and run it:

```r
source("load.R")
```

## Configuration (`load.R`)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `beg_date` | `2010-01-01` | Sample start date |
| `end_date` | `2022-12-31` | Sample end date |
| `td_pre` | `-2L` | Pre-event trading day for IV measurement (Smith & So: −2) |
| `td_post` | `+1L` | Post-event trading day for IV measurement (Smith & So: +1) |

Changing `td_pre` and `td_post` allows alternative window specifications
without touching the pipeline.

## Pipeline

| Step | Description |
|------|-------------|
| 1–3 | Build event panel from Capital IQ (`ciq.wrds_keydev`), filter to US public companies, match to most recently completed fiscal quarter from Compustat (`comp.fundq`) via `fqe <= actdate` |
| 4 | Link Compustat → CRSP via `crsp.ccmxpf_lnkhist`; add S&P 500 membership flag and NYSE size decile |
| 5 | Pull call options from OptionMetrics (`optionm.stdopd{year}`) at maturities 30/60/122/182/365 days; compute IV term structure at `td_pre` and `td_post`; add VIX changes |
| 6 | Compute returns, turnover, Amihud illiquidity, and bid-ask spread over multiple windows around the event date |
| 7 | Estimate Fama-French 4-factor betas and idiosyncratic volatility in pre/post windows (±26 and ±127 trading days) |
| 9 | Merge all components; construct risk information measures |

## Risk Information Measure

$$\textit{RiskInfo}_{30} = 30 \cdot [IV_{t_{\text{post}},30} - IV_{t_{\text{pre}},30}] + \frac{IV_{t_{\text{pre}},30} - IV_{t_{\text{pre}},60}}{\frac{1}{30} - \frac{1}{60}}$$

where $IV$ denotes ATM implied variance (annualized volatility² / 252) from
OptionMetrics standardized options. Smith & So (2021) set $t_{\text{pre}} = -2$
and $t_{\text{post}} = +1$ relative to the event date.

## Key Output Variables

| Variable | Definition |
|----------|------------|
| `ri30`, `ri60`, `ri182`, `ri365` | Risk information by maturity |
| `sri30`, `sri182` | Scaled risk information (÷ baseline diffusion variance) |
| `ppvma`, `dvma` | Changes in market-adjusted return volatility |
| `divol`, `dsiv` | Log-changes in idiosyncratic volatility |
| `delta_mktbeta_sq` | Change in squared market beta |
| `eanret`, `eanto` | Event-window abnormal return and turnover |

## Data Sources (WRDS)

| Library | Content |
|---------|---------|
| `comp.fundq` | Compustat quarterly fundamentals |
| `ciq.wrds_keydev`, `ciq.ciq*` | Capital IQ Analyst/Investor Day events |
| `crsp.ccmxpf_lnkhist`, `crsp.dsf_v2`, `crsp.dsi`, `crsp.msp500list`, `crsp.mport1` | CRSP daily stock (CIZ v2) and index data |
| `optionm.stdopd{year}`, `optionm.securd` | OptionMetrics standardized options |
| `ff.factors_daily` | Fama-French daily factors |
| `cboe.cboe` | CBOE VIX daily series |

## Output

`data/smithsoJAR.csv` — panel of event-firm observations with identifiers and risk-information measures.

## References

- Smith, K. C. and So, E. C. (2021). Measuring Risk Information. *Journal of Accounting Research*, 59(5), 1729–1797.
