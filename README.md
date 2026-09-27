# Well-being, GDP per capita and world regions

Research project by Adrien Fabre (CNRS, CIRED) on two questions:

1. **Region vs. income.** Is national subjective well-being (SWB) better predicted by GDP per capita or by the country's world region?
2. **Gallup vs. WVS discrepancy.** Life evaluations are more strongly correlated with GDP in the Gallup World Poll (Cantril ladder, 0–10) than in the World Values Survey (life satisfaction, 1–10). Is this due to *question wording/scale* or to *different samples* (sampling frame, mode, timing)? An original survey (Fabre 2025) randomizes the wording and scale within the same online samples to find out.

Target outlets: Journal of Public Economics > Journal of Economic Psychology > Journal of Happiness Studies (see `TODO.md` and the recommendation in `papers/`).

## Repository layout

| Path | Content |
|---|---|
| `code_wellbeing/main.R` | **Single reproducible pipeline** (data preparation, all analyses, all tables and figures of the papers). Run from `code_wellbeing/`. |
| `code_wellbeing/old_data.R` | Original analysis (WVS 1981–2022, Gallup, first look at Fabre 2025). Kept unchanged for reference. |
| `code_wellbeing/wave6.R` | Earlier exploratory code on WVS wave 6 (unchanged, not used). |
| `code_wellbeing/.Rprofile` | Helper functions used by `old_data.R` (`decrit`, `barres`, `no.na`, …). `main.R` does not depend on it. |
| `data/WVS.rds` | WVS time-series (waves 1–7, 1981–2022), individual level. |
| `data/gallup.xlsx` | Gallup World Poll: distribution of the Cantril ladder by country × wave (waves 1–18, i.e. 2005/06–2023), exported from Gallup Analytics. **Wave *w* corresponds to year *w* + 2005** (see `TODO.md`). |
| `data/Fabre2025.csv` | Original survey (Fabre 2025), 11,000 respondents in 10 high-income countries (US, JP, DE, SA, GB, FR, IT, ES, PL, CH), fielded online Apr.–Jul. 2025 (Bilendi; Kantar in Saudi Arabia), quota-representative on gender, age, income, education, region, urbanicity. Four randomized branches for the life-evaluation question: `gallup_0` (ladder, 0–10), `gallup_1` (ladder, 1–10), `wvs_0` (satisfaction, 0–10), `wvs_1` (satisfaction, 1–10). IRB-CIRED-2025-2; pre-registration osf.io/7mzn4. Questionnaire and survey details: github.com/bixiou/robustness_global_redistr. |
| `data/GDPpcPPP17.csv`, `data/GDPpcPPP21.csv`, `data/GDPpc15.csv` | World Bank WDI GDP per capita (PPP constant 2017 $ with manual IMF imputations; PPP constant 2021 $; nominal constant 2015 $). |
| `data/pop.xlsx` | UN World Population Prospects 2022. |
| `data/country_code_mapping.csv` | ISO2/ISO3/country names. |
| `data/WHR26_Data_Figure_2.1.xlsx`, `data/WHR25_Data_Figure_2.1v3.xlsx` | World Happiness Report (Gallup) country-level ladder means, 3-year averages, 2011–2025 (downloaded 2026-09-27 from worldhappiness.report/data-sharing). |
| `data/*_wdi_*.csv` (created by `main.R`) | Cached World Bank API downloads (GDP, population) used by `main.R`. |
| `data/deprecated/` | Raw versions of older data files. |
| `presentations/wellbeing_prez.tex` | Beamer presentation of the region vs. income results (Jan. 2024). |
| `papers/` | Papers: `wellbeing.tex` (combined, Journal of Economic Psychology format), `wellbeing_region.tex` (income vs. region), `wellbeing_discrepancy.tex` (Gallup vs. WVS: wording or samples); shared sections in `papers/sections/`, common preamble `preamble_paper.tex`, bibliography `wellbeing.bib`. Compile in `papers/build/`. `papers/Adrien_paper/` holds the original (French) draft and Stata code. |
| `figures/`, `tables/` | Outputs of `old_data.R` (used by the presentation). |
| `figures/main/`, `tables/main/` | Outputs of `main.R` (used by the papers). |
| `region6/`, `backup_figures_tables/` | Outputs of earlier versions (6-region classification; backups). |

## Reproducing the results

Requirements: R ≥ 4.1 and the packages listed at the top of `code_wellbeing/main.R` (dplyr, tidyr, readxl, openxlsx, ggplot2, ggrepel, relaimpo, sandwich, lmtest, fixest, kableExtra, jsonlite).

```sh
cd code_wellbeing
Rscript --no-init-file main.R   # ~15 min (1,000 bootstrap replications; N_BOOTSTRAP=50 for a quick run); writes to ../tables/main and ../figures/main
cd ../papers
latexmk -pdf -outdir=build wellbeing.tex   # also wellbeing_region.tex, wellbeing_discrepancy.tex
```

`--no-init-file` skips `code_wellbeing/.Rprofile`, which is only needed by `old_data.R` (it installs extra packages at start-up). Numbers quoted in the papers are LaTeX macros written by `main.R` to `tables/main/numbers.tex`, so the papers update automatically when the analysis changes.

`main.R` downloads World Bank data once and caches it in `data/`; set `refresh_downloads <- TRUE` at the top to re-download. Random elements (k-means clustering, bootstrap) use a fixed seed.

## Main definitions

National well-being indicators (all computed with survey weights, excluding non-responses): *Happy* (share quite/very happy), *Very Happy*, *Very Unhappy* (not at all happy), *V. Happy – V. Unhappy*, *Happiness (mean)* (coded −3/−1/1/3), *Satisfaction (mean)*, *Satisfied* (share ≥ 6), *Happy + Satisfied* (Inglehart & Klingemann 2000). Income: log GDP p.c. (PPP or nominal), income sextiles, k-means income clusters (k = 5, 6, 7). Regions: Africa, Asia (incl. Middle East and Central Asia), Eastern Europe, Latin America, Western (incl. Turkey), following UN regional groups.

## License

See `LICENSE` (GNU AGPL-3.0).
