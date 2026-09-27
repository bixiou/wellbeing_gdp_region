# TODO

Legend: `[x]` done, `[ ]` to do, `[~]` partly done / see note. **(A)** = needs Adrien (data access, decision, or information only he has).

## 0. Project goals (from `CLAUDE.md`)

- [x] 0. Understand the repository, create `README.md` and `TODO.md` (with these instructions and the TODOs of `wellbeing_prez.tex` and `old_data.R`).
- [ ] 1. Use Fabre (2025) to find the reason for the Gallup/WVS discrepancy: regress well-being on wording × scale, estimate the effect of wording, compare the predicted indicators of past data (for the alternative wording/scale) with the empirical evidence; decompose the discrepancy into wording and residual (≈ different samples). If wording explains more, the multi-indicator/multi-dataset strategy is validated; otherwise emphasize the dataset with the best sampling.
- [ ] 2. Update the analysis of `old_data.R`: (a) update data series (GDP p.c.…); (b) integrate more recent waves of Gallup or WVS if they exist; (c) extend the analysis to Fabre (2025).
- [ ] 3. Do the other TODOs (below).
- [ ] 4. Find the weaknesses of the analysis and propose improvements in methodology/analysis.
- [ ] 5. Write `papers/wellbeing.tex` following the Journal of Economic Psychology's requirements, the structure and interpretation of `wellbeing_prez.tex`, plus steps 1–4.
- [ ] 6. Write `papers/wellbeing_region.tex` (region vs. GDP only) and `papers/wellbeing_discrepancy.tex` (wording vs. sampling only), ≤ 9k words each excluding appendices; recommend combined vs. split and the best journal for each.

## 1. Bugs and issues found in `old_data.R` (to fix in `main.R`, not in `old_data.R`)

- [ ] **Gallup wave → year mapping is off by 5 years.** `g$year <- floor(g$wave) + 2000` should be `floor(g$wave) + 2005` (Gallup World Poll wave 1 = 2005/06). Evidence: Lebanon's ladder collapses in waves 15–17 (2.83, 2.25, 2.47) and the WHR 3-year averages match only with wave 14 = 2019 (e.g. WHR 2019–21 average 2.955 vs. (4.01 + 2.83 + 2.25)/3 = 3.03); Afghanistan's record low 1.25 is wave 17 = 2022. Consequence: Gallup observations were matched with GDP 5 years too early, and the Gallup/WVS/Fabre comparison paired the wrong years (e.g. "GBR: 2018, JPN: 2017").
- [ ] **`very_happy_minus_very_unhappy` is a weighted *count*, not a share difference** (`sum(w * (h==1)) - sum(w * (h==4))`), so it scales with the sample size of each country-year. Should be `weighted.mean(h==1) - weighted.mean(h==4)`.
- [ ] **k-means clusters are not reproducible**: the k-means block in `create_gdp_vars` is commented out (so `run_regressions` fails as is), and there is no `set.seed()`; k-means with random starts can give different clusters across runs. Use `set.seed()` and `nstart = 50` (or 1-D exact clustering, e.g. `Ckmeans.1d.dp`).
- [ ] `plot_all()` is called (l. 457) before being defined (l. 459).
- [ ] Code depends on `.Rprofile` helpers and on `%>%`; `main.R` is self-contained and uses `|>`.
- [ ] Presentation typo: "Region is a better predictor than region" → "than income".

## 2. TODOs from `presentations/wellbeing_prez.tex`

- [ ] Cite Galbraith et al. (2024), Ritter et al. (2025), Prica & Bartlett (2026); check Blanchflower & Bryson (2023). **(A)** full references needed — I will not guess them.
- [ ] Plot *Happy* against *Satisfied*.
- [ ] Fix NA in tables `gdp.tex` and `share_gdp.tex`.
- [ ] Results: some indicators are not significantly related to GDP p.c., some (e.g. share *Very happy*) even decrease with it → report slopes and significance.
- [ ] Results: table of happiest countries (by indicator × wave).
- [ ] Robustness checks: only last observation per country, population weights, excluding pandemic years, without imputed GDP (already in old code) — keep in the paper.
- [ ] Robustness: number of missing answers (DK/refusals) by country.
- [ ] Other explanatory variables: growth, median income.
- [ ] Future research: check whether emotions (affect) are better predicted by region than by income (Gallup positive/negative affect; WHR data).
- Journals aimed (from prez): JPubE (9k words, 165 $) > Journal of Economic Psychology (12k, 0 $) > Journal of Happiness Studies (10k, 0 $) > Journal of Wellbeing Economics (10k, 0 $).

## 3. TODOs from `code_wellbeing/old_data.R`

- [ ] Check documentation of Gallup and WVS (question wording, sampling, modes).
- [ ] Do the Gallup analysis restricted to WVS countries.
- [ ] Use adjusted R² (and/or cross-validated R²): region (4 dummies) and income clusters (4–6 dummies) have more degrees of freedom than log GDP (1).
- [ ] Correlation matrix between well-being indicators.
- [ ] Share of people with well-being below 60% of the (national) average.
- [ ] First split in a regression tree with region and income.
- [ ] Robustness: redo the analysis without Latin America and the former Eastern Bloc.
- [ ] Appendix table (csv/xlsx) with well-being indicator values for each country-wave.
- [ ] ? Use `happiness_Inglehart` (average of rescaled happiness and satisfaction means).
- [ ] ? Add other explanatory variables, e.g. growth, median income.
- [ ] Variance explained by religiosity, tolerance, free choice, democracy, GDP, growth.
- [ ] Automate recovery of missing GDP data (IMF or WB Global Economic Prospects) instead of manual imputations.
- [ ] Switch to GDP p.c. PPP in constant 2021 $ (WDI, `NY.GDP.PCAP.PP.KD`) and impute missing years.
- [ ] Compute constant nominal $ from IMF for missing nominal GDP (currently current $ are used).
- [ ] Complete missing Gallup GDP with IMF data.
- [ ] Cite Guriev & Zhuravskaya (2009); Sofia Panasiuk (no paper yet). **(A)**
- Note from old code: "we don't have a clear-cut method to attribute the discrepancy to question wording vs. sample representativeness … we could instead conduct a survey that compares answers to the two wordings" → done: Fabre (2025).

## 4. Data updates

- [ ] GDP p.c.: download the latest WDI vintage via the World Bank API (PPP constant 2021 $ and constant 2015 $), cache in `data/`.
- [ ] Gallup: newer waves (2023–2025) are only available publicly as World Happiness Report 3-year ladder means (`data/WHR26_Data_Figure_2.1.xlsx`, 2011–2025). Use them for the *Satisfaction (mean)* analysis; share-based Gallup indicators remain limited to `gallup.xlsx` (waves ≤ 18 = 2023).
- [ ] **(A)** Gallup Analytics export of the ladder distribution for waves 19–20 (2024–2025), same format as `gallup.xlsx`, would allow share-based indicators for recent years and the exact 2025 comparison with Fabre (2025).
- [ ] **(A)** WVS: `WVS.rds` is the WVS time-series v3.0 (2022-12-14). WVS wave 8 (2024–2026) is still in fieldwork (no public release as of Sept. 2026). A newer time-series release (with the final WVS-7, which added a few countries) and the EVS 2017 (→ Integrated Values Surveys, cf. `data/IVS_dictionary.xlsx`) would add ~30 European country-years. Both require accepting the terms of use on worldvaluessurvey.org / GESIS: please download them to `data/`.

## 5. New TODO suggestions (added during the analysis)

- [ ] Use alternative region classifications (World Bank regions, UN geoscheme, Inglehart–Welzel cultural zones) to show that results do not hinge on the ex-post grouping (Turkey in "Western", Middle East in "Asia"…).
- [ ] Account for repeated observations of the same country (cluster by country; one observation per country; country-level averages across waves).
- [ ] Out-of-sample prediction (leave-one-country-out / predict wave *t+1* from wave *t*) as a direct measure of "predictive capacity".
- [ ] Within-country (over time) relation between GDP and well-being, to connect with the Easterlin paradox.
