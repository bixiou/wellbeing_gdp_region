# TODO

Legend: `[x]` done, `[ ]` to do, `[~]` partly done / see note. **(A)** = needs Adrien (data access, decision, or information only he has); all such items are collected in the next section.

## TODO for Adrien

- [x] WVS time-series: v3.0 (2022-12-14) is the latest release (confirmed by Adrien). WVS wave 8 (2024–2026) is not public yet: add it when released.
- [x] Joint EVS/WVS 2017–2022 (ZA7505 v5.0.0) added in `data/` and integrated in `main.R`: +33 EVS 2017 surveys, +2 WVS-7 surveys (India 2023, Uzbekistan 2022). All survey countries except Saudi Arabia now have a WVS/EVS survey from 2017–2022.
- [ ] **Optional**: EVS Trend File 1981–2017 (ZA7503 v3.0.0, https://search.gesis.org/research_data/ZA7503) would add the earlier EVS waves (1981–2008, ~125 European country-years) and complete the Integrated Values Surveys for all waves; only useful for the region vs. income analysis (not for the decomposition).
- [x] Gallup data provenance: access given by Armon Rezai (WU Wien); acknowledged in the papers.
- [ ] **Optional ask to Armon Rezai / WU Wien** (Gallup World Poll microdata): ask for (i) the same crosstab *weighted* (variable WGT), (ii) waves 19–20 (2024–2025), (iii) the positive/negative affect items, to test whether emotions are better predicted by region than by income.
- [ ] **Postal address** of the corresponding author (title footnote of the papers).
- [ ] **AI-use declaration**: adapt the draft at the end of each paper.
- [ ] **References**: verify entries flagged `TODO: verify` in `papers/wellbeing.bib` (Blanchflower & Bryson 2023, Nilsson et al. 2024, WVS trend-file authors, Kapteyn et al. 2010 pages); provide full references for Galbraith et al. (2024), Ritter et al. (2025), Prica & Bartlett (2026), Sofia Panasiuk.
- [ ] **Companion-paper citation** in the split papers (`\todo{cite companion paper}`), if you choose to split.
- [ ] **Data deposit**: create an OSF project with data and code (JEP requires public data before acceptance) and state how to obtain the Gallup data, which cannot be redistributed.
- [ ] **Decisions**: combined vs. split papers (recommendation in §8); title of the combined paper; whether to update `presentations/wellbeing_prez.tex` with the new results.
- [ ] **Optional, new data collection**: replicate the 2×2 wording × scale experiment face-to-face or by phone in 5–10 low/middle-income countries, with official Gallup/WVS translations and the question asked early (the only way to settle the discrepancy where it is largest); add anchoring vignettes.


## 0. Project goals (from `CLAUDE.md`)

- [x] 0. Understand the repository, create `README.md` and `TODO.md` (with these instructions and the TODOs of `wellbeing_prez.tex` and `old_data.R`).
- [x] 1. Use Fabre (2025) to find the reason for the Gallup/WVS discrepancy: regress well-being on wording × scale, estimate the effect of wording, compare the predicted indicators of past data (for the alternative wording/scale) with the empirical evidence; decompose the discrepancy into wording and residual (≈ different samples). If wording explains more, the multi-indicator/multi-dataset strategy is validated; otherwise emphasize the dataset with the best sampling.
- [x] 2. Update the analysis of `old_data.R`: (a) update data series (GDP p.c.…); (b) integrate more recent waves of Gallup or WVS if they exist; (c) extend the analysis to Fabre (2025).
- [x] 3. Do the other TODOs (below).
- [x] 4. Find the weaknesses of the analysis and propose improvements in methodology/analysis.
- [x] 5. Write `papers/wellbeing.tex` following the Journal of Economic Psychology's requirements, the structure and interpretation of `wellbeing_prez.tex`, plus steps 1–4.
- [x] 6. Write `papers/wellbeing_region.tex` (region vs. GDP only) and `papers/wellbeing_discrepancy.tex` (wording vs. sampling only), ≤ 9k words each excluding appendices; recommend combined vs. split and the best journal for each.

## 1. Bugs and issues found in `old_data.R` (to fix in `main.R`, not in `old_data.R`)

- [x] **Gallup wave → year mapping is off by 5 years.** `g$year <- floor(g$wave) + 2000` should be `floor(g$wave) + 2005` (Gallup World Poll wave 1 = 2005/06). Evidence: Lebanon's ladder collapses in waves 15–17 (2.83, 2.25, 2.47) and the WHR 3-year averages match only with wave 14 = 2019 (e.g. WHR 2019–21 average 2.955 vs. (4.01 + 2.83 + 2.25)/3 = 3.03); Afghanistan's record low 1.25 is wave 17 = 2022. Consequence: Gallup observations were matched with GDP 5 years too early, and the Gallup/WVS/Fabre comparison paired the wrong years (e.g. "GBR: 2018, JPN: 2017").
- [x] **`very_happy_minus_very_unhappy` is a weighted *count*, not a share difference** (`sum(w * (h==1)) - sum(w * (h==4))`), so it scales with the sample size of each country-year. Should be `weighted.mean(h==1) - weighted.mean(h==4)`.
- [x] **k-means clusters are not reproducible**: the k-means block in `create_gdp_vars` is commented out (so `run_regressions` fails as is), and there is no `set.seed()`; k-means with random starts can give different clusters across runs. Use `set.seed()` and `nstart = 50` (or 1-D exact clustering, e.g. `Ckmeans.1d.dp`).
- [x] `plot_all()` is called (l. 457) before being defined (l. 459).
- [x] Code depends on `.Rprofile` helpers and on `%>%`; `main.R` is self-contained and uses `|>`.
- [x] Presentation typo: "Region is a better predictor than region" → "than income".

## 2. TODOs from `presentations/wellbeing_prez.tex`

- [ ] Cite Galbraith et al. (2024), Ritter et al. (2025), Prica & Bartlett (2026); check Blanchflower & Bryson (2023). **(A)** full references needed — I will not guess them.
- [x] Plot *Happy* against *Satisfied* (`figures/main/happy_vs_satisfied.pdf`).
- [x] Fix NA in tables `gdp.tex` and `share_gdp.tex` (new tables `tables/main/r2_income.tex`, `share_income.tex` have no NA; the NA came from the k-means code being commented out).
- [x] Results: some indicators are not significantly related to GDP p.c., some (e.g. share *Very happy*) even decrease with it → report slopes and significance.
- [x] Results: table of happiest countries (`tables/main/happiest_countries.tex/.csv`).
- [x] Robustness checks: only last observation per country, population weights, excluding pandemic years, without imputed GDP (already in old code) — keep in the paper.
- [x] Robustness: number of missing answers (DK/refusals) by country (≈1.5%, uncorrelated with GDP; see `numbers.tex`).
- [~] Other explanatory variables: growth done (`correlates.tex`); median income still to do (World Bank PIP).
- [ ] Future research: check whether emotions (affect) are better predicted by region than by income (Gallup positive/negative affect; WHR data).
- Journals aimed (from prez): JPubE (9k words, 165 $) > Journal of Economic Psychology (12k, 0 $) > Journal of Happiness Studies (10k, 0 $) > Journal of Wellbeing Economics (10k, 0 $).

## 3. TODOs from `code_wellbeing/old_data.R`

- [~] Check documentation of Gallup and WVS (question wording, sampling, modes): WVS modes by country from the `mode` variable are used; Gallup modes to document from the Gallup World Poll methodology. **(A)**
- [x] Do the Gallup analysis restricted to WVS countries (spec `gallup_wvs_countries`).
- [x] Use adjusted R² (and/or cross-validated R²): region (4 dummies) and income clusters (4–6 dummies) have more degrees of freedom than log GDP (1).
- [x] Correlation matrix between well-being indicators (`correlation_indicators.tex`).
- [x] Share of people with well-being below 60% of the (national) average (indicator `low_satisfaction`).
- [x] First split in a regression tree with region and income (region for all 8 indicators).
- [x] Robustness: redo the analysis without Latin America and the former Eastern Bloc (**the region advantage disappears**: see §6).
- [x] Appendix table with well-being indicator values for each country-wave (`tables/main/wellbeing_country_year_*.csv`).
- [ ] ? Use `happiness_Inglehart` (average of rescaled happiness and satisfaction means).
- [ ] ? Add other explanatory variables, e.g. growth, median income.
- [x] Variance explained by religiosity, tolerance, free choice, democracy, GDP, growth (`correlates.tex`, Shapley decomposition).
- [x] Automate recovery of missing GDP data (IMF or WB Global Economic Prospects) instead of manual imputations.
- [x] Switch to GDP p.c. PPP in constant 2021 $ (WDI, `NY.GDP.PCAP.PP.KD`) and impute missing years.
- [x] Compute constant nominal $ from IMF for missing nominal GDP (currently current $ are used).
- [x] Complete missing Gallup GDP with IMF data (only Cuba and Yemen 2006–13 remain missing).
- [ ] Cite Guriev & Zhuravskaya (2009); Sofia Panasiuk (no paper yet). **(A)**
- Note from old code: "we don't have a clear-cut method to attribute the discrepancy to question wording vs. sample representativeness … we could instead conduct a survey that compares answers to the two wordings" → done: Fabre (2025).

## 4. Data updates

- [x] GDP p.c.: download the latest WDI vintage via the World Bank API (PPP constant 2021 $ and constant 2015 $), cache in `data/`.
- [x] Gallup: newer waves (2023–2025) are only available publicly as World Happiness Report 3-year ladder means (`data/WHR26_Data_Figure_2.1.xlsx`, 2011–2025). Use them for the *Satisfaction (mean)* analysis; share-based Gallup indicators remain limited to `gallup.xlsx` (waves ≤ 18 = 2023).
- [ ] ~~Gallup Analytics export for 2024–2025~~: no longer possible (Adrien has no Gallup Analytics access anymore). Recent Gallup years rely on WHR 3-year means; share-based Gallup indicators stop in 2023.
- [x] WVS/EVS: `WVS.rds` (time-series v3.0) completed with the Joint EVS/WVS 2017–2022 dataset (see TODO for Adrien).

## 5. New TODO suggestions (added during the analysis)

- [x] Ten-country comparison of income gradients, past (Gallup, WVS/EVS) vs. new data (4 Fabre variants), and individual-level regression well-being ~ income × wording × scale (requested in `wellbeing.tex`): wording hardly changes the gradient, the WVS/Gallup difference in gradients is mostly not due to the question (Table `ten_countries.tex`).
- [x] Representativeness of WVS/EVS samples tested (`main.R` §3.4, `tables/main/sample_representativeness.tex`, `wvs_sample_composition.csv`): tertiary-educated and urban residents are over-represented in poorer countries (≈2× below 10k$ GDP p.c.), but this does not predict the Gallup–WVS gap and education reweighting barely changes the WVS income gradient. Internal criterion (women among married = 50%) violated at 5% in about a third of surveys.
- [ ] **(A)** Ask Armon Rezai whether Gallup microdata can give weighted vs. unweighted ladder means and sample composition (education, urbanicity) by country-year, to run the same test on Gallup.
- [ ] Robustness: exclude Gallup 2020–2021 (switch from face-to-face to telephone during COVID) and flag 2023 in the 27 countries where 20% of interviews came from (mostly opt-in) web panels.
- [ ] With the EVS, test whether EVS and WVS surveys in the same country and period differ systematically (7 countries; mean absolute difference 0.31 points), e.g. by mode.

- [x] Use alternative region classifications (World Bank regions, UN geoscheme, Inglehart–Welzel cultural zones) to show that results do not hinge on the ex-post grouping (Turkey in "Western", Middle East in "Asia"…).
- [~] Account for repeated observations of the same country (cluster by country; one observation per country; country-level averages across waves).
- [x] Out-of-sample prediction (leave-one-country-out / predict wave *t+1* from wave *t*) as a direct measure of "predictive capacity".
- [x] Within-country (over time) relation between GDP and well-being, to connect with the Easterlin paradox.

- [x] Paper TODOs of commit e2f591c implemented: Section 4 reordered (discrepancy → ten-country test with R² first → levels → data quality); pooled Fabre rows (both scales, all variants) with bootstrap CIs of R² and of the share of the Gallup–WVS R² difference due to wording; D, Q, R redefined as WVS − Gallup, satisfaction − ladder (positive); decomposition summary with CIs in a separate column; decomposition figure (D orange, on top); region-classification table (w/o Latin America, w/o Eastern Europe, w/o both; UN sub-regions removed); Gallup LMG table in the main text; Very Unhappy figure without Egypt 2013; Blanchflower & Bryson (2024), Deaton (2008), Killingsworth et al. (2023) and within-country references added; conclusion rewritten.
- [ ] **New result to discuss (A)**: with R² (predictive capacity), the wording accounts for about half of the Gallup–WVS difference in the ten countries (pooled scales: 0.56, CI 0.36–0.83), whereas with slopes it accounts for little. Only Gallup's original variant (ladder 0–10) yields a high R² (0.76; the three other variants 0.23–0.33): with 10 countries, wording and scale effects on R² cannot be clearly separated. A pre-registered replication with more countries would settle this.
- [x] Sensitivity to Japan (largest deviation with the satisfaction question): without Japan, the share of the Gallup–WVS R² difference due to wording falls from 0.56 to 0.38 (reported in the paper).
- [ ] Full leave-one-country-out table of the ten-country R² and of the wording share (appendix), to show the sensitivity to each country.

## 6. Weaknesses of the analysis and proposed improvements (step 4)

Based on `wellbeing_prez.tex` and on the new analyses in `code_wellbeing/main.R` (numbers from `tables/main/`).

**A. Region vs. income**

1. [ ] **The headline result is dataset-dependent.** In the WVS, income explains less of the explained variance than region in 86% of specifications. In Gallup (168 countries, 2006–2023) it is roughly a tie: income wins for the mean ladder (share ≈ 60%) and for the shares satisfied/unsatisfied, while region wins for the upper-tail indicators. WHR 2023–25: income wins (R² 0.63 vs. 0.55). *Improvement:* reframe the claim as "GDP explains little of national well-being as measured by the WVS, and no more than region in Gallup"; present Gallup on equal footing; make the Gallup/WVS discrepancy (part B) an integral part of the argument.
2. [ ] **The region advantage is driven by Latin America (happier than GDP predicts) and the former Eastern Bloc (unhappier).** Without them, region beats income in only 27% of specifications. Region remains better when only one of the two is excluded (70% w/o Latin America, 73% w/o Eastern Europe), and with classifications that do not isolate them (continents 48%, World Bank regions 55%) the advantage shrinks (Table `region_classifications.tex`). *Improvement:* say so explicitly — the result is about two well-known "anomalies" (cf. Inglehart et al. 2008; Guriev & Zhuravskaya 2009; Graham & Lora 2009) rather than about regions in general; investigate their mechanisms (e.g. social ties, transition shock, response styles).
3. [ ] **Unequal numbers of parameters and forking paths.** Region has 4 dummies vs. 1 slope for log GDP, and the classification was chosen ex post (Turkey in "Western", Middle East in "Asia"…). *Done:* adjusted R², leave-one-country-out cross-validated R² (region still better in the WVS), 4 alternative classifications. *Still to do:* pre-specify one standard classification (UN geoscheme or Inglehart–Welzel cultural zones) in the paper; permutation benchmark (R² of random partitions of countries into 5 groups).
4. [ ] **Pooling unbalanced country-years without inference.** Countries surveyed in many waves weigh more; R² shares have no confidence intervals. *Improvement:* country-clustered bootstrap CIs for R² and LMG shares; country-mean specification (one observation per country, averaged over waves).
5. [ ] **Multiplicity of indicators treated as independent evidence** ("94% of specifications"). The 8 indicators are correlated (0.6–0.8). *Improvement:* pre-specify mean satisfaction/ladder as the primary outcome, others as secondary; or use the first principal component.
6. [ ] **Region is not a mechanism.** It may capture culture, history, institutions — or cross-cultural differences in scale use (response styles), which would make measured well-being less comparable across regions rather than well-being truly different. *Improvement:* (i) Shapley decompositions with freedom of choice, religiosity, tolerance, democracy (done: freedom of choice alone explains 63% of the variance of mean satisfaction); (ii) anchoring vignettes or scale-use corrections (e.g. Kapteyn, Smith & van Soest; Benjamin et al.) in a future survey; (iii) affect measures (Gallup positive/negative affect).
7. [ ] **GDP p.c. is a poor proxy of household material living standards** (esp. in resource-rich countries: Qatar, Kuwait, Saudi Arabia). *Improvement:* use household final consumption expenditure per capita and median income (World Bank PIP) as alternative income measures.
8. [ ] **Gallup distributions are unweighted counts** (SPSS crosstab of the World Poll microdata). They correlate at 0.993 with WHR (weighted) 3-year averages, but share indicators may be slightly biased. Weighted distributions would require renewed access to the microdata (see TODO for Adrien).
9. [ ] **Cross-section vs. time series.** The within-country relation (country fixed effects) is weaker (within R² 0.22 for mean satisfaction) — link the paper to the Easterlin paradox debate (Easterlin et al. 2010; Stevenson & Wolfers 2008; Sacks et al. 2012).
10. [ ] Presentation: the formula for $s_i$ on the slide "Comparing the share of variance…" lacks a division by 2 (the code, based on `relaimpo`, was correct). The slide "Variance explained by GDP p.c." describes the $R^2$ of income alone, but `gdp.tex` reported the LMG of income in the joint model: make consistent.

**B. Gallup vs. WVS discrepancy**

11. [ ] **Fabre (2025) covers only 10 high-income countries**, whereas the Gallup–WVS gap (and thus the difference in GDP gradients) is largest in low- and middle-income countries (gap ≈ −2 points at 3,000 $ vs. ≈ 0 at 50,000 $). The question effect cannot be extrapolated (its GDP gradient in the 10 countries is 0.30 with s.e. 1.75). *Improvement:* field the same 2×2 experiment in 5–10 low/middle-income countries with face-to-face or phone interviews (ideally with Gallup's or WVS's own fieldwork partners), which is the only way to settle the question for the global gradient.
12. [ ] **The residual lumps several things together:** sampling frame, mode (Gallup phone/F2F; WVS F2F, web, mail depending on the country; Fabre online), questionnaire context (Fabre asks well-being at the end of a long policy survey; WVS asks satisfaction right after happiness), translations (Fabre's own vs. official ones), and time (WVS years range from 2003 to 2022, Fabre 2025; the question effect is assumed stable over time). *Improvement:* in a new wave, use the official Gallup/WVS translations, ask the question early, and add a within-subject arm (both questions, random order).
13. [ ] **Online panel respondents report ~1 point lower well-being than Gallup and WVS respondents in the same countries** — a sample/mode effect of the same order as the wording effect. Worth a dedicated discussion (selection into online panels, social desirability in interviewer-administered modes; cf. Dolan & Kavetsos 2016). Compare with probability-based online panels (LISS, GESIS Panel, KnowledgePanel).
14. [ ] **Power:** with 10 countries, cross-country statistics (variance shares, gradients) have wide CIs; report them (done with bootstrap) and avoid over-interpreting point estimates.
15. [ ] Scale: 0–10 vs. 1–10 answers are compared through a linear stretch; alternative: compare distributions (e.g. share ≥ 6, done) or use ordered-probit thresholds.

## 7. Papers: remaining TODOs (search for `\todo{` in `papers/`)

- [x] Corresponding-author e-mail, funding (none) and acknowledgements added. Gallup access acknowledged (Armon Rezai, WU Wien). Still missing: postal address, AI-use declaration (a draft is provided) → see TODO for Adrien.
- [ ] **(A)** Verify the references flagged `TODO: verify` in `papers/wellbeing.bib` (Blanchflower & Bryson: done, published version 2024 in Social Indicators Research, rankings checked against its Table 5; Nilsson et al. 2024, WVS trend file authors, Kapteyn et al. 2010 pages) and add Galbraith et al. (2024), Ritter et al. (2025), Prica & Bartlett (2026), Sofia Panasiuk.
- [ ] **(A)** Deposit data and code on OSF (JEP requires public data before acceptance; Gallup data cannot be redistributed: explain how to obtain them).
- [ ] Highlights (3–5, ≤ 85 characters) are only needed after a revise-and-resubmit at JEP.
- [ ] Update `presentations/wellbeing_prez.tex`? (not modified, per CLAUDE.md: a new presentation could reuse `figures/main/` and `tables/main/`).

## 8. Recommendation: combined vs. split papers (step 6)

Submit the **combined paper** (`wellbeing.tex`) to the **Journal of Economic Psychology**. Rationale: with the updated data, "region predicts better than income" holds in the WVS but not in Gallup, so a stand-alone region paper would immediately face the objection that the leading dataset contradicts it; the experiment answers precisely that objection, and together they tell one coherent story (the result depends on the dataset; wording explains about half of the Gallup–WVS difference in predictive capacity, samples the rest and most of the difference in income gradients). It fits JEP's scope (economic psychology, survey measurement) and its 12,000-word limit (current draft ≈ 9,900 words including tables, references and appendix). JPubE is not a good fit: the paper is descriptive/methodological, without a public-finance question or causal policy analysis.
If split: `wellbeing_discrepancy.tex` (novel experimental contribution) → JEP (or as a *Brief Report*, ≤ 4,000 words excluding abstract and references, after light trimming); `wellbeing_region.tex` → Journal of Happiness Studies (or Social Indicators Research).

- [x] **Word count (JEP)**: shortened (Section 4 reordered, secondary tables moved to the appendix, discussion 5.1 condensed): ≈ 7,900 words main text + ≈ 1,350 references + ≈ 2,400 appendix (tables included, figures excluded) ≈ 11,700 words in total.
- [x] **JEP compliance** (guide for authors: 12,000 words including abstract, text, references, tables, figures, captions and appendix, but not the Online Appendix; footnotes avoided; abstract ≤ 250 words; public data with download instructions in the title footnote): appendix moved to `papers/wellbeing_online_appendix.tex` (separate PDF titled "Online Appendix"); footnotes turned into text; data statement completed. Counted manuscript ≈ 8,100 words (text, tables, captions) + ≈ 1,450 references ≈ 9,500 words.
- [ ] **(A)** Deposit data and code on OSF or Mendeley Data before submission (JEP asks for a repository URL, GitHub may not suffice).
- [ ] **(A) PNAS** (`papers/wellbeing_pnas.tex` + `wellbeing_pnas_si.tex`): check the requirements on pnas.org (the draft follows: abstract ≤ 250 words, significance statement ≤ 120 words, ~6 pages / ~4,000 words, 4 display items, numbered references, Materials and Methods last); add ORCID; check the postal address of CIRED; choose the title; transfer to the official PNAS LaTeX template (Overleaf) after acceptance (format-neutral initial submission); decide Direct Submission vs. contributed/communicated by an NAS member.
- [x] Paper TODOs of commit d5f6542: appendix back in `wellbeing.tex`; six continents redefined (North America incl. Central America & Caribbean vs. South America; region better in 48% of cases with all three definitions: 5 continents, 6 old, 6 new); median income (PIP) and household consumption in the correlates table (region remains the main contributor); country-clustered bootstrap CIs for the main R² and LMG shares; Deaton (2008) replicated (growth coefficient −5.50, p = .017, 2006 Gallup) but driven by former communist countries (−0.53, p = .855 without them); shares in percent; cov(D,R)/var(D) = 89%; Guriev & Zhuravskaya, Galbraith et al. (2024), Global Flourishing Study (VanderWeele et al. 2025) cited; bibliography checked against Crossref/DataCite, DOIs and URLs added; ORCID; highlights drafted (commented out).
- [x] Cover letters: `papers/submission/cover_letter_pnas.docx`, `papers/submission/cover_letter_jep.docx`. Referee-style reviews: `papers/submission/review_pnas.md` (reject), `papers/submission/review_jep.md` (major revision).
- [ ] **Word count (JEP)**: ≈ 12,150 words (≈ 8,350 text/tables/captions + ≈ 2,470 appendix + ≈ 1,340 references; figures not counted): ≈ 150 words to cut, or move part of the appendix to an Online Appendix (not counted). **(A)**
- [ ] JEP asks to avoid footnotes at initial submission: 4 footnotes remain (World Bank regions, GDP imputation, Russia, quotas). **(A)**
- [ ] From the reviews: (i) country-level inference for the ten-country R² and slopes (leave-one-country-out and permutation distributions of the R² difference and of the slope diff-in-diff); (ii) test of wording × scale on R²; (iii) report "88% of specifications" per indicator; (iv) state which analyses of the wording experiment were pre-registered (OSF 7mzn4) **(A)**; (v) residual R by mode / year gap / EVS vs. WVS.
- [x] Paper TODOs of commit 1bd1ca6: datasets on equal footing (abstract, introduction); average R² of region in abstract and introduction; six continents (North America separate from Latin America; results unchanged: 48% of cases); bold highest R² per row (Table 1) and shares > 0.5 (ten-country table); CV R² of the best income measure; new subsection for Gallup; happiest country-years of all waves; shares of the R² difference in percent; Deaton (2008) growth test (growth not negatively related to satisfaction conditional on GDP: +0.039, p = .044 in the WVS; 0.001, p = .957 in Gallup).
- [ ] Extend the representativeness test: reweight WVS samples jointly on education, urbanicity and age (raking), where benchmarks allow.

## 9. Submission strategy (answer to the TODO at the top of `wellbeing.tex`)

My assessment (judgment, not certainty):
- **Top 5 (AER, QJE, JPE, Econometrica, REStud): reject highly likely.** The contribution is descriptive and methodological, without causal identification or a new economic mechanism, and the experiment covers 10 high-income countries only.
- **Journal of Economic Perspectives: not a submission venue** for original research (articles are mostly commissioned surveys); an invited overview on cross-country well-being data could be pitched to the editors later.
- **Brookings Papers on Economic Activity: by invitation only** (papers commissioned for the conference).
- **PNAS: long shot but plausible.** Short format, broad-interest message (the income–well-being gradient across countries depends on survey samples, not on the question), and precedents in the same area (Kahneman & Deaton 2010; Kaiser & Oswald 2022; Killingsworth et al. 2023). High desk-rejection risk; requires a much shorter version (main text ≈ 3,000–4,000 words, rest in SI).
- **Nature Human Behaviour: long shot.** Publishes cross-country well-being work (Jebb et al. 2018), but the bar on novelty and generality is high, and the experiment's restriction to high-income countries is a weakness they are likely to point out.

Proposed pecking order: (1) PNAS (short version: headline = the strength of the income–well-being relation depends on the question and the sample, the question explaining about half of the Gallup/WVS difference in predictive capacity; region vs. income as motivation); (2) Journal of Economic Psychology (combined paper, current format); (3) Journal of Happiness Studies. The Journal of Public Economics is a weak fit (no public-finance question); I would skip it unless the paper is reframed around policy use of well-being data.

**CNRS section 37 list (June 2020)**: JEP (Journal of Economic Psychology) is category 2, Journal of Happiness Studies category 4. Among category-1 journals, the most likely to accept the paper (judgment): **World Development** (DevTrans; cross-country well-being and "beyond GDP" in developing countries — frame the introduction/conclusion around development, present the high-income-only experiment as a limitation), then **Social Science & Medicine** (SANT; measurement of subjective well-being). Less likely: Journal of Public Economics, European Economic Review, Economic Journal, REStat, JEEA, Journal of Development Economics. Check whether a more recent CNRS list exists and World Development's word limit before submitting.
