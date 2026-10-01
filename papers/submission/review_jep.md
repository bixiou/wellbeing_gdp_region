# Referee report — Journal of Economic Psychology

**Manuscript:** "Does Money Buy National Happiness? It Depends on Who You Ask and How" (Research Article)
**Recommendation:** Major revision

## Summary

The paper asks how well GDP per capita predicts national well-being, and why the World Values Survey (WVS/EVS) and the Gallup World Poll disagree on the answer.

**Part 1: income versus region.** Using all WVS waves (342 country-years, 1981–2023), the paper compares log GDP and other income measures with a five-region classification. It uses Shapley decompositions of R², adjusted R², leave-one-country-out cross-validation, and many robustness checks.
- Region predicts better in the WVS (14% vs. 33% of variance on average), driven by Latin America and Eastern Europe.
- In Gallup, income predicts as well as region.
- A replication of Deaton's (2008) negative growth effect shows that it is driven by former communist countries in 2000–2003.

**Part 2: question versus sample.** A randomized 2×2 experiment (ladder vs. satisfaction; 0–10 vs. 1–10) in an online survey of about 11,000 respondents in ten high-income countries gives three results:
- Wording shifts levels by about 0.45 points and fully accounts for the higher level of WVS answers.
- Across the ten countries, the ladder is better predicted by GDP than satisfaction. The author reads this as wording explaining about half of the Gallup–WVS difference in R².
- Both questions yield income gradients as steep as Gallup's, so the flat WVS gradient comes from its samples.

The paper fits the journal well: it sits at the intersection of the economics of happiness and the psychology of survey response. It is transparent (code, frozen data vintages, bootstrap CIs, sensitivity analyses) and honest about its limitations. Combining global observational data with a randomized design is a real strength. My main concerns are about inference with ten countries, focus, and length.

## Major comments

**1. Inference in the ten-country analysis.**
- The confidence intervals resample respondents and treat the ten countries as given. For cross-country R² and slopes, the relevant uncertainty is the sampling of countries.
- With n = 10, results are fragile: dropping Japan lowers the wording share from 56% to 38%.
- Among the four variants, only the ladder 0–10 has a high R² (0.76). The other three range from 0.23 to 0.33.
- **Requests:**
  - leave-one-country-out and permutation distributions of the R² difference and of the slope difference-in-differences;
  - a test of the wording × scale interaction on the R²;
  - wording that presents "about half" as a point estimate with very wide uncertainty.
- The slope result (wording does not change the gradient) seems more robust and deserves at least equal prominence.

**2. What exactly is "samples"?**
- The residual mixes sampling frames, interview modes, questionnaire context, translations, and time. The Fabre data are from 2025; the WVS/EVS from 2017–2022, and 2003 for Saudi Arabia.
- The data-quality section usefully shows that education and urbanicity imbalances in WVS samples do not explain the gap.
- Two things would help: a table decomposing the residual by observable survey features (mode, year gap, EVS vs. WVS), and, if possible, comparing like-mode subsets (e.g., the US WVS 2017, which was online, versus the online experiment).

**3. Region versus income: framing and forking paths.**
- The region advantage disappears when Latin America and Eastern Europe are both excluded, and with continents. The finding is therefore best stated as "two well-known regional deviations explain more cross-country variance than income in the WVS".
- The paper now says this, but the abstract and introduction still lead with "region is a better predictor in 88% of 952 specifications". That count treats correlated indicators and income measures as independent; I suggest reporting it per indicator.
- The main classification involves ex post choices (Turkey in the Western group, Israel in Asia). The exact UN groups give the same result, which is reassuring and should be stated up front.

**4. Length and focus.**
- The paper is at, or slightly above, the journal's 12,000-word limit, and it covers two related but distinct questions.
- Some material could move to an online appendix without loss:
  - the happiest-country rankings;
  - the correlates section;
  - the Deaton replication;
  - the detailed level decomposition.
- This would let the main text focus on the central argument: the strength of the income–well-being relation depends on the dataset, and the experiment shows why.

**5. Online panel and question placement.**
- The experiment was run online, at the end of a questionnaire on global policies. Context effects on life evaluations are documented (Deaton & Stone 2016).
- Please discuss whether the preceding questions could interact with wording, e.g., by priming material comparisons more with the ladder.
- Please also state whether the wording analysis was pre-registered.

## Minor comments

- Gallup statistics come from unweighted answer distributions. Report the correlation with the weighted WHR series in the main text, and discuss any implications for the share-based indicators.
- In Table 1 and similar tables, bold marks the row maximum. When two cells tie at the displayed precision (Very Unhappy: 0.14 and 0.14), explain the tie-break.
- The within-country (fixed-effects) results are interesting but brief. A sentence on how they relate to recent Easterlin-paradox debates would help.
- Abstract: "three times steeper" depends on the ten-country sample. State it as such.
- Footnotes: the journal asks authors to avoid footnotes at initial submission. The few footnotes (World Bank regions, GDP imputation, survey details) could move into the text.

## Assessment

The paper is careful, transparent and relevant to the journal's readership, and the randomized wording experiment is a valuable addition to a debate usually conducted with observational data. The two main issues are inference with ten countries and the framing of the region result. Both can be addressed within a revision without new data collection, and length can be reduced by moving secondary material to an online appendix. I recommend a major revision.
