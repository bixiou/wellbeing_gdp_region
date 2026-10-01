# Referee report — PNAS

**Manuscript:** "Whether richer countries are happier depends on the survey" (Research Article, Social Sciences)
**Recommendation:** Reject (encourage submission to a field journal)

## Summary

The paper makes two claims.

1. **Region versus income.** In the World Values Survey / European Values Study (WVS, 342 country-years, 112 countries), log GDP per capita explains on average 14% of the cross-country variance of eight well-being indicators. A five-region classification explains 33%, and it predicts better out of sample. In the Gallup World Poll, income predicts the ladder as well as region.
2. **Question versus sample.** To separate the two, the author randomizes the question (Cantril ladder vs. life satisfaction, 0–10 vs. 1–10 scale) in an online survey of about 11,000 respondents in ten high-income countries.
   - Asked to the same sample, the ladder has a higher cross-country R² on GDP than life satisfaction (0.61 vs. 0.28).
   - The author interprets this as wording accounting for 56% (95% CI 22–82%) of the Gallup–WVS difference in R² in these ten countries.
   - The slopes of both questions are close to Gallup's (about 3.6–3.9 vs. 4.2), while the WVS slope is 1.4.

The question matters: the Gallup–WVS discrepancy shapes widely cited conclusions, including World Happiness Report rankings and the Easterlin debate, and a randomized design is the right tool to separate question from sample. The paper is clearly written, transparent, and unusually careful about robustness. It shares code, reports confidence intervals, and openly flags its own fragilities: sensitivity to Japan, and the high R² of only one of four variants. My recommendation reflects the fit with PNAS's bar for broad significance and the strength of the central experimental inference, not the quality of the execution.

## Main concerns

**1. The headline experimental quantity rests on ten data points.**
- The "wording explains about half" claim compares R² from regressions with ten observations each.
- The bootstrap resamples respondents within countries and treats the ten countries as fixed. It therefore ignores the dominant source of uncertainty for a cross-country R², which is the draw of countries.
- Dropping Japan moves the share from 56% to 38%, and the confidence interval already spans 22–82%.
- Among the four randomized variants, only Gallup's exact question (ladder 0–10) yields a high R² (0.76). Ladder 1–10 yields 0.33, and the two satisfaction variants 0.23 and 0.31.
- This pattern fits an interaction of wording and scale, a chance outcome in one cell, or a genuine wording effect. The data cannot tell these apart.
- **Request:** a permutation or leave-one-country-out distribution of the R² difference, and inference that treats countries as the sampling unit. As it stands, the evidence for a wording effect on predictive capacity is suggestive at best.

**2. External validity is limited exactly where the discrepancy lies.**
- The WVS–Gallup gap is about two points in low-income countries and small in high-income countries (Fig. 2).
- The experiment covers only high-income countries, through an opt-in online panel, at the end of a long questionnaire on international redistribution.
- The paper acknowledges this, but it means the experiment cannot speak to the global relation that motivates the paper.
- For PNAS, a design run where the discrepancy is (face-to-face, in low- and middle-income countries, with official translations) would be needed to support general claims.

**3. Confounds in the comparison with past data.**
- The R² and slopes of Fabre (2025) are compared with WVS/EVS surveys from 2017–2022 (2003 for Saudi Arabia) and with Gallup data from the same years.
- The two sides also differ in GDP year and in mode (online vs. phone or face-to-face).
- The decomposition "wording vs. samples" therefore attributes to "samples" everything else, including time.
- This is acknowledged, but the label "samples" invites a narrower reading than the residual warrants.

**4. Novelty of the region result.**
- That Latin America is happier and post-communist countries unhappier than their income predicts is well documented (Graham & Lora; Guriev & Zhuravskaya; Inglehart et al.).
- The paper itself shows that the region advantage disappears when both groups are excluded, and with continents.
- Framing this as "region beats income" is a useful benchmark, but it is mostly a re-expression of two known anomalies.
- The classification was also chosen after seeing the data (Turkey Western, Israel in Asia). Cross-validation helps, but does not remove that concern.

**5. Interpretation of R² versus slopes.**
- The paper argues that predictive capacity (R²) is the object of interest. Yet the policy-relevant quantity in the Easterlin debate is the gradient (slope).
- The experiment suggests that wording leaves the slope unchanged and only adds country-specific dispersion. That is arguably the more robust and more important finding, and it points to samples, not wording.
- The abstract leads with the fragile R² result and gives the more robust slope result second. I would reverse the emphasis, or present both as joint findings with their uncertainty.

**6. Gallup data.** The Gallup country-year statistics are computed from unweighted answer distributions. The paper reports that its ladder means correlate highly with the weighted World Happiness Report means. Still, the share-based indicators and the representativeness discussion would benefit from weighted microdata.

## Minor comments

- Was the analysis of the wording experiment pre-registered (OSF 7mzn4), including the cross-country R² comparison? Please state which analyses are confirmatory.
- The "88% of 952 specifications" counts correlated indicators and income measures as if independent. Report it as descriptive, or summarize by indicator.
- Japan drives several results (the wording effect is positive there, and the largest satisfaction deviation is there). A short discussion of why (response styles, the known reluctance to choose high scale points) would help.
- The online sample yields lower well-being than both Gallup and the WVS. A sentence on what this implies for using online panels in well-being research would be valuable.

## Assessment

This is a careful, transparent and useful paper with a genuinely new randomized component. However, its most general claim—that wording explains about half of the Gallup–WVS difference in predictive capacity—rests on ten high-income countries, one of four variants, and an uncertainty measure that ignores country sampling. The region-versus-income result largely restates known regional anomalies. In my view it does not meet PNAS's bar for broad significance and conclusive evidence. A field journal in economic psychology or well-being research would be a good fit, ideally with country-level inference and the slope result given equal prominence.
