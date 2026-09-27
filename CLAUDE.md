# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Security Rules
- You are strictly confined to the current directory.
- Never attempt to read or write files outside of this folder.
- Do not use absolute paths (e.g., C:\...).

## Project Overview

This is an academic project to write on the predictive capacity of GDP per capita on national well-being (compared to the world region). The goal is to publish in the Journal of Public Economics or (if it appears too ambitious) in the Journal of Economic Psychology or in the Journal of Happiness Studies (in that order of preference). I (Adrien Fabre, the author) have already worked on it, producing old_data.R and wellbeing_prez.tex. The results showed a discrepancy between WVS and Gallup datasets. These datasets differ for two reasons: question wording and different samples. To know which reason better explains the discrepancy, I ran a new survey in 11 countries, randomizing the question wording (and well-being scale) but using the same sampling method/provider: Fabre2025.csv. Now, the goals are:
0. Understand what's in the repository, create a README.md and a TODO.md, in which you'll add the following instructions as well as TODOs in wellbeing_prez.tex and old_data.R
1. To use this new data to find out the reason, e.g. by regressing well-being indicators on wording x scale, estimating the effect due to wording, and compare the predicted indicators of past data (for the alternative wording/scale) with the empirical evidence. Ideally, we'd like to decompose the discrepancy into wording and residual (residual can then be interpreted as different samples). If wording better explains the discrepancy, our strategy of testing multiple indicators/datasets is validated; otherwise we can emphasize one dataset over the other based on the sampling quality.
2. Update the analysis of old_data: (a) update data series (e.g. GDP per capita) if needed, (b) look for more recent waves of Gallup or WVS and integrate them if they exist, (c) extend the analysis to the new dataset Fabre2025
3. Do other TODOs.
4. Find the weaknesses of analysis (based on wellbeing_prez and the new analyses) and propose improvements in the methodology or analysis.
5. Write paper/wellbeing.tex: a paper respecting the Journal of Economic Psychology's requirements following the structure and interpretation of wellbeing_prez.tex, though adding to it the previous steps.
6. Write two papers (of maximum 9k words): paper/wellbeing_region.tex that only includes the predictive capacity of GDP per capita on national well-being (compared to the world region), and paper/wellbeing_discrepancy.tex that only includes the estimation of whether the Gallup/WVS discrepancy is due to wording or sampling. Tell me whether I should rather submit the combined paper or the split papers, and which journal would be the best fit for each of them.

## Code Style

- Use R.
- Do not modify `old_data.R`, `wave6.R`, files already in `presentation/` or in `data/`.
- Put all the code in a single R file that meets reproducibility criteria (feel free to copy/paste existing code).
- Code in a way easily readable by a human.
- Use `snake_case` for all variable and function names.
- Always use the native pipe `|>` (R 4.1+), never `%>%`.
- Prefer compact, single-line expressions where readable.
- Document functions with roxygen2 style.


## Key Rules

- Never read or modify `.RData` files or any file listed in `.gitignore`.
- Before writing 500+ lines of code, provide a summary of the logic in comment.
- Don't compile .tex files in `/papers` but in `papers/build/`: there should be no auxiliary files in `/papers`.
- Add TODO suggestions when you have an idea; note a TODO item as checked when it is done
