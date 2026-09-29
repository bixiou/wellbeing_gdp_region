# =====================================================================================================
# main.R — Reproducible pipeline for:
#   (i)  "GDP per capita is a poor predictor of national well-being" (region vs. income), and
#   (ii) "Gallup vs. WVS: wording or sampling?" (decomposition using the Fabre 2025 survey experiment).
# Author: Adrien Fabre (CNRS, CIRED). Run from the folder code_wellbeing/:  Rscript main.R
# Inputs: ../data/ (see README.md). Outputs: ../tables/main/ and ../figures/main/ (overwritten at each run).
# =====================================================================================================
#
# SUMMARY OF THE LOGIC
# 0. Setup: packages, options, seed, paths, small helpers (weighted means, LaTeX export, number macros).
# 1. Data
#    1.1 Country codes and world regions: the 5-region classification of old_data.R (UN regional groups,
#        with the Middle East and Central Asia in "Asia", Turkey in "Western"), extended by rule to all
#        countries; plus alternative classifications (6 regions of old_data.R, World Bank regions,
#        continents, UN sub-regions) for robustness.
#    1.2 GDP p.c. and population from the World Bank API (vintage of 2026-09-27, frozen in ../data/). Missing
#        values are filled automatically (replacing old manual imputations): PPP series are back-casted with
#        the growth of constant-$ GDP; countries absent from the World Bank (Taiwan, recent Venezuela...)
#        use IMF WEO data converted to constant $ with the U.S. deflator. Flags keep track of imputations.
#    1.3 WVS (waves 1-7, 1981-2022) completed with the Joint EVS/WVS 2017-2022 dataset (EVS 2017 wave and recent WVS-7 surveys):
#        country-year well-being indicators from the microdata (survey weights).
#    1.4 Gallup World Poll: country-year indicators from the ladder distributions (gallup.xlsx, where
#        wave w = year w + 2005; sub-waves of a year are pooled), and World Happiness Report (WHR) 3-year
#        ladder means up to 2023-2025.
#    1.5 Fabre (2025): individual data from 10 countries with 4 randomized question variants
#        (Gallup ladder vs. WVS satisfaction wording x 0-10 vs. 1-10 scale).
# 2. Region vs. income as predictors of national well-being (update of old_data.R)
#    2.1 For each specification x well-being indicator x income variable: R² of income alone, region alone,
#        both; Shapley/LMG share of explained variance due to income (analytical with two regressors);
#        adjusted R²; leave-one-country-out cross-validated R² (out-of-sample predictive capacity).
#    2.2 Specifications: WVS pooled, by wave, population-weighted, last observation per country, without
#        pandemic years, without GDP imputations, old GDP vintage, without Latin America & Eastern Europe,
#        alternative region classifications, Gallup (all years / latest), WHR 2023-2025.
#    2.3 Descriptives: correlation between indicators, slopes and significance, happiest countries,
#        first split of a regression tree, within-country relation, cultural variables, non-response.
#    2.4 Figures.
# 3. The Gallup/WVS discrepancy: wording or sampling?
#    3.1 Experimental effects of wording and scale in Fabre (2025): individual-level regressions with
#        country fixed effects, heterogeneity by country and by income.
#    3.2 Country-level decomposition, for the 10 countries: observed gap D = Gallup - WVS (same year)
#        = question effect Q (Fabre: ladder 0-10 minus satisfaction 1-10, same sample)
#        + residual R (sample frame, mode, context, translation, timing). Covariance decomposition across
#        countries, relation with GDP, bootstrap confidence intervals.
#    3.3 Global discrepancy: Gallup vs. WVS in all common country-years; how much of the difference in
#        the GDP gradient/R² could the (experimentally estimated) wording effect account for?
# 4. Export of numbers used in the papers as LaTeX macros (../tables/main/numbers.tex).
# =====================================================================================================


##### 0. Setup #####
packages <- c("dplyr", "tidyr", "readxl", "openxlsx", "haven", "ggplot2", "ggrepel", "sandwich", "lmtest", "fixest", "jsonlite", "countrycode", "rpart")
missing_packages <- packages[!packages %in% rownames(installed.packages())]
if (length(missing_packages)) install.packages(missing_packages)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(20250415) # Used for k-means and bootstrap
data_vintage <- "2026-09-27" # Date of the World Bank / IMF downloads used in the papers (files ../data/*_2026-09-27.json), frozen for reproducibility
refresh_downloads <- FALSE # TRUE to download a new vintage (saved with today's date, without overwriting the frozen one); results may then change
n_bootstrap <- as.numeric(Sys.getenv("N_BOOTSTRAP", 1000)) # number of bootstrap replications (env. variable N_BOOTSTRAP for quick tests)
data_folder <- "../data/"
tables_folder <- "../tables/main/"
figures_folder <- "../figures/main/"
dir.create(tables_folder, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_folder, showWarnings = FALSE, recursive = TRUE)
options(dplyr.summarise.inform = FALSE, width = 200, timeout = 900) # large downloads from the World Bank API can be slow

#' Weighted mean ignoring NA
#' @param x Numeric (or logical) vector.
#' @param w Weights (default: equal weights).
#' @return The weighted mean of the non-missing values.
wmean <- function(x, w = rep(1, length(x))) { keep <- !is.na(x) & !is.na(w); sum(x[keep] * w[keep]) / sum(w[keep]) }

#' Store a number to be exported as a LaTeX macro (e.g. \NshareRegionBetter) for the papers
#' @param name Macro name (letters only).
#' @param value Number or string.
#' @param digits Number of digits for rounding.
#' @param percent If TRUE, value is multiplied by 100 and rounded (without the % sign).
add_number <- function(name, value, digits = 2, percent = FALSE) {
  if (is.numeric(value)) value <- if (percent) format(round(100 * value, max(0, digits - 2)), nsmall = max(0, digits - 2)) else format(round(value, digits), nsmall = digits, big.mark = ",")
  numbers[[name]] <<- latex_minus(trimws(as.character(value)))
}

#' Store a p-value as a LaTeX macro including the relation sign, following the Journal of Economic Psychology style
#' (three decimals without leading zero, e.g. "= .023", or "< .001"); to be used in math mode as $p \macro$
#' @param name Macro name (letters only).
#' @param p p-value.
add_p <- function(name, p) numbers[[name]] <<- if (p < 0.001) "< .001" else paste0("= ", sub("^0", "", formatC(p, format = "f", digits = 3)))

#' Typeset a leading minus sign as a proper minus in LaTeX (works in text and math mode)
#' @param x Character vector of formatted numbers.
#' @return x with "-" at the start replaced by "\\ensuremath{-}".
latex_minus <- function(x) sub("^-", "\\\\ensuremath{-}", x)
numbers <- list()

#' Write a data frame as a booktabs LaTeX table (tabular only, to be \input in a table environment)
#' @param df Data frame (first column is used as row labels).
#' @param file File name (without folder).
#' @param digits Rounding of numeric columns.
#' @param align Column alignment string (default: l then c).
#' @param header Optional custom header line (LaTeX, without trailing \\).
#' @param midrule_before Row indices before which a \midrule is inserted.
write_latex_table <- function(df, file, digits = 2, align = NULL, header = NULL, midrule_before = c()) {
  df <- as.data.frame(df)
  for (j in seq_along(df)) if (is.numeric(df[[j]])) df[[j]] <- ifelse(is.na(df[[j]]), "", latex_minus(formatC(df[[j]], format = "f", digits = digits)))
  if (is.null(align)) align <- paste0("l", strrep("c", ncol(df) - 1))
  if (is.null(header)) header <- paste(names(df), collapse = " & ")
  rows <- apply(df, 1, function(r) paste(r, collapse = " & "))
  body <- unlist(lapply(seq_along(rows), function(i) c(if (i %in% midrule_before) "\\midrule", paste(rows[i], "\\\\"))))
  writeLines(c(paste0("\\begin{tabular}{", align, "}"), "\\toprule", paste(header, "\\\\"), "\\midrule", body, "\\bottomrule", "\\end{tabular}"), paste0(tables_folder, file))
}

#' Use a frozen download (vintage data_vintage), or download a new vintage if refresh_downloads is TRUE
#' @param url URL.
#' @param name File name without extension; the vintage date and ".json" are appended.
#' @return The local path.
cached_download <- function(url, name) {
  vintage <- if (refresh_downloads) as.character(Sys.Date()) else data_vintage
  file <- paste0(data_folder, name, "_", vintage, ".json")
  if (!file.exists(file)) {
    if (!refresh_downloads) stop("Frozen download ", file, " is missing: restore it from the repository, or set refresh_downloads <- TRUE to download a new vintage.")
    download.file(url, file, quiet = TRUE, mode = "wb")
  }
  return(file)
}


##### 1.1 Country codes and regions #####
country_mapping <- read.csv(paste0(data_folder, "country_code_mapping.csv"))
iso3_of_iso2 <- setNames(country_mapping$code, country_mapping$alpha.2)
country_of_iso3 <- setNames(country_mapping$country, country_mapping$code)

# 6-region classification of old_data.R (UN regional groups, WVS countries), and its 5-region version (main).
region6_list <- list(
  "Africa" = c("BFA", "DZA", "ETH", "GHA", "KEN", "LBY", "MAR", "MLI", "NGA", "RWA", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE", "XXS"),
  "Latin America" = c("ARG", "BOL", "BRA", "CHL", "COL", "DOM", "ECU", "GTM", "HTI", "MEX", "NIC", "PER", "PRI", "SLV", "TTO", "URY", "VEN"),
  "Ex-Eastern Block" = c("ALB", "ARM", "AZE", "BIH", "BGR", "BLR", "CZE", "EST", "GEO", "HRV", "HUN", "KAZ", "KGZ", "LTU", "LVA", "MDA", "MKD", "MNE", "POL", "ROU", "RUS", "SRB", "SVK", "SVN", "TJK", "UKR", "UZB", "XXN", "XXK", "XKX"),
  "Middle East" = c("EGY", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "PSE", "QAT", "SAU", "TUR", "YEM"),
  "Western" = c("AND", "AUS", "CAN", "CHE", "CYP", "DEU", "ESP", "FIN", "FRA", "GBR", "GRC", "ITA", "NIR", "NLD", "NOR", "NZL", "SWE", "USA", "XXY"),
  "Asia" = c("BGD", "CHN", "HKG", "IDN", "IND", "JPN", "KOR", "MAC", "MDV", "MMR", "MNG", "MYS", "PAK", "PHL", "SGP", "THA", "TWN", "VNM"))
ex_communist_europe <- c(region6_list$`Ex-Eastern Block`[!region6_list$`Ex-Eastern Block` %in% c("KAZ", "KGZ", "TJK", "UZB")])
central_asia <- c("KAZ", "KGZ", "TJK", "UZB", "TKM")

#' Main 5-region classification (Africa, Asia, Eastern Europe, Latin America, Western)
#'
#' Follows old_data.R for WVS countries (UN regional groups; Middle East and Central Asia in Asia; Egypt in
#' Africa; Turkey in Western) and extends it by rule to other countries (e.g. Gallup-only countries).
#' @param code ISO3 codes (vector).
#' @return Character vector of regions.
region5_of <- function(code) {
  continent <- suppressWarnings(countrycode(code, "iso3c", "continent", warn = FALSE))
  sapply(seq_along(code), function(i) {
    c <- code[i]; k <- continent[i]
    if (c %in% c("EGY", region6_list$Africa)) "Africa"
    else if (c %in% c(region6_list$Western, "TUR")) "Western"
    else if (c %in% ex_communist_europe) "Eastern Europe"
    else if (c %in% c(region6_list$`Latin America`)) "Latin America"
    else if (c %in% c(region6_list$Asia, region6_list$`Middle East`, central_asia)) "Asia"
    else if (is.na(k)) NA_character_
    else if (k == "Africa") "Africa"
    else if (k == "Americas") { if (c %in% c("USA", "CAN")) "Western" else "Latin America" }
    else if (k == "Europe") "Western" # remaining European countries are non ex-communist
    else if (k == "Oceania") { if (c %in% c("AUS", "NZL")) "Western" else "Asia" }
    else "Asia" })
}

#' Alternative region classifications, for robustness
#' @param code ISO3 codes.
#' @param classification One of "region6" (old_data.R, extended by rule), "wb" (World Bank regions), "continent", "un_sub" (UN sub-regions).
#' @return Character vector of regions.
region_alt_of <- function(code, classification) {
  proxy <- c("NIR" = "GBR", "XXK" = "XKX", "XXN" = "ARM", "XXS" = "SOM", "XXY" = "CYP") # territories not in countrycode
  code_proxied <- ifelse(code %in% names(proxy), proxy[code], code)
  if (classification == "region6") {
    r5 <- region5_of(code)
    middle_east <- c(region6_list$`Middle East`, "ARE", "BHR", "OMN", "SYR")
    return(ifelse(code %in% middle_east, "Middle East", ifelse(code %in% central_asia, "Ex-Eastern Block", ifelse(r5 == "Eastern Europe", "Ex-Eastern Block", r5))))
  }
  destination <- c("wb" = "region", "continent" = "continent", "un_sub" = "un.regionsub.name")[classification]
  out <- suppressWarnings(countrycode(code_proxied, "iso3c", destination, warn = FALSE))
  out[code_proxied == "TWN"] <- c("wb" = "East Asia & Pacific", "continent" = "Asia", "un_sub" = "Eastern Asia")[classification]
  out[code_proxied == "XKX"] <- c("wb" = "Europe & Central Asia", "continent" = "Europe", "un_sub" = "Southern Europe")[classification]
  return(out)
}


##### 1.2 GDP per capita and population #####
#' World Bank WDI indicator for all countries (frozen download, see cached_download)
#' @param indicator WDI code, e.g. "NY.GDP.PCAP.PP.KD".
#' @return Data frame with columns code, year, value.
get_wdi <- function(indicator) {
  file <- cached_download(paste0("https://api.worldbank.org/v2/country/all/indicator/", indicator, "?format=json&date=1960:2025&per_page=20000"), paste0("wdi_", indicator))
  raw <- fromJSON(file)[[2]]
  data.frame(code = raw$countryiso3code, year = as.integer(raw$date), value = raw$value) |> filter(code != "", !is.na(value))
}

#' IMF World Economic Outlook indicator from the IMF DataMapper API (frozen download, see cached_download)
#' @param indicator IMF code, e.g. "PPPPC" (GDP p.c. PPP, current international $) or "NGDPDPC" (GDP p.c., current $).
#' @return Data frame with columns code, year, value.
get_imf <- function(indicator) {
  file <- cached_download(paste0("https://www.imf.org/external/datamapper/api/v1/", indicator), paste0("imf_", indicator))
  raw <- fromJSON(file)$values[[indicator]]
  bind_rows(lapply(names(raw), function(c) data.frame(code = c, year = as.integer(names(raw[[c]])), value = as.numeric(unlist(raw[[c]]))))) |> filter(!is.na(value), year <= 2025)
}

#' Fill a WDI series: back-/forward-cast with the growth of a reference series, then IMF converted with the U.S. deflator, then closest year
#' @param main Data frame (code, year, value) of the target series (e.g. GDP p.c. PPP constant 2021 $).
#' @param growth Data frame of a series used for its growth rates (e.g. GDP p.c. constant 2015 $).
#' @param imf Data frame of the IMF series in current $ (converted to the constant $ of `main` via the U.S. ratio).
#' @param grid Data frame (code, year) of the observations needed.
#' @return grid with columns value, source ("wb", "growth", "imf", "nearest" or NA).
fill_series <- function(main, growth, imf, grid) {
  out <- grid |> left_join(main, by = c("code", "year")) |> mutate(source = ifelse(is.na(value), NA, "wb"))
  for (i in which(is.na(out$value))) { # (1) growth of reference series from the closest year with both series
    c <- out$code[i]; y <- out$year[i]
    anchors <- main$year[main$code == c]
    anchors <- anchors[anchors %in% growth$year[growth$code == c]]
    g_y <- growth$value[growth$code == c & growth$year == y]
    if (length(anchors) && length(g_y)) {
      a <- anchors[which.min(abs(anchors - y))]
      out$value[i] <- main$value[main$code == c & main$year == a] * g_y / growth$value[growth$code == c & growth$year == a]
      out$source[i] <- "growth"
  } }
  us_main <- fill_series_us(main, growth)
  for (i in which(is.na(out$value))) { # (2) IMF current $ converted to constant $ with the U.S. ratio main/IMF
    c <- out$code[i]; y <- out$year[i]
    v <- imf$value[imf$code == c & imf$year == y]; us_imf <- imf$value[imf$code == "USA" & imf$year == y]; us <- us_main$value[us_main$year == y]
    if (length(v) && length(us_imf) && length(us)) { out$value[i] <- v * us / us_imf; out$source[i] <- "imf" }
  }
  for (i in which(is.na(out$value))) { # (3) closest year available (at most 3 years apart), e.g. Montenegro 1996
    c <- out$code[i]; y <- out$year[i]; available <- main[main$code == c & abs(main$year - y) <= 3, ]
    if (nrow(available)) { out$value[i] <- available$value[which.min(abs(available$year - y))]; out$source[i] <- "nearest" }
  }
  return(out)
}

#' U.S. series of `main`, back-casted with the growth of `growth` (used to convert IMF current $ into constant $)
#' @inheritParams fill_series
#' @return Data frame (year, value) for the U.S.
fill_series_us <- function(main, growth) {
  us <- main |> filter(code == "USA"); first <- min(us$year)
  g <- growth |> filter(code == "USA")
  back <- g |> filter(year < first) |> mutate(value = us$value[us$year == first] * value / g$value[g$year == first])
  bind_rows(back, us) |> select(year, value)
}

gdp_ppp_wb <- get_wdi("NY.GDP.PCAP.PP.KD") # GDP p.c. PPP, constant 2021 international $ (1990-2025)
gdp_wb <- get_wdi("NY.GDP.PCAP.KD") # GDP p.c., constant 2015 US$ (1960-2025)
pop_wb <- get_wdi("SP.POP.TOTL")
imf_ppp <- get_imf("PPPPC") # GDP p.c. PPP, current international $
imf_gdp <- get_imf("NGDPDPC") # GDP p.c., current US$
imf_pop <- get_imf("LP") |> mutate(value = value * 1e6) # Population (millions)
gdp_proxy <- c("NIR" = "GBR", "XXK" = "XKX", "XXN" = "ARM", "XXS" = "SOM", "XXY" = "CYP") # Territories: GDP of the closest entity

#' Attach GDP p.c. (PPP and nominal), imputation flags, growth and population to country-year data
#' @param df Data frame with columns code and year.
#' @return df with gdp_ppp, gdp (nominal), gdp_ppp_na / gdp_na (NA when not original WB data), gdp_ppp17 (old vintage), growth, pop.
add_gdp <- function(df) {
  keys <- df |> distinct(code, year) |> mutate(code_gdp = ifelse(code %in% names(gdp_proxy), gdp_proxy[code], code))
  grid <- keys |> transmute(code = code_gdp, year) |> distinct()
  grid_growth <- grid |> mutate(year = year - 5) |> bind_rows(grid) |> distinct()
  ppp <- fill_series(gdp_ppp_wb, gdp_wb, imf_ppp, grid) |> rename(gdp_ppp = value, source_ppp = source)
  nominal <- fill_series(gdp_wb, gdp_ppp_wb, imf_gdp, grid_growth) |> rename(gdp = value, source_nominal = source)
  nominal_lag <- nominal |> transmute(code, year = year + 5, gdp_lag5 = gdp)
  pop <- grid |> left_join(pop_wb, by = c("code", "year")) |> left_join(imf_pop |> rename(value_imf = value), by = c("code", "year")) |> transmute(code, year, pop = ifelse(is.na(value), value_imf, value))
  gdp <- keys |> left_join(ppp, by = c("code_gdp" = "code", "year")) |> left_join(nominal, by = c("code_gdp" = "code", "year")) |>
    left_join(nominal_lag, by = c("code_gdp" = "code", "year")) |> left_join(pop, by = c("code_gdp" = "code", "year")) |>
    mutate(gdp_ppp_na = ifelse(source_ppp == "wb" & code == code_gdp, gdp_ppp, NA), gdp_na = ifelse(source_nominal == "wb" & code == code_gdp, gdp, NA),
           growth = (log(gdp) - log(gdp_lag5)) / 5, pop = ifelse(code == "NIR", 1.9e6, pop)) |> # NIR: ~1.9 million inhabitants (ONS)
    select(-code_gdp, -gdp_lag5)
  old <- read.csv(paste0(data_folder, "GDPpcPPP17.csv"), fileEncoding = "UTF-8-BOM") |> pivot_longer(matches("^X[0-9]{4}$"), names_to = "year", values_to = "gdp_ppp17") |>
    transmute(code = Country.Code, year = as.integer(sub("X", "", year)), gdp_ppp17 = as.numeric(gdp_ppp17))
  df |> left_join(gdp, by = c("code", "year")) |> left_join(old, by = c("code", "year"))
}


##### 1.3 WVS and EVS (Integrated Values Surveys) #####
mode_labels_trend <- c("1" = "CAPI", "2" = "PAPI", "3" = "Web", "4" = "Mail", "5" = "Phone", "6" = "Face-to-face", "7" = "Web") # WVS time-series coding
mode_labels_joint <- c("1" = "CAPI", "2" = "PAPI", "3" = "Web", "4" = "Mail", "5" = "Phone", "6" = "Web") # Joint EVS/WVS coding
wvs_trend <- readRDS(paste0(data_folder, "WVS.rds")) |>
  transmute(wave = as.integer(s002), year = as.integer(s020), code = unname(iso3_of_iso2[as.character(s009)]), weight = as.numeric(s018),
            happiness = as.numeric(a008), satisfaction = as.numeric(a170), god = as.numeric(f063), homosexuality = as.numeric(f118),
            freedom = as.numeric(a173), democracy = as.numeric(e235), study = "WVS", mode = unname(mode_labels_trend[as.character(as.numeric(mode))])) # as.numeric() drops the haven labels
# Joint EVS/WVS 2017-2022 (ZA7505 v5.0.0, 2024-06-24): adds the EVS 2017 wave (EVS 5) and WVS-7 surveys absent from the time series v3.0.
# Same question codes (A008, A170...); a country-year already in the WVS time series is kept from the time series only.
joint <- haven::read_dta(paste0(data_folder, "EVS-WVS_ZA7505_v5-0-0.dta"), col_select = c(study, cntry_AN, year, mode, gwght, A008, A170, A173, E235, F063, F118)) |>
  transmute(wave = 7L, year = as.integer(year), code = unname(iso3_of_iso2[as.character(cntry_AN)]), weight = as.numeric(gwght),
            happiness = as.numeric(A008), satisfaction = as.numeric(A170), god = as.numeric(F063), homosexuality = as.numeric(F118),
            freedom = as.numeric(A173), democracy = as.numeric(E235), study = as.character(haven::as_factor(study)), mode = unname(mode_labels_joint[as.character(as.numeric(mode))]))
joint_new <- joint |> anti_join(wvs_trend |> distinct(code, year), by = c("code", "year"))
wvs <- bind_rows(wvs_trend, joint_new)
for (v in c("happiness", "satisfaction", "god", "homosexuality", "freedom", "democracy")) wvs[[v]][wvs[[v]] < 0] <- NA # negative codes: DK, refusal, not asked
add_number("nEVSSurveys", nrow(joint_new |> filter(study == "EVS") |> distinct(code, year)), 0)
add_number("nNewWVSSurveys", nrow(joint_new |> filter(study == "WVS") |> distinct(code, year)), 0)

wellbeing_variables <- c("happy", "very_happy", "very_unhappy", "very_happy_minus_very_unhappy", "happiness_mean", "satisfied_mean", "satisfied", "happiness_layard")
satisfaction_variables <- c("satisfied", "very_satisfied", "extremely_satisfied", "completely_satisfied", "unsatisfied", "dissatisfied", "satisfied_mean")
wellbeing_names <- c("happy" = "Happy", "very_happy" = "Very Happy", "very_unhappy" = "Very Unhappy", "very_happy_minus_very_unhappy" = "V. Happy -- V. Unhappy",
                     "happiness_mean" = "Happiness (mean)", "satisfied_mean" = "Satisfaction (mean)", "satisfied" = "Satisfied", "happiness_layard" = "Happy + Satisfied",
                     "very_satisfied" = "Very satisfied (8--10)", "extremely_satisfied" = "Extremely satisfied (9--10)", "completely_satisfied" = "Completely satisfied (10)",
                     "unsatisfied" = "Unsatisfied (0/1--4)", "dissatisfied" = "Dissatisfied (0/1--2)", "low_satisfaction" = "Below 60\\% of mean satisfaction")

wvs_cy <- wvs |> group_by(code, year, wave) |> summarise(
  n = n(), study = first(study),
  happy = wmean(happiness <= 2, weight), very_happy = wmean(happiness == 1, weight), very_unhappy = wmean(happiness == 4, weight),
  happiness_mean = wmean(3 - 2 * (happiness - 1), weight), # 1->3, 2->1, 3->-1, 4->-3
  satisfied_mean = wmean(satisfaction, weight), satisfied = wmean(satisfaction >= 6, weight),
  very_satisfied = wmean(satisfaction >= 8, weight), extremely_satisfied = wmean(satisfaction >= 9, weight), completely_satisfied = wmean(satisfaction == 10, weight),
  unsatisfied = wmean(satisfaction <= 4, weight), dissatisfied = wmean(satisfaction <= 2, weight),
  low_satisfaction = wmean(satisfaction < 0.6 * wmean(satisfaction, weight), weight),
  nonresponse_happiness = mean(is.na(happiness)), nonresponse_satisfaction = mean(is.na(satisfaction)),
  god = wmean(god, weight), homosexuality = wmean(homosexuality, weight), freedom = wmean(freedom, weight), democracy = wmean(democracy, weight)) |>
  ungroup() |>
  mutate(very_happy_minus_very_unhappy = very_happy - very_unhappy, # NB: old_data.R used weighted counts instead of shares
         happiness_layard = (happy + satisfied) / 2, source = study, non_pandemic = !year %in% 2020:2021) |>
  mutate(across(c(god, homosexuality, freedom, democracy), ~ ifelse(is.nan(.x), NA, .x)))
wvs_cy <- add_gdp(wvs_cy) |> mutate(region = region5_of(code), country = country_of_iso3[code])
wvs_cy <- wvs_cy |> group_by(code) |> mutate(last_year = max(year)) |> ungroup()

# Same question, different samples: countries surveyed by both the EVS and the WVS in 2017-2023 (closest surveys)
evs_vs_wvs <- inner_join(wvs_cy |> filter(source == "EVS") |> select(code, year_evs = year, evs = satisfied_mean),
                         wvs_cy |> filter(source == "WVS", wave == 7) |> select(code, year_wvs = year, wvs = satisfied_mean), by = "code") |>
  mutate(difference = wvs - evs, years_apart = abs(year_wvs - year_evs))
print(evs_vs_wvs)
add_number("nEVSvsWVS", nrow(evs_vs_wvs), 0)
add_number("meanAbsDiffEVSWVS", mean(abs(evs_vs_wvs$difference)), 2)
add_number("maxAbsDiffEVSWVS", max(abs(evs_vs_wvs$difference)), 2)


##### 1.4 Gallup #####
gallup_raw <- read_excel(paste0(data_folder, "gallup.xlsx"), col_names = FALSE, skip = 10, .name_repair = "minimal")
names(gallup_raw) <- c("wave", "na", "country", paste0("s", 0:10), "s_dk", "s_refused", "n_total")
gallup_raw <- gallup_raw[seq_len(which(gallup_raw$wave == "Total")[1] - 1), ] |> # drop the final "all waves" block
  fill(wave) |> filter(!is.na(country)) |> mutate(across(c(wave, starts_with("s"), n_total), ~ as.numeric(.x))) |>
  mutate(across(starts_with("s"), ~ ifelse(is.na(.x), 0, .x)), year = floor(wave) + 2005) # Wave 1 = 2005/06 (cf. TODO.md: old_data.R used + 2000)
gallup_custom_codes <- c("Kosovo" = "XKX", "Somaliland region" = "XXS", "Northern Cyprus" = "XXY", "Nagorno-Karabakh Region" = "XXN", "Taiwan Province of China" = "TWN")
gallup_raw$code <- ifelse(gallup_raw$country %in% names(gallup_custom_codes), gallup_custom_codes[gallup_raw$country], suppressWarnings(countrycode(gallup_raw$country, "country.name", "iso3c", warn = FALSE)))
if (any(is.na(gallup_raw$code))) warning("Unmatched Gallup countries: ", paste(unique(gallup_raw$country[is.na(gallup_raw$code)]), collapse = ", "))

#' Satisfaction indicators from a distribution of answers (counts or weights) on a 0-10 or 1-10 scale
#' @param counts Matrix with one row per unit and one column per answer.
#' @param values Answer values of the columns (0:10 or 1:10).
#' @return Data frame of indicators (shares and mean).
satisfaction_indicators <- function(counts, values) {
  share <- function(sel) rowSums(counts[, values %in% sel, drop = FALSE]) / rowSums(counts)
  data.frame(satisfied_mean = as.vector(counts %*% values) / rowSums(counts), satisfied = share(6:10), very_satisfied = share(8:10),
             extremely_satisfied = share(9:10), completely_satisfied = share(10), unsatisfied = share(0:4), dissatisfied = share(0:2))
}
gallup_counts <- gallup_raw |> group_by(code, year) |> summarise(across(c(paste0("s", 0:10), s_dk, s_refused), sum)) |> ungroup()
gallup_cy <- bind_cols(gallup_counts |> select(code, year), satisfaction_indicators(as.matrix(gallup_counts[, paste0("s", 0:10)]), 0:10)) |>
  mutate(n = rowSums(gallup_counts[, c(paste0("s", 0:10), "s_dk", "s_refused")]), nonresponse_satisfaction = (gallup_counts$s_dk + gallup_counts$s_refused) / n,
         wave = year, source = "Gallup", non_pandemic = !year %in% 2020:2021) |>
  add_gdp() |> mutate(region = region5_of(code), country = country_of_iso3[code])
gallup_cy <- gallup_cy |> group_by(code) |> mutate(last_year = max(year)) |> ungroup()

# World Happiness Report: 3-year average of the ladder (year t = average over t-2..t), Gallup weights.
whr <- read_excel(paste0(data_folder, "WHR26_Data_Figure_2.1.xlsx")) |> transmute(year = Year, country = `Country name`, satisfied_mean = `Life evaluation (3-year average)`) |> filter(!is.na(satisfied_mean))
whr_custom_codes <- c("Kosovo" = "XKX", "Somaliland region" = "XXS", "Taiwan Province of China" = "TWN", "Hong Kong SAR of China" = "HKG", "Somaliland Region" = "XXS", "State of Palestine" = "PSE", "Türkiye" = "TUR")
whr$code <- ifelse(whr$country %in% names(whr_custom_codes), whr_custom_codes[whr$country], suppressWarnings(countrycode(whr$country, "country.name", "iso3c", warn = FALSE)))
if (any(is.na(whr$code))) warning("Unmatched WHR countries: ", paste(unique(whr$country[is.na(whr$code)]), collapse = ", "))
whr <- whr |> filter(!is.na(code)) |> mutate(wave = year, source = "WHR") |> select(-country) |> add_gdp() |> mutate(region = region5_of(code), country = country_of_iso3[code])

# Check: 3-year averages of gallup.xlsx (unweighted counts) vs. WHR (weighted), to validate the wave -> year mapping and gauge the weighting.
gallup_3y <- gallup_cy |> arrange(code, year) |> group_by(code) |> mutate(avg3 = (satisfied_mean + lag(satisfied_mean) + lag(satisfied_mean, 2)) / 3, consecutive = year - lag(year, 2) == 2) |> ungroup() |> filter(consecutive)
check_mapping <- sapply(-5:2, function(shift) { m <- inner_join(gallup_3y |> mutate(year = year + shift), whr, by = c("code", "year")); cor(m$avg3, m$satisfied_mean.y) })
names(check_mapping) <- paste0("shift_", -5:2)
print(round(check_mapping, 3)) # max at shift 0 validates year = wave + 2005
add_number("corGallupWHR", check_mapping["shift_0"], 3)
add_number("corGallupWHRoldMapping", check_mapping["shift_-5"], 2) # correlation with the mapping of old_data.R (year = wave + 2000)
add_number("nGallupCountryYears", nrow(gallup_cy), 0)
add_number("nGallupCountries", length(unique(gallup_cy$code)), 0)
add_number("nWVSCountryYears", nrow(wvs_cy), 0)
add_number("nWVSCountries", length(unique(wvs_cy$code)), 0)
add_number("nWVSRespondents", nrow(wvs), 0)


##### 1.5 Fabre (2025) #####
fabre <- read.csv(paste0(data_folder, "Fabre2025.csv")) |>
  transmute(code = countrycode(country, "iso2c", "iso3c"), variant = variant_well_being, wording = variant_well_being_wording,
            ladder = as.numeric(wording == "Gallup"), scale_1_10 = as.numeric(variant_well_being_scale == "1"), well_being,
            well_being_0_10 = ifelse(scale_1_10 == 1, (well_being - 1) * 10 / 9, well_being), # linear stretch of 1-10 answers to 0-10
            weight_pooled = weight, income_quartile = as.numeric(sub("Q", "", income_quartile)), income_decile = as.numeric(sub("d", "", income_decile)),
            man, age = age_factor, education, urbanity) |>
  group_by(code) |> mutate(weight = weight_pooled / mean(weight_pooled)) |> ungroup() # weights normalized to mean 1 within country
fabre_countries <- sort(unique(fabre$code))
add_number("nFabre", nrow(fabre), 0)
add_number("nFabreCountries", length(fabre_countries), 0)


##### 2.1 Region vs. income: estimation functions #####
income_variables <- c("log_gdp_ppp", "log_gdp", "group_gdp_ppp", "cluster5_gdp_ppp", "cluster6_gdp_ppp", "cluster7_gdp_ppp", "cluster7_gdp")
income_names <- c("log_gdp_ppp" = "log GDP p.c. PPP", "log_gdp" = "log GDP p.c. nominal", "group_gdp_ppp" = "Sextile PPP", "cluster5_gdp_ppp" = "Cluster k=5 PPP",
                  "cluster6_gdp_ppp" = "Cluster k=6 PPP", "cluster7_gdp_ppp" = "Cluster k=7 PPP", "cluster7_gdp" = "Cluster k=7 nominal")

#' One-dimensional k-means clusters, labelled by increasing center (deterministic: fixed seed, 50 starts)
#' @param x Numeric vector (no NA).
#' @param k Number of clusters.
#' @return Character vector of cluster labels ("1" = poorest cluster).
kmeans_sorted <- function(x, k) {
  set.seed(k)
  fit <- kmeans(x, centers = k, nstart = 50)
  as.character(rank(fit$centers)[fit$cluster])
}

#' Create income variables within a sample: log GDP, sextiles and k-means clusters of log GDP (PPP and nominal)
#' @param df Country-year data frame with gdp_ppp and gdp.
#' @return df with log_*, group_* and cluster*_* variables.
make_income_vars <- function(df) {
  for (v in c("gdp_ppp", "gdp")) {
    ok <- !is.na(df[[v]])
    df[[paste0("log_", v)]] <- log10(df[[v]])
    df[[paste0("group_", v)]] <- NA_character_
    df[[paste0("group_", v)]][ok] <- as.character(cut(rank(df[[v]][ok]), breaks = 6, labels = FALSE))
    for (k in 5:7) { df[[paste0("cluster", k, "_", v)]] <- NA_character_; df[[paste0("cluster", k, "_", v)]][ok] <- kmeans_sorted(log10(df[[v]][ok]), k) }
  }
  return(df)
}

#' Weighted R² of a linear model defined by its design matrix
#' @param y Outcome. @param X Design matrix. @param w Weights.
#' @return R² (weighted).
r2_of <- function(y, X, w) { fit <- lm.wfit(X, y, w); 1 - sum(w * fit$residuals^2) / sum(w * (y - wmean(y, w))^2) }

#' Leave-one-country-out cross-validated R²
#'
#' For each country, the model is fitted on the other countries and used to predict all observations of that
#' country. When a category (e.g. an income cluster) is absent from the training sample, the prediction is the
#' training mean. CV R² = 1 - SSE_out-of-sample / SST.
#' @param y Outcome. @param X Design matrix. @param w Weights. @param country Country codes.
#' @return Cross-validated R².
loco_r2 <- function(y, X, w, country) {
  prediction <- rep(NA_real_, length(y))
  for (c in unique(country)) {
    test <- country == c; train <- !test
    fit <- lm.wfit(X[train, , drop = FALSE], y[train], w[train])
    beta <- fit$coefficients; beta[is.na(beta)] <- 0
    unseen <- colSums(abs(X[train, , drop = FALSE])) == 0
    prediction[test] <- X[test, , drop = FALSE] %*% beta
    prediction[test & rowSums(abs(X[, unseen, drop = FALSE])) > 0] <- wmean(y[train], w[train])
  }
  1 - sum(w * (y - prediction)^2) / sum(w * (y - wmean(y, w))^2)
}

#' Variance explained by income vs. region for one outcome and one income variable
#'
#' Fits y ~ income, y ~ region and y ~ income + region. The LMG (Shapley) share of the joint R² attributed to
#' income is [R²_income + (R²_both - R²_region)] / 2, divided by R²_both (Lindeman, Merenda & Gold 1980).
#' @param df Data frame. @param y Outcome name. @param income Income variable name. @param region Region variable name.
#' @param weights Optional name of the weight variable. @param cv If TRUE, computes leave-one-country-out R².
#' @return One-row data frame of statistics.
income_vs_region <- function(df, y, income, region = "region", weights = NULL, cv = TRUE) {
  d <- df[complete.cases(df[, c(y, income, region)]), ]
  w <- if (is.null(weights)) rep(1, nrow(d)) else d[[weights]] / mean(d[[weights]])
  X_income <- model.matrix(as.formula(paste("~", income)), d)
  X_region <- model.matrix(as.formula(paste("~ factor(", region, ")")), d)
  X_both <- cbind(X_income, X_region[, -1, drop = FALSE])
  r2 <- sapply(list(X_income, X_region, X_both), function(X) r2_of(d[[y]], X, w))
  p <- c(ncol(X_income), ncol(X_region), ncol(X_both)) - 1; n <- nrow(d)
  adj <- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  cv_r2 <- if (cv) sapply(list(X_income, X_region, X_both), function(X) loco_r2(d[[y]], X, w, d$code)) else rep(NA, 3)
  lmg_income <- (r2[1] + r2[3] - r2[2]) / 2
  lmg_income_adj <- (adj[1] + adj[3] - adj[2]) / 2
  data.frame(indicator = y, income = income, region_var = region, n = n, n_countries = length(unique(d$code)),
             r2_income = r2[1], r2_region = r2[2], r2_both = r2[3], adj_r2_income = adj[1], adj_r2_region = adj[2], adj_r2_both = adj[3],
             cv_r2_income = cv_r2[1], cv_r2_region = cv_r2[2], cv_r2_both = cv_r2[3],
             lmg_income = lmg_income, lmg_region = r2[3] - lmg_income, share_income = lmg_income / r2[3], share_income_adj = lmg_income_adj / adj[3])
}

#' Run income_vs_region for all indicators x income variables of a specification
#' @param spec List with elements data, indicators, and optionally weights, region, cv, name.
#' @return Data frame of results.
run_spec <- function(spec) {
  df <- make_income_vars(spec$data)
  region <- if (is.null(spec$region)) "region" else spec$region
  grid <- expand.grid(indicator = spec$indicators, income = income_variables, stringsAsFactors = FALSE)
  out <- bind_rows(lapply(seq_len(nrow(grid)), function(i) income_vs_region(df, grid$indicator[i], grid$income[i], region, spec$weights, cv = !isFALSE(spec$cv))))
  out$spec <- spec$name
  return(out)
}


##### 2.2 Region vs. income: specifications #####
wvs_indicators <- c(wellbeing_variables, "low_satisfaction")
wvs_main <- wvs_cy |> filter(!is.na(gdp_ppp), !is.na(gdp))
add_region_alternatives <- function(df) df |> mutate(region6 = region_alt_of(code, "region6"), region_wb = region_alt_of(code, "wb"), region_continent = region_alt_of(code, "continent"), region_un_sub = region_alt_of(code, "un_sub"))
wvs_main <- add_region_alternatives(wvs_main)
gallup_main <- gallup_cy |> filter(!is.na(gdp_ppp), !is.na(gdp), !is.na(region)) |> add_region_alternatives()
whr_main <- whr |> filter(!is.na(gdp_ppp), !is.na(gdp), !is.na(region))

specs <- list(
  list(name = "wvs", data = wvs_main, indicators = wvs_indicators),
  list(name = "wvs_waves12", data = wvs_main |> filter(wave %in% 1:2), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_wave3", data = wvs_main |> filter(wave == 3), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_wave4", data = wvs_main |> filter(wave == 4), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_wave5", data = wvs_main |> filter(wave == 5), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_wave6", data = wvs_main |> filter(wave == 6), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_wave7", data = wvs_main |> filter(wave == 7), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_wave7_no_pandemic", data = wvs_main |> filter(wave == 7, non_pandemic), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_weighted", data = wvs_main, indicators = wellbeing_variables, weights = "pop", cv = FALSE),
  list(name = "wvs_last", data = wvs_main |> filter(year == last_year), indicators = wellbeing_variables),
  list(name = "wvs_no_imputation", data = wvs_main |> mutate(gdp_ppp = gdp_ppp_na, gdp = gdp_na) |> filter(!is.na(gdp_ppp), !is.na(gdp)), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_gdp_2017_vintage", data = wvs_main |> mutate(gdp_ppp = gdp_ppp17), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_only", data = wvs_main |> filter(source == "WVS"), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_no_latam_ee", data = wvs_main |> filter(!region %in% c("Latin America", "Eastern Europe")), indicators = wellbeing_variables, cv = FALSE),
  list(name = "wvs_region6", data = wvs_main, indicators = wellbeing_variables, region = "region6", cv = FALSE),
  list(name = "wvs_region_wb", data = wvs_main, indicators = wellbeing_variables, region = "region_wb", cv = FALSE),
  list(name = "wvs_region_continent", data = wvs_main, indicators = wellbeing_variables, region = "region_continent", cv = FALSE),
  list(name = "wvs_region_un_sub", data = wvs_main, indicators = wellbeing_variables, region = "region_un_sub", cv = FALSE),
  list(name = "gallup", data = gallup_main, indicators = satisfaction_variables),
  list(name = "gallup_last", data = gallup_main |> filter(year == last_year), indicators = satisfaction_variables),
  list(name = "gallup_wvs_countries", data = gallup_main |> filter(year == last_year, code %in% wvs_main$code), indicators = satisfaction_variables, cv = FALSE),
  list(name = "gallup_region6", data = gallup_main |> filter(year == last_year), indicators = satisfaction_variables, region = "region6", cv = FALSE),
  list(name = "gallup_region_wb", data = gallup_main |> filter(year == last_year), indicators = satisfaction_variables, region = "region_wb", cv = FALSE),
  list(name = "whr_2025", data = whr_main |> filter(year == 2025), indicators = "satisfied_mean"),
  list(name = "wvs_satisfaction", data = wvs_main, indicators = satisfaction_variables, cv = FALSE))
spec_names <- c("wvs" = "WVS, all waves", "wvs_waves12" = "WVS, waves 1--2 (1981--91)", "wvs_wave3" = "WVS, wave 3 (1995--99)", "wvs_wave4" = "WVS, wave 4 (1999--2004)",
                "wvs_wave5" = "WVS, wave 5 (2004--09)", "wvs_wave6" = "WVS, wave 6 (2010--16)", "wvs_wave7" = "WVS, wave 7 (2017--23)", "wvs_wave7_no_pandemic" = "WVS, wave 7 w/o 2020--21",
                "wvs_weighted" = "WVS, population-weighted", "wvs_last" = "WVS, last obs. per country", "wvs_no_imputation" = "WVS, no GDP imputation",
                "wvs_gdp_2017_vintage" = "WVS, GDP PPP 2017 \\$ (old)", "wvs_only" = "WVS, without EVS surveys", "wvs_no_latam_ee" = "WVS, w/o Latin Am. \\& E. Europe", "wvs_region6" = "WVS, 6 regions",
                "wvs_region_wb" = "WVS, World Bank regions (7)", "wvs_region_continent" = "WVS, continents (5)", "wvs_region_un_sub" = "WVS, UN sub-regions",
                "gallup" = "Gallup, all years (2006--23)", "gallup_last" = "Gallup, last obs. per country", "gallup_wvs_countries" = "Gallup, last obs., WVS countries",
                "gallup_region6" = "Gallup, last obs., 6 regions", "gallup_region_wb" = "Gallup, last obs., WB regions", "whr_2025" = "WHR, 2023--25 average", "wvs_satisfaction" = "WVS, satisfaction indicators")

results <- bind_rows(lapply(specs, run_spec))
write.csv(results, paste0(tables_folder, "region_vs_income_all_results.csv"), row.names = FALSE)

# Main tables: WVS pooled (R² of income alone; share of explained variance due to income), and robustness summary.
main_results <- results |> filter(spec == "wvs", indicator %in% wellbeing_variables)
best_income <- main_results |> group_by(income) |> summarise(r2 = mean(r2_income)) |> slice_max(r2) |> pull(income)

#' Table indicators x income variables of a statistic, with a Region column, mean and max rows
#' @param res Results (one spec). @param stat Statistic column (e.g. "r2_income"). @param region_stat Optional statistic for the Region column.
#' @return Data frame ready for write_latex_table.
table_indicator_income <- function(res, stat, region_stat = NULL, indicators = wellbeing_variables) {
  wide <- res |> select(indicator, income, value = all_of(stat)) |> pivot_wider(names_from = income, values_from = value)
  wide <- wide[match(indicators, wide$indicator), c("indicator", income_variables)]
  if (!is.null(region_stat)) wide$region <- res[[region_stat]][match(wide$indicator, res$indicator)]
  numeric <- wide[, -1]
  out <- rbind(wide, data.frame(indicator = "Mean", t(colMeans(numeric))), data.frame(indicator = "Max", t(apply(numeric, 2, max))))
  out$indicator <- ifelse(out$indicator %in% names(wellbeing_names), wellbeing_names[out$indicator], out$indicator)
  return(out)
}
income_header <- "Well-being indicator & \\multicolumn{2}{c}{log GDP p.c.} & Sextile & \\multicolumn{4}{c}{Income cluster} \\\\ & PPP & nominal & PPP & $k$=5 PPP & $k$=6 PPP & $k$=7 PPP & $k$=7 nominal"
tab_r2 <- table_indicator_income(main_results, "r2_income", "r2_region")
write_latex_table(tab_r2, "r2_income.tex", header = paste(income_header, "& Region"), midrule_before = length(wellbeing_variables) + 1, align = "lcccccccc")
tab_share <- table_indicator_income(main_results, "share_income")
write_latex_table(tab_share, "share_income.tex", header = income_header, midrule_before = length(wellbeing_variables) + 1)
gallup_last_results <- results |> filter(spec == "gallup_last")
write_latex_table(table_indicator_income(gallup_last_results, "share_income", indicators = satisfaction_variables), "share_income_gallup.tex", header = income_header, midrule_before = length(satisfaction_variables) + 1)
write_latex_table(table_indicator_income(gallup_last_results, "r2_income", "r2_region", indicators = satisfaction_variables), "r2_income_gallup.tex", header = paste(income_header, "& Region"), midrule_before = length(satisfaction_variables) + 1, align = "lcccccccc")

robustness <- results |> filter(spec != "wvs_satisfaction", indicator != "low_satisfaction") |> group_by(spec) |>
  summarise(n = max(n), n_countries = max(n_countries), r2_income_ppp = mean(r2_income[income == "log_gdp_ppp"]), r2_income_best = mean(r2_income[income == best_income]),
            r2_region = mean(r2_region[income == "log_gdp_ppp"]), share_ppp = mean(share_income[income == "log_gdp_ppp"]), share_best = mean(share_income[income == best_income]),
            share_adj = mean(share_income_adj), region_better = mean(share_income < 0.5)) |>
  mutate(order = match(spec, names(spec_names))) |> arrange(order) |> select(-order)
write.csv(robustness, paste0(tables_folder, "robustness.csv"), row.names = FALSE)
write_latex_table(robustness |> mutate(spec = spec_names[spec], n = as.character(n), n_countries = as.character(n_countries)), "robustness.tex",
                  header = "Specification & Obs. & Countries & \\multicolumn{2}{c}{$R^2$ income} & $R^2$ region & \\multicolumn{3}{c}{Share of explained variance due to income} & Region better \\\\ & & & log PPP & best & & log PPP & best & adj. $R^2$ & (share of cases)",
                  midrule_before = which(grepl("^gallup", robustness$spec))[1], align = "lccccccccc")

# Out-of-sample predictive capacity (leave-one-country-out R²).
cv_table <- results |> filter(spec %in% c("wvs", "wvs_last", "gallup", "gallup_last", "whr_2025"), income %in% c("log_gdp_ppp", best_income),
                              indicator %in% c(wellbeing_variables, "satisfied_mean", "satisfied", "unsatisfied")) |>
  filter(!(spec %in% c("gallup", "gallup_last") & !indicator %in% c("satisfied_mean", "satisfied", "unsatisfied"))) |>
  group_by(spec, indicator) |> summarise(n = first(n), cv_income_ppp = cv_r2_income[income == "log_gdp_ppp"], cv_income_best = cv_r2_income[income == best_income],
                                         cv_region = first(cv_r2_region), cv_both = cv_r2_both[income == "log_gdp_ppp"]) |> ungroup() |>
  mutate(order_spec = match(spec, c("wvs", "wvs_last", "gallup", "gallup_last", "whr_2025")), order_ind = match(indicator, c(wellbeing_variables, "unsatisfied"))) |> arrange(order_spec, order_ind)
write.csv(cv_table, paste0(tables_folder, "cross_validated_r2.csv"), row.names = FALSE)
write_latex_table(cv_table |> transmute(data = spec_names[spec], indicator = wellbeing_names[indicator], n = as.character(n), cv_income_ppp, cv_income_best, cv_region, cv_both), "cross_validated_r2.tex",
                  header = "Data & Indicator & Obs. & log GDP PPP & Best income variable & Region & log GDP PPP + Region", align = "llccccc",
                  midrule_before = which(!duplicated(cv_table$spec))[-1])

# Numbers for the text
wvs_like <- results |> filter(grepl("^wvs", spec), spec != "wvs_satisfaction", indicator != "low_satisfaction")
add_number("bestIncome", income_names[best_income])
add_number("shareRegionBetterWVS", mean(wvs_like$share_income < 0.5), 2, percent = TRUE)
add_number("shareRegionBetterWVSbest", mean(wvs_like$share_income[wvs_like$income == best_income] < 0.5), 2, percent = TRUE)
add_number("nSpecsWVS", nrow(wvs_like), 0)
add_number("meanShareIncomeBest", mean(main_results$share_income[main_results$income == best_income]), 2, percent = TRUE)
add_number("meanRtwoIncomeBest", mean(main_results$r2_income[main_results$income == best_income]), 2, percent = TRUE)
add_number("meanRtwoIncomePPP", mean(main_results$r2_income[main_results$income == "log_gdp_ppp"]), 2, percent = TRUE)
add_number("meanRtwoRegion", mean(main_results$r2_region[main_results$income == "log_gdp_ppp"]), 2, percent = TRUE)
add_number("RtwoSatisfiedMeanPPP", main_results$r2_income[main_results$income == "log_gdp_ppp" & main_results$indicator == "satisfied_mean"], 2, percent = TRUE)
add_number("RtwoHappinessMeanPPP", main_results$r2_income[main_results$income == "log_gdp_ppp" & main_results$indicator == "happiness_mean"], 2, percent = TRUE)
add_number("RtwoVeryHappyPPP", main_results$r2_income[main_results$income == "log_gdp_ppp" & main_results$indicator == "very_happy"], 2, percent = TRUE)
add_number("shareRegionBetterGallup", mean(results$share_income[grepl("^gallup", results$spec)] < 0.5), 2, percent = TRUE)
add_number("shareIncomeGallupMean", results$share_income[results$spec == "gallup_last" & results$income == "log_gdp_ppp" & results$indicator == "satisfied_mean"], 2, percent = TRUE)
add_number("RtwoGallupMeanPPP", results$r2_income[results$spec == "gallup_last" & results$income == "log_gdp_ppp" & results$indicator == "satisfied_mean"], 2, percent = TRUE)
add_number("RtwoGallupRegion", results$r2_region[results$spec == "gallup_last" & results$income == "log_gdp_ppp" & results$indicator == "satisfied_mean"], 2, percent = TRUE)
add_number("RtwoWHRPPP", results$r2_income[results$spec == "whr_2025" & results$income == "log_gdp_ppp"], 2, percent = TRUE)
add_number("RtwoWHRRegion", results$r2_region[results$spec == "whr_2025" & results$income == "log_gdp_ppp"], 2, percent = TRUE)
add_number("shareRegionBetterWVSonly", robustness$region_better[robustness$spec == "wvs_only"], 2, percent = TRUE)
add_number("shareRegionBetterNoLatamEE", robustness$region_better[robustness$spec == "wvs_no_latam_ee"], 2, percent = TRUE)
add_number("shareIncomeNoLatamEE", robustness$share_ppp[robustness$spec == "wvs_no_latam_ee"], 2, percent = TRUE)
add_number("shareRegionBetterContinents", robustness$region_better[robustness$spec == "wvs_region_continent"], 2, percent = TRUE)
add_number("shareRegionBetterWB", robustness$region_better[robustness$spec == "wvs_region_wb"], 2, percent = TRUE)
add_number("cvIncomeSatisfiedMean", cv_table$cv_income_ppp[cv_table$spec == "wvs" & cv_table$indicator == "satisfied_mean"], 2)
add_number("cvRegionSatisfiedMean", cv_table$cv_region[cv_table$spec == "wvs" & cv_table$indicator == "satisfied_mean"], 2)
add_number("cvIncomeGallupMean", cv_table$cv_income_ppp[cv_table$spec == "gallup_last" & cv_table$indicator == "satisfied_mean"], 2)
add_number("cvRegionGallupMean", cv_table$cv_region[cv_table$spec == "gallup_last" & cv_table$indicator == "satisfied_mean"], 2)
add_number("shareCvRegionBetterWVS", mean((cv_table |> filter(spec == "wvs") |> mutate(b = cv_region > pmax(cv_income_ppp, cv_income_best)))$b), 2, percent = TRUE)


##### 2.3 Descriptives #####
# Correlation between well-being indicators (WVS country-years)
correlation <- cor(wvs_main[, wellbeing_variables], use = "pairwise.complete.obs")
write_latex_table(data.frame(indicator = wellbeing_names[wellbeing_variables], correlation, check.names = FALSE), "correlation_indicators.tex",
                  header = paste("&", paste(wellbeing_names[wellbeing_variables], collapse = " & ")), align = paste0("l", strrep("c", length(wellbeing_variables))))
add_number("corHappySatisfied", correlation["happy", "satisfied"], 2)
add_number("corHappinessSatisfactionMean", correlation["happiness_mean", "satisfied_mean"], 2)
add_number("corVeryHappySatisfiedMean", correlation["very_happy", "satisfied_mean"], 2)

# Slopes with respect to log GDP p.c. PPP (standard errors clustered by country)
slope_of <- function(df, y, data_name) {
  model <- lm(as.formula(paste(y, "~ log10(gdp_ppp)")), data = df)
  test <- coeftest(model, vcov = vcovCL(model, cluster = ~code))
  data.frame(data = data_name, indicator = y, slope = test[2, 1], se = test[2, 2], p_value = test[2, 4], r2 = summary(model)$r.squared, n = nobs(model))
}
slopes <- bind_rows(lapply(wellbeing_variables, function(y) slope_of(wvs_main, y, "WVS")), lapply(satisfaction_variables, function(y) slope_of(gallup_main, y, "Gallup")))
write.csv(slopes, paste0(tables_folder, "slopes.csv"), row.names = FALSE)
write_latex_table(slopes |> transmute(data, indicator = wellbeing_names[indicator], slope, se, p_value, r2, n = as.character(n)), "slopes.tex", digits = 3,
                  header = "Data & Indicator & Slope & s.e. & $p$-value & $R^2$ & Obs.", align = "llccccc", midrule_before = length(wellbeing_variables) + 1)
add_number("slopeVeryHappy", slopes$slope[slopes$data == "WVS" & slopes$indicator == "very_happy"], 3)
add_p("pVeryHappy", slopes$p_value[slopes$data == "WVS" & slopes$indicator == "very_happy"])

# Happiest country-years by indicator and wave
happiest <- expand.grid(indicator = wellbeing_variables, wave = c(as.character(1:7), "all"), stringsAsFactors = FALSE)
happiest$country <- sapply(seq_len(nrow(happiest)), function(i) {
  d <- if (happiest$wave[i] == "all") wvs_main else wvs_main[wvs_main$wave == as.numeric(happiest$wave[i]), ]
  best <- if (happiest$indicator[i] == "very_unhappy") which.min(d[[happiest$indicator[i]]]) else which.max(d[[happiest$indicator[i]]])
  paste(d$country[best], d$year[best]) })
happiest$region <- wvs_main$region[match(happiest$country, paste(wvs_main$country, wvs_main$year))]
write.csv(happiest, paste0(tables_folder, "happiest_countries.csv"), row.names = FALSE)
happiest_counts <- sort(table(sub(" [0-9]{4}$", "", happiest$country)), decreasing = TRUE)
happiest_region_counts <- sort(table(happiest$region), decreasing = TRUE)
print(happiest_counts[1:8]); print(happiest_region_counts)
write_latex_table(data.frame(country = names(happiest_counts)[1:10], occurrences = as.character(as.vector(happiest_counts)[1:10]), region = region5_of(countrycode(names(happiest_counts)[1:10], "country.name", "iso3c", warn = FALSE))),
                  "happiest_countries.tex", header = "Country & Occurrences as happiest & Region", align = "lcl")
add_number("happiestFirst", names(happiest_counts)[1]); add_number("happiestFirstN", as.vector(happiest_counts)[1], 0)
add_number("happiestSecond", names(happiest_counts)[2]); add_number("happiestSecondN", as.vector(happiest_counts)[2], 0)
add_number("happiestThird", names(happiest_counts)[3]); add_number("happiestThirdN", as.vector(happiest_counts)[3], 0)
for (r in names(happiest_region_counts)) add_number(paste0("happiestRegion", gsub(" ", "", r)), as.vector(happiest_region_counts[r]), 0)
add_number("nHappiestCells", nrow(happiest), 0)

# First split of a regression tree with income and region
tree_first_split <- sapply(wellbeing_variables, function(y) {
  tree <- rpart(as.formula(paste(y, "~ log_gdp_ppp + region")), data = make_income_vars(wvs_main) |> mutate(region = factor(region)), control = rpart.control(maxdepth = 1, cp = 0))
  as.character(tree$frame$var[1]) })
print(tree_first_split)
add_number("treeRegionFirst", sum(tree_first_split == "region"), 0)

# Within-country relation (country fixed effects), cf. Easterlin paradox
within <- bind_rows(lapply(wellbeing_variables, function(y) {
  model <- feols(as.formula(paste(y, "~ log10(gdp_ppp) | code")), data = wvs_main |> group_by(code) |> filter(n() >= 2) |> ungroup(), cluster = ~code)
  data.frame(indicator = y, slope = coef(model)[1], se = se(model)[1], p_value = pvalue(model)[1], within_r2 = r2(model, "wr2"), n = nobs(model)) }))
write.csv(within, paste0(tables_folder, "within_country.csv"), row.names = FALSE)
write_latex_table(within |> transmute(indicator = wellbeing_names[indicator], slope, se, p_value, within_r2, n = as.character(n)), "within_country.tex", digits = 3,
                  header = "Indicator & Slope & s.e. & $p$-value & Within $R^2$ & Obs.", align = "lccccc")
add_number("withinRtwoSatisfiedMean", within$within_r2[within$indicator == "satisfied_mean"], 2)
add_number("withinSlopeSatisfiedMean", within$slope[within$indicator == "satisfied_mean"], 2); add_number("crossSlopeSatisfiedMean", slopes$slope[slopes$data == "WVS" & slopes$indicator == "satisfied_mean"], 2)
add_number("withinSlopeVeryHappy", within$slope[within$indicator == "very_happy"], 2); add_p("pWithinVeryHappy", within$p_value[within$indicator == "very_happy"])
add_number("nWithin", within$n[within$indicator == "satisfied_mean"], 0)

# Cultural and institutional correlates: Shapley decomposition of R² among income, region and one other variable
#' Shapley (LMG) decomposition of R² among groups of regressors
#' @param df Data. @param y Outcome. @param groups Named list of regressor terms (formula strings).
#' @return Named vector of R² shares attributed to each group (summing to the R² of the full model).
shapley_r2 <- function(df, y, groups) {
  d <- df[complete.cases(df[, unique(c(y, unlist(lapply(groups, all.vars))))]), ]
  k <- length(groups); r2_subset <- list()
  for (m in 0:(2^k - 1)) {
    in_set <- as.logical(intToBits(m))[1:k]
    r2_subset[[as.character(m)]] <- if (!any(in_set)) 0 else summary(lm(as.formula(paste(y, "~", paste(unlist(groups[in_set]), collapse = " + "))), data = d))$r.squared
  }
  sapply(1:k, function(j) {
    sum(sapply(0:(2^k - 1), function(m) {
      in_set <- as.logical(intToBits(m))[1:k]
      if (in_set[j]) return(0)
      s <- sum(in_set); with_j <- m + 2^(j - 1)
      factorial(s) * factorial(k - s - 1) / factorial(k) * (r2_subset[[as.character(with_j)]] - r2_subset[[as.character(m)]]) })) }) |> setNames(names(groups))
}
correlates <- c("freedom" = "Freedom of choice", "god" = "Importance of God", "homosexuality" = "Tolerance (homosexuality)", "democracy" = "Importance of democracy", "growth" = "GDP growth (5-year)")
culture <- bind_rows(lapply(c("satisfied_mean", "happiness_mean"), function(y) bind_rows(lapply(names(correlates), function(v) {
  sh <- shapley_r2(wvs_main, y, list(income = "log10(gdp_ppp)", region = "factor(region)", other = v))
  data.frame(indicator = y, variable = v, r2_alone = summary(lm(as.formula(paste(y, "~", v)), data = wvs_main))$r.squared, income = sh[1], region = sh[2], other = sh[3], total = sum(sh),
             n = sum(complete.cases(wvs_main[, c(y, v, "gdp_ppp", "region")]))) }))))
write.csv(culture, paste0(tables_folder, "correlates.csv"), row.names = FALSE)
write_latex_table(culture |> transmute(indicator = wellbeing_names[indicator], variable = correlates[variable], n = as.character(n), r2_alone, income, region, other, total), "correlates.tex",
                  header = "Indicator & Other variable & Obs. & $R^2$ other alone & \\multicolumn{4}{c}{Shapley decomposition of $R^2$} \\\\ & & & & Income & Region & Other & Total", align = "llcccccc",
                  midrule_before = length(correlates) + 1)
add_number("RtwoFreedomSatisfaction", culture$r2_alone[culture$indicator == "satisfied_mean" & culture$variable == "freedom"], 2, percent = TRUE)
freedom_row <- culture[culture$indicator == "satisfied_mean" & culture$variable == "freedom", ]
add_number("shapleyFreedom", freedom_row$other, 2, percent = TRUE); add_number("shapleyFreedomRegion", freedom_row$region, 2, percent = TRUE); add_number("shapleyFreedomIncome", freedom_row$income, 2, percent = TRUE)
add_number("RtwoTolerance", culture$r2_alone[culture$indicator == "satisfied_mean" & culture$variable == "homosexuality"], 2, percent = TRUE)
add_number("RtwoGod", culture$r2_alone[culture$indicator == "satisfied_mean" & culture$variable == "god"], 2, percent = TRUE)
add_number("RtwoGrowth", culture$r2_alone[culture$indicator == "satisfied_mean" & culture$variable == "growth"], 2, percent = TRUE)

# Non-response
add_number("nonresponseSatisfactionWVS", mean(wvs_main$nonresponse_satisfaction), 3, percent = TRUE)
add_number("nonresponseHappinessWVS", mean(wvs_main$nonresponse_happiness), 3, percent = TRUE)
add_number("corNonresponseGDP", cor(wvs_main$nonresponse_satisfaction, log(wvs_main$gdp_ppp)), 2)

# Appendix: well-being indicators by country-year
write.csv(wvs_main |> select(country, code, year, wave, region, all_of(wvs_indicators), gdp_ppp, gdp, pop, source_ppp, source_nominal), paste0(tables_folder, "wellbeing_country_year_wvs.csv"), row.names = FALSE)
write.csv(gallup_main |> select(country, code, year, region, all_of(satisfaction_variables), n, gdp_ppp, gdp, source_ppp), paste0(tables_folder, "wellbeing_country_year_gallup.csv"), row.names = FALSE)


##### 2.4 Figures #####
region_colors <- c("Africa" = "black", "Asia" = "purple", "Eastern Europe" = "red", "Latin America" = "#4CAF50", "Western" = "#64B5F6")
region_shapes <- c("Africa" = 15, "Asia" = 17, "Eastern Europe" = 16, "Latin America" = 0, "Western" = 1)

#' Scatter plot of a well-being indicator against GDP p.c. (log scale), by region, with country labels
#' @param df Data. @param y Indicator. @param file File name (without extension). @param label_year Whether to add the year to labels.
#' @param x_var GDP variable. @param y_label Axis label.
#' @return The ggplot (also saved as PDF and PNG).
scatter_wellbeing <- function(df, y, file, label_year = TRUE, x_var = "gdp_ppp", y_label = wellbeing_names[y]) {
  df <- df[!is.na(df[[y]]) & !is.na(df[[x_var]]), ]
  r2 <- summary(lm(as.formula(paste(y, "~ log10(", x_var, ")")), data = df))$r.squared
  df$label <- if (label_year) paste0(df$code, substr(df$year, 3, 4)) else df$code
  p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y]], color = region, shape = region, label = label)) + geom_point() +
    geom_text_repel(size = 1.8, show.legend = FALSE, segment.size = 0.2, max.overlaps = 15) +
    scale_x_log10(labels = scales::label_comma()) + scale_color_manual(values = region_colors, name = paste0("R-squared (log GDP) = ", round(r2, 2), "   ")) +
    scale_shape_manual(values = region_shapes, name = paste0("R-squared (log GDP) = ", round(r2, 2), "   ")) +
    labs(x = if (x_var == "gdp_ppp") "GDP per capita, PPP (constant 2021 $, log scale)" else "GDP per capita (constant 2015 $, log scale)", y = gsub("\\\\", "", gsub("--", "-", y_label))) +
    theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 9)) + guides(color = guide_legend(nrow = 1))
  ggsave(paste0(figures_folder, file, ".pdf"), p, width = 7, height = 4.5)
  ggsave(paste0(figures_folder, file, ".png"), p, width = 7, height = 4.5, dpi = 200)
  return(p)
}
for (y in wellbeing_variables) scatter_wellbeing(wvs_main, y, paste0("wvs_", y, "_vs_gdp_ppp"))
scatter_wellbeing(gallup_main |> filter(year == last_year), "satisfied_mean", "gallup_ladder_mean_vs_gdp_ppp", label_year = FALSE, y_label = "Ladder (mean), Gallup, last year available")
scatter_wellbeing(whr_main |> filter(year == 2025), "satisfied_mean", "whr2025_ladder_vs_gdp_ppp", label_year = FALSE, y_label = "Ladder (mean), WHR 2023-2025")
p_happy_satisfied <- ggplot(wvs_main, aes(x = satisfied, y = happy, color = region, shape = region)) + geom_point() + scale_color_manual(values = region_colors) + scale_shape_manual(values = region_shapes) +
  labs(x = "Satisfied (share 6-10)", y = "Happy (share quite or very happy)", color = NULL, shape = NULL) + theme_minimal() + theme(legend.position = "bottom")
ggsave(paste0(figures_folder, "happy_vs_satisfied.pdf"), p_happy_satisfied, width = 6, height = 4.5)


##### 3.1 Fabre (2025): experimental effects of wording and scale #####
fabre <- fabre |> mutate(satisfied = as.numeric(well_being >= 6), gallup_native = as.numeric(variant == "gallup_0"))
fabre_models <- list(
  "Answer" = feols(well_being ~ ladder + scale_1_10 | code, data = fabre, weights = ~weight, vcov = "hetero"),
  "Answer " = feols(well_being ~ ladder * scale_1_10 | code, data = fabre, weights = ~weight, vcov = "hetero"),
  "Answer (0-10 equiv.)" = feols(well_being_0_10 ~ ladder * scale_1_10 | code, data = fabre, weights = ~weight, vcov = "hetero"),
  "Satisfied (6+)" = feols(satisfied ~ ladder * scale_1_10 | code, data = fabre, weights = ~weight, vcov = "hetero"),
  "Answer (0-10 equiv.) " = feols(well_being_0_10 ~ ladder * income_quartile + scale_1_10 | code, data = fabre, weights = ~weight, vcov = "hetero"),
  "Answer (0-10 equiv.)  " = feols(well_being_0_10 ~ income_quartile * ladder * scale_1_10 | code, data = fabre, weights = ~weight, vcov = "hetero"))
etable(fabre_models, tex = TRUE, float = FALSE, file = paste0(tables_folder, "fabre_regressions.tex"), replace = TRUE, fitstat = ~ n + r2,
       dict = c(ladder = "Ladder wording (Gallup)", scale_1_10 = "Scale 1--10", income_quartile = "Income quartile (1--4)", code = "Country", well_being = "Answer", well_being_0_10 = "Answer (0--10 equiv.)", satisfied = "Satisfied (6+)"),
       depvar = TRUE, digits = 3, signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05)) # Journal of Economic Psychology convention
print(etable(fabre_models))
add_number("effectLadder", coef(fabre_models[[1]])["ladder"], 2); add_number("seLadder", se(fabre_models[[1]])["ladder"], 2)
add_number("effectScale", coef(fabre_models[[1]])["scale_1_10"], 2); add_number("seScale", se(fabre_models[[1]])["scale_1_10"], 2)
add_number("effectLadderZeroTen", coef(fabre_models[[3]])["ladder"], 2)
add_number("effectScaleZeroTen", coef(fabre_models[[3]])["scale_1_10"], 2)
add_number("effectLadderSatisfied", coef(fabre_models[[4]])["ladder"], 2, percent = TRUE)
add_number("interactionLadderIncome", coef(fabre_models[[5]])["ladder:income_quartile"], 3); add_p("pInteractionLadderIncome", pvalue(fabre_models[[5]])["ladder:income_quartile"])
add_number("gradientIncomeSatisfaction", coef(fabre_models[[5]])["income_quartile"], 2)
add_number("tripleInteraction", coef(fabre_models[[6]])["income_quartile:ladder:scale_1_10"], 3); add_p("pTripleInteraction", pvalue(fabre_models[[6]])["income_quartile:ladder:scale_1_10"])
add_number("interactionLadderIncomeFull", coef(fabre_models[[6]])["income_quartile:ladder"], 3); add_p("pInteractionLadderIncomeFull", pvalue(fabre_models[[6]])["income_quartile:ladder"])
add_number("interactionScaleIncome", coef(fabre_models[[6]])["income_quartile:scale_1_10"], 3); add_p("pInteractionScaleIncome", pvalue(fabre_models[[6]])["income_quartile:scale_1_10"])

# Heterogeneity of the wording effect across countries (Wald test of equal country-specific effects)
heterogeneity_model <- feols(well_being ~ i(code, ladder) + i(code, scale_1_10) | code, data = fabre, weights = ~weight, vcov = "hetero")
restricted <- feols(well_being ~ ladder + i(code, scale_1_10) | code, data = fabre, weights = ~weight, vcov = "hetero")
wald_heterogeneity <- wald(heterogeneity_model, keep = "::.*:ladder", print = FALSE) # H0: all ladder effects equal 0 (joint)
wording_by_country <- data.frame(code = fabre_countries, effect = coef(heterogeneity_model)[paste0("code::", fabre_countries, ":ladder")], se = se(heterogeneity_model)[paste0("code::", fabre_countries, ":ladder")])
equality_test <- {
  b <- wording_by_country$effect; V <- vcov(heterogeneity_model)[paste0("code::", fabre_countries, ":ladder"), paste0("code::", fabre_countries, ":ladder")]
  C <- cbind(diag(length(b) - 1), -1) # differences with the last country
  stat <- t(C %*% b) %*% solve(C %*% V %*% t(C)) %*% (C %*% b)
  c(chi2 = stat, df = length(b) - 1, p = pchisq(stat, length(b) - 1, lower.tail = FALSE)) }
print(wording_by_country); print(equality_test)
add_p("pHeterogeneityWording", equality_test["p"])
add_number("chiHeterogeneityWording", equality_test["chi2"], 1)
for (c in fabre_countries) add_number(paste0("wording", c), wording_by_country$effect[wording_by_country$code == c], 2)
add_number("wordingMin", min(wording_by_country$effect), 2); add_number("wordingMax", max(wording_by_country$effect), 2)


##### 3.2 Country-level decomposition of the Gallup/WVS gap #####
#' Country-level indicators of the Fabre (2025) variants (mean on the native scale, share satisfied 6+)
#' @param df Fabre data (possibly resampled).
#' @return Data frame code x variant with mean and satisfied.
fabre_means <- function(df) df |> group_by(code, variant) |> summarise(mean = wmean(well_being, weight), satisfied = wmean(satisfied, weight)) |> ungroup()

# WVS/EVS: latest survey per Fabre country; Gallup: same year (or closest year available, |gap| <= 3 years)
wvs_fabre <- wvs |> filter(code %in% fabre_countries) |> group_by(code) |> filter(year == max(year), !is.na(satisfaction)) |> ungroup()
wvs_mode <- wvs_fabre |> filter(!is.na(mode)) |> count(code, study, mode) |> group_by(code) |>
  summarise(wvs_mode = paste0(first(study), ": ", paste(unique(mode[order(-n)]), collapse = "/")))
wvs_year <- wvs_fabre |> distinct(code, year) |> rename(year_wvs = year)
gallup_year <- wvs_year |> left_join(gallup_counts |> select(code, year_gallup = year), by = "code", relationship = "many-to-many") |>
  group_by(code) |> slice_min(abs(year_gallup - year_wvs), with_ties = FALSE) |> ungroup() |> filter(abs(year_gallup - year_wvs) <= 3)
gallup_fabre_counts <- gallup_counts |> semi_join(gallup_year, by = c("code", "year" = "year_gallup"))

#' Compute the decomposition statistics from (possibly resampled) Fabre, WVS and Gallup data
#' @param fabre_df Fabre respondents. @param wvs_df WVS respondents (latest wave, Fabre countries). @param gallup_df Gallup counts (matched year).
#' @return List with the country-level table and summary statistics.
decomposition <- function(fabre_df, wvs_df, gallup_df) {
  f <- fabre_means(fabre_df) |> pivot_wider(names_from = variant, values_from = c(mean, satisfied))
  w <- wvs_df |> group_by(code) |> summarise(wvs_mean = wmean(satisfaction, weight), wvs_satisfied = wmean(satisfaction >= 6, weight))
  g <- bind_cols(gallup_df |> select(code), satisfaction_indicators(as.matrix(gallup_df[, paste0("s", 0:10)]), 0:10)) |> transmute(code, gallup_mean = satisfied_mean, gallup_satisfied = satisfied)
  d <- f |> inner_join(w, by = "code") |> inner_join(g, by = "code") |>
    mutate(gap = gallup_mean - wvs_mean, question = mean_gallup_0 - mean_wvs_1, residual = gap - question,
           wording_0_10 = mean_gallup_0 - mean_wvs_0, scale_wvs = mean_wvs_0 - mean_wvs_1,
           gap_sat = gallup_satisfied - wvs_satisfied, question_sat = satisfied_gallup_0 - satisfied_wvs_1, residual_sat = gap_sat - question_sat,
           sample_gallup = gallup_mean - mean_gallup_0, sample_wvs = wvs_mean - mean_wvs_1)
  d <- d |> left_join(gdp_ten, by = "code")
  slope <- function(y, x) unname(coef(lm(y ~ x))[2])
  cov_share <- function(gap, part) cov(gap, part) / var(gap)
  stats <- c(mean_gap = mean(d$gap), mean_question = mean(d$question), mean_residual = mean(d$residual),
             share_level_question = mean(d$question) / mean(d$gap),
             share_abs_question = abs(mean(d$question)) / (abs(mean(d$question)) + abs(mean(d$residual))), # share of the gross (absolute) movements between Gallup and WVS levels due to the question
             share_abs_question_country = mean(abs(d$question) / (abs(d$question) + abs(d$residual))), # same, computed country by country then averaged
             var_share_question = cov_share(d$gap, d$question), var_share_residual = cov_share(d$gap, d$residual),
             mean_abs_gap = mean(abs(d$gap)), mean_abs_residual = mean(abs(d$residual)),
             rmse_prediction_gallup = sqrt(mean((d$gallup_mean - (d$wvs_mean + d$question))^2)), rmse_naive = sqrt(mean((d$gallup_mean - d$wvs_mean - mean(d$gap))^2)),
             mean_gap_sat = mean(d$gap_sat), mean_question_sat = mean(d$question_sat), var_share_question_sat = cov_share(d$gap_sat, d$question_sat),
             sd_gap = sd(d$gap), sd_question = sd(d$question), sd_residual = sd(d$residual), cor_gap_question = cor(d$gap, d$question),
             slope_gallup = slope(d$gallup_mean, d$log_gdp_past), slope_wvs = slope(d$wvs_mean, d$log_gdp_past),
             slope_fabre_ladder = slope(d$mean_gallup_0, d$log_gdp_2025), slope_fabre_satisfaction = slope(d$mean_wvs_1, d$log_gdp_2025),
             slope_diff_past = slope(d$gallup_mean - d$wvs_mean, d$log_gdp_past), slope_diff_fabre = slope(d$mean_gallup_0 - d$mean_wvs_1, d$log_gdp_2025),
             slope_diff_in_diff = slope(d$gallup_mean - d$wvs_mean, d$log_gdp_past) - slope(d$mean_gallup_0 - d$mean_wvs_1, d$log_gdp_2025)) # part of the Gallup-WVS difference in income gradients not due to the question
  list(table = d, stats = stats)
}
gdp_ten <- add_gdp(wvs_year |> transmute(code, year = year_wvs)) |> transmute(code, log_gdp_past = log10(gdp_ppp)) |>
  left_join(add_gdp(data.frame(code = fabre_countries, year = 2025L)) |> transmute(code, log_gdp_2025 = log10(gdp_ppp)), by = "code")
decomp <- decomposition(fabre, wvs_fabre, gallup_fabre_counts)

# Bootstrap: resample Fabre respondents within country x variant, WVS respondents within country, Gallup answers (multinomial) within country
#' Indices of a stratified bootstrap sample (resampling with replacement within each stratum)
#' @param strata Vector of strata. @return Vector of row indices.
stratified_sample <- function(strata) unlist(lapply(split(seq_along(strata), strata), function(i) i[sample.int(length(i), length(i), replace = TRUE)]), use.names = FALSE)
boot_stats <- t(sapply(1:n_bootstrap, function(b) {
  fb <- fabre[stratified_sample(paste(fabre$code, fabre$variant)), ]
  wb <- wvs_fabre[stratified_sample(wvs_fabre$code), ]
  gb <- gallup_fabre_counts; m <- as.matrix(gb[, paste0("s", 0:10)])
  gb[, paste0("s", 0:10)] <- t(apply(m, 1, function(x) rmultinom(1, sum(x), x / sum(x))))
  decomposition(fb, wb, gb)$stats }))
decomp_ci <- apply(boot_stats, 2, quantile, c(0.025, 0.975), na.rm = TRUE)
print(round(rbind(estimate = decomp$stats, decomp_ci), 3))

country_table <- decomp$table |> left_join(wvs_year, by = "code") |> left_join(gallup_year |> select(code, year_gallup), by = "code") |> left_join(wvs_mode, by = "code") |>
  mutate(country = country_of_iso3[code]) |> arrange(gap)
write.csv(country_table, paste0(tables_folder, "decomposition_by_country.csv"), row.names = FALSE)
write_latex_table(country_table |> transmute(country, years = paste0(year_wvs, if_else(year_gallup != year_wvs, paste0(" (", year_gallup, ")"), "")), wvs_mode, gallup_mean, wvs_mean, gap,
                                             mean_gallup_0, mean_wvs_1, question, residual),
                  "decomposition_by_country.tex", header = "Country & Year WVS/EVS (Gallup) & Survey: mode & Gallup & WVS/EVS & Gap $D$ & Ladder 0--10 & Satisf. 1--10 & Question effect $Q$ & Residual $R$ \\\\ & & & (1) & (2) & (1)$-$(2) & \\multicolumn{2}{c}{Fabre (2025)} & & $D-Q$",
                  align = "lllccccccc")
fmt_ci <- function(s, digits = 2) { f <- function(v) latex_minus(formatC(v, format = "f", digits = digits)); paste0(f(decomp$stats[s]), " [", f(decomp_ci[1, s]), "; ", f(decomp_ci[2, s]), "]") }
decomp_summary <- data.frame(statistic = c("Mean gap $D$ (Gallup $-$ WVS)", "Mean question effect $Q$ (wording + scale)", "Mean residual $R$ (sampling, mode, context)", "Share of mean gap due to question ($\\bar Q/\\bar D$)", "Share of gross level difference due to question ($|\\bar Q|/(|\\bar Q|+|\\bar R|)$)", "Same, country average ($\\overline{|Q_c|/(|Q_c|+|R_c|)}$)",
                                           "Share of cross-country variance of $D$ due to $Q$ ($\\mathrm{cov}(D,Q)/\\mathrm{var}(D)$)", "Share of cross-country variance of $D$ due to $R$",
                                           "Mean absolute gap $|D|$", "Mean absolute residual $|R|$", "Correlation between $D$ and $Q$",
                                           "Mean gap, share satisfied (6+)", "Mean question effect, share satisfied (6+)", "Variance share due to $Q$, share satisfied"),
                             estimate = c(fmt_ci("mean_gap"), fmt_ci("mean_question"), fmt_ci("mean_residual"), fmt_ci("share_level_question"), fmt_ci("share_abs_question"), fmt_ci("share_abs_question_country"), fmt_ci("var_share_question"), fmt_ci("var_share_residual"),
                                          fmt_ci("mean_abs_gap"), fmt_ci("mean_abs_residual"), fmt_ci("cor_gap_question"), fmt_ci("mean_gap_sat"), fmt_ci("mean_question_sat"), fmt_ci("var_share_question_sat")))
write_latex_table(decomp_summary, "decomposition_summary.tex", header = "Statistic & Estimate [95\\% bootstrap CI]", align = "lc", midrule_before = c(4, 7, 12))
for (s in names(decomp$stats)) add_number(paste0("decomp", gsub("[^A-Za-z]", "", tools::toTitleCase(gsub("_", " ", s)))), decomp$stats[s], 2)
for (s in c("mean_gap", "mean_question", "mean_residual", "var_share_question", "share_level_question", "cor_gap_question", "share_abs_question", "share_abs_question_country")) {
  add_number(paste0("decompLow", gsub("[^A-Za-z]", "", tools::toTitleCase(gsub("_", " ", s)))), decomp_ci[1, s], 2)
  add_number(paste0("decompHigh", gsub("[^A-Za-z]", "", tools::toTitleCase(gsub("_", " ", s)))), decomp_ci[2, s], 2) }
add_number("nDecompCountries", nrow(decomp$table), 0)
# Robustness: only countries where Gallup and WVS are observed the same year; only countries with a recent WVS (2017+)
same_year_codes <- gallup_year$code[gallup_year$year_gallup == gallup_year$year_wvs]
recent_codes <- wvs_year$code[wvs_year$year_wvs >= 2017]
decomp_same_year <- decomposition(fabre |> filter(code %in% same_year_codes), wvs_fabre |> filter(code %in% same_year_codes), gallup_fabre_counts |> filter(code %in% same_year_codes))$stats
decomp_recent <- decomposition(fabre |> filter(code %in% recent_codes), wvs_fabre |> filter(code %in% recent_codes), gallup_fabre_counts |> filter(code %in% recent_codes))$stats
print(round(rbind(same_year = decomp_same_year, recent = decomp_recent), 3))
add_number("nSameYear", length(same_year_codes), 0); add_number("nRecent", length(recent_codes), 0)
add_number("varShareQuestionSameYear", decomp_same_year["var_share_question"], 2); add_number("varShareQuestionRecent", decomp_recent["var_share_question"], 2)
add_number("shareLevelQuestionSameYear", decomp_same_year["share_level_question"], 2); add_number("shareLevelQuestionRecent", decomp_recent["share_level_question"], 2)

# Income gradient across the 10 countries: past data (Gallup, WVS/EVS, same year) vs. new data (Fabre 2025, 4 variants)
ten <- decomp$table |> mutate(region = region5_of(code)) # GDP (log_gdp_past, log_gdp_2025) already joined in decomposition()
ten_series <- list(c("Gallup (ladder 0--10)", "gallup_mean", "log_gdp_past"), c("WVS/EVS (satisfaction 1--10)", "wvs_mean", "log_gdp_past"),
                   c("Fabre 2025: ladder 0--10", "mean_gallup_0", "log_gdp_2025"), c("Fabre 2025: ladder 1--10", "mean_gallup_1", "log_gdp_2025"),
                   c("Fabre 2025: satisfaction 0--10", "mean_wvs_0", "log_gdp_2025"), c("Fabre 2025: satisfaction 1--10", "mean_wvs_1", "log_gdp_2025"))
ten_table <- bind_rows(lapply(ten_series, function(v) {
  model <- lm(as.formula(paste(v[2], "~", v[3])), data = ten)
  lmg <- income_vs_region(ten |> mutate(log_x = .data[[v[3]]]), v[2], "log_x", cv = FALSE)
  data.frame(series = v[1], slope = coef(model)[2], se = sqrt(vcovHC(model, type = "HC1")[2, 2]), r2_income = lmg$r2_income, r2_region = lmg$r2_region, share_income = lmg$share_income) }))
print(ten_table)
write_latex_table(ten_table, "ten_countries.tex", header = "Data & Slope on $\\log_{10}$ GDP & s.e. & $R^2$ income & $R^2$ region & Share due to income", align = "lccccc", midrule_before = 3)
add_number("slopeTenGallup", ten_table$slope[1], 2); add_number("slopeTenWVS", ten_table$slope[2], 2)
add_number("slopeTenFabreLadder", ten_table$slope[3], 2); add_number("slopeTenFabreSatisfaction", ten_table$slope[6], 2)
add_number("RtwoTenGallup", ten_table$r2_income[1], 2); add_number("RtwoTenWVS", ten_table$r2_income[2], 2)
add_number("RtwoTenFabreLadder", ten_table$r2_income[3], 2); add_number("RtwoTenFabreSatisfaction", ten_table$r2_income[6], 2)
for (s in c("slope_diff_past", "slope_diff_fabre", "slope_diff_in_diff")) {
  name <- gsub("[^A-Za-z]", "", tools::toTitleCase(gsub("_", " ", s)))
  add_number(name, decomp$stats[s], 2); add_number(paste0(name, "Low"), decomp_ci[1, s], 2); add_number(paste0(name, "High"), decomp_ci[2, s], 2) }

# Same-period comparison of samples with the same (ladder, 0-10) question: Gallup/WHR 2023-2025 vs. Fabre 2025
gallup_vs_fabre <- fabre_means(fabre) |> filter(variant == "gallup_0") |> inner_join(whr |> filter(year == 2025) |> select(code, whr = satisfied_mean), by = "code") |>
  mutate(sample_effect = whr - mean, country = country_of_iso3[code])
add_number("meanSampleEffectGallup", mean(gallup_vs_fabre$sample_effect), 2)
add_number("corFabreWHR", cor(gallup_vs_fabre$mean, gallup_vs_fabre$whr), 2)
add_number("minSampleEffectGallup", min(gallup_vs_fabre$sample_effect), 2); add_number("maxSampleEffectGallup", max(gallup_vs_fabre$sample_effect), 2)
add_number("meanSampleEffectWVS", mean(decomp$table$sample_wvs), 2)
add_number("meanFabreGallupZero", mean(decomp$table$mean_gallup_0), 2); add_number("meanFabreWVSOne", mean(decomp$table$mean_wvs_1), 2)
add_number("sampleEffectUSWVS", decomp$table$sample_wvs[decomp$table$code == "USA"], 2) # U.S.: WVS 2017 was a web survey, like Fabre (2025)

# Figure: gap, question effect and residual by country
decomp_long <- country_table |> select(country, gap, question, residual) |> pivot_longer(-country) |>
  mutate(name = factor(c("gap" = "Observed gap D (Gallup - WVS)", "question" = "Question effect Q (Fabre 2025)", "residual" = "Residual R = D - Q")[name], levels = c("Observed gap D (Gallup - WVS)", "Question effect Q (Fabre 2025)", "Residual R = D - Q")),
         country = factor(country, levels = country_table$country))
p_decomp <- ggplot(decomp_long, aes(x = value, y = country, fill = name)) + geom_col(position = position_dodge(width = 0.8), width = 0.75) + geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("grey40", "#1f78b4", "#e6550d"), name = NULL) + labs(x = "Difference in mean answer (points, native scales)", y = NULL) +
  theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 10)) + guides(fill = guide_legend(nrow = 1))
ggsave(paste0(figures_folder, "decomposition_by_country.pdf"), p_decomp, width = 7.5, height = 4.5)
ggsave(paste0(figures_folder, "decomposition_by_country.png"), p_decomp, width = 7.5, height = 4.5, dpi = 200)

# Figure: mean answer by variant and country
fabre_plot <- fabre_means(fabre) |> mutate(country = country_of_iso3[code], variant = c("gallup_0" = "Ladder 0-10", "gallup_1" = "Ladder 1-10", "wvs_0" = "Satisfaction 0-10", "wvs_1" = "Satisfaction 1-10")[variant])
p_variants <- ggplot(fabre_plot, aes(x = mean, y = reorder(country, mean), color = variant, shape = variant)) + geom_point(size = 2.5) +
  scale_color_manual(values = c("#1f78b4", "#a6cee3", "#e6550d", "#fdae6b"), name = NULL) + scale_shape_manual(values = c(16, 1, 17, 2), name = NULL) +
  labs(x = "Mean answer (native scale)", y = NULL) + theme_minimal() + theme(legend.position = "bottom")
ggsave(paste0(figures_folder, "fabre_variants_by_country.pdf"), p_variants, width = 7, height = 4.5)
ggsave(paste0(figures_folder, "fabre_variants_by_country.png"), p_variants, width = 7, height = 4.5, dpi = 200)


##### 3.3 Global discrepancy: can wording explain the stronger income gradient in Gallup? #####
common <- inner_join(gallup_main |> select(code, year, gallup = satisfied_mean, gallup_satisfied = satisfied, gdp_ppp, gdp, region),
                     wvs_main |> select(code, year, wvs = satisfied_mean, wvs_satisfied = satisfied), by = c("code", "year")) |>
  mutate(log_gdp_ppp = log10(gdp_ppp), wvs_0_10 = (wvs - 1) * 10 / 9, gap = gallup - wvs)
r2_common <- c(gallup = summary(lm(gallup ~ log_gdp_ppp, common))$r.squared, wvs = summary(lm(wvs ~ log_gdp_ppp, common))$r.squared)
slope_common <- c(gallup = coef(lm(gallup ~ log_gdp_ppp, common))[2], wvs_0_10 = coef(lm(wvs_0_10 ~ log_gdp_ppp, common))[2])
gap_gdp <- lm(gap ~ log_gdp_ppp, common)
# Question effect as a function of GDP, estimated on the 10 Fabre countries (extrapolation outside high-income countries!)
fabre_gdp <- decomp$table |> left_join(add_gdp(data.frame(code = fabre_countries, year = 2024L)) |> select(code, gdp_ppp), by = "code") |> mutate(log_gdp_ppp = log10(gdp_ppp))
question_gdp <- lm(question ~ log_gdp_ppp, fabre_gdp)
common$wvs_adjusted <- common$wvs + predict(question_gdp, common)
r2_adjusted <- summary(lm(wvs_adjusted ~ log_gdp_ppp, common))$r.squared
share_by_income <- function(y) { r <- income_vs_region(make_income_vars(common), y, "log_gdp_ppp", cv = FALSE); c(r2_income = r$r2_income, r2_region = r$r2_region, share_income = r$share_income) }
common_shares <- rbind(Gallup = share_by_income("gallup"), WVS = share_by_income("wvs"))
print(r2_common); print(slope_common); print(summary(gap_gdp)$coefficients); print(coef(question_gdp)); print(r2_adjusted); print(common_shares)
add_number("nCommon", nrow(common), 0); add_number("nCommonCountries", length(unique(common$code)), 0)
add_number("RtwoCommonGallup", r2_common["gallup"], 2); add_number("RtwoCommonWVS", r2_common["wvs"], 2)
add_number("slopeCommonGallup", slope_common[1], 2); add_number("slopeCommonWVS", slope_common[2], 2)
add_number("slopeGapGDP", coef(gap_gdp)[2], 2); add_number("seSlopeGapGDP", sqrt(vcovCL(gap_gdp, cluster = ~code)[2, 2]), 2)
add_number("RtwoGapGDP", summary(gap_gdp)$r.squared, 2)
add_number("slopeQuestionGDP", coef(question_gdp)[2], 2); add_number("seSlopeQuestionGDP", summary(question_gdp)$coefficients[2, 2], 2)
add_number("RtwoAdjustedWVS", r2_adjusted, 2)
add_number("shareIncomeCommonGallup", common_shares["Gallup", "share_income"], 2, percent = TRUE); add_number("shareIncomeCommonWVS", common_shares["WVS", "share_income"], 2, percent = TRUE)
p_common <- ggplot(common, aes(x = gdp_ppp, y = gap)) + geom_hline(yintercept = 0, color = "grey") + geom_point(aes(color = region, shape = region)) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5, formula = y ~ x) + geom_text_repel(aes(color = region, label = paste0(code, substr(year, 3, 4))), size = 1.8, show.legend = FALSE, max.overlaps = 20) +
  scale_x_log10(labels = scales::label_comma()) + scale_color_manual(values = region_colors) + scale_shape_manual(values = region_shapes) +
  labs(x = "GDP per capita, PPP (constant 2021 $, log scale)", y = "Gallup ladder minus WVS satisfaction", color = NULL, shape = NULL) +
  theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 9))
ggsave(paste0(figures_folder, "gap_gallup_wvs_vs_gdp.pdf"), p_common, width = 7, height = 4.5)
ggsave(paste0(figures_folder, "gap_gallup_wvs_vs_gdp.png"), p_common, width = 7, height = 4.5, dpi = 200)


##### 4. Export numbers #####
writeLines(paste0("\\newcommand{\\", names(numbers), "}{", unlist(numbers), "\\xspace}"), paste0(tables_folder, "numbers.tex"))
writeLines(capture.output(sessionInfo()), paste0(tables_folder, "session_info.txt"))
