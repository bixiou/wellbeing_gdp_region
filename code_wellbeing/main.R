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
#    1.2 GDP p.c. and population from the World Bank API (latest vintage, cached in ../data/). Missing
#        values are filled automatically (replacing old manual imputations): PPP series are back-casted with
#        the growth of constant-$ GDP; countries absent from the World Bank (Taiwan, recent Venezuela...)
#        use IMF WEO data converted to constant $ with the U.S. deflator. Flags keep track of imputations.
#    1.3 WVS (waves 1-7, 1981-2022): country-year well-being indicators from the microdata (survey weights).
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
packages <- c("dplyr", "tidyr", "readxl", "openxlsx", "ggplot2", "ggrepel", "sandwich", "lmtest", "fixest", "jsonlite", "countrycode", "rpart")
missing_packages <- packages[!packages %in% rownames(installed.packages())]
if (length(missing_packages)) install.packages(missing_packages)
invisible(lapply(packages, library, character.only = TRUE))

set.seed(20250415) # Used for k-means and bootstrap
refresh_downloads <- FALSE # TRUE to re-download World Bank / IMF data instead of using the cached files in ../data/
n_bootstrap <- 1000
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
  if (is.numeric(value)) value <- if (percent) format(round(100 * value, max(0, digits - 2)), nsmall = max(0, digits - 2)) else format(round(value, digits), nsmall = digits)
  numbers[[name]] <<- as.character(value)
}
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
  for (j in seq_along(df)) if (is.numeric(df[[j]])) df[[j]] <- ifelse(is.na(df[[j]]), "", formatC(df[[j]], format = "f", digits = digits))
  if (is.null(align)) align <- paste0("l", strrep("c", ncol(df) - 1))
  if (is.null(header)) header <- paste(names(df), collapse = " & ")
  rows <- apply(df, 1, function(r) paste(r, collapse = " & "))
  body <- unlist(lapply(seq_along(rows), function(i) c(if (i %in% midrule_before) "\\midrule", paste(rows[i], "\\\\"))))
  writeLines(c(paste0("\\begin{tabular}{", align, "}"), "\\toprule", paste(header, "\\\\"), "\\midrule", body, "\\bottomrule", "\\end{tabular}"), paste0(tables_folder, file))
}

#' Download a file once and cache it (re-download if refresh_downloads is TRUE)
#' @param url URL.
#' @param file Local path.
#' @return The local path.
cached_download <- function(url, file) {
  if (refresh_downloads || !file.exists(file)) download.file(url, file, quiet = TRUE, mode = "wb")
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
#' Download a World Bank WDI indicator for all countries (cached)
#' @param indicator WDI code, e.g. "NY.GDP.PCAP.PP.KD".
#' @return Data frame with columns code, year, value.
get_wdi <- function(indicator) {
  file <- cached_download(paste0("https://api.worldbank.org/v2/country/all/indicator/", indicator, "?format=json&date=1960:2025&per_page=20000"), paste0(data_folder, "wdi_", indicator, ".json"))
  raw <- fromJSON(file)[[2]]
  data.frame(code = raw$countryiso3code, year = as.integer(raw$date), value = raw$value) |> filter(code != "", !is.na(value))
}

#' Download an IMF World Economic Outlook indicator from the IMF DataMapper API (cached)
#' @param indicator IMF code, e.g. "PPPPC" (GDP p.c. PPP, current international $) or "NGDPDPC" (GDP p.c., current $).
#' @return Data frame with columns code, year, value.
get_imf <- function(indicator) {
  file <- cached_download(paste0("https://www.imf.org/external/datamapper/api/v1/", indicator), paste0(data_folder, "imf_", indicator, ".json"))
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


##### 1.3 WVS #####
wvs <- readRDS(paste0(data_folder, "WVS.rds")) |>
  transmute(wave = as.integer(s002), year = as.integer(s020), code = unname(iso3_of_iso2[as.character(s009)]), weight = as.numeric(s018),
            happiness = as.numeric(a008), satisfaction = as.numeric(a170), god = as.numeric(f063), homosexuality = as.numeric(f118),
            freedom = as.numeric(a173), democracy = as.numeric(e235)) # as.numeric() drops the haven labels
for (v in c("happiness", "satisfaction", "god", "homosexuality", "freedom", "democracy")) wvs[[v]][wvs[[v]] < 0] <- NA # negative codes: DK, refusal, not asked

wellbeing_variables <- c("happy", "very_happy", "very_unhappy", "very_happy_minus_very_unhappy", "happiness_mean", "satisfied_mean", "satisfied", "happiness_layard")
satisfaction_variables <- c("satisfied", "very_satisfied", "extremely_satisfied", "completely_satisfied", "unsatisfied", "dissatisfied", "satisfied_mean")
wellbeing_names <- c("happy" = "Happy", "very_happy" = "Very Happy", "very_unhappy" = "Very Unhappy", "very_happy_minus_very_unhappy" = "V. Happy -- V. Unhappy",
                     "happiness_mean" = "Happiness (mean)", "satisfied_mean" = "Satisfaction (mean)", "satisfied" = "Satisfied", "happiness_layard" = "Happy + Satisfied",
                     "very_satisfied" = "Very satisfied (8--10)", "extremely_satisfied" = "Extremely satisfied (9--10)", "completely_satisfied" = "Completely satisfied (10)",
                     "unsatisfied" = "Unsatisfied (0/1--4)", "dissatisfied" = "Dissatisfied (0/1--2)", "low_satisfaction" = "Below 60\\% of mean satisfaction")

wvs_cy <- wvs |> group_by(code, year, wave) |> summarise(
  n = n(),
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
         happiness_layard = (happy + satisfied) / 2, source = "WVS", non_pandemic = !year %in% 2020:2021) |>
  mutate(across(c(god, homosexuality, freedom, democracy), ~ ifelse(is.nan(.x), NA, .x)))
wvs_cy <- add_gdp(wvs_cy) |> mutate(region = region5_of(code), country = country_of_iso3[code])
wvs_cy <- wvs_cy |> group_by(code) |> mutate(last_year = max(year)) |> ungroup()


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
                "wvs_wave5" = "WVS, wave 5 (2004--09)", "wvs_wave6" = "WVS, wave 6 (2010--16)", "wvs_wave7" = "WVS, wave 7 (2017--22)", "wvs_wave7_no_pandemic" = "WVS, wave 7 w/o 2020--21",
                "wvs_weighted" = "WVS, population-weighted", "wvs_last" = "WVS, last obs. per country", "wvs_no_imputation" = "WVS, no GDP imputation",
                "wvs_gdp_2017_vintage" = "WVS, GDP PPP 2017 \\$ (old)", "wvs_no_latam_ee" = "WVS, w/o Latin Am. \\& E. Europe", "wvs_region6" = "WVS, 6 regions",
                "wvs_region_wb" = "WVS, World Bank regions (7)", "wvs_region_continent" = "WVS, continents (5)", "wvs_region_un_sub" = "WVS, UN sub-regions",
                "gallup" = "Gallup, all years (2006--23)", "gallup_last" = "Gallup, last obs. per country", "gallup_wvs_countries" = "Gallup, last obs., WVS countries",
                "gallup_region6" = "Gallup, last obs., 6 regions", "gallup_region_wb" = "Gallup, last obs., WB regions", "whr_2025" = "WHR, 2023--25 average", "wvs_satisfaction" = "WVS, satisfaction indicators")

results <- bind_rows(lapply(specs, run_spec))
write.csv(results, paste0(tables_folder, "region_vs_income_all_results.csv"), row.names = FALSE)
