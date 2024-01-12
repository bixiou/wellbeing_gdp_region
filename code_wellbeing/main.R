# TODO: latex tables
# TODO: (non urgent) ajouter d'autres variables explicatives, e.g. croissance, revenu médian
# TODO: robustness check: redo analysis with UN regional groups (i.e. with Middle East and Central Asia in Asia); without Latin America and Ex-Eastern Block
# DONE: (not found) trouver définitions de region qui correspondent aux nôtres. UN regional groups: 5 regions (Middle East and Central Asia in Asia) https://en.wikipedia.org/wiki/United_Nations_Regional_Groups
# DONE: enlever les pays 2020-2021 comme robustness check
# DONE: regarder si les donnees de GDP correspondent a la date de l'enquete, et le faire le cas echeant
# DONE: poids
# DONE: graphiques
# DONE: reproduire l'analyse économétrique


##### Data preparation #####
# WVS7 <- read_csv("../data/WVS7.csv")
# w7 <- WVS7[, c("Q46", "Q49", "GDPpercap2", "B_COUNTRY_ALPHA", "W_WEIGHT")]
# decrit("region", w7)
# names(w7) <- c("happiness", "satisfaction", "gdp", "code", "weight")
# 
# # adding missing values
# w7$gdp_pc[w7$code == "AND"] <- 42903
# w7$gdp_pc[w7$code == "IRN"] <- 13338
# w7$gdp_pc[w7$code == "NIR"] <- 32226
# w7$gdp_pc[w7$code == "TWN"] <- 25903
# w7$gdp_pc[w7$code == "VEN"] <- 6106
# w7$region[w7$code == "NIR"] <- "Western"
# w7$region[w7$code == "MAC"] <- "Asia"
# w7 <- w7 %>%  mutate(region = sapply(code, get_country_region))
# write.csv(w7, "../data/WVS7.csv", row.names = FALSE)

start <- Sys.time()
kvalues <- 5:7
country_mapping <- read.csv("../data/country_code_mapping.csv")
country <- setNames(country_mapping$country, country_mapping$alpha.2)
code <- setNames(country_mapping$code, country_mapping$alpha.2)
wvs <- readRDS("../data/WVS.rds") # All waves concatenated https://www.worldvaluessurvey.org/WVSEVStrend.jsp
wvs <- wvs %>% rename(s002 = wave, s020 = year, s009 = alpha.2, a008 = happiness, a170 = satisfaction, c006 = financial_satisfaction, d002 = home_satisfaction, s018 = weight, pwght = pop_weight) # cow_alpha = code, 
wvs$country <- country[wvs$alpha.2]
wvs$code <- code[wvs$alpha.2]
region <- list(
  "Africa" = c("BFA", "DZA", "ETH", "GHA", "KEN", "LBY", "MAR", "MLI", "NGA", "RWA", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE"), # 16
  "Latin America" = c("ARG", "BOL", "BRA", "CHL", "COL", "DOM", "ECU", "GTM", "HTI", "MEX", "NIC", "PER", "PRI", "SLV", "TTO", "URY", "VEN"), # 17
  "Ex-Eastern Block" = c("ALB", "ARM", "AZE", "BIH", "BGR", "BLR", "CZE", "EST", "GEO", "HRV", "HUN", "KAZ", "KGZ", "LTU", "LVA", "MDA", "MKD", "MNE", "POL", "ROU", "RUS", "SRB", "SVK", "SVN", "TJK", "UKR", "UZB"), # 27
  "Middle East" = c("EGY", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "PSE", "QAT", "SAU", "TUR", "YEM"), # 12 Israel in it?
  "Western" = c("AND", "AUS", "CAN", "CHE", "CYP", "DEU", "ESP", "FIN", "FRA", "GBR", "GRC", "ITA", "NIR", "NLD", "NOR", "NZL", "SWE", "USA"), # 18
  "Asia" = c("BGD", "CHN", "HKG", "IDN", "IND", "JPN", "KOR", "MAC", "MDV", "MMR", "MNG", "MYS", "PAK", "PHL", "SGP", "THA", "TWN", "VNM") # 18
)
region_mapping <- c()
for (reg in names(region)) for (i in region[[reg]]) region_mapping <- c(region_mapping, setNames(reg, i))
wvs$region <- region_mapping[wvs$code]
wvs <- wvs[, c("wave", "alpha.2", "weight", "pop_weight", "year", "happiness", "satisfaction", "financial_satisfaction", "home_satisfaction", "country", "code", "region")]

# /!\ Imputations (esp. for pre-90 PPP) use IMF data which are not in 17$ but in current $, hence they are lower than WB estimates => perhaps it's better to exclude missing data rather than using imputed one.
GDPpcPPP <- read.csv("../data/GDPpcPPP17.csv" , sep = ",") # GDP pc PPP constant 2017 $, World Bank (2023-07-25) NY.GDP.PCAP.PP.KD Completed from IMF for pre-1990, YEM14, AND, TWN and VEN. https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)_per_capita - in /deprecated there is the original data without manual imputations (for AND05 I use HKG05 instead, SVK90: 8k, MNE96: MNE97, POL89: 7k)
GDPpc <- read.csv("../data/GDPpc15.csv" , sep = ",") # GDP pc nominal constant 2015 $, World Bank (2023-07-25) NY.GDP.PCAP.KD, completed manually for missing data (see below) - in /deprecated there is the original data without manual imputations TODO: compute and automatize IMF constant nominal $ (for the moment we use current nominal $ instead), cf. deprecated/IMF23

for (c in intersect(wvs$code, GDPpcPPP$Country.Code)) for (y in unique(wvs$year[wvs$code == c])) {
  gdp_pc_ppp_c <- GDPpcPPP[GDPpcPPP$Country.Code == c, paste0("X",  y)]
  if (!is.na(gdp_pc_ppp_c)) wvs$gdp_ppp[wvs$code ==  c & wvs$year == y]  <- gdp_pc_ppp_c
  else print(paste(c, y, "ppp")) # (c, y) with missing data are printed (cf. below)
  gdp_pc_c <- GDPpc[GDPpc$Country.Code == c, paste0("X",  y)]
  if (!is.na(gdp_pc_c)) wvs$gdp[wvs$code ==  c & wvs$year == y]  <- gdp_pc_c
  else print(paste(c, y)) # (c, y) with missing data are printed: SVK90, VEN96,00,21, HUN82, MNE96, POL89 completed with first year available except for VEN: with IMF GDP pc at current prices https://www.imf.org/external/datamapper/PPPPC@WEO/OEMDC/ADVEC/WEOWORLD/VEN
} 
wvs$gdp_na <- wvs$gdp # Version of GDP pc (PPP) where we take out missing data (instead of using imputed one)
wvs$gdp_na[paste0(wvs$code, wvs$year) %in% c("SVK1990", "VEN2021", "VEN1996", "VEN2000", "HUN1982", "MNE1996", "POL1989", "TWN1998", "TWN2006", "TWN2012", "TWN2019", "NIR2022")] <- NA
wvs$gdp_ppp_na <- wvs$gdp_ppp
wvs$gdp_ppp_na[paste0(wvs$code, wvs$year) %in% c("AND2018", "AND2005", "ARG1984", "AUS1981", "JPN1981", "KOR1982", "MEX1981", "SVK1990", "VEN2021", "VEN1996", "VEN2000", "FIN1981", "HUN1982", "MNE1996", "POL1989", "ZAF1982", "CHE1989", "YEM2014", "TWN1998", "TWN2006", "TWN2012", "TWN2019", "NIR2022")] <- NA

a <- wvs %>% group_by(code, year) %>% 
  dplyr::summarize(very_happy = weighted.mean(happiness[happiness > 0] == 1, weight[happiness > 0]),
                   happy = weighted.mean(happiness[happiness > 0] < 3, weight[happiness > 0]),
                   very_unhappy = weighted.mean(happiness[happiness > 0] == 4, weight[happiness > 0]),
                   very_happy_minus_very_unhappy = min(1000, sum((happiness[happiness > 0] == 1) * weight[happiness > 0]) - sum((happiness[happiness > 0] == 4) * weight[happiness > 0])),
                   very_happy_over_very_unhappy = min(1000, sum((happiness[happiness > 0] == 1) * weight[happiness > 0]) / sum((happiness[happiness > 0] == 4) * weight[happiness > 0])),
                   satisfied = weighted.mean(satisfaction[satisfaction > 0] > 5, weight[satisfaction > 0]),
                   satisfied_mean = weighted.mean(satisfaction[satisfaction > 0], weight[satisfaction > 0]),
                   happiness_mean = weighted.mean(((5 - happiness[happiness > 0]) * 2 - 5), weight[happiness > 0]), # -3/-1/1/3
                   financial_satisfaction = weighted.mean(financial_satisfaction[financial_satisfaction > 0] > 5, weight[financial_satisfaction > 0]), home_satisfaction = weighted.mean(home_satisfaction[home_satisfaction > 0] > 5, weight[home_satisfaction > 0]),
                   gdp = unique(gdp), gdp_ppp = unique(gdp_ppp), gdp_na = unique(gdp_na), gdp_ppp_na = unique(gdp_ppp_na), region = unique(region), wave = unique(wave), alpha.2 = unique(alpha.2), country = unique(country),
  )
a$happiness_Layard <- (a$happy + a$satisfied)/2
a$non_pandemic <- !a$year %in% c(2020, 2021)

pop <- read.xlsx("../data/pop.xlsx") # UN World Population Prospect 2022 GEN/01/REV1 https://population.un.org/wpp/Download/Standard/MostUsed/ I have manually copied 2022 figures into the spreadsheet (from the sheet medium projection to past estimate's): CZE, GBR, LBY, NIR, NLD, SVK, URY
for (c in unique(a$code)) for (y in a$year[a$code == c]) {
  pop_cy <- 1e3 * as.numeric(pop$pop[no.na(pop$code) == c & no.na(pop$year) == y])
  if (length(pop_cy)) a$pop[a$code == c & a$year == y] <- pop_cy
  else print(paste(c, y)) # (c, y) with missing data are printed
} 

create_gdp_vars <- function(var = "gdp_ppp", waves = 1:7, k_values = kvalues, pandemic_years = TRUE, data = a) {
  a <- data[data$wave %in% waves,]
  if (!pandemic_years) a <- a[a$non_pandemic == T,]
  a[[paste0("log_", var)]] <- log10(a[[var]])
  a[[paste0("ranked_", var)]] <- rank(a[[var]])
  a[[paste0("group_", var)]] <- as.character(cut(a[[paste0("ranked_", var)]], breaks = 6, labels = FALSE))
  #1 being the lowest and 6 being the highest gdp group (corresponding to the y_6ile variable in the previous paper)
  # k is the number of bins / GDP clusters
  for (k in k_values) {
    kmeans_total <- kmeans(a[[paste0("log_", var)]][!is.na(a[[paste0("log_", var)]])], centers = k)
    a[[paste0("kmeans_", var, "_", k)]][!is.na(a[[paste0("log_", var)]])] <- as.character(kmeans_total$cluster)
  }
  return(a)
}
for (var in c("gdp", "gdp_ppp", "gdp_na", "gdp_ppp_na")) a <- create_gdp_vars(var, waves = 1:7, k_values = kvalues, pandemic_years = TRUE, data = a)


##### regressions #####
happiness_variables <- c("very_happy", "happy", "very_unhappy", "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard", "very_happy_minus_very_unhappy") 
hapiness_names <- setNames(c("Very Happy", "Happy", "Very Unhappy", "Satisfied", "Satisfaction (mean)", "Happiness (mean)", "Happy-Satisfied", "Very Happy minus Very Unhappy"), happiness_variables)

run_regressions <- function(var_gdp = "gdp_ppp", waves = 1:7, weight = FALSE, pandemic_years = TRUE, only_last = FALSE, happiness_vars = happiness_variables, data = a, return = "var_explained") {
  # if (!pandemic_years) data <- create_gdp_vars(pandemic_years = FALSE)
  data <- create_gdp_vars(var = var_gdp, waves = waves, k_values = kvalues, pandemic_years = pandemic_years, data = a)
  if (only_last) for (c in data$code) data <- data[!(data$code == c & data$year != max(data$year[data$code == c])),]
  
  weights <- if (weight) data$pop else NULL
  lgdp <- paste0("log_", var_gdp)
  
  regressions <- list()
  for (i in happiness_vars) { 
    regressions[[i]] <- list("region" = lm(as.formula(paste(i, "~ region")), data = data, weights = weights),
                             "log_gdp" = lm(as.formula(paste(i, "~ ", lgdp)), data = data, weights = weights),
                             "log_gdp_quadratic" = lm(as.formula(paste(i, "~ ", lgdp, " + I(", lgdp, "^2)")), data = data, weights = weights),
                             "gdp_group" = lm(as.formula(paste0(i, "~ group_", var_gdp)), data = data, weights = weights), 
                             "region_log_gdp" = lm(as.formula(paste(i, "~ ", lgdp, " + as.factor(region)")), data = data, weights = weights),
                             "region_log_gdp_quadratic" = lm(as.formula(paste(i, "~ ", lgdp, " + I(", lgdp, "^2) + as.factor(region)")), data = data, weights = weights),
                             "region_gdp_group" = lm(as.formula(paste0(i, "~ group_", var_gdp, " + as.factor(region)")), data = data, weights = weights))
                             # "log_gdp_ppp" = lm(as.formula(paste(i, "~ log_gdp_ppp")), data = data, weights = weights),
                             # "log_gdp_ppp_quadratic" = lm(as.formula(paste(i, "~ log_gdp_ppp + I(log_gdp_ppp^2)")), data = data, weights = weights),
                             # "gdp_ppp_group" = lm(as.formula(paste(i, "~ gdp_ppp_group")), data = data, weights = weights), 
                             # "region_log_gdp_ppp" = lm(as.formula(paste(i, "~ log_gdp_ppp + as.factor(region)")), data = data, weights = weights),
                             # "region_log_gdp_ppp_quadratic" = lm(as.formula(paste(i, "~ log_gdp_ppp + I(log_gdp_ppp^2) + as.factor(region)")), data = data, weights = weights),
                             # "region_gdp_ppp_group" = lm(as.formula(paste(i, "~ gdp_ppp_group + as.factor(region)")), data = data, weights = weights))
    for (k in kvalues) regressions[[i]][[paste0("gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ kmeans_", var_gdp, "_", k)), data = data, weights = weights)
    for (k in kvalues) regressions[[i]][[paste0("region_gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ kmeans_", var_gdp, "_", k, " + as.factor(region)")), data = data, weights = weights)
    
    # for (k in kvalues) regressions[[i]][[paste0("gdp_ppp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_ppp_cluster", k)), data = data, weights = weights)
    # for (k in kvalues) regressions[[i]][[paste0("region_gdp_ppp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_ppp_cluster", k, " + as.factor(region)")), data = data, weights = weights)
  }
  
  result_tables <- list()
  combined_results <- data.frame() 
  for (j in names(regressions[[1]])) if (!grepl("region_", j)) {
    result_tables[[j]] <- list()
    for (i in happiness_vars) {
      result_tables[[j]] <- rbind(result_tables[[j]], glance(regressions[[i]][[j]]) %>% mutate(Dependent_Variable = i, Independent_Variable = j))
    }
    if (!grepl("region", j)) combined_results <- rbind(combined_results, result_tables[[j]])
  }
  combined_results_max <- combined_results %>% group_by(Dependent_Variable) %>% slice(which.max(r.squared))
  # print(combined_results_max)
  
  # Toy example:
  # reg_tot<- lm(happy ~ log_gdp + as.factor(region), data = a7)
  # variance_reg_tot <- calc.relimp(reg_tot, type = c("lmg"), rela = F, rank= F)
  # variance_reg_tot$lmg

  var_explained <- data.frame()
  for (j in names(regressions[[1]])) if (grepl("region_", j)) {
    for (i in happiness_vars) {
      variance_reg_tot <- calc.relimp(regressions[[i]][[j]], type = c("lmg"), rela = F, rank= F)
      var_explained <- rbind(var_explained, data.frame("var_happiness" = i, "var_gdp" = j,
                                                       "var_explained" = sum(variance_reg_tot$lmg), 
                                                       "var_explained_by_gdp" = variance_reg_tot$lmg[grepl("gdp", names(variance_reg_tot$lmg))], 
                                                       "var_explained_by_region" = variance_reg_tot$lmg[grepl("region", names(variance_reg_tot$lmg))],
                                                       "share_var_explained_by_gdp" = variance_reg_tot$lmg[grepl("gdp", names(variance_reg_tot$lmg))]/sum(variance_reg_tot$lmg)))
    }
  }
  rownames(var_explained) <- NULL
  
  if (return == "var_explained") return(var_explained)
  else if (return == "regressions") return(regressions)
  else if (return == "combined_results_max") return(combined_results_max)
  else if (return == "all") return(list(regressions, combined_results_max, var_explained))
}

# r6p <- run_regressions(var_gdp = "gdp_ppp", waves = 6, weight = FALSE) # reproduction of my original work
# r7p <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = FALSE, pandemic_years = T)
# r7p_wo_pandemic_years <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = FALSE, pandemic_years = F)
# r7p_weighted <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = T, pandemic_years = T)
# r7 <- run_regressions(var_gdp = "gdp", waves = 7, weight = FALSE, pandemic_years = T)
# r7p_na <- run_regressions(var_gdp = "gdp_ppp_na", waves = 7, weight = FALSE, pandemic_years = T)
# rp <- run_regressions(var_gdp = "gdp_ppp", waves = 1:7, weight = FALSE)
# r <- run_regressions(var_gdp = "gdp", waves = 1:7, weight = FALSE)
# r6 <- run_regressions(var_gdp = "gdp", waves = 6, weight = FALSE)
# r5p <- run_regressions(var_gdp = "gdp_ppp", waves = 5, weight = FALSE)
# r5 <- run_regressions(var_gdp = "gdp", waves = 5, weight = FALSE)
# r4p <- run_regressions(var_gdp = "gdp_ppp", waves = 4, weight = FALSE)
# r4 <- run_regressions(var_gdp = "gdp", waves = 4, weight = FALSE)
# r3p <- run_regressions(var_gdp = "gdp_ppp", waves = 3, weight = FALSE)
# r3 <- run_regressions(var_gdp = "gdp", waves = 3, weight = FALSE)
# r2p <- run_regressions(var_gdp = "gdp_ppp", waves = 2, weight = FALSE)
# r2 <- run_regressions(var_gdp = "gdp", waves = 2, weight = FALSE)
# r12p <- run_regressions(var_gdp = "gdp_ppp", waves = 1:2, weight = FALSE)
# r12 <- run_regressions(var_gdp = "gdp", waves = 1:2, weight = FALSE)

# Naming: r: regular / w: weighted; #waves (or no number if all waves); p if PPP; _only_last if keep only last obs. for a given country; _wo_pandemic_years if pandemic_years = F; _na if GDP variable is .._na (i.e. doesn't impute values missing from WB data)
reg_tables <- list() 
for (w in c(2:7, list(c(1:7), c(1:2), c(3:5)))) for (var_gdp in c("gdp", "gdp_ppp")) {
  table_name <- paste0("r", if (length(w)==7) "" else paste0(w, collapse = ""), if (var_gdp=="gdp_ppp") "p" else "")
  reg_tables[[table_name]] <- run_regressions(var_gdp = var_gdp, waves = w, weight = FALSE)
}
reg_tables$rp_weighted <- run_regressions(var_gdp = "gdp_ppp", waves = 1:7, weight = T, pandemic_years = T)
reg_tables$rp_na <- run_regressions(var_gdp = "gdp_ppp_na", waves = 1:7, weight = FALSE, pandemic_years = T)
reg_tables$rp_only_last <- run_regressions(var_gdp = "gdp_ppp", waves = 1:7, weight = FALSE, only_last = T) 
reg_tables$r7p_wo_pandemic_years <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = FALSE, pandemic_years = F)
reg_tables$r7p_weighted <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = T, pandemic_years = T)
reg_tables$r7p_na <- run_regressions(var_gdp = "gdp_ppp_na", waves = 7, weight = FALSE, pandemic_years = T)
reg_tables$r345p_only_last_na <- run_regressions(var_gdp = "gdp_ppp_na", waves = 3:5, weight = FALSE, only_last = T)
reg_tables$r345_only_last <- run_regressions(var_gdp = "gdp", waves = 3:5, weight = FALSE, only_last = T)
reg_tables$r345p_only_last <- run_regressions(var_gdp = "gdp_ppp", waves = 3:5, weight = FALSE, only_last = T) # reproduction of my original work
reg_tables$r_only_last <- run_regressions(var_gdp = "gdp", waves = 1:7, weight = FALSE, only_last = T)

(max_share_var_explained_by_gdp <- round(sapply(names(reg_tables), function(n) n = max(reg_tables[[n]]$share_var_explained_by_gdp)), 3))
(argmax_share_var_explained_by_gdp <- sapply(names(reg_tables), function(n) n = paste(reg_tables[[n]]$var_happiness, reg_tables[[n]]$var_gdp)[which.max(reg_tables[[n]]$share_var_explained_by_gdp)]))
(mean_share_var_explained_by_gdp <- round(sapply(names(reg_tables), function(n) n = mean(reg_tables[[n]]$share_var_explained_by_gdp)), 3))
(share_var_explained_more_by_gdp <- round(sapply(names(reg_tables), function(n) n = mean(reg_tables[[n]]$share_var_explained_by_gdp > 0.5)), 3))
(mean_var_explained_by_gdp <- round(sapply(names(reg_tables), function(n) n = mean(reg_tables[[n]]$var_explained_by_gdp)), 3))
mean(mean_share_var_explained_by_gdp[grepl("p", names(mean_share_var_explained_by_gdp))]) # 23%
mean(mean_share_var_explained_by_gdp[!grepl("p", names(mean_share_var_explained_by_gdp))]) # 29%: nominal explains better
(mean_var_explained_by_gdp_variables <- sort(round(rowSums(sapply(names(reg_tables), function(n) sapply(unique(reg_tables[[n]]$var_gdp), function(v) v = mean(reg_tables[[n]]$var_explained_by_gdp[reg_tables[[n]]$var_gdp == v])))), 3)))
# (mean_var_explained_by_gdp_variables <- round(sapply(names(reg_tables), function(n) sapply(unique(reg_tables[[n]]$var_gdp), function(v) v = mean(reg_tables[[n]]$var_explained_by_gdp[reg_tables[[n]]$var_gdp == v]))), 3)) # shows that higher k generally means higher R² but not always (e.g. r6 k=4>5), and that R² is generally higher for cluster7 than gdp_group (incl. for r, rp)

# run_all <- function(wave = 7, happiness_vars = happiness_variables, weight = FALSE, pandemic_years = TRUE, return = "var_explained") {
#   a <- create_happiness_vars(wave)
#   a <- create_gdp_vars(wave)
#   res <- run_regressions(happiness_vars = happiness_vars, weight = weight, pandemic_years = pandemic_years, data = a, return = return, PPP = PPP)
#   return(res)
# }


##### Table Variance explained by GDP #####
gdp_variables <- c("log_gdp", "log_gdp_nominal", "gdp_group", "gdp_cluster5", "gdp_cluster6", "gdp_cluster7", "gdp_cluster7_nominal") # , c("log GDP p.c. PPP", "GDP sextile", "GDP cluster (k=5)"
supp_specs <- c("rp_weighted", "rp_na", "rp_only_last", "r12p", "r3p", "r4p", "r5p", "r6p", "r7p", "r7p_wo_pandemic_years") 
add_specs <- c("rp_weighted", "r12p", "r3p", "r4p", "r5p", "r6p", "r7p") 
add2_specs <- c("rp_weighted", "rp_na", "rp_only_last", "r7p_wo_pandemic_years") # not_log quadratic

latex_names <- c("very_happy_minus_very_unhappy" = "V. Happy -- V. Unhappy", "happiness_Layard" = "Happy + Satisfied", hapiness_names, 
                       "log_gdp" = "\\makecell{log GDP p.c.\\\\PPP}", "gdp_group" = "\\makecell{quantile\\\\(sextile)}",
                       "gdp_cluster5" = "k = 5", "gdp_cluster6" = "k = 6", "gdp_cluster7" = "k = 7", "log_gdp_nominal" = "\\makecell{log GDP p.c.\\\\nominal}",
                       "gdp_cluster7_nominal" = "\\makecell{cluster (k = 7)\\\\nominal}",
                       "mean" = "Mean", "max" = "Max", "Number of country $\\times$ wave",
                       "rp_weighted" = "\\makecell{All waves\\\\Population\\\\weighted}", "rp_na" = "\\makecell{All waves\\\\Without missing\\\\GDP imputation}", 
                       "rp_only_last" = "\\makecell{Only last\\\\wave for\\\\each country}", "r12p" = "\\makecell{Waves 1 \\& 2\\\\(1981-1991)}", 
                       "r3p" = "\\makecell{Wave 3\\\\(1995-1999)}", "r4p" = "\\makecell{Wave 4\\\\(1999-2004)}", "r5p" = "\\makecell{Wave 5\\\\(2004-2009)}", 
                       "r6p" = "\\makecell{Wave 6\\\\(2010-2016)}", "r7p" = "\\makecell{Wave 7\\\\(2017-2022)}", "r7p_wo_pandemic_years" = "\\makecell{Wave 7\\\\without pandemic\\\\(2020-2021)}")
latex_short_names <- c("log_gdp" = "\\makecell{\\,\\\\PPP}", "log_gdp_nominal" = "\\makecell{\\,\\\\nominal}", "gdp_group" = "\\makecell{sextile\\\\PPP}", 
                       "gdp_cluster5" = "\\makecell{k = 5\\\\PPP}", "gdp_cluster6" = "\\makecell{k = 6\\\\PPP}", "gdp_cluster7" = "\\makecell{k = 7\\\\PPP}", 
                       "n_obs" = "Number of obs.", "gdp_cluster7_nominal" = "\\makecell{k = 7\\\\nominal}", 
                       "rp_weighted" = "\\makecell{Pop.\\\\weight}", "r12p" = "\\makecell{1 \\& 2}", 
                       "r3p" = "\\makecell{3}", "r4p" = "\\makecell{4}", "r5p" = "\\makecell{5}", 
                       "r6p" = "\\makecell{6}", "r7p" = "\\makecell{7}", "r7p_wo_pandemic_years" = "\\makecell{Wave 7\\\\without pandemic\\\\(2020-2021)}", latex_names)

mean_max_table <- function(out_var, col_vars, default_gdp = "gdp_cluster7", happiness_vars = happiness_variables, n_obs = T, export = TRUE, filename = NULL, caption = NULL) {
  table <- matrix(nrow = length(happiness_vars)+2, ncol = length(col_vars)+2, dimnames = list(c(happiness_vars, "mean", "max"), c(col_vars, "mean", "max")))
  Ns <- c()
  for (g in col_vars) {
    tab <- if (grepl("gdp", g)) { if (grepl("nominal", g)) "r" else "rp" } else g
    waves <- str_split_1(gsub("\\D", "", tab), pattern = "")
    if (length(waves) == 0) waves <- 1:7
    Ns <- c(Ns, sum(a$wave %in% waves & (if (grepl("wo_pandemic", g)) a$non_pandemic else T) & (if (grepl("_na", g)) { if (grepl("p_", g)) !is.na(a$gdp_ppp_na) else !is.na(a$gdp_na)} else T)))
    var_name <- if (grepl("gdp", g)) { if (grepl("nominal", g)) sub("_nominal", "", g) else g } else default_gdp
    table[happiness_vars, g] <- round(sapply(happiness_vars, function(h) reg_tables[[tab]][[out_var]][reg_tables[[tab]]$var_gdp == paste0("region_", var_name) & reg_tables[[tab]]$var_happiness == h]), 3)
  }
  table["mean",] <- round(colMeans(table, na.rm = T), 3)
  table[,"mean"] <- round(rowMeans(table, na.rm = T), 3)
  table["max",] <- sapply(1:ncol(table), function(j) max(table[1:(nrow(table)-2),j], na.rm = T))
  table[,"max"] <- sapply(1:nrow(table), function(i) max(table[i,1:(ncol(table)-2)], na.rm = T))
  if (n_obs) table <- rbind(round(table, 2), "n_obs" = c(Ns, "", ""))
  
  if (export) {
    latex <- table
    row.names(latex) <- latex_short_names[rownames(table)]
    if (deparse(substitute(col_vars)) == "gdp_variables") toprule <- "toprule Happiness variable & \\\\multicolumn{2}{c}{log GDP p.c.} & \\\\multicolumn{5}{c}{Income cluster} & & \\\\\\\\"
    else if (deparse(substitute(col_vars)) == "add_specs") toprule <- "toprule & All waves & \\\\multicolumn{6}{c}{Only selected waves} &  & \\\\\\\\"
    else toprule <- "toprule "
    if (is.null(filename)) filename <- paste0(if (grepl("^share_", out_var)) "share_" else "", "gdp", 
            if (deparse(substitute(col_vars)) == "add_specs") "_add" else { if (deparse(substitute(col_vars)) == "supp_specs") "_supp" else "" } )
    
    cat(sub("toprule", toprule, sub("\\nMean", " \\\\midrule \nMean", sub("\nNumber", " \\\\midrule \nNumber", paste(
      kbl(latex, "latex", caption = caption, label = filename, position = "h", escape = F, booktabs = T, align = "c",
          col.names = latex_short_names[colnames(latex)], linesep = rep("", nrow(latex)-1))
      , collapse=" \n")))), file = paste0("../tables/", filename, ".tex")) 
  }
  
  return(table)  
}
(table_gdp <- mean_max_table(out_var = "var_explained_by_gdp", col_vars = gdp_variables, happiness_vars = happiness_variables))
(table_share_gdp <- mean_max_table(out_var = "share_var_explained_by_gdp", col_vars = gdp_variables, happiness_vars = happiness_variables))
(table_gdp_supp <- mean_max_table(out_var = "var_explained_by_gdp", col_vars = supp_specs, happiness_vars = happiness_variables))
(table_share_gdp_supp <- mean_max_table(out_var = "share_var_explained_by_gdp", col_vars = supp_specs, happiness_vars = happiness_variables))
(table_gdp_add <- mean_max_table(out_var = "var_explained_by_gdp", col_vars = add_specs, happiness_vars = happiness_variables))
(table_share_gdp_add <- mean_max_table(out_var = "share_var_explained_by_gdp", col_vars = add_specs, happiness_vars = happiness_variables))

# DONE: Share of variance explained by GDP pc: by happiness variable x gdp variables; DONE then showing it holds for other defs and waves
# DONE: Happiest countries by indicator/wave => Table of occurrences by country
# DONE: Table Region is a better predictor of national well-being than income
# DONE: Table showing that results holds with other variables or waves

happiest_region <- happiest_country <- matrix(nrow = length(happiness_variables), ncol = 8, dimnames = list(happiness_variables, c(1:7, "all")))
for (h in happiness_variables) for (w in colnames(happiest_country)) {
  wave_col <- a$wave %in% if (w == "all") 1:7 else as.numeric(w)
  fun <- if (h == "very_unhappy") "which.min" else "which.max"
  happiest_country[h,w] <- a$country[wave_col][do.call(fun, list(a[[h]][wave_col]))]
  happiest_region[h,w] <- region_mapping[a$code[wave_col][do.call(fun, list(a[[h]][wave_col]))]]
}
happiest_country
sort(table(unlist(happiest_country)), decreasing = T)
sort(table(unlist(happiest_region)), decreasing = T)
sort(table(unlist(happiest_country[,3:ncol(happiest_country)])), decreasing = T)
sort(table(unlist(happiest_region[,3:ncol(happiest_region)])), decreasing = T)
sort(table(unlist(happiest_region[,"all"])), decreasing = T)


##### Plot #####
# Graphs with country names, R² in legend, and log scale for both x and specific y-variables
region_colors <- c("Africa" = "black", "Asia" = "purple", "Ex-Eastern Block" = "red", "Latin America" = "#4CAF50", "Middle East" = "#FFA000", "Western" = "#64B5F6") # purple: #a020f0; red: #ff0000
region_shapes <- c("Africa" = 15, "Asia" = 16, "Ex-Eastern Block" = 17, "Latin America" = 0, "Middle East" = 1, "Western" = 2) # , 3: +, 4: x ●▲■○△□

create_scatter_plot <- function(y_var, log_scale_y = FALSE, data = a, PPP = T, waves = 7, only_last = FALSE, size_pop = FALSE, legend = TRUE, label = "country", fontsize = 7, labelsize = 2, shape_region = TRUE) { 
  wave <- if (length(waves)>1) paste0(min(waves), " to ", max(waves)) else waves 
  df <- data[data$wave %in% waves,]
  if (only_last) for (c in df$code) df <- df[!(df$code == c & df$year != max(df$year[df$code == c])),]
  df$lab <- df[[label]] # country or code

  model <- lm(as.formula(paste(y_var, "~", paste0(if (log_scale_y) "log_" else "", if (PPP) "gdp_ppp" else "gdp"))), data = df, weights = if (size_pop) df$pop else NULL)
  rsquared <- summary(model)$r.squared
  name_legend <- paste0("Wave", if (length(waves)>1) "s" else "", " = ", wave, " (R² = ", round(rsquared, 2), ") ")

  p <- qplot(if (PPP) gdp_ppp else gdp, get(y_var), data = df, color = region, shape = if (shape_region) region else NULL,
             label = if (label == "code" & length(waves) > 2) paste0(lab, substr(year, 3, 4)) else {if (length(waves)>1) paste(lab, year) else lab}, 
             size = if (size_pop) pop else NULL, show.legend = T) + 
    geom_point() + scale_color_manual(values = region_colors, name = name_legend) + scale_x_log10(breaks = 10^(-10:10), minor_breaks = rep(1:9, 21)*(10^rep(-10:10, each=9))) + 
    labs(x = paste("GDP pc", if (PPP) "(PPP)" else ""), y = hapiness_names[y_var]) +
    theme_minimal() + theme(legend.position = if (legend) "bottom" else "none", legend.spacing.x = unit(0.05, unit = "cm"), plot.background = element_rect(fill = "white"), #legend.background = element_rect(fill = "white"), 
          axis.text = element_text(size = fontsize), legend.text = element_text(size = fontsize), legend.title = element_text(size = fontsize), axis.title = element_text(size = fontsize+2), plot.caption = element_text(size = fontsize))
  if (shape_region) p <- p + scale_shape_manual(values = region_shapes, labels = names(region_shapes), name = name_legend)
  if (log_scale_y) p <- p + scale_y_log10()
  
  p <- p + labs(color = paste0("Wave", if (length(waves)>1) "s" else "", " = ", wave, " (R² = ", round(rsquared, 2), ") "))
  p <- p + geom_text_repel(show.legend = FALSE, segment.size = 0.2, force = 4, point.padding = unit(0.2, "lines"), nudge_x = 0.005, nudge_y = 0.005, size = labelsize)
  p <- p + guides(color = guide_legend(nrow = 1)) + scale_size(guide = FALSE)
  print(p)
  return(p)
}
plot_all(waves = 1:2, vars = "happy")

plot_all <- function(waves = 7, PPP = T, size_pop = FALSE, only_last = FALSE, data = a, legend = TRUE, label = "country", fontsize = 7, labelsize = 2, shape_region = TRUE, vars = happiness_variables, width = 6, height = 4, format = "all") {
  for (v in vars) {
    p <- create_scatter_plot(y_var = v, log_scale_y = v %in% c("very_unhappy", "very_happy_over_very_unhappy"), data = data, PPP = PPP, waves = waves, only_last = only_last, size_pop = size_pop, legend = legend, label = label, fontsize = fontsize, labelsize = labelsize, shape_region = shape_region)
    filename <- paste0(v, "_vs_GDP", if (PPP) "ppp" else "", if (!identical(waves, 1:7)) paste(c("_wave", waves), collapse = "") else "", if (only_last) "only_last" else "", if (size_pop) "_weighted." else ".")
    if (format == "all") {
      ggsave(filename = paste0(filename, "pdf"), plot = p, path = "../figures", width = width, height = height, device = "pdf")
      ggsave(filename = paste0(filename, "png"), plot = p, path = "../figures", width = width, height = height, device = "png")
    } else ggsave(filename = paste0(filename, format), plot = p, path = "../figures", width = width, height = height, device = format)
  }
}
# start <- Sys.time() # 11 min
plot_all(waves = 7)
plot_all(waves = 6)
plot_all(waves = 5)
plot_all(waves = 4)
plot_all(waves = 3)
plot_all(waves = 1:2)
plot_all(waves = 1:7)
plot_all(waves = 7, PPP = FALSE)
plot_all(waves = 6, PPP = FALSE)
plot_all(waves = 5, PPP = FALSE)
plot_all(waves = 4, PPP = FALSE)
plot_all(waves = 3, PPP = FALSE)
plot_all(waves = 1:2, PPP = FALSE)
plot_all(waves = 1:7, PPP = FALSE)
plot_all(waves = 1:7, size_pop = T)
plot_all(waves = 7, size_pop = T)
plot_all(waves = 3:5, only_last = T)
Sys.time() - start
beep()

decrit("wave", a, weight = F) # 1:8(81-84) 2:18(89-91) 3:56(95-99) 4:40(99-04) 5:58(04-09) 6:60(10-16) 7:64(17-22) 
# for (i in 1:7) print(paste(min(a$year[a$wave == i]), max(a$year[a$wave == i])))

# for (var in c("gdp_ppp", "gdp", "gdp_ppp_na", "gdp_na")) {
#   a[[paste0("log_", var)]] <- log10(a[[var]])
#   a[[paste0("total_ranked_", var)]] <- rank(a[[var]])
#   for (w in unique(a$wave)) a[[paste0("wave_ranked_", var)]][a$wave == w] <- rank(a[[var]][a$wave == w])
#   a[[paste0("total_group_", var)]] <- as.character(cut(a[[paste0("total_ranked_", var)]], breaks = 6, labels = FALSE))
#   for (w in unique(a$wave)) a[[paste0("wave_group_", var)]][a$wave == w] <- as.character(cut(a[[paste0("wave_ranked_", var)]][a$wave == w], breaks = 6, labels = FALSE))
#   #1 being the lowest and 6 being the highest gdp group (corresponding to the y_6ile variable in the previous paper)
#   # clustered Y variables (corresponding to Y_clus4, Y_clus5, Y_clus6 and Y_clus7 in the previous paper)
#   for (k in k_values) {
#     kmeans_total <- kmeans(a[[paste0("log_", var)]][!is.na(a[[paste0("log_", var)]])], centers = k)
#     a[[paste0("total_kmeans_", var, "_", k)]][!is.na(a[[paste0("log_", var)]])] <- as.character(kmeans_total$cluster)
#     for (w in unique(a$wave)) if (sum(!is.na(a[[paste0("log_", var)]]) & a$wave == w) > k) {
#       kmeans_wave <- kmeans(a[[paste0("log_", var)]][!is.na(a[[paste0("log_", var)]]) & a$wave == w], centers = k)
#       a[[paste0("wave_kmeans_", var, "_", k)]][!is.na(a[[paste0("log_", var)]]) & a$wave == w] <- as.character(kmeans_wave$cluster)
#     }
#   }
# }

# get_country_region <- function(code) {
#   for (region_name in names(region)) {
#     if (code %in% region[[region_name]]) {
#       return(region_name)
#     }
#   }
#   return("Other")
# }
# 
# w7 <- read.csv("../data/WVS7.csv")
# info7 <- read.csv2("../data/WVSinfo7.csv")
# 
# w6 <- read_csv("../data/WVS6.csv")
# w6$weight <- 1
# info6 <- read.csv2("../data/WVSinfo6.csv")


# adding the weights and calculating the means
# create_happiness_vars <- function(wave = 7) {
#   if (wave == 7) {
#     data <- w7
#     info <- info7
#   } else {
#     data <- w6
#     info <- info6
#   }
#   a <- data %>% group_by(code) %>% 
#     dplyr::summarize(very_happy = weighted.mean(happiness[happiness > 0] == 1, weight[happiness > 0]),
#                      happy = weighted.mean(happiness[happiness > 0] < 3, weight[happiness > 0]),
#                      very_unhappy = weighted.mean(happiness[happiness > 0] == 4, weight[happiness > 0]),
#                      very_happy_over_very_unhappy = sum((happiness[happiness > 0] == 1) * weight[happiness > 0]) / sum((happiness[happiness > 0] == 4) * weight[happiness > 0]),
#                      satisfied = weighted.mean(satisfaction[satisfaction > 0] > 5, weight[satisfaction > 0]),
#                      satisfied_mean = weighted.mean(satisfaction[satisfaction > 0], weight[satisfaction > 0]),
#                      happiness_mean = weighted.mean(((5 - happiness[happiness > 0]) * 2 - 5), weight[happiness > 0]), # -3/-1/1/3
#                      # gdp = unique(gdp), region = unique(region)
#                      )
#   a$happiness_Layard <- (a$happy + a$satisfied)/2
#   a$region <- region_mapping[a$code]
#   # a <- a %>%  mutate(region = sapply(code, get_country_region))
#   a <- merge(a, info)
#   return(a)
# }
# a7 <- create_happiness_vars(7)
# a6 <- create_happiness_vars(6)


# TODO: where does this data come from? It needs to be automatized (and with the URL of the data source)
# population_data <- data.frame(
#   code = c("AND", "ARG", "ARM", "AUS", "BGD", "BOL", "BRA", "CAN", "CHL", "CHN", "COL", "CYP", "CZE", "DEU", "ECU", "EGY", "ETH", "GBR", "GRC", "GTM", "HKG", "IDN", "IRN", "IRQ", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KOR", "LEB", "LYB", "MAC", "MAR", "MDV", "MEX", "MMR", "MNG", "MYS", "NGA", "NIC", "NIR", "NLD", "NZL", "PAK", "PER", "PHL", "PRI", "ROU", "RUS", "SGP", "SRB", "SVK", "THA", "TJK", "TUN", "TUR", "TWN", "UKR", "URY", "USA", "VEN", "VNM", "ZWE" ),
#   population = c(75013, 44044811, 2790974, 24966643, 163683958, 11435533, 210166592, 38007166, 18701450, 1402760000, 49276961, 1228836, 10526073, 82905782, 17015672, 103740765, 117190911, 66971411, 10754679, 16604026, 7452600, 267066843, 87290193, 40590700, 10459865, 126633000, 18276452, 53005614, 6579900, 51585058, 5950839, 6812341, 663653, 37076584, 521457, 124013861, 53423198, 3347782, 32399271, 198387623, 6755895, 5086988, 17703090, 5124100, 219731479, 32203944, 110380804, 3325286, 19473970, 144496739, 5685807, 7020858, 5431752, 71127802, 9543207, 12049314, 82809304, 23777737, 44132049, 3422794, 325122128, 28199867, 96648685, 15669666)
# )
# a7 <- a7 %>% left_join(population_data, by = "code")
# write.csv(a7, "../data/a7.csv", row.names = FALSE)
# a7 <- read.csv("../data/a7.csv")

# econometric analysis
# rearranging alternative GDP variable  (Y^)
# create_gdp_vars <- function(wave = 7, k_values = kvalues, pandemic_years = TRUE) {
#   if (wave == 7) data <- a7
#   else if (wave == 6) data <- a6
#   if (!pandemic_years) data <- data[!data$year %in% c(2020, 2021), ]
#   for (c in intersect(data$code, GDPpcPPP$Country.Code)) {
#     gdp_pc_ppp_c <- GDPpcPPP[GDPpcPPP$Country.Code == c, paste0("X",  unique(data$year[data$code == c]))]
#     if (!is.na(gdp_pc_ppp_c)) data$gdp_ppp[data$code == c]  <- gdp_pc_ppp_c
#   }
#   for (c in intersect(data$code, GDPpc$Country.Code)) {
#     gdp_pc_c <- GDPpc[GDPpc$Country.Code == c, paste0("X",  unique(data$year[data$code == c]))]
#     if (!is.na(gdp_pc_c)) data$gdp[data$code == c]  <- gdp_pc_c
#   }
#   if (wave == 6) {
#     data$gdp[data$code == "TWN"] <- 21256
#     data$gdp[data$code == "LBN"] <- 8255
#     data$gdp[data$code == "YEM"] <- 8255 # TODO
#     data$gdp_ppp[data$code == "TWN"] <- 21256 # TODO
#     data$gdp_ppp[data$code == "YEM"] <- 8255 # TODO
#   } else if (wave == 7) {
#     data$gdp_ppp[data$code %in% c("AND", "NIR", "TWN", "VEN")] <- 999 # TODO
#     data$gdp[data$code %in% c("NIR", "TWN", "VEN")] <- 999 # TODO
#   }
#   for (var in intersect(c("gdp_ppp", "gdp"), names(data))) {
#     data[[paste0("log_", var)]] <- log10(data[[var]])
#     data[[paste0("ranked_", var)]] <- rank(data[[var]])
#     data[[paste0(var, "_group")]] <- as.character(cut(data[[paste0("ranked_", var)]], breaks = 6, labels = FALSE))
#     #1 being the lowest and 6 being the highest gdp group (corresponding to the y_6ile variable in the previous paper)
#     # clustered Y variables (corresponding to Y_clus4, Y_clus5, Y_clus6 and Y_clus7 in the previous paper)
#     cluster_assignments <- list()
#     for (k in k_values) {
#       kmeans_result <- kmeans(data[[paste0("log_", var)]], centers = k)
#       data[[paste0(var, "_cluster", k)]] <- as.character(kmeans_result$cluster)
#     }
#   }
#   return(data)
# }
# a7 <- create_gdp_vars(7)
# a6 <- create_gdp_vars(6)

# table_var_explained_by_gdp <- matrix(nrow = length(happiness_variables)+2, ncol = length(gdp_variables)+2, dimnames = list(c(happiness_variables, "mean", "max"), c(gdp_variables, "mean", "max")))
# for (g in gdp_variables) {
#   tab <- if (grepl("nominal", g)) "r" else "rp"
#   var_name <- paste0("region_", if (grepl("nominal", g)) sub("_nominal", "", g) else g)
#   table_var_explained_by_gdp[happiness_variables, g] <- round(sapply(happiness_variables, function(h) reg_tables[[tab]]$var_explained_by_gdp[reg_tables[[tab]]$var_gdp == var_name & reg_tables[[tab]]$var_happiness == h]), 3)
# }
# table_var_explained_by_gdp["mean",] <- round(colMeans(table_var_explained_by_gdp, na.rm = T), 3)
# table_var_explained_by_gdp[,"mean"] <- round(rowMeans(table_var_explained_by_gdp, na.rm = T), 3)
# table_var_explained_by_gdp["max",] <- sapply(1:ncol(table_var_explained_by_gdp), function(j) max(table_var_explained_by_gdp[1:(nrow(table_var_explained_by_gdp)-2),j], na.rm = T))
# table_var_explained_by_gdp[,"max"] <- sapply(1:nrow(table_var_explained_by_gdp), function(i) max(table_var_explained_by_gdp[i,1:(ncol(table_var_explained_by_gdp)-2)], na.rm = T))
# table_var_explained_by_gdp

# table_share_var_explained_by_gdp <- matrix(nrow = length(happiness_variables)+2, ncol = length(gdp_variables)+2, dimnames = list(c(happiness_variables, "mean", "max"), c(gdp_variables, "mean", "max")))
# for (g in gdp_variables) {
#   tab <- if (grepl("nominal", g)) "r" else "rp"
#   var_name <- paste0("region_", if (grepl("nominal", g)) sub("_nominal", "", g) else g)
#   table_share_var_explained_by_gdp[happiness_variables, g] <- round(sapply(happiness_variables, function(h) reg_tables[[tab]]$share_var_explained_by_gdp[reg_tables[[tab]]$var_gdp == var_name & reg_tables[[tab]]$var_happiness == h]), 3)
# }
# table_share_var_explained_by_gdp["mean",] <- round(colMeans(table_share_var_explained_by_gdp, na.rm = T), 3)
# table_share_var_explained_by_gdp[,"mean"] <- round(rowMeans(table_share_var_explained_by_gdp, na.rm = T), 3)
# table_share_var_explained_by_gdp["max",] <- sapply(1:ncol(table_share_var_explained_by_gdp), function(j) max(table_share_var_explained_by_gdp[1:(nrow(table_share_var_explained_by_gdp)-2),j], na.rm = T))
# table_share_var_explained_by_gdp[,"max"] <- sapply(1:nrow(table_share_var_explained_by_gdp), function(i) max(table_share_var_explained_by_gdp[i,1:(ncol(table_share_var_explained_by_gdp)-2)], na.rm = T))
# table_share_var_explained_by_gdp # happiness_Layard is the highest

# cat(sub("toprule", "toprule Happiness variable & \\\\textit{Income}: & Income & \\\\multicolumn{3}{c}{Income cluster} & & Income & & \\\\\\\\", sub("\\nMean", " \\\\midrule \nMean", sub("\nNumber", " \\\\midrule \nNumber", paste(
#   kbl(table, "latex", caption = NULL, label = NULL, position = "h", escape = F, booktabs = T, align = "c",
#       col.names = latex_short_names[colnames(table)], linesep = rep("", nrow(table)-1))
#   , collapse=" \n")))), file = "../tables/allocation.tex") 

# row.names(table) <- latex_short_names[rownames(table_gdp)]
# cat(sub("toprule", "toprule Happiness variable & \\\\multicolumn{2}{c}{log GDP p.c.} & \\\\multicolumn{5}{c}{Income cluster} & & \\\\\\\\", sub("\\nMean", " \\\\midrule \nMean", sub("\nNumber", " \\\\midrule \nNumber", paste(
#   kbl(table, "latex", caption = NULL, label = "gdp", position = "h", escape = F, booktabs = T, align = "c",
#       col.names = latex_short_names[colnames(table)], linesep = rep("", nrow(table)-1))
#   , collapse=" \n")))), file = "../tables/gdp.tex") 
# 
# row.names(table) <- latex_short_names[rownames(table_gdp_supp)]
# cat(sub("toprule", "toprule ", sub("\\nMean", " \\\\midrule \nMean", sub("\nNumber", " \\\\midrule \nNumber", paste(
#   kbl(table, "latex", caption = NULL, label = NULL, position = "h", escape = F, booktabs = T, align = "c",
#       col.names = latex_short_names[colnames(table)], linesep = rep("", nrow(table)-1))
#   , collapse=" \n")))), file = "../tables/gdp_supp.tex") 
# 
# row.names(table) <- latex_short_names[rownames(table_gdp_add)]
# cat(sub("toprule", "toprule & All waves & \\\\multicolumn{6}{c}{Only selected waves} &  & \\\\\\\\", sub("\\nMean", " \\\\midrule \nMean", sub("\nNumber", " \\\\midrule \nNumber", paste(
#   kbl(table, "latex", caption = NULL, label = NULL, position = "h", escape = F, booktabs = T, align = "c",
#       col.names = latex_short_names[colnames(table)], linesep = rep("", nrow(table)-1))
#   , collapse=" \n")))), file = "../tables/gdp_add.tex") 