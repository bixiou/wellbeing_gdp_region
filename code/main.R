# TODO: trouver définitions de region qui correspondent aux nôtres
# TODO: latex tables
# TODO: (non urgent) ajouter d'autres variables explicatives, e.g. croissance, revenu médian
# NOW: traiter les autres vagues
# TODO: latex tables
# TODO: (non urgent) ajouter d'autres variables explicatives, e.g. croissance, revenu médian
# TODO: robustness check: redo analysis with UN regional groups; without Latin America and Ex-Eastern Block
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
# names(w7) <- c("happiness", "satisfaction", "gdp", "country_code", "weight")
# 
# # adding missing values
# w7$gdp_pc[w7$country_code == "AND"] <- 42903
# w7$gdp_pc[w7$country_code == "IRN"] <- 13338
# w7$gdp_pc[w7$country_code == "NIR"] <- 32226
# w7$gdp_pc[w7$country_code == "TWN"] <- 25903
# w7$gdp_pc[w7$country_code == "VEN"] <- 6106
# w7$region[w7$country_code == "NIR"] <- "Western"
# w7$region[w7$country_code == "MAC"] <- "Asia"
# w7 <- w7 %>%  mutate(region = sapply(country_code, get_country_region))
# write.csv(w7, "../data/WVS7.csv", row.names = FALSE)

# defining the regions
region <- list(
  "Africa" = c("ETH", "MAR", "LBY", "NGA", "TUN", "ZWE", "KEN", "DZA", "GHA", "RWA", "ZAF"), # 11
  "Latin America" = c("ARG", "BOL", "BRA", "CHL", "NIC", "PRI", "URY", "COL", "ECU", "GTM", "MEX", "PER", "HTI", "TTO", "VEN"), # 15
  "Ex-Eastern Block" = c("ARM", "CZE", "KAZ", "ROU", "SRB", "TJK", "KGZ", "SVK", "RUS", "UKR", "AZE", "BLR", "EST", "SVN", "UZB", "POL"), # 16
  "Middle East" = c("EGY", "GEO", "IRN", "IRQ", "TUR", "JOR", "LBN", "KWT", "PSE", "QAT", "YEM"), # 11
  "Western" = c("CAN", "CYP", "USA", "DEU", "ESP", "SWE", "NIR", "NZL", "GRC", "GBR", "AUS", "AND", "NLD"), # 13
  "Asia" = c("THA", "PHL", "VNM", "SGP", "HKG", "TWN", "BGD", "CHN", "MYS", "KOR", "IND", "IDN", "PAK", "JPN", "MNG", "MMR", "MDV", "MAC") # 18
)

get_country_region <- function(country_code) {
  for (region_name in names(region)) {
    if (country_code %in% region[[region_name]]) {
      return(region_name)
    }
  }
  return("Other")
}

w7 <- read.csv("../data/WVS7.csv")
GDPpcPPP <- read.csv("../data/GDPpcPPP17.csv" , sep = ",", skip = 4) # GDP pc PPP constant 2017 $, World Bank (2023) NY.GDP.PCAP.PP.KD
info7 <- read.csv2("../data/WVSinfo7.csv")

GDPpc <- read.csv("../data/GDPpc15.csv" , sep = ",", skip = 4) # GDP pc nominal constant 2015 $, World Bank (2023) NY.GDP.PCAP.KD
GDPpc <- GDPpc[, c("Country.Code", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015")]
names(GDPpc) <- c("country_code", "2010", "2011", "2012", "2013", "2014", "2015")
info6 <- read.csv2("../data/WVSinfo6.csv")

w6 <- WVS6 <- read_csv("../data/WVS6.csv")
# w6 <- WVS6[, c("V10", "V23", "B_COUNTRY_ALPHA", "V258A")] # there was no variable for GDP in this survey, we add it later on the code
# names(w6) <- c("happiness", "satisfaction", "country_code", "weight")

gdp_long <- GDPpc %>% pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "gdp") %>% mutate(year = as.integer(year))
merged_data <- info6 %>% left_join(gdp_long, by = c("country_code", "year"))
w6 <- w6 %>% left_join(merged_data, by = "country_code") %>% mutate(gdp = ifelse(is.na(gdp), 0, gdp))
w6$weight <- 1

# adding the weights and calculating the means
create_happiness_vars <- function(wave = 7) {
  if (wave == 7) {
    data <- w7
    info <- info7
  } else {
    data <- w6
    info <- info6
  }
  a <- data %>% group_by(country_code) %>% 
    dplyr::summarize(very_happy = weighted.mean(happiness[happiness > 0] == 1, weight[happiness > 0]),
                     happy = weighted.mean(happiness[happiness > 0] < 3, weight[happiness > 0]),
                     very_unhappy = weighted.mean(happiness[happiness > 0] == 4, weight[happiness > 0]),
                     very_happy_over_very_unhappy = sum((happiness[happiness > 0] == 1) * weight[happiness > 0]) / sum((happiness[happiness > 0] == 4) * weight[happiness > 0]),
                     satisfied = weighted.mean(satisfaction[satisfaction > 0] > 5, weight[satisfaction > 0]),
                     satisfied_mean = weighted.mean(satisfaction[satisfaction > 0], weight[satisfaction > 0]),
                     happiness_mean = weighted.mean(((5 - happiness[happiness > 0]) * 2 - 5), weight[happiness > 0]), # -3/-1/1/3
                     gdp = unique(gdp), region = unique(region))
  a$happiness_Layard <- (a$happy + a$satisfied)/2
  a <- a %>%  mutate(region = sapply(country_code, get_country_region))
  a <- merge(a, info)
  return(a)
}
a7 <- create_happiness_vars(7)
a6 <- create_happiness_vars(6)


# all (sort(a7$country_code) == sort(info7$ISO))
# names(info7) <- c("country", "country_code", "year")
# a7 <- merge(a7, info7)

#new column region
# a7 <- a7 %>%  mutate(region = sapply(country_code, get_country_region))

# names(GDPpcPPP)[names(GDPpcPPP) %in% c("Country.Name", "Country.Code")] <- c("country", "country_code")

# for (c in intersect(a7$country_code, wb$country_code)) {
#   gdp_pc_ppp_c <- GDPpcPPP[GDPpcPPP$country_code == c, paste0("X",  a7$year[a7$country_code == c])]
#   if (!is.na(gdp_pc_c)) a7$gdp_ppp[a7$country_code == c]  <- gdp_pc_ppp_c
# }
# # for (c in intersect(a6$country_code, wb$country_code)) {
# #   gdp_pc_ppp_c <- GDPpcPPP[GDPpcPPP$country_code == c, paste0("X",  a6$year[a6$country_code == c])]
# #   if (!is.na(gdp_pc_c)) a6$gdp_ppp[a6$country_code == c]  <- gdp_pc_ppp_c
# # }

# TODO: where does this data come from? It needs to be automatized (and with the URL of the data source)
population_data <- data.frame(
  country_code = c("AND", "ARG", "ARM", "AUS", "BGD", "BOL", "BRA", "CAN", "CHL", "CHN", "COL", "CYP", "CZE", "DEU", "ECU", "EGY", "ETH", "GBR", "GRC", "GTM", "HKG", "IDN", "IRN", "IRQ", "JOR", "JPN", "KAZ", "KEN", "KGZ", "KOR", "LEB", "LYB", "MAC", "MAR", "MDV", "MEX", "MMR", "MNG", "MYS", "NGA", "NIC", "NIR", "NLD", "NZL", "PAK", "PER", "PHL", "PRI", "ROU", "RUS", "SGP", "SRB", "SVK", "THA", "TJK", "TUN", "TUR", "TWN", "UKR", "URY", "USA", "VEN", "VNM", "ZWE" ),
  population = c(75013, 44044811, 2790974, 24966643, 163683958, 11435533, 210166592, 38007166, 18701450, 1402760000, 49276961, 1228836, 10526073, 82905782, 17015672, 103740765, 117190911, 66971411, 10754679, 16604026, 7452600, 267066843, 87290193, 40590700, 10459865, 126633000, 18276452, 53005614, 6579900, 51585058, 5950839, 6812341, 663653, 37076584, 521457, 124013861, 53423198, 3347782, 32399271, 198387623, 6755895, 5086988, 17703090, 5124100, 219731479, 32203944, 110380804, 3325286, 19473970, 144496739, 5685807, 7020858, 5431752, 71127802, 9543207, 12049314, 82809304, 23777737, 44132049, 3422794, 325122128, 28199867, 96648685, 15669666)
)
a7 <- a7 %>% left_join(population_data, by = "country_code")
# write.csv(a7, "../data/a7.csv", row.names = FALSE)
# a7 <- read.csv("../data/a7.csv")

# econometric analysis
# rearranging alternative GDP variable  (Y^)
create_gdp_vars <- function(wave = 7, k_values = 4:7) {
  if (wave == 7) data <- a7
  else if (wave == 6) data <- a6
  for (c in intersect(data$country_code, GDPpcPPP$Country.Code)) {
    gdp_pc_ppp_c <- GDPpcPPP[GDPpcPPP$Country.Code == c, paste0("X",  unique(data$year[data$country_code == c]))]
    if (!is.na(gdp_pc_ppp_c)) data$gdp_ppp[data$country_code == c]  <- gdp_pc_ppp_c
  }
  if (wave == 6) {
    data$gdp[data$country_code == "TWN"] <- 21256
    data$gdp[data$country_code == "LBN"] <- 8255
    data$gdp_ppp[data$country_code == "TWN"] <- 21256 # TODO
    data$gdp_ppp[data$country_code == "YEM"] <- 8255 # TODO
  } else if (wave == 7) {
    data$gdp_ppp[data$country_code %in% c("AND", "NIR", "TWN", "VEN")] <- 999 # TODO
  }
  for (var in intersect(c("gdp_ppp", "gdp"), names(data))) {
    data[[paste0("log_", var)]] <- log10(data[[var]])
    data[[paste0("ranked_", var)]] <- rank(data[[var]])
    data[[paste0(var, "_group")]] <- as.character(cut(data[[paste0("ranked_", var)]], breaks = 6, labels = FALSE))
    #1 being the lowest and 6 being the highest gdp group (corresponding to the y_6ile variable in the previous paper)
    # clustered Y variables (corresponding to Y_clus4, Y_clus5, Y_clus6 and Y_clus7 in the previous paper)
    cluster_assignments <- list()
    for (k in k_values) {
      kmeans_result <- kmeans(data[[paste0("log_", var)]], centers = k)
      data[[paste0(var, "_cluster", k)]] <- as.character(kmeans_result$cluster)
    }
  }
  return(data)
}
a7 <- create_gdp_vars(7)
a6 <- create_gdp_vars(6)


##### regressions #####
happiness_variables <- c("very_happy", "happy", "very_unhappy", "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard")
hapiness_names <- setNames(c("Very Happy", "Happy", "Very Unhappy", "Satisfied", "Satisfaction (mean)", "Happiness (mean)", "Happiness Layard"), happiness_variables)

run_regressions <- function(happiness_vars = happiness_variables, weight = FALSE, pandemic_years = TRUE, data = a7, return = "var_explained", PPP = T) {
  if (!pandemic_years) data <- create_gdp_vars(data[!data$year %in% c(2020, 2021), ])
  
  weights <- if (weight) data$population else NULL
  
  regressions <- list()
  for (i in happiness_vars) { # TODO non-PPP
    regressions[[i]] <- list("region" = lm(as.formula(paste(i, "~ region")), data = data, weights = weights),
                             "log_gdp" = lm(as.formula(paste(i, "~ log_gdp")), data = data, weights = weights),
                             "log_gdp_quadratic" = lm(as.formula(paste(i, "~ log_gdp + I(log_gdp^2)")), data = data, weights = weights),
                             "gdp_group" = lm(as.formula(paste(i, "~ gdp_group")), data = data, weights = weights), 
                             "region_log_gdp" = lm(as.formula(paste(i, "~ log_gdp + as.factor(region)")), data = data, weights = weights),
                             "region_log_gdp_quadratic" = lm(as.formula(paste(i, "~ log_gdp + I(log_gdp^2) + as.factor(region)")), data = data, weights = weights),
                             "region_gdp_group" = lm(as.formula(paste(i, "~ gdp_group + as.factor(region)")), data = data, weights = weights),
                             
                             "log_gdp_ppp" = lm(as.formula(paste(i, "~ log_gdp_ppp")), data = data, weights = weights),
                             "log_gdp_ppp_quadratic" = lm(as.formula(paste(i, "~ log_gdp_ppp + I(log_gdp_ppp^2)")), data = data, weights = weights),
                             "gdp_ppp_group" = lm(as.formula(paste(i, "~ gdp_ppp_group")), data = data, weights = weights), 
                             "region_log_gdp_ppp" = lm(as.formula(paste(i, "~ log_gdp_ppp + as.factor(region)")), data = data, weights = weights),
                             "region_log_gdp_ppp_quadratic" = lm(as.formula(paste(i, "~ log_gdp_ppp + I(log_gdp_ppp^2) + as.factor(region)")), data = data, weights = weights),
                             "region_gdp_ppp_group" = lm(as.formula(paste(i, "~ gdp_ppp_group + as.factor(region)")), data = data, weights = weights))
    for (k in 4:7) regressions[[i]][[paste0("gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_cluster", k)), data = data, weights = weights)
    for (k in 4:7) regressions[[i]][[paste0("region_gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_cluster", k, " + as.factor(region)")), data = data, weights = weights)
    
    for (k in 4:7) regressions[[i]][[paste0("gdp_ppp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_ppp_cluster", k)), data = data, weights = weights)
    for (k in 4:7) regressions[[i]][[paste0("region_gdp_ppp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_ppp_cluster", k, " + as.factor(region)")), data = data, weights = weights)
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
      var_explained <- rbind(var_explained, cbind("var_happiness" = i, "var_gdp" = j,
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

var7 <- run_regressions(weight = F, pandemic_years = T)
var7_wo_pandemic_years <- run_regressions(weight = F, pandemic_years = F)
var7_weighted <- run_regressions(weight = T, pandemic_years = T)
var7_weighted_wo_pandemic_years <- run_regressions(weight = T, pandemic_years = F)
var6 <- run_regressions(data = a6, weight = F, pandemic_years = T)


##### Plot #####
# Graphs with country names, R² in legend, and log scale for both x and specific y-variables
region_colors <- c("Africa" = "black", "Latin America" = "#4CAF50", "Ex-Eastern Block" = "red", "Middle East" = "#FFA000", "Western" = "#64B5F6", "Asia" = "purple")

create_scatter_plot <- function(y_var, log_scale_y = FALSE, data = a7, PPP = T, wave = "7") { # TODO: size dot function of pop
  p <- ggplot(data, aes(x = gdp_pc, y = get(y_var), color = region, label = country)) +
    geom_point() + scale_color_manual(values = region_colors) + scale_x_log10() + labs(x = paste("GDP pc", if (PPP) "(PPP)" else ""), y = hapiness_names[y_var], color = "Region") +
    theme_minimal() + theme(legend.position = "bottom", plot.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"), axis.text = element_text(size = 7), legend.text = element_text(size = 7),
          axis.title = element_text(size = 8), plot.caption = element_text(size = 8))
  
  if (log_scale_y) p <- p + scale_y_log10()
  
  model <- lm(get(y_var) ~ gdp_pc, data = data)
  rsquared <- summary(model)$r.squared
  
  p <- p + labs(color = paste0("Region (Wave = ", wave, ", R² = ", round(rsquared, 3), ")"))
  p <- p + geom_text_repel(segment.size = 0.2, force = 4, point.padding = unit(0.2, "lines"), nudge_x = 0.005, nudge_y = 0.005, size = 2)
  
  print(p)
}

scatter_plot_vars <- c("very_happy", "happy", "very_unhappy", "very_happy_over_very_unhappy", "satisfied", "satisfied_mean", "happiness_mean")
for (var in scatter_plot_vars) {
  log_scale_y <- var %in% c("very_unhappy", "very_happy_over_very_unhappy")
  p <- create_scatter_plot(var, log_scale_y)
  filename <- paste("scatter_", var, "_vs_gdp.png", sep = "")
  ggsave(filename = filename, plot = p, path = "../figures", width = 6, height = 4, dpi = 300)
}


