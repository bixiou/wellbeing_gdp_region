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
  "Ex-Eastern Block" = c("ALB", "ARM", "AZE", "BIH", "BGR", "BLR", "CZE", "EST", "HRV", "HUN", "KAZ", "KGZ", "LTU", "LVA", "MDA", "MKD", "MNE", "POL", "ROU", "RUS", "SRB", "SVK", "SVN", "TJK", "UKR", "UZB"), # 26
  "Middle East" = c("EGY", "GEO", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "PSE", "QAT", "SAU", "TUR", "YEM"), # 13 Israel in it?
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
                   very_happy_over_very_unhappy = sum((happiness[happiness > 0] == 1) * weight[happiness > 0]) / sum((happiness[happiness > 0] == 4) * weight[happiness > 0]),
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

create_gdp_vars <- function(var = "gdp_ppp", waves = 1:7, k_values = 4:7, pandemic_years = TRUE, data = a) {
  a <- data[data$wave %in% waves,]
  if (!pandemic_years) a <- a[a$non_pandemic == T,]
  a[[paste0("log_", var)]] <- log10(a[[var]])
  a[[paste0("ranked_", var)]] <- rank(a[[var]])
  a[[paste0("group_", var)]] <- as.character(cut(a[[paste0("ranked_", var)]], breaks = 6, labels = FALSE))
  #1 being the lowest and 6 being the highest gdp group (corresponding to the y_6ile variable in the previous paper)
  # clustered Y variables (corresponding to Y_clus4, Y_clus5, Y_clus6 and Y_clus7 in the previous paper)
  for (k in k_values) {
    kmeans_total <- kmeans(a[[paste0("log_", var)]][!is.na(a[[paste0("log_", var)]])], centers = k)
    a[[paste0("kmeans_", var, "_", k)]][!is.na(a[[paste0("log_", var)]])] <- as.character(kmeans_total$cluster)
  }
  return(a)
}


##### regressions #####
happiness_variables <- c("very_happy", "happy", "very_unhappy", "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard")
hapiness_names <- setNames(c("Very Happy", "Happy", "Very Unhappy", "Satisfied", "Satisfaction (mean)", "Happiness (mean)", "Happiness Layard"), happiness_variables)

run_regressions <- function(var_gdp = "gdp_ppp", waves = 1:7, weight = FALSE, pandemic_years = TRUE, happiness_vars = happiness_variables, data = a, return = "var_explained") {
  # if (!pandemic_years) data <- create_gdp_vars(pandemic_years = FALSE)
  data <- create_gdp_vars(var = var_gdp, waves = waves, k_values = 4:7, pandemic_years = pandemic_years, data = a)
  
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
    for (k in 4:7) regressions[[i]][[paste0("gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ kmeans_", var_gdp, "_", k)), data = data, weights = weights)
    for (k in 4:7) regressions[[i]][[paste0("region_gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ kmeans_", var_gdp, "_", k, " + as.factor(region)")), data = data, weights = weights)
    
    # for (k in 4:7) regressions[[i]][[paste0("gdp_ppp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_ppp_cluster", k)), data = data, weights = weights)
    # for (k in 4:7) regressions[[i]][[paste0("region_gdp_ppp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_ppp_cluster", k, " + as.factor(region)")), data = data, weights = weights)
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

r6p <- run_regressions(var_gdp = "gdp_ppp", waves = 6, weight = FALSE) # reproduction of my original work
r7p <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = FALSE, pandemic_years = T)
r7p_wo_pandemic_years <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = FALSE, pandemic_years = F)
r7p_weighted <- run_regressions(var_gdp = "gdp_ppp", waves = 7, weight = T, pandemic_years = T)
r7 <- run_regressions(var_gdp = "gdp", waves = 7, weight = FALSE, pandemic_years = T)
r7p_na <- run_regressions(var_gdp = "gdp_ppp_na", waves = 7, weight = FALSE, pandemic_years = T)
rp <- run_regressions(var_gdp = "gdp_ppp", waves = 1:7, weight = FALSE)
r <- run_regressions(var_gdp = "gdp", waves = 1:7, weight = FALSE)
r6 <- run_regressions(var_gdp = "gdp", waves = 6, weight = FALSE)
r5p <- run_regressions(var_gdp = "gdp_ppp", waves = 5, weight = FALSE)
r5 <- run_regressions(var_gdp = "gdp", waves = 5, weight = FALSE)
r4p <- run_regressions(var_gdp = "gdp_ppp", waves = 4, weight = FALSE)
r4 <- run_regressions(var_gdp = "gdp", waves = 4, weight = FALSE)
r3p <- run_regressions(var_gdp = "gdp_ppp", waves = 3, weight = FALSE)
r3 <- run_regressions(var_gdp = "gdp", waves = 3, weight = FALSE)
r2p <- run_regressions(var_gdp = "gdp_ppp", waves = 2, weight = FALSE)
r2 <- run_regressions(var_gdp = "gdp", waves = 2, weight = FALSE)
r12p <- run_regressions(var_gdp = "gdp_ppp", waves = 1:2, weight = FALSE)
r12 <- run_regressions(var_gdp = "gdp", waves = 1:2, weight = FALSE)

# run_all <- function(wave = 7, happiness_vars = happiness_variables, weight = FALSE, pandemic_years = TRUE, return = "var_explained") {
#   a <- create_happiness_vars(wave)
#   a <- create_gdp_vars(wave)
#   res <- run_regressions(happiness_vars = happiness_vars, weight = weight, pandemic_years = pandemic_years, data = a, return = return, PPP = PPP)
#   return(res)
# }


##### Plot #####
# Graphs with country names, R² in legend, and log scale for both x and specific y-variables
region_colors <- c("Africa" = "black", "Asia" = "purple", "Ex-Eastern Block" = "red", "Latin America" = "#4CAF50", "Middle East" = "#FFA000", "Western" = "#64B5F6")
region_shapes <- c("Africa" = "●", "Asia" = "▲", "Ex-Eastern Block" = "■", "Latin America" = "○", "Middle East" = "△", "Western" = "□") # , 3: +, 4: x

create_scatter_plot <- function(y_var, log_scale_y = FALSE, data = a, PPP = T, waves = 7, size_pop = FALSE, legend = TRUE, label = "country", fontsize = 7, labelsize = 2, shape_region = TRUE) { 
  wave <- if (length(waves)>1) paste0(min(waves), " to ", max(waves)) else waves 
  df <- data[data$wave %in% waves,]
  df$label <- df[[label]] # country or code
  p <- qplot(if (PPP) gdp_ppp else gdp, get(y_var), data = df, color = region, shape = if (shape_region) region else NULL,
             label = if (length(waves)>1) paste(label, year) else label, 
             size = if (size_pop) pop else NULL, show.legend = FALSE) + scale_shape_manual(values = region_shapes, labels = names(region_shapes)) + 
    geom_point(show.legend = FALSE) + scale_color_manual(values = region_colors) + scale_x_log10() + labs(x = paste("GDP pc", if (PPP) "(PPP)" else ""), y = hapiness_names[y_var]) +
    theme_minimal() + theme(legend.position = if (legend) "bottom" else "none", plot.background = element_rect(fill = "white"), #legend.background = element_rect(fill = "white"), 
          axis.text = element_text(size = fontsize), legend.text = element_text(size = fontsize), legend.title = element_text(size = fontsize),
          axis.title = element_text(size = fontsize), plot.caption = element_text(size = fontsize))
  
  if (log_scale_y) p <- p + scale_y_log10()
  
  model <- lm(as.formula(paste(y_var, "~", paste0(if (log_scale_y) "log_" else "", if (PPP) "gdp_ppp" else "gdp"))), data = df, weights = if (size_pop) pop else NULL)
  rsquared <- summary(model)$r.squared
  
  p <- p + labs(color = paste0("Wave", if (length(waves)>1) "s" else "", " = ", wave, "  (R² = ", round(rsquared, 2), ")  Regions:"))
  p <- p + geom_text_repel(segment.size = 0.2, force = 4, point.padding = unit(0.2, "lines"), nudge_x = 0.005, nudge_y = 0.005, size = labelsize)
  p <- p + guides(color = guide_legend(override.aes = list(shape = NULL, labels = region_shapes), nrow = 1))
  print(p)
}
create_scatter_plot(v, log_scale_y, waves = 7)

scatter_plot_vars <- c("very_happy", "happy", "very_unhappy", "very_happy_over_very_unhappy", "satisfied", "satisfied_mean", "happiness_mean")
for (v in scatter_plot_vars) {
  log_scale_y <- v %in% c("very_unhappy", "very_happy_over_very_unhappy")
  p <- create_scatter_plot(v, log_scale_y)
  filename <- paste("scatter_", v, "_vs_gdp.png", sep = "")
  ggsave(filename = filename, plot = p, path = "../figures", width = 6, height = 4, dpi = 300)
}


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
# create_gdp_vars <- function(wave = 7, k_values = 4:7, pandemic_years = TRUE) {
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
