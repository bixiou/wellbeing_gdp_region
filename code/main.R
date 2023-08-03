# TODO: analyse variance (anova, r2 total ≠ r2y + r2reg): reg_tot<- lm(b ~ y + r, data = a7)
# variance_reg_tot <- calc.relimp(reg_tot, type = c("lmg"), rela = F, rank= F)
# plot: requires function "barres". Not so useful in our case as we have only two variables
# (plot_variance_reg_tot <- barres(data = unname(t(as.matrix(variance_reg_tot @lmg))), labels = names(variance_main_policies_C_all@lmg)], legend = "% of response variances", show_ticks = F, rev = F, digits = 1))

# TODO: graphiques sur R
# TODO: enlever les pays 2020-2021 comme robustness check (et ponderer les pays par leur population dans les regressions)
# TODO: (moins prioritaire que graphiques sur R) regarder les autres vagues.
# TODO: trouver définitions de region qui correspondent aux nôtres

WVS7 <- read_csv("WVS7.csv")
w7 <- WVS7[, c("Q46", "Q49", "GDPpercap2", "B_COUNTRY_ALPHA", "W_WEIGHT")]
names(w7) <- c("happiness", "satisfaction", "gdp_pc", "country_code", "weight")
View(w7)

# new column "region"
w7 <- w7 %>%
   mutate(region = sapply(country_code, get_country_region))

# adding missing values
w7$gdp_pc[w7$country_code == "AND"] <- 42903
w7$gdp_pc[w7$country_code == "IRN"] <- 13338
w7$gdp_pc[w7$country_code == "NIR"] <- 32226
w7$gdp_pc[w7$country_code == "TWN"] <- 25903
w7$gdp_pc[w7$country_code == "VEN"] <- 6106
w7$region[w7$country_code == "NIR"] <- "Western"
w7$region[w7$country_code == "MAC"] <- "Asia"
decrit("region", w7)
write.csv(w7, "../data/WVS7.csv", row.names = FALSE)

# defining the regions
region <- list(
  "Africa" = c("ETH", "MAR", "LBY", "NGA", "TUN", "ZWE", "KEN"),
  "Latin America" = c("ARG", "BOL", "BRA", "CHL", "NIC", "PRI", "URY", "COL", "ECU", "GTM", "MEX", "PER", "VEN"),
  "Ex-Eastern Block" = c("ARM", "CZE", "KAZ", "ROU", "SRB", "TJK", "KGZ", "SVK", "RUS", "UKR"),
  "Middle East" = c("EGY", "IRN", "IRQ", "TUR", "JOR", "LBN"),
  "Western" = c("CAN", "CYP", "USA", "DEU", "NIR", "NZL", "GRC", "GBR", "AUS", "AND", "NLD"),
  "Asia" = c("THA", "PHL", "VNM", "SGP", "HKG", "MDV", "TWN", "BGD", "CHN", "MYS", "KOR", "IDN", "PAK", "JPN", "MNG", "MMR", "MDV", "MAC")
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
wb <- read.csv("../data/WBGDPpercap.csv" , sep = ",", skip = 4)
info <- read.csv2("../data/WVSinfo.csv")

# adding the weights and calculating the means
a7 <- w7 %>% group_by(country_code) %>% 
  dplyr::summarize(very_happy = weighted.mean(happiness[happiness > 0] == 1, weight[happiness > 0]),
                   happy = weighted.mean(happiness[happiness > 0] < 3, weight[happiness > 0]),
                   very_unhappy = weighted.mean(happiness[happiness > 0] == 4, weight[happiness > 0]),
                   very_happy_over_very_unhappy = sum((happiness[happiness > 0] == 1) * weight[happiness > 0]) / sum((happiness[happiness > 0] == 4) * weight[happiness > 0]),
                   satisfied = weighted.mean(satisfaction[satisfaction > 0] > 5, weight[satisfaction > 0]),
                   satisfied_mean = weighted.mean(satisfaction[satisfaction > 0], weight[satisfaction > 0]),
                   happiness_mean = weighted.mean(((5 - happiness[happiness > 0] * 2) - 5), weight[happiness > 0]),
                   gdp_pc = unique(gdp_pc), region = unique(region))
a7$happiness_Layard <- (a7$happy + a7$satisfied)/2

all (sort(a7$country_code) == sort(info$ISO))
names(info) <- c("country", "country_code", "year")
a7 <- merge(a7, info)

names(wb)[names(wb) %in% c("Country.Name", "Country.Code")] <- c("country", "country_code")

for (c in intersect(a7$country_code, wb$country_code)) {
  gdp_pc_c <- wb[wb$country_code == c, paste0("X",  a7$year[a7$country_code == c])]
  if (!is.na(gdp_pc_c)) a7$gdp_pc[a7$country_code == c]  <- gdp_pc_c
}
View(a7)

# econometric analysis
# rearranging alternative GDP variable  (Y^)
a7$log_gdp <- log10(a7$gdp_pc)
a7$ranked_gdp <- rank(a7$gdp_pc)
a7$gdp_group <- as.character(cut(a7$ranked_gdp, breaks = 6, labels = FALSE))
#1 being the lowest and 6 being the highest gdp group (corresponding to the y_6ile variable in the previous paper)
# clustered Y variables (corresponding to Y_clus4, Y_clus5, Y_clus6 and Y_clus7 in the previous paper)
k_values <- c(4, 5, 6, 7)
cluster_assignments <- list()
for (k in k_values) {
  kmeans_result <- kmeans(a7$log_gdp, centers = k)
  a7[[paste0("gdp_cluster", k)]] <- as.character(kmeans_result$cluster)
}

# regressions
happiness_variables <- c("very_happy", "happy", "very_unhappy", "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard")
regressions <- list()
for (i in happiness_variables) {
  regressions[[i]] <- list("region" = lm(as.formula(paste(i, "~ region")), data = a7),
                           "log_gdp" = lm(as.formula(paste(i, "~ log_gdp")), data = a7),
                           "log_gdp_quadratic" = lm(as.formula(paste(i, "~ log_gdp + I(log_gdp^2)")), data = a7),
                           "gdp_group" = lm(as.formula(paste(i, "~ gdp_group")), data = a7), 
                           "region_log_gdp" = lm(as.formula(paste(i, "~ log_gdp + as.factor(region)")), data = a7),
                           "region_log_gdp_quadratic" = lm(as.formula(paste(i, "~ log_gdp + I(log_gdp^2) + as.factor(region)")), data = a7),
                           "region_gdp_group" = lm(as.formula(paste(i, "~ gdp_group + as.factor(region)")), data = a7))
  for (k in 4:7) regressions[[i]][[paste0("gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_cluster", k)), data = a7)
  for (k in 4:7) regressions[[i]][[paste0("region_gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_cluster", k, " + as.factor(region)")), data = a7)
}

result_tables <- list()
combined_results <- data.frame() 
for (j in names(regressions[[1]])) if (!grepl("region_", j)) {
  result_tables[[j]] <- list()
  for (i in happiness_variables) {
    result_tables[[j]] <- rbind(result_tables[[j]], glance(regressions[[i]][[j]]) %>% mutate(Dependent_Variable = i, Independent_Variable = j))
  }
  if (!grepl("region", j)) combined_results <- rbind(combined_results, result_tables[[j]])
}
combined_results_max <- combined_results %>%
  group_by(Dependent_Variable) %>%
  slice(which.max(r.squared))
print(combined_results_max)

var_explained <- data.frame()
for (j in names(regressions[[1]])) if (grepl("region_", j)) {
  for (i in happiness_variables) {
    variance_reg_tot <- calc.relimp(regressions[[i]][[j]], type = c("lmg"), rela = F, rank= F)
    var_explained <- rbind(var_explained, cbind("var_happiness" = i, "var_gdp" = j,
                            "var_explained" = sum(variance_reg_tot$lmg), 
                            "var_explained_by_gdp" = variance_reg_tot$lmg[grepl("gdp", names(variance_reg_tot$lmg))], 
                            "var_explained_by_region" = variance_reg_tot$lmg[grepl("region", names(variance_reg_tot$lmg))],
                            "share_var_explained_by_gdp" = variance_reg_tot$lmg[grepl("gdp", names(variance_reg_tot$lmg))]/sum(variance_reg_tot$lmg)))
  }
}
rownames(var_explained) <- NULL
var_explained

reg_tot<- lm( ~ log_gdp + (region == "Africa") + (region == "Latin  America") + (region == "Ex-Eastern Block") + (region == "Middle East") + (region == "Western") + (region == "Asia"), data = a7)
reg_tot<- lm(happy ~ gdp_pc + as.factor(region), data = a7)
variance_reg_tot <- calc.relimp(reg_tot, type = c("lmg"), rela = F, rank= F)
# plot: requires function "barres". Not so useful in our case as we have only two variables
# (plot_variance_reg_tot <- barres(data = unname(t(as.matrix(variance_reg_tot @lmg))), labels = names(variance_main_policies_C_all@lmg)], legend = "% of response variances", show_ticks = F, rev = F, digits = 1))
variance_reg_tot$lmg[1]
variance_reg_tot$lmg[2]
