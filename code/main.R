# TODO: regarder si les donnees de GDP correspondent a la date de l'enquete, et le faire le cas echeant
# TODO: analyse variance (anova, r2 total ≠ r2y + r2reg): reg_tot<- lm(b ~ y + r, data = a7)
# variance_reg_tot <- calc.relimp(reg_tot, type = c("lmg"), rela = F, rank= F)
# plot: requires function "barres". Not so useful in our case as we have only two variables
# (plot_variance_reg_tot <- barres(data = unname(t(as.matrix(variance_reg_tot @lmg))), labels = names(variance_main_policies_C_all@lmg)], legend = "% of response variances", show_ticks = F, rev = F, digits = 1))

# TODO: graphiques sur R
# TODO: enlever les pays 2020-2021 comme robustness check (et ponderer les pays par leur population dans les regressions)
# TODO: (moins prioritaire que graphiques sur R) regarder les autres vagues.
# TODO: creer colonne country
# TODO: reproduire l'analyse économétrique!
# TODO: trouver définitions de region qui correspondent aux nôtres

# defining the regions
region <- list(
  "Africa" = c("ETH", "MAR", "LBY", "NGA", "TUN", "ZWE", "KEN"),
  "Latin America" = c("ARG", "BOL", "BRA", "CHL", "NIC", "PRI", "URY", "COL", "ECU", "GTM", "MEX", "PER", "VEN"),
  "Ex-Eastern Block" = c("ARM", "CZE", "KAZ", "ROU", "SRB", "TJK", "KGZ", "SVK", "RUS", "UKR"),
  "Middle East" = c("EGY", "IRN", "IRQ", "TUR", "JOR", "LBN"),
  "Western" = c("NDL", "CAN", "CYP", "USA", "DEU", "NIR", "NZL", "GRC", "GBR", "AUS", "AND"),
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

# Prepare  WVS7
# WVS7 <- read_csv("WVS7.csv")
# w7 <- WVS7[, c("Q46", "Q49", "GDPpercap2", "B_COUNTRY_ALPHA", "W_WEIGHT")]
# names(w7) <- c("happiness", "satisfaction", "gdp_pc", "country_code", "weight")
# View(w7)
# 
# # new column "region"
# w7 <- w7 %>%
#   mutate(region = sapply(country_code, get_country_region))
# 
# # adding missing values
# w7$gdp_pc[w7$country_code == "AND"] <- 42903
# w7$gdp_pc[w7$country_code == "IRN"] <- 13338
# w7$gdp_pc[w7$country_code == "NIR"] <- 32226
# w7$gdp_pc[w7$country_code == "TWN"] <- 25903
# w7$gdp_pc[w7$country_code == "VEN"] <- 6106
# w7$region[w7$country_code == "NIR"] <- "Western"
# w7$region[w7$country_code == "MAC"] <- "Asia"
# decrit("region", w7)
# write.csv(w7, "../data/WVS7.csv", row.names = FALSE)

w7 <- read.csv("../data/WVS7.csv")

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

# econometric analysis
# rearranging alternative GDP variable  (Y^)
a7$log_gdp <- log10(a7$gdp_pc)
a7$ranked_gdp <- rank(a7$gdp_pc)
# a7$ranked_log_gdp <- rank(a7$log_gdp)
a7$gdp_group <- as.character(cut(a7$ranked_gdp, breaks = 6, labels = FALSE))
#1 being the lowest and 6 being the highest gdp group (corresponding to the y_6ile variable in the previous paper)
# clustered Y variables (corresponding to Y_clus4, Y_clus5, Y_clus6 and Y_clus7 in the previous paper)
k_values <- c(4, 5, 6, 7)
cluster_assignments <- list()
for (k in k_values) {
  kmeans_result <- kmeans(a7$log_gdp, centers = k)
  # cluster_assignments[[paste0("gdp_cluster", k)]] <- as.character(kmeans_result$cluster)
  a7[[paste0("gdp_cluster", k)]] <- as.character(kmeans_result$cluster)
}
# a7$gdp_cluster4 <- cluster_assignments$gdp_cluster4
# a7$gdp_cluster5 <- cluster_assignments$gdp_cluster5
# a7$gdp_cluster6 <- cluster_assignments$gdp_cluster6
# a7$gdp_cluster7 <- cluster_assignments$gdp_cluster7
# View(a7)

# regressions
happiness_variables <- c("very_happy", "happy", "very_unhappy", "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard")
regressions <- list()
for (i in happiness_variables) {
  regressions[[i]] <- list("zg" = lm(as.formula(paste(i, "~ region")), data = a7),
                           "gdp" = lm(as.formula(paste(i, "~ log_gdp + I(log_gdp^2)")), data = a7),
                           "gdp_group" = lm(as.formula(paste(i, "~ gdp_group")), data = a7))
  for (k in 4:7) regressions[[i]][[paste0("gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_cluster", k)), data = a7)
}

# # Zone Geographique (ZG)
# very_happyZG <- lm(a7$very_happy ~ a7$region)
# happyZG <- lm(a7$happy ~ a7$region)
# very_unhappyZG <- lm(a7$very_unhappy ~ a7$region)
# very_happy_over_very_unhappyZG <- lm(a7$very_happy_over_very_unhappy ~ a7$region)
# satisfiedZG <- lm(a7$satisfied ~ a7$region)
# satisfied_meanZG <- lm(a7$satisfied_mean ~ a7$region)
# happiness_meanZG <- lm(a7$happiness_mean ~ a7$region)
# happiness_LayardZG <- lm(a7$happiness_Layard ~ a7$region)
# 
# # GDP per capita (Y)
# very_happyGDP <- lm(very_happy ~ log_gdp + I(log_gdp^2), data = a7)
# happyGDP <- lm(happy ~ log_gdp + I(log_gdp^2), data = a7)
# very_unhappyGDP <- lm(very_unhappy ~ log_gdp + I(log_gdp^2), data = a7)
# very_happy_over_very_unhappyGDP <- lm(very_happy_over_very_unhappy ~ log_gdp + I(log_gdp^2), data = a7)
# satisfiedGDP <- lm(satisfied ~ log_gdp + I(log_gdp^2), data = a7)
# sartisfied_meanGDP <- lm(satisfied_mean ~ log_gdp + I(log_gdp^2), data = a7)
# happiness_meanGDP <- lm(happiness_mean ~ log_gdp + I(log_gdp^2), data = a7)
# happiness_LayardGDP <- lm(happiness_Layard ~ log_gdp + I(log_gdp^2), data = a7)
# 
# # GDP per capita groups (variantes de Y)
# # Y_6ile (rearranged growing gdp and clusters)
# very_happyGDPgroup <- lm(a7$very_happy ~ a7$gdp_group)
# happyGDPgroup <- lm(a7$happy ~ a7$gdp_group)
# very_unhappyGDPgroup <- lm(a7$very_unhappy ~ a7$gdp_group)
# very_happy_over_very_unhappyGDPgroup <- lm(a7$very_happy_over_very_unhappy ~ a7$gdp_group)
# satisfiedGDPgroup <- lm(a7$satisfied ~ a7$gdp_group)
# satisfied_meanGDPgroup <- lm(a7$satisfied_mean ~ a7$gdp_group)
# happiness_meanGDPgroup <- lm(a7$happiness_mean ~ a7$gdp_group)
# happiness_LayardGDPgroup <- lm(a7$happiness_Layard ~ a7$gdp_group)
# 
# # Y_cluster4 (cluster of 4)
# very_happyGDPcluster4 <- lm(a7$very_happy ~ a7$gdp_cluster4)
# happyGDPcluster4 <- lm(a7$happy ~ a7$gdp_cluster4)
# very_unhappyGDPcluster4 <- lm(a7$very_unhappy ~ a7$gdp_cluster4)
# very_happy_over_very_unhappyGDPcluster4 <- lm(a7$very_happy_over_very_unhappy ~ a7$gdp_cluster4)
# satisfiedGDPcluster4 <- lm(a7$satisfied ~ a7$gdp_cluster4)
# satisfied_meanGDPcluster4 <- lm(a7$satisfied_mean ~ a7$gdp_cluster4)
# happiness_meanGDPcluster4 <- lm(a7$happiness_mean ~ a7$gdp_cluster4)
# happiness_LayardGDPcluster4 <- lm(a7$happiness_Layard ~ a7$gdp_cluster4)
# 
# # Y_cluster5 (cluster of 5)
# very_happyGDPcluster5 <- lm(a7$very_happy ~ a7$gdp_cluster5)
# happyGDPcluster5 <- lm(a7$happy ~ a7$gdp_cluster5)
# very_unhappyGDPcluster5 <- lm(a7$very_unhappy ~ a7$gdp_cluster5)
# very_happy_over_very_unhappyGDPcluster5 <- lm(a7$very_happy_over_very_unhappy ~ a7$gdp_cluster5)
# satisfiedGDPcluster5 <- lm(a7$satisfied ~ a7$gdp_cluster5)
# satisfied_meanGDPcluster5 <- lm(a7$satisfied_mean ~ a7$gdp_cluster5)
# happiness_meanGDPcluster5 <- lm(a7$happiness_mean ~ a7$gdp_cluster5)
# happiness_LayardGDPcluster5 <- lm(a7$happiness_Layard ~ a7$gdp_cluster5)
# 
# # Y_cluster6 (cluster of 6)
# very_happyGDPcluster6 <- lm(a7$very_happy ~ a7$gdp_cluster6)
# happyGDPcluster6 <- lm(a7$happy ~ a7$gdp_cluster6)
# very_unhappyGDPcluster6 <- lm(a7$very_unhappy ~ a7$gdp_cluster6)
# very_happy_over_very_unhappyGDPcluster6 <- lm(a7$very_happy_over_very_unhappy ~ a7$gdp_cluster6)
# satisfiedGDPcluster6 <- lm(a7$satisfied ~ a7$gdp_cluster6)
# satisfied_meanGDPcluster6 <- lm(a7$satisfied_mean ~ a7$gdp_cluster6)
# happiness_meanGDPcluster6 <- lm(a7$happiness_mean ~ a7$gdp_cluster6)
# happiness_LayardGDPcluster6 <- lm(a7$happiness_Layard ~ a7$gdp_cluster6)
# 
# # Y_cluster7 (cluster of 7)
# very_happyGDPcluster7 <- lm(a7$very_happy ~ a7$gdp_cluster7)
# happyGDPcluster7 <- lm(a7$happy ~ a7$gdp_cluster7)
# very_unhappyGDPcluster7 <- lm(a7$very_unhappy ~ a7$gdp_cluster7)
# very_happy_over_very_unhappyGDPcluster7 <- lm(a7$very_happy_over_very_unhappy ~ a7$gdp_cluster7)
# satisfiedGDPcluster7 <- lm(a7$satisfied ~ a7$gdp_cluster7)
# satisfied_meanGDPcluster7 <- lm(a7$satisfied_mean ~ a7$gdp_cluster7)
# happiness_meanGDPcluster7 <- lm(a7$happiness_mean ~ a7$gdp_cluster7)
# happiness_LayardGDPcluster7 <- lm(a7$happiness_Layard ~ a7$gdp_cluster7)
# View(a7)

# happiness_variables <- c(
#   "very_happy", "happy", "very_unhappy", "very_happy_over_very_unhappy",
#   "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard"
# )

result_tables <- list()
combined_results <- data.frame() 
for (j in names(regressions[[1]])) {
  result_tables[[j]] <- list()
  for (i in happiness_variables) {
    result_tables[[j]] <- rbind(result_tables[[j]], glance(regressions[[i]][[j]]) %>% mutate(Dependent_Variable = i, Independent_Variable = j))
  }
  if (grepl("gdp_", j)) combined_results <- rbind(combined_results, result_tables[[j]])
}


# happiness_variables <- c(
#   "very_happy", "happy", "very_unhappy", "very_happy_over_very_unhappy",
#   "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard"
# )

result_tables <- data.frame(
  Dependent_Variable = character(),
  Independent_Variable = character(),
  r.squared = numeric(),
  p.value = numeric()
)

for (j in names(regressions)) {
  for (i in happiness_variables) {
    regression_result <- glance(eval(parse(text = paste0(i, j)))) %>%
      mutate(Dependent_Variable = i, Independent_Variable = j)
    
    result_tables <- rbind(result_tables, regression_result)
  }
}

combined_results <- result_tables[grepl("gdp_|GDPgroup|GDPcluster", result_tables$Independent_Variable), ]

# bind_rows(
#   glance(very_happyGDP) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "log_gdp"),
#   glance(happyGDP) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "log_gdp"),
#   glance(very_unhappyGDP) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "log_gdp"),
#   glance(very_happy_over_very_unhappyGDP) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "log_gdp"),
#   glance(satisfiedGDP) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "log_gdp"),
#   glance(satisfied_meanGDP) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "log_gdp"),
#   glance(happiness_meanGDP) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "log_gdp"),
#   glance(happiness_LayardGDP) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "log_gdp")
# )
# 
# result_table_log_gdp <- bind_rows(
#   glance(very_happyGDP) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "log_gdp"),
#   glance(happyGDP) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "log_gdp"),
#   glance(very_unhappyGDP) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "log_gdp"),
#   glance(very_happy_over_very_unhappyGDP) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "log_gdp"),
#   glance(satisfiedGDP) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "log_gdp"),
#   glance(satisfied_meanGDP) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "log_gdp"),
#   glance(happiness_meanGDP) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "log_gdp"),
#   glance(happiness_LayardGDP) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "log_gdp")
# )
# print(result_table_log_gdp)
# 
# result_table_region <- bind_rows(
#   glance(very_happyZG) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "region"),
#   glance(happyZG) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "region"),
#   glance(very_unhappyZG) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "region"),
#   glance(very_happy_over_very_unhappyZG) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "region"),
#   glance(satisfiedZG) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "region"),
#   glance(satisfied_meanZG) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "region"),
#   glance(happiness_meanZG) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "region"),
#   glance(happiness_LayardZG) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "region")
# )
# print(result_table_region)
# 
# result_table_gdp_group <- bind_rows(
#   glance(very_happyGDPgroup) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "gdp_group"),
#   glance(happyGDPgroup) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "gdp_group"),
#   glance(very_unhappyGDPgroup) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "gdp_group"),
#   glance(very_happy_over_very_unhappyGDPgroup) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "gdp_group"),
#   glance(satisfiedGDPgroup) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "gdp_group"),
#   glance(satisfied_meanGDPgroup) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "gdp_group"),
#   glance(happiness_meanGDPgroup) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "gdp_group"),
#   glance(happiness_LayardGDPgroup) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "gdp_group")
# )
# print(result_table_gdp_group)
# 
# result_table_gdp_cluster4 <- bind_rows(
#   glance(very_happyGDPcluster4) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "gdp_cluster4"),
#   glance(happyGDPcluster4) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "gdp_cluster4"),
#   glance(very_unhappyGDPcluster4) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "gdp_cluster4"),
#   glance(very_happy_over_very_unhappyGDPcluster4) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "gdp_cluster4"),
#   glance(satisfiedGDPcluster4) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "gdp_cluster4"),
#   glance(satisfied_meanGDPcluster4) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "gdp_cluster4"),
#   glance(happiness_meanGDPcluster4) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "gdp_cluster4"),
#   glance(happiness_LayardGDPcluster4) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "gdp_cluster4")
# )
# print(result_table_gdp_cluster4)
# 
# result_table_gdp_cluster5 <- bind_rows(
#   glance(very_happyGDPcluster5) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "gdp_cluster5"),
#   glance(happyGDPcluster5) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "gdp_cluster5"),
#   glance(very_unhappyGDPcluster5) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "gdp_cluster5"),
#   glance(very_happy_over_very_unhappyGDPcluster5) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "gdp_cluster5"),
#   glance(satisfiedGDPcluster5) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "gdp_cluster5"),
#   glance(satisfied_meanGDPcluster5) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "gdp_cluster5"),
#   glance(happiness_meanGDPcluster5) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "gdp_cluster5"),
#   glance(happiness_LayardGDPcluster5) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "gdp_cluster5")
# )
# print(result_table_gdp_cluster5)
# 
# result_table_gdp_cluster6 <- bind_rows(
#   glance(very_happyGDPcluster6) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "gdp_cluster6"),
#   glance(happyGDPcluster6) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "gdp_cluster6"),
#   glance(very_unhappyGDPcluster6) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "gdp_cluster6"),
#   glance(very_happy_over_very_unhappyGDPcluster6) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "gdp_cluster6"),
#   glance(satisfiedGDPcluster6) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "gdp_cluster6"),
#   glance(satisfied_meanGDPcluster6) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "gdp_cluster6"),
#   glance(happiness_meanGDPcluster6) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "gdp_cluster6"),
#   glance(happiness_LayardGDPcluster6) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "gdp_cluster6")
# )
# print(result_table_gdp_cluster6)
# 
# result_table_gdp_cluster7 <- bind_rows(
#   glance(very_happyGDPcluster7) %>% mutate(Dependent_Variable = "happy", Independent_Variable = "gdp_cluster7"),
#   glance(happyGDPcluster7) %>% mutate(Dependent_Variable = "very_happy", Independent_Variable = "gdp_cluster7"),
#   glance(very_unhappyGDPcluster7) %>% mutate(Dependent_Variable = "very_unhappy", Independent_Variable = "gdp_cluster7"),
#   glance(very_happy_over_very_unhappyGDPcluster7) %>% mutate(Dependent_Variable = "very_happy_over_very_unhappy", Independent_Variable = "gdp_cluster7"),
#   glance(satisfiedGDPcluster7) %>% mutate(Dependent_Variable = "satisfied", Independent_Variable = "gdp_cluster7"),
#   glance(satisfied_meanGDPcluster7) %>% mutate(Dependent_Variable = "satisfied_mean", Independent_Variable = "gdp_cluster7"),
#   glance(happiness_meanGDPcluster7) %>% mutate(Dependent_Variable = "happiness_mean", Independent_Variable = "gdp_cluster7"),
#   glance(happiness_LayardGDPcluster7) %>% mutate(Dependent_Variable = "happiness_Layard", Independent_Variable = "gdp_cluster7")
# )
# print(result_table_gdp_cluster7)
# 
# combined_results <- bind_rows(
#   gdp_cluster4 = result_table_gdp_cluster4,
#   gdp_cluster5 = result_table_gdp_cluster5,
#   gdp_cluster6 = result_table_gdp_cluster6,
#   gdp_cluster7 = result_table_gdp_cluster7,
#   gdp_group = result_table_gdp_group
# )

#result_tables <- list()
#for (j in names(regressions)) {
#  result_table <- data.frame()
#  for (i in happiness_variables) {
#    result_table <- rbind(result_table, glance(eval(parse(text = i))) %>% mutate(Dependent_Variable = i, Independent_Variable = j))
#  }
#  result_tables[[j]] <- result_table
#  if (grepl("gdp_", j)) {
#    if (exists("combined_results")) {
#      combined_results <- rbind(combined_results, result_table)
#    } else {
#      combined_results <- result_table
#    }
#  }
#}
#print(lapply(result_tables, head))
#print(head(combined_results))

combined_results_max <- combined_results %>%
group_by(Dependent_Variable) %>%
slice(which.max(r.squared))
print(combined_results_max)

write.csv(w7, "../data/WVS7.csv", row.names = FALSE)
