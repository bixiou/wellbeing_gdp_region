WVS6 <- read_csv2("WVS6.csv")
w6 <- WVS6[, c("V10", "V23", "B_COUNTRY_ALPHA", "V258A")] # there was no variable for GDP in this survey, so ill add it later on the code
names(w6) <- c("happiness", "satisfaction", "country_code", "weight")

# defining the regions
w6$region <- NA
regions <- list(
  "Africa" = c("DZA", "GHA", "LBY", "MAR", "NGA", "RWA", "ZAF", "TUN", "ZWE"),
  "Latin America" = c("ARG", "BRA", "COL", "CHL", "ECU", "HTI", "MEX", "PER", "TTO", "URY"), #i put Haiti ad well as Trinidad and Tobago as latinamerica because it best fits there but honestly it doesnt belong to any of these regions
  "Ex-Eastern Block" = c("ARM", "AZE", "BLR", "EST", "KAZ", "KGZ", "POL", "ROU", "RUS", "SVN", "UKR", "UZB"),
  "Middle East" = c("EGY", "GEO", "IRQ", "JOR", "KWT", "LBN", "PSE", "QAT", "TUR", "YEM"),
  "Western" = c("AUS", "CYP", "DEU", "NLD", "NZL", "ESP", "SWE", "USA"),
  "Asia" = c("CHN", "HKG", "IND", "JPN", "MYS", "PAK", "PHL", "SGP", "KOR", "TWN", "THA")
)

get_country_region <- function(country_code) {
  for (region_name in names(region)) {
    if (country_code %in% region[[region_name]]) {
      return(region_name)
    }
  }
  return("Other")
}
w6$region <- sapply(w6$country_code, get_country_region)

WB <- read.csv("../data/WBpercapitappp2015.csv" , sep = ",", skip = 4)
wb <- WB[, c("Country.Code", "X2010", "X2011", "X2012", "X2013", "X2014", "X2015")]
names(wb) <- c("country_code", "2010", "2011", "2012", "2013", "2014", "2015")
view(wb)
info <- read.csv2("../data/WVSinfo6.csv")

wb_long <- wb %>%
  pivot_longer(cols = starts_with("20"), names_to = "year", values_to = "gdp") %>%
  mutate(year = as.integer(year))
merged_data <- info %>%
  left_join(wb_long, by = c("country_code", "year"))
w6_with_gdp <- w6 %>%
  left_join(merged_data, by = "country_code") %>%
  mutate(gdp = ifelse(is.na(gdp), 0, gdp))

# (adding the weights) and calculating the means
a6 <- w6_with_gdp %>% group_by(country_code) %>% 
  dplyr::summarize(very_happy = weighted.mean(happiness[happiness > 0] == 1, weight[happiness > 0]),
                   happy = weighted.mean(happiness[happiness > 0] < 3, weight[happiness > 0]),
                   very_unhappy = weighted.mean(happiness[happiness > 0] == 4, weight[happiness > 0]),
                   very_happy_over_very_unhappy = sum((happiness[happiness > 0] == 1) * weight[happiness > 0]) / sum((happiness[happiness > 0] == 4) * weight[happiness > 0]),
                   satisfied = weighted.mean(satisfaction[satisfaction > 0] > 5, weight[satisfaction > 0]),
                   satisfied_mean = weighted.mean(satisfaction[satisfaction > 0], weight[satisfaction > 0]),
                   happiness_mean = weighted.mean(((5 - happiness[happiness > 0] * 2) - 5), weight[happiness > 0]),
                   gdp = unique(gdp), region = unique(region))
a6$happiness_Layard <- (a6$happy + a6$satisfied)/2

# econometric analysis
a6$gdp[a6$country_code == "TWN"] <- 21256
a6$gdp[a6$country_code == "LBN"] <- 8255
# rearranging alternative GDP variable  (Y^)
a6$log_gdp <- log10(a6$gdp)
a6$ranked_gdp <- rank(a6$gdp)
a6$gdp_group <- as.character(cut(a6$ranked_gdp, breaks = 6, labels = FALSE))

k_values <- c(4, 5, 6, 7)
cluster_assignments <- list()
for (k in k_values) {
  kmeans_result <- kmeans(a6$log_gdp, centers = k)
  a6[[paste0("gdp_cluster", k)]] <- as.character(kmeans_result$cluster)
}
view(a6)

# regressions
happiness_variables <- c("very_happy", "happy", "very_unhappy", "satisfied", "satisfied_mean", "happiness_mean", "happiness_Layard")
regressions <- list()
for (i in happiness_variables) {
  regressions[[i]] <- list("region" = lm(as.formula(paste(i, "~ region")), data = a6),
                           "log_gdp" = lm(as.formula(paste(i, "~ log_gdp")), data = a6),
                           "log_gdp_quadratic" = lm(as.formula(paste(i, "~ log_gdp + I(log_gdp^2)")), data = a6),
                           "gdp_group" = lm(as.formula(paste(i, "~ gdp_group")), data = a6), 
                           "region_log_gdp" = lm(as.formula(paste(i, "~ log_gdp + as.factor(region)")), data = a6),
                           "region_log_gdp_quadratic" = lm(as.formula(paste(i, "~ log_gdp + I(log_gdp^2) + as.factor(region)")), data = a6),
                           "region_gdp_group" = lm(as.formula(paste(i, "~ gdp_group + as.factor(region)")), data = a6))
  for (k in 4:7) regressions[[i]][[paste0("gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_cluster", k)), data = a6)
  for (k in 4:7) regressions[[i]][[paste0("region_gdp_cluster", k)]] <- lm(as.formula(paste0(i, "~ gdp_cluster", k, " + as.factor(region)")), data = a6)
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

# variance
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

reg_tot<- lm(satisfied ~ log_gdp + (region == "Africa") + (region == "Latin  America") + (region == "Ex-Eastern Block") + (region == "Middle East") + (region == "Western") + (region == "Asia"), data = a6)
reg_tot<- lm(happy ~ gdp + as.factor(region), data = a6)
variance_reg_tot <- calc.relimp(reg_tot, type = c("lmg"), rela = F, rank= F)
variance_reg_tot$lmg[1]
variance_reg_tot$lmg[2]

# graphs
library(ggplot2)
library(ggrepel)
# graphs with country names, R² in legend, and log scale for both x and specific y-variables
region_colors <- c("Africa" = "black", "Latin America" = "#4CAF50", "Ex-Eastern Block" = "red",
                   "Middle East" = "#FFA000", "Western" = "#64B5F6", "Asia" = "purple")

create_scatter_plot <- function(y_var, y_label, log_scale_y = FALSE) {
  a6_filtered <- a6 %>% 
    filter(country_code != "MYS") # il faudrait voir quel est le problème avec Malaysia et pourquoi ses resultats donnent des valeurs 0 et infinies, pour l'instant j'ai juste ignoré le pays dans les graphiques
  p <- ggplot(a6_filtered, aes(x = gdp, y = get(y_var), color = region, label = country_code)) + #j'ai mis country_code parce que je trouvais que  c'était plus visible sur les graphique que le nom entier du pays
    geom_point() +
    scale_color_manual(values = region_colors) +
    scale_x_log10() + 
    labs(x = "GDP PC", y = y_label, color = "Region") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          axis.text = element_text(size = 7),
          legend.text = element_text(size = 7),
          axis.title = element_text(size = 8),
          plot.caption = element_text(size = 8))
  
  if (log_scale_y) {
    p <- p + scale_y_log10()
  }
  
  model <- lm(get(y_var) ~ gdp, data = a6_filtered)
  rsquared <- summary(model)$r.squared
  
  p <- p + labs(color = paste("Region (R² =", round(rsquared, 3), ")"))
  
  p <- p + geom_text_repel(
    segment.size = 0.2,
    force = 4,
    point.padding = unit(0.2, "lines"),
    nudge_x = 0.005,
    nudge_y = 0.005,
    size = 2
  )
  
  print(p)
}

scatter_plot_vars <- c("very_happy", "happy", "very_unhappy", "very_happy_over_very_unhappy", "satisfied", "satisfied_mean", "happiness_mean")
for (var in scatter_plot_vars) {
  log_scale_y <- var %in% c("very_unhappy", "very_happy_over_very_unhappy")
  p <- create_scatter_plot(var, var, log_scale_y)
  filename <- paste("scatter_", var, "_vs_gdp_wave6.png", sep = "")
  ggsave(filename = filename, plot = p, path = "../figures", width = 6, height = 4, dpi = 300)
}

# il manque juste le robustness check des weight par pays mais j'ai pas eu le temps de le faire car c'était à la main pays par pays

