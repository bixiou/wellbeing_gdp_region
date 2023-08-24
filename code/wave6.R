WVS6 <- read_csv2("WVS6.csv")
w6 <- WVS6[, c("V10", "V23", "B_COUNTRY_ALPHA", "V258A")] # there was no variable for GDP in this survey, so ill add it later on the code
decrit("region", w6)
names(w6) <- c("happiness", "satisfaction", "country_code", "weight")

# defining the regions
region <- list(
  "Africa" = c("DZA", "GHA", "LBY", "MAR", "NGA", "RWA", "ZAF", "TUN", "ZWE"),
  "Latin America" = c("ARG", "BRA", "COL", "CHL", "ECU", "HTI", "MEX", "PER", "TTO", "URY"), #i put Haiti ad well as Trinidad and Tobago as latinamerica because it best fits there but honestly it doesnt belong to any of these regions
  "Ex-Eastern Block" = c("ARM", "AZE", "BLR", "EST", "KAZ", "KGZ", "POL", "ROU", "RUS", "SVN", "UKR", "UZB"),
  "Middle East" = c("EGY", "GEO", "IRQ", "JOR", "KWT", "LEB", "PSE", "QAT", "TUR", "YEM"),
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
view(w6)
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
view(merged_data)
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

view(a6)