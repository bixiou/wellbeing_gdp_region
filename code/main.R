WVS7 <- read_csv("WVS7.csv")
w7 <- WVS7[, c("Q46", "Q49", "fhregion", "GDPpercap2", "B_COUNTRY_ALPHA")]
names(w7) <- c("happiness", "satisfaction", "region", "gdp_pc", "country_code")
# TODO: enlever les pays 2020-2021 comme robustness check
# TODO: regarder si les donnees de GDP correspondent a la date de l'enquete, et le faire le cas echeant
# TODO: creer colonne country
# TODO: (poids,  avant analyse  econom.) , graphiques sur R, écrire  les mails!!, reproduire l'analyse économétrique!, (moins prioritaire que graphiques sur R) regarder les autres vagues.

w7$veryHappy <- w7$happiness == 1
# w7$region <- case_when(w7$region == 1 ~ "Europa",
#                        )

variables_kept <- c("country_code", "gdp_pc", "region")
a7 <- setNames(aggregate(w7$happiness, lapply(variables_kept, function(i) { w7[[i]] }), 
                function(v) {mean(v[v > 0] == 1, na.rm = T)}), c(variables_kept, "very_happy"))
a7 <- merge(a7, setNames(aggregate(w7$happiness, lapply(variables_kept, function(i) { w7[[i]] }), 
                  function(v) {mean(v[v > 0] < 3, na.rm = T)}), c(variables_kept, "happy")))
a7 <- merge(a7, setNames(aggregate(w7$happiness, lapply(variables_kept, function(i) { w7[[i]] }), 
                                   function(v) {mean(v[v > 0] == 4, na.rm = T)}), c(variables_kept, "very_unhappy")))
a7 <- merge(a7, setNames(aggregate(w7$happiness, lapply(variables_kept, function(i) { w7[[i]] }), 
                                   function(v) {sum(v == 1, na.rm = T)/sum(v == 4, na.rm = T)}), c(variables_kept, "very_happy_over_very_unhappy")))
a7 <- merge(a7, setNames(aggregate(w7$satisfaction, lapply(variables_kept, function(i) { w7[[i]] }), 
                                   function(v) {mean(v[v > 0] > 5, na.rm = T)}), c(variables_kept, "satisfied")))
a7 <- merge(a7, setNames(aggregate(w7$satisfaction, lapply(variables_kept, function(i) { w7[[i]] }), 
                                   function(v) {mean(v[v > 0], na.rm = T)}), c(variables_kept, "satisfaction_mean")))
a7 <- merge(a7, setNames(aggregate(w7$happiness, lapply(variables_kept, function(i) { w7[[i]] }), 
                                   function(v) {mean(((5-v[v > 0])*2)-5, na.rm = T)}), c(variables_kept, "happiness_mean")))
a7$happiness_Layard <- (a7$happy + a7$satisfied)/2
view(a7)
write.xlsx(a7, "../data/WVS7.xlsx")

mean(W7 $Q46)
median(W7 $Q46)
sd(W7 $Q46)
var(W7 $Q46)
quantile(W7 $Q46, 0.25)
quantile(W7 $Q46, 0.75)
mean(W7 $Q49)
median(W7 $Q49)
sd(W7 $Q49)
var(W7 $Q49)
quantile(W7 $Q49, 0.25)
quantile(W7 $Q49, 0.75)

joie <- lm(W7 $Q46 ~ W7 $GDPpercap2)
summary(joie)
satis <- lm(W7 $Q49 ~ W7 $GDPpercap2)
summary(satis)
joieregion <- lm(W7 $Q46 ~ W7 $GDPpercap2 + W7 $fhregion)
summary(joieregion)
satisregion <- lm(W7 $Q49 ~ W7 $GDPpercap2 + W7 $fhregion)
summary(satisregion)

library(ggplot2)
ggplot(W7, aes(x = GDPpercap2, y = Q46, color = fhregion)) + geom_smooth(method = "lm", se = FALSE) + labs(title = "Régression GDP et Happiness, contrôle région", x = "GDP", y = "Happiness")
ggplot(W7, aes(x = GDPpercap2, y = Q49, color = fhregion)) + geom_smooth(method = "lm", se = FALSE) + labs(title = "Régression GDP et Satisfaction, contrôle région", x = "GDP", y = "Satisfaction")

tresheureux <- subset(W7, Q46 == 4)
print(tresheureux)
heureux <- subset(W7, Q46 == 3)
print(heureux)
tresmalheureux <- subset(W7, Q46 == 1)
print(tresmalheureux)

##### Sandbox #####
decrit("satisfaction", w7[w7$country_code == "USA",])
