/***** Zones Géographiques ou PIB/habitant (PPP) : 
    Qu'est-ce qui prédit le mieux le bonheur d'un pays ? *****/

/* Création du fichier bonheur.dta (partie technique et peu importante) */

// Utilisation des données du World Values Survey, nettoyage des données avec bonheurLayard.do
// Les variables inutiles ont été évacuées, et les indicateurs de bonheur des pays créées
// Les valeurs du PIB/habitant PPP ($ constants 2011) ont été ajoutées à la main dans la variable Y
// Les valeurs retenues pour Y correspondent à la date de la dernière vague d'enquête de chaque pays
// Ainsi, on évacue les données des vagues d'enquête antérieures
//     drop if s002 != derniereVague
//     duplicates drop s003, force
// Les indicatrices de 6 Zones Géographiques sont créées : Afrique (11 pays), AmeriqueLatine (11), MoyenOrient (7 dont Égypte), Occident (14 dont Australie et Nouvelle-Zélande), Asie (13), EuropeDelEst (dont Russie et pays du Caucase)
cd "\\VBOXSVR\Google_Drive\Economie\Travail\Well-being\"
use "bonheur.dta", clear

/* Réponse à la question */

// Pour chaque indicateur de bonheur, on évalue la part de la variance expliquée par la zone géographique : r^2_ZG
//   ainsi que la part de la variance expliquée par le PIB/habitant (PPP) : r^2_Y
// Le signe de la différence des deux permet de savoir ce qui prédit le mieux l'indicateur, entre la zone géographique et le PIB/hab
// On calcule également le F-test qui a pour hypothèse nulle que Y est indépendant linéairement de l'indicateur, quand on contrôle pour la zone géographique

global affiche 1
foreach bonheur in tresHeureux heureux tresMalheureux ratioHappy satisfaits6a10 satisfaction bonheur bonheurLayard {
	if ($affiche) {
		display "           r^2 ZG | p-value du F-test | r^2_ZG - r^2_Y | r^2_ZG - r^2_Y_max"
		global affiche 0
	}
	quiet:sum `bonheur'
	// On normalise les indicateurs pour que les p-value s'interprètent comme la significativité de l'écart de la ZG au bonheur moyen
	quiet:capture gen `bonheur'Normalized = `bonheur' - r(mean)
	quiet:reg `bonheur'Normalized Y
	global r2_Y = e(r2)
	
	// Pour des questions de robustesse, on regarde le r^2 lors de regressions avec des variantes de Y, à savoir différents clusters de Y et ln(Y)
	// Les Y_clus_* ont été construit avec "cluster kmeans Y, k(*)" (Y_clus_* = 1 pour les pays les plus pauvres, et croît jusqu'à * pour les pays les plus riches)
	// Y_6ile répartit les pays en 6 groupes homogènes (pour Y) et de taille égale
	global r2_Y_max = e(r2)
	foreach Y_clus in Y_clus_4 Y_clus_5 Y_clus_6 Y_clus_7 Y_6ile {
		quiet:reg `bonheur'Normalized i.`Y_clus'
		if e(r2) > $r2_Y_max {
			global r2_Y_max = e(r2)
		}
	}
	quiet:reg `bonheur'Normalized lnY
	if e(r2) > $r2_Y_max {
		global r2_Y_max = e(r2)
	}
	
	quiet:reg `bonheur'Normalized EuropeDelEst MoyenOrient Asie AmeriqueLatine Occident Afrique, nocons
	global r2_ZG = e(r2)
	quiet:reg `bonheur'Normalized Y EuropeDelEst MoyenOrient Asie AmeriqueLatine Occident Afrique, nocons
	quiet:test Y = 0
	display %16,2f $r2_ZG %16,5f r(p) %16,2f $r2_ZG - $r2_Y %16,2f $r2_ZG - $r2_Y_max " :	`bonheur'"
}

/* Résultats : Zone Géographique !


           r^2 ZG | p-value du F-test | r^2_ZG - r^2_Y | r^2_ZG - r^2_Y_max
            0,57         0,25722            0,53            0,40 :   tresHeureux
            0,55         0,00981            0,29            0,21 :   heureux
            0,44         0,06269            0,28            0,25 :   tresMalheureux
            0,28         0,12694            0,15            0,10 :   ratioHappy
            0,66         0,00001            0,24            0,20 :   satisfaits6a10
            0,58         0,00025            0,26            0,20 :   satisfaction
            0,53         0,01572            0,39            0,32 :   bonheur
            0,65         0,00006            0,25            0,24 :   bonheurLayard

		   
Quel que soit l'indicateur retenu, la zone géographique prédit toujours mieux le bonheur d'un pays que son PIB/hab (PPP).
La variance expliquée additionnelle est en général autour de 30% (+/- 3%), sauf pour l'indicateur tresHeureux, le seul pour lequel le PIB/hab n'est pas significatif au seuil de 20%
Toutefois, pour la plupart des indicateurs, le PIB/hab (PPP) a malgré tout une valeur prédictive, puisque dans 4 cas sur 6, l'hypothèse nulle du F-test est rejetée au seuil de 3%
Pour deux estimateurs, on peut même affirmer avec une probabilité inférieure à 1/1000 de se tromper qu'il y a une corrélation entre le PIB/hab et le bonheur du pays, même en controlant pour la zone géographique.
Répartir les pays en groupes de richesse homogène pour tenter de prédire leur bonheur n'améliore jamais drastiquement le r^2 (comparé à une simple régression avec Y comme variable dépendante), comme le montre la dernière colonne.
L'Europe de l'Est est systématiquement significativement moins heureuse. Les autres zones géographiques sont souvent significatives pour prédire l'indicateur de bonheur :
L'Amérique latine, l'Occident et l'Asie sont corrélés à plus de bonheur, alors que l'Afrique et le Moyen-Orient sont associés à moins de bonheur.
*/

/* Test de la significativiité de la corrélation entre bien-être et revenu au sein de chaque ZG */
global corr = 0
global nb_significatifs_1 = 0
global nb_significatifs_5 = 0
global nb_significatifs_10 = 0
foreach ZG in Afrique AmeriqueLatine MoyenOrient Occident Asie EuropeDelEst {
	if (! $corr) {
		display "corrélation moyenne | nb significatifs à 1% | nb sign. à 5% | nb sign. à 10% | ZG"
	}
	global corr_$`ZG' = 0 
	global nb_significatifs_1_$`ZG' = 0 
	global nb_significatifs_5_$`ZG' = 0
	global nb_significatifs_10_$`ZG' = 0
	foreach bonheur in tresHeureux heureux tresMalheureux ratioHappy satisfaits6a10 satisfaction bonheur bonheurLayard {
		foreach revenu in Y lnY {
			quiet:reg `bonheur' `revenu' if `ZG'
			quiet:test `revenu'
			if r(p) < 0.01 {
				global nb_significatifs_1_$`ZG' = 1 + $nb_significatifs_1_$`ZG'
			}
			if r(p) < 0.05 {
				global nb_significatifs_5_$`ZG' = 1 + $nb_significatifs_5_$`ZG'
			}
			if r(p) < 0.1 {
				global nb_significatifs_10_$`ZG' = 1 + $nb_significatifs_10_$`ZG'
			}
			quiet: cor `bonheur' `revenu' if `ZG'
			global corr_$`ZG' = $corr_$`ZG' + r(rho)
		}
	}
	global corr = $corr + $corr_$`ZG' / 8
	global nb_significatifs_1 = $nb_significatifs_1_$`ZG' + $nb_significatifs_1
	global nb_significatifs_5 = $nb_significatifs_5_$`ZG' + $nb_significatifs_5
	global nb_significatifs_10 = $nb_significatifs_10_$`ZG' + $nb_significatifs_10 
	display "  " %16,2f $corr_$`ZG' / 8 %16,0f $nb_significatifs_1_$`ZG' %16,0f $nb_significatifs_5_$`ZG' %16,0f $nb_significatifs_10_$`ZG' "             `ZG', (16 régressions)"
}
display "  " %16,2f $corr / 8 %16,0f $nb_significatifs_1 %16,0f $nb_significatifs_5 %16,0f $nb_significatifs_10 "             total, (96 régressions)"

/* Résultats :

corrélation moyenne | nb significatifs à 1% | nb sign. à 5% | nb sign. à 10% | ZG
              0,25               0               0               0             Afrique, (16 régressions)
             -0,02               0               0               2             AmeriqueLatine, (16 régressions)
              0,75               0               5               7             MoyenOrient, (16 régressions)
              0,33               0               0               0             Occident, (16 régressions)
              0,47               1               4               6             Asie, (16 régressions)
              0,58               6               9              11             EuropeDelEst, (16 régressions)

              0,29               7              18              26             total, (96 régressions)
*/

/* Qui sont les plus heureux entre l'Amérique latine et l'Occident ? */
foreach bonheur in tresHeureux heureux tresMalheureux ratioHappy satisfaits6a10 satisfaction bonheur bonheurLayard {
	reg `bonheur' Occident if Occident == 1 | AmeriqueLatine == 1
}
foreach bonheur in tresHeureux heureux tresMalheureux ratioHappy satisfaits6a10 satisfaction bonheur bonheurLayard {
	reg `bonheur' Occident Y if Occident == 1 | AmeriqueLatine == 1
}
/* Réponse : ils sont autant heureux */
