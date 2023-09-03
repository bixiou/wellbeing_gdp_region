clear
set more off, permanently
use "C:\Users\Adrien\Documents\COURS\Economie\STATA\wvs1981_2008_v20090914.dta"
keep a008 s003 s002 a170
*garde 101 pays : ceux de Layard U les 70 pays les plus peuplés*
keep if s003==8 | s003==12 | s003==32 | s003==36 | s003==40 | s003==31 | s003==50 | s003==112 | s003==56 | s003==76 | s003==100 | s003==124 | s003==152 | s003==156 | s003==170 | s003==191 | s003==203 | s003==208 | s003==214 | s003==222 | s003==233 | s003==246 | s003==250 | s003==268 | s003==276 | s003==300 | s003==348 | s003==356 | s003==360 | s003==364 | s003==372 | s003==376 | s003==380 | s003==392 | s003==400 | s003==410 | s003==428 | s003==440 | s003==807 | s003==484 | s003==498 | s003==504 | s003==528 | s003==554 | s003==566 | s003==578 | s003==586 | s003==604 | s003==608 | s003==616 | s003==620 | s003==642 | s003==643 | s003==702 | s003==703 | s003==705 | s003==710 | s003==724 | s003==752 | s003==756 | s003==834 | s003==792 | s003==800 | s003==804 | s003==826 | s003==840 | s003==858 | s003==862 | s003==704 | s003==716 | s003==818 | s003==4 | s003==24 | s003==854 | s003==116 | s003==120 | s003==180 | s003==384 | s003==218 | s003==231 | s003==288 | s003==320 | s003==368 | s003==398 | s003==404 | s003==408 | s003==450 | s003==454 | s003==458 | s003==466 | s003==508 | s003==104 | s003==524 | s003==562 | s003==682 | s003==144 | s003==736 | s003==760 | s003==764 | s003==860 | s003==887 | s003==158

*garde les mêmes pays que Layard*
*drop if  s003==4 | s003==24 | s003==854 | s003==116 | s003==120 | s003==180 | s003==384 | s003==218 | s003==231 | s003==288 | s003==320 | s003==368 | s003==398 | s003==404 | s003==408 | s003==450 | s003==454 | s003==458 | s003==466 | s003==508 | s003==104 | s003==524 | s003==562 | s003==682 | s003==144 | s003==736 | s003==760 | s003==764 | s003==860 | s003==887 | s003==158*

*vague4 vaut 4 si le pays a eu la vague 4, ou 3 s'il a eu la vague 3 mais pas la 4, 0 sinon*
*vague3 vaut 3 si le pays a eu la vague 3, ou 4 s'il a eu la vague 4 mais pas la 3, 0 sinon*
*derniereVague vaut le chiffre de la vague la plus récente du pays*
gen bonheurLayard = 0
gen repondants= 0
gen tresMalheureux=0
gen malheureux=0
gen heureux=0
gen tresHeureux=0
gen vague4 = 0
gen vague3 = 0
gen derniereVague = 0
gen satisfaits6a10 = 0
gen tamp=0


levelsof s003, local (l)
foreach v of local l{
	quiet:tab a008 if s003==`v' & s002==3
	replace vague3=3 if r(N)>0 & s003==`v'
	replace vague4=3 if r(N)>0 & s003==`v'
	replace derniereVague = 3 if r(N)>0 & s003==`v'
	quiet:tab a008 if s003==`v' & s002==4
	replace vague4=4 if r(N)>0 & s003==`v'
	replace vague3=4 if r(N)>0 & vague3==0 & s003==`v'
	replace derniereVague = 4  if r(N)>0 & s003==`v'
	quiet:tab a008 if s003==`v' & s002==5
	replace derniereVague = 5 if r(N)>0 & s003==`v'
}

clear
set more off, permanently
use "C:\Users\Adrien\Documents\COURS\Economie\STATA\bonheur wvs.dta"

*garde les mêmes données de Layard (1ere ligne s'il a utilisé la vague 3, 2è sinon) ou celles de la vague la plus récente*
*drop if s002!=vague3*
*drop if s002!=vague4*
drop if s002!=derniereVague

gen indicateurBonheur = 0
gen indicateurSatisfaction = 0
gen bonheurMoyen = 0
gen satisfactionMoyenne = 0

levelsof s003, local (l)
foreach v of local l{
	quiet:sum a008 if s003==`v'
	replace bonheurMoyen=r(mean) if s003==`v'
	quiet:sum a170 if s003==`v'
	replace satisfactionMoyenne=r(mean) if s003==`v'
}

levelsof s003, local (l)
foreach v of local l{
	quiet:tab a008 if a008<=2 & s003==`v'
	replace tamp=r(N) if s003==`v'
	quiet:tab a008 if s003==`v'
	replace repondants=r(N) if s003==`v'
	quiet:tab a008 if a008==3 & s003==`v'
	replace malheureux=r(N)/repondants if s003==`v'
	replace indicateurBonheur=-r(N)  if s003==`v'
	quiet:tab a008 if a008==4 & s003==`v'
	replace tresMalheureux=r(N)/repondants if s003==`v'
	replace indicateurBonheur=indicateurBonheur-2*r(N)  if s003==`v'
	quiet:tab a008 if a008==1 & s003==`v'
	replace tresHeureux=r(N)/repondants if s003==`v'
	replace heureux=tamp/repondants if s003==`v'
	replace indicateurBonheur=indicateurBonheur+2*r(N) if s003==`v'	
	quiet:tab a008 if a008==2 & s003==`v'
	replace tamp=r(N) if s003==`v'
	replace indicateurBonheur = (indicateurBonheur + tamp)/repondants if s003==`v'
	quiet:tab a170 if s003==`v'
	replace repondants=r(N) if s003==`v'
	quiet:tab a170 if a170>=6 & s003==`v'
	replace satisfaits6a10=r(N)/repondants if s003==`v'
	replace bonheurLayard=(heureux+satisfaits6a10)/2 if s003==`v'
	quiet:tab a170 if a170==1 & s003==`v'
	replace indicateurSatisfaction = -5*r(N) if s003==`v'
	quiet:tab a170 if a170==2 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction-4*r(N)  if s003==`v'
	quiet:tab a170 if a170==3 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction-3*r(N)  if s003==`v'
	quiet:tab a170 if a170==4 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction-2*r(N)  if s003==`v'
	quiet:tab a170 if a170==5 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction-r(N)  if s003==`v'
	quiet:tab a170 if a170==6 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction+r(N)  if s003==`v'
	quiet:tab a170 if a170==7 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction+2*r(N)  if s003==`v'
	quiet:tab a170 if a170==8 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction+3*r(N)  if s003==`v'
	quiet:tab a170 if a170==9 & s003==`v'
	replace indicateurSatisfaction = indicateurSatisfaction+4*r(N)  if s003==`v'
	quiet:tab a170 if a170==10 & s003==`v'
	replace indicateurSatisfaction = (indicateurSatisfaction+5*r(N))/repondants if s003==`v'
}

preserve
	set more off
	duplicates drop s003, force
	*bysort bonheurLayard : tabulate s003*
	*bysort tresHeureux : tabulate s003*
	outsheet s003 tresHeureux heureux indicateurBonheur indicateurSatisfaction satisfaits6a10 malheureux tresMalheureux tamp using "C:\Users\Adrien\Documents\COURS\Economie\STATA\bonheur10.xls", nolabel replace
restore
preserve
	duplicates drop s003, force
	outsheet bonheurMoyen using "C:\Users\Adrien\Documents\COURS\Economie\STATA\bonheur1.xls", nolabel replace
	outsheet satisfactionMoyenne using "C:\Users\Adrien\Documents\COURS\Economie\STATA\bonheur2.xls", nolabel replace
	outsheet s003 using "C:\Users\Adrien\Documents\COURS\Economie\STATA\bonheur3.xls", nolabel replace
restore

browse