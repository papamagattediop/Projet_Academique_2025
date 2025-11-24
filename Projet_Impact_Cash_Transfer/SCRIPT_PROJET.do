/******************************************************************************
  PROJET   : Évaluation de l'impact des transferts monétaires sur la pauvreté 
                      et les inégalités au Sénégal				  
  AUTEUR   : Papa Magatte DIOP
  EMAIL    : papamagtte8diop@gmail.com
  GITHUD   : 
  DATE     : Juin 2025
  DONNÉES  : EHCVM 2018
  OBJECTIF : Simulation des politiques de transferts monétaires et analyse 
                      de leur impact sur la pauvreté et les inégalités       
******************************************************************************/

notes: "UN FICHIER resultat_projet sera crée dans votre bureau après exécution !"

notes: "Pour ré-éxecuter le do-file assurer vous de supprimer le dossier **resultat_projet** déja creer dans votre bureau"

notes: "Thanks and Enjoy 🙂 !! "

// Configuration initiale
clear all
set more off
set linesize 120
capture log close
log using "analyse_transferts_monetaires.log", replace



****************************************************************************
*
*                     1.PREPARATION DU SAUVEGARDE
*
****************************************************************************

* Identifier le chemin du bureau de l'utilisateur 
local user = c(username)
local desktop_path "C:\Users\\`user'\Desktop"

* Chemin pour le dossier `resultat_projet` sur le bureau
local result_path "`desktop_path'\resultat_projet"

* Créer le dossier `resultat_projet` s'il n'existe pas
mkdir "`result_path'"

* Créer les sous-dossiers pour les bases et les outputs
mkdir "`result_path'\bases"
mkdir "`result_path'\outputs"

****************************************************************************
*
* 		CHARGEMENT DES DONNÉES ET NETTOYAGE
*
****************************************************************************

* Cloner le dépôt GitHub contenant les données
// shell git clone "https://github.com/papamagattediop/data_bases.git"

shell git clone "https://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@github.com/papamagattediop/data_bases.git"

* Aller dans le dossier du dépôt
cd data_bases

* Charger la base des individus
use "ehcvm_individu_SEN2018.dta", clear

******************************************************************************
*
*                    2. EXPLORATION ET NETTOYAGE DES DONNÉES
*
******************************************************************************   
// Aperçu général des données
describe, short

// Structure des données
codebook, compact

// Recuperation et créations des variables importantes

gen under2 = (age < 2)
lab var under2 "enfant de moins de 2 ans"

gen under5 =(age <5)
lab var under5 "individu de moins de 5 ans "

gen under18 = (age <18)
lab var under18 "individu de moins de 18 ans"

gen elder =(age > 65)
lab var elder "individu de plus de 65 ans"

gen handicap =(handit==1)
lab var handicap "handicap de l'individu"

keep hhid  under2 under5 under18 elder handicap
*auvegarde des données
save scenarios, replace

* Merging avec la base welfare pour les autres variables importantes
merge m:1 hhid using ehcvm_welfare_SEN2018, nogen

* Recalcul du pcexp en intégrant le def_spa
replace pcexp = dtot/(hhsize*def_spa*def_temp)

* Sauvegarde de la base
save "`result_path'\bases\ECHVM_2018.dta", replace

******************************************************************************
*
*                    3. VIEILLISSEMENT DES DONNÉES (2018 → 2023)
*
******************************************************************************

/*
		Cette étape permet d'amener  les données importantes de 2018 au réalité de 2023.
		Processus:
		1. Mise à jour des poids (hhweight)
		2. Mise à jour du seuil de pauvrete (zref)
		3. Mise à jour de l'indicateurs de bien être (pcexp)
*/
// valeurs des taux de croissance (source : ANSD / BCEAO)
scalar t_crois_pop = 0.15 // taux de croissance de la population entre 2018 et 2023
scalar t_crois_pib_t= 0.26 // taux de croissance du PIB par tête entre 2018 et 2023
scalar t_crois_i_prix= 0.2 // taux de croissance de l'IHPC entre 2018 et 2023

// Mise à jour des poids
replace year =2023
replace hhweight =(1+t_crois_pop)*hhweight 

// Mise à jour du seuil 
replace zref = (1 + t_crois_i_prix)*zref

// Mise à jour de l'indicateur de bien être

replace pcexp = (1 + t_crois_pib_t)*pcexp


// Sauvegarde de la base 
save "`result_path'\bases\ECHVM_2023",replace

*******************************************************************************
*
*                    4. CALCUL DES INDICATEURS DE BASE (2018 vs 2023)
*
*******************************************************************************

/*
		- Indicateurs de pauvreté FGT 2018 et 2023
		- Indices d'inégalité Gini 2018 et 2023
		
 */
 
* Initialiser les matrices (2 années × 2 milieux × 4 indicateurs)
matrix base_2018_nat = J(1, 4, .)
matrix base_2023_nat = J(1, 4, .)
matrix base_2018_urbain = J(1, 4, .)
matrix base_2018_rural = J(1, 4, .)
matrix base_2023_urbain = J(1, 4, .)
matrix base_2023_rural = J(1, 4, .)

* Boucle sur les bases
foreach annee in 2018 2023 {
    use "`result_path'\bases\ECHVM_`annee'", clear
preserve
    * Bien-être et poids
    gen w_pcexp = hhweight * pcexp
    sum w_pcexp
    scalar totalY = r(sum)
    sum hhweight
    scalar totalW = r(sum)
    scalar mu = totalY / totalW

    * ==== NIVEAU NATIONAL ====
    gen pauvre = pcexp <= zref
    sum pauvre [iw=hhweight]
    scalar P0 = r(mean) * 100

    gen p1 = ((zref - pcexp) / zref) * pauvre
    sum p1 [iw=hhweight]
    scalar P1 = r(mean) * 100

    gen p2 = ((zref - pcexp) / zref)^2 * pauvre
    sum p2 [iw=hhweight]
    scalar P2 = r(mean) * 100

    * Gini (trapèze)
    sort pcexp
    gen cumw = sum(hhweight)
    gen cumwfrac = cumw / totalW
    gen cumwinc = sum(w_pcexp)
    gen Lor = cumwinc / totalY
    gen LagLor = Lor[_n-1]
    replace LagLor = 0 if _n == 1
    gen dX = cumwfrac - cumwfrac[_n-1]
    replace dX = cumwfrac if _n == 1
    gen trapeze = dX * (Lor + LagLor)
    sum trapeze
    scalar A = r(sum)
    scalar Gini = (1 - A) 

    if "`annee'" == "2018" {
        matrix base_2018_nat[1,1] = P0
        matrix base_2018_nat[1,2] = P1
        matrix base_2018_nat[1,3] = P2
        matrix base_2018_nat[1,4] = Gini
    }
    else {
        matrix base_2023_nat[1,1] = P0
        matrix base_2023_nat[1,2] = P1
        matrix base_2023_nat[1,3] = P2
        matrix base_2023_nat[1,4] = Gini
    }
restore
    * ==== PAR MILIEU ====
    foreach m in 1 2 {
        preserve
        keep if milieu == `m'

        gen w_pcexp = hhweight * pcexp
        sum w_pcexp
        scalar totalY = r(sum)
        sum hhweight
        scalar totalW = r(sum)
        scalar mu = totalY / totalW

        gen pauvre = pcexp <= zref
        sum pauvre [iw=hhweight]
        scalar P0 = r(mean) * 100

        gen p1 = ((zref - pcexp) / zref) * pauvre
        sum p1 [iw=hhweight]
        scalar P1 = r(mean) * 100

        gen p2 = ((zref - pcexp) / zref)^2 * pauvre
        sum p2 [iw=hhweight]
        scalar P2 = r(mean) * 100

        sort pcexp
        gen cumw = sum(hhweight)
        gen cumwfrac = cumw / totalW
        gen cumwinc = sum(hhweight * pcexp)
        gen Lor = cumwinc / totalY
        gen LagLor = Lor[_n-1]
        replace LagLor = 0 if _n == 1
        gen dX = cumwfrac - cumwfrac[_n-1]
        replace dX = cumwfrac if _n == 1
        gen trapeze = dX * (Lor + LagLor)
        sum trapeze
        scalar A = r(sum)
        scalar Gini = (1 - A) 

        if "`annee'" == "2018" & `m' == 1 {
            matrix base_2018_urbain[1,1] = P0
            matrix base_2018_urbain[1,2] = P1
            matrix base_2018_urbain[1,3] = P2
            matrix base_2018_urbain[1,4] = Gini
        }
        if "`annee'" == "2018" & `m' == 2 {
            matrix base_2018_rural[1,1] = P0
            matrix base_2018_rural[1,2] = P1
            matrix base_2018_rural[1,3] = P2
            matrix base_2018_rural[1,4] = Gini
        }
        if "`annee'" == "2023" & `m' == 1 {
            matrix base_2023_urbain[1,1] = P0
            matrix base_2023_urbain[1,2] = P1
            matrix base_2023_urbain[1,3] = P2
            matrix base_2023_urbain[1,4] = Gini
        }
        if "`annee'" == "2023" & `m' == 2 {
            matrix base_2023_rural[1,1] = P0
            matrix base_2023_rural[1,2] = P1
            matrix base_2023_rural[1,3] = P2
            matrix base_2023_rural[1,4] = Gini
        }

        restore
    }
}

* Sauvegarde des résultats
putexcel set "`result_path'\outputs\indices_base_2018_2023.xlsx", sheet("National") replace
putexcel A1 = "Année" B1 = "P0 (%)" C1 = "P1 (%)" D1 = "P2 (%)" E1 = "Gini"
putexcel A2 = "2018" B2 = matrix(base_2018_nat)
putexcel A3 = "2023" B3 = matrix(base_2023_nat)

putexcel set "`result_path'\outputs\indices_base_2018_2023.xlsx", sheet("Urbain") modify
putexcel A1 = "Année" B1 = "P0 (%)" C1 = "P1 (%)" D1 = "P2 (%)" E1 = "Gini"
putexcel A2 = "2018" B2 = matrix(base_2018_urbain)
putexcel A3 = "2023" B3 = matrix(base_2023_urbain)

putexcel set "`result_path'\outputs\indices_base_2018_2023.xlsx", sheet("Rural") modify
putexcel A1 = "Année" B1 = "P0 (%)" C1 = "P1 (%)" D1 = "P2 (%)" E1 = "Gini "
putexcel A2 = "2018" B2 = matrix(base_2018_rural)
putexcel A3 = "2023" B3 = matrix(base_2023_rural)

****************************************************************************
*
*                    5.DÉFINITION DES SCÉNARIOS DE TRANSFERTS
*
*****************************************************************************

/* 
		- Création des 8 variables de ciblage pour les scenarios
		- Determination du Nb de bénéficiaires, du Coût total et de la
		  Part dans le PIB pour chaque scenarios

 */
 
use "`result_path'\bases\ECHVM_2023", clear

// Definition des scenarios
gen sc1 = 1
gen sc2 = (milieu == 2)
gen sc3 = (under2 == 1)
gen sc4 = (under2 == 1 & milieu == 2)
gen sc5 = (under5 == 1)
gen sc6 = (under18 == 1)
gen sc7 = (elder == 1)
gen sc8 = (handicap == 1)

* Passer au niveau ménage 
collapse (max) sc1-sc8 hhweight eqadu2 dtot def_spa def_temp hhsize pcexp milieu, by(hhid)

* Sauvegarde de la base ménage
save "`result_path'\bases\ECHVM_menage_2023", replace

//  Determination des bénéficiaires, du coût total et de la Part dans le PIB
* Paramètres globaux (en milliards de FCFA)
scalar transfert = 100000
scalar pib = 18619.5e9

* Initialisation de la matrice vide
matrix results = J(8, 4, .)  // 8 scénarios, 4 colonnes (Scénario, Nb Bénéficiaires, Coût, Part PIB)

* Définir les scénarios et les labels
local scenarios sc1 sc2 sc3 sc4 sc5 sc6 sc7 sc8
local noms "Universel" "Rural" "under2" "under2 and rural" "under5" "under18" "Elder" "Handicap"

* Boucle sur chaque scénario*
local i = 1
foreach sc of local scenarios {
    
	use "`result_path'\bases\ECHVM_menage_2023", clear

    * Variable bénéficiaire pondérée
    gen benef_poids = `sc' * hhweight
    quietly summarize benef_poids
    scalar nb_benef = r(sum)

    * Coût total
    scalar cout = nb_benef * transfert

    * Part du PIB
    scalar part = (cout / pib) * 100

    * Stockage dans la matrice
    matrix results[`i', 1] = nb_benef
    matrix results[`i', 2] = cout
    matrix results[`i', 3] = part
    matrix results[`i', 4] = `i'

    local ++i
}

* Affichage dans Stata
matlist results, format(%20.0g)

* Export Excel
putexcel set "`result_path'\outputs\resultats_transferts.xlsx", sheet("Scénarios") replace
putexcel A1 = ("Scénario") B1 = ("Nb bénéficiaires") C1 = ("Coût total (FCFA)") D1 = ("Part PIB (%)")

* Ajouter les valeurs numériques (colonnes B, C, D)
putexcel B2 = matrix(results)

****************************************************************************
*
*                    6.SIMULATION DES TRANSFERTS MONÉTAIRES
*
*****************************************************************************
/*
		- Application des transferts
		- Ajuster la dépense de consommation (pcexp) après transfert
		- Recalculer les FGT et Gini après transfert pour chaque scénario
*/

// Application des transfert
foreach s in 1 2 3 4 5 6 7 8 {
    
    * 1. Appliquer le transfert au ménage
    gen transfert_menage`s' = sc`s' * transfert

    * 3. Recalcul de la dépense par tête corrigée (pcexp)
    gen pcexp_post`s' = pcexp + transfert_menage`s'/ (hhsize * def_temp * def_spa)
}

merge 1:m hhid using "`result_path'\bases\ECHVM_2023" , nogen

// Recalculer les FGT et Gini après transfert pour chaque scénario

* Initialisation
matrix indicateurs_final = J(9, 4, .)
scalar z = zref

* Situation de référence
preserve

sort pcexp
sum hhweight
scalar totalW = r(sum)

gen w_pcexp = hhweight * pcexp
sum w_pcexp
scalar totalY = r(sum)
scalar mu = totalY / totalW

gen pauvre = pcexp < z
sum pauvre [iw=hhweight]
scalar P0 = r(mean) * 100

gen p1 = ((z - pcexp) / z) * pauvre
sum p1 [iw=hhweight]
scalar P1 = r(mean) * 100

gen p2 = ((z - pcexp) / z)^2 * pauvre
sum p2 [iw=hhweight]
scalar P2 = r(mean) * 100

gen cumw = sum(hhweight)
gen cumwfrac = cumw / totalW

gen cumwinc = sum(w_pcexp)
gen Lor = cumwinc / totalY

gen LagLor = Lor[_n-1]
replace LagLor = 0 if _n == 1
gen dX = cumwfrac - cumwfrac[_n-1]
replace dX = cumwfrac if _n == 1
gen trapeze = dX * (Lor + LagLor)
sum trapeze
scalar A = r(sum)
scalar Gini = (1 - A) 

* Stocker dans la première ligne
matrix indicateurs_final[1, 1] = P0
matrix indicateurs_final[1, 2] = P1
matrix indicateurs_final[1, 3] = P2
matrix indicateurs_final[1, 4] = Gini

restore

* calcul des indicateurs pour les scenarios
local i = 2
foreach s in 1 2 3 4 5 6 7 8 {
    preserve

    sort pcexp_post`s'

    sum hhweight
    scalar totalW = r(sum)

    gen w_pcexp = hhweight * pcexp_post`s'
    sum w_pcexp
    scalar totalY = r(sum)
    scalar mu = totalY / totalW

    gen pauvre = pcexp_post`s' < z
    sum pauvre [iw=hhweight]
    scalar P0 = r(mean) * 100

    gen p1 = ((z - pcexp_post`s') / z) * pauvre
    sum p1 [iw=hhweight]
    scalar P1 = r(mean) * 100

    gen p2 = ((z - pcexp_post`s') / z)^2 * pauvre
    sum p2 [iw=hhweight]
    scalar P2 = r(mean) * 100

    gen cumw = sum(hhweight)
    gen cumwfrac = cumw / totalW

    gen cumwinc = sum(w_pcexp)
    gen Lor = cumwinc / totalY

    gen LagLor = Lor[_n-1]
    replace LagLor = 0 if _n == 1
    gen dX = cumwfrac - cumwfrac[_n-1]
    replace dX = cumwfrac if _n == 1
    gen trapeze = dX * (Lor + LagLor)
    sum trapeze
    scalar A = r(sum)
    scalar Gini = (1 - A) 

    matrix indicateurs_final[`i', 1] = P0
    matrix indicateurs_final[`i', 2] = P1
    matrix indicateurs_final[`i', 3] = P2
    matrix indicateurs_final[`i', 4] = Gini

    restore
    local ++i
}

* Sauvegarde des résultats

	** Transposer la matrice
matrix indicateurs_transpose = indicateurs_final'

	** Export vers Excel
putexcel set "`result_path'\outputs\indices_FGT_Gini_transpose.xlsx", sheet("Résultats") replace

* Écrire manuellement les noms des scénarios en B1 à J1
putexcel B1 = "Réf_2023"
putexcel C1 = "Universel"
putexcel D1 = "Rural"
putexcel E1 = "under2"
putexcel F1 = "under2_rural"
putexcel G1 = "under5"
putexcel H1 = "under18"
putexcel I1 = "Elder"
putexcel J1 = "Handicap"

* Écrire les noms des indices 
putexcel A1 = "Scénarios"
putexcel A2 = "P0 (%)"
putexcel A3 = "P1 (%)"
putexcel A4 = "P2 (%)"
putexcel A5 = "Gini "

* Coller la matrice transposée (indices en lignes, scénarios en colonnes)
putexcel B2 = matrix(indicateurs_transpose)
	** Coller la matrice (indices en lignes, scénarios en colonnes)
putexcel B2 = matrix(indicateurs_transpose)

***************************************************************************
*
*                    7. Calcul de L'efficacité des scenarios
*
***************************************************************************

* Initialisation d'une ligne (1 × 8) pour les efficacités
matrix efficacite = J(1, 8, .)

* Extraire P1 de base (référence)
scalar P1_base = indicateurs_final[1, 2]

forvalues i = 1/8 {
    * P1 après transfert
    scalar P1_s = indicateurs_final[`i'+1, 2]

    * Gap de la base et final 
    scalar gap_reduction = P1_base - P1_s

    * Coût total du scénario `i`
    scalar cout_total = results[`i', 2]

    * Calcul efficacité (% de réduction du gap par FCFA dépensé)
    scalar eff = (gap_reduction / cout_total)
    matrix efficacite[1, `i'] = eff
}

* Enregistrer dans la base 
putexcel A6 = "Efficacité"
putexcel C6 = matrix(efficacite)

***************************************************************************
*
*                    8. Résultats selon le milieu de residence
*
***************************************************************************

* Initialiser matrices pour résultats (8 scénarios × 4 indices × 2 milieux)
matrix FGT_urbain = J(8, 4, .)
matrix FGT_rural  = J(8, 4, .)

scalar z = zref

local scen = 1
foreach s in 1 2 3 4 5 6 7 8 {
    
    foreach m in 1 2 {
        preserve

        * Restreindre au milieu m (1 = urbain, 2 = rural)
        keep if milieu == `m'

        * Moyenne pondérée
        gen w_pcexp = hhweight * pcexp_post`s'
        sum w_pcexp
        scalar totalY = r(sum)

        sum hhweight
        scalar totalW = r(sum)
        scalar mu = totalY / totalW

        * FGT
        gen pauvre = pcexp_post`s' <= z
        sum pauvre [iw=hhweight]
        scalar P0 = r(mean) * 100

        gen p1 = ((z - pcexp_post`s') / z) * pauvre
        sum p1 [iw=hhweight]
        scalar P1 = r(mean) * 100

        gen p2 = ((z - pcexp_post`s') / z)^2 * pauvre
        sum p2 [iw=hhweight]
        scalar P2 = r(mean) * 100

        * Gini (méthode des trapèzes)
        sort pcexp_post`s'
        gen cumw = sum(hhweight)
        gen cumwfrac = cumw / totalW

        gen cumwinc = sum(w_pcexp)
        gen Lor = cumwinc / totalY
        gen LagLor = Lor[_n-1]
        replace LagLor = 0 if _n == 1
        gen dX = cumwfrac - cumwfrac[_n-1]
        replace dX = cumwfrac if _n == 1
        gen trapeze = dX * (Lor + LagLor)
        sum trapeze
        scalar A = r(sum)
        scalar Gini = (1 - A) 

        * Stockage dans la matrice correspondante
        if `m' == 1 {
            matrix FGT_urbain[`scen', 1] = P0
            matrix FGT_urbain[`scen', 2] = P1
            matrix FGT_urbain[`scen', 3] = P2
            matrix FGT_urbain[`scen', 4] = Gini
        }
        else if `m' == 2 {
            matrix FGT_rural[`scen', 1] = P0
            matrix FGT_rural[`scen', 2] = P1
            matrix FGT_rural[`scen', 3] = P2
            matrix FGT_rural[`scen', 4] = Gini
        }

        restore
    }

    local ++scen
}

* Sauvegarde des résultats
	** Labels de lignes
putexcel set "`result_path'\outputs\indices_urbain_rural.xlsx", sheet("Urbain_rural") replace

putexcel C1= "Urbain" I1= "Rural" C12= "Situation de référence 2023"

putexcel A2 = "Scénario" B2 = "P0 (%)" C2 = "P1 (%)" D2 = "P2 (%)" E2 = "Gini "
putexcel G2 = "Scénario" H2 = "P0 (%)" I2 = "P1 (%)" J2 = "P2 (%)" K2 = "Gini "
putexcel C13 = "P0 (%)" D13 = "P1 (%)" E13 = "P2 (%)" F13 = "Gini "

putexcel A3 = ("Universel") A4 = ("Rural") A5 = ("under2") A6 = ("under2_rural") A7 = ("under5") A8 = ("under18") A9 = ("Elder") A10 = ("Handicap") 

putexcel G3 = ("Universel") G4 = ("Rural") G5 = ("under2") G6 = ("under2_rural") G7 = ("under5") G8 = ("under18") G9 = ("Elder") G10 = ("Handicap") 

putexcel B3 = matrix(FGT_urbain) 
putexcel H3	= matrix(FGT_rural)

* Sauvegarde des valeurs pour la situation de référence
scalar C14 = indicateurs_final[1, 1] 
scalar D14 = indicateurs_final[1, 2] 
scalar E14 = indicateurs_final[1, 3] 
scalar F14 = indicateurs_final[1, 4] 

putexcel C14 = C14
putexcel D14 = D14 
putexcel E14 = E14 
putexcel F14 = F14 



***************************************************************************
*                                                                         *
*                    	"--"FIN DO-FILE "--"                              *
*                                                                         *
***************************************************************************
