/*******************************************************************************
    Projet : Analyse des conditions de vie – Matrice des Accomplissements
    Auteur : Papa Magatte Diop
    Date   : Juin 2025
    Objectif : Identification des privations liées aux conditions de vie
*******************************************************************************/

*** 0. Définition du répertoire de travail et chargement des données principales ***
cd "C:\Users\LENOVO\Desktop\ECOLE\ENSAE\AS2_2024_2025\SEMESTE2\STATA\Projet\Base"


/*******************************************************************************
    1. IDENTIFICATION DES VARIABLES POUR LA DIMENSION CONDITIONS DE VIE
*******************************************************************************/

* Logement : s11q01
* Electricité : source d'éclairage : s11q37
* Evacuation eaux usées : s11q59
* Evacuation ordures : s11q53
* Indice de surpeuplement : taille ménage  / nombre de pièces : s11q02
* Eau potable : s11q26b (+ autres)
* Energie de cuisson : s11q52__1 à s11q52__6
* Sanitaires : s11q54
* Biens d'équipements : variables à importer (TV, frigo, etc.)

/*******************************************************************************
    2. CONSTRUCTION DE LA MATRICE DES ACCOMPLISSEMENTS
*******************************************************************************/

*** Étape 1 : Sélection des variables et sauvegarde initiale ***
keep grappe menage s11q01 s11q02 s11q26b s11q37 s11q53 s11q54 s11q59 ///
    s11q52__1 s11q52__2 s11q52__3 s11q52__4 s11q52__5 s11q52__6
save "Mat_accomplissement0.dta", replace

*** Création d'un identifiant unique du ménage ***
use "Mat_accomplissement0.dta", clear
tostring menage, gen(menage_)
tostring grappe, gen(grappe_)
gen hhid1 = cond(strlen(menage_) == 1, grappe_ + "0" + menage_, grappe_ + menage_)
destring hhid1, gen(hhid)
drop hhid1 menage_ grappe_
save "Mat_accomplissement0.dta", replace

*** Étape 2 : Ajout des équipements ménagers ***
use "ehcvm_menage_SEN2021.dta", clear
gen nb_equipm = ordin + tv + fer + frigo + cuisin + decod + car
save "ehcvm_menage_SEN2021_copie.dta", replace

use "Mat_accomplissement0.dta", clear
merge 1:1 hhid using "ehcvm_menage_SEN2021_copie.dta", keepusing(nb_equipm)
drop _merge
save "Mat_accomplissement1.dta", replace

*** Étape 3 : Ajout de la taille du ménage et du poids ***
use "Mat_accomplissement1.dta", clear
merge 1:1 hhid using "C:\Users\LENOVO\Desktop\ECOLE\ENSAE\AS2_2024_2025\SEMESTE2\STATA\Fwd_ Base EHCVM\Fichiers_Traitement&AnalysePauvrete\ehcvm_welfare_SEN2021.dta", keepusing(hhsize hhweight)
drop _merge
save "Mat_accomplissement2.dta", replace

/*******************************************************************************
    3. CONSTRUCTION DES INDICATEURS DE PRIVATION
*******************************************************************************/

use "Mat_accomplissement2.dta", clear

*** Indice de surpeuplement : plus de 3 personnes par pièce ***
gen ind_surpl = hhsize / s11q02
gen surpeuple = ind_surpl > 3

*** Indicateur énergie de cuisson : gaz ou électricité non utilisés ***
gen energ_cuis = (s11q52__4 == 1 | s11q52__4 == 2 | s11q52__5 == 1 | s11q52__5 == 2)

*** Renommage pour plus de clarté ***
rename s11q01 type_logement
rename s11q26b eau_potable
rename s11q37 electricite
rename s11q53 evacuation_ordures
rename s11q54 sanitaire
rename s11q59 evacuation_eaux
rename nb_equipm total_equipements

/*******************************************************************************
    4. CRÉATION DES INDICATEURS DE PRIVATION (1 = Privé, 0 = Non privé)
*******************************************************************************/

*** 1) Type de logement (cases, baraques ou autres) ***
* À adapter avec des modalités précises si disponibles
* Exemple : gen priv_logement = inlist(type_logement, 3, 4, 9)
gen priv_logement = .

*** 2) Électricité non disponible ***
gen priv_electricite = !inlist(electricite, 1, 2, 3)

*** 3) Evacuation des eaux usées dans cour/rue ***
gen priv_eaux = inlist(evacuation_eaux, 2, 3)

*** 4) Évacuation des ordures par tas / rue ***
gen priv_ordures = inlist(evacuation_ordures, 3, 4)

*** 5) Surpeuplement ***
gen priv_surpeuplement = surpeuple

*** 6) Eau non potable ***
gen priv_eau = eau_potable != 1

*** 7) Énergie de cuisson non propre ***
gen priv_cuisson = energ_cuis == 0

*** 8) Sanitaires non améliorés ***
gen priv_sanitaire = !inlist(sanitaire, 1, 2)

*** 9) Moins de 2 équipements parmi les essentiels ***
gen priv_equipement = total_equipements < 2

/*******************************************************************************
    5. LABELISATION (optionnel mais conseillé pour les tableaux)
*******************************************************************************/

label variable priv_logement     "Privation - Type de logement"
label variable priv_electricite  "Privation - Électricité"
label variable priv_eaux         "Privation - Évacuation eaux usées"
label variable priv_ordures      "Privation - Évacuation ordures"
label variable priv_surpeuplement "Privation - Surpeuplement"
label variable priv_eau          "Privation - Eau potable"
label variable priv_cuisson      "Privation - Énergie de cuisson"
label variable priv_sanitaire    "Privation - Sanitaires"
label variable priv_equipement   "Privation - Biens d'équipement"

save "matrice_conditions_vie_finale.dta", replace
