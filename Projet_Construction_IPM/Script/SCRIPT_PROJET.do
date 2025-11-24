******************************************************************************
*                     TP LOGICIEL STATA										 *
*                          M. CISSE	                                         *
*		Calcul de l'Indice Multidimensionnelle de pauvrete(IPM)              *
*				Methode Sabina Alkire et James Foster                        *
*   		Auteurs : Papa Magatte   Diop et Aîssatou Gueye                  *
*                                                                            *
******************************************************************************
	*** Vider la mémoire
	clear all
	*** 
	set more off 
	*** 
	
** Importation de la base
cd "C:\Users\LENOVO\Desktop\ECOLE\ENSAE\AS2_2024_2025\SEMESTE2\STATA\PROJET"
use "data\ehcvm_individu_SEN2021.dta", clear

** METHODOLOGIE
/*
Partie 1 : Indentifications des indicateurs qui nous interresse et céations 
		   de la matrice d'accomplissements
Partie 2 : Création de la matrice des privations 

*/
*-------------------------------------------------------------------------------
*___ Partie 1 :  Indentifications des indicateurs qui nous interresse et céations de la matrice d'accomplissements
*-------------------------------------------------------------------------------
/* Dans cette partie, nous allons identifier ou bien creer  les variables 
qui ont ete retenu pour le calcul de l'IPM d'après l'ANSD
*/

* Indentifation de nos variables d'intêret
*** Pour la dimension Education

* Indicateur / variable correspondante dans la base / base
* Fréquentation scolaire / educ_scol / individu
* Retard scolaire / age vs educ_scol  / individu
* Années de scolarité /  educ_ hi individu
* Alphabétisation / alfa / individu

*** Pour la dimension Sante
* Couverture maladie / couvmal / individu
* Maladies et problèmes de santé  / s03q02 / s03_me_SEN2021
* Qualité des services de Sante / s03q09 / s03_me_SEN2021
* Vaccination des enfants de 0 - 6 ans / s03q52 / s03_me_SEN2021
* Handicap physique et mental / s03q03 /s03_me_SEN2021


*** Pour la dimension  Emploi
* Dépendance économique /
* travail des enfants

*** Pour la dimension condition de vie
* - Type de logement (s11q01)
* - Électricité (s11q37)
* - Évacuation eaux usées (s11q59)
* - Nombre de pièces (s11q02)
* - Eau potable (s11q26b)
* - Énergie de cuisson (s11q52_*)
* - Équipement sanitaire (s11q54)


*** Pour la dimension  Gouvernance et institutions
* Corruption /  s20bq12__3 /  s20b_1_me_SEN2021
* Agression et vol / s20cq02 vs s20cq04  / s20b_1_me_SEN2021

*_______________________________________________________________________________
///  Création de la matrice des score de  bien être ou d'accomplissements
*_______________________________________________________________________________
use "data\ehcvm_individu_SEN2021.dta", clear

*****************************Dimension Education******************************* 

*                         Création des variables 
* Frequentation
capture drop frequentation
clonevar frequentation = educ_hi
lab var frequentation "Niveau d'éducation"

* Alphabétisation
lab var alfa "Alphabétisation des individus"

* nombres d'années scolaire
gen annee_scolaire = 0 if educ_hi==1
replace annee_scolaire= 1 if educ_hi==2
replace annee_scolaire= 2 if educ_hi==3
replace annee_scolaire= 3 if educ_hi==4
replace annee_scolaire= 4 if educ_hi==5
replace annee_scolaire= 5 if educ_hi==6
replace annee_scolaire= 6 if educ_hi==7
replace annee_scolaire= 7 if educ_hi==8
replace annee_scolaire= 8 if educ_hi==9

*** definition des labels
label define annee_scolairelabel 0 "Aucun " 1 "Au moins 1" 2 "Au moins 2"  3 "Au moins 3" 4 "Au moins 4" 5 "Au moins 5" 6 "Au moins 6" 7 "Au moins 7" 8 "Au moins 8" 9 "Au moins 9"

label values annee_scolaire annee_scolairelabel
lab var annee_scolaire "nombres d'annees scolaire"

* Nombres d'années de retard scolaire

/*
Au Sénégal, l'âge pour chaque niveau d'étude est le suivant : 
Préscolaire: 3 à 5 ans.
Enseignement primaire: 6 à 11 ans.
Enseignement moyen: 12 à 15 ans.
Enseignement secondaire: 16 à 18 ans.
Enseignement supérieur: À partir de 19 ans.
*/

* Determination des individus en retard
gen en_retard =""
replace en_retard="retard préscolaire" if educ_scol==1 & age > 5
replace en_retard="retard primaire" if educ_scol==2  & age  > 11
replace en_retard="retard moyen" if (educ_scol==3  |educ_scol==4) & age > 15
replace en_retard="retard secondaire" if (educ_scol==5 |educ_scol==6)  & age > 18
replace en_retard ="autres" if missing(en_retard)

* codifier la variable 
encode en_retard, gen(is_retard)

* Determination du nombres d'années de retard 
gen nb_a_retard=0
lab var nb_a_retard "nombres d'année de retard scolaire"
replace nb_a_retard= age - 5 if is_retard==4
replace nb_a_retard= age - 11 if is_retard==3
replace nb_a_retard= age - 15 if is_retard==2
replace nb_a_retard= age - 18 if is_retard==5

keep hhid nb_a_retard annee_scolaire alfa frequentation age 
save "data\d_education.dta", replace


******************************* Dimension sante *******************************
use "Data\base_section\s03_me_SEN2021.dta",clear

* Qualité des services de Sante

local var  s03q10__1 s03q10__2 s03q10__3 s03q10__4 s03q10__5 s03q10__6 s03q10__8
 foreach i in `var'{
	replace `i'=0 if `i'==2
}

gen nb_pen_qs =  s03q10__1 + s03q10__2 + s03q10__3 + s03q10__4 + s03q10__5 + s03q10__6 + s03q10__8 
lab var  nb_pen_qs "Qualité du service de santé "

* Maladies et problèmes de Santé
replace s03q02 = 0 if s03q01==2
drop if missing(s03q02)

label define s03q02 1 "Fièvre/Paludisme" 2 "Diarrhée" 3 "Accident/Blessure" 4 "Problème dentaire" 5 "Problème de peau" 6 "Maladie des yeux" 7 "Problème de tension" 8 "Fièvre typhoïde" 9 "Problème d'estomac (ulcère, cancer, etc)" 10 "Mal de gorge" 11 "Toux, rhume " 12 "Diabète" 13 "Meningite" 14 "COVID-19" 15 "Complications liées à grossesse ou à l'accouchement" 16 "Douleurs/fatigue" 17 "Anémie/drépanocytose" 18 "Autre" 19 "Maux de ventre" 20 "Probleme respiratoire" 0 "Pas de maladies", replace

clonevar maladie = s03q02 
lab var maladie "Principal problème de sante de l'individu"

* Vaccination des enfants de 0-6 ans
clonevar vaccination_enfant = s03q52
replace vaccination_enfant=	0 if vaccination_enfant==2 | missing(vaccination_enfant)
label define vaccination_enfant 1 "Oui" 0 "Non"
label values vaccination_enfant vaccination_enfant
lab var vaccination_enfant "Est ce que l'enfant à pris tous ses vaccins obligatoires"
 
* Handicap physique et mental
clonevar handicap = s03q03 
lab var handicap "l'individu a un handicap physique ou mentale empéchant son activité"

* Création de l'id
tostring grappe, gen(grap)
tostring menage, gen(men)
gen id =cond(strlen(men)==1,grap+"0"+men,grap+men)

destring id,gen(hhid)
drop id grap men

* Merge avec la base individu pour recup la variable converture maladies
merge m:m hhid using "data\ehcvm_individu_SEN2021.dta", keepusing(couvmal) nogen
* Couverture maladie 
lab var couvmal "Couverture maladie"

keep hhid couvmal handicap vaccination_enfant maladie nb_pen_qs
save "data\d_sante.dta", replace


****************** Dimension gouvernance et institutions***********************
use "data\base_section\s20b_1_me_SEN2021",clear

* Corruption
clonevar corruption = s20bq12__3 
lab var corruption "Si le ménage a subi un racket"
label define ouinon 0 "oui" 1 "non"
label values corruption ouinon

* création de l'id
tostring grappe, gen(grap)
tostring menage, gen(men)
gen id =cond(strlen(men)==1,grap+"0"+men,grap+men)

destring id,gen(hhid)
drop id grap men

keep hhid corruption

save "data\corruption.dta",replace

* Agression et vol 
use "data\base_section\s20c_me_SEN2021", clear 

clonevar nb_agress_vol = s20cq03
replace nb_agress_vol =0 if missing(nb_agress_vol)
lab var nb_agress_vol "nombres de personnes ayant subi une agression ou vol dans le menage"

* création de l'id
tostring grappe, gen(grap)
tostring menage, gen(men)
gen id =cond(strlen(men)==1,grap+"0"+men,grap+men)

destring id,gen(hhid)
drop id grap men

* Merging
merge 1:1 hhid using "Data\corruption.dta" , nogen

keep hhid nb_agress_vol corruption 
save "data\d_gouv_inst.dta",replace


******************************** dimension emploi ******************************
use "Data\base_section\s01_me_SEN2021.dta", clear
rename s01q04a age

gen tranche_age = .
replace tranche_age = 0 if age < 15
replace tranche_age = 1 if age >= 15 & age <= 65
replace tranche_age = 2 if age > 65

label define tranche_age_lbl 0 "Moins de 15 ans" ///
                           1 "15 à 65 ans" ///
                           2 "Plus de 65 ans"				
label values tranche_age tranche_age_lbl		
				
gen moins15=cond(tranche_age==0,1,0)
gen entre15_65=cond(tranche_age==1,1,0)
gen plus65=cond(tranche_age==2,1,0)

* Convertion en str
tostring menage, gen(menage_)
tostring grappe, gen(grappe_)

gen hhid1=cond(strlen(menage_)==1,grappe_+"0"+menage_,grappe_+menage_)
destring hhid1, gen(hhid) 
drop hhid1 grappe_ menage_

egen a_moins_de15 = total(moins15), by(hhid)
egen a_entre_15_65 = total(entre15_65), by(hhid)
egen a_plus_de65 = total(plus65), by(hhid)

keep hhid age a_moins_de15 a_entre_15_65 a_plus_de65
save "data\emploi1.dta", replace

keep age hhid
save "data\age_indiv.dta", replace

*Identification des variables de la dimension emploi 
use "data\base_section\s04a_me_SEN2021.dta", clear

* Convertion en str
tostring menage, gen(menage_)
tostring grappe, gen(grappe_)
* création de hhid
gen hhid1=cond(strlen(menage_)==1,grappe_+"0"+menage_,grappe_+menage_)
destring hhid1, gen(hhid) 
drop hhid1 grappe_ menage_

merge m:m hhid using "data\age_indiv.dta"
drop _merge

///définition des population 
 
*************  Population active 
replace s04q10=0 if missing(s04q10)
replace s04q11=0 if missing(s04q11)
replace s04q13=0 if missing(s04q13)
replace s04q14=0 if missing(s04q14)

gen pop_activ=cond(age>=15 & s04q10==1 | s04q11==1 | s04q13==1 | s04q14==1 | s04q17==1,1,0) 
label var pop_activ "Population active"
label define pop_activ 1 "actif" 2 "non actif"

egen actif_par_menage = total(pop_activ), by(hhid)
lab var actif_par_menage "nombre d'actifs dans le ménage"

* l'activité chez les enfants 
gen enfant_actif=cond(age<15 & s04q10==1 | s04q11==1 | s04q13==1 | s04q14==1 | s04q17==1,1,0) 
label var enfant_actif "les enfants actifs"
label define enfant_actif 1 "enfant actif" 2 "enfant non actif"

egen child_actif_par_menage = total(enfant_actif), by(hhid)
lab var child_actif_par_menage "nombre d'actifs dans le ménage"

***********  population occupée 
replace s04q10=0 if missing(s04q10)
replace s04q11=0 if missing(s04q11)
replace s04q13=0 if missing(s04q13)
replace s04q14=0 if missing(s04q14)

gen pop_occup=cond(age>=15 & s04q10==1 | s04q11==1 | s04q13==1 | s04q14==1| s04q15==1,1,0)
label var pop_occup "Population occupée"
label define pop_occup 1 "occupé(e)" 0 "non occupé(e)"

egen occup_par_menage = total(pop_occup), by(hhid)
lab var occup_par_menage "nombre d'occupés dans le ménage"

* l'occupation chez les enfants 
gen enf_occup=cond(age<15 & s04q10==1 | s04q11==1 | s04q13==1 | s04q14==1| s04q15==1,1,0)
label var enf_occup "Population d'enfants occupés"
label define enf_occup 1 "enfant occupé(e)" 0 "enfant non occupé(e)"

egen chlid_occup_par_menage = total(enf_occup), by(hhid)
lab var chlid_occup_par_menage "nombre d'enfants occupés dans le ménage"

keep hhid occup_par_menage chlid_occup_par_menage actif_par_menage child_actif_par_menage
save "data\emploi2.dta", replace

//********  récupation des dimensions
use "data\emploi2.dta", clear

merge m:m hhid using "Data\emploi1.dta"
drop _merge
duplicates drop hhid, force
save "data\emploi3.dta", replace

use "data\emploi3.dta", clear 


	*Dépendance économique : le taux de dépendance est inférieur ou égal à 2

	gen taux_dependance = ((a_moins_de15 + a_plus_de65) / actif_par_menage)

keep hhid chlid_occup_par_menage taux_dependance
save "data\d_emploi.dta" , replace 


************************** dimension conditions de vie ************************
use "data\base_section\s11_me_SEN2021.dta", clear

keep grappe menage s11q01 s11q37 s11q59 s11q54 s11q02 s11q26b
save "data\Mat_accomplissement0.dta", replace

/*** Etape 2 : Création identifiant unique du ménage ***/
tostring menage, gen(menage_)
tostring grappe, gen(grappe_)
gen hhid1 = cond(strlen(menage_) == 1, grappe_ + "0" + menage_, grappe_ + menage_)
destring hhid1, gen(hhid)
drop hhid1 grappe_ menage_
save "data\Mat_accomplissement0.dta", replace

/*** Etape 3 : Ajout des équipements ***/
use "data\ehcvm_menage_SEN2021.dta", clear
gen nb_equipm = ordin + tv + fer + frigo + cuisin + decod + car
save "data\ehcvm_menage_SEN2021_copie.dta", replace

use "data\Mat_accomplissement0.dta", clear
merge 1:1 hhid using "data\ehcvm_menage_SEN2021_copie.dta", keepusing(nb_equipm cuisin)
drop _merge
save "data\Mat_accomplissement1.dta", replace

/*** Etape 4 : Ajout de la taille du ménage ***/
merge 1:1 hhid using "data\ehcvm_welfare_SEN2021.dta", keepusing(hhweight hhsize) nogen
save "data\Mat_accomplissement2.dta", replace

/*** Etape 5 : Calcul de l'indice de surpeuplement ***/
use "data\Mat_accomplissement2.dta", clear
gen ind_surpl = hhsize / s11q02
drop s11q02 

/*** Etape 6 : Réorganisation et nommination des variables ***/
rename s11q01 type_logment
rename s11q26b eau_potable
rename s11q37 electricite
rename s11q54 sanitaire
rename cuisin energ_cuis
rename s11q59 eaux_usees
rename nb_equipm nb_biens

save "data\d_cond_vie.dta", replace


********* Création de la matrice de bien être ou d'accomplissements
merge 1:1 hhid using "data\d_gouv_inst",nogen
merge 1:m hhid using "data\d_sante.dta", nogen
merge m:m hhid using "data\d_education.dta", nogen
merge m:1 hhid using "data\d_emploi",nogen


* Sauvegarde de la matrice d'accomplissements
save "data\matrice_accom.dta",replace

merge m:1 hhid using "data\ehcvm_welfare_SEN2021.dta",nogen keepusing(hhweight zref hhsize)

save "data\matrice_accom.dta",replace

*-------------------------------------------------------------------------------
*____ Partie 2 : Création de la matrice des privations ___*
*-------------------------------------------------------------------------------
* Réorganisation des colonnes
move hhid grappe
move hhweight menage

use "data\matrice_accom.dta", clear

**** Traitemant des valeurs manquantes
replace handicap=2 if maladie==0
replace nb_pen_qs=0 if missing(nb_pen_qs)
*_______________________________________________________________________________
///            Determination des privations 
*_______________________________________________________________________________

*****************************Dimension Education*******************************
* Frequentation scolaire
gen indiv_freq =cond(inrange(age,6,16)& frequentation==1,1,0)
egen pr_freq = max (indiv_freq), by(hhid)
lab var pr_freq "fréquentation scolaire"

* Retard scolaire
gen late_scol =cond(inrange(age,8,13)& nb_a_retard>2,1,0)
egen pr_late_scol =max(late_scol),by(hhid)
lab var pr_late_scol "retard scolaire"

* Nombres d'années de scolarite
gen annee_scol =cond(age>15 & annee_scolaire==6,1,0)
egen pr_annee_scol= max(annee_scol),by(hhid)
lab var pr_annee_scol "nombres d'années de scolarite"

* Alphabétisation
** Determination des alpahabètes qui ont plus de 15 ans 
gen n_alfa_15=cond(alfa==0 & age>=15,1,0)

** Total par menage
egen t_nalfa_15 = total(n_alfa_15),by(hhid)

** Les privations
gen pr_alfa =cond(t_nalfa_15>(1/4*hhsize),1,0)
lab var pr_alfa "Alphabetisation du menage"


******************************* Dimension sante *******************************
* Couverture maladie
** Nombre de couverture par ménage
egen t_couv_mal= total(couvmal), by(hhid)

** Les privations
gen pr_assurance =cond(t_couv_mal<(1/3*hhsize),1,0)
lab var pr_assurance " Couverture maladie"

* Qualité des services de santé
gen qs_sante= (nb_pen_qs > 5)

egen pr_qsante= max(qs_sante),by(hhid)
lab var pr_qsante "qualites des services de sante"

* Maladies et problèmes de santé
gen affecte=inlist(maladie,7,12,17)

egen pr_maladie =max(affecte),by(hhid)
lab var pr_maladie "maladie et problème de sante"

* Vaccination des enfant
egen pr_vacciantion=max(vaccination_enfant),by(hhid)
lab var pr_vacciantion "vaccination des enfants"

* Handicap physique et mental
egen pr_handicap =max(handicap),by(hhid)
replace pr_handicap=0 if pr_handicap==2
lab var pr_handicap "handicap physique et mental"


****************** Dimension gouvernance et institutions***********************
* Corruption
egen pr_corruption=max(corruption),by(hhid)
lab var pr_corruption "corruption"

* Agression et vol
gen subi_agress= (nb_agress_vol!=0)

egen pr_agres_vol =max(subi_agress),by(hhid)
lab var pr_agres_vol "Agression et Vol"


************************** dimension conditions de vie ************************
/*** Création des indicateurs de privation ***/

* 1. Type de logement (privé si case ou baraque)
gen pr_type_log = (type_logment == 4 | type_logment == 5)
label variable pr_type_log     "Type de logement"

* 2. Électricité (privation si autre que électricité, groupe électrogène ou solaire)
gen pr_electr = !(electricite == 1 | electricite == 2 | electricite == 3)
label variable pr_electr  "Électricité"

* 3. Évacuation eaux usées (privation si cour ou nature)
gen pr_evac_eu = !(eaux_usees == 1 | eaux_usees == 2)
label variable pr_evac_eu      "Évacuation eaux usées"

* 5. Surpeuplement (privation si plus de 3 personnes par pièce)
gen pr_surpeuplemnt = (ind_surpl >= 3)
label variable pr_surpeuplemnt "Surpeuplement"

* 6. Eau potable (privation si source non améliorée)
gen pr_eau_pot = !(inlist(eau_potable, 1, 2, 3, 11, 14, 16))
label variable pr_eau_pot "Eau potable"

* 7. Énergie de cuisson (privation si pas d'énergie propre)
gen pr_energ_cuis = (energ_cuis == 0)
label variable pr_energ_cuis   "Énergie pour cuisson"

* 8. Équipements sanitaires (privation si toilettes non améliorées)
gen pr_equipm_sanit = !(inlist(sanitaire, 1, 2, 3, 4))
label variable pr_equipm_sanit "Équipement sanitaire"

* 9. Biens d'équipement (privation si < 2 équipements)
gen pr_bien_equipm = (nb_biens < 2)
label variable pr_bien_equipm  "Biens et équipements"


******************************** dimension emploi ******************************
*Dépendance économique : le taux de dépendance est inférieur ou égal à 2
gen pr_depd_eco =cond(taux_dependance>2,1,0)
label values pr_depd_eco ind_depd_eco_label
label var pr_depd_eco "indicateur de Dépendance économique"

*Travail des enfants : le ménage ne compte aucun enfant de moins de 15 ans exerçant un travail
gen pr_trav_child =cond(chlid_occup_par_menage>=1,1,0)
label values pr_trav_child ind_travchild_label
label var pr_trav_child "travail des enfants"


///              Sauvegarde de la matrice des privations
keep hhid hhweight pr_*
save "data\matrice_privations.dta", replace
*-------------------------------------------------------*
* Étape 1 : Pondération des indicateurs (pondération globale)
* Chaque dimension reçoit un poids de 1/5 = 0.2
* Pondération à l'intérieur de chaque dimension = 0.2 / nb_indicateurs
*-------------------------------------------------------*
use "data\matrice_privations.dta", clear
svyset [pw=hhweight]

gen poids_sant= 0.04
gen poids_edu= 0.05
gen poids_gouv_inst =0.1
gen poids_emploi= 0.1
gen poids_cond_vie= 0.025

ds pr_freq - pr_alfa
local ind_dim `r(varlist)'
foreach i of local ind_dim {
	gen scor_`i'= `i' * poids_edu
}

ds pr_assurance - pr_handicap
local ind_dim `r(varlist)'
foreach i of local ind_dim {
	gen scor_`i'= `i' * poids_sant
}

ds pr_corruption - pr_agres_vol
local ind_dim `r(varlist)'
foreach i of local ind_dim {
	gen scor_`i'= `i' * poids_gouv_inst
}

ds pr_type_log - pr_bien_equipm
local ind_dim `r(varlist)'
foreach i of local ind_dim {
	gen scor_`i'= `i' * poids_cond_vie
}

ds pr_depd_eco - pr_trav_child
local ind_dim `r(varlist)'
foreach i of local ind_dim {
	gen scor_`i'= `i' * poids_emploi
}

*-------------------------------------------------------*
* Étape 2 : Cumul pondéré des privations
*-------------------------------------------------------*
egen cumul_pr =rowtotal(pr_freq-pr_trav_child)
label variable cumul_pr "Score total des privations"

egen scor_priv =rowtotal(scor_pr_freq-scor_pr_trav_child)
label variable scor_priv "Score total des privations pondéré "

*-------------------------------------------------------*
* Étape 3 : Identification des pauvres multidimensionnels
* Seuil fixé à 33% le seuil standard (4/12=0.33)
*-------------------------------------------------------*
gen is_pauvre = (scor_priv >= 0.33)
label variable is_pauvre "Pauvre multidimensionnel (1=Oui)"

*-------------------------------------------------------*
* Étape 4 : Calcul de l'incidence (H)
*-------------------------------------------------------*
svy: mean is_pauvre
scalar H_scl =r(table)[1,1]

gen H = H_scl
scalar drop H_scl
label variable H "Incidence H de la pauvreté multidimensionnelle"

*-------------------------------------------------------*
* Étape 5 : Calcul de l'intensité (A)
*-------------------------------------------------------*
gen nb_ind= 21
gen indiv_priv= cumul_pr/nb_ind 

svy, subpop(is_pauvre) : mean indiv_priv
scalar A_scl =r(table)[1,1]

gen A = A_scl
scalar drop A_scl
label variable A "Intensité moyenne A chez les pauvres"

*-------------------------------------------------------*
* Étape 6 : Calcul de l'IPM
*-------------------------------------------------------*
gen IPM = H * A
label variable IPM "Indice de pauvreté multidimensionnelle"

*-------------------------------------------------------*
* Étape 7 : Sauvegarde des résultats
*-------------------------------------------------------*
save "data\base_IPM.dta", replace

*____________________________________________________________________________
*______________________ Partie: Analyse de l'IPM_____________________________
*____________________________________________________________________________
use "data\base_IPM.dta", clear 
merge m:1 hhid using "data\ehcvm_welfare_SEN2021.dta", keepusing(region pcexp hmstat hgender heduc milieu hhsize dtot) nogen

*duplicates drop hhid, force
save "data\Base_finale.dta", replace
 
* Utilisation des poids d'échantillonnage
local poids "[aweight=hhweight]"

/// Contribution des dimensions 
preserve
tempname contrib_results
postfile `contrib_results' str20 dimension float contribution using "contributions.dta", replace

* Définition des dimensions dans une matrice
matrix define dims = (1,2,3,4,5)
matrix colnames dims = sante education condition_vie emploi gouvernance

local dim_vars_1 "pr_assurance pr_qsante pr_maladie pr_vacciantion pr_handicap"
local dim_vars_2 "pr_freq pr_late_scol pr_annee_scol pr_alfa"
local dim_vars_3 "pr_type_log pr_electr pr_evac_eu pr_surpeuplemnt pr_eau_pot pr_energ_cuis pr_equipm_sanit pr_bien_equipm"
local dim_vars_4 "pr_depd_eco pr_trav_child"
local dim_vars_5 "pr_corruption pr_agres_vol"

local poids_1 = 0.04
local poids_2 = 0.05
local poids_3 = 0.025
local poids_4 = 0.1
local poids_5 = 0.1

* Calcul automatique pour chaque dimension
forvalues i = 1/5 {
    local total_contrib = 0
    foreach var of local dim_vars_`i' {
        quietly sum `var' if is_pauvre == 1
        local total_contrib = `total_contrib' + r(mean)
    }
    local final_contrib = 100*((`total_contrib' * `poids_`i'') / A)
    
    local dim_name : word `i' of "Santé" "Éducation" "Conditions_vie" "Emploi" "Gouvernance"
    post `contrib_results' ("`dim_name'") (`final_contrib')
}
postclose `contrib_results'

* Affichage des résultats
use "contributions.dta", clear
list
drop if _n > _N
restore


/// Contribution des indicateurs

local indicateurs "pr_freq pr_late_scol pr_annee_scol pr_alfa pr_assurance pr_qsante pr_maladie pr_vacciantion pr_handicap pr_corruption pr_agres_vol pr_type_log pr_electr pr_evac_eu pr_surpeuplemnt pr_eau_pot pr_energ_cuis pr_equipm_sanit pr_bien_equipm pr_depd_eco pr_trav_child"
local dimensions `" "ÉDUCATION" "ÉDUCATION" "ÉDUCATION" "ÉDUCATION" "SANTÉ" "SANTÉ" "SANTÉ" "SANTÉ" "SANTÉ" "GOURVANCE ET INSTITUTIONS" "GOURVANCE ET INSTITUTIONS" "CONDITION DE VIE" "CONDITION DE VIE" "CONDITION DE VIE" "CONDITION DE VIE" "CONDITION DE VIE" "CONDITION DE VIE" "CONDITION DE VIE" "CONDITION DE VIE" "EMPLOI" "EMPLOI" "' 
local labels `" "fréquentation scolaire" "retard scolaire" "nombres années de scolarite" "Alphabetisation du menage" " Couverture maladie" "qualites des services de sante" "maladie et problème de sante" "vaccination des enfants" "handicap physique et mental" "corruption" "Agression et Vol" "Type de logement" "Électricité" "Évacuation eaux usées" "Surpeuplement" "Eau potable" "Énergie pour cuisson" "Équipement sanitaire" "Biens et équipements" "indicateur de Dépendance économique" "travail des enfants" "'
local weight 0.05 0.05 0.05 0.05 0.04 0.04 0.04 0.04 0.04 0.10 0.10 0.025 0.025 0.025 0.025 0.025 0.025 0.025 0.025 0.1 0.1

* --- Calcul des contributions individuelles ---
local i = 1
foreach var of local indicateurs {
    local dim : word `i' of `dimensions'
    local label : word `i' of `labels'
    local w : word `i' of `weight'
    
    quietly summarize `var' if is_pauvre == 1
    scalar contrib_`var' = (r(mean) * `w') / A
    display "`dim'" _col(25) "`label'" _col(50) %5.1f r(mean)*100 "%" _col(62) %5.1f contrib_`var'*100 "%"
    
    local i = `i' + 1
}



******************** Décomposition par milieu
svyset [pweight=hhweight]

 * On récupère les valeurs uniques du milieu
levelsof milieu, local(milieux)

 * Initialisation de la variable cible
 gen H_milieu = .
 gen A_milieu = .

 * Calcul de l'incidence
 foreach h of local milieux {
    svy, subpop(if milieu == `h'): mean is_pauvre
    scalar H_scl = r(table)[1,1]
    replace H_milieu = H_scl if milieu == `h'
	
	svy, subpop(if milieu == `h' & is_pauvre == 1): mean indiv_priv
    scalar A_scl = r(table)[1,1]
    replace A_milieu = A_scl if milieu == `h'
 }
  
 gen IPM_milieu = H_milieu * A_milieu
table milieu [iw=hhweight], statistic(mean IPM_milieu A_milieu H_milieu)


******************** Décomposition par region
svyset [pweight=hhweight]

 * On récupère les valeurs uniques du milieu
levelsof region, local(les_regions)

 * Initialisation de la variable cible
 gen H_region = .
 gen A_region = .

 * Calcul de l'incidence
 foreach rg of local les_regions {
    svy, subpop(if region == `rg'): mean is_pauvre
    scalar H_scl = r(table)[1,1]
    replace H_region = H_scl if region == `rg'
	
	svy, subpop(if region == `rg' & is_pauvre == 1): mean indiv_priv
    scalar A_scl = r(table)[1,1]
    replace A_region = A_scl if region == `rg'
 }
gen IPM_region = H_region * A_region
table region [iw=hhweight], statistic(mean IPM_region H_region A_region)


******************** Décomposition par milieu et par region de résidence 
table region milieu [iw=hhweight], statistic(mean IPM_region H_region A_region)


******************** Décomposition par genre du sexe du chef de ménage
svyset [pweight=hhweight]
 * On récupère les valeurs uniques du milieu
levelsof hgender, local(sexes)
 * Initialisation de la variable cible
 gen H_genre = .
 gen A_genre = .

 * Calcul de l'incidence
 foreach hg of local sexes {
    svy, subpop(if hgender == `hg'): mean is_pauvre
    scalar H_scl = r(table)[1,1]
    replace H_genre = H_scl if hgender == `hg'
	*------------------------------------------------------------------
	svy, subpop(if hgender == `hg' & is_pauvre == 1): mean indiv_priv
    scalar A_scl = r(table)[1,1]
    replace A_genre = A_scl if hgender == `hg'
 }
gen IPM_genre = H_genre * A_genre
table hgender [iw=hhweight], statistic(mean IPM_genre H_genre A_genre)

***** Loupe sur DAKAR 
preserve
keep if region==1
svyset [pweight=hhweight]
 * On récupère les valeurs uniques du milieu
levelsof hgender, local(sexes)
 * Initialisation de la variable cible
 gen H_genred = .
 gen A_genred = .

 * Calcul de l'incidence
 foreach hg of local sexes {
    svy, subpop(if hgender == `hg'): mean is_pauvre
    scalar H_scld = r(table)[1,1]
    replace H_genred = H_scld if hgender == `hg'
	*------------------------------------------------------------------
	svy, subpop(if hgender == `hg' & is_pauvre == 1): mean indiv_priv
    scalar A_scl = r(table)[1,1]
    replace A_genred = A_scl if hgender == `hg'
 }
gen IPM_genred = H_genred * A_genred
table hgender [iw=hhweight], statistic(mean IPM_genred H_genred A_genred)

restore 

******************** Décomposition par situation matrimoniale du CM
svyset [pweight=hhweight]
 * On récupère les valeurs uniques du milieu
levelsof hmstat, local(mat_sit)
 * Initialisation de la variable cible
 gen H_sitmat = .
 gen A_sitmat = .

 * Calcul de l'incidence
 foreach sm of local mat_sit {
    svy, subpop(if hmstat == `sm'): mean is_pauvre
    scalar H_scl = r(table)[1,1]
    replace H_sitmat = H_scl if hmstat == `sm'
	*------------------------------------------------------------------
	svy, subpop(if hmstat == `sm' & is_pauvre == 1): mean indiv_priv
    scalar A_scl = r(table)[1,1]
    replace A_sitmat = A_scl if hmstat == `sm'
 }
gen IPM_sitmat = H_sitmat * A_sitmat
table hmstat [iw=hhweight], statistic(mean IPM_sitmat H_sitmat A_sitmat)

