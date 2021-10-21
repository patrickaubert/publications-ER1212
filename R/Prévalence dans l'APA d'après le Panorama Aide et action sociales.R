# Copyright (C) 2021. Logiciel élaboré par l'État, via la Drees.
# Nom de l'auteur : Patrick Aubert, Drees.
# Ce programme informatique a été développé par la Drees. Il permet de produire les tableaux de l'ER n°1212 sur l'espérance de vie dans l'APA, ainsi qu'un certain nombre d'analyses complémentaires.
# Ce programme a été exécuté le 24/09/2020 avec la version 4.0.5 de R (voir ci-dessous les informations sur la session).
# Ce programme utilise les données agrégées publiées par la DREES à partir de son enquête annuelle sur l'aide sociale des départements.
# Bien qu'il n'existe aucune obligation légale à ce sujet, les utilisateurs de ce programme sont invités à signaler à la DREES leurs travaux issus de la réutilisation de ce code, ainsi que les éventuels problèmes ou anomalies qu'ils y rencontreraient, en écrivant à DREES-CODE@sante.gouv.fr


# Ce logiciel est régi par la licence "GNU General Public License" GPL-3.0. # https://spdx.org/licenses/GPL-3.0.html#licenseText
# À cet égard l'attention de l'utilisateur est attirée sur les risques associés au chargement, à l'utilisation, à la modification et/ou au développement et à la reproduction du logiciel par l'utilisateur étant donné sa spécificité de logiciel libre, qui peut le rendre complexe à manipuler et qui le réserve donc à des développeurs et des professionnels avertis possédant des connaissances informatiques approfondies. Les utilisateurs sont donc invités à charger et tester l'adéquation du logiciel à leurs besoins dans des conditions permettant d'assurer la sécurité de leurs systèmes et ou de leurs données et, plus généralement, à l'utiliser et l'exploiter dans les mêmes conditions de sécurité.
# Le fait que vous puissiez accéder à cet en-tête signifie que vous avez pris connaissance de la licence GPL-3.0, et que vous en avez accepté les termes.

# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.If not, see <https://www.gnu.org/licenses/>.

# =======
#
# > sessionInfo()
#
#R version 4.0.5 (2021-03-31)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 17134)
#
#Matrix products: default
#
#locale:
#  [1] LC_COLLATE=French_France.1252  LC_CTYPE=French_France.1252    LC_MONETARY=French_France.1252
#[4] LC_NUMERIC=C                   LC_TIME=French_France.1252    
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#other attached packages:
#  [1] healthexpectancies_0.0.0.9000 asdep_0.9.0.0006              forcats_0.5.1                 stringr_1.4.0                
#[5] dplyr_1.0.7                   purrr_0.3.4                   readr_2.0.1                   tidyr_1.1.3                  
#[9] tibble_3.1.4                  ggplot2_3.3.5                 tidyverse_1.3.1               readxl_1.3.1                 
#
#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.7       cellranger_1.1.0 pillar_1.6.2     compiler_4.0.5   dbplyr_2.1.1     tools_4.0.5      lubridate_1.7.10
#[8] jsonlite_1.7.2   lifecycle_1.0.0  gtable_0.3.0     pkgconfig_2.0.3  rlang_0.4.11     reprex_2.0.1     cli_3.0.1       
#[15] rstudioapi_0.13  DBI_1.1.1        haven_2.4.3      xml2_1.3.2       withr_2.4.2      httr_1.4.2       fs_1.5.0        
#[22] generics_0.1.0   vctrs_0.3.8      hms_1.1.0        grid_4.0.5       tidyselect_1.1.1 glue_1.4.2       R6_2.5.1        
#[29] fansi_0.5.0      tzdb_0.1.2       modelr_0.1.8     magrittr_2.0.1   backports_1.2.1  scales_1.1.1     ellipsis_0.3.2  
#[36] rvest_1.0.1      assertthat_0.2.1 colorspace_2.0-2 utf8_1.2.2       stringi_1.7.4    munsell_0.5.0    broom_0.7.9     
#[43] crayon_1.4.1    



# ==================================================================================================
# Prévalence dans l'APA d'après le Panorama Aide et action sociales
# ==================================================================================================


#library(readxl)
library(openxlsx)
library(tidyverse)

options(encoding="UTF8",
        scipen=999)

setwd("C:/Users/patrick.aubert/Documents/Etudes/2021_08_EVAPA/ouverture du code/")



# ===
# Récupération des prévalences du Panorama AAS

# RQ : on a interverti à la main les colonnes APA établissements/domicile dans le fichier 2019, qui semblaient erronnées

aas <- bind_rows(
  read.xlsx("data-raw/aas_fiche_12_apa.xlsx",sheet="G02",
            rows=c(3:27),cols=c(1:5)) %>% mutate(annee = 2016),
  read.xlsx("data-raw/aas19_15_les_be_ne_ficiaires_et_les_de_penses_d_apa_srok.xlsx",sheet="G01",
            rows=c(4:28),cols=c(2:7)) %>% mutate(annee = 2017),
  read.xlsx("data-raw/Fiche 15 - L%u2019allocation personnalisée d%u2019autonomie (APA).xlsx",
            sheet="G01",
            rows=c(4:25),cols=c(2:6)) %>% mutate(annee = 2018),
  read.xlsx("data-raw/APA - Données détaillées par GIR, âge et sexe en 2019.xlsx",
            sheet="nat-part APA dans pop",
            rows=c(3:27),cols=c(2:6)) %>% mutate(annee = 2019)
) %>% 
  rename(sexe = X1,
         age = X2) %>%
  mutate(age = age %>%
           str_replace("(^[Dd]e |^)(?=[[:digit:]]{2})","[") %>%
           str_replace(" à ",",") %>%
           str_replace(" ans (et|ou) plus$",",Inf]"),
         finage = str_replace(age,"\\[[[:digit:]]{2}(,|)",""),
         age = paste0(str_extract(age,"\\[[[:digit:]]{2},"),
                      ifelse(grepl("[[:digit:]]{2} ans$",finage),
                             paste0((1+as.numeric(str_extract(finage,"[[:digit:]]{2}(?= ans)"))),")"),
                             finage ) ),
         #age = recode(age, !!!trancheage),
         sexe = recode(sexe, "Total" = "Ensemble"),
         gir = "Ensemble") %>%
  select(-finage)

for (i in 2:nrow(aas)) {if (is.na(aas$sexe[i])) {aas$sexe[i] <- aas$sexe[i-1]} }

aas <- aas %>%
  pivot_longer(cols=-c("annee","sexe","age","gir"),names_to="prestation",values_to="prevalence") %>%
  mutate(prestation = recode(prestation,
                             "TOTAL" = "APA",
                             "APA.à.domicile" = "APAdom",
                             "APA.en.établissement" = "APAetab"),
         lieu = recode(prestation,
                       "APA" = "ensemble",
                       "APAdom" = "domicile",
                       "APAetab" = "établissement"),
         prevalence = prevalence / ifelse(annee %in% c(2019),100,1))

# ===
# Récupération des populations

# source : https://www.insee.fr/fr/outil-interactif/5014911/pyramide.htm
# téléchargé le 16/05/2021
popnew <- read_csv2("data-raw/donnees_pyramide_act.csv") 
names(popnew) <- tolower(names(popnew))

pop <- popnew %>%
  rename(agefin = age) %>%
  mutate(annee = annee-1) %>% #pour passer de la population au 01/01/N à celle au 31/12/N-1
  select(-sexe) %>%
  filter(agefin>=60,annee>=2016) %>%
  group_by(annee,agefin) %>% summarise_all(sum) %>% ungroup()

popagr <- pop  %>%
  mutate(travt = cut(agefin, breaks=c(60,65,70,75,80,85,90,95,Inf),include.lowest = TRUE, right = FALSE ),
         trapr = cut(agefin, breaks=c(60,65,70,75,80,85,90,Inf),
                     include.lowest = TRUE, right = FALSE ),
         age = ifelse(annee!=2018,as.character(travt),as.character(trapr))) %>%
  select(annee,age,pop) %>%
  group_by(annee,age) %>% summarise_all(sum) %>% ungroup() %>%
  select(annee,age,pop)

aas <- left_join(aas, popagr, by=c("annee","age")) %>%
  mutate(nb = prevalence * pop)

# ==================================================================================================
# 4) Sauvegarde

# dernière réalisation : 24/09/2021
# == historique des versions
# 24/09/2021 : vérif pour ouverture du code
# 24/08/2021 : ajout des données par sexe*âge 2019 (mise en ligne juillet 2021)
# 08/07/2021 : première version

write_csv2(x = aas , 
           file = "data/panorama AAS - prevalences de l APA par âges.csv")

save(aas,file="data/panorama AAS - prevalences de l APA par âges.Rda")

