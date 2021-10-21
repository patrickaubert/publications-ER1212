# == fonctions auxiliaires pour l'étude sur l'espérance de vie dans l'APA
# ----------------------------------------------------------------------------


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

# ----------------------------------------------------------------------------





# ==
# une fonction pourla manipulation des tranches d'âge
trage_to_text <- function(age) {
  minage <- str_extract(age,"(?<=\\[)[[:digit:]]{2}")
  maxage <- str_extract(age,"(?<=\\[[[:digit:]]{2},)[[:digit:]]{2}") %>%
    as.numeric() %>% (function(x){x-1})() %>% as.character()
  return(paste(minage,ifelse(is.na(maxage),
                             "ans et plus",
                             paste("à",maxage,"ans"))))
}

#== une fonction locale pour l'export des tableaux excel
export_tableau <- function(..., list_meta) {
  exporter_tableau_punaise(...,
                           titre = list_meta$titre,
                           note = list_meta$note, 
                           lecture = list_meta$lecture, 
                           champ = list_meta$champ,
                           sources = list_meta$sources,
                           visualiser = FALSE
                           )
}

#== fonction pour les rubriques "lecture","champ", etc.
texte_notes <- function(meta) {
  t <- ""
  if(!is.null(meta$note)) {t<-paste0(t,"Note >",meta$note,"\n")}
  if(!is.null(meta$lecture)) {t<-paste0(t,"Lecture >",meta$lecture,"\n")}
  if(!is.null(meta$champ)) {t<-paste0(t,"Champ >",meta$champ,"\n")}
  if(!is.null(meta$sources)) {t<-paste0(t,"Sources >",meta$sources,"\n")}
  return(t)
}