# Copyright (C) 2021. Logiciel élaboré par l'État, via la Drees.
# Nom de l'auteur : DREES.
# Ce programme a été exécuté le 24/09/2020 avec la version 4.0.5 de R (voir ci-dessous les informations sur la session).
# Bien qu'il n'existe aucune obligation légale à ce sujet, les utilisateurs de ce programme sont invités à signaler à la DREES leurs travaux issus de la réutilisation de ce code, ainsi que les éventuels problèmes ou anomalies qu'ils y rencontreraient, en écrivant à DREES-CODE@sante.gouv.fr


# Ce logiciel est régi par la licence "GNU General Public License" GPL-3.0. # https://spdx.org/licenses/GPL-3.0.html#licenseText
# À cet égard l'attention de l'utilisateur est attirée sur les risques associés au chargement, à l'utilisation, à la modification et/ou au développement et à la reproduction du logiciel par l'utilisateur étant donné sa spécificité de logiciel libre, qui peut le rendre complexe à manipuler et qui le réserve donc à des développeurs et des professionnels avertis possédant des connaissances informatiques approfondies. Les utilisateurs sont donc invités à charger et tester l'adéquation du logiciel à leurs besoins dans des conditions permettant d'assurer la sécurité de leurs systèmes et ou de leurs données et, plus généralement, à l'utiliser et l'exploiter dans les mêmes conditions de sécurité.
# Le fait que vous puissiez accéder à cet en-tête signifie que vous avez pris connaissance de la licence GPL-3.0, et que vous en avez accepté les termes.

# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.If not, see <https://www.gnu.org/licenses/>.

# =================================================================

# FONCTION EXPORTER_TABLEAU_PUNAISE SCRIPT

#' Exporte un tableau
#'
#' Exporte un tableau au format simple representant les donnees fourni par l'utilisateur
#'
#' @param donnees donnees sous forme de dataframe ou matrice permettant de faire le tableau
#' @param onglet nom de l'onglet sur lequel creer le tableau
#' @param format_donnees format general des donnees, s'applique a toutes les colonnes. On peut ensuite modifier
#' le format de certaines a l'aide de colonnes_format_specifique et format_specifique_voulu. \cr
#' Formats disponibles:\cr
#'  GENERAL,\cr NUMBER,\cr CURRENCY,\cr ACCOUNTING,\cr DATE,\cr LONGDATE,\cr TIME,\cr PERCENTAGE,\cr
#'  FRACTION,\cr SCIENTIFIC,\cr TEXT,\cr COMMA pour les milliers separes par virgules\cr
#'  Pour les dates des formats peuvent etre fait avec n'importe quelle combinaison de : d, dd, ddd, dddd,
#'  m, mm, mmm, mmmm, mmmmm,  yy, yyyy \cr
#'  Pour les arrondis numeriques, preciser le nombre de chiffres apres la virgule dans la variable
#'  nombres_decimales_voulues.
#'
#' @param noms_lignes = TRUE si les noms de lignes ont aussi un titre
#' @param fichier_source fichier excel que l'on veut editer (si null, la fonction creer un nouveau fichier
#' a l'aide de fichier_destination, ou ecrase le fichier fichier_destination et le remplace par le tableau
#' nouvellement cree)
#' @param fichier_destination fichier excel d'enregistrement du tableau
#' @param colonnes_donnees indices dans excel des colonnes de notre tableau
#' @param lignes_donnees indices dans excel des lignes de notre tableau
#' @param titre titre du tableau
#' @param aides_lecture permet d ajouter des precisions sous forme de notes de bas de tableaux avec asterisque.
#' S'il y en a plusieurs, aides_lecture doit etre sous forme de vecteur ayant pour elements les differentes
#' aides.
#' @param note permet d'editer des notes sur les donnees
#' @param lecture permet de preciser la lecture du tableau
#' @param champ permet de preciser le champ des donnees
#' @param sources permet de preciser la source des donnees
#' @param lignes_style_specifique vecteur des lignes auxquelles on veut appliquer un style specifique (1 etant
#' la premiere ligne du tableau apres le titre) (styles possibles : bold, strikeout, italic, underline,
#' underline2)
#' @param style_specifique_voulu vecteur des styles voulus pour chaque ligne de lignes_style_specifique, si
#' null, ils seront tous en gras
#' @param colonnes_format_specifique vecteur des colonnes auxquelles on veut appliquer un format particulier
#' Les numeros de colonnes correspondent a la place de la colonne dans le dataframe.
#' @param format_specifique_voulu vecteur de meme taille que colonnes_format_specifique des formats voulus pour
#' chaque colonne de format_specifique_colonne\cr
#' Formats disponibles:\cr
#' GENERAL,\cr NUMBER,\cr CURRENCY,\cr ACCOUNTING,\cr DATE,\cr LONGDATE,\cr TIME,\cr PERCENTAGE,\cr
#' FRACTION,\cr SCIENTIFIC,\cr TEXT,\cr COMMA pour les milliers separes par virgules\cr
#' Pour les dates des formats peuvent etre fait avec n'importe quelle combinaison de : d, dd, ddd, dddd,
#' m, mm, mmm, mmmm, mmmmm,  yy, yyyy \cr
#' Pour les arrondissements numeriques, donner le nombre de chiffres apres la virgule.
#' @param lignes_a_ajuster vecteur des lignes e ajuster (indices selon les lignes excell)
#' @param ajustements_hauteurs_lignes vecteur des hauteurs de lignes (en unites excell)
#' @param visualiser booleens permettant de visualiser directement le tableau en sortie si egal a TRUE.
#' @param nombre_decimales_voulues nombre de decimales voulues pour chaque element specifie dans format
#' specifique voulu, doit donc etre de meme longueur que le vecteur de format_specifique_voulu.
#' @param garder_premiere_case TRUE par defaut. Dans le cas ou il y a des noms de lignes, est-ce que le premier
#' element du tableau doit etre garde (le titre de la colonne correspondant aux noms de lignes)
#' @param sauvegarde Booleen, s'il est TRUE alors le fichier .xlsx s'enregistre a l'endroit specifie,
#' sinon, alors on n'enregistre pas le fichier.
#' @return Objet workbook de openxlsx, le tableau representant les donnees dans un fichier excel
#' @export
#' @import openxlsx
#' @import textshape
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' exporter_tableau_punaise(donnees = accueil_enfant_2014_punaise, 
#'                          noms_lignes = TRUE,
#'                          fichier_destination = 
#'                          "exemple_exporter_tableau_punaise.xlsx", 
#'                          onglet = "tab_acc_enf",
#'                          titre = legende_accueil_enfant_2014_punaise$titre,
#'                          aides_lecture = 
#'                          legende_accueil_enfant_2014_punaise$aides_lecture,
#'                          lecture = 
#'                          legende_accueil_enfant_2014_punaise$lecture,
#'                          champ = legende_accueil_enfant_2014_punaise$champ,
#'                          sources = 
#'                          legende_accueil_enfant_2014_punaise$sources, 
#'                          colonnes_format_specifique = c(2,3),
#'                          format_specifique_voulu = 
#'                          c("PERCENTAGE", "PERCENTAGE"),
#'                          lignes_a_ajuster = c("header", "noms_colonnes", 
#'                          "footer"),
#'                          ajustements_hauteurs_lignes = c(25, 30, 90), 
#'                          visualiser = T, garder_premiere_case = FALSE)
#'
#' exporter_tableau_punaise(donnees = activite_parents_enfants, 
#'                          noms_lignes = TRUE,
#'                          fichier_source = 
#'                          "exemple_exporter_tableau_punaise.xlsx", 
#'                          onglet = "act_par_enf",
#'                          titre = legende_activite_parents_enfants$titre,
#'                          lecture = legende_activite_parents_enfants$lecture,
#'                          champ = legende_activite_parents_enfants$champ,
#'                          sources = legende_activite_parents_enfants$source, 
#'                          visualiser = T,
#'                          garder_premiere_case = FALSE)
#'
#' exporter_tableau_punaise(donnees = evolution_mode_garde, noms_lignes = TRUE,
#'                          fichier_source = 
#'                          "exemple_exporter_tableau_punaise.xlsx", 
#'                          onglet = "evo_mod_gard",
#'                          titre = legende_evolution_mode_garde$titre,
#'                          aides_lecture = 
#'                          legende_evolution_mode_garde$aides_lecture,
#'                          champ = legende_evolution_mode_garde$champ,
#'                          sources = legende_evolution_mode_garde$sources, 
#'                          lignes_a_ajuster = c("header"),
#'                          ajustements_hauteurs_lignes = c(40), 
#'                          visualiser = TRUE, garder_premiere_case = TRUE)

#' }


exporter_tableau_punaise <- function(donnees, onglet = "tableau1", format_donnees = "NUMBER",
                                     noms_lignes = FALSE, fichier_source = NULL, fichier_destination = NULL,
                                     colonnes_donnees = 2:(ncol(donnees)+1),
                                     lignes_donnees = 4:(nrow(donnees)+3), titre = NULL, aides_lecture = NULL,
                                     note = NULL, lecture = NULL, champ = NULL, sources = NULL,
                                     lignes_style_specifique = NULL, style_specifique_voulu = NULL,
                                     colonnes_format_specifique = NULL, format_specifique_voulu = NULL,
                                     nombre_decimales_voulues = NULL, visualiser = TRUE,
                                     lignes_a_ajuster = c("header", "noms_colonnes", "footer"),
                                     ajustements_hauteurs_lignes = c(25, 35, 150),
                                     garder_premiere_case = TRUE,
                                     sauvegarde = T){

  options("openxlsx.borderColour" = "black")
  options("openxlsx.borderStyle" = "hair")
  options("openxlsx.numFmt" = format_donnees)

  if (length(lignes_donnees)>nrow(donnees)){
    print("Erreur, le nombre de lignes des donnees depasse le nombre de lignes du tableau en entree")
  }
  if (length(colonnes_donnees)> ncol(donnees)){
    print("Erreur, le nombre de colonnes des donnees depasse le nombre de colonnes du tableau en entree")
  }

  # formatage des donnees,
  if (class(donnees)[1] == "tbl_df"){
    donnees <- as.data.frame(donnees)
    if (noms_lignes == TRUE){
      colonnes_donnees <- colonnes_donnees[1:length(colonnes_donnees)-1]
    }
  }
  else{
    donnees <- as.data.frame(donnees)
  }

  c_titres_lignes <- 1
  if (noms_lignes == TRUE){
    c_titres_lignes <- 2
    colonnes_donnees <- colonnes_donnees + 1
    style_titres_lignes <- openxlsx::createStyle(halign = "LEFT", valign = "CENTER",
                                                 border = "TopBottomLeftRight", wrapText = T)
    if (garder_premiere_case == FALSE){
      style_premiere_case <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER",
                                                   border = "BottomRight", wrapText = T)
    }
    else{
      style_premiere_case <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER",
                                                   textDecoration = "bold", border = "TopBottomLeftRight",
                                                   wrapText = T)
    }
  }


  # creation du workbook
  if (is.null(fichier_source)){
    wb <- openxlsx::createWorkbook()
    if (is.null(fichier_destination)){
      print("Veuillez indiquer un fichier sur lequelle ecrire les donnees")
      return(wb)
    }
  }
  else{
    wb <- openxlsx::loadWorkbook(file = fichier_source)
    if (onglet %in% openxlsx::getSheetNames(fichier_source)){
      print("Erreur : ce nom d'onglet existe deja")
      return(wb)
    }
  }

  # premier onglet (on peut en creer plusieurs)
  sheet <- openxlsx::addWorksheet(wb, sheetName = onglet)

  # mise en forme des titres de colonnes
  hs1 <- openxlsx::createStyle(halign = "CENTER", valign = "CENTER", textDecoration = "bold",
                               border = "TopBottomLeftRight", wrapText = T)
  openxlsx::writeData(wb,sheet = onglet, donnees, startCol = 2, startRow = 3,
                      borders = "all", headerStyle = hs1)


  body_style <- openxlsx::createStyle(numFmt = format_donnees,border = "TopBottomLeftRight",
                                      halign = "CENTER", valign = "top")
  openxlsx::addStyle(wb, sheet = onglet, body_style,rows = lignes_donnees,cols = c(2,colonnes_donnees),
                     gridExpand = T)

  if (noms_lignes == TRUE){
    if (garder_premiere_case == FALSE){
      openxlsx::writeData(wb, onglet, x = " ", startCol = 2, startRow = 3)
    }
    openxlsx::addStyle(wb, sheet = onglet, style_premiere_case, rows = 3, cols = 2)
    openxlsx::addStyle(wb, sheet = onglet, style_titres_lignes, rows = lignes_donnees, cols = 2)
  }
  # openxlsx::addStyle(wb, sheet = onglet, body_style,rows = 5,cols = colonnes_donnees)
  # openxlsx::addStyle(wb, sheet = onglet, body_style,rows = 6,cols = colonnes_donnees)
  # openxlsx::addStyle(wb, sheet = onglet, body_style,rows = 7,cols = colonnes_donnees)

  openxlsx::setColWidths(wb, sheet = onglet, cols = c(1), widths = 3)
  # openxlsx::setColWidths(wb, sheet = onglet, cols = 3, widths = 15)
  # openxlsx::setColWidths(wb, sheet = onglet, cols = 4, widths = 15)
  # openxlsx::setColWidths(wb, sheet = onglet, cols = 1, widths = 2)
  openxlsx::showGridLines(wb, onglet,showGridLines = F)
  openxlsx::modifyBaseFont(wb, fontSize = 8, fontColour = "black",fontName = "Marianne") # anciennement : "Arial"

  # ajustement des colonnes
  largeur_ajuster <- 1.5
  largeur_vec_header <- NULL
  for (i in 1 : length(colnames(donnees))){
    largeur <- 0
    largeur2 <- 0
    j <- 1
    while (j <= nchar(colnames(donnees)[i])){
      if ((substr(colnames(donnees)[i], j,j) == "\r") | (substr(colnames(donnees)[i], j,j) == "\n")){
        if (largeur > largeur2){
          largeur2 <- largeur
        }
        largeur <- 0
        j <- j + 1
      }
      else{
        largeur <- largeur + 1
        j <- j + 1
      }
    }
    largeur_vec_header <- c(largeur_vec_header, (max(largeur, largeur2) + largeur_ajuster))
  }
  largeur_vec <- apply(donnees, 2, function(x) max(nchar(as.character(x)) + largeur_ajuster, na.rm = TRUE))
  largeur_vec_max <- pmax(largeur_vec, largeur_vec_header)
  openxlsx::setColWidths(wb, sheet = onglet,
                         cols = 2 : (length(colonnes_donnees) + 2), widths = largeur_vec_max)

  # styles a appliquer a des lignes specifiques :
  if (!is.null(lignes_style_specifique)){
    if (is.null(style_specifique_voulu)){
      style_specifique_voulu <- rep(("bold"), length(lignes_style_specifique))
    }
    if (length(lignes_style_specifique) != length(style_specifique_voulu)){
      print("Erreur : le nombre de lignes sur lesquelles appliquer un style est different du nombre de styles
            correspondant")
      if (length(lignes_style_specifique) > length(style_specifique_voulu)){
        bold_ajoute <- rep("bold", length(lignes_style_specifique) - length(style_specifique_voulu))
        style_specifique_voulu <- c(style_specifique_voulu, bold_ajoute)
      }
      else{
        style_specifique_voulu <- style_specifique_voulu[1 : length(lignes_style_specifique)]
      }
    }
    for (i in 1 : length(lignes_style_specifique)){
      style_specifique <- openxlsx::createStyle(textDecoration = style_specifique_voulu[i],
                                                numFmt = format_donnees, halign = "CENTER", valign = "CENTER")
      openxlsx::addStyle(wb, sheet = onglet, style_specifique, rows = 3 + lignes_style_specifique[i],
                         cols = c(2,colonnes_donnees), gridExpand = T, stack = TRUE)
    }
}

  # formats e appliquer e des colonnes specifiques :
  if (!is.null(colonnes_format_specifique)){
    if (is.null(nombre_decimales_voulues)){
      nombre_decimales_voulues <- rep(2, length(colonnes_format_specifique))
    }
    for (i in 1 : length(colonnes_format_specifique)){
      if (format_specifique_voulu[i] %in% c("PERCENTAGE", "CURRENCY", "NUMBER", "SCIENTIFIC")){
        nb_dec = "0."
      }
      if (nombre_decimales_voulues[i] > 0){
        for (j in 1 : nombre_decimales_voulues[i]){
          nb_dec = paste0(nb_dec, "0")
        }
      }
      if (nombre_decimales_voulues[i] == 0){
        nb_dec <- "0."
      }
      if(format_specifique_voulu[i] == "PERCENTAGE"){
        bon_format <- paste0(nb_dec, " %")
      }
      else if(format_specifique_voulu[i] == "CURRENCY"){
        bon_format <- paste0(nb_dec, " \u20AC")
      }
      else if(format_specifique_voulu[i] %in% c("NUMBER", "SCIENTIFIC")){
        bon_format <- nb_dec
      }
      else{
        bon_format <- format_specifique_voulu[i]
      }
      style_format_specifique <- openxlsx::createStyle(numFmt = bon_format)
      openxlsx::addStyle(wb, sheet = onglet, style_format_specifique, rows = 3 : (length(lignes_donnees) + 3),
                         cols = 1 + colonnes_format_specifique[i], gridExpand = T, stack = TRUE)
    }
  }

  # titre
  titre_style <- openxlsx::createStyle(halign = "LEFT", textDecoration = "bold", valign = "top",
                                       wrapText = TRUE)

  openxlsx::writeData(wb, onglet, titre, startCol = 2, startRow = 2)
  openxlsx::setRowHeights(wb, onglet, 1, 10)
  openxlsx::setRowHeights(wb, onglet, 2, 15)

  openxlsx::addStyle(wb, onglet, titre_style, rows = 2, cols = 2)
  openxlsx::mergeCells(wb, onglet, cols = c(2,colonnes_donnees), rows = 2)


  # footer
  # openxlsx::removeColWidths(wb, onglet, cols =  c(2,3,4,5))
  footer_style <- openxlsx::createStyle(halign = "LEFT", valign = "TOP", wrapText = T)

  if (is.null(aides_lecture)){
    aides_lecture = ""
  }
  else{
    if (length(aides_lecture)>1){
      new_aides_lecture <- paste0("* ", aides_lecture[1])
      for (i in 2:(length(aides_lecture))){
        new_aides_lecture <- paste(new_aides_lecture, paste0("* ", aides_lecture[i]), sep = "\n")
      }
      aides_lecture <- new_aides_lecture
    }
    aides_lecture = paste(aides_lecture, "", sep = "\n")
  }
  if (is.null(note)){
    note <- ""
  }
  else{
    note <- paste0("Note > ", paste(note, "", sep = "\n"))
  }
  if (is.null(lecture)){
    lecture <- ""
  }
  else{
    lecture <- paste0("Lecture > ", paste(lecture, "", sep = "\n"))
  }
  if (is.null(champ)){
    champ <- ""
  }
  else{
    champ <- paste0("Champ > ", paste(champ, "", sep = "\n"))
  }
  if (is.null(sources)){
    sources <- ""
  }
  else{
    sources <- paste0("Sources > ", sources)
  }

  legende <- paste0(aides_lecture, note, lecture, champ, sources)
  openxlsx::writeData(wb, onglet, legende, startCol = 2, startRow = lignes_donnees[length(lignes_donnees)] + 2)

  openxlsx::mergeCells(wb, onglet, cols = c(2,colonnes_donnees),
                       rows = lignes_donnees[length(lignes_donnees)] + 2)
  openxlsx::addStyle(wb, onglet, footer_style, rows = lignes_donnees[length(lignes_donnees)] + 2, cols = 2)
  openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 1, 10)

  # ajustements lignes :
  if (!is.null(lignes_a_ajuster)){
    for (i in 1 : length(ajustements_hauteurs_lignes)){
      if (lignes_a_ajuster[i] == "header"){
        openxlsx::setRowHeights(wb, onglet, 2, heights = ajustements_hauteurs_lignes[i])
      }
      else if (lignes_a_ajuster[i] == "noms_colonnes"){
        openxlsx::setRowHeights(wb, onglet, 3, heights = ajustements_hauteurs_lignes[i])
      }
      else if (lignes_a_ajuster[i] == "footer"){
        openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 2,
                                heights = ajustements_hauteurs_lignes[i])
      }
    }
    if (!("header" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, 2, heights = 25)
    }
    if (!("noms_colonnes" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, 3, heights = 35)
    }
    if (!("footer" %in% lignes_a_ajuster)){
      openxlsx::setRowHeights(wb, onglet, lignes_donnees[length(lignes_donnees)] + 2,
                              heights = 150)
    }
  }
  # visualisation et sauvegarde
  if (visualiser == TRUE){
    openxlsx::openXL(wb)
  }
  if(sauvegarde == T){
    if (is.null(fichier_destination)){
      openxlsx::saveWorkbook(wb, fichier_source, overwrite = TRUE)
    }
    else{
      openxlsx::saveWorkbook(wb, fichier_destination, overwrite = TRUE)
    }
  }
  return(wb)
}
