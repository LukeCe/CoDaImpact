library("data.table")

load("data-raw/VOTE.rda")

rename <- c(
  "others"     = "extreme_right",
  "Age_65"     = "Age_65plus",
  "No_CAPBEP"  = "Educ_BeforeHighschool",
  "Bac"        = "Educ_Highschool",
  "Diplom_sup" = "Educ_Higher",
  "owner_rate" = "asset_owner_rate",
  "income_rate"= "income_taxpayer_rate",
  "foreign"    = "forgeigner_rate")


election <- VOTE
setnames(election,old = names(rename), new = rename)
usethis::use_data(election)



#' Results of french departmental elections in 2015
#'
#' The data is used by Nguyen et. al (2020) and originally disseminated by the
#' French ministry (Ministère de l'Intérieur et des Outre-Mer).
#' Information about the population characteristics comes from the french
#' national statistics institute (INSEE).
#'
#' @details
#'
#'   + `left`, `right`, `extreme_right`: Vote shares during the election grouped into three blocks
#'   + `Age_1839`, `Age_4064`, `Age_65plus`: Share of the population falling into one of three age categories
#'   + `Educ_BeforeHighschool`, `Educ_Highschool`, `Educ_Higher`: Share of the population having completed a certain level of education.
#'     + Educ_BeforeHighschool
#'
#' @source
#'   - https://www.data.gouv.fr/fr/datasets/elections-departementales-2015-resultats-par-bureaux-de-vote/
#'   - https://www.insee.fr/fr/statistiques
#' @references
#'   Nguyen THA, Laurent T, Thomas-Agnan C, Ruiz-Gazen A. Analyzing the impacts of socio-economic factors on French departmental elections with CoDa methods. J Appl Stat. 2020 Dec 9;49(5):1235-1251. doi: 10.1080/02664763.2020.1858274. PMID: 35707505; PMCID: PMC9041641.
"elections"
