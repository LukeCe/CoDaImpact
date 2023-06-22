#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL


#' Data on the rice yields in the Vietnamese provinces
#'
#' @description
#' The data is used in the article by (Huong et al. 2023), who study the impact
#' of climate change on the rice production in Vietnam. \cr
#' It contains the following information:
#'   +  `PROVINCE`: a factor for the 63 provinces of Vietnam
#'   +  `REGION`: a factor with the 6 main regions
#'   +  `YEAR`: a numeric corresponding to the year
#'   +  `YIELD`: a numeric for the rice production in tonnes per hectare
#'   +  `PRECIPITATION`: a numeric for the annual precipitation in liters
#'   +  `TEMPERATURES`: a compositional variable represented as a matrix \cr
#'       It corresponding to the fraction of days in a year where the maximal
#'       temperature falls into one of the three categories,
#'       "LOW", "MIDDLE" and "HIGH".
#'
#' @name rice_yields
#' @docType data
#' @author Lukas Dargel, Christine Thomas-Agnan
#' @references
#'   Thi-Huong Trinh, Michel Simioni, and Christine Thomas-Agnan,
#'   “Discrete and Smooth Scalar-on-Density Compositional Regression
#'   for Assessing the Impact of Climate Change on Rice Yield in Vietnam”,
#'   TSE Working Paper, n. 23-1410, February 2023.
#' @keywords data
"election"





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
#'   - https://www.data.gouv.fr/fr/datasets/elections-departementales-2015-resultats-par-bureaux-de-vote
#'   - https://www.insee.fr
#' @references
#'   Nguyen THA, Laurent T, Thomas-Agnan C, Ruiz-Gazen A. Analyzing the impacts of socio-economic factors on French departmental elections with CoDa methods. J Appl Stat. 2020 Dec 9;49(5):1235-1251. doi: 10.1080/02664763.2020.1858274. PMID: 35707505; PMCID: PMC9041641.
#' @docType data
#' @keywords data
#' @name election
"election"

