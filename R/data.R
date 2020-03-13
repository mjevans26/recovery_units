#' Characteristics of listed species with recovery plans
#'
#' A dataset containing characteristics of ESA listed species with recovery plans
#'
#' @format A data frame with 1363 rows and 18 variabels:
#' \describe{
#'   \item{Scientific}{latin species name used by FWS}
#'   \item{Total}{Total federal expenditures}
#'   \item{Office}{FWS office responsible for species}
#'   \item{Group}{taxonomic group}
#'   \item{Region}{FWS region responsible for species}
#'   \item{Status}{ESA listing stats}
#'   \item{Priority}{recovery priority number RPN}
#'   \item{ymd}{date of recovery unit designation}
#'   \item{Area}{size of species range (ac)}
#'   \item{group}{recovery unit (R) or other species with recovery plan (A)}
#'   \item{scholar}{number of google scholar citations}
#'   \item{Area_z}{range size standardized by taxonomic group}
#'   \item{Priority_z}
#'   \item{Scholar_z}{number of google scholar citations standardized by year}
#'   \item{Year}{five-year strata}
#'   \item{Total_z}{}
#'   \item{fcon}{proportion of consultations that were formal}
#'   \item{consults}{number of consultations as recorded in TAILS}
#' }
"clean_data"

#' Taxonomic characteristics
#' 
#' A dataset containing summary statistics related to listed species under the ESA
#' summarized per taxonomic group used by FWS
#' 
#' @format A data frme with 10 observation of 9 variables:
#' \describe{
#'   \item{Group}{Taxonomic group}
#'   \item{All}{number of listed species in taxonomic group with recovery plans}
#'   \item{RUs}{number of listed species in taxonomic group with recovery units}
#'   \item{All.d}
#'   \item{RUs.d}
#'   \item{Biops}{number of biological opinions}
#'   \item{p}{proportion of listed species in the taxonomic group}
#'   \item{pM}{proportion of biological opinions in which recovery units are mentioned}
#'   \item{pA}{proportion of biological opinions in which recovery units are used}
#' }
"group_cont"

#' Comparison species
#' 
#' A dataset containing summary statistics related to listed species under the ESA
#' summarized per taxonomic group used by FWS
#' 
#' @format A data frme with 63 observations of 23 variables:
#' \describe{
#'   \item{Scientific}{latin species name used by FWS}
#'   \item{Common}{common species name}
#'   \item{Group}{taxonomic group}
#'   \item{Region}{FWS region responsible for species}
#'   \item{Range}{}
#'   \item{Units}{Number of recovery units}
#'   \item{Status}{ESA listing stats}
#'   \item{Priority}{recovery priority number RPN}
#'   \item{Date}{character string of recovery plan publication date}
#'   \item{ymd}{date format of recovery plan publication date}
#'   \item{Pair}{int variable designating groups of similar species for comparison}
#'   \item{Area}{size of species range (ac)}
#'   \item{Office}{FWS office responsible for species}
#'   \item{cons}{number of consultations as recorded in TAILS}
#'   \item{fcons}{proportion of consultations that were formal}
#'   \item{group}{recovery unit (R) or other species with recovery plan (A)}
#'   \item{frate}{proportion of consultations that were formal}
#'   \item{recov}
#'   \item{scholar}{number of google scholar citations}
#'   \item{Area_z}{range size standardized by taxonomic group}
#'   \item{Scholar_z}{number of google scholar citations standardized by year}
#'   \item{Conflict}{Conflict designation?}
#'   \item{Total}{Total federal expenditures}
#'   
#'   
#' }
"compare"

#' Biological opinions
#' 
#' A dataset containing tabulating the use of recovery units in biological opinions
#' 
#' @format A data frme with 63 observations of 23 variables:
#' \describe{
#'   \item{SCIENTIFIC}{latin species name used by FWS}
#'   \item{COMMON}{common species name}
#'   \item{ACTIVITY_CODE}{unique identifier for biological opinion}
#'   \item{FWS_OFFICE}{FWS office conducting biological opinion}
#'   \item{DATE}{date of completion}
#'   \item{Mention}{were recovery units mentioned}
#'   \item{Analysis}{were recovery units considered in the jeopardy analysis}
#'   \item{Programmatic}{was the consultation programmatic}
#'   \item{Taxon}{taxonomic group of species}
#' }
"biops"