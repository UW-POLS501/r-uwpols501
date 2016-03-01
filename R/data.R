#'

#' Turnout Data Set from the 1992 American National Election Survey
#'
#' This data set contains individual-level turnout data. It pools several
#' American National Election Surveys conducted during the 1992 presidential
#' election year.  Only the first 2,000 observations (from a total of 15,837
#'                                                     observations) are included in the sample data.
#'
#' @format
#' A table containing 5 variables ("race", "age", "educate",  "income", and "vote") and 2,000 observations.
#'
#' @source Zelig package
#' @references
#'  King, Gary, Michael Tomz, Jason Wittenberg (2000).
#'   ``Making the Most of Statistical Analyses: Improving Interpretation and
#'   Presentation,'' \emph{American Journal of Political Science}, vol. 44,
#'   pp.341--355.
"turnout"

#' Party Systems and Redistributive Government
#'
#' @format A data frame containng 4 variables and 14 observations.
#'
#' \describe{
#'  \item{cty}{Country}
#'  \item{elec_sys}{Electoral system: Majoritarian "maj", Proportional representation "pr", unamimity government "unam"}
#'  \item{povred}{Percent of citizens liftedn out of poverty by taxes and transfers.}
#'  \item{enp}{Effective number of parties.}
#' }
#'
#' @references
#'  Torben Iversen and David Soskice. 2006. ``Electoral Institutions and the Politics of Coalitions: Why Some
#'  Democracies Redistribute More Than Others.`` \emph{American Political Science Review}.
#'
"iver"

#' Prestige of Canadian Occupations
#'
#' @format
#' The Prestige data frame has 102 rows and 6 columns. The observations are occupations.
#' 
#' \describe{
#'  \item{education}{Average education of occupational incumbents, years, in 1971.}
#'  \item{income}{Average income of incumbents, dollars, in 1971.}
#'  \item{women}{Percentage of incumbents who are women.}
#'  \item{prestige}{Pineo-Porter prestige score for occupation, from a social survey conducted in the mid-1960s.}
#'  \item{census}{Canadian Census occupational code}
#'  \item{type}{Type of occupation. A factor with levels (note: out of order): bc, Blue Collar; prof, Professional, 
#'  Managerial, and Technical; wc, White Collar.}
#' }
#'
#' @source car package.
"Prestige"
