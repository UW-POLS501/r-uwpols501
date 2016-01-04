#' @import assertthat
NULL 

#' Create Directory Structure for an Assignment
#'
#' @param assignment Assignment number
#' @param netId Student netID. Example: jarnold
#' @export  
create_assignment_skeleton <- function(assignment, netId) {
  assert_that(is.numeric(assignment))
  dir.create(paste0('assignment', assignment, '-', netId))
}

