#' @import assertthat
NULL

#' Create Directory Structure for an Assignment
#'
#' @param assignment Assignment number
#' @param netId Student netID. Example: jarnold
#' @param dir Directory in which the assignment will be created
#' @export
create_assignment_skeleton <- function(assignment, netId, dir='.') {
  assert_that(is.numeric(assignment))
  mainDir <- file.path(dir, paste0('assignment', assignment, '-', netId))
  dir.create(mainDir)
  dirNames <- c('data','data-raw','figures','output')
  for (dir in dirNames){
    dir.create(file.path(mainDir,dir))
  }
  cat(paste0('Code and data for POLS-501 assignment ', assignment, '\n'),
             file= file.path(mainDir,'README.md'))
  cat(paste('Assignment', assignment, 'in', mainDir))
}

