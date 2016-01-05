#" @import assertthat
#' @importFrom devtools add_rstudio_project
NULL

#' Create Directory Structure for an Assignment
#'
#' @param assignment Assignment number
#' @param net_id Student netID. Example: jarnold
#' @param dir Directory in which the assignment will be created
#' @export
create_assignment_skeleton <- function(assignment, net_id, dir = ".") {
  assert_that(is.numeric(assignment))
  main_dir <- file.path(dir, paste0("assignment", assignment, "-", net_id))
  dir.create(main_dir)
  dir_names <- c("data", "data-raw","figures","output")
  add_rstudio_project(main_dir)
  for (dir in dir_names) {
    dir.create(file.path(main_dir,dir))
  }
  cat(paste0("Code and data for POLS-501 assignment ", assignment, "\n"),
             file= file.path(main_dir,"README.md"))
  cat(paste("Assignment", assignment, "in", main_dir))
}
