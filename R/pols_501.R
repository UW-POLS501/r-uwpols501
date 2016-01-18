#' @import assertthat
#' @importFrom devtools add_rstudio_project
NULL

.create_project_dirs <- function(d) {
  main_dir <- dir
  dir.create(d)
  dir_names <- c("data", "data-raw", "figures", "output")
  add_rstudio_project(d)
  for (dn in dir_names) {
    dir.create(file.path(d, dn))
  }
}

#' Create Directory Structure for Projects
#'
#' @param assignment Assignment number
#' @param net_id Student netID. Example: jarnold
#' @param dir Directory in which the assignment will be created
#' @export
create_assignment_skeleton <- function(assignment, net_id, dir = ".") {
  assert_that(is.numeric(assignment))
  main_dir <- file.path(dir, paste0("assignment", assignment, "-", net_id))
  .create_project_dirs(main_dir)
  cat(paste0("Code and data for POLS-501 assignment ", assignment, "\n"),
             file = file.path(main_dir, "README.md"))
  cat(paste("Createted empty project for Assignment",
            assignment, "in", main_dir))
}

#' @rdname create_assignment_skeleton
#' @export
create_project_skeleton <- function(dir = ".") {
  .create_project_dirs(dir)
  cat(paste("Created empty project in ", dir))
}

#' Zip project or assignment for submission
#'
#' Will create a zipfile of the current directory that you can submit as an assignment.
#'
#' @param use_tar Create a tar.gz file instead of a zip file.
#'
#' @export
zip_project <- function(use_tar = FALSE) {
  wd <- getwd()
  dir_to_zip <- basename(wd)
  on.exit(setwd(wd))
  setwd("..")
  if (use_tar) {
    zipfile <- paste0(dir_to_zip, ".tar.gz")
    tar(zipfile, files = dir_to_zip, compression = "gzip")
  } else {
    zipfile <- paste0(dir_to_zip, ".zip")
    zip(zipfile, dir_to_zip)
  }
  message("Created file ", zipfile)
}
