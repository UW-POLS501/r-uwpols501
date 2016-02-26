#' @import assertthat
#' @importFrom plyr revalue mapvalues
#' @import lazyeval
#' @import dplyr
#' @importFrom devtools add_rstudio_project
NULL

# avoid R CMD check warnings
globalVariables(c("everything"))

.create_project_dirs <- function(d) {
  dir.create(d)
  dir_names <- c("data", "figure", "output")
  add_rstudio_project(d)
  for (dn in dir_names) {
    dir.create(file.path(d, dn))
  }
}

#' Create Directory Structure for Projects
#'
#' Creates a directory with the correct directory structure.
#' Use `create_assignment_skeleton` for computation assignments,
#' and `create_research_project_skeleton` for research projects.
#'
#' @param assignment Assignment number
#' @param net_id Student netID. Example:
#' @param dst Directory in which the project will be created
#' @param dirname Directory name of the project
#' @export
create_assignment_skeleton <- function(assignment, net_id, dst = ".") {
  assert_that(is.numeric(assignment))
  main_dir <- file.path(dir, paste0("assignment-", assignment, "-", net_id))
  create_rproject_skeleton(main_dir)
}

#' @rdname create_assignment_skeleton
#' @export
create_research_project_skeleton <- function(assignment, net_id, dst = ".") {
  assert_that(is.numeric(assignment))
  main_dir <- file.path(dir, paste0("project-", assignment, "-", net_id))
  create_rproject_skeleton(main_dir)
}


#' @rdname create_assignment_skeleton
#' @export
create_rproject_skeleton <- function(dirname, dst = ".") {
  .create_project_dirs(dirname)
  cat(paste("Created empty R project for data analysis in ", dir))
  print(dir(dirname, recursive = TRUE))
  invisible(dirname)
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


#' Compare vectors with missing values
#'
#' Compare two vectors treating \code{NA} like any other value.
#' If both vectors have an \code{NA} for that element, the function returns \code{TRUE},
#' if one has an \code{NA} and the other does not, it returns \code{FALSE}.
#'
#' @param x,y vectors to compare
#' @return A logical vector: \code{TRUE} if both vectors have the same value, including
#' \code{NA}, and \code{FALSE}, if they differ.
#' @author Winston Chang
#' @references \url{http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/}
#' @export
is_same <- function(x, y) {
  same <- (x == y) | (is.na(x) & is.na(y))
  same[is.na(same)] <- FALSE
  same
}


#' First non-missing element for vectors
#'
#' @param ... vectors
#' @return For each element, the value of the first non \code{NA} element in the
#' vectors of \code{...}
#' @author \href{http://stackoverflow.com/users/903061/gregor}{Gregor}, \href{http://stackoverflow.com/users/2588184/mrip}{mrip}
#' @references \url{http://stackoverflow.com/questions/19253820/}
#' @export
coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x
  },
  list(...))
}


#' Filter rows with missing values
#'
#' \code{filter_na()} filters rows with missing (\code{NA}) values.
#'
#' @param .data A tbl. Currently only methods for \code{\link{data.frame}} and
#'   \code{\link{tbl_df}} are provided.
#' @param ... Comma separated list of unquoted expressions to select variables.
#'    See \code{\link[dplyr]{select}}.
#' @param .dots Use \code{filter_na_()} to do standard evaluation. See
#'    \code{vignette("nse", package = "dplyr")} for details.
#' @return An object of the same class as tbl.
#'
#' @export
filter_na <- function(.data, ...) {
  filter_na_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @rdname filter_na
#' @export
filter_na_ <- function(.data, ..., .dots) {
  UseMethod("filter_na_")
}

filter_na_.tbl_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  if (length(dots) == 0) {
    dots <- lazyeval::lazy_dots(everything())
  }
  vars <- select_vars_(names(.data), dots,
                       exclude = as.character(groups(.data)))

  # replace with Rcpp code
  keep <- if (length(vars) == 1) {
    !is.na(.data[[vars]])
  } else {
    Reduce(function(x, y) {
      !is.na(x) & !is.na(y)
    },
    .data[, vars])
  }
  .data[keep, ]
}

filter_na_.data.frame <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  as.data.frame(filter_na_(tbl_df(.data), .dots = dots))
}


#' Pulls label names from STATA datasets imported to R
#'
#' @param dataset An dataframe containing a STATA dataset
#'    imported to R
#' @export
extract_var_info <- function(dataset){
  variables_info <- NULL
  for (i in 1:length(dataset)) {
    cl <- dataset[[i]]
    variables_info <- c(variables_info, attr(cl, which = "label"))
  }
  return(variables_info)
}

#' @export
plyr::revalue

#' @export
plyr::mapvalues

final_paper <- function(fig_width = 4,
                        fig_height = 2.5,
                        fig_crop = TRUE,
                        dev = "pdf",
                        highlight = "default",
                        keep_tex = FALSE,
                        includes = NULL,
                        md_extensions = NULL,
                        pandoc_args = NULL) {
  if (identical(highlight, "default"))
    highlight <- "pygments"
  template <- system.file(file.path("rmarkdown",
                                    "templates", "tufte_handout", "resources",
                                    "tufte-handout.tex"),
                          package = "rmarkdown")
  # format <- rmarkdown::pdf_document(fig_width = fig_width,
  #                                   fig_height = fig_height,
  #                                   fig_crop = fig_crop,
  #                                   dev = dev,
  #                                   highlight = highlight,
  #                                   template = template,
  #                                   keep_tex = keep_tex,
  #                                   citation_package = citation_package,
  #                                   latex_engine = "pdflatex",
  #                                   includes = includes,
  #                                   md_extensions = md_extensions,
  #                                   pandoc_args = pandoc_args)

}
