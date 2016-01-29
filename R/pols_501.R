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
  dir_names <- c("data", "data-raw", "figures", "output")
  #add_rstudio_project(d)
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
  assert_that(is.numeric(assignment) | is.character(assignment))
  main_dir <- file.path(dir, paste0("assignment-", assignment, "-", net_id))
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




#' Return NA if x == y, and x otherwise
#'
#' @param x vector
#' @param y vector
#' @return vector, with \code{NA} where \code{x == y}, and \code{x} otherwise.
#'
#' @details
#'
#' This is similar to the SQL function \code{NULLIF}. It can be used
#' to peform the inverse operation of \code{\link{coalesce}}.
#' @export
naif <- function(x, y) ifelse(x == y, NA, x)

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
