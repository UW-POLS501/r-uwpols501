#' @import assertthat
#' @importFrom plyr revalue mapvalues
#' @import lazyeval
#' @import dplyr
NULL

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
  everything <- NULL
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
