
##-Simple functions--------------------------------------


#' Min Zero Function
#'
#' This function just replaces negative values with zero.
#' @param arg A number
#' @return \code{arg} if positive, otherwise 0.
#' @examples
#' min_zero(27)
#' min_zero(-27)

min_zero <- function(arg) {
  max(0,arg)
}


#' Test Fit Function
#'
#' Test to see if an item fits in a bin, given the dimensions of each.
#' @param it_vec Vector containing item dimensions
#' @param to_vec Vector containing bin dimensions
#' @examples
#' test_fit(c(7.5,5,.25),c(24,12,10))
#' test_fit(c(14,12.5,3),c(24,12,10))

test_fit <- function(it_vec, to_vec) {
  if (!is.na(sum(to_vec)) & !is.na(sum(it_vec))) {
    toMa <- c(max(to_vec),sort(to_vec,decreasing = TRUE)[2]
              ,sort(to_vec,decreasing = TRUE)[3])
    itMi <- c(max(it_vec),sort(it_vec,decreasing = TRUE)[2]
              ,sort(it_vec,decreasing = TRUE)[3])
    if (prod(sapply(toMa-itMi,function(x) min_zero(x))) > 0) TRUE else FALSE
  } else FALSE
}


