#' Binary weight and other functions for SNI and J
#'
#' \code{binWeight} - Calculates the binary weight, i.e. the number of 1's in
#' the binary expansion, of an integer.
#'
#' @author Sophie Kersting
#' @param number integer >=0
#' @return the number of 1's in the binary expansion (numeric)
#' @export
#' @rdname binWeight
#' @examples
#' binWeight(55)
binWeight <- function(number) {
  NOBITS <- ceiling(log2(number))
  #maximal index of a "1" in the binary expansion
  return (sum(Number2Binary(number,NOBITS+1)))
}
#' Binary weight and other functions for SNI and J
#'
#' \code{MinSNIorJOfn} - Calculates the minimal value of the symmetry nodes
#' index and the Rogers J index for trees with n leaves.
#'
#' @param n integer >=0
#' @return the minimal SNI and J value for trees with n leaves (numeric)
#' @rdname binWeight
#' @export
#' @examples
#' MinSNIorJOfn(55)
MinSNIorJOfn <- function(n) {
  return (binWeight(n)-1)
}
#' Binary weight and other functions for SNI and J
#'
#' \code{Number2Binary} - Determines the binary expansion of an integer.
#'
#' @param noBits desired length of binary expansion (default: 32 bits)
#' @return numeric vector containing the binary expansion
#' @rdname binWeight
#' @export
#' @examples
#' Number2Binary(55,ceiling(log2(55)))
Number2Binary <- function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}
