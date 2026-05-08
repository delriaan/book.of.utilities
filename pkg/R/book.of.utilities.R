#' @title Book of Utilities: utilities for working with data
#
#' @description
#' `book.of.utilities` seeks to facilitate execution of those repetitive, ad-hoc tasks often encountered during data processing.
#'
#'
#' The following functional chapters are covered in \code{book.of.utilities}:\cr
#'
#' @section Chapter 1 - Calculators:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{calc.means}} \tab Calculates mean variants for vectors and tabular inputs.\cr
#' \code{\link{calc.zero_mean}} \tab Centers values at zero by subtracting the mean.\cr
#' \code{\link{calc.rms}} \tab Computes root-mean-square of numeric input.\cr
#' \code{\link{calc.harmonic_mean}} \tab Computes harmonic mean of numeric input.\cr
#' \code{\link{calc.geo_mean}} \tab Computes geometric mean of numeric input.\cr
#' \code{\link{ratio}} \tab Computes ratio-based transforms for numeric input.\cr
#' \code{\link{ranking.algorithm}} \tab Assigns rank values using threshold mapping logic.\cr
#' \code{\link{range_diff}} \tab Returns inclusive range width (max minus min).\cr
#' \code{\link{odds2probs}} \tab Converts odds representations to probabilities.\cr
#' }
#'
#' @section Chapter 2 - Counters:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{count_cycles}} \tab Generates a cycle counter from a logical condition vector.\cr
#' \code{\link{factor_int}} \tab Computes integer factors for numeric inputs.\cr
#' \code{\link{radix}} \tab Converts numeric representations across radix bases.\cr
#' \code{\link{factor.int}} \tab Alias for \code{\link{factor_int}}.\cr
#' \code{\link{count.cycles}} \tab Alias for \code{\link{count_cycles}}.\cr
#' }
#'
#' @section Chapter 3 - Miscellaneous:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{log_note}} \tab Appends timestamped note text to file or console.\cr
#' \code{\link{vlogical}} \tab Performs vectorized pattern matching for one or more patterns.\cr
#' \code{\link{polyname2orig}} \tab Rewrites polynomial feature names to source-variable form.\cr
#' \code{\link{gen_primes}} \tab Generates prime numbers within a supplied domain.\cr
#' \code{\link{call_recursion}} \tab Applies a function repeatedly while a test condition is true.\cr
#' \code{\link{checksum}} \tab Compares an object digest to an expected hash value.\cr
#' \code{\link{gen.primes}} \tab Alias for \code{\link{gen_primes}}.\cr
#' \code{\link{call.recursion}} \tab Alias for \code{\link{call_recursion}}.\cr
#' }
#'
#' @section Chapter 4 - Custom Operators:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{\%bin\%}} \tab Bins numeric values to fixed intervals using modulo subtraction.\cr
#' \code{\link{\%><\%}} \tab Wrapper for \code{\link{test_between}} interval logic.\cr
#' \code{\link{\%tf\%}} \tab Creates paired true/false value choices.\cr
#' \code{\link{\%?\%}} \tab Applies if-then-else selection using logical conditions.\cr
#' \code{\link{\%??\%}} \tab Performs correlated row-wise conditional selection.\cr
#' \code{\link{\%if_na_empty\%}} \tab Replaces \code{NA} or empty values from another object.\cr
#' \code{\link{test_between}} \tab Tests whether ranges/values are between target ranges.\cr
#' }
#'
#' @section Chapter 5 - Object Management:
#' \tabular{ll}{
#' \strong{Function} \tab \strong{Synopsis}\cr
#' \code{\link{distinct.list}} \tab Returns first-level unique list elements.\cr
#' \code{\link{enlist}} \tab Converts vectors to named lists.\cr
#' \code{\link{get.object_sizes}} \tab Recursively reports object sizes with depth information.\cr
#' }
#'
#' @importFrom magrittr %>% %T>% or %<>%
#' @importFrom purrr map map_lgl map_chr reduce modify modify_if modify_at
#' @importFrom data.table %ilike% %like% like := .N .SD
#' @importFrom foreach %do% %dopar%
#' @importFrom methods new
#' @importFrom stats sd
#' @importFrom utils object.size
#' @name book.of.utilities
NULL
