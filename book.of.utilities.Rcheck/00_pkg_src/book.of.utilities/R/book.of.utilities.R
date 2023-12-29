#' @title Book of Utilities: utilities for working with data
#
#' @description
#' `book.of.utilities` seeks to facilitate execution of those repetitive, ad-hoc tasks often encountered during data processing.
#'
#' The following functional chapters are covered in \code{book.of.utilities}:\cr
#'
#' @section Chapter 1 - Authentication: 
#' \itemize{
#' \item{\code{\link{gen.pass}}}
#' \item{\code{\link{keyring_export}}}
#' \item{\code{\link{keyring_import}}}
#' \item{\code{\link{kr_key}}}
#' \item{\code{\link{s7_properties}}}
#' }
#'
#' @section Chapter 2 - Calculators: 
#' \itemize{
#' \item{\code{\link{calc.geo_mean}}}
#' \item{\code{\link{calc.harmonic_mean}}}
#' \item{\code{\link{calc.means}}}
#' \item{\code{\link{calc.rms}}}
#' \item{\code{\link{calc.zero_mean}}}
#' \item{\code{\link{odds2probs}}}
#' \item{\code{\link{radix}}}
#' \item{\code{\link{range_diff}}}
#' \item{\code{\link{ranking.algorithm}}}
#' \item{\code{\link{ratio}}}
#' }
#'
#' @section Chapter 3 - Counters: 
#' \itemize{
#' \item{\code{\link{count.cycles}}}
#' \item{\code{\link{factor.int}}}
#' }
#'
#' @section Chapter 4 - Custom Operators: 
#' \itemize{
#' \item{\code{\link{\%?\%}}}
#' \item{\code{\link{\%??\%}}}
#' \item{\code{\link{\%><\%}}}
#' \item{\code{\link{\%bin\%}}}
#' \item{\code{\link{\%tf\%}}}
#' \item{\code{\link{test_between}}}
#' }
#'
#' @section Chapter 5 - Object Management: 
#' \itemize{
#' \item{\code{\link{distinct.list}}}
#' \item{\code{\link{enlist}}}
#' \item{\code{\link{get.object_sizes}}}
#' \item{\code{\link{scrub.data}}}
#' }
#'
#' @section Chapter 6 - Package: 
#' \itemize{

#' }
#'
#' @section Chapter 7 - Z Miscellaneous: 
#' \itemize{
#' \item{\code{\link{as.regex}}}
#' \item{\code{\link{call.recursion}}}
#' \item{\code{\link{checksum}}}
#' \item{\code{\link{gen.primes}}}
#' \item{\code{\link{is.regex}}}
#' \item{\code{\link{log_note}}}
#' \item{\code{\link{polyname2orig}}}
#' \item{\code{\link{unregex}}}
#' \item{\code{\link{vlogical}}}
#' }
#'
#'
#' @importFrom magrittr %>% %T>% or %<>%
#' @importFrom purrr map map_lgl map_chr reduce modify modify_if modify_at
#' @importFrom rlang is_empty %||%
#' @importFrom data.table %ilike% %like% like := .N .SD
#' @importFrom foreach %do% %dopar%
#' @importFrom methods new
#' @importFrom stats sd
#' @importFrom utils object.size
#' @name book.of.utilities
NULL
