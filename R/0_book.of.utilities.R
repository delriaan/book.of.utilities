#' @title Book of Utilities Overview
#' @description
#' `book.of.utilities` seeks to facilitate execution of those repetitive, ad-hoc tasks often encountered during data processing.
#'
#' The following functional families are covered in \code{book.of.utilities}:\cr
#'
#' @section Calculators:
#' \itemize{
#'	\item{\code{\link{range.diff}}}
#'	\item{\code{\link{calc.means}}}
#'	\item{\code{\link{calc.geo_means}}}
#'	\item{\code{\link{calc.harmonic_mean}}}
#'	\item{\code{\link{calc.rms}}}
#'	\item{\code{\link{ratio}}}
#'	\item{\code{\link{ranking.algorithm}}}
#' }
#' @section Object Management:
#' \itemize{
#'	\item{\code{\link{distinct.list}}}
#'	\item{\code{\link{enlist}}}
#'	\item{\code{\link{scrub.data}}}
#'	\item{\code{\link{fill.na}}}
#' }
#' @section Counters:
#' \itemize{
#'	\item{\code{\link{do.count}}}
#'	\item{\code{\link{count.cycles}}}
#'	\item{\code{\link{factor.int}}}
#' }
#' @section Custom Operators:
#' \itemize{
#'	\item{\code{\link{\%bin\%}}}
#'	\item{\code{\link{\%><\%}}}
#'	\item{\code{\link{\%::\%}}}
#'	\item{\code{\link{\%?\%}}}
#'	\item{\code{\link{\%??\%}}}
#'	\item{\code{\link{\%all\%}}}
#' }
#' @section Miscellaneous:
#' \itemize{
#'	\item{\code{\link{mark.time}}}
#'	\item{\code{\link{log_note}}}
#'	\item{\code{\link{vlogical}}}
#'	\item{\code{\link{as.regex}}}
#'	\item{\code{\link{is.regex}}}
#'	\item{\code{\link{unregex}}}
#'	\item{\code{\link{polyname2orig}}}
#'	\item{\code{\link{gen.primes}}}
#' }
#'
#' @importFrom magrittr %>% %T>% or %<>%
#' @importFrom purrr map map_lgl map_chr reduce modify modify_if modify_at
#' @importFrom rlang is_empty %||%
#' @importFrom data.table %ilike% %like% like
#' @importFrom foreach %do% %dopar%
#' @name Book of Utilities Package
NULL

