distinct.list <- function(i, logical.only = FALSE){
#' Unique List Selection
#'
#' Return the distinct set of first-level list elements based on their respective base64-encoded signatures
#'
#' @param i (object) The input list object (or coercible)
#' @param logical.only (logical) When \code{TRUE}, a logical vector is returned
#'
#' @return The list with unique, first-level child elements
#' @family List management
#'
#' @family Object management
#'
#' @export

 if (!foreach::getDoParRegistered()){ foreach::registerDoSEQ() }

	i = if (any(class(i) %in% c("data.table", "data.frame", "tibble"))){ as.list(i) } else { i }
	f = { foreach::foreach(
					j = i
					, .packages = "magrittr"
					, .multicombine = TRUE, .maxcombine = 1E4
					, .final = function(k){ not(duplicated(unlist(k))) }
					)}
	x = foreach::`%dopar%`(f, { jsonlite::serializeJSON(j) %>% jsonlite::base64_enc() %>% purrr::reduce(paste0) });
	if (logical.only){ x } else { i[x] }
}
#
enlist <- function(x, ...){
#' Create a Named List
#'
#' \code{enlist} creates a list from an atomic vector.  When names having the length of the input are provided to \code{...}, a list with members named accordingly is produced.  Otherwise, the members are named by their values (eg. \code{list(this="this", that="that")} ): this is the default behavior.
#'
#' @param x A vector of values
#' @param ... Names to use for the list members: must be the same length as \code{x}.  Can be a vector or an atomic listing.
#'
#' @return A list, the names of which being the values of \code{x}
#'
#' @family Object management
#'
#' @export
#'

	idx = 1:length(x);
	old.names = purrr::map(x, names);
	new.names = as.character(c(...))[idx];
	.out = purrr::map(idx, ~{
			ifelse(is.null(old.names[[.x]]), ifelse(is.na(new.names[.x]), x[[.x]], new.names[.x]), old.names[[.x]])
		}) %>% unlist()

	names(x) <- ifelse(is.na(.out), unlist(x), .out);
	as.list(x)
}
#
scrub.data <- function(input, condFn = is.na, replacement, ...) {
#' Scrub and Replace Values
#'
#' \code{scrub.data} replaces values that meet the condition given by the function passed to \code{condFn}
#'
#' @param input: An object that contains values to scrub
#' @param condFn: A function name or function expression that serves as the test for values to scrub
#' @param replacement: The replacement value
#' @param ... Not used
#'
#' @family Object management
#'
#' @export

  has.dimensions =  any(class(input) %ilike% "matrix|data|array|tibb|tabl");
  class(replacement) <- class(input);

  .index = which(condFn(input), arr.ind = has.dimensions);

  input[.index] <- replacement[ifelse(length(replacement) == 1, 1, .index)];
  input;
}
#
fill.na <- function(i, incl.null = TRUE){
#' Fill in a Sparse Vector
#' \code{fill.na} fills in positions equivalent to \code{val} with the first preceding value not equal to \code{val}
#'
#' @param i The input vector
#' @param incl.null (logical) Should \code{NULL} values be accepted with \code{NA}'s?
#'
#' @return The input vector with values filled in
#'
#' @family Object management
#'
#' @export

	x = data.table::data.table(this_col = i);
	logi_vec = function(k){ !is.na(k) & { if (incl.null){ !is.null(k) } else { TRUE } }}

	# count.cycles() creates the fill group index based on a logical vector
	x[, fill_group := count.cycles(cond = logi_vec(this_col), offset = 1, reset = FALSE)][
		# The following takes the non-{NA/NULL} value by group and repeats the value depending on the number of rows in the group (go to this link and search 'Special symbol .N')
		, this_col := rep.int(data.table::last(this_col[logi_vec(this_col)]), .N)
		,  by = fill_group
	][, this_col]
}
#
