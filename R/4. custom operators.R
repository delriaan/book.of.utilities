`%bin%` <- function(x, b){
#' Fixed-Interval Binning Operator
#'
#' \code{\%bin\%} creates a vector of values in \code{a} represented as multiples of \code{b}
#'
#' @param x (vector) A vector of numeric values
#' @param b (integer) A bin size to use
#'
#' @return The input after subtracting Modulo(b): x - (x %% b)
#'
#' @family Custom operators
#'
#' @export

	.mod_diff = (x %% b);
	.mod_diff_ratio = round(.mod_diff/x, 4);
  .out_data = x - (x %% b);
	attr(.out_data, "mod_diff_ratio") <- .mod_diff_ratio;
	.out_data;
}
#
`%><%` <- function(a, z, temporal = FALSE, as.text = FALSE, debug = FALSE){
#' "Between" Operator
#'
#' \code{\%><\%} chunks out each of the inputs using row-wise 'apply' and returns a boolean result matrix. When arguments \code{a} and \code{z} are vectors or tensors, "between" is interpreted as the existence of the intersection of the domains spanned by the vectors/tensors. When \code{temporal} is \code{TRUE}, an ordinal response is returned depending on the value of \code{as.text}
#'
#' \code{as.text FALSE}: the return matrix is comprised of \code{"-1, 0, 1"} denoting \code{"before" "during" "after"}
#'
#' \code{as.text TRUE}: the return matrix is comprised of \code{"before" "during" "after"}
#'
#' @param a Preferrably a data table or data frame, but any object that can be coerced into such
#' @param z Preferrably a data table or data frame, but any object that can be coerced into such
#' @param temporal (logical) Should relative temporal output, e.g., `before`, `during`, `after`, be returned as integers -1 0 1 rather than logical values?
#' @param as.text (logical) Should relative temporal output, e.g., `before`, `during`, `after`, be returned as words rather than logical values?
#'
#' @return An Boolean object comprised of the results of testing for "between-ness"
#'
#' @family Custom operators
#'
#' @export

	if ((FALSE %in% class(z) %like% "(frame|table|tibble|atrix)") | rlang::has_length(unlist(z), 1)) {
	  stop("Argument 'z' cannot be a single value: two values are required at a minimum")
	}
	if (!data.table::is.data.table(a)){ a %<>% data.table::as.data.table() }
	if (!data.table::is.data.table(z)){ z %<>% data.table::as.data.table() }

	# The case of a single-valued `a` argument is addressed by choosing the `*apply` function based on its length
	apply.exprs <- list(
	  stage.apply = c(quote({ inner.loop(a) }), quote({ a[, apply(.SD, 1, inner.loop)] }))
		, stage.dimname = c(quote({ as.character(a) }), quote({ apply(a, 1, stringi::stri_flatten, collapse = ":") }))
		);

	inner.loop <- function(o){
	  sub_fn = function(i, j, k) {
			i %<>% unlist(); j %<>% unlist(); k %<>% unlist();
			if (!(j | k)) {
				# :: Default mode of "between-ness"
				(min(o, na.rm = TRUE) <= max(i, na.rm = TRUE)) & (max(o, na.rm = TRUE) >= min(i, na.rm = TRUE))
			} else {
				# :: Temporal mode to return "before"(-1), "during"(0), "after"(1)
				t1 = min(o) > max(i, na.rm = TRUE)
				t2 = (min(o) <= max(i)) & (max(o, na.rm = TRUE) >= min(i, na.rm = TRUE))
				t3 = c(1, 0, -1)[max(which(t1, t2, TRUE), na.rm = TRUE)]

				# :: Temporal mode to return "before"(-1), "during"(0), "after"(1)
				if (k) {
					c("-1" = "before", "0" = "during", "1" = "after")[as.character(t3)]
				} else { t3 }
			}
		}
		o %<>% unlist();
	  z[, apply(.SD, 1, sub_fn, temporal, as.text)]
  }

	apply.exprs$stage.apply[[1 + as.integer(length(unlist(a)) > 1)]] %>%
	  eval() %>% { if (debug) { assign("debug.op.btwn", eval(.), envir = .GlobalEnv) }; .;} %>%
	  matrix(
	  	nrow	= nrow(a), byrow 		= TRUE
			, dimnames	= list(
					apply.exprs$stage.dimname[[1 + as.integer(length(unlist(a)) > 1)]] |> eval()
					, z |> apply(1, stringi::stri_flatten, collapse = ":")
					)
		)
}
#
`%::%` <- function(tr = TRUE, fls = FALSE, id = 0, ...){
#' Decision Operator
#'
#' Use \code{\%::\%} anywhere where binary choice vectors can be useful
#'
#' @param tr (logical) : Scalar or vector value to return as the TRUE  value
#' @param fls (logical) : Scalar or vector value to return as the FALSE value
#' @param id (scalar) : Scalar or vector value to return as the identifier for the result
#' @param ... (Not used)
#'
#' @return a \code{\link[data.table]{data.table}} of resultant values options for `true` and `false`
#'
#' @family Custom operators
#'
#' @export

	if (!rlang::is_empty(dim(tr))){ tr <- apply(tr, 1, as.list) }
	if (!rlang::is_empty(dim(fls))){ fls <- apply(fls, 1, as.list) }

	# if (length(tr) != length(fls)) { stop("Vectors for TRUE and FALSE must be of the same length: exiting ...") }
	data.table::as.data.table(list(false = fls, true = tr, id = c(id)))
}
#
`%?%` <- function(cond, result){
#' Simple IF-THEN-ELSE Comparison
#'
#' When \code{cond} is \code{TRUE}, \code{result$true} is returned, and the same for \code{cond == FALSE}.  For \code{result}, the easiest way to set the available choices is to use \code{`\%::\%`}; otherwise, a environment(-like) object with members named 'true' and 'false' must be provided
#'
#' @param cond (logical) A \emph{vector} that evaluates to \code{TRUE} or \code{FALSE}
#' @param result (tensor) Resultant values for TRUE and FALSE conditionals
#'
#' @return A \code{\link[data.table]{data.table}} object comprised of values occupying the 'then' and 'else' slots in the 'if-then-else' logical test
#'
#' @family Custom operators
#'
#' @export
#'

	data.table::as.data.table(purrr::imap_dfr(cond + 1, ~c(result = rlang::new_box(result[[.x]]), cond_id = .y)))
}
#
`%??%` <- function(cond, result){
#' Correlated IF-THEN-ELSE Comparison
#'
#' For each element \code{E} in \code{cond}, when \code{E} is \code{TRUE}, the corresponding index of \code{result$true} is returned: the same for \code{cond == FALSE}.
#' For \code{result}, the easiest way to set the available choices is to use \code{\%::\%}; otherwise, a environment(-like) object with members named \code{true} and \code{false}
#'
#' @param cond (logical) A \emph{vector} or \emph{tensor} that evaluates to \code{TRUE} or \code{FALSE}
#' @param result (vector) Resultant values for TRUE and FALSE conditionals, ideally stored in a dimension-ed object (e.g, \code{\link[base]{data.frame}}, \code{\link[data.table]{data.table}})
#'
#' @family Custom operators
#'
#' @export
#'

	if (is.environment(result)){ result <<- data.table::as.data.table(mget(c("true", "false"), envir = result)) }
	if (!data.table::is.data.table(result)){ result <<- data.table::as.data.table(result) }

	foreach::foreach(x = cond, y = iterators::iapply(X = result, MARGIN = 1), id = sequence(length(cond)), .combine = rbind) %do% {  `%?%`(x, y) }
}
#
`%all%` <- function(i, ..., logical.out = FALSE, chatty = FALSE){
#' Retrieve all elements with the given object names
#'
#' This is intended to operate on a named object with duplicate names.  The idea is to allow for "soft" accessing by name
#'
#' @param i The named input object
#' @param ... Symbols or strings of objects to return
#' @param logical.out (logical) When \code{TRUE}, a logical vector or list of such is returned instead of the values satisfying the condition
#' @param chatty (logical) When \code{TRUE}, additional information is printed to console
#'
#' @return An object containing the values or logical results relative to the names of the input
#'
#' @family Custom operators
#'
#' @export

  nms = unique(as.character(rlang::enexprs(...))) |> book.of.utilities::enlist();

  purrr::map(nms, ~{
  	.out = if (logical.out){ .x == names(i) } else { i[.x == names(i)] };
  	sapply(.out, rlang::new_box)
  }) |> purrr::flatten()
}
#
