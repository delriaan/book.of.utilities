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
	#' @family Custom Operators
	#'
	#' @export

	.mod_diff = x %% b;
	.mod_diff_ratio = round(.mod_diff/x, 10);
  .out_data = x - (x %% b);
	attr(.out_data, "mod_diff_ratio") <- .mod_diff_ratio;
	.out_data;
}

#
test_between <- function(a, z, temporal = FALSE, as.text = FALSE){
	#' "Between" Operator
	#'
	#' \code{test_between} and related operator \code{\%><\%} chunks out each of the inputs using row-wise 'apply' and returns a Boolean result matrix. When arguments \code{a} and \code{z} are vectors or tensors, "between" is interpreted as the existence of the intersection of the domains spanned by the vectors/tensors. When \code{temporal} is \code{TRUE}, an ordinal response is returned depending on the value of \code{as.text}
	#'
	#' \code{as.text FALSE}: the return matrix is comprised of \code{"-1, 0, 1"} denoting \code{"before" "during" "after"}
	#'
	#' \code{as.text TRUE}: the return matrix is comprised of \code{"before" "during" "after"}
	#'
	#' @param a Preferably a data table or data frame, but any object that can be coerced into such
	#' @param z Preferably a data table or data frame, but any object that can be coerced into such
	#' @param temporal (logical) Should relative temporal output (e.g., `before`, `during`, `after`) be returned as integers -1 0 1 rather than logical values?
	#' @param as.text (logical) Should relative temporal output (e.g., `before`, `during`, `after`) be returned as words rather than logical values?
	#'
	#' @return An Boolean object comprised of the results of testing for "between-ness"
	#'
	#' @family Custom Operators
	#'
	#' @export

	if (!data.table::is.data.table(a)){ a %<>% data.table::as.data.table() }
	if (!data.table::is.data.table(z)){ z %<>% data.table::as.data.table() }

	# The case of a single-valued `a` argument is addressed by choosing the `*apply` function based on its length
	apply.exprs <- list(
	  stage.apply = list(
	  		rlang::expr(inner.loop(a))
	  		, rlang::expr(purrr::as_mapper(~.x[, apply(.x, 1, inner.loop)])(a))
	  		)
		, stage.dimname = list(
				rlang::expr(as.character(a))
				, rlang::expr(apply(a, 1, stringi::stri_flatten, collapse = ":"))
				)
		);

	inner.loop <- function(o){
	  sub_fn = function(i, j, k) {
			i %<>% unlist();
	  	j %<>% unlist();
	  	k %<>% unlist();

			if (!(j | k)) {
				# :: Default mode of "between-ness"
				(min(o, na.rm = TRUE) <= max(i, na.rm = TRUE)) & (max(o, na.rm = TRUE) >= min(i, na.rm = TRUE))
			} else {
				# :: Temporal mode to return "before"(-1), "during"(0), "after"(1)
				t1 = min(o) > max(i, na.rm = TRUE)
				t2 = (min(o) <= max(i)) & (max(o, na.rm = TRUE) >= min(i, na.rm = TRUE))
				t3 = c(1, 0, -1)[max(which(t1, t2, TRUE), na.rm = TRUE)]

				# :: Temporal mode to return "before"(-1), "during"(0), "after"(1)
				if (k) { c("-1" = "before", "0" = "during", "1" = "after")[as.character(t3)] } else { t3 }
			}
		}
		o %<>% unlist();
	  purrr::as_mapper(~.x[, apply(.x, 1, sub_fn, temporal, as.text)])(z)
  }

	apply.exprs$stage.apply[[1 + as.integer(length(unlist(a)) > 1)]] %>%
	  eval() %>%
	  matrix(
	  	nrow	= nrow(a), byrow 		= TRUE
			, dimnames	= list(
					apply.exprs$stage.dimname[[1 + as.integer(length(unlist(a)) > 1)]] |> eval()
					, apply(z, 1, stringi::stri_flatten, collapse = ":")
					)
		)
}
#
`%><%` <- function(a, z){
	#' \code{\link{test_between}} Operator
	#'
	#' @param a Preferably a data table or data frame, but any object that can be coerced into such
	#' @param z Preferably a data table or data frame, but any object that can be coerced into such
	#'
	#' @return An Boolean object comprised of the results of testing for "between-ness"
	#'
	#' @family Custom Operators
	#'
	#' @export
	test_between(a = a, z = z)
}

#
`%tf%` <- function(true = TRUE, false = FALSE){
	#' True-False Operator
	#'
	#' @description
	#' Use \code{\%tf\%} where binary choice vectors can be useful
	#'
	#' @param true (logical) : Scalar or vector value to return as the TRUE  value
	#' @param false (logical) : Scalar or vector value to return as the FALSE value
	#'
	#' @return a \code{\link[data.table]{data.table}} of resultant values options for `true` and `false`
	#'
	#' @family Custom Operators
	#'
	#' @export

	if (!rlang::is_empty(dim(true))){ true <- apply(true, 1, as.list) }
	if (!rlang::is_empty(dim(false))){ false <- apply(false, 1, as.list) }

	data.table::data.table(false = false, true = true, key = NULL)
}

#
`%?%` <- function(cond, result){
	#' Simple IF-THEN-ELSE Operator
	#'
	#' When \code{cond} is \code{TRUE}, \code{result$true} is returned, and the same for \code{cond == FALSE}.  For \code{result}, the easiest way to set the available choices is to use \code{\%::\%}; otherwise, a environment(-like) object with members named 'true' and 'false' must be provided
	#'
	#' @param cond (logical) A \emph{vector} that evaluates to \code{TRUE} or \code{FALSE}
	#' @param result (tensor) Resultant values for TRUE and FALSE conditionals
	#'
	#' @return A \code{\link[data.table]{data.table}} object comprised of values occupying the 'then' and 'else' slots in the 'if-then-else' logical test
	#'
	#' @family Custom Operators
	#'
	#' @export

	purrr::imap_dfr(cond + 1, \(x, y) c(result = rlang::new_box(result[[x]]))) |>
		data.table::as.data.table()
}

#
`%??%` <- function(cond, result){
	#' Correlated IF-THEN-ELSE Comparison
	#'
	#' For each element \code{E} in \code{cond}, when \code{E} is \code{TRUE}, the corresponding index of \code{result$true} is returned: the same for \code{cond == FALSE}. The important thing to remember is that the function returns a value for \code{TRUE} \emph{\bold{and}} \code{FALSE} per row.\cr For \code{result}, the easiest way to set the available choices is to use \code{\link{\%tf\%}}; otherwise, a environment(-like) object with members named \code{true} and \code{false}.
	#'
	#' @param cond (logical) A rank-n object for which each row contains \code{TRUE} or \code{FALSE}
	#' @param result (vector) A rank-n object for which each row contains resultant values for each element of \code{cond[m, n]} containing values for \code{TRUE} and \code{FALSE}. The result can be of a different structure than the input as is the case when each result is of heterogeneous lengths.
	#'
	#' @return A list of values, one element per row of input.
	#'
	#' @family Custom Operators
	#'
	#' @export

	if (is.environment(result)){
		result <- with(result, mget(c("true", "false"))) |> data.table::as.data.table()
	}

	assertive::assert_all_are_true(c(
		assertive::has_dims(cond)
		, assertive::has_dims(result)
	))

	.queue <- 1:nrow(cond);
	res <- replicate(nrow(cond), vector(mode = "list", length = ncol(cond)));

	lapply(.queue, \(i){
		# browser()
		j <- names(result)[unlist(cond[i, ], use.names = FALSE) + 1];
		result[i, mget(c(j))]
	})
}

#' Replace NA and Empty Values
#' 
#' This function is a wrapper for \code{\link[base]{ifelse}} by means of \code{\link[purrr]{modify2}}. \code{NA} and empty values in \code{x} are replaced by values provided in \code{y}.
#' 
#' @param x,y The input and replacement values, respectively. Data types are treated as per \code{\link[purrr]{modify2}}, and arguments must be of equal length
#' 
#' @return The input with \code{NA} and empty values replaced.
#' @family Custom Operators
#'
#' @export
`%if_na_empty%` <- \(x, y) purrr::modify2(x, y, \(a, b) ifelse(rlang::is_empty(na.omit(a)), b, a))
