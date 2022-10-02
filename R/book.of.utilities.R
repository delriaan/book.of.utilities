# @importFrom magrittr %>% %T>% or
#' @importFrom purrr map map_lgl map_chr reduce modify modify_if modify_at
#' @importFrom rlang is_empty %||%
NULL
# :: Calculators ====
#
calc.range_diff <- function(...) {
#' Magnitude of a Range of Values
#'
#' \code{calc.range_diff} calculates the difference in the range of values given by \code{input}
#'
#' @param ... (numeric[]): A numeric vector for which the range and corresponding difference is calculated. If a list is provided, the calculation is executed over each element of the list.
#' @return A scalar value of the inclusive difference of upper and lower boundaries of the range of the input vector
#' @family Calculators
#'
#' @export

	action = function(i){ i = as.numeric(unlist(i)); max(i, na.rm = TRUE) - min(i, na.rm = TRUE); }

	i = rlang::list2(...);

	if (any(purrr::map_lgl(i, ~length(.x) > 1))){ purrr::map(i, action) } else { action(i = i) }
}
#
calc.means <- function(a, mean.type = "am", post.op = eval) {
#' Calculate Means
#'
#' \code{calc.means} calculates various types of central-tendency measures for the vector passed to argument \code{a}
#'
#' If `mean.type` contains multiple values, a list object is returned containing one member for each output; otherwise, a single value is returned with a class dependent on the value of `mean.type`.
#' Valid `mean.type` values are as follows:
#' \enumerate{
#' \item "am" (Arithmetic-Mean \url{https://en.wikipedia.org/wiki/Arithmetic_mean}): Return the sum of vector values divided by the magnitude of the vector
#' \item "zm" (Zero-Mean): Return the vector with the mean of the vector subtracted from all values
#' \item "gm" (Geometric Mean \url{https://en.wikipedia.org/wiki/Geometric_mean}): Return the \emph{n-th} root of the product of an \emph{n}-element input vector.  Negative input values are allowed, resulting in a complex result
#' \item "hm" (Harmonic Mean \url{https://en.wikipedia.org/wiki/Harmonic_mean}): Return the reciprocal of the arithmetic mean of the reciprocal of the elements of the input vector
#' \item "rms"(Root Mean Squared \url{https://en.wikipedia.org/wiki/Root_mean_square}): Return the square-root of the arithmetic mean of the squares of the elements of the input vector
#' }
#'
#' @param a (numeric): A numeric vector
#' @param mean.type (string): A character vector of the types of mean value operations to execute (see "Details")
#' @param post.op (call | \code{\link[base]{eval}}): A function that will process the output before returning
#'
#' @return Arguments \code{a}, \code{mean.type}, and \code{post.op} determine the return type.
#'
#' @family Central-tendency calculations
#'
#' @export

	a = as.vector(a);

  func.list <- { list(
      am	= function(i){ if (length(unique(i)) == 1) { 0 } else { mean(i, na.rm = TRUE) }}
		, zm	= function(i){ if (length(unique(i)) == 1) { 0 } else { i - mean(i, na.rm = TRUE) }}
		, gm	= function(i){
				if (any(sign(i) == -1)) { i <- as.complex(i) }
				prod(i, na.rm = TRUE)^(length(i) ^ -1)
			}
		, hm	= function(i){ length(i) / sum(i^-1, na.rm = TRUE) }
		, rms = function(i){ ifelse(length(unique(i)) == 1, i, sapply(i, `^`, 2) %>% mean(na.rm = TRUE) %>% sqrt) }
    )}

  if ("*" %in% mean.type) { mean.type <- names(func.list) }

	output = if ("list" %in% class(a)){
		purrr::map(func.list[mean.type], ~{ fn = .x; purrr::map(a, fn) })
		} else if (any(c("data.frame", "data.table", "matrix") %in% class(a))){
			purrr::map(func.list[mean.type], ~{ fn = .x; apply(X = a, FUN = fn, ...) })
			} else { purrr::map(func.list[mean.type], ~.x(a) ) }

	post.op(if (rlang::has_length(output, 1)){ output[[1]] } else { output })
}
#
calc.zero_mean <- function(a, post.op = eval) {
#' Calculate the Zero-Mean
#'
#' \code{calc.zero_mean} subtracts the mean from the input
#'
#' This is an alias that calls \code{\link{calc.means}(a, "hm")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "zm", post.op = post.op) %>% unlist()
}
#
calc.rms <- function(a, post.op = eval) {
#' Calculate the Root-Mean-Square
#'
#' This is an alias that calls \code{\link{calc.means}(a, "rms")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "rms", post.op = post.op) %>% unlist()
}
#
calc.harmonic_mean <- function(a, post.op = eval) {
#' Calculate the Harmonic Mean
#'
#' This is an alias that calls \code{\link{calc.means}(a, "hm")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "hm", post.op = post.op) %>% unlist()
}
#
calc.geo_mean <- function(a, post.op = eval) {
#' Calculate the Geometric Mean
#'
#' This is an alias that calls \code{\link{calc.means}(a, "gm")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "gm", post.op = post.op) %>% unlist()
}
#
ratio <- function(i, j = i, type = NULL, decimals = 2){
#' Ratio Calculator
#'
#' \code{ratio} calculates one of the following ratio types:\cr
#' \enumerate{
#' \item{\code{"pareto"} (cumulative total vs. total)}
#' \item{\code{"of.max|of.min"} (relative to maximum/minimum value)}
#' \item{\code{NULL} (relative to total)}
#' }.\cr Using the related operator \code{\%ratio\%} assumes simple division by the total of \code{j}.
#'
#' @param i (vector) The vector of numeric scores in the numerator
#' @param j (vector) The number of numeric scores in the denominator
#' @param type (string[]) The types of ratio algorithms to use (see Details): a vector of values is supported
#' @param decimals (integer | 2) The number of decimal places to which the output should be rounded
#'
#' @family Calculators
#'
#' @export

	action = function(ii, jj){
		sub.fn = function(k, .i, .j, .d){
			if (!is.list(.j)){
				switch(
					k
					, "pareto" = cumsum(.i)/sum(.j)
					, "of.max" = .i/max(.j)
					, "of.min" = .i/min(.j)
					, .i/sum(.j)
					) %>% round(.d)
			} else { map(.j, ~sub.fn(k = k, .i = .i, .j = .x)) }
		}
		if (length(type) > 1){ enlist(type) %>% map(sub.fn, ii, jj, decimals) } else { sub.fn(type, ii, jj, decimals) }
	}
	if (any(purrr::map_lgl(i, ~length(.x) > 1))){ purrr::map2(i, j, action) } else { action(i, j) }
}
	#' @export
	`%ratio%` <- ratio;
	#' @export
	`%pareto%` <- { assign("x", ratio); formals(x) <- alist(i = , j = , type = "pareto"); x };
#
ranking.algorithm <- function(
	scores, rank.size, poss.scores, test = `>=`, rescale = 1
	, map.only = FALSE, show.all = FALSE, use.futures = FALSE, dflt.fn = min
	, map.fn = (purrr::map)
	){
#' Ranking Algorithm
#'
#' \code{ranking.algorithm} ranks values across a vector of values
#'
#' @param scores (vector) The vector of numeric scores to rank
#' @param rank.size (integer) The number of ranks to allow
#' @param poss.scores (vector) The distinct values demarcating each rank threshold
#' @param test (object) A function or operator that takes in as input the scores, performs a conditional test, and returns a logical vector of equal length
#' @param rescale (numeric) A multiplier that scales the scores vector up or down
#' @param map.only (logical|FALSE) When \code{TRUE}, a \href{https://www.rdocumentation.org/packages/data.table/versions/1.12.8}{data.table} object of the rank and rank thresholds is returned
#' @param show.all (logical|FALSE) When \code{TRUE}, a \href{https://www.rdocumentation.org/packages/data.table/versions/1.12.8}{data.table} object of the scores, ranks thresholds, and ranks are returned
#' @param use.futures (logical|FALSE) When \code{TRUE}, the operation is parallelized using \href{https://www.rdocumentation.org/packages/future/versions/1.17.0}{future}s
#' @param dflt.fn (function) The default function to use when replacing \code{NA} values in ranks
#' @param map.fn (function) The function to iterate the scores when computing the output
#'
#' @family Calculators
#'
#' @export

  if (sign(rescale) < 1) {
    message("`rescale` is not positive or > 0: setting to 1");
    rescale == 1;
  }

  # %>%  Ensure atomic vectors are used
  scores		= rescale * unlist(scores);
  ranks 		= 1:rank.size - 1;
  increment = calc.range_diff(poss.scores) / length(ranks);

  # %>%  Create the vector of thresholds using the value of `rescale` to ensure correct results
  thresholds.list <- min(poss.scores) + (ranks * increment);

  #  %>% Put the ranks output on a 1-based scale
  ranks <- ranks + 1;

  if (map.only) { return(data.table(ranks = ranks, thresholds = thresholds.list))
  } else {
    output <- map.fn(scores, ~ ranks[ test(.x, thresholds.list) %>% which() %>% max() ]
		) %>% unlist() %>% scrub.data(replacement = dflt.fn(ranks))

	if (show.all) {
		data.table(
			scores = scores / rescale
			, thresholds = list(c(thresholds.list / rescale))
			, rank = output
			, key = c("scores")
			)
		}
	else { output }
  }
}
#
# :: Counters ====
#
do_count <- function(i, distinct = FALSE) {
#' Count Stuff
#'
#' @param i A vector of things to count
#' @param distinct (logical) When \code{TRUE}, a distinct count of objects is returned
#'
#' @return The count of elements in \code{i}, possibly distinct
#'
#' @family Counters
#'
#' @export

  cls <- class(i)

 switch(
	cls
	, "list"		= length(if(distinct){ distinct.list(i) } else { i })
	, "matrix"	= if (distinct){ nrow(unique(as.data.table(i))) } else { nrow(i) }
	, "data.frame" = if (distinct){ nrow(unique(as.data.table(i))) } else { nrow(i) }
	, "data.table" = if (distinct){ nrow(unique(as.data.table(i))) } else { nrow(i) }
	, length(if (distinct) { unique(i) } else { i })
	)
}
#
count.cycles <- function(cond, offset = 1, reset){
#' Cycle Counter
#'
#' \code{count.cycles} detects and maps conditional occurrence frequency
#'
#' @param cond (logical[]) A logical vector
#' @param offset (integer|1) An integer defining the series origin
#' @param reset (logical[]) A logical vector of the same length as \code{cond} such that when \code{TRUE}, the cycle counter resets to the offset
#'
#' @return A one-based (default), conditionally-incremented series
#'
#' @family Counters
#'
#' @examples
#' data.table::data.table(cond_vec = (runif(1000) >= 0.7) %>% sample(100), rand_col = sample(letters, 100, TRUE), key = "rand_col")[, .(cond_vec, rand_col, cyc = count.cycles(cond_vec, reset = !duplicated(rand_col)))]
#'
#' @export

	# :: cycle_idx MUST be initialized before session() (see below) is called iteratively
	offset = as.integer(offset)[1];
	cycle_idx = offset;

	# :: "counter()" is a function delegate and is the ESSENTIAL part of the routine
	# Note how it is contained WITHIN this specific code block and is designed for conditional increment
	# See http://adv-r.had.co.nz/Functional-programming.html, section "Mutable State"

	count_idx = iterators::iter(1:length(cond));

	# %>% Define the counter function
	counter = function(tf) {
		idx = iterators::nextElem(count_idx);
	  if (tf) {
	  # cycle_idx only resets to the offset value when the limiter function returns 'TRUE'; otherwise, it increments
			cycle_idx <<- cycle_idx + 1
	  }

	  # cycle_idx only resets to the offset value when the limiter function returns 'TRUE'; otherwise, it increments
		cycle_idx <<- ifelse((idx %in% which(reset))|(idx == 1), offset, cycle_idx)

		# return the current value of 'cycle_idx'
		cycle_idx
	}

  # %>% Call the counter function
  sapply(cond, counter);
}
#
factor.int <- function(i, ...){
#' Factorization of Integers
#'
#' @param i (integer[]) One or more integers to factor
#' @param ... Additional integers to factor (can be used in conjunction with \code{i})
#'
#' @return The factors of the input given as an vector or list of vectors
#'
#' @family Counters
#'
#' @export

	if (missing(i) & ...length() == 0){ message("[ERROR] Please supply integer values to either 'i' or '...'"); return(0); }
	if (missing(i) & ...length() != 0){ i <- 0 }
	i <- c(i, rlang::list2(...));

	if (is.list(i)){ i <- unlist(i, use.names = FALSE) }
	if (is.factor(i)){ i <- as.integer(i) }
	if (is.character(i)){ i <- as.integer(i) }
	if (is.numeric(i)){ i <- as.integer(i) }

	i <- purrr::set_names(i);
	.out = purrr::map(i, ~{ .int = .x; purrr::keep(sequence(.int), ~.int %% .x == 0) });

	if (length(i) > 1){ distinct.list(.out[order(as.integer(names(.out)))]) } else { unlist(.out, use.names = FALSE) }
}
# :: Object Management ====
#
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
	x = foreach::`%dopar%`(
				foreach::foreach(
					j = i
					, .packages = "magrittr"
					, .multicombine = TRUE, .maxcombine = 1E4
					, .final = function(k){ k %>% unlist %>% duplicated	%>% not }
					)
				, { jsonlite::serializeJSON(j) %>% jsonlite::base64_enc() %>% purrr::reduce(paste0) }
			);

	if (logical.only){ x } else { i[x] }
}

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
	purrr::map(idx, ~ifelse(is.null(old.names[[.x]]), ifelse(is.na(new.names[.x]), x[[.x]], new.names[.x]), old.names[[.x]])) %>% unlist() %>% {
		names(x) <- ifelse(is.na(.), unlist(x), .);
		as.list(x)
	}
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

  input[which(condFn(input), arr.ind = has.dimensions)] <- replacement;
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
	x[, fill_group := count.cycles(cond = logi_vec(this_col), offset = 1, reset = FALSE)
	][
		# The following takes the non-{NA/NULL} value by group and repeats the value depending on the number of rows in the group (go to this link and search 'Special symbol .N')
		, this_col := rep.int(last(this_col[logi_vec(this_col)]), .N)
		,  by = fill_group
	][, this_col]
}
#
# :: Prime number generator ====
gen.primes <- function(n = 1, domain = 2:n, random = FALSE, distinct = TRUE, chatty = FALSE){
#' Prime Number Generator
#'
#' \code{get.primes} generates \code{n} prime numbers..
#' @param n (integer) The number of prime numbers to generate
#' @param domain (integer[]) The range of that prime numbers should fall into given as a length two (2) integer vector
#' @param out.length (integer) The number of prime numbers to generate return
#' @param random (logical) When \code{TRUE}, the internally-generated vector is subsetted via sampling
#' @param distinct (logical) When \code{TRUE}, only unique (and sorted) values are returned
#' @param chatty (logical) When \code{TRUE}, various output is printed to console, mainly for debugging purposes
#'
#' @return A vector of prime numbers
#'
#' @export

	# Initialize
	set.seed(Sys.time(), kind = sample(c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Knuth-TAOCP-2002", "L'Ecuyer-CMRG"), 1));
	x = min(domain):max(domain);
	z = 1:max(domain);

	logi.vec = { outer(x, z, `%%`) == 0 } %>% rowSums == 2;

	output = x[logi.vec];
	output = if (random){ sample(output, n, replace = TRUE) } else { output[1:n] }
	output = if (distinct){ unique(output) } else { output }
	output %>% sort
	# gen.primes(n = 100, domain = c(450, 9571), random = FALSE, distinct = TRUE) %>% { data.table::data.table(x = ., d = c(0, diff(.))) } %T>% print %>% plot
}

# :: Custom Operators ====
#
`%bin%` <- function(x, b) {
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

`%><%` <- function(a, z, temporal = FALSE, as.text = FALSE, debug = FALSE) {
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

	if ((FALSE %in% class(z) %like% "(frame|table|tibble|atrix)") | length(z %>% unlist()) == 1) {
	  stop("Argument 'z' cannot be a single value: two values are required at a minimum")}

	if (!is.data.table(a)){ a <- as.data.table(a) }
	if (!is.data.table(z)){ z <- as.data.table(z) }

	# The case of a single-valued `a` argument is addressed by choosing the `*apply` function based on its length
	apply.exprs <- list(
	    stage.apply = c(quote({ inner.loop(a) }), quote({ a[, apply(.SD, 1, inner.loop)] }))
		, stage.dimname = c(quote({ a %>% as.character() }), quote({ a %>% apply(1, stri_flatten, collapse = ":") }))
		);

	inner.loop <- function(o){
		o = unlist(o);
	  z[, apply(.SD, 1, function(i, j, k) {
				i = unlist(i); j = unlist(j); k = unlist(k);
				if (!(j | k)) { # :: Default mode of "between-ness"
					(min(o) <= max(i)) & (max(o) >= min(i))
					} else {
					# :: Temporal mode to return "before"(-1), "during"(0), "after"(1)
						c(ifelse( min(o) > max(i), 1,
							ifelse((min(o) <= max(i)) & (max(o) >= min(i)), 0 ,-1)
							)) %>% max() %>% {
							# :: Temporal mode to return "before"(-1), "during"(0), "after"(1)
							if (k) { c("-1" = "before", "0" = "during", "1" = "after")[as.character(.)] } else { . }
							}
						}
			}, temporal, as.text)]
  }

	apply.exprs$stage.apply[[1 + as.integer(length(unlist(a)) > 1)]] %>%
	  eval() %>% { if (debug) { assign("debug.op.btwn", eval(.), envir = .GlobalEnv) }; .;} %>%
	  matrix(nrow	= nrow(a), byrow 		= TRUE
		, dimnames	= list(
				apply.exprs$stage.dimname[[1 + as.integer(length(unlist(a)) > 1)]] %>% eval()
				, z %>% apply(1, stringi::stri_flatten, collapse = ":")
				)
		)
}

`%::%` <- function(tr = TRUE, fls = FALSE, id = 0, ...) {
#' Decision Operator
#'
#' Use \code{\%::\%} anywhere where binary choice vectors can be useful
#'
#' @param tr (logical) : Scalar or vector value to return as the TRUE  value
#' @param fls (logical) : Scalar or vector value to return as the FALSE value
#' @param id (scalar) : Scalar or vector value to return as the identifier for the result
#'
#' @return a \code{\link[data.table]{data.table}} of resultant values options for `true` and `false`
#'
#' @family Custom operators
#'
#' @export

	if (!rlang::is_empty(dim(tr))){ tr <- apply(tr, 1, as.list) }
	if (!rlang::is_empty(dim(fls))){ fls <- apply(fls, 1, as.list) }

	# if (length(tr) != length(fls)) { stop("Vectors for TRUE and FALSE must be of the same length: exiting ...") }
	as.data.table(list(false = fls, true = tr, id = c(id)))
}
#
`%?%` <- function(cond, result) {
#' Simple IF-THEN-ELSE Comparison
#'
#' When \code{cond} is \code{TRUE}, \code{result$true} is returned, and the same for \code{cond == FALSE}.  For \code{result}, the easiest way to set the available choices is to use \code{`\%::\%`}; otherwise, a environment(-like) object with members named 'true' and 'false' must be provided
#'
#' @param cond (logical) A \emph{vector} that evaluates to \code{TRUE} or \code{FALSE}
#' @param result (tensor) Resultant values for TRUE and FALSE conditionals
#'
#' @return A \code{\link[data.table]{data.table}} object comprised of values occupying the 'then' and 'else' slots in the 'if-then-else' logical test
#'
#' @aliases `%?%`
#'
#' @family Custom operators
#'
#' @export
#'

	as.data.table(purrr::imap_dfr(cond + 1, ~c(result = rlang::new_box(result[[.x]]), cond_id = .y)))
}
#
`%??%` <- function(cond, result) {
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

	if (is.environment(result)){ result <<- as.data.table(mget(c("true", "false"), envir = result)) }
	if (!is.data.table(result)){ result <<- as.data.table(result) }

	foreach(x = cond, y = iapply(X = result, MARGIN = 1), id = sequence(length(cond)), .combine = rbind) %do% {  `%?%`(x, y) }
}
#
`%all%` <- function(i, ..., logical.out = FALSE, chatty = FALSE){
#' Retrieve all elements with the given object names
#'
#' \code{src.events} Is intended to operate on a named object with duplicate names.  The idea is to allow for "soft" accessing by name
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

  nms = unique(substitute(c(...))) %>% { if (...length() == 1){ . } else { .[-1L] } } %>% book.of.utilities::enlist();

  purrr::map(nms, ~{ if (logical.out){ .x == names(i) } else { i[.x == names(i)] }} %>% sapply(rlang::new_box)) %>% purrr::flatten()
}
#
# :: Other Utilities ====
#
mark.time <- function(msg = "", reset = TRUE, lap = FALSE, end = FALSE, history = FALSE, ...) {
#' Time Stamp Utility
#'
#' \code{mark.time} generates timestamps during execution workflow
#'
#' Assign this function to a variable as a delegate and invoke the variable as a function.  By default, each call to the function will toggle the message returned indicating the beginning or end of execution along with the time of invocation.  See \href{http://adv-r.had.co.nz/Functional-programming.html#closures}{Advanced R by Hadley Wickham: Closures} for additional context. All parameters are independent; therefore, the logical `AND` should be assumed
#'
#' @param msg (string) The message to print along side the timestamp; defaults to \code{""}
#' @param reset (logical | TRUE) \code{TRUE} resets the closure and replaces the stopwatch
#' @param lap (logical | FALSE) \code{TRUE} preserves the closure and appends the stopwatch
#' @param end (logical | FALSE) \code{TRUE} resets closure and appends the stopwatch.
#' @param history (logical | FALSE) \code{TRUE} shows the current contents of the stopwatch and then exits without taking additional action
#' @param ... Not used.
#'
#' @return A copy of the library function
#'
# @export

	if (history){ return(globalenv()$.stopwatch) }
  tf <- TRUE;

  function(msg = "", reset = TRUE, lap = FALSE, end = FALSE, ...){
    {
      ops = list(...)

      sub.quote <- quote({ if (is.null(msg)| msg == ""){""} else {paste(": ", msg, collapse = "")} })

      if (end) {
        # %>% End the routine without replacing the closure
        reset = FALSE

        lap = TRUE
      } else if (reset | end) {
        # %>% Replace the closure
        tf <<- TRUE
      } else {
        # %>% Append the closure and toggle its state
        tf <<- ifelse(lap, tf, !tf)
      }

      if (reset | !exists(".stopwatch", envir = globalenv())) { .GlobalEnv$stopwatch <- new("list") }

      output = ifelse(
        lap & !end
        , sprintf("[%s] Continuing execution%s", Sys.time(), sub.quote %>% eval())
        , switch(
            ifelse(end, 2, 1 + (tf * 1))
            , "1" = sprintf("[%s] Beginning execution%s", Sys.time(), sub.quote %>% eval())
            , "2" = sprintf("[%s] Completed execution%s", Sys.time(), sub.quote %>% eval())
          	)
        );
    } %>% { stopwatch <<- append(get(".stopwatch", envir = globalenv()), print(.))}
  }
}
#
log_note <- function(..., file = "", show = FALSE, append = TRUE){
#' Create an Entry into a Development Log
#'
#' \code{log_note} serves as a wrapper for \code{\link[base]{cat}} and provides an easy way to write to a plain-text file
#' Even though the original use-case is the real-time capturing of developer notes, it can be used for incremental writing to a file when needed.
#' @param ... (string[]) The string(s) of text to write to the file.  Multiple values are collapsed into a single string separated by a newline. Expressions are allowed and will have the captured output included.
#' @param file (string|"") The name of the file to (over)write.  If no file is given, the output is sent to the console.
#' @param show (logical | \code{FALSE}) When \code{TRUE}, print the contents of \code{file} (requires library \code{readtext}).
#' @param append (logical | \code{TRUE}) When \code{TRUE} new content is appended.
#'
#' @return See parameter \code{show}
#' @export
#'
	tstamp = sprintf("[%s, %s]\n", Sys.time(), Sys.getenv("USERNAME"));

	if (...length() > 0){
		out.txt = paste0(
			"\n"
			, tstamp
			, purrr::map_chr(
					unname(rlang::exprs(...))
					, ~ if (!is.character(.x)){ capture.output(eval(.x)) %>% paste(collapse = "\n") } else { .x }
				) %>% paste(collapse = "\n")
		);

		cat(out.txt, file = file, append = append & !(file == ""));
	}

	if (show & (!file == "")){ readtext::readtext(file)$text %>% message() }
}
#
vlogical <- function(vector, vpattern, test, simplify_with = NULL, ...){
#' Vectorized Logical Tests
#'
#' \code{vlike} is a vectorized version of \code{\link[data.table]{like}} allowing a pattern vector to be supplied
#'
#' @param vector A vector or dimensional object to be processed (e.g., matrix, data.frame, etc.)
#' @param vpattern A vector of patterns to be matched
#' @param test (function) The function to use for logical testing: the function should be appropriate for the values of \code{vpattern} and have a named argument of \code{vector}, an additional argument to accept the pattern fed atomically, and '...' (even if not used).
#' @param simplify_with (function) When provided, the function operates over rows, thereby simplifying the result
#' @param ... Additional arguments to be sent to the function held by argument \code{test}
#'
#' @return A logical matrix, with rows of the same length as \code{vector} and columns the length of \code{vpattern} TRUE for items that match pattern. If vector is dimensional, multiple \code{TRUE} values may be found for each row.
#'
#' @export

	if (missing(test)){ test <- data.table::like }
	if (is.character(test)){ test <- eval(as.symbol(test), envir = globalenv()) }
	if (!is.function(test)){ message("Argument 'test' is not a function: using default (data.table::like)"); test <- data.table::like; }

	sub_fn = function(v, ...){ sapply(vpattern, purrr::as_mapper(~test(v, .x, ...))) }

	if (class(vector) == "list"){ vector <- unlist(vector); }

	.out = if (any(c("matrix", "array", "data.table", "data.frame", "tibble") %in% class(vector))){ t(apply(vector, 1, sub_fn, ...)) } else { sub_fn(vector, ...)}

	if (!rlang::is_empty(simplify_with)){ apply(.out, 1, simplify_with) } else { provideDimnames(.out, base = list(NULL, c(vpattern))) }
}
#
as.regex <- function(...){
#' Tag a Regular-Expression String
#'
#' \code{as.regex} adds class "regex" to a strings passed to \code{...}
#'
#' @param ... The string to set "as regex"
#'
#' @return The string appended with attribute "regex" set to \code{TRUE}
#'
#' @export

	purrr::map(rlang::exprs(...), ~setattr(eval(.x), name = "regex", value = TRUE));
}
#
is.regex <- function(i){
#' Test for Class 'regex'
#'
#' \code{is.regex} returns \code{TRUE} when the value of \code{i} contains class "regex"
#'
#' @param i (string[]) A string or string vector
#'
#' @export

	purrr::map(i, ~ifelse(is.null(attr(.x, "regex")), FALSE, attr(.x, "regex"))) %>% unlist();
}
#
unregex <- function(i, x){
#' Convert REGEX Pattern to Object Names
#'
#' @param i (string[]) A string or string vector.  Pattern matching is executed if the string is of class regex" (see \code{as.regex}, \code{is.regex})
#' @param x (object) The names to search OR an object with column names
#'
#' @return Matching values in \code{x} based on values of \code{i}
#'
#' @export
	x = if (any(class(x) %in% c("data.table", "data.frame", "matrix", "tibble"))){ names(x) } else { x }

	reg.needle = i[is.regex(i)] %>% unlist();

	no_reg.needle = i[!is.regex(i)] %>% unlist();

	reg.match = if (!is_empty(reg.needle)){ x[x %like% paste(reg.needle, collapse = "|")] }

	no_reg.match = if (!is_empty(no_reg.needle)){ intersect(x, no_reg.needle) }

	c(if (!any(is_empty(reg.match))) reg.match, if (!any(is_empty(no_reg.match))) no_reg.match);
}
#
polyname2orig <- function(poly.names, orig.names, degree, ...){
#' Polynomial Names to Original
#'
#' \code{polyname2orig} renames the column output of the call to \code{\link[stats]{poly}} by replacing each ordinal slot with the original name
#'
#' @param i An object created by the call to \code{\link[stats]{poly}}
#' @param poly.names (string[]) The variable names after polynomial conversion
#' @param orig.names (string[]) The variable names before polynomial conversion
#' @param degree (int) The degree used in polynomial conversion
#' @param ... Arguments to send to \code{\link[base]{paste}}, namely \code{sep} (default "_") and \code{collapse} (default ("_x_"))
#'
#' @return The polynomial names vector with positions replaced with original name elements
#'
#' @export

	.paste.args = list(
			sep = ifelse("sep" %in% ...names(), list(...)$sep,  "_")
			, collapse = ifelse("collapse" %in% ...names(), list(...)$collapse, "_x_")
			);

	.new.names = stri_split_fixed(poly.names, ".", simplify = TRUE) %>%
    rbind() %>%
    apply(1, function(j){
        paste(orig.names[j != "0"], j[j != "0"], sep = .paste.args$sep, collapse = .paste.args$collapse)
      });

  return(.new.names);
}
#
