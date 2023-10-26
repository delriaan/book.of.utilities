log_note <- function(..., file = "", show = FALSE, append = TRUE){
#' Create an Entry into a Development Log
#'
#' \code{log_note} serves as a wrapper for \code{\link[base]{cat}} and provides an easy way to write to a plain-text file
#' Even though the original use-case is the real-time capturing of developer notes, it can be used for incremental writing to a file when needed.
#' @param ... (\code{\link[rlang]{dots_list}}): The string(s) of text to write to the file.  Multiple values are collapsed into a single string separated by a newline. Expressions are allowed and will have the captured output included.
#' @param file (string|"") The name of the file to (over)write.  If no file is given, the output is sent to the console.
#' @param show (logical | \code{FALSE}) When \code{TRUE}, print the contents of \code{file} (requires library \code{readtext}).
#' @param append (logical | \code{TRUE}) When \code{TRUE} new content is appended.
#'
#' @return See parameter \code{show}
#'
#' @family Miscellaneous Functions
#' @export

	tstamp = sprintf("[%s]\n", Sys.time());

	if (...length() > 0){
		out.txt = paste0(
			"\n"
			, tstamp
			, purrr::map_chr(
					unname(rlang::exprs(...))
					, \(x) if (!is.character(x)){ capture.output(eval(x)) |> paste(collapse = "\n") } else { x }
				) |> paste(collapse = "\n")
		);

		cat(paste0(out.txt, "\n"), file = file, append = append & !(file == ""));
	}

	if (show & (!file == "")){ readtext::readtext(file)$text |> message() }
}
#
vlogical <- function(vector, vpattern, test = stringi::stri_detect_regex, simplify_with = NULL, ...){
#' Vectorized Logical Tests
#'
#' \code{vlike} is a vectorized version of \code{\link[data.table]{like}} allowing a pattern vector to be supplied
#'
#' @param vector A vector or dimensional object to be processed (e.g., matrix, data.frame, etc.)
#' @param vpattern A named vector of patterns to be matched
#' @param test (function) The function to use for logical testing: the function should be appropriate for the values of \code{vpattern} accepting \code{vector} as the first argument, the pattern as the second, and '...' as the third (even if not used).
#' @param simplify_with (function) A function to column-wise simplify the logical matrix when not empty
#' @param ... Additional arguments to be sent to the function held by argument \code{test}
#'
#' @return A logical matrix, with rows of the same length as \code{vector} and columns the length of \code{vpattern} when \code{simplify_with} is not provided.  Otherwise, the result of applying \code{simplify_with} on each column.
#'
#' @family Miscellaneous Functions
#'
#' @examples
#' vlogical(
#' 	vector = letters
#' 	, vpattern = c(sample(LETTERS, 5), sample(letters, 5))
#' 	, simplify_with = mean
#' 	, ignore.case = TRUE
#' 	)
#'
#' @export

	.default_test <- stringi::stri_detect_regex

	if (!is.function(test)){ test <- .default_test }

	.nms <- which(rlang::names2(vpattern) == "")

	if (!rlang::is_empty(.nms)){ names(vpattern)[.nms] <- paste0("x_", .nms) }

	.out <- outer(vector, vpattern, test)

	if (!rlang::is_empty(simplify_with)){ apply(.out, 2, simplify_with) |> rlang::set_names(names(vpattern)) } else { .out }
}
#
as.regex <- function(...){
#' Tag a Regular-Expression String
#'
#' \code{as.regex()} adds class "regex" to a strings passed to \code{...}
#'
#' @param ... (\code{\link[rlang]{dots_list}}): The string to set "as regex"
#'
#' @return The string appended with attribute "regex" set to \code{TRUE}
#' @family Miscellaneous Functions
#' @export

	purrr::map(rlang::exprs(...), ~data.table::setattr(eval(.x), name = "regex", value = TRUE));
}
#
is.regex <- function(i){
#' Test for Class 'regex'
#'
#' @param i (string[]) A string or string vector
#'
#' @return Returns \code{TRUE} when the value of \code{i} contains class "regex"
#' @family Miscellaneous Functions
#' @export

	purrr::map(i, ~ifelse(is.null(attr(.x, "regex")), FALSE, attr(.x, "regex"))) |> unlist();
}
#
unregex <- function(i, x){
#' Convert REGEX Pattern to Object Names
#'
#' @param i (string[]) A string or string vector.  Pattern matching is executed if the string is of class regex" (see \code{as.regex}, \code{is.regex})
#' @param x (object) The names to search OR an object with column names
#'
#' @return Matching values in \code{x} based on values of \code{i}
#' @family Miscellaneous Functions
#' @export
	x = if (any(class(x) %in% c("data.table", "data.frame", "tibble"))){ names(x) } else { x }

	reg.needle = i[is.regex(i)] |> unlist();

	no_reg.needle = i[!is.regex(i)] |> unlist();

	reg.match = if (!rlang::is_empty(reg.needle)){ x[grepl(paste(reg.needle, collapse = "|"), x)] }

	no_reg.match = if (!rlang::is_empty(no_reg.needle)){ intersect(x, no_reg.needle) }

	c(if (!any(rlang::is_empty(reg.match))) reg.match, if (!any(rlang::is_empty(no_reg.match))) no_reg.match);
}
#
polyname2orig <- function(poly.names, orig.names, degree, ...){
#' Polynomial Names to Original
#'
#' \code{polyname2orig} renames the column output of the call to \code{\link[stats]{poly}} by replacing each ordinal slot with the original name
#'
#' @param poly.names (string[]) The variable names after polynomial conversion
#' @param orig.names (string[]) The variable names before polynomial conversion
#' @param degree (int) The degree used in polynomial conversion
#' @param ... Arguments to send to \code{\link[base]{paste}}, namely \code{sep} (default "_") and \code{collapse} (default ("_x_"))
#'
#' @return The polynomial names vector with positions replaced with original name elements
#' @family Miscellaneous Functions
#' @export

	.paste.args = list(
			sep = ifelse("sep" %in% ...names(), list(...)$sep,  "_")
			, collapse = ifelse("collapse" %in% ...names(), list(...)$collapse, "_x_")
			);

	.new.names = stringi::stri_split_fixed(poly.names, ".", simplify = TRUE) |>
    rbind() |>
		apply(MARGIN = 1, FUN = function(j){ paste(orig.names[j != "0"], j[j != "0"], sep = .paste.args$sep, collapse = .paste.args$collapse) });

  return(.new.names);
}
#
gen.primes <- function(n = 1, domain = 2:n, random = FALSE, distinct = TRUE, chatty = FALSE){
#' Prime Number Generator
#'
#' \code{get.primes} generates \code{n} prime numbers..
#' @param n (integer) The number of prime numbers to generate
#' @param domain (integer[]) The range of that prime numbers should fall into given as a length two (2) integer vector
#' @param random (logical) When \code{TRUE}, the internally-generated vector is subsetted via sampling
#' @param distinct (logical) When \code{TRUE}, only unique (and sorted) values are returned
#' @param chatty (logical) When \code{TRUE}, various output is printed to console, mainly for debugging purposes
#'
#' @return A vector of prime numbers
#' @family Miscellaneous Functions
#' @export

	# Initialize
	set.seed(Sys.time(), kind = sample(c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper", "Knuth-TAOCP-2002", "L'Ecuyer-CMRG"), 1));
	x = min(domain, na.rm = TRUE):max(domain, na.rm = TRUE);
	z = 1:max(domain, na.rm = TRUE);

	logi.vec = rowSums(outer(x, z, `%%`) == 0) == 2;

	output = x[logi.vec];
	output = if (random){ sample(output, n, replace = TRUE) } else { output[1:n] }
	output = if (distinct){ unique(output) } else { output }
	sort(output)
	# gen.primes(n = 100, domain = c(450, 9571), random = FALSE, distinct = TRUE) %>% { data.table::data.table(x = ., d = c(0, diff(.))) } %T>% print %>% plot
}
#
call.recursion <- function(x, fun, test, nxt, max.iter = 1, cur.iter = 0, simplify = TRUE){
#' Execute a Function as Recursively
#'
#' \code{call.recursion} Executes a recursive calls to \code{fun} based on the output of \code{test} up to \code{max.iter} times
#'
#' @param x The input object
#' @param fun A function that operates on \code{x} and produces output
#' @param test A single-argument function returning a single Boolean: \code{FALSE} stops iteration
#' @param nxt A function that operates on the current output of \code{fun(x)} to send to the \emph{next} iterative call to \code{fun()}
#' @param max.iter (integer) The maximum number of iterations
#' @param cur.iter (integer) The current iteration index
#' @param simplify (logical) Should only the last value be returned (\code{TRUE}) or intermediate values as well (\code{FALSE})?
#'
#' @importFrom magrittr %<>% %>%
#'
#' @examples
#' book.of.utilities::call.recursion(
#'	x = sample(1000, size = 100)
#'	, fun = \(x){ abs(x - mean(x, na.rm = TRUE)) }
#'	, test = \(x){
#'			if (length(unique(x)) == 1 || is.na(sd(x))){
#'				FALSE
#'			} else {
#'				i <- abs(x - mean(x, na.rm = TRUE))/sd(x);
#'				mean(i > 3) <= 0.7
#'			}
#'		}
#'	, nxt = \(x){ sample(x, size = length(x)- 1, prob = runif(n = length(x), min = 0.1, max = 0.9)) }
#'	, max.iter = 1000
#'	, simplify = !FALSE
#'	)
#'
#' @export
#'
	force(fun);
	force(test);
	force(nxt)

	iterations <- list();

	ans <- fun(x);

	iterations[[1]] <- mget(c("ans", "test"));

	while(test(ans) & (cur.iter < max.iter)){
		cur.iter <- cur.iter + 1;

		x <- nxt(ans);

		ans <- fun(x);

		iterations[[cur.iter]] <- mget(c("cur.iter", "x", "ans")) |> purrr::modify_at(c("x", "ans"), list);
	}

	if (!simplify){
		purrr::map(iterations, \(x) x$ans)
	} else {
		iterations[[length(iterations)]][["ans"]]
	}
}
#
checksum <- function(object, hash, ...){
	#' Checksum Validation
	#'
	#' \code{checksum} provides a wrapper to \code{\link[digest]{digest}} providing the hash to use for comparison
	#'
	#' @param object,... See \code{\link[digest]{digest}}
	#' @param hash (string) The hash to compare
	#'
	#' @return A logical scalar
	#' @family Miscellaneous Functions
	#' @export
	msg_list <- list(object = "Enter the path of the file to check: ", hash = "Enter the hash to compare");

	if (missing(object)){
		object <- if (interactive()){ tcltk::tk_choose.files(msg_list$object) } else { readline(msg_list$object) }
	}

	if (missing(hash)){
		hash <- if (interactive()){ tcltk::tk_choose.files(msg_list$hash) } else { readline(msg_list$hash) }
	}

	arg_list <- rlang::list2(...)

	if (is.character(object)){
		arg_list$file <- object
	} else {
		arg_list$object <- object
	}

	check_result <- do.call(digest::digest, args = arg_list)

	identical(check_result, hash)
}
#
