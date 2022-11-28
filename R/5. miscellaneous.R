mark.time <- function(msg = "", reset = TRUE, lap = FALSE, end = FALSE, history = FALSE, ...){
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
    .out = {
      ops = list(...)

      sub.quote <- quote({ if (is.null(msg)| msg == ""){""} else { paste(": ", msg, collapse = "")} })

      if (end) { #  End the routine without replacing the closure
        reset = FALSE
        lap = TRUE
      } else if (reset | end) { # Replace the closure
        tf <<- TRUE
      } else { # Append the closure and toggle its state
        tf <<- ifelse(lap, tf, !tf)
      }

      if (reset | !exists(".stopwatch", envir = globalenv())) { .GlobalEnv$stopwatch <- new("list") }

      output = { ifelse(
        lap & !end
        , sprintf("[%s] Continuing execution%s", Sys.time(), sub.quote %>% eval())
        , switch(
            ifelse(end, 2, 1 + (tf * 1))
            , "1" = sprintf("[%s] Beginning execution%s", Sys.time(), eval(sub.quote))
            , "2" = sprintf("[%s] Completed execution%s", Sys.time(), eval(sub.quote))
          	)
        )}
    }
    stopwatch <<- append(get(".stopwatch", envir = globalenv()), print(.out))
  }
}
#
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
#' @export
#'
	tstamp = sprintf("[%s]\n", Sys.time());

	if (...length() > 0){
		out.txt = paste0(
			"\n"
			, tstamp
			, purrr::map_chr(
					unname(rlang::exprs(...))
					, ~ if (!is.character(.x)){ capture.output(eval(.x)) |> paste(collapse = "\n") } else { .x }
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
#' `as.regex()` adds class "regex" to a strings passed to \code{...}
#'
#' @param ... (\code{\link[rlang]{dots_list}}): The string to set "as regex"
#'
#' @return The string appended with attribute "regex" set to \code{TRUE}
#'
#' @export

	purrr::map(rlang::exprs(...), ~data.table::setattr(eval(.x), name = "regex", value = TRUE));
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
#'
#' @export
	x = if (any(class(x) %in% c("data.table", "data.frame", "matrix", "tibble"))){ names(x) } else { x }

	reg.needle = i[is.regex(i)] |> unlist();

	no_reg.needle = i[!is.regex(i)] |> unlist();

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
	x = min(domain, na.rm = TRUE):max(domain, na.rm = TRUE);
	z = 1:max(domain, na.rm = TRUE);

	logi.vec = rowSums(outer(x, z, `%%`) == 0) == 2;

	output = x[logi.vec];
	output = if (random){ sample(output, n, replace = TRUE) } else { output[1:n] }
	output = if (distinct){ unique(output) } else { output }
	sort(output)
	# gen.primes(n = 100, domain = c(450, 9571), random = FALSE, distinct = TRUE) %>% { data.table::data.table(x = ., d = c(0, diff(.))) } %T>% print %>% plot
}
