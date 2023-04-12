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
#' @family Chapter 5 - Miscellaneous Functions
#' @export

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

		cat(paste0(out.txt, "\n"), file = file, append = append & !(file == ""));
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
#' @family Chapter 5 - Miscellaneous Functions
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
#' \code{as.regex()} adds class "regex" to a strings passed to \code{...}
#'
#' @param ... (\code{\link[rlang]{dots_list}}): The string to set "as regex"
#'
#' @return The string appended with attribute "regex" set to \code{TRUE}
#' @family Chapter 5 - Miscellaneous Functions
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
#' @family Chapter 5 - Miscellaneous Functions
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
#' @family Chapter 5 - Miscellaneous Functions
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
#' @family Chapter 5 - Miscellaneous Functions
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
#' @family Chapter 5 - Miscellaneous Functions
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
as.recursive <- function(fun, cond_def, finalize = I, max.iter = 0){
#' Recast a Function as Recursive
#'
#' \code{as.recursive} creates a recursive version of the function passed to \code{fun}.
#'
#' @param fun A function
#' @param cond_def A lambda expression defining the condition for which recursion continues
#' @param finalize A lambda expression or function that finalizes the result
#' @param max.iter (integer) The maximum number of recursive iterations
#'
#' @importFrom magrittr %<>% %>%
#'
#' @export
#'

  function(...){
    out <- list();

    i <- 1;

    . <- fun(...);

    cond <- rlang::eval_tidy(rlang::f_rhs(cond_def));

    out[[i]] <- .;

    while((i < max.iter) & (i > 0) & cond){
      . <- fun(...);

      cond <- rlang::eval_tidy(rlang::f_rhs(cond_def));

      out[[i+1]] <- .;
    }

    if (purrr::is_function(finalize)){
    	finalize(.)
    } else {
    	rlang::eval_tidy(rlang::f_rhs(finalize))
    }
  }
}
#
checksum <- function(object, hash, ...){
#' Checksum Validation
#'
#' \code{checksum} provides a wrapper to \code{\link[digest]{digest}} to make checksum validation easy
#'
#' @param object See \code{\link[digest]{digest}}
#' @param hash (string) The hash to compare
#' @param ... See \code{\link[digest]{digest}}
#'
#' @return A logical scalar
#' @family Chapter 5 - Miscellaneous Functions
#' @export
	msg_list <- list(object = "Enter the path of the file to check: ", hash = "Enter the hash to compare");

	if (missing(object)){
		object <- if (interactive()){ tcltk::tk_choose.files(msg_list$object) } else { readline(msg_list$object) }
	}

	if (missing(hash)){
		hash <- if (interactive()){ tcltk::tk_choose.files(msg_list$hash) } else { readline(msg_list$hash) }
	}

	identical(digest::digest(object = object, ...), hash)
}
#
gen.pass <- function(glyphs = "@$", length = NULL, raw = FALSE, chatty = FALSE){
#' Generate a Password
#'
#' \code{gen.pass} creates a password consisting of alphanumeric glyphs and symbols
#'
#' @param glyphs Character-coercibles to use in the creation of the password: this is combined with the output of \code{\link[sodium]{keygen}}
#' @param length (int) The length of the password in character format
#' @param raw (logical) Should the output be returned as raw?
#' @param chatty (logical) Should diagnostic information be provided?
#'
#' @note The generated string \emph{always} begins with a letter before being returned as-is or as a raw vector
#' @family Chapter 5 - Miscellaneous Functions
#' @export

	set.seed(Sys.time());

	force(glyphs);

	glyphs <- { c(sodium::keygen(), LETTERS, glyphs) |>
			stringi::stri_extract_all_regex(".", simplify = TRUE) |>
			as.vector() |>
			purrr::keep(~.x != "") |>
			table()
	}

	.sample_wgt <- c(.75, 1, .5);

	sample_glyphs <- purrr::as_mapper(~{
		.this <- { ifelse(
			grepl("[0-9A-Z]", names(.x))
			, .x * .sample_wgt[1]
			, ifelse(
				grepl("[a-z]", names(.x))
				, .x * .sample_wgt[2]
				, .x * .sample_wgt[3]
			)) * (3/.x)
		} |>
			ceiling() |>
			purrr::imap(~rep.int(.y, .x)) |>
			unlist(use.names = FALSE);

		sample(
			x = .this
			, size = ifelse(rlang::is_empty(length), length(.this), length)
			, replace = TRUE
			, prob = c(table(.this))[.this]
		) |>
			paste(collapse = "") |>
			stringi::stri_extract_all_regex(pattern = ".", simplify = TRUE) |>
			as.vector();
	});

	.out <- sample_glyphs(glyphs);
	.alpha_r <- sum(.out %in% letters) / length(.out);
	.ALPHA_r <- sum(.out %in% LETTERS) / length(.out);
	.alpha_ratio <- abs(.alpha_r - .ALPHA_r);

	.iter <- 0;

	while((.alpha_ratio > .10) & (.iter < 1000L)){
		set.seed(sample(.Random.seed, 1));

		.out <- sample_glyphs(glyphs);
		.alpha_r <- sum(.out %in% letters) / length(.out);
		.ALPHA_r <- sum(.out %in% LETTERS) / length(.out);
		.ALPHA_r <- sum(.out %in% LETTERS) / length(.out);
		.alpha_ratio <- abs(.alpha_r - .ALPHA_r);
		.iter <- .iter + 1
	}

	if (chatty){ message(glue::glue("\nPassword generated with replication \ntries: {.iter}\nalpha_ratio:{.alpha_ratio}")) }

	.out <- paste(c(sample(c(letters,LETTERS), 1), .out), collapse = "");

	if (raw){ charToRaw(.out) } else { .out }
}
#
keyring_export <- function(keyring = NULL, as.raw = FALSE){
	#' Export keyring Entries
	#'
	#' \code{keyring_export} creates JSON output for available \code{\link[keyring]{keyring}}s
	#'
	#' @param keyring (string[]) The name(s) of keyrings to export (defaults to all named keyrings when calling \code{\link[keyring]{keyring_list}})
	#' @param as.raw (logical | FALSE) Should each entry be cast as a raw vector?
	#'
	#' @return Keyring entries as JSON or raw-encoded JSON
	#' @family keyring Utilities
	#' @export
	kr_idx <- if (rlang::is_empty(keyring)){
		which(keyring::keyring_list()$keyring != "")
	} else {
		which(keyring::keyring_list()$keyring %in% keyring) %||% which(keyring::keyring_list()$keyring != "")
	}

	keyring::keyring_list()[kr_idx, ] |>
		purrr::modify_at(3, as.logical) |>
		purrr::modify_at(2, as.integer) |>
		purrr::pmap(~{
			kr <- ..1;

			if (..3){ keyring::keyring_unlock(keyring = ..1) }

			f <- purrr::as_mapper(~{
				rlang::inject(
					c(!!!.x
						, value = keyring::key_get(!!!purrr::discard(.x, ~.x == ""), keyring = kr)
					)
				)
			})

			.out <- rlang::list2(!!kr := keyring::key_list(keyring = kr) |>
													 	apply(1, f, simplify = TRUE) |> t() |>
													 	as.data.frame() |>
													 	jsonlite::toJSON("columns"))
			if (as.raw){ purrr::modify_at(.out, kr, charToRaw) } else { .out }
		}) |>
		purrr::flatten()
}
#
keyring_import <- function(data, ...){
	#' Import keyring Entries
	#'
	#' \code{keyring_import} registers exported \code{\link[keyring]{keyring}}s (see \code{\link{keyring_export}})
	#'
	#' @param data The named list of exported keyring data
	#' @param ... Additional named keyring entries
	#'
	#' @note Unnamed inputs will not be imported but indicated via console message
	#'
	#' @return Keyring entries as JSON or raw-encoded JSON
	#' @family keyring Utilities
	#' @export

	data <- append(data, rlang::list2(...));
	no.names <- which(names(data) == "")
	if (!identical(integer(), no.names)){
		message(sprintf("Entries at the following positions will not be imported: %s", paste(no.names, collapse = ", ")))
	}

	data <- data[-no.names];

	if (rlang::is_empty(data)){
		message("No action taken (all entries are unnamed): exiting ..."); return()
	}

	purrr::iwalk(data, ~{
		keyring <- .y
		kr_data <- if (is.raw(.x)){ rawToChar(.x) } else { .x }
		jsonlite::fromJSON(kr_data) |>
			purrr::pwalk(~{
				keyring::key_set_with_value(
					service 		= ..1
					, username	= ..2
					, password	= ..3
					, keyring 	= keyring
				)
			})
	})
}
