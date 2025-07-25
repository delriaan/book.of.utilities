gen_pass <- function(exclude = "", out_len = 0L, raw = FALSE, chatty = FALSE){
	#' Generate a Password
	#'
	#' \code{gen_pass} creates a password consisting of symbols and alphanumeric values.
	#'
	#' @param exclude Character-coercibles to exclude from output
	#' @param out_len (integer) The length of the password in character format
	#' @param raw (logical) Should the output be returned as raw?
	#' @param chatty (logical) Should diagnostic information be provided?
	#'
	#' @note The generated string \emph{always} begins with a letter before being returned as-is or as a raw vector
	#'
	#' @family Authentication Functions
	#'
	#' @export

	# exclude = "_#&"; out_len = 20; raw = FALSE; chatty = FALSE
	#
	set.seed(Sys.time());
	exclude <- strsplit(exclude, "")[[1]] |> unique()

	symbols <- list(
		glyphs = strsplit("!@#$%^&*()_+;", split = "")[[1]]
		, ucase = LETTERS
		, lcase = letters
		, number = 0:9
		)

	haystack <- unlist(symbols, use.names = FALSE)

	symbol_wgts <- replicate(length(symbols), 1/length(symbols)) |>
		rlang::set_names(names(symbols))

	dims <-  c(sum(sapply(symbols, length)), out_len)
	symbol_arr <- out_arr <- array(rep.int(FALSE, prod(dims)), dim = dims)

	spsUtil::quiet({
		seq_along(symbols) |>
			lapply(\(i){
				needle <- symbols[[i]]
				n <- length(needle)

				k <- replicate(
					n = out_len
					, rbinom(
							n = 1
							, size = n
							, prob = mean(runif(n))
							)
					)

				browser()
				out_arr[match(sample(needle, 1), haystack, nomatch = 0), ] <<- TRUE
			})
	})

	out_arr <- diag(1, nrow = length(haystack)) |>
		magrittr::set_attr("dimnames", list(haystack, haystack)) %*% symbol_arr

		out_arr |>
		(\(m){
			browser()
			m <- matrixStats::rowCumprods(m)
			m <- matrixStats::rowCumsums(m)
			m <- exp(m)/sum(exp(m))
			m <- m %*% diag(1/matrixStats::colMaxs(m), ncol = ncol(m))

			apply(m, 2, \(x) haystack[which.max(x)])
		})()



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

	if (chatty){
		cli::cli_alert_info(glue::glue(
			"\nPassword generated with replication \ntries: {.iter}\nalpha_ratio:{.alpha_ratio}"))
		}

	.out <- paste(c(sample(c(letters,LETTERS), 1), .out), collapse = "");

	if (raw){ charToRaw(.out) } else { .out }
}

keyring_export <- function(keyring = NULL, as.raw = FALSE){
	#' Export keyring Entries
	#'
	#' \code{keyring_export} creates JSON output for available \code{\link[keyring]{keyring}}s
	#'
	#' @param keyring (string[]) The name(s) of keyrings to export (defaults to all named keyrings when calling \code{\link[keyring]{keyring_list}})
	#' @param as.raw (logical | FALSE) Should each entry be cast as a raw vector?
	#'
	#' @return Keyring entries as JSON or raw-encoded JSON
	#'
	#' @family Authentication Functions
	#'
	#' @export

	kr_idx <- if (rlang::is_empty(keyring)){
		which(keyring::keyring_list()$keyring != "")
	} else {
		which(keyring::keyring_list()$keyring %in% keyring) %||%
			which(keyring::keyring_list()$keyring != "")
	}

	keyring::keyring_list()[kr_idx, ] |>
		purrr::modify_at(3, as.logical) |>
		purrr::modify_at(2, as.integer) |>
		purrr::pmap(~{
			kr <- ..1;

			if (..3){ keyring::keyring_unlock(keyring = ..1) }

			f <- \(x){
				rlang::inject(
					c(!!!x, value = keyring::key_get(!!!purrr::discard(x, \(i) i == ""), keyring = kr))
				)}

			.out <- rlang::list2(
				!!kr := keyring::key_list(keyring = kr) |>
					apply(1, f, simplify = TRUE) |> t() |>
					as.data.frame() |>
					jsonlite::toJSON("columns")
			)

			if (as.raw){ purrr::modify_at(.out, kr, charToRaw) } else { .out }
		}) |>
		purrr::flatten()
}

keyring_import <- function(kr_data, kr_name = names(kr_data), kr_pass = NULL, dry.run = FALSE){
	#' Import keyring Entries
	#'
	#' \code{keyring_import} registers exported \code{\link[keyring]{keyring}}s (see \code{\link{keyring_export}})
	#'
	#' @param kr_data The named list of exported keyring data: each name should be the name of an existing keyring.
	#' @param kr_name (string) The names of the target keyring(s) to populate: defaults to the names of \code{kr_data}
	#' @param kr_pass (string) The password to unlock existing keyrings:\cr
	#' \itemize{
	#' \item{A named list indicating the per-keyring mapping}
	#' \item{A single password (used for all supplied keyrings)}
	#' \item{An unnamed list the same length as \code{kr_name}}
	#' }
	#'
	#' @param dry.run (logical|FALSE) When \code{TRUE}, the expression that would be evaluated is returned (passwords are redacted)
	#'
	#' @return A list of logical vectors indicating success or failure of import for each keyring entry
	#'
	#' @family Authentication Functions
	#'
	#' @export

	if (missing(kr_data)){ stop("No keyring data provided") }

	kr_data <- purrr::imap(kr_data, \(x, k){
			if (is.raw(x)){ x <- rawToChar(x) }
			jsonlite::fromJSON(x) |>
			as.data.frame() |>
			apply(1, \(i) purrr::list_assign(as.list(i), keyring = k))
		});

	# `create()` and `update()` only return unevlauated expressions:
	kr_update <- \(keyring, password, kr_args){
		# browser()
		rlang::expr({
			if (keyring::keyring_is_locked(keyring = !!keyring)){
				keyring::keyring_unlock(keyring = !!keyring, password = !!password);
			}
			do.call(keyring::key_set_with_value, args = !!kr_args)
		});
	}

	kr_create <- \(keyring, password, kr_args){
		# browser()
		rlang::expr({
			keyring::keyring_create(keyring = !!keyring, password = !!password);
			kr_update(!!keyring, !!password, !!kr_args);
		});
	}

	# Name recycling check:
	if (rlang::is_empty(kr_pass) || rlang::has_length(kr_pass, 1L)){
		kr_pass <- replicate(length(kr_name), kr_pass) |> rlang::set_names(kr_name);
	} else if (!rlang::is_empty(names(kr_pass))){
		# Check for missing names in `kr_pass`. If found, replace with names from `kr_data`:
		idx <- which(names(kr_pass) == "");

		if (!rlang::is_empty(idx)){
			names(kr_pass)[idx] <- kr_name[idx];
		}

		# Check for names in `kr_pass` that are not in `kr_data` and remove them:
		idx <- which(kr_name %in% names(kr_pass));

		if (rlang::is_empty(idx)){
			stop(glue::glue("Invalid password mapping for `kr_pass` (provided names {names(kr_pass) |> paste(collapse = \", \")})"));
		} else {
			kr_pass <- kr_pass[idx] |> rlang::set_names(kr_name[idx]);
		}
	} else {
		stop(glue::glue("Invalid password mapping for `kr_pass` (provided names {names(kr_pass) |> paste(collapse = \", \")})"));
	}

	# `kr_action` holds a named function list:
	kr_action <- c("kr_create", "kr_update")[1 + (kr_name %in% keyring::keyring_list()$keyring)] |>
		rlang::set_names(kr_name) |>
		rlang::syms() |>
		lapply(eval, envir = environment());

	purrr::imap(kr_action, \(.action, .keyring){
		purrr::map_lgl(kr_data[[.keyring]], \(this){
			this <- rlang::set_names(this, c("service", "username", "password", "keyring"));
			expr <- rlang::expr(do.call(.action, args = rlang::list2(keyring = .keyring, password = kr_pass[[.keyring]], kr_args = this)));

			if (dry.run){
				message(.keyring);
				# browser()
				expr$args$password <- this$password <- "...";
				print(eval(expr));
				TRUE
			} else {
				tryCatch({
					# browser()
					expr |> eval() |> eval();
					TRUE
				}, error = \(e) FALSE);
			}
		});
	})
}

#' Store an encrypted \code{keyring} key
#'
#' The \code{kr_key} class
#'
#' @slot service See \code{\link[keyring]{key_get}}
#' @slot username See \code{\link[keyring]{key_get}}
#' @slot keyring See \code{\link[keyring]{key_get}}
#' @slot key,password An anonymous function to retrieve and return the value of the current \code{keyring} key.
#' @slot update An anonymous function to set a new value for the \code{keyring} key referenced by the object.
#'
#' @rdname kr_key
#' @name kr_key
#'
#' @family Authentication Functions
#'
#' @export
# Set the structure for the class properties in a separte, unexported list
NULL

kr_key <- local({
	s7_properties <- list(
		service = S7::new_property(class = S7::class_character, name = "service", default = NULL)
		, username = S7::new_property(class = S7::class_character, name = "username", default = NULL)
		, keyring = S7::new_property(class = S7::class_character, name = "keyring")
		, key = S7::new_property(
				class = S7::class_function
				, name = "key"
				, getter = function(self){
						function(){
              invisible(keyring::key_get(service = self@service, username = self@username, keyring = self@keyring))
            }
					}
        )
    , password = S7::new_property(
        class = S7::class_function
        , getter = function(self){ function() self@key() }
        )
    , update = S7::new_property(
        class = S7::class_function
        , getter = function(self){
            function(value){
              keyring::key_set_with_value(
                  service = self@service
                  , username = self@username
                  , keyring = self@keyring
                  , password = value
                  )
              }
					}
        , default = new("function")
				)
		)

	S7::new_class(name = "kr_key", properties = s7_properties, package = "book.of.utilities")
})
