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
	#' @family Authentication Functions
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

keyring_export <- function(keyring = NULL, as.raw = FALSE){
	#' Export keyring Entries
	#'
	#' \code{keyring_export} creates JSON output for available \code{\link[keyring]{keyring}}s
	#'
	#' @param keyring (string[]) The name(s) of keyrings to export (defaults to all named keyrings when calling \code{\link[keyring]{keyring_list}})
	#' @param as.raw (logical | FALSE) Should each entry be cast as a raw vector?
	#'
	#' @return Keyring entries as JSON or raw-encoded JSON
	#' @family Authentication Functions
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

keyring_import <- function(data, kr_name = rlang::as_label(rlang::enexpr(data)), dry.run = FALSE){
	#' Import keyring Entries
	#'
	#' \code{keyring_import} registers exported \code{\link[keyring]{keyring}}s (see \code{\link{keyring_export}})
	#'
	#' @param data The named list of exported keyring data
	#' @param kr_name (string) The name of the target keyring to populate: defaults to the deparsed value of argument \code{data}
	#' @param dry.run (logical|FALSE) When \code{TRUE}, the expression that would be evaluated is returned (passwords are redacted)
	#'
	#' @return A logical scalar for each imported key
	#' @family keyring Utilities
	#' @export

	if (missing(data)){ stop("No keyring data provided") }

	if (is.raw(data)){ data <- rawToChar(data) }

	kr_args <- jsonlite::fromJSON(data)

	purrr::pmap_lgl(kr_args, \(...){
		.action <- { rlang::expr(
			keyring::key_set_with_value(
				!!!rlang::list2(keyring = kr_name, ...) |>
					purrr::map(\(x) if (x == ""){ NULL } else { x }) |>
					rlang::set_names(c("keyring", "service", "username", "password"))
			)
		)}

		if (dry.run){
			.action$password <- "..."

			rlang::expr_print(.action)

			TRUE
		} else {
			tryCatch({ eval(.action); TRUE }, error = \(e) FALSE)
		}
	})
}
	#' Store an encrypted \code{keyring} key
	#'
	#' The \code{kr_key} class
	#'
	#' @slot service,usernane,keyring See \code{\link[keyring]{key_get}}
	#' @slot key A function to retrieve the \code{keyring} key
	#'
	# @rdname kr_key
	# @name kr_key
	#'
	#' @examples
	#' \dontrun{
	#' x <- kr_key(service = "service", keyring = "this_keyring")
	#' }
	#'
	#' @family Authentication Functions
	#'
	#' @export
kr_key <- {
	# Set the structure for the class properties in a separte, unexported list
	s7_properties <- { list(
		service = S7::new_property(
				class = S7::class_character
				, name = "service"
				, default = NULL
				)
		, username = S7::new_property(
				class = S7::class_character
				, name = "username"
				, default = NULL
				)
		, keyring = S7::new_property(
				class = S7::class_character
				, name = "keyring"
				)
		, key = S7::new_property(
				class = S7::class_function
				, name = "key"
				, getter = function(self){
						function(){ invisible(keyring::key_get(
							service = self@service
							, username = self@username
							, keyring = self@keyring
							))
						}
					}
				, setter = function(self, value){
					self@password <- value;

					keyring::key_set_with_value(
						service = self@service
						, username = self@username
						, keyring = self@keyring
						, password = value
					);

					self
				}
				)
		)
	}

	S7::new_class(name = "kr_key", properties = s7_properties, package = "book.of.utilities")
}
