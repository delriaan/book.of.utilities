distinct.list <- function(i, logical.only = FALSE){
#' Unique List Selection
#'
#' Return the distinct set of first-level list elements based on their respective base64-encoded signatures.  This is an inefficient implementation used for nested lists
#'
#' @param i (object) The input list object (or coercible)
#' @param logical.only (logical) When \code{TRUE}, a logical vector is returned
#'
#' @return The list with unique, first-level child elements
#' @family List management
#'
#' @family Object Management
#'
# @export

	i = if (any(class(i) %in% c("data.table", "data.frame", "tibble"))){ as.list(i) } else { i }

	x <- slider::slide(.x = i, .step = 1, .f = purrr::as_mapper(~sodium::hash(serialize(object = .x, connection = NULL), size = 24))) |> duplicated() |> magrittr::not()
	if (logical.only){ x } else { i[x] }
}
#
enlist <- function(x, ...){
#' Create a Named List
#'
#' \code{enlist} creates a list from an atomic vector.  When names having the length of the input are provided to \code{...}, a list with members named accordingly is produced.  Otherwise, the members are named by their values (eg. \code{list(this="this", that="that")} ): this is the default behavior.
#'
#' @param x A vector of values
#' @param ... (\code{\link[rlang]{dots_list}}):: Names to use for the list members: must be the same length as \code{x}.  Can be a vector or an atomic listing.
#'
#' @return A list, the names of which being the values of \code{x}
#'
#' @family Object Management
#'
#' @examples
#' # :: Test Objects
#' test_x = list(
#' 	test_1 = c(1:5)
#' 	, test_2 = list(c(1:5), 3, 4, c(letters[1:5]))
#' );
#' nms = c("up", "down", "left", "right", "center");
#'
#' # :: Unnamed Vector -> Elements become names
#' enlist(test_x$test_1);
#' enlist(test_x$test_2);
#'
#' # :: Unnamed Vector & Provided Names (Full) -> All elements have names in 'nms'
#' enlist(test_x$test_1, nms);
#' enlist(test_x$test_2, !!!nms);
#' enlist(test_x$test_2, up, down, left, right, center);
#'
#' # %>% Unnamed Vector & Provided Names (Partial) -> First two names are 'nms[1:2]' with the balance as default
#' enlist(test_x$test_1, !!!nms[1:2]);
#' enlist(test_x$test_2, !!!nms[1:2])
#'
#' @export
#'

	idx = 1:length(x);
	old.names = purrr::map(x, names);
	new.names = as.character(rlang::enexprs(...))[idx];
	.out = purrr::map(idx, ~{
			ifelse(is.null(old.names[[.x]]), ifelse(is.na(new.names[.x]), x[[.x]], new.names[.x]), old.names[[.x]])
		}) |> unlist()

	names(x) <- ifelse(is.na(.out), unlist(x), .out);
	as.list(x)
}
#
scrub.data <- function(input, condFn = is.na, replacement, ...) {
#' Scrub and Replace Values
#'
#' \code{scrub.data} replaces values that meet the condition given by the function passed to \code{condFn}
#'
#' @param input An object that contains values to scrub
#' @param condFn A function name or function expression that serves as the test for values to scrub
#' @param replacement The replacement value
#' @param ... (Not used)
#'
#' @family Object Management
#'
#' @export

  has.dimensions <-  any(class(input) %ilike% "matrix|data|array|tibb|tabl");
  class(replacement) <- class(input);

  .index <- which(condFn(input), arr.ind = has.dimensions);

  input[.index] <- replacement[ifelse(length(replacement) == 1, 1, .index)];
  input;
}
#
get.object_sizes <- function(i = rlang::caller_env(), nm = as.character(substitute(i)), depth = 0L, max.depth = 2L, chatty = FALSE, ...){
#' Recursive Object Size Retrieval
#'
#' \code{get.object_sizes} recursively steps into child environments, retrieving object sizes along the way
#'
#' @param i (object) The top-level object
#' @param nm (string) The object name for the current iteration
#' @param depth (integer) The current iteration depth
#' @param max.depth (integer) The maximum recursion depth
#' @param chatty (logical) Execution messages
#' @param ... Additional arguments passed to \code{\link[base]{ls}}
#'
#' @return A vector of object sizes
#' @family Object Management
#'
#' @export
  if (class(i)[1] == "environment"){
    purrr::imap(mget(ls(i, ...), envir = i), ~{
      if (chatty){ message(sprintf("Stepping into %s", .y)) }
      get.object_sizes(
        i = .x
        , nm = paste(nm, .y, sep = "$")
        , depth = depth + 1
        , max.depth = max.depth
        );
    }) |>
    purrr::reduce(rbind) |>
    data.table::setattr("timestamp", Sys.time())
  } else {
    .out <- list(
    	obj_path = stringi::stri_replace_first_regex(nm, "[.]GlobalEnv[$]", "")
      , depth = depth
    	, size = object.size(i)
    	);
    units <- which(
    					c(b = 0, Kb = 1, Mb = 2, Gb = 3) <=
    					log(.out$size, base = 1024)
    					) |> names() |>  data.table::last()
    .out$size_desc <- format(.out$size, units = units);
    .out;
  }
}
#
