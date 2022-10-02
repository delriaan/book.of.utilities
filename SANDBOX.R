library(magrittr); library(purrr)
outer(1:20, 1:20, `%%`) %>% apply(1, function(i){ 2 %in% i})

sapply(
	stringi::stri_split_regex(
		"jsonlite
		data.table,
		magrittr,
		stringi,
		purrr,
		furrr,
		doParallel,
		doFuture,
		stringdist", "[, ]|\n|\t"
		, omit_empty = TRUE
		, simplify = TRUE
		)
	, library, character.only = TRUE, quietly = TRUE)

