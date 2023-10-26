dir("pkg/R", pattern = "^[a-z].+R$") %>%
	rlang::set_names(
		stringi::stri_split_fixed(., "_") |>
			sapply(\(i) stringi::stri_trans_totitle(i) |> paste(collapse = " ")) |>
			stringi::stri_replace_last_fixed(".r", "") |>
			sprintf(fmt = "Chapter %2$s - %1$s", seq_along(.))
		) |>
	lapply(\(i){
		readLines(paste0("pkg/R/", i)) |>
			stringi::stri_extract_all_regex(pattern = "(^[a-z_`%].+ (?=(<-)))", omit_no_match = TRUE) |>
			sapply(\(k) stringi::stri_split_fixed(k, "@family ", simplify = TRUE, omit_empty = TRUE) |> as.vector()) |>
			purrr::compact() |>
			unlist() |>
			trimws() |>
			lapply(htmltools::tags$li) |>
			htmltools::tags$ul()
		}) |>
	htmltools::tagList() |>
	htmltools::html_print()
