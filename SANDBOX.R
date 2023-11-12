dir("../resources/R", pattern = "list", full.names = TRUE) |> source();

dir("pkg/R", pattern = "^[a-z].+R$") %>%
	rlang::set_names(
		stringi::stri_split_fixed(., "_") |>
			sapply(\(i) stringi::stri_trans_totitle(i) |> paste(collapse = " ")) |>
			stringi::stri_replace_last_fixed(".r", "") |>
			sprintf(fmt = "Chapter %2$s - %1$s", seq_along(.))
		) |>
	purrr::map(\(i){
		readLines(paste0("pkg/R/", i)) |>
			stringi::stri_extract_all_regex(pattern = "(^[[:punct:]a-z_`%].+ (?=(<-)))", omit_no_match = TRUE) |>
			sapply(\(k){
				stringi::stri_split_fixed(k, "@family ", simplify = TRUE, omit_empty = TRUE) |> as.vector()
			}) |>
			purrr::compact() |>
			unlist() |>
			trimws() |>
			sort() %>%
			slider::slide(
				.after = 2
				, .step = 3
				, .f = c
				, .complete = length(.) > 2
				) |>
			purrr::compact() %>%
			rlang::set_names(rep.int("", length(.)))
		}) |>
	list2html(.ordered = FALSE) |>
	htmltools::html_print(viewer = NULL) |>
	file.copy(to = paste0("pkg/toc.html"))

git_repo <- git2r::repository(getwd())
git_repo |> plot()

git2r::commits() |>
	unlist() |>
	stringi::stri_extract_all_regex("[#].+", omit_no_match = TRUE, simplify = TRUE) |>
	as.vector() |>
	unique() |>
	purrr::discard(~.x == "") |>
	sort()

git2r::last_commit()
