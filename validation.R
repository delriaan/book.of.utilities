dir("pkg/R", full.names = TRUE) |> sapply(source)
library(magrittr, include.only = c("%<>%", "%>%"));

# calc.means() ====
calc.means(sample(30, 10) |> print());
calc.means(sample(30, 10) |> print(), "hm");
calc.means(sample(30, 10) |> print(), mean.type = c("am", "hm"));

pop <- rnorm(n = 1000, 30, 1)
samp <- sample(pop, 100)

calc.zero_mean(pop)
calc.zero_mean(pop, use.population = TRUE) # Should be identical to calling 'calc.zero_mean(pop)'

calc.zero_mean(samp)
calc.zero_mean(samp, use.population = TRUE) # Should be identical to calling 'calc.zero_mean(samp)'

x.boot <- NULL
x.boot <- replicate(100, sample(samp, length(samp), TRUE)) |> colMeans();
calc.zero_mean(x.boot, as.zscore = TRUE) |>
	sort() |>
	print() |>
	list() |>
	rlang::set_names("z") |>
	data.table::as.data.table(key = "z") %>%
	.[, pareto := as.quantile(z, digits = 6) %>% magrittr::add(abs(min(.))) |> ratio(type = "of.max", decimals = 6)] |>
	ggplot2::ggplot(ggplot2::aes(x = z, y = pareto)) +
	ggplot2::geom_point() +
	ggplot2::labs(title = "Bootstrapped Means")

x.boot <- NULL
x.boot <- replicate(100, sample(samp, length(samp), TRUE)) |> matrixStats::colMedians(); # Using a different point-estimate
calc.zero_mean(x.boot, as.zscore = TRUE) |>
	sort() |>
	print() |>
	list() |>
	rlang::set_names("z") |>
	data.table::as.data.table(key = "z") %>%
	.[, pareto := as.quantile(z, digits = 6) %>% magrittr::add(abs(min(.))) |> ratio(type = "of.max", decimals = 6)] |>
	ggplot2::ggplot(ggplot2::aes(x = z, y = pareto)) +
	ggplot2::geom_point() +
	ggplot2::labs(title = "Bootstrapped Medians")


# List Output
x <- list(set_1 = c(1, 26, 7, 21, 27, 3, 29, 24, 12, 20)
					, set_2 = c(90, 78, 1, 54, 40, 11, 48, 20, 28, 22)
					)

print(x) |>
	calc.harmonic_mean();

print(x) |>
	calc.geo_mean();

print(x) |>
	calc.means(mean.type = c("am", "hm"));

# Array/Matrix output
print(x) |>
	calc.means(post.op = simplify2array);

print(x) |>
	calc.means(mean.type = c("am", "hm", "zm", "gm"), post.op = simplify2array) |> str()

print(x) |>
	calc.means(mean.type = c("am", "hm", "zm"), post.op = simplify2array, as.zscore = TRUE) |> str()

print(x) |>
	calc.means(mean.type = c("am", "hm", "zm"), post.op = simplify2array, as.zscore = TRUE, use.population = TRUE) |> str()
#
# enlist() ====
# :: Test Objects
test_x = list(
	test_1 = c(1:5)
	, test_2 = list(c(1:5), 3, 4, c(letters[1:5]))
	);
nms = c("up", "down", "left", "right", "center");

# :: Unnamed Vector -> Elements become names
enlist(test_x$test_1);
enlist(test_x$test_2);

# :: Unnamed Vector & Provided Names (Full) -> All elements have names in 'nms'
enlist(test_x$test_1, nms);
enlist(test_x$test_2, !!!nms);
enlist(test_x$test_2, up, down, left, right, center);

# %>% Unnamed Vector & Provided Names (Partial) -> First two names are 'nms[1:2]' with the balance as default
enlist(test_x$test_1, !!!nms[1:2]);
enlist(test_x$test_2, !!!nms[1:2])
#
# vlogical() ====
vlogical(
	vector = data.table::data.table(t(sapply(1:100000, function(i){ c(a = sample(LETTERS, 1), b = sample(letters, 1)) })))
	, c(sample(LETTERS, 5), sample(letters, 5))
	, test = function(vector, q, ...){ any(unique(vector) %in% unique(q))}
	, simplify_with = mean
	, ignore.case = TRUE
	)

# miscellaneous ----
get.object_sizes() |> View()
#
# *regex ====
as.regex("test") |> is.regex() # TRUE
c(as.regex("test", "this"), "or_that") |> is.regex() # TRUE TRUE FALSE

unregex(i = c(as.regex("mp|[cye]+"), "hp", "hq"), x = colnames(mtcars)) #  "mpg"  "cyl"  "qsec" "gear" "carb" "hp

unregex(i = c(as.regex("mp|[cye]+"), "hp", "hq"), x = mtcars) #  "mpg"  "cyl"  "qsec" "gear" "carb" "hp

# custom operators ----
x <- data.table::data.table(i = sample(100, 10), j = sample(100, 10), key = c("i", "j")) |> unique()
y <- data.table::data.table(i = sample(200, 10, TRUE), j = sample(200, 10, TRUE), key = c("i", "j")) |> unique()
(z <- x %><% y) |> str()
(q <- x %tf% y) |> str()

debug(`%??%`)
(v <- data.table::data.table(y$i > mean(y$i), y$j <= mean(y$j)) %??% q) |> str()
v
purrr::reduce(v$result, rbind)

# as.recursive() ====
fun <- as.recursive(
        fun = function(...){
            sample(rlang::list2(...) |> unlist() |> as.vector(), size = 10, replace = TRUE)
          }
        , cond_def = ~mean(.) <= median(.)
        , finalize = ~list(x = rlang::set_names(., seq_along(.)), y = purrr::reduce(., ~mean(c(.x, .x + .y), na.rm = TRUE)))
        )

(inspect <- fun(!!!c(1:100)))

# counters
sample(c(TRUE, FALSE), 50, TRUE) %>% print() %>% count.cycles(reset = cumsum(.) %% 5 == 0)
factor.int(100, 90)
#
# Build Site ----
# usethis::use_pkgdown()
# pkgdown::build_site(pkg = "pkg", lazy = TRUE, override = list(destination = "../docs"))
