# calc.means() ====
calc.means(sample(30, 10));
calc.means(sample(30, 10), "hm");
calc.means(sample(30, 10), mean.type = c("am", "hm"));

x <- sample(100, 50)
calc.zero_mean(x)
calc.zero_mean(x, as.zscore = TRUE)
calc.zero_mean(x, use.population = TRUE )
calc.zero_mean(x, as.zscore = TRUE, use.population = TRUE)

# %>% List Output
calc.harmonic_mean(list(set_1 = sample(30, 10), set_2 = sample(30, 10)));
calc.means(list(set_1 = sample(30, 10), set_2 = sample(30, 10)), mean.type = c("am", "hm"));

# Array/Matrix output
calc.means(list(set_1 = sample(30, 10), set_2 = sample(30, 10)), mean.type = c("am", "hm"), post.op = simplify2array);
calc.harmonic_mean(list(set_1 = sample(30, 10), set_2 = sample(30, 10)), post.op = simplify2array)
#
# enlist() ====
# %>% Test Objects
test_x = list(
	test_1 = c(1:5)
	, test_2 = list(c(1:5), 3, 4, c(letters[1:5]))
	);
nms = c("up", "down", "left", "right", "center");

# %>% Unnamed Vector -> Elements become names
enlist(test_x$test_1);
enlist(test_x$test_2);

# %>% Unnamed Vector & Provided Names (Full) -> All elements have names in 'nms'
enlist(test_x$test_1, nms);
enlist(test_x$test_2, nms);
enlist(test_x$test_2, up, down, left, right, center);

# %>% Unnamed Vector & Provided Names (Partial) -> First two names are 'nms[1:2]' with the balance as default
enlist(test_x$test_1, nms[1:2]);
enlist(test_x$test_2, nms[1:2])
#
# vlogical() ====
vlogical(
	vector = data.table::data.table(t(sapply(1:100000, function(i){ c(a = sample(LETTERS, 1), b = sample(letters, 1)) })))
	, c(sample(LETTERS, 5), sample(letters, 5))
	, test = function(vector, q, ...){ any(unique(vector) %in% unique(q))}
	, simplify_with = mean
	, ignore.case = TRUE
	)

# *regex ====
as.regex("test") |> is.regex() # TRUE
c(as.regex("test", "this"), "or_that") |> is.regex() # TRUE TRUE FALSE

unregex(i = c(as.regex("mp|[cye]+"), "hp", "hq"), x = colnames(mtcars)) #  "mpg"  "cyl"  "qsec" "gear" "carb" "hp

unregex(i = c(as.regex("mp|[cye]+"), "hp", "hq"), x = mtcars) #  "mpg"  "cyl"  "qsec" "gear" "carb" "hp

# ::: ----
x <- data.table::data.table(i = sample(100, 10), j = sample(100, 10), key = c("i", "j")) |> unique()
y <- data.table::data.table(i = sample(200, 10, TRUE), j = sample(200, 10, TRUE), key = c("i", "j")) |> unique()
z <- x %><% y
q <- x %::% y
v <- z %??% q
v
purrr::reduce(v$result, rbind)
