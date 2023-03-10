
library(magrittr);
library(data.table);
library(glmnetUtils)

D <- purrr::map_dfr(100:1000, ~{
	x = runif(n = 200, min = 100, max = 500)
	mean.x = mean(x)
	sd.x = sd(x)
	rms.x = book.of.utilities::calc.rms(x)
	rm(x)
	mget(ls())
}) %>% as.data.table()

D[, y := rms.x/sd.x]

D.mdl <- cva.glmnet(x = as.matrix(D[, .(sd.x, mean.x)]), y = D$rms.x)

D.mdl %$% {
	plot(c(0, alpha %>% diff()), c(0, modlist %>% purrr::map_dbl(~.x$cvm %>% sum()) %>% diff()), type = "b")
}

D.mdl_coefs <- D.mdl %$% modlist[[3]]  %>% coef() %>% .[c("(Intercept)", "mean.x", "sd.x"), ]

D[order(mean.x, sd.x)
	, list(x = .I, rms.x, mean.x, sd.x, y = { D.mdl_coefs[1] + (mean.x * D.mdl_coefs[2]) + (sd.x * D.mdl_coefs[3]) })
][,{
	plot(x = mean.x, y = rms.x, ylab = "rms.x", xlab = "mean.x", col= "blue");
	points(x = mean.x, y = y, col = "red ")

	plot(x = sd.x, y = rms.x/2, ylab = "rms.x/2", xlab = "sd.x", col = "blue");
	points(x = sd.x, y = y, col = "red")
}]

D[order(rms.x), (rms.x - mean.x)/mean.x] %>% density() %>% plot()
