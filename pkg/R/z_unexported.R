# Helper Functions for `ratio()`: ----
softmax <- function(.p) exp(.p)/sum(exp(.p), na.rm = TRUE)

of.max <- function(.i) .i/max(.i, na.rm = TRUE);

of.sum <- function(.i) .i/sum(.i, na.rm = TRUE);

cumulative <- function(.i){ cumsum(.i)/sum(.i) }

dens <- function(.i, .type = "of.sum"){
  .p <- .i

  if (.type == "of.max"){
    .i <- softmax(.i)
  } else if (.type == "cumulative"){
    .i <- ratio(.i)
  }
  .p <- sapply(1:length(.i), function(n) mean(.i <= .i[n], na.rm = TRUE))
  .i * .p * (1 - .p)
}
