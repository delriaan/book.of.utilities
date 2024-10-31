# Helper Functions for `ratio()`: ----
softmax <- \(.p) exp(.p)/sum(exp(.p), na.rm = TRUE)

of.max <- \(.i) .i/max(.i, na.rm = TRUE);

of.sum <- \(.i) .i/sum(.i, na.rm = TRUE);

cumulative <- \(.i){ cumsum(.i)/sum(.i) }

dens <- \(.i, .type = "of.sum"){
  .p <- .i

  if (.type == "of.max"){
    .i <- softmax(.i)
  } else if (.type == "cumulative"){
    .i <- ratio(.i)
  }
  .p <- sapply(1:length(.i), \(n) mean(.i <= .i[n], na.rm = TRUE))
  .i * .p * (1 - .p)
}
